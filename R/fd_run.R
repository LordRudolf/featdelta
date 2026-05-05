#' Run the featdelta incremental feature pipeline
#'
#' Executes the standard featdelta pipeline:
#'
#' `fd_fetch()` -> `fd_compute()` -> `fd_upsert()`
#'
#' `fd_run()` is the orchestration entry point that connects database fetching,
#' in-memory computation, and database upsert into a single incremental run.
#'
#' @param con A live `DBI` connection.
#' @param sql SQL query used to fetch raw rows.
#' @param defs A `featdelta_defs` object created by `fd_define()`.
#' @param key Character scalar naming the key column shared by raw data and
#'   feature table.
#' @param feat_table_name Character scalar naming the target feature table.
#' @param verbose Logical. If `TRUE`, print progress messages.
#' @param fetch_mode Fetch mode. `"new_only"` fetches rows whose key is not yet
#'   present in `feat_table_name`. `"all"` fetches all rows returned by `sql`,
#'   allowing explicit refresh/backfill runs.
#' @param use_max_key Logical. Passed to `fd_fetch()` when `fetch_mode =
#'   "new_only"` and the feature table already exists.
#' @param fetch_limit Optional positive row limit applied after fetching. This
#'   is intended for previews and small dry development runs, not as a SQL
#'   optimizer.
#' @param compute_strict Logical strictness flag passed to `fd_compute()`.
#' @param compute_envir Optional environment override passed to `fd_compute()`.
#' @param create_table Logical or `"auto"`. Passed to `fd_upsert()`.
#' @param alter_table Logical. Passed to `fd_upsert()`.
#' @param update_table Logical. Passed to `fd_upsert()`.
#' @param dialect Optional dialect override. Supported values are `"postgres"`,
#'   `"sqlite"`, and `"mysql"`.
#' @param chunk_size Optional chunk size. Passed to `fd_upsert()`.
#' @param fail_fast Logical. If `TRUE`, stage errors are raised immediately. If
#'   `FALSE`, errors from fetch, compute, or upsert are captured in the returned
#'   `fd_run_report`.
#' @param return_data Controls whether raw and/or computed data should be
#'   included in the returned run report.
#' @param preview_n Non-negative number of rows to include in report previews.
#' @param ... Reserved for future context options passed to `resolve_ctx()`.
#'
#' @return An object of class `"fd_run_report"` containing stage summaries,
#'   timings, row counts, the compute report, and the upsert report. Depending
#'   on `return_data`, it may also contain raw and/or computed feature data.
#'
#' @details
#' `fetch_mode = "new_only"` is the default incremental mode. If the feature
#' table already exists, `fd_fetch()` returns only rows whose key is missing
#' from the feature table. If the feature table does not exist yet, all rows
#' returned by `sql` are fetched because no rows have been processed. This mode
#' is key-based: it does not recompute existing feature-table rows just because
#' feature definitions changed or new feature definitions were added.
#'
#' `fetch_mode = "all"` is an explicit refresh/backfill mode. It recomputes all
#' rows returned by `sql` and passes them to `fd_upsert()`. With the default
#' `update_table = TRUE`, existing keys are updated and new keys are inserted.
#' Use this mode when existing feature values should be refreshed, for example
#' after changing a definition or adding a feature that should be backfilled for
#' already-processed keys.
#'
#' `fd_run()` does not currently coordinate concurrent writers. For the MVP,
#' avoid running multiple `fd_run()`/`fd_upsert()` calls against the same feature
#' table at the same time. Concurrent writes to different feature tables are
#' independent.
#'
#' The returned upsert report can include `extra_columns`: columns that exist in
#' the target feature table but are not produced by the current definitions.
#' These columns are left untouched; the package does not drop, rename, or retire
#' columns automatically.
#'
#' @examples
#' \dontrun{
#' defs <- fd_define(
#'   hp_per_cyl = hp / cyl,
#'   engine_ratios = fd_block({
#'     data.frame(
#'       disp_per_cyl = disp / cyl,
#'       wt_per_hp = wt / hp
#'     )
#'   })
#' )
#'
#' res <- fd_run(
#'   con = con,
#'   sql = "select * from raw_schema.raw_table",
#'   defs = defs,
#'   key = "application_id",
#'   feat_table_name = "feat_schema.features",
#'   verbose = TRUE
#' )
#' }
#'
#' @family featdelta pipeline helpers
#' @export
fd_run <- function(
    con,
    sql,
    defs,
    key,
    feat_table_name,
    verbose = FALSE,

    # ---- fetch controls
    fetch_mode = c("new_only", "all"),
    use_max_key = FALSE,
    fetch_limit = NULL,

    # ---- compute controls
    compute_strict = TRUE,
    compute_envir = NULL,

    # ---- upsert controls
    create_table = "auto",
    alter_table  = TRUE,
    update_table = TRUE,
    dialect = NULL,
    chunk_size = NULL,

    # ---- orchestration
    fail_fast = TRUE,
    return_data = c("none", "features", "raw", "both"),
    preview_n = 10L,
    ...
) {
  fetch_mode <- match.arg(fetch_mode)
  return_data <- match.arg(return_data)
  started_at <- Sys.time()

  sql_clean <- clean_sql(sql)

  ctx <- resolve_ctx(
    con,
    feat_table_name = feat_table_name,
    key = key,
    dialect = dialect,
    ...
  )

  ensure_supported_dialect(ctx$dialect)

  validate_general_args(
    key = key,
    feat_table_name = feat_table_name,
    sql = sql_clean,
    defs = defs,
    compute_envir = compute_envir,
    logicals = list(
      verbose = verbose,
      use_max_key = use_max_key,
      compute_strict = compute_strict,
      alter_table = alter_table,
      update_table = update_table,
      fail_fast = fail_fast
    ),
    logicals_with_text = list(
      create_table = create_table
    )
  )

  fetch_limit <- fd_run_validate_optional_positive_int(fetch_limit, "fetch_limit")
  chunk_size <- fd_run_validate_optional_positive_int(chunk_size, "chunk_size")
  preview_n <- fd_run_validate_nonnegative_int(preview_n, "preview_n")

  table_exists <- DBI::dbExistsTable(ctx$con, ctx$feat_table_id)

  fetch_stage <- tryCatch(
    fd_run_fetch_stage(
      ctx = ctx,
      sql_clean = sql_clean,
      fetch_mode = fetch_mode,
      table_exists = table_exists,
      use_max_key = use_max_key,
      fetch_limit = fetch_limit,
      verbose = verbose
    ),
    error = function(e) fd_run_stage_error("fetch", e)
  )

  if (!isTRUE(fetch_stage$ok)) {
    return(fd_run_handle_stage_failure(
      err = fetch_stage,
      fail_fast = fail_fast,
      started_at = started_at,
      ctx = ctx,
      sql_clean = sql_clean,
      fetch_mode = fetch_mode,
      table_exists = table_exists,
      use_max_key = use_max_key,
      fetch_limit = fetch_limit,
      preview_n = preview_n
    ))
  }

  raw_data <- fetch_stage$data

  validate_general_args(
    data = raw_data,
    key = ctx$key
  )

  compute_stage <- tryCatch({
    compute_result <- fd_compute(
      raw_data,
      defs,
      key = ctx$key,
      compute_strict = compute_strict,
      compute_envir = compute_envir,
      verbose = verbose,
      return_report = TRUE
    )

    list(
      ok = TRUE,
      data = compute_result$data,
      report = compute_result$report,
      error = NULL
    )
  }, error = function(e) {
    fd_run_stage_error(
      stage = "compute",
      err = e,
      report = attr(e, "fd_report"),
      data = attr(e, "fd_partial")
    )
  })

  if (!isTRUE(compute_stage$ok)) {
    return(fd_run_handle_stage_failure(
      err = compute_stage,
      fail_fast = fail_fast,
      started_at = started_at,
      ctx = ctx,
      sql_clean = sql_clean,
      fetch_mode = fetch_mode,
      table_exists = table_exists,
      use_max_key = use_max_key,
      fetch_limit = fetch_limit,
      preview_n = preview_n,
      raw_data = raw_data,
      compute_report = compute_stage$report,
      features_df = compute_stage$data,
      return_data = return_data
    ))
  }

  features_df <- compute_stage$data

  upsert_stage <- tryCatch({
    upsert_report <- fd_upsert(
      ctx$con,
      features_df = features_df,
      feat_table_name = ctx$feat_table_name,
      key = ctx$key,
      create_table = create_table,
      alter_table = alter_table,
      update_table = update_table,
      chunk_size = chunk_size,
      dialect = ctx$dialect,
      verbose = verbose,
      return_report = TRUE
    )

    list(ok = TRUE, report = upsert_report, error = NULL)
  }, error = function(e) {
    fd_run_stage_error("upsert", e)
  })

  if (!isTRUE(upsert_stage$ok)) {
    return(fd_run_handle_stage_failure(
      err = upsert_stage,
      fail_fast = fail_fast,
      started_at = started_at,
      ctx = ctx,
      sql_clean = sql_clean,
      fetch_mode = fetch_mode,
      table_exists = table_exists,
      use_max_key = use_max_key,
      fetch_limit = fetch_limit,
      preview_n = preview_n,
      raw_data = raw_data,
      compute_report = compute_stage$report,
      features_df = features_df,
      return_data = return_data
    ))
  }

  fd_run_build_report(
    success = TRUE,
    stage = "complete",
    started_at = started_at,
    ctx = ctx,
    sql_clean = sql_clean,
    fetch_mode = fetch_mode,
    table_exists = table_exists,
    use_max_key = use_max_key,
    fetch_limit = fetch_limit,
    fetch_info = fetch_stage$info,
    raw_data = raw_data,
    compute_report = compute_stage$report,
    features_df = features_df,
    upsert_report = upsert_stage$report,
    return_data = return_data,
    preview_n = preview_n
  )
}


fd_run_fetch_stage <- function(ctx,
                               sql_clean,
                               fetch_mode,
                               table_exists,
                               use_max_key,
                               fetch_limit,
                               verbose) {
  if (isTRUE(verbose)) {
    message("fd_run(): fetch_mode = ", fetch_mode)
  }

  if (identical(fetch_mode, "new_only") && isTRUE(table_exists)) {
    raw_data <- fd_fetch(
      con = ctx$con,
      sql = sql_clean,
      key = ctx$key,
      feat_table_name = ctx$feat_table_name,
      use_max_key = use_max_key,
      verbose = verbose
    )
    fetch_source <- "fd_fetch"
    fetch_attr <- attr(raw_data, "fd_fetch")
  } else {
    raw_data <- DBI::dbGetQuery(ctx$con, sql_clean)
    fetch_source <- if (identical(fetch_mode, "all")) "all" else "initial_all"
    fetch_attr <- NULL
  }

  n_before_limit <- nrow(raw_data)
  limit_applied <- !is.null(fetch_limit) && n_before_limit > fetch_limit

  if (isTRUE(limit_applied)) {
    raw_data <- raw_data[seq_len(fetch_limit), , drop = FALSE]
  }

  info <- list(
    mode = fetch_mode,
    source = fetch_source,
    table_exists = table_exists,
    use_max_key = use_max_key,
    limit = fetch_limit,
    limit_applied = limit_applied,
    n_rows_before_limit = as.integer(n_before_limit),
    n_rows = as.integer(nrow(raw_data)),
    fd_fetch = fetch_attr
  )

  list(ok = TRUE, data = raw_data, info = info)
}


fd_run_stage_error <- function(stage, err, report = NULL, data = NULL) {
  list(
    ok = FALSE,
    stage = stage,
    error = err,
    message = conditionMessage(err),
    class = class(err),
    report = report,
    data = data
  )
}


fd_run_handle_stage_failure <- function(err,
                                        fail_fast,
                                        started_at,
                                        ctx,
                                        sql_clean,
                                        fetch_mode,
                                        table_exists,
                                        use_max_key,
                                        fetch_limit,
                                        preview_n,
                                        raw_data = NULL,
                                        compute_report = NULL,
                                        features_df = NULL,
                                        return_data = "none") {
  if (isTRUE(fail_fast)) {
    stop(err$error)
  }

  fd_run_build_report(
    success = FALSE,
    stage = err$stage,
    started_at = started_at,
    ctx = ctx,
    sql_clean = sql_clean,
    fetch_mode = fetch_mode,
    table_exists = table_exists,
    use_max_key = use_max_key,
    fetch_limit = fetch_limit,
    fetch_info = NULL,
    raw_data = raw_data,
    compute_report = compute_report,
    features_df = features_df,
    upsert_report = NULL,
    error = err,
    return_data = return_data,
    preview_n = preview_n
  )
}


fd_run_build_report <- function(success,
                                stage,
                                started_at,
                                ctx,
                                sql_clean,
                                fetch_mode,
                                table_exists,
                                use_max_key,
                                fetch_limit,
                                fetch_info = NULL,
                                raw_data = NULL,
                                compute_report = NULL,
                                features_df = NULL,
                                upsert_report = NULL,
                                error = NULL,
                                return_data = "none",
                                preview_n = 10L) {
  finished_at <- Sys.time()

  raw_n <- if (is.null(raw_data)) NA_integer_ else as.integer(nrow(raw_data))
  feature_n <- if (is.null(features_df)) NA_integer_ else as.integer(nrow(features_df))
  feature_cols <- if (is.null(features_df)) character() else setdiff(names(features_df), ctx$key)

  out <- list(
    success = success,
    stage = stage,
    started_at = started_at,
    finished_at = finished_at,
    time_sec = as.numeric(difftime(finished_at, started_at, units = "secs")),
    key = ctx$key,
    feat_table_name = ctx$feat_table_name,
    dialect = ctx$dialect,
    sql = sql_clean,
    fetch = fetch_info %||% list(
      mode = fetch_mode,
      table_exists = table_exists,
      use_max_key = use_max_key,
      limit = fetch_limit,
      n_rows = raw_n
    ),
    compute = list(
      n_rows = feature_n,
      n_features = length(feature_cols),
      feature_names = feature_cols,
      report = compute_report
    ),
    upsert = upsert_report,
    preview = list(
      raw = fd_run_preview(raw_data, preview_n),
      features = fd_run_preview(features_df, preview_n)
    ),
    data = fd_run_select_return_data(raw_data, features_df, return_data),
    error = if (is.null(error)) NULL else list(
      stage = error$stage,
      message = error$message,
      class = error$class
    )
  )

  class(out) <- c("fd_run_report", "list")
  out
}


fd_run_select_return_data <- function(raw_data, features_df, return_data) {
  if (identical(return_data, "none")) {
    return(NULL)
  }

  out <- list()

  if (return_data %in% c("raw", "both")) {
    out$raw <- raw_data
  }

  if (return_data %in% c("features", "both")) {
    out$features <- features_df
  }

  out
}


fd_run_preview <- function(x, n) {
  if (is.null(x) || n == 0L) {
    return(NULL)
  }

  utils::head(x, n)
}


fd_run_validate_optional_positive_int <- function(x, name) {
  if (is.null(x)) {
    return(NULL)
  }

  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x <= 0) {
    stop("`", name, "` must be NULL or a positive number.", call. = FALSE)
  }

  as.integer(x)
}


fd_run_validate_nonnegative_int <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0) {
    stop("`", name, "` must be a non-negative number.", call. = FALSE)
  }

  as.integer(x)
}


#' Print a featdelta run report
#'
#' @param x An `fd_run_report` object.
#' @param ... Unused.
#'
#' @return The input object invisibly.
#'
#' @export
print.fd_run_report <- function(x, ...) {
  cat("<fd_run_report>\n")
  cat(" success:   ", x$success, "\n", sep = "")
  cat(" stage:     ", x$stage, "\n", sep = "")
  cat(" table:     ", x$feat_table_name, "\n", sep = "")
  cat(" dialect:   ", x$dialect, "\n", sep = "")
  cat(" fetched:   ", x$fetch$n_rows %||% NA_integer_, " rows\n", sep = "")
  cat(" computed:  ", x$compute$n_rows %||% NA_integer_, " rows", sep = "")
  cat(", ", x$compute$n_features %||% 0L, " feature columns\n", sep = "")

  if (!is.null(x$upsert) && !is.null(x$upsert$counts)) {
    cat(
      " upsert:    would_insert=", x$upsert$counts$would_insert,
      ", would_update=", x$upsert$counts$would_update,
      "\n",
      sep = ""
    )
  }

  if (!is.null(x$error)) {
    cat(" error:     [", x$error$stage, "] ", x$error$message, "\n", sep = "")
  }

  invisible(x)
}
