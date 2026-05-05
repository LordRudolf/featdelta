#' Upsert computed features into a database features table
#'
#' Takes a feature `data.frame` produced by `fd_compute()` (one row per entity id)
#' and writes it into a database table, using explicit incremental semantics:
#' new keys are inserted; existing keys are optionally updated.
#'
#' The function is set-based (staging + merge) and avoids "read all ids into R"
#' patterns. Writes are performed in chunks (optional) inside a transaction.
#'
#' Schema evolution is supported in a restricted, safe form: when `alter_table = TRUE`,
#' missing feature columns are added to the target table via `ALTER TABLE ... ADD COLUMN ...`.
#' No column drops/renames/type changes are performed. Columns that exist in
#' the target table but are not present in `features_df` are left untouched and
#' reported as `extra_columns`.
#'
#' @param con A `DBIConnection`. Must be a live connection to the database where
#'   `feat_table_name` exists (or will be created).
#'
#' @param features_df A `data.frame` containing at minimum the primary key column
#'   specified by `key`, plus one or more feature columns to be stored.
#'   Requirements:
#'   * `key` must exist in `names(features_df)`.
#'   * `features_df[[key]]` must contain no `NA` and no duplicate values.
#'   * Each feature column should be an atomic vector (numeric, integer, logical,
#'     character, Date/POSIXct) that can be written by DBI for the chosen dialect.
#'
#' @param feat_table_name A single string. Name of the database table where features
#'   are stored (the "target" table). For dialects supporting schemas (e.g. Postgres),
#'   this may be schema-qualified (e.g. `"public.my_features"`), subject to dialect rules.
#'
#' @param key A single string. Name of the primary key column in both `features_df`
#'   and `feat_table_name`. This column uniquely identifies each entity/row.
#'
#' @param create_table Logical, or character value `auto`. If `TRUE`, create `feat_table_name` when it does not exist.
#'   The created table includes all columns present in `features_df` and defines
#'   `PRIMARY KEY(key)`. If `FALSE` and `feat_table_name` is missing, the function errors.If `auto`,
#'   create the table it hasn't been already created.
#'
#' @param alter_table Logical. If `TRUE`, add missing feature columns found in
#'   `features_df` but not present in `feat_table_name` using `ALTER TABLE ... ADD COLUMN ...`.
#'   Only additive schema changes are performed. If `FALSE`, the function errors
#'   when `features_df` contains columns not present in `feat_table_name`.
#'
#' @param update_table  Logical. Controls incremental behavior for keys that already exist
#'   in `feat_table_name`:
#'   * If `TRUE` (default), existing keys are updated (upsert).
#'   * If `FALSE`, only new keys are inserted; any conflict with existing keys
#'     results in an error (insert-only mode).
#'
#' @param chunk_size Optional integer batch size. If provided, `features_df` is written
#'   in batches of up to `chunk_size` rows, each merged via a staging table.
#'   Use this to limit memory/packet sizes for large writes. If `NULL`, write in one batch.
#' @param verbose Logical. If `TRUE`, emit progress messages (e.g., chunk progress,
#'   table creation/alteration actions). Does not affect returned results.
#' @param return_report Logical. If `TRUE`, return a structured
#'   `fd_upsert_report`; if `FALSE`, return `TRUE` invisibly after successful
#'   side effects.
#' @param dialect Optional dialect override. Supported values are `"postgres"`,
#'   `"sqlite"`, and `"mysql"`.
#'
#' @details
#' **Incremental semantics**
#' \itemize{
#'   \item Insert new keys (keys in `features_df` not present in `feat_table_name`).
#'   \item Update existing keys only when `update_table = TRUE`.
#'   \item When `update_table = FALSE`, any overlap between staged keys and existing keys
#'         is treated as a conflict and aborts the write.
#' }
#'
#' **Counts**
#' The returned report contains `would_insert` / `would_update` counts, computed as
#' existence-based counts *prior to the merge* (within the transaction). These are
#' not "rows whose values changed", only "rows targeted as insert/update".
#'
#' **Transactions**
#' The operation runs in a transaction. If any step fails (schema change, staging write,
#' merge), the transaction is rolled back and the target table is unchanged.
#' Existing target tables used with `update_table = TRUE` must have a primary
#' key or unique constraint/index on `key`. Tables created by `fd_upsert()` get
#' this primary key automatically.
#'
#' **Concurrency**
#' `fd_upsert()` does not currently take an explicit table-level lock. Avoid
#' running concurrent writes to the same feature table. Concurrent writes to
#' different feature tables are independent.
#'
#' @return
#' An `fd_upsert_report` object (S3) with a structured summary of actions performed:
#' \itemize{
#'   \item `table_created` (logical)
#'   \item `columns_added` (character vector)
#'   \item `extra_columns` (character vector): target-table columns not present
#'         in the incoming features data, excluding `key`
#'   \item `counts$would_insert` (integer)
#'   \item `counts$would_update` (integer; `0` when `update_table = FALSE`)
#'   \item optional per-chunk breakdown (if chunking is used)
#' }
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'
#' feats <- data.frame(
#'   id = c(1, 2, 3),
#'   f_age = c(10, 20, 30),
#'   f_flag = c(TRUE, FALSE, TRUE)
#' )
#'
#' # Create table and insert initial rows
#' r1 <- fd_upsert(
#'   con = con,
#'   features_df = feats,
#'   feat_table_name = "features_tbl",
#'   key = "id",
#'   create_table = TRUE,
#'   alter_table = FALSE,
#'   update_table = TRUE
#' )
#'
#' # Upsert: update ids 2-3, insert id 4
#' feats2 <- data.frame(
#'   id = c(2, 3, 4),
#'   f_age = c(21, 31, 40),
#'   f_flag = c(FALSE, TRUE, FALSE)
#' )
#'
#' r2 <- fd_upsert(
#'   con = con,
#'   features_df = feats2,
#'   feat_table_name = "features_tbl",
#'   key = "id",
#'   update_table = TRUE
#' )
#'
#' # Schema evolution: add a new feature column
#' feats3 <- data.frame(
#'   id = c(4, 5),
#'   f_age = c(41, 50),
#'   f_flag = c(TRUE, TRUE),
#'   f_new = c("A", "B")
#' )
#'
#' r3 <- fd_upsert(
#'   con = con,
#'   features_df = feats3,
#'   feat_table_name = "features_tbl",
#'   key = "id",
#'   alter_table = TRUE,
#'   update_table = TRUE
#' )
#' }
#'
#' @export
fd_upsert <- function(
    con,
    features_df,
    feat_table_name,
    key,
    create_table = "auto",
    alter_table  = TRUE,
    update_table = TRUE,
    chunk_size   = NULL,
    verbose      = TRUE,
    return_report = TRUE,
    dialect      = NULL) {

  ctx <- resolve_ctx(
    con_or_fd = con,
    feat_table_name = feat_table_name,
    key = key,
    dialect = dialect
    #meta_enabled = meta_enabled,
    #meta_schema = meta_schema
  )

  ensure_supported_dialect(ctx$dialect)

  ## validation of inputs
  stopifnot(!is.null(features_df))
  validate_general_args(
    features_df = features_df,
    key = key,
    logicals = list(
      alter_table = alter_table,
      update_table = update_table,
      verbose = verbose,
      return_report = return_report
    ),
    logicals_with_text = list(
      create_table = create_table
    )
  )

  if (!is.null(chunk_size)) {
    if (!is.numeric(chunk_size) || length(chunk_size) != 1L || is.na(chunk_size) || chunk_size <= 0) {
      stop("`chunk_size` must be NULL or a positive number.")
    }
    chunk_size <- as.integer(chunk_size)
  }

  # Normalize common problematic types for DB writes
  features_df <- normalize_features_df(features_df)

  ## MAIN transaction here (schema + all chunks)
  report <- DBI::dbWithTransaction(ctx$con, {

    # 1) Make sure the target table is ready for writing:
    #    - resolve create_table = "auto"
    #    - create table if requested/missing
    #    - add missing columns if allowed
    target_state <- fd_upsert_prepare_target(
      ctx = ctx,
      features_df = features_df,
      create_table = create_table,
      alter_table = alter_table,
      update_table = update_table,
      verbose = verbose
    )

    # 2) Nothing to write.
    if (nrow(features_df) == 0L) {
      if (!isTRUE(return_report)) {
        if (isTRUE(verbose)) message("No rows to write.")
        TRUE
      } else {
        fd_upsert_empty_report(
          feat_table_name = feat_table_name,
          key = key,
          dialect = ctx$dialect,
          table_created = target_state$table_created,
          columns_added = target_state$columns_added,
          extra_columns = target_state$extra_columns
        )
      }
    } else {
      # 3) Write the incoming features in chunks.
      if (isTRUE(verbose)) {
        message(
          "Writing features to ", feat_table_name,
          " (dialect=", ctx$dialect, "): ",
          nrow(features_df), " rows. Mode: ",
          if (isTRUE(update_table)) "upsert" else "insert-only", "."
        )
      }

      chunk_state <- fd_upsert_execute_chunks(
        con = ctx$con,
        dialect = ctx$dialect,
        target_q = target_state$target_q,
        key = key,
        features_df = features_df,
        update_table = update_table,
        chunk_size = chunk_size,
        verbose = verbose,
        return_report = return_report
      )

      # 4) If the caller only wants side effects, finish here.
      if (!isTRUE(return_report)) {
        if (isTRUE(verbose)) message("Done.")
        TRUE
      } else {
        # 5) Build the final structured report.
        out <- fd_upsert_build_report(
          feat_table_name = feat_table_name,
          key = key,
          dialect = ctx$dialect,
          n_rows = nrow(features_df),
          n_chunks = chunk_state$n_chunks,
          table_created = target_state$table_created,
          columns_added = target_state$columns_added,
          extra_columns = target_state$extra_columns,
          totals_would_insert = chunk_state$totals_would_insert,
          totals_would_update = chunk_state$totals_would_update,
          chunk_details = chunk_state$chunk_details
        )

        if (isTRUE(verbose)) {
          message(
            "Done. would_insert=", out$counts$would_insert,
            if (isTRUE(update_table)) paste0(", would_update=", out$counts$would_update) else ""
          )
          if (length(out$columns_added) > 0L) {
            message("Columns added: ", paste(out$columns_added, collapse = ", "))
          }
          if (length(out$extra_columns) > 0L) {
            message("Extra target columns left untouched: ", paste(out$extra_columns, collapse = ", "))
          }
        }

        out
      }
    }
  })

  if (isTRUE(return_report)) {
    report
  } else {
    invisible(TRUE)
  }
}


fd_upsert_prepare_target <- function(
    ctx,
    features_df,
    create_table,
    alter_table,
    update_table,
    verbose
) {
  table_id <- ctx$feat_table_id
  table_exists <- DBI::dbExistsTable(ctx$con, table_id)

  if (is.character(create_table) && identical(create_table, "auto")) {
    create_table <- !table_exists
  }

  if (isTRUE(create_table) && table_exists) {
    drop_sql <- render_sql(
      fd_sql_templates$drop_table[[ctx$dialect]],
      list(target = ctx$feat_table_q)
    )

    stop(
      paste0(
        "create_table=TRUE but table already exists: ", ctx$feat_table_name, "\n\n",
        "Overwriting is not allowed.\n",
        "If you are sure you want to delete the table, run:\n",
        "DBI::dbExecute(con, ", shQuote(drop_sql), ")\n"
      ),
      call. = FALSE
    )
  }

  if (!table_exists && !isTRUE(create_table)) {
    stop(
      paste0(
        "Table does not exist: ", ctx$feat_table_name,
        ". Set create_table=TRUE or create_table='auto' to create it."
      ),
      call. = FALSE
    )
  }

  table_created <- FALSE

  if (!table_exists && isTRUE(create_table)) {
    if (isTRUE(verbose)) {
      message("Creating table: ", ctx$feat_table_name)
    }

    create_features_table(
      con = ctx$con,
      dialect = ctx$dialect,
      feat_table_name = ctx$feat_table_name,
      target_q = ctx$feat_table_q,
      features_df = features_df,
      key = ctx$key
    )

    table_created <- TRUE
  }

  columns_added <- ensure_columns(
    con = ctx$con,
    dialect = ctx$dialect,
    table_id = ctx$feat_table_id,
    table_q = ctx$feat_table_q,
    features_df = features_df,
    key = ctx$key,
    alter_table = alter_table,
    verbose = verbose
  )
  extra_columns <- attr(columns_added, "extra_columns") %||% character()
  attr(columns_added, "extra_columns") <- NULL

  if (isTRUE(update_table) && !isTRUE(table_created)) {
    ensure_key_constraint(
      con = ctx$con,
      dialect = ctx$dialect,
      table_name = ctx$feat_table_name,
      table_id = ctx$feat_table_id,
      table_q = ctx$feat_table_q,
      key = ctx$key
    )
  }

  list(
    target_q = ctx$feat_table_q,
    table_created = table_created,
    columns_added = columns_added,
    extra_columns = extra_columns
  )
}

ensure_key_constraint <- function(con, dialect, table_name, table_id, table_q, key) {
  ok <- switch(
    dialect,
    sqlite = sqlite_has_key_constraint(con, table_q, key),
    postgres = postgres_has_key_constraint(con, table_name, key),
    mysql = mysql_has_key_constraint(con, table_name, key),
    FALSE
  )

  if (!isTRUE(ok)) {
    stop(
      sprintf(
        "Target table %s must have a PRIMARY KEY or UNIQUE constraint on %s when update_table=TRUE.",
        table_q,
        shQuote(key)
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

sqlite_has_key_constraint <- function(con, table_q, key) {
  table_info <- DBI::dbGetQuery(con, paste0("PRAGMA table_info(", table_q, ")"))
  if (nrow(table_info) > 0L && any(table_info$name == key & table_info$pk > 0L)) {
    return(TRUE)
  }

  index_list <- DBI::dbGetQuery(con, paste0("PRAGMA index_list(", table_q, ")"))
  if (nrow(index_list) == 0L) {
    return(FALSE)
  }

  unique_indexes <- index_list$name[index_list$unique == 1L]
  for (idx in unique_indexes) {
    idx_q <- as.character(DBI::dbQuoteIdentifier(con, idx))
    idx_info <- DBI::dbGetQuery(con, paste0("PRAGMA index_info(", idx_q, ")"))
    if (identical(idx_info$name, key)) {
      return(TRUE)
    }
  }

  FALSE
}

postgres_has_key_constraint <- function(con, table_name, key) {
  table_lit <- as.character(DBI::dbQuoteLiteral(con, table_name))
  key_lit <- as.character(DBI::dbQuoteLiteral(con, key))

  sql <- paste0(
    "SELECT 1 ",
    "FROM pg_constraint c ",
    "WHERE c.conrelid = ", table_lit, "::regclass ",
    "AND c.contype IN ('p', 'u') ",
    "AND (",
    "  SELECT array_agg(a.attname ORDER BY ck.ord) ",
    "  FROM unnest(c.conkey) WITH ORDINALITY AS ck(attnum, ord) ",
    "  JOIN pg_attribute a ON a.attrelid = c.conrelid AND a.attnum = ck.attnum",
    ") = ARRAY[", key_lit, "]::name[] ",
    "LIMIT 1"
  )

  nrow(DBI::dbGetQuery(con, sql)) > 0L
}

mysql_has_key_constraint <- function(con, table_name, key) {
  parts <- strsplit(table_name, ".", fixed = TRUE)[[1]]
  if (length(parts) == 1L) {
    schema_expr <- "DATABASE()"
    table_lit <- as.character(DBI::dbQuoteLiteral(con, parts[1]))
  } else if (length(parts) == 2L) {
    schema_expr <- as.character(DBI::dbQuoteLiteral(con, parts[1]))
    table_lit <- as.character(DBI::dbQuoteLiteral(con, parts[2]))
  } else {
    stop("`feat_table_name` must be 'table' or 'schema.table'.", call. = FALSE)
  }

  key_lit <- as.character(DBI::dbQuoteLiteral(con, key))

  sql <- paste0(
    "SELECT 1 ",
    "FROM information_schema.table_constraints tc ",
    "JOIN information_schema.key_column_usage kcu ",
    "  ON kcu.constraint_schema = tc.constraint_schema ",
    " AND kcu.constraint_name = tc.constraint_name ",
    " AND kcu.table_schema = tc.table_schema ",
    " AND kcu.table_name = tc.table_name ",
    "WHERE tc.constraint_type IN ('PRIMARY KEY', 'UNIQUE') ",
    "AND tc.table_schema = ", schema_expr, " ",
    "AND tc.table_name = ", table_lit, " ",
    "GROUP BY tc.constraint_schema, tc.constraint_name ",
    "HAVING COUNT(*) = 1 AND SUM(kcu.column_name = ", key_lit, ") = 1 ",
    "LIMIT 1"
  )

  nrow(DBI::dbGetQuery(con, sql)) > 0L
}

fd_upsert_execute_chunks <- function(
    con,
    dialect,
    target_q,
    key,
    features_df,
    update_table,
    chunk_size,
    verbose,
    return_report
) {
  chunks <- make_chunks(nrow(features_df), chunk_size)
  n_chunks <- length(chunks)

  totals_would_insert <- 0L
  totals_would_update <- 0L
  chunk_details <- if (isTRUE(return_report)) vector("list", n_chunks) else NULL

  for (i in seq_len(n_chunks)) {
    chunk_result <- fd_upsert_apply_chunk(
      con = con,
      dialect = dialect,
      target_q = target_q,
      key = key,
      chunk_df = features_df[chunks[[i]], , drop = FALSE],
      chunk_id = i,
      update_table = update_table,
      verbose = verbose,
      return_report = return_report
    )

    if (isTRUE(return_report)) {
      totals_would_insert <- totals_would_insert + chunk_result$would_insert
      totals_would_update <- totals_would_update + chunk_result$would_update

      chunk_details[[i]] <- data.frame(
        chunk = i,
        n = chunk_result$n,
        would_insert = chunk_result$would_insert,
        would_update = chunk_result$would_update,
        stringsAsFactors = FALSE
      )
    }
  }

  list(
    n_chunks = n_chunks,
    totals_would_insert = totals_would_insert,
    totals_would_update = totals_would_update,
    chunk_details = if (isTRUE(return_report)) do.call(rbind, chunk_details) else NULL
  )
}

fd_upsert_apply_chunk <- function(
    con,
    dialect,
    target_q,
    key,
    chunk_df,
    chunk_id,
    update_table,
    verbose,
    return_report
) {
  stage_name <- get_stage_name(chunk_id)
  stage_q <- as.character(DBI::dbQuoteIdentifier(con, stage_name))
  key_q <- as.character(DBI::dbQuoteIdentifier(con, key))

  on.exit({
    if (DBI::dbExistsTable(con, stage_name)) {
      DBI::dbRemoveTable(con, stage_name)
    }
  }, add = TRUE)

  if (isTRUE(verbose)) {
    message("Chunk ", chunk_id, " (", nrow(chunk_df), " rows): staging -> merge")
  }

  fd_upsert_write_stage_table(
    con = con,
    dialect = dialect,
    stage_name = stage_name,
    chunk_df = chunk_df
  )

  if (!isTRUE(update_table)) {
    conflicts <- find_conflicts(
      con = con,
      dialect = dialect,
      target_q = target_q,
      stage_q = stage_q,
      key_q = key_q,
      limit = 50L
    )

    if (length(conflicts) > 0L) {
      stop(
        sprintf(
          "Conflicts detected (update_table=FALSE). Example key values: %s",
          paste(conflicts, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  would_insert <- 0L
  would_update <- 0L

  if (isTRUE(return_report)) {
    would_insert <- count_scalars(
      con,
      fd_sql_templates$upsert_count_would_insert[[dialect]],
      target_q = target_q,
      stage_q = stage_q,
      key_q = key_q
    )

    would_update <- if (isTRUE(update_table)) {
      count_scalars(
        con,
        fd_sql_templates$upsert_count_would_update[[dialect]],
        target_q = target_q,
        stage_q = stage_q,
        key_q = key_q
      )
    } else {
      0L
    }
  }

  DBI::dbExecute(
    con,
    fd_upsert_build_merge_sql(
      con = con,
      dialect = dialect,
      target_q = target_q,
      stage_q = stage_q,
      key = key,
      cols = names(chunk_df),
      update_table = update_table
    )
  )

  if (isTRUE(return_report) && isTRUE(verbose)) {
    if (isTRUE(update_table)) {
      message("  would_insert=", would_insert, ", would_update=", would_update)
    } else {
      message("  would_insert=", would_insert, " (insert-only; conflicts would error)")
    }
  }

  list(
    n = nrow(chunk_df),
    would_insert = as.integer(would_insert),
    would_update = as.integer(would_update)
  )
}

fd_upsert_write_stage_table <- function(con, dialect, stage_name, chunk_df) {
  chunk_df <- as.data.frame(chunk_df)

  if (identical(dialect, "sqlite")) {
    DBI::dbCreateTable(
      conn = con,
      name = stage_name,
      fields = chunk_df,
      temporary = TRUE
    )
    DBI::dbAppendTable(con, stage_name, chunk_df)
    return(invisible(TRUE))
  }

  DBI::dbWriteTable(
    con,
    name = stage_name,
    value = chunk_df,
    overwrite = TRUE,
    append = FALSE,
    temporary = TRUE,
    row.names = FALSE
  )

  invisible(TRUE)
}

fd_upsert_build_merge_sql <- function(
    con,
    dialect,
    target_q,
    stage_q,
    key,
    cols,
    update_table
) {
  key_q <- as.character(DBI::dbQuoteIdentifier(con, key))
  cols_q <- as.character(DBI::dbQuoteIdentifier(con, cols))
  cols_all_q <- paste(cols_q, collapse = ", ")
  cols_select_q <- paste(paste0("s.", cols_q), collapse = ", ")

  feature_cols <- setdiff(cols, key)

  if (length(feature_cols) == 0L) {
    excluded_prefix <- fd_upsert_excluded_prefix(dialect)
    set_clause <- paste0(key_q, " = ", excluded_prefix, key_q)
  } else {
    feature_cols_q <- as.character(DBI::dbQuoteIdentifier(con, feature_cols))
    excluded_prefix <- fd_upsert_excluded_prefix(dialect)
    set_clause <- paste(paste0(feature_cols_q, " = ", excluded_prefix, feature_cols_q), collapse = ", ")
  }

  template_name <- if (isTRUE(update_table)) {
    "upsert_merge_upsert"
  } else {
    "upsert_merge_insert_only"
  }

  render_sql(
    fd_sql_templates[[template_name]][[dialect]],
    list(
      target = target_q,
      stage = stage_q,
      cols_all = cols_all_q,
      cols_insert = cols_all_q,
      cols_select = cols_select_q,
      key = key_q,
      set_clause = set_clause,
      t_key = paste0("t.", key_q),
      s_key = paste0("s.", key_q)
    )
  )
}

fd_upsert_excluded_prefix <- function(dialect) {
  switch(
    dialect,
    postgres = "EXCLUDED.",
    sqlite = "excluded.",
    mysql = "fd_src.",
    stop("Unsupported/unknown dialect: ", dialect, call. = FALSE)
  )
}

fd_upsert_empty_report <- function(
    feat_table_name,
    key,
    dialect,
    table_created,
    columns_added,
    extra_columns = character()
) {
  structure(
    list(
      feat_table_name = feat_table_name,
      key = key,
      dialect = dialect,
      n_rows = 0L,
      n_chunks = 0L,
      table_created = table_created,
      columns_added = columns_added,
      extra_columns = extra_columns,
      counts = list(
        would_insert = 0L,
        would_update = 0L
      ),
      chunk_details = data.frame(
        chunk = integer(),
        n = integer(),
        would_insert = integer(),
        would_update = integer()
      )
    ),
    class = "fd_upsert_report"
  )
}

fd_upsert_build_report <- function(
    feat_table_name,
    key,
    dialect,
    n_rows,
    n_chunks,
    table_created,
    columns_added,
    extra_columns = character(),
    totals_would_insert,
    totals_would_update,
    chunk_details
) {
  structure(
    list(
      feat_table_name = feat_table_name,
      key = key,
      dialect = dialect,
      n_rows = as.integer(n_rows),
      n_chunks = as.integer(n_chunks),
      table_created = table_created,
      columns_added = columns_added,
      extra_columns = extra_columns,
      counts = list(
        would_insert = as.integer(totals_would_insert),
        would_update = as.integer(totals_would_update)
      ),
      chunk_details = chunk_details
    ),
    class = "fd_upsert_report"
  )
}


#' Normalize feature data types before DB write
#'
#' Performs minimal type normalization to improve DBI write compatibility.
#' Currently converts factor columns to character. Leaves Date/POSIXct unchanged.
#'
#' @keywords internal
normalize_features_df <- function(df) {
  for (nm in names(df)) {
    if (is.factor(df[[nm]])) df[[nm]] <- as.character(df[[nm]])
    # keep Date/POSIXct as-is (DBI drivers decide mapping)
  }
  df
}

#' Generate a unique staging table name
#'
#' Creates a unique, dialect-safe staging table name for use inside a transaction/chunk.
#' Names use only `[A-Za-z0-9_]` to avoid quoting issues across DBs.
#'
#' @keywords internal
get_stage_name <- function(i) {
  # Only [A-Za-z0-9_] to avoid quoting surprises across dialects.
  rnd <- paste(sample(c(letters, LETTERS, 0:9), 8, replace = TRUE), collapse = "")
  paste0("fd_stage_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", i, "_", rnd)
}

#' Split row indices into chunks
#'
#' Returns a list of integer index vectors partitioning `1:n` into chunks of size
#' `chunk_size`. If `chunk_size` is NULL, returns a single chunk containing all rows.
#'
#' @keywords internal
make_chunks <- function(n, chunk_size) {
  if (n == 0L) return(list(integer(0)))
  if (is.null(chunk_size)) return(list(seq_len(n)))
  chunk_size <- as.integer(chunk_size)
  split(seq_len(n), ceiling(seq_len(n) / chunk_size))
}


create_features_table <- function(con, dialect, feat_table_name, target_q, features_df, key) {
  cols <- names(features_df)

  # Column definitions using dbDataType
  col_defs <- vapply(cols, function(nm) {
    type <- DBI::dbDataType(con, features_df[[nm]])
    col_q <- as.character(DBI::dbQuoteIdentifier(con, nm))
    paste(col_q, type)
  }, character(1))

  key_q <- as.character(DBI::dbQuoteIdentifier(con, key))

  sql <- paste0(
    "CREATE TABLE ", target_q, " (",
    paste(col_defs, collapse = ", "),
    ", PRIMARY KEY (", key_q, ")",
    ")"
  )

  DBI::dbExecute(con, sql)
  invisible(TRUE)
}

ensure_columns <- function(
    con,
    dialect,
    table_id,
    table_q,
    features_df,
    key,
    alter_table,
    verbose
) {
  existing <- DBI::dbListFields(con, table_id)
  extra_columns <- setdiff(existing, names(features_df))
  extra_columns <- setdiff(extra_columns, key)

  if (!key %in% existing) {
    stop(
      sprintf(
        "Target table %s does not contain key column %s.",
        table_q,
        shQuote(key)
      ),
      call. = FALSE
    )
  }

  missing <- setdiff(names(features_df), existing)
  if (length(missing) == 0L) {
    out <- character()
    if (length(extra_columns) > 0L) {
      attr(out, "extra_columns") <- extra_columns
    }
    return(out)
  }

  if (!isTRUE(alter_table)) {
    stop(
      sprintf(
        "Target table %s is missing columns: %s. Set alter_table=TRUE to add them.",
        table_q,
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (isTRUE(verbose)) {
    message("Altering table to add missing columns: ", paste(missing, collapse = ", "))
  }

  added <- character()
  for (nm in missing) {
    if (identical(nm, key)) {
      stop(
        "Refusing to add key column via alter_table; key must already exist in target.",
        call. = FALSE
      )
    }

    col_q <- as.character(DBI::dbQuoteIdentifier(con, nm))
    type  <- DBI::dbDataType(con, features_df[[nm]])

    sql <- render_sql(
      fd_sql_templates$alter_add_column[[dialect]],
      list(
        target = table_q,
        col = col_q,
        type = as.character(type)
      )
    )

    DBI::dbExecute(con, sql)
    added <- c(added, nm)
  }

  if (length(extra_columns) > 0L) {
    attr(added, "extra_columns") <- extra_columns
  }
  added
}

count_scalars <- function(con, template, target_q, stage_q, key_q) {
  t_key <- paste0("t.", key_q)
  s_key <- paste0("s.", key_q)
  sql <- render_sql(
    template,
    list(target = target_q, stage = stage_q, t_key = t_key, s_key = s_key)
  )
  res <- DBI::dbGetQuery(con, sql)
  as.integer(res[[1]][[1]])
}

find_conflicts <- function(con, dialect, target_q, stage_q, key_q, limit = 50L) {
  t_key <- paste0("t.", key_q)
  s_key <- paste0("s.", key_q)

  sql <- render_sql(
    fd_sql_templates$upsert_find_conflicts[[dialect]],
    list(
      target = target_q,
      stage  = stage_q,
      t_key  = t_key,
      s_key  = s_key,
      limit  = as.character(as.integer(limit))
    )
  )
  out <- DBI::dbGetQuery(con, sql)
  if (nrow(out) == 0L) return(character())
  as.character(out[[1]])
}
