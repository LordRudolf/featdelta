# Compute layer
#
# This file contains in-memory feature computation. The functions here do not
# know anything about database context objects and should not call resolve_ctx().
#
# Rule of thumb:
# - fd_define() creates and normalizes a defs;
# - fd_compute() evaluates that defs against an in-memory data frame;
# - DB-facing functions such as fd_fetch()/fd_upsert()/fd_run() belong to the
#   context/database layer, not here.



#' Compute features from featdelta definitions on in-memory data
#'
#' Evaluates a `featdelta_defs` object created by `fd_define()` against an
#' in-memory data frame/tibble and returns a feature data frame containing the
#' key column plus computed feature columns.
#'
#' `fd_compute()` is the package's pure in-memory execution step. It does not
#' access the database and does not use the featdelta context system.
#'
#' Definition steps are evaluated sequentially in the order stored in `defs`.
#' This means later steps may depend on columns created by earlier steps in the
#' same compute call.
#'
#' Guardrails implemented by `fd_compute()` are compute-specific:
#' \itemize{
#'   \item validate the structure of the supplied defs object
#'   \item validate result type and row alignment for each step
#'   \item preserve 1:1 row alignment with the input data
#'   \item reject output-name collisions with `key` and previously produced names
#'   \item attach a per-step computation report
#' }
#'
#' Guardrails that belong at definition time, such as malformed construction of
#' individual definition steps, should be handled by `fd_define()`.
#'
#' @param data A data frame/tibble containing the raw input variables used to
#'   compute features.
#' @param defs A `featdelta_defs` object created by `fd_define()`.
#' @param key Character scalar naming the primary key column in `data`.
#' @param compute_envir Optional environment. If supplied, it overrides the
#'   stored evaluation environment for expression-based steps.
#' @param compute_strict Logical. If `TRUE`, any failed step causes the overall
#'   compute call to error after the per-step report is assembled. If `FALSE`,
#'   failed column steps return `NA` columns, and failed block steps return
#'   `NA` columns only when `output_names` are known in advance.
#' @param verbose Logical. If `TRUE`, print progress messages and a short
#'   summary.
#' @param return_report Logical. If `TRUE`, return a list with elements
#'   `data` and `report`. If `FALSE`, return only the computed data frame and
#'   attach the report as `attr(x, "fd_report")`.
#'
#' @return Either:
#'   \itemize{
#'     \item a data frame with class `"featdelta_features"` and attached report
#'           attribute, or
#'     \item a list `{data, report}` with class
#'           `"featdelta_compute_result"` when `return_report = TRUE`
#'   }
#'
#' The output preserves row order and row count relative to `data`.
#'
#' @details
#' Supported definition step types are:
#' \itemize{
#'   \item ordinary single-column steps
#'   \item `fd_block()` multi-column steps
#' }
#'
#' For single-column steps, the result must be a supported vector-like output.
#'
#' For block steps:
#' \itemize{
#'   \item the result must be a `data.frame`
#'   \item the number of rows must match `nrow(data)`
#'   \item returned column names must be non-empty and unique
#'   \item returned names must not collide with `key`
#'   \item returned names must not collide with feature names already produced
#'         earlier in the same compute run
#' }
#'
#' If a block declares `expected_names`, then:
#' \itemize{
#'   \item returned names must be a subset of `expected_names`
#'   \item missing expected names are added as `NA`
#'   \item final output order follows `expected_names`
#' }
#'
#' Because definitions are evaluated sequentially, later steps may use:
#' \itemize{
#'   \item raw input columns from `data`
#'   \item columns produced by earlier single-column steps
#'   \item columns produced by earlier block steps
#' }
#'
#' Reordering or removing earlier steps may therefore break downstream
#' definitions. When evaluation fails with "object not found"-style errors,
#' `fd_compute()` augments the message with likely reasons.
#'
#' @examples
#' raw_df <- mtcars
#' raw_df$car_id <- seq_len(nrow(raw_df))
#'
#' defs <- fd_define(
#'   hp_per_cyl = hp / cyl,
#'   engine_ratios = fd_block({
#'     data.frame(
#'       disp_per_cyl = disp / cyl,
#'       wt_per_hp = wt / hp
#'     )
#'   }),
#'   double_ratio = disp_per_cyl * 2
#' )
#'
#' out <- fd_compute(
#'   data = raw_df,
#'   defs = defs,
#'   key = "car_id",
#'   compute_strict = TRUE
#' )
#'
#' head(out)
#'
#' @family featdelta compute helpers
#' @export
fd_compute <- function(data,
                       defs,
                       key,
                       compute_envir = NULL,
                       compute_strict = TRUE,
                       verbose = FALSE,
                       return_report = FALSE) {

  ## validation of inputs
  validate_general_args(
    data = data,
    key = key,
    defs = defs,
    compute_envir = compute_envir,

    logicals = list(
      compute_strict = compute_strict,
      verbose = verbose,
      return_report = return_report
    )
  )

  fd_compute_validate_defs_structure(defs, key)


  ##
  steps <- defs$steps
  n <- nrow(data)
  if (verbose) {
    message("fd_compute(): n = ", n, ", steps = ", length(steps))
  }

  # working data is what function-blocks receive
  working_data <- data

  # Evaluation data contains raw columns plus features produced so far.
  # Keep this as a data frame/list, not an environment, because passing a plain
  # environment as `data` to rlang::eval_tidy() is deprecated.
  mask <- working_data


  # evaluate features sequentially
  produced_cols <- list()
  produced_names <- character(0)
  report <- vector("list", length(steps))

  step_names <- names(steps)

  for (i in seq_along(steps)) {
    step <- steps[[i]]
    step_name_i <- step_names[[i]]

    t0 <- Sys.time()

    step_result <- fd_compute_eval_step(
      step = step,
      step_name = step_name_i,
      working_data = working_data,
      mask = mask,
      n = n,
      key = key,
      produced_names = produced_names,
      compute_envir = compute_envir,
      compute_strict = compute_strict
    )

    t1 <- Sys.time()

    # In non-strict mode failed steps may provide NA fallback columns.
    if ((step_result$ok || !isTRUE(compute_strict)) && length(step_result$cols)) {
      new_names <- names(step_result$cols)

      for (j in seq_along(step_result$cols)) {
        nm_j <- new_names[[j]]
        col_j <- step_result$cols[[j]]

        produced_cols[[nm_j]] <- col_j
        produced_names <- c(produced_names, nm_j)
        working_data[[nm_j]] <- col_j
        mask[[nm_j]] <- col_j
      }
    }

    report[[i]] <- list(
      step_name = step_name_i,
      type = step$type %||% NA_character_,
      mode = step$mode %||% NA_character_,
      ok = step_result$ok,
      error = if (step_result$ok) NA_character_ else step_result$error,
      error_raw = if (step_result$ok) NA_character_ else step_result$error_raw,
      error_class = if (step_result$ok) NA_character_ else step_result$error_class,
      hint = if (step_result$ok) NA_character_ else step_result$hint,
      time_sec = as.numeric(difftime(t1, t0, units = "secs")),
      text = step$text %||% NA_character_,
      output_names = paste(names(step_result$cols), collapse = ", ")
    )

    if (verbose) {
      message(sprintf("  [%s] %s", if (step_result$ok) "OK" else "FAIL", step_name_i))
    }
  }

  report_df <- data.frame(
    step_name = vapply(report, `[[`, character(1), "step_name"),
    type = vapply(report, `[[`, character(1), "type"),
    mode = vapply(report, `[[`, character(1), "mode"),
    ok = vapply(report, `[[`, logical(1), "ok"),
    error = vapply(report, `[[`, character(1), "error"),
    error_raw = vapply(report, `[[`, character(1), "error_raw"),
    error_class = vapply(report, `[[`, character(1), "error_class"),
    hint = vapply(report, `[[`, character(1), "hint"),
    time_sec = vapply(report, `[[`, numeric(1), "time_sec"),
    text = vapply(report, `[[`, character(1), "text"),
    output_names = vapply(report, `[[`, character(1), "output_names"),
    stringsAsFactors = FALSE
  )

  any_fail <- any(!report_df$ok)
  if (verbose) {
    message("fd_compute(): success = ", !any_fail)
  }

  out <- data.frame(
    c(stats::setNames(list(data[[key]]), key), produced_cols),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  if (nrow(out) != n) {
    stop(
      "Internal fd_compute error: output row count does not match input row count.",
      call. = FALSE
    )
  }

  if (length(produced_names)) {
    expected_names <- c(key, produced_names)
  } else {
    expected_names <- key
  }

  if (!identical(names(out), expected_names)) {
    stop(
      "Internal fd_compute error: output column names do not match expected names.",
      call. = FALSE
    )
  }

  attr(out, "fd_report") <- report_df
  attr(out, "fd_key") <- key
  class(out) <- c("featdelta_features", class(out))

  if (any_fail && isTRUE(compute_strict)) {
    e <- simpleError("fd_compute(): one or more definition steps failed (see report).")
    attr(e, "fd_report") <- report_df
    attr(e, "fd_partial") <- out
    class(e) <- c("featdelta_compute_error", class(e))
    stop(e)
  }

  if (isTRUE(return_report)) {
    res <- list(data = out, report = report_df)
    class(res) <- c("featdelta_compute_result", "list")
    return(res)
  }

  out
}



#' Validate the internal structure of a featdelta defs for computation
#'
#' Defensive internal helper used by `fd_compute()` to verify that the supplied
#' defs object is structurally sound before evaluation begins.
#'
#' This validation is intentionally compute-focused. It checks the shape and
#' consistency of `defs$features`, including:
#' \itemize{
#'   \item non-empty named feature list
#'   \item unique feature names
#'   \item no feature name collision with `key`
#'   \item presence and basic validity of stored fields such as `quo`, `expr`,
#'         `env`, and `text`
#' }
#'
#' Although `fd_define()` should normally construct a valid defs, this helper
#' protects `fd_compute()` against manually modified or malformed defs objects.
#'
#' @param defs A `featdelta_defs`.
#' @param key Character scalar naming the key column used for the current
#'   compute call.
#'
#' @return Invisibly returns `TRUE` on success. Errors on malformed defs.
#'
#' @family featdelta compute helpers
#' @keywords internal
fd_compute_validate_defs_structure <- function(defs, key = NULL) {
  steps <- defs$steps

  if (!is.list(steps) || !length(steps)) {
    stop("`defs$steps` must be a non-empty list.", call. = FALSE)
  }

  nm <- names(steps)
  if (is.null(nm) || any(is.na(nm)) || any(!nzchar(nm))) {
    stop("`defs$steps` must be a named list.", call. = FALSE)
  }

  if (anyDuplicated(nm)) {
    stop("`defs$steps` contains duplicate step names.", call. = FALSE)
  }

  for (i in seq_along(steps)) {
    step <- steps[[i]]
    step_name_i <- nm[[i]]

    if (!is.list(step)) {
      stop(
        "Definition step `", step_name_i, "` must be stored as a list.",
        call. = FALSE
      )
    }

    if (!is.character(step$type) || length(step$type) != 1L || is.na(step$type)) {
      stop(
        "Definition step `", step_name_i, "` has malformed `type`.",
        call. = FALSE
      )
    }

    if (!is.character(step$step_name) || length(step$step_name) != 1L || is.na(step$step_name)) {
      stop(
        "Definition step `", step_name_i, "` has malformed `step_name`.",
        call. = FALSE
      )
    }

    if (!identical(step$step_name, step_name_i)) {
      stop(
        "Definition step `", step_name_i, "` has inconsistent `step_name`.",
        call. = FALSE
      )
    }

    if (!is.null(step$output_names)) {
      if (!is.character(step$output_names) ||
          any(is.na(step$output_names)) ||
          any(!nzchar(step$output_names))) {
        stop(
          "Definition step `", step_name_i, "` has malformed `output_names`.",
          call. = FALSE
        )
      }

      if (anyDuplicated(step$output_names)) {
        stop(
          "Definition step `", step_name_i, "` has duplicate `output_names`.",
          call. = FALSE
        )
      }

      if (!is.null(key) && any(step$output_names %in% key)) {
        stop(
          "Definition step `", step_name_i,
          "` declares output name(s) that collide with `key`.",
          call. = FALSE
        )
      }
    }

    if (!is.environment(step$env)) {
      stop(
        "Definition step `", step_name_i, "` has malformed `env`.",
        call. = FALSE
      )
    }

    if (!is.character(step$text) || length(step$text) != 1L || is.na(step$text)) {
      stop(
        "Definition step `", step_name_i, "` has malformed `text`.",
        call. = FALSE
      )
    }

    if (identical(step$type, "column")) {
      if (!rlang::is_quosure(step$quo)) {
        stop(
          "Column step `", step_name_i, "` has malformed `quo`.",
          call. = FALSE
        )
      }

      if (!identical(step$output_names, step_name_i)) {
        stop(
          "Column step `", step_name_i,
          "` must have exactly one output name equal to the step name.",
          call. = FALSE
        )
      }

    } else if (identical(step$type, "block")) {
      if (!is.character(step$mode) || length(step$mode) != 1L || is.na(step$mode)) {
        stop(
          "Block step `", step_name_i, "` has malformed `mode`.",
          call. = FALSE
        )
      }

      if (identical(step$mode, "function")) {
        if (!is.function(step$fn)) {
          stop(
            "Block step `", step_name_i, "` has malformed `fn`.",
            call. = FALSE
          )
        }
      } else if (identical(step$mode, "expression")) {
        if (!rlang::is_quosure(step$quo)) {
          stop(
            "Block step `", step_name_i, "` has malformed `quo`.",
            call. = FALSE
          )
        }
      } else {
        stop(
          "Block step `", step_name_i, "` has unsupported `mode`: ",
          step$mode,
          call. = FALSE
        )
      }

    } else {
      stop(
        "Definition step `", step_name_i, "` has unsupported `type`: ",
        step$type,
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}


#' Evaluate one definition step
#'
#' Internal dispatcher used by `fd_compute()`.
#'
#' @param step Normalized definition step.
#' @param step_name Step name.
#' @param working_data Current working data frame.
#' @param mask Current evaluation mask.
#' @param n Expected row count.
#' @param key Key column name.
#' @param produced_names Names already produced in prior steps.
#' @param compute_envir Optional overriding environment.
#' @param compute_strict Logical strictness flag.
#'
#' @return A list with elements `ok`, `error`, and `cols`.
#'
#' @keywords internal
fd_compute_eval_step <- function(step,
                                 step_name,
                                 working_data,
                                 mask,
                                 n,
                                 key,
                                 produced_names,
                                 compute_envir = NULL,
                                 compute_strict = TRUE) {

  if (identical(step$type, "column")) {
    return(
      fd_compute_eval_column_step(
        step = step,
        step_name = step_name,
        mask = mask,
        n = n,
        compute_envir = compute_envir,
        compute_strict = compute_strict
      )
    )
  }

  if (identical(step$type, "block")) {
    return(
      fd_compute_eval_block_step(
        step = step,
        step_name = step_name,
        working_data = working_data,
        mask = mask,
        n = n,
        key = key,
        produced_names = produced_names,
        compute_envir = compute_envir,
        compute_strict = compute_strict
      )
    )
  }

  list(
    ok = FALSE,
    error = paste0("Unsupported step type: ", step$type),
    cols = list()
  )
}


#' Evaluate one column step
#'
#' @param step Column step.
#' @param step_name Step name.
#' @param mask Current evaluation mask.
#' @param n Expected row count.
#' @param compute_envir Optional overriding environment.
#' @param compute_strict Logical strictness flag.
#'
#' @return A list with elements `ok`, `error`, and `cols`.
#'
#' @keywords internal
fd_compute_eval_column_step <- function(step,
                                        step_name,
                                        mask,
                                        n,
                                        compute_envir = NULL,
                                        compute_strict = TRUE) {

  expr_i <- step$expr %||% rlang::get_expr(step$quo)
  env_i  <- compute_envir %||% step$env %||% rlang::get_env(step$quo)

  ok <- TRUE
  diag <- NULL
  res <- NULL

  res <- tryCatch(
    rlang::eval_tidy(expr_i, data = mask, env = env_i),
    error = function(e) {
      ok <<- FALSE
      diag <<- fd_compute_diagnose_error(e)
      NULL
    }
  )

  if (ok) {
    validation <- fd_compute_validate_column_result(res, n = n, name = step_name)
    ok <- validation$ok

    if (!ok) {
      diag <- list(
        class = "invalid_result",
        raw_message = validation$error,
        message = validation$error,
        hint = NA_character_
      )
    }

    res <- validation$result
  }

  if (!ok) {
    msg <- fd_compute_format_diagnostic_message(diag)
    cols <- stats::setNames(list(rep(NA, n)), step_name)

    if (!isTRUE(compute_strict)) {
      warning(sprintf("fd_compute(): step '%s' failed: %s", step_name, msg), call. = FALSE)
    }

    return(list(
      ok = FALSE,
      error = msg,
      error_raw = diag$raw_message %||% msg,
      error_class = diag$class %||% "other_error",
      hint = diag$hint %||% NA_character_,
      cols = cols
    ))
  }

  list(
    ok = TRUE,
    error = NA_character_,
    error_raw = NA_character_,
    error_class = NA_character_,
    hint = NA_character_,
    cols = stats::setNames(list(res), step_name)
  )
}


#' Evaluate one block step
#'
#' @param step Block step.
#' @param step_name Step name.
#' @param working_data Current working data frame.
#' @param mask Current evaluation mask.
#' @param n Expected row count.
#' @param key Key column name.
#' @param produced_names Names already produced in prior steps.
#' @param compute_envir Optional overriding environment.
#' @param compute_strict Logical strictness flag.
#'
#' @return A list with elements `ok`, `error`, and `cols`.
#'
#' @keywords internal
fd_compute_eval_block_step <- function(step,
                                       step_name,
                                       working_data,
                                       mask,
                                       n,
                                       key,
                                       produced_names,
                                       compute_envir = NULL,
                                       compute_strict = TRUE) {

  ok <- TRUE
  diag <- NULL
  res <- NULL

  if (identical(step$mode, "function")) {
    res <- tryCatch(
      fd_compute_call_block_function(step$fn, working_data),
      error = function(e) {
        ok <<- FALSE
        diag <<- fd_compute_diagnose_error(e)
        NULL
      }
    )

  } else if (identical(step$mode, "expression")) {
    expr_i <- step$expr %||% rlang::get_expr(step$quo)
    env_i  <- compute_envir %||% step$env %||% rlang::get_env(step$quo)

    res <- tryCatch(
      rlang::eval_tidy(expr_i, data = mask, env = env_i),
      error = function(e) {
        ok <<- FALSE
        diag <<- fd_compute_diagnose_error(e)
        NULL
      }
    )

  } else {
    ok <- FALSE
    diag <- list(
      class = "unsupported_mode",
      raw_message = paste0("Unsupported block mode: ", step$mode),
      message = paste0("Unsupported block mode: ", step$mode),
      hint = NA_character_
    )
  }

  if (ok) {
    validation <- fd_compute_validate_block_result(
      res = res,
      n = n,
      step_name = step_name,
      key = key,
      produced_names = produced_names,
      expected_names = step$output_names %||% NULL
    )
    ok <- validation$ok

    if (!ok) {
      diag <- list(
        class = "invalid_block_result",
        raw_message = validation$error,
        message = validation$error,
        hint = NA_character_
      )
    }

    res <- validation$result
  }

  if (!ok) {
    msg <- fd_compute_format_diagnostic_message(diag)
    fallback_cols <- list()

    if (length(step$output_names)) {
      fallback_cols <- stats::setNames(
        replicate(length(step$output_names), rep(NA, n), simplify = FALSE),
        step$output_names
      )
    }

    if (!isTRUE(compute_strict)) {
      warning(sprintf("fd_compute(): step '%s' failed: %s", step_name, msg), call. = FALSE)
    }

    return(list(
      ok = FALSE,
      error = msg,
      error_raw = diag$raw_message %||% msg,
      error_class = diag$class %||% "other_error",
      hint = diag$hint %||% NA_character_,
      cols = fallback_cols
    ))
  }

  cols <- as.list(res)
  names(cols) <- names(res)

  list(
    ok = TRUE,
    error = NA_character_,
    error_raw = NA_character_,
    error_class = NA_character_,
    hint = NA_character_,
    cols = cols
  )
}

#' Validate one computed column result
#'
#' @param x Computed result.
#' @param n Expected row count.
#' @param name Output name used in error messages.
#'
#' @return A list with elements `ok`, `error`, and `result`.
#'
#' @keywords internal
fd_compute_validate_column_result <- function(x, n, name) {
  if (is.null(x)) {
    return(list(
      ok = FALSE,
      error = paste0("Result for `", name, "` is NULL; expected a vector."),
      result = NULL
    ))
  }

  if (!fd_compute_is_supported_result(x)) {
    return(list(
      ok = FALSE,
      error = paste(
        "Result for `", name, "` has unsupported type/class:",
        paste(class(x), collapse = "/"),
        "- expected an atomic/Date/POSIXct/difftime vector."
      ),
      result = NULL
    ))
  }

  if (n == 0L) {
    if (length(x) %in% c(0L, 1L)) {
      return(list(ok = TRUE, error = NA_character_, result = x[0]))
    }

    return(list(
      ok = FALSE,
      error = sprintf(
        "Result for `%s` has length %s; expected 0 (or scalar recyclable to 0-row output).",
        name, length(x)
      ),
      result = NULL
    ))
  }

  if (length(x) == 1L) {
    return(list(ok = TRUE, error = NA_character_, result = rep(x, n)))
  }

  if (length(x) != n) {
    return(list(
      ok = FALSE,
      error = sprintf(
        "Result for `%s` has length %s; expected %s (or 1).",
        name, length(x), n
      ),
      result = NULL
    ))
  }

  list(ok = TRUE, error = NA_character_, result = x)
}


#' Validate one computed block result
#'
#' @param res Computed result.
#' @param n Expected row count.
#' @param step_name Step name used in error messages.
#' @param key Key column name.
#' @param produced_names Names already produced before this block.
#'
#' @return A list with elements `ok`, `error`, and `result`.
#'
#' @keywords internal
fd_compute_validate_block_result <- function(res,
                                             n,
                                             step_name,
                                             key,
                                             produced_names,
                                             expected_names = NULL) {

  if (is.null(res)) {
    return(list(
      ok = FALSE,
      error = paste0("Block step `", step_name, "` returned NULL; expected a data.frame."),
      result = NULL
    ))
  }

  if (!is.data.frame(res)) {
    return(list(
      ok = FALSE,
      error = paste0(
        "Block step `", step_name, "` returned unsupported type/class: ",
        paste(class(res), collapse = "/"),
        ". Expected a data.frame."
      ),
      result = NULL
    ))
  }

  if (nrow(res) != n) {
    return(list(
      ok = FALSE,
      error = sprintf(
        "Block step `%s` returned %s rows; expected %s.",
        step_name, nrow(res), n
      ),
      result = NULL
    ))
  }

  nm <- names(res)
  if (is.null(nm) || any(is.na(nm)) || any(!nzchar(nm))) {
    return(list(
      ok = FALSE,
      error = paste0("Block step `", step_name, "` returned unnamed or empty-named columns."),
      result = NULL
    ))
  }

  if (anyDuplicated(nm)) {
    dups <- unique(nm[duplicated(nm)])
    return(list(
      ok = FALSE,
      error = paste0(
        "Block step `", step_name, "` returned duplicate column names: ",
        paste(dups, collapse = ", ")
      ),
      result = NULL
    ))
  }

  if (key %in% nm) {
    return(list(
      ok = FALSE,
      error = paste0(
        "Block step `", step_name, "` returned column name(s) colliding with `key`: ",
        key
      ),
      result = NULL
    ))
  }

  colliding <- intersect(nm, produced_names)
  if (length(colliding)) {
    return(list(
      ok = FALSE,
      error = paste0(
        "Block step `", step_name, "` returned column name(s) already produced earlier: ",
        paste(colliding, collapse = ", ")
      ),
      result = NULL
    ))
  }

  if (!is.null(expected_names)) {
    unexpected <- setdiff(nm, expected_names)
    if (length(unexpected)) {
      return(list(
        ok = FALSE,
        error = paste0(
          "Block step `", step_name, "` returned unexpected column name(s): ",
          paste(unexpected, collapse = ", "),
          ". Allowed names are: ",
          paste(expected_names, collapse = ", ")
        ),
        result = NULL
      ))
    }
  }

  for (j in seq_along(res)) {
    col_j <- res[[j]]
    nm_j <- nm[[j]]

    validation <- fd_compute_validate_column_result(col_j, n = n, name = nm_j)
    if (!validation$ok) {
      return(list(ok = FALSE, error = validation$error, result = NULL))
    }

    res[[j]] <- validation$result
  }

  if (!is.null(expected_names)) {
    res <- fd_compute_complete_expected_block_outputs(
      res = res,
      expected_names = expected_names,
      n = n
    )

    # after completion, re-check collisions against already produced names
    nm2 <- names(res)

    if (key %in% nm2) {
      return(list(
        ok = FALSE,
        error = paste0(
          "Block step `", step_name, "` would produce column name(s) colliding with `key`: ",
          key
        ),
        result = NULL
      ))
    }

    colliding2 <- intersect(nm2, produced_names)
    if (length(colliding2)) {
      return(list(
        ok = FALSE,
        error = paste0(
          "Block step `", step_name, "` would produce column name(s) already produced earlier: ",
          paste(colliding2, collapse = ", ")
        ),
        result = NULL
      ))
    }
  }

  list(ok = TRUE, error = NA_character_, result = res)
}



#' Complete missing expected outputs from a block with `NA`
#'
#' Internal helper used by `fd_compute()` for block steps that declare
#' `expected_names`.
#'
#' When a block returns only a subset of its declared outputs, this helper adds
#' the missing expected columns as `NA` vectors and then reorders the result to
#' match the declared `expected_names`.
#'
#' This implements featdelta's lenient block-output completion policy:
#' \itemize{
#'   \item returned block columns must be a subset of `expected_names`
#'   \item any missing expected columns are added as `NA`
#'   \item final output order follows `expected_names`
#' }
#'
#' The helper does not validate whether unexpected columns are present; that
#' check belongs earlier in `fd_compute_validate_block_result()`.
#'
#' @param res A data frame returned by a block step.
#' @param expected_names Character vector of declared expected output names.
#' @param n Expected row count.
#'
#' @return A data frame containing exactly the columns in `expected_names`,
#'   ordered accordingly, with missing expected columns filled by `NA`.
#'
#' @family featdelta compute helpers
#' @keywords internal
fd_compute_complete_expected_block_outputs <- function(res, expected_names, n) {
  if (is.null(expected_names)) {
    return(res)
  }

  missing_names <- setdiff(expected_names, names(res))

  if (length(missing_names)) {
    for (nm in missing_names) {
      res[[nm]] <- rep(NA, n)
    }
  }

  res <- res[, expected_names, drop = FALSE]
  res
}


#' Call a block function against the current working data
#'
#' @param fn Function stored in a function-mode block.
#' @param working_data Current working data.
#'
#' @return Block result.
#'
#' @keywords internal
fd_compute_call_block_function <- function(fn, working_data) {
  fmls <- formals(fn)

  if (length(fmls) == 0L) {
    return(fn())
  }

  fn(working_data)
}


#' Diagnose an evaluation error from a compute step
#'
#' Internal helper used by `fd_compute()` to convert a raw evaluation error into
#' a structured diagnostic record that is more useful for users and for the
#' computation report.
#'
#' The function is intentionally lightweight. It does not attempt full static
#' dependency analysis. Instead, it classifies a few high-value failure patterns
#' and adds a concise hint when possible.
#'
#' In particular, "object not found" style errors are expanded with likely
#' causes relevant to featdelta's ordered step semantics:
#' \itemize{
#'   \item the raw input column is missing
#'   \item an earlier definition step was removed
#'   \item definition steps were reordered
#'   \item an earlier block no longer returns the expected column
#' }
#'
#' @param err An error object or error message.
#'
#' @return A list with fields:
#'   \itemize{
#'     \item `class`: diagnostic class label
#'     \item `raw_message`: original error message
#'     \item `message`: enriched user-facing message
#'     \item `hint`: short additional hint or `NA_character_`
#'   }
#'
#' @family featdelta compute helpers
#' @keywords internal
fd_compute_diagnose_error <- function(err) {
  raw_msg <- if (inherits(err, "error")) {
    conditionMessage(err)
  } else {
    as.character(err)[1]
  }

  if (!is.character(raw_msg) || length(raw_msg) != 1L || is.na(raw_msg)) {
    raw_msg <- "Unknown compute error."
  }

  if (grepl("object .* not found", raw_msg)) {
    hint <- paste(
      "Possible reasons:",
      "the raw input column is missing;",
      "an earlier definition step was removed;",
      "the order of definition steps was changed;",
      "or an earlier block no longer returns the expected column."
    )

    return(list(
      class = "missing_object",
      raw_message = raw_msg,
      message = paste(raw_msg, hint),
      hint = hint
    ))
  }

  if (grepl("could not find function", raw_msg, fixed = TRUE)) {
    hint <- paste(
      "Possible reasons:",
      "the function is not available in the stored evaluation environment;",
      "the compute environment was changed;",
      "or a required package/function was not attached or referenced explicitly."
    )

    return(list(
      class = "missing_function",
      raw_message = raw_msg,
      message = paste(raw_msg, hint),
      hint = hint
    ))
  }

  if (grepl("argument .* is missing", raw_msg)) {
    hint <- paste(
      "Possible reasons:",
      "a helper function inside the definition now requires different arguments;",
      "or a function-based block no longer matches the expected calling pattern."
    )

    return(list(
      class = "missing_argument",
      raw_message = raw_msg,
      message = paste(raw_msg, hint),
      hint = hint
    ))
  }

  list(
    class = "other_error",
    raw_message = raw_msg,
    message = raw_msg,
    hint = NA_character_
  )
}


#' Format a step failure for warnings and reports
#'
#' @param diag Diagnostic list from `fd_compute_diagnose_error()`.
#'
#' @return Character scalar.
#'
#' @family featdelta compute helpers
#' @keywords internal
fd_compute_format_diagnostic_message <- function(diag) {
  if (!is.list(diag) || is.null(diag$message)) {
    return("Unknown compute error.")
  }
  diag$message
}



#' Check whether a computed feature result is supported for column output
#'
#' Internal helper used by `fd_compute()` to reject unsupported evaluation
#' results before they propagate into the final feature frame.
#'
#' Supported result classes are vector-like column outputs suitable for
#' inclusion in a data frame. Tabular or list-like results should be rejected
#' here rather than failing later during DB write or report construction.
#'
#' @param x A single feature evaluation result.
#'
#' @return Logical scalar. `TRUE` if `x` is a supported result type for
#'   `fd_compute()`, otherwise `FALSE`.
#'
#' @family featdelta compute helpers
#' @keywords internal
fd_compute_is_supported_result <- function(x) {
  is.atomic(x) ||
    inherits(x, "Date") ||
    inherits(x, "POSIXct") ||
    inherits(x, "difftime")
}
