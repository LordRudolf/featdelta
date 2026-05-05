# Definition-definition layer
#
# This file defines the user-facing definitions object.
# fd_define() is responsible for definition-level normalization and guardrails.
#
# Rule of thumb:
# - fd_define() rejects malformed definitions and invalid step/output names;
# - fd_compute() evaluates a valid defs object against data;
# - fd_define() does not know the future key column or row count and should
#   not try to enforce compute-time invariants.



#' Define featdelta feature definitions
#'
#' Creates a `featdelta_defs` object that stores ordered definition steps in a
#' normalized internal representation suitable for later evaluation by
#' `fd_compute()`.
#'
#' `fd_define()` is the definitions-construction step of the package. It does
#' not compute features and does not access the database. Its role is to:
#' \itemize{
#'   \item capture user definitions,
#'   \item normalize supported input styles to a canonical internal structure,
#'   \item enforce definition-level guardrails such as name validity and
#'         duplicate handling,
#'   \item store the resulting ordered definition steps for later execution.
#' }
#'
#' Definitions are evaluated later by `fd_compute()` in the order they are
#' stored. This means later definitions may depend on columns created by
#' earlier steps. Reordering or removing earlier steps may therefore break
#' downstream definitions.
#'
#' @param ... Definitions supplied inline.
#'   Ordinary named inputs such as `new_feature = hp / cyl` are stored as
#'   single-column definition steps.
#' @param defs Optional list of programmatically supplied definitions.
#'   Supported inputs are the same as for `...`.
#' @param description Optional character scalar describing the definitions.
#' @param overwrite Logical. If `FALSE`, duplicate step names are rejected.
#'   If `TRUE`, later steps replace earlier ones.
#' @param envir Optional environment used as the explicit evaluation
#'   environment for normalized definitions. If `NULL`, environments are taken
#'   from the captured inputs.
#'
#' @return A list of class `"featdelta_defs"` containing:
#'   \itemize{
#'     \item `steps`: ordered normalized definition steps
#'     \item `description`: optional description
#'     \item `created_at`: creation time
#'     \item `defs_version`: internal version marker
#'     \item `envir_policy`: whether environments were inherited or explicit
#'     \item `envir`: the explicit environment, if supplied
#'   }
#'
#' @details
#' Supported definition inputs include:
#' \itemize{
#'   \item inline expressions captured from `...`
#'   \item quosures
#'   \item `expression()` objects of length 1
#'   \item quoted calls or names
#'   \item direct scalar constants
#'   \item `fd_block()` objects for multi-column feature generation
#' }
#'
#' Ordinary named expressions define one output column per step.
#'
#' `fd_block()` defines a multi-column step. At compute time, a block is
#' expected to return a `data.frame` with one row per input row. The returned
#' column names become the produced feature names. Optionally, expected output
#' names may be declared in advance via `fd_block(expected_names = ...)`.
#'
#' Direct atomic constants must be scalar. If vectorized behavior is desired,
#' provide an expression instead of embedding a fixed-length atomic vector in
#' the definitions.
#'
#' Duplicate step handling is part of definitions construction:
#' \itemize{
#'   \item with `overwrite = FALSE`, duplicate step names error;
#'   \item with `overwrite = TRUE`, the last definition wins.
#' }
#'
#' @examples
#' # Single-column definitions
#' defs <- fd_define(
#'   hp_per_cyl = hp / cyl,
#'   wt_per_hp = wt / hp
#' )
#'
#' # Definitions can also be supplied programmatically
#' predef <- expression(log(hp))
#' defs <- fd_define(
#'   log_hp = predef
#' )
#'
#' # Multi-column block using an inline expression
#' defs <- fd_define(
#'   engine_ratios = fd_block({
#'     data.frame(
#'       hp_per_cyl = hp / cyl,
#'       disp_per_cyl = disp / cyl
#'     )
#'   })
#' )
#'
#' # Multi-column block with declared expected names
#' defs <- fd_define(
#'   engine_ratios = fd_block(
#'     {
#'       data.frame(
#'         hp_per_cyl = hp / cyl,
#'         disp_per_cyl = disp / cyl
#'       )
#'     },
#'     expected_names = c("hp_per_cyl", "disp_per_cyl")
#'   )
#' )
#'
#' # Function-based block
#' make_engine_features <- function(data) {
#'   data.frame(
#'     hp_per_cyl = data$hp / data$cyl,
#'     disp_per_cyl = data$disp / data$cyl
#'   )
#' }
#'
#' defs <- fd_define(
#'   engine_ratios = fd_block(make_engine_features)
#' )
#'
#' # A block can contain a small feature script with temporary variables
#' defs <- fd_define(
#'   engine_script = fd_block({
#'     hp_per_cyl <- hp / cyl
#'     disp_per_cyl <- disp / cyl
#'
#'     data.frame(
#'       hp_per_cyl = hp_per_cyl,
#'       disp_per_cyl = disp_per_cyl,
#'       engine_index = hp_per_cyl + disp_per_cyl
#'     )
#'   })
#' )
#'
#' # A function-based block can create a variable number of columns in a loop
#' make_scaled_features <- function(data) {
#'   vars <- c("hp", "disp", "wt")
#'   out <- list()
#'
#'   for (var in vars) {
#'     center <- mean(data[[var]], na.rm = TRUE)
#'     spread <- stats::sd(data[[var]], na.rm = TRUE)
#'     out[[paste0(var, "_scaled")]] <- (data[[var]] - center) / spread
#'   }
#'
#'   as.data.frame(out)
#' }
#'
#' defs <- fd_define(
#'   scaled_inputs = fd_block(make_scaled_features)
#' )
#'
#' @family featdelta defs helpers
#' @export
fd_define <- function(...,
                      defs = NULL,
                      description = NULL,
                      overwrite = FALSE,
                      envir = NULL) {

  if (!is.null(description)) {
    if (!is.character(description) || length(description) != 1L) {
      stop("`description` must be a character scalar or NULL.")
    }
  }

  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("`overwrite` must be TRUE/FALSE.")
  }

  if (!is.null(envir) && !is.environment(envir)) {
    stop("`envir` must be an environment or NULL.")
  }

  dots <- rlang::enquos(..., .ignore_empty = "all")

  defs_in <- list()
  if (!is.null(defs)) {
    if (!is.list(defs)) {
      stop("`defs` must be a list or NULL.")
    }
    if (is.null(names(defs)) || any(!nzchar(names(defs)))) {
      stop("`defs` must be a *named* list.")
    }
    defs_in <- defs
  }

  all_inputs <- c(defs_in, dots)
  if (!length(all_inputs)) {
    stop("No definitions supplied.")
  }

  nm <- names(all_inputs)
  fd_define_validate_step_names(nm, allow_duplicates = isTRUE(overwrite))

  if (isTRUE(overwrite) && anyDuplicated(nm)) {
    all_inputs <- all_inputs[!duplicated(names(all_inputs), fromLast = TRUE)]
  }

  nm <- names(all_inputs)
  fd_define_validate_step_names(nm, allow_duplicates = FALSE)

  step_defs <- lapply(names(all_inputs), function(step_name) {
    x <- all_inputs[[step_name]]

    if (rlang::is_quosure(x)) {
      x <- maybe_unwrap_symbol(x)
      if (rlang::is_quosure(x)) {
        x <- maybe_eval_fd_block_call(x)
      }
      default_env <- if (rlang::is_quosure(x)) {
        rlang::get_env(x)
      } else if (inherits(x, "featdelta_block")) {
        x$env %||% rlang::caller_env()
      } else {
        rlang::caller_env()
      }
    } else {
      default_env <- rlang::caller_env()
    }

    target_env <- envir %||% default_env

    normalize_definition_step(
      x = x,
      env = target_env,
      step_name = step_name
    )
  })

  names(step_defs) <- names(all_inputs)
  fd_define_validate_step_defs(step_defs)

  out <- list(
    steps = step_defs,
    description = description,
    created_at = Sys.time(),
    defs_version = 2L,
    envir_policy = if (is.null(envir)) "caller" else "explicit",
    envir = envir
  )

  class(out) <- c("featdelta_defs", "list")
  out
}

#' Print a featdelta definitions object
#'
#' Prints a concise human-readable summary of a `featdelta_defs` object.
#'
#' @param x A `featdelta_defs` object.
#' @param ... Unused.
#'
#' @return The input object invisibly.
#'
#' @family featdelta defs helpers
#' @export
print.featdelta_defs <- function(x, ...) {
  cat("<featdelta_defs>", "\n", sep = "")

  if (!is.null(x$description)) {
    cat("Description: ", x$description, "\n", sep = "")
  }

  cat("Definition steps (", length(x$steps), "):\n", sep = "")

  for (i in seq_along(x$steps)) {
    step <- x$steps[[i]]
    step_name <- names(x$steps)[[i]]

    if (identical(step$type, "column")) {
      cat(
        "  - [column] ",
        step_name,
        " -> ",
        step$text,
        "\n",
        sep = ""
      )

    } else if (identical(step$type, "block")) {
      cat(
        "  - [block]  ",
        step_name,
        " -> ",
        step$text,
        "\n",
        sep = ""
      )

      if (!is.null(step$output_names)) {
        cat(
          "      expected: ",
          paste(step$output_names, collapse = ", "),
          "\n",
          sep = ""
        )
      }

    } else {
      cat("  - [unknown] ", step_name, "\n", sep = "")
    }
  }

  invisible(x)
}


#' Summarize a featdelta definitions object
#'
#' Returns a compact data-frame summary of the ordered definition steps stored
#' in a `featdelta_defs` object.
#'
#' @param object A `featdelta_defs` object.
#' @param ... Unused.
#'
#' @return A data frame with one row per definition step and columns such as
#'   step name, type, mode, declared outputs, and stored expression text.
#'
#' @family featdelta defs helpers
#' @export
summary.featdelta_defs <- function(object, ...) {
  data.frame(
    step_name = names(object$steps),
    type = vapply(object$steps, function(s) s$type, character(1)),
    mode = vapply(
      object$steps,
      function(s) s$mode %||% NA_character_,
      character(1)
    ),
    outputs = vapply(
      object$steps,
      function(s) paste(s$output_names %||% character(0), collapse = ", "),
      character(1)
    ),
    text = vapply(
      object$steps,
      function(s) s$text %||% NA_character_,
      character(1)
    ),
    stringsAsFactors = FALSE
  )
}


#' Normalize one user-supplied definition into a step object
#'
#' Internal helper used by `fd_define()` to convert one supported input style
#' into the package's canonical stored step representation.
#'
#' At present, ordinary inputs are normalized to `type = "column"` steps.
#' The returned structure is intentionally general enough to support future
#' step types such as `type = "block"`.
#'
#' @param x A single user-supplied definition.
#' @param env Environment used when constructing the normalized quosure.
#' @param step_name Step name used for clearer error messages.
#'
#' @return A normalized step object.
#'
#' @family featdelta defs helpers
#' @keywords internal
normalize_definition_step <- function(x, env, step_name) {

  if (inherits(x, "featdelta_block")) {
    return(normalize_block_step(x = x, env = env, step_name = step_name))
  }

  q <- normalize_feature_input(x, env = env, feature_name = step_name)
  e <- rlang::get_expr(q)

  list(
    type = "column",
    mode = "expression",
    step_name = step_name,
    output_names = step_name,
    fn = NULL,
    quo = q,
    expr = e,
    env = rlang::get_env(q),
    text = rlang::expr_text(e)
  )
}


#' Normalize a block definition into a step object
#'
#' Internal placeholder for future `fd_block()` support.
#'
#' @param x A block definition object.
#' @param env Environment used for evaluation.
#' @param step_name Step name used for clearer error messages.
#'
#' @return A normalized block step object.
#'
#' @family featdelta defs helpers
#' @keywords internal
normalize_block_step <- function(x, env, step_name) {
  list(
    type = "block",
    mode = x$mode,
    step_name = step_name,
    output_names = x$expected_names %||% NULL,
    fn = x$fn %||% NULL,
    quo = x$quo %||% NULL,
    expr = x$expr %||% NULL,
    env = x$env %||% env,
    text = x$text %||% "<block>"
  )
}


#' Normalize a single feature-like input to a canonical quosure representation
#'
#' Internal helper used by `fd_define()` to convert one supported single-column
#' definition input style into the package's canonical stored form.
#'
#' Supported input styles include:
#' \itemize{
#'   \item quosures
#'   \item `expression()` objects of length 1
#'   \item quoted calls
#'   \item quoted names
#'   \item direct scalar constants
#' }
#'
#' Direct atomic constants must be scalar. Multi-element atomic constants are
#' rejected because they encode row-count assumptions into the definitions and
#' belong in compute-time expressions instead.
#'
#' @param x A single user-supplied single-column definition.
#' @param env Environment used when constructing the normalized quosure.
#' @param feature_name Optional name used for clearer error messages.
#'
#' @return A quosure representing the normalized single-column definition.
#'
#' @family featdelta defs helpers
#' @keywords internal
normalize_feature_input <- function(x, env, feature_name = NULL) {
  nm <- feature_name %||% "<unknown>"

  if (rlang::is_quosure(x)) {
    expr <- rlang::get_expr(x)
    return(rlang::new_quosure(expr, env = env))
  }

  if (inherits(x, "expression")) {
    if (length(x) != 1L) {
      stop(
        sprintf(
          "Definition `%s` was supplied as `expression(...)` with length %d. ",
          nm, length(x)
        ),
        "Each single-column definition must correspond to exactly one expression.",
        call. = FALSE
      )
    }
    return(rlang::new_quosure(x[[1]], env = env))
  }

  if (is.call(x) || is.name(x)) {
    return(rlang::new_quosure(x, env = env))
  }

  if (is.atomic(x)) {
    if (length(x) != 1L) {
      stop(
        sprintf(
          "Definition `%s` was supplied as an atomic constant of length %d. ",
          nm, length(x)
        ),
        "Direct constants must be scalar. ",
        "If you want vectorized behavior, supply an expression instead.",
        call. = FALSE
      )
    }
    return(rlang::new_quosure(x, env = env))
  }

  stop(
    sprintf(
      "Definition `%s` must be an expression, quosure, call, symbol, or scalar constant.",
      nm
    )
  )
}


#' Maybe unwrap a captured symbol that points to a quoted expression container
#'
#' Internal helper used by `fd_define()` when a user writes a definition
#' through an intermediate object, for example:
#'
#' `predef <- expression(hp / cyl)`
#'
#' `fd_define(x = predef)`
#'
#' In such cases, tidy capture initially sees the symbol `predef` rather than
#' the expression stored inside it. This helper resolves the symbol only when
#' it refers to a recognized quoted-expression container such as:
#' \itemize{
#'   \item a quosure
#'   \item an `expression()` object
#'   \item a quoted language object
#' }
#'
#' Ordinary values are intentionally left untouched so that `fd_define()` does
#' not silently rewrite arbitrary objects from the calling environment.
#'
#' @param q A quosure captured from `...`.
#'
#' @return Either the original quosure or a normalized quosure built from the
#'   referenced quoted-expression container.
#'
#' @family featdelta defs helpers
#' @keywords internal
maybe_unwrap_symbol <- function(q) {
  if (!rlang::is_quosure(q)) {
    stop("`q` must be a quosure.", call. = FALSE)
  }

  expr <- rlang::get_expr(q)
  env  <- rlang::get_env(q)

  if (!is.symbol(expr)) {
    return(q)
  }

  name <- as.character(expr)

  if (!rlang::env_has(env, name, inherit = TRUE)) {
    return(q)
  }

  value <- rlang::env_get(env, name, inherit = TRUE)

  if (inherits(value, "featdelta_block")) {
    return(value)
  }

  if (rlang::is_quosure(value)) {
    return(value)
  }

  if (inherits(value, "expression")) {
    if (length(value) != 1L) {
      stop(
        sprintf(
          "Definition `%s` refers to an `expression()` object of length %d. ",
          name, length(value)
        ),
        "Each single-column definition must correspond to exactly one expression.",
        call. = FALSE
      )
    }
    return(rlang::new_quosure(value[[1]], env = env))
  }

  if (is.call(value) || is.name(value)) {
    return(rlang::new_quosure(value, env = env))
  }

  q
}


#' Maybe evaluate a captured `fd_block()` call
#'
#' Internal helper used by `fd_define()` to support inline block definitions
#' such as `fd_define(x = fd_block({ data.frame(...) }))`.
#'
#' Ordinary feature expressions are intentionally left unevaluated. Only direct
#' calls to `fd_block()` are evaluated at definition-construction time.
#'
#' @param q A quosure captured from `...`.
#'
#' @return Either a `featdelta_block` object or the original quosure.
#'
#' @family featdelta defs helpers
#' @keywords internal
maybe_eval_fd_block_call <- function(q) {
  if (!rlang::is_quosure(q)) {
    stop("`q` must be a quosure.", call. = FALSE)
  }

  expr <- rlang::get_expr(q)

  if (!is.call(expr)) {
    return(q)
  }

  head <- expr[[1]]
  is_fd_block_call <- is.symbol(head) && identical(as.character(head), "fd_block")

  if (!is_fd_block_call) {
    return(q)
  }

  value <- rlang::eval_tidy(expr, env = rlang::get_env(q))

  if (!inherits(value, "featdelta_block")) {
    stop("A direct `fd_block()` call must return a `featdelta_block` object.", call. = FALSE)
  }

  value
}


#' Validate step names at definition-construction time
#'
#' Internal helper used by `fd_define()` to enforce the package's step-name
#' policy before a defs object is created.
#'
#' @param nm Character vector of proposed step names.
#' @param allow_duplicates Logical. If `TRUE`, duplicate names are permitted
#'   temporarily (for example before `overwrite = TRUE` resolution). If
#'   `FALSE`, duplicates error.
#'
#' @return Invisibly returns `TRUE` on success. Errors on invalid names.
#'
#' @family featdelta defs helpers
#' @keywords internal
fd_define_validate_step_names <- function(nm, allow_duplicates = FALSE) {
  if (is.null(nm) || any(is.na(nm)) || any(!nzchar(nm))) {
    stop("All definition steps must be named with non-empty, non-NA names.", call. = FALSE)
  }

  reserved <- c(".key", ".rowid", ".featdelta")
  bad <- nm %in% reserved
  if (any(bad)) {
    stop(
      sprintf(
        "Reserved definition step name(s): %s",
        paste(unique(nm[bad]), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (!isTRUE(allow_duplicates) && anyDuplicated(nm)) {
    dups <- unique(nm[duplicated(nm)])
    stop(
      sprintf(
        "Duplicate definition step name(s): %s",
        paste(dups, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


#' Validate normalized step definitions before returning defs
#'
#' Internal helper used by `fd_define()` after normalization to verify that the
#' final `step_defs` object is internally consistent.
#'
#' @param step_defs Named list of normalized definition step objects.
#'
#' @return Invisibly returns `TRUE` on success. Errors on malformed normalized
#'   steps.
#'
#' @family featdelta defs helpers
#' @keywords internal
fd_define_validate_step_defs <- function(step_defs) {
  if (!is.list(step_defs) || !length(step_defs)) {
    stop("`step_defs` must be a non-empty list.", call. = FALSE)
  }

  nm <- names(step_defs)
  fd_define_validate_step_names(nm, allow_duplicates = FALSE)

  for (i in seq_along(step_defs)) {
    step <- step_defs[[i]]
    step_name_i <- nm[[i]]

    if (!is.list(step)) {
      stop(
        "Definition step `", step_name_i, "` must be stored as a list.",
        call. = FALSE
      )
    }

    required_common <- c("type", "step_name", "output_names", "env", "text")
    missing_common <- setdiff(required_common, names(step))
    if (length(missing_common)) {
      stop(
        "Definition step `", step_name_i, "` is malformed; missing field(s): ",
        paste(missing_common, collapse = ", "),
        call. = FALSE
      )
    }

    if (!identical(step$step_name, step_name_i)) {
      stop(
        "Definition step `", step_name_i,
        "` is malformed: stored `step_name` does not match list name.",
        call. = FALSE
      )
    }

    if (!is.character(step$type) || length(step$type) != 1L || is.na(step$type)) {
      stop(
        "Definition step `", step_name_i, "` has invalid `type`.",
        call. = FALSE
      )
    }

    if (!is.environment(step$env)) {
      stop(
        "Definition step `", step_name_i, "` has invalid `env`.",
        call. = FALSE
      )
    }

    if (!is.character(step$text) || length(step$text) != 1L || is.na(step$text)) {
      stop(
        "Definition step `", step_name_i, "` has invalid `text`.",
        call. = FALSE
      )
    }

    if (!is.null(step$output_names)) {
      if (!is.character(step$output_names) ||
          any(is.na(step$output_names)) ||
          any(!nzchar(step$output_names))) {
        stop(
          "Definition step `", step_name_i, "` has invalid `output_names`.",
          call. = FALSE
        )
      }

      if (anyDuplicated(step$output_names)) {
        stop(
          "Definition step `", step_name_i, "` has duplicate `output_names`.",
          call. = FALSE
        )
      }
    }

    if (identical(step$type, "column")) {
      required_column <- c("quo", "expr")
      missing_column <- setdiff(required_column, names(step))
      if (length(missing_column)) {
        stop(
          "Column step `", step_name_i, "` is malformed; missing field(s): ",
          paste(missing_column, collapse = ", "),
          call. = FALSE
        )
      }

      if (!rlang::is_quosure(step$quo)) {
        stop(
          "Column step `", step_name_i, "` has invalid `quo`.",
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
      required_block <- c("mode", "fn", "quo", "expr")
      missing_block <- setdiff(required_block, names(step))
      if (length(missing_block)) {
        stop(
          "Block step `", step_name_i, "` is malformed; missing field(s): ",
          paste(missing_block, collapse = ", "),
          call. = FALSE
        )
      }

      if (!is.character(step$mode) || length(step$mode) != 1L || is.na(step$mode)) {
        stop(
          "Block step `", step_name_i, "` has invalid `mode`.",
          call. = FALSE
        )
      }

      if (identical(step$mode, "function")) {
        if (!is.function(step$fn)) {
          stop(
            "Block step `", step_name_i, "` has invalid `fn` for `mode = 'function'`.",
            call. = FALSE
          )
        }

        if (!is.null(step$quo)) {
          stop(
            "Block step `", step_name_i,
            "` must have `quo = NULL` for `mode = 'function'`.",
            call. = FALSE
          )
        }

      } else if (identical(step$mode, "expression")) {
        if (!rlang::is_quosure(step$quo)) {
          stop(
            "Block step `", step_name_i,
            "` has invalid `quo` for `mode = 'expression'`.",
            call. = FALSE
          )
        }

        if (!is.null(step$fn)) {
          stop(
            "Block step `", step_name_i,
            "` must have `fn = NULL` for `mode = 'expression'`.",
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
