# Validation layer
#
# This file contains the package's central argument validator.
# The design intentionally keeps one main validation function
# (validate_general_args()) rather than splitting validation into many
# top-level functions.
#
# Rule of thumb:
# - use this validator for common scalar/type/range checks;
# - do not use it for operation-specific semantics such as table existence,
#   feature result shape, or SQL execution outcomes.




#' Validate common featdelta function arguments
#'
#' Central internal validator used across featdelta functions to check common
#' argument classes, scalar constraints, simple value ranges, and keyed-data
#' invariants.
#'
#' The function is intentionally broad: callers pass only the arguments they
#' want checked and use `NULL` for the rest. This keeps validation logic in a
#' single place while allowing different callers to validate different subsets
#' of arguments.
#'
#' The validator covers common categories such as:
#' \itemize{
#'   \item data frames (`data`, `features_df`)
#'   \item DB connections (`con`)
#'   \item SQL text (`sql`)
#'   \item key/table names
#'   \item defs objects
#'   \item grouped logical flags
#'   \item grouped environment arguments
#'   \item grouped positive or non-negative integer-like controls
#'   \item keyed-data invariants such as "key column exists", "key unique",
#'         and "key contains no NA"
#' }
#'
#' This function does not perform database-side checks such as whether a table
#' exists, whether a column exists in a database table, or whether an SQL query
#' can actually be executed.
#'
#' @param data Optional data frame/tibble to validate.
#' @param con Optional `DBIConnection` to validate.
#' @param sql Optional SQL query string to validate.
#' @param key Optional character scalar naming the primary key column.
#' @param defs Optional featdelta specification object to validate.
#' @param feat_table_name Optional character scalar naming the feature table.
#' @param features_df Optional data frame of computed features.
#' @param logicals_with_text Optional named list of scalar flags allowed to be
#'   either `TRUE`/`FALSE` or a small sentinel string such as `"auto"`.
#' @param logicals Optional named list of scalar logical flags.
#'
#' @return Invisibly returns `TRUE` on success. Throws an error on the first
#'   failed validation.
#'
#' @details
#' This function is designed to be read and maintained as one main validation
#' routine. Internal assertion closures may be used inside the function body
#' to reduce repetition, but the package should continue to expose and use a
#' single central validator rather than many top-level validation functions.
#'
#' @family featdelta validation helpers
#' @noRd
validate_general_args <- function(
    data = NULL,
    con = NULL,
    sql = NULL,
    key = NULL,
    defs = NULL,
    feat_table_name = NULL,
    features_df = NULL,
    compute_envir = NULL,
    logicals_with_text = NULL,
    logicals = NULL
) {

  ## data
  if(!is.null(data)) {
    if (!is.data.frame(data)) {
      stop("`data` must be a data.frame or tibble.")
    }
  }

  ## defs
  if(!is.null(defs)) {
    if(!inherits(defs, "featdelta_defs")) {
      stop("`defs` must be a `featdelta_defs`.")
    }
  }

  ## compute_envir
  if (!is.null(compute_envir) && !is.environment(compute_envir)) {
    stop("`compute_envir` must be an environment or NULL.")
  }

  ## con
  if(!is.null(con)) {
    if (!inherits(con, "DBIConnection")) {
      stop("`con` must be a DBIConnection (from DBI::dbConnect()).")
    }
    if (!DBI::dbIsValid(con)) {
      stop("`con` is not a valid (open) DBIConnection.")
    }
  }

  ## sql
  if(!is.null(sql)) {
    if (!is.character(sql) || length(sql) != 1L || !nzchar(sql)) {
      stop("`sql` must be a non-empty character scalar.")
    }

    if (!grepl("^\\s*(SELECT|WITH)\\b", sql, ignore.case = TRUE)) {
      stop("`sql` must be a SELECT query (may start with SELECT or WITH).")
    }
  }

  ## feat_table_name
  if(!is.null(feat_table_name)) {
    if (!is.character(feat_table_name) || length(feat_table_name) != 1L || !nzchar(feat_table_name)) {
      stop("`feat_table_name` must be a non-empty character scalar naming the features table.")
    }
  }

  ## key
  if(!is.null(key)) {
    if (!is.character(key) || length(key) != 1L || !nzchar(key)) {
      stop("`key` must be a non-empty string.")
    }
  }

  ## features_df
  if(!is.null(features_df)) {
    if (!is.data.frame(features_df)) {
      stop("`features_df` must be a data.frame.")
    }
  }

  ## key && features_df
  if(!is.null(key) && !is.null(features_df)) {
    if (!key %in% names(features_df)) {
      stop("`key` must be a column in `features_df`.")
    }
    if(anyDuplicated(features_df[[key]])) {
      stop("`features_df[[key]]` contains duplicates; keys must be unique.")
    }
    if(anyNA(features_df[[key]])) {
      stop("`features_df[[key]]` must not contain NAs.")
    }
  }

  ## key && data
  if(!is.null(key) && !is.null(data)) {
    if (!key %in% names(data)) {
      stop("`key` must be a column in `data`.")
    }
    if(anyDuplicated(data[[key]])) {
      stop("`data[[key]]` contains duplicates; keys must be unique.")
    }
    if(anyNA(data[[key]])) {
      stop("`data[[key]]` must not contain NAs.")
    }
  }

  ## handling logicals
  if(!is.null(logicals)) {
    for(i in 1:length(logicals)) {
      var_name <- names(logicals)[[i]]
      value <- logicals[[i]]
      if (!is.logical(value) || length(value) != 1L || is.na(value)) {
        stop(var_name, " must be TRUE/FALSE.")
      }
    }
  }

  ## handling logicals with text
  if(!is.null(logicals_with_text)) {
    for(i in 1:length(logicals_with_text)) {
      var_name <- names(logicals_with_text)[[i]]
      value <- logicals_with_text[[i]]

      if(length(value) != 1L || is.na(value)) {
        stop(var_name, " must be TRUE/FALSE, or 'auto'.")
      }
      if(!is.logical(value) && !is.character(value)) {
        stop(var_name, " must be TRUE/FALSE, or 'auto'.")
      }
      if(is.character(value) && value != 'auto') {
        stop(var_name, " must be TRUE/FALSE, or 'auto'.")
      }

    }
  }

  invisible(TRUE)
}
