

#' Connect to a database with featdelta defaults
#'
#' Creates a `featdelta_con` context object from a DBI driver and connection
#' arguments. The returned object stores the live DBI connection plus optional
#' featdelta defaults such as the raw table, feature table, key column, and
#' metadata settings.
#'
#' @param driver A DBI driver object, such as `RSQLite::SQLite()`,
#'   `RPostgres::Postgres()`, or `RMariaDB::MariaDB()`.
#' @param ... Additional arguments passed to `DBI::dbConnect()`.
#' @param raw_table Optional character scalar naming the raw/source table.
#' @param raw_table_name Deprecated alias for `raw_table`.
#' @param feat_table_name Optional character scalar naming the feature table.
#' @param key Optional character scalar naming the primary key column.
#' @param meta_enabled Logical. Whether featdelta metadata tracking is enabled.
#' @param meta_schema Optional list of metadata schema settings.
#'
#' @return A `featdelta_con` object.
#'
#' @family featdelta context helpers
#' @export
fd_connect <- function(driver,
                       ...,
                       raw_table = NULL,
                       raw_table_name = NULL,
                       feat_table_name = NULL,
                       key = NULL,
                       meta_enabled = FALSE,
                       meta_schema = NULL) {

  if (!is.null(raw_table_name)) {
    if (!is.null(raw_table)) {
      stop("Use only one of `raw_table` or `raw_table_name`.", call. = FALSE)
    }
    warning(
      "`raw_table_name` is deprecated; use `raw_table` instead.",
      call. = FALSE
    )
    raw_table <- raw_table_name
  }

  con <- DBI::dbConnect(driver, ...)

  ctx <- make_ctx_from_args(
    con = con,
    raw_table = raw_table,
    feat_table_name = feat_table_name,
    key = key,
    meta_enabled = meta_enabled,
    meta_schema = meta_schema
  )
  class(ctx) <- c("featdelta_con", class(ctx))
  ctx
}

#' Print a featdelta connection context
#'
#' @param x A `featdelta_con` object.
#' @param ... Unused.
#'
#' @return The input object invisibly.
#'
#' @export
print.featdelta_con <- function(x, ...) {
  cat("<featdelta_con>\n")
  cat(" dialect:   ", x$dialect, "\n", sep = "")
  cat(" raw_table: ", x$raw_table %||% "<unset>", "\n", sep = "")
  cat(" feat_table_name:", x$feat_table_name %||% "<unset>", "\n", sep = "")
  cat(" key:       ", x$key %||% "<unset>", "\n", sep = "")
  cat(" meta:      ", if (isTRUE(x$meta_enabled)) "enabled" else "disabled", "\n", sep = "")
  invisible(x)
}
