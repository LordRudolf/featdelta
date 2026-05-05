`%||%` <- function(x, y) if (is.null(x)) y else x

clean_sql <- function(sql) {
  sql_clean <- trimws(sql)
  sql_clean <- sub(";\\s*$", "", sql_clean)

  return(sql_clean)
}

#' Check whether object is a DBI connection
#'
#' Internal helper used to distinguish plain DBI connections from
#' featdelta-specific connection/context objects.
#'
#' @param x Any R object.
#'
#' @return Logical scalar. `TRUE` if `x` inherits from `"DBIConnection"`,
#'   otherwise `FALSE`.
#'
#' @noRd
is_dbi_connection <- function(x) {
  inherits(x, "DBIConnection")
}


#' Check whether object is a featdelta connection/context
#'
#' Identifies objects created by `fd_connect()` (or future equivalents)
#' that carry featdelta-specific defaults and metadata in addition to
#' a DBI connection.
#'
#' @param x Any R object.
#'
#' @return Logical scalar. `TRUE` if `x` inherits from `"featdelta_con"`,
#'   otherwise `FALSE`.
#'
#' @noRd
is_featdelta_con <- function(x) {
  inherits(x, "featdelta_con")
}


#' Convert table name string to DBI table identifier
#'
#' Converts a table name like `"table"` or `"schema.table"` into a value suitable
#' for DBI functions such as `dbExistsTable()` and `dbListFields()`.
#'
#' For Postgres and MySQL, `"schema.table"` is converted to
#' `DBI::Id(schema=..., table=...)`.
#' For SQLite, schema-qualified names are rejected.
#'
#' @noRd
parse_table_id <- function(dialect, table_name) {
  parts <- strsplit(table_name, ".", fixed = TRUE)[[1]]
  if (length(parts) == 1L) return(table_name)

  if (dialect == "sqlite") {
    stop("SQLite does not support schema-qualified `feat_table_name` ('schema.table').")
  }
  if (length(parts) != 2L) {
    stop("`feat_table_name` must be 'table' or 'schema.table'.")
  }
  DBI::Id(schema = parts[1], table = parts[2])
}

quote_table <- function(con, table_name, dialect) {
  as.character(DBI::dbQuoteIdentifier(con, parse_table_id(dialect, table_name)))
}

supported_dialects <- function() {
  c("postgres", "sqlite", "mysql")
}

ensure_supported_dialect <- function(dialect) {
  if (!dialect %in% supported_dialects()) {
    stop(
      "Unsupported/unknown dialect: ", dialect,
      ". Supported: ", paste(supported_dialects(), collapse = ", "), ".",
      call. = FALSE
    )
  }

  invisible(TRUE)
}


#' Detect database dialect from a DBI connection
#'
#' Attempts to infer the database backend (dialect) from DBI metadata and/or
#' the class of a DBI connection. Used to select dialect-specific SQL.
#'
#' @param con A `DBIConnection` object.
#'
#' @return Character scalar identifying the dialect:
#'   one of `"postgres"`, `"sqlite"`, `"mysql"`, or `"unknown"`.
#'
#' @details
#' Detection uses (1) `DBI::dbGetInfo(con)$dbms.name` when available, and
#' falls back to connection class names (e.g. `PqConnection`, `SQLiteConnection`,
#' `MariaDBConnection`) if metadata is unavailable or inconclusive.
#'
#' If detection fails, returns `"unknown"`; callers can decide whether to error.
#'
#' @noRd
detect_backend <- function(con) {
  if (!inherits(con, "DBIConnection")) {
    stop("`con` must be a DBIConnection (from DBI::dbConnect()).", call. = FALSE)
  }
  if (!DBI::dbIsValid(con)) {
    stop("`con` is not a valid (live) DBIConnection.", call. = FALSE)
  }

  # 1) Try DBI metadata
  info <- tryCatch(DBI::dbGetInfo(con), error = function(e) NULL)

  dbms_name <- ""
  if (!is.null(info) && !is.null(info$dbms.name) && length(info$dbms.name) > 0) {
    dbms_name <- tolower(as.character(info$dbms.name)[1])
  }

  # 2) Fall back to connection class
  con_classes <- tolower(class(con))

  is_sqlite <- grepl("sqlite", dbms_name) || any(grepl("sqliteconnection", con_classes))
  if (is_sqlite) return("sqlite")

  is_postgres <- grepl("postgres|postgresql|redshift", dbms_name) ||
    any(grepl("pqconnection|postgresqlconnection", con_classes))
  if (is_postgres) return("postgres")

  is_mysql <- grepl("mysql|mariadb", dbms_name) ||
    any(grepl("mariadbconnection|mysqlconnection", con_classes))
  if (is_mysql) return("mysql")

  # If you prefer strict behavior, error here; otherwise return "unknown".
  "unknown"
}
