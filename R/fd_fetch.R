#' Fetch source rows that are not yet present in the features table
#'
#' `fd_fetch()` executes a user-supplied SQL `SELECT` query against a database and
#' returns **only** those rows whose primary key (`key`) is **not present** in an
#' existing features table (`feat_table_name`).
#'
#' This function is intentionally not a general-purpose query runner. It always
#' applies a "not yet processed" filter against `feat_table_name` and errors if that
#' cannot be done honestly (e.g., missing tables/columns).
#'
#' ## Important assumption
#' `fd_fetch()` assumes that:
#' - the `sql` query is executed on the **same database** (same `con`) where
#'   `feat_table_name` is stored, and
#' - the returned `key` values are comparable to `feat_table_name.key`.
#'
#' Cross-database fetching (e.g., pulling data from one database and comparing to
#' a features table stored in another database) is not supported, because the
#' "not present in `feat_table_name`" filter must be evaluated by the database engine
#' in a single query context.
#'
#' @param con A live `DBIConnection`, created with [DBI::dbConnect()].
#' @param sql A single SQL string (typically a `SELECT`) that defines the **source
#'   dataset** you want to process into features. In other words, it should return
#'   the rows you would normally feed into your feature creation pipeline.
#'   The query may join multiple tables and apply filters.
#'
#'   The query must be usable as a derived table: `FROM (<sql>) AS r`.
#'   Trailing semicolons are tolerated and removed.
#' @param key Name of the primary key column (character scalar). Must exist in
#'   both the result of `sql` and in `feat_table_name`. This is the identifier used to
#'   decide whether a source row has already been processed into the features table.
#' @param feat_table_name Name of the existing features table in the database
#'   (character scalar). Must exist. This table is used only to identify which
#'   `key` values are already present.
#' @param use_max_key Logical. If `TRUE`, `fd_fetch()` may reduce join work by
#'   first computing `MAX(feat_table_name.key)` and selecting rows with `key > max_key`
#'   (guaranteed not in `feat_table_name`). If `FALSE`, `fd_fetch()` selects all the rows
#'   with any key no in `feat_table_name.key` which may be computationally more expensive.
#' @param verbose Logical. If `TRUE`, prints the executed SQL.
#'
#' @return A `data.frame` containing only rows returned by `sql` whose `key` is not
#' present in `feat_table_name`. The result includes an attribute `attr(x, "fd_fetch")`
#' (a list) with metadata such as `key`, `feat_table_name`, `use_max_key`, `max_key`
#' (if computed), `executed_sql`, and `n_rows`.
#'
#' @examples
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' on.exit(DBI::dbDisconnect(con), add = TRUE)
#'
#' DBI::dbExecute(con, "CREATE TABLE raw (id INTEGER, x INTEGER)")
#' DBI::dbExecute(con, "CREATE TABLE r_variables_table (id INTEGER)")
#' DBI::dbExecute(con, "INSERT INTO raw (id, x) VALUES (1,10), (2,20), (3,30), (4,40), (5,50)")
#' DBI::dbExecute(con, "INSERT INTO r_variables_table (id) VALUES (1), (2), (4)")
#'
#' # Returns ids 3 and 5 only (rows not yet present in the features table)
#' new_rows <- fd_fetch(
#'   con = con,
#'   sql = "SELECT * FROM raw",
#'   key = "id",
#'   feat_table_name = "r_variables_table"
#' )
#' }
#'
#' @export
fd_fetch <- function(
    con,
    sql,
    key,
    feat_table_name,
    use_max_key = FALSE,
    verbose = FALSE
) {

  sql_clean <- clean_sql(sql)

  ctx <- resolve_ctx(
    con_or_fd = con,
    feat_table_name = feat_table_name,
    key = key
  )

  validate_general_args(
    con = ctx$con,
    sql = sql_clean,
    key = ctx$key,
    feat_table_name = ctx$feat_table_name,
    logicals = list(
      use_max_key = use_max_key,
      verbose = verbose
    )
  )

  if (!DBI::dbExistsTable(ctx$con, ctx$feat_table_id)) {
    stop("`feat_table_name` does not exist in the database: ", ctx$feat_table_name)
  }

  feat_fields <- DBI::dbListFields(ctx$con, ctx$feat_table_id)
  if (!(ctx$key %in% feat_fields)) {
    stop("`feat_table_name` does not contain the `key` column '", ctx$key, "'.")
  }

  probe_sql <- paste0("SELECT * FROM (", sql_clean, ") AS fd_raw LIMIT 0")
  probe_df <- DBI::dbGetQuery(ctx$con, probe_sql)
  if (!(ctx$key %in% names(probe_df))) {
    stop("`sql` does not return a column named '", ctx$key, "'.")
  }

  max_key <- NA

  if (!use_max_key) {
    executed_sql <- paste0(
      "SELECT r.* FROM (", sql_clean, ") AS r ",
      "LEFT JOIN ", ctx$feat_table_q, " AS f ",
      "ON r.", ctx$key_q, " = f.", ctx$key_q, " ",
      "WHERE f.", ctx$key_q, " IS NULL"
    )
  } else {
    max_sql <- paste0(
      "SELECT MAX(", ctx$key_q, ") AS max_key FROM ", ctx$feat_table_q
    )
    max_df <- DBI::dbGetQuery(ctx$con, max_sql)
    max_key <- max_df$max_key[[1]]

    if (is.null(max_key) || is.na(max_key)) {
      executed_sql <- paste0("SELECT * FROM (", sql_clean, ") AS r")
    } else {
      q_max <- as.character(DBI::dbQuoteLiteral(ctx$con, max_key))
      executed_sql <- paste0(
        "SELECT * FROM (", sql_clean, ") AS r ",
        "WHERE r.", ctx$key_q, " > ", q_max
      )
    }
  }

  if (isTRUE(verbose)) {
    message("fd_fetch SQL:\n", executed_sql)
  }

  out <- DBI::dbGetQuery(ctx$con, executed_sql)

  attr(out, "fd_fetch") <- list(
    key = ctx$key,
    feat_table_name = ctx$feat_table_name,
    use_max_key = use_max_key,
    max_key = max_key,
    sql = sql_clean,
    executed_sql = executed_sql,
    n_rows = nrow(out)
  )

  out
}

