render_sql <- function(template, values) {
  out <- template

  for (nm in names(values)) {
    val <- values[[nm]]

    if (is.null(val) || length(val) != 1L || is.na(val)) {
      stop(
        sprintf("render_sql(): placeholder {%s} has invalid replacement value.", nm),
        call. = FALSE
      )
    }

    out <- gsub(paste0("{", nm, "}"), as.character(val), out, fixed = TRUE)
  }

  out
}


#' SQL templates used internally by featdelta
#'
#' A structured list of SQL templates keyed by feature (`fd_upsert`) and backend.
#' Templates are rendered by substituting placeholders with pre-quoted identifiers
#' and pre-built clauses.
#'
#' This object is internal and not part of the public API.
#'
#' ## Placeholder conventions
#' Templates expect the caller to provide the following placeholders as strings:
#' - `{target}`: quoted target table identifier
#' - `{stage}`: quoted staging table identifier
#' - `{cols_all}`: comma-separated quoted column identifiers
#' - `{key}`: quoted key column identifier
#' - `{set_clause}`: update `SET` clause, e.g. `"f1" = EXCLUDED."f1", "f2" = EXCLUDED."f2"`
#' - `{t_key}` / `{s_key}`: qualified key references for joins, e.g. `t."id"` and `s."id"`
#' - `{limit}`: integer literal for sampling conflicts
#'
#' @noRd
fd_sql_templates <- list(

  # ---- upsert: merge / write paths ----
  upsert_merge_upsert = list(
    postgres = paste(
      "INSERT INTO {target} ({cols_all})",
      "SELECT {cols_all} FROM {stage}",
      "ON CONFLICT ({key}) DO UPDATE SET {set_clause}"
    ),

    sqlite = paste(
      "INSERT INTO {target} ({cols_all})",
      "SELECT {cols_all} FROM {stage}",
      "WHERE TRUE",
      "ON CONFLICT({key}) DO UPDATE SET {set_clause}"
    ),

    mysql = paste(
      "INSERT INTO {target} ({cols_all})",
      "SELECT {cols_all}",
      "FROM (SELECT {cols_all} FROM {stage}) AS fd_src",
      "ON DUPLICATE KEY UPDATE {set_clause}"
    )
  ),

  upsert_merge_insert_only = list(
    postgres = paste(
      "INSERT INTO {target} ({cols_insert})",
      "SELECT {cols_select}",
      "FROM {stage} s",
      "LEFT JOIN {target} t ON {t_key} = {s_key}",
      "WHERE {t_key} IS NULL"
    ),

    sqlite = paste(
      "INSERT INTO {target} ({cols_insert})",
      "SELECT {cols_select}",
      "FROM {stage} s",
      "LEFT JOIN {target} t ON {t_key} = {s_key}",
      "WHERE {t_key} IS NULL"
    ),

    mysql = paste(
      "INSERT INTO {target} ({cols_insert})",
      "SELECT {cols_select}",
      "FROM {stage} s",
      "LEFT JOIN {target} t ON {t_key} = {s_key}",
      "WHERE {t_key} IS NULL"
    )
  ),

  # ---- upsert: counts (existence-based, pre-merge) ----
  upsert_count_would_insert = list(
    postgres = paste(
      "SELECT COUNT(*) AS n",
      "FROM {stage} s",
      "LEFT JOIN {target} t ON {t_key} = {s_key}",
      "WHERE {t_key} IS NULL"
    ),
    sqlite = paste(
      "SELECT COUNT(*) AS n",
      "FROM {stage} s",
      "LEFT JOIN {target} t ON {t_key} = {s_key}",
      "WHERE {t_key} IS NULL"
    ),
    mysql = paste(
      "SELECT COUNT(*) AS n",
      "FROM {stage} s",
      "LEFT JOIN {target} t ON {t_key} = {s_key}",
      "WHERE {t_key} IS NULL"
    )
  ),

  upsert_count_would_update = list(
    postgres = paste(
      "SELECT COUNT(*) AS n",
      "FROM {stage} s",
      "JOIN {target} t ON {t_key} = {s_key}"
    ),
    sqlite = paste(
      "SELECT COUNT(*) AS n",
      "FROM {stage} s",
      "JOIN {target} t ON {t_key} = {s_key}"
    ),
    mysql = paste(
      "SELECT COUNT(*) AS n",
      "FROM {stage} s",
      "JOIN {target} t ON {t_key} = {s_key}"
    )
  ),

  # ---- upsert: conflict sampling for insert-only mode ----
  upsert_find_conflicts = list(
    postgres = paste(
      "SELECT {s_key} AS key_value",
      "FROM {stage} s",
      "JOIN {target} t ON {t_key} = {s_key}",
      "LIMIT {limit}"
    ),
    sqlite = paste(
      "SELECT {s_key} AS key_value",
      "FROM {stage} s",
      "JOIN {target} t ON {t_key} = {s_key}",
      "LIMIT {limit}"
    ),
    mysql = paste(
      "SELECT {s_key} AS key_value",
      "FROM {stage} s",
      "JOIN {target} t ON {t_key} = {s_key}",
      "LIMIT {limit}"
    )
  ),

  # ---- DDL helpers (used for messaging / schema evolution) ----
  drop_table = list(
    postgres = "DROP TABLE {target}",
    sqlite   = "DROP TABLE {target}",
    mysql    = "DROP TABLE {target}"
  ),

  alter_add_column = list(
    postgres = "ALTER TABLE {target} ADD COLUMN {col} {type}",
    sqlite   = "ALTER TABLE {target} ADD COLUMN {col} {type}",
    mysql    = "ALTER TABLE {target} ADD COLUMN {col} {type}"
  )
)
