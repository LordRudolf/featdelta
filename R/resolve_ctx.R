# Context layer
#
# This file defines the featdelta execution context used by all DB-facing
# functions. The context is the package's single source of truth for:
# - the DB connection,
# - selected target/raw table names,
# - the primary key,
# - the detected dialect,
# - stable derived identifiers such as parsed table ids and quoted names.
#
# Rule of thumb:
# - DB-facing functions such as fd_fetch(), fd_upsert(), and fd_run()
#   should call resolve_ctx() at the top and then use ctx fields.
# - Pure in-memory functions such as fd_define() and fd_compute()
#   should not use ctx at all.


#' Apply explicit overrides to an existing featdelta context
#'
#' Internal helper used by `resolve_ctx()` when the user supplies an existing
#' featdelta connection/context object rather than a plain DBI connection.
#'
#' This function updates only raw context fields such as `raw_table`,
#' `feat_table_name`, `key`, `dialect`, and metadata controls.
#' It does not build derived fields by itself; callers should pass the result
#' through `ctx_finalize()`.
#'
#' @param ctx A list-like featdelta context object.
#' @param raw_table Optional character scalar override.
#' @param feat_table_name Optional character scalar override.
#' @param key Optional character scalar override.
#' @param dialect Optional character scalar override.
#' @param meta_enabled Optional logical override.
#' @param meta_schema Optional list override.
#' @param warn Logical. If `TRUE`, emit warnings when an existing stored value
#'   is replaced by an explicit argument.
#'
#' @return The updated context object, not yet finalized.
#'
#' @family featdelta context helpers
#' @noRd
ctx_apply_overrides <- function(
    ctx,
    raw_table = NULL,
    feat_table_name = NULL,
    key = NULL,
    dialect = NULL,
    meta_enabled = NULL,
    meta_schema = NULL,
    warn = TRUE
) {
  overrides <- list(
    raw_table = raw_table,
    feat_table_name = feat_table_name,
    key = key,
    dialect = dialect,
    meta_enabled = if (is.null(meta_enabled)) NULL else isTRUE(meta_enabled),
    meta_schema = meta_schema
  )

  for (nm in names(overrides)) {
    value <- overrides[[nm]]
    if (is.null(value)) next

    if (!identical(ctx[[nm]], value)) {
      if (isTRUE(warn) && inherits(ctx, "featdelta_con")) {
        warning(
          "Overriding fd_con$", nm, " with `", nm, "` argument.",
          call. = FALSE
        )
      }
      ctx[[nm]] <- value
    }
  }

  ctx
}


#' Build stable derived fields for a featdelta context
#'
#' Computes reusable derived values from the raw context fields. This keeps
#' downstream DB-facing code from reparsing table names or requoting
#' identifiers repeatedly.
#'
#' Typical derived fields include:
#' \itemize{
#'   \item parsed raw table identifier (`raw_table_id`)
#'   \item quoted raw table name (`raw_table_q`)
#'   \item parsed feature table identifier (`feat_table_id`)
#'   \item quoted feature table name (`feat_table_q`)
#'   \item quoted key identifier (`key_q`)
#' }
#'
#' Only stable derived fields should be created here. Mutable state such as
#' current table existence or operation-specific settings should be computed
#' by the calling function instead.
#'
#' @param ctx A partially normalized featdelta context.
#'
#' @return The same context object with derived fields added.
#'
#' @family featdelta context helpers
#' @noRd
ctx_build_derived <- function(ctx) {
  ctx$dialect <- ctx$dialect %||% detect_backend(ctx$con)

  ctx$raw_table_id <- NULL
  ctx$raw_table_q  <- NULL
  if (!is.null(ctx$raw_table)) {
    ctx$raw_table_id <- parse_table_id(ctx$dialect, ctx$raw_table)
    ctx$raw_table_q  <- as.character(
      DBI::dbQuoteIdentifier(ctx$con, ctx$raw_table_id)
    )
  }

  ctx$feat_table_id <- NULL
  ctx$feat_table_q  <- NULL
  if (!is.null(ctx$feat_table_name)) {
    ctx$feat_table_id <- parse_table_id(ctx$dialect, ctx$feat_table_name)
    ctx$feat_table_q  <- as.character(
      DBI::dbQuoteIdentifier(ctx$con, ctx$feat_table_id)
    )
  }

  ctx$key_q <- NULL
  if (!is.null(ctx$key)) {
    ctx$key_q <- as.character(DBI::dbQuoteIdentifier(ctx$con, ctx$key))
  }

  ctx
}


#' Validate a featdelta execution context
#'
#' Performs structural and type validation on a context object after raw fields
#' and derived fields have been assembled.
#'
#' Validation covers:
#' \itemize{
#'   \item `ctx$con` is a live `DBIConnection`
#'   \item scalar character fields such as `raw_table`, `feat_table_name`,
#'         `key`, and `dialect`
#'   \item metadata controls
#'   \item internal consistency of derived fields such as `feat_table_id`,
#'         `feat_table_q`, and `key_q`
#' }
#'
#' This function validates the context object itself. It does not check
#' database-side state such as whether a table exists or whether a column is
#' present in a given table.
#'
#' @param ctx A featdelta context object.
#'
#' @return The validated context object.
#'
#' @family featdelta context helpers
#' @noRd
ctx_validate <- function(ctx) {
  stopifnot(is.list(ctx))

  if (!is_dbi_connection(ctx$con)) {
    stop("`ctx$con` must be a DBIConnection.", call. = FALSE)
  }
  if (!DBI::dbIsValid(ctx$con)) {
    stop("`ctx$con` is not a valid (open) DBIConnection.", call. = FALSE)
  }

  check_chr1 <- function(x, nm) {
    if (is.null(x)) return(invisible(NULL))
    if (!is.character(x) || length(x) != 1L || !nzchar(x)) {
      stop("`ctx$", nm, "` must be a non-empty character scalar.", call. = FALSE)
    }
  }

  check_chr1(ctx$raw_table, "raw_table")
  check_chr1(ctx$feat_table_name, "feat_table_name")
  check_chr1(ctx$key, "key")
  check_chr1(ctx$dialect, "dialect")

  if (!is.logical(ctx$meta_enabled) || length(ctx$meta_enabled) != 1L || is.na(ctx$meta_enabled)) {
    stop("`ctx$meta_enabled` must be TRUE/FALSE.", call. = FALSE)
  }

  if (!is.null(ctx$meta_schema) && !is.list(ctx$meta_schema)) {
    stop("`ctx$meta_schema` must be a list (or NULL).", call. = FALSE)
  }

  if (!is.null(ctx$raw_table) && is.null(ctx$raw_table_id)) {
    stop("Internal ctx error: `raw_table_id` was not built.", call. = FALSE)
  }
  if (!is.null(ctx$feat_table_name) && is.null(ctx$feat_table_id)) {
    stop("Internal ctx error: `feat_table_id` was not built.", call. = FALSE)
  }
  if (!is.null(ctx$feat_table_name) &&
      (!is.character(ctx$feat_table_q) || length(ctx$feat_table_q) != 1L)) {
    stop("Internal ctx error: `feat_table_q` must be a single quoted identifier.", call. = FALSE)
  }
  if (!is.null(ctx$key) &&
      (!is.character(ctx$key_q) || length(ctx$key_q) != 1L)) {
    stop("Internal ctx error: `key_q` must be a single quoted identifier.", call. = FALSE)
  }

  ctx
}


#' Finalize a featdelta context
#'
#' @noRd
ctx_finalize <- function(ctx) {
  class(ctx) <- unique(c(class(ctx), "featdelta_ctx", "list"))
  ctx <- ctx_build_derived(ctx)
  ctx_validate(ctx)
}


#' Construct a featdelta context from explicit arguments
#'
#' Low-level internal constructor used when the caller supplies a plain
#' `DBIConnection` and explicit featdelta settings rather than an existing
#' featdelta context object.
#'
#' This constructor records raw context fields and then finalizes the context
#' via `ctx_finalize()`.
#'
#' @param con A live `DBIConnection`.
#' @param raw_table Optional character scalar naming the raw/source table.
#' @param feat_table_name Optional character scalar naming the feature table.
#' @param key Optional character scalar naming the primary key column.
#' @param dialect Optional character scalar. If `NULL`, the dialect is
#'   inferred from `con`.
#' @param meta_schema Optional list storing metadata layout/configuration.
#' @param meta_enabled Logical. Whether featdelta metadata tracking is enabled
#'   for this context.
#' @param ... Reserved for future internal extensions.
#'
#' @return A finalized context object of class `"featdelta_ctx"`.
#'
#' @family featdelta context helpers
#' @noRd
make_ctx_from_args <- function(
    con,
    raw_table = NULL,
    feat_table_name = NULL,
    key = NULL,
    dialect = NULL,
    meta_schema = NULL,
    meta_enabled = FALSE,
    ...
) {
  dots <- list(...)
  if (length(dots)) {
    # reserved for future internal fields
  }

  ctx <- list(
    con = con,
    dialect = dialect,
    raw_table = raw_table,
    feat_table_name = feat_table_name,
    key = key,
    meta_enabled = isTRUE(meta_enabled),
    meta_schema = meta_schema
  )

  ctx_finalize(ctx)
}


#' Resolve a featdelta execution context
#'
#' Central internal entry point for DB-facing featdelta functions.
#' `resolve_ctx()` normalizes either a plain `DBIConnection` or a previously
#' constructed featdelta connection/context object into a validated
#' `featdelta_ctx`.
#'
#' This function is intended to be called at the beginning of exported
#' DB-facing functions such as `fd_fetch()`, `fd_upsert()`, and `fd_run()`.
#' The returned context should then be used as the single source of truth for
#' database identity fields instead of reparsing table names or requoting
#' identifiers in downstream code.
#'
#' Context resolution has two stages:
#' 1. normalize raw inputs and explicit overrides;
#' 2. compute stable derived fields (for example parsed table identifiers and
#'    quoted names) and validate the final object.
#'
#' Pure in-memory functions such as `fd_define()` and `fd_compute()` do not
#' use this function.
#'
#' @param con_or_fd Either a live `DBIConnection` or a featdelta connection
#'   object such as one produced by `fd_connect()`.
#' @param ... Reserved for future internal extensions passed through to
#'   `make_ctx_from_args()`.
#' @param raw_table Optional character scalar overriding the raw/source table
#'   name stored in the context.
#' @param feat_table_name Optional character scalar overriding the feature
#'   table name stored in the context.
#' @param key Optional character scalar overriding the primary key stored in
#'   the context.
#' @param dialect Optional character scalar overriding the detected dialect.
#'   If `NULL`, the dialect is inferred from `con`.
#' @param meta_enabled Optional logical overriding metadata enablement.
#' @param meta_schema Optional list overriding metadata schema settings.
#'
#' @return A validated context object of class `"featdelta_ctx"`. Depending on
#'   the supplied fields, the object may contain:
#'   \itemize{
#'     \item `con`
#'     \item `dialect`
#'     \item `raw_table`, `raw_table_id`, `raw_table_q`
#'     \item `feat_table_name`, `feat_table_id`, `feat_table_q`
#'     \item `key`, `key_q`
#'     \item `meta_enabled`, `meta_schema`
#'   }
#'
#' @details
#' Explicit arguments take precedence over stored values in a
#' `"featdelta_con"` object. When this happens, the context is rebuilt so that
#' all derived fields remain consistent with the overridden raw fields.
#'
#' The context stores only stable derived values. Operation-specific or
#' mutable state such as whether a table currently exists, chunk sizes, or
#' cleaned SQL strings should not be stored in the context.
#'
#' @family featdelta context helpers
#' @noRd
resolve_ctx <- function(
    con_or_fd,
    ...,
    raw_table = NULL,
    feat_table_name = NULL,
    key = NULL,
    dialect = NULL,
    meta_enabled = NULL,
    meta_schema = NULL
) {
  if (is_featdelta_con(con_or_fd)) {
    ctx <- ctx_apply_overrides(
      ctx = con_or_fd,
      raw_table = raw_table,
      feat_table_name = feat_table_name,
      key = key,
      dialect = dialect,
      meta_enabled = meta_enabled,
      meta_schema = meta_schema,
      warn = TRUE
    )
    return(ctx_finalize(ctx))
  }

  if (is_dbi_connection(con_or_fd)) {
    return(make_ctx_from_args(
      con = con_or_fd,
      raw_table = raw_table,
      feat_table_name = feat_table_name,
      key = key,
      dialect = dialect,
      meta_enabled = isTRUE(meta_enabled),
      meta_schema = meta_schema,
      ...
    ))
  }

  stop("Expected a DBIConnection or a featdelta_con.", call. = FALSE)
}
