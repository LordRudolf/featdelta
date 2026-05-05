ctx_ns <- function(name) {
  getFromNamespace(name, "featdelta")
}

test_that("null-coalescing and SQL cleanup utilities behave predictably", {
  `%||%` <- ctx_ns("%||%")
  clean_sql <- ctx_ns("clean_sql")

  expect_identical(NULL %||% "fallback", "fallback")
  expect_identical(FALSE %||% TRUE, FALSE)
  expect_identical(character() %||% "fallback", character())

  expect_identical(clean_sql("  SELECT * FROM raw_tbl;  "), "SELECT * FROM raw_tbl")
  expect_identical(clean_sql("WITH x AS (SELECT 1) SELECT * FROM x"), "WITH x AS (SELECT 1) SELECT * FROM x")
})

test_that("parse_table_id validates dialect-specific table names", {
  parse_table_id <- ctx_ns("parse_table_id")

  expect_identical(parse_table_id("sqlite", "features_tbl"), "features_tbl")
  expect_error(
    parse_table_id("sqlite", "main.features_tbl"),
    "SQLite does not support schema-qualified"
  )

  id <- parse_table_id("postgres", "public.features_tbl")
  expect_true(methods::is(id, "Id"))
  expect_equal(id@name, c(schema = "public", table = "features_tbl"))

  mysql_id <- parse_table_id("mysql", "analytics.features_tbl")
  expect_true(methods::is(mysql_id, "Id"))
  expect_equal(mysql_id@name, c(schema = "analytics", table = "features_tbl"))

  expect_error(
    parse_table_id("postgres", "too.many.parts"),
    "must be 'table' or 'schema.table'"
  )
})

test_that("database utility helpers recognize SQLite connections", {
  is_dbi_connection <- ctx_ns("is_dbi_connection")
  is_featdelta_con <- ctx_ns("is_featdelta_con")
  quote_table <- ctx_ns("quote_table")
  detect_backend <- ctx_ns("detect_backend")

  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  expect_true(is_dbi_connection(con))
  expect_false(is_featdelta_con(con))
  expect_identical(detect_backend(con), "sqlite")
  expect_identical(quote_table(con, "features_tbl", "sqlite"), "`features_tbl`")
  expect_error(
    quote_table(con, "main.features_tbl", "sqlite"),
    "SQLite does not support schema-qualified"
  )
})

test_that("detect_backend rejects non-DBI and closed connections", {
  detect_backend <- ctx_ns("detect_backend")

  expect_error(detect_backend(list()), "must be a DBIConnection")

  con <- fd_test_sqlite_con()
  DBI::dbDisconnect(con)

  expect_error(detect_backend(con), "not a valid")
})

test_that("make_ctx_from_args builds finalized context fields", {
  make_ctx_from_args <- ctx_ns("make_ctx_from_args")

  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  ctx <- make_ctx_from_args(
    con = con,
    raw_table = "raw_tbl",
    feat_table_name = "features_tbl",
    key = "id",
    meta_enabled = TRUE,
    meta_schema = list(version = 1)
  )

  expect_s3_class(ctx, "featdelta_ctx")
  expect_identical(ctx$con, con)
  expect_identical(ctx$dialect, "sqlite")
  expect_identical(ctx$raw_table, "raw_tbl")
  expect_identical(ctx$feat_table_name, "features_tbl")
  expect_identical(ctx$key, "id")
  expect_true(ctx$meta_enabled)
  expect_equal(ctx$meta_schema, list(version = 1))
  expect_identical(ctx$raw_table_id, "raw_tbl")
  expect_identical(ctx$feat_table_id, "features_tbl")
  expect_identical(ctx$raw_table_q, "`raw_tbl`")
  expect_identical(ctx$feat_table_q, "`features_tbl`")
  expect_identical(ctx$key_q, "`id`")
})

test_that("resolve_ctx accepts plain DBI connections and featdelta contexts", {
  resolve_ctx <- ctx_ns("resolve_ctx")
  is_featdelta_con <- ctx_ns("is_featdelta_con")

  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  ctx <- resolve_ctx(
    con,
    feat_table_name = "features_tbl",
    key = "id"
  )

  expect_s3_class(ctx, "featdelta_ctx")
  expect_false(is_featdelta_con(ctx))
  expect_identical(ctx$feat_table_name, "features_tbl")
  expect_identical(ctx$key, "id")

  fd_con <- fd_connect(
    RSQLite::SQLite(),
    ":memory:",
    raw_table = "raw_tbl",
    feat_table_name = "features_tbl",
    key = "id"
  )
  on.exit(fd_test_disconnect(fd_con$con), add = TRUE)

  expect_true(is_featdelta_con(fd_con))

  expect_warning(
    overridden <- resolve_ctx(fd_con, feat_table_name = "features_new"),
    "Overriding fd_con\\$feat_table_name"
  )
  expect_s3_class(overridden, "featdelta_ctx")
  expect_s3_class(overridden, "featdelta_con")
  expect_identical(overridden$feat_table_name, "features_new")
  expect_identical(overridden$feat_table_q, "`features_new`")
  expect_identical(overridden$key, "id")
  expect_identical(overridden$key_q, "`id`")

  expect_error(resolve_ctx(list()), "Expected a DBIConnection or a featdelta_con")
})

test_that("ctx_apply_overrides updates raw fields and warns for featdelta_con objects", {
  ctx_apply_overrides <- ctx_ns("ctx_apply_overrides")

  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  fd_con <- fd_connect(
    RSQLite::SQLite(),
    ":memory:",
    feat_table_name = "features_tbl",
    key = "id"
  )
  on.exit(fd_test_disconnect(fd_con$con), add = TRUE)

  expect_warning(
    out <- ctx_apply_overrides(fd_con, feat_table_name = "features_new"),
    "Overriding fd_con\\$feat_table_name"
  )
  expect_identical(out$feat_table_name, "features_new")
  expect_identical(out$key, "id")

  plain <- list(con = con, feat_table_name = "features_tbl", key = "id")
  expect_silent(
    out_plain <- ctx_apply_overrides(
      plain,
      feat_table_name = "features_new",
      key = "row_id",
      warn = TRUE
    )
  )
  expect_identical(out_plain$feat_table_name, "features_new")
  expect_identical(out_plain$key, "row_id")
})

test_that("ctx_build_derived, ctx_validate, and ctx_finalize enforce context invariants", {
  ctx_build_derived <- ctx_ns("ctx_build_derived")
  ctx_validate <- ctx_ns("ctx_validate")
  ctx_finalize <- ctx_ns("ctx_finalize")

  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  raw_ctx <- list(
    con = con,
    dialect = NULL,
    raw_table = "raw_tbl",
    feat_table_name = "features_tbl",
    key = "id",
    meta_enabled = FALSE,
    meta_schema = NULL
  )

  derived <- ctx_build_derived(raw_ctx)
  expect_identical(derived$dialect, "sqlite")
  expect_identical(derived$raw_table_q, "`raw_tbl`")
  expect_identical(derived$feat_table_q, "`features_tbl`")
  expect_identical(derived$key_q, "`id`")

  expect_s3_class(ctx_finalize(raw_ctx), "featdelta_ctx")
  expect_type(ctx_validate(derived), "list")

  bad_meta <- derived
  bad_meta$meta_enabled <- NA
  expect_error(ctx_validate(bad_meta), "`ctx\\$meta_enabled`")

  bad_key <- derived
  bad_key$key <- ""
  expect_error(ctx_validate(bad_key), "`ctx\\$key`")

  missing_derived <- derived
  missing_derived$feat_table_id <- NULL
  expect_error(ctx_validate(missing_derived), "`feat_table_id` was not built")
})

test_that("fd_connect creates featdelta_con objects and handles deprecated raw_table_name", {
  testthat::skip_if_not_installed("RSQLite")

  fd_con <- fd_connect(
    RSQLite::SQLite(),
    ":memory:",
    raw_table = "raw_tbl",
    feat_table_name = "features_tbl",
    key = "id",
    meta_enabled = TRUE
  )
  on.exit(fd_test_disconnect(fd_con$con), add = TRUE)

  expect_s3_class(fd_con, "featdelta_con")
  expect_s3_class(fd_con, "featdelta_ctx")
  expect_identical(fd_con$dialect, "sqlite")
  expect_identical(fd_con$raw_table, "raw_tbl")
  expect_identical(fd_con$feat_table_name, "features_tbl")
  expect_identical(fd_con$key, "id")
  expect_true(fd_con$meta_enabled)

  expect_warning(
    aliased <- fd_connect(
      RSQLite::SQLite(),
      ":memory:",
      raw_table_name = "raw_tbl"
    ),
    "deprecated"
  )
  on.exit(fd_test_disconnect(aliased$con), add = TRUE)
  expect_identical(aliased$raw_table, "raw_tbl")

  expect_error(
    fd_connect(
      RSQLite::SQLite(),
      ":memory:",
      raw_table = "raw_tbl",
      raw_table_name = "other_raw"
    ),
    "Use only one"
  )
})

test_that("print.featdelta_con returns input invisibly and shows core context fields", {
  testthat::skip_if_not_installed("RSQLite")

  fd_con <- fd_connect(
    RSQLite::SQLite(),
    ":memory:",
    raw_table = "raw_tbl",
    feat_table_name = "features_tbl",
    key = "id"
  )
  on.exit(fd_test_disconnect(fd_con$con), add = TRUE)

  printed <- capture.output(ret <- print(fd_con))

  expect_identical(ret, fd_con)
  expect_true(any(grepl("<featdelta_con>", printed, fixed = TRUE)))
  expect_true(any(grepl("dialect:   sqlite", printed, fixed = TRUE)))
  expect_true(any(grepl("raw_table: raw_tbl", printed, fixed = TRUE)))
  expect_true(any(grepl("feat_table_name:features_tbl", printed, fixed = TRUE)))
  expect_true(any(grepl("key:       id", printed, fixed = TRUE)))
})

test_that("validate_general_args accepts valid common argument combinations", {
  validate_general_args <- ctx_ns("validate_general_args")

  raw <- fd_test_raw_data()
  features <- fd_test_features_data(c(1L, 2L))
  defs <- fd_define(ratio = x / y)
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  expect_true(validate_general_args(
    data = raw,
    con = con,
    sql = "SELECT * FROM raw_tbl",
    key = "id",
    defs = defs,
    feat_table_name = "features_tbl",
    features_df = features,
    compute_envir = new.env(parent = emptyenv()),
    logicals = list(verbose = FALSE),
    logicals_with_text = list(create_table = "auto")
  ))

  expect_true(validate_general_args(sql = "WITH x AS (SELECT 1) SELECT * FROM x"))
})

test_that("validate_general_args rejects invalid data, SQL, defs, and scalar controls", {
  validate_general_args <- ctx_ns("validate_general_args")
  raw <- fd_test_raw_data()
  defs <- fd_define(ratio = x / y)

  expect_error(validate_general_args(data = list(id = 1)), "`data` must be")
  expect_error(validate_general_args(defs = list()), "`defs` must be")
  expect_error(validate_general_args(compute_envir = list()), "`compute_envir`")
  expect_error(validate_general_args(sql = ""), "`sql` must be")
  expect_error(validate_general_args(sql = "DELETE FROM raw_tbl"), "SELECT query")
  expect_error(validate_general_args(feat_table_name = ""), "`feat_table_name`")
  expect_error(validate_general_args(key = ""), "`key`")
  expect_error(validate_general_args(features_df = list(id = 1)), "`features_df`")
  expect_error(validate_general_args(data = raw, key = "missing"), "`key` must be a column")
  expect_error(
    validate_general_args(data = transform(raw, id = c(1, 1, 3, 4, 5)), key = "id"),
    "duplicates"
  )
  expect_error(
    validate_general_args(data = transform(raw, id = c(1, NA, 3, 4, 5)), key = "id"),
    "must not contain NAs"
  )
  expect_error(
    validate_general_args(
      features_df = data.frame(id = c(1, 1), value = 1:2),
      key = "id"
    ),
    "duplicates"
  )
  expect_error(
    validate_general_args(logicals = list(verbose = NA)),
    "verbose must be TRUE/FALSE"
  )
  expect_error(
    validate_general_args(logicals_with_text = list(create_table = "yes")),
    "create_table must be TRUE/FALSE, or 'auto'"
  )
  expect_error(
    validate_general_args(logicals_with_text = list(create_table = NA)),
    "create_table must be TRUE/FALSE, or 'auto'"
  )

  expect_true(validate_general_args(defs = defs))
})
