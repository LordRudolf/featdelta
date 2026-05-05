fd_run_ns <- function(name) {
  getFromNamespace(name, "featdelta")
}

fd_run_seed_raw <- function(con, raw = fd_test_raw_data()) {
  DBI::dbWriteTable(con, "raw_tbl", raw, overwrite = TRUE)
  invisible(raw)
}

fd_run_defs <- function() {
  fd_define(
    ratio = x / y,
    total = x + y
  )
}

fd_run_expected_features <- function(raw) {
  data.frame(
    id = raw$id,
    ratio = raw$x / raw$y,
    total = raw$x + raw$y
  )
}

fd_run_read_features <- function(con) {
  DBI::dbGetQuery(con, "SELECT * FROM features_tbl ORDER BY id")
}

fd_run_create_features <- function(con, features) {
  fd_upsert(
    con = con,
    features_df = features,
    feat_table_name = "features_tbl",
    key = "id",
    create_table = TRUE,
    verbose = FALSE
  )
  invisible(TRUE)
}

test_that("fd_run performs an initial new_only run when the feature table is missing", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)
  raw <- fd_run_seed_raw(con)
  expected <- fd_run_expected_features(raw)

  report <- fd_run(
    con = con,
    sql = "SELECT id, x, y FROM raw_tbl ORDER BY id",
    defs = fd_run_defs(),
    key = "id",
    feat_table_name = "features_tbl",
    fetch_mode = "new_only",
    preview_n = 2L,
    verbose = FALSE
  )

  expect_s3_class(report, "fd_run_report")
  expect_true(report$success)
  expect_identical(report$stage, "complete")
  expect_identical(report$key, "id")
  expect_identical(report$feat_table_name, "features_tbl")
  expect_identical(report$dialect, "sqlite")
  expect_identical(report$fetch$source, "initial_all")
  expect_false(report$fetch$table_exists)
  expect_identical(report$fetch$n_rows, 5L)
  expect_identical(report$compute$n_rows, 5L)
  expect_identical(report$compute$n_features, 2L)
  expect_equal(report$compute$feature_names, c("ratio", "total"))
  expect_true(all(report$compute$report$ok))
  expect_true(report$upsert$table_created)
  expect_identical(report$upsert$counts$would_insert, 5L)
  expect_identical(report$upsert$counts$would_update, 0L)
  expect_null(report$data)
  expect_equal(report$preview$raw, raw[1:2, c("id", "x", "y")])
  expect_s3_class(report$preview$features, "featdelta_features")
  expect_equal(report$preview$features, expected[1:2, ], ignore_attr = TRUE)
  expect_equal(fd_run_read_features(con), expected)
})

test_that("fd_run new_only fetches only rows absent from an existing feature table", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)
  raw <- fd_run_seed_raw(con)
  expected <- fd_run_expected_features(raw)
  fd_run_create_features(con, expected[expected$id %in% c(1L, 3L), ])

  report <- fd_run(
    con = con,
    sql = "SELECT id, x, y FROM raw_tbl ORDER BY id",
    defs = fd_run_defs(),
    key = "id",
    feat_table_name = "features_tbl",
    fetch_mode = "new_only",
    verbose = FALSE
  )

  expect_true(report$success)
  expect_identical(report$fetch$source, "fd_fetch")
  expect_true(report$fetch$table_exists)
  expect_false(report$fetch$limit_applied)
  expect_identical(report$fetch$n_rows, 3L)
  expect_equal(report$fetch$fd_fetch$n_rows, 3L)
  expect_identical(report$compute$n_rows, 3L)
  expect_identical(report$upsert$counts$would_insert, 3L)
  expect_identical(report$upsert$counts$would_update, 0L)
  expect_equal(fd_run_read_features(con), expected)
})

test_that("fd_run new_only handles existing feature table with no new raw rows", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)
  raw <- fd_run_seed_raw(con)
  expected <- fd_run_expected_features(raw)
  fd_run_create_features(con, expected)

  report <- fd_run(
    con = con,
    sql = "SELECT id, x, y FROM raw_tbl ORDER BY id",
    defs = fd_run_defs(),
    key = "id",
    feat_table_name = "features_tbl",
    fetch_mode = "new_only",
    verbose = FALSE
  )

  expect_true(report$success)
  expect_identical(report$fetch$source, "fd_fetch")
  expect_identical(report$fetch$n_rows, 0L)
  expect_identical(report$compute$n_rows, 0L)
  expect_identical(report$upsert$n_rows, 0L)
  expect_identical(report$upsert$counts$would_insert, 0L)
  expect_identical(report$upsert$counts$would_update, 0L)
  expect_equal(fd_run_read_features(con), expected)
})

test_that("fd_run fetch_mode all refreshes existing rows and inserts new rows", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)
  raw <- fd_run_seed_raw(con)
  expected <- fd_run_expected_features(raw)
  stale <- expected[expected$id %in% c(1L, 3L), ]
  stale$ratio <- -1
  stale$total <- -1
  fd_run_create_features(con, stale)

  report <- fd_run(
    con = con,
    sql = "SELECT id, x, y FROM raw_tbl ORDER BY id",
    defs = fd_run_defs(),
    key = "id",
    feat_table_name = "features_tbl",
    fetch_mode = "all",
    verbose = FALSE
  )

  expect_true(report$success)
  expect_identical(report$fetch$source, "all")
  expect_identical(report$fetch$n_rows, 5L)
  expect_identical(report$upsert$counts$would_insert, 3L)
  expect_identical(report$upsert$counts$would_update, 2L)
  expect_equal(fd_run_read_features(con), expected)
})

test_that("fd_run documents feature-definition backfill behavior through fetch_mode", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)
  raw <- fd_run_seed_raw(con)

  initial_defs <- fd_define(ratio = x / y)
  initial_features <- data.frame(id = raw$id, ratio = raw$x / raw$y)
  fd_run_create_features(con, initial_features)

  expanded_defs <- fd_define(
    ratio = x / y,
    total = x + y
  )

  new_only_report <- fd_run(
    con = con,
    sql = "SELECT id, x, y FROM raw_tbl ORDER BY id",
    defs = expanded_defs,
    key = "id",
    feat_table_name = "features_tbl",
    fetch_mode = "new_only",
    verbose = FALSE
  )

  new_only_out <- fd_run_read_features(con)
  expect_true(new_only_report$success)
  expect_identical(new_only_report$fetch$n_rows, 0L)
  expect_identical(new_only_report$upsert$columns_added, "total")
  expect_true(all(is.na(new_only_out$total)))

  all_report <- fd_run(
    con = con,
    sql = "SELECT id, x, y FROM raw_tbl ORDER BY id",
    defs = expanded_defs,
    key = "id",
    feat_table_name = "features_tbl",
    fetch_mode = "all",
    verbose = FALSE
  )

  all_out <- fd_run_read_features(con)
  expect_true(all_report$success)
  expect_identical(all_report$fetch$n_rows, 5L)
  expect_identical(all_report$upsert$counts$would_insert, 0L)
  expect_identical(all_report$upsert$counts$would_update, 5L)
  expect_equal(all_out$total, raw$x + raw$y)
})

test_that("fd_run applies fetch_limit after fetching and records return data", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)
  raw <- fd_run_seed_raw(con)
  expected <- fd_run_expected_features(raw[1:2, ])

  report <- fd_run(
    con = con,
    sql = "SELECT id, x, y FROM raw_tbl ORDER BY id",
    defs = fd_run_defs(),
    key = "id",
    feat_table_name = "features_tbl",
    fetch_limit = 2L,
    return_data = "both",
    preview_n = 1L,
    verbose = FALSE
  )

  expect_true(report$success)
  expect_true(report$fetch$limit_applied)
  expect_identical(report$fetch$n_rows_before_limit, 5L)
  expect_identical(report$fetch$n_rows, 2L)
  expect_named(report$data, c("raw", "features"))
  expect_equal(report$data$raw, raw[1:2, c("id", "x", "y")])
  expect_s3_class(report$data$features, "featdelta_features")
  expect_equal(report$data$features, expected, ignore_attr = TRUE)
  expect_equal(report$preview$raw, raw[1, c("id", "x", "y"), drop = FALSE])
  expect_s3_class(report$preview$features, "featdelta_features")
  expect_equal(report$preview$features, expected[1, , drop = FALSE], ignore_attr = TRUE)
  expect_equal(fd_run_read_features(con), expected)
})

test_that("fd_run supports compute_envir overrides", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)
  raw <- fd_run_seed_raw(con)

  defs_env <- new.env(parent = baseenv())
  defs_env$offset <- 10
  compute_env <- new.env(parent = baseenv())
  compute_env$offset <- 100
  defs <- fd_define(shifted = x + offset, envir = defs_env)

  report <- fd_run(
    con = con,
    sql = "SELECT id, x FROM raw_tbl ORDER BY id",
    defs = defs,
    key = "id",
    feat_table_name = "features_tbl",
    compute_envir = compute_env,
    return_data = "features",
    verbose = FALSE
  )

  expect_true(report$success)
  expect_equal(report$data$features$shifted, raw$x + 100)
  expect_equal(fd_run_read_features(con)$shifted, raw$x + 100)
})

test_that("fd_run captures compute failures when fail_fast is FALSE", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)
  raw <- fd_run_seed_raw(con)
  defs <- fd_define(
    ratio = x / y,
    bad = missing_column + 1
  )

  report <- fd_run(
    con = con,
    sql = "SELECT id, x, y FROM raw_tbl ORDER BY id",
    defs = defs,
    key = "id",
    feat_table_name = "features_tbl",
    fail_fast = FALSE,
    return_data = "both",
    preview_n = 2L,
    verbose = FALSE
  )

  expect_s3_class(report, "fd_run_report")
  expect_false(report$success)
  expect_identical(report$stage, "compute")
  expect_identical(report$error$stage, "compute")
  expect_match(report$error$message, "one or more definition steps failed")
  expect_identical(report$compute$n_rows, 5L)
  expect_identical(report$compute$n_features, 1L)
  expect_equal(report$compute$report$ok, c(TRUE, FALSE))
  expect_named(report$data, c("raw", "features"))
  expect_equal(report$data$raw, raw[, c("id", "x", "y")])
  expect_equal(report$data$features$ratio, raw$x / raw$y)
  expect_null(report$upsert)
  expect_false(DBI::dbExistsTable(con, "features_tbl"))
})

test_that("fd_run raises stage errors when fail_fast is TRUE", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)
  fd_run_seed_raw(con)
  defs <- fd_define(bad = missing_column + 1)

  expect_error(
    fd_run(
      con = con,
      sql = "SELECT id, x FROM raw_tbl ORDER BY id",
      defs = defs,
      key = "id",
      feat_table_name = "features_tbl",
      fail_fast = TRUE,
      verbose = FALSE
    ),
    "one or more definition steps failed",
    class = "featdelta_compute_error"
  )
})

test_that("fd_run captures upsert failures when fail_fast is FALSE", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)
  raw <- fd_run_seed_raw(con)
  expected <- fd_run_expected_features(raw)
  fd_run_create_features(con, expected[expected$id %in% c(1L, 3L), ])

  report <- fd_run(
    con = con,
    sql = "SELECT id, x, y FROM raw_tbl ORDER BY id",
    defs = fd_run_defs(),
    key = "id",
    feat_table_name = "features_tbl",
    fetch_mode = "all",
    update_table = FALSE,
    fail_fast = FALSE,
    return_data = "features",
    verbose = FALSE
  )

  expect_false(report$success)
  expect_identical(report$stage, "upsert")
  expect_identical(report$error$stage, "upsert")
  expect_match(report$error$message, "Conflicts detected")
  expect_identical(report$compute$n_rows, 5L)
  expect_identical(report$compute$n_features, 2L)
  expect_named(report$data, "features")
  expect_s3_class(report$data$features, "featdelta_features")
  expect_equal(report$data$features, expected, ignore_attr = TRUE)
  expect_null(report$upsert)
  expect_equal(
    fd_run_read_features(con),
    expected[expected$id %in% c(1L, 3L), ],
    ignore_attr = TRUE
  )
})

test_that("fd_run validates controls and rejects unsupported dialects", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)
  fd_run_seed_raw(con)
  defs <- fd_run_defs()

  expect_error(
    fd_run(con, "SELECT * FROM raw_tbl", defs, key = "id", feat_table_name = "features_tbl", fetch_limit = 0),
    "`fetch_limit` must be NULL or a positive number"
  )
  expect_error(
    fd_run(con, "SELECT * FROM raw_tbl", defs, key = "id", feat_table_name = "features_tbl", chunk_size = -1),
    "`chunk_size` must be NULL or a positive number"
  )
  expect_error(
    fd_run(con, "SELECT * FROM raw_tbl", defs, key = "id", feat_table_name = "features_tbl", preview_n = -1),
    "`preview_n` must be a non-negative number"
  )
  expect_error(
    fd_run(con, "SELECT * FROM raw_tbl", defs, key = "id", feat_table_name = "features_tbl", fetch_mode = "bad"),
    "'arg' should be one of"
  )
  expect_error(
    fd_run(con, "SELECT * FROM raw_tbl", defs, key = "id", feat_table_name = "features_tbl", return_data = "bad"),
    "'arg' should be one of"
  )
  expect_error(
    fd_run(con, "SELECT * FROM raw_tbl", defs, key = "id", feat_table_name = "features_tbl", dialect = "oracle"),
    "Supported: postgres, sqlite, mysql"
  )
})

test_that("fd_run_fetch_stage records source and limit metadata", {
  resolve_ctx <- fd_run_ns("resolve_ctx")
  fetch_stage <- fd_run_ns("fd_run_fetch_stage")

  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)
  raw <- fd_run_seed_raw(con)
  expected <- fd_run_expected_features(raw)
  fd_run_create_features(con, expected[expected$id %in% c(1L, 2L), ])
  ctx <- resolve_ctx(con, feat_table_name = "features_tbl", key = "id")

  stage <- fetch_stage(
    ctx = ctx,
    sql_clean = "SELECT id, x, y FROM raw_tbl ORDER BY id",
    fetch_mode = "new_only",
    table_exists = TRUE,
    use_max_key = FALSE,
    fetch_limit = 2L,
    verbose = FALSE
  )

  expect_true(stage$ok)
  expect_equal(stage$data$id, c(3L, 4L))
  expect_identical(stage$info$source, "fd_fetch")
  expect_true(stage$info$limit_applied)
  expect_identical(stage$info$n_rows_before_limit, 3L)
  expect_identical(stage$info$n_rows, 2L)
  expect_type(stage$info$fd_fetch, "list")
})

test_that("fd_run report helpers select data and previews", {
  select_data <- fd_run_ns("fd_run_select_return_data")
  preview <- fd_run_ns("fd_run_preview")
  validate_positive <- fd_run_ns("fd_run_validate_optional_positive_int")
  validate_nonnegative <- fd_run_ns("fd_run_validate_nonnegative_int")

  raw <- fd_test_raw_data()
  features <- data.frame(id = raw$id, ratio = raw$x / raw$y)

  expect_null(select_data(raw, features, "none"))
  expect_named(select_data(raw, features, "raw"), "raw")
  expect_named(select_data(raw, features, "features"), "features")
  expect_named(select_data(raw, features, "both"), c("raw", "features"))
  expect_equal(preview(raw, 2L), raw[1:2, ])
  expect_null(preview(raw, 0L))
  expect_null(preview(NULL, 2L))
  expect_null(validate_positive(NULL, "x"))
  expect_identical(validate_positive(2.9, "x"), 2L)
  expect_error(validate_positive(0, "x"), "`x` must be NULL or a positive number")
  expect_identical(validate_nonnegative(0, "x"), 0L)
  expect_error(validate_nonnegative(-1, "x"), "`x` must be a non-negative number")
})

test_that("fd_run_stage_error and fd_run_build_report produce structured failures", {
  stage_error <- fd_run_ns("fd_run_stage_error")
  build_report <- fd_run_ns("fd_run_build_report")
  resolve_ctx <- fd_run_ns("resolve_ctx")

  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)
  ctx <- resolve_ctx(con, feat_table_name = "features_tbl", key = "id")
  err <- stage_error("fetch", simpleError("broken fetch"))

  expect_false(err$ok)
  expect_identical(err$stage, "fetch")
  expect_identical(err$message, "broken fetch")
  expect_true("simpleError" %in% err$class)

  report <- build_report(
    success = FALSE,
    stage = "fetch",
    started_at = Sys.time(),
    ctx = ctx,
    sql_clean = "SELECT * FROM raw_tbl",
    fetch_mode = "new_only",
    table_exists = FALSE,
    use_max_key = FALSE,
    fetch_limit = NULL,
    error = err,
    preview_n = 2L
  )

  expect_s3_class(report, "fd_run_report")
  expect_false(report$success)
  expect_identical(report$stage, "fetch")
  expect_identical(report$error$stage, "fetch")
  expect_identical(report$error$message, "broken fetch")
  expect_null(report$preview$raw)
  expect_null(report$preview$features)
})

test_that("print.fd_run_report returns input invisibly and shows summary fields", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)
  fd_run_seed_raw(con)

  report <- fd_run(
    con = con,
    sql = "SELECT id, x, y FROM raw_tbl ORDER BY id",
    defs = fd_run_defs(),
    key = "id",
    feat_table_name = "features_tbl",
    verbose = FALSE
  )

  printed <- capture.output(ret <- print(report))

  expect_identical(ret, report)
  expect_true(any(grepl("<fd_run_report>", printed, fixed = TRUE)))
  expect_true(any(grepl("success:   TRUE", printed, fixed = TRUE)))
  expect_true(any(grepl("stage:     complete", printed, fixed = TRUE)))
  expect_true(any(grepl("table:     features_tbl", printed, fixed = TRUE)))
  expect_true(any(grepl("dialect:   sqlite", printed, fixed = TRUE)))
  expect_true(any(grepl("upsert:    would_insert=5, would_update=0", printed, fixed = TRUE)))
})
