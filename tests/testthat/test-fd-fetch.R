test_that("fd_fetch returns rows whose keys are absent from the feature table", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)
  fd_test_seed_sqlite(con, feature_ids = c(1L, 3L))

  out <- fd_fetch(
    con = con,
    sql = "SELECT * FROM raw_tbl ORDER BY id",
    key = "id",
    feat_table_name = "features_tbl",
    use_max_key = FALSE
  )

  expect_s3_class(out, "data.frame")
  expect_identical(out$id, c(2L, 4L, 5L))
  expect_equal(out$x, c(20, 40, 50))
  expect_equal(out$y, c(4, 8, 10))
  expect_equal(out$group, c("a", "b", "c"))
})

test_that("fd_fetch attaches fetch metadata", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)
  fd_test_seed_sqlite(con, feature_ids = c(1L, 3L))

  out <- fd_fetch(
    con = con,
    sql = "  SELECT * FROM raw_tbl ORDER BY id;  ",
    key = "id",
    feat_table_name = "features_tbl",
    use_max_key = FALSE
  )
  info <- attr(out, "fd_fetch")

  expect_type(info, "list")
  expect_identical(info$key, "id")
  expect_identical(info$feat_table_name, "features_tbl")
  expect_false(info$use_max_key)
  expect_true(is.na(info$max_key))
  expect_identical(info$sql, "SELECT * FROM raw_tbl ORDER BY id")
  expect_match(info$executed_sql, "LEFT JOIN `features_tbl` AS f", fixed = TRUE)
  expect_match(info$executed_sql, "WHERE f.`id` IS NULL", fixed = TRUE)
  expect_identical(info$n_rows, 3L)
})

test_that("fd_fetch use_max_key fetches only keys above the current maximum", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)
  fd_test_seed_sqlite(con, feature_ids = c(1L, 3L))

  out <- fd_fetch(
    con = con,
    sql = "SELECT * FROM raw_tbl ORDER BY id",
    key = "id",
    feat_table_name = "features_tbl",
    use_max_key = TRUE
  )
  info <- attr(out, "fd_fetch")

  expect_identical(out$id, c(4L, 5L))
  expect_identical(info$max_key, 3L)
  expect_match(info$executed_sql, "WHERE r.`id` > 3", fixed = TRUE)
  expect_identical(info$n_rows, 2L)
})

test_that("fd_fetch use_max_key returns all rows when the feature table is empty", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)
  fd_test_seed_sqlite(con, feature_ids = integer())

  out <- fd_fetch(
    con = con,
    sql = "SELECT * FROM raw_tbl ORDER BY id",
    key = "id",
    feat_table_name = "features_tbl",
    use_max_key = TRUE
  )
  info <- attr(out, "fd_fetch")

  expect_identical(out$id, 1:5)
  expect_true(is.na(info$max_key))
  expect_match(
    info$executed_sql,
    "SELECT * FROM (SELECT * FROM raw_tbl ORDER BY id) AS r",
    fixed = TRUE
  )
  expect_identical(info$n_rows, 5L)
})

test_that("fd_fetch supports WITH queries that can be used as derived tables", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)
  fd_test_seed_sqlite(con, feature_ids = c(2L, 4L))

  out <- fd_fetch(
    con = con,
    sql = paste(
      "WITH raw_subset AS (",
      "SELECT id, x, y FROM raw_tbl WHERE x >= 20",
      ")",
      "SELECT * FROM raw_subset ORDER BY id"
    ),
    key = "id",
    feat_table_name = "features_tbl"
  )

  expect_named(out, c("id", "x", "y"))
  expect_identical(out$id, c(3L, 5L))
})

test_that("fd_fetch errors when the feature table is missing", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "raw_tbl", fd_test_raw_data(), overwrite = TRUE)

  expect_error(
    fd_fetch(
      con = con,
      sql = "SELECT * FROM raw_tbl",
      key = "id",
      feat_table_name = "features_tbl"
    ),
    "`feat_table_name` does not exist"
  )
})

test_that("fd_fetch errors when the feature table lacks the key column", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "raw_tbl", fd_test_raw_data(), overwrite = TRUE)
  DBI::dbWriteTable(
    con,
    "features_tbl",
    data.frame(other_id = c(1L, 3L), existing_feature = c(100, 300)),
    overwrite = TRUE
  )

  expect_error(
    fd_fetch(
      con = con,
      sql = "SELECT * FROM raw_tbl",
      key = "id",
      feat_table_name = "features_tbl"
    ),
    "does not contain the `key` column"
  )
})

test_that("fd_fetch errors when SQL does not return the key column", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)
  fd_test_seed_sqlite(con, feature_ids = c(1L, 3L))

  expect_error(
    fd_fetch(
      con = con,
      sql = "SELECT x, y FROM raw_tbl",
      key = "id",
      feat_table_name = "features_tbl"
    ),
    "`sql` does not return a column named 'id'"
  )
})

test_that("fd_fetch validates common arguments before querying", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)
  fd_test_seed_sqlite(con, feature_ids = c(1L))

  expect_error(
    fd_fetch(con, "DELETE FROM raw_tbl", key = "id", feat_table_name = "features_tbl"),
    "SELECT query"
  )
  expect_error(
    fd_fetch(con, "SELECT * FROM raw_tbl", key = "", feat_table_name = "features_tbl"),
    "key.*non-empty"
  )
  expect_error(
    fd_fetch(con, "SELECT * FROM raw_tbl", key = "id", feat_table_name = ""),
    "`feat_table_name`"
  )
  expect_error(
    fd_fetch(con, "SELECT * FROM raw_tbl", key = "id", feat_table_name = "features_tbl", use_max_key = NA),
    "use_max_key must be TRUE/FALSE"
  )
  expect_error(
    fd_fetch(con, "SELECT * FROM raw_tbl", key = "id", feat_table_name = "features_tbl", verbose = "yes"),
    "verbose must be TRUE/FALSE"
  )
})

test_that("fd_fetch emits executed SQL when verbose", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)
  fd_test_seed_sqlite(con, feature_ids = c(1L, 3L))

  expect_message(
    out <- fd_fetch(
      con = con,
      sql = "SELECT * FROM raw_tbl ORDER BY id",
      key = "id",
      feat_table_name = "features_tbl",
      verbose = TRUE
    ),
    "fd_fetch SQL:"
  )

  expect_identical(out$id, c(2L, 4L, 5L))
})
