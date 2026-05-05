test_that("shared raw-data fixture is keyed and deterministic", {
  raw <- fd_test_raw_data()

  expect_s3_class(raw, "data.frame")
  expect_named(raw, c("id", "x", "y", "group"))
  expect_identical(raw$id, 1:5)
  expect_equal(anyDuplicated(raw$id), 0L)
  expect_false(anyNA(raw$id))
})

test_that("shared feature-data fixture is keyed by requested ids", {
  features <- fd_test_features_data(c(2L, 4L))

  expect_s3_class(features, "data.frame")
  expect_named(features, c("id", "existing_feature"))
  expect_identical(features$id, c(2L, 4L))
  expect_identical(features$existing_feature, c(200, 400))
})

test_that("SQLite fixture seeds raw and feature tables", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  seeded <- fd_test_seed_sqlite(con, feature_ids = c(1L, 3L))

  expect_true(DBI::dbExistsTable(con, "raw_tbl"))
  expect_true(DBI::dbExistsTable(con, "features_tbl"))
  expect_equal(DBI::dbReadTable(con, "raw_tbl"), seeded$raw)
  expect_equal(DBI::dbReadTable(con, "features_tbl"), seeded$features)
})
