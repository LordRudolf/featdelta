fd_upsert_read_table <- function(con, table = "features_tbl") {
  DBI::dbGetQuery(con, paste0("SELECT * FROM ", table, " ORDER BY id"))
}

fd_upsert_create_target <- function(con, features) {
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

test_that("fd_upsert creates a missing table with create_table auto and inserts rows", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  features <- data.frame(
    id = 1:3,
    score = c(10, 20, 30),
    flag = c(TRUE, FALSE, TRUE),
    label = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )

  report <- fd_upsert(
    con = con,
    features_df = features,
    feat_table_name = "features_tbl",
    key = "id",
    create_table = "auto",
    verbose = FALSE
  )

  expect_s3_class(report, "fd_upsert_report")
  expect_true(report$table_created)
  expect_identical(report$columns_added, character())
  expect_identical(report$n_rows, 3L)
  expect_identical(report$n_chunks, 1L)
  expect_identical(report$counts$would_insert, 3L)
  expect_identical(report$counts$would_update, 0L)
  expect_equal(report$chunk_details$n, 3L)
  expect_equal(report$chunk_details$would_insert, 3L)

  out <- fd_upsert_read_table(con)
  expect_equal(out$id, features$id)
  expect_equal(out$score, features$score)
  expect_equal(out$flag, as.integer(features$flag))
  expect_equal(out$label, features$label)
})

test_that("fd_upsert inserts new keys and updates existing keys", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  initial <- data.frame(
    id = 1:3,
    score = c(10, 20, 30),
    label = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )
  fd_upsert_create_target(con, initial)

  incoming <- data.frame(
    id = c(2L, 3L, 4L),
    score = c(21, 31, 40),
    label = c("b2", "c2", "d"),
    stringsAsFactors = FALSE
  )
  report <- fd_upsert(
    con = con,
    features_df = incoming,
    feat_table_name = "features_tbl",
    key = "id",
    update_table = TRUE,
    verbose = FALSE
  )

  expect_false(report$table_created)
  expect_identical(report$counts$would_insert, 1L)
  expect_identical(report$counts$would_update, 2L)

  out <- fd_upsert_read_table(con)
  expect_equal(out$id, 1:4)
  expect_equal(out$score, c(10, 21, 31, 40))
  expect_equal(out$label, c("a", "b2", "c2", "d"))
})

test_that("fd_upsert insert-only mode inserts non-conflicting rows", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  fd_upsert_create_target(con, data.frame(id = 1:2, score = c(10, 20)))

  incoming <- data.frame(id = 3:4, score = c(30, 40))
  report <- fd_upsert(
    con = con,
    features_df = incoming,
    feat_table_name = "features_tbl",
    key = "id",
    update_table = FALSE,
    verbose = FALSE
  )

  expect_identical(report$counts$would_insert, 2L)
  expect_identical(report$counts$would_update, 0L)
  expect_equal(fd_upsert_read_table(con)$id, 1:4)
})

test_that("fd_upsert insert-only mode errors and leaves existing rows unchanged on conflicts", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  initial <- data.frame(id = 1:3, score = c(10, 20, 30))
  fd_upsert_create_target(con, initial)

  expect_error(
    fd_upsert(
      con = con,
      features_df = data.frame(id = c(3L, 4L), score = c(31, 40)),
      feat_table_name = "features_tbl",
      key = "id",
      update_table = FALSE,
      verbose = FALSE
    ),
    "Conflicts detected"
  )

  expect_equal(fd_upsert_read_table(con), initial)
})

test_that("fd_upsert adds missing feature columns when alter_table is TRUE", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  fd_upsert_create_target(con, data.frame(id = 1:2, score = c(10, 20)))

  incoming <- data.frame(
    id = c(2L, 3L),
    score = c(21, 30),
    label = c("b2", "c"),
    stringsAsFactors = FALSE
  )
  report <- fd_upsert(
    con = con,
    features_df = incoming,
    feat_table_name = "features_tbl",
    key = "id",
    alter_table = TRUE,
    verbose = FALSE
  )

  expect_identical(report$columns_added, "label")
  expect_identical(report$counts$would_insert, 1L)
  expect_identical(report$counts$would_update, 1L)
  expect_equal(DBI::dbListFields(con, "features_tbl"), c("id", "score", "label"))

  out <- fd_upsert_read_table(con)
  expect_equal(out$id, 1:3)
  expect_equal(out$score, c(10, 21, 30))
  expect_equal(out$label, c(NA, "b2", "c"))
})

test_that("fd_upsert reports extra target columns without mutating them", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  initial <- data.frame(
    id = 1:2,
    score = c(10, 20),
    retired = c("old-a", "old-b"),
    stringsAsFactors = FALSE
  )
  fd_upsert_create_target(con, initial)

  report <- fd_upsert(
    con = con,
    features_df = data.frame(id = c(2L, 3L), score = c(21, 30)),
    feat_table_name = "features_tbl",
    key = "id",
    verbose = FALSE
  )

  expect_identical(report$extra_columns, "retired")
  expect_identical(report$columns_added, character())
  expect_identical(report$counts$would_insert, 1L)
  expect_identical(report$counts$would_update, 1L)

  out <- fd_upsert_read_table(con)
  expect_equal(out$id, 1:3)
  expect_equal(out$score, c(10, 21, 30))
  expect_equal(out$retired, c("old-a", "old-b", NA))
})

test_that("fd_upsert errors clearly when upsert target lacks key constraint", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  DBI::dbWriteTable(
    con,
    "features_tbl",
    data.frame(id = 1:2, score = c(10, 20)),
    overwrite = TRUE
  )

  expect_error(
    fd_upsert(
      con = con,
      features_df = data.frame(id = c(2L, 3L), score = c(21, 30)),
      feat_table_name = "features_tbl",
      key = "id",
      update_table = TRUE,
      verbose = FALSE
    ),
    "PRIMARY KEY or UNIQUE constraint"
  )
})

test_that("fd_upsert errors on missing feature columns when alter_table is FALSE", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  initial <- data.frame(id = 1:2, score = c(10, 20))
  DBI::dbWriteTable(con, "features_tbl", initial, overwrite = TRUE)

  expect_error(
    fd_upsert(
      con = con,
      features_df = data.frame(id = 3L, score = 30, label = "c"),
      feat_table_name = "features_tbl",
      key = "id",
      alter_table = FALSE,
      verbose = FALSE
    ),
    "missing columns"
  )

  expect_equal(DBI::dbListFields(con, "features_tbl"), names(initial))
  expect_equal(fd_upsert_read_table(con), initial)
})

test_that("fd_upsert chunks writes and reports pre-merge insert/update counts", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  fd_upsert_create_target(con, data.frame(id = c(1L, 3L), score = c(10, 30)))

  incoming <- data.frame(
    id = c(1L, 2L, 3L, 4L, 5L),
    score = c(11, 20, 31, 40, 50)
  )
  report <- fd_upsert(
    con = con,
    features_df = incoming,
    feat_table_name = "features_tbl",
    key = "id",
    chunk_size = 2L,
    verbose = FALSE
  )

  expect_identical(report$n_chunks, 3L)
  expect_identical(report$counts$would_insert, 3L)
  expect_identical(report$counts$would_update, 2L)
  expect_equal(
    report$chunk_details,
    data.frame(
      chunk = 1:3,
      n = c(2L, 2L, 1L),
      would_insert = c(1L, 1L, 1L),
      would_update = c(1L, 1L, 0L)
    )
  )
  expect_equal(fd_upsert_read_table(con)$score, c(11, 20, 31, 40, 50))
})

test_that("fd_upsert handles empty feature frames with a structured report", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  features <- data.frame(id = integer(), score = numeric())

  report <- fd_upsert(
    con = con,
    features_df = features,
    feat_table_name = "features_tbl",
    key = "id",
    create_table = "auto",
    verbose = FALSE
  )

  expect_s3_class(report, "fd_upsert_report")
  expect_true(report$table_created)
  expect_identical(report$n_rows, 0L)
  expect_identical(report$n_chunks, 0L)
  expect_identical(report$counts$would_insert, 0L)
  expect_identical(report$counts$would_update, 0L)
  expect_equal(DBI::dbListFields(con, "features_tbl"), c("id", "score"))
  expect_equal(nrow(DBI::dbReadTable(con, "features_tbl")), 0L)
})

test_that("fd_upsert can run for side effects only", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  features <- data.frame(id = 1:2, score = c(10, 20))
  ret <- fd_upsert(
    con = con,
    features_df = features,
    feat_table_name = "features_tbl",
    key = "id",
    create_table = TRUE,
    verbose = FALSE,
    return_report = FALSE
  )

  expect_true(ret)
  expect_equal(fd_upsert_read_table(con), features)
})

test_that("fd_upsert refuses to overwrite an existing table with create_table TRUE", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  initial <- data.frame(id = 1L, score = 10)
  DBI::dbWriteTable(con, "features_tbl", initial, overwrite = TRUE)

  expect_error(
    fd_upsert(
      con = con,
      features_df = data.frame(id = 2L, score = 20),
      feat_table_name = "features_tbl",
      key = "id",
      create_table = TRUE,
      verbose = FALSE
    ),
    "create_table=TRUE but table already exists"
  )
  expect_equal(fd_upsert_read_table(con), initial)
})

test_that("fd_upsert errors when table is missing and create_table is FALSE", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  expect_error(
    fd_upsert(
      con = con,
      features_df = data.frame(id = 1L, score = 10),
      feat_table_name = "features_tbl",
      key = "id",
      create_table = FALSE,
      verbose = FALSE
    ),
    "Table does not exist"
  )
  expect_false(DBI::dbExistsTable(con, "features_tbl"))
})

test_that("fd_upsert validates feature keys and scalar controls", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  expect_error(
    fd_upsert(con, data.frame(other_id = 1L), "features_tbl", key = "id", verbose = FALSE),
    "`key` must be a column"
  )
  expect_error(
    fd_upsert(con, data.frame(id = c(1L, 1L)), "features_tbl", key = "id", verbose = FALSE),
    "duplicates"
  )
  expect_error(
    fd_upsert(con, data.frame(id = c(1L, NA_integer_)), "features_tbl", key = "id", verbose = FALSE),
    "must not contain NAs"
  )
  expect_error(
    fd_upsert(con, data.frame(id = 1L), "features_tbl", key = "id", create_table = "yes", verbose = FALSE),
    "create_table must be TRUE/FALSE, or 'auto'"
  )
  expect_error(
    fd_upsert(con, data.frame(id = 1L), "features_tbl", key = "id", chunk_size = 0, verbose = FALSE),
    "`chunk_size` must be NULL or a positive number"
  )
  expect_error(
    fd_upsert(con, data.frame(id = 1L), "features_tbl", key = "id", update_table = NA, verbose = FALSE),
    "update_table must be TRUE/FALSE"
  )
})

test_that("fd_upsert normalizes factor columns before writing", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  features <- data.frame(
    id = 1:2,
    label = factor(c("a", "b"))
  )
  fd_upsert(
    con = con,
    features_df = features,
    feat_table_name = "features_tbl",
    key = "id",
    create_table = TRUE,
    verbose = FALSE
  )

  out <- fd_upsert_read_table(con)
  expect_type(out$label, "character")
  expect_equal(out$label, c("a", "b"))
})

test_that("fd_upsert rolls back schema changes and data writes on failure", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  initial <- data.frame(id = 1:2, score = c(10, 20))
  DBI::dbWriteTable(con, "features_tbl", initial, overwrite = TRUE)

  expect_error(
    fd_upsert(
      con = con,
      features_df = data.frame(
        id = c(2L, 3L),
        score = c(21, 30),
        label = c("b2", "c"),
        stringsAsFactors = FALSE
      ),
      feat_table_name = "features_tbl",
      key = "id",
      alter_table = TRUE,
      update_table = FALSE,
      verbose = FALSE
    ),
    "Conflicts detected"
  )

  expect_equal(DBI::dbListFields(con, "features_tbl"), names(initial))
  expect_equal(fd_upsert_read_table(con), initial)
})

test_that("fd_upsert emits useful progress messages when verbose", {
  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  messages <- capture.output(
    report <- fd_upsert(
      con = con,
      features_df = data.frame(id = 1:2, score = c(10, 20)),
      feat_table_name = "features_tbl",
      key = "id",
      create_table = TRUE,
      verbose = TRUE
    ),
    type = "message"
  )

  expect_true(any(grepl("Creating table: features_tbl", messages, fixed = TRUE)))
  expect_true(any(grepl("Writing features to features_tbl", messages, fixed = TRUE)))
  expect_true(any(grepl("Done. would_insert=2, would_update=0", messages, fixed = TRUE)))
  expect_s3_class(report, "fd_upsert_report")
  expect_true(DBI::dbExistsTable(con, "features_tbl"))
})
