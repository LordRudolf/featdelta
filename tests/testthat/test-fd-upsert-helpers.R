upsert_ns <- function(name) {
  getFromNamespace(name, "featdelta")
}

test_that("render_sql replaces placeholders and rejects invalid values", {
  render_sql <- upsert_ns("render_sql")

  out <- render_sql(
    "INSERT INTO {target} ({cols}) SELECT {cols} FROM {stage}",
    list(
      target = "`features_tbl`",
      cols = "`id`, `score`",
      stage = "`stage_tbl`"
    )
  )

  expect_identical(
    out,
    "INSERT INTO `features_tbl` (`id`, `score`) SELECT `id`, `score` FROM `stage_tbl`"
  )

  expect_error(
    render_sql("SELECT {missing}", list(missing = NULL)),
    "invalid replacement value"
  )
  expect_error(
    render_sql("SELECT {missing}", list(missing = c("a", "b"))),
    "invalid replacement value"
  )
  expect_error(
    render_sql("SELECT {missing}", list(missing = NA_character_)),
    "invalid replacement value"
  )
})

test_that("fd_upsert_build_merge_sql renders SQLite upsert SQL", {
  build_merge_sql <- upsert_ns("fd_upsert_build_merge_sql")

  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  sql <- build_merge_sql(
    con = con,
    dialect = "sqlite",
    target_q = "`features_tbl`",
    stage_q = "`stage_tbl`",
    key = "id",
    cols = c("id", "score", "flag"),
    update_table = TRUE
  )

  expect_match(sql, "INSERT INTO `features_tbl` (`id`, `score`, `flag`)", fixed = TRUE)
  expect_match(sql, "SELECT `id`, `score`, `flag` FROM `stage_tbl`", fixed = TRUE)
  expect_match(sql, "ON CONFLICT(`id`) DO UPDATE SET", fixed = TRUE)
  expect_match(sql, "`score` = excluded.`score`", fixed = TRUE)
  expect_match(sql, "`flag` = excluded.`flag`", fixed = TRUE)
})

test_that("fd_upsert_build_merge_sql renders SQLite insert-only SQL", {
  build_merge_sql <- upsert_ns("fd_upsert_build_merge_sql")

  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  sql <- build_merge_sql(
    con = con,
    dialect = "sqlite",
    target_q = "`features_tbl`",
    stage_q = "`stage_tbl`",
    key = "id",
    cols = c("id", "score"),
    update_table = FALSE
  )

  expect_match(sql, "INSERT INTO `features_tbl` (`id`, `score`)", fixed = TRUE)
  expect_match(sql, "SELECT s.`id`, s.`score`", fixed = TRUE)
  expect_match(sql, "FROM `stage_tbl` s", fixed = TRUE)
  expect_match(sql, "LEFT JOIN `features_tbl` t ON t.`id` = s.`id`", fixed = TRUE)
  expect_match(sql, "WHERE t.`id` IS NULL", fixed = TRUE)
  expect_false(grepl("ON CONFLICT", sql, fixed = TRUE))
})

test_that("fd_upsert_build_merge_sql renders MySQL upsert SQL", {
  build_merge_sql <- upsert_ns("fd_upsert_build_merge_sql")

  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  sql <- build_merge_sql(
    con = con,
    dialect = "mysql",
    target_q = "`features_tbl`",
    stage_q = "`stage_tbl`",
    key = "id",
    cols = c("id", "score", "flag"),
    update_table = TRUE
  )

  expect_match(sql, "INSERT INTO `features_tbl` (`id`, `score`, `flag`)", fixed = TRUE)
  expect_match(sql, "FROM (SELECT `id`, `score`, `flag` FROM `stage_tbl`) AS fd_src", fixed = TRUE)
  expect_match(sql, "ON DUPLICATE KEY UPDATE", fixed = TRUE)
  expect_match(sql, "`score` = fd_src.`score`", fixed = TRUE)
  expect_match(sql, "`flag` = fd_src.`flag`", fixed = TRUE)
  expect_false(grepl("VALUES(", sql, fixed = TRUE))
})

test_that("all SQL templates include supported dialects", {
  fd_sql_templates <- upsert_ns("fd_sql_templates")
  supported_dialects <- upsert_ns("supported_dialects")

  expect_true(all(vapply(fd_sql_templates, function(template) {
    all(supported_dialects() %in% names(template))
  }, logical(1))))
})

test_that("fd_upsert_build_merge_sql handles key-only updates", {
  build_merge_sql <- upsert_ns("fd_upsert_build_merge_sql")

  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  sql <- build_merge_sql(
    con = con,
    dialect = "sqlite",
    target_q = "`features_tbl`",
    stage_q = "`stage_tbl`",
    key = "id",
    cols = "id",
    update_table = TRUE
  )

  expect_match(sql, "ON CONFLICT(`id`) DO UPDATE SET `id` = excluded.`id`", fixed = TRUE)
})

test_that("fd_upsert report constructors return structured report objects", {
  empty_report <- upsert_ns("fd_upsert_empty_report")
  build_report <- upsert_ns("fd_upsert_build_report")

  empty <- empty_report(
    feat_table_name = "features_tbl",
    key = "id",
    dialect = "sqlite",
    table_created = TRUE,
    columns_added = "score"
  )

  expect_s3_class(empty, "fd_upsert_report")
  expect_identical(empty$feat_table_name, "features_tbl")
  expect_identical(empty$key, "id")
  expect_identical(empty$dialect, "sqlite")
  expect_identical(empty$n_rows, 0L)
  expect_identical(empty$n_chunks, 0L)
  expect_true(empty$table_created)
  expect_identical(empty$columns_added, "score")
  expect_identical(empty$counts$would_insert, 0L)
  expect_identical(empty$counts$would_update, 0L)
  expect_equal(nrow(empty$chunk_details), 0L)

  chunk_details <- data.frame(
    chunk = 1:2,
    n = c(2L, 3L),
    would_insert = c(2L, 1L),
    would_update = c(0L, 2L)
  )
  report <- build_report(
    feat_table_name = "features_tbl",
    key = "id",
    dialect = "sqlite",
    n_rows = 5,
    n_chunks = 2,
    table_created = FALSE,
    columns_added = character(),
    totals_would_insert = 3,
    totals_would_update = 2,
    chunk_details = chunk_details
  )

  expect_s3_class(report, "fd_upsert_report")
  expect_identical(report$n_rows, 5L)
  expect_identical(report$n_chunks, 2L)
  expect_false(report$table_created)
  expect_identical(report$counts$would_insert, 3L)
  expect_identical(report$counts$would_update, 2L)
  expect_equal(report$chunk_details, chunk_details)
})

test_that("normalize_features_df converts factors and preserves date-like columns", {
  normalize_features_df <- upsert_ns("normalize_features_df")

  df <- data.frame(
    id = 1:2,
    group = factor(c("a", "b")),
    day = as.Date(c("2026-05-01", "2026-05-02")),
    stringsAsFactors = FALSE
  )
  df$time <- as.POSIXct(c("2026-05-01", "2026-05-02"), tz = "UTC")

  out <- normalize_features_df(df)

  expect_type(out$group, "character")
  expect_identical(out$group, c("a", "b"))
  expect_s3_class(out$day, "Date")
  expect_s3_class(out$time, "POSIXct")
})

test_that("get_stage_name creates unique dialect-safe names", {
  get_stage_name <- upsert_ns("get_stage_name")

  names <- replicate(20L, get_stage_name(3L))

  expect_true(all(grepl("^fd_stage_[0-9]{14}_3_[A-Za-z0-9]{8}$", names)))
  expect_equal(length(unique(names)), length(names))
})

test_that("make_chunks partitions row indices", {
  make_chunks <- upsert_ns("make_chunks")

  expect_equal(make_chunks(0L, NULL), list(integer()))
  expect_equal(make_chunks(5L, NULL), list(1:5))
  expect_equal(unname(make_chunks(5L, 2L)), list(1:2, 3:4, 5L))
  expect_equal(unname(make_chunks(3L, 10L)), list(1:3))
})

test_that("create_features_table creates a SQLite table with expected fields", {
  create_features_table <- upsert_ns("create_features_table")

  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  features <- data.frame(
    id = 1:2,
    score = c(10.5, 20.5),
    flag = c(TRUE, FALSE),
    label = c("a", "b"),
    stringsAsFactors = FALSE
  )

  create_features_table(
    con = con,
    dialect = "sqlite",
    feat_table_name = "features_tbl",
    target_q = "`features_tbl`",
    features_df = features,
    key = "id"
  )

  expect_true(DBI::dbExistsTable(con, "features_tbl"))
  expect_equal(DBI::dbListFields(con, "features_tbl"), names(features))

  DBI::dbAppendTable(con, "features_tbl", features)
  roundtrip <- DBI::dbReadTable(con, "features_tbl")
  expect_equal(roundtrip$id, features$id)
  expect_equal(roundtrip$score, features$score)
  expect_equal(roundtrip$flag, as.integer(features$flag))
  expect_equal(roundtrip$label, features$label)
  expect_error(DBI::dbAppendTable(con, "features_tbl", features[1, , drop = FALSE]))
})

test_that("ensure_columns returns no additions when target schema already matches", {
  ensure_columns <- upsert_ns("ensure_columns")

  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  features <- data.frame(id = 1:2, score = c(10, 20))
  DBI::dbWriteTable(con, "features_tbl", features[0, , drop = FALSE], overwrite = TRUE)

  added <- ensure_columns(
    con = con,
    dialect = "sqlite",
    table_id = "features_tbl",
    table_q = "`features_tbl`",
    features_df = features,
    key = "id",
    alter_table = TRUE,
    verbose = FALSE
  )

  expect_identical(added, character())
})

test_that("ensure_columns adds missing columns when allowed", {
  ensure_columns <- upsert_ns("ensure_columns")

  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  DBI::dbWriteTable(
    con,
    "features_tbl",
    data.frame(id = integer(), score = numeric()),
    overwrite = TRUE
  )
  features <- data.frame(
    id = 1:2,
    score = c(10, 20),
    label = c("a", "b"),
    flag = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  added <- ensure_columns(
    con = con,
    dialect = "sqlite",
    table_id = "features_tbl",
    table_q = "`features_tbl`",
    features_df = features,
    key = "id",
    alter_table = TRUE,
    verbose = FALSE
  )

  expect_identical(added, c("label", "flag"))
  expect_equal(DBI::dbListFields(con, "features_tbl"), names(features))
})

test_that("ensure_columns errors for missing columns when alteration is disabled", {
  ensure_columns <- upsert_ns("ensure_columns")

  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  DBI::dbWriteTable(
    con,
    "features_tbl",
    data.frame(id = integer(), score = numeric()),
    overwrite = TRUE
  )

  expect_error(
    ensure_columns(
      con = con,
      dialect = "sqlite",
      table_id = "features_tbl",
      table_q = "`features_tbl`",
      features_df = data.frame(id = 1L, score = 10, label = "a"),
      key = "id",
      alter_table = FALSE,
      verbose = FALSE
    ),
    "missing columns"
  )
})

test_that("ensure_columns errors when target table lacks the key column", {
  ensure_columns <- upsert_ns("ensure_columns")

  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  DBI::dbWriteTable(
    con,
    "features_tbl",
    data.frame(other_id = integer(), score = numeric()),
    overwrite = TRUE
  )

  expect_error(
    ensure_columns(
      con = con,
      dialect = "sqlite",
      table_id = "features_tbl",
      table_q = "`features_tbl`",
      features_df = data.frame(id = 1L, score = 10),
      key = "id",
      alter_table = TRUE,
      verbose = FALSE
    ),
    "does not contain key column"
  )
})

test_that("count_scalars counts staged inserts and updates", {
  count_scalars <- upsert_ns("count_scalars")
  fd_sql_templates <- upsert_ns("fd_sql_templates")

  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  DBI::dbWriteTable(
    con,
    "features_tbl",
    data.frame(id = c(1L, 3L), score = c(10, 30)),
    overwrite = TRUE
  )
  DBI::dbWriteTable(
    con,
    "stage_tbl",
    data.frame(id = c(2L, 3L, 4L), score = c(20, 31, 40)),
    overwrite = TRUE
  )

  would_insert <- count_scalars(
    con,
    fd_sql_templates$upsert_count_would_insert$sqlite,
    target_q = "`features_tbl`",
    stage_q = "`stage_tbl`",
    key_q = "`id`"
  )
  would_update <- count_scalars(
    con,
    fd_sql_templates$upsert_count_would_update$sqlite,
    target_q = "`features_tbl`",
    stage_q = "`stage_tbl`",
    key_q = "`id`"
  )

  expect_identical(would_insert, 2L)
  expect_identical(would_update, 1L)
})

test_that("find_conflicts returns staged keys already present in target", {
  find_conflicts <- upsert_ns("find_conflicts")

  con <- fd_test_sqlite_con()
  on.exit(fd_test_disconnect(con), add = TRUE)

  DBI::dbWriteTable(
    con,
    "features_tbl",
    data.frame(id = c(1L, 3L, 5L), score = c(10, 30, 50)),
    overwrite = TRUE
  )
  DBI::dbWriteTable(
    con,
    "stage_tbl",
    data.frame(id = c(2L, 3L, 5L, 6L), score = c(20, 31, 51, 60)),
    overwrite = TRUE
  )

  conflicts <- find_conflicts(
    con = con,
    dialect = "sqlite",
    target_q = "`features_tbl`",
    stage_q = "`stage_tbl`",
    key_q = "`id`",
    limit = 1L
  )
  all_conflicts <- find_conflicts(
    con = con,
    dialect = "sqlite",
    target_q = "`features_tbl`",
    stage_q = "`stage_tbl`",
    key_q = "`id`",
    limit = 50L
  )

  expect_length(conflicts, 1L)
  expect_true(conflicts %in% c("3", "5"))
  expect_setequal(all_conflicts, c("3", "5"))
})
