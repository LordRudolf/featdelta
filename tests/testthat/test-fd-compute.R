test_that("fd_compute evaluates ordinary column definitions in order", {
  raw <- fd_test_raw_data()
  defs <- fd_define(
    ratio = x / y,
    doubled = ratio * 2,
    shifted = doubled + 1
  )

  out <- fd_compute(raw, defs, key = "id")

  expect_s3_class(out, "featdelta_features")
  expect_s3_class(out, "data.frame")
  expect_named(out, c("id", "ratio", "doubled", "shifted"))
  expect_identical(out$id, raw$id)
  expect_equal(out$ratio, raw$x / raw$y)
  expect_equal(out$doubled, (raw$x / raw$y) * 2)
  expect_equal(out$shifted, ((raw$x / raw$y) * 2) + 1)
  expect_identical(attr(out, "fd_key"), "id")
})

test_that("fd_compute recycles scalar column results to all input rows", {
  raw <- fd_test_raw_data()
  defs <- fd_define(
    one = 1,
    label = "constant"
  )

  out <- fd_compute(raw, defs, key = "id")

  expect_named(out, c("id", "one", "label"))
  expect_identical(out$one, rep(1, nrow(raw)))
  expect_identical(out$label, rep("constant", nrow(raw)))
})

test_that("fd_compute uses compute_envir as an evaluation override", {
  raw <- fd_test_raw_data()
  defs_env <- new.env(parent = baseenv())
  defs_env$offset <- 10

  compute_env <- new.env(parent = baseenv())
  compute_env$offset <- 100

  defs <- fd_define(
    shifted = x + offset,
    envir = defs_env
  )

  out_stored <- fd_compute(raw, defs, key = "id")
  out_override <- fd_compute(raw, defs, key = "id", compute_envir = compute_env)

  expect_equal(out_stored$shifted, raw$x + 10)
  expect_equal(out_override$shifted, raw$x + 100)
})

test_that("fd_compute returns a report when requested", {
  raw <- fd_test_raw_data()
  defs <- fd_define(
    ratio = x / y,
    doubled = ratio * 2
  )

  res <- fd_compute(raw, defs, key = "id", return_report = TRUE)

  expect_s3_class(res, "featdelta_compute_result")
  expect_named(res, c("data", "report"))
  expect_s3_class(res$data, "featdelta_features")
  expect_s3_class(res$report, "data.frame")
  expect_named(
    res$report,
    c(
      "step_name", "type", "mode", "ok", "error", "error_raw",
      "error_class", "hint", "time_sec", "text", "output_names"
    )
  )
  expect_equal(res$report$step_name, c("ratio", "doubled"))
  expect_equal(res$report$type, c("column", "column"))
  expect_true(all(res$report$ok))
  expect_equal(res$report$output_names, c("ratio", "doubled"))
  expect_identical(attr(res$data, "fd_report"), res$report)
})

test_that("fd_compute attaches a report when returning only data", {
  raw <- fd_test_raw_data()
  defs <- fd_define(ratio = x / y)

  out <- fd_compute(raw, defs, key = "id")
  report <- attr(out, "fd_report")

  expect_s3_class(report, "data.frame")
  expect_equal(report$step_name, "ratio")
  expect_true(report$ok)
  expect_equal(report$output_names, "ratio")
})

test_that("fd_compute handles zero-row input with scalar definitions", {
  raw <- fd_test_raw_data()[0, , drop = FALSE]
  defs <- fd_define(
    one = 1,
    ratio = x / y
  )

  out <- fd_compute(raw, defs, key = "id")

  expect_s3_class(out, "featdelta_features")
  expect_equal(nrow(out), 0L)
  expect_named(out, c("id", "one", "ratio"))
  expect_identical(out$id, integer())
  expect_identical(out$one, numeric())
  expect_identical(out$ratio, numeric())
})

test_that("fd_compute validates data and key inputs", {
  raw <- fd_test_raw_data()
  defs <- fd_define(ratio = x / y)

  expect_error(fd_compute(list(id = 1), defs, key = "id"), "`data` must be")
  expect_error(fd_compute(raw, defs, key = "missing_id"), "`key` must be a column")
  expect_error(fd_compute(transform(raw, id = c(1, 1, 3, 4, 5)), defs, key = "id"), "duplicates")
  expect_error(fd_compute(transform(raw, id = c(1, NA, 3, 4, 5)), defs, key = "id"), "must not contain NAs")
  expect_error(fd_compute(raw, list(), key = "id"), "`defs` must be")
  expect_error(fd_compute(raw, defs, key = ""), "`key` must be")
  expect_error(fd_compute(raw, defs, key = "id", compute_envir = list()), "`compute_envir`")
})

test_that("fd_compute strict mode raises a compute error with report and partial data", {
  raw <- fd_test_raw_data()
  defs <- fd_define(
    ratio = x / y,
    bad = missing_column + 1
  )

  err <- expect_error(
    fd_compute(raw, defs, key = "id", compute_strict = TRUE),
    class = "featdelta_compute_error"
  )

  report <- attr(err, "fd_report")
  partial <- attr(err, "fd_partial")

  expect_s3_class(report, "data.frame")
  expect_equal(report$step_name, c("ratio", "bad"))
  expect_equal(report$ok, c(TRUE, FALSE))
  expect_equal(report$error_class[[2]], "missing_object")
  expect_match(report$hint[[2]], "raw input column")

  expect_s3_class(partial, "featdelta_features")
  expect_named(partial, c("id", "ratio"))
  expect_equal(partial$ratio, raw$x / raw$y)
})

test_that("fd_compute non-strict mode records failed column steps as NA outputs", {
  raw <- fd_test_raw_data()
  defs <- fd_define(
    ratio = x / y,
    bad = missing_column + 1,
    after = ratio + 1
  )

  expect_warning(
    out <- fd_compute(raw, defs, key = "id", compute_strict = FALSE),
    "step 'bad' failed"
  )

  report <- attr(out, "fd_report")

  expect_named(out, c("id", "ratio", "bad", "after"))
  expect_equal(out$ratio, raw$x / raw$y)
  expect_true(all(is.na(out$bad)))
  expect_equal(out$after, (raw$x / raw$y) + 1)
  expect_equal(report$step_name, c("ratio", "bad", "after"))
  expect_equal(report$ok, c(TRUE, FALSE, TRUE))
  expect_equal(report$error_class[[2]], "missing_object")
})

test_that("fd_compute rejects unsupported and mis-sized column results", {
  raw <- fd_test_raw_data()

  expect_error(
    fd_compute(raw, fd_define(bad = list(1, 2, 3, 4, 5)), key = "id"),
    class = "featdelta_compute_error"
  )

  expect_error(
    fd_compute(raw, fd_define(bad = c(1, 2)), key = "id"),
    class = "featdelta_compute_error"
  )
})
