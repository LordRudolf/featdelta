test_that("fd_define constructs ordered column definitions from dots", {
  defs <- fd_define(
    ratio = x / y,
    doubled = ratio * 2,
    description = "basic definitions"
  )

  expect_s3_class(defs, "featdelta_defs")
  expect_named(defs$steps, c("ratio", "doubled"))
  expect_identical(defs$description, "basic definitions")
  expect_identical(defs$defs_version, 2L)
  expect_identical(defs$envir_policy, "caller")
  expect_null(defs$envir)
  expect_s3_class(defs$created_at, "POSIXct")

  expect_identical(defs$steps$ratio$type, "column")
  expect_identical(defs$steps$ratio$mode, "expression")
  expect_identical(defs$steps$ratio$step_name, "ratio")
  expect_identical(defs$steps$ratio$output_names, "ratio")
  expect_true(rlang::is_quosure(defs$steps$ratio$quo))
  expect_true(is.environment(defs$steps$ratio$env))

  expect_identical(defs$steps$doubled$type, "column")
  expect_identical(defs$steps$doubled$step_name, "doubled")
  expect_identical(defs$steps$doubled$output_names, "doubled")
})

test_that("fd_define accepts programmatic named defs inputs", {
  defs <- fd_define(defs = list(
    ratio = expression(x / y),
    total = quote(x + y),
    constant = 1,
    label = "ok"
  ))

  expect_s3_class(defs, "featdelta_defs")
  expect_named(defs$steps, c("ratio", "total", "constant", "label"))
  expect_identical(
    vapply(defs$steps, `[[`, character(1), "type"),
    c(ratio = "column", total = "column", constant = "column", label = "column")
  )
  expect_match(defs$steps$ratio$text, "x")
  expect_match(defs$steps$ratio$text, "y")
  expect_match(defs$steps$total$text, "x")
  expect_match(defs$steps$total$text, "y")
  expect_identical(defs$steps$constant$text, "1")
  expect_match(defs$steps$label$text, "ok")
})

test_that("fd_define unwraps captured symbols that hold quoted expressions", {
  ratio_expr <- expression(x / y)
  total_call <- quote(x + y)

  defs <- fd_define(
    ratio = ratio_expr,
    total = total_call
  )

  expect_named(defs$steps, c("ratio", "total"))
  expect_match(defs$steps$ratio$text, "x")
  expect_match(defs$steps$ratio$text, "y")
  expect_false(grepl("ratio_expr", defs$steps$ratio$text, fixed = TRUE))
  expect_match(defs$steps$total$text, "x")
  expect_match(defs$steps$total$text, "y")
  expect_false(grepl("total_call", defs$steps$total$text, fixed = TRUE))
})

test_that("fd_define uses explicit environments when supplied", {
  env <- new.env(parent = baseenv())
  env$offset <- 10

  defs <- fd_define(
    shifted = x + offset,
    envir = env
  )

  expect_identical(defs$envir_policy, "explicit")
  expect_identical(defs$envir, env)
  expect_identical(defs$steps$shifted$env, env)
})

test_that("fd_define handles duplicate names according to overwrite", {
  expect_error(
    fd_define(score = x, score = y),
    "Duplicate definition step"
  )

  defs <- fd_define(score = x, score = y, overwrite = TRUE)

  expect_named(defs$steps, "score")
  expect_length(defs$steps, 1L)
  expect_match(defs$steps$score$text, "y")
  expect_false(grepl("^x$", defs$steps$score$text))
})

test_that("fd_define rejects malformed construction inputs", {
  expect_error(fd_define(), "No definitions supplied")
  expect_error(fd_define(x), "All definition steps")
  expect_error(fd_define(defs = 1), "`defs` must be a list")
  expect_error(fd_define(defs = list(1)), "`defs` must be a \\*named\\* list")
  expect_error(fd_define(.key = x), "Reserved definition step name")
  expect_error(fd_define(defs = list(value = 1:2)), "Direct constants must be scalar")
  expect_error(fd_define(defs = list(value = expression(x, y))), "exactly one expression")
  expect_error(fd_define(value = x, description = c("a", "b")), "`description`")
  expect_error(fd_define(value = x, overwrite = NA), "`overwrite`")
  expect_error(fd_define(value = x, envir = list()), "`envir`")
})

test_that("summary.featdelta_defs returns one row per definition step", {
  defs <- fd_define(
    ratio = x / y,
    constant = 1
  )

  out <- summary(defs)

  expect_s3_class(out, "data.frame")
  expect_named(out, c("step_name", "type", "mode", "outputs", "text"))
  expect_equal(out$step_name, c("ratio", "constant"))
  expect_equal(out$type, c("column", "column"))
  expect_equal(out$mode, c("expression", "expression"))
  expect_equal(out$outputs, c("ratio", "constant"))
  expect_true(all(nzchar(out$text)))
})

test_that("print.featdelta_defs returns input invisibly and shows key fields", {
  defs <- fd_define(
    ratio = x / y,
    description = "printable definitions"
  )

  printed <- capture.output(ret <- print(defs))

  expect_identical(ret, defs)
  expect_true(any(grepl("<featdelta_defs>", printed, fixed = TRUE)))
  expect_true(any(grepl("Description: printable definitions", printed, fixed = TRUE)))
  expect_true(any(grepl("Definition steps (1):", printed, fixed = TRUE)))
  expect_true(any(grepl("[column] ratio", printed, fixed = TRUE)))
})
