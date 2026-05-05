fd_ns <- function(name) {
  getFromNamespace(name, "featdelta")
}

test_that("fd_compute_validate_defs_structure accepts valid column definitions", {
  validate_defs <- fd_ns("fd_compute_validate_defs_structure")

  defs <- fd_define(
    ratio = x / y,
    shifted = ratio + 1
  )

  expect_true(validate_defs(defs, key = "id"))
})

test_that("fd_compute_validate_defs_structure rejects malformed defs containers", {
  validate_defs <- fd_ns("fd_compute_validate_defs_structure")
  defs <- fd_define(ratio = x / y)

  empty_defs <- defs
  empty_defs$steps <- list()
  expect_error(validate_defs(empty_defs, key = "id"), "non-empty list")

  unnamed_defs <- defs
  names(unnamed_defs$steps) <- NULL
  expect_error(validate_defs(unnamed_defs, key = "id"), "named list")

  duplicate_defs <- defs
  duplicate_defs$steps <- c(defs$steps, defs$steps)
  names(duplicate_defs$steps) <- c("ratio", "ratio")
  expect_error(validate_defs(duplicate_defs, key = "id"), "duplicate step names")

  non_list_step <- defs
  non_list_step$steps$ratio <- "not a step"
  expect_error(validate_defs(non_list_step, key = "id"), "must be stored as a list")
})

test_that("fd_compute_validate_defs_structure rejects malformed column steps", {
  validate_defs <- fd_ns("fd_compute_validate_defs_structure")
  defs <- fd_define(ratio = x / y)

  bad_type <- defs
  bad_type$steps$ratio$type <- NA_character_
  expect_error(validate_defs(bad_type, key = "id"), "malformed `type`")

  bad_name <- defs
  bad_name$steps$ratio$step_name <- "other"
  expect_error(validate_defs(bad_name, key = "id"), "inconsistent `step_name`")

  key_collision <- defs
  key_collision$steps$ratio$output_names <- "id"
  expect_error(validate_defs(key_collision, key = "id"), "collide with `key`")

  bad_env <- defs
  bad_env$steps$ratio$env <- NULL
  expect_error(validate_defs(bad_env, key = "id"), "malformed `env`")

  bad_text <- defs
  bad_text$steps$ratio$text <- NA_character_
  expect_error(validate_defs(bad_text, key = "id"), "malformed `text`")

  bad_quo <- defs
  bad_quo$steps$ratio$quo <- quote(x / y)
  expect_error(validate_defs(bad_quo, key = "id"), "malformed `quo`")

  bad_output <- defs
  bad_output$steps$ratio$output_names <- c("ratio", "extra")
  expect_error(validate_defs(bad_output, key = "id"), "exactly one output name")
})

test_that("fd_compute_validate_defs_structure rejects malformed block steps", {
  validate_defs <- fd_ns("fd_compute_validate_defs_structure")
  defs <- fd_define(block = fd_block({ data.frame(value = x + y) }))

  bad_mode <- defs
  bad_mode$steps$block$mode <- NA_character_
  expect_error(validate_defs(bad_mode, key = "id"), "malformed `mode`")

  bad_fn <- defs
  bad_fn$steps$block$mode <- "function"
  bad_fn$steps$block$fn <- NULL
  expect_error(validate_defs(bad_fn, key = "id"), "malformed `fn`")

  bad_quo <- defs
  bad_quo$steps$block$quo <- quote(data.frame(value = x + y))
  expect_error(validate_defs(bad_quo, key = "id"), "malformed `quo`")

  unsupported_mode <- defs
  unsupported_mode$steps$block$mode <- "other"
  expect_error(validate_defs(unsupported_mode, key = "id"), "unsupported `mode`")

  duplicate_outputs <- defs
  duplicate_outputs$steps$block$output_names <- c("a", "a")
  expect_error(validate_defs(duplicate_outputs, key = "id"), "duplicate `output_names`")
})

test_that("fd_compute_eval_step reports unsupported step types", {
  eval_step <- fd_ns("fd_compute_eval_step")

  res <- eval_step(
    step = list(type = "unknown"),
    step_name = "bad",
    working_data = fd_test_raw_data(),
    mask = fd_test_raw_data(),
    n = 5L,
    key = "id",
    produced_names = character()
  )

  expect_false(res$ok)
  expect_match(res$error, "Unsupported step type")
  expect_equal(res$cols, list())
})

test_that("fd_compute_eval_column_step returns columns or diagnostics", {
  eval_column <- fd_ns("fd_compute_eval_column_step")
  raw <- fd_test_raw_data()
  step <- fd_define(ratio = x / y)$steps$ratio

  ok <- eval_column(
    step = step,
    step_name = "ratio",
    mask = raw,
    n = nrow(raw)
  )

  expect_true(ok$ok)
  expect_named(ok$cols, "ratio")
  expect_equal(ok$cols$ratio, raw$x / raw$y)

  bad_step <- fd_define(bad = missing_column + 1)$steps$bad
  bad <- eval_column(
    step = bad_step,
    step_name = "bad",
    mask = raw,
    n = nrow(raw),
    compute_strict = TRUE
  )

  expect_false(bad$ok)
  expect_named(bad$cols, "bad")
  expect_true(all(is.na(bad$cols$bad)))
  expect_equal(bad$error_class, "missing_object")
  expect_match(bad$hint, "raw input column")
})

test_that("fd_compute_eval_block_step returns block columns or declared fallbacks", {
  eval_block <- fd_ns("fd_compute_eval_block_step")
  raw <- fd_test_raw_data()
  step <- fd_define(
    block = fd_block({
      data.frame(sum_xy = x + y)
    })
  )$steps$block

  ok <- eval_block(
    step = step,
    step_name = "block",
    working_data = raw,
    mask = raw,
    n = nrow(raw),
    key = "id",
    produced_names = character()
  )

  expect_true(ok$ok)
  expect_named(ok$cols, "sum_xy")
  expect_equal(ok$cols$sum_xy, raw$x + raw$y)

  bad_step <- fd_define(
    block = fd_block(
      {
        missing_column + 1
      },
      expected_names = c("a", "b")
    )
  )$steps$block
  expect_warning(
    bad <- eval_block(
      step = bad_step,
      step_name = "block",
      working_data = raw,
      mask = raw,
      n = nrow(raw),
      key = "id",
      produced_names = character(),
      compute_strict = FALSE
    ),
    "step 'block' failed"
  )

  expect_false(bad$ok)
  expect_named(bad$cols, c("a", "b"))
  expect_true(all(is.na(bad$cols$a)))
  expect_true(all(is.na(bad$cols$b)))
  expect_equal(bad$error_class, "missing_object")
})

test_that("fd_compute_validate_column_result validates supported result shapes", {
  validate_column <- fd_ns("fd_compute_validate_column_result")

  scalar <- validate_column(1, n = 3L, name = "one")
  expect_true(scalar$ok)
  expect_identical(scalar$result, c(1, 1, 1))

  vector <- validate_column(c(1, 2, 3), n = 3L, name = "value")
  expect_true(vector$ok)
  expect_identical(vector$result, c(1, 2, 3))

  zero_scalar <- validate_column(1, n = 0L, name = "one")
  expect_true(zero_scalar$ok)
  expect_identical(zero_scalar$result, numeric())

  zero_empty <- validate_column(integer(), n = 0L, name = "empty")
  expect_true(zero_empty$ok)
  expect_identical(zero_empty$result, integer())
})

test_that("fd_compute_validate_column_result rejects invalid outputs", {
  validate_column <- fd_ns("fd_compute_validate_column_result")

  null_res <- validate_column(NULL, n = 3L, name = "bad")
  expect_false(null_res$ok)
  expect_match(null_res$error, "is NULL")

  list_res <- validate_column(list(1, 2, 3), n = 3L, name = "bad")
  expect_false(list_res$ok)
  expect_match(list_res$error, "unsupported type/class")

  short_res <- validate_column(c(1, 2), n = 3L, name = "bad")
  expect_false(short_res$ok)
  expect_match(short_res$error, "expected 3")

  zero_long <- validate_column(c(1, 2), n = 0L, name = "bad")
  expect_false(zero_long$ok)
  expect_match(zero_long$error, "expected 0")
})

test_that("fd_compute_validate_block_result validates expected-name completion", {
  validate_block <- fd_ns("fd_compute_validate_block_result")

  res <- validate_block(
    res = data.frame(b = c(2, 4), stringsAsFactors = FALSE),
    n = 2L,
    step_name = "block",
    key = "id",
    produced_names = "existing",
    expected_names = c("a", "b")
  )

  expect_true(res$ok)
  expect_named(res$result, c("a", "b"))
  expect_true(all(is.na(res$result$a)))
  expect_equal(res$result$b, c(2, 4))
})

test_that("fd_compute_validate_block_result rejects malformed block outputs", {
  validate_block <- fd_ns("fd_compute_validate_block_result")
  valid_args <- list(
    n = 2L,
    step_name = "block",
    key = "id",
    produced_names = character(),
    expected_names = NULL
  )

  expect_false(do.call(validate_block, c(list(res = NULL), valid_args))$ok)
  expect_false(do.call(validate_block, c(list(res = c(1, 2)), valid_args))$ok)
  expect_false(do.call(validate_block, c(list(res = data.frame(a = 1)), valid_args))$ok)

  unnamed <- data.frame(1:2)
  names(unnamed) <- ""
  unnamed_res <- do.call(validate_block, c(list(res = unnamed), valid_args))
  expect_false(unnamed_res$ok)
  expect_match(unnamed_res$error, "unnamed or empty-named")

  duplicate <- data.frame(a = 1:2, b = 3:4)
  names(duplicate) <- c("dup", "dup")
  duplicate_res <- do.call(validate_block, c(list(res = duplicate), valid_args))
  expect_false(duplicate_res$ok)
  expect_match(duplicate_res$error, "duplicate column names")

  key_collision <- do.call(
    validate_block,
    c(list(res = data.frame(id = 1:2)), valid_args)
  )
  expect_false(key_collision$ok)
  expect_match(key_collision$error, "colliding with `key`")

  earlier_collision <- validate_block(
    res = data.frame(existing = 1:2),
    n = 2L,
    step_name = "block",
    key = "id",
    produced_names = "existing"
  )
  expect_false(earlier_collision$ok)
  expect_match(earlier_collision$error, "already produced earlier")

  unexpected <- validate_block(
    res = data.frame(extra = 1:2),
    n = 2L,
    step_name = "block",
    key = "id",
    produced_names = character(),
    expected_names = "allowed"
  )
  expect_false(unexpected$ok)
  expect_match(unexpected$error, "unexpected column name")

  bad_column <- do.call(
    validate_block,
    c(list(res = data.frame(a = I(list(1, 2)))), valid_args)
  )
  expect_false(bad_column$ok)
  expect_match(bad_column$error, "unsupported type/class")
})

test_that("fd_compute_complete_expected_block_outputs fills missing columns in declared order", {
  complete_outputs <- fd_ns("fd_compute_complete_expected_block_outputs")

  out <- complete_outputs(
    res = data.frame(b = c(2, 4), stringsAsFactors = FALSE),
    expected_names = c("a", "b", "c"),
    n = 2L
  )

  expect_named(out, c("a", "b", "c"))
  expect_true(all(is.na(out$a)))
  expect_equal(out$b, c(2, 4))
  expect_true(all(is.na(out$c)))
})

test_that("fd_compute_call_block_function supports zero- and one-argument functions", {
  call_block <- fd_ns("fd_compute_call_block_function")
  raw <- fd_test_raw_data()

  no_arg <- function() {
    data.frame(a = 1:2)
  }
  one_arg <- function(data) {
    data.frame(a = data$x + data$y)
  }

  expect_equal(call_block(no_arg, raw), data.frame(a = 1:2))
  expect_equal(call_block(one_arg, raw), data.frame(a = raw$x + raw$y))
})

test_that("fd_compute_diagnose_error classifies common evaluation failures", {
  diagnose <- fd_ns("fd_compute_diagnose_error")

  missing_object <- diagnose(simpleError("object 'x' not found"))
  expect_identical(missing_object$class, "missing_object")
  expect_match(missing_object$message, "Possible reasons")
  expect_match(missing_object$hint, "raw input column")

  missing_function <- diagnose(simpleError("could not find function \"foo\""))
  expect_identical(missing_function$class, "missing_function")
  expect_match(missing_function$hint, "function is not available")

  missing_argument <- diagnose(simpleError("argument \"x\" is missing"))
  expect_identical(missing_argument$class, "missing_argument")
  expect_match(missing_argument$hint, "requires different arguments")

  other <- diagnose(simpleError("plain error"))
  expect_identical(other$class, "other_error")
  expect_identical(other$message, "plain error")
  expect_true(is.na(other$hint))
})

test_that("fd_compute_format_diagnostic_message handles malformed diagnostics", {
  format_diag <- fd_ns("fd_compute_format_diagnostic_message")

  expect_identical(
    format_diag(list(message = "specific message")),
    "specific message"
  )
  expect_identical(format_diag(NULL), "Unknown compute error.")
  expect_identical(format_diag(list()), "Unknown compute error.")
})

test_that("fd_compute_is_supported_result accepts vector-like columns only", {
  is_supported <- fd_ns("fd_compute_is_supported_result")

  expect_true(is_supported(c(1, 2)))
  expect_true(is_supported(c("a", "b")))
  expect_true(is_supported(as.Date("2026-05-02")))
  expect_true(is_supported(as.POSIXct("2026-05-02", tz = "UTC")))
  expect_true(is_supported(as.difftime(1, units = "days")))
  expect_false(is_supported(list(1, 2)))
  expect_false(is_supported(data.frame(a = 1)))
})
