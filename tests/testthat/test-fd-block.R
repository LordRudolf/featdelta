test_that("fd_block expression blocks compute through fd_define and fd_compute", {
  raw <- fd_test_raw_data()
  defs <- fd_define(
    ratios = fd_block({
      data.frame(
        ratio = x / y,
        sum_xy = x + y
      )
    }),
    after_block = ratio + sum_xy
  )

  out <- fd_compute(raw, defs, key = "id")
  report <- attr(out, "fd_report")

  expect_named(out, c("id", "ratio", "sum_xy", "after_block"))
  expect_equal(out$ratio, raw$x / raw$y)
  expect_equal(out$sum_xy, raw$x + raw$y)
  expect_equal(out$after_block, (raw$x / raw$y) + raw$x + raw$y)

  expect_equal(report$step_name, c("ratios", "after_block"))
  expect_equal(report$type, c("block", "column"))
  expect_equal(report$mode, c("expression", "expression"))
  expect_true(all(report$ok))
  expect_equal(report$output_names, c("ratio, sum_xy", "after_block"))
})

test_that("fd_block function blocks receive raw and previously produced columns", {
  raw <- fd_test_raw_data()
  make_more <- function(data) {
    data.frame(
      ratio_plus_one = data$ratio + 1,
      group_copy = data$group
    )
  }

  defs <- fd_define(
    ratio = x / y,
    more = fd_block(make_more)
  )

  out <- fd_compute(raw, defs, key = "id")

  expect_named(out, c("id", "ratio", "ratio_plus_one", "group_copy"))
  expect_equal(out$ratio, raw$x / raw$y)
  expect_equal(out$ratio_plus_one, (raw$x / raw$y) + 1)
  expect_identical(out$group_copy, raw$group)
})

test_that("fd_block expected names complete missing outputs with NA and define order", {
  raw <- fd_test_raw_data()
  defs <- fd_define(
    expected = fd_block(
      {
        data.frame(sum_xy = x + y)
      },
      expected_names = c("ratio", "sum_xy")
    )
  )

  out <- fd_compute(raw, defs, key = "id")
  report <- attr(out, "fd_report")

  expect_named(out, c("id", "ratio", "sum_xy"))
  expect_true(all(is.na(out$ratio)))
  expect_equal(out$sum_xy, raw$x + raw$y)
  expect_true(report$ok)
  expect_equal(report$output_names, "ratio, sum_xy")
})

test_that("fd_block rejects unexpected names when expected names are declared", {
  raw <- fd_test_raw_data()
  defs <- fd_define(
    expected = fd_block(
      {
        data.frame(sum_xy = x + y, unexpected = x - y)
      },
      expected_names = c("ratio", "sum_xy")
    )
  )

  err <- expect_error(
    fd_compute(raw, defs, key = "id"),
    class = "featdelta_compute_error"
  )
  report <- attr(err, "fd_report")

  expect_equal(report$step_name, "expected")
  expect_false(report$ok)
  expect_match(report$error, "unexpected column name")
})

test_that("fd_block rejects block outputs that collide with key or earlier features", {
  raw <- fd_test_raw_data()

  key_collision <- fd_define(
    bad = fd_block({
      data.frame(id = x + y)
    })
  )
  err_key <- expect_error(
    fd_compute(raw, key_collision, key = "id"),
    class = "featdelta_compute_error"
  )
  expect_match(attr(err_key, "fd_report")$error, "colliding with `key`")

  earlier_collision <- fd_define(
    ratio = x / y,
    bad = fd_block({
      data.frame(ratio = x + y)
    })
  )
  err_earlier <- expect_error(
    fd_compute(raw, earlier_collision, key = "id"),
    class = "featdelta_compute_error"
  )
  expect_match(attr(err_earlier, "fd_report")$error[[2]], "already produced earlier")
})

test_that("fd_block reports malformed block return values", {
  raw <- fd_test_raw_data()

  expect_error(
    fd_compute(
      raw,
      fd_define(bad = fd_block({ x + y })),
      key = "id"
    ),
    "one or more definition steps failed",
    class = "featdelta_compute_error"
  )

  expect_error(
    fd_compute(
      raw,
      fd_define(bad = fd_block({ data.frame(value = x[1:2]) })),
      key = "id"
    ),
    "one or more definition steps failed",
    class = "featdelta_compute_error"
  )

  expect_error(
    fd_compute(
      raw,
      fd_define(bad = fd_block({
        out <- data.frame(a = x, b = y)
        names(out) <- c("dup", "dup")
        out
      })),
      key = "id"
    ),
    "one or more definition steps failed",
    class = "featdelta_compute_error"
  )
})

test_that("fd_block non-strict failures can fill declared outputs with NA", {
  raw <- fd_test_raw_data()
  defs <- fd_define(
    bad = fd_block(
      {
        stop("broken block")
      },
      expected_names = c("first", "second")
    ),
    after = x + y
  )

  expect_warning(
    out <- fd_compute(raw, defs, key = "id", compute_strict = FALSE),
    "step 'bad' failed"
  )
  report <- attr(out, "fd_report")

  expect_named(out, c("id", "first", "second", "after"))
  expect_true(all(is.na(out$first)))
  expect_true(all(is.na(out$second)))
  expect_equal(out$after, raw$x + raw$y)
  expect_equal(report$ok, c(FALSE, TRUE))
  expect_equal(report$output_names, c("first, second", "after"))
})

test_that("fd_block validates constructor-only arguments", {
  expect_error(fd_block(), "`x` must be supplied")
  expect_error(fd_block({ data.frame(a = x) }, envir = list()), "`envir`")
  expect_error(
    fd_block({ data.frame(a = x) }, expected_names = character()),
    "must not be empty"
  )
  expect_error(
    fd_block({ data.frame(a = x) }, expected_names = c("a", NA_character_)),
    "non-empty, non-NA"
  )
  expect_error(
    fd_block({ data.frame(a = x) }, expected_names = c("a", "a")),
    "Duplicate names"
  )
})

test_that("print.featdelta_block returns input invisibly and shows block metadata", {
  block <- fd_block(
    {
      data.frame(ratio = x / y)
    },
    expected_names = "ratio"
  )

  printed <- capture.output(ret <- print(block))

  expect_s3_class(block, "featdelta_block")
  expect_identical(ret, block)
  expect_true(any(grepl("<featdelta_block>", printed, fixed = TRUE)))
  expect_true(any(grepl("Mode: expression", printed, fixed = TRUE)))
  expect_true(any(grepl("Expected names: ratio", printed, fixed = TRUE)))
  expect_true(any(grepl("Definition:", printed, fixed = TRUE)))
})
