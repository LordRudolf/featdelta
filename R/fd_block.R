#' Define a multi-column feature block
#'
#' Creates a block definition object for use inside `fd_define()`.
#'
#' A block represents one ordered definition step that may produce multiple
#' feature columns when later evaluated by `fd_compute()`. Unlike ordinary
#' named single-column definitions, a block may return a whole `data.frame`
#' whose column names become the produced feature names.
#'
#' Supported block inputs are:
#' \itemize{
#'   \item an inline expression, including braced expressions such as
#'         `{ data.frame(...) }`
#'   \item a quosure
#'   \item an `expression()` object of length 1
#'   \item a quoted call or symbol
#'   \item a function, intended to receive the current working data and return a
#'         `data.frame`
#' }
#'
#' `fd_block()` does not compute anything by itself. It only stores the block
#' definition in a normalized form so that `fd_define()` can include it in the
#' ordered definitions object.
#'
#' @param x Block definition input. Must be either a function or a single
#'   expression-like input that later evaluates to a `data.frame`.
#' @param expected_names Optional character vector of expected output column
#'   names. If supplied, names must be non-empty, non-`NA`, and unique.
#'   These names are not required for normal operation, but they can later be
#'   used by `fd_compute()` for validation and for optional NA-completion of
#'   expected-but-missing outputs.
#' @param envir Optional environment used when normalizing expression-like
#'   inputs. If `NULL`, the calling environment is used.
#'
#' @return An object of class `"featdelta_block"`.
#'
#' @examples
#' # Inline expression block
#' blk <- fd_block({
#'   data.frame(
#'     hp_per_cyl = hp / cyl,
#'     disp_per_cyl = disp / cyl
#'   )
#' })
#'
#' # Function-based block
#' make_engine_features <- function(data) {
#'   data.frame(
#'     hp_per_cyl = data$hp / data$cyl,
#'     disp_per_cyl = data$disp / data$cyl
#'   )
#' }
#'
#' blk <- fd_block(make_engine_features)
#'
#' # Optional expected names
#' blk <- fd_block(
#'   {
#'     data.frame(
#'       hp_per_cyl = hp / cyl
#'     )
#'   },
#'   expected_names = c("hp_per_cyl", "disp_per_cyl")
#' )
#'
#' @family featdelta defs helpers
#' @export
fd_block <- function(x, expected_names = NULL, envir = NULL) {

  if (missing(x)) {
    stop("`x` must be supplied.", call. = FALSE)
  }

  if (!is.null(envir) && !is.environment(envir)) {
    stop("`envir` must be an environment or NULL.", call. = FALSE)
  }

  if (!is.null(expected_names)) {
    fd_block_validate_expected_names(expected_names)
  }

  q <- rlang::enquo(x)
  captured <- fd_block_resolve_captured_input(q)

  target_env <- envir %||%
    if (rlang::is_quosure(captured)) rlang::get_env(captured) else rlang::caller_env()

  out <- normalize_fd_block_input(
    x = captured,
    env = target_env,
    expected_names = expected_names
  )

  class(out) <- c("featdelta_block", "list")
  out
}


#' Resolve captured block input
#'
#' Internal helper for `fd_block()`. It keeps inline expressions as quosures,
#' while also resolving symbols that point to supported quoted-expression
#' containers or functions in the calling environment.
#'
#' @param q A quosure captured from `fd_block(x = ...)`.
#'
#' @return Either a quosure or a function.
#'
#' @noRd
fd_block_resolve_captured_input <- function(q) {
  if (!rlang::is_quosure(q)) {
    stop("`q` must be a quosure.", call. = FALSE)
  }

  expr <- rlang::get_expr(q)
  env  <- rlang::get_env(q)

  # Inline expressions, including `{ ... }`, stay as quosures.
  if (!is.symbol(expr)) {
    return(q)
  }

  name <- as.character(expr)

  if (!rlang::env_has(env, name, inherit = TRUE)) {
    return(q)
  }

  value <- rlang::env_get(env, name, inherit = TRUE)

  # Function object supplied through a symbol.
  if (is.function(value)) {
    return(value)
  }

  # Quoted-expression containers supplied through a symbol.
  if (rlang::is_quosure(value)) {
    return(value)
  }

  if (inherits(value, "expression")) {
    if (length(value) != 1L) {
      stop(
        sprintf(
          "`fd_block()` input `%s` refers to an `expression()` object of length %d. ",
          name, length(value)
        ),
        "A block expression must be length 1.",
        call. = FALSE
      )
    }
    return(rlang::new_quosure(value[[1]], env = env))
  }

  if (is.call(value) || is.name(value)) {
    return(rlang::new_quosure(value, env = env))
  }

  q
}


#' Normalize block input to canonical stored form
#'
#' Internal helper used by `fd_block()` to normalize supported block inputs.
#'
#' @param x Block input.
#' @param env Environment used for expression-like inputs.
#' @param expected_names Optional expected output names.
#'
#' @return A normalized list suitable for class `"featdelta_block"`.
#'
#' @noRd
normalize_fd_block_input <- function(x, env, expected_names = NULL) {

  # Case 1: function block
  if (is.function(x)) {
    fn_text <- paste0("<function: ", fd_block_fn_label(x), ">")

    return(list(
      type = "block",
      mode = "function",
      fn = x,
      quo = NULL,
      expr = NULL,
      env = environment(x) %||% env,
      text = fn_text,
      expected_names = expected_names
    ))
  }

  # Case 2: quosure block (includes inline `{ ... }`)
  if (rlang::is_quosure(x)) {
    expr <- rlang::get_expr(x)

    return(list(
      type = "block",
      mode = "expression",
      fn = NULL,
      quo = rlang::new_quosure(expr, env = env),
      expr = expr,
      env = env,
      text = rlang::expr_text(expr),
      expected_names = expected_names
    ))
  }

  # Case 3: base expression() object
  if (inherits(x, "expression")) {
    if (length(x) != 1L) {
      stop(
        sprintf(
          "`fd_block()` received `expression(...)` with length %d. ",
          length(x)
        ),
        "A block expression must be length 1.",
        call. = FALSE
      )
    }

    expr <- x[[1]]

    return(list(
      type = "block",
      mode = "expression",
      fn = NULL,
      quo = rlang::new_quosure(expr, env = env),
      expr = expr,
      env = env,
      text = rlang::expr_text(expr),
      expected_names = expected_names
    ))
  }

  # Case 4: quoted call / symbol
  if (is.call(x) || is.name(x)) {
    return(list(
      type = "block",
      mode = "expression",
      fn = NULL,
      quo = rlang::new_quosure(x, env = env),
      expr = x,
      env = env,
      text = rlang::expr_text(x),
      expected_names = expected_names
    ))
  }

  stop(
    paste0(
      "`fd_block()` requires either a function or a single expression-like input ",
      "(including `{ ... }`, quosure, expression of length 1, call, or symbol)."
    ),
    call. = FALSE
  )
}


#' Validate expected output names for a block
#'
#' @param expected_names Character vector of expected output names.
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @noRd
fd_block_validate_expected_names <- function(expected_names) {
  if (!is.character(expected_names)) {
    stop("`expected_names` must be a character vector or NULL.", call. = FALSE)
  }

  if (!length(expected_names)) {
    stop("`expected_names` must not be empty if supplied.", call. = FALSE)
  }

  if (any(is.na(expected_names)) || any(!nzchar(expected_names))) {
    stop(
      "`expected_names` must contain only non-empty, non-NA names.",
      call. = FALSE
    )
  }

  if (anyDuplicated(expected_names)) {
    dups <- unique(expected_names[duplicated(expected_names)])
    stop(
      sprintf(
        "Duplicate names in `expected_names`: %s",
        paste(dups, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


#' Create a readable label for a function block
#'
#' @param fn A function.
#'
#' @return Character scalar label.
#'
#' @noRd
fd_block_fn_label <- function(fn) {
  nm <- tryCatch(deparse(substitute(fn)), error = function(e) NULL)

  if (is.character(nm) && length(nm) == 1L && nzchar(nm) && nm != "x") {
    return(nm)
  }

  "anonymous"
}


#' @export
print.featdelta_block <- function(x, ...) {
  cat("<featdelta_block>", "\n", sep = "")
  cat("Mode: ", x$mode, "\n", sep = "")

  if (!is.null(x$expected_names)) {
    cat(
      "Expected names: ",
      paste(x$expected_names, collapse = ", "),
      "\n",
      sep = ""
    )
  }

  cat("Definition: ", x$text, "\n", sep = "")
  invisible(x)
}
