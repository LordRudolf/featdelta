fd_test_raw_data <- function() {
  data.frame(
    id = 1:5,
    x = c(10, 20, 30, 40, 50),
    y = c(2, 4, 5, 8, 10),
    group = c("a", "a", "b", "b", "c"),
    stringsAsFactors = FALSE
  )
}

fd_test_features_data <- function(ids = integer()) {
  data.frame(
    id = ids,
    existing_feature = ids * 100,
    stringsAsFactors = FALSE
  )
}

fd_test_sqlite_con <- function() {
  testthat::skip_if_not_installed("RSQLite")
  DBI::dbConnect(RSQLite::SQLite(), ":memory:")
}

fd_test_disconnect <- function(con) {
  if (inherits(con, "DBIConnection") && DBI::dbIsValid(con)) {
    DBI::dbDisconnect(con)
  }
  invisible(TRUE)
}

fd_test_seed_sqlite <- function(con,
                                raw = fd_test_raw_data(),
                                feature_ids = integer()) {
  features <- fd_test_features_data(feature_ids)

  DBI::dbWriteTable(con, "raw_tbl", raw, overwrite = TRUE)
  DBI::dbWriteTable(con, "features_tbl", features, overwrite = TRUE)

  invisible(list(
    raw = raw,
    features = features
  ))
}
