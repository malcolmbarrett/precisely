returns_well_formed <- function(x, .ncol = NULL, .nrow = NULL, expected_names = NULL) {
  expect_s3_class(x, "tbl")
  if (!is.null(.ncol)) expect_length(x, .ncol)
  if (!is.null(.nrow)) expect_equal(nrow(x), .nrow)
  if (!is.null(expected_names)) expect_named(x, expected_names)
  invisible()
}
