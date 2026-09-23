edge_effect_fixture <- function(rows = 4L, columns = 4L) {
  grid <- expand.grid(
    plateID = "plate1",
    row = LETTERS[seq_len(rows)],
    column = seq_len(columns),
    KEEP.OUT.ATTRS = FALSE
  )
  data.frame(
    plateID = grid$plateID,
    wellID = paste0(grid$row, sprintf("%03d", grid$column)),
    viab_norm = seq_len(nrow(grid)) / 10,
    control = "neg",
    stringsAsFactors = FALSE
  )
}

test_that("fitEdgeEffect estimates LOESS factors without modifying input", {
  screen_data <- edge_effect_fixture(rows = 2L, columns = 3L)
  original_values <- screen_data$viab_norm

  result <- fitEdgeEffect(screen_data, method = "loess")

  expect_true("edgeFactor" %in% names(result))
  expect_true(all(is.finite(result$edgeFactor)))
  expect_identical(screen_data$viab_norm, original_values)
})

test_that("fitEdgeEffect estimates sigmoid factors", {
  result <- fitEdgeEffect(edge_effect_fixture(), method = "sigmoid")

  expect_true(all(is.finite(result$edgeFactor)))
})

test_that("fitEdgeEffect returns neutral factors for excluded plates", {
  screen_data <- edge_effect_fixture()

  result <- fitEdgeEffect(screen_data, exclude = "plate1")

  expect_equal(result$edgeFactor, rep(1, nrow(screen_data)))
})

test_that("fitEdgeEffect validates method and required controls", {
  screen_data <- edge_effect_fixture()

  expect_error(fitEdgeEffect(screen_data, method = "bad"), "method")
  expect_error(
    fitEdgeEffect(screen_data[setdiff(names(screen_data), "control")], useNeg = TRUE),
    "control"
  )
  expect_error(fitEdgeEffect(screen_data, useLowConcentrations = 1L),
               "not yet supported")
})
