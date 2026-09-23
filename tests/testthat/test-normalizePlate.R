library(dplyr)

test_that("normalizePlate uses plate-specific negative-control scaling when by = 'plate'", {
  screen_data <- data.frame(
    experimentID = rep("exp1", 4),
    plateID = c("p1", "p1", "p2", "p2"),
    wellID = c("A001", "A002", "A001", "A002"),
    raw_count = c(10, 20, 30, 40),
    drug1_name = c("NEG", "NEG", "NEG", "NEG"),
    drug2_name = rep(NA_character_, 4),
    control = c("neg", "neg", "neg", "neg"),
    stringsAsFactors = FALSE
  )

  result <- normalizePlate(screenData = screen_data, method = "negative", by = "plate")

  expect_true("viab_norm" %in% names(result))
  expect_equal(result$viab_norm, c(2 / 3, 4 / 3, 6 / 7, 8 / 7), tolerance = 1e-8)
  expect_identical(screen_data$raw_count, c(10, 20, 30, 40))
})

test_that("normalizePlate errors when NPI is requested without positive controls", {
  screen_data <- data.frame(
    experimentID = rep("exp1", 4),
    plateID = c("p1", "p1", "p1", "p1"),
    wellID = c("A001", "A002", "A003", "A004"),
    raw_count = c(10, 10, 30, 30),
    drug1_name = c("NEG", "NEG", "NEG", "NEG"),
    drug2_name = rep(NA_character_, 4),
    control = c("neg", "neg", "neg", "neg"),
    stringsAsFactors = FALSE
  )

  expect_error(normalizePlate(screenData = screen_data, method = "npi"), "positive")
})

test_that("normalizePlate can identify controls from explicit arguments when control column is absent", {
  screen_data <- data.frame(
    experimentID = rep("exp1", 4),
    plateID = c("p1", "p1", "p1", "p1"),
    wellID = c("A001", "A002", "A003", "A004"),
    raw_count = c(10, 20, 30, 40),
    drug1_name = c("CTRL_NEG", "CTRL_NEG", "CTRL_NEG", "CTRL_NEG"),
    drug2_name = rep(NA_character_, 4),
    stringsAsFactors = FALSE
  )

  result <- normalizePlate(
    screenData = screen_data,
    method = "negative",
    negWell = "CTRL_NEG"
  )

  expect_equal(result$viab_norm, c(0.4, 0.8, 1.2, 1.6), tolerance = 1e-8)
})

test_that("normalizePlate rejects unsupported methods", {
  screen_data <- data.frame(
    experimentID = rep("exp1", 2),
    plateID = c("p1", "p1"),
    wellID = c("A001", "A002"),
    raw_count = c(10, 20),
    drug1_name = c("NEG", "NEG"),
    drug2_name = rep(NA_character_, 2),
    control = c("neg", "neg"),
    stringsAsFactors = FALSE
  )

  expect_error(normalizePlate(screenData = screen_data, method = "bad"), "method")
})

test_that("normalizePlate supports experiment-level normalization when by = 'experiment'", {
  screen_data <- data.frame(
    experimentID = rep("exp1", 4),
    plateID = c("p1", "p1", "p2", "p2"),
    wellID = c("A001", "A002", "A001", "A002"),
    raw_count = c(10, 20, 30, 40),
    drug1_name = c("NEG", "NEG", "NEG", "NEG"),
    drug2_name = rep(NA_character_, 4),
    control = c("neg", "neg", "neg", "neg"),
    stringsAsFactors = FALSE
  )

  result <- normalizePlate(screenData = screen_data, method = "negative", by = "experiment")

  expect_equal(result$viab_norm, c(0.4, 0.8, 1.2, 1.6), tolerance = 1e-8)
})

test_that("normalizePlate rejects raw_value when the canonical raw_count column is absent", {
  screen_data <- data.frame(
    experimentID = rep("exp1", 4),
    plateID = c("p1", "p1", "p1", "p1"),
    wellID = c("A001", "A002", "A003", "A004"),
    raw_value = c(10, 20, 30, 40),
    drug1_name = c("NEG", "NEG", "NEG", "NEG"),
    drug2_name = rep(NA_character_, 4),
    control = c("neg", "neg", "neg", "neg"),
    stringsAsFactors = FALSE
  )

  expect_error(normalizePlate(screenData = screen_data, method = "negative"), "raw_count")
})

