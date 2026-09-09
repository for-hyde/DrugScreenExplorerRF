new_read_screen_directory <- function() {
  path <- tempfile("readScreen-")
  dir.create(path)
  path
}

write_read_screen_inputs <- function(path, raw_files = 1L) {
  well_ids <- paste0(rep(LETTERS[1:2], each = 3),
                     sprintf("%03d", rep(1:3, times = 2)))
  well_file <- file.path(path, "wellInput.csv")
  write.csv(data.frame(
    WellID = well_ids,
    Drug1_name = c("NEG", "POS", rep("", 4)),
    Drug1_concentration = rep("", 6),
    Drug2_name = rep("", 6),
    Drug2_concentration = rep("", 6),
    stringsAsFactors = FALSE
  ), well_file, row.names = FALSE)

  raw_paths <- vapply(seq_len(raw_files), function(index) {
    raw_file <- file.path(path, paste0("sample", index, ".csv"))
    measurement_rows <- c(
      "A,1,2,3",
      "B,4,5,6"
    )
    writeLines(c(
      "Results for LUM",
      paste(c("", 1:3), collapse = ","),
      measurement_rows,
      "Analysis Result",
      "Platemap",
      paste(c("", 1:3), collapse = ","),
      "A,-,-,-",
      "B,-,-,-"
    ), raw_file)
    raw_file
  }, character(1))

  plate_file <- file.path(path, "plateInput.csv")
  write.csv(data.frame(
    filename = basename(raw_paths),
    filepath = raw_paths,
    sample_name = paste0("sample", seq_along(raw_paths)),
    stringsAsFactors = FALSE
  ), plate_file, row.names = FALSE)
  list(well_file = well_file, plate_file = plate_file)
}

test_that("readScreen extracts the numeric table and joins well metadata", {
  path <- new_read_screen_directory()
  inputs <- write_read_screen_inputs(path)

  result <- readScreen(inputs$well_file, inputs$plate_file,
                       negWell = "NEG", posWell = "POS")

  expect_identical(names(result), c(
    "wellID", "experimentID", "raw_count", "drug1_name",
    "drug1_concentration", "drug2_name", "drug2_concentration", "control"
  ))
  expect_identical(result$wellID, paste0(rep(LETTERS[1:2], each = 3),
                                         sprintf("%03d", rep(1:3, times = 2))))
  expect_identical(result$raw_count, c(1, 2, 3, 4, 5, 6))
  expect_identical(result$experimentID, rep("sample1", 6))
  expect_identical(result$control[1:2], c("neg", "pos"))
  expect_true(all(is.na(result$control[3:6])))
})

test_that("readScreen combines files in plateInput order", {
  path <- new_read_screen_directory()
  inputs <- write_read_screen_inputs(path, raw_files = 2L)

  result <- readScreen(inputs$well_file, inputs$plate_file)

  expect_identical(nrow(result), 12L)
  expect_identical(result$experimentID, rep(c("sample1", "sample2"),
                                            each = 6))
  expect_identical(result$raw_count, rep(c(1, 2, 3, 4, 5, 6), times = 2))
})

test_that("readScreen supports an explicit table row range", {
  path <- new_read_screen_directory()
  inputs <- write_read_screen_inputs(path)

  result <- readScreen(inputs$well_file, inputs$plate_file,
                       rowRange = c(2, 4))

  expect_identical(result$raw_count, c(1, 2, 3, 4, 5, 6))
})

test_that("readScreen prefers a Results-for table among duplicate matrices", {
  path <- new_read_screen_directory()
  inputs <- write_read_screen_inputs(path)
  raw_file <- file.path(path, "duplicate.csv")
  writeLines(c(
    "Calculated results: correction",
    ",1,2,3",
    "A,10,10,10",
    "B,10,10,10",
    "Results for assay (CPS)",
    ";1;2;3;",
    "A;1;2;3;",
    "B;4;5;6;"
  ), raw_file)
  write.csv(data.frame(
    filepath = raw_file,
    sample_name = "duplicate"
  ), inputs$plate_file, row.names = FALSE)

  result <- readScreen(inputs$well_file, inputs$plate_file, sep = ";")

  expect_identical(result$raw_count, c(1, 2, 3, 4, 5, 6))
})

test_that("readScreen rejects invalid inputs and table dimensions", {
  path <- new_read_screen_directory()
  inputs <- write_read_screen_inputs(path)

  expect_error(readScreen(file.path(path, "missing.csv"), inputs$plate_file),
               "wellInputFile")
  expect_error(readScreen(inputs$well_file, inputs$plate_file,
                          rowRange = c(0, 2)), "rowRange")

  bad_plate <- file.path(path, "bad-plate.csv")
  write.csv(data.frame(filepath = inputs$plate_file), bad_plate,
            row.names = FALSE)
  expect_error(readScreen(inputs$well_file, bad_plate), "missing required columns")

  bad_raw <- file.path(path, "bad.csv")
  writeLines(c("", ",1,2", "A,1,2"), bad_raw)
  bad_inventory <- file.path(path, "bad-inventory.csv")
  write.csv(data.frame(filepath = bad_raw, sample_name = "bad"),
            bad_inventory, row.names = FALSE)
  expect_error(readScreen(inputs$well_file, bad_inventory), "dimensions")
})
