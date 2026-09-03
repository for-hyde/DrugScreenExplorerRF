new_test_directory <- function() {
  path <- tempfile("createPlateInput-")
  dir.create(path)
  path
}

test_that("createPlateInput inventories CSV files recursively", {
  input_dir <- new_test_directory()
  nested_dir <- file.path(input_dir, "nested")
  dir.create(nested_dir)
  writeLines("not a valid plate file", file.path(input_dir, "z.csv"))
  writeLines("also not parsed", file.path(nested_dir, "a.CSV"))

  output_file <- createPlateInput(input_dir)
  inventory <- read.csv(output_file, stringsAsFactors = FALSE)

  expect_identical(names(inventory), c("filename", "filepath", "sample_name"))
  expect_identical(inventory$filename, c("a.CSV", "z.csv"))
  expect_identical(inventory$sample_name, c("a", "z"))
  expect_true(all(file.exists(inventory$filepath)))
  expect_identical(output_file, normalizePath(file.path(input_dir, "plateInput.csv")))
})

test_that("createPlateInput honors a separate output directory", {
  input_dir <- new_test_directory()
  output_dir <- new_test_directory()
  writeLines("plate", file.path(input_dir, "plate.csv"))

  output_file <- createPlateInput(input_dir, output_dir)

  expect_identical(output_file, normalizePath(file.path(output_dir, "plateInput.csv")))
  expect_true(file.exists(output_file))
  expect_false(file.exists(file.path(input_dir, "plateInput.csv")))
})

test_that("createPlateInput excludes its existing output", {
  input_dir <- new_test_directory()
  writeLines("plate", file.path(input_dir, "plate.csv"))

  createPlateInput(input_dir)
  expect_warning(
    output_file <- createPlateInput(input_dir),
    "Overwriting existing output file"
  )
  inventory <- read.csv(output_file, stringsAsFactors = FALSE)

  expect_identical(nrow(inventory), 1L)
  expect_identical(inventory$filename, "plate.csv")
})

test_that("createPlateInput retains duplicate basenames and warns on sample names", {
  input_dir <- new_test_directory()
  dir.create(file.path(input_dir, "one"))
  dir.create(file.path(input_dir, "two"))
  writeLines("one", file.path(input_dir, "one", "sample.csv"))
  writeLines("two", file.path(input_dir, "two", "sample.csv"))

  expect_warning(
    inventory_path <- createPlateInput(input_dir),
    "sample_name.*duplicated"
  )
  inventory <- read.csv(inventory_path, stringsAsFactors = FALSE)

  expect_identical(nrow(inventory), 2L)
  expect_identical(inventory$filename, c("sample.csv", "sample.csv"))
  expect_identical(length(unique(inventory$filepath)), 2L)
})

test_that("createPlateInput rejects invalid directories and empty inputs", {
  expect_error(
    createPlateInput(file.path(tempdir(), "does-not-exist")),
    "dirpath.*does not exist"
  )

  input_dir <- new_test_directory()
  writeLines("text", file.path(input_dir, "notes.txt"))
  expect_error(createPlateInput(input_dir), "No readable CSV files")

  output_dir <- new_test_directory()
  expect_error(
    createPlateInput(input_dir, file.path(output_dir, "missing")),
    "savepath.*does not exist"
  )
})

test_that("createPlateInput is deterministic", {
  input_dir <- new_test_directory()
  dir.create(file.path(input_dir, "nested"))
  writeLines("b", file.path(input_dir, "b.csv"))
  writeLines("a", file.path(input_dir, "nested", "a.csv"))

  first <- read.csv(createPlateInput(input_dir), stringsAsFactors = FALSE)
  second <- suppressWarnings(read.csv(createPlateInput(input_dir),
                                      stringsAsFactors = FALSE))

  expect_identical(first, second)
  expect_identical(first$filepath, sort(first$filepath))
})
