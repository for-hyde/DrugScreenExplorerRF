test_that("createWellInput creates a standard row-major layout", {
  output_dir <- tempfile("createWellInput-")
  dir.create(output_dir)

  output_file <- createWellInput(96, savepath = output_dir)
  input <- read.csv(output_file, stringsAsFactors = FALSE)
  csv_lines <- readLines(output_file, encoding = "UTF-8")

  expect_identical(names(input), c(
    "WellID", "Drug1_name", "Drug1_concentration", "Drug2_name",
    "Drug2_concentration"
  ))
  expect_identical(nrow(input), 96L)
  expect_identical(input$WellID[c(1, 2, 12, 13, 96)],
                  c("A001", "A002", "A012", "B001", "H012"))
  expect_match(csv_lines[2], '^"A001","","","",""$')
  expect_identical(output_file, normalizePath(file.path(
    output_dir, "wellInput.csv"
  )))
})

test_that("createWellInput supports custom dimensions", {
  output_dir <- tempfile("createWellInput-")
  dir.create(output_dir)

  output_file <- createWellInput(dimensions = c(2, 3), savepath = output_dir)
  input <- read.csv(output_file, stringsAsFactors = FALSE)

  expect_identical(input$WellID,
                  c("A001", "A002", "A003", "B001", "B002", "B003"))
})

test_that("createWellInput validates layout arguments", {
  output_dir <- tempfile("createWellInput-")
  dir.create(output_dir)

  expect_error(createWellInput(), "Exactly one")
  expect_error(createWellInput(96, dimensions = c(2, 3)), "Exactly one")
  expect_error(createWellInput(358), "platesize")
  expect_error(createWellInput(dimensions = c(2, 1.5)), "dimensions")
  expect_error(createWellInput(dimensions = c(27, 1)), "more than 26")
  expect_error(createWellInput(96, savepath = file.path(output_dir, "missing")),
               "savepath")
})

test_that("createWellInput does not overwrite an existing output", {
  output_dir <- tempfile("createWellInput-")
  dir.create(output_dir)
  output_file <- file.path(output_dir, "wellInput.csv")
  writeLines("user metadata", output_file)

  expect_error(createWellInput(96, savepath = output_dir), "already exists")
  expect_identical(readLines(output_file), "user metadata")
})
