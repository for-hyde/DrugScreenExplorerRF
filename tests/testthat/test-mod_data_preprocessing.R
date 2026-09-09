new_preprocessing_fixture <- function() {
  path <- tempfile("preprocessing-")
  dir.create(path)
  well_file <- file.path(path, "wellInput.csv")
  write.csv(data.frame(
    WellID = c("A001", "A002", "B001", "B002"),
    Drug1_name = c("DMSO", "Drug", "", ""),
    Drug1_concentration = c("", "1", "", ""),
    Drug2_name = rep("", 4),
    Drug2_concentration = rep("", 4),
    stringsAsFactors = FALSE
  ), well_file, row.names = FALSE)
  raw_file <- file.path(path, "raw.csv")
  writeLines(c(
    ",1,2",
    "A,10,20",
    "B,30,40"
  ), raw_file)
  plate_file <- file.path(path, "plateInput.csv")
  write.csv(data.frame(
    filename = "raw.csv",
    filepath = raw_file,
    sample_name = "sample1",
    stringsAsFactors = FALSE
  ), plate_file, row.names = FALSE)
  list(well_file = well_file, plate_file = plate_file)
}

test_that("preprocessing module starts empty and validates missing uploads", {
  skip_if_not_installed("shiny")
  shiny::testServer(mod_data_preprocessing_server, {
    expect_identical(session$returned$status(), "empty")
    session$setInputs(import = 1)
    expect_identical(session$returned$status(), "error")
    expect_match(session$returned$error_message(), "Select exactly one")
  })
})

test_that("preprocessing module imports uploaded datapaths", {
  skip_if_not_installed("shiny")
  fixture <- new_preprocessing_fixture()
  shiny::testServer(mod_data_preprocessing_server, {
    session$setInputs(
      wellfile = data.frame(datapath = fixture$well_file),
      platefile = data.frame(datapath = fixture$plate_file)
    )
    session$setInputs(import = 1)

    expect_identical(session$returned$status(), "ready")
    expect_identical(nrow(session$returned$data()), 4L)
    expect_identical(session$returned$data()$raw_count, c(10, 20, 30, 40))
    expect_null(session$returned$error_message())
  })
})
