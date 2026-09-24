makeReport <- function(screenData,
                       edgeEffectEstimation = "loess",
                       title = "DrugScreenExplorer QC Report",
                       author = "Author Name",
                       outputPath = ".",
                       log10 = FALSE
                       ) {
    # Check that Preconditions are met
    check_screen_data_format(screenData, normalized = TRUE)

    # Check that outputPath is a valid directory
    if (!dir.exists(outputPath)) {
        stop(sprintf("`outputPath` must be a valid directory: %s", outputPath), call. = FALSE)
    }

    # Check that edgeEffectEstimation is valid
    if (!edgeEffectEstimation %in% c("loess", "sigmoid")) {
        stop("`edgeEffectEstimation` must be one of 'loess' or 'sigmoid'.", call. = FALSE)
    }

    # Check that log10 is a logical value
    if (!is.logical(log10) || length(log10) != 1 || is.na(log10)) {
        stop("`log10` must be a single logical value.", call. = FALSE)
    }

    # Check that the author and title are character strings
    if (!is.character(title) || length(title) != 1 || is.na(title))
        stop("`title` must be a single character string.", call. = FALSE)
    if (!is.character(author) || length(author) != 1 || is.na(author))
        stop("`author` must be a single character string.", call. = FALSE)

    # create a directory for the report if it doesn't exist
    report_dir <- file.path(outputPath, "report")
    dir.create(report_dir, showWarnings = FALSE, recursive = TRUE)

    #If save figures, create folder and save

    # load template file and save to report directory
    tempRmd <- system.file("rmarkdown", "templates", "report_template",
                "skeleton", "skeleton.Rmd", package = "DrugScreenExplorerRF")
    file.copy(tempRmd, file.path(report_dir, "report.Rmd"), overwrite = TRUE)

    #render html with parameters
    rmarkdown::render(
        input = file.path(report_dir, "report.Rmd"),
        output_dir = report_dir,
        output_file = "report.html",
        params = list(
            screenData = screenData,
            edgeEffectEstimation = edgeEffectEstimation,
            title = title,
            author = author,
            output_dir = report_dir,
            log10 = log10
        ),
        quiet = TRUE
    )
}
