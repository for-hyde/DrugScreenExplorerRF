#' Create an inventory of plate-reader CSV files.
#'
#' @param dirpath A directory containing raw plate-reader CSV files.
#' @param savepath Optional output directory. Defaults to `dirpath`.
#'
#' @return The normalized path to the written `plateInput.csv`, returned
#'   invisibly.
#' @export
createPlateInput <- function(dirpath, savepath = NULL) {
	validate_directory <- function(path, argument, writable = FALSE) {
		if (!is.character(path) || length(path) != 1L || is.na(path) ||
				!nzchar(path)) {
			stop(sprintf("`%s` must be a single non-empty character path.", argument),
					 call. = FALSE)
		}

		if (!dir.exists(path)) {
			stop(sprintf("`%s` does not exist or is not a directory: %s",
									 argument, path), call. = FALSE)
		}

		normalized <- normalizePath(path, mustWork = TRUE)
		if (writable && file.access(normalized, mode = 2) != 0) {
			stop(sprintf("`%s` is not writable: %s", argument, normalized),
					 call. = FALSE)
		}
		normalized
	}

	input_dir <- validate_directory(dirpath, "dirpath")
	output_dir <- if (is.null(savepath)) {
		input_dir
	} else {
		validate_directory(savepath, "savepath", writable = TRUE)
	}
	output_file <- normalizePath(file.path(output_dir, "plateInput.csv"),
															 mustWork = FALSE)

	discovered <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE,
													 recursive = TRUE, ignore.case = TRUE,
													 include.dirs = FALSE)
	discovered <- discovered[!grepl("^\\.|^~\\$", basename(discovered))]
	discovered <- normalizePath(discovered, mustWork = FALSE)
	discovered <- discovered[file.exists(discovered)]

	if (length(discovered) > 0L) {
		discovered <- discovered[discovered != output_file]
	}

	unreadable <- discovered[file.access(discovered, mode = 4) != 0]
	if (length(unreadable) > 0L) {
		warning(sprintf("Excluding %d unreadable CSV file%s: %s",
										length(unreadable),
										if (length(unreadable) == 1L) "" else "s",
										paste(unreadable, collapse = ", ")),
						call. = FALSE)
		discovered <- setdiff(discovered, unreadable)
	}

	if (length(discovered) == 0L) {
		stop(sprintf("No readable CSV files were found under `dirpath`: %s",
								 input_dir), call. = FALSE)
	}

	discovered <- sort(discovered)
	inventory <- data.frame(
		filename = basename(discovered),
		filepath = discovered,
		sample_name = sub("\\.csv$", "", basename(discovered),
		                   ignore.case = TRUE),
		stringsAsFactors = FALSE
	)

	duplicate_names <- duplicated(inventory$sample_name) |
		duplicated(inventory$sample_name, fromLast = TRUE)
	if (any(duplicate_names)) {
		warning(paste0("Some derived `sample_name` values are duplicated; review ",
									 "them before analysis."), call. = FALSE)
	}

	if (file.exists(output_file)) {
		warning(sprintf("Overwriting existing output file: %s", output_file),
						call. = FALSE)
	}

	tryCatch(
		utils::write.csv(inventory, output_file, row.names = FALSE),
		error = function(error) {
			stop(sprintf("Could not write `plateInput.csv` to %s: %s",
									 output_file, conditionMessage(error)), call. = FALSE)
		}
	)

	invisible(output_file)
}
