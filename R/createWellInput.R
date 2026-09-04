#' Create a well-layout CSV file.
#'
#' @param platesize Number of wells in a supported standard plate: 24, 48,
#'   96, 384, or 1536.
#' @param dimensions Optional numeric vector containing `c(rows, columns)` for
#'   a custom plate layout.
#' @param savepath Optional existing output directory. Defaults to the current
#'   working directory.
#'
#' @return The normalized path to the written `wellInput.csv`, returned
#'   invisibly.
#' @export
createWellInput <- function(platesize = NULL, dimensions = NULL,
                            savepath = NULL) {

	platesize_supplied <- !is.null(platesize)
	dimensions_supplied <- !is.null(dimensions)
	if (platesize_supplied == dimensions_supplied) {
		stop("Exactly one of `platesize` and `dimensions` must be supplied.",
			 call. = FALSE)
	}

	standard_dimensions <- list(
		`24` = c(4L, 6L),
		`48` = c(6L, 8L),
		`96` = c(8L, 12L),
		`384` = c(16L, 24L),
		`1536` = c(32L, 48L)
	)

	if (platesize_supplied) {
		valid_platesize <- is.numeric(platesize) && length(platesize) == 1L &&
			!is.na(platesize) && is.finite(platesize) &&
			platesize == as.integer(platesize) &&
			as.character(platesize) %in% names(standard_dimensions)
		if (!valid_platesize) {
			stop("`platesize` must be one of 24, 48, 96, 384, or 1536.",
				 call. = FALSE)
		}
		dimensions <- standard_dimensions[[as.character(platesize)]]
	} else {
		valid_dimensions <- is.numeric(dimensions) && length(dimensions) == 2L &&
			all(is.finite(dimensions)) && all(dimensions > 0) &&
			all(dimensions == as.integer(dimensions))
		if (!valid_dimensions) {
			stop(paste0("`dimensions` must contain exactly two positive integers: ",
						 "c(rows, columns)."), call. = FALSE)
		}
		dimensions <- as.integer(dimensions)
	}

	rows <- dimensions[[1L]]
	columns <- dimensions[[2L]]
	if (rows > 26L) {
		stop(paste0("`dimensions` cannot specify more than 26 rows under the ",
						 "single-letter row-label convention."), call. = FALSE)
	}

	if (is.null(savepath)) {
		output_dir <- normalizePath(getwd(), mustWork = TRUE)
	} else {
		if (!is.character(savepath) || length(savepath) != 1L ||
				is.na(savepath) || !nzchar(savepath)) {
			stop("`savepath` must be a single non-empty character path.",
				 call. = FALSE)
		}
		if (!dir.exists(savepath)) {
			stop(sprintf("`savepath` does not exist or is not a directory: %s",
						 savepath), call. = FALSE)
		}
		output_dir <- normalizePath(savepath, mustWork = TRUE)
	}
	if (file.access(output_dir, mode = 2) != 0) {
		stop(sprintf("`savepath` is not writable: %s", output_dir), call. = FALSE)
	}

	output_file <- normalizePath(file.path(output_dir, "wellInput.csv"),
								 mustWork = FALSE)
	if (file.exists(output_file)) {
		stop(sprintf("`wellInput.csv` already exists: %s", output_file),
			 call. = FALSE)
	}

	row_labels <- LETTERS[seq_len(rows)]
	column_labels <- sprintf("%03d", seq_len(columns))
	well_ids <- paste0(rep(row_labels, each = columns),
					   rep(column_labels, times = rows))
	well_input <- data.frame(
		WellID = well_ids,
		Drug1_name = rep("", length(well_ids)),
		Drug1_concentration = rep("", length(well_ids)),
		Drug2_name = rep("", length(well_ids)),
		Drug2_concentration = rep("", length(well_ids)),
		stringsAsFactors = FALSE
	)

	tryCatch(
		utils::write.csv(well_input, output_file, row.names = FALSE),
		error = function(error) {
			stop(sprintf("Could not write `wellInput.csv` to %s: %s",
						 output_file, conditionMessage(error)), call. = FALSE)
		}
	)

	invisible(output_file)
}
