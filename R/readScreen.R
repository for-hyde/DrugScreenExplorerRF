#' Read plate-reader files and combine them with well metadata.
#'
#' @param wellInputFile Path to a populated `wellInput.csv` file.
#' @param plateInputFile Path to a populated `plateInput.csv` file.
#' @param negWell Optional character vector identifying negative-control drug
#'   names.
#' @param posWell Optional character vector identifying positive-control drug
#'   names.
#' @param rowRange Optional integer vector `c(first_row, last_row)` specifying
#'   the inclusive lines containing the measurement table.
#' @param sep Field separator used by the raw plate-reader files. Defaults to
#'   `","`.
#' @param tablePattern Optional regular expression identifying measurement-table
#'   header lines when `rowRange` is not supplied.
#'
#' @return A data frame containing one row per well per raw plate-reader file.
#' @export
readScreen <- function(wellInputFile, plateInputFile, negWell = NULL,
                       posWell = NULL, rowRange = NULL, sep = ",",
                       tablePattern = NULL) {
	#Functions to validate input files and read CSV files with error handling.
    validate_file <- function(path, argument) {
		if (!is.character(path) || length(path) != 1L || is.na(path) ||
				!nzchar(path)) {
			stop(sprintf("`%s` must be a single non-empty character path.", argument),
				 call. = FALSE)
		}
		if (!file.exists(path) || dir.exists(path)) {
			stop(sprintf("`%s` does not identify an existing file: %s", argument,
						 path), call. = FALSE)
		}
		if (file.access(path, mode = 4) != 0) {
			stop(sprintf("`%s` is not readable: %s", argument, path),
				 call. = FALSE)
		}
		normalizePath(path, mustWork = TRUE)
	}
	read_csv <- function(path, argument) {
		tryCatch(
			utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
			error = function(error) {
				stop(sprintf("Could not read `%s` (%s): %s", argument, path,
							 conditionMessage(error)), call. = FALSE)
			}
		)
	}
    # Validate and read the input files.
	well_path <- validate_file(wellInputFile, "wellInputFile")
	plate_path <- validate_file(plateInputFile, "plateInputFile")
	well_input <- read_csv(well_path, "wellInputFile")
	plate_input <- read_csv(plate_path, "plateInputFile")
    
    #Ensure that the input files contain the required columns and valid data.
	required_well_columns <- c(
		"WellID", "Drug1_name", "Drug1_concentration", "Drug2_name",
		"Drug2_concentration"
	)
	missing_well_columns <- setdiff(required_well_columns, names(well_input))

    #If any required columns are missing from the well input file, stop with an error.
	if (length(missing_well_columns) > 0L) {
		stop(sprintf("`wellInputFile` is missing required columns: %s",
					 paste(missing_well_columns, collapse = ", ")), call. = FALSE)
	}
	missing_plate_columns <- setdiff(c("filepath", "sample_name"),
									 names(plate_input))
	if (length(missing_plate_columns) > 0L) {
		stop(sprintf("`plateInputFile` is missing required columns: %s",
					 paste(missing_plate_columns, collapse = ", ")), call. = FALSE)
	}
	if (nrow(well_input) == 0L || nrow(plate_input) == 0L) {
		stop("Both input files must contain at least one data row.", call. = FALSE)
	}

	well_ids <- as.character(well_input$WellID)
	if (anyNA(well_ids) || any(!nzchar(well_ids)) || anyDuplicated(well_ids)) {
		stop("`wellInputFile` must contain unique, non-empty `WellID` values.",
			 call. = FALSE)
	}

    #Esnures that the well IDs are in the correct format and that the plate layout is complete and rectangular.
	well_parts <- regexec("^([A-Z])([0-9]+)$", well_ids)
	well_matches <- regmatches(well_ids, well_parts)
	if (any(lengths(well_matches) != 3L)) {
		stop(paste0("`WellID` values must use the format `A001` with a single ",
						 "letter and numeric column."), call. = FALSE)
	}
	well_rows <- vapply(well_matches, `[[`, character(1), 2L)
	well_columns <- as.integer(vapply(well_matches, `[[`, character(1), 3L))
	row_indices <- match(well_rows, LETTERS)
	if (anyNA(row_indices) || any(well_columns < 1L)) {
		stop("`wellInputFile` contains invalid row or column labels.",
			 call. = FALSE)
	}
	row_labels <- LETTERS[seq_len(max(row_indices))]
	if (!identical(sort(unique(well_rows)), row_labels) ||
			!identical(sort(unique(well_columns)), seq_len(max(well_columns))) ||
			nrow(well_input) != length(row_labels) * max(well_columns)) {
		stop("`wellInputFile` must describe a complete rectangular plate layout.",
			 call. = FALSE)
	}
	if (any(row_indices > 26L) ||
			!identical(sort(unique(well_columns)), seq_len(max(well_columns))) ||
			nrow(well_input) != length(row_labels) * max(well_columns)) {
		stop("`wellInputFile` must describe a complete rectangular plate layout.",
			 call. = FALSE)
	}
	row_labels <- LETTERS[seq_len(length(row_labels))]
	column_count <- max(well_columns)
	well_order <- order(match(well_rows, row_labels), well_columns)
	well_input <- well_input[well_order, , drop = FALSE]
	well_ids <- well_ids[well_order]

	if (!is.character(sep) || length(sep) != 1L || is.na(sep) || !nzchar(sep)) {
		stop("`sep` must be a single non-empty character string.", call. = FALSE)
	}
	if (!is.null(rowRange)) {
		if (!is.numeric(rowRange) || length(rowRange) != 2L ||
				anyNA(rowRange) || any(!is.finite(rowRange)) ||
				any(rowRange != as.integer(rowRange)) || rowRange[1L] < 1L ||
				rowRange[1L] > rowRange[2L]) {
			stop("`rowRange` must be two increasing positive integer line numbers.",
				 call. = FALSE)
		}
	}
	if (!is.null(tablePattern) && (!is.character(tablePattern) ||
			length(tablePattern) != 1L || is.na(tablePattern) ||
			!nzchar(tablePattern))) {
		stop("`tablePattern` must be a single non-empty character string.",
			 call. = FALSE)
	}
	validate_controls <- function(values, argument) {
		if (!is.null(values) && (!is.character(values) || anyNA(values))) {
			stop(sprintf("`%s` must be a character vector without `NA` values.",
						 argument), call. = FALSE)
		}
		values
	}
	negWell <- validate_controls(negWell, "negWell")
	posWell <- validate_controls(posWell, "posWell")
	#Ensure that the negative and positive control names do not overlap.
    if (!is.null(negWell) && !is.null(posWell) &&
			any(intersect(negWell, posWell))) {
		stop("`negWell` and `posWell` contain overlapping control names.",
			 call. = FALSE)
	}

	parse_fields <- function(line) {
		fields <- trimws(strsplit(line, split = sep, fixed = TRUE)[[1L]])
		while (length(fields) > 0L && !nzchar(fields[length(fields)])) {
			fields <- fields[-length(fields)]
		}
		fields
	}
	is_header <- function(fields) {
		if (length(fields) < 2L || nzchar(fields[1L])) {
			return(FALSE)
		}
		columns <- suppressWarnings(as.integer(fields[-1L]))
		!anyNA(columns) && identical(columns, seq_along(columns))
	}
	is_measurement <- function(fields, expected_columns) {
		if (length(fields) != expected_columns + 1L ||
				!grepl("^[A-Z]$", fields[1L])) {
			return(FALSE)
		}
		values <- fields[-1L]
		valid_missing <- is.na(values) | !nzchar(values) |
			toupper(values) %in% c("NA", "NAN")
		numeric_values <- suppressWarnings(as.numeric(values))
		all(valid_missing | !is.na(numeric_values))
	}

	extract_table <- function(path, experiment_id) {
		lines <- tryCatch(
			readLines(path, encoding = "UTF-8", warn = FALSE),
			error = function(error) {
				stop(sprintf("Could not read raw file `%s`: %s", path,
							 conditionMessage(error)), call. = FALSE)
			}
		)
		if (!is.null(rowRange)) {
			if (rowRange[2L] > length(lines)) {
				stop(sprintf("`rowRange` exceeds the number of lines in `%s`.", path),
					 call. = FALSE)
			}
			starts <- as.integer(rowRange[1L])
			candidate_ranges <- list(seq.int(rowRange[1L], rowRange[2L]))
		} else {
			candidate_lines <- if (is.null(tablePattern)) {
				seq_along(lines)
			} else {
				grep(tablePattern, lines, perl = TRUE)
			}
			candidate_lines <- candidate_lines[vapply(candidate_lines, function(i) {
				is_header(parse_fields(lines[i]))
			}, logical(1))]
			starts <- candidate_lines
			candidate_ranges <- lapply(starts, function(start) {
				header <- parse_fields(lines[start])
				end <- start
				while (end + 1L <= length(lines) &&
						is_measurement(parse_fields(lines[end + 1L]), length(header) - 1L)) {
					end <- end + 1L
				}
				seq.int(start, end)
			})
		}
		valid_tables <- list()
		for (index in seq_along(candidate_ranges)) {
			range <- candidate_ranges[[index]]
			fields <- lapply(lines[range], parse_fields)
			if (length(fields) < 2L || !is_header(fields[[1L]])) {
				next
			}
			number_columns <- length(fields[[1L]]) - 1L
			data_fields <- fields[-1L]
			if (all(vapply(data_fields, is_measurement, logical(1),
							 expected_columns = number_columns))) {
				row_values <- vapply(data_fields, `[[`, character(1), 1L)
				if (!anyDuplicated(row_values)) {
					start <- candidate_ranges[[index]][1L]
					preceding_lines <- if (start > 1L) {
						trimws(lines[seq_len(start - 1L)])
					} else {
						character()
					}
					valid_tables[[length(valid_tables) + 1L]] <- list(
						fields = fields,
						is_results_table = any(grepl("^Results\\s+for\\b",
												 preceding_lines, ignore.case = TRUE))
					)
				}
			}
		}
		preferred_tables <- Filter(function(table) table$is_results_table,
									 valid_tables)
		if (length(preferred_tables) > 0L) {
			valid_tables <- preferred_tables
		}
		if (length(valid_tables) != 1L) {
			stop(sprintf("Expected exactly one numeric measurement table in `%s`; found %d.",
						 path, length(valid_tables)), call. = FALSE)
		}
		fields <- valid_tables[[1L]]$fields
		number_columns <- length(fields[[1L]]) - 1L
		row_values <- vapply(fields[-1L], `[[`, character(1), 1L)
		if (!identical(row_values, row_labels) || number_columns != column_count) {
			stop(sprintf("Measurement table in `%s` has dimensions %d x %d; expected %d x %d.",
						 path, length(row_values),
						 number_columns, length(row_labels), column_count), call. = FALSE)
		}
		measurements <- unlist(lapply(fields[-1L], function(row) {
			values <- row[-1L]
			values[is.na(values) | !nzchar(values) |
					toupper(values) %in% c("NA", "NAN")] <- NA_character_
			as.numeric(values)
		}), use.names = FALSE)
		data.frame(
			wellID = well_ids,
			experimentID = rep(experiment_id, length(well_ids)),
			raw_count = measurements,
			stringsAsFactors = FALSE
		)
	}

	if (anyNA(plate_input$filepath) || any(!nzchar(as.character(plate_input$filepath)))) {
		stop("`plateInputFile` contains missing or empty `filepath` values.",
			 call. = FALSE)
	}
	if (anyNA(plate_input$sample_name) || any(!nzchar(as.character(plate_input$sample_name)))) {
		stop("`plateInputFile` contains missing or empty `sample_name` values.",
			 call. = FALSE)
	}
	plate_directory <- dirname(plate_path)
	results <- vector("list", nrow(plate_input))
	for (index in seq_len(nrow(plate_input))) {
		raw_path <- as.character(plate_input$filepath[index])
		if (!grepl("^(/|[A-Za-z]:[/\\\\])", raw_path)) {
			raw_path <- file.path(plate_directory, raw_path)
		}
		raw_path <- validate_file(raw_path, "plateInputFile filepath")
		results[[index]] <- extract_table(raw_path,
										  as.character(plate_input$sample_name[index]))
	}

	screen_data <- do.call(rbind, results)
	screen_data <- cbind(
		screen_data[c("wellID", "experimentID", "raw_count")],
		well_input[match(screen_data$wellID, well_ids),
			c("Drug1_name", "Drug1_concentration", "Drug2_name",
			  "Drug2_concentration"), drop = FALSE]
	)
	names(screen_data) <- c("wellID", "experimentID", "raw_count",
							"drug1_name", "drug1_concentration", "drug2_name",
							"drug2_concentration")
	if (!is.null(negWell) || !is.null(posWell)) {
		neg_match <- if (is.null(negWell)) rep(FALSE, nrow(screen_data)) else
			screen_data$drug1_name %in% negWell |
				screen_data$drug2_name %in% negWell
		pos_match <- if (is.null(posWell)) rep(FALSE, nrow(screen_data)) else
			screen_data$drug1_name %in% posWell |
				screen_data$drug2_name %in% posWell
		if (any(neg_match & pos_match)) {
			stop("At least one well matches both negative and positive controls.",
				 call. = FALSE)
		}
		if (length(negWell) > 0L && !any(neg_match)) {
			warning("None of the values in `negWell` matched the well metadata.",
					call. = FALSE)
		}
		if (length(posWell) > 0L && !any(pos_match)) {
			warning("None of the values in `posWell` matched the well metadata.",
					call. = FALSE)
		}
		control <- rep(NA_character_, nrow(screen_data))
		control[neg_match] <- "neg"
		control[pos_match] <- "pos"
		screen_data$control <- control
	}
	screen_data
}
