# Function to check that screenData is in the proper format
check_screen_data_format <- function(screenData,
                                     normalized = FALSE,
                                     edge_effect = FALSE) {
    # Check that screenData is a data frame or tibble
    if (!is.data.frame(screenData) || nrow(screenData) == 0L) {
        stop("`screenData` must be a non-empty data frame or tibble.",
             call. = FALSE)
    }
    # Check for required columns based on the context
    required_columns <- c("experimentID", "plateID", "WellID", "raw_count")
    if (normalized) {
        required_columns <- c(required_columns, "viab_norm")
    } else if (edge_effect) {
        required_columns <- c(required_columns, "edgeFactor")
    }
    missing_columns <- setdiff(required_columns, names(screenData))
    if (length(missing_columns) > 0L) {
        stop(sprintf("`screenData` is missing required columns: %s",
                     paste(missing_columns, collapse = ", ")), call. = FALSE)
    }

    if (!is.numeric(screenData$raw_count)) {
        stop("`screenData$raw_count` must be numeric.", call. = FALSE)
    }
    if (normalized && !is.numeric(screenData$viab_norm)) {
        stop("`screenData$viab_norm` must be numeric when `normalized = TRUE`.",
             call. = FALSE)
    }
    if (edge_effect && !is.numeric(screenData$edgeFactor)) {
        stop("`screenData$edgeFactor` must be numeric when `edge_effect = TRUE`.",
             call. = FALSE)
    }


}

# Helper function called in printPlate that takes the well IDs and splits them into rows and columns.
parse_well_ids <- function(well_ids) {
    if (!is.character(well_ids)) {
        stop("`well_ids` must be a character vector.", call. = FALSE)
    }
    #Seems inefficient. Keep for now.
    rows <- c()
    columns <- c()
    for (well in well_ids) {
        rows <- c(rows, substr(well, 1, 1))
        columns <- c(columns, substr(well, 2, 4))
    }

    return (list(x = rows, y = columns))
}

#Function for determining z_score
calculate_zscore <- function(plate) {
    neg_mask <- plate$control == "neg"
    neg_mean <- mean(plate$viab_norm[neg_mask], na.rm = TRUE)
    neg_sd <- sd(plate$viab_norm[neg_mask], na.rm = TRUE)

    if (is.na(neg_mean) || is.na(neg_sd) || neg_sd == 0) {
        stop("Unable to compute z-score: missing or zero variance in negative controls.", call. = FALSE)
    }

    z_score <- (plate$viab_norm - neg_mean) / neg_sd
    return(z_score)
}

#Function to determining which colors to run in plotting functions. 
get_color_data <- function(plate, plotType){
    switch(
        plotType,

        viability = list(
            data = plate$raw_count,
            label = "Raw Viability"
        ),
        zscore = list(
            data = calculate_zscore(plate),
            label = "Z-score"
        ),
        layout = list(
            data = plate$control,
            label = "Control Type"
        ),
        edgeEffect = list(
            data = estimate_edge_effect(plate),
            label = "Edge Effect"
        ),

        stop("Unknown plotType: ", plotType)
    )
}

fitOneLoess <- function(plate, response, span) {
    fitting_data <- data.frame(
        row_num = plate$row_num,
        col_num = plate$col_num,
        response = response
    )
    usable <- is.finite(fitting_data$row_num) & is.finite(fitting_data$col_num) &
        is.finite(response)
    if (sum(usable) < 4L) {
        stop("LOESS edge-effect fitting requires at least four usable wells.",
             call. = FALSE)
    }
    fit <- stats::loess(
        response ~ row_num + col_num,
        data = fitting_data[usable, , drop = FALSE],
        span = span,
        degree = 1L,
        na.action = stats::na.exclude,
        control = stats::loess.control(surface = "direct")
    )
    fitted <- as.numeric(stats::predict(fit, newdata = fitting_data))
    if (any(!is.finite(fitted))) {
        stop("LOESS edge-effect fitting produced non-finite values.",
             call. = FALSE)
    }
    fitted
}

fitOneSigmoid <- function(plate, response) {
    usable <- is.finite(plate$row_num) & is.finite(plate$col_num) &
        is.finite(response)
    if (sum(usable) < 12L) {
        stop("Sigmoid edge-effect fitting requires at least twelve usable wells.",
             call. = FALSE)
    }
    row_scale <- max(plate$row_num, na.rm = TRUE)
    col_scale <- max(plate$col_num, na.rm = TRUE)
    row_sigmoid <- stats::plogis((plate$row_num - (row_scale + 1) / 2) /
        max(row_scale / 4, 1))
    col_sigmoid <- stats::plogis((plate$col_num - (col_scale + 1) / 2) /
        max(col_scale / 4, 1))
    design <- cbind(
        1,
        row_sigmoid,
        col_sigmoid,
        row_sigmoid * col_sigmoid,
        row_sigmoid^2,
        col_sigmoid^2,
        row_sigmoid^2 * col_sigmoid,
        row_sigmoid * col_sigmoid^2,
        row_sigmoid^2 * col_sigmoid^2,
        plate$row_num,
        plate$col_num,
        plate$row_num * plate$col_num
    )
    fit <- stats::lm.fit(design[usable, , drop = FALSE], response[usable])
    if (anyNA(fit$coefficients)) {
        stop("Sigmoid edge-effect fitting failed because the surface is singular.",
             call. = FALSE)
    }
    fitted <- as.numeric(design %*% fit$coefficients)
    if (any(!is.finite(fitted))) {
        stop("Sigmoid edge-effect fitting produced non-finite values.",
             call. = FALSE)
    }
    fitted
}

estimate_edge_effect <- function(plate, method = "loess", span = 1) {
    if (!is.data.frame(plate) || !"viab_norm" %in% names(plate) ||
        !any(c("wellID", "WellID") %in% names(plate))) {
        stop("`plate` must contain `wellID` and `viab_norm`.", call. = FALSE)
    }
    well_ids <- if ("wellID" %in% names(plate)) plate$wellID else plate$WellID
    coordinates <- parse_well_ids(well_ids)
    fitting_data <- data.frame(
        row_num = match(coordinates$x, LETTERS),
        col_num = suppressWarnings(as.numeric(coordinates$y))
    )
    if (method == "loess") {
        fitOneLoess(fitting_data, plate$viab_norm, span)
    } else if (method == "sigmoid") {
        fitOneSigmoid(fitting_data, plate$viab_norm)
    } else {
        stop("`method` must be one of `\"loess\"` or `\"sigmoid\"`.",
             call. = FALSE)
    }
}

# Used by the Shiny preprocessing module.
load_from_zip <- function(zippath,
                            separator = ",",
                            wellfilename = "wellInput.csv",
                            platefilename = "plateInput.csv") {
    if (!is.character(zippath) || length(zippath) != 1L || is.na(zippath) ||
            !file.exists(zippath) || dir.exists(zippath)) {
        stop("`zippath` must identify one existing ZIP file.", call. = FALSE)
    }
    temp_dir <- tempfile("screen-zip-")
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
    utils::unzip(zippath, exdir = temp_dir)
    extracted_files <- list.files(temp_dir, full.names = TRUE, recursive = TRUE)

    find_one <- function(filename, label) {
        matches <- extracted_files[
            tolower(basename(extracted_files)) == tolower(filename)
        ]
        if (length(matches) != 1L) {
            stop(sprintf("Expected exactly one %s in the ZIP archive; found %d.",
                                     label, length(matches)), call. = FALSE)
        }
        matches
    }
    well_path <- find_one(wellfilename, "well metadata file")
    plate_path <- find_one(platefilename, "plate metadata file")
    plate_input <- utils::read.csv(
        plate_path,
        stringsAsFactors = FALSE,
        check.names = FALSE
    )
    if (!all(c("filepath", "sample_name") %in% names(plate_input))) {
        stop("The archived plate metadata file must contain `filepath` and `sample_name`.",
                 call. = FALSE)
    }

    resolve_raw_file <- function(path) {
        matches <- extracted_files[
            basename(extracted_files) == basename(as.character(path))
        ]
        if (length(matches) != 1L) {
            stop(sprintf("Expected exactly one archived raw file for `%s`; found %d.",
                                     path, length(matches)), call. = FALSE)
        }
        matches
    }
    plate_input$filepath <- vapply(
        plate_input$filepath,
        resolve_raw_file,
        character(1)
    )
    rewritten_plate <- file.path(temp_dir, "plateInput-resolved.csv")
    utils::write.csv(plate_input, rewritten_plate, row.names = FALSE)
    readScreen(wellInputFile = well_path, plateInputFile = rewritten_plate)
}

#' Parse a delimited file with optional row and column ranges
#' called by readScreen when provided exact location to extract.
#' 
#' @param file_path Path to the CSV/TSV file.
#' @param sep Separator character (e.g., ",", "\t", ";").
#' @param row_range Integer vector representing row range (e.g., 1:12). 
#' @param col_range Integer vector representing column range (e.g., 1:5). 
#' @return A subsetted data frame.
manual_parse <- function(file_path, sep = ",", row_range = NULL, col_range = NULL) {
  
  # 1. Read the full file raw (no headers assumed initially to preserve exact coordinate mapping)
  df <- utils::read.table(
    file_path, 
    sep = sep, 
    header = FALSE, 
    stringsAsFactors = FALSE, 
    fill = TRUE,
    quote = ""
  )
  
  # 2. Handle row indexing (default to all rows if NULL)
  if (is.null(row_range)) {
    row_range <- seq_len(nrow(df))
  } else {
    # Optional safety filter to prevent out-of-bounds errors
    row_range <- row_range[row_range <= nrow(df) & row_range > 0]
  }
  
  # 3. Handle column indexing (default to all columns if NULL)
  if (is.null(col_range)) {
    col_range <- seq_len(ncol(df))
  } else {
    col_range <- col_range[col_range <= ncol(df) & col_range > 0]
  }
  
  # 4. Apply the position-based subsetting safely
  df <- df[row_range, col_range, drop = FALSE]
  
  return(df)
}


#  #Add as function called in Shiny application to ensure Row and Column ranges are properly chosen.

#   # If provided as a string, evaluate the row_range expression to get numeric indices
#   if (row_range.typeof(row_range) == "character" && nzchar(row_range)) {
#     row_range <- suppressWarnings(tryCatch(eval(parse(text = row_range)), error = function(e) NULL))
#   }
#   # If provided as a string, evaluate the col_range expression to get numeric indices
#   if (typeof(col_range) == "character" && nzchar(col_range)) {
#     col_range <- suppressWarnings(tryCatch(eval(parse(text = col_range)), error = function(e) NULL))
#   }

#   # 2. Apply row range if provided and not empty
#   if (!is.null(row_range) && nzchar(row_range)) {
#     # Safely evaluate string like "1:12" into a numeric vector 1:12
#     rows <- suppressWarnings(tryCatch(eval(parse(text = row_range)), error = function(e) NULL))
#     if (!is.null(rows) && is.numeric(rows)) {
#       # Ensure indices don't exceed actual rows in the file
#       rows <- rows[rows <= nrow(df) & rows > 0]
#       df <- df[rows, , drop = FALSE]
#     }
#   }
  
#   # 3. Apply column range if provided and not empty
#   if (!is.null(col_range) && nzchar(col_range)) {
#     cols <- NULL
    
#     # Check if user entered Excel-style letters (e.g., "A:H" or "a:h")
#     if (grepl("^[A-Za-z]+:[A-Za-z]+$", col_range)) {
#       parts <- strsplit(toupper(col_range), ":")[[1]]
#       # Simple letter-to-index conversion (A=1, B=2, ..., Z=26)
#       col_start <- match(parts[1], LETTERS)
#       col_end <- match(parts[2], LETTERS)
#       if (!is.na(col_start) && !is.na(col_end)) {
#         cols <- col_start:col_end
#       }
#     } else {
#       # Otherwise assume a numeric range string like "1:5"
#       cols <- suppressWarnings(tryCatch(eval(parse(text = col_range)), error = function(e) NULL))
#     }
    
#     if (!is.null(cols) && is.numeric(cols)) {
#       cols <- cols[cols <= ncol(df) & cols > 0]
#       df <- df[, cols, drop = FALSE]
#     }
#   }
  
#   return(df)
# }
