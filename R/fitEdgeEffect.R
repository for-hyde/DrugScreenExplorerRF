#' Fit a two-dimensional surface to estimate per-plate edge effects.
#'
#' @param screenData A data frame or tibble containing the standard
#'   `readScreen()` columns and a numeric `viab_norm` column from
#'   `normalizePlate()`.
#' @param method Fitting method: `"loess"` or `"sigmoid"`.
#' @param useNeg If `TRUE`, fit negative-control wells only.
#' @param useLowConcentrations Non-negative integer for including low-dose
#'   sample wells. This option is not yet supported by the current schema.
#' @param span Positive LOESS smoothing parameter.
#' @param exclude Character vector of `plateID` values excluded from fitting.
#' @return A copy of `screenData` with a numeric `edgeFactor` column.
#' @export
fitEdgeEffect <- function(screenData, method = "loess", useNeg = TRUE,
                          useLowConcentrations = 0L, span = 1,
                          exclude = character()) {
    if (!is.data.frame(screenData) || nrow(screenData) == 0L) {
        stop("`screenData` must be a non-empty data frame or tibble.",
             call. = FALSE)
    }
    required_columns <- c("plateID", "wellID", "viab_norm")
    missing_columns <- setdiff(required_columns, names(screenData))
    if (length(missing_columns) > 0L) {
        stop(sprintf("`screenData` is missing required columns: %s",
                     paste(missing_columns, collapse = ", ")), call. = FALSE)
    }
    if (!is.numeric(screenData$viab_norm)) {
        stop("`screenData$viab_norm` must be numeric.", call. = FALSE)
    }
    if (!is.character(method) || length(method) != 1L || is.na(method) ||
        !method %in% c("loess", "sigmoid")) {
        stop("`method` must be one of `\"loess\"` or `\"sigmoid\"`.",
             call. = FALSE)
    }
    if (!is.logical(useNeg) || length(useNeg) != 1L || is.na(useNeg)) {
        stop("`useNeg` must be a single non-missing logical value.", call. = FALSE)
    }
    if (!is.numeric(useLowConcentrations) ||
        length(useLowConcentrations) != 1L ||
        is.na(useLowConcentrations) ||
        useLowConcentrations < 0 ||
        useLowConcentrations != as.integer(useLowConcentrations)) {
        stop("`useLowConcentrations` must be a non-negative integer.",
             call. = FALSE)
    }
    if (useLowConcentrations > 0L) {
        stop("`useLowConcentrations > 0` is not yet supported with the current schema.",
             call. = FALSE)
    }
    if (!is.numeric(span) || length(span) != 1L || is.na(span) ||
        !is.finite(span) || span <= 0) {
        stop("`span` must be a single positive finite numeric value.",
             call. = FALSE)
    }
    if (!is.character(exclude) || anyNA(exclude)) {
        stop("`exclude` must be a character vector without `NA` values.",
             call. = FALSE)
    }
    plate_ids <- as.character(screenData$plateID)
    if (anyNA(plate_ids) || any(!nzchar(plate_ids))) {
        stop("`screenData$plateID` must contain non-empty values.", call. = FALSE)
    }
    parsed_wells <- parse_well_ids(as.character(screenData$wellID))
    row_num <- match(parsed_wells$x, LETTERS)
    col_num <- suppressWarnings(as.integer(parsed_wells$y))
    if (anyNA(row_num) || anyNA(col_num) || any(col_num < 1L)) {
        stop("`screenData$wellID` contains invalid row or column identifiers.",
             call. = FALSE)
    }
    if (useNeg && !"control" %in% names(screenData)) {
        stop("`screenData` must contain `control` when `useNeg = TRUE`.",
             call. = FALSE)
    }

    out <- screenData
    out$edgeFactor <- 1
    fit_rows <- !plate_ids %in% exclude
    minimum_wells <- if (method == "loess") 4L else 12L

    for (plate_id in unique(plate_ids[fit_rows])) {
        plate_rows <- which(plate_ids == plate_id & fit_rows)
        eligible <- is.finite(out$viab_norm[plate_rows])
        if (useNeg) {
            eligible <- eligible &
                !is.na(out$control[plate_rows]) &
                tolower(as.character(out$control[plate_rows])) == "neg"
        }
        if (sum(eligible) < minimum_wells) {
            stop(sprintf("Plate `%s` has too few eligible wells for `%s` fitting.",
                         plate_id, method), call. = FALSE)
        }
        plate_data <- data.frame(row_num = row_num[plate_rows],
                                 col_num = col_num[plate_rows])
        response <- out$viab_norm[plate_rows]
        response[!eligible] <- NA_real_
        fitted <- if (method == "loess") {
            fitOneLoess(plate_data, response, span)
        } else {
            fitOneSigmoid(plate_data, response)
        }
        out$edgeFactor[plate_rows] <- fitted
    }
    out
}
