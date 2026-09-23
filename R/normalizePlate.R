#' Normalize raw assay values using documented control wells.
#'
#' @param screenData A data frame or tibble produced by `readScreen()`.
#' @param method Normalization method. Supported values are `"negative"` and
#'   `"npi"`.
#' @param posWell Optional character vector identifying positive-control wells.
#' @param negWell Optional character vector identifying negative-control wells.
#' @param by Grouping level used for control medians: `"plate"`,
#'   `"experiment"`, or `"experiment_plate"`.
#'
#' @return A data frame with a new `viab_norm` column appended.
#' @export
normalizePlate <- function(screenData = NULL, method = NULL, posWell = NULL,
                          negWell = NULL, by = "experiment") {

    ### Validate inputs and prepare data for normalization.
    if (is.null(screenData)) {
        stop("`screenData` must be supplied.", call. = FALSE)
    }
    if (!is.data.frame(screenData)) {
        stop("`screenData` must be a data frame or tibble.", call. = FALSE)
    }

    missing_columns <- setdiff(c("experimentID", "plateID", "WellID", "raw_count", "drug1_name"), names(screenData))
    if (length(missing_columns) > 0L) {
        stop(
            paste0("`screenData` is missing required columns: ", paste(missing_columns, collapse = ", ")),
            call. = FALSE
        )
    }

    if (!is.numeric(screenData$raw_count)) {
        stop("`screenData$raw_count` must be numeric.", call. = FALSE)
    }

    valid_methods <- c("negative", "npi")
    if (!is.null(method) && !method %in% valid_methods) {
        stop("`method` must be one of `\"negative\"` or `\"npi\"`.", call. = FALSE)
    }

    valid_by <- c("plate", "experiment", "experiment_plate")
    if (!by %in% valid_by) {
        stop("`by` must be one of `\"plate\"`, `\"experiment\"`, or `\"experiment_plate\"`.", call. = FALSE)
    }

    # Validate the grouping columns requested by `by`.
    if (by == "plate" && !("plateID" %in% names(screenData))) {
        stop("`by = \"plate\"` requires a `plateID` column in `screenData`.", call. = FALSE)
    }
    if (by == "experiment" && !("experimentID" %in% names(screenData))) {
        stop("`by = \"experiment\"` requires an `experimentID` column in `screenData`.", call. = FALSE)
    }
    if (by == "experiment_plate" && !(all(c("experimentID", "plateID") %in% names(screenData)))) {
        stop("`by = \"experiment_plate\"` requires `experimentID` and `plateID` columns in `screenData`.", call. = FALSE)
    }

    # Preserve standard package schema while allowing the caller to override the
    # control labels for this normalization run.
    out <- screenData
    out$drug2_name <- if ("drug2_name" %in% names(out)) out$drug2_name else NA_character_

    known_names <- unique(c(out$drug1_name, out$drug2_name))
    known_names <- known_names[!is.na(known_names)]

    effective_control <- if ("control" %in% names(out)) {
        as.character(out$control)
    } else {
        rep(NA_character_, nrow(out))
    }

    if (!is.null(posWell)) {
        posWell <- unique(posWell)
        if (!all(posWell %in% known_names)) {
            stop("posWell contains value(s) not found among drug names", call. = FALSE)
        }

        pos_matches <- out$drug1_name %in% posWell | out$drug2_name %in% posWell
        effective_control[pos_matches] <- "pos"
    }

    if (!is.null(negWell)) {
        negWell <- unique(negWell)
        if (!all(negWell %in% known_names)) {
            stop("`negWell` value(s) not found among drug names", call. = FALSE)
        }

        neg_matches <- out$drug1_name %in% negWell | out$drug2_name %in% negWell
        effective_control[neg_matches] <- "neg"
    }

    out$control <- effective_control

    neg_present <- any(out$control == "neg", na.rm = TRUE)
    if (!neg_present) {
        stop("No negative controls provided, required for normalization", call. = FALSE)
    }

    if (is.null(method)) {
        method <- if (any(out$control == "pos", na.rm = TRUE)) "npi" else "negative"
    }

    if (method == "npi") {
        pos_present <- any(out$control == "pos", na.rm = TRUE)
        if (!pos_present) {
            stop("NPI method called, but no positive controls found", call. = FALSE)
        }
    }

    out$viab_norm <- NA_real_

    if (by == "plate") {
        grouping_cols <- "plateID"
    } else if (by == "experiment") {
        grouping_cols <- "experimentID"
    } else {
        grouping_cols <- c("experimentID", "plateID")
    }

    out <- out %>%
        dplyr::group_by(across(all_of(grouping_cols))) %>%
        dplyr::mutate(
            med_neg = median(raw_count[control == "neg"], na.rm = TRUE),
            med_pos = if (method == "npi") {
                median(raw_count[control == "pos"], na.rm = TRUE)
            } else {
                NA_real_
            },
            viab_norm = if (method == "npi") {
                (raw_count - med_pos) / (med_neg - med_pos)
            } else {
                raw_count / med_neg
            }
        ) %>%
        dplyr::select(-c(med_neg, med_pos)) %>%
        dplyr::ungroup()

    return(out)
}
