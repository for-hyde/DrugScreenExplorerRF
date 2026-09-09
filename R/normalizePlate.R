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
                          negWell = NULL, by = "plate") {

    ### Validate inputs and prepare data for normalization.
    #screendata
    if (is.null(screenData)) {
        stop("`screenData` must be supplied.", call. = FALSE)
    }
    if (!is.data.frame(screenData)) {
        stop("`screenData` must be a data frame or tibble.", call. = FALSE)
    }
    
    #raw_count column in screenData
    if (!("raw_count" %in% names(screenData))) {
        stop("`screenData` must contain a numeric raw-value column named `raw_count`, `raw_value`, or `measurement`.", call. = FALSE)
    }
    if (!is.numeric(screenData$raw_count)) {
        stop("`screenData$raw_count` must be numeric.", call. = FALSE)
    }

    #if method supplied ensure it is valid, otherwise determine method based on presence of controls
    valid_methods <- c("negative", "npi")
    if (!is.null(method) && !method %in% valid_methods) {
        stop("`method` must be one of `\"negative\"` or `\"npi\"`.", call. = FALSE)
    }

    #Ensure `by` argument is valid
    valid_by <- c("plate", "experiment", "experiment_plate")
    if (!by %in% valid_by) {
        stop("`by` must be one of `\"plate\"`, `\"experiment\"`, or `\"experiment_plate\"`.", call. = FALSE)
    }

    
    #Extract values from control column if present
    if ("control" %in% names(screenData)) {
        pos_drugs <- unique(screenData$drug1_name[tolower(screenData$control) == "pos"])
        pos_drugs <- pos_drugs[!is.na(pos_drugs)]
        neg_drugs <- unique(screenData$drug1_name[tolower(screenData$control) == "neg"])
        neg_drugs <- neg_drugs[!is.na(neg_drugs)]
    } 

    #Check that supplied drug names are present in the dataset
    if (!is.null(posWell) && all(posWell %in% unique(c(screenData$drug1_name, screenData$drug2_name)))) {
        pos_drugs <- posWell #Override pos_drugs with user-supplied posWell if present and in set
    } else if (!is.null(posWell) && !all(posWell %in% unique(c(screenData$drug1_name, screenData$drug2_name)))) {
        stop("posWell contains value(s) not found among drug names", call. = FALSE)
    }

    if (!is.null(negWell) && all(negWell %in% unique(c(screenData$drug1_name, screenData$drug2_name)))) {
        neg_drugs <- negWell #Override neg_drugs with user-supplied negWell if present and in set
    } else if (!is.null(negWell) && !all(negWell %in% unique(c(screenData$drug1_name, screenData$drug2_name)))){
        stop("`negWell` value(s) not found among drug names", call. = FALSE)
    }

    #If there are no negative controls in list, throw an error
    if(length(neg_drugs) == 0 || is.na(neg_drugs)) {
        stop("No negative controls provided, required for normalization", call. = FALSE)
    }
    
    #Update control column to based on inputs
    out <- screenData
    out$control <- NA_character_ #clear control collumn,
    # negcontrol should be confirmed by this point
    # If is single drug
    if (all(is.na(out$drug2_name))){
        neg_matches <- out$drug1_name %in% neg_drugs
        if (exists("pos_drugs", inherits = FALSE)){
            pos_matches <- out$drug1_name %in% pos_drugs
        }
    } else { #if combination
        neg_matches <- out$drug1_name %in% neg_drugs & out$drug1_name == out$drug2_name
        if (exists("pos_drugs", inherits = FALSE)){
            pos_matches <- out$drug1_name %in% pos_drugs | out$drug2_name %in% pos_drugs
        }
    }

    #Update control column
    out$control[neg_matches] <- "neg"
    if (exists("pos_matches", inherits = FALSE)){
        out$control[pos_matches] <- "pos"
    }

    #Determine method if no method supplied
    if (is.null(method)) {
        #Detect if positive and negative controls are present in the control column, if not, throw an error
        values <- tolower(unique(screenData$control))
        #if no neg, throw error
        if (!any(values %in% c("neg"))) {
            stop("`screenData` must contain a `control` column or `negWell` and `posWell` must be supplied.", call. = FALSE)
        }
        else if (!any(values %in% c("pos"))) {
            method <- "negative"
        }
        else {
            method <- "npi"
        }
    }

    #initialize column for normalized viability values
    out$viab_norm <- NA_real_

    #Output
    out <- out %>%
        group_by(experimentID) %>%
        mutate(
            # 1. Fixed subsetting bracket and added comma
            med_neg = median(raw_count[control == "neg"], na.rm = TRUE),
            
            # 2. Conditionally calculate med_pos only if method is "npi"
            med_pos = if (method == "npi") {
                median(raw_count[control == "pos"], na.rm = TRUE)
            } else {
                NA_real_
            },
            
            # 3. Conditionally calculate viab_norm based on method
            viab_norm = if (method == "npi") {
                (raw_count - med_pos) / (med_neg - med_pos)
            } else {
                raw_count / med_neg
            }
        ) %>%
        select(!c(med_neg, med_pos)) %>%
        ungroup()

    return(out)
}
