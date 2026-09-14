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

#Function to determining which colors to run in 
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