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
    if (!is.data.frame(plate) ||
        !all(c("wellID", "viab_norm") %in% names(plate))) {
        stop("`plate` must contain `wellID` and `viab_norm`.", call. = FALSE)
    }
    coordinates <- parse_well_ids(plate$wellID)
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
