#' Plot Screening Plate Data
#'
#' This function generates visualizations of screening plate data, including heatmaps of viability, Z-scores, layout, and edge effects. It supports plotting multiple plates with optional facet wrapping and allows for saving the plots to disk.
#' 
#' @importFrom ggplot2 ggplot aes geom_tile scale_y_discrete xlab ylab
#' @importFrom ggplot2 theme_void ggtitle theme element_text element_blank margin
#' 
#' @param screenData The data frame containing the screen data to be plotted. It must include columns for `experimentID`, `WellID`, and the relevant measurement (e.g., `viab_norm` for viability plots).
#' @param plate A list of character vectors describing a subset of plates to plot.
#' @param plotType A character string specifying the type of plot to generate.
#' @param facetwrap A logical value indicating whether to use facet wrapping for multiple plates.
#' @param outputPath A character string specifying the path to save the plots. If NULL, plots will not be saved to disk.
#' @param ncol An integer specifying the number of columns in the facet wrap layout. Default is 2.
#' @param nrow An integer specifying the number of rows in the facet wrap layout. Default is 3.
#' @param width A numeric value specifying the width of the output plot in inches. Default is 16.
#' @param height A numeric value specifying the height of the output plot in inches. Default is 16.
#' @param returnObject A logical value indicating whether to return the plot object instead of displaying it.
#' @return A list of ggplot objects representing the generated plots.
#' @examples
#' \dontrun{
#' # Example usage of plotPlate function
#' screenData <- readScreen("wellInput.csv", "plateInput.csv")
#' plotPlate(screenData, plate = "all", plotType = "viability", facetwrap = TRUE, outputPath = "plots", ncol = 2, nrow = 3, width = 16, height = 16, returnObject = FALSE)
#' }
#' @export  
plotPlate <- function(
        screenData,
        plate = "all",
        plotType = "viability",
        facetwrap = TRUE,
        outputPath = NULL,
        ncol = 2,
        nrow = 3,
        width = 16,
        height = 16,
        returnObject = FALSE
){
    # Validate inputs and prepare data for plotting.
    if (!is.data.frame(screenData)) {
        stop("`screenData` must be a data frame.", call. = FALSE)
    }
    if (plate != "all" && !(plate %in% unique(screenData$experimentID))) {
        stop(sprintf("`plate` must be one of the experimentID values in `screenData`: %s",
                     paste(unique(screenData$experimentID), collapse = ", ")),
             call. = FALSE)
    }
    if (!(plotType %in% c("viability","zscore","layout","edgeEffect"))){
        stop(sprintf("`plotType` must be one of 'viability', 'zscore', 'layout', or 'edgeEffect'."),
             call. = FALSE)
    }
    if (plotType == "layout" && !("control" %in% names(screenData))) {
        stop("`screenData` must contain a `control` column when `plotType` is 'layout'.", call. = FALSE)
    }
    if (plotType == "zscore" && !("viab_norm" %in% names(screenData))) {
        stop("`screenData` must contain a `viab.norm` column when `plotType` is 'zscore'.", call. = FALSE)
    }
    #If not a null path,
    if (!is.null(outputPath) && (dirname(outputPath) != "." && !is.dir(outputPath))) {
        stop(sprintf("`outputPath` must be a valid directory: %s", outputPath), call. = FALSE)
    }

    # Parse plate IDs.
    if (plate == "all") {
        plates_to_plot <- unique(screenData$experimentID)
    } else {
        plates_to_plot <- plate
    }

    plotlist <- list()

    #Create Well ID info
    ids <- parse_well_ids(screenData$WellID)
    screenData <- screenData %>%
        dplyr::mutate(
            Row = ids$x,
            Column = ids$y
        )

    #Slit into plates
    plates <- screenData %>%
        dplyr::filter(experimentID %in% plates_to_plot) %>%
        dplyr::group_split(experimentID)

    plotlist <- vector("list", length(plates))

    for (i in seq_along(plates)){
        plate <- plates[[i]]
        plate_id <- unique(plate$experimentID)

        # Call helper function and attach the computed color data to the plate data
        color <- get_color_data(plate, plotType)
        plate$ColorData <- color$data

        p <- ggplot(plate, aes(x = Column, y = Row, fill = ColorData)) +
            geom_tile(color = "grey80") +
            scale_y_discrete(limits = rev(levels(factor(plate$Row)))) + #Ensure A sits at top, may replace.
            xlab("")  + ylab("") +
            theme_void() +
            ggtitle(paste("Plate", plate_id, "-", plotType)) +
            theme(axis.text = element_text(size = 6),
                  axis.ticks = element_blank(),
                  plot.title = element_text(hjust = 0.5, size = 10),
                  plot.margin = margin(5, 5, 5, 5))

        plotlist[[i]] <- p


    }
    return (plotlist)
}

