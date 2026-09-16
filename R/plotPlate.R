#' @importFrom ggplot2 ggplot aes geom_tile scale_y_discrete xlab ylab
#' @importFrom ggplot2 theme_void ggtitle theme element_text element_blank margin

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
    ids <- parse_well_ids(screenData$wellID)
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

