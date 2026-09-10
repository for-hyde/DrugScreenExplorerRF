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
    if (plotType == "zscore" && !("viab.norm" %in% names(screenData))) {
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

    # Generate plots
    for (plate_id in plates_to_plot) {
        plate <- screenData[screenData$experimentID == plate_id, ]
        # Separate well info to plot plate
        ids <- parse_well_ids(plate$wellID)
        
        # Create a new data frame with the parsed well IDs and the original data
        plate$Row <- ids$x
        plate$Column <- ids$y

        #Extract relevant coloring data
        if (plotType == "viability") {
            color_data <- plate$raw_count
            color_label <- "Raw Viability" #Working, consider adding normalization option
        #Write normalization, then try z-scre
        } else if (plotType == "zscore") {
            color_data <- calculate_zscore(plate)
            color_label <- "Z-score"
        } else if (plotType == "layout") {
            color_data <- plate$control
            color_label <- "Control Type" #Working, consider adding color scheme for controls.
        } else if (plotType == "edgeEffect") {
            color_data <- estimate_edge_effect(plate$raw_data) #Should I use the raw or normalized data here?
            color_label <- "Edge Effect"
        }
        # Add the color data to the plate data frame
        plate$ColorData <- color_data

        # Create the plot object
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

        plotlist <- c(plotlist, list(p))

    }
    # If outputPath is provided, save the plot to a file
    if (!is.null(outputPath)) {
        ggsave(filename = file.path(outputPath, paste0("plate_", plate_id, "_", plotType, ".png")),
               plot = p, width = width, height = height, units = "cm")
    }
    #This part is wrong...
    if (returnObject) {
        return(plotlist)
    } else {
        return(plotlist)
    }


}

