#' Data-preprocessing module UI.
#'
#' @param id Namespace identifier for the module.
#'
#' @return A Shiny UI fragment.
#' @export
mod_data_preprocessing_ui <- function(id) {
	ns <- shiny::NS(id)
	shiny::tagList(
		shiny::h2("Data preprocessing"),
		shiny::sidebarLayout(
			shiny::sidebarPanel(
				width = 4,
				shiny::fileInput(
					ns("wellfile"),
					"Select well metadata file",
					accept = c(".csv", ".txt", ".tsv")
				),
				shiny::fileInput(
					ns("platefile"),
					"Select plate metadata file",
					accept = c(".csv", ".txt", ".tsv")
				),
				shiny::fileInput(
					ns("zipfile"),
					"(Optional) standard-format ZIP file",
					accept = ".zip"
				),
				shiny::selectInput(
					ns("separator"),
					"CSV separator",
					choices = c(
						"comma" = ",",
						"semicolon" = ";",
						"tab" = "\t"
					),
					selected = ","
				),
				shiny::actionButton(ns("import"), "Import data")
			),
		
			shiny::mainPanel(
				width = 8,
				shiny::textOutput(ns("status")),
				shiny::textOutput(ns("error_message")),
				shiny::br(),
				DT::DTOutput(ns("data_table"))
			)
	)
)}

#' Data-preprocessing module server.
#'
#' @param id Namespace identifier for the module.
#'
#' @return A named list containing `data`, `status`, and `error_message`
#'   reactive expressions.
#' @export
mod_data_preprocessing_server <- function(id) {
	shiny::moduleServer(id, function(input, output, session) {
    #Initialize reactive values
		data_value <- shiny::reactiveVal(NULL)
		status_value <- shiny::reactiveVal("empty")
		error_value <- shiny::reactiveVal(NULL)

		shiny::observeEvent(input$import, {
			#Ensure User has only selected either the zip file or the individual files, not both
			using_zip <- !is.null(input$zipfile) && nrow(input$zipfile) == 1L
			using_files <- !is.null(input$wellfile) && nrow(input$wellfile) == 1L &&
				!is.null(input$platefile) && nrow(input$platefile) == 1L
			individual_file_selected <- (!is.null(input$wellfile) && nrow(input$wellfile) > 0L) ||
				(!is.null(input$platefile) && nrow(input$platefile) > 0L)

			if (using_zip && individual_file_selected) {
				status_value("error")
				error_value("Choose either a ZIP file or the well and plate files, not both.")
				data_value(NULL)
				return()
			}
			if (!using_zip && !using_files) {
				status_value("error")
				error_value("Select exactly one well metadata file and one plate metadata file, or one ZIP file, before importing.")
				data_value(NULL)
				return()
			}

			status_value("loading")
			error_value(NULL)
			data_value(NULL)
			tryCatch({
				df <- if (using_zip) {
					load_from_zip(input$zipfile$datapath, separator = input$separator)
				} else {
					readScreen(
						wellInputFile = input$wellfile$datapath,
						plateInputFile = input$platefile$datapath,
						sep = input$separator
					)
				}
				if (!is.data.frame(df) || nrow(df) == 0L) {
					stop("The import returned no data.", call. = FALSE)
				}
				data_value(df)
				status_value("ready")
			}, error = function(e) {
				status_value("error")
				error_value(conditionMessage(e))
				data_value(NULL)
			})
		})

		output$status <- shiny::renderText(status_value())
		output$error_message <- shiny::renderText(error_value())
		output$data_table <- DT::renderDT(
			data_value(),
			options = list(pageLength = 5L, scrollX = TRUE)
		)

		list(
			data = shiny::reactive(data_value()),
			status = shiny::reactive(status_value()),
			error_message = shiny::reactive(error_value())
		)
	})
}
