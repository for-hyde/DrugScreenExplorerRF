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
		shiny::fileInput(
			ns("wellfile"),
			"Well metadata file",
			accept = c(".csv", ".txt")
		),
		shiny::fileInput(
			ns("platefile"),
			"Plate metadata file",
			accept = c(".csv", ".txt")
		),
		shiny::actionButton(ns("import"), "Import data"),
		shiny::br(),
		shiny::textOutput(ns("status")),
		shiny::textOutput(ns("error_message")),
		shiny::tableOutput(ns("data_table"))
	)
}

#' Data-preprocessing module server.
#'
#' @param id Namespace identifier for the module.
#'
#' @return A named list containing `data`, `status`, and `error_message`
#'   reactive expressions.
#' @export
mod_data_preprocessing_server <- function(id) {
	shiny::moduleServer(id, function(input, output, session) {
		data_value <- shiny::reactiveVal(NULL)
		status_value <- shiny::reactiveVal("empty")
		error_value <- shiny::reactiveVal(NULL)

		shiny::observeEvent(input$import, {
			if (is.null(input$wellfile) || is.null(input$platefile) ||
					nrow(input$wellfile) != 1L || nrow(input$platefile) != 1L) {
				status_value("error")
				error_value("Select exactly one well metadata file and one plate metadata file before importing.")
				return()
			}

			status_value("loading")
			error_value(NULL)
			data_value(NULL)

			tryCatch({
				imported_data <- readScreen(
					wellInputFile = input$wellfile$datapath,
					plateInputFile = input$platefile$datapath
				)
				if (!is.data.frame(imported_data) || nrow(imported_data) == 0L) {
					stop("readScreen() returned no data.", call. = FALSE)
				}
				data_value(imported_data)
				status_value("ready")
			}, error = function(error) {
				status_value("error")
				error_value(conditionMessage(error))
			})
		})

		output$status <- shiny::renderText(status_value())
		output$error_message <- shiny::renderText({
			if (status_value() == "error") {
				error_value()
			} else {
				NULL
			}
		})
		output$data_table <- shiny::renderTable({
			data_value()
		}, striped = TRUE, bordered = TRUE, hover = TRUE)

		list(
			data = shiny::reactive(data_value()),
			status = shiny::reactive(status_value()),
			error_message = shiny::reactive(error_value())
		)
	})
}
