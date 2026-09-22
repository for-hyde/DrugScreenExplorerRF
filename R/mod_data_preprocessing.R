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
				shiny::tags$div(
                   	style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px;",
                    shiny::tags$h3(shiny::tags$b("1 File Import"), style = "background-color: lightgray; padding: 10px;"),
					#UX decision to allow user to select upload method
					shiny::radioButtons(
						ns("import_method"),
						"Select import method",
						choices = c(
							"Import from ZIP file" = "zip",
							"Import from individual files" = "files"
						),
						selected = "files"
					),
					#Update based on radio button selection
					shiny::uiOutput(ns("dynamic_inputs")),
					shiny::uiOutput(ns("dynamic_separator")),
					shiny::actionButton(ns("import"), "Import data"),

					#Check boxes for advanced options
					shiny::checkboxInput(
						ns("advanced_options"),
						"Advanced Loading Options",
						value = FALSE
					),
					shiny::uiOutput(ns("advanced_options_ui")),
				),
				#Normalization
				shiny::tags$div(
					style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px;",
                    shiny::tags$h3(shiny::tags$b("2 Normalization"), style = "background-color: lightgray; padding: 10px;"),

					shiny::selectInput(
						ns("normalization_method"),
						"Normalization method",
						choices = c(
							"NPI" = "npi",
							"Negative control" = "negative"
						),
						selected = "none"
					),
					shiny::selectInput(
						ns("positive_wells"),
						"Positive control wells",
						choices = c(
							"None" = "none",
							"POS" = "POS"
						)
					),

				)
			),

			shiny::mainPanel(
				width = 8,
				#shiny::textOutput(ns("status")),
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

		# Dynamically render file input UI based on the selected import method
		output$dynamic_inputs <- shiny::renderUI({
			if (input$import_method == "zip") {
				shiny::fileInput(
					session$ns("zipfile"),
					"Select ZIP file",
					accept = ".zip"
				)
			} else {
				shiny::tagList(
					shiny::fileInput(
						session$ns("wellfile"),
						"Select well metadata file",
						accept = c(".csv", ".txt", ".tsv")
					),
					shiny::fileInput(
						session$ns("platefile"),
						"Select plate metadata file",
						accept = c(".csv", ".txt", ".tsv")
					)
				)
			}
		})

		# Render the separator input only when individual files are selected
		output$dynamic_separator <- shiny::renderUI({
			if (!is.null(input$wellfile) && nrow(input$wellfile) > 0L && !is.null(input$platefile) && nrow(input$platefile) > 0L) {
				shiny::textInput(
					session$ns("separator"),
					"Select file separator (for individual files)",
					value = ","
				)
			}
		})

		# Dynamically render advanced options UI based on the checkbox
		output$advanced_options_ui <- shiny::renderUI({
			if (input$advanced_options) {
				shiny::tagList(
					shiny::textInput(
						session$ns("row_range"),
						"Select row range for table in the raw data file(s) (e.g., 1:12)",
						value = ""
					),
					shiny::textInput(
						session$ns("col_range"),
						"Select column range for table in the raw data file(s) (e.g., A:Z) or numeric range (e.g., 1:24)",
						value = ""
					)
				)
			}
		})

		#When the import button is clicked, load the data based on the selected method
		shiny::observeEvent(input$import, {
			#If rows and columns are specified, parse them into numeric ranges
			row_selection <- if (isTRUE(input$advanced_options) && nzchar(input$row_range)) {
				tryCatch(eval(parse(text = input$row_range)), error = function(e) NULL)
			} else {
				NULL
			}
			col_selection <- if (isTRUE(input$advanced_options) && nzchar(input$col_range)) {
				tryCatch(eval(parse(text = input$col_range)), error = function(e) NULL)
			} else {
				NULL
			}	

			#If using .zip file, call load_from_zip, otherwise call readScreen
			


		})

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
