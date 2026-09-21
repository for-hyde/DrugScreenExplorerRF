buildApp <- function(){
  #Define UI
  ui <- shiny::navbarPage(

    title = "DrugScreenExplorer",
    inverse = TRUE,
    id = "tabs",

    shiny::tabPanel(
      "Data preprocessing",
      mod_data_preprocessing_ui("preprocessing_1")
    )
  )

  #Define server
  server <- function(input, output, session){
    mod_data_preprocessing_server("preprocessing_1")
  }


  shiny::shinyApp(ui = ui, server = server)
}
