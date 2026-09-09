buildApp <- function(){
  #Define UI
  ui <- shiny::navbarPage(

    title = "DrugScreenExplorer",
    inverse = TRUE,
    id = "tabs",


  )

  #Define server
  server <- function(input, output, session){
    mod_data_preprocessing("preprocessing_1")
  }


  shiny::shinyApp(ui = ui, server = server)
}
