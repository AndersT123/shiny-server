#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  output$ops1 <- renderPrint({
    PATH_TO_DATA <- golem::get_golem_options("path")
    df <- readr::read_csv(PATH_TO_DATA)
    df
  })

}
