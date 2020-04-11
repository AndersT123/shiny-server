#' user_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_user_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    tableOutput(ns("table"))
  )
}
    
#' user_table Server Function
#'
#' @noRd 
mod_user_table_server <- function(input, output, session){
  ns <- session$ns
  output$table <- renderTable({
    # league_table object is expected from global environment
    league_table})
}
    
## To be copied in the UI
# mod_user_table_ui("user_table_ui_1")
    
## To be copied in the server
# callModule(mod_user_table_server, "user_table_ui_1")
 
