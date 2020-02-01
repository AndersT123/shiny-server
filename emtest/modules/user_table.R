# Shiny module for rendering the user table, i.e. the points that the users have accumulated

user_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    tableOutput(ns("table"))
  )
}

user_table <- function(input, output, session){
  
  output$table <- renderTable({
    # league_table object is expected from global environment
    league_table})
}