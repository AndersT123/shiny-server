# module for making played games appear from a selected user appear as table

plyd_games_output <- function(id){
  ns <- NS(id)
  
  user_names <- credentials %>% pull("user_name")
  tagList(
    selectInput(ns("plyd_user"), label = "Select user", choices = user_names),
    tableOutput(ns("plyd_table"))
  )
}

plyd_games <- function(input, output, session){
  
  output$plyd_table <- renderTable({
    df <- read_csv(file.path(data_dir, input$plyd_user))
  })
}