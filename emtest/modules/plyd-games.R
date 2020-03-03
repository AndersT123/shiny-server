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
    df <- read_csv(file.path(data_dir, input$plyd_user, "games.csv"),
                   col_types = cols(
                     date_time = col_datetime(),
                     home_team = col_character(),
                     away_team = col_character(),
                     home_pred = col_integer(),
                     away_pred = col_integer()))
    # join with league games to get the actual game scores for the played games
    df  <- inner_join(df, league_games)
    
    
    df$points <- pmap_dbl(df, compute_game_score) %>% as.integer()
    
    df <- df %>% select(home_team, away_team, home_pred, away_pred, points)
  })
}