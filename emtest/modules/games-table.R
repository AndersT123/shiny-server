# Module to generate table showing games with dates, scores and predicted scores.

games_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
     actionButton(ns("save_button") , "Save"),
     rHandsontableOutput(ns("hot"))
  )
}


games_table <- function(input, output, session, user_name, server_start) {

  output$hot <- renderRHandsontable({
    # update the table at every 15 minutes to ensure that cells will be locked if session is started close to game time
    # Not the best implementation as it will cause unsaved cells to be deleted when table is refreshed
    invalidateLater(1000 * 60 * 15)
    
    # Add the input$save_button to the reactive expression, such that the expression will recompute when it changes
    # This will make the submit_time variable update in the table that the user is viewing
    input$save_button
    
    # do the data manipulation here
    data <- get_data(user_name = user_name() )
    
    #server_start <- server_start()
    data
  })
  
  observeEvent(input$save_button,
               {
                 #browser()
                 server_start <- server_start()
                 # The data to be saved is only the user games table, so the predictions and the submit time of the predictions
                 
                 data <- hot_to_r(input$hot)
                 
                 # input$date should be replaced with a updating variable or server upstart time
                 # submit_time should only be set if the pred_ variables are valid for being set during the session
                 # and they are non-NA
                 
                 condition <- server_start - minutes(30) > data$date_time
                 #condition <- as_datetime(server_start) - minutes(30) > as_datetime(data$date_time)
                 
                 data <- data %>% mutate(submit_time = if_else(!condition &
                                                                 !is.na(home_pred) | !is.na(away_pred),
                                                               as.character(server_start),
                                                               submit_time)) %>%
                   select(date_time, home_team, away_team, home_pred, away_pred, submit_time)
                 
                 write_csv(data, path = paste0(data_dir,"/", user_name(), "/games.csv"))
               })
}

## Function used within the games_table to extract the correct data given username and do some data manipulations
get_data <- function(user_name) {
  #browser()
  server_start <- server_start()
  
  # Load the latest saved user data
  test_path <- paste0(data_dir,"/", user_name, "/games.csv")
  data <- read_csv(paste0(data_dir,"/", user_name, "/games.csv"),
                   col_types = cols(
                     date_time = col_datetime(),
                     home_team = col_character(),
                     away_team = col_character(),
                     home_pred = col_integer(),
                     away_pred = col_integer(),
                     submit_time = col_datetime()))
  # The latest saved league games data are loaded in global.R as league_table
  # Join user data with league games and format such that all relevant is presented in rhandsontable
  
  data <- left_join(league_games, data, by = c("date_time", 'home_team', 'away_team'))
  
  data <- data %>% mutate(scores = if_else(!is.na(home_score) & !is.na(away_score),
                                           paste0(home_score, "-", away_score),
                                           NA_character_)) %>%
    select(date_time, home_team, away_team, scores, home_pred, away_pred, submit_time)
  
  
  read_only_cols <- c("date_time", "home_team", "away_team", "scores", "submit_time")
  # Only games that are 30 mins away from starting should have editable prediction cells
  # Find a good solution for how to update this during the user is using the app
  # Conversion from character back to datetime to do the comparison with server_start
  #condition <- as_datetime(server_start) - minutes(30) > as_datetime(data$date_time)
  condition <- server_start - minutes(30) > data$date_time
  read_only_rows <- which(condition)
  #browser()
  # coerce date_time variable to characters because rhandsontable do not support POSIXt and format
  data <- data %>% mutate(date_time   = format(date_time, format = "%d-%b %H:%M"),
                          submit_time = format(submit_time, format = "%d-%b %H:%M"))
  
  data <- rhandsontable(data) %>%
    hot_col(col = read_only_cols, readOnly = T) %>%
    hot_row(row = read_only_rows, readOnly = T) %>%
    hot_table(highlightCol = T, highlightRow = T)
  data
}