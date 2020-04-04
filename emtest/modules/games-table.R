# Module to generate table showing games with dates, scores and predicted scores.

games_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
     checkboxInput(ns("mobile"), "Using Mobile?"),
     actionButton(ns("save_button") , "Save"),
     rHandsontableOutput(ns("hot"))
  )
}


games_table <- function(input, output, session, user_name) {
  browser()    
  # Fetch specific user data from disk and join with league data
  data_list <- reactive(get_data(user_name = user_name()))
  
  # Create the output Handsontable to be displayed
  output$hot <- renderRHandsontable({
    # update the table at every 15 minutes to ensure that cells will be locked if session is started close to game time
    # Not the best implementation as it will cause unsaved cells to be deleted when table is refreshed
    invalidateLater(1000 * 60 * 15)
    
    # Add the input$save_button to the reactive expression, such that the expression will recompute when it changes
    # This will make the submit_time variable update in the table that the user is viewing
    input$save_button
    
    # The widget, i.e. the table displayed to the user is different depending on if the user has ticked the mobile checkbox
    # and therefore we need to handle them differently. To this end, format_mobile_widget() and format_pc_widget() is defined.
    if(input$mobile) {
      widget <- format_mobile_widget(data_list())
    } else {
      widget <- format_pc_widget(data_list())
    }
    widget
  })
  
  observeEvent(input$save_button,
               {
                 browser()
                 # The data to be saved is only the user predictions and the submit time of the predictions
                 
                 # input$hot can either be the mobile or pc version of the widgets. 
                 # They are converted to data.frames with hot_to_r
                 data <- hot_to_r(input$hot)
                 # Ensure that the data written to CSV file contains:
                 #   date_time, home_team, away_team, home_pred, away_pred, submit_time
                 #
                 # There are two cases:
                 # 1. User is on PC and the data object created by hot_to_r(input$hot) contains:
                 #   date_time, home_team, away_team, scores, home_pred, away_pred, submit_time
                 #
                 # 2. User is on mobile (input$mobile is TRUE) and the data object created by hot_to_r(input$hot) contains:
                 #   date_time, teams_abbrev, scores,
                 
                 if(input$mobile) {
                   # Do something such that mobile data are identical to pc data
                   # data_list() is a reactive wraped around get_data() so it returns whatever get_data() returns.
                   # The reactive will not change during the user session, since the only reactive component is the user log-in
                   # Data list returns data: the input data joined with league-games and read_only_rows, which is a logical
                   # vector determining which rows that are read only in the widget.
                   not_in_mobile <- data_list()$data %>% select(home_team, away_team, submit_time)
                   data <- bind_cols(data, not_in_mobile) %>% 
                     select(date_time, home_team, away_team, scores, home_pred, away_pred, submit_time)
                 }
                 
                 # input$date should be replaced with a updating variable or server upstart time
                 # submit_time should only be set if the pred_ variables are valid for being set during the session
                 # and they are non-NA
                 
                 # Convert the data$date_time and data$submit_time to date-time variables
                 # Add year to string such that data$date_time character can be converted to string
                 # warnings produced about formats failing to parse if NAs present
                 data <- data %>% mutate(
                   date_time = ydm_hm(paste0("2020-", date_time)),
                   submit_time = ydm_hm(paste0("2020-", submit_time)))
                 
                 
                 condition <- (now() - minutes(30)) > data$date_time
                 
                 
                  
                  data <- data %>% mutate(submit_time = if_else(!condition &
                                                                 !is.na(home_pred) | !is.na(away_pred),
                                                               now(),
                                                               submit_time)) %>%
                   select(date_time, home_team, away_team, home_pred, away_pred, submit_time)
                 
                 
                 write_csv(data, path = paste0(data_dir,"/", user_name(), "/games.csv"))
               })
}

# Read user specific data from disk and join with the league level data. Compute also which rows should be read only and
# pass them along with the actual data in a list.
get_data <- function(user_name) {
  #browser()
  #server_start <- server_start()
  
  # Load the latest saved user data
  test_path <- paste0(data_dir,"/", user_name, "/games.csv")
  data <- read_csv(paste0(data_dir,"/", user_name, "/games.csv"),
                   col_types = cols(
                     date_time = col_datetime(),
                     home_team = col_character(),
                     away_team = col_character(),
                     home_pred = col_integer(),
                     away_pred = col_integer(),
                     submit_time = col_datetime()),
                   # This will read in the date-times as the system local. The time saved on disk is confusingly saved as UTF
                   # time and will be behind our time zone, but this is remidied by locale = locale(tz = "")
                   locale = locale(tz = ""))
  # The latest saved league games data are loaded in global.R as league_table
  # Join user data with league games and format such that all relevant is presented in rhandsontable
  
  data <- left_join(league_games, data, by = c("date_time", 'home_team', 'away_team'))
  
  data <- data %>% mutate(scores = if_else(!is.na(home_score) & !is.na(away_score),
                                           paste0(home_score, "-", away_score),
                                           NA_character_)) %>%
    select(date_time, home_team, away_team, scores, home_pred, away_pred, submit_time)
  
  # Only games that are 30 mins away from starting should have editable prediction cells
  # Find a good solution for how to update this during the user is using the app
  # Conversion from character back to datetime to do the comparison with server_start
  #condition <- as_datetime(server_start) - minutes(30) > as_datetime(data$date_time)
  condition <- now() - minutes(30) > data$date_time
  read_only_rows <- which(condition)
  
  list(data = data, read_only_rows = read_only_rows)
}

format_mobile_widget <- function(data_list) {
  #browser()
  data <- data_list$data
  read_only_rows <- data_list$read_only_rows
  read_only_cols <- c("date_time", "teams", "scores")
  
  
  widget <- data %>% mutate(date_time = format(date_time, format = "%d-%b %H:%M"),
                            teams = make_team_abbrev(home_team, away_team)) %>%
    select(date_time, teams, scores, home_pred, away_pred) %>%
    rhandsontable(
                  rowHeaders = NULL) %>%
    hot_col(col = read_only_cols, readOnly = T) %>%
    hot_row(row = read_only_rows, readOnly = T)
  
  widget
}
format_pc_widget <- function(data_list) {
  data <- data_list$data
  read_only_rows <- data_list$read_only_rows
  read_only_cols <- c("date_time", "home_team", "away_team", "scores", "submit_time")

    # Convert date_time variable to characters because rhandsontable do not support POSIXt and format
  data <- data %>% mutate(date_time   = format(date_time, format = "%d-%b %H:%M"),
                          submit_time = format(submit_time, format = "%d-%b %H:%M"))
  widget <- rhandsontable(data,
                        rowHeaders = NULL) %>%
    hot_col(col = read_only_cols, readOnly = T) %>%
    hot_row(row = read_only_rows, readOnly = T)
  widget
}

make_team_abbrev <- function(home_team, away_team){
  lookup <- c(
    "Brighton" = "BRH",
    "Burnley"  = "BUR",
    "Chelsea"  = "CHE",
    "Crystal Palace" = "CRY",
    "Everton" = "EVE",
    "Leicester" = "LEI",
    "Liverpool" = "LIV",
    "Manchester City" = "MCI",
    "Manchester United" = "MUN",
    "Newcastle" = "NEW",
    "Norwich" = "NOR",
    "Sheffield United" = "SHU",
    "Southampton" = "SOU",
    "Tottenham" = "TOT",
    "West Ham" = "WHU",
    "Wolverhampton" = "WLV")
  home_abbrev <- lookup[home_team] 
  away_abbrev <- lookup[away_team]
  paste0(home_abbrev,"-",away_abbrev)
}
