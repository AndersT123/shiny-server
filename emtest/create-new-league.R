# 1. 
#
#
library(readr)
library(purrr)  
library(sodium)

create_league <- function(league_name = "new-league", 
                          user_data, 
                          csv_init = c("date_time", "home_team", "away_team", "home_pred", "away_pred", "submit_time")){
  #browser()
  #user_data: user_name, user_pw
  user_data <- read_csv(user_data,
                        col_types = cols(
                          user_name = col_character(),
                          user_pw   = col_character()
                        ))
  # create directory at league level
  dir.create(league_name)
  # write meta file to store user names and encrypted passwords
  meta <- data.frame(user_name   = user_data$user_name,
                     user_pwpass = map_chr(user_data$user_pw, password_store))
  write_csv(meta, path = file.path(league_name, "meta.csv"))
  
  # create directories at user level
  walk(user_data$user_name, function(x) dir.create(file.path(league_name,x))
       )
  # create games.csv in every user folder
  df <- setNames(data.frame(matrix(ncol = length(csv_init), nrow = 0)), csv_init)
  
  walk(user_data$user_name, function(x) write_csv(df, path = file.path(league_name, x, "games.csv")))
  
  
}

create_league("data/test", "test.csv")
