# scrape games from bold.dr
scrape_games <- function(from_date, to_date){
  #browser()
  # from_date, to_date: dd-mm-yyyy as character
  bold_link <- bold_link <- read_html("https://www.bold.dk/fodbold/england/premier-league/")
  
  dates <- bold_link %>% html_nodes(".small a") %>% html_text()
  time <- bold_link %>% html_nodes(".time a") %>% html_text()
  teams  <- bold_link %>% html_nodes(".name a") %>% html_text()
  scores <- bold_link %>% html_nodes(".score a") %>% html_text()
  
  games <- tibble(dates = dates, time = time, teams = teams, scores = scores)
  
  games <- games %>% unite(col = date_time, dates, time, sep=" ", remove = T) %>% 
    mutate(date_time = dmy_hm(date_time)) %>% 
    filter(date_time >= dmy(from_date) & date_time <= dmy(to_date)) %>%
    separate(col = teams, into = c("home_team", "away_team"), sep = "[:space:]+[:punct:]+[:space:]", remove = T) %>%
    mutate(home_team = str_trim(home_team), away_team = str_trim(away_team)) %>%
    # Warnings are produced by games that have empty scores, i.e. games that have not been played yet. This is ok as they produce NAs
    # in home_score and away_score
    separate(col = scores, into = c("home_score", "away_score"), remove = T, convert = T)
}

# Compute scores
compute_game_score <- function(home_score, home_pred, away_score, away_pred){
  # Returns score, with is in 0-5
  # Correct winner/draw prediction gives 2 points
  # Correct score for a team gives 1 point, for each team
  # A bonus of 1 point is given if both scores are predicted corectly
  
  # If computed on missing scores then return NA
  if(is.na(home_score) | is.na(away_score) ) return(score <- NA_integer_)
  
  # home_pred or away_pred is NA --> score is zero
  if(is.na(home_pred) | is.na(away_pred)) return(score <- 0)
  #browser()
  # Establish correct winner/draw
  game_outcome <- case_when(
    # Home win
    home_score > away_score ~ 1L,
    # Draw
    home_score == away_score ~ 2L,
    # Away win
    home_score < away_score ~ 3L
  )
  game_pred <- case_when(
    # Home win
    home_pred > away_pred ~ 1L,
    # Draw
    home_pred == away_pred ~ 2L,
    # Away win
    home_pred < away_pred ~ 3L
  )
  
  # Score from game outcome
  game_point <- 0L
  if(game_outcome == game_pred) game_point <- 2L
  
  # Score from home team's score
  home_point <- 0L
  if(home_score == home_pred) home_point <- 1L
  
  # Score from away team's score
  away_point <- 0L
  if(away_score == away_pred) away_point <- 1L
  
  # Add bonus if both home and away teams' scores are correct
  bonus <- 0L
  if(home_score == home_pred & away_score == away_pred) bonus <- 1L
  score <- sum(game_point, home_point, away_point, bonus)
  # return game_total 
  score
}
