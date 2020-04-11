#' scrape games from bold.dr
#' @noRd
scrape_games <- function(from_date, to_date){
  #browser()
  # from_date, to_date: dd-mm-yyyy as character
  bold_link <- bold_link <- xml2::read_html("https://www.bold.dk/fodbold/england/premier-league/")
  
  dates <- bold_link %>% rvest::html_nodes(".small a") %>% rvest::html_text()
  time <- bold_link %>% rvest::html_nodes(".time a") %>% rvest::html_text()
  teams  <- bold_link %>% rvest::html_nodes(".name a") %>% rvest::html_text()
  scores <- bold_link %>% rvest::html_nodes(".score a") %>% rvest::html_text()
  
  games <- tibble::tibble(dates = dates, time = time, teams = teams, scores = scores)
  
  games <- games %>% tidyr::unite(col = date_time, dates, time, sep=" ", remove = T) %>% 
    mutate(date_time = lubridate::dmy_hm(date_time)) %>% 
    filter(date_time >= lubridate::dmy(from_date) & date_time <= lubridate::dmy(to_date)) %>%
    tidyr::separate(col = teams, into = c("home_team", "away_team"), sep = "[:space:]+[:punct:]+[:space:]", remove = T) %>%
    dplyr::mutate(home_team = stringr::str_trim(home_team), away_team = stringr::str_trim(away_team)) %>%
    # Warnings are produced by games that have empty scores, i.e. games that have not been played yet. This is ok as they produce NAs
    # in home_score and away_score
    tidyr::separate(col = scores, into = c("home_score", "away_score"), remove = T, convert = T)
}

#' Compute scores
#' @noRd
compute_game_score <- function(home_score, home_pred, away_score, away_pred, ...){
  # Returns score, with is in 0-5
  # Correct winner/draw prediction gives 2 points
  # Correct score for a team gives 1 point, for each team
  # A bonus of 1 point is given if both scores are predicted corectly
  
  # If computed on missing scores then return NA
  if(is.na(home_score) | is.na(away_score) ) return(score <- NA_integer_)
  
  # home_pred or away_pred is NA --> score is zero
  if(is.na(home_pred) | is.na(away_pred)) return(score <- 0L)
  #browser()
  # Establish correct winner/draw
  game_outcome <- dplyr::case_when(
    # Home win
    home_score > away_score ~ 1L,
    # Draw
    home_score == away_score ~ 2L,
    # Away win
    home_score < away_score ~ 3L
  )
  game_pred <- dplyr::case_when(
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
