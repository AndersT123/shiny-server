#source("./helper.R")
data_dir <- "data/test"

# Usernames are taken from the meta-data file
user_names <- read_csv(paste0(data_dir,"/meta.csv"),  col_names = T) %>% pull("user_name")


## Calculate the current league standings. This is placed in global.R because it is not changing during the time that user are 
## interacting with the app

## 1. Update games to predict on and get newest results by scraping from bold.dk
league_games <- scrape_games("27-01-2020", "01-03-2020")

# 2. Load user data into R
# Extract the directories that correspond to user data. This means exlcuding the data directory it self
user_dirs <- list.dirs(data_dir)[list.dirs(data_dir) != data_dir]
# add filename to map over
user_dirs <- paste0(user_dirs, "/games.csv")

user_games <- map(user_dirs, read_csv, col_types = cols(
  date_time = col_datetime(),
  home_team = col_character(),
  away_team = col_character(),
  home_pred = col_integer(),
  away_pred = col_integer(),
  submit_time = col_datetime()))

# 3. join data
scores_and_preds <- map(user_games, function(x) inner_join(x, league_games,
                                                           by = c("date_time", "home_team", "away_team")) %>%
                          select(home_score, home_pred, away_score, away_pred))

# 4. Calculate user points. 
user_points <- map(scores_and_preds, function(x) pmap_dbl(x, compute_game_score))

user_points_sum <- map(user_points, sum, na.rm = T) %>% unlist() %>% as.integer()

league_table <- tibble("User Name" = user_names, "Points" = user_points_sum) %>% arrange(desc(Points))
