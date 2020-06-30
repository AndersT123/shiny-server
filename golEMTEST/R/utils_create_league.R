#' Process for creating a new league
#' 
#' Expects a csv file with two entries user_name and user_pw where user_pw is the unencrypted passwords.
#' This csv file with user_name and user_pw should not stay with the rest of the source code with the app. It is only used
#' temporarily.

#' 1. Create folder structure
#' 2. Initialize the user specific csv files with dates, team names etc. 
#' 3. Creates a meta data file with usernames and encrypted passwords

#' create_league_r
#' @param league_name,file_name,add_user,csv_init hsha
#' @export
create_league_r <- function(league_name = file.path(golem::get_golem_wd(), "user-data", "new-league"), # path to where the league structure should begin
                            file_name, #path to the filename of the user_data holding usernames and unencrypted passwords 
                            add_user = F,
                            csv_init = c("date_time", "home_team", "away_team", "home_pred", "away_pred", "submit_time")){
  #browser()
  #user_data: user_name, user_pw
  user_data <- readr::read_csv(file_name,
                               col_types = readr::cols(
                                 user_name = readr::col_character(),
                                 user_pw   = readr::col_character()
                               ))
  if(!add_user){
    # create directory at league level, but only if add == F.
    if(dir.exists(league_name)) stop("Directory already exists choose league name that doesn't exist, or use add=T to add users to existing league.")
    
    dir.create(league_name, recursive = T)
  }
  
  # If add=TRUE, find the new users to add from opt$file and existing names on folders
  existing_users <- list.dirs(league_name, full.names = F, recursive = F)
  new_user_names <- user_data$user_name[!(user_data$user_name %in% existing_users)]
  new_user_pwpass <- user_data$user_pw[!(user_data$user_name %in% existing_users)] 
  
  # case where add=T. There should be appended rows to the meta.csv file
  
  meta <- tibble::tibble(user_name   = new_user_names,
                         user_pwpass = purrr::map_chr(new_user_pwpass, sodium::password_store))
  readr::write_csv(meta, path = file.path(league_name, "meta.csv"), append = add_user)  
  
  # write meta file to store user names and encrypted passwords
  # This is the file that that is stored in the app.
  # meta <- data.frame(user_name   = user_data$user_name,
  #                    user_pwpass = map_chr(user_data$user_pw, password_store))
  # write_csv(meta, path = file.path(PATH_TO_LEAGUE, "meta.csv"))
  
  
  # create directories at user level
  #walk(user_data$user_name, function(x) dir.create(file.path(PATH_TO_LEAGUE,x)))
  purrr::walk(new_user_names, function(x) dir.create(file.path(league_name,x)))
  # create games.csv in every user folder
  df <- setNames(tibble::as_tibble(matrix(ncol = length(csv_init), nrow = 0), .name_repair = "unique"), csv_init)
  purrr::walk(new_user_names, function(x) readr::write_csv(df, path = file.path(league_name, x, "games.csv")))
  
}

#' create_league_cmd
# 
# create_league_cmd <- function(league_name,
#                               file_name,
#                               add_user,
#                               csv_init) {
#   
# }

#' 
#' 
#' 
#' #' Setup for optparse:
#' #' ref.: https://www.r-bloggers.com/passing-arguments-to-an-r-script-from-command-lines/
#' option_list <- list(
#'   optparse::make_option(c("-f", "--file"), type = "character", default = NULL, 
#'               help = "file with usernames and passwords", metavar = "character"),
#'   optparse::make_option(c("-o", "--out"), type = "character", default = "new-league",
#'               help = "name of directory in data/. [default=%default]", metavar = "character"),
#'   optparse::make_option(c("-a", "--add"), type = "logical", default = F,
#'               help = "Flag for adding user to existing league. [default=%default]", metavar = "logical")
#' )
#' 
#' opt_parser <- optparse::OptionParser(option_list = option_list)
#' opt <- optparse::parse_args(opt_parser)
#' if (is.null(opt$file)){
#'   optparse::print_help(opt_parser)
#'   stop("File containing the username and passwords must be supplied as .csv file with structure:
#'        \nuser_name,user_pw\n\n", call.=FALSE)
#' }
#' 
#' #' 
#' create_league_cmd <- function(league_name = opt$out, # path to where the league structure should begin
#'                           file_name = opt$file, #path to the filename of the user_data holding usernames and unencrypted passwords 
#'                           csv_init = c("date_time", "home_team", "away_team", "home_pred", "away_pred", "submit_time")){
#'   
#'   
#'   
#'   
#'   
#'   PATH_TO_LEAGUE <- file.path("data", league_name)
#'   print(PATH_TO_LEAGUE)
#'   #browser()
#'   #user_data: user_name, user_pw
#'   user_data <- read_csv(file_name,
#'                         col_types = cols(
#'                           user_name = col_character(),
#'                           user_pw   = col_character()
#'                         ))
#'   if(!opt$add){
#'     # create directory at league level, but only if add == F.
#'     if(dir.exists(file.path("data", opt$out))) stop("Directory already exists choose league name that doesn't exist, or use add=T to add users to existing league.")
#'     
#'     dir.create(PATH_TO_LEAGUE)
#'   }
#'   
#'   # If add=TRUE, find the new users to add from opt$file and existing names on folders
#'   existing_users <- list.dirs(PATH_TO_LEAGUE, full.names = F, recursive = F)
#'   new_user_names <- user_data$user_name[!(user_data$user_name %in% existing_users)]
#'   new_user_pwpass <- user_data$user_pw[!(user_data$user_name %in% existing_users)] 
#'   
#'   # case where add=T. There should be appended rows to the meta.csv file
#'   
#'   meta <- data.frame(user_name   = new_user_names,
#'                      user_pwpass = map_chr(new_user_pwpass, password_store))
#'   write_csv(meta, path = file.path(PATH_TO_LEAGUE, "meta.csv"), append = opt$add)  
#'   
#'   # write meta file to store user names and encrypted passwords
#'   # This is the file that that is stored in the app.
#'   # meta <- data.frame(user_name   = user_data$user_name,
#'   #                    user_pwpass = map_chr(user_data$user_pw, password_store))
#'   # write_csv(meta, path = file.path(PATH_TO_LEAGUE, "meta.csv"))
#'   
#'   
#'   # create directories at user level
#'   #walk(user_data$user_name, function(x) dir.create(file.path(PATH_TO_LEAGUE,x)))
#'   walk(new_user_names, function(x) dir.create(file.path(PATH_TO_LEAGUE,x)))
#'   # create games.csv in every user folder
#'   df <- setNames(data.frame(matrix(ncol = length(csv_init), nrow = 0)), csv_init)
#'   walk(new_user_names, function(x) write_csv(df, path = file.path(PATH_TO_LEAGUE, x, "games.csv")))
#'   
#' }
#' 
#' #create_league(opt$out, opt$file)
