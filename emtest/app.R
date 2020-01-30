#browser()
#print(getwd())
#browser()
library(rvest)
library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)
library(tidyverse)
library(rhandsontable)
library(lubridate)
source("./helper.R")
source("./global.R")
# Main login screen



loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("user_name", placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                  padding: 10px 15px; width: 150px; cursor: pointer;
                                  font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600; 
                                  padding-top: 5px;font-size:16px;", 
                                  class = "text-center")))
                     ))
                     )

# Encrypted passwords by make-password.R file
credentials <- read_csv("./data/test/meta.csv",
                        col_types = cols(
                          user_name = col_character(),
                          user_pwpass  = col_character()
                        ))

header <- dashboardHeader( title = "Simple Dashboard", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui<-dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({
    #browser()
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$user_name)
          Password <- isolate(input$passwd)
          if(length(which(credentials$user_name==Username))==1) {
            # Extract the encrypted password as character
            pasmatch  <- credentials["user_pwpass"][which(credentials$user_name==Username),] %>% pull()
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })

  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
            font-weight: bold; margin:5px; padding: 10px;")
  })
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      sidebarMenu(
        menuItem("Main Page", tabName = "dashboard", icon = icon("dashboard"))
      )
    }
  })
  
  observeEvent(input$debug,{
    browser()
  })
  
  # server_start <- eventReactive(input$update_time,
  #                             {
  #                               server_start <- paste0(input$date, "-", input$hours, "-", input$minutes) %>% ymd_hm()
  #                               server_start
  #                             }, 
  #                             # Setting ignoreNULL = FALSE will make the table render immidiately at startup, because otherwise the 
  #                             # server_start object is not defined and the renderRHandsontABLE function depends on this, this is why
  #                             # update server start needed to be pressed before it would render the table
  #                             ignoreNULL = FALSE)
  server_start <- function() now()
  
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      tabItem(tabName ="dashboard", class = "active",
              # fluidRow(
              #   box(width = 12,
              #       column(2, dateInput("date", label = "Select date to test", value = "2019-01-05")),
              #       column(1, numericInput("hours", label = "Hours", min = 0, max = 23, step = 1, value = 16)),
              #       column(1, numericInput("minutes", label = "Minutes", min = 0, max = 59, step = 1, value = 0)),
              #       column(1, actionButton("update_time", label = "Update Server start")),
              #       column(1, actionButton("debug", label = "Debug")))
              # ),
              fluidRow(
                box(width = 6, 
                    actionButton("save_button", "Save"),
                    rHandsontableOutput("hot")),
                box(width = 6, tableOutput("league_table"))
              )
              
              )
    }
    else {
      loginpage
    }
  })


  output$hot <- renderRHandsontable({
    # update the table at every 15 minutes to ensure that cells will be locked if session is started close to game time
    # Not the best implementation as it will cause unsaved cells to be deleted when table is refreshed
    invalidateLater(1000 * 60 * 15)
    
    
    # Add the input$save_button to the reactive expression, such that the expression will recompute when it changes
    # This will make the submit_time variable update in the table that the user is viewing
    input$save_button

    #test_at()
    server_start <- server_start()

    # Load the latest saved user data
    test_path <- paste0(data_dir,"/", input$user_name, "/games.csv")
    data <- read_csv(paste0(data_dir,"/", input$user_name, "/games.csv"),
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

    # coerce date_time variable to characters because rhandsontable do not support POSIXt
    data <- rhandsontable(data %>% mutate(date_time   = as.character(date_time),
                                          submit_time = as.character(submit_time))) %>%
      hot_col(col = read_only_cols, readOnly = T) %>%
      hot_row(row = read_only_rows, readOnly = T) %>%
      hot_table(highlightCol = T, highlightRow = T)

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
                 
                 write_csv(data, path = paste0(data_dir,"/", input$user_name, "/games.csv"))
               })
  
  output$league_table <- renderTable({
    league_table
  })
}

#runApp(list(ui = ui, server = server), launch.browser = TRUE)