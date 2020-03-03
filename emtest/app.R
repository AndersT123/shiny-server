library(rvest)
library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)
library(tidyverse)
library(rhandsontable)
library(lubridate)
source("helper.R")
source("global.R")
source("modules/user-table.R")
source("modules/games-table.R")
source("modules/login-page.R")
source("modules/plyd-games.R")
#source("modules/test.R")
header <- dashboardHeader( title = "Simple Dashboard", uiOutput("logoutbtn"))
sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))

ui<-dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  observeEvent(input$debug,{
    browser()
  })
  
  login_mod <- callModule(loginpage, "login")
  callModule(user_table, "user_table")
  callModule(games_table, "games_table", reactive({login_mod$user()}) )#, server_start = now())
  callModule(plyd_games, "plyd_games")

  output$logoutbtn <- renderUI({
    req(login_mod$login())
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
            font-weight: bold; margin:5px; padding: 10px;")
  })
  
  output$sidebarpanel <- renderUI({
    if (login_mod$login() == TRUE){ 
      sidebarMenu(
        menuItem("Main Page", tabName = "dashboard", icon = icon("dashboard"))
      )
    }
  })

  output$body <- renderUI({
    if (login_mod$login() == TRUE) {
      tabItem(tabName ="dashboard", class = "active",
              fluidRow(
                column(6, tags$b("Current standings"), 
                       box(width = NULL, 
                    games_table_ui("games_table"))),
                column(6, 
                       box(width = NULL, user_table_ui("user_table")),
                       box(width = NULL, plyd_games_output("plyd_games"))
                       )
              )
              
      )
    }
    else { 
      loginpage_ui("login")
    }
  })

}

shinyApp(ui = ui, server = server)