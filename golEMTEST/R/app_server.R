#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # List the first level callModules here
  login_mod <- callModule(mod_loginpage_server, "loginpage_ui_1")
  callModule(mod_games_table_server, "games_table_ui_1", reactive({login_mod$user()}))
  callModule(mod_plyd_games_server, "plyd_games_ui_1")
  callModule(mod_user_table_server, "user_table_ui_1")
  callModule(mod_user_standings_server, "user_standings_ui_1")
  
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
                column(6, 
                       box(width = NULL, games_table_ui("games_table_ui_1"))
                ),
                column(6, 
                       box(width = NULL, title = "Current Standings",
                           mod_user_table_ui("user_table_ui_1")),
                       box(width = NULL, 
                           mod_user_standings_ui("user_standings_ui_1")),
                       box(width = NULL, mod_plyd_games_ui("plyd_games_ui_1"))
                )
              )
              
      )
    }
    else { 
      mod_loginpage_ui("loginpage_ui_1")
    }
  })
}
