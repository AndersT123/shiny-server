# Login page module


# UI generating function of login-page: Renders a login screen with three reactive inputs:
# 1) user_name, 2) passwd and 3) login. login is the id of actionButton

loginpage_ui <- function(id){
  ns <- NS(id)
  #reactive inputs: user_name, passwd, login
   div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
             wellPanel(
               tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
               textInput(ns("user_name"), placeholder="Username", label = tagList(icon("user"), "Username")),
               passwordInput(ns("passwd"), placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
               br(),
               div(
                 style = "text-align: center;",
                 actionButton(ns("login"), "SIGN IN", style = "color: white; background-color:#3c8dbc;
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
  
}


# The server function for loginpage module:
# Returns two reactive expressions as a list:
# 1) login: This is a logical used by the remainder of the app to choose which UI components to render
# 2) user: This is the reactive text input field provided by the user, i.e. user name. This is used to create the correct path for retrieving data by the app
loginpage <- function(input, output, session){
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({
    if ( USER$login == FALSE) {
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
  

  list("login" = reactive({USER$login}),
       "user" = reactive({input$user_name}))
}