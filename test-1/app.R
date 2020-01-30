#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           textOutput("wd"),
           tableOutput("games")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$wd <- renderPrint({
        out <- getwd()
        out
    })
    output$games <- renderTable({
        league_games
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
