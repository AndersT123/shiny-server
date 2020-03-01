library(shiny)
library(DT)


ui <- fluidPage(
 DTOutput('tbl') 
)

server <- function(input, output, session) {
  output$tbl <- renderDT({
      
      
      datatable(iris, editable = "cell")
  })
}

shinyApp(ui, server)