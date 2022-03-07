library(shiny)

shinyServer(function(input, output) {
  output$affgrille <- renderPlot({affgrille()})
})
