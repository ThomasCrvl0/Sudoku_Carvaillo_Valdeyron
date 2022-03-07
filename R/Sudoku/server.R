library(shiny)

shinyServer(function(input, output) {
  output$affgrille <- renderPlot({
    l = input$diff
    d <- affgrille(l)
    if (input$sol) {
      
    }
    })
})
