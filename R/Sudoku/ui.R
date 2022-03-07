library(shiny)

shinyUI(fluidPage(
  
  headerPanel("Sudoku"),
  mainPanel(
    plotOutput(outputId = 'affgrille'),
  )
))


