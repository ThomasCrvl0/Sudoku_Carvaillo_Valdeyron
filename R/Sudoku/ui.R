library(shiny)

shinyUI(fluidPage(
  
  headerPanel("Sudoku"),
  sliderInput("Diff", label = "Difficulté", 1,3, 1 ),
  mainPanel(
    plotOutput(outputId = 'affgrille'),
  )
))


