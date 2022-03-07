library(shiny)

shinyUI(fluidPage(
  
  headerPanel("Sudoku"),
  sliderInput("diff","Difficulté", min = 1,  max = 3,  value = 1),
  checkboxInput(inputId = "sol", label = "Solution", value = FALSE),
  mainPanel(
    plotOutput(outputId = 'affgrille'),
  )
))


