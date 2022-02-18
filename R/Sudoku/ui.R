library(shiny)

shinyUI(fluidPage(
  
  headerPanel("Sudoku"),
  
  mainPanel(
    uiOutput('matrix')
  )
))
