library(shiny)

shinyUI(fluidPage(
  
  headerPanel("Sudoku"),
  sliderInput("diff","Difficulté", min = 1,  max = 3,  value = 1),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Sudoku", plotOutput("aff1"), verbatimTextOutput("Text")),
      tabPanel("Solution", plotOutput("aff2")),
      tabPanel("Légende", verbatimTextOutput("legende"))
    )
  )
  
))


