library(shiny)
library(shinyFiles)

shinyUI(
  fluidPage(
  titlePanel("Graduate School Reports"),
  sidebarLayout(
  sidebarPanel(
    shinyDirButton("dir", "Chose directory", "Upload"),
    h4("output$dir"),
    verbatimTextOutput("dir"), br(),
    h4("Files in that dir"),
    verbatimTextOutput("files")
  ),
  mainPanel(
    htmlOutput("inc")
  )
  )
))
