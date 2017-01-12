library(shiny)

shinyUI(fluidPage(
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
    tabsetPanel(type = "tabs",
      tabPanel("First Report", includeHTML("templates/Report.html")),
      tabPanel("Second Report", includeHTML("templates/Report.html")),
      tabPanel("Third Report", includeHTML("templates/Report.html")),
      tabPanel("Fourth Report", includeHTML("templates/Report.html")),
      tabPanel("Fifth Report", includeHTML("templates/Report.html"))
    )
  )
  )
))
