#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Graduate School Report"),
    tabsetPanel(type = "tabs",
                tabPanel("File Upload and Report Build",
                         sidebarLayout(
                           sidebarPanel(
                             fileInput("file", label = h3("File input"), multiple = TRUE),
                             hr("Header"),
                             fluidRow(column(4, verbatimTextOutput("value"))), label = "Head")
                           ,
                           mainPanel(
                             downloadButton('downloadReport')
                           )
                           )
                  ),
                tabPanel("Summary", verbatimTextOutput("summary")),
                tabPanel("Table", tableOutput("table"))
    )




))
