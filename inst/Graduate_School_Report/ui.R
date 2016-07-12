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
              tabPanel("File Upload",
                       sidebarLayout(
                         sidebarPanel(
                           h4("Applicant offered admission and accepts offer"),
                           p("A – line 2:  Department recommends admission"),
                           p("A – line 3:  Graduate School agrees and extends offer of admission"),
                           p("Y, Ys, Yd – line 4:  Applicant accepts offer"),

                           h4("Applicant offered PROBATIONARY admission and accepts offer"),
                           p("P – line 2:  Departmental recommends probationary admission"),
                           p("P – line 3:  Graduate School agrees and extends probationary offer of admission"),
                           p("Y, Ys, Yd – line 4:  Applicant accepts offer"),

                           h4("Applicant offered admission and declines offer"),
                           p("A – line 2:  Department recommends admission"),
                           p("A – line 3:  Graduate School agrees and extends offer of admission"),
                           p("N, Ns, Nd – line 4:  Applicant declined offer"),

                           h4("Applicant offered PROBATIONARY admission and accepts offer"),
                           p("P – line 2:  Departmental recommends probationary admission"),
                           p("P – line 3:  Graduate School agrees and extends probationary offer of admission"),
                           p("N, Ns, Nd – line 4:  Applicant declined offer"),

                           h4("Applicant file is rejected"),
                           p("D – line 2:  Department recommends decline (reject)"),
                           p("D – line 3:  Graduate School agrees and declines (reject) file"),

                           h4("File Cancelled"),
                           p("C – line 4"),
                           h4("File Cancelled (no offer extended)"),
                           p("A – line 2"),
                           p("C – line 4"),
                           h4("File Cancelled (no decline sent)"),
                           p("D – line 2"),
                           p("C – line 4"),
                           h4("No Decision Entered"),
                           p("No codes on line 2, 3, or 4"),
                           width = 4),
                         mainPanel(
                           fileInput("file", label = h3("File input"), multiple = TRUE),
                           h3("Header"),
                           tableOutput('contents')
                         )
                       )
              ),
              tabPanel("Filters",
                       sidebarLayout(
                         sidebarPanel(uiOutput("choose_degree"),
                                      uiOutput("choose_major")),
                         mainPanel(h3("Header"),
                                   tableOutput('filtered'))
                       )),
              tabPanel("Download Report", downloadButton('downloadReport'))
  )




))
