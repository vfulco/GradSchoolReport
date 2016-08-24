#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plyr)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(magrittr)
library(rmarkdown)
library(knitr)
library(xtable)
library(ggplot2)
library(lazyeval)
library(GradSchoolReport)

# Define server logic required to draw a histogram
shinyServer(function(input, output){


  ################## Reactives #################################
  files <- reactive({
    if(is.null(input$file)){
      return(NULL)
    }else{
      inFile <- input$file
      inFile <- inFile[!(str_detect(inFile$name, ".docx")),]
      file.rename(inFile$datapath,
                  paste(inFile$datapath, ".xls", sep=""))
      files <- paste(inFile$datapath, ".xls", sep="") %>% read_clean
    }
  })

  gathered <- reactive({
    if(is.null(files()$crystal)){
      return(NULL)
    }else{
      files()$crystal %>%
        gather("Variable", "Value", 16:26) %>%
        select(DEGR, MAJOR, `Year Term`, ID, YEAR, Variable, Value) %>%
        distinct %>%
        group_by(YEAR, DEGR, MAJOR)
    }
  })


  ########################   Table  ####################################
  output$contents <- renderTable({
    if(is.null(input$file)){
      return(NULL)
    }else{
      files()$crystal %>% head(20)
    }
  })


  ########################   Text  ####################################
  output$applicationsText <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$file)){
      return(NULL)
    }else{
      tags$textarea(id = "applications",
                    rows = 17, cols = 45,
                    paste("Below is a figure of Total applications recieved by year from",
                          min(files()$crystal$YEAR),"to", max(files()$crystal$YEAR),
                          "Pay close attention to the ordinate scale since it will almost never start at 0 and scales according to the counts of applications for the respective years. Generally you want to see applications increase every year but expect some cyclical trends that might obscure the true trend. Cyclical trends don't become aparent till we see several cycles.")
      )
    }
  })

  output$acceptedDeclinedText <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$file)){
      return(NULL)
    }else{
      tags$textarea(id = "acceptedDeclined",
                    rows = 17, cols = 45,
                    paste("Below is a figure of accepted and declined offers by year from",
                          min(files()$crystal$YEAR), "to", max(files()$crystal$YEAR),
                          "Pay close attention to the ordinate scale since it might start at 0 and end at a relatively high number which can hide serious trends. As your offers extended increase don't be surprised to see any or all of these values increase.  We might expect there to be cyclical trends in any of these groups but typically cyclical trends aren't visible till several cycles.")
      )
    }
  })

  output$offerRejectionCancelledText <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$file)){
      return(NULL)
    }else{
      tags$textarea(id = "offerRejectionCancelled",
                    rows = 17, cols = 45,
                    paste("Below is a figure of offers, rejections and cancelled applications by year from",
                          min(files()$crystal$YEAR), "to", max(files()$crystal$YEAR),
                          "Pay close attention to the ordinate scale since it might start at 0 and end at a relatively high number which can hide serious trends. As your total applications increase don't be surprised to see any or all of these values increase. With large decreases in total applications we might expect that rejections to be affected the most. If we were to test if a large increase in rejections was significant or not we would test the proportions and not the raw values of rejections. We might expect there to be cyclical trends in any of these groups but typically cyclical trends aren't visible till several cycles."))
    }
  })



  ################################   Plots   ########################################
  output$plotApplications <- renderPlot({
    gathered() %>%
      group_by(YEAR) %>%
      select(DEGR, MAJOR, ID, YEAR) %>%
      unique %>%
      summarise(`Total Applications` = length(ID)) %>%
      ggplot() +
      geom_line(aes(x = YEAR, y = `Total Applications`, group = 1)) +
      theme_bw() +
      labs(title = "Applications") +
      theme(legend.position = "bottom")
  })

  output$plotOfferRejectionCancelled <- renderPlot({
    gathered() %>%
      filter(Variable %in% c("Regular Admission Extended",
                             "Probationary Admission Extended",
                             "Rejected",
                             "Cancelled",
                             "Cancelled No Offer",
                             "Cancelled No Decline")) %>%
      group_by(Variable, YEAR) %>%
      summarise(`Total Applications` = sum(Value, na.rm = TRUE)) %>%
      ggplot() +
      geom_line(aes(x = YEAR, y = `Total Applications`, color = Variable, group = Variable)) +
      theme_bw() +
      labs(title = "Offers, Rejections and Cancelled Applications") +
      theme(legend.position = "bottom")
  })

  output$plotAcceptedDeclined <- renderPlot({
    gathered() %>%
      filter(Variable %in% c("Regular Admission Accepted",
                             "Regular Admission Declined",
                             "Probationary Admission Accepted",
                             "Probationary Admission Declined")) %>%
      group_by(Variable, YEAR) %>%
      summarise(`Total Applications` = sum(Value, na.rm = TRUE)) %>%
      ggplot() +
      geom_line(aes(x = YEAR, y = `Total Applications`, color = Variable, group = Variable)) +
      theme_bw() +
      labs(title = "Accepted and Declined Offers") +
      theme(legend.position = "bottom")
  })



  #################################   Download   ###################################################
  output$downloadReport <- downloadHandler(
    filename = 'Reports.zip',
    content =  function(file){
      files <- gathered() %>% dlply(.(MAJOR, DEGR),function(program){
        render('report.Rmd',
               pdf_document(),
               output_file = paste0(unique(program$MAJOR), "_", unique(program$DEGR), ".pdf"),
               params = list(degree = unique(program$DEGR),
                             major = unique(program$MAJOR)))
      }) %>% unlist %>% basename
      zip(file, files)
      file
    })


})

