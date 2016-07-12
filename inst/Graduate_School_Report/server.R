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
library(GradSchoolReport)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  files <- reactive({
    inFile <- input$file
    inFile <- inFile[!(str_detect(inFile$name, ".docx")),]
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xls", sep=""))
    files <- paste(inFile$datapath, ".xls", sep="") %>% read_clean
  })

  output$contents <- renderTable({
    if(is.null(input$file)){
      return(NULL)
    }else{
      files()$crystal %>% head(20)
    }
  })

  output$choose_degree <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$file)){
      return(NULL)
    }else{
      colnames <- files()$crystal$DEGR %>% unique
      names(colnames) <- colnames
      checkboxGroupInput("degrees", "Choose Degrees",
                         choices  = colnames,
                         selected = NULL)
    }
  })

  output$choose_major <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$file)){
      return(NULL)
    }else{
      colnames <- files()$crystal$MAJOR %>% unique
      names(colnames) <- colnames
      checkboxGroupInput("majors", "Choose Majors",
                         choices  = colnames,
                         selected = NULL)
    }
  })

  output$filtered <- renderTable({
    if(is.null(input$file)){
      return(NULL)
    }else{
      files()$crystal %>%
        filter(DEGR %in% input$degrees) %>%
        filter(MAJOR %in% input$majors) %>%
        head(20)
    }
  })

  output$downloadReport <- downloadHandler(
    filename = function(){
      report_name <- "report"
      paste(report_name, ".pdf", sep = "")
      },
    content =  function(file){
      docs <- c('report.Rmd',
                   'applications.Rmd',
                   'offerRejectionCancelled.Rmd',
                   'acceptedDeclined.Rmd',
                   'whereTheyAreGoing.Rmd')
      src <- docs %>%
        ldply(normalizePath)
      src_applications <- normalizePath('applications.Rmd')
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      cbind(src = src$V1, docs = docs) %>%
        m_ply(function(src, docs){
          file.copy(src, docs)
        })
      out <- render('report.Rmd', pdf_document())
      file.rename(out, file)
    }
  )

})
