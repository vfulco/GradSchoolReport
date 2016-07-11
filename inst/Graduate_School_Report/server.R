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



  output$contents <- renderTable({
    inFile <- input$file
    if(is.null(inFile)){
      return(NULL)
    }else{
      inFile <- inFile[!(str_detect(inFile$name, ".docx")),]
      file.rename(inFile$datapath,
                  paste(inFile$datapath, ".xls", sep=""))
      files <- paste(inFile$datapath, ".xls", sep="") %>% read_clean
      files$crystal %>% head(20)
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
