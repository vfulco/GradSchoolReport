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
library(ggvis)
library(lazyeval)
library(GradSchoolReport)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

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
                         selected = colnames)
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
                         selected = colnames)
    }
  })

  output$applicationsText <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$file)){
      return(NULL)
    }else{
      tags$textarea(id = "applications",
                    rows = 20, cols = 45,
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
                    rows = 20, cols = 45,
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
                    rows = 20, cols = 45,
                    paste("Below is a figure of offers, rejections and cancelled applications by year from",
                          min(files()$crystal$YEAR), "to", max(files()$crystal$YEAR),
                          "Pay close attention to the ordinate scale since it might start at 0 and end at a relatively high number which can hide serious trends. As your total applications increase don't be surprised to see any or all of these values increase. With large decreases in total applications we might expect that rejections to be affected the most. If we were to test if a large increase in rejections was significant or not we would test the proportions, $rejections / total\text{ }applications$ and not the raw values of rejections. We might expect there to be cyclical trends in any of these groups but typically cyclical trends aren't visible till several cycles."))
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


  totalApps <- reactive({
    if(is.null(input$file) | is.null(input$degrees)){
      data.frame(x = 0, y = 0) %>% ggvis(~x, ~y)
    }else{
      files()$crystal %>%
      filter(DEGR %in% input$degrees) %>%
        filter(MAJOR %in% input$majors) %>%
        gather("Variable", "Value", 16:26) %>%
        select(DEGR, MAJOR, `Year Term`, ID, YEAR) %>%
        distinct %>%
        group_by(YEAR) %>%
        summarise(`Total Applications` = length(ID)) %>%
        ggvis(~YEAR, ~`Total Applications`) %>%
        layer_lines()
    }
  }) %>% bind_shiny('totalApps', 'totalApps_ui')


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
