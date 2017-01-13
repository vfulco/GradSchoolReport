library(shiny)
library(shinyFiles)
library(plyr)
library(rmarkdown)
library(GradSchoolReport)

shinyServer(function(input, output, session){

  shinyDirChoose(input, 'dir', roots = c(home = '~'), filetypes = c('', 'txt'))

  dir <- reactive(input$dir)

  # path
  path <- reactive({
    if(is.null(input$dir)){
      return()
    }else{
      home <- normalizePath("~")
      path <- file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
      templates <- system.file("Graduate_School_Report/templates",
                               package = "GradSchoolReport")
      l_ply(as.list(list.files(templates, full.names = TRUE)),
            file.copy, to = paste0(path, "/reports"), overwrite = TRUE)
      l_ply(as.list(list.files(templates, pattern = ".Rmd", full.names = TRUE)), render, params = list(path = path), quiet = TRUE)
      l_ply(as.list(list.files(templates, pattern = ".Rmd", full.names = TRUE)), file.remove)
      path
    }
  })

  # files
  output$files <- renderPrint({
    if(is.null(input$dir)){
      return()
    }else{
    list.files(path())
    }
    })

  output$dir <- renderPrint({
    if(is.null(input$dir)){
      return()
    }else{
      path()
    }


    output$inc<-renderUI({
      if(is.null(path())){
        return()
      }else{
        tabsetPanel(type = "tabs",
                    tabPanel("Graduate School Charts", includeHTML(paste0(path(), "/reports/GraduateSchoolCharts.html")))
        )
      }
    })

  })




})

