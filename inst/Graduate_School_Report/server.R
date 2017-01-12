library(shiny)
library(shinyFiles)
library(rmarkdown)
library(GradSchoolReport)

shinyServer(function(input, output, session){

  shinyDirChoose(input, 'dir', roots = c(home = '~'), filetypes = c('', 'txt'))

  dir <- reactive(input$dir)


  # path
  path <- reactive({
    home <- normalizePath("~")
    file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
  })

  # files
  output$files <- renderPrint(list.files(path()))





})

