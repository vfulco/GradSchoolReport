#' Title
#'
#' @param dir
#'
#' @return
#' @export
#' @importFrom %>% magrittr
#' @importFrom llply plyr
#' @importFrom ldply plyr
#' @importFrom read_csv readr
#' @importFrom read_excel readxl
#' @importFrom str_subset stringr
#' @importFrom str_detect stringr
#' @importFrom rename dplyr
#' @importFrom select dplyr
#'
#'
#' @examples
read_clean <- function(dir){
  wd <- getwd()
  setwd(dir)
  files <- list.files()

  excel_files <- files %>%
    str_subset(".xls") %>%
    llply(read_excel)

  csv_files <- files %>%
    str_subset(".csv") %>%
    llply(read_csv)

  all_files <- c(excel_files, csv_files)
  browser()

  all_files %>% ldply(function(all_files){
    browser()
  degree_program_count <- all_files[str_detect(all_files$ID, "[:alpha:]"),] %>%
    select(ID, NAME, TERM) %>%
    rename(Degree = ID, Program = NAME, Count = TERM)

  apps <- all_files[!(str_detect(all_files$ID, "[:alpha:]")),]
  })
  setwd(wd)
}

