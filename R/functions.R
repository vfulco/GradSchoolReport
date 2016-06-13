read_clean <- function(dir){
  wd <- getwd()
  setwd(dir)
  files <- list.files()
  excel_files <- plyr::llply(stringr::str_subset(files, ".xls"), readxl::read_excel)
  csv_files <- plyr::llply(stringr::str_subset(files, ".csv"), readr::read_csv)
  all_files <- c(excel_files, csv_files)
  browser()
  unique_df_names <- unique(plyr::llply(all_files, names))
  suppressWarnings(plyr::llply(unique_df_names, function(x, y){
    matches <- plyr::ldply(seq(length(y)), function(z, y, x){
      if(all(length(names(y[[z]]) == length(x))) && all(names(y[[z]]) == x)){1}else{0}
    }, y = y, x = x)
    dplyr::tbl_df(plyr::ldply(y[as.logical(matches$V1)]))
  }, y = all_files))
  setwd(wd)
}
