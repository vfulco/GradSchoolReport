#' Read and Clean Data
#'
#' @param dir charater string of directory where files are located
#'
#' @return
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom plyr llply
#' @importFrom readr read_csv
#' @importFrom readxl read_excel
#' @importFrom stringr str_subset
#'
#' @examples stuff
read_clean <- function(files){

  excel_files <- files %>%
    str_subset(".xls") %>%
    llply(read_excel)

  csv_files <- files %>%
    str_subset(".csv") %>%
    llply(read_csv)

  all_files <- c(excel_files, csv_files)

  all_files %<>% llply(removeTermSeperatorColumn1) %>%
    combineFiles

  rm(csv_files, excel_files, files)

if(nrow(all_files$goBaylor) > 0){
  all_files$goBaylor %<>% mutate(`Go Baylor Created` = !(is.na(`GO BAYLOR CREATED_ON`)),
                                    `App Created` = !(is.na(`APP CREATED_ON`)),
                                    `Submitted` = !(is.na(`SUBMITTED_ON`)))
}

if(nrow(all_files$crystal) > 0){
  all_files$crystal %<>%
    columnTransform(`Regular Admission Extended`, `LINE 2` == "A" &
                      `LINE 3` == "A") %>%
    columnTransform(`Probationary Admission Extended`, `LINE 2` == "P" &
                      `LINE 3` == "P") %>%
    columnTransform(`Rejected`, `LINE 2` == "D" &
                      `LINE 3` == "D") %>%
    columnTransform(`Cancelled`, `LINE 4` == "C") %>%
    columnTransform(`Regular Admission Accepted`, `LINE 2` == "A" &
                      `LINE 3` == "A" &
                      `LINE 4` %in% c("Y", "Ys", "Yd")) %>%
    columnTransform(`Regular Admission Declined`, `LINE 2` == "A" &
                      `LINE 3` == "A" &
                      `LINE 4` %in% c("N", "Ns", "Nd")) %>%
    columnTransform(`Probationary Admission Accepted`, `LINE 2` == "P" &
                      `LINE 3` == "P" &
                      `LINE 4` %in% c("Y", "Ys", "Yd")) %>%
    columnTransform(`Probationary Admission Declined`, `LINE 2` == "P" &
                      `LINE 3` == "P" &
                      `LINE 4` %in% c("N", "Ns", "Nd")) %>%
    columnTransform(`Cancelled No Offer`, `LINE 2` == "A" &
                      `LINE 4` == "C") %>%
    columnTransform(`Cancelled No Decline`, `LINE 2` == "D" &
                      `LINE 4` == "C") %>%
    columnTransform(`No Decision Entered`, is.na(`LINE 2`) &
                      is.na(`LINE 3`) &
                      is.na(`LINE 4`)) %>%
    seperateTerms %>%
    mutate(`Year Term` = paste(TERM, YEAR))}

  all_files
}






