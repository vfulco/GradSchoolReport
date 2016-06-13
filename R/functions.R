#' Read and Clean Data
#'
#' @param dir
#'
#' @return
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom plyr llply
#' @importFrom plyr ldply
#' @importFrom plyr adply
#' @importFrom readr read_csv
#' @importFrom readxl read_excel
#' @importFrom stringr str_subset
#' @importFrom stringr str_detect
#' @importFrom stringr str_sub
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFram dplyr data_frame
#' @importFrom dplyr left_join
#'
#'
#' @examples stuff
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
  setwd(wd)


  all_files %<>% ldply(function(all_files){
    terms <- data_frame(TERM_Num = c("10", "20", "30", "21", "22",
                            "31", "32", "11", "12"),
               TERM = c("Spring", "Summer", "Fall",
                        "Summer Term 1", "Summer Term 2",
                        "Fall Term 1", "Fall Term 2",
                        "Spring Term 1", "Srping Term 2"))

  apps <- all_files[!(str_detect(all_files$ID, "[:alpha:]")),]
  apps$YEAR <- apps$TERM %>% str_sub(1, 4)
  apps$TERM_Num <- apps$TERM %>% str_sub(5, 6)

  apps %<>% left_join(terms, by = c("TERM_Num" = "TERM_Num")) %>%
    select(-TERM.x, -TERM_Num) %>% rename(TERM = TERM.y)
  })
  browser()
all_files$`Offer Extended` <- 0
all_files$`Offer Extended`[all_files$`LINE 2` %in% c("A", "P") &
  all_files$`LINE 3` %in% c("A", "P")] <- 1

all_files$`Admission Offer Extended` <- 0
all_files$`Admission Offer Extended`[all_files$`LINE 2` %in% c("A") &
                             all_files$`LINE 3` %in% c("A")] <- 1

all_files$`Probationary Offer Extended` <- 0
all_files$`Probationary Offer Extended`[all_files$`LINE 2` %in% c("P") &
                             all_files$`LINE 3` %in% c("P")] <- 1


all_files$`Accepted BU Offer` <- 0
all_files$`Accepted BU Offer`[all_files$`LINE 2` %in% c("A", "P") &
                                all_files$`LINE 3` %in% c("A", "P") &
                                all_files$`LINE 4` %in% c("Y", "Yd", "Ys")] <- 1

all_files$`Declined BU Offer` <- 0
all_files$`Declined BU Offer`[all_files$`LINE 2` %in% c("A", "P") &
                                all_files$`LINE 3` %in% c("A", "P") &
                                all_files$`LINE 4` %in% c("N", "Nd", "Ns")] <- 1

`No Decision for Applicant`
`Cancelled`
`No Decision Entered by Program`

}

