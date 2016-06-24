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
#' @importFrom plyr ldply
#' @importFrom plyr adply
#' @importFrom readr read_csv
#' @importFrom readxl read_excel
#' @importFrom stringr str_subset
#' @importFrom stringr str_detect
#' @importFrom stringr str_sub
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr data_frame
#' @importFrom dplyr left_join
#'
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

all_files$`Accepted BU Admission Offer` <- 0
all_files$`Accepted BU Admission Offer`[all_files$`LINE 2` %in% c("A") &
                                all_files$`LINE 3` %in% c("A") &
                                all_files$`LINE 4` %in% c("Y", "Yd", "Ys")] <- 1

all_files$`Accepted BU Probationary Offer` <- 0
all_files$`Accepted BU Probationary Offer`[all_files$`LINE 2` %in% c("P") &
                                all_files$`LINE 3` %in% c("P") &
                                all_files$`LINE 4` %in% c("Y", "Yd", "Ys")] <- 1

all_files$`Declined BU Offer` <- 0
all_files$`Declined BU Offer`[all_files$`LINE 2` %in% c("A", "P") &
                                all_files$`LINE 3` %in% c("A", "P") &
                                all_files$`LINE 4` %in% c("N", "Nd", "Ns")] <- 1

all_files$`Declined BU Admission Offer` <- 0
all_files$`Declined BU Admission Offer`[all_files$`LINE 2` %in% c("A") &
                                all_files$`LINE 3` %in% c("A") &
                                all_files$`LINE 4` %in% c("N", "Nd", "Ns")] <- 1

all_files$`Declined BU Probationary Offer` <- 0
all_files$`Declined BU Probationary Offer`[all_files$`LINE 2` %in% c("P") &
                                all_files$`LINE 3` %in% c("P") &
                                all_files$`LINE 4` %in% c("N", "Nd", "Ns")] <- 1

all_files$`Applicant file is rejected` <- 0
all_files$`Applicant file is rejected`[all_files$`LINE 2` %in% c("D") &
                                         all_files$`LINE 3` %in% c("D")] <- 1

all_files$`File Cancelled` <- 0
all_files$`File Cancelled`[all_files$`LINE 4` %in% c("C")] <- 1

all_files$`File Cancelled (no offer extended)` <- 0
all_files$`File Cancelled (no offer extended)`[all_files$`LINE 2` %in% c("A") &
                                                 all_files$`LINE 4` %in% c("C")] <- 1

all_files$`File Cancelled (no decline sent)` <- 0
all_files$`File Cancelled (no decline sent)`[all_files$`LINE 2` %in% c("D") &
                                               all_files$`LINE 4` %in% c("C")] <- 1

all_files$`No Decision Entered` <- 0
all_files$`No Decision Entered`[is.na(all_files$`LINE 2`) &
                                  is.na(all_files$`LINE 3`) &
                                  is.na(all_files$`LINE 4`)] <- 1

all_files
}

#' Build Gradaute School Report
#'
#' @param data Data frame of Graduate School data or character string of location of excel and/or csv data
#'
#' @return
#' @export
#'
#' @importFrom rmarkdown render
#'
#' @examples
buildReport <- function(data){
  if(is.character(data)){data <- read_clean(data)}
  fileLocation <- system.file("dynamicReport.Rmd", package="GradSchoolReport")
  render(fileLocation, params = "ask")
}

#' Invisible Package Load
#'
#' @param package character vector of packages
#'
#' @return
#' @export
#'
#' @examples
invisiblePackage <- function(package){
  invisible(lapply(package, function(x){
    library(x, character.only = TRUE)
  }))
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
runGradSchoolApp <- function() {
  appDir <- system.file("inst", "Graduate_School_Report", package = "GradSchoolReport")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `GradSchoolReport`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
