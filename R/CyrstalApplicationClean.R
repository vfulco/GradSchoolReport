#' Crystal Application Cleaning Function
#'
#' Given the data, this function should remove blank columns that don't have any data.
#' The function should also fix any data irregularities, and should combine lists of
#' data. This function is not going to read in data.
#'
#'
#' @param x data in a list or data frame
#' @param ... other options
#'
#' @return data frame
#' @export
#'
CrystalApplicationClean <- function(x, ...){
  UseMethod("CrystalApplicationClean")
}

#' @rdname CrystalApplicationClean
#' @importFrom plyr ldply
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
#' @export
CrystalApplicationClean.list <- function(x, ...){
  dots <- lazy_dots(...)
  x <- ldply(x)
  do.call(what = CrystalApplicationClean.data.frame,
          args = c(x = list(x),
                   lazy_eval(dots)))
}

#' @rdname CrystalApplicationClean
#' @export
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom lazyeval lazy_dots
CrystalApplicationClean.data.frame <- function(x, ...){
  dots <- lazy_dots(...)
  browser()
  x <- filter(x, str_detect(ID, "[0-9]"))



  xfilter <- bind_rows(mutate(filter(x, `LINE 2` == "A" & `LINE 3` == "A" & `LINE 4` %in% c("Y", "Ys", "Yd", "A")),
              Event = "Regular Accept",
              `Event Date` = `LINE 4 DATE`),
              mutate(filter(x, `LINE 2` %in% c("P", "A") & `LINE 3` == "P" & `LINE 4` %in% c("Y", "Ys", "Yd", "A")),
                     Event = "Probationary Accept",
                     `Event Date` = `LINE 4 DATE`),
              mutate(filter(x, `LINE 2` == "A" & `LINE 3` == "A" & `LINE 4` %in% c("N", "Ns", "Nd")),
                     Event = "Regular Decline",
                     `Event Date` = `LINE 4 DATE`),
              mutate(filter(x, `LINE 2` %in% c("P", "A") & `LINE 3` == "P" & `LINE 4` %in% c("N", "Ns", "Nd")),
                     Event = "Probationary Decline",
                     `Event Date` = `LINE 4 DATE`),
              mutate(filter(x, `LINE 2` == "D", is.na(`LINE 3`), is.na(`LINE 4`)),
                     Event = "Rejected",
                     `Event Date` = `LINE 2 DATE`),
              mutate(filter(x, `LINE 3` %in% c("D", "N"), `LINE 2` %in% c("A", "P"), is.na(`LINE 4`)),
                     Event = "Graduate School Rejected",
                     `Event Date` = `LINE 2 DATE`),
              mutate(filter(x, `LINE 3` %in% c("D", "N"), `LINE 2` %in% c("A", "P"), `LINE 4` %in% c("N", "Ns", "Nd")),
                     Event = "Graduate School Rejected",
                     `Event Date` = `LINE 2 DATE`),
              mutate(filter(x, `LINE 3` %in% c("D", "d"), `LINE 2` %in% c("D", "d"), is.na(`LINE 4`)),
                     Event = "Rejected",
                     `Event Date` = `LINE 3 DATE`),
              mutate(filter(x, `LINE 4` == "C" | `LINE 2` == "C" | `LINE 3` == "C"),
                     Event = "Cancelled",
                     `Event Date` = `LINE 4 DATE`),
              mutate(filter(x, is.na(`LINE 2`), is.na(`LINE 3`), is.na(`LINE 4`)),
                     Event = "App Started",
                     `Event Date` = `APP DATE`),
              mutate(filter(x, `LINE 2` %in% c("A", "P"), `LINE 3` %in% c("A", "P"), is.na(`LINE 4`)),
                     Event = "No Applicant Decision",
                     `Event Date` = `LINE 3 DATE`),
              mutate(filter(x, `LINE 2` %in% c("A", "P"), is.na(`LINE 3`), is.na(`LINE 4`)),
                     Event = "No Graduate School Decision",
                     `Event Date` = `LINE 2 DATE`),
              mutate(filter(x, `LINE 3` %in% c("D", "d"), `LINE 2` %in% c("D", "d"), `LINE 4` %in% c("N", "Ns", "Nd", "Y", "Ys", "Yd", "A")),
                     Event = "Rejected",
                     `Event Date` = `LINE 3 DATE`),
              mutate(filter(x, `LINE 2` %in% c("P", "A"), is.na(`LINE 3`), `LINE 4` %in% c("Y", "Ys", "Yd", "A")),
                     Event = "Accept No Department Decision",
                     `Event Date` = `LINE 4 DATE`),
              mutate(filter(x, `LINE 2` == "A", is.na(`LINE 3`), `LINE 4` %in% c("N", "Ns", "Nd")),
                     Event = "Decline No Department Decision",
                     `Event Date` = `LINE 4 DATE`),
              mutate(filter(x, `LINE 3` %in% c("P", "A"), is.na(`LINE 2`), `LINE 4` %in% c("Y", "Ys", "Yd", "A")),
                     Event = "Accept No Graduate School Decision",
                     `Event Date` = `LINE 4 DATE`),
              mutate(filter(x, `LINE 3` == "A", is.na(`LINE 2`), `LINE 4` %in% c("N", "Ns", "Nd")),
                     Event = "Decline No Graduate School Decision",
                     `Event Date` = `LINE 4 DATE`)
  )


}
