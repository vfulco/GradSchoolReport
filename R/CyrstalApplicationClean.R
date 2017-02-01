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

  x <- mutate(x, Event = ifelse(`LINE 2` == "A" & `LINE 3` == "A" & `LINE 4` %in% c("Y", "Ys", "Yd"),
                                "Regular Accept",
                                ifelse(`LINE 2` == "P" & `LINE 3` == "P" & `LINE 4` %in% c("Y", "Ys", "Yd"),
                                       "Probationary Accepte",
                                       ifelse(`LINE 2` == "A" & `LINE 3` == "A" & `LINE 4` %in% c("N", "Ns", "Nd"),
                                              "Regular Decline",
                                              ifelse(`LINE 2` == "P" & `LINE 3` == "P" & `LINE 4` %in% c("N", "Ns", "Nd"),
                                                     "Probationary Decline",
                                                     ifelse(`LINE 2` == "D" | `LINE 3` == "D",
                                                            "Rejected",
                                                            ifelse(`LINE 4` == "C",
                                                                   "Cancelled", "App Started")))))),
              `Event Date` = ifelse(`LINE 2` == "A" & `LINE 3` == "A" & `LINE 4` %in% c("Y", "Ys", "Yd"),
                                    `LINE 4 DATE`,
                                    ifelse(`LINE 2` == "P" & `LINE 3` == "P" & `LINE 4` %in% c("Y", "Ys", "Yd"),
                                           `LINE 4 DATE`,
                                           ifelse(`LINE 2` == "A" & `LINE 3` == "A" & `LINE 4` %in% c("N", "Ns", "Nd"),
                                                  `LINE 4 DATE`,
                                                  ifelse(`LINE 2` == "P" & `LINE 3` == "P" & `LINE 4` %in% c("N", "Ns", "Nd"),
                                                         `LINE 4 DATE`,
                                                         ifelse(`LINE 2` == "D" | `LINE 3` == "D",
                                                                `LINE 3 DATE`,
                                                                ifelse(`LINE 4` == "C",
                                                                       `LINE 4 DATE`, `APP DATE`)))))))



  View(x[x$`LINE 4` == "C",])

}
