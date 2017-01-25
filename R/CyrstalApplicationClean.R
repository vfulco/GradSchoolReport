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
#' @export
CrystalApplicationClean.list <- function(x, ...){
  dots <- lazyeval::lazy_dots(...)
  x <- plyr::ldply(x)
  do.call(what = CrystalApplicationClean.data.frame,
          args = c(x = list(x),
                   lazyeval::lazy_eval(dots)))
}

#' @rdname CrystalApplicationClean
#' @export
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
CrystalApplicationClean.data.frame <- function(x, ...){
  dots <- lazyeval::lazy_dots(...)
  browser()
  x <- filter(x, str_detect(ID, "[0-9]"))
  x$Event <- NA
  x$`Event Date` <- NA
  x[x$`LINE 2` == "A" & x$`LINE 3` == "A" & x$`LINE 4` %in% c("Y", "Ys", "Yd"),]$Event <- "Regular Accept"
  x[x$`LINE 2` == "A" & x$`LINE 3` == "A" & x$`LINE 4` %in% c("Y", "Ys", "Yd"),]$`Event Date` <- x$`LINE 4 DATE`
  x[x$`LINE 2` == "P" & x$`LINE 3` == "P" & x$`LINE 4` %in% c("Y", "Ys", "Yd"),]$Event <- "Probationary Accept"
  x[x$`LINE 2` == "A" & x$`LINE 3` == "A" & x$`LINE 4` %in% c("Y", "Ys", "Yd"),]$`Event Date` <- x$`LINE 4 DATE`
  x[x$`LINE 2` == "A" & x$`LINE 3` == "A" & x$`LINE 4` %in% c("N", "Ns", "Nd"),]$Event <- "Regular Decline"
  x[x$`LINE 2` == "A" & x$`LINE 3` == "A" & x$`LINE 4` %in% c("N", "Ns", "Nd"),]$`Event Date` <- x$`LINE 4 DATE`
  x[x$`LINE 2` == "P" & x$`LINE 3` == "P" & x$`LINE 4` %in% c("N", "Ns", "Nd"),]$Event <- "Probationary Decline"
  x[x$`LINE 2` == "A" & x$`LINE 3` == "A" & x$`LINE 4` %in% c("N", "Ns", "Nd"),]$`Event Date` <- x$`LINE 4 DATE`
  x[x$`LINE 2` == "D" & x$`LINE 3` == "D",]$Event <- "Rejected"
  x[x$`LINE 2` == "D" & x$`LINE 3` == "D",]$`Event Date` <- x$`LINE 3 DATE`
  x[x$`LINE 4` == "C",]$Event <- "Cancelled"
  x[x$`LINE 2` == "C",]$`Event Date` <- x$`LINE 4 DATE`

  x
}
