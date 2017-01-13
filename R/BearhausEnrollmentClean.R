#' Bearhaus Enrollment Cleaning Function
#'
#' Given the data, this function should remove blank columns that don't have any data.
#' The function should also fix any data irregularities, and should combine lists of
#' data. This function is not going to read in data.
#'
#'
#' @param x data in a list or data frame
#' @param ... other options
#' @param test never use this only for package testing purposes
#'
#' @return data frame
#' @export
#'
BearhausEnrollmentClean <- function(x, ...){
  UseMethod("BearhausEnrollmentClean")
}

#' @rdname BearhausEnrollmentClean
#' @export
BearhausEnrollmentClean.list <- function(x, ...){
  dots <- lazyeval::lazy_dots(...)
  x <- plyr::ldply(x)
  do.call(what = BearhausApplicationClean.data.frame,
          args = c(x = list(x),
                   lazyeval::lazy_eval(dots)))
}

#' @rdname BearhausEnrollmentClean
#' @export
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom stringr str_extract
BearhausEnrollmentClean.data.frame <- function(x, ..., test = FALSE){
  dots <- lazyeval::lazy_dots(...)
  if(!(test)){
  x <- x[,!(sapply(x, function(y){
    all(is.na(y))
    }))]
  x <- select(x, -c(`GRE Quan Percent`, `GRE Verbal Percent`, `GRE Quant REV Percent`, GRE_Verbal_REV_Percent, `GRE Writing Percent`))
  x <- rename(x, `GRE Quantitative` = `GRE HI Quantitative`, `GRE Verbal` = `GRE HI Verbal`, `Academic Year` = Year)
  x$`Year` <- str_extract(x$Term, "[0-9]{4}")
  }
  x
}
