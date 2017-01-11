#' Bearhaus Enrollment Cleaning Function
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
BearhausEnrollmentClean.data.frame <- function(x, ...){
  dots <- lazyeval::lazy_dots(...)
  x
}
