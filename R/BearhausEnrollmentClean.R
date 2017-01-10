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

}

#' @rdname BearhausEnrollmentClean
#' @export
BearhausEnrollmentClean.data.frame <- function(x, ...){

}
