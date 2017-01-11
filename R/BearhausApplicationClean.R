#' Bearhaus Application Cleaning Function
#'
#' Given the data, this function should remove blank columns that don't have any data.
#' The function should also fix any data irregularities, and should combine lists of
#' data. This function is not going to read in data.
#'
#' @param x data in a list or data frame
#' @param ... other options
#'
#' @return data frame
#' @export
#'
BearhausApplicationClean <- function(x, ...){
  UseMethod("BearhausApplicationClean")
}

#' @rdname BearhausApplicationClean
#' @export
BearhausApplicationClean.list <- function(x, ...){
  dots <- lazyeval::lazy_dots(...)
  x <- plyr::ldply(x)
  do.call(what = BearhausApplicationClean.data.frame,
          args = c(x = list(x),
                   lazyeval::lazy_eval(dots)))
}

#' @rdname BearhausApplicationClean
#' @export
BearhausApplicationClean.data.frame <- function(x, ...){
 dots <- lazyeval::lazy_dots(...)
x
}
