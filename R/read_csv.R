#' Wrapper for read_csv to handle lists of homogeneous files
#'
#' @param files list of files for read_csv()
#' @param ... options pased to read_csv()
#' @param combine whether to combine the list of read data frames or not
#'
#' @return list or data frame of read files
#' @export
#'
read_csv <- function(files, ...){
  UseMethod("read_csv")
}

#' @export
#' @rdname read_csv
read_csv.list <- function(files, ..., combine = FALSE){
  dots <- lazyeval::lazy_dots(...)

  x <- if(combine){
      do.call(what = plyr::ldply,
              args = c(.data = list(files),
                       .fun = readr::read_csv,
                       lazyeval::lazy_eval(dots))
      )
  }else{
      do.call(what = plyr::llply,
              args = c(.data = list(files),
                       .fun = readr::read_csv,
                       lazyeval::lazy_eval(dots))
      )
  }

  x
}

#' @export
#' @rdname read_csv
read_csv.default <- function(files, ...){
  dots <- lazyeval::lazy_dots(...)

  do.call(what = readr::read_csv,
          args = c(file = files,
                   lazyeval::lazy_eval(dots)))
}
