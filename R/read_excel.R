#' Wrapper for read_excel to handle lists of homogeneous files
#'
#' @param files list of files for read_excel()
#' @param ... options pased to read_excel()
#' @param combine whether to combine the list of read data frames or not
#'
#' @return list or data frame of read files
#' @export
#'
read_excel <- function(files, ...){
  UseMethod("read_excel")
}


#' @export
#' @rdname read_excel
read_excel.list <- function(files, ..., combine = FALSE){
  dots <- lazyeval::lazy_dots(...)

  x <- if(combine){
    do.call(what = plyr::ldply,
            args = c(.data = list(files),
                     .fun = readxl::read_excel,
                     lazyeval::lazy_eval(dots))
    )
  }else{
    do.call(what = plyr::llply,
            args = c(.data = list(files),
                     .fun = readxl::read_excel,
                     lazyeval::lazy_eval(dots))
    )
  }

  x
}

#' @export
#' @rdname read_excel
read_excel.default <- function(files, ...){
  dots <- lazyeval::lazy_dots(...)

  do.call(what = readxl::read_excel,
          args = c(path = files,
                   lazyeval::lazy_eval(dots)))
}
