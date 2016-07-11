#' @keywords internal
#'
#' @importFrom lazyeval lazy_dots
#' @importFrom dplyr mutate_
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval interp
#' @importFrom lazyeval lazy
#'
#'
columnTransform <- function(data, columnName, ...){
 dots <- lazy_dots(...)

 data %>% mutate_(.dots = setNames(
   list(interp(~ ifelse(dot, 1, 0),
     dot = dots[[1]][[1]])),
     paste(lazy(columnName)[[1]])))

}
