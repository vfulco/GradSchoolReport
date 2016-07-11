#' Which list element is larger
#'
#' @param ... list of data frames
#'
#' @keywords internal
#'
#' @importFrom magrittr %>%
#' @importFrom plyr ldply
#' @importFrom magrittr extract2
#'
isLarger <- function(...){
  list <- list(...)
  index <- list %>% ldply(nrow) %>% extract2(1) %>% which.max
  list %>% extract2(index)
}
