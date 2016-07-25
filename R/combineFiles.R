#' @keywords internal
#'
#' @importFrom magrittr %>%
#' @importFrom plyr ldply
#'
combineFiles <- function(data){

  crystal <- data %>% ldply(function(data){
    if(names(data)[1] == "ID"){data}
  })

  goBaylor <- data %>% ldply(function(data){
    if(names(data)[1] == "LAST_NAME"){data[,!(is.na(names(data)))]}
  })

  list(crystal = crystal, goBaylor = goBaylor)
}
