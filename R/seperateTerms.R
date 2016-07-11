#' @keywords internal
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom stringr str_sub
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#' @importFrom dplyr select
#'
seperateTerms <- function(dataFrame){
  dataFrame$YEAR <- dataFrame$TERM %>% str_sub(1, 4)
  dataFrame$TERM_Num <- dataFrame$TERM %>% str_sub(5, 6)

  dataFrame %<>% left_join(terms(), by = c("TERM_Num" = "TERM_Num")) %>%
    select(-TERM.x, -TERM_Num) %>% rename(TERM = TERM.y)
}
