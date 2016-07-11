#' Remove Term Seperator from Column 1
#'
#' @param dataFrame data frame
#'
#' @keywords internal
#'
#' @importFrom stringr str_detect
#' @importFrom stringr str_sub
#'
removeTermSeperatorColumn1 <- function(dataFrame){
  removedAlphas <- dataFrame[!(str_detect(str_sub(dataFrame[[1]], 1, 6), "[[a-z][A-Z]]")),]
  removedNumerics <- dataFrame[!(str_detect(str_sub(dataFrame[[1]], 1, 6), "[0-9]{6}")),]
  isLarger(removedAlphas, removedNumerics)
}
