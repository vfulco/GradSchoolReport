#' Application Summaries
#'
#' @param data
#'
#' @export
#'
application_summary <- function(data){
  data %>% summarise(`Total Apps` = length(ID),
                     `Offers` = sum(`Regular Admission Extended`, `Probationary Admission Extended`, na.rm = TRUE),
                     `Rejected` = sum(Rejected, na.rm = TRUE),
                     `Enrolled` = sum(`Regular Admission Accepted`, `Probationary Admission Accepted`, na.rm = TRUE),
                     `Declined` = sum(`Regular Admission Declined`, `Probationary Admission Declined`, na.rm = TRUE))
}

