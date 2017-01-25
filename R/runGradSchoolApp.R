#' Title
#'
#' @export
#'
runGradSchoolApp <- function() {
  appDir <- system.file("Graduate_School_Report", package = "GradSchoolReport")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `GradSchoolReport`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
