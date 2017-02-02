#' Deadline Helper Function
#'
#' @export
#'
deadlines_ <- function(data){
  browser()
  year <- str_extract(data$TERM, "[0-9]{4}")
  semester <- str_replace(data$TERM, "[0-9]{4}", "")
  dl <- if(data$DEGR == "EDD" & data$MAJOR == "EDC"){
    if(semester == "20" | semester == "30"){
      make_datetime(year = year, month = 3, day = 15, hour = 23, min = 59, sec = 59)
    }else{
      if(semester == "10"){
        make_datetime(year = year - 1, month = 10, day = 15, hour = 23, min = 59, sec = 59)
      }else{
        warning(paste("You do not have a deadline set up for", data$DEGR, data$MAJOR, semester, "\n"))
      }
    }
  }else{
    if(data$DEGR == "PHD" & data$MAJOR == "BIO"){
      if(semester == "30" | semester == "20"){
        make_datetime(year = year, month = 2, day = 15, hour = 23, min = 59, sec = 59)
      }else{
        warning(paste("You do not have a deadline set up for", data$DEGR, data$MAJOR, semester, "\n"))
      }
    }else{
      if(data$DEGR == "PHD" & data$MAJOR == "BMS"){
        if(semester == "30"){
          make_datetime(year = year, month = 2, day = 15, hour = 23, min = 59, sec = 59)
        }else{
          if(semester == "20"){
            make_datetime(year = year, month = 5, day = 1, hour = 23, min = 59, sec = 59)
          }else{
            if(semester == "10"){
              make_datetime(year = year - 1, month = 12, day = 1, hour = 23, min = 59, sec = 59)
            }else{
              warning(paste("You do not have a deadline set up for", data$DEGR, data$MAJOR, semester, "\n"))
            }
          }
        }
      }else{
        if(data$DEGR == "PHD" & data$MAJOR == "CHE"){
          if(semester == "30"){
            make_datetime(year = year, month = 5, day = 15, hour = 23, min = 59, sec = 59)
          }else{
            if(semester == "20"){
              make_datetime(year = year, month = 5, day = 1, hour = 23, min = 59, sec = 59)
            }else{
              if(semester == "10"){
                make_datetime(year = year - 1, month = 12, day = 1, hour = 23, min = 59, sec = 59)
              }else
                warning(paste("You do not have a deadline set up for", data$DEGR, data$MAJOR, semester, "\n"))
            }
          }
        }else{
          if(data$DEGR == "PHD" & data$MAJOR == "CMUS"){
            if(semester == "30"){

            }else{
              warning(paste("You do not have a deadline set up for", data$DEGR, data$MAJOR, semester, "\n"))
            }
          }else{
            if(dataDEGR == "PHD" & data$MAJOR == "CUTE"){
              if(semester == "30"){

              }else{
                warning(paste("You do not have a deadline set up for", data$DEGR, data$MAJOR, semester, "\n"))
              }
            }else{
              if(data$DEGR == "PHD" & data$MAJOR == "EDP"){
                if(semester == "30"){

                }else{
                  warning(paste("You do not have a deadline set up for", data$DEGR, data$MAJOR, semester, "\n"))
                }
              }else{
              }
            }
          }
        }
      }
    }
  }
}

#' Deadline Function
#'
#' @export
#' @importFrom plyr adply
#'
deadlines <- function(data){
adply(data, 1, deadlines_)
}
