rankall <- function(outcome, num = "best") {
  # Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
  
  # Check that outcome and num are valid
  valid_outcome <- c("heart attack", "heart failure", "pneumonia")
  if (!outcome %in% valid_outcome) {
    stop("invalid outcome")
  }
  
  valid_num <- c("best", "worst")
  if (!num %in% valid_num & !is.numeric(num)) {
    stop("invalid number")
  }
  
  # For each state, find the hospital of the given rank
  # Return a data frame with the hospital names and the (abbreviated) state name
  outcome_col <- switch(outcome, "heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  states <- unique(data$State)
  states <- states[order(states)]
  hospitals <- vector()
  
  for (i in 1:length(states)) {
    
    state_data <- data[data$State == states[i], ]
    sorted_data <- state_data[order(as.numeric(state_data[, outcome_col]), state_data[, 2], na.last = NA), ]
    sorted_outcome <- as.vector(sorted_data[, outcome_col])

    if (num == "best") {
      hosp_name <- sorted_data[1, 2]
    } else if (num == "worst") {
      hosp_name <- sorted_data[length(sorted_outcome), 2]
    } else if (num < length(sorted_outcome)) {
      hosp_name <- sorted_data[num, 2]
    } else {
      hosp_name <- NA
    }

    hospitals <- c(hospitals, hosp_name)
  }

  ranked_data <- data.frame(hospital = hospitals, state = states, row.names = states)
  ranked_data

}