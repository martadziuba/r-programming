best <- function(state, outcome) {
  # Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
  
  # Check that state and outcome are valid
  if (!state %in% data$State) {
    stop("invalid state")
  }
  
  valid_outcome <- c("heart attack", "heart failure", "pneumonia")
  if (!outcome %in% valid_outcome) {
    stop("invalid outcome")
  }
  
  # Return hospital name in that state with lowest 30-day death rate
  outcome_col <- switch(outcome, "heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  state_data <- data[data$State == state, ]
  state_data[, outcome_col] <- as.numeric(state_data[, outcome_col]) # transform outcome data to numeric type for sorting
  sorted_data <- state_data[order(state_data[, outcome_col], state_data[, 2], na.last = NA), ]
  hosp_name <- sorted_data[1, 2]
  hosp_name

}