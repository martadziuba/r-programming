# Write a function that takes a directory of data files and a threshold for complete cases and
# calculates the correlation between sulfate and nitrate for monitor locations where the number
# of completely observed cases (on all variables) is greater than the threshold. The function
# should return a vector of correlations for the monitors that meet the threshold requirement.
# If no monitors meet the threshold requirement, then the function should return a numeric vector
# of length 0.

corr <- function (directory, threshold = 0) {
  filelist <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  compl <- complete(directory, id = 1:332) # use complete function from previous exercise
  corVector <- numeric()
  for (i in compl$id) {
    if (compl$nobs[i] > threshold) {
      data <- read.csv(filelist[i])
      cor <- cor(data[["sulfate"]], data[["nitrate"]], na.rm = TRUE)
      corVector <- c(corVector, cor)
    }
  }
  corVector
}