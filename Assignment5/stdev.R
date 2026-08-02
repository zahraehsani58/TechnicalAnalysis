# ==========================================================
# Standard Deviation (STDEV)
# BDA400 Assignment 5
# Student: Zahra Ehsani
# ==========================================================

stdev <- function(data) {
  
  # Number of observations
  n <- length(data)
  
  # Check if enough observations exist
  if (n < 2) {
    stop("At least two observations are required.")
  }
  
  # Calculate the mean
  mean_value <- sum(data) / n
  
  # Calculate squared differences
  squared_diff <- (data - mean_value)^2
  
  # Calculate variance
  variance <- sum(squared_diff) / (n - 1)
  
  # Calculate standard deviation
  standard_deviation <- sqrt(variance)
  
  return(standard_deviation)
}