# ==========================================================
# Simple Moving Average (SMA)
# BDA400 Assignment 5
# Student: Zahra Ehsani
# ==========================================================

sma <- function(data, period) {
  
  # Check if enough data exists
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }
  
  # Create an empty vector
  sma_values <- numeric()
  
  # Calculate SMA
  for (i in 1:(length(data) - period + 1)) {
    
    current_window <- data[i:(i + period - 1)]
    
    mean_value <- sum(current_window) / period
    
    sma_values[i] <- mean_value
  }
  
  return(sma_values)
}