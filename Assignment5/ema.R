# ==========================================================
# Exponential Moving Average (EMA)
# BDA400 Assignment 5
# Student: Zahra Ehsani
# ==========================================================

ema <- function(data, period) {
  
  # Calculate multiplier
  multiplier <- 2 / (period + 1)
  
  # Create empty vector
  ema_values <- numeric(length(data))
  
  # First EMA value
  ema_values[1] <- data[1]
  
  # Calculate remaining EMA values
  for (i in 2:length(data)) {
    
    ema_values[i] <- (data[i] - ema_values[i - 1]) *
      multiplier + ema_values[i - 1]
    
  }
  
  return(ema_values)
}