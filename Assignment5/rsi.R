# ==========================================================
# Relative Strength Index (RSI)
# BDA400 Assignment 5
# Student: Zahra Ehsani
# ==========================================================

rsi <- function(data, period) {
  
  # Check that enough data is available
  if (length(data) <= period) {
    stop("Data length must be greater than the period")
  }
  
  # Calculate differences between consecutive data points
  diff_values <- diff(data)
  
  # Initialize vectors for gains and losses
  gains <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))
  
  # Calculate gains and losses
  for (i in 1:length(diff_values)) {
    
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
      losses[i] <- 0
    } else {
      gains[i] <- 0
      losses[i] <- abs(diff_values[i])
    }
  }
  
  # Calculate initial average gain and loss
  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period
  
  # Initialize RSI vector with NA values
  rsi_values <- rep(NA, length(data))
  
  # Calculate the first RSI value
  if (avg_loss == 0) {
    rsi_values[period + 1] <- 100
  } else {
    rs <- avg_gain / avg_loss
    rsi_values[period + 1] <- 100 - (100 / (1 + rs))
  }
  
  # Calculate remaining RSI values using Wilder's smoothing
  if (length(data) > period + 1) {
    
    for (i in (period + 2):length(data)) {
      
      avg_gain <- (
        avg_gain * (period - 1) + gains[i - 1]
      ) / period
      
      avg_loss <- (
        avg_loss * (period - 1) + losses[i - 1]
      ) / period
      
      if (avg_loss == 0) {
        rsi_values[i] <- 100
      } else {
        rs <- avg_gain / avg_loss
        rsi_values[i] <- 100 - (100 / (1 + rs))
      }
    }
  }
  
  return(rsi_values)
}