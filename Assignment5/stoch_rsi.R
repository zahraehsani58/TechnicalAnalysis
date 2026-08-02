# ==========================================================
# Stochastic RSI (StochRSI)
# BDA400 Assignment 5
# Student: Zahra Ehsani
# ==========================================================

stoch_rsi <- function(data, period, k_period, d_period) {
  
  # Calculate RSI values
  rsi_values <- rsi(data, period)
  
  # Find the valid RSI values, excluding NA values
  valid_rsi <- rsi_values[!is.na(rsi_values)]
  
  # Check that RSI produced valid values
  if (length(valid_rsi) == 0) {
    stop("No valid RSI values are available.")
  }
  
  # Find the minimum and maximum RSI values
  min_rsi <- min(valid_rsi)
  max_rsi <- max(valid_rsi)
  
  # Check that the RSI range is not zero
  if (max_rsi == min_rsi) {
    
    # All valid StochRSI values are zero when RSI values are equal
    k_values <- rep(0, length(valid_rsi))
    
  } else {
    
    # Normalize RSI values between 0 and 1
    k_values <- (valid_rsi - min_rsi) / (max_rsi - min_rsi)
    
  }
  
  # Check whether enough values exist for the %K calculation
  if (length(k_values) < k_period) {
    stop("Not enough RSI values to calculate the K line.")
  }
  
  # Calculate the %K line using SMA
  k_line <- sma(k_values, k_period)
  
  # Check whether enough values exist for the %D calculation
  if (length(k_line) < d_period) {
    stop("Not enough K values to calculate the D line.")
  }
  
  # Calculate the %D line using SMA
  d_line <- sma(k_line, d_period)
  
  # Store the results in a list
  result <- list(
    k_line = k_line,
    d_line = d_line
  )
  
  return(result)
}