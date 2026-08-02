# ==========================================================
# Moving Average Convergence Divergence (MACD)
# BDA400 Assignment 5
# Student: Zahra Ehsani
# ==========================================================

macd <- function(data, short_period, long_period, signal_period) {
  
  # Calculate the short-term EMA
  short_ema <- ema(data, short_period)
  
  # Calculate the long-term EMA
  long_ema <- ema(data, long_period)
  
  # Calculate the MACD line
  macd_line <- short_ema - long_ema
  
  # Calculate the signal line
  signal_line <- ema(macd_line, signal_period)
  
  # Calculate the histogram
  histogram <- macd_line - signal_line
  
  # Store results in a list
  result <- list(
    macd_line = macd_line,
    signal_line = signal_line,
    histogram = histogram
  )
  
  return(result)
}