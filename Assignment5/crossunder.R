# ==========================================================
# Crossunder Function
# BDA400 Assignment 5
# Student: Zahra Ehsani
# ==========================================================

crossunder <- function(arr1, arr2) {
  
  # Check if both arrays have the same length
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }
  
  # Initialize vector for crossunder signals
  crossunder_signals <- character(length(arr1))
  
  # The first position cannot contain a crossunder
  crossunder_signals[1] <- "None"
  
  # Check for crossunder signals
  for (i in 2:length(arr1)) {
    
    if (arr1[i] < arr2[i] &&
        arr1[i - 1] >= arr2[i - 1]) {
      
      crossunder_signals[i] <- "True"
      
    } else {
      
      crossunder_signals[i] <- "False"
      
    }
  }
  
  return(crossunder_signals)
}s