# Crossover function

crossover <- function(arr1, arr2) {
  
  # Check if both arrays have the same length
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length.")
  }
  
  # Initialize result vector
  crossover_signals <- character(length(arr1))
  crossover_signals[1] <- "None"
  
  # Check for crossover
  for (i in 2:length(arr1)) {
    
    if (arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1]) {
      
      crossover_signals[i] <- "Up"
      
    } else if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
      
      crossover_signals[i] <- "Down"
      
    } else {
      
      crossover_signals[i] <- "None"
      
    }
  }
  
  return(crossover_signals)
}