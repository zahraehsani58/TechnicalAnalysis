# ==========================================================
# Linear Regression (LINREG)
# BDA400 Assignment 5
# Student: Zahra Ehsani
# ==========================================================

linreg <- function(regressionSource, regressionLength, regressionOffset) {
  
  # Calculate total number of elements
  n <- length(regressionSource)
  
  # Check regressionLength
  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource")
  }
  
  # Check regressionOffset
  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength")
  }
  
  # Calculate start and end indices
  start_index <- max(1, n - regressionLength + regressionOffset)
  end_index <- min(n, n - regressionOffset)
  
  # Extract subset
  source_subset <- regressionSource[start_index:end_index]
  
  # Index values
  index_values <- 1:length(source_subset)
  
  # Means
  mean_index <- sum(index_values) / length(index_values)
  mean_source <- sum(source_subset) / length(source_subset)
  
  # Numerator
  numerator <- sum((index_values - mean_index) *
                     (source_subset - mean_source))
  
  # Denominator
  denominator <- sum((index_values - mean_index)^2)
  
  # Slope
  slope <- numerator / denominator
  
  # Intercept
  intercept <- mean_source - slope * mean_index
  
  # Predicted values
  predicted_values <- slope * index_values + intercept
  
  # Store results
  result <- list(
    slope = slope,
    intercept = intercept,
    predicted_values = predicted_values
  )
  
  return(result)
}