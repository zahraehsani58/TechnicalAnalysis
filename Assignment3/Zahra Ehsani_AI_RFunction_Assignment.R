# =========================================================
# AI-Generated R Function Assignment
# Student: Zahra Ehsani
# =========================================================


# ---------------------------------------------------------
# Original Task Description
# ---------------------------------------------------------

# Create an R function that removes outliers from a numeric
# vector using the interquartile range method and returns
# the cleaned values.


# ---------------------------------------------------------
# Original AI-Generated Function
# ---------------------------------------------------------

remove_outliers_original <- function(x) {
  
  # Calculate the first quartile
  q1 <- quantile(x, 0.25)
  
  # Calculate the third quartile
  q3 <- quantile(x, 0.75)
  
  # Calculate the interquartile range
  iqr_value <- q3 - q1
  
  # Calculate the lower outlier boundary
  lower_limit <- q1 - 1.5 * iqr_value
  
  # Calculate the upper outlier boundary
  upper_limit <- q3 + 1.5 * iqr_value
  
  # Keep only values inside the boundaries
  cleaned_data <- x[
    x >= lower_limit &
      x <= upper_limit
  ]
  
  # Return the cleaned values
  return(cleaned_data)
}


# ---------------------------------------------------------
# Refined AI-Generated Function
# ---------------------------------------------------------

remove_outliers <- function(x) {
  
  # Check whether the input is numeric
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector.")
  }
  
  # Remove missing values
  x_clean <- x[!is.na(x)]
  
  # Check whether valid values remain
  if (length(x_clean) == 0) {
    stop("The vector contains no valid numeric values.")
  }
  
  # Calculate the first quartile
  q1 <- quantile(x_clean, 0.25)
  
  # Calculate the third quartile
  q3 <- quantile(x_clean, 0.75)
  
  # Calculate the interquartile range
  iqr_value <- q3 - q1
  
  # Calculate the lower outlier boundary
  lower_limit <- q1 - 1.5 * iqr_value
  
  # Calculate the upper outlier boundary
  upper_limit <- q3 + 1.5 * iqr_value
  
  # Keep only values inside the boundaries
  cleaned_data <- x_clean[
    x_clean >= lower_limit &
      x_clean <= upper_limit
  ]
  
  # Return the cleaned values
  return(cleaned_data)
}


# ---------------------------------------------------------
# Synthetic Test Data
# ---------------------------------------------------------

test_data <- c(10, 12, 14, 15, 16, 18, 20, 100, NA)


# ---------------------------------------------------------
# Test 1: Original Function
# ---------------------------------------------------------

original_result <- tryCatch(
  remove_outliers_original(test_data),
  error = function(e) e$message
)

print("Original function result:")
print(original_result)


# ---------------------------------------------------------
# Test 2: Refined Function
# ---------------------------------------------------------

refined_result <- remove_outliers(test_data)

print("Refined function result:")
print(refined_result)
# ---------------------------------------------------------
# Independent Cross-Check using R
# ---------------------------------------------------------

boxplot_result <- boxplot.stats(test_data)$out

print("Outliers identified by boxplot.stats():")
print(boxplot_result)
# ---------------------------------------------------------
# Self Spot-Check
# ---------------------------------------------------------

manual_expected <- c(10, 12, 14, 15, 16, 18, 20)

identical_result <- identical(refined_result, manual_expected)

print("Self spot-check passed:")
print(identical_result)