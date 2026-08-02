#==================================================
# BDA400 Assignment 4
# Dataset Cleaning with AI
# Student: Zahra Ehsani
#==================================================

#==================================================
# Purpose:
# Explore, clean, and validate a sales dataset using
# R and dplyr by removing duplicates, handling missing
# values, replacing blank product names, and removing
# quantity outliers.
#==================================================

# Load package
library(dplyr)

# Load dataset
sales_data <- read.csv("sales_data_dirty.csv", stringsAsFactors = FALSE)

# Display the first 6 rows
head(sales_data)
#==================================================
# Step 1 - Explore the Dataset
#==================================================

# View dataset structure
glimpse(sales_data)

# Summary statistics
summary(sales_data)

# Missing values in each column
colSums(is.na(sales_data))

# Total missing values
sum(is.na(sales_data))

# Number of duplicate rows
sum(duplicated(sales_data))
#==================================================
# Check categorical values
#==================================================

table(sales_data$Product)

table(sales_data$Customer)
#==================================================
# Check for outliers
#==================================================

boxplot(sales_data$Quantity,
        main = "Boxplot of Quantity")

boxplot(sales_data$Price,
        main = "Boxplot of Price")
# Count blank Product values
sum(sales_data$Product == "")



# Count blank Product values
sum(sales_data$Product == "")


#==================================================
# Step 2 - AI Prompts
#==================================================

# AI Prompt 1 (Seed)
# I have a sales dataset in R with missing values,
# duplicate rows, blank product names,
# and outliers in Quantity.
# What are common data cleaning steps I should
# perform using dplyr?

# AI Prompt 2 (Refinement)
# Here is a sample of my dataset (first 10 rows).
# Suggest R code using dplyr to clean the dataset.

# AI Prompt 3 (Validation)
# Please explain what each line of the R code does
# and how I can verify that it worked correctly.


#==================================================
# Step 2 - Cleaning the Dataset
#==================================================


# Create a copy of the original dataset
clean_sales <- sales_data


#------------------------------------------
# Remove duplicate rows
#------------------------------------------

clean_sales <- clean_sales %>%
  distinct()

# Check the number of rows after removing duplicates
nrow(clean_sales)


#------------------------------------------
# Replace blank Product values
#------------------------------------------

clean_sales$Product[clean_sales$Product == ""] <- "Unknown"

# Verify that no blank Product values remain
sum(clean_sales$Product == "")


#------------------------------------------
# Fill missing Quantity values
#------------------------------------------

clean_sales$Quantity[is.na(clean_sales$Quantity)] <-
  median(clean_sales$Quantity, na.rm = TRUE)

# Verify that there are no missing values
sum(is.na(clean_sales$Quantity))


#------------------------------------------
# Remove Quantity outliers using the IQR method
#------------------------------------------

Q1 <- quantile(clean_sales$Quantity, 0.25)
Q3 <- quantile(clean_sales$Quantity, 0.75)
IQR_value <- IQR(clean_sales$Quantity)

lower_limit <- Q1 - 1.5 * IQR_value
upper_limit <- Q3 + 1.5 * IQR_value

# Display the calculated limits
lower_limit
upper_limit

# Keep only reasonable Quantity values
clean_sales <- clean_sales %>%
  filter(Quantity >= lower_limit,
         Quantity <= upper_limit)

# Check the number of rows after removing outliers
nrow(clean_sales)


#==================================================
# Verify the cleaned dataset
#==================================================

# Summary of the cleaned dataset
summary(clean_sales)

# Check for missing values
sum(is.na(clean_sales))

# Check for duplicate rows
sum(duplicated(clean_sales))

# Check Product categories
table(clean_sales$Product)

# Check Customer names
table(clean_sales$Customer)

# Number of rows
nrow(clean_sales)


#==================================================
# Before vs After Comparison
#==================================================

# Original dataset
summary(sales_data)
sum(is.na(sales_data))
sum(duplicated(sales_data))
nrow(sales_data)

# Cleaned dataset
summary(clean_sales)
sum(is.na(clean_sales))
sum(duplicated(clean_sales))
nrow(clean_sales)

# Save cleaned dataset
write.csv(clean_sales,
          "clean_sales_data.csv",
          row.names = FALSE)

#==================================================
# Final Verification
#==================================================

cat("Original rows:", nrow(sales_data), "\n")
cat("Cleaned rows:", nrow(clean_sales), "\n")
cat("Missing values:", sum(is.na(clean_sales)), "\n")
cat("Duplicate rows:", sum(duplicated(clean_sales)), "\n")
