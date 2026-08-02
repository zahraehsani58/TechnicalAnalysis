#===========================================================
# Assignment 1 - Technical Analysis
# Student: Zahra Ehsani
# Course: BDA400
# Professor: Jinalben Patel
# Date: July 2026
#===========================================================


#===========================================================
# Install required packages
# Run these lines only once
#===========================================================

install.packages("quantmod")
install.packages("TTR")
install.packages("xts")
install.packages("zoo")


#===========================================================
# Load required packages
#===========================================================

library(quantmod)
library(TTR)
library(xts)
library(zoo)


#===========================================================
# Function to Load Stock Data
#===========================================================

load_stock_data <- function(file_name) {
  
  # Read stock symbols from portfolio.txt
  symbols <- readLines(file_name)
  
  # Remove blank lines, if any
  symbols <- symbols[symbols != ""]
  
  # Create an empty list
  stock_list <- list()
  
  # Download stock data for each symbol
  for (symbol in symbols) {
    
    stock_data <- getSymbols(
      symbol,
      src = "yahoo",
      auto.assign = FALSE
    )
    
    # Store each stock dataset in the list
    stock_list[[symbol]] <- stock_data
  }
  
  # Return all stock datasets
  return(stock_list)
}


#===========================================================
# Load stock data from portfolio.txt
#===========================================================

stocks <- load_stock_data("portfolio.txt")

#===========================================================
# Function to Calculate Basic Statistics
#===========================================================

calculate_statistics <- function(stock_list) {
  
  statistics <- data.frame(
    Stock = character(),
    Mean = numeric(),
    Median = numeric(),
    Mode = numeric(),
    Standard_Deviation = numeric(),
    Moving_Average = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (symbol in names(stock_list)) {
    
    stock_data <- stock_list[[symbol]]
    
    # Closing prices
    closing_price <- Cl(stock_data)
    
    # Moving Average (20-day)
    moving_average <- SMA(closing_price, n = 20)
    
    # Mean
    mean_value <- mean(closing_price, na.rm = TRUE)
    
    # Median
    median_value <- median(closing_price, na.rm = TRUE)
    
    # Standard Deviation
    sd_value <- sd(closing_price, na.rm = TRUE)
    
    # Mode
    mode_value <- as.numeric(names(sort(table(round(closing_price,2)),
                                        decreasing = TRUE)[1]))
    
    # Latest Moving Average
    latest_ma <- as.numeric(last(na.omit(moving_average)))
    
    statistics <- rbind(
      statistics,
      data.frame(
        Stock = symbol,
        Mean = round(mean_value,2),
        Median = round(median_value,2),
        Mode = round(mode_value,2),
        Standard_Deviation = round(sd_value,2),
        Moving_Average = round(latest_ma,2)
      )
    )
  }
  
  return(statistics)
}


statistics <- calculate_statistics(stocks)

statistics
#===========================================================
# Function to Display Stock Data
#===========================================================

display_stock_data <- function(stock_list) {
  
  for(symbol in names(stock_list)) {
    
    cat("\n=========================================\n")
    cat("Stock Symbol:", symbol, "\n")
    cat("=========================================\n")
    
    print(head(stock_list[[symbol]]))
    
    cat("\n")
  }
  
}
display_stock_data(stocks)
#===========================================================
# Plot Apple Stock Closing Price
#===========================================================

chartSeries(
  stocks$AAPL,
  theme = chartTheme("white"),
  name = "Apple (AAPL) Stock Price"
)
chartSeries(
  stocks$MSFT,
  theme = chartTheme("white"),
  name = "Microsoft (MSFT) Stock Price"
)