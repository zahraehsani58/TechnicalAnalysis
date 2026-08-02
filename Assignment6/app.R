# Load required packages
library(shiny)
library(ggplot2)
library(quantmod)
library(zoo)

# -----------------------------
# Step 1: Fetch Historical Stock Data
# -----------------------------

stock_symbol <- "AAPL"

start_date <- "2023-01-01"
end_date <- "2023-07-01"

stock_data <- getSymbols(
  stock_symbol,
  src = "yahoo",
  from = start_date,
  to = end_date,
  auto.assign = FALSE
)

# Display the first six rows of the stock data
head(stock_data)

# Display a summary of the stock data
summary(stock_data)


# -----------------------------
# Step 2: Visualizing Stock Data
# -----------------------------

# Define the user interface
ui <- fluidPage(
  
  titlePanel("Stock Portfolio Dashboard"),
  
  dateRangeInput(
    "date_range",
    "Select Date Range:",
    start = "2023-01-01",
    end = "2023-07-01"
  ),
  
  selectInput(
    "time_frame",
    "Select Time Frame:",
    choices = c("Daily", "Weekly", "Monthly")
  ),
  checkboxGroupInput(
    "technical_indicators",
    "Select Technical Indicators:",
    choices = c(
      "Moving Average",
      "RSI",
      "MACD"
    )
  ),
  plotOutput("stock_chart")
  
)
# Define the server logic
  server <- function(input, output) {
    
    output$stock_chart <- renderPlot({
      
      # Filter stock data by selected date range
      filtered_data <- stock_data[
        index(stock_data) >= input$date_range[1] &
          index(stock_data) <= input$date_range[2]
      ]
      
      # Change data frequency based on the selected time frame
      if (input$time_frame == "Weekly") {
        filtered_data <- apply.weekly(
          filtered_data,
          function(x) x[NROW(x), ]
        )
      } else if (input$time_frame == "Monthly") {
        filtered_data <- apply.monthly(
          filtered_data,
          function(x) x[NROW(x), ]
        )
      }
      
      # Convert stock data to a data frame for ggplot2
      stock_df <- data.frame(
        Date = index(filtered_data),
        Close = as.numeric(Cl(filtered_data))
      )
      
      # Create the ggplot2 line chart
      p <- ggplot(stock_df, aes(x = Date, y = Close)) +
        geom_line(color = "blue", size = 1) +
        geom_point(size = 1.5) +
        labs(
          title = paste(
            stock_symbol,
            input$time_frame,
            "Stock Price"
          ),
          x = "Date",
          y = "Closing Price"
        ) +
        theme_minimal()
      # Calculate short-term and long-term moving averages
      stock_df$MA5 <- zoo::rollmean(
        stock_df$Close,
        k = 5,
        fill = NA,
        align = "right"
      )
      
      stock_df$MA20 <- zoo::rollmean(
        stock_df$Close,
        k = 20,
        fill = NA,
        align = "right"
      )
      
      # Add the 20-period moving average when selected
      if ("Moving Average" %in% input$technical_indicators) {
        
        p <- p +
          geom_line(
            data = stock_df,
            aes(x = Date, y = MA20),
            color = "red",
            linewidth = 1,
            na.rm = TRUE
          )
      }
      
      # Generate Buy, Sell, and Hold signals
      stock_df$Signal <- "Hold"
      
      buy_signal <-
        stock_df$MA5 > stock_df$MA20 &
        dplyr::lag(stock_df$MA5) <= dplyr::lag(stock_df$MA20)
      
      sell_signal <-
        stock_df$MA5 < stock_df$MA20 &
        dplyr::lag(stock_df$MA5) >= dplyr::lag(stock_df$MA20)
      
      # Replace unavailable comparisons with FALSE
      buy_signal[is.na(buy_signal)] <- FALSE
      sell_signal[is.na(sell_signal)] <- FALSE
      
      stock_df$Signal[buy_signal] <- "Buy"
      stock_df$Signal[sell_signal] <- "Sell"
      
      # Add Buy and Sell labels to the chart
      p <- p +
        geom_text(
          data = subset(stock_df, Signal != "Hold"),
          aes(x = Date, y = Close, label = Signal),
          vjust = -1,
          color = "blue",
          na.rm = TRUE
        )
      
      print(p)
      
    })
    
  }

# Run the Shiny application
shinyApp(ui = ui, server = server)


