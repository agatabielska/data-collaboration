library(plotly)
library(ggplot2)
source("api.r")

function(input, output, session) {
  output$base_currency_output <- renderPrint({
    req(input$base_currency)
    paste("Selected base currency:", input$base_currency)
  })
  
  output$currency_output <- renderPrint({
    req(input$currency)
    paste("Selected target currency:", input$currency)
  })
  
  output$currency_date <- renderPrint({
    req(input$dateRange)
    start_date <- input$dateRange[1]
    end_date <- input$dateRange[2]
    paste("Date range:", start_date, "to", end_date)
  })

  output$currency_plot <- renderPlotly({
    # Ensure we have all inputs before proceeding
    req(input$base_currency, input$currency, input$dateRange)
    
    base_currency <- input$base_currency
    chosen_currency <- input$currency  # This was the key issue - using wrong variable name
    start_date <- input$dateRange[1]
    end_date <- input$dateRange[2]
    
    # Get data from API
    data <- from_to_values(base_currency, start_date, end_date)
    
    # Make sure data exists and has the chosen currency column
    req(data, chosen_currency %in% colnames(data))
    
    # Create data frame with the specific currency data
    currency_data <- data.frame(
      date = as.Date(data[[1]]),  # Convert first column to date
      value = as.numeric(data[[chosen_currency]])  # Get values for chosen currency
    )
    
    # Check if we have data
    req(nrow(currency_data) > 0)
    
    # Debug print - remove this in production
    print(paste("Data rows:", nrow(currency_data)))
    print(head(currency_data))
    
    # Create the plot
    p <- ggplot(currency_data, aes(x = date, y = value)) +
      geom_line(color = "blue", size = 1) +
      labs(
        title = paste("Exchange Rate:", base_currency, "to", chosen_currency), 
        y = "Value", 
        x = "Date"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
}