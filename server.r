library(plotly)
library(webshot2)
library(promises)
library(future)
plan(multisession)
library(htmlwidgets)
library(ggplot2)
library(slickR)
library(htmltools)

# Should return TRUE



source("api.r")

apikey = plotlyapikey = '99RHTNDE9YD9TMOW'
twelve_apikey = '6abb374eb01c45979b59e87327fed240'

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
      geom_line(color = "blue", linewidth = 1) +
      labs(
        title = paste("Exchange Rate:", base_currency, "to", chosen_currency), 
        y = "Value", 
        x = "Date"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })

  output$base_currency_bnb_output <- renderPrint({
    req(input$base_currency_bnb)
    paste("Selected base currency:", input$base_currency_bnb)
  })

  output$currencies_bnb_output <- renderPrint({
    req(input$currencies_bnb)
    if (length(input$currencies_bnb) > 0) {
      paste("Selected target currencies:", paste(input$currencies_bnb, collapse = ", "))
    } else {
      paste("No target currencies selected.")
    }
  })

  output$date_range_bnb_output <- renderPrint({
    req(input$dateRange_bnb)
    start_date <- input$dateRange_bnb[1]
    end_date <- input$dateRange_bnb[2]
    paste("Date range:", start_date, "to", end_date)
  })

  output$bull_bear_plot <- renderPlotly({
    req(input$base_currency_bnb, input$currencies_bnb, input$dateRange_bnb)
    chosen_currencies <- input$currencies_bnb
    base_currency <- input$base_currency_bnb
    start_date <- input$dateRange_bnb[1]
    end_date <- input$dateRange_bnb[2]
    currency_data_list <- two_days_values(base_currency, start_date, end_date) 
    selected_columns <- c("date", chosen_currencies)
    filtered_data <- currency_data_list[, selected_columns, drop = FALSE]
    # Calculate percentage change between the second and first currency columns
    values <- c()
    for (i in 2:length(filtered_data)) {
      values <- c(values, ((filtered_data[2, i] / filtered_data[1, i]) - 1) * 100)
    }
    # Create a tidy data frame with currency and percentage change
    colnames <- colnames(filtered_data)[-1]
    result <- data.frame(
        Currency = colnames,
        PercentChange = round(values, 2)
    )
    currency_data <- result
    # Add color column based on positive or negative change
    currency_data$Color <- ifelse(currency_data$PercentChange >= 0, "Increase", "Decrease")

    # Sort data by percentage change (ascending)
    currency_data <- currency_data[order(currency_data$PercentChange), ]

    # Create the ggplot2 visualization (vertical barchart)
    p <- ggplot(currency_data, aes(x = reorder(Currency, PercentChange), 
                              y = PercentChange, 
                              fill = Color)) +
      geom_bar(stat = "identity", width = 0.7) +
      geom_text(aes(label = sprintf("%.2f%%", PercentChange)), 
                vjust = ifelse(currency_data$PercentChange >= 0, -0.5, 1.5),
                color = "black", size = 3.5) +
      scale_fill_manual(values = c("Decrease" = "#FF6B6B", "Increase" = "#4CAF50")) +
      labs(title = "Currency % Change against Base Currency",
          x = "Currency",
          y = "Percentage Change (%)") +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      # Add a reference line at 0
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.5) +
      coord_flip()

    # Print the static plot

    # Convert to interactive plotly chart
    ggplotly(p) %>%
      layout(
        hoverlabel = list(bgcolor = "white"),
        title = list(
          text = "Currency % Change against Base Currency",
          font = list(size = 16)
        ),
        xaxis = list(title = list(text = "Currency", font = list(size = 14))),
        yaxis = list(title = list(text = "", font = list(size = 14)))
      )
  })

  output$currency_converter_output <- renderPrint({
    req(input$from_currency, input$to_currency, input$amount_to_convert, input$dateInput_converter)

    calculated <- input$amount_to_convert * full_api(input$dateInput_converter, input$from_currency)[[input$to_currency]]
    paste("Calculated amount:", calculated, input$to_currency)

  })
  
  output$stock_symbol_output = renderPrint({
    req(input$stock_symbol)
    
    paste("Selected symbol:", input$stock_symbol)
  })
  
  output$stock_date_output = renderPrint({
    req(input$stock_dateRange)

    paste("Selected range:", input$stock_dateRange[1], "to", input$stock_dateRange[2])
  })
  
  output$stock_interval_output = renderPrint({
    req(input$stock_interval)
    
    paste("Selected interval:", input$stock_interval)
  })
  
  output$stock_plot = renderPlotly({
    req(input$stock_symbol, input$stock_dateRange, input$stock_interval)
    
    #vantage_daily_plot(input$stock_symbol, TRUE, input$stock_dateRange[1], input$stock_dateRange[2], api_key = apikey)
    #vantage_weekly_plot(input$stock_symbol, input$stock_dateRange[1], input$stock_dateRange[2], api_key = apikey)
    twelve_candle(input$stock_symbol, input$stock_dateRange[1], input$stock_dateRange[2], input$stock_interval, api_key = twelve_apikey)
  })

  output$currencyTicker <- renderSlickR({

  today <- format(Sys.Date(), "%Y-%m-%d")
  yesterday <- format(Sys.Date() - 1, "%Y-%m-%d")

  chosen_currencies <- c("eur", "gbp", "jpy", "aud", "cad", "chf", "cny", "inr", "rub")
  
  currency_data_list <- two_days_values("usd", today, yesterday)
  req(currency_data_list)
  
  all_cols <- names(currency_data_list)
  currency_cols <- if(all_cols[1] %in% c("date", "Date")) all_cols[-1] else all_cols
  
  currency_items <- c()
  
  for (col in currency_cols) {
    if (!(col %in% chosen_currencies)) {
      next 
    }
    yesterday_val <- as.numeric(currency_data_list[1, col])
    today_val <- as.numeric(currency_data_list[2, col])
    
    if (!is.na(yesterday_val) && !is.na(today_val) && yesterday_val != 0) {
      change <- ((today_val / yesterday_val) - 1) * 100
      color <- ifelse(change > 0, "#28a745", "#dc3545")
      color <- ifelse(change == 0, "#666", color)
      arrow <- ifelse(change > 0, "▲", "▼")
      arrow <- ifelse(change == 0, "-", arrow)
      
      html_item <- sprintf(
        '<div style="text-align: center; padding: 0 20px; font-family: Arial, sans-serif;">
          <div style="font-weight: bold; font-size: 16px;">%s/USD</div>
          <div style="color: %s; font-size: 14px;">%s %.2f%%</div>
          <div style="color: #666; font-size: 18px; font-weight: 500;">%.4f</div>
        </div>',
        toupper(col), color, arrow, abs(change), today_val
      )
      tmp_dir <- '/tmp'
      html_file <- file.path(tmp_dir, paste0("currency_", col, ".html"))
      img_file  <- file.path(tmp_dir, paste0("currency_", col, ".png"))
      htmltools::save_html(HTML(html_item), html_file)
      webshot2::webshot(html_file, img_file, vwidth = 250, vheight = 100)
      
      currency_items <- c(currency_items, img_file)
    }
  }
  
  if (length(currency_items) == 0) {
    return(HTML('<div style="text-align: center; padding: 20px; color: #666;">No currency data available.</div>'))
  }
  
  slickR(
    obj = currency_items,
    slideId = "currencySlider",
    height = 80,
    width = "100%",
    objLinks = "character"
  ) + 
  settings(
    dots = FALSE,
    infinite = TRUE,
    slidesToShow = 6,
    slidesToScroll = 1,
    autoplay = TRUE,
    autoplaySpeed = 3000,
    arrows = FALSE,
    pauseOnHover = TRUE,
    responsive = JS(
      "
      function(slick) {
        var updateSlidesToShow = function() {
          var width = window.innerWidth;
          var slidesToShow = 6;
          if (width < 576) {
        slidesToShow = 2;
          } else if (width < 768) {
        slidesToShow = 3;
          } else if (width < 992) {
        slidesToShow = 4;
          } else if (width < 1200) {
        slidesToShow = 5;
          }
          slick.slickSetOption('slidesToShow', slidesToShow, true);
        };
        updateSlidesToShow();
        window.addEventListener('resize', function() {
          updateSlidesToShow();
        });
      }
      "
        )
  )
})
}