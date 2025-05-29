library(plotly)
library(webshot2)
library(promises)
library(future)
plan(multisession)
library(htmlwidgets)
library(ggplot2)
library(slickR)
library(htmltools)
library(shinyBS)

source("api.r")
long_curr_names <- short_to_currency()

apikey = plotlyapikey = '99RHTNDE9YD9TMOW'
twelve_apikey = '6abb374eb01c45979b59e87327fed240'

function(input, output, session) {

  ticker_base_currency <- reactiveVal("usd")
  ticker_display_currencies <- reactiveVal(c("eur", "gbp", "jpy", "aud", "cad", "chf", "cny", "inr"))

  observeEvent(input$saveSettings, {
    display_currencies <- display_currencies[display_currencies != base_currency]
    
    if (length(display_currencies) == 0) {
      showNotification("Please select at least one currency to display!", type = "warning", duration = 5)
      return()
    }
    
    ticker_base_currency(base_currency)
    ticker_display_currencies(display_currencies)
    toggleModal(session, "settingsModal", toggle = "close")
    showNotification(paste("Ticker settings updated! Showing", length(display_currencies), "currencies against", toupper(base_currency)), type = "message", duration = 3)
  })

  observeEvent(input$resetDefaults, {
    updateSelectizeInput(session, "ticker_base_currency", selected = "usd")
    updateSelectizeInput(session, "ticker_display_currencies", selected = c("eur", "gbp", "jpy", "aud", "cad", "chf", "cny", "inr"))
  })

  observeEvent(input$tickerSettings, {
    updateSelectizeInput(session, "ticker_base_currency", selected = ticker_base_currency())
    updateSelectizeInput(session, "ticker_display_currencies", selected = ticker_display_currencies())
  })
  
  output$base_currency_output <- renderPrint({
    req(input$base_currency)
    paste("Selected base currency: ", long_curr_names[[input$base_currency]], " (", input$base_currency, ")")
  })
  
  output$currency_output <- renderPrint({
    req(input$currency)
    paste0("Selected target currency: ", long_curr_names[[input$currency]], " (", input$currency, ")")
  })
  
  output$currency_date <- renderPrint({
    req(input$dateRange)
    start_date <- input$dateRange[1]
    end_date <- input$dateRange[2]
    paste("Date range:", start_date, "to", end_date)
  })

  output$currency_plot <- renderPlotly({
    req(input$base_currency, input$currency, input$dateRange)
    
    base_currency <- input$base_currency
    chosen_currency <- input$currency  # This was the key issue - using wrong variable name
    start_date <- input$dateRange[1]
    end_date <- input$dateRange[2]
    
    data <- from_to_values(base_currency, start_date, end_date)
    
    req(data, chosen_currency %in% colnames(data))

    currency_data <- data.frame(
      date = as.Date(data[[1]]),
      value = as.numeric(data[[chosen_currency]])
    )
    
    req(nrow(currency_data) > 0)
    

    print(paste("Data rows:", nrow(currency_data)))
    print(head(currency_data))
    
    p <- ggplot(currency_data, aes(x = date, y = value)) +
      geom_line(color = "blue", linewidth = 1) +
      labs(
        title = paste0("Exchange Rate: ", long_curr_names[[base_currency]],
                      " (", base_currency, ")", " to ", long_curr_names[[chosen_currency]],
                      " (", chosen_currency, ")"), 
        subtitle = paste("From", start_date, "to", end_date),
        y = "Value", 
        x = "Date"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })

  output$base_currency_bnb_output <- renderPrint({
    req(input$base_currency_bnb)
    paste0("Selected base currency: ", long_curr_names[[input$base_currency_bnb]], " (", input$base_currency_bnb, ")")
  })

  output$currencies_bnb_output <- renderPrint({
    req(input$currencies_bnb)
    if (length(input$currencies_bnb) > 0) {
      paste0("Selected target currencies: ", paste(input$currencies_bnb, collapse = ", "))
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
    values <- c()
    for (i in 2:length(filtered_data)) {
      values <- c(values, ((filtered_data[2, i] / filtered_data[1, i]) - 1) * 100)
    }

    colnames <- colnames(filtered_data)[-1]
    result <- data.frame(
        Currency = colnames,
        PercentChange = round(values, 2)
    )
    currency_data <- result
    currency_data$Color <- ifelse(currency_data$PercentChange >= 0, "Increase", "Decrease")

    currency_data <- currency_data[order(currency_data$PercentChange), ]

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

      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.5) +
      coord_flip()

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
    paste("Calculated amount:", round(calculated, 2), long_curr_names[[input$to_currency]])

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

    base_currency <- ticker_base_currency()
    chosen_currencies <- ticker_display_currencies()
    
    currency_data_list <- two_days_values(base_currency, today, yesterday)
    req(currency_data_list)
    
    all_cols <- names(currency_data_list)
    currency_cols <- if(all_cols[1] %in% c("date", "Date")) all_cols[-1] else all_cols
    
    currency_cols <- currency_cols[currency_cols %in% chosen_currencies & currency_cols != base_currency]
    
    currency_items <- c()

    process_currency <- function(col, currency_data_list, chosen_currencies, base_currency, tmp_dir = '/tmp') {
      yesterday_val <- as.numeric(currency_data_list[1, col])
      today_val <- as.numeric(currency_data_list[2, col])
      
      if (!is.na(yesterday_val) && !is.na(today_val) && yesterday_val != 0) {
        change <- ((today_val / yesterday_val) - 1) * 100
        color <- ifelse(change > 0, "#28a745", "#dc3545")
        color <- ifelse(change == 0, "#666", color)
        arrow <- ifelse(change > 0, "▲", "▼")
        arrow <- ifelse(change == 0, "-", arrow)
        
        html_item <- sprintf(
          '<div style="background-color: #101111; color: #dcdddd; text-align: center; font-family: Arial, sans-serif; padding: 6px; margin: 0;">
          <style>body { margin: 0; padding: 0; background-color: #101111; } html { margin: 0; padding: 0; background-color: #101111; }</style>
          <div style="font-weight: bold; font-size: 16px;">%s/%s</div>
          <div style="color: %s; font-size: 14px;">%s %.2f%%</div>
          <div style="color: #dcdddd; font-size: 18px; font-weight: 500;">%.4f</div>
          </div>',
          toupper(col), toupper(base_currency), color, arrow, abs(change), today_val
        )
        
        html_file <- file.path(tmp_dir, paste0("currency_", col, ".html"))
        img_file <- file.path(tmp_dir, paste0("currency_", col, ".png"))
        
        htmltools::save_html(HTML(html_item), html_file)
        webshot2::webshot(
          html_file,
          img_file,
          vwidth = 200,
          vheight = 200,
          zoom = 2,
          delay = 0.1
        )
        
        return(img_file)
      }
      return(NULL)
    }

    batch_size <- 3
    batches <- split(currency_cols, ceiling(seq_along(currency_cols) / batch_size))

    plan(multisession, workers = min(length(batches), availableCores() - 1))

    # Run in parallel - now currency_cols is already filtered
    currency_items <- future_map(currency_cols, process_currency,
                                currency_data_list = currency_data_list,
                                chosen_currencies = chosen_currencies,
                                base_currency = base_currency,
                                .options = furrr_options(seed = TRUE))

    currency_items <- unlist(currency_items[!sapply(currency_items, is.null)])
    plan(sequential)

    
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
  
  output$stock_comparison_plot = renderPlotly({
    req(input$stock_symbols, input$stocks_comparison_dateRange, input$stock_comparison_interval)
    
    twelve_compare(input$stock_symbols, input$stocks_comparison_dateRange[1], input$stocks_comparison_dateRange[2], input$stock_comparison_interval, api_key = twelve_apikey)
  })
}