library(plotly)
library(webshot2)
library(promises)
library(htmlwidgets)
library(ggplot2)
library(slickR)
library(htmltools)
library(shinyBS)
library(DT) # Added DT library

source("api.r")
long_curr_names <- short_to_currency()

apikey = plotlyapikey = '99RHTNDE9YD9TMOW'
twelve_apikey = '6abb374eb01c45979b59e87327fed240'

function(input, output, session) {

  ticker_base_currency <- reactiveVal("usd")
  ticker_display_currencies <- reactiveVal(c("eur", "gbp", "jpy", "aud", "cad", "chf", "cny", "inr"))

  observeEvent(input$saveSettings, {
    base_currency <- input$ticker_base_currency
    display_currencies <- input$ticker_display_currencies
    display_currencies <- display_currencies[display_currencies != base_currency]
    
    if (length(display_currencies) <= 7) {
      showNotification("Please select more than 7 currencies to display!", type = "warning", duration = 5)
      return()
    }

    ticker_base_currency(base_currency)
    ticker_display_currencies(display_currencies)

    toggleModal(session, "settingsModal", toggle = "close")
    showNotification(
      paste("Ticker settings updated! Showing", length(display_currencies), "currencies against", toupper(base_currency)),
      type = "message", duration = 3
    )
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
    chosen_currency <- input$currency
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
      geom_line(color = "#25be76", linewidth = 1) +
      labs(
        title = paste0("Exchange Rate: ", long_curr_names[[base_currency]],
                      " (", base_currency, ")", " to ", long_curr_names[[chosen_currency]],
                      " (", chosen_currency, ")"), 
        subtitle = paste("From", start_date, "to", end_date),
        y = "Value", 
        x = "Date"
      ) +      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#2c2c2c", color = NA),
        panel.background = element_rect(fill = "#2c2c2c", color = NA),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        panel.grid = element_line(color = "#454545")
      )
    
    ggplotly(p) %>%
      layout(
        plot_bgcolor = '#2c2c2c',
        paper_bgcolor = '#2c2c2c',
        font = list(color = 'white')
      )
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
                              fill = Color)) +      geom_bar(stat = "identity", width = 0.7) +
      geom_text(aes(label = sprintf("%.2f%%", PercentChange)), 
                vjust = ifelse(currency_data$PercentChange >= 0, -0.5, 1.5),
                color = "white", size = 3.5) +
      scale_fill_manual(values = c("Decrease" = "#FF6B6B", "Increase" = "#4CAF50")) +
      labs(title = "Currency % Change against Base Currency",
          x = "Currency",
          y = "Percentage Change (%)") +      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#2c2c2c", color = NA),
        panel.background = element_rect(fill = "#2c2c2c", color = NA),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.text.x = element_text(color = "white"),
        axis.text.y = element_text(color = "white")
      ) +geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.5) +
      coord_flip()

    ggplotly(p) %>%
      layout(
        hoverlabel = list(bgcolor = "white"),
        title = list(
          text = "Currency % Change against Base Currency",
          font = list(size = 16, color = "white")
        ),
        xaxis = list(title = list(text = "Currency", font = list(size = 14, color = "white")), color = "white"),
        yaxis = list(title = list(text = "", font = list(size = 14, color = "white")), color = "white"),
        plot_bgcolor = '#2c2c2c',
        paper_bgcolor = '#2c2c2c',
        font = list(color = 'white')
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
    
    p <- twelve_candle(input$stock_symbol, input$stock_dateRange[1], input$stock_dateRange[2], input$stock_interval, api_key = twelve_apikey)
    
    if (!is.null(p)) {
      p <- p %>%
        layout(template = "plotly_dark",
               paper_bgcolor = "#2c2c2c",
               plot_bgcolor = "#2c2c2c",
               font = list(color = 'white'))
    }
    p
  })


output$currencyTicker <- renderUI({
  req(ticker_base_currency(), ticker_display_currencies())
  
  base_currency <- ticker_base_currency()
  chosen_currencies <- ticker_display_currencies()

  today <- format(Sys.Date(), "%Y-%m-%d")
  yesterday <- format(Sys.Date() - 1, "%Y-%m-%d")


  currency_data_list <- two_days_values(base_currency, today, yesterday)
  req(currency_data_list)

  all_cols <- names(currency_data_list)
  currency_cols <- if (all_cols[1] %in% c("date", "Date")) all_cols[-1] else all_cols
  currency_cols <- currency_cols[currency_cols %in% chosen_currencies & currency_cols != base_currency]

  if (length(currency_cols) == 0) {
    return(div(style = "text-align: center; padding: 20px; color: #666;", "No currency data available."))
  }

  currency_cards <- lapply(currency_cols, function(col) {
    yesterday_val <- as.numeric(currency_data_list[1, col])
    today_val <- as.numeric(currency_data_list[2, col])

    if (!is.na(yesterday_val) && !is.na(today_val) && yesterday_val != 0) {
      change <- ((today_val / yesterday_val) - 1) * 100
      color <- ifelse(change > 0, "#28a745", ifelse(change < 0, "#dc3545", "#666"))
      arrow <- ifelse(change > 0, "▲", ifelse(change < 0, "▼", "-"))

      div(
        class = "currency-card",
        style = paste0(
          "display: inline-block; ",
          "background-color: #101111; ",
          "color: #dcdddd; ",
          "text-align: center; ",
          "font-family: Arial, sans-serif; ",
          "padding: 10px 15px; ",
          "margin: 5px; ",
          "border-radius: 8px; ",
          "border: 1px solid #333; ",
          "min-width: 120px; ",
          "box-shadow: 0 2px 4px rgba(0,0,0,0.3);"
        ),
        div(style = "font-weight: bold; font-size: 14px; margin-bottom: 5px;",
            paste0(toupper(col), "/", toupper(base_currency))),
        div(style = paste0("color: ", color, "; font-size: 12px; margin-bottom: 3px;"),
            paste(arrow, sprintf("%.2f%%", abs(change)))),
        div(style = "color: #dcdddd; font-size: 16px; font-weight: 500;",
            sprintf("%.4f", 1/today_val))
      )
    } else {
      NULL
    }
  })

  currency_cards <- currency_cards[!sapply(currency_cards, is.null)]
  duration_seconds <- max(28, length(currency_cards) * 4)

  if (length(currency_cards) == 0) {
    return(div(style = "text-align: center; padding: 20px; color: #666;", "No valid currency data."))
  }

  div(
    id = "currency-ticker-container",
    style = paste0(
      "position: fixed; ",
      "bottom: 0; ",
      "left: 0; ",
      "right: 0; ",
      "z-index: 999; ",
      "background: #101111; ",
      "border-top: 1px solid #333; ",
      "padding: 10px 0; ",
      "overflow: hidden; "
    ),
    tags$style(HTML(sprintf("
      #currency-ticker-container {
        overflow: hidden;
        white-space: nowrap;
      }

      .ticker-track {
        display: inline-block;
        white-space: nowrap;
        animation: tickerMove %ds linear infinite;
      }

      .ticker-track:hover {
        animation-play-state: paused;
      }

      .currency-card {
        display: inline-block;
        margin-right: 20px;
      }

      @keyframes tickerMove {
        0%% { transform: translateX(0%%); }
        100%% { transform: translateX(-50%%); }
      }
    ", duration_seconds))),
    div(
      class = "ticker-track",
      currency_cards,
      currency_cards,
      currency_cards, 
      currency_cards
    )
  )
})

  output$stock_comparison_plot = renderPlotly({
    req(input$stock_symbols, input$stocks_comparison_dateRange, input$stock_comparison_interval)
    
    p <- twelve_compare(input$stock_symbols, input$stocks_comparison_dateRange[1], input$stocks_comparison_dateRange[2], input$stock_comparison_interval, api_key = twelve_apikey)
    
    if (!is.null(p)) {
      p <- p %>%
        layout(template = "plotly_dark",
               paper_bgcolor = "#2c2c2c",
               plot_bgcolor = "#2c2c2c",
               font = list(color = 'white'))
    }
    p
  })

  create_gainer_card_html <- function(gainer_data) {
    required_fields <- c("ticker", "price", "change_amount", "change_percentage", "volume")
    if (!all(required_fields %in% names(gainer_data))) {
      return(tags$div(class = "col-md-3", 
                      tags$div(class = "gainer-card error-card", 
                               style="padding: 10px; border: 1px solid red; margin-bottom: 10px; background-color: #3c3c3c; color: white;",
                               tags$p("Error: Missing data for this gainer."))))
    }
    
    change_percentage_numeric <- suppressWarnings(as.numeric(sub("%", "", gainer_data$change_percentage)))
    
    card_border_color <- if (is.na(change_percentage_numeric)) {
        "border-left: 5px solid #6c757d;" 
    } else if (change_percentage_numeric > 0) {
      "border-left: 5px solid #28a745;" 
    } else if (change_percentage_numeric < 0) {
      "border-left: 5px solid #dc3545;"
    } else {
      "border-left: 5px solid #6c757d;"
    }
    
    tags$div(class = "col-md-3", 
             style = "padding-bottom: 15px;", 
             tags$div(class = "gainer-card", 
                      style = paste0("background-color: #2c2c2c; color: #e0e0e0; border-radius: 5px; padding: 15px; height: 100%;", card_border_color),
                      tags$h5(style = "margin-top: 0; font-weight: bold; color: #ffffff;", gainer_data$ticker),
                      tags$p(style = "margin-bottom: 5px; font-size: 0.9em;", paste("Price:", gainer_data$price)),
                      tags$p(style = "margin-bottom: 5px; font-size: 0.9em;", paste("Change:", gainer_data$change_amount)),
                      {
                        display_change_percentage_str <- if (!is.na(change_percentage_numeric)) {
                          sprintf("%.2f%%", change_percentage_numeric)
                        } else {
                          gainer_data$change_percentage
                        }
                        tags$p(style = "margin-bottom: 5px; font-size: 0.9em;", paste("Change %:", display_change_percentage_str))
                      },
                      tags$p(style = "margin-bottom: 0; font-size: 0.9em;", paste("Volume:", gainer_data$volume))
             )
    )
  }


  output$top_gainers_stocks_ui <- renderUI({
    top_gainers_df <- vantage_top_gainers() 
    
    if (!is.null(top_gainers_df) && is.data.frame(top_gainers_df)) {
      
      if (nrow(top_gainers_df) > 0 && "change_percentage" %in% names(top_gainers_df)) {
        top_gainers_df$change_percentage_numeric <- suppressWarnings(as.numeric(sub("%", "", top_gainers_df$change_percentage)))
        top_gainers_df <- top_gainers_df[!is.na(top_gainers_df$change_percentage_numeric), ]
        
        if (nrow(top_gainers_df) > 0) {
            top_gainers_df <- top_gainers_df[order(-top_gainers_df$change_percentage_numeric), ]
            top_n_gainers <- head(top_gainers_df, 10)

            if(nrow(top_n_gainers) > 0) { # Ensure top_n_gainers is not empty before lapply
              gainer_cards <- lapply(seq_len(nrow(top_n_gainers)), function(i) {
                create_gainer_card_html(top_n_gainers[i, ])
              })
              return(fluidRow(gainer_cards))
            }
        }
      }
    }
    return(tags$div(style = "color: #e0e0e0; padding: 15px; text-align: center;", "No top gainers data available or data format issue."))
  })
  output$top_losers_comparison_ui <- renderUI({
    top_losers_df <- vantage_top_losers() 
    
    if (!is.null(top_losers_df) && is.data.frame(top_losers_df)) {
      
      if (nrow(top_losers_df) > 0 && "change_percentage" %in% names(top_losers_df)) {
        top_losers_df$change_percentage_numeric <- suppressWarnings(as.numeric(sub("%", "", top_losers_df$change_percentage)))
        top_losers_df <- top_losers_df[!is.na(top_losers_df$change_percentage_numeric), ]

        if (nrow(top_losers_df) > 0) {
            top_losers_df <- top_losers_df[order(top_losers_df$change_percentage_numeric), ]
            top_n_losers <- head(top_losers_df, 10)

            if(nrow(top_n_losers) > 0) {
              loser_cards <- lapply(seq_len(nrow(top_n_losers)), function(i) {
                create_gainer_card_html(top_n_losers[i, ])
              })
              return(fluidRow(loser_cards))
            }
        }
      }
    }
    return(tags$div(style = "color: #e0e0e0; padding: 15px; text-align: center;", "No top losers data available or data format issue."))
  })
  output$market_status_table <- renderDT({
    market_data <- vantage_market()
    
    if (!is.null(market_data) && is.data.frame(market_data) && nrow(market_data) > 0) {      dt <- datatable(market_data,
                rownames = FALSE,
                class = 'cell-border stripe hover compact',                options = list(
                  pageLength = 5,
                  scrollX = TRUE,
                  autoWidth = FALSE,
                  order = list(list(which(colnames(market_data) == "current_status") - 1, 'desc')),
                  columnDefs = list(list(className = 'dt-center', targets = "_all")),                  initComplete = JS(
                    "function(settings, json) {",
                    "  $(this.api().table().header()).css({'background-color': '#343a40', 'color': '#fff'});",
                    "  $(this.api().table().container()).css('background-color', '#2c2c2c');",
                    "  $('.dataTables_wrapper .dataTables_length select').css({'background-color': '#343a40', 'color': '#fff', 'border': '1px solid #454d55'});",
                    "  $('.dataTables_wrapper .dataTables_filter input').css({'background-color': '#343a40', 'color': '#fff', 'border': '1px solid #454d55'});",
                    "  $('.dataTables_wrapper .dataTables_info').css({'color': '#fff', 'font-weight': 'bold'});",
                    "  $('.dataTables_wrapper .dataTables_length label').css('color', '#fff');",
                    "  $('.dataTables_wrapper .dataTables_filter label').css('color', '#fff');",
                    "  $('.dataTables_wrapper .dataTables_paginate .paginate_button').css({'background-color': '#343a40', 'color': '#fff !important', 'border': '1px solid #454d55'});",
                    "  $('.dataTables_wrapper .dataTables_paginate .paginate_button.current').css({'background-color': '#007bff', 'color': '#fff !important'});",
                    "  $('.dataTables_wrapper .dataTables_paginate .paginate_button:hover').css({'background-color': '#495057', 'color': '#fff !important'});",
                    "  $('.dataTables_wrapper .dataTables_paginate .paginate_button.disabled').css({'background-color': '#2c2c2c', 'color': '#6c757d !important'});",
                    "  $('.dataTables_wrapper .dataTables_paginate').css('color', '#fff');",
                    "  $('.dataTables_wrapper .dataTables_paginate .ellipsis').css('color', '#fff');",
                    "}"
                  )
                )
      ) %>%
      formatStyle(columns = TRUE, color = 'white')
      
      if ("current_status" %in% colnames(market_data)) {
        dt <- dt %>% formatStyle(
          'current_status',
          color = styleEqual(c("open", "closed"), c('#28a745', '#dc3545'))
        )
      }
      return(dt)
    } else {
      datatable(data.frame(Message = "No market data available"),
                rownames = FALSE,
                options = list(dom = 't'))
    }
  })

  observeEvent(input$aboutButton, {
    updateTabItems(session, "sidebarmenu", selected = "about_page_content")
  })
}