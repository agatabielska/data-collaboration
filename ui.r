library(shiny)
library(shinydashboard)
library(shinythemes)
library(fresh)
library(plotly)
library(ggplot2)
library(dplyr)
library(httr)
library(slickR)
library(shinyBS)
source("api.r")

currency_list <- colnames(currency_finder())


theme = create_theme(adminlte_color(light_blue = "#006288"))

dashboardPage(
  skin = "black",
  dashboardHeader(
    title = tagList(
      span("Multi-Page Dashboard")
    )
  ),
  dashboardSidebar(
    width = 240,
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Currency",
        tabName = "currency_main",
        icon = icon("dollar-sign"),
        menuSubItem("Exchange Rates", tabName = "currency", icon = icon("chart-line")),
        menuSubItem("Bull and Bear Movements", tabName = "currency_comparison", icon = icon("exchange-alt")),
        menuSubItem("Currency Converter", tabName = "currency_converter", icon = icon("calculator"))
      ),
      menuItem("Stocks", 
        tabName = "stocks_main", 
        icon = icon("chart-line"),
        menuSubItem("Stock Overview", tabName = "stocks", icon = icon("chart-line")),
        menuSubItem("Stock Comparison", tabName = "stocks_comparison", icon = icon("balance-scale"))
      )
    ),
    conditionalPanel(
      "input.tabs == 'currency'",
      selectizeInput(
        inputId = "base_currency",
        label   = "Choose base currency:",
        choices = currency_list,
        selected = "usd",
        options = list(placeholder = "Search…")
      ),
      selectizeInput(
        inputId = "currency",
        label   = "Choose target currency:",
        choices = currency_list,
        selected = "eur",
        options = list(placeholder = "Search…")
      ),
      dateRangeInput(
        inputId = "dateRange",
        label   = "Select date range:",
        start   = Sys.Date() - 30,
        end     = Sys.Date(),
        min     = "2024-03-01",
        max     = Sys.Date(),
        format  = "yyyy-mm-dd",
        separator = " to "
      )
    ),
    conditionalPanel(
      "input.tabs == 'currency_comparison'",
      selectizeInput(
        inputId = "base_currency_bnb",
        label   = "Choose base currency:",
        choices = currency_list,
        selected = "usd",
        options = list(placeholder = "Search…")
      ),
      selectizeInput(
        inputId = "currencies_bnb",
        label   = "Choose target currencies:",
        choices = currency_list,
        selected = "eur",
        multiple = TRUE,
        options = list(placeholder = "Search…")
      ),
      dateRangeInput(
        inputId = "dateRange_bnb",
        label   = "Select date range:",
        start   = Sys.Date() - 30,
        end     = Sys.Date(),
        min     = "2024-03-01",
        max     = Sys.Date(),
        format  = "yyyy-mm-dd",
        separator = " to "
      )
    ),
    conditionalPanel(
      "input.tabs == 'currency_converter'",
      selectizeInput(
        inputId = "from_currency",
        label   = "From currency:",
        choices = currency_list,
        selected = "usd",
        options = list(placeholder = "Search…")
      ),
      selectizeInput(
        inputId = "to_currency",
        label   = "To currency:",
        choices = currency_list,
        selected = "eur",
        options = list(placeholder = "Search…")
      ),
      numericInput(
        inputId = "amount_to_convert",
        label   = "Amount:",
        value   = 1,
        min     = 0
      ),
      dateInput(
        inputId = "dateInput_converter",
        label   = "Select date:",
        min     = "2024-03-01",
        max     = Sys.Date(),
        format  = "yyyy-mm-dd"
      )
    ),
    conditionalPanel(
      "input.tabs == 'stocks'",
      selectizeInput(
        inputId = "stock_symbol",
        label   = "Stock symbol:",
        choices = vantage_symbols(),
        selected = "AAPL",
        options = list(placeholder = "Search…")
      ),
      dateRangeInput(
        inputId = "stock_dateRange",
        label   = "Select date range:",
        start   = Sys.Date() - 30,
        end     = Sys.Date(),
        format  = "yyyy-mm-dd",
        separator = " to "
      ),
      selectizeInput(
        inputId = "stock_interval",
        label   = "Choose interval:",
        choices = c('1min', '5min', '15min', '30min', '45min', '1h', '2h', '4h', '1day', '1week', '1month'),
        selected = "1day",
        options = list(placeholder = "Search...")
      )
    ),
    conditionalPanel(
      "input.tabs == 'stocks_comparison'",
      selectizeInput(
        inputId = "stock_symbols",
        label   = "Stock Symbols (comma-separated):",
        choices = vantage_symbols(),
        multiple = TRUE,
        options = list(placeholder = "Search...")
      ),
      dateRangeInput(
        inputId = "stocks_comparison_dateRange",
        label   = "Select date range:",
        start   = Sys.Date() - 30,
        end     = Sys.Date(),
        format  = "yyyy-mm-dd",
        separator = " to "
      ),
      selectizeInput(
        inputId = "stock_comparison_interval",
        label   = "Choose interval:",
        choices = c('1min', '5min', '15min', '30min', '45min', '1h', '2h', '4h', '1day', '1week', '1month'),
        selected = "1day",
        options = list(placeholder = "Search...")
      )
    )
  ),
  
  dashboardBody(

    tags$head(
      includeCSS("styles.css")
    ),
    tags$style(
      HTML(
        ".content-wrapper, .right-side { background-color: #101111; }"
      )
    ),
    
    tabItems(
      tabItem(
        tabName = "currency",
        class = "currency-tab",
        fluidRow(
          box(
            title = "Currency Exchange Information",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            collapsed = FALSE
          )
        ),
        fluidRow(
          box(
            width = 4,
            status = "info",
            solidHeader = TRUE,
            verbatimTextOutput("base_currency_output")
          ),
          box(
            width = 4,
            status = "info",
            solidHeader = TRUE,
            verbatimTextOutput("currency_output")
          ),
          box(
            width = 4,
            status = "info",
            solidHeader = TRUE,
            verbatimTextOutput("currency_date")
          )
        ),
        fluidRow(
          box(
            title = "Currency Exchange Rate Plot",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("currency_plot", height = "400px")
          )
        )
      ),
      tabItem(
        tabName = "currency_comparison",
        fluidRow(
          box(
            title = "Currency Bull and Bear Movements",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            collapsed = FALSE
          )
        ),
        fluidRow(
          box(
            width = 4,
            status = "info",
            solidHeader = TRUE,
            verbatimTextOutput("base_currency_bnb_output")
          ),
          box(
            width = 4,
            status = "info",
            solidHeader = TRUE,
            verbatimTextOutput("date_range_bnb_output")
          ),
          box(
            width = 4,
            status = "info",
            solidHeader = TRUE,
            verbatimTextOutput("currencies_bnb_output")
          )
        ),
        fluidRow(
          box(
            title = "Currency Performance Comparison",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("bull_bear_plot", height = "400px")
          )
        )
      ),
      tabItem(
        tabName = "currency_converter",
        fluidRow(
          box(
            title = "Currency Converter",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            collapsed = FALSE,
            verbatimTextOutput("currency_converter_output")
          )
        )
      ),
      tabItem(
        tabName = "stocks",
        fluidRow(
          box(
            title = "Stock Dashboard",
            status = "primary",
            solidHeader = TRUE,
            width = 12
          )
        ), 
        fluidRow(
          box(
            width = 4,
            status = "info",
            solidHeader = TRUE,
            verbatimTextOutput("stock_symbol_output")
          ),
          box(
            width = 4,
            status = "info",
            solidHeader = TRUE,
            verbatimTextOutput("stock_date_output")
          ),
          box(
            width = 4,
            status = "info",
            solidHeader = TRUE,
            verbatimTextOutput("stock_interval_output")
          )
        ), 
        fluidRow(
          box(
            title = "Stock Price Plot",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("stock_plot", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = "Top Gainers",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            uiOutput("top_gainers_stocks_ui")
          )
        )
      ),
      tabItem(
        tabName = "stocks_comparison",
        fluidRow(
          box(
            title = "Stock Comparison",
            status = "primary",
            solidHeader = TRUE,
            width = 12
          )
        ),
        fluidRow(
          box(
            width = 12,
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("stock_comparison_plot", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = "Top Gainers",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            uiOutput("top_gainers_comparison_ui")
          )
        )
      ),
      tabItem(
        tabName = "about_page_content",
        fluidRow(
          box(
            title = "About This Dashboard",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            tags$div(
              style = "padding: 20px; color: #e0e0e0; background-color: #2c2c2c; border-radius: 5px;",
              tags$h4("Made by:"),
              tags$p("Agata Bielska 160313"),
              tags$p("Paweł Charkiewicz 160288"),
              tags$hr(),
              tags$h4("Data sources:"),
              tags$ul(
                tags$li("fawazahmed0's currency API"),
                tags$li("TwelveData stock API"),
                tags$li("AlphaVantage stock API")
              ),
              tags$hr(),
              tags$h4("Remarks:"),
              tags$p("Dashboard rendering may be computationally demanding at times. Please be patient."),
              tags$p("TwelveData API only handles eight tokens per minute. Please try to not overload the API with too many subsequent changes in stock charts. This also applies to comparison chart, as every symbol consumes one token.")
            )
          )
        )
      )
    ),
    
    tags$div(
      style = "position: fixed; bottom: 90px; right: 20px; z-index: 1000; background: #101111; border-radius: 5px; padding: 10px; box-shadow: 0 2px 5px rgba(0,0,0,0.3);",
      class = "ticker-settings",
      actionButton("tickerSettings", "⚙️ Settings", class = "settings-btn")
    ),
    
    tags$div(
      style = "position: fixed; bottom: 0; left: 0; right: 0; z-index: 999 ; background: #101111; border-top: 1px solid #101111; padding: 0px 0;",
      slickROutput("currencyTicker", height = "60px")
    ),
    
    # Settings Modal
    bsModal(
      id = "settingsModal",
      title = "Currency Ticker Settings",
      trigger = "tickerSettings",
      size = "large",
      fluidRow(
        column(6,
          selectizeInput(
            inputId = "ticker_base_currency",
            label = "Choose base currency for ticker:",
            choices = currency_list,
            selected = "usd",
            options = list(placeholder = "Search…")
          )
        ),
        column(6,
          selectizeInput(
            inputId = "ticker_display_currencies",
            label = "Choose currencies to display:",
            choices = currency_list,
            selected = c("eur", "gbp", "jpy", "aud", "cad", "chf", "cny", "inr"),
            multiple = TRUE,
            options = list(
              placeholder = "Search and select currencies…",
              maxItems = 50
            )
          )
        )
      ),
      fluidRow(
        column(12,
          tags$div(
            style = "margin-top: 15px; padding: 10px; background-color: #101111; border-radius: 5px; border-left: 4px solid #25be76 ;",
            tags$small(
              style = "color: #dcdddd;",
              HTML("<strong>Tip:</strong> Select up to 50 currencies to display in the ticker. Default selection includes 8 major currencies. <br>You can reset to defaults at any time. Note that this can take a while to load if many currencies are selected.")
            )
          )
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("resetDefaults", "Reset to Defaults", class = "btn btn-secondary"),
        actionButton("saveSettings", "Save Settings", class = "btn btn-primary")
      )
    ),
    
    tags$script(HTML(
      '$(document).ready(function() {\n        var newMenuItemHTML = \'<a href="#shiny-tab-about_page_content" data-toggle="tab" data-value="about_page_content" style="color: white; display: flex; align-items: center; padding: 15px;"><i class="fa fa-info-circle" style="margin-right: 5px;"></i> <span>About</span></a>\';\n        var $newlyAddedLink = $(newMenuItemHTML);\n        $("nav").append($newlyAddedLink);\n\n        $newlyAddedLink.on(\'click\', function(e) {\n          e.preventDefault(); \n          if (typeof Shiny !== \'undefined\' && Shiny.setInputValue) {\n            Shiny.setInputValue("tabs", "about_page_content", { priority: "event" });\n          }\n        });\n      });'
    ))
  )
)