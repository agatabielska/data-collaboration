library(shiny)
library(shinydashboard)
library(shinythemes)
library(fresh)
library(plotly)
library(ggplot2)
library(dplyr)
library(httr)
library(slickR)
source("api.r")

currency_list <- colnames(currency_finder())

theme = create_theme(adminlte_color(light_blue = "#006288"))

dashboardPage(
  skin = "black",
  dashboardHeader(title = tagList(
    span("Multi-Page Dashboard")
  )),
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
        )
      )
    ),
    tags$head(
      tags$style(HTML("
        #currencyTicker, 
        #currencyTicker .slick-track, 
        #currencyTicker .slick-slide {
          background-color: #101111 !important;
        }
      "))
    ),
    tags$div(
      style = "position: fixed; bottom: 0; left: 0; right: 0; z-index: 1000; background: #101111; border-top: 1px solid #101111; padding: 0px 0;",
      slickROutput("currencyTicker", height = "60px")
    )
  )
)