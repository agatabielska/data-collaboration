library(shiny)
library(shinythemes)
library(plotly)
library(ggplot2)
library(dplyr)
library(httr)
source("api.R")

# Get list of available currencies
currency_list <- colnames(currency_finder())

dashboardPage(
  skin = "blue",  # top header bar color
  dashboardHeader(title = "Multi-Page Dashboard"),
  
  dashboardSidebar(
    width = 240,
    sidebarMenu(id = "tabs",
                menuItem("Currency", tabName = "currency", icon = icon("dollar-sign")),
                menuItem("Stocks",   tabName = "stocks",   icon = icon("chart-line"))
    ),
    # Inputs for Currency tab
    conditionalPanel(
      "input.tabs == 'currency'", 
      hr(),
      selectizeInput(
        inputId = "base_currency",
        label   = "Choose base currency:",
        choices = currency_list,
        selected = "USD",
        options = list(placeholder = "Search…")
      ),
      selectizeInput(
        inputId = "currency",
        label   = "Choose target currency:",
        choices = currency_list,
        selected = "EUR",
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
    # Inputs for Stocks tab
    conditionalPanel(
      "input.tabs == 'stocks'", 
      hr(),
      textInput(
        inputId = "stock_symbol",
        label   = "Stock Symbol:",
        value   = "AAPL"
      ),
      dateRangeInput(
        inputId = "stock_dateRange",
        label   = "Select date range:",
        start   = Sys.Date() - 30,
        end     = Sys.Date(),
        format  = "yyyy-mm-dd",
        separator = " to "
      )
    )
  ),
  
  dashboardBody(
    # set light grey background
    tags$style(HTML(
      ".content-wrapper, .right-side { background-color: #f5f7fa; }"
    )),
    
    tabItems(
      # Currency Tab Content
      tabItem(
        tabName = "currency",
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
            width = 4, status = "info", solidHeader = TRUE,
            verbatimTextOutput("base_currency_output")
          ),
          box(
            width = 4, status = "info", solidHeader = TRUE,
            verbatimTextOutput("currency_output")
          ),
          box(
            width = 4, status = "info", solidHeader = TRUE,
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
      
      # Stocks Tab Content
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
            width = 6, status = "info", solidHeader = TRUE,
            verbatimTextOutput("stock_symbol_output")
          ),
          box(
            width = 6, status = "info", solidHeader = TRUE,
            verbatimTextOutput("stock_date_output")
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
      )
    )
  )
)