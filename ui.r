library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)
library(httr)
source("api.r")

currency_list <- colnames(currency_finder())

dashboardPage(
  dashboardHeader(title = "Currency Dashboard"),
  dashboardSidebar(
    selectizeInput(
      "base_currency", 
      label = "Choose base currency:",  # Updated label for clarity
      choices = currency_list,
      selected = character(0),
      options = list(
        placeholder = "Search"
      )
    ),
    selectizeInput(
      "currency",
      label = "Choose target currency:",  # Updated label for clarity
      choices = currency_list,
      selected = NULL,  # Changed default to EUR to see exchange rate difference
      options = list(
        placeholder = "Search"
      )
    ),
    dateRangeInput(
      inputId = "dateRange",
      label = "Select date range:",
      start = Sys.Date() - 30,
      end = Sys.Date(),
      min = "2024-03-01",
      max = Sys.Date(),
      format = "yyyy-mm-dd",
      separator = " to "
    )
  ),
  dashboardBody(
    h2("Currency Exchange Information"),
    fluidRow(
      column(width = 4,
        verbatimTextOutput("base_currency_output")
      ),
      column(width = 4,
        verbatimTextOutput("currency_output")
      ),
      column(width = 4,
        verbatimTextOutput("currency_date")
      )
    ),
    h2("Currency Exchange Rate Plot"),
    plotlyOutput("currency_plot", height = "400px")
  )
)