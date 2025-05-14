# Load necessary libraries
library(shiny)
library(httr)
library(jsonlite)
library(dplyr)



full_api <- function(date="latest", base_currency="usd") {
  url <- sprintf("https://cdn.jsdelivr.net/npm/@fawazahmed0/currency-api@%s/v1/currencies/%s.json", date, base_currency)
  response <- GET(url)

  if (status_code(response) == 200) {
    data <- content(response, "text", encoding = "UTF-8")
    data <- fromJSON(data)
  }
  else {
    return(NULL)
  }

  df = data.frame(t(unlist(data[[base_currency]])))
  df = df[, -1]
  
  tmp_df = data.frame(date = data$date)
  return(cbind(tmp_df, df))
}


from_to_values <- function(base_currency = "usd", start_date = "2024-03-01", end_date = "2025-05-13") {
  start_obj <- as.Date(start_date, format="%y-%m-%d")
  end_obj <- as.Date(end_date, format="%y-%m-%d")
  
  # First day of data
  data <- full_api(as.character(start_date), base_currency)
  data$date <- as.character(start_obj)
  dates <- seq(as.Date(start_date), as.Date(end_date), by=1)
  print(dates)
  
  # Loop over remaining dates
  
  for (i in seq_along(dates)) {
    date <- dates[i]
    row <- full_api(date, base_currency)
    
    if (!is.null(row)){
      data <- bind_rows(data, row)
    }
  }
  
  return(data)
}
options(max.print = 1000000)


safe_cbind <- function(df1, df2) {
  if (!is.data.frame(df2)) {
    return(NULL)
  }
  new_cols <- setdiff(names(df2), names(df1))
  if (length(new_cols) > 0) {
    df2_filtered <- df2[, new_cols, drop = FALSE]
    return(cbind(df1, df2_filtered))
  }
  return(df1)
}



short_to_currency <- function(date = "2025-01-01") {
  url <- sprintf("https://cdn.jsdelivr.net/npm/@fawazahmed0/currency-api@%s/v1/currencies.json", date)
  
  response <- GET(url)
  if (status_code(response) == 200) {
    data <- content(response, "text", encoding = "UTF-8")
    data <- fromJSON(data)
  
  df = data.frame(t(unlist(data)))
  df = df[, -1]

  return(df)
  }
  else {
    return(NULL)
  }
}

currency_finder <- function(){
  dates <- seq(as.Date("2024-03-05"), as.Date("2025-05-13"), by=30)
  print(dates)
  
  # Loop over remaining dates
  data <- short_to_currency(dates[1])
  for (i in seq_along(dates)) {
    date <- dates[i]
    row <- short_to_currency(date)
    if (!is.null(row)){
      data <- safe_cbind(data, row)
    }
  }
  return(data)
}

colnames(short_to_currency("2024-03-06"))

currency_transformer <- currency_finder()
currency_transformer

rate_values <- from_to_values()
rate_values
