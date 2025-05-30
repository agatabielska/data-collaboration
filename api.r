# Load necessary libraries
library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(future)
library(furrr)
library(plotly)
library(lubridate)

apikey = plotlyapikey = '99RHTNDE9YD9TMOW'
twelve_apikey = '6abb374eb01c45979b59e87327fed240'

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
  plan(multisession, workers = parallel::detectCores() - 1)
  
  dates <- seq(as.Date(start_date), as.Date(end_date), by=1)

  results <- future_map_dfr(dates, function(date) {
    row <- full_api(as.character(date), base_currency)
    if (!is.null(row)) {
      return(row)
    } else {
      return(NULL)
    }
  }, .progress = TRUE)
  
  plan(sequential)
  
  return(results)
}

two_days_values <- function(base_currency = "usd", start_date = "2024-03-01", end_date = "2025-05-13") {
  plan(multisession, workers = parallel::detectCores() - 1)
  
  row1 <- full_api(as.character(start_date), base_currency)
  row2 <- full_api(as.character(end_date), base_currency)
  print(dim(row1))
  print(dim(row2))
  if (!is.null(row1) && !is.null(row2)) {
    all_cols <- union(names(row1), names(row2))
    for (col in setdiff(all_cols, names(row1))) {
      row1[[col]] <- NA
    }
    for (col in setdiff(all_cols, names(row2))) {
      row2[[col]] <- NA
    }
    row1 <- row1[, all_cols, drop = FALSE]
    row2 <- row2[, all_cols, drop = FALSE]
    results <- bind_rows(row1, row2)
  } else {
    results <- NULL
  }
  plan(sequential)
  
  return(results)
}


vantage_query = function(command, arguments, api_key) {
  parsed_arguments = paste0("&", sapply(arguments, function(x) paste(x, collapse = "=")), collapse = '')
  url = paste0('https://www.alphavantage.co/query?function=', command, parsed_arguments, '&apikey=', api_key)
  res = GET(url)
  if(status_code(res) == 200){
    return(res)
  }
  else{
    return(NULL)
  }
}

vantage_top_gainers = function() {
  url = 'https://www.alphavantage.co/query?function=TOP_GAINERS_LOSERS&apikey=demo'
  res = GET(url)
  if (status_code(res) == 200) {
    content = fromJSON(content(res, "text", encoding = "UTF-8"))
    return(content$top_gainers)
  } else {
    return(NULL)
  }
}

vantage_market_status = function() {
  url = 'https://www.alphavantage.co/query?function=MARKET_STATUS&apikey=demo'
  res = GET(url)
  if (status_code(res) == 200) {
    content = fromJSON(content(res, "text", encoding = "UTF-8"))
    return(content$markets)
  } else {
    return(NULL)
  }
}

vantage_lytics = function(symbols, calculations, interval, from, to, api_key) {
  q = vantage_query(command = "ANALYTICS_FIXED_WINDOW", arguments = list(c("SYMBOLS", paste(as.vector(symbols), collapse = ',')), c("CALCULATIONS", paste(calculations, collapse = ',')), c("INTERVAL", interval), c("RANGE", from), c("RANGE", to)), api_key = api_key)
  return(q)
}

vantage_daily = function(symbol, full=FALSE, api_key){
  q = vantage_query("TIME_sERIES_DAILY", list(c("symbol", symbol), c("outputsize", if (full) "full" else "compact")), api_key)
  return(q)
}

vantage_weekly = function(symbol, api_key){
  q = vantage_query("TIME_sERIES_WEEKLY", list(c("symbol", symbol)), api_key)
  return(q)
}



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
  dates <- seq(as.Date("2025-05-07"), as.Date("2025-05-13"), by=3)
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

unpack = function(list){
  return(unlist(list, use.names = FALSE))
}

vantage_weekly_plot = function(symbol, from, to, api_key){
  res = vantage_weekly(symbol, api_key = apikey)
  if(is.null(res)){
    return(NULL)
  }
  data = fromJSON(content(res, "text", encoding = "UTF-8"))
  wts = data$`Weekly Time Series`
  wts = do.call(rbind, wts)
  wts = as.data.frame(wts)
  wts = data.frame(Date = as.Date(rownames(wts)), Open = unpack(wts$`1. open`), High = unpack(wts$`2. high`), Low = unpack(wts$`3. low`), Close = unpack(wts$`4. close`), Volume = unpack(wts$`5. volume`))
  wts = wts %>% filter(between(Date, as.Date(from), as.Date(to)))
  fig = wts %>% plot_ly(x = ~Date, type="candlestick", open = ~Open, close = ~Close, high = ~High, low = ~Low) 
  fig = fig %>% layout(title = paste("Candlestick chart of", symbol), xaxis=list(tickangle=45))
  return(fig)
}

vantage_daily_plot = function(symbol, full = FALSE, from, to, api_key){
  res = vantage_daily(symbol, full, api_key = apikey)
  if(is.null(res)){
    return(NULL)
  }
  data = fromJSON(content(res, "text", encoding = "UTF-8"))
  wts = data$`Time Series (Daily)`
  wts = do.call(rbind, wts)
  wts = as.data.frame(wts)
  wts = data.frame(Date = as.Date(rownames(wts)), Open = unpack(wts$`1. open`), High = unpack(wts$`2. high`), Low = unpack(wts$`3. low`), Close = unpack(wts$`4. close`), Volume = unpack(wts$`5. volume`))
  wts = wts %>% filter(between(Date, as.Date(from), as.Date(to)))
  fig = wts %>% plot_ly(x = ~Date, type="candlestick", open = ~Open, close = ~Close, high = ~High, low = ~Low) 
  fig = fig %>% layout(title = paste("Candlestick chart of", symbol), xaxis=list(tickangle=45))
  return(fig)
}

vantage_symbols = function(){
  url = "https://www.alphavantage.co/query?function=LISTING_STATUS&apikey=demo"
  symbols_df = read.csv(url)
  return(symbols_df$symbol)
}

twelve_candle = function(symbol, from, to, interval = '1day', api_key){
  timezone = 'Europe/Warsaw'
  url = paste0('https://api.twelvedata.com/heikinashicandles?symbol=', symbol, '&interval=', interval, '&start_date=', from, '&end_date=', to, '&timezone=', timezone, '&apikey=', api_key)
  res = GET(url)
  data = fromJSON(content(res, "text", encoding = "UTF-8"))
  if(data$status != "ok"){
    return(NULL)
  }
  df = data.frame(data$values)
  df = df %>% mutate(datetime = parse_date_time(datetime, orders = c("ymd", "ymd HMS")))
  fig = df %>% plot_ly(x = ~datetime, type="candlestick", open = ~heikinopens, close = ~heikincloses, high = ~heikinhighs, low = ~heikinlows) 
  fig = fig %>% layout(title = paste("Candlestick chart of", symbol), xaxis=list(title="Time", tickangle=45))
  return(fig)
}

twelve_compare = function(symbols, from, to, interval = '1day', api_key){
  timezone = 'Europe/Warsaw'
  url = paste0('https://api.twelvedata.com/time_series?symbol=', paste(symbols, collapse=','), '&interval=', interval, '&start_date=', from, '&end_date=', to, '&timezone=', timezone, '&apikey=', api_key)
  res = GET(url)
  content = content(res, "text", encoding = "UTF-8")
  data = fromJSON(content(res, "text", encoding = "UTF-8"))
  symbol_dfs = list()
  
  for(sym in names(data)){
    if(data[[sym]]$status != "ok"){
      next
    }
    df = data.frame(data[[sym]]$values)
    df = df %>% mutate(datetime = parse_date_time(datetime, orders = c("ymd", "ymd HMS")))
    symbol_dfs[[sym]] = df
  }
  
  fig = NULL
  
  for (nm in names(symbol_dfs)) {
    df = symbol_dfs[[nm]]
    df$close = as.numeric(df$close)  # ensure numeric
    
    if (is.null(fig)) {
      fig = plot_ly(
        data = df,
        x    = ~datetime,
        y    = ~close,
        type = 'scatter',
        mode = 'lines',
        name = nm
      )
    } else {
      fig = fig %>%
        add_lines(
          data = df,
          x    = ~datetime,
          y    = ~close,
          name = nm
        )
    }
  }
  
  fig = fig %>%
    layout(
      title = "Closing Prices Over Time",
      xaxis = list(title = "Time"),
      yaxis = list(title = "Close")
    )
  return(fig)
}

# colnames(short_to_currency("2024-03-06"))

# currency_transformer <- currency_finder()
# currency_transformer

# rate_values <- from_to_values()
# rate_values


# vantage_query('TIME_SERIES_WEEKLY', list(c('symbol', 'GOOG'), c('interval', '30min')), apikey)
# q = vantage_query('TOP_GAINERS_LOSERS', list(), apikey)
# vantage_grab(q, 'metadata')
# vantage_daily_plot("GOOG", api_key = apikey)
#twelve_candle('AAPL', as.Date('2024-05-23'), as.Date('2025-05-26') twelve_apikey)
url = paste0('https://api.twelvedata.com/heikinashicandles?symbol=AAPL,GOOG&interval=1day&apikey=',twelve_apikey)
twelve_compare(c('AAPL','GOOG'), '2023-01-01', '2025-01-01', '1day', twelve_apikey)



