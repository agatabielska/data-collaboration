#!/usr/bin/env Rscript

# Load required library
library(rsconnect)

# Get environment variables
token <- Sys.getenv("SHINYAPPS_TOKEN")
secret <- Sys.getenv("SHINYAPPS_SECRET")
app_name <- Sys.getenv("APP_NAME")
account_name <- Sys.getenv("ACCOUNT_NAME")

# Check if required environment variables are set
if (token == "" || secret == "" || app_name == "" || account_name == "") {
  stop("Missing required environment variables: SHINYAPPS_TOKEN, SHINYAPPS_SECRET, APP_NAME, ACCOUNT_NAME")
}

# Set up account
cat("Setting up shinyapps.io account...\n")
rsconnect::setAccountInfo(
  name = account_name,
  token = token,
  secret = secret
)

# Deploy the application with force update
cat("Deploying application to shinyapps.io...\n")
rsconnect::deployApp(
  appDir = "/app",
  appName = app_name,
  account = account_name,
  forceUpdate = TRUE,
  launch.browser = FALSE
)

cat("Deployment completed successfully!\n")