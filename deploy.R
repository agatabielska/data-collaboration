install.packages("rsconnect")
library(rsconnect)
rsconnect::setAccountInfo(
  name = Sys.getenv("SHINYAPPS_NAME"),
  token = Sys.getenv("SHINYAPPS_TOKEN"),
  secret = Sys.getenv("SHINYAPPS_SECRET")
)

rsconnect::deployApp(appDir = ".", appName = "your-app-name")
