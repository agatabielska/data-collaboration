# Add custom library path explicitly
.libPaths(c(Sys.getenv("R_LIBS_USER"), .libPaths()))

library(rsconnect)
rsconnect::setAccountInfo(
  name = Sys.getenv("SHINYAPPS_NAME"),
  token = Sys.getenv("SHINYAPPS_TOKEN"),
  secret = Sys.getenv("SHINYAPPS_SECRET")
)

rsconnect::deployApp(appDir = ".")
