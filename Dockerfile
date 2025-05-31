FROM rocker/r-ver:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'shinythemes', 'fresh', 'plotly', 'ggplot2', 'dplyr', 'httr', 'webshot2', 'promises', 'htmlwidgets', 'slickR', 'htmltools', 'shinyBS', 'DT', 'jsonlite', 'lubridate', 'rsconnect'), repos='https://cran.rstudio.com/')"

# Set working directory
WORKDIR /app

# Copy deployment script
COPY deploy.R /deploy.R

# Make the script executable
RUN chmod +x /deploy.R

# Run the deployment script
CMD ["Rscript", "/deploy.R"]