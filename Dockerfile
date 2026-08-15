# AI4RPT — Dockerfile for deploying the Plumber API on Render.com
# ============================================================================
# Render builds and runs this automatically once linked to your GitHub repo.
# No local Docker installation needed on your machine — Render builds it
# on their servers.
# ============================================================================

FROM rocker/r-ver:4.3.1

# System dependencies needed by the R packages we use
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN R -e "install.packages(c('plumber', 'dplyr', 'readr', 'purrr', 'lubridate', 'tibble', 'httr', 'jsonlite', 'htmltools', 'nasapower'), repos='https://cran.rstudio.com/')"

# Copy all project files into the container
WORKDIR /app
COPY . /app

# Render assigns a port via the PORT environment variable — the API must
# listen on that, not a hardcoded port like 8000
EXPOSE 8000
CMD ["R", "-e", "pr <- plumber::pr('plumber.R'); pr$run(host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', 8000)))"]
