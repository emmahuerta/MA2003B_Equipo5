FROM rocker/shiny:4.4.1

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('shiny', 'ggplot2', 'DT'))"

COPY . /app
WORKDIR /app

EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/app/quarto/app/R', host = '0.0.0.0', port = as.numeric(Sys.getenv('PORT', 3838)))"]
