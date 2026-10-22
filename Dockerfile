FROM rocker/shiny:4.5.0

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    libgdal-dev libgeos-dev libproj-dev libudunits2-dev \
    && rm -rf /var/lib/apt/lists/*

# Usar paquetes precompilados (mucho más rápido)
ENV RENV_CONFIG_REPOS_OVERRIDE=https://p3m.dev/cran/__linux__/noble/latest

WORKDIR /app
COPY . /app

# Instalar todos los paquetes del renv.lock durante el build
RUN R -e "renv::restore(prompt = FALSE)"

EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/app/quarto/app/R', host = '0.0.0.0', port = as.numeric(Sys.getenv('PORT', 3838)))"]
