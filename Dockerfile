FROM rocker/shiny:4.5.0

# Instalar solo los paquetes que usa la app (precompilados, es rápido)
RUN R -e "pk <- c('shiny','bslib','geepack','ranger','ggplot2','plotly','leaflet','DT'); install.packages(pk); stopifnot(all(sapply(pk, requireNamespace, quietly = TRUE)))"

WORKDIR /app
COPY . /app

# No activar renv en el servidor y corregir la mayúscula de utils_modelos
RUN rm -f /app/.Rprofile && sed -i 's#utils_modelos\.R#utils_modelos.r#' /app/quarto/app/R/global.r

EXPOSE 3838
CMD ["R", "--no-save", "-e", "setwd('/app'); source('quarto/app/R/global.r', encoding = 'UTF-8'); for (f in setdiff(list.files('quarto/app/R', pattern = '[.][Rr]$', full.names = TRUE), c('quarto/app/R/app.r', 'quarto/app/R/global.r'))) source(f, encoding = 'UTF-8'); app <- source('quarto/app/R/app.r', encoding = 'UTF-8')$value; shiny::runApp(app, host = '0.0.0.0', port = as.numeric(Sys.getenv('PORT', 3838)))"]
