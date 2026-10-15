# app/global.R
paquetes <- c("shiny", "bslib", "mgcv", "nlme", "geepack", "ranger",
              "ggplot2", "plotly", "leaflet", "DT")
faltan <- setdiff(paquetes, rownames(installed.packages()))
if (length(faltan)) install.packages(faltan)

suppressPackageStartupMessages({
  library(shiny); library(bslib); library(mgcv); library(nlme)
  library(geepack); library(ranger); library(ggplot2)
})

# Funciones auxiliares (se cargan aquí de forma explícita; si Shiny también
# las autocarga desde R/, solo se redefinen sin efectos secundarios)
for (f in sort(list.files("R", pattern = "\\.[Rr]$", full.names = TRUE)))
  source(f, encoding = "UTF-8")


buscar_archivo <- function(nombre) {
  dirs <- c(
    Sys.getenv("APP_SALIDAS"),
    "../../salidas",
    "quarto/salidas",
    "data",
    "../salidas",
    "salidas"
  )

  dirs <- dirs[nzchar(dirs)]

  for (d in dirs) {
    f <- file.path(d, nombre)

    if (file.exists(f)) {
      return(normalizePath(f))
    }
  }

  NULL
}

ERROR_CARGA <- NULL
source("quarto/app/R/utils_modelos.R", encoding = "UTF-8")
RUTA_RDS <- buscar_archivo("etapa3_modelos.rds")
MODELOS <- if (is.null(RUTA_RDS)) {
  ERROR_CARGA <- paste("No se encontró 'etapa3_modelos.rds'. Cópielo a app/data/,",
                       "o defina la variable de entorno APP_SALIDAS con la carpeta que lo contiene.")
  NULL
} else {
  tryCatch(cargar_modelos(RUTA_RDS), error = function(e) {
    ERROR_CARGA <<- conditionMessage(e); NULL })
}

# ---- AGREGAR al final de app/global.R -----------------------------------------
suppressPackageStartupMessages({ library(leaflet); library(DT); library(plotly) })
RUTA_BOOT    <- buscar_archivo("etapa3_orden_bootstrap.csv")   # opcional
RUTA_DIARIOS <- buscar_archivo("etapa2_datos_diarios.rds")     # opcional

# ---- AGREGAR al final de app/global.R ---------------------------------------
# Tema común de las gráficas (envuelve ggplot2::theme_minimal)
theme_minimal <- function(base_size = 13, ...) {
  ggplot2::theme_minimal(base_size = base_size, ...) +
    ggplot2::theme(
      text             = ggplot2::element_text(colour = "#1F3641"),
      plot.subtitle    = ggplot2::element_text(colour = "#4D6470", size = ggplot2::rel(0.85)),
      plot.title       = ggplot2::element_text(face = "bold", colour = "#0F2F3A"),
      axis.title       = ggplot2::element_text(colour = "#174653", face = "bold", size = ggplot2::rel(0.9)),
      axis.text        = ggplot2::element_text(colour = "#4D6470"),
      panel.grid.major = ggplot2::element_line(colour = "#E1EAEE", linewidth = 0.4),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = "top",
      legend.text      = ggplot2::element_text(colour = "#1F3641"),
      plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA)
    )
}