# app/R/utils_estaciones.R
COLORES_GRUPO <- c("1" = "#0072B2", "2" = "#E69F00", "3" = "#B3261E")
ESTABILIDAD_GRUPO <- c("1" = "alta (1.00 en las tres estaciones)",
                       "2" = "media (0.80–0.84; CE 0.59, limítrofe con el grupo 3)",
                       "3" = "alta (1.00 en ambas)")   # Reporte Etapa 3, Tabla 3

# Coordenadas SIMA 2025 de las 12 estaciones con modelo (grados, minutos, segundos; lat N, lon W)
COORD_DMS <- data.frame(
  estacion = c("SE","NE","CE","NO","SO","NO2","NTE2","NE2","SE2","SO2","SE3","SUR"),
  lat_g = 25,
  lat_m = c(39,44,40,45,40,48,43,46,38,39,36,37),
  lat_s = c(55,41,33,46,32, 1,47,38,45,54, 4, 1),
  lon_g = c(100,100,100,100,100,100,100,100,100,100,99,100),
  lon_m = c( 14, 15, 20, 22, 27, 35, 18, 11,  5, 24,59, 16),
  lon_s = c( 37, 11, 18,  9, 29,  4, 36, 17, 43, 46,57, 26),
  msnm  = c(500,474,562,568,674,702,520,432,387,636,334,555),
  stringsAsFactors = FALSE)

dms_a_decimal <- function(grados, minutos, segundos) grados + minutos / 60 + segundos / 3600

coordenadas_estaciones <- function() {
  d <- COORD_DMS
  d$lat <- dms_a_decimal(d$lat_g, d$lat_m, d$lat_s)
  d$lon <- -dms_a_decimal(d$lon_g, d$lon_m, d$lon_s)      # oeste = negativo
  d[, c("estacion", "lat", "lon", "msnm")]
}

# Intervalos bootstrap (opcional): devuelve NULL si el archivo no existe o no tiene el formato esperado
leer_bootstrap <- function(ruta) {
  if (is.null(ruta) || !file.exists(ruta)) return(NULL)
  b <- tryCatch(read.csv(ruta, stringsAsFactors = FALSE, fileEncoding = "UTF-8"), error = function(e) NULL)
  need <- c("Contaminante","estacion","media","media_LI","media_LS","rango_med","rango_LI","rango_LS",
            "oms","nom","nom_LI","nom_LS")
  if (is.null(b) || length(setdiff(need, names(b)))) return(NULL)
  b <- b[b$Contaminante == "PM25", , drop = FALSE]
  if (!nrow(b)) NULL else b
}

# Tabla maestra de estaciones (grupo, nivel observado, ajustado, coordenadas y bootstrap si hay)
tabla_estaciones <- function(m, boot = NULL) {
  na <- as.data.frame(m$nivel_ajustado, stringsAsFactors = FALSE)
  if (ncol(na) < 9) stop("'nivel_ajustado' no tiene las 9 columnas esperadas.", call. = FALSE)
  na <- na[, 1:9]       # se renombra por posición para no depender de acentos en los nombres
  names(na) <- c("estacion","obs","g0","g1","g1_li","g1_ls","cambio","rango_g0","rango_g1")
  g <- as.data.frame(m$grupos, stringsAsFactors = FALSE)[, c("estacion","grupo","zona","msnm")]
  g$zona <- as.character(g$zona)
  d <- merge(g, na, by = "estacion")
  d <- merge(d, coordenadas_estaciones()[, c("estacion","lat","lon")], by = "estacion")
  if (!is.null(boot)) {
    b <- boot[, c("estacion","media_LI","media_LS","rango_med","rango_LI","rango_LS","oms","nom","nom_LI","nom_LS")]
    names(b) <- c("estacion","obs_li","obs_ls","rango_obs","rango_li","rango_ls","pct_oms","pct_nom","nom_li","nom_ls")
    d <- merge(d, b, by = "estacion", all.x = TRUE)
  }
  d <- d[order(d$g1), ]
  d$col <- COLORES_GRUPO[as.character(d$grupo)]
  rownames(d) <- NULL
  d
}

popup_estacion <- function(d) {
  sprintf("<b>%s</b> · %s · %d msnm<br/>Grupo de exposición: <b>%d</b><br/>PM2.5 observado 2021–2023: %.1f µg/m³<br/>Ajustado por meteorología (G1): %.1f µg/m³ (%+.0f %%)",
          d$estacion, d$zona, as.integer(d$msnm), as.integer(d$grupo), d$obs, d$g1, d$cambio)
}

tabla_grupos <- function(tb) {
  do.call(rbind, lapply(sort(unique(tb$grupo)), function(k) {
    s <- tb[tb$grupo == k, ]
    data.frame(Grupo = k, Estaciones = paste(s$estacion, collapse = ", "),
               `PM2.5 medio (µg/m³)` = round(mean(s$obs), 1), `Elevación media (m)` = round(mean(s$msnm)),
               Estabilidad = ESTABILIDAD_GRUPO[[as.character(k)]], check.names = FALSE, row.names = NULL)
  }))
}