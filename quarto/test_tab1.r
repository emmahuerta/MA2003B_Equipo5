# test_tab1.R  -- correr desde la raíz del proyecto
suppressPackageStartupMessages({ library(shiny); library(bslib); library(mgcv); library(nlme)
  library(geepack); library(ranger); library(ggplot2) })
# Cargar funciones auxiliares sin ejecutar app.R ni global.R
archivos_R <- sort(list.files(
  "quarto/app/R",
  pattern = "\\.[Rr]$",
  full.names = TRUE
))

archivos_R <- archivos_R[
  !basename(archivos_R) %in% c("app.R", "global.R")
]

for (f in archivos_R) {
  source(f, encoding = "UTF-8")
}

# Cargar los modelos una sola vez
m <- cargar_modelos("quarto/salidas/etapa3_modelos.rds")
chk <- list(); ok <- function(nombre, cond) chk[[length(chk) + 1]] <<- data.frame(prueba = nombre, ok = isTRUE(cond))

# 1. Límites NOM por año
ok("NOM 2021 = 45", limite_nom("2021-06-01") == 45)
ok("NOM 2022 y 2023 = 41", all(limite_nom(c("2022-01-01", "2023-12-31")) == 41))
ok("NOM 2024 y 2025 = 33", all(limite_nom(c("2024-01-01", "2025-12-31")) == 33))

# 2. Semáforo en los bordesshiny::runApp("quarto/app/R")
s1 <- clasificar_semaforo(14.9, 14, 15.5, 33, 2025); s2 <- clasificar_semaforo(20, 19, 21, 33, 2025)
s3 <- clasificar_semaforo(40, 38, 42, 33, 2025);     s4 <- clasificar_semaforo(33.0, 31, 35, 33, 2025)
ok("semáforo bajo", s1$nivel == "bajo"); ok("semáforo medio", s2$nivel == "medio")
ok("semáforo alto", s3$nivel == "alto"); ok("borde NOM=33 es medio", s4$nivel == "medio")
ok("aviso de cruce con OMS", grepl("OMS", s1$aviso)); ok("aviso de cruce con NOM", grepl("NOM", s4$aviso))
ok("sin cruce, sin aviso", !nzchar(s3$aviso))

# 3. Contribuciones del GAM reconstruyen la predicción
nd <- preparar_entrada(m, "SE2", "2023-06-15", 28, 60, 6, 0, 0.2, 20, 1.5)
cb <- contribuciones_gam(m, nd); p <- predecir_modelos(m, nd)
ok("contribuciones no nulas", !is.null(cb))
if (!is.null(cb)) {
  recon <- exp(sum(log(cb$factor)) + attr(cb, "constante"))
  ok("exp(suma de términos + constante) = predicción G2", abs(recon - p$pm25_g2) < 1e-6)
  cat("\nContribuciones G2 (SE2, 2023-06-15):\n"); print(cb, digits = 3, row.names = FALSE)
}

# 4. Módulo completo con testServer
vals <- list(s_TOUT = 28, s_RH = 60, s_WSR = 6, s_RAINF = 0, s_SR = 0.2, s_NO2 = 20, s_CO = 1.5)
testServer(mod_estimador_server, args = list(m = m), {
  do.call(session$setInputs, c(list(estacion = "SE2", fecha = as.Date("2023-06-15"), manual = FALSE), vals))
  cc <- calc()
  ok("módulo: calc ok", cc$ok)
  ok("módulo: PM2.5 G2 coincide con predecir_modelos", abs(cc$pred$pm25_g2 - p$pm25_g2) < 1e-8)
  for (o in c("semaforo", "cifras", "texto", "tabla_modelos", "grafica_modelos", "grafica_contrib")) {
    r <- tryCatch({ output[[o]]; TRUE }, error = function(e) { message(o, ": ", conditionMessage(e)); FALSE })
    ok(paste("salida", o), r)
  }
  # fecha vacía -> error entendible
  session$setInputs(fecha = NA)
  ok("fecha vacía: calc ok = FALSE con mensaje", !calc()$ok && nzchar(calc()$msg))
  # modo manual fuera de rango -> avisos
  session$setInputs(fecha = as.Date("2025-12-31"), manual = TRUE, n_TOUT = -10, n_RH = 60, n_WSR = 6,
                    n_RAINF = 50, n_SR = 0.2, n_NO2 = 120, n_CO = 1.5)
  cc2 <- calc()
  ok("manual: calc ok", cc2$ok)
  ok("manual: avisos de rango (TOUT, NO2 fuera; RAINF extrema)", cc2$ok && length(cc2$msgs) >= 3)
  cat("\nAvisos en modo manual:\n"); cat(cc2$msgs, sep = "\n")
})

res <- do.call(rbind, chk); cat("\n== Resultados ==\n"); print(res, row.names = FALSE)
cat("\nRESULTADO GLOBAL:", if (all(res$ok)) "TODO OK" else "REVISAR filas con ok = FALSE", "\n")