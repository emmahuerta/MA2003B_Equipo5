# test_tabs.R  -- correr desde la raíz del proyecto

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(mgcv)
  library(nlme)
  library(geepack)
  library(ranger)
  library(ggplot2)
  library(leaflet)
  library(DT)
  library(plotly)
})


inicio <- Sys.time()

# ---- Carga de funciones auxiliares ----------------------------------------

archivos_R <- sort(list.files(
  "quarto/app/R",
  pattern = "\\.[Rr]$",
  full.names = TRUE
))

# Excluir archivos de arranque de la aplicación
archivos_R <- archivos_R[
  !tolower(basename(archivos_R)) %in% c("app.r", "global.r")
]

for (f in archivos_R) {
  cat("Cargando:", f, "\n")

  system.time(
    source(f, encoding = "UTF-8")
  ) |> print()
}

# ---- Carga de modelos -----------------------------------------------------

ruta_modelos <- "quarto/salidas/etapa3_modelos.rds"

if (!file.exists(ruta_modelos)) {
  stop("No se encontró el archivo: ", ruta_modelos)
}

cat("\nCargando modelos...\n")

tiempo_modelos <- system.time({
  m <- cargar_modelos(ruta_modelos)
})

cat(
  "Modelos cargados en:",
  round(tiempo_modelos[["elapsed"]], 2),
  "segundos\n"
)

cat("\nArchivos auxiliares y modelos cargados.\n")

# ---- Localización de bases auxiliares ------------------------------------

ruta_boot <- if (
  file.exists("quarto/salidas/etapa3_orden_bootstrap.csv")
) "quarto/salidas/etapa3_orden_bootstrap.csv" else NULL

ruta_diar <- if (
  file.exists("quarto/salidas/etapa2_datos_diarios.rds")
) "quarto/salidas/etapa2_datos_diarios.rds" else NULL

cat(
  "Bootstrap CSV:",
  if (is.null(ruta_boot)) "no encontrado" else "encontrado",
  "| Base diaria:",
  if (is.null(ruta_diar)) "no encontrada" else "encontrada",
  "\n"
)


ruta_boot <- if (file.exists("quarto/salidas/etapa3_orden_bootstrap.csv")) "quarto/salidas/etapa3_orden_bootstrap.csv" else NULL
ruta_diar <- if (file.exists("quarto/salidas/etapa2_datos_diarios.rds")) "quarto/salidas/etapa2_datos_diarios.rds" else NULL
cat("Bootstrap CSV:", if (is.null(ruta_boot)) "no encontrado" else "encontrado",
    "| Base diaria:", if (is.null(ruta_diar)) "no encontrada" else "encontrada", "\n")

chk <- list(); ok <- function(nombre, cond) chk[[length(chk) + 1]] <<- data.frame(prueba = nombre, ok = isTRUE(cond))
sal <- function(output, nombre) tryCatch({ output[[nombre]]; TRUE }, error = function(e) { message("  [", nombre, "] ", conditionMessage(e)); FALSE })

# ---- 1. Coordenadas ----------------------------------------------------------------
cd <- coordenadas_estaciones()
ok("12 estaciones = est_c", nrow(cd) == 12 && setequal(cd$estacion, m$est_c))
ok("SE: latitud decimal", abs(cd$lat[cd$estacion == "SE"] - (25 + 39/60 + 55/3600)) < 1e-9)
ok("SE: longitud decimal (negativa)", abs(cd$lon[cd$estacion == "SE"] + (100 + 14/60 + 37/3600)) < 1e-9)
ok("SE3: longitud 99°59'57\"", abs(cd$lon[cd$estacion == "SE3"] + (99 + 59/60 + 57/3600)) < 1e-9)
ok("coordenadas dentro de la ZMM", all(cd$lat > 25.5 & cd$lat < 26 & cd$lon > -100.7 & cd$lon < -99.9))
cc <- as.data.frame(m$cat_est); ok("msnm coincide con cat_est", all(cd$msnm == cc$msnm[match(cd$estacion, cc$estacion)]))

# ---- 2. Tabla de estaciones -----------------------------------------------------------
boot <- leer_bootstrap(ruta_boot); tb <- tabla_estaciones(m, boot)
ok("tabla: 12 filas sin NA en G0/G1", nrow(tb) == 12 && !anyNA(tb$g0) && !anyNA(tb$g1))
ok("SE2 baja con el ajuste, SO sube", tb$g1[tb$estacion == "SE2"] < tb$g0[tb$estacion == "SE2"] &&
                                      tb$g1[tb$estacion == "SO"]  > tb$g0[tb$estacion == "SO"])
ok("SE2: 28.0 -> 21.7 (reporte)", abs(tb$g0[tb$estacion == "SE2"] - 28.0) < 0.1 && abs(tb$g1[tb$estacion == "SE2"] - 21.7) < 0.1)
ok("grupos: SE2 y SO en el grupo 3", all(tb$grupo[tb$estacion %in% c("SE2", "SO")] == 3))
if (!is.null(boot)) ok("bootstrap: IC contiene la media observada", all(tb$obs_li <= tb$obs & tb$obs <= tb$obs_ls))
print(tabla_grupos(tb), row.names = FALSE)

# ---- 3. Curvas de efecto ----------------------------------------------------------------
rg <- as.data.frame(m$rangos); rownames(rg) <- rg$variable
base <- setNames(as.list(rg[VARS_CRUDAS, "p50"]), VARS_CRUDAS)
for (v in VARS_CRUDAS) {
  cu <- curva_efecto(m, "CE", "2025-01-15", base, v, rango_curva(rg, v, FALSE))
  ok(paste0("curva ", v, ": 80 puntos finitos y banda coherente"),
     nrow(cu) == 80 && all(is.finite(unlist(cu))) && all(cu$li <= cu$fit & cu$fit <= cu$ls))
  ok(paste0("curva ", v, ": extremos = p01 y p99"), abs(cu$x[1] - rg[v, "p01"]) < 1e-9 && abs(cu$x[80] - rg[v, "p99"]) < 1e-9)
}
a <- base; a$NO2 <- rg["NO2", "p99"]
cu <- curva_efecto(m, "CE", "2025-01-15", a, "NO2", rango_curva(rg, "NO2", FALSE))
nd <- preparar_entrada(m, "CE", "2025-01-15", a$TOUT, a$RH, a$WSR, a$RAINF, a$SR, a$NO2, a$CO)
ok("curva NO2 en p99 = predict() directo", abs(cu$fit[80] - predecir_gam(m, nd, "G2")$fit) < 1e-8)
cw <- curva_efecto(m, "CE", "2025-01-15", base, "WSR", rango_curva(rg, "WSR", FALSE))
ratio <- cw$fit[1] / approx(cw$x, cw$fit, xout = 9)$y
cat("\nRazón PM2.5 en calma (p01) / PM2.5 a 9 km/h:", round(ratio, 2), "(reporte: 1.23/0.93 ≈ 1.32)\n")
ok("viento: calma > 9 km/h", ratio > 1.1)
cr <- curva_efecto(m, "CE", "2025-01-15", base, "RAINF", rango_curva(rg, "RAINF", TRUE))
ok("curva con rango completo (RAINF hasta el máximo)", abs(cr$x[80] - rg["RAINF", "max"]) < 1e-6 && all(is.finite(cr$fit)))

# ---- 4. Módulo escenarios ----------------------------------------------------------------
testServer(mod_escenarios_server, args = list(m = m), {
  ini <- function(pref, est, wsr, no2) {
    s <- list(estacion = est, fecha = as.Date("2025-01-15"))
    for (v in VARS_UI$v) s[[v]] <- rg[v, "p50"]; s$WSR <- wsr; s$NO2 <- no2
    names(s) <- paste0(pref, names(s)); do.call(session$setInputs, s)
  }
  session$setInputs(variable = "WSR", ampliar = FALSE)
  ini("a_", "CE", rg["WSR", "p50"], rg["NO2", "p50"]); ini("b_", "SE2", rg["WSR", "p01"], rg["NO2", "p99"])
  ra <- res_a(); rb <- res_b()
  ok("escenarios: A y B ok", ra$ok && rb$ok)
  ok("escenarios: B (estancamiento) > A en el punto", rb$punto$fit > ra$punto$fit)
  for (o in c("grafica", "tabla", "resumen", "nota_variable", "aviso")) ok(paste("escenarios: salida", o), sal(output, o))
  session$setInputs(variable = "RAINF", ampliar = TRUE)
  ok("escenarios: RAINF ampliado ok", res_a()$ok && sal(output, "grafica"))
  session$setInputs(a_fecha = NA)
  ok("escenarios: fecha vacía -> mensaje", !res_a()$ok && nzchar(res_a()$msg))
})

# ---- 5. Módulo estaciones --------------------------------------------------------------------
testServer(mod_estaciones_server, args = list(m = m, ruta_boot = ruta_boot), {
  for (o in c("aviso_boot", "mapa", "tabla_grupos", "tabla", "grafica", "texto_ajuste")) ok(paste("estaciones: salida", o), sal(output, o))
})

cat("\n--- INICIO: prueba de validación ---\n")
tiempo_validacion <- Sys.time()

# ---- 6. Módulo validación -----------------------------------------------------------------------
testServer(mod_validacion_server, args = list(m = m, ruta_diarios = ruta_diar), {
  for (o in c("tabla_global", "grafica_rmse", "tabla_import", "grafica_import", "tabla_logistica", "aviso_diarios"))
    ok(paste("validación: salida", o), sal(output, o))
  if (!is.null(ruta_diar)) {
    session$setInputs(est_sel = "SE2", mod_sel = names(MODELOS_VAL)[1])
    session$setInputs(cargar = 1)
    e <- estado(); ok("validación: base diaria procesada", isTRUE(e$ok))
    if (isTRUE(e$ok)) {
      cat("\nFilas usadas:", e$n, "| descartadas:", e$n_desc, "(reporte: 7,234 filas)\n")
      v <- as.data.frame(m$validacion); g <- e$global
      cmp <- data.frame(Modelo = c("G2", "G1", "M2", "RF"),
                        RMSE_guardado = v$RMSE[match(c("G2", "G1", "M2", "RF"), v$Modelo)], RMSE_recalculado = g$RMSE,
                        r_guardado = v$r[match(c("G2", "G1", "M2", "RF"), v$Modelo)], r_recalculado = g$r)
      cmp$dif_RMSE <- cmp$RMSE_recalculado - cmp$RMSE_guardado
      cat("\n== Métricas guardadas vs recalculadas ==\n"); print(cmp, digits = 4, row.names = FALSE)
      ok("validación: n = 7,234", e$n == 7234)
      ok("validación: RMSE recalculado = guardado (|dif| < 0.02)", all(abs(cmp$dif_RMSE) < 0.02))
      if (!is.null(e$logistica)) {
        cat("\n== Logística: reporte vs recalculado ==\n")
        print(data.frame(metrica = names(REPORTE_LOGISTICA), reporte = unname(REPORTE_LOGISTICA), recalculado = round(unname(e$logistica[names(REPORTE_LOGISTICA)]), 3)), row.names = FALSE)
        ok("validación: AUC ≈ 0.82", abs(e$logistica[["AUC"]] - 0.82) < 0.01)
        ok("validación: Brier ≈ 0.084", abs(e$logistica[["Brier"]] - 0.084) < 0.003)
        ok("validación: sensibilidad ≈ 0.66", abs(e$logistica[["Sensibilidad"]] - 0.66) < 0.02)
      }
      for (o in c("serie", "dispersion", "tabla_anio", "tabla_est", "resumen_calculo")) ok(paste("validación: salida", o), sal(output, o))
      cat("\n== G2 por año ==\n"); print(metricas_por(e$datos, "g2", "anio"), digits = 3, row.names = FALSE)
      cat("(reporte: RMSE G2 = 9.30 en 2024 y 8.09 en 2025)\n")
    }
  } else cat("\n[Validación con base diaria omitida: no se encontró etapa2_datos_diarios.rds]\n")
})

cat(
  "\nTiempo total de validación:",
  round(
    as.numeric(difftime(
      Sys.time(),
      tiempo_validacion,
      units = "secs"
    )),
    2
  ),
  "segundos\n"
)

res <- do.call(rbind, chk); cat("\n== Resultados ==\n"); print(res, row.names = FALSE)
cat("\nRESULTADO GLOBAL:", if (all(res$ok)) "TODO OK" else "REVISAR filas con ok = FALSE", "\n")