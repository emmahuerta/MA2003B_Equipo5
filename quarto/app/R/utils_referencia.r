# app/R/utils_referencia.R
LIM_OMS_PM25 <- 15
VALIDACION_LOGISTICA <- list(sens = 0.66, espec = 0.82, vpp = 0.34, auc = 0.82)  # Reporte Etapa 3, §4.5

ETIQUETAS_TERMINOS <- c("s(TOUT)" = "Temperatura", "s(RH)" = "Humedad relativa",
  "s(WSR)" = "Viento", "s(lluvia)" = "Lluvia", "s(SR)" = "Radiación solar",
  "s(lNO2)" = "NO2", "s(lCO)" = "CO", "s(doy)" = "Época del año",
  "estacion" = "Estación", "dow" = "Día de la semana")

limite_nom <- function(fecha) {
  anio <- as.integer(format(as.Date(fecha), "%Y"))
  ifelse(anio <= 2021, 45, ifelse(anio <= 2023, 41, 33))
}

fmt_pct <- function(p) {
  if (p < 0.005) "< 1 %" else if (p > 0.995) "> 99 %" else sprintf("%.0f %%", 100 * p)
}

discrepancia_modelos <- function(p) {
  v <- c(p$pm25_g2, p$pm25_g1, p$pm25_m2, p$pm25_rf)
  (max(v) - min(v)) / mean(v)
}

clasificar_semaforo <- function(pm, li, ls, nom, anio, oms = LIM_OMS_PM25) {
  if (pm <= oms) {
    nivel <- "bajo"; etiqueta <- "Dentro de la guía de la OMS"; icono <- "\u2714"
    resumen <- "Está por debajo de la guía de la OMS."
  } else if (pm <= nom) {
    nivel <- "medio"; etiqueta <- "Sobre la guía OMS, bajo el límite de la NOM"; icono <- "\u25B2"
    resumen <- "Está sobre la guía de la OMS, pero por debajo del límite diario de la NOM."
  } else {
    nivel <- "alto"; etiqueta <- "Supera el límite diario de la NOM"; icono <- "\u2716"
    resumen <- "Supera el límite diario de la NOM."
  }
  lim <- c(OMS = oms, NOM = nom)
  cruza <- names(lim)[li < lim & ls > lim]
  aviso <- if (length(cruza))
    sprintf("El intervalo del valor esperado cruza el límite de %s: no se puede afirmar de qué lado queda.",
            paste(cruza, collapse = " y ")) else ""
  detalle <- sprintf("PM2.5 esperado: %.1f \u00b5g/m\u00b3. Guía OMS 2021 (24 h): %g. Límite NOM-025-SSA1-2021 vigente en %d: %g.",
                     pm, oms, anio, nom)
  list(nivel = nivel, clase = paste0("semaforo-", nivel), etiqueta = etiqueta,
       icono = icono, resumen = resumen, detalle = detalle, aviso = aviso)
}

# Efecto de cada término del GAM G2 como factor multiplicativo (respecto a un día promedio)
contribuciones_gam <- function(m, nd) {
  mod <- m$gam$G2; nd1 <- nd[1, , drop = FALSE]
  tm <- tryCatch(predict(mod, newdata = nd1, type = "terms", discrete = FALSE),
                 error = function(e) tryCatch(predict(mod, newdata = nd1, type = "terms"),
                                              error = function(e2) NULL))
  if (is.null(tm)) return(NULL)
  nm <- colnames(tm); v <- as.numeric(tm[1, ])
  lab <- unname(ETIQUETAS_TERMINOS[nm]); lab[is.na(lab)] <- nm[is.na(lab)]
  out <- data.frame(termino = nm, variable = lab, factor = exp(v), stringsAsFactors = FALSE)
  out <- out[order(-abs(log(out$factor))), ]
  attr(out, "constante") <- as.numeric(attr(tm, "constant"))
  out
}

texto_interpretativo <- function(m, nd, p, sem, contrib, nom) {
  est <- as.character(nd$estacion[1])
  g <- m$grupos[m$grupos$estacion == est, ]
  grupo_txt <- if (nrow(g)) sprintf("grupo de exposición %d, zona %s", g$grupo[1], g$zona[1]) else "sin grupo asignado"
  estab <- if (nrow(g) && g$grupo[1] == 2) "; el grupo 2 es el más heterogéneo y su estabilidad es media" else ""
  p1 <- sprintf("Para <b>%s</b> (%s%s) el %s, el modelo principal (GAM G2) estima un PM2.5 diario esperado de <b>%.1f \u00b5g/m\u00b3</b> (IC 95 %% del valor esperado: %.1f\u2013%.1f). %s",
                est, grupo_txt, estab, format(nd$fecha[1], "%d/%m/%Y"),
                p$pm25_g2, p$g2_li, p$g2_ls, sem$resumen)
  p2 <- ""
  if (!is.null(contrib)) {
    cb <- contrib[!contrib$termino %in% c("estacion", "dow"), ]
    f  <- function(d) paste(sprintf("%s (\u00d7%.2f)", d$variable, d$factor), collapse = ", ")
    sube <- head(cb[cb$factor > 1.05, ], 2); baja <- head(cb[cb$factor < 0.95, ][order(cb$factor[cb$factor < 0.95]), ], 2)
    partes <- c(if (nrow(sube)) paste("lo que más lo eleva:", f(sube)),
                if (nrow(baja)) paste("lo que más lo reduce:", f(baja)))
    p2 <- if (length(partes)) sprintf("Frente a un día promedio, %s.", paste(partes, collapse = "; "))
          else "Ninguna variable se aleja mucho de sus condiciones promedio."
  }
  V <- VALIDACION_LOGISTICA
  p3 <- sprintf("La probabilidad de que sea un día crítico (PM2.5 > %g \u00b5g/m\u00b3) es de <b>%s</b>; con el umbral de %.3f el día se clasifica como <b>%s</b>. En la validación 2024\u20132025 el clasificador detectó %.0f %% de los días críticos reales, y %.0f %% de los días que marcó como críticos lo fueron.",
                m$umbral_critico, fmt_pct(p$p_critico), m$umbral_prob,
                if (p$critico) "crítico" else "no crítico", 100 * V$sens, 100 * V$vpp)
  p4 <- "Recuerde: los modelos son explicativos (usan meteorología y gases del mismo día), no pronósticos, y las asociaciones no son causales."
  HTML(paste0("<p>", c(p1, p2[nzchar(p2)], p3, p4), "</p>", collapse = ""))
}