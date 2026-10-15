# app/R/utils_validacion.R
MODELOS_VAL <- c("GAM G2 (principal)" = "g2", "GAM G1 (sin NO2 ni CO)" = "g1",
                 "Regresión lineal M2" = "m2", "Bosque aleatorio" = "rf")

metricas_pred <- function(obs, pred) {
  data.frame(n = length(obs), RMSE = sqrt(mean((obs - pred)^2)), MAE = mean(abs(obs - pred)),
             r = if (length(obs) > 2) suppressWarnings(cor(obs, pred)) else NA_real_)
}

metricas_por <- function(d, col, por = "estacion", orden = NULL) {
  g <- split(d, d[[por]])
  out <- do.call(rbind, lapply(names(g), function(k) cbind(setNames(data.frame(k), por),
                                                           metricas_pred(g[[k]]$obs, g[[k]][[col]]))))
  if (!is.null(orden)) out <- out[match(intersect(orden, out[[por]]), out[[por]]), ]
  rownames(out) <- NULL
  out
}

auc_rangos <- function(y, p) {
  r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

construir_validacion <- function(m, ruta) {
  if (is.null(ruta) || !file.exists(ruta))
    return(list(ok = FALSE, msg = "No se encontró 'etapa2_datos_diarios.rds'. Cópielo a app/data/ o defina APP_SALIDAS (vea el README)."))
  d <- tryCatch(as.data.frame(readRDS(ruta)), error = function(e) NULL)
  if (is.null(d)) return(list(ok = FALSE, msg = "No se pudo leer 'etapa2_datos_diarios.rds'."))
  col_dia <- intersect(c("dia", "fecha"), names(d))[1]
  falt <- setdiff(c("estacion", "PM25", VARS_CRUDAS), names(d))
  if (is.na(col_dia) || length(falt))
    return(list(ok = FALSE, msg = paste("A la base diaria le faltan columnas:",
                                        paste(c(if (is.na(col_dia)) "dia", falt), collapse = ", "))))
  d$fecha <- as.Date(d[[col_dia]]); d$estacion <- as.character(d$estacion)
  d <- d[!is.na(d$fecha) & d$estacion %in% m$est_c &
           d$fecha >= as.Date("2024-01-01") & d$fecha <= as.Date("2025-12-31"), ]
  n0 <- nrow(d)
  bien <- is.finite(d$PM25) & d$PM25 > 0 &
    Reduce(`&`, lapply(VARS_CRUDAS, function(v) is.finite(d[[v]]))) &
    d$RH >= 0 & d$RH <= 100 & d$WSR >= 0 & d$RAINF >= 0 & d$SR >= 0 & d$NO2 >= 0 & d$CO >= 0
  d <- d[bien, ]
  if (nrow(d) < 100) return(list(ok = FALSE, msg = "Quedaron muy pocas filas completas en 2024–2025."))

  nd <- preparar_entrada(m, d$estacion, d$fecha, d$TOUT, d$RH, d$WSR, d$RAINF, d$SR, d$NO2, d$CO)
  p  <- predecir_modelos(m, nd)
  out <- data.frame(estacion = d$estacion, fecha = d$fecha, anio = as.integer(format(d$fecha, "%Y")),
                    obs = d$PM25, g2 = p$pm25_g2, g1 = p$pm25_g1, m2 = p$pm25_m2, rf = p$pm25_rf,
                    p = p$p_critico, stringsAsFactors = FALSE)
  out$crit <- as.integer(out$obs > m$umbral_critico)

  glob <- do.call(rbind, lapply(names(MODELOS_VAL), function(k)
    cbind(Modelo = k, metricas_pred(out$obs, out[[MODELOS_VAL[[k]]]]))))
  logi <- NULL
  if (length(unique(out$crit)) == 2) {
    pc <- as.integer(out$p >= m$umbral_prob)
    tp <- sum(pc == 1 & out$crit == 1); fn <- sum(pc == 0 & out$crit == 1)
    tn <- sum(pc == 0 & out$crit == 0); fp <- sum(pc == 1 & out$crit == 0)
    logi <- c(AUC = auc_rangos(out$crit, out$p), Brier = mean((out$p - out$crit)^2),
              Sensibilidad = tp / (tp + fn), Especificidad = tn / (tn + fp), VPP = tp / (tp + fp))
  }
  list(ok = TRUE, datos = out, n = nrow(out), n_desc = n0 - nrow(out), global = glob, logistica = logi)
}