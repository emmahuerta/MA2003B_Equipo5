# app/R/utils_modelos.R   (guardar en UTF-8; cargar con source(..., encoding = "UTF-8"))

MESES_ES    <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
DOW_ES      <- c("Lun","Mar","Mi\u00e9","Jue","Vie","S\u00e1b","Dom")
TEMPORADAS  <- c("invierno","primavera","verano","oto\u00f1o")
TEMP_DE_MES <- c(1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 1)   # dic-feb, mar-may, jun-ago, sep-nov
VARS_CRUDAS <- c("TOUT","RH","WSR","RAINF","SR","NO2","CO")
VARS_X      <- c("TOUT","RH","WSR","lluvia","SR","lNO2","lCO")
ELEMENTOS_REQUERIDOS <- c("gam","m2","rf","gee","mu","sdv","vars_x","smear_m2","smear_rf",
                          "umbral_prob","umbral_critico","est_c","grupos","cat_est",
                          "validacion","importancia_gam","nivel_ajustado","rangos")

# ---- Carga ------------------------------------------------------------------
niveles_factores <- function(m) {
  est <- tryCatch(levels(m$gam$G2$model$estacion), error = function(e) NULL)
  if (is.null(est) || !length(est)) est <- as.character(m$est_c)
  if (!setequal(est, as.character(m$est_c)))
    stop("Las estaciones del GAM no coinciden con 'est_c' del rds.", call. = FALSE)
  list(estacion = est, mes_lab = MESES_ES, dow = DOW_ES, temporada = TEMPORADAS)
}

cargar_modelos <- function(ruta = "salidas/etapa3_modelos.rds") {
  if (!file.exists(ruta))
    stop("No se encontr\u00f3 '", ruta, "'. Verifique la ruta o vuelva a generar el rds con 3_Modelado.qmd.", call. = FALSE)
  m <- tryCatch(readRDS(ruta),
                error = function(e) stop("No se pudo leer el rds: ", conditionMessage(e), call. = FALSE))
  falt <- setdiff(ELEMENTOS_REQUERIDOS, names(m))
  if (length(falt))
    stop("Al rds le faltan elementos: ", paste(falt, collapse = ", "), call. = FALSE)
  if (!all(c("G0","G1","G2") %in% names(m$gam)))
    stop("La lista 'gam' debe contener G0, G1 y G2.", call. = FALSE)
  if (isTRUE(m$usa_wd))
    stop("El GAM se ajust\u00f3 con direcci\u00f3n del viento (usa_wd = TRUE); la app a\u00fan no tiene esa entrada.", call. = FALSE)
  m$niveles <- niveles_factores(m)
  m
}

# ---- Rangos de entrenamiento ---------------------------------------------------
evaluar_rango <- function(crudo, rangos) {
  r <- as.data.frame(rangos); rownames(r) <- r$variable
  filas <- lapply(VARS_CRUDAS, function(v) {
    x <- crudo[[v]]
    estado <- ifelse(x < r[v, "min"] | x > r[v, "max"], "fuera",
              ifelse(x < r[v, "p01"] | x > r[v, "p99"], "extremo", "dentro"))
    data.frame(fila = seq_along(x), variable = v, valor = x, estado = estado,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, filas)
}

mensajes_rango <- function(ev, rangos) {
  r <- as.data.frame(rangos); rownames(r) <- r$variable
  ev <- ev[ev$estado != "dentro", , drop = FALSE]
  if (!nrow(ev)) return(character(0))
  f <- function(x) format(round(x, 2), nsmall = 0)
  vapply(seq_len(nrow(ev)), function(i) {
    v <- ev$variable[i]
    if (ev$estado[i] == "fuera")
      sprintf("%s = %s est\u00e1 fuera del rango de entrenamiento [%s, %s]: el modelo extrapola y el resultado no es confiable.",
              v, f(ev$valor[i]), f(r[v, "min"]), f(r[v, "max"]))
    else
      sprintf("%s = %s est\u00e1 en la cola extrema (fuera de p01\u2013p99 = [%s, %s]): resultado menos confiable.",
              v, f(ev$valor[i]), f(r[v, "p01"]), f(r[v, "p99"]))
  }, character(1))
}

# ---- Preparación de entradas (funci\u00f3n única para todos los modelos) --------------
preparar_entrada <- function(modelos, estacion, fecha, TOUT, RH, WSR, RAINF, SR, NO2, CO) {
  fecha_d <- tryCatch(as.Date(fecha), error = function(e) NULL)
  if (is.null(fecha_d) || anyNA(fecha_d))
    stop("La fecha no es v\u00e1lida. Use el formato AAAA-MM-DD.", call. = FALSE)

  num <- list(TOUT = TOUT, RH = RH, WSR = WSR, RAINF = RAINF, SR = SR, NO2 = NO2, CO = CO)
  for (v in names(num))
    if (!(is.numeric(num[[v]]) || all(is.na(num[[v]]))))
      stop("La variable ", v, " debe ser num\u00e9rica.", call. = FALSE)

  crudo <- tryCatch(
    data.frame(estacion = as.character(estacion), fecha = fecha_d,
               TOUT = TOUT, RH = RH, WSR = WSR, RAINF = RAINF, SR = SR, NO2 = NO2, CO = CO,
               stringsAsFactors = FALSE),
    error = function(e) stop("Las entradas tienen longitudes incompatibles.", call. = FALSE))

  if (any(!is.finite(unlist(crudo[VARS_CRUDAS]))))
    stop("Hay valores faltantes o infinitos en las variables num\u00e9ricas.", call. = FALSE)
  if (any(crudo$RH < 0 | crudo$RH > 100))
    stop("La humedad relativa (RH) debe estar entre 0 y 100 %.", call. = FALSE)
  for (v in c("WSR","RAINF","SR","NO2","CO"))
    if (any(crudo[[v]] < 0)) stop(v, " no puede ser negativa.", call. = FALSE)

  niv <- modelos$niveles
  mala <- setdiff(unique(crudo$estacion), niv$estacion)
  if (length(mala))
    stop("Estaci\u00f3n no reconocida: ", paste(mala, collapse = ", "),
         ". Estaciones con modelo: ", paste(niv$estacion, collapse = ", "), ".", call. = FALSE)

  lt  <- as.POSIXlt(crudo$fecha)
  mes <- lt$mon + 1L
  nd <- data.frame(
    estacion  = factor(crudo$estacion, levels = niv$estacion),
    fecha     = crudo$fecha,
    mes_lab   = factor(MESES_ES[mes], levels = MESES_ES),
    temporada = factor(TEMPORADAS[TEMP_DE_MES[mes]], levels = TEMPORADAS),
    dow       = factor(DOW_ES[((lt$wday + 6L) %% 7L) + 1L], levels = DOW_ES),
    doy       = lt$yday + 1L,
    TOUT = crudo$TOUT, RH = crudo$RH, WSR = crudo$WSR, RAINF = crudo$RAINF,
    SR = crudo$SR, NO2 = crudo$NO2, CO = crudo$CO,
    lluvia = log1p(crudo$RAINF), lNO2 = log1p(crudo$NO2), lCO = log1p(crudo$CO),
    stringsAsFactors = FALSE)
  for (v in VARS_X)
    nd[[paste0(v, "_z")]] <- (nd[[v]] - as.numeric(modelos$mu[[v]])) / as.numeric(modelos$sdv[[v]])

  attr(nd, "rango") <- evaluar_rango(crudo, modelos$rangos)
  nd
}

# ---- Predicciones ----------------------------------------------------------------
# GAM: media esperada (µg/m3) e IC 95 % aproximado del valor esperado (escala del enlace)
predecir_gam <- function(modelos, nd, cual = "G2") {
  mod <- modelos$gam[[cual]]
  p <- tryCatch(predict(mod, newdata = nd, type = "link", se.fit = TRUE, discrete = FALSE),
                error = function(e) predict(mod, newdata = nd, type = "link", se.fit = TRUE))
  fit <- as.numeric(p$fit); se <- as.numeric(p$se.fit)
  data.frame(fit = exp(fit), li = exp(fit - 1.96 * se), ls = exp(fit + 1.96 * se), se_link = se)
}

lineal_manual <- function(m2, nd) {
  tt <- delete.response(terms(formula(m2)))
  X  <- model.matrix(tt, model.frame(tt, nd, na.action = na.pass), contrasts.arg = m2$contrasts)
  as.numeric(X %*% coef(m2))
}

predecir_m2 <- function(modelos, nd) {
  p <- tryCatch(as.numeric(predict(modelos$m2, newdata = nd)), error = function(e) NULL)
  if (is.null(p) || length(p) != nrow(nd) || anyNA(p)) p <- lineal_manual(modelos$m2, nd)
  exp(p) * modelos$smear_m2
}

predecir_rf <- function(modelos, nd) {
  p <- predict(modelos$rf, data = nd, num.threads = 1)$predictions
  exp(as.numeric(p)) * modelos$smear_rf
}

prob_gee_manual <- function(gee, nd) {
  tt <- delete.response(terms(gee))
  mf <- model.frame(tt, data = nd, xlev = gee$xlevels, na.action = na.pass)
  X  <- model.matrix(tt, mf, contrasts.arg = gee$contrasts)
  b  <- coef(gee)
  if (!identical(colnames(X), names(b))) stop("Columnas de la matriz de diseño distintas a los coeficientes del GEE.", call. = FALSE)
  as.numeric(plogis(X %*% b))
}

predecir_gee <- function(modelos, nd) {
  p <- tryCatch(as.numeric(predict(modelos$gee, newdata = nd, type = "response")),
                error = function(e) NULL)
  if (is.null(p) || length(p) != nrow(nd) || anyNA(p)) p <- prob_gee_manual(modelos$gee, nd)
  p
}

predecir_modelos <- function(modelos, nd) {
  g2 <- predecir_gam(modelos, nd, "G2"); g1 <- predecir_gam(modelos, nd, "G1")
  p  <- predecir_gee(modelos, nd)
  data.frame(pm25_g2 = g2$fit, g2_li = g2$li, g2_ls = g2$ls,
             pm25_g1 = g1$fit, pm25_m2 = predecir_m2(modelos, nd), pm25_rf = predecir_rf(modelos, nd),
             p_critico = p, critico = p >= modelos$umbral_prob)
}