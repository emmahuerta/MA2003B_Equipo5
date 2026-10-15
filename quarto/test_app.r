# test_app.R  -- correr desde la raíz del proyecto
pk <- c("mgcv", "nlme", "geepack", "ranger")
faltan <- setdiff(pk, rownames(installed.packages()))
if (length(faltan)) install.packages(faltan)
invisible(lapply(pk, function(p) suppressPackageStartupMessages(library(p, character.only = TRUE))))
options(width = 220)

source("quarto/app/R/utils_modelos.R", encoding = "UTF-8")
m <- cargar_modelos("quarto/salidas/etapa3_modelos.rds")
TOL <- 1e-8

cat("R:", R.version.string, "\n")
cat("Paquetes:", paste(pk, sapply(pk, function(p) as.character(packageVersion(p))), collapse = " | "), "\n\n")

# ---- (b) Escenarios -----------------------------------------------------------
rg  <- as.data.frame(m$rangos); rownames(rg) <- rg$variable
q   <- function(col) setNames(rg[VARS_CRUDAS, col], VARS_CRUDAS)
p01 <- q("p01"); p50 <- q("p50"); p99 <- q("p99"); mn <- q("min"); mx <- q("max")
stopifnot(all(c("SE2","SUR","SO","NE2","NO","CE","NTE2","SE3","SE") %in% m$est_c))

mk <- function(nombre, estacion, fecha, v)
  data.frame(nombre = nombre, estacion = estacion, fecha = as.Date(fecha), t(v[VARS_CRUDAS]),
             stringsAsFactors = FALSE)
estanc <- p50; estanc[c("WSR","NO2","CO","TOUT")] <- c(p01["WSR"], p99["NO2"], p99["CO"], p01["TOUT"]); estanc["RAINF"] <- 0
disper <- p50; disper[c("TOUT","WSR","NO2","CO","RH","SR")] <- c(p99["TOUT"], p99["WSR"], p01["NO2"], p01["CO"], p01["RH"], p99["SR"]); disper["RAINF"] <- 0
fuera  <- p50; fuera[c("NO2","TOUT","CO")] <- c(mx["NO2"] * 1.5, mn["TOUT"] - 10, mx["CO"] * 2)

esc <- rbind(
  mk("1 mediana, SE2, verano",          "SE2",  "2023-06-15", p50),
  mk("2 mediana, SUR, invierno 2024",   "SUR",  "2024-01-10", p50),
  mk("3 estancamiento invernal, SO",    "SO",   "2022-12-20", estanc),
  mk("4 dispersi\u00f3n c\u00e1lida, NE2",  "NE2",  "2023-05-02", disper),
  mk("5 m\u00e1ximos de entrenamiento, NO", "NO",   "2021-09-09", mx),
  mk("6 m\u00ednimos de entrenamiento, CE", "CE",   "2025-03-03", mn),
  mk("7 bisiesto 29-feb, NTE2",         "NTE2", "2024-02-29", p50),
  mk("8 31-dic bisiesto (doy 366), SE", "SE",   "2024-12-31", p50),
  mk("9 FUERA DE RANGO, SE3",           "SE3",  "2025-12-31", fuera))

# ---- (c) Construcción a mano + predict() directo (independiente de utils) --------
MES  <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
DIA  <- c("Lun","Mar","Mi\u00e9","Jue","Vie","S\u00e1b","Dom")
TEM  <- c("invierno","primavera","verano","oto\u00f1o")
TEM_IDX <- c(1,1,2,2,2,3,3,3,4,4,4,1)

datos_a_mano <- function(r) {
  f <- as.Date(r$fecha); mes <- as.integer(format(f, "%m"))
  nd <- data.frame(
    estacion  = factor(r$estacion, levels = levels(m$gam$G2$model$estacion)),
    dow       = factor(DIA[as.integer(format(f, "%u"))], levels = DIA),
    doy       = as.integer(format(f, "%j")),
    mes_lab   = factor(MES[mes], levels = MES),
    temporada = factor(TEM[TEM_IDX[mes]], levels = TEM),
    TOUT = r$TOUT, RH = r$RH, WSR = r$WSR, SR = r$SR,
    lluvia = log1p(r$RAINF), lNO2 = log1p(r$NO2), lCO = log1p(r$CO))
  nd$TOUT_z   <- (nd$TOUT   - m$mu$TOUT)   / m$sdv$TOUT
  nd$RH_z     <- (nd$RH     - m$mu$RH)     / m$sdv$RH
  nd$WSR_z    <- (nd$WSR    - m$mu$WSR)    / m$sdv$WSR
  nd$lluvia_z <- (nd$lluvia - m$mu$lluvia) / m$sdv$lluvia
  nd$SR_z     <- (nd$SR     - m$mu$SR)     / m$sdv$SR
  nd$lNO2_z   <- (nd$lNO2   - m$mu$lNO2)   / m$sdv$lNO2
  nd$lCO_z    <- (nd$lCO    - m$mu$lCO)    / m$sdv$lCO
  nd
}
gam_dir <- function(mod, nd) {
  p <- tryCatch(predict(mod, nd, type = "link", se.fit = TRUE, discrete = FALSE),
                error = function(e) predict(mod, nd, type = "link", se.fit = TRUE))
  c(fit = exp(as.numeric(p$fit)), li = exp(as.numeric(p$fit) - 1.96 * as.numeric(p$se.fit)),
    ls = exp(as.numeric(p$fit) + 1.96 * as.numeric(p$se.fit)),
    resp = as.numeric(predict(mod, nd, type = "response")))
}
directo <- function(nd) {
  g2 <- gam_dir(m$gam$G2, nd); g1 <- gam_dir(m$gam$G1, nd)
  c(pm25_g2 = g2[["fit"]], g2_li = g2[["li"]], g2_ls = g2[["ls"]], g2_resp = g2[["resp"]],
    pm25_g1 = g1[["fit"]],
    pm25_m2 = exp(as.numeric(predict(m$m2, nd))) * m$smear_m2,
    pm25_rf = exp(as.numeric(predict(m$rf, nd, num.threads = 1)$predictions)) * m$smear_rf,
    p_critico = as.numeric(predict(m$gee, nd, type = "response")))
}

# ---- Corrida ----------------------------------------------------------------------
cols_cmp <- c("pm25_g2","g2_li","g2_ls","pm25_g1","pm25_m2","pm25_rf","p_critico")
orden <- c(dentro = 1, extremo = 2, fuera = 3)
filas <- lapply(seq_len(nrow(esc)), function(i) {
  r  <- esc[i, ]
  nd <- preparar_entrada(m, r$estacion, r$fecha, r$TOUT, r$RH, r$WSR, r$RAINF, r$SR, r$NO2, r$CO)
  pa <- predecir_modelos(m, nd)
  nh <- datos_a_mano(r); ph <- directo(nh)

  # diferencia en los datos de entrada (columnas comunes)
  com <- intersect(names(nh), names(nd))
  dif_in <- max(sapply(com, function(cn)
    if (is.numeric(nh[[cn]])) abs(nh[[cn]] - nd[[cn]])
    else as.numeric(!identical(as.character(nh[[cn]]), as.character(nd[[cn]])) |
                    !identical(levels(nh[[cn]]), levels(nd[[cn]])))))
  dif_pred <- max(abs(unlist(pa[cols_cmp]) - ph[cols_cmp]))
  dif_resp <- abs(ph[["pm25_g2"]] - ph[["g2_resp"]])      # exp(enlace) vs type = "response"

  ev  <- attr(nd, "rango")
  peor <- names(orden)[max(orden[ev$estado])]
  ok_pos  <- all(is.finite(unlist(pa[cols_cmp]))) && all(unlist(pa[c("pm25_g2","pm25_g1","pm25_m2","pm25_rf")]) > 0)
  ok_prob <- pa$p_critico >= 0 && pa$p_critico <= 1
  ok_ic   <- pa$g2_li <= pa$pm25_g2 && pa$pm25_g2 <= pa$g2_ls
  ok_cls  <- identical(pa$critico, pa$p_critico >= m$umbral_prob)
  ok <- dif_in < TOL && dif_pred < TOL && dif_resp < 1e-6 && ok_pos && ok_prob && ok_ic && ok_cls
  data.frame(escenario = r$nombre, rango = peor,
             G2 = pa$pm25_g2, IC95 = sprintf("%.1f-%.1f", pa$g2_li, pa$g2_ls),
             G1 = pa$pm25_g1, M2 = pa$pm25_m2, RF = pa$pm25_rf,
             p_crit = pa$p_critico, critico = pa$critico,
             dif_entrada = dif_in, dif_pred = dif_pred, ok = ok, stringsAsFactors = FALSE)
})
res <- do.call(rbind, filas)

cat("== Resultados (PM2.5 en \u00b5g/m3) ==\n")
print(res, digits = 4, row.names = FALSE)

# ---- Verificaciones adicionales ------------------------------------------------------
cat("\n== Comprobaciones adicionales ==\n")
cat("Estancamiento (3) > dispersi\u00f3n (4) en G2:", res$G2[3] > res$G2[4], "(esperado TRUE)\n")
cat("p_critico estancamiento > dispersi\u00f3n:    ", res$p_crit[3] > res$p_crit[4], "(esperado TRUE)\n")
cat("Umbral de probabilidad:", round(m$umbral_prob, 4), "| umbral cr\u00edtico:", m$umbral_critico, "\n")
cat("Niveles de factores del bosque coinciden con las constantes:\n")
lv <- m$rf$forest$covariate.levels
if (is.null(lv)) cat("  (el bosque no guard\u00f3 covariate.levels; revisar con inspeccionar_rds.R)\n") else {
  if (!is.null(lv$mes_lab)) cat("  mes_lab:", identical(as.character(lv$mes_lab), MES), "\n")
  if (!is.null(lv$estacion)) cat("  estacion:", setequal(lv$estacion, m$niveles$estacion), "\n")
}
if (!is.null(m$gee$xlevels$temporada)) cat("  temporada (GEE):", identical(m$gee$xlevels$temporada, TEM), "\n")

cat("\n== Manejo de errores (deben fallar con mensaje claro) ==\n")
casos <- list(
  "estaci\u00f3n inexistente" = quote(preparar_entrada(m, "XX", "2023-01-01", 20, 50, 8, 0, 0.15, 13, 1.4)),
  "valor NA"             = quote(preparar_entrada(m, "SE2", "2023-01-01", NA, 50, 8, 0, 0.15, 13, 1.4)),
  "RH = 120"             = quote(preparar_entrada(m, "SE2", "2023-01-01", 20, 120, 8, 0, 0.15, 13, 1.4)),
  "lluvia negativa"      = quote(preparar_entrada(m, "SE2", "2023-01-01", 20, 50, 8, -1, 0.15, 13, 1.4)),
  "fecha inv\u00e1lida"      = quote(preparar_entrada(m, "SE2", "no-es-fecha", 20, 50, 8, 0, 0.15, 13, 1.4)))
for (nm in names(casos)) {
  msg <- tryCatch({ eval(casos[[nm]]); "NO FALL\u00d3 (revisar)" }, error = function(e) conditionMessage(e))
  cat(sprintf("  %-20s -> %s\n", nm, msg))
}

cat("\n== Mensajes de rango del escenario 9 ==\n")
nd9 <- preparar_entrada(m, esc$estacion[9], esc$fecha[9], esc$TOUT[9], esc$RH[9], esc$WSR[9],
                        esc$RAINF[9], esc$SR[9], esc$NO2[9], esc$CO[9])
cat(mensajes_rango(attr(nd9, "rango"), m$rangos), sep = "\n")

cat("\nRESULTADO GLOBAL:", if (all(res$ok)) "TODO OK" else "REVISAR filas con ok = FALSE", "\n")
dir.create("salidas", showWarnings = FALSE)
write.csv(res, "salidas/test_app_resultados.csv", row.names = FALSE)