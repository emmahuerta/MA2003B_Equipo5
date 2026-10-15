# inspeccionar_rds.R  -- correr desde la raíz del proyecto
ruta <- "quarto/salidas/etapa3_modelos.rds"
stopifnot("No existe el rds" = file.exists(ruta))

pk <- c("mgcv", "nlme", "geepack", "ranger")
faltan <- setdiff(pk, rownames(installed.packages()))
if (length(faltan)) install.packages(faltan)
invisible(lapply(pk, function(p) suppressPackageStartupMessages(library(p, character.only = TRUE))))

m <- readRDS(ruta)

cat("== 1. Elementos ==\n")
for (n in names(m)) cat(sprintf("%-16s %s\n", n, paste(class(m[[n]]), collapse = " / ")))

req <- c("gam", "m2", "rf", "gee", "mu", "sdv", "vars_x", "smear_m2", "smear_rf",
         "umbral_prob", "umbral_critico", "est_c", "pca", "grupos", "cat_est",
         "validacion", "importancia_gam", "nivel_ajustado", "rangos")
cat("\nElementos requeridos que faltan:", if (length(setdiff(req, names(m)))) setdiff(req, names(m)) else "ninguno", "\n")
cat("usa_wd:", m$usa_wd, "\n")

cat("\n== 2. GAM ==\n")
for (nm in names(m$gam)) {
  g <- m$gam[[nm]]
  cat(nm, "| clase:", class(g)[1], "| familia:", g$family$family, g$family$link,
      "| modo:", g$modo, "| rho:", round(g$rho_ar1, 3), "\n  fórmula:", deparse(formula(g), width.cutoff = 200), "\n")
}
cat("Niveles estacion (GAM G2):", levels(m$gam$G2$model$estacion), "\n")
cat("Niveles dow (GAM G2):     ", levels(m$gam$G2$model$dow), "\n")
cat("Rango de doy en G2:", range(m$gam$G2$model$doy), "\n")

cat("\n== 3. M2 (gls) ==\n")
cat("Fórmula:", deparse(formula(m$m2), width.cutoff = 200), "\n")
cat("Correlación:", class(m$m2$modelStruct$corStruct)[1], "| phi:", round(coef(m$m2$modelStruct$corStruct, unconstrained = FALSE), 3), "\n")
cat("Contrastes guardados:", names(m$m2$contrasts), "\n")
cat("smear_m2:", m$smear_m2, "\n")

cat("\n== 4. Bosque aleatorio ==\n")
cat("Variables:", m$rf$forest$independent.variable.names, "\n")
cat("Niveles de factores en el bosque:\n"); print(m$rf$forest$covariate.levels)
cat("smear_rf:", m$smear_rf, "\n")

cat("\n== 5. GEE ==\n")
cat("Fórmula:", deparse(formula(m$gee), width.cutoff = 200), "\n")
cat("xlevels:\n"); print(m$gee$xlevels)
cat("umbral_prob:", m$umbral_prob, "| umbral_critico:", m$umbral_critico, "\n")

cat("\n== 6. Estandarización y rangos ==\n")
cat("vars_x:", m$vars_x, "\n"); print(m$mu); print(m$sdv)
print(as.data.frame(m$rangos), digits = 5)

cat("\n== 7. Catálogos ==\n")
cat("est_c:", as.character(m$est_c), "\n")
print(m$grupos); print(as.data.frame(m$cat_est))
cat("\nvalidacion:\n"); print(head(as.data.frame(m$validacion), 12))
cat("\nimportancia_gam:\n"); print(head(as.data.frame(m$importancia_gam), 12))
cat("\nnivel_ajustado:\n"); print(as.data.frame(m$nivel_ajustado), digits = 4)
cat("\nTamaño del rds (MB):", round(file.size(ruta) / 1e6, 1), "\n")