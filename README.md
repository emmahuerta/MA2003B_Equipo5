# MA2003B · Equipo 5 

## Orden de ejecución

| Paso | Documento | Entrada | Salida |
|---|---|---|---|
| 1 | `1_ComprensionDeDatos.qmd` (Etapa 1: unión, limpieza, imputación, preparación) | Siete bases anuales del SIMA (`BD_2020.xlsx` ... `BD_2026.xlsx`), `Etiquetas.xlsx`, rangos del SIMA, ubicación de estaciones | `salidas/datos_modelo.rds` (base final: 552,503 filas, 13 estaciones, 2020-01-02 a 2025-12-31) |
| 2 | `2_ExploracionDescriptiva.qmd` (Etapa 2: exploración descriptiva) | `salidas/datos_modelo.rds` | `salidas/etapa2_datos_horarios.rds`, `etapa2_datos_diarios.rds`, `etapa2_datos_diarios.csv`, `etapa2_datos_diarios_comunes.rds`, `etapa2_catalogo_estaciones.rds`, `etapa2_perfil_estacional.rds` |
| 3 | `reporte_etapa2/etapa2.tex` (reporte) | Figuras en `reporte_etapa2/figuras/` | `etapa2.pdf` |

Ejecutar siempre los documentos en ese orden y desde la carpeta raíz del repositorio (los qmd leen y escriben en `salidas/`).
La base limpia en `.csv` de esta etapa es `salidas/etapa2_datos_diarios.csv` (promedios diarios por estación).

## Requisitos

- R 4.1 o superior (se usa el operador `|>` y funciones anónimas `\(x)`) y Quarto.
- Paquetes de la Etapa 2: `tidyverse`, `lubridate`, `scales`, `knitr`.
- Paquetes adicionales de la Etapa 1: `readxl`, `missForest`, `imputeTS`, `VIM`, `data.table`, `doParallel`, `mgcv`.
- Reporte: LaTeX (`pdflatex`) con los paquetes `babel` (español), `booktabs`, `tabularx`, `caption`, `fancyhdr`, `titlesec`, `enumitem`, `hyperref`.
