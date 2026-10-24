# MA2003B · Equipo 5

**Exposición a PM2.5 y PM10 en la red del SIMA, 2021–2025: comparación entre estaciones, tipologías de exposición y asociación con la meteorología del mismo día.**

Reto *Datos en el Aire* del curso Aplicación de métodos multivariados en ciencia de datos (Tecnológico de Monterrey), con el Sistema Integral de Monitoreo Ambiental (SIMA) de Nuevo León como socio formador. El proyecto sigue la metodología CRISP-DM.

Equipo: Leonardo Leal Mendoza (A01199225), Alan Robles Gracia (A00841816), Emma Huerta Villafuerte (A01723584), Andrea Mestizo Hernández (A01735773).

## Lenguajes

- **R (4.5.0) con Quarto**: todo el análisis estadístico (pasos 1 a 5) y la interfaz gráfica en Shiny.
- **Python**: solo `data/Juntardatos.ipynb`, que une las bases anuales en un solo archivo (paso 0).

## Estructura del repositorio

```
.
├── data/                      Datos del socio formador (entradas)
│   ├── BD 2020.xlsx ... BD 2026.xlsx       Bases anuales (una hoja por estación)
│   ├── BD_2020_2026_concatenada.xlsx       Base unida (salida del paso 0)
│   ├── Juntardatos.ipynb                   Paso 0 (Python)
│   ├── Etiquetas.xlsx                      Unidades y descripción de variables
│   ├── Rangos de los parámetros del SIMA.pdf
│   └── Ubicación de las estaciones de monitoreo.docx
├── quarto/                    Código del análisis (directorio de trabajo)
│   ├── 1.ComprensionDeDatos.qmd ... 5.AjustesInformeFinal.qmd
│   ├── app/                   Interfaz Shiny (R/ con módulos, www/ con estilos)
│   ├── salidas/               Bases, tablas (.csv) y modelos (.rds) que generan los qmd
│   ├── presentacion/          Figuras y tablas de la presentación final (paso 4)
│   └── informe/figuras/       Figuras nuevas del informe final (paso 5)
├── docs/quarto/               Versiones ejecutadas (HTML) de cada qmd
├── renv.lock, renv/           Versiones exactas de los paquetes de R
└── Dockerfile                 Entorno reproducible
```

## Orden de ejecución

Todos los qmd se ejecutan **con `quarto/` como directorio de trabajo**: leen los datos de `../data/` y escriben en `salidas/` (es decir, `quarto/salidas/`). Cada paso usa las salidas del anterior.

| Paso | Documento | Qué hace | Entrada | Salida principal |
|---|---|---|---|---|
| 0 | `data/Juntardatos.ipynb` (Python) | Une las siete bases anuales | `data/BD 2020.xlsx` ... `BD 2026.xlsx` | `data/BD_2020_2026_concatenada.xlsx` |
| 1 | `1.ComprensionDeDatos.qmd` | Etapa 1: lectura, limpieza, imputación y preparación | Base concatenada, `Etiquetas.xlsx`, rangos y ubicación del SIMA | `salidas/datos_modelo.rds` (552,503 filas, 13 estaciones, 2020-01-02 a 2025-12-31); caché de imputación en `salidas/cache_meteo/` |
| 2 | `2.ExploracionDescriptiva.qmd` | Etapa 2: bases horaria y diaria y exploración descriptiva | `salidas/datos_modelo.rds` | `salidas/etapa2_datos_horarios.rds`, `etapa2_datos_diarios.rds`, **`etapa2_datos_diarios.csv`** (base limpia), `etapa2_datos_diarios_comunes.rds`, `etapa2_catalogo_estaciones.rds` |
| 3 | `3.Modelado.qmd` | Etapa 3: PCA y conglomerados, modelos lineales con AR(1), GAM, bosque aleatorio y logística con GEE; validación 2024–2025 | `salidas/etapa2_*.rds` | `salidas/etapa3_*.csv`, `salidas/etapa3_modelos.rds`, `salidas/etapa3_insumos_presentacion.rds` |
| 4 | `4.Presentacion.qmd` | Figuras, mapas, índice integral y GAM de PM10 para la presentación | `salidas/etapa3_*` | `presentacion/figuras/`, `presentacion/tablas/`, `salidas/etapa4_ranking_integral.csv`, `salidas/etapa4_gam_pm10.rds` |
| 5 | `5.AjustesInformeFinal.qmd` | Comprobaciones del informe final: ACF residual, largo de bloque, interacción estación × mes, métricas de validación con IC, linealidad en el logit, GEE con pocos conglomerados, PM10 sin winsorizar, cumplimiento de la NOM y sensibilidad a la imputación | `salidas/etapa2_*`, `salidas/etapa3_*`, base concatenada | `salidas/etapa4_*.csv`, `informe/figuras/` |
| — | `app/` | Interfaz gráfica (Shiny) que usa los modelos guardados | `salidas/etapa3_modelos.rds` y salidas del paso 4 | Aplicación interactiva |

Notas:

- El paso 1 es el más lento (imputación con bosque aleatorio por estación y año). Los resultados intermedios se guardan en `salidas/cache_meteo/`; si existen, no se recalculan.
- Semilla `2026` en todos los documentos; los bootstrap usan 500 réplicas (parámetro `B_boot`, se puede bajar a 200 para una prueba rápida).
- `4.Presentacion.qmd` descarga mosaicos de OpenStreetMap para los mapas; sin internet, cambiar `usar_mapa_base <- FALSE`.
- En el paso 5, `glmtoolbox` es opcional (estimador de Mancl–DeRouen).

## Cómo reproducir

1. Clonar el repositorio y abrir `MA2003B_Blank.Rproj`.
2. Restaurar los paquetes con `renv::restore()` (o construir la imagen con el `Dockerfile`).
3. Si no existe `data/BD_2020_2026_concatenada.xlsx`, ejecutar `data/Juntardatos.ipynb`.
4. Desde `quarto/`, renderizar en orden:

```bash
cd quarto
quarto render 1.ComprensionDeDatos.qmd
quarto render 2.ExploracionDescriptiva.qmd
quarto render 3.Modelado.qmd
quarto render 4.Presentacion.qmd
quarto render 5.AjustesInformeFinal.qmd
```

5. Para la interfaz, abrir `quarto/app/R/app.r` en RStudio y pulsar **Run App**.

## Requisitos

- R 4.1 o superior (se usan `|>` y funciones anónimas `\(x)`; el análisis se corrió con R 4.5.0) y Quarto.
- Paquetes por paso (todos fijados en `renv.lock`):

| Paso | Paquetes |
|---|---|
| 1 | `tidyverse`, `lubridate`, `readxl`, `data.table`, `missForest`, `imputeTS`, `VIM`, `doParallel`, `mgcv`, `scales`, `knitr` |
| 2 | `tidyverse`, `lubridate`, `scales`, `knitr` |
| 3 | `nlme`, `emmeans`, `car`, `geepack`, `pROC`, `ranger`, `cluster`, `mgcv` |
| 4 | `sf`, `ggrepel`, `patchwork`, `gt`, `webshot2`, `ggspatial`, `rosm`, `prettymapr`, `mgcv`, `pROC` |
| 5 | `nlme`, `emmeans`, `geepack`, `ranger`, `mgcv`, `splines`, `np`, `readxl`, `patchwork` (opcional: `glmtoolbox`) |
| App | `shiny` y los paquetes de los modelos (ver `quarto/app/R/global.r`) |

- Python 3 con `pandas` y `openpyxl` para el paso 0.

## Informes

Los informes de cada etapa se escribieron en LaTeX (`pdflatex` + `biber`, `biblatex` con estilo APA en español). Las figuras que usan provienen de `quarto/presentacion/figuras/` y `quarto/informe/figuras/`.
