# app/R/mod_acerca.R
mod_acerca_ui <- function(id) {
  ref <- function(...) tags$p(class = "referencia", ...)
  tagList(
    card(card_header("Objetivo del proyecto"),
      p("Caracterizar y explicar la exposición a PM2.5 y PM10 en la red del SIMA de la Zona Metropolitana de Monterrey (12 estaciones; ajuste en 2021–2023 y validación temporal en 2024–2025), y cuantificar cuánto de la variación diaria y de la probabilidad de un día crítico se asocia con la meteorología, el NO2 y el CO. Reto de «Aplicación de métodos multivariados en ciencia de datos» (Tecnológico de Monterrey, Gpo 201, Equipo 5), con el SIMA como socio formador.")),
    card(card_header("Datos"),
      p("Promedios diarios de 24 h con al menos 18 h válidas (criterio de la NOM-025-SSA1-2021; Secretaría de Salud, 2021), construidos a partir de 552,503 registros horarios de 13 estaciones (2020–2025). El ajuste usa 2021–2023 (12 estaciones × 1,095 días = 13,140 estación-días) y la validación 2024–2025 (7,234 estación-días). Se partió en el tiempo, no al azar, porque los días vecinos son dependientes.")),
    card(card_header("Modelos que usa la interfaz"),
      tags$ul(
        tags$li(tags$b("GAM G2"), " (modelo principal): familia Gamma con enlace log y errores AR(1); calendario, meteorología, NO2 y CO como curvas suaves (Wood, 2017), con el enfoque de AirGAM (Walker et al., 2023). ", tags$b("G1"), ": igual, sin NO2 ni CO."),
        tags$li(tags$b("Regresión lineal M2"), ": sobre log(PM2.5) con errores AR(1); se retransforma con un factor de corrección."),
        tags$li(tags$b("Bosque aleatorio"), ": referencia no lineal (Breiman, 2001)."),
        tags$li(tags$b("Regresión logística con GEE"), ": probabilidad de día crítico (PM2.5 > 33 µg/m³), con correlación AR(1) y errores robustos (Højsgaard et al., 2006; Liang & Zeger, 1986)."))),
    card(card_header("Qué dicen los resultados"),
      tags$ul(
        tags$li("Variables más importantes: NO2 y temperatura; luego humedad y viento. El PM2.5 baja hasta ~9 km/h de viento y después se aplana. La temperatura sube sobre ~20 °C, pero su concurvidad es alta (0.79): léase con cautela."),
        tags$li("Grupos de exposición: 1 (SUR, SE, NO), 2 (SE3, SO2, NO2, NTE2, NE, NE2, CE) y 3 (SO, SE2). Estabilidad alta en 1 y 3, media en 2."),
        tags$li("Ajuste por meteorología: SE2 baja de 28.0 a 21.7 µg/m³ (su exceso es en buena parte de dispersión local); SO sube de 26.1 a 28.4 (su exceso no es meteorológico)."),
        tags$li("Desempeño en validación (RMSE, µg/m³): M2 9.46, G1 9.20, G2 8.81, bosque aleatorio 8.46; meteorología, NO2 y CO explican ≈ 40 % de la varianza diaria fuera de muestra. Logística: AUC 0.82 (modelo base 0.74)."),
        tags$li("Referencias de contexto: Chetna et al. (2024) y Walker et al. (2023) siguen el mismo enfoque de GAM para separar el efecto del clima."))),
    card(card_header("Advertencias y límites de uso"),
      div(class = "alert alert-warning", role = "note", tags$ul(
        tags$li("Los modelos son ", tags$b("explicativos"), ": usan la meteorología y los gases del mismo día. No son pronósticos."),
        tags$li("Las asociaciones ", tags$b("no son causales"), "."),
        tags$li("No extrapolar fuera de los rangos de entrenamiento (la app avisa cuando un valor sale de p01–p99 o de mín–máx)."),
        tags$li("En 2025 el PM10 cae ~20 % y el CO ~33 % en toda la red sin cambio de clima: posible efecto de cambios de equipos (hipótesis pendiente de confirmar con el socio formador). Los resultados de 2025 son provisionales."),
        tags$li("El CO no tiene una relación estable entre periodos; no se interpreta como estable."),
        tags$li("El intervalo del GAM es del valor esperado, no de un día concreto (RMSE de validación de G2: 8.81 µg/m³)."))),
      tags$h5("Otras limitaciones del análisis"),
      tags$ul(
        tags$li("Solo hay tres años de ajuste y 12 estaciones: no se estima tendencia y la estructura de grupos es débil (silueta ≈ 0.44)."),
        tags$li("El 16.8 % del PM2.5 horario fue imputado (la prueba de sensibilidad resultó estable)."),
        tags$li("La estación NO2 se comporta distinto (autocorrelación residual y el peor R² de validación)."),
        tags$li("Faltan la dirección del viento y la altura de la capa límite, que son las variables más importantes en la literatura."))),
    card(card_header("Código y reproducibilidad"),
      p("Repositorio del Equipo 5: ", tags$a(href = "https://github.com/emmahuerta/MA2003B_Equipo5", target = "_blank", rel = "noopener",
                                              "github.com/emmahuerta/MA2003B_Equipo5"), ".")),
    card(card_header("Referencias (APA 7)"),
      ref("Breiman, L. (2001). Random forests. ", tags$em("Machine Learning, 45"), "(1), 5–32. https://doi.org/10.1023/A:1010933404324"),
      ref("Chetna, Dhaka, S. K., Walker, S.-E., Rawat, V., & Singh, N. (2024). Long-term meteorology-adjusted and unadjusted trends of PM2.5 using the AirGAM model over Delhi, 2007–2022. ", tags$em("Atmospheric Environment: X, 22"), ", Artículo 100255. https://doi.org/10.1016/j.aeaoa.2024.100255"),
      ref("Equipo 5. (2026). ", tags$em("MA2003B_Equipo5: código y datos derivados del reto de calidad del aire (SIMA)"), " [Repositorio de código]. GitHub. https://github.com/emmahuerta/MA2003B_Equipo5"),
      ref("Højsgaard, S., Halekoh, U., & Yan, J. (2006). The R package geepack for generalized estimating equations. ", tags$em("Journal of Statistical Software, 15"), "(2), 1–11. https://doi.org/10.18637/jss.v015.i02"),
      ref("Liang, K.-Y., & Zeger, S. L. (1986). Longitudinal data analysis using generalized linear models. ", tags$em("Biometrika, 73"), "(1), 13–22. https://doi.org/10.1093/biomet/73.1.13"),
      ref("Secretaría de Salud. (2021, 27 de octubre). ", tags$em("NOM-025-SSA1-2021, Salud ambiental. Criterio para evaluar la calidad del aire ambiente, con respecto a las partículas suspendidas PM10 y PM2.5"), ". Diario Oficial de la Federación."),
      ref("Walker, S.-E., Solberg, S., Schneider, P., & Guerreiro, C. (2023). The AirGAM 2022r1 air quality trend and prediction model. ", tags$em("Geoscientific Model Development, 16"), "(2), 573–595. https://doi.org/10.5194/gmd-16-573-2023"),
      ref("Wood, S. N. (2017). ", tags$em("Generalized additive models: An introduction with R"), " (2.ª ed.). Chapman and Hall/CRC. https://doi.org/10.1201/9781315370279"),
      ref("World Health Organization. (2021). ", tags$em("WHO global air quality guidelines"), " (WHO/HEP/ECH/AQG/2021.1). World Health Organization."),
      helpText("Los paquetes de R usados (shiny, bslib, mgcv, nlme, geepack, ranger, ggplot2, plotly, leaflet, DT) se agregarán a esta lista con la salida de citation() al cerrar el proyecto.")))
}