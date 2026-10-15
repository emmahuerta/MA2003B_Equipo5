# app/app.R

# ============================================================
# 1. Tema visual (sin descargar fuentes: funciona sin internet)
# ============================================================
tema <- bs_theme(
  version = 5,
  primary = "#206775",
  secondary = "#4D6470",
  bg = "#EEF3F5",
  fg = "#1F3641",
  base_font = font_collection("Inter", "Segoe UI", "-apple-system", "Helvetica Neue", "Arial", "sans-serif"),
  heading_font = font_collection("Inter", "Segoe UI", "-apple-system", "Helvetica Neue", "Arial", "sans-serif"),
  # Las tarjetas deben distinguirse del fondo de la página
  "card-bg" = "#FFFFFF",
  "card-border-color" = "#D3E0E6"
)

# ============================================================
# 2. Hoja de estilos incrustada en la página
#    (evita depender de la ruta /estilos.css y del caché del navegador;
#     si cambia el CSS, reinicie la app)
# ============================================================
cargar_estilos <- function() {
  candidatos <- c("quarto/app/www/estilos.css", "../www/estilos.css")
  f <- candidatos[file.exists(candidatos)]
  if (!length(f)) {
    message("[app] AVISO: no se encontró www/estilos.css (directorio de trabajo: ", getwd(),
            "). La app se verá sin estilos propios.")
    return(NULL)
  }
  tags$style(HTML(paste(readLines(f[1], warn = FALSE, encoding = "UTF-8"), collapse = "\n")))
}
ESTILOS <- cargar_estilos()

recursos <- tags$head(
  ESTILOS,
  tags$script("document.documentElement.lang = 'es';")
)

aviso_estilos <- if (is.null(ESTILOS))
  div(class = "alert alert-warning", role = "alert",
      tags$strong("Aviso: "), "no se encontró 'www/estilos.css'; la app se muestra sin su diseño propio.")

# ============================================================
# 3. Banner y pie de página
# ============================================================
banner_app <- div(
  class = "app-banner",
  div(class = "app-eyebrow", "Monitoreo ambiental · SIMA"),
  h2("Calidad del aire en la Zona Metropolitana de Monterrey"),
  p("Estimación, análisis y validación de PM2.5 con los modelos del reto de métodos multivariados."),
  div(class = "app-banner-chips",
      span("12 estaciones"), span("Ajuste 2021–2023"), span("Validación 2024–2025"),
      span("Modelos explicativos"))
)

pie_app <- div(
  class = "app-footer",
  tags$strong("Datos en el Aire"), " · Equipo 5 · Tecnológico de Monterrey · Socio formador: SIMA",
  tags$br(),
  "Herramienta explicativa: no es un pronóstico ni una evaluación formal de cumplimiento normativo."
)

# ============================================================
# 4. Interfaz
# ============================================================
if (is.null(MODELOS)) {

  ui <- fluidPage(
    theme = tema, recursos,
    div(class = "app-contenedor-error",
        banner_app,
        div(class = "alert alert-danger", role = "alert",
            tags$h4(class = "alert-heading", icon("triangle-exclamation"), " No se pudieron cargar los modelos"),
            p(ERROR_CARGA)),
        pie_app))
  server <- function(input, output, session) {}

} else {

  ui <- page_navbar(
    title = tagList(icon("wind"), span("Datos en el Aire")),
    id = "pestanas",
    theme = tema,
    window_title = "Datos en el Aire · SIMA ZMM",
    fillable = FALSE,
    header = tagList(recursos, aviso_estilos, banner_app),
    footer = pie_app,

    nav_panel(tagList(icon("sliders"), "Estimador de un día"), mod_estimador_ui("est", MODELOS)),
    nav_panel(tagList(icon("flask"), "Qué pasaría si…"),       mod_escenarios_ui("esc", MODELOS)),
    nav_panel(tagList(icon("map"), "Estaciones"),               mod_estaciones_ui("estac")),
    nav_panel(tagList(icon("chart-column"), "Validación"),      mod_validacion_ui("val", MODELOS)),
    nav_panel(tagList(icon("circle-info"), "Acerca de"),        mod_acerca_ui("acerca")),

    nav_spacer(),
    nav_item(tags$a(icon("github"), " Repositorio", class = "nav-link",
                    href = "https://github.com/emmahuerta/MA2003B_Equipo5",
                    target = "_blank", rel = "noopener"))
  )

  server <- function(input, output, session) {
    mod_estimador_server("est", MODELOS)
    mod_escenarios_server("esc", MODELOS)
    mod_estaciones_server("estac", MODELOS, RUTA_BOOT)
    mod_validacion_server("val", MODELOS, RUTA_DIARIOS)
  }
}

shinyApp(ui, server)