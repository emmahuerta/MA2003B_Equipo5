# app/R/mod_estaciones.R
mod_estaciones_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("aviso_boot")),
    card(card_header("Mapa de estaciones por grupo de exposición"),
         leafletOutput(ns("mapa"), height = "480px"),
         card_footer("Sin conexión a internet los mosaicos del mapa no cargan, pero las estaciones y sus etiquetas se siguen mostrando.")),
    card(card_header("Grupos de exposición (Ward, 2021–2023)"), tableOutput(ns("tabla_grupos")),
         helpText("Los grupos difieren sobre todo en nivel, no en zona ni en elevación. La estructura es débil a moderada (silueta ≈ 0.44); el grupo 2 es más difuso.")),
    card(card_header("Nivel de PM2.5 por estación, 2021–2023"), DT::DTOutput(ns("tabla"))),
    card(card_header("Nivel con y sin ajuste meteorológico"),
         plotOutput(ns("grafica"), height = "480px"), uiOutput(ns("texto_ajuste"))))
}

mod_estaciones_server <- function(id, m, ruta_boot = NULL) {
  moduleServer(id, function(input, output, session) {
    boot <- leer_bootstrap(ruta_boot)
    tb <- tryCatch(tabla_estaciones(m, boot), error = function(e) NULL)

    output$aviso_boot <- renderUI({
      if (is.null(tb)) div(class = "alert alert-danger", role = "alert", "No se pudo construir la tabla de estaciones desde el rds.")
      else if (is.null(boot)) div(class = "alert alert-info", role = "status",
        "No se encontró 'etapa3_orden_bootstrap.csv' (opcional): se omiten los intervalos bootstrap y los porcentajes sobre OMS y NOM.")
    })

    output$mapa <- leaflet::renderLeaflet({
      req(tb)
      leaflet(tb, options = leafletOptions(minZoom = 8)) |>
        addTiles() |>
        addCircleMarkers(lng = ~lon, lat = ~lat, radius = 14, color = "black", weight = 2,
                         fillColor = ~col, fillOpacity = 0.92, popup = popup_estacion(tb),
                         label = paste0(tb$estacion, " · G", tb$grupo),
                         labelOptions = labelOptions(noHide = TRUE, direction = "top", offset = c(0, -12),
                                                     style = list("font-weight" = "bold", "font-size" = "13px"))) |>
        addLegend(position = "bottomright", colors = unname(COLORES_GRUPO), opacity = 1,
                  labels = c("Grupo 1 (nivel bajo de PM10)", "Grupo 2 (intermedio, más difuso)", "Grupo 3 (PM2.5 más alto)"),
                  title = "Grupo de exposición") |>
        fitBounds(min(tb$lon) - 0.03, min(tb$lat) - 0.03, max(tb$lon) + 0.03, max(tb$lat) + 0.03)
    })

    output$tabla_grupos <- renderTable({ req(tb); tabla_grupos(tb) }, striped = TRUE)

    output$tabla <- DT::renderDT({
      req(tb)
      v <- data.frame(`Estación` = tb$estacion, Grupo = tb$grupo, Zona = tb$zona, msnm = tb$msnm,
                      `PM2.5 observado (µg/m³)` = round(tb$obs, 1), check.names = FALSE)
      if (!is.null(boot)) {
        v$`IC 95 % bootstrap` <- sprintf("%.1f–%.1f", tb$obs_li, tb$obs_ls)
        v$`Rango (IC)` <- sprintf("%.0f (%.0f–%.0f)", tb$rango_obs, tb$rango_li, tb$rango_ls)
        v$`% días sobre guía OMS` <- round(tb$pct_oms, 1)
        v$`% días sobre NOM (IC)` <- sprintf("%.1f (%.1f–%.1f)", tb$pct_nom, tb$nom_li, tb$nom_ls)
      }
      v$`Sin ajuste (G0)` <- round(tb$g0, 1); v$`Ajustado (G1)` <- round(tb$g1, 1)
      v$`IC 95 % G1` <- sprintf("%.1f–%.1f", tb$g1_li, tb$g1_ls)
      v$`Cambio por ajuste (%)` <- round(tb$cambio, 1)
      v$`Rango G0 → G1` <- sprintf("%.0f → %.0f", tb$rango_g0, tb$rango_g1)
      DT::datatable(v, rownames = FALSE, options = list(dom = "t", pageLength = 12, scrollX = TRUE),
                    caption = "Rango: 1 = menor nivel. % sobre la NOM = sobre el límite vigente cada año (45, 41 y 41 µg/m³ en 2021–2023).")
    })

    output$grafica <- renderPlot({
      req(tb)
      tb$estacion_f <- factor(tb$estacion, levels = tb$estacion[order(tb$g1)])
      ggplot(tb, aes(y = estacion_f)) +
        geom_vline(xintercept = LIM_OMS_PM25, linetype = "dashed", colour = "grey30") +
        geom_segment(aes(x = g0, xend = g1, yend = estacion_f), colour = "grey40",
                     arrow = grid::arrow(length = grid::unit(0.12, "inches"))) +
        geom_errorbar(aes(xmin = g1_li, xmax = g1_ls), width = 0.25, orientation = "y", colour = "#D55E00") +
        geom_point(aes(x = g0, colour = "Sin ajuste (G0)", shape = "Sin ajuste (G0)"), size = 3.8) +
        geom_point(aes(x = g1, colour = "Ajustado por meteorología (G1)", shape = "Ajustado por meteorología (G1)"), size = 3.8) +
        geom_text(aes(x = pmax(g0, g1_ls) + 0.5, label = sprintf("%+.0f %%", cambio)), hjust = 0, size = 4) +
        scale_colour_manual(values = c("Sin ajuste (G0)" = "#009E73", "Ajustado por meteorología (G1)" = "#D55E00"), name = NULL) +
        scale_shape_manual(values = c("Sin ajuste (G0)" = 17, "Ajustado por meteorología (G1)" = 16), name = NULL) +
        scale_x_continuous(expand = expansion(mult = c(0.03, 0.12))) +
        labs(x = "PM2.5 medio (µg/m³)", y = NULL,
             subtitle = sprintf("Flecha: de G0 a G1. Línea discontinua: guía OMS (%g). Barra: IC 95 %% de G1.", LIM_OMS_PM25)) +
        theme_minimal(base_size = 14) + theme(legend.position = "top")
    }, alt = "Nivel de PM2.5 por estación con y sin ajuste meteorológico; flechas muestran el cambio al igualar la meteorología.")

    output$texto_ajuste <- renderUI({
      req(tb); partes <- character(0)
      s <- tb[tb$estacion == "SE2", ]
      if (nrow(s)) partes <- c(partes, sprintf("<b>SE2</b> baja de %.1f a %.1f µg/m³ (%+.0f %%): una parte importante de su mala exposición se explica por su meteorología local (dispersión).", s$g0, s$g1, s$cambio))
      s <- tb[tb$estacion == "SO", ]
      if (nrow(s)) partes <- c(partes, sprintf("<b>SO</b> sube de %.1f a %.1f µg/m³ (%+.0f %%): su exceso no es meteorológico y apunta a fuentes cercanas o a topografía, por evaluar en la siguiente etapa.", s$g0, s$g1, s$cambio))
      HTML(paste0(paste0("<p>", partes, "</p>", collapse = ""),
        "<p class='text-muted'>G1 = modelo aditivo con calendario y meteorología (sin NO2 ni CO, que son indicadores de fuentes); a cada estación se le imponen los días de todas las estaciones en 2021–2023. Los intervalos de G1 no incluyen la correlación entre estaciones vecinas.</p>"))
    })
  })
}