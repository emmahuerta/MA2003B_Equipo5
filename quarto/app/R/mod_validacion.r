# app/R/mod_validacion.R
ETIQ_VAR_GAM <- c(lNO2 = "NO2", TOUT = "Temperatura", RH = "Humedad relativa", WSR = "Viento",
                  SR = "Radiación solar", lluvia = "Lluvia", lCO = "CO")
REPORTE_LOGISTICA <- c(AUC = 0.82, Brier = 0.084, Sensibilidad = 0.66, Especificidad = 0.82, VPP = 0.34)  # Reporte Etapa 3, §4.5

mod_validacion_ui <- function(id, m) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-warning", role = "note",
        tags$strong("Nota sobre 2025. "),
        "En 2025 el PM10 cae ~20 % y el CO ~33 % en toda la red sin cambio de clima: posible efecto de cambios de equipos (hipótesis pendiente de confirmar con el socio formador). Como el CO es predictora de G2, M2 y la logística, los resultados de 2025 deben leerse como provisionales."),
    card(card_header("Métricas de validación temporal 2024–2025 (guardadas con los modelos)"),
         tableOutput(ns("tabla_global")), plotOutput(ns("grafica_rmse"), height = "260px"),
         helpText("Base: 7,234 estación-días de las 12 estaciones. RMSE, MAE y MB en µg/m³. FAC2: fracción de predicciones dentro de un factor de 2. COE: coeficiente de eficiencia.")),
    card(card_header("Importancia de las variables en el GAM G2"),
         layout_columns(col_widths = c(5, 7), tableOutput(ns("tabla_import")), plotOutput(ns("grafica_import"), height = "280px")),
         helpText("Aumento del error de validación al quitar cada variable. NO2 y temperatura dominan; el CO aporta poco al GAM y su relación no es estable entre periodos.")),
    card(card_header("Día crítico (PM2.5 > 33 µg/m³): regresión logística con GEE"),
         tableOutput(ns("tabla_logistica")),
         helpText("Umbral de probabilidad (Youden, entrenamiento) tomado del modelo guardado. La columna 'Recalculado' aparece al calcular la validación con la base diaria.")),
    card(card_header("Serie observada vs predicha por estación"),
         uiOutput(ns("aviso_diarios")),
         layout_columns(col_widths = c(4, 4, 4),
           actionButton(ns("cargar"), "Calcular validación con la base diaria", class = "btn btn-primary"),
           selectInput(ns("est_sel"), "Estación", choices = m$niveles$estacion, selected = "SE2"),
           selectInput(ns("mod_sel"), "Modelo", choices = names(MODELOS_VAL))),
         uiOutput(ns("resumen_calculo")),
         plotly::plotlyOutput(ns("serie"), height = "380px"),
         helpText("Zona sombreada: 2025 (provisional). Línea punteada: 33 µg/m³."),
         layout_columns(col_widths = c(6, 6), plotOutput(ns("dispersion"), height = "340px"), tableOutput(ns("tabla_anio"))),
         DT::DTOutput(ns("tabla_est"))))
}

mod_validacion_server <- function(id, m, ruta_diarios = NULL) {
  moduleServer(id, function(input, output, session) {
    estado <- reactiveVal(NULL)

    observeEvent(input$cargar, {
      prog <- tryCatch(shiny::Progress$new(session, min = 0, max = 1), error = function(e) NULL)
      if (!is.null(prog)) { prog$set(message = "Calculando predicciones 2024–2025…", value = 0.4); on.exit(prog$close(), add = TRUE) }
      estado(tryCatch(construir_validacion(m, ruta_diarios), error = function(e) list(ok = FALSE, msg = conditionMessage(e))))
    })

    dv <- function() { e <- estado(); validate(need(!is.null(e), "Presione «Calcular validación con la base diaria»."), need(isTRUE(e$ok), e$msg)); e }

    output$aviso_diarios <- renderUI({
      if (is.null(ruta_diarios)) div(class = "alert alert-info", role = "status",
        "No se encontró 'etapa2_datos_diarios.rds' (opcional). Sin esa base no se pueden mostrar las series; el resto de la pestaña funciona.")
    })
    output$resumen_calculo <- renderUI({
      e <- estado(); if (isTRUE(e$ok))
        div(class = "text-muted", sprintf("Filas usadas: %s estación-días (reporte: 7,234). Descartadas por datos incompletos o inválidos: %d.",
                                          format(e$n, big.mark = ","), e$n_desc))
    })

    output$tabla_global <- renderTable({
      v <- as.data.frame(m$validacion); v$Tipo <- as.character(v$Tipo); v
    }, striped = TRUE, digits = 3)

    output$grafica_rmse <- renderPlot({
      v <- as.data.frame(m$validacion); v$Modelo <- factor(v$Modelo, levels = rev(v$Modelo))
      ggplot(v, aes(x = RMSE, y = Modelo)) + geom_col(fill = "#0072B2", colour = "black", width = 0.65) +
        geom_text(aes(label = sprintf("%.2f", RMSE)), hjust = -0.15, size = 4) +
        scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
        labs(x = "RMSE en validación (µg/m³)", y = NULL) + theme_minimal(base_size = 13)
    }, alt = "RMSE de validación por modelo.")

    imp <- function() {
      i <- as.data.frame(m$importancia_gam); i <- i[, 1:3]; names(i) <- c("variable", "d_rmse", "d_r")
      i$Variable <- ifelse(i$variable %in% names(ETIQ_VAR_GAM), ETIQ_VAR_GAM[as.character(i$variable)], as.character(i$variable)); i
    }
    output$tabla_import <- renderTable({
      i <- imp(); data.frame(Variable = i$Variable, `ΔRMSE al quitarla (µg/m³)` = i$d_rmse, `Δr al quitarla` = i$d_r, check.names = FALSE)
    }, striped = TRUE, digits = 3)
    output$grafica_import <- renderPlot({
      i <- imp(); i$Variable <- factor(i$Variable, levels = rev(i$Variable))
      ggplot(i, aes(x = d_rmse, y = Variable)) + geom_col(fill = "#D55E00", colour = "black", width = 0.65) +
        geom_text(aes(label = sprintf("%.3f", d_rmse)), hjust = -0.15, size = 4) +
        scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
        labs(x = "Aumento del RMSE al quitar la variable (µg/m³)", y = NULL) + theme_minimal(base_size = 13)
    }, alt = "Importancia de las variables del GAM G2 medida como aumento del RMSE al quitarlas.")

    output$tabla_logistica <- renderTable({
      e <- estado(); rec <- if (isTRUE(e$ok) && !is.null(e$logistica)) e$logistica else NULL
      out <- data.frame(`Métrica` = names(REPORTE_LOGISTICA), Reporte = unname(REPORTE_LOGISTICA),
                        Recalculado = if (is.null(rec)) NA_real_ else unname(rec[names(REPORTE_LOGISTICA)]), check.names = FALSE)
      rbind(out, data.frame(`Métrica` = "Umbral de probabilidad (modelo guardado)", Reporte = NA_real_, Recalculado = m$umbral_prob, check.names = FALSE))
    }, striped = TRUE, digits = 3, na = "—")

    col_mod <- reactive(MODELOS_VAL[[input$mod_sel]])

    output$serie <- plotly::renderPlotly({
      e <- dv(); d <- e$datos[e$datos$estacion == input$est_sel, ]; validate(need(nrow(d) > 0, "Sin datos para esta estación."))
      full <- data.frame(fecha = seq(min(d$fecha), max(d$fecha), by = "day"))
      d <- merge(full, d, by = "fecha", all.x = TRUE)
      largo <- data.frame(fecha = rep(d$fecha, 2), valor = c(d$obs, d[[col_mod()]]),
                          serie = rep(c("Observado", "Predicho"), each = nrow(d)))
      p <- ggplot(largo, aes(fecha, valor, colour = serie, linetype = serie)) +
        annotate("rect", xmin = as.Date("2025-01-01"), xmax = max(d$fecha) + 1, ymin = -Inf, ymax = Inf, fill = "grey40", alpha = 0.08) +
        geom_hline(yintercept = m$umbral_critico, linetype = "dotted", colour = "grey30") +
        geom_line(linewidth = 0.5, na.rm = TRUE) +
        scale_colour_manual(values = c(Observado = "#000000", Predicho = "#D55E00"), name = NULL) +
        scale_linetype_manual(values = c(Observado = "solid", Predicho = "longdash"), name = NULL) +
        labs(x = NULL, y = "PM2.5 (µg/m³)", title = sprintf("%s · %s", input$est_sel, input$mod_sel)) + theme_minimal(base_size = 12)
      plotly::ggplotly(p) |> plotly::layout(legend = list(orientation = "h", y = -0.15))
    })

    output$dispersion <- renderPlot({
      e <- dv(); d <- e$datos[e$datos$estacion == input$est_sel, ]; d$pred <- d[[col_mod()]]; d$anio <- factor(d$anio)
      ggplot(d, aes(pred, obs, colour = anio, shape = anio)) + geom_point(alpha = 0.6, size = 1.8) +
        geom_abline(linetype = "dashed") + scale_x_log10() + scale_y_log10() +
        scale_colour_manual(values = c("2024" = "#0072B2", "2025" = "#D55E00"), name = "Año") +
        scale_shape_manual(values = c("2024" = 16, "2025" = 17), name = "Año") +
        labs(x = "PM2.5 predicho (µg/m³, escala log)", y = "PM2.5 observado (µg/m³, escala log)") + theme_minimal(base_size = 12)
    }, alt = "Dispersión de PM2.5 observado contra predicho para la estación y modelo elegidos, por año.")

    output$tabla_anio <- renderTable({
      e <- dv(); metricas_por(e$datos, col_mod(), por = "anio")
    }, striped = TRUE, digits = 2, caption = "Métricas por año (todas las estaciones)")

    output$tabla_est <- DT::renderDT({
      e <- dv(); mt <- metricas_por(e$datos, col_mod(), por = "estacion", orden = m$niveles$estacion)
      glob <- cbind(estacion = "Todas", metricas_pred(e$datos$obs, e$datos[[col_mod()]]))
      DT::datatable(rbind(mt, glob), rownames = FALSE, colnames = c("Estación", "n", "RMSE", "MAE", "r"),
                    options = list(dom = "t", pageLength = 13), caption = "Métricas por estación (µg/m³, salvo r)") |>
        DT::formatRound(c("RMSE", "MAE", "r"), 2)
    })
  })
}