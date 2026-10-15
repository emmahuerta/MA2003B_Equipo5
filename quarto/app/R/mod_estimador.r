# app/R/mod_estimador.R
VARS_UI <- data.frame(
  v        = c("TOUT", "RH", "WSR", "RAINF", "SR", "NO2", "CO"),
  etiqueta = c("Temperatura (°C)", "Humedad relativa (%)", "Viento (km/h)",
               "Lluvia del día (mm)", "Radiación solar (kW/m²)", "NO2 (ppb)", "CO (ppm)"),
  paso     = c(0.1, 1, 0.1, 0.1, 0.01, 0.5, 0.05),
  stringsAsFactors = FALSE)

limites_control <- function(rg, v, paso) {
  lo  <- round(ceiling(rg[v, "p01"] / paso) * paso, 6)
  hi  <- round(floor(rg[v, "p99"] / paso) * paso, 6)
  med <- round(min(max(round(rg[v, "p50"] / paso) * paso, lo), hi), 6)
  c(lo = lo, hi = hi, med = med)
}

control_variable <- function(ns, rg, v, etiqueta, paso) {
  l <- limites_control(rg, v, paso)
  tagList(
    conditionalPanel("!input.manual", ns = ns,
      sliderInput(ns(paste0("s_", v)), etiqueta, min = l[["lo"]], max = l[["hi"]],
                  value = l[["med"]], step = paso)),
    conditionalPanel("input.manual", ns = ns,
      numericInput(ns(paste0("n_", v)), etiqueta, value = l[["med"]], step = paso)))
}

mod_estimador_ui <- function(id, m) {
  ns <- NS(id)
  rg <- as.data.frame(m$rangos); rownames(rg) <- rg$variable
  g  <- m$grupos[match(m$niveles$estacion, m$grupos$estacion), ]
  choices <- setNames(g$estacion, sprintf("%s · %s (grupo %d)", g$estacion, g$zona, g$grupo))

  layout_sidebar(
    fill = FALSE,
    sidebar = sidebar(
      width = 340, title = "Condiciones del día",
      selectInput(ns("estacion"), "Estación", choices = choices, selected = "CE"),
      dateInput(ns("fecha"), "Fecha", value = as.Date("2025-01-15"), min = "2021-01-01",
                format = "dd/mm/yyyy", language = "es", weekstart = 1),
      checkboxInput(ns("manual"), "Modo manual (permite valores fuera de p01\u2013p99)", FALSE),
      lapply(seq_len(nrow(VARS_UI)), function(i)
        control_variable(ns, rg, VARS_UI$v[i], VARS_UI$etiqueta[i], VARS_UI$paso[i])),
      actionButton(ns("reset"), "Volver a las medianas", class = "btn btn-outline-secondary btn-sm"),
      helpText("Los deslizadores cubren del percentil 1 al 99 del periodo de entrenamiento (2021\u20132023); el valor inicial es la mediana.")),

    uiOutput(ns("aviso_error")),
    uiOutput(ns("avisos_rango")),
    layout_columns(col_widths = c(6, 6), uiOutput(ns("semaforo")), uiOutput(ns("cifras"))),
    card(card_header("Comparación entre modelos"),
         plotOutput(ns("grafica_modelos"), height = "300px"),
         uiOutput(ns("aviso_discrepancia")),
         tableOutput(ns("tabla_modelos"))),
    card(card_header("Qué empuja el resultado (GAM G2)"),
         plotOutput(ns("grafica_contrib"), height = "340px")),
    card(card_header("Interpretación"),
         uiOutput(ns("texto")),
         tags$details(
           tags$summary("Cómo leer este resultado"),
           tags$ul(
             tags$li("Los modelos son explicativos: usan la meteorología y los gases del mismo día. No son pronósticos y las asociaciones no son causales."),
             tags$li("Viento: el PM2.5 baja hasta cerca de 9 km/h y después se aplana."),
             tags$li("Temperatura: sube sobre ~20 °C, pero su concurvidad es alta (0.79); léase con cautela."),
             tags$li("CO: su relación con el PM2.5 no es estable entre periodos (en 2025 el CO cae ~33 % en toda la red, posible cambio de equipos, hipótesis por confirmar)."),
             tags$li("Resultados de 2025 provisionales. No extrapole fuera de los rangos de entrenamiento."),
             tags$li("El intervalo del GAM es del valor esperado, no de un día concreto: el error típico diario en validación es ~8.8 µg/m³ (RMSE de G2)."),
             tags$li("Día crítico = PM2.5 > 33 µg/m³ en todos los años (límite NOM vigente desde 2024). Los límites NOM por año son solo referencia diaria, no una evaluación formal de cumplimiento.")))))
}

mod_estimador_server <- function(id, m) {
  moduleServer(id, function(input, output, session) {
    rg <- as.data.frame(m$rangos); rownames(rg) <- rg$variable
    lim <- lapply(seq_len(nrow(VARS_UI)), function(i) limites_control(rg, VARS_UI$v[i], VARS_UI$paso[i]))
    names(lim) <- VARS_UI$v

    # Sincroniza controles al cambiar de modo y al restablecer
    observeEvent(input$manual, ignoreInit = TRUE, {
      for (v in VARS_UI$v) {
        if (isTRUE(input$manual)) {
          x <- input[[paste0("s_", v)]]
          if (!is.null(x)) updateNumericInput(session, paste0("n_", v), value = x)
        } else {
          x <- input[[paste0("n_", v)]]
          if (!is.null(x) && is.finite(x))
            updateSliderInput(session, paste0("s_", v), value = min(max(x, lim[[v]][["lo"]]), lim[[v]][["hi"]]))
        }
      }
    })
    observeEvent(input$reset, {
      for (v in VARS_UI$v) {
        updateSliderInput(session, paste0("s_", v), value = lim[[v]][["med"]])
        updateNumericInput(session, paste0("n_", v), value = lim[[v]][["med"]])
      }
    })

    calc <- reactive({
      req(input$estacion, !is.null(input$manual))
      if (is.null(input$fecha) || length(input$fecha) != 1 || is.na(input$fecha))
        return(list(ok = FALSE, msg = "Seleccione una fecha válida."))
      vals <- lapply(VARS_UI$v, function(v) {
        x <- if (isTRUE(input$manual)) input[[paste0("n_", v)]] else input[[paste0("s_", v)]]
        if (is.null(x) || length(x) != 1) NA_real_ else x })
      names(vals) <- VARS_UI$v
      tryCatch({
        nd <- preparar_entrada(m, input$estacion, input$fecha, vals$TOUT, vals$RH, vals$WSR,
                               vals$RAINF, vals$SR, vals$NO2, vals$CO)
        list(ok = TRUE, nd = nd, pred = predecir_modelos(m, nd), contrib = contribuciones_gam(m, nd),
             msgs = mensajes_rango(attr(nd, "rango"), m$rangos))
      }, error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
    })

    output$aviso_error <- renderUI({
      cc <- calc()
      if (!cc$ok) div(class = "alert alert-danger", role = "alert",
                      tags$strong("No se puede estimar: "), cc$msg)
    })

    output$avisos_rango <- renderUI({
      cc <- calc(); req(cc$ok)
      if (length(cc$msgs))
        div(class = "alert alert-warning", role = "alert",
            tags$strong("Aviso de rango. "), tags$ul(lapply(cc$msgs, tags$li)))
    })

    output$semaforo <- renderUI({
      cc <- calc(); req(cc$ok); p <- cc$pred
      anio <- as.integer(format(cc$nd$fecha[1], "%Y")); nom <- limite_nom(cc$nd$fecha[1])
      s <- clasificar_semaforo(p$pm25_g2, p$g2_li, p$g2_ls, nom, anio)
      div(class = paste("semaforo", s$clase), role = "status", `aria-live` = "polite",
          div(class = "semaforo-icono", `aria-hidden` = "true", s$icono),
          div(class = "semaforo-texto",
              tags$h3(s$etiqueta), tags$p(s$detalle),
              if (nzchar(s$aviso)) tags$p(class = "semaforo-aviso", s$aviso)))
    })

    output$cifras <- renderUI({
      cc <- calc(); req(cc$ok); p <- cc$pred
      anio <- as.integer(format(cc$nd$fecha[1], "%Y")); nom <- limite_nom(cc$nd$fecha[1])
      div(class = "cifras",
          div(class = "cifra",
              span(class = "cifra-etiqueta", "PM2.5 esperado (GAM G2)"),
              span(class = "cifra-valor", sprintf("%.1f", p$pm25_g2), span(class = "cifra-unidad", " \u00b5g/m\u00b3")),
              span(class = "cifra-nota", sprintf("IC 95 %% del valor esperado: %.1f\u2013%.1f", p$g2_li, p$g2_ls))),
          div(class = paste("cifra", if (p$critico) "crit-si" else "crit-no"),
              span(class = "cifra-etiqueta", sprintf("Probabilidad de día crítico (> %g \u00b5g/m\u00b3)", m$umbral_critico)),
              span(class = "cifra-valor", fmt_pct(p$p_critico)),
              span(class = "cifra-nota",
                   if (p$critico) sprintf("\u25B2 Clasificado como CRÍTICO (p \u2265 %.3f)", m$umbral_prob)
                   else sprintf("\u2714 No clasificado como crítico (p < %.3f)", m$umbral_prob))),
          div(class = "cifra",
              span(class = "cifra-etiqueta", "Límites de referencia (24 h)"),
              span(class = "cifra-nota", sprintf("Guía OMS 2021: %g \u00b5g/m\u00b3", LIM_OMS_PM25)),
              span(class = "cifra-nota", sprintf("NOM-025-SSA1-2021 vigente en %d: %g \u00b5g/m\u00b3", anio, nom))))
    })

    nombres_mod <- c("GAM G2 (principal)", "GAM G1 (sin NO2 ni CO)", "Regresión lineal M2", "Bosque aleatorio")

    output$grafica_modelos <- renderPlot({
      cc <- calc(); req(cc$ok); p <- cc$pred; nom <- limite_nom(cc$nd$fecha[1])
      df <- data.frame(modelo = factor(nombres_mod, levels = rev(nombres_mod)),
                       pm = c(p$pm25_g2, p$pm25_g1, p$pm25_m2, p$pm25_rf),
                       li = c(p$g2_li, NA, NA, NA), ls = c(p$g2_ls, NA, NA, NA))
      ggplot(df, aes(x = pm, y = modelo)) +
        geom_vline(xintercept = LIM_OMS_PM25, linetype = "dashed", colour = "grey30") +
        geom_vline(xintercept = nom, linetype = "dotted", linewidth = 0.9, colour = "grey30") +
        geom_errorbar(aes(xmin = li, xmax = ls), width = 0.25, orientation = "y", na.rm = TRUE) +
        geom_point(size = 3.5, shape = 21, fill = "#0072B2", colour = "black") +
        geom_text(aes(label = sprintf("%.1f", pm)), nudge_y = 0.3, size = 4.2) +
        scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
        labs(x = "PM2.5 esperado (\u00b5g/m\u00b3)", y = NULL,
             subtitle = sprintf("Línea discontinua: guía OMS (%g). Línea punteada: NOM vigente (%g).", LIM_OMS_PM25, nom)) +
        theme_minimal(base_size = 14)
    }, alt = "Comparación del PM2.5 esperado por los cuatro modelos frente a la guía de la OMS y la NOM vigente.")

    output$aviso_discrepancia <- renderUI({
      cc <- calc(); req(cc$ok)
      d <- discrepancia_modelos(cc$pred)
      if (d > 0.5)
        div(class = "alert alert-warning", role = "alert",
            tags$strong("Los modelos discrepan mucho. "),
            "Con condiciones extremas la regresión lineal extrapola y el bosque aleatorio se aplana; interprete el resultado con cautela.")
    })

    output$tabla_modelos <- renderTable({
      cc <- calc(); req(cc$ok); p <- cc$pred
      data.frame(Modelo = nombres_mod,
                 `PM2.5 esperado (µg/m³)` = sprintf("%.1f", c(p$pm25_g2, p$pm25_g1, p$pm25_m2, p$pm25_rf)),
                 Nota = c("Modelo principal; IC del valor esperado.",
                          "Solo calendario y meteorología: aísla el papel del clima.",
                          "Lineal en log(PM2.5); extrapola mal en extremos.",
                          "Referencia no lineal; no extrapola fuera de lo visto."),
                 check.names = FALSE)
    }, striped = TRUE)

    output$grafica_contrib <- renderPlot({
      cc <- calc(); req(cc$ok, cc$contrib)
      cb <- cc$contrib
      cb$cambio <- 100 * (cb$factor - 1)
      cb$efecto <- ifelse(cb$cambio >= 0, "Aumenta", "Reduce")
      cb$variable <- factor(cb$variable, levels = rev(cb$variable))
      ggplot(cb, aes(x = cambio, y = variable, fill = efecto)) +
        geom_col(colour = "black", width = 0.7) +
        geom_vline(xintercept = 0) +
        geom_text(aes(label = sprintf("%+.0f %%", cambio), hjust = ifelse(cambio >= 0, -0.1, 1.1)), size = 4) +
        scale_fill_manual(values = c(Aumenta = "#D55E00", Reduce = "#0072B2"), name = NULL) +
        scale_x_continuous(expand = expansion(mult = 0.2)) +
        labs(x = "Cambio en el PM2.5 esperado (%) respecto a un día promedio", y = NULL) +
        theme_minimal(base_size = 14) + theme(legend.position = "top")
    }, alt = "Barras con el cambio porcentual que cada variable aporta al PM2.5 esperado según el GAM G2.")

    output$texto <- renderUI({
      cc <- calc(); req(cc$ok); p <- cc$pred
      anio <- as.integer(format(cc$nd$fecha[1], "%Y")); nom <- limite_nom(cc$nd$fecha[1])
      s <- clasificar_semaforo(p$pm25_g2, p$g2_li, p$g2_ls, nom, anio)
      texto_interpretativo(m, cc$nd, p, s, cc$contrib, nom)
    })

    invisible(NULL)
  })
}