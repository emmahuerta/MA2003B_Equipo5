# app/R/mod_escenarios.R
NOTAS_VARIABLE <- c(
  TOUT  = "Temperatura: el PM2.5 sube sobre ~20 °C, pero su concurvidad es alta (0.79) y comparte señal con la estacionalidad; léase con cautela.",
  RH    = "Humedad: el efecto sube hasta cerca de 75 % y baja después.",
  WSR   = "Viento: el PM2.5 baja hasta ~9 km/h y después se aplana (umbral de dispersión).",
  RAINF = "Lluvia: efecto pequeño y con concurvidad alta (0.94); interprétese con cautela.",
  SR    = "Radiación solar: aporta poco al modelo.",
  NO2   = "NO2: es la variable más importante del GAM. Es un trazador de combustión y estancamiento, no una causa directa.",
  CO    = "CO: su relación con el PM2.5 no es estable entre periodos (cae ~33 % en 2025 en toda la red); no se interpreta como estable.")

rango_curva <- function(rg, v, ampliar = FALSE)
  if (ampliar) c(rg[v, "min"], rg[v, "max"]) else c(rg[v, "p01"], rg[v, "p99"])

curva_efecto <- function(m, estacion, fecha, vals, variable, rango, n = 80) {
  x <- if (variable %in% c("RAINF", "NO2", "CO")) expm1(seq(log1p(rango[1]), log1p(rango[2]), length.out = n))
       else seq(rango[1], rango[2], length.out = n)
  x[1] <- rango[1]; x[n] <- rango[2]
  a <- vals; a[[variable]] <- x
  nd <- do.call(preparar_entrada, c(list(modelos = m, estacion = estacion, fecha = fecha), a[VARS_CRUDAS]))
  p <- predecir_gam(m, nd, "G2")
  data.frame(x = x, fit = p$fit, li = p$li, ls = p$ls)
}

panel_escenario <- function(ns, pref, titulo, m, rg, ini = list()) {
  g <- m$grupos[match(m$niveles$estacion, m$grupos$estacion), ]
  choices <- setNames(g$estacion, sprintf("%s · %s (grupo %d)", g$estacion, as.character(g$zona), g$grupo))
  ctrls <- lapply(seq_len(nrow(VARS_UI)), function(i) {
    v <- VARS_UI$v[i]; l <- limites_control(rg, v, VARS_UI$paso[i])
    sliderInput(ns(paste0(pref, v)), VARS_UI$etiqueta[i], min = l[["lo"]], max = l[["hi"]],
                value = if (is.null(ini[[v]])) l[["med"]] else l[[ini[[v]]]], step = VARS_UI$paso[i])
  })
  card(card_header(titulo),
       selectInput(ns(paste0(pref, "estacion")), "Estación", choices = choices, selected = "CE"),
       dateInput(ns(paste0(pref, "fecha")), "Fecha", value = as.Date("2025-01-15"),
                 format = "dd/mm/yyyy", language = "es", weekstart = 1),
       ctrls)
}

mod_escenarios_ui <- function(id, m) {
  ns <- NS(id)
  rg <- as.data.frame(m$rangos); rownames(rg) <- rg$variable
  tagList(
    card(card_header("¿Qué variable quiere recorrer?"),
         layout_columns(col_widths = c(5, 4, 3),
           selectInput(ns("variable"), "Variable a variar (las demás se mantienen en los valores de cada escenario)",
                       choices = setNames(VARS_UI$v, VARS_UI$etiqueta), selected = "WSR"),
           checkboxInput(ns("ampliar"), "Ampliar al rango completo de entrenamiento (mín–máx); la cola extrema se sombrea", FALSE),
           actionButton(ns("copiar"), "Copiar A en B", class = "btn btn-outline-secondary btn-sm")),
         helpText("Los deslizadores de cada escenario marcan el punto de partida; la curva recorre la variable elegida. Es un análisis ceteris paribus del GAM G2: en la realidad, NO2, CO y viento cambian juntos, así que algunas combinaciones pueden ser poco realistas.")),
    layout_columns(col_widths = c(6, 6),
      panel_escenario(ns, "a_", "Escenario A", m, rg),
      panel_escenario(ns, "b_", "Escenario B (inicia como día de estancamiento: viento bajo y NO2 alto)", m, rg,
                      ini = list(WSR = "lo", NO2 = "hi"))),
    uiOutput(ns("aviso")),
    card(card_header("Efecto sobre el PM2.5 esperado (GAM G2, banda 95 % del valor esperado)"),
         plotOutput(ns("grafica"), height = "420px"), uiOutput(ns("nota_variable"))),
    card(card_header("Comparación de los dos escenarios"), tableOutput(ns("tabla")), uiOutput(ns("resumen"))))
}

mod_escenarios_server <- function(id, m) {
  moduleServer(id, function(input, output, session) {
    rg <- as.data.frame(m$rangos); rownames(rg) <- rg$variable

    res_esc <- function(pref) reactive({
      v <- input$variable; req(v)
      est <- input[[paste0(pref, "estacion")]]; f <- input[[paste0(pref, "fecha")]]
      vals <- lapply(VARS_UI$v, function(x) input[[paste0(pref, x)]]); names(vals) <- VARS_UI$v
      tryCatch({
        if (is.null(est) || is.null(f) || length(f) != 1 || is.na(f)) stop("Seleccione estación y una fecha válida.")
        if (any(vapply(vals, is.null, logical(1)))) stop("Faltan valores en los controles.")
        cur <- curva_efecto(m, est, f, vals, v, rango_curva(rg, v, isTRUE(input$ampliar)))
        nd  <- preparar_entrada(m, est, f, vals$TOUT, vals$RH, vals$WSR, vals$RAINF, vals$SR, vals$NO2, vals$CO)
        list(ok = TRUE, curva = cur, punto = predecir_gam(m, nd, "G2"), x0 = vals[[v]], est = est, fecha = f)
      }, error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
    })
    res_a <- res_esc("a_"); res_b <- res_esc("b_")

    observeEvent(input$copiar, {
      updateSelectInput(session, "b_estacion", selected = input$a_estacion)
      updateDateInput(session, "b_fecha", value = input$a_fecha)
      for (v in VARS_UI$v) updateSliderInput(session, paste0("b_", v), value = input[[paste0("a_", v)]])
    })

    output$aviso <- renderUI({
      a <- res_a(); b <- res_b(); msgs <- c(if (!a$ok) paste("A:", a$msg), if (!b$ok) paste("B:", b$msg))
      if (length(msgs)) div(class = "alert alert-danger", role = "alert",
                            tags$strong("No se puede calcular. "), paste(msgs, collapse = " "))
    })

    output$grafica <- renderPlot({
      a <- res_a(); b <- res_b(); req(a$ok, b$ok)
      v <- input$variable; et <- VARS_UI$etiqueta[VARS_UI$v == v]
      lab <- c(A = sprintf("A: %s, %s", a$est, format(a$fecha, "%d/%m/%Y")),
               B = sprintf("B: %s, %s", b$est, format(b$fecha, "%d/%m/%Y")))
      df <- rbind(cbind(a$curva, esc = "A"), cbind(b$curva, esc = "B"))
      pts <- data.frame(esc = c("A", "B"), x = c(a$x0, b$x0), y = c(a$punto$fit, b$punto$fit))
      noms <- c(limite_nom(a$fecha), limite_nom(b$fecha))
      p <- ggplot(df, aes(x = x, colour = esc, fill = esc, linetype = esc))
      if (isTRUE(input$ampliar)) p <- p +
        annotate("rect", xmin = -Inf, xmax = rg[v, "p01"], ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.35) +
        annotate("rect", xmin = rg[v, "p99"], xmax = Inf, ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.35)
      p <- p +
        geom_hline(yintercept = LIM_OMS_PM25, linetype = "dashed", colour = "grey30") +
        geom_hline(yintercept = unique(noms), linetype = "dotted", colour = "grey30", linewidth = 0.9) +
        geom_ribbon(aes(ymin = li, ymax = ls), alpha = 0.18, colour = NA) +
        geom_line(aes(y = fit), linewidth = 1.1) +
        geom_point(data = pts, aes(x = x, y = y, shape = esc), inherit.aes = FALSE, size = 4.5, fill = "white", stroke = 1.3) +
        scale_colour_manual(values = c(A = "#0072B2", B = "#D55E00"), labels = lab, name = NULL) +
        scale_fill_manual(values = c(A = "#0072B2", B = "#D55E00"), labels = lab, name = NULL) +
        scale_linetype_manual(values = c(A = "solid", B = "longdash"), labels = lab, name = NULL) +
        scale_shape_manual(values = c(A = 21, B = 24), labels = lab, name = NULL) +
        labs(x = et, y = "PM2.5 esperado (µg/m³)",
             subtitle = sprintf("Línea discontinua gris: guía OMS (%g). Línea punteada gris: NOM vigente (%s). Punto: valor actual del escenario.",
                                LIM_OMS_PM25, paste(unique(noms), collapse = " / "))) +
        theme_minimal(base_size = 14) + theme(legend.position = "top")
      if (v == "RAINF" && isTRUE(input$ampliar)) p <- p + scale_x_sqrt()
      p
    }, alt = "Curvas del PM2.5 esperado por el GAM G2 al variar una variable, para dos escenarios, con banda de confianza.")

    output$nota_variable <- renderUI({
      req(input$variable); div(class = "text-muted", NOTAS_VARIABLE[[input$variable]])
    })

    output$tabla <- renderTable({
      a <- res_a(); b <- res_b(); req(a$ok, b$ok)
      fila <- function(r, nombre) data.frame(
        Escenario = nombre, `Estación` = r$est, Fecha = format(r$fecha, "%d/%m/%Y"),
        `Valor actual de la variable` = format(round(r$x0, 3), nsmall = 0),
        `PM2.5 esperado G2 (µg/m³)` = sprintf("%.1f", r$punto$fit),
        `IC 95 % del valor esperado` = sprintf("%.1f–%.1f", r$punto$li, r$punto$ls),
        check.names = FALSE)
      out <- rbind(fila(a, "A"), fila(b, "B"))
      out$`B frente a A` <- c("—", sprintf("%+.0f %%", 100 * (b$punto$fit / a$punto$fit - 1)))
      out
    }, striped = TRUE)

    output$resumen <- renderUI({
      a <- res_a(); b <- res_b(); req(a$ok, b$ok); v <- input$variable
      et <- VARS_UI$etiqueta[VARS_UI$v == v]
      f1 <- function(r, n) { cu <- r$curva
        sprintf("<li><b>%s</b>: al recorrer %s de %.3g a %.3g, el PM2.5 esperado va de %.1f a %.1f µg/m³ (mínimo %.1f, máximo %.1f).</li>",
                n, et, cu$x[1], cu$x[nrow(cu)], cu$fit[1], cu$fit[nrow(cu)], min(cu$fit), max(cu$fit)) }
      HTML(paste0("<ul>", f1(a, "A"), f1(b, "B"), "</ul>"))
    })
  })
}