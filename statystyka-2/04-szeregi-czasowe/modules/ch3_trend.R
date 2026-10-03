# ============================================================================
# CHAPTER 3: Trend — jak go mierzyć i usuwać
# ============================================================================

ch3_ui <- list(
  id    = "ch-trend",
  num   = "03",
  title = "Trend: jak go mierzyć i usuwać",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 03 · Szeregi czasowe",
      num    = "03",
      title  = "Trend.",
      lead   = "Długoterminowa tendencja wzrostu lub spadku. Trzy sposoby wyodrębnienia trendu —
                każdy z innymi założeniami i inaczej wrażliwy na szum."
    ),

    lc_h2("ch3-co-to-trend", "Co to jest trend i dlaczego go potrzebujemy?"),

    tagList(
      lc_p("Trend to składowa szeregu, która zmienia się powoli i systematycznie w czasie.
        Interesuje nas z co najmniej dwóch powodów:"),
      tags$ul(
        tags$li(tags$strong("Opis:"),
          " chcemy wiedzieć, czy zjawisko rośnie, maleje czy jest stabilne
          — w długim horyzoncie, po wygaszeniu wahań sezonowych i losowych."),
        tags$li(tags$strong("Przygotowanie do modelowania:"),
          " wiele modeli (ARIMA, ETS) wymaga usunięcia trendu przed dopasowaniem.
          Trzeba go zidentyfikować, by móc go odjąć lub wyeliminować różnicowaniem.")
      ),
      lc_p("Przyjrzymy się trzem metodom wyodrębniania trendu, od najprostszej do najbardziej elastycznej.")
    ),

    lc_h2("ch3-step-methods", "Trzy metody — krok po kroku"),

    figure_panel(
      label = "Ryc. 3.1",
      full_width = TRUE,
      lc_step_widget("ch3_trend",
        title = "Nałóż kolejne metody estymacji trendu",
        steps = c("Dane", "Regresja liniowa", "Średnia krocząca (k=12)", "LOESS"),
        toolbar = lc_toolbar(
          lc_step_from(4, lc_slider("ch3_loess_span", "Span LOESS (gładkość)",
                                    0.1, 1.0, 0.3, 0.05))
        ),
        plot_id = "ch3_step_plot"
      )
    ),

    lc_h2("ch3-porownanie", "Porównanie metod"),

    tagList(
      tags$table(class = "lc-table lc-table-bordered lc-table-striped lc-table-sm",
        tags$thead(tags$tr(
          tags$th("Metoda"),
          tags$th("Założenia"),
          tags$th("Zalety"),
          tags$th("Wady")
        )),
        tags$tbody(
          tags$tr(
            tags$td("Regresja liniowa"),
            tags$td("Trend jest liniowy"),
            tags$td("Prosta, daje jedną liczbę (nachylenie)"),
            tags$td("Zła jeśli trend nie jest liniowy")
          ),
          tags$tr(
            tags$td("Średnia krocząca (MA)"),
            tags$td("Trend zmienia się powoli"),
            tags$td("Intuicyjna, łatwa do obliczenia"),
            tags$td("Traci obserwacje na brzegach, reaguje z opóźnieniem")
          ),
          tags$tr(
            tags$td("LOESS"),
            tags$td("Trend jest lokalnie liniowy"),
            tags$td("Elastyczna, wyłapuje zmiany kierunku trendu"),
            tags$td("Jeden parametr (span) do doboru, trudna do interpretacji liczbowo")
          )
        )
      )
    ),

    lc_h2("ch3-usuniecie", "Usunięcie trendu = residua"),

    tagList(
      lc_p("Po wyodrębnieniu trendu możemy go ", tags$em("odjąć"),
        " od szeregu i uzyskać ", tags$strong("residua"),
        " — dane bez trendu, gotowe do dalszej analizy. Kliknij 'Usuń trend' poniżej,
        żeby zobaczyć efekt dla wybranej metody.")
    ),

    figure_panel(
      label = "Ryc. 3.2", title = "Widok po usunięciu trendu",
      full_width = TRUE,
      fluidRow(
        column(4,
          selectInput("ch3_res_method", "Metoda trendu:",
                      choices = c("Regresja liniowa" = "lm",
                                  "Średnia krocząca (k=12)" = "ma",
                                  "LOESS (span=0.3)" = "loess"),
                      selected = "loess"),
          selectInput("ch3_res_data", "Szereg:",
                      choices = .ts_choices_for("warszawa", "bezrobocie", "pszenica"),
                      selected = "warszawa"),
          uiOutput("ch3_res_stats")
        ),
        column(8,
          zoom_plot_ui("ch3_res_plot", height = "340px")
        )
      )
    ),

    lc_h2("ch3-roznicowanie", "Różnicowanie jako alternatywa"),

    tagList(
      lc_p("Zamiast szacować i odejmować trend, możemy go usunąć przez ",
        tags$strong("różnicowanie"),
        ": obliczamy różnice między kolejnymi wartościami. Jeśli szereg ma trend liniowy,
        pierwsza różnica da szereg stacjonarny."),
      lc_formula_box(
        withMathJax(helpText("$$\\nabla x_t = x_t - x_{t-1}$$")),
        p("Pierwsza różnica eliminuje trend liniowy. Druga różnica eliminuje trend kwadratowy.")
      ),
      lc_p("Różnicowanie omówimy szerzej w rozdziale o stacjonarności (ch7).
        Na razie zapamiętaj, że to alternatywa dla odejmowania trendu.")
    ),

    margin_callout(label = "Zapamiętaj", color = "wskazowka",
      "Trend to nie zawsze linia prosta. LOESS potrafi wykryć zmiany w kierunku trendu,
       np. spowolnienie wzrostu po 2010 w danych o energii. Dobierz metodę do kształtu danych."
    ),

    lc_chapter_next(
      num       = "04",
      title     = "Sezonowość: wzorce cykliczne",
      lead      = "seasonal subseries, heatmap rok×miesiąc, addytywna vs multiplikatywna",
      target_id = "ch-sezonowosc"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch3_server <- function(input, output, session) {

  # Krok widgetu (1..4) żyje w przeglądarce; suwak span nie zmienia kroku.
  ch3_step <- lc_step_server("ch3_trend", input)$step

  ch3_plot_data <- reactive({
    df   <- .ts_datasets[["warszawa"]]$get_df()
    df_sub <- df[df$date >= as.Date("1980-01-01"), ]
    df_sub$t <- seq_len(nrow(df_sub))
    df_sub
  })

  zoom_plot_server("ch3_step_plot", reactive({
    df   <- ch3_plot_data()
    step <- ch3_step()
    span <- if (!is.null(input$ch3_loess_span)) input$ch3_loess_span else 0.3

    # Stała rama: zakres danych, z miejscem na etykiety metod po prawej.
    x_lims <- c(min(df$date), max(df$date) + as.numeric(diff(range(df$date))) * 0.14)
    y_pad  <- diff(range(df$temp)) * 0.04
    y_lims <- range(df$temp) + c(-y_pad, y_pad)
    x_end  <- max(df$date) + as.numeric(diff(range(df$date))) * 0.01
    last_value <- function(v) utils::tail(v[!is.na(v)], 1)

    # Surowe dane w kroku 1; potem tło pod liniami trendu.
    p <- ggplot(df, aes(x = date, y = temp)) +
      step_layer(geom_line, if (step == 1) "data" else "background", linewidth = 0.5) +
      labs(x = NULL, y = "°C")

    # Etykiety metod przy końcach linii; rozsunięte w pionie, gdy końce są blisko.
    ends <- list()
    if (step >= 2) {
      lm_fit  <- lm(temp ~ t, data = df)
      df$lm_trend <- predict(lm_fit)
      role <- step_role(step, 2)
      p <- p + step_layer(geom_line, role, data = df, mapping = aes(y = lm_trend),
                          linetype = "longdash")
      ends$lm <- list(y = last_value(df$lm_trend), role = role)
    }
    if (step >= 3) {
      k <- 12
      df$ma_trend <- as.numeric(stats::filter(df$temp, rep(1/k, k), sides = 2))
      role <- step_role(step, 3)
      p <- p + step_layer(geom_line, role, data = df, mapping = aes(y = ma_trend),
                          na.rm = TRUE)
      ends$`MA(12)` <- list(y = last_value(df$ma_trend), role = role)
    }
    if (step >= 4) {
      lo_fit  <- loess(temp ~ t, data = df, span = span)
      df$loess_trend <- predict(lo_fit)
      p <- p + step_layer(geom_line, "new", data = df, mapping = aes(y = loess_trend))
      ends$LOESS <- list(y = last_value(df$loess_trend), role = "new")
    }
    if (length(ends)) {
      ends <- ends[order(vapply(ends, `[[`, 0, "y"))]
      gap  <- diff(y_lims) * 0.07
      y_lab <- vapply(ends, `[[`, 0, "y")
      for (i in seq_along(y_lab)[-1]) y_lab[i] <- max(y_lab[i], y_lab[i - 1] + gap)
      for (i in seq_along(ends)) {
        p <- p + step_label(x_end, y_lab[i], names(ends)[i],
                            role = ends[[i]]$role, vjust = 0.5)
      }
    }

    p + step_frame(xlim = x_lims, ylim = y_lims)
  }))

  output$ch3_trend_text <- renderUI({
    switch(as.character(ch3_step()),
      "1" = "Surowe dane: temperatura z wyraźną sezonowością.",
      "2" = "Regresja liniowa (przerywana): zakłada stały trend. Słabo radzi sobie z krzywizną.",
      "3" = "Średnia krocząca MA(12): wygładza miesięczną sezonowość. Traci 6 obserwacji z każdej strony.",
      "4" = "LOESS: elastyczna krzywa lokalna. Wyłapuje przyspieszenie trendu po 2000 r.",
      ""
    )
  })

  ch3_res_ts <- reactive({
    key    <- input$ch3_res_data
    method <- input$ch3_res_method
    df     <- .ts_datasets[[key]]$get_df()
    y      <- df[[2]]
    n      <- nrow(df)
    t      <- seq_len(n)

    trend_vals <- switch(method,
      lm    = predict(lm(y ~ t)),
      ma    = as.numeric(stats::filter(y, rep(1/12, 12), sides = 2)),
      loess = predict(loess(y ~ t, span = 0.3))
    )

    data.frame(
      date   = df$date,
      value  = y,
      trend  = trend_vals,
      resid  = y - ifelse(is.na(trend_vals), mean(y, na.rm = TRUE), trend_vals)
    )
  })

  zoom_plot_server("ch3_res_plot", reactive({
    df   <- ch3_res_ts()
    key  <- input$ch3_res_data
    unit <- .ts_datasets[[key]]$unit

    df_long <- data.frame(
      date  = rep(df$date, 2),
      value = c(df$value, df$resid),
      panel = rep(c("Oryginał", "Residua (bez trendu)"), each = nrow(df))
    )

    ggplot(df_long, aes(x = date, y = value)) +
      geom_line(color = upwr_accent, linewidth = 0.8) +
      geom_hline(data = data.frame(panel = "Residua (bez trendu)", y = 0),
                 aes(yintercept = y), color = upwr_reference, linetype = "dashed") +
      facet_wrap(~ panel, ncol = 1, scales = "free_y") +
      labs(x = NULL, y = unit) +
      theme_upwr() +
      theme(strip.text = element_text(face = "bold"))
  }))

  output$ch3_res_stats <- renderUI({
    df  <- ch3_res_ts()
    r   <- df$resid[!is.na(df$resid)]
    lc_stat_grid(
      lc_stat_box("SD residuów", round(sd(r), 3), color = upwr_accent),
      lc_stat_box("Max |resid|",  round(max(abs(r)), 2), color = upwr_secondary),
      columns = 2
    )
  })
}
