# ============================================================================
# CHAPTER 15: Anomalie i zdarzenia strukturalne
# ============================================================================

ch15_ui <- list(
  id    = "ch-anomalie",
  num   = "15",
  title = "Anomalie i zdarzenia strukturalne",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 15 · Szeregi czasowe",
      num    = "15",
      title  = "Anomalie.",
      lead   = "Niektóre zdarzenia nie są 'szumem' — są prawdziwymi zerwaniami struktury.
                COVID w noclegach, inwazja Rosji i ceny pszenicy. Jak model sobie z tym radzi?"
    ),

    lc_h2("ch15-covid", "COVID-19 w noclegach — przykład anomalii"),

    tagList(
      lc_p("Liczba noclegów w Polsce w 2020 r. spadła nagle o ponad 70%. To nie sezonowość,
        nie trend — to zewnętrzny wstrząs, którego żaden model nie przewidzi bez dodatkowej informacji."),
      margin_callout(label = "Kiedy to anomalia, a kiedy nowy reżim?", color = "wskazowka",
        "Anomalia: jednorazowy wstrząs po którym szereg wraca do poprzedniego wzorca (np. lockdown 2020).
         Nowy reżim (structural break): trwała zmiana poziomu lub trendu — model musi się przestroić.
         Pszenica 2022 to przykład potencjalnego nowego reżimu."
      )
    ),

    figure_panel(
      label = "Ryc. 15.1", title = "Noclegi w Polsce — anomalia COVID vs. prognoza bez korekcji",
      full_width = TRUE,
      fluidRow(
        column(4,
          radioButtons("ch15_covid_view", "Widok:",
                       choices = c("Dane historyczne" = "data",
                                   "Prognoza bez korekcji" = "forecast",
                                   "Zaznacz anomalię" = "highlight"),
                       selected = "data"),
          uiOutput("ch15_covid_info")
        ),
        column(8,
          zoom_plot_ui("ch15_covid_plot", height = "300px")
        )
      )
    ),

    lc_h2("ch15-strukturalna", "Przerwa strukturalna — pszenica 2022"),

    tagList(
      lc_p("Inwazja Rosji na Ukrainę w lutym 2022 r. spowodowała gwałtowny wzrost cen pszenicy.
        Ukraina i Rosja odpowiadają za ok. 30% światowego eksportu pszenicy —
        przerwa w dostawach zmieniła bazowy poziom cen.")
    ),

    figure_panel(
      label = "Ryc. 15.2", title = "Ceny pszenicy — przerwa strukturalna vs. model",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch15_break_year", "Rok przerwy:", min = 2019, max = 2023, value = 2022, step = 1),
          actionButton("ch15_break_run", "Porównaj modele", class = "lc-btn-primary", width = "100%"),
          uiOutput("ch15_break_result")
        ),
        column(8,
          zoom_plot_ui("ch15_break_plot", height = "300px")
        )
      )
    ),

    lc_h2("ch15-dummy", "ARIMAX — model z dummy variable"),

    tagList(
      lc_p("Najprostszy sposób uwzględnienia anomalii w modelu: dodaj zmienną dummy (0/1) dla okresu zdarzenia.
        Model ARIMAX (ARIMA z egzogeniczną zmienną zewnętrzną) potrafi to wbudować bezpośrednio."),
      lc_formula_box(
        withMathJax(helpText("$$x_t = \\underbrace{\\text{ARIMA}(p,d,q)}_\\text{część czasowa} + \\underbrace{\\beta \\cdot D_t}_\\text{dummy} + \\varepsilon_t$$")),
        p("D_t = 1 w okresie zdarzenia, 0 poza nim. β mierzy wielkość wstrząsu."),
        p("Dla przerwy strukturalnej: D_t = 1 od punktu przerwy wzwyż (step dummy).")
      ),
      margin_code_note(
        code = 'dummy <- ifelse(time(x) >= 2022, 1, 0)\nfit <- forecast::Arima(x, order=c(1,1,1), xreg=dummy)',
        description = "ARIMAX z dummy zmienną w R."
      )
    ),

    figure_panel(
      label = "Ryc. 15.3", title = "Noclegi: SARIMA bez vs. z dummy COVID",
      full_width = TRUE,
      fluidRow(
        column(4,
          actionButton("ch15_arimax_run", "Dopasuj oba modele", class = "lc-btn-primary", width = "100%"),
          uiOutput("ch15_arimax_table")
        ),
        column(8,
          zoom_plot_ui("ch15_arimax_plot", height = "300px")
        )
      )
    ),

    lc_chapter_next(
      num       = "16",
      title     = "Ściąga i ćwiczenia",
      lead      = "drzewo decyzyjne, tabela metod, ćwiczenia dla trzech kierunków",
      target_id = "ch-sciaga"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch15_server <- function(input, output, session) {

  ch15_noclegi_ts <- reactive({ .ts_datasets[["noclegi"]]$get_ts() })

  zoom_plot_server("ch15_covid_plot", reactive({
    ts_obj  <- ch15_noclegi_ts()
    view    <- if (!is.null(input$ch15_covid_view)) input$ch15_covid_view else "data"
    df      <- ts_to_df(ts_obj, "noclegi")
    df$date <- as.Date(df$date)

    n_train <- nrow(df[df$date < as.Date("2019-01-01"), ])
    if (n_train < 12) n_train <- round(length(ts_obj) * 0.6)
    train_ts <- ts(as.numeric(ts_obj)[seq_len(n_train)],
                   start = start(ts_obj), frequency = 12)

    if (view == "forecast") {
      h   <- length(ts_obj) - n_train
      fit <- tryCatch(
        forecast::forecast(forecast::ets(train_ts), h = h),
        error = function(e) forecast::snaive(train_ts, h = h)
      )
      fc_vals <- as.numeric(fit$mean)
      df_fc   <- data.frame(date = tail(df$date, h), noclegi = fc_vals, typ = "Prognoza (bez korekcji)")
      df$typ  <- "Dane rzeczywiste"
      df_all  <- rbind(df[, c("date", "noclegi", "typ")], df_fc)

      ggplot(df_all, aes(x = date, y = noclegi, color = typ, linetype = typ)) +
        geom_line(linewidth = 0.9) +
        scale_color_manual(values = c("Dane rzeczywiste" = upwr_secondary,
                                      "Prognoza (bez korekcji)" = upwr_accent), name = NULL) +
        scale_linetype_manual(values = c("Dane rzeczywiste" = "solid",
                                         "Prognoza (bez korekcji)" = "dashed"), name = NULL) +
        labs(x = NULL, y = "tys.", title = "Noclegi — prognoza vs. rzeczywistość w pandemii") +
        theme_upwr() + theme(legend.position = "bottom")

    } else if (view == "highlight") {
      covid_start <- as.Date("2020-03-01")
      covid_end   <- as.Date("2021-12-01")
      ggplot(df, aes(x = date, y = noclegi)) +
        annotate("rect", xmin = covid_start, xmax = covid_end,
                 ymin = -Inf, ymax = Inf, fill = upwr_accent, alpha = 0.15) +
        geom_line(color = upwr_secondary, linewidth = 0.9) +
        annotate("text", x = covid_start + 90, y = max(df$noclegi, na.rm = TRUE) * 0.85,
                 label = "Pandemia COVID-19", color = upwr_accent, size = 3.5, hjust = 0) +
        labs(x = NULL, y = "tys.", title = "Noclegi — anomalia pandemii") +
        theme_upwr()

    } else {
      ggplot(df, aes(x = date, y = noclegi)) +
        geom_line(color = upwr_secondary, linewidth = 0.9) +
        labs(x = NULL, y = "tys.", title = "Noclegi w Polsce — liczba miesięczna") +
        theme_upwr()
    }
  }))

  output$ch15_covid_info <- renderUI({
    view <- if (!is.null(input$ch15_covid_view)) input$ch15_covid_view else "data"
    switch(view,
      data = lc_feedback(type = "info", p("Oglądasz surowe dane. Wybierz inny widok.")),
      forecast = lc_feedback(type = "warning",
        tags$strong("Model się myli."),
        tags$br(),
        "ETS dopasowany do danych sprzed 2019 prognozuje 'normalne lata' —
         tymczasem 2020 to lockdown. Prognoza jest dramatycznie za wysoka."
      ),
      highlight = lc_feedback(type = "info",
        tags$strong("Zaznaczono okres pandemii (III 2020 – XII 2021)."),
        tags$br(),
        "Widać wyraźny dołek niemożliwy do przewidzenia bez zewnętrznej informacji."
      )
    )
  })

  ch15_break_fits <- reactiveVal(NULL)

  observeEvent(input$ch15_break_run, {
    ts_obj     <- .ts_datasets[["pszenica"]]$get_ts()
    break_year <- input$ch15_break_year
    t_vals     <- as.numeric(time(ts_obj))
    dummy      <- as.integer(t_vals >= break_year)

    fit_no_dummy <- tryCatch(
      forecast::auto.arima(ts_obj),
      error = function(e) NULL
    )
    fit_dummy <- tryCatch(
      forecast::Arima(ts_obj, order = c(1, 1, 1), xreg = matrix(dummy, ncol = 1)),
      error = function(e) NULL
    )
    ch15_break_fits(list(base = fit_no_dummy, dummy = fit_dummy, dummy_vals = dummy))
  })

  output$ch15_break_result <- renderUI({
    fits <- ch15_break_fits()
    if (is.null(fits)) return(lc_feedback(type = "info", p("Kliknij 'Porównaj modele'.")))
    aic_base  <- if (!is.null(fits$base))  round(fits$base$aic,  2) else NA
    aic_dummy <- if (!is.null(fits$dummy)) round(fits$dummy$aic, 2) else NA
    lc_stack(
      lc_stat_grid(
        lc_stat_box("AIC bez dummy",  aic_base,  color = upwr_secondary),
        lc_stat_box("AIC z dummy", aic_dummy, color = upwr_accent),
        columns = 2
      ),
      if (!is.na(aic_base) && !is.na(aic_dummy)) {
        if (aic_dummy < aic_base) {
          lc_feedback(type = "ok", p("Model z dummy lepszy — dummy poprawia dopasowanie."))
        } else {
          lc_feedback(type = "info", p("Model bez dummy ma niższy AIC — rok przerwy może być inny."))
        }
      }
    )
  })

  zoom_plot_server("ch15_break_plot", reactive({
    ts_obj     <- .ts_datasets[["pszenica"]]$get_ts()
    break_year <- if (!is.null(input$ch15_break_year)) input$ch15_break_year else 2022
    fits       <- ch15_break_fits()
    df         <- ts_to_df(ts_obj, "cena")

    p <- ggplot(df, aes(x = date, y = cena)) +
      geom_line(color = upwr_secondary, linewidth = 0.9) +
      geom_vline(xintercept = as.numeric(as.Date(paste0(break_year, "-01-01"))),
                 color = upwr_accent, linetype = "dashed", linewidth = 1) +
      annotate("text", x = as.Date(paste0(break_year, "-01-01")) + 30,
               y = max(df$cena, na.rm = TRUE) * 0.9,
               label = paste0("Przerwa: ", break_year), color = upwr_accent, size = 3.5, hjust = 0) +
      labs(x = NULL, y = "PLN/dt", title = "Ceny pszenicy — zaznaczona przerwa strukturalna") +
      theme_upwr()
    p
  }))

  ch15_arimax_fits <- reactiveVal(NULL)

  observeEvent(input$ch15_arimax_run, {
    ts_obj      <- .ts_datasets[["noclegi"]]$get_ts()
    t_vals      <- as.numeric(time(ts_obj))
    covid_dummy <- as.integer(t_vals >= 2020 & t_vals < 2022)
    fit_plain <- tryCatch(
      forecast::auto.arima(ts_obj),
      error = function(e) NULL
    )
    fit_arimax <- tryCatch(
      forecast::auto.arima(ts_obj, xreg = matrix(covid_dummy, ncol = 1)),
      error = function(e) NULL
    )
    ch15_arimax_fits(list(plain = fit_plain, arimax = fit_arimax, dummy = covid_dummy))
  })

  output$ch15_arimax_table <- renderUI({
    fits <- ch15_arimax_fits()
    if (is.null(fits)) return(lc_feedback(type = "info", p("Kliknij 'Dopasuj oba modele'.")))
    rmse_plain  <- if (!is.null(fits$plain))
      round(sqrt(mean(residuals(fits$plain)^2, na.rm = TRUE)), 1) else NA
    rmse_arimax <- if (!is.null(fits$arimax))
      round(sqrt(mean(residuals(fits$arimax)^2, na.rm = TRUE)), 1) else NA
    lc_stack(
      lc_stat_grid(
        lc_stat_box("RMSE bez dummy",  rmse_plain,  color = upwr_secondary),
        lc_stat_box("RMSE z dummy", rmse_arimax, color = upwr_accent),
        columns = 2
      ),
      if (!is.na(rmse_plain) && !is.na(rmse_arimax)) {
        if (rmse_arimax < rmse_plain) {
          lc_feedback(type = "ok", p(tags$strong("ARIMAX wygrywa: "),
            paste0("redukcja RMSE o ", round((rmse_plain - rmse_arimax) / rmse_plain * 100, 1), "%")))
        } else {
          lc_feedback(type = "info", p("Dummy nie poprawiło RMSE na danych historycznych."))
        }
      }
    )
  })

  zoom_plot_server("ch15_arimax_plot", reactive({
    ts_obj <- .ts_datasets[["noclegi"]]$get_ts()
    fits   <- ch15_arimax_fits()
    df     <- ts_to_df(ts_obj, "noclegi")

    if (is.null(fits)) {
      p <- ggplot(df, aes(x = date, y = noclegi)) +
        geom_line(color = upwr_secondary, linewidth = 0.9) +
        labs(x = NULL, y = "tys.", title = "Noclegi — kliknij 'Dopasuj oba modele'") +
        theme_upwr()
      return(p)
    }

    r_plain  <- if (!is.null(fits$plain))  as.numeric(residuals(fits$plain))  else NULL
    r_arimax <- if (!is.null(fits$arimax)) as.numeric(residuals(fits$arimax)) else NULL
    df$resid_plain  <- r_plain
    df$resid_arimax <- r_arimax

    df_long <- data.frame(
      date  = rep(df$date, 2),
      resid = c(df$resid_plain, df$resid_arimax),
      Model = rep(c("SARIMA (bez dummy)", "ARIMAX (z dummy COVID)"), each = nrow(df))
    )

    ggplot(df_long, aes(x = date, y = resid, color = Model)) +
      geom_line(linewidth = 0.8) +
      geom_hline(yintercept = 0, color = upwr_reference, linetype = "dashed") +
      scale_color_manual(values = c("SARIMA (bez dummy)" = upwr_secondary,
                                    "ARIMAX (z dummy COVID)" = upwr_accent), name = NULL) +
      labs(x = NULL, y = "Residuum (tys.)", title = "Residua: bez dummy vs. z dummy") +
      theme_upwr() +
      theme(legend.position = "bottom")
  }))
}
