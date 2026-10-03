# ============================================================================
# CHAPTER 3: Bootstrap jednej proby
# ============================================================================

ch3_ui <- lecture_chapter(
  id = "ch-bootstrap-jednopr",
  num = "03",
  title = "Bootstrap jednej próby",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 03 · Symulacje statystyczne",
      num    = "03",
      title  = "Bootstrap jednej próby",
      lead   = "Krok po kroku budujemy rozkład bootstrapowy i sprawdzamy stabilność wyniku."
    ),

    lc_feedback(type = "info",
      "Bootstrap CI dla dużych prób działa świetnie.
       A dla jednej małej próby z niestandardowym rozkładem?
       To właśnie bootstrap jednej próby."
    ),

    lc_h2("ch3-sec-01", "Wnioskowanie o populacji z jednej próby"),

    tagList(
      p("Mamy próbę. Pytanie: co można powiedzieć o parametrze populacji?"),
      p("Klasyczny CI wymaga wzoru i założeń. Bootstrap CI nie wymaga.
         Dla mediany w ogóle nie istnieje prosty wzór analityczny —
         bootstrap wypełnia tę lukę automatycznie.")
    ),

    lc_feedback(type = "info",
      tags$strong("Algorytm bootstrap CI (krok po kroku):"),
      tags$ol(
        tags$li("Pobierz próbę x = (x₁, …, xₙ) z populacji"),
        tags$li("Dla b = 1, …, B: wylosuj xₖ* = n obserwacji ze zwracaniem z x"),
        tags$li("Oblicz θₖ* = statystyka(xₖ*)"),
        tags$li("95% CI = [percentyl 2.5%, percentyl 97.5%] z (θ₁*, …, θᴮ*)")
      )
    ),

    # ========================================================================
    # WIDGET 1: Krok po kroku
    # ========================================================================
    lc_h2("ch3-sec-02", "Bootstrap CI krok po kroku"),

    figure_panel(label = "Ryc. 3.1",
      lc_step_widget("ch3_boot",
        title = "Budowanie CI krok po kroku",
        steps = c("Dane", "Resample", "Rozkład", "CI"),
        toolbar = lc_toolbar(
          selectInput("ch3_scenario", "Scenariusz",
            choices = c(
              "Czas reakcji (skośny, n=18)"         = "reaction",
              "Zawartość białka (normalny, n=20)" = "protein",
              "Ocena satysfakcji (skala 1-10, n=15)"  = "satisfaction"
            ),
            selected = "reaction"
          ),
          lc_step_from(3, lc_slider("ch3_B", "B (próby bootstrapowe)", 100, 3000, 1000, 100)),
          lc_step_from(4, lc_slider("ch3_conf", "Poziom ufności", 0.80, 0.99, 0.95, 0.01)),
          lc_step_from(2, lc_action("ch3_resample", "Nowy resample", icon = "shuffle",
                                    variant = "outline")),
          lc_action("ch3_new_data", "Nowe dane", icon = "shuffle", variant = "outline")
        ),
        plot_id = "ch3_step_plot",
        extra = uiOutput("ch3_step_result")
      )
    ),

    # ========================================================================
    # WIDGET 2: Stabilnosc wg B
    # ========================================================================
    lc_h2("ch3-sec-03", "Ile prób bootstrapowych potrzeba?"),

    tagList(
      p("Szerokość CI stabilizuje się wraz z rosnącym B.
         Ponad pewną wartością B dodawanie kolejnych prób nic już nie zmienia.")
    ),

    figure_panel(label = "Ryc. 3.2", title = "Stabilność CI vs B",
      fluidRow(
        column(4,
          lc_slider("ch3_B_max", "Maksymalne B", 200, 5000, 2000, 200),
          selectInput("ch3_stab_stat", "Statystyka:",
            choices = c("Średniana" = "mean", "Mediana" = "median"),
            selected = "median"
          ),
          lc_action("ch3_B_run", "Pokaż stabilność", variant = "solid")
        ),
        column(8,
          zoom_plot_ui("ch3_B_stability", height = "260px")
        )
      )
    ),

    lc_feedback(type = "info",
      tags$strong("Praktyczna reguła:"),
      tags$ul(
        tags$li(tags$b("B = 1000"), " — wystarczy dla przedziałów ufności (orientacyjne)"),
        tags$li(tags$b("B = 2000–5000"), " — dla dokładnych CI"),
        tags$li(tags$b("B ≥ 10 000"), " — dla p-wartości (testy permutacyjne)")
      )
    ),

    # ========================================================================
    # QUIZ
    # ========================================================================
    lc_h2("ch3-sec-04", "Quiz: która metoda?"),

    tagList(
      p("Próba n = 15 czasów reakcji kierowcy, wyraźnie prawoskosńna.
         Mediana = 320ms. Pytanie: czy mediana populacji różni się od 300ms?")
    ),

    figure_panel(label = "Ryc. 3.3", title = "Wybierz odpowiednie podejście:",
      uiOutput("ch3_quiz_options"),
      uiOutput("ch3_quiz_feedback")
    ),

    lc_chapter_next(
      num = "04",
      title = "Testy permutacyjne",
      lead = "jak testować hipotezy przez przetasowanie etykiet.",
      target_id = "ch-permutacje"
    )

  )
)
# ============================================================================
# SERVER
# ============================================================================

ch3_server <- function(input, output, session) {

  # Krok widgetu (1..4) żyje w przeglądarce. Losowania są reaktywne: dane od
  # scenariusza i przycisku „Nowe dane”, resample od danych i „Nowy resample”,
  # rozkład bootstrapowy od danych i B. Przyciski losowania przesuwają krok.
  ch3_s    <- lc_step_server("ch3_boot", input)
  ch3_step <- ch3_s$step

  # Parametry scenariuszy
  ch3_scenario_params <- reactive({
    switch(input$ch3_scenario,
      "reaction"     = list(n = 18, dist = "skewed",     stat = median,
                             stat_lbl = "Mediana czasu reakcji (ms)"),
      "protein"      = list(n = 20, dist = "normal",     stat = mean,
                             stat_lbl = "Średniana zawartości białka"),
      "satisfaction" = list(n = 15, dist = "heavy_tail", stat = median,
                             stat_lbl = "Mediana oceny satysfakcji")
    )
  })

  # Krok 1: dane
  ch3_data <- reactive({
    input$ch3_new_data
    params <- ch3_scenario_params()
    generate_sample_data(params$n, dist = params$dist)
  })

  observeEvent(input$ch3_new_data, ch3_s$set(1), ignoreInit = TRUE)

  # Krok 2: jeden resample (indeksy, żeby liczyć krotność także przy remisach)
  ch3_one_rs <- reactive({
    input$ch3_resample
    x   <- ch3_data()
    idx <- sample(seq_along(x), size = length(x), replace = TRUE)
    list(values = x[idx], freq = tabulate(idx, nbins = length(x)))
  })

  observeEvent(input$ch3_resample, ch3_s$set(2), ignoreInit = TRUE)

  # Krok 3: pelny rozklad (B resampli)
  ch3_boot_res <- reactive({
    run_bootstrap(ch3_data(), ch3_scenario_params()$stat, B = input$ch3_B)
  })

  ch3_ci <- reactive({
    bootstrap_ci_percentile(ch3_boot_res(), conf_level = input$ch3_conf)
  })

  # Stała rama kroków 3–4: zakres i liczebności rozkładu bootstrapowego.
  ch3_boot_frame <- reactive({
    result <- ch3_boot_res()
    lims   <- range(result$boot_stats, result$observed)
    pad    <- diff(lims) * 0.06 + 1e-9
    breaks <- seq(lims[1] - pad, lims[2] + pad, length.out = 41)
    counts <- hist(result$boot_stats, breaks = breaks, plot = FALSE)$counts
    list(breaks = breaks, xlim = range(breaks), ylim = c(0, max(counts) * 1.15))
  })

  zoom_plot_server("ch3_step_plot", reactive({
    step   <- ch3_step()
    x      <- ch3_data()
    params <- ch3_scenario_params()
    obs    <- params$stat(x)

    # Rama kroków 1–2: zakres danych.
    pad    <- diff(range(x)) * 0.06 + 1e-9
    x_lims <- range(x) + c(-pad, pad)

    if (step == 1) {
      breaks <- seq(x_lims[1], x_lims[2], length.out = 16)
      y_max  <- max(hist(x, breaks = breaks, plot = FALSE)$counts) * 1.15
      ggplot(data.frame(val = x), aes(x = val)) +
        step_result(geom_histogram, breaks = breaks) +
        step_line("new", xintercept = obs) +
        step_label(obs, y_max, paste0(" obs = ", round(obs, 2)),
                   role = "new", vjust = 1) +
        labs(x = params$stat_lbl, y = "Liczebność") +
        step_frame(xlim = x_lims, ylim = c(0, y_max))
    } else if (step == 2) {
      rs <- ch3_one_rs()
      freq_role <- ifelse(rs$freq == 0, "background",
                          ifelse(rs$freq == 1, "data", "group"))
      df_orig <- data.frame(x = x, y = 0,
        colour = vapply(freq_role, function(r) STEP_ROLES[[r]]$colour, ""),
        alpha  = vapply(freq_role, function(r) STEP_ROLES[[r]]$alpha, 0))
      df_boot <- data.frame(x = rs$values, y = 1)
      ggplot() +
        geom_jitter(data = df_orig, aes(x = x, y = y),
                    colour = df_orig$colour, alpha = df_orig$alpha,
                    height = 0.12, size = 3) +
        step_layer(geom_jitter, "new", data = df_boot, mapping = aes(x = x, y = y),
                   height = 0.12, size = 2.5) +
        step_line("known", xintercept = mean(x)) +
        step_layer(geom_segment, "new",
                   data = data.frame(xm = mean(rs$values)),
                   mapping = aes(x = xm, xend = xm, y = 0.65, yend = 1.35),
                   linetype = "22") +
        scale_y_continuous(breaks = 0:1,
                           labels = c("Oryginalna próba", "Bootstrap 1")) +
        labs(x = "Wartość", y = NULL) +
        step_frame(xlim = x_lims, ylim = c(-0.5, 1.5))
    } else {
      result <- ch3_boot_res()
      fr     <- ch3_boot_frame()
      p <- ggplot(data.frame(stat = result$boot_stats), aes(x = stat)) +
        step_result(geom_histogram, breaks = fr$breaks) +
        step_line("known", xintercept = result$observed) +
        labs(x = params$stat_lbl, y = "Liczba prób")
      if (step == 4) {
        ci <- ch3_ci()
        p <- p +
          step_line("new", xintercept = ci$lower) +
          step_line("new", xintercept = ci$upper) +
          step_label(result$observed, fr$ylim[2],
                     paste0(" obs = ", round(result$observed, 2)),
                     role = "known", vjust = 1)
      }
      p + step_frame(xlim = fr$xlim, ylim = fr$ylim)
    }
  }))

  output$ch3_boot_text <- renderUI({
    step   <- ch3_step()
    params <- ch3_scenario_params()
    switch(as.character(step),
      "1" = paste0("Próba pobrana. Obserwowana statystyka: ",
                   round(params$stat(ch3_data()), 3), ". Teraz wylosujemy z niej próbę bootstrapową."),
      "2" = paste0("Jedna próba bootstrapowa (ze zwracaniem). Jej statystyka będzie się nieco różnić od oryginalnej. ",
                   "Średnia oryginału: ", round(mean(ch3_data()), 2),
                   ", średnia próby bootstrapowej: ", round(mean(ch3_one_rs()$values), 2), "."),
      "3" = paste0("Rozkład bootstrapowy z B = ", input$ch3_B,
                   " prób. Odch. stand. = SE = ", round(ch3_boot_res()$se, 4), "."),
      "4" = {
        ci <- ch3_ci()
        paste0(round(input$ch3_conf * 100), "% bootstrap CI: [",
               round(ci$lower, 3), ", ", round(ci$upper, 3), "].")
      },
      ""
    )
  })

  # Odczyty pod wykresem: w kroku 2 legenda krotności, w kroku 4 wynik CI.
  output$ch3_step_result <- renderUI({
    step <- ch3_step()
    if (step == 2) {
      freq <- ch3_one_rs()$freq
      bg   <- grDevices::adjustcolor(STEP_ROLES$background$colour,
                                     alpha.f = STEP_ROLES$background$alpha)
      lc_status(lc_readouts(
        lc_readout("Pominięty (0x)", sum(freq == 0), color = bg, swatch = TRUE),
        lc_readout("Raz (1x)", sum(freq == 1),
                   color = STEP_ROLES$data$colour, swatch = TRUE),
        lc_readout("Wielokrotnie (2x+)", sum(freq >= 2),
                   color = STEP_ROLES$group$colour, swatch = TRUE)
      ))
    } else if (step == 4) {
      ci  <- ch3_ci()
      res <- ch3_boot_res()
      lc_status(lc_readouts(
        lc_readout("Dół", lc_fmt(ci$lower, 3), color = STEP_ROLES$new$colour),
        lc_readout("Obs", lc_fmt(res$observed, 3), color = STEP_ROLES$known$colour),
        lc_readout("Góra", lc_fmt(ci$upper, 3), color = STEP_ROLES$new$colour),
        lc_readout("SE", lc_fmt(res$se, 4), color = STEP_ROLES$data$colour)
      ))
    }
  })

  # --- Widget 2: Stabilnosc wg B ---
  zoom_plot_server("ch3_B_stability", reactive({
    input$ch3_B_run
    isolate({
      x <- ch3_data()
      stat_fn <- if (input$ch3_stab_stat == "mean") mean else median
      B_max   <- input$ch3_B_max
      B_seq   <- unique(c(seq(50, min(500, B_max), by = 50),
                          seq(500, B_max, by = 200)))
      B_seq   <- B_seq[B_seq <= B_max]

      widths <- vapply(B_seq, function(b) {
        res <- run_bootstrap(x, stat_fn, B = b)
        ci  <- bootstrap_ci_percentile(res, conf_level = 0.95)
        ci$width
      }, numeric(1))

      df <- data.frame(B = B_seq, width = widths)
      ggplot(df, aes(x = B, y = width)) +
        geom_line(color = sim_bootstrap, linewidth = 1.5) +
        geom_point(color = sim_bootstrap, size = 2) +
        geom_vline(xintercept = 1000, color = sim_observed,
                   linetype = "dashed", linewidth = 1) +
        annotate("text", x = 1000, y = max(widths),
                 label = "B = 1000", hjust = -0.1, color = sim_observed, size = 4) +
        labs(
             
             x = "B (liczba prób bootstrapowych)",
             y = "Szerokość 95% CI") +
        theme_upwr()
    })
  }))

  # --- Quiz ---
  ch3_quiz_answered <- reactiveVal(FALSE)
  ch3_quiz_selected <- reactiveVal(NULL)

  ch3_quiz_choices <- list(
    list(letter = "A", value = "A", text = "T-test (test t dla jednej próby)"),
    list(letter = "B", value = "B", text = "Test Wilcoxona (test znaku / rang)"),
    list(letter = "C", value = "C", text = "Bootstrap CI dla mediany"),
    list(letter = "D", value = "D", text = "Z-test (rozkad normalny)")
  )

  output$ch3_quiz_options <- renderUI({
    if (ch3_quiz_answered()) return(NULL)
    div(class = "quiz-tiles quiz-cols-2",
      lapply(ch3_quiz_choices, function(opt) {
        actionButton(paste0("ch3_tile_", opt$value),
          tagList(
            div(class = "tile-letter", opt$letter),
            div(class = "tile-text",   opt$text)
          ),
          class = "quiz-tile"
        )
      })
    )
  })

  observe({
    for (opt in ch3_quiz_choices) {
      local({
        val <- opt$value
        observeEvent(input[[paste0("ch3_tile_", val)]], {
          if (ch3_quiz_answered()) return()
          ch3_quiz_selected(val)
          ch3_quiz_answered(TRUE)
        }, ignoreInit = TRUE)
      })
    }
  })

  output$ch3_quiz_feedback <- renderUI({
    req(ch3_quiz_answered())
    answer <- ch3_quiz_selected()

    if (answer %in% c("B", "C")) {
      lc_feedback(type = "ok",
        tags$strong("Dobrze!"),
        p(
          if (answer == "B") {
            "Test Wilcoxona (test rang) testuje H₀: mediana = 300 bez założenia normalności.
             Daje p-wartość, ale nie daje CI dla mediany."
          } else {
            "Bootstrap CI dla mediany jest idealny: brak założeń i daje pełny CI.
             Można sprawdzić czy 300ms leży w przedziale."
          }
        ),
        p(tags$b("Obie odpowiedzi (B i C) są uzasadnione"),
          " — B daje p-wartość, C daje przedział ufności.
           W praktyce często stosuje się oba.")
      )
    } else if (answer == "A") {
      lc_feedback(type = "danger",
        tags$strong("Nie do końca."),
        p("T-test dla jednej próby testuje średnią, nie medianę.
           Przy silnie skośnych danych i n=15, t-test jest wątpliwy.
           Poprawne: B lub C.")
      )
    } else {
      lc_feedback(type = "danger",
        tags$strong("Nie."),
        p("Z-test wymaga znania σ i normalności populacji.
           Nie ma zastosowania tutaj. Poprawne: B lub C.")
      )
    }
  })

}
