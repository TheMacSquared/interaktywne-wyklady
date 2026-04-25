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

    figure_panel(label = "Ryc. 3.1", title = "Budowanie CI krok po kroku",
      fluidRow(
        column(4,
          selectInput("ch3_scenario", "Scenariusz:",
            choices = c(
              "Czas reakcji (skośny, n=18)"         = "reaction",
              "Zawartość białka (normalny, n=20)" = "protein",
              "Ocena satysfakcji (skala 1-10, n=15)"  = "satisfaction"
            ),
            selected = "reaction"
          ),
          sliderInput("ch3_B", "B (próby bootstrapowe):",
                      min = 100, max = 3000, value = 1000, step = 100),
          sliderInput("ch3_conf", "Poziom ufności:",
                      min = 0.80, max = 0.99, value = 0.95, step = 0.01),
          hr(),
          div(class = "step-buttons",
            actionButton("ch3_step1", "1. Dane",
                         class = "lc-btn-outline"),
            actionButton("ch3_step2", "2. Resample",
                         class = "lc-btn-outline")
          ),
          div(class = "step-buttons",
            actionButton("ch3_step3", "3. Rozkład",
                         class = "lc-btn-outline"),
            actionButton("ch3_step4", "4. CI",
                         class = "lc-btn-ok-outline")
          ),
          br(),
          actionButton("ch3_new_data", "↺ Nowe dane",
                       class = "lc-btn-secondary-outline lc-btn-sm", width = "100%"),
          br(), br(),
          uiOutput("ch3_step_explanation")
        ),
        column(8,
          plotOutput("ch3_step_plot", height = "400px"),
          uiOutput("ch3_step_result")
        )
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
          sliderInput("ch3_B_max", "Maksymalne B:",
                      min = 200, max = 5000, value = 2000, step = 200),
          selectInput("ch3_stab_stat", "Statystyka:",
            choices = c("Średniana" = "mean", "Mediana" = "median"),
            selected = "median"
          ),
          actionButton("ch3_B_run", "Pokaż stabilność",
                       class = "lc-btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch3_B_stability", height = "260px")
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

  ch3_step     <- reactiveVal(0)
  ch3_data     <- reactiveVal(NULL)
  ch3_boot_res <- reactiveVal(NULL)
  ch3_one_rs   <- reactiveVal(NULL)   # jeden resample do kroku 2

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

  # Reset przy zmianie scenariusza
  observeEvent(input$ch3_scenario, {
    ch3_step(0); ch3_data(NULL); ch3_boot_res(NULL); ch3_one_rs(NULL)
  })

  observeEvent(input$ch3_new_data, {
    ch3_step(0); ch3_data(NULL); ch3_boot_res(NULL); ch3_one_rs(NULL)
  })

  # Krok 1: dane
  observeEvent(input$ch3_step1, {
    params <- ch3_scenario_params()
    x      <- generate_sample_data(params$n, dist = params$dist)
    ch3_data(x)
    ch3_step(1)
  })

  # Krok 2: jeden resample
  observeEvent(input$ch3_step2, {
    req(ch3_data())
    x  <- ch3_data()
    rs <- sample(x, size = length(x), replace = TRUE)
    ch3_one_rs(rs)
    ch3_step(2)
  })

  # Krok 3: pelny rozklad (B resampli)
  observeEvent(input$ch3_step3, {
    req(ch3_data())
    params <- ch3_scenario_params()
    result <- run_bootstrap(ch3_data(), params$stat, B = input$ch3_B)
    ch3_boot_res(result)
    ch3_step(3)
  })

  # Krok 4: CI
  observeEvent(input$ch3_step4, {
    req(ch3_boot_res())
    ch3_step(4)
  })

  output$ch3_step_plot <- renderPlot({
    step   <- ch3_step()
    x      <- ch3_data()
    params <- ch3_scenario_params()

    if (step == 0 || is.null(x)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij '1. Dane'",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else if (step == 1) {
      df <- data.frame(val = x, idx = seq_along(x))
      ggplot(df, aes(x = val)) +
        geom_histogram(bins = 15, fill = sim_bootstrap, color = "white", alpha = 0.8) +
        geom_vline(xintercept = params$stat(x), color = sim_observed,
                   linewidth = 1.4, linetype = "dashed") +
        annotate("text", x = params$stat(x), y = Inf,
                 label = paste0("obs = ", round(params$stat(x), 2)),
                 vjust = -0.3, hjust = -0.1, color = sim_observed, size = 4) +
        labs(title = paste0("Krok 1: Próba (n = ", length(x), ")"),
             x = params$stat_lbl, y = "Liczebność") +
        theme_upwr()
    } else if (step == 2) {
      rs <- ch3_one_rs()
      plot_bootstrap_step(x, rs, sim_bootstrap, sim_warning, sim_secondary)
    } else if (step >= 3) {
      result <- ch3_boot_res()
      ci     <- bootstrap_ci_percentile(result, conf_level = input$ch3_conf)
      if (step == 3) {
        # Tylko rozklad bez CI
        df <- data.frame(stat = result$boot_stats)
        ggplot(df, aes(x = stat)) +
          geom_histogram(bins = 40, fill = sim_bootstrap, color = "white", alpha = 0.8) +
          geom_vline(xintercept = result$observed, color = sim_observed,
                     linewidth = 1.4) +
          labs(title = paste0("Krok 3: Rozkład bootstrapowy (B = ", result$B, ")"),
               x = params$stat_lbl, y = "Liczba prób") +
          theme_upwr()
      } else {
        # Krok 4: z CI
        plot_bootstrap_distribution(result, ci,
                                     stat_label = params$stat_lbl,
                                     sim_bootstrap = sim_bootstrap,
                                     sim_observed = sim_observed,
                                     sim_success = sim_success,
                                     conf_level = input$ch3_conf)
      }
    }
  })

  output$ch3_step_explanation <- renderUI({
    step   <- ch3_step()
    params <- ch3_scenario_params()
    txt <- switch(as.character(step),
      "0" = "Kliknij kolejne kroki, aby przejść przez algorytm bootstrap CI.",
      "1" = paste0("Próba pobrana. Obserwowana statystyka: ",
                   round(params$stat(ch3_data()), 3), ". Teraz wylosujemy z niej próbę bootstrapową."),
      "2" = "Jedna próba bootstrapowa (ze zwracaniem). Jej statystyka będzie się nieco różnić od oryginalnej.",
      "3" = paste0("Rozkład bootstrapowy z B = ", input$ch3_B,
                   " prób. Odch. stand. = SE = ", round(ch3_boot_res()$se, 4), "."),
      "4" = {
        ci <- bootstrap_ci_percentile(ch3_boot_res(), conf_level = input$ch3_conf)
        paste0(round(input$ch3_conf * 100), "% bootstrap CI: [",
               round(ci$lower, 3), ", ", round(ci$upper, 3), "].")
      },
      ""
    )
    lc_feedback(type = "info", txt)
  })

  output$ch3_step_result <- renderUI({
    req(ch3_step() == 4, ch3_boot_res())
    ci <- bootstrap_ci_percentile(ch3_boot_res(), conf_level = input$ch3_conf)
    tagList(
      div(class = "lc-stat-box", style = paste0("background:", sim_success, ";"),
          paste0("Dół: ", round(ci$lower, 3))),
      div(class = "lc-stat-box", style = paste0("background:", sim_observed, ";"),
          paste0("Obs: ", round(ch3_boot_res()$observed, 3))),
      div(class = "lc-stat-box", style = paste0("background:", sim_success, ";"),
          paste0("Góra: ", round(ci$upper, 3))),
      div(class = "lc-stat-box", style = paste0("background:", sim_bootstrap, ";"),
          paste0("SE: ", round(ch3_boot_res()$se, 4)))
    )
  })

  # --- Widget 2: Stabilnosc wg B ---
  output$ch3_B_stability <- renderPlot({
    input$ch3_B_run
    isolate({
      if (is.null(ch3_data())) {
        # Generuj dane jesli nie ma
        x <- generate_sample_data(20, dist = "skewed")
      } else {
        x <- ch3_data()
      }
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
        labs(title = "Szerokość CI vs liczba prób bootstrapowych",
             subtitle = "Plateau po ok. B = 1000",
             x = "B (liczba prób bootstrapowych)",
             y = "Szerokość 95% CI") +
        theme_upwr()
    })
  })

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
