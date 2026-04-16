# ============================================================================
# CHAPTER 3: Bootstrap jednej proby
# ============================================================================

ch3_ui <- tabPanel("3. Bootstrap jednej pr\u00f3by",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Bootstrap CI dla du\u017cych pr\u00f3b dzia\u0142a \u015bwietnie.
       A dla jednej ma\u0142ej pr\u00f3by z niestandardowym rozk\u0142adem?
       To w\u0142a\u015bnie bootstrap jednej pr\u00f3by."
    ),

    div(class = "section-title",
        "Wnioskowanie o populacji z jednej pr\u00f3by"),

    div(class = "narrative",
      p("Mamy pr\u00f3b\u0119. Pytanie: co mo\u017cna powiedzie\u0107 o parametrze populacji?"),
      p("Klasyczny CI wymaga wzoru i za\u0142o\u017ce\u0144. Bootstrap CI nie wymaga.
         Dla mediany w og\u00f3le nie istnieje prosty wz\u00f3r analityczny \u2014
         bootstrap wype\u0142nia t\u0119 luk\u0119 automatycznie.")
    ),

    div(class = "callout-info",
      tags$strong("Algorytm bootstrap CI (krok po kroku):"),
      tags$ol(
        tags$li("Pobierz pr\u00f3b\u0119 x = (x\u2081, \u2026, x\u2099) z populacji"),
        tags$li("Dla b = 1, \u2026, B: wylosuj x\u2096* = n obserwacji ze zwracaniem z x"),
        tags$li("Oblicz \u03b8\u2096* = statystyka(x\u2096*)"),
        tags$li("95% CI = [percentyl 2.5%, percentyl 97.5%] z (\u03b8\u2081*, \u2026, \u03b8\u1d2e*)")
      )
    ),

    # ========================================================================
    # WIDGET 1: Krok po kroku
    # ========================================================================
    div(class = "section-title", "Bootstrap CI krok po kroku"),

    div(class = "widget-block",
      h4("Budowanie CI krok po kroku"),
      fluidRow(
        column(4,
          selectInput("ch3_scenario", "Scenariusz:",
            choices = c(
              "Czas reakcji (sko\u015bny, n=18)"         = "reaction",
              "Zawarto\u015b\u0107 bia\u0142ka (normalny, n=20)" = "protein",
              "Ocena satysfakcji (skala 1-10, n=15)"  = "satisfaction"
            ),
            selected = "reaction"
          ),
          sliderInput("ch3_B", "B (pr\u00f3by bootstrapowe):",
                      min = 100, max = 3000, value = 1000, step = 100),
          sliderInput("ch3_conf", "Poziom ufno\u015bci:",
                      min = 0.80, max = 0.99, value = 0.95, step = 0.01),
          hr(),
          div(class = "step-buttons",
            actionButton("ch3_step1", "1. Dane",
                         class = "btn-outline-primary"),
            actionButton("ch3_step2", "2. Resample",
                         class = "btn-outline-primary")
          ),
          div(class = "step-buttons",
            actionButton("ch3_step3", "3. Rozk\u0142ad",
                         class = "btn-outline-primary"),
            actionButton("ch3_step4", "4. CI",
                         class = "btn-outline-success")
          ),
          br(),
          actionButton("ch3_new_data", "\u21ba Nowe dane",
                       class = "btn-outline-secondary btn-sm", width = "100%"),
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
    div(class = "section-title", "Ile pr\u00f3b bootstrapowych potrzeba?"),

    div(class = "narrative",
      p("Szeroko\u015b\u0107 CI stabilizuje si\u0119 wraz z rosn\u0105cym B.
         Ponad pewn\u0105 warto\u015bci\u0105 B dodawanie kolejnych pr\u00f3b nic ju\u017c nie zmienia.")
    ),

    div(class = "widget-block",
      h4("Stabilno\u015b\u0107 CI vs B"),
      fluidRow(
        column(4,
          sliderInput("ch3_B_max", "Maksymalne B:",
                      min = 200, max = 5000, value = 2000, step = 200),
          selectInput("ch3_stab_stat", "Statystyka:",
            choices = c("\u015aredniana" = "mean", "Mediana" = "median"),
            selected = "median"
          ),
          actionButton("ch3_B_run", "Poka\u017c stabilno\u015b\u0107",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch3_B_stability", height = "260px")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Praktyczna regu\u0142a:"),
      tags$ul(
        tags$li(tags$b("B = 1000"), " \u2014 wystarczy dla przedzia\u0142\u00f3w ufno\u015bci (orientacyjne)"),
        tags$li(tags$b("B = 2000\u20135000"), " \u2014 dla dok\u0142adnych CI"),
        tags$li(tags$b("B \u2265 10\u202f000"), " \u2014 dla p-warto\u015bci (testy permutacyjne)")
      )
    ),

    # ========================================================================
    # QUIZ
    # ========================================================================
    div(class = "section-title", "Quiz: kt\u00f3ra metoda?"),

    div(class = "narrative",
      p("Pr\u00f3ba n = 15 czas\u00f3w reakcji kierowcy, wyra\u017anie prawoskos\u0144na.
         Mediana = 320ms. Pytanie: czy mediana populacji r\u00f3\u017cni si\u0119 od 300ms?")
    ),

    div(class = "widget-block",
      h4("Wybierz odpowiednie podej\u015bcie:"),
      uiOutput("ch3_quiz_options"),
      uiOutput("ch3_quiz_feedback")
    ),

    div(class = "chapter-transition",
      p("Dalej: testowanie hipotez bez za\u0142o\u017ce\u0144 \u2014 testy permutacyjne"),
      actionButton("ch3_next",
                   "Dalej \u2192 4. Testy permutacyjne",
                   class = "btn-primary btn-lg")
    )

  ))
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
                             stat_lbl = "\u015aredniana zawarto\u015bci bia\u0142ka"),
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
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else if (step == 1) {
      df <- data.frame(val = x, idx = seq_along(x))
      ggplot(df, aes(x = val)) +
        geom_histogram(bins = 15, fill = col_primary, color = "white", alpha = 0.8) +
        geom_vline(xintercept = params$stat(x), color = col_secondary,
                   linewidth = 1.4, linetype = "dashed") +
        annotate("text", x = params$stat(x), y = Inf,
                 label = paste0("obs = ", round(params$stat(x), 2)),
                 vjust = -0.3, hjust = -0.1, color = col_secondary, size = 4) +
        labs(title = paste0("Krok 1: Pr\u00f3ba (n = ", length(x), ")"),
             x = params$stat_lbl, y = "Liczebno\u015b\u0107") +
        theme_educational()
    } else if (step == 2) {
      rs <- ch3_one_rs()
      plot_bootstrap_step(x, rs, col_primary, col_warning, col_dark)
    } else if (step >= 3) {
      result <- ch3_boot_res()
      ci     <- bootstrap_ci_percentile(result, conf_level = input$ch3_conf)
      if (step == 3) {
        # Tylko rozklad bez CI
        df <- data.frame(stat = result$boot_stats)
        ggplot(df, aes(x = stat)) +
          geom_histogram(bins = 40, fill = col_primary, color = "white", alpha = 0.8) +
          geom_vline(xintercept = result$observed, color = col_secondary,
                     linewidth = 1.4) +
          labs(title = paste0("Krok 3: Rozk\u0142ad bootstrapowy (B = ", result$B, ")"),
               x = params$stat_lbl, y = "Liczba pr\u00f3b") +
          theme_educational()
      } else {
        # Krok 4: z CI
        plot_bootstrap_distribution(result, ci,
                                     stat_label = params$stat_lbl,
                                     col_primary = col_primary,
                                     col_secondary = col_secondary,
                                     col_success = col_success,
                                     conf_level = input$ch3_conf)
      }
    }
  })

  output$ch3_step_explanation <- renderUI({
    step   <- ch3_step()
    params <- ch3_scenario_params()
    txt <- switch(as.character(step),
      "0" = "Kliknij kolejne kroki, aby przej\u015b\u0107 przez algorytm bootstrap CI.",
      "1" = paste0("Pr\u00f3ba pobrana. Obserwowana statystyka: ",
                   round(params$stat(ch3_data()), 3), ". Teraz wylosujemy z niej pr\u00f3b\u0119 bootstrapow\u0105."),
      "2" = "Jedna pr\u00f3ba bootstrapowa (ze zwracaniem). Jej statystyka b\u0119dzie si\u0119 nieco r\u00f3\u017cni\u0107 od oryginalnej.",
      "3" = paste0("Rozk\u0142ad bootstrapowy z B = ", input$ch3_B,
                   " pr\u00f3b. Odch. stand. = SE = ", round(ch3_boot_res()$se, 4), "."),
      "4" = {
        ci <- bootstrap_ci_percentile(ch3_boot_res(), conf_level = input$ch3_conf)
        paste0(round(input$ch3_conf * 100), "% bootstrap CI: [",
               round(ci$lower, 3), ", ", round(ci$upper, 3), "].")
      },
      ""
    )
    div(class = "callout-info", txt)
  })

  output$ch3_step_result <- renderUI({
    req(ch3_step() == 4, ch3_boot_res())
    ci <- bootstrap_ci_percentile(ch3_boot_res(), conf_level = input$ch3_conf)
    tagList(
      div(class = "stat-box", style = paste0("background:", col_success, ";"),
          paste0("D\u00f3\u0142: ", round(ci$lower, 3))),
      div(class = "stat-box", style = paste0("background:", col_secondary, ";"),
          paste0("Obs: ", round(ch3_boot_res()$observed, 3))),
      div(class = "stat-box", style = paste0("background:", col_success, ";"),
          paste0("G\u00f3ra: ", round(ci$upper, 3))),
      div(class = "stat-box", style = paste0("background:", col_primary, ";"),
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
        geom_line(color = col_primary, linewidth = 1.5) +
        geom_point(color = col_primary, size = 2) +
        geom_vline(xintercept = 1000, color = col_secondary,
                   linetype = "dashed", linewidth = 1) +
        annotate("text", x = 1000, y = max(widths),
                 label = "B = 1000", hjust = -0.1, color = col_secondary, size = 4) +
        labs(title = "Szeroko\u015b\u0107 CI vs liczba pr\u00f3b bootstrapowych",
             subtitle = "Plateau po ok. B = 1000",
             x = "B (liczba pr\u00f3b bootstrapowych)",
             y = "Szeroko\u015b\u0107 95% CI") +
        theme_educational()
    })
  })

  # --- Quiz ---
  ch3_quiz_answered <- reactiveVal(FALSE)
  ch3_quiz_selected <- reactiveVal(NULL)

  ch3_quiz_choices <- list(
    list(letter = "A", value = "A", text = "T-test (test t dla jednej pr\u00f3by)"),
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
      div(class = "callout-success",
        tags$strong("Dobrze!"),
        p(
          if (answer == "B") {
            "Test Wilcoxona (test rang) testuje H\u2080: mediana = 300 bez za\u0142o\u017cenia normalno\u015bci.
             Daje p-warto\u015b\u0107, ale nie daje CI dla mediany."
          } else {
            "Bootstrap CI dla mediany jest idealny: brak za\u0142o\u017ce\u0144 i daje pe\u0142ny CI.
             Mo\u017cna sprawdzi\u0107 czy 300ms le\u017cy w przedziale."
          }
        ),
        p(tags$b("Obie odpowiedzi (B i C) s\u0105 uzasadnione"),
          " \u2014 B daje p-warto\u015b\u0107, C daje przedzia\u0142 ufno\u015bci.
           W praktyce cz\u0119sto stosuje si\u0119 oba.")
      )
    } else if (answer == "A") {
      div(class = "callout-danger",
        tags$strong("Nie do ko\u0144ca."),
        p("T-test dla jednej pr\u00f3by testuje \u015bredni\u0105, nie median\u0119.
           Przy silnie sko\u015bnych danych i n=15, t-test jest w\u0105tpliwy.
           Poprawne: B lub C.")
      )
    } else {
      div(class = "callout-danger",
        tags$strong("Nie."),
        p("Z-test wymaga znania \u03c3 i normalno\u015bci populacji.
           Nie ma zastosowania tutaj. Poprawne: B lub C.")
      )
    }
  })

}
