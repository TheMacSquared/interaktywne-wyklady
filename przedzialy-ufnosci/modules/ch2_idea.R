# ============================================================================
# CHAPTER 2: Idea przedzialow ufnosci
# ============================================================================

ch2_ui <- list(
  id    = "ch-idea",
  num   = "02",
  title = "Idea przedziałów",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 02 · Przedziały ufności",
      num    = "02",
      title  = "Idea przedziałów.",
      lead   = "Estymata punktowa zmienia się z próby na próbę.
                Czas dodać do niej zakres niepewności."
    ),

    lc_h2("ch2-czym-jest", "Czym jest przedział ufności?"),

    tagList(
      p("Przedział ufności (CI — ", tags$em("confidence interval"),
        ") to zakres wartości, który z określonym poziomem ufności
        (np. 95%) zawiera prawdziwy parametr populacji."),
      p("Kluczowa idea: gdybyśmy powtarzali eksperyment wiele razy,
        to 95% skonstruowanych przedziałów zawierałoby prawdziwe ",
        withMathJax("\\(\\mu\\)"), ".")
    ),

    lc_h2("ch2-wiele-ci", "Wiele przedziałów ufności"),

    tagList(
      p("To kluczowa wizualizacja. Każdy poziomy odcinek to jeden
        przedział ufności — skonstruowany z osobnej próby.
        Szałwiowe trafiają w ", withMathJax("\\(\\mu\\)"),
        ", terakotowe — nie."),
      p("Klikaj „Dolosuj” porcjami i obserwuj, jak pokrycie zbliża się
        do nominalnego poziomu ufności. Przy małej liczbie prób
        możesz mieć 80% lub 100%, ale przy 200+ pokrycie powinno
        ustabilizować się wokół 95%.")
    ),

    figure_panel(
      label = "Ryc. 2.1", title = "Symulacja przedziałów ufności",
      full_width = TRUE,
      fluidRow(
        column(4,
          selectInput("ch2_dist", "Rozkład populacji:",
            choices = c(
              "Normalny (wzrost)"         = "normal",
              "Wykładniczy (prawoskośny)" = "exponential",
              "Jednostajny"               = "uniform"
            ),
            selected = "normal"
          ),
          sliderInput("ch2_n", "Wielkość próby (n):",
                      min = 5, max = 100, value = 30, step = 5),
          sliderInput("ch2_conf", "Poziom ufności:",
                      min = 0.80, max = 0.99, value = 0.95, step = 0.01),
          hr(),
          lc_stack(gap = "md",
            actionButton("ch2_sim_10", "Dolosuj 10 przedziałów",
                         class = "lc-btn-primary", width = "100%"),
            actionButton("ch2_sim_50", "Dolosuj 50 przedziałów",
                         class = "lc-btn-warning", width = "100%"),
            actionButton("ch2_sim_reset", "Reset",
                         class = "lc-btn-secondary-outline", width = "100%")
          ),
          br(),
          uiOutput("ch2_coverage_info")
        ),
        column(8,
          plotOutput("ch2_ci_plot", height = "500px")
        )
      )
    ),

    lc_h2("ch2-jak-interpretowac", "Jak (nie) interpretować przedział ufności"),

    tagList(
      p("95% przedział ufności [165, 175] dla średniej wzrostu.
        Która interpretacja jest poprawna?")
    ),

    figure_panel(
      label = "Ryc. 2.2", title = "Quiz: interpretacja CI",
      full_width = TRUE,
      p("Wybierz poprawną interpretację:"),
      uiOutput("ch2_quiz_options"),
      uiOutput("ch2_quiz_feedback")
    ),

    margin_callout(label = "Częsty błąd", color = "uwaga",
      "Przedział ufności nie mówi o prawdopodobieństwie, że parametr
       leży w konkretnym przedziale. Parametr jest stały — to przedział
       jest losowy. Poprawnie: „metoda daje przedziały, które w 95%
       przypadków trafiają”."
    ),

    lc_chapter_next(
      num       = "03",
      title     = "Przedział dla średniej",
      lead      = "konkretne wzory: x̄ ± t·s/√n i jak je liczyć",
      target_id = "ch-srednia"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch2_server <- function(input, output, session) {

  # --- Widget 1: Symulacja przedzialow ufnosci (akumulacja) ---
  ch2_sim_data <- reactiveVal(NULL)

  # Helper: dolosuj k przedzialow i dodaj do akumulatora
  ch2_add_intervals <- function(k) {
    new_result <- simulate_coverage(
      dist_type = input$ch2_dist,
      n = input$ch2_n,
      conf_level = input$ch2_conf,
      n_sims = k,
      method = "t"
    )
    old <- ch2_sim_data()
    if (is.null(old)) {
      new_result$sim <- seq_len(nrow(new_result))
      ch2_sim_data(new_result)
    } else {
      new_result$sim <- nrow(old) + seq_len(nrow(new_result))
      ch2_sim_data(rbind(old, new_result))
    }
  }

  observeEvent(input$ch2_sim_10, { ch2_add_intervals(10) })
  observeEvent(input$ch2_sim_50, { ch2_add_intervals(50) })
  observeEvent(input$ch2_sim_reset, { ch2_sim_data(NULL) })

  # Reset przy zmianie parametrow (inaczej akumulujemy przedzialy z roznymi parametrami)
  observeEvent(input$ch2_dist, { ch2_sim_data(NULL) })
  observeEvent(input$ch2_n,    { ch2_sim_data(NULL) })
  observeEvent(input$ch2_conf, { ch2_sim_data(NULL) })

  output$ch2_ci_plot <- renderPlot({
    df <- ch2_sim_data()
    if (is.null(df) || nrow(df) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Dolosuj 10 przedziałów'",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      params <- get_population_params(input$ch2_dist)
      n_total <- nrow(df)
      # Skaluj grubosc linii i punktow do liczby przedzialow
      seg_lw <- if (n_total <= 50) 1.0 else if (n_total <= 150) 0.7 else 0.5
      pt_size <- if (n_total <= 50) 2.0 else if (n_total <= 150) 1.3 else 0.8

      ggplot(df, aes(y = sim)) +
        geom_vline(xintercept = params$mu, color = col_true,
                   linewidth = 1.2, linetype = "dashed") +
        geom_segment(aes(x = lower, xend = upper, yend = sim, color = covers),
                     linewidth = seg_lw) +
        geom_point(aes(x = xbar, color = covers), size = pt_size) +
        scale_color_manual(values = c("TRUE" = col_hit, "FALSE" = col_miss),
                           labels = c("TRUE" = "Trafiony", "FALSE" = "Chybiony"),
                           name = NULL) +
        labs(title = paste0(n_total, " przedziałów ufności (",
                            round(input$ch2_conf * 100), "%)"),
             x = "Wartość parametru",
             y = "Numer próby") +
        theme_upwr() +
        theme(legend.position = "top")
    }
  })

  output$ch2_coverage_info <- renderUI({
    df <- ch2_sim_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    n_total <- nrow(df)
    n_hits <- sum(df$covers)
    coverage <- round(n_hits / n_total * 100, 1)
    nominal <- round(input$ch2_conf * 100)
    # Kolor pokrycia: zielony jesli w +/- 5pp od nominalnego, czerwony w przeciwnym razie
    color <- if (abs(coverage - nominal) <= 5) col_hit else col_miss
    tagList(
      lc_stat_box("Prób", n_total, color = upwr_secondary),
      lc_stat_box("Pokrycie", coverage, "% (", n_hits, "/", n_total, ")", color = color),
      lc_stat_box("Oczekiwane", nominal, "%", color = col_ci)
    )
  })

  # --- Widget 2: Quiz (tiles) ---
  ch2_quiz_answered <- reactiveVal(FALSE)
  ch2_quiz_selected <- reactiveVal(NULL)

  ch2_quiz_choices <- list(
    list(letter = "A", value = "A",
         text = "Jest 95% prawdopodobieństwa, że μ leży w [165, 175]"),
    list(letter = "B", value = "B",
         text = "95% danych z populacji leży w [165, 175]"),
    list(letter = "C", value = "C",
         text = "Gdybyśmy powtarzali badanie, 95% tak skonstruowanych przedziałów zawierałoby μ"),
    list(letter = "D", value = "D",
         text = "Jesteśmy w 95% pewni, że średnia z próby leży w [165, 175]")
  )

  output$ch2_quiz_options <- renderUI({
    if (ch2_quiz_answered()) return(NULL)
    div(class = "quiz-tiles quiz-cols-4",
      lapply(ch2_quiz_choices, function(opt) {
        actionButton(paste0("ch2_tile_", opt$value),
          tagList(
            div(class = "tile-letter", opt$letter),
            div(class = "tile-text", opt$text)
          ),
          class = "quiz-tile"
        )
      })
    )
  })

  observe({
    for (opt in ch2_quiz_choices) {
      local({
        val <- opt$value
        observeEvent(input[[paste0("ch2_tile_", val)]], {
          if (ch2_quiz_answered()) return()
          ch2_quiz_selected(val)
          ch2_quiz_answered(TRUE)
        }, ignoreInit = TRUE)
      })
    }
  })

  output$ch2_quiz_feedback <- renderUI({
    req(ch2_quiz_answered())
    answer <- ch2_quiz_selected()
    if (answer == "C") {
      lc_feedback(type = "ok",
        tags$strong("Poprawnie!"),
        p("Przedział ufności opisuje ",
          tags$b("metodę"), ", nie konkretny wynik.
          95% przedziałów skonstruowanych tą metodą zawiera prawdziwe μ.")
      )
    } else {
      feedback <- switch(answer,
        "A" = "To najczęstszy błąd! μ jest stałe, nie losowe. To przedział jest losowy, nie parametr.",
        "B" = "Nie — przedział dotyczy parametru (średniej), nie poszczególnych obserwacji.",
        "D" = "Nie — średnia z próby zawsze leży w środku przedziału (jest punktem wyjścia)."
      )
      lc_feedback(type = "danger",
        tags$strong("Nie do końca!"),
        p(feedback),
        p("Poprawna odpowiedź to C.")
      )
    }
  })
}
