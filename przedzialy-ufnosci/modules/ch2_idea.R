# ============================================================================
# CHAPTER 2: Idea przedzialow ufnosci
# ============================================================================

ch2_ui <- tabPanel("2. Idea przedziałów",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Estymata punktowa zmienia się z próby na próbę.
       Czas dodać do niej zakres niepewności."
    ),

    div(class = "section-title", "Czym jest przedział ufności?"),

    div(class = "narrative",
      p("Przedział ufności (CI — confidence interval) to zakres wartości,
        który z określonym poziomem ufności (np. 95%) zawiera prawdziwy parametr populacji."),
      p("Kluczowa idea: gdybyśmy powtarzali eksperyment wiele razy,
        to 95% skonstruowanych przedziałów zawierałoby prawdziwe ",
        withMathJax("\\(\\mu\\)"), ".")
    ),

    # ========================================================================
    # WIDGET 1: Symulacja przedzialow ufnosci
    # ========================================================================
    div(class = "section-title", "Wiele przedziałów ufności"),

    div(class = "narrative",
      p("To kluczowa wizualizacja. Każdy poziomy odcinek to jeden przedział
        ufności — skonstruowany z osobnej próby. Zielone trafiają w ",
        withMathJax("\\(\\mu\\)"), ", czerwone — nie."),
      p("Klikaj \"Dolosuj\" porcjami i obserwuj, jak pokrycie zbliża się do nominalnego poziomu ufności.
        Przy małej liczbie prób możesz mieć 80% lub 100%, ale przy 200+ pokrycie powinno
        ustabilizować się wokół 95%.")
    ),

    div(class = "widget-block",
      h4("Symulacja przedziałów ufności"),
      fluidRow(
        column(4,
          selectInput("ch2_dist", "Rozkład populacji:",
            choices = c(
              "Normalny (wzrost)"           = "normal",
              "Wykładniczy (prawoskośny)" = "exponential",
              "Jednostajny"                 = "uniform"
            ),
            selected = "normal"
          ),
          sliderInput("ch2_n", "Wielkość próby (n):",
                      min = 5, max = 100, value = 30, step = 5),
          sliderInput("ch2_conf", "Poziom ufności:",
                      min = 0.80, max = 0.99, value = 0.95, step = 0.01),
          hr(),
          div(style = "display: flex; flex-direction: column; gap: 8px;",
            actionButton("ch2_sim_10", "Dolosuj 10 przedziałów",
                         class = "btn-primary", width = "100%"),
            actionButton("ch2_sim_50", "Dolosuj 50 przedziałów",
                         class = "btn-warning", width = "100%"),
            actionButton("ch2_sim_reset", "Reset",
                         class = "btn-outline-secondary", width = "100%")
          ),
          br(),
          uiOutput("ch2_coverage_info")
        ),
        column(8,
          plotOutput("ch2_ci_plot", height = "500px")
        )
      )
    ),

    # ========================================================================
    # WIDGET 2: Czesty blad interpretacji
    # ========================================================================
    div(class = "section-title", "Jak (nie) interpretować przedział ufności"),

    div(class = "narrative",
      p("95% przedział ufności [165, 175] dla średniej wzrostu.
        Która interpretacja jest poprawna?")
    ),

    div(class = "widget-block",
      h4("Quiz: interpretacja CI"),
      p("Wybierz poprawną interpretację:"),
      uiOutput("ch2_quiz_options"),
      uiOutput("ch2_quiz_feedback")
    ),

    div(class = "callout-danger",
      tags$strong("Częsty błąd:"),
      " Przedział ufności nie mówi o prawdopodobieństwie, że parametr leży w konkretnym przedziale.
        Parametr jest stały — to przedział jest losowy!
        Poprawnie: \"metoda daje przedziały, które w 95% przypadków trafają\"."
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: konkretne wzory — przedział dla średniej"),
      actionButton("ch2_next", "Dalej → 3. Przedział dla średniej",
                   class = "btn-primary btn-lg")
    )
  ))
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
                 size = 6, color = "#7f8c8d") +
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
        theme_educational() +
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
      div(class = "stat-box", style = paste0("background:", col_dark, ";"),
          paste0("Prób: ", n_total)),
      div(class = "stat-box", style = paste0("background:", color, ";"),
          paste0("Pokrycie: ", coverage, "% (", n_hits, "/", n_total, ")")),
      div(class = "stat-box", style = paste0("background:", col_primary, ";"),
          paste0("Oczekiwane: ", nominal, "%"))
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
      div(class = "callout-success",
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
      div(class = "callout-danger",
        tags$strong("Nie do końca!"),
        p(feedback),
        p("Poprawna odpowiedź to C.")
      )
    }
  })
}
