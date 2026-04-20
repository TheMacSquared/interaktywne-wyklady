# ============================================================================
# CHAPTER 2: Idea przedzialow ufnosci
# ============================================================================

ch2_ui <- tabPanel("2. Idea przedzia\u0142\u00f3w",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Estymata punktowa zmienia si\u0119 z pr\u00f3by na pr\u00f3b\u0119.
       Czas doda\u0107 do niej zakres niepewno\u015bci."
    ),

    div(class = "section-title", "Czym jest przedzia\u0142 ufno\u015bci?"),

    div(class = "narrative",
      p("Przedzia\u0142 ufno\u015bci (CI \u2014 confidence interval) to zakres warto\u015bci,
        kt\u00f3ry z okre\u015blonym poziomem ufno\u015bci (np. 95%) zawiera prawdziwy parametr populacji."),
      p("Kluczowa idea: gdyby\u015bmy powtarzali eksperyment wiele razy,
        to 95% skonstruowanych przedzia\u0142\u00f3w zawiera\u0142oby prawdziwe ",
        withMathJax("\\(\\mu\\)"), ".")
    ),

    # ========================================================================
    # WIDGET 1: Symulacja przedzialow ufnosci
    # ========================================================================
    div(class = "section-title", "Wiele przedzia\u0142\u00f3w ufno\u015bci"),

    div(class = "narrative",
      p("To kluczowa wizualizacja. Ka\u017cdy poziomy odcinek to jeden przedzia\u0142
        ufno\u015bci \u2014 skonstruowany z osobnej pr\u00f3by. Zielone trafiaj\u0105 w ",
        withMathJax("\\(\\mu\\)"), ", czerwone \u2014 nie."),
      p("Klikaj \"Dolosuj\" porcjami i obserwuj, jak pokrycie zbli\u017ca si\u0119 do nominalnego poziomu ufno\u015bci.
        Przy ma\u0142ej liczbie pr\u00f3b mo\u017cesz mie\u0107 80% lub 100%, ale przy 200+ pokrycie powinno
        ustabilizowa\u0107 si\u0119 wok\u00f3\u0142 95%.")
    ),

    div(class = "widget-block",
      h4("Symulacja przedzia\u0142\u00f3w ufno\u015bci"),
      fluidRow(
        column(4,
          selectInput("ch2_dist", "Rozk\u0142ad populacji:",
            choices = c(
              "Normalny (wzrost)"           = "normal",
              "Wyk\u0142adniczy (prawosko\u015bny)" = "exponential",
              "Jednostajny"                 = "uniform"
            ),
            selected = "normal"
          ),
          sliderInput("ch2_n", "Wielko\u015b\u0107 pr\u00f3by (n):",
                      min = 5, max = 100, value = 30, step = 5),
          sliderInput("ch2_conf", "Poziom ufno\u015bci:",
                      min = 0.80, max = 0.99, value = 0.95, step = 0.01),
          hr(),
          div(style = "display: flex; flex-direction: column; gap: 8px;",
            actionButton("ch2_sim_10", "Dolosuj 10 przedzia\u0142\u00f3w",
                         class = "btn-primary", width = "100%"),
            actionButton("ch2_sim_50", "Dolosuj 50 przedzia\u0142\u00f3w",
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
    div(class = "section-title", "Jak (nie) interpretowa\u0107 przedzia\u0142 ufno\u015bci"),

    div(class = "narrative",
      p("95% przedzia\u0142 ufno\u015bci [165, 175] dla \u015bredniej wzrostu.
        Kt\u00f3ra interpretacja jest poprawna?")
    ),

    div(class = "widget-block",
      h4("Quiz: interpretacja CI"),
      p("Wybierz poprawn\u0105 interpretacj\u0119:"),
      uiOutput("ch2_quiz_options"),
      uiOutput("ch2_quiz_feedback")
    ),

    div(class = "callout-danger",
      tags$strong("Cz\u0119sty b\u0142\u0105d:"),
      " Przedzia\u0142 ufno\u015bci nie m\u00f3wi o prawdopodobie\u0144stwie, \u017ce parametr le\u017cy w konkretnym przedziale.
        Parametr jest sta\u0142y \u2014 to przedzia\u0142 jest losowy!
        Poprawnie: \"metoda daje przedzia\u0142y, kt\u00f3re w 95% przypadk\u00f3w trafaj\u0105\"."
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: konkretne wzory \u2014 przedzia\u0142 dla \u015bredniej"),
      actionButton("ch2_next", "Dalej \u2192 3. Przedzia\u0142 dla \u015bredniej",
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
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Dolosuj 10 przedzia\u0142\u00f3w'",
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
        labs(title = paste0(n_total, " przedzia\u0142\u00f3w ufno\u015bci (",
                            round(input$ch2_conf * 100), "%)"),
             x = "Warto\u015b\u0107 parametru",
             y = "Numer pr\u00f3by") +
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
          paste0("Pr\u00f3b: ", n_total)),
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
         text = "Jest 95% prawdopodobie\u0144stwa, \u017ce \u03bc le\u017cy w [165, 175]"),
    list(letter = "B", value = "B",
         text = "95% danych z populacji le\u017cy w [165, 175]"),
    list(letter = "C", value = "C",
         text = "Gdyby\u015bmy powtarzali badanie, 95% tak skonstruowanych przedzia\u0142\u00f3w zawiera\u0142oby \u03bc"),
    list(letter = "D", value = "D",
         text = "Jeste\u015bmy w 95% pewni, \u017ce \u015brednia z pr\u00f3by le\u017cy w [165, 175]")
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
        p("Przedzia\u0142 ufno\u015bci opisuje ",
          tags$b("metod\u0119"), ", nie konkretny wynik.
          95% przedzia\u0142\u00f3w skonstruowanych t\u0105 metod\u0105 zawiera prawdziwe \u03bc.")
      )
    } else {
      feedback <- switch(answer,
        "A" = "To najcz\u0119stszy b\u0142\u0105d! \u03bc jest sta\u0142e, nie losowe. To przedzia\u0142 jest losowy, nie parametr.",
        "B" = "Nie \u2014 przedzia\u0142 dotyczy parametru (\u015bredniej), nie poszczeg\u00f3lnych obserwacji.",
        "D" = "Nie \u2014 \u015brednia z pr\u00f3by zawsze le\u017cy w \u015brodku przedzia\u0142u (jest punktem wyj\u015bcia)."
      )
      div(class = "callout-danger",
        tags$strong("Nie do ko\u0144ca!"),
        p(feedback),
        p("Poprawna odpowied\u017a to C.")
      )
    }
  })
}
