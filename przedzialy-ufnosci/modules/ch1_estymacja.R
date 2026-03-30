# ============================================================================
# CHAPTER 1: Od proby do populacji
# ============================================================================

ch1_ui <- tabPanel("1. Od pr\u00f3by do populacji",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Wiemy ju\u017c, \u017ce \u015brednia z pr\u00f3by zbiega do rozk\u0142adu normalnego (CTG).
       Teraz wykorzystamy to do szacowania parametr\u00f3w populacji."
    ),

    div(class = "section-title", "Estymacja \u2014 od pr\u00f3by do populacji"),

    div(class = "narrative",
      p("W statystyce rzadko znamy parametry ca\u0142ej populacji.
        Zamiast tego pobieramy ", tags$b("pr\u00f3b\u0119"), " i na jej podstawie
        ", tags$b("szacujemy"), " (estymujemy) nieznany parametr."),
      p("Na przyk\u0142ad: nie znamy \u015bredniego wzrostu wszystkich student\u00f3w
        w Polsce, ale mo\u017cemy zmierzy\u0107 100 os\u00f3b i obliczy\u0107 \u015bredni\u0105 z pr\u00f3by ",
        withMathJax("\\(\\bar{x}\\)"), " jako ", tags$b("estymator"),
        " \u015bredniej populacyjnej ", withMathJax("\\(\\mu\\)"), ".")
    ),

    # ========================================================================
    # WIDGET 1: Estymator w akcji
    # ========================================================================
    div(class = "section-title", "Estymator w akcji"),

    div(class = "narrative",
      p("Zobaczmy, jak dzia\u0142a estymacja. Znamy prawdziwe ",
        withMathJax("\\(\\mu\\)"), " populacji (fioletowa linia).
        Za ka\u017cdym razem losujemy pr\u00f3b\u0119 i obliczamy ",
        withMathJax("\\(\\bar{x}\\)"), ".")
    ),

    div(class = "widget-block",
      h4("Losowanie pr\u00f3b z populacji"),
      fluidRow(
        column(4,
          selectInput("ch1_dist", "Rozk\u0142ad populacji:",
            choices = c(
              "Normalny (wzrost)"           = "normal",
              "Wyk\u0142adniczy (prawosko\u015bny)" = "exponential",
              "Jednostajny"                 = "uniform",
              "Dwumodalny"                  = "bimodal"
            ),
            selected = "normal"
          ),
          sliderInput("ch1_n", "Wielko\u015b\u0107 pr\u00f3by (n):",
                      min = 5, max = 200, value = 30, step = 5),
          hr(),
          div(style = "display: flex; flex-direction: column; gap: 8px;",
            actionButton("ch1_draw_1", "Pobierz 1 pr\u00f3b\u0119",
                         class = "btn-primary", width = "100%"),
            actionButton("ch1_draw_20", "Pobierz 20 pr\u00f3b",
                         class = "btn-warning", width = "100%"),
            actionButton("ch1_reset", "Reset",
                         class = "btn-outline-secondary", width = "100%")
          ),
          br(),
          uiOutput("ch1_count_info")
        ),
        column(8,
          plotOutput("ch1_estimates_plot", height = "400px"),
          uiOutput("ch1_estimates_stats")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Obserwacja:"),
      " Ka\u017cda pr\u00f3ba daje inny wynik! Ale \u015brednie z pr\u00f3b
        skupiaj\u0105 si\u0119 wok\u00f3\u0142 prawdziwego ", withMathJax("\\(\\mu\\)"),
      ". Im wi\u0119ksze n, tym bli\u017cej."
    ),

    # ========================================================================
    # WIDGET 2: Obciazenie vs trafnosc
    # ========================================================================
    div(class = "section-title", "Obci\u0105\u017cenie vs trafno\u015b\u0107"),

    div(class = "narrative",
      p("Dobry estymator powinien by\u0107:"),
      tags$ul(
        tags$li(tags$b("Nieobci\u0105\u017cony"), " \u2014 \u015brednio trafia w prawdziw\u0105 warto\u015b\u0107 (brak systematycznego b\u0142\u0119du)"),
        tags$li(tags$b("Zgodny"), " \u2014 z wi\u0119ksz\u0105 pr\u00f3b\u0105 jest coraz dok\u0142adniejszy"),
        tags$li(tags$b("Efektywny"), " \u2014 ma ma\u0142\u0105 wariancj\u0119 (ma\u0142o si\u0119 waha)")
      ),
      p("Por\u00f3wnajmy trzy estymatory \u015bredniej: \u015bredni\u0105 arytmetyczn\u0105,
        median\u0119 i \u015bredni\u0105 obci\u0119t\u0105 (trimmed mean).")
    ),

    div(class = "widget-block",
      h4("Por\u00f3wnanie estymator\u00f3w"),
      fluidRow(
        column(4,
          selectInput("ch1_bias_dist", "Rozk\u0142ad populacji:",
            choices = c(
              "Normalny"                    = "normal",
              "Wyk\u0142adniczy (prawosko\u015bny)" = "exponential",
              "Jednostajny"                 = "uniform"
            ),
            selected = "normal"
          ),
          sliderInput("ch1_bias_n", "Wielko\u015b\u0107 pr\u00f3by (n):",
                      min = 10, max = 200, value = 30, step = 10),
          actionButton("ch1_bias_run", "Symuluj 500 pr\u00f3b",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch1_bias_plot", height = "350px"),
          uiOutput("ch1_bias_stats")
        )
      )
    ),

    # ========================================================================
    # WIDGET 3: Dlaczego punkt nie wystarczy?
    # ========================================================================
    div(class = "section-title", "Dlaczego sam punkt nie wystarczy?"),

    div(class = "narrative",
      p("Nawet najlepszy estymator punktowy zmienia si\u0119 z pr\u00f3by na pr\u00f3b\u0119.
        Podanie samej liczby ", withMathJax("\\(\\bar{x} = 171.3\\)"),
        " nie m\u00f3wi nic o tym, jak bardzo mo\u017cemy si\u0119 myli\u0107."),
      p("Potrzebujemy czego\u015b wi\u0119cej \u2014 ", tags$b("przedzia\u0142u"),
        ", kt\u00f3ry powie: ", tags$em("\"z 95% pewno\u015bci\u0105 prawdziwa warto\u015b\u0107 le\u017cy mi\u0119dzy ... a ...\""),
        ".")
    ),

    div(class = "widget-block",
      h4("Wahania estymatora"),
      fluidRow(
        column(4,
          sliderInput("ch1_fluct_n", "Wielko\u015b\u0107 pr\u00f3by (n):",
                      min = 5, max = 200, value = 10, step = 5),
          helpText("Ka\u017cde klikni\u0119cie losuje now\u0105 pr\u00f3b\u0119.
                    Obserwuj, jak bardzo skacze estymata."),
          actionButton("ch1_fluct_draw", "Losuj pr\u00f3b\u0119",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch1_fluct_plot", height = "300px")
        )
      )
    ),

    div(class = "callout-warning",
      tags$strong("Wniosek:"),
      " Estymacja punktowa to za ma\u0142o. Potrzebujemy ",
      tags$b("przedzia\u0142u ufno\u015bci"), " \u2014 zakresu warto\u015bci,
      kt\u00f3ry z okre\u015blonym prawdopodobie\u0144stwem zawiera prawdziwy parametr."
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: jak skonstruowa\u0107 taki przedzia\u0142?"),
      actionButton("ch1_next", "Dalej \u2192 2. Idea przedzia\u0142\u00f3w",
                   class = "btn-primary btn-lg")
    )
  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch1_server <- function(input, output, session) {

  # --- Widget 1: Estymator w akcji ---
  ch1_estimates <- reactiveVal(data.frame(
    i = integer(0), xbar = numeric(0)
  ))

  draw_samples <- function(k) {
    dist <- input$ch1_dist
    n <- input$ch1_n
    params <- get_population_params(dist)
    old <- ch1_estimates()
    new_rows <- lapply(seq_len(k), function(j) {
      samp <- generate_population_sample(dist, n)
      data.frame(i = nrow(old) + j, xbar = mean(samp))
    })
    ch1_estimates(rbind(old, do.call(rbind, new_rows)))
  }

  observeEvent(input$ch1_draw_1, draw_samples(1))
  observeEvent(input$ch1_draw_20, draw_samples(20))
  observeEvent(input$ch1_reset, {
    ch1_estimates(data.frame(i = integer(0), xbar = numeric(0)))
  })
  observeEvent(input$ch1_dist, {
    ch1_estimates(data.frame(i = integer(0), xbar = numeric(0)))
  })

  output$ch1_count_info <- renderUI({
    n_est <- nrow(ch1_estimates())
    div(class = "stat-box", style = paste0("background:", col_primary, ";"),
        paste0("Pr\u00f3b: ", n_est))
  })

  output$ch1_estimates_plot <- renderPlot({
    est <- ch1_estimates()
    params <- get_population_params(input$ch1_dist)

    if (nrow(est) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Pobierz pr\u00f3b\u0119'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      ggplot(est, aes(x = xbar)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30,
                       fill = col_ci, alpha = 0.6, color = "white") +
        geom_vline(xintercept = params$mu, color = col_true,
                   linewidth = 1.5, linetype = "dashed") +
        annotate("text", x = params$mu, y = Inf, vjust = 2,
                 label = paste0("\u03bc = ", params$mu),
                 color = col_true, fontface = "bold", size = 5) +
        labs(title = "Rozk\u0142ad estymat \u015bredniej",
             x = expression(bar(x)), y = "G\u0119sto\u015b\u0107") +
        theme_ci()
    }
  })

  output$ch1_estimates_stats <- renderUI({
    est <- ch1_estimates()
    if (nrow(est) == 0) return(NULL)
    params <- get_population_params(input$ch1_dist)
    tagList(
      div(class = "stat-box", style = paste0("background:", col_true, ";"),
          paste0("\u03bc = ", round(params$mu, 2))),
      div(class = "stat-box", style = paste0("background:", col_estimate, ";"),
          paste0("\u015ar. estymat = ", round(mean(est$xbar), 2))),
      div(class = "stat-box", style = paste0("background:", col_dark, ";"),
          paste0("SD estymat = ", round(sd(est$xbar), 2)))
    )
  })

  # --- Widget 2: Obciazenie vs trafnosc ---
  ch1_bias_data <- reactiveVal(NULL)

  observeEvent(input$ch1_bias_run, {
    dist <- input$ch1_bias_dist
    n <- input$ch1_bias_n
    results <- lapply(1:500, function(i) {
      samp <- generate_population_sample(dist, n)
      data.frame(
        mean = mean(samp),
        median = median(samp),
        trimmed = mean(samp, trim = 0.1)
      )
    })
    ch1_bias_data(do.call(rbind, results))
  })

  output$ch1_bias_plot <- renderPlot({
    df <- ch1_bias_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Symuluj 500 pr\u00f3b'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      params <- get_population_params(input$ch1_bias_dist)
      long_df <- tidyr::pivot_longer(df, cols = everything(),
                                      names_to = "estimator", values_to = "value")
      long_df$estimator <- factor(long_df$estimator,
        levels = c("mean", "median", "trimmed"),
        labels = c("\u015arednia", "Mediana", "\u015ar. obci\u0119ta (10%)"))

      ggplot(long_df, aes(x = value, fill = estimator)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30,
                       alpha = 0.6, color = "white") +
        geom_vline(xintercept = params$mu, color = col_true,
                   linewidth = 1.2, linetype = "dashed") +
        facet_wrap(~estimator, ncol = 1) +
        labs(title = "Rozk\u0142ad trzech estymator\u00f3w (500 pr\u00f3b)",
             x = "Warto\u015b\u0107 estymatora", y = "G\u0119sto\u015b\u0107") +
        scale_fill_manual(values = c(col_primary, col_secondary, col_warning)) +
        theme_ci() +
        theme(legend.position = "none")
    }
  })

  output$ch1_bias_stats <- renderUI({
    df <- ch1_bias_data()
    if (is.null(df)) return(NULL)
    params <- get_population_params(input$ch1_bias_dist)
    tagList(
      div(class = "stat-box", style = paste0("background:", col_true, ";"),
          paste0("\u03bc = ", round(params$mu, 2))),
      div(class = "stat-box", style = paste0("background:", col_primary, ";"),
          paste0("\u015ar. \u015bredniej = ", round(mean(df$mean), 2),
                 " (SD=", round(sd(df$mean), 2), ")")),
      div(class = "stat-box", style = paste0("background:", col_secondary, ";"),
          paste0("\u015ar. mediany = ", round(mean(df$median), 2),
                 " (SD=", round(sd(df$median), 2), ")")),
      div(class = "stat-box", style = paste0("background:", col_warning, ";"),
          paste0("\u015ar. obci\u0119tej = ", round(mean(df$trimmed), 2),
                 " (SD=", round(sd(df$trimmed), 2), ")"))
    )
  })

  # --- Widget 3: Wahania estymatora ---
  ch1_fluct_history <- reactiveVal(data.frame(
    draw = integer(0), xbar = numeric(0)
  ))

  observeEvent(input$ch1_fluct_draw, {
    samp <- generate_population_sample("normal", input$ch1_fluct_n)
    old <- ch1_fluct_history()
    ch1_fluct_history(rbind(old, data.frame(
      draw = nrow(old) + 1, xbar = mean(samp)
    )))
  })

  observeEvent(input$ch1_fluct_n, {
    ch1_fluct_history(data.frame(draw = integer(0), xbar = numeric(0)))
  })

  output$ch1_fluct_plot <- renderPlot({
    df <- ch1_fluct_history()
    params <- get_population_params("normal")

    if (nrow(df) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Losuj pr\u00f3b\u0119'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      ggplot(df, aes(x = draw, y = xbar)) +
        geom_hline(yintercept = params$mu, color = col_true,
                   linewidth = 1.2, linetype = "dashed") +
        geom_point(color = col_estimate, size = 3) +
        geom_line(color = col_estimate, alpha = 0.5) +
        annotate("text", x = max(df$draw), y = params$mu,
                 label = paste0("\u03bc = ", params$mu),
                 vjust = -1, color = col_true, fontface = "bold") +
        labs(title = paste0("Kolejne estymaty \u015bredniej (n = ", input$ch1_fluct_n, ")"),
             x = "Numer losowania", y = expression(bar(x))) +
        theme_ci()
    }
  })
}
