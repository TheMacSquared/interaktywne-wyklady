# ============================================================================
# CHAPTER 3: HDI vs CI - przedzialy dwoch szkol
# ============================================================================

ch3_ui <- tabPanel("3. HDI vs CI",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "BF vs p-value porównali\u015bmy.
       Teraz dwa rodzaje przedzia\u0142\u00f3w: cz\u0119sto\u015bciowy CI i bayesowski HDI."
    ),

    div(class = "section-title", "CI i HDI maj\u0105 t\u0119 sam\u0105 nazw\u0119, inne znaczenie"),

    div(class = "narrative",
      p(tags$b("Cz\u0119sto\u015bciowy 95% CI:"),
         " je\u015bli powt\u00f3rzymy eksperyment bardzo wiele razy, to 95% tak skonstruowanych przedzia\u0142\u00f3w
         pokryje prawdziwy parametr. O ", tags$em("tym konkretnym"), " przedziale nie mo\u017cemy tak powiedzie\u0107 \u2014
         on albo obejmuje prawd\u0119, albo nie."),
      p(tags$b("Bayesowski 95% HDI"), " (Highest Density Interval):
         z prawdopodobie\u0144stwem 95% parametr le\u017cy w tym przedziale (warunkowo na danych i priorze).
         To jest stwierdzenie o ", tags$em("tym konkretnym"), " przedziale.")
    ),

    div(class = "callout-warning",
      tags$b("Pu\u0142apka j\u0119zykowa:"),
      " studenci cz\u0119sto m\u00f3wi\u0105 o CI to, co jest prawd\u0105 o HDI (\u201edaj\u0119 95% pewno\u015bci, \u017ce
       \u03bc jest tu\u201f). To cz\u0119sty b\u0142\u0105d interpretacyjny w cz\u0119sto\u015bciowej statystyce,
       kt\u00f3ry w bayesowskim \u015bwiecie po prostu ", tags$em("jest prawdziwy"), "."
    ),

    div(class = "section-title", "Pokaz na jednej próbie"),

    div(class = "widget-block",
      h4("Te same dane, dwa rodzaje przedzia\u0142\u00f3w"),
      fluidRow(column(12,
        fluidRow(
          column(4,
            sliderInput("ch3_n", "n:", min = 5, max = 200, value = 30, step = 5)
          ),
          column(4,
            sliderInput("ch3_true_mu", "Prawdziwe \u03bc:",
                        min = -1, max = 2, value = 0.5, step = 0.1)
          ),
          column(4,
            br(),
            actionButton("ch3_draw", "\u21bb Nowa pr\u00f3ba",
                         class = "btn-primary", width = "100%")
          )
        )
      )),

      fluidRow(
        column(6,
          div(class = "panel-frequentist",
            h5("95% CI (cz\u0119sto\u015bciowy)"),
            plotOutput("ch3_freq_plot", height = "280px"),
            uiOutput("ch3_freq_result")
          )
        ),
        column(6,
          div(class = "panel-bayesian",
            h5("95% HDI (bayesowski)"),
            plotOutput("ch3_bayes_plot", height = "280px"),
            uiOutput("ch3_bayes_result")
          )
        )
      ),

      div(class = "callout-info",
        uiOutput("ch3_comparison_narrative")
      )
    ),

    div(class = "section-title", "Kiedy CI \u2248 HDI?"),

    div(class = "narrative",
      p("Przy ", tags$b("nieinformatywnym priorze"), " i ", tags$b("du\u017cej pr\u00f3bie"),
         " oba przedzia\u0142y s\u0105 niemal identyczne numerycznie \u2014 ale interpretacja zostaje r\u00f3\u017cna."),
      p("Gdy prior jest silny lub pr\u00f3ba ma\u0142a, HDI \u201e\u015bci\u0105ga\u201f si\u0119 w stron\u0119 priora,
         a CI pozostaje oparty wy\u0142\u0105cznie na danych.")
    ),

    div(class = "chapter-transition",
      p("Teorii wystarczy. Wchodzimy w konkret \u2014 te same zastosowania, co we wnioskowaniu,
         ale z bayesowskim odpowiednikiem obok."),
      actionButton("ch3_next",
                   "Dalej: Jedna pr\u00f3ba \u2192",
                   class = "btn-primary btn-lg")
    )

  )) # column, fluidRow
)

ch3_server <- function(input, output, session) {

  sample_data <- reactiveVal(NULL)

  observe({
    if (is.null(sample_data())) {
      x <- rnorm(input$ch3_n, mean = input$ch3_true_mu, sd = 1)
      sample_data(x)
    }
  })

  observeEvent(list(input$ch3_draw, input$ch3_n, input$ch3_true_mu), {
    x <- rnorm(input$ch3_n, mean = input$ch3_true_mu, sd = 1)
    sample_data(x)
  }, ignoreInit = TRUE)

  bf_result <- reactive({
    x <- sample_data()
    req(x)
    compute_bf_one_sample(x, mu0 = 0)
  })

  # LEWA - CI czestosciowy, wizualizacja na osi
  output$ch3_freq_plot <- renderPlot({
    r <- bf_result()
    df <- data.frame(x = sample_data())
    ci <- r$ci_freq
    mean_x <- r$mean_x

    ggplot(df, aes(x = x)) +
      geom_histogram(bins = 20, fill = col_frequentist, color = "white", alpha = 0.6) +
      geom_vline(xintercept = mean_x, color = col_dark,
                 linewidth = 1.3) +
      geom_errorbarh(data = data.frame(y = 0.5, xmin = ci[1], xmax = ci[2]),
                     aes(y = y, xmin = xmin, xmax = xmax),
                     height = 0, color = col_frequentist, linewidth = 3,
                     inherit.aes = FALSE) +
      annotate("text", x = ci[1], y = Inf, label = round(ci[1], 2),
               vjust = -0.3, hjust = 1.1, color = col_frequentist, size = 3.5) +
      annotate("text", x = ci[2], y = Inf, label = round(ci[2], 2),
               vjust = -0.3, hjust = -0.1, color = col_frequentist, size = 3.5) +
      labs(title = "Dane + 95% CI (t-Studenta)",
           subtitle = "Przedzia\u0142 zbudowany metod\u0105, kt\u00f3ra w 95% przypadk\u00f3w pokrywa prawd\u0119",
           x = "Warto\u015b\u0107", y = "Liczno\u015b\u0107") +
      theme_educational()
  })

  output$ch3_freq_result <- renderUI({
    r <- bf_result()
    ci <- r$ci_freq
    div(class = "callout-info",
      tags$b("\u015arednia pr\u00f3by: "), round(r$mean_x, 3), tags$br(),
      tags$b("95% CI: "), "[", round(ci[1], 3), ", ", round(ci[2], 3), "]", tags$br(),
      tags$em("Interpretacja: je\u015bli powt\u00f3rzymy eksperyment wiele razy,
               95% takich przedzia\u0142\u00f3w pokryje \u03bc.")
    )
  })

  # PRAWA - HDI z posterior
  output$ch3_bayes_plot <- renderPlot({
    r <- bf_result()
    plot_posterior_density(
      r$posterior_mu,
      hdi = r$hdi,
      ref_value = 0,
      x_label = "\u03bc (posterior)",
      title = "Posterior dla \u03bc + 95% HDI",
      col_posterior = col_posterior,
      col_hdi = col_hdi
    )
  })

  output$ch3_bayes_result <- renderUI({
    r <- bf_result()
    hdi <- r$hdi
    div(class = "callout-info",
      tags$b("Mediana posterior: "), round(r$posterior_median, 3), tags$br(),
      tags$b("95% HDI: "), "[", round(hdi["lower"], 3), ", ",
      round(hdi["upper"], 3), "]", tags$br(),
      tags$em("Interpretacja: przy danych i priorze z prawdopodobie\u0144stwem 95%
               \u03bc le\u017cy w tym przedziale.")
    )
  })

  output$ch3_comparison_narrative <- renderUI({
    r <- bf_result()
    ci_width  <- r$ci_freq[2] - r$ci_freq[1]
    hdi_width <- r$hdi["upper"] - r$hdi["lower"]
    ratio <- hdi_width / ci_width
    tagList(
      tags$b("Por\u00f3wnanie szeroko\u015bci: "),
      "CI = ", round(ci_width, 3),
      "  |  HDI = ", round(hdi_width, 3),
      "  |  stosunek HDI/CI = ", round(ratio, 2),
      tags$br(),
      tags$em("Przy nieinformatywnym priorze i dostatecznie du\u017cej pr\u00f3bie te dwa przedzia\u0142y
               s\u0105 niemal to\u017csame numerycznie \u2014 r\u00f3\u017cni\u0105 si\u0119 tylko interpretacj\u0105.")
    )
  })
}
