# ============================================================================
# CHAPTER 3: HDI vs CI - przedzialy dwoch szkol
# ============================================================================

ch3_ui <- tabPanel("3. HDI vs CI",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "BF vs p-value porównaliśmy.
       Teraz dwa rodzaje przedziałów: częstościowy CI i bayesowski HDI."
    ),

    div(class = "section-title", "CI i HDI mają tę samą nazwę, inne znaczenie"),

    div(class = "narrative",
      p(tags$b("Częstościowy 95% CI:"),
         " jeśli powtórzymy eksperyment bardzo wiele razy, to 95% tak skonstruowanych przedziałów
         pokryje prawdziwy parametr. O ", tags$em("tym konkretnym"), " przedziale nie możemy tak powiedzieć —
         on albo obejmuje prawdę, albo nie."),
      p(tags$b("Bayesowski 95% HDI"), " (Highest Density Interval):
         z prawdopodobieństwem 95% parametr leży w tym przedziale (warunkowo na danych i priorze).
         To jest stwierdzenie o ", tags$em("tym konkretnym"), " przedziale.")
    ),

    div(class = "callout-warning",
      tags$b("Pułapka językowa:"),
      " studenci często mówią o CI to, co jest prawdą o HDI („daję 95% pewności, że
       μ jest tu‟). To częsty błąd interpretacyjny w częstościowej statystyce,
       który w bayesowskim świecie po prostu ", tags$em("jest prawdziwy"), "."
    ),

    div(class = "section-title", "Pokaz na jednej próbie"),

    div(class = "widget-block",
      h4("Te same dane, dwa rodzaje przedziałów"),
      fluidRow(column(12,
        fluidRow(
          column(4,
            sliderInput("ch3_n", "n:", min = 5, max = 200, value = 30, step = 5)
          ),
          column(4,
            sliderInput("ch3_true_mu", "Prawdziwe μ:",
                        min = -1, max = 2, value = 0.5, step = 0.1)
          ),
          column(4,
            br(),
            actionButton("ch3_draw", "↻ Nowa próba",
                         class = "btn-primary", width = "100%")
          )
        )
      )),

      fluidRow(
        column(6,
          div(class = "panel-frequentist",
            h5("95% CI (częstościowy)"),
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

    div(class = "section-title", "Kiedy CI ≈ HDI?"),

    div(class = "narrative",
      p("Przy ", tags$b("nieinformatywnym priorze"), " i ", tags$b("dużej próbie"),
         " oba przedziały są niemal identyczne numerycznie — ale interpretacja zostaje różna."),
      p("Gdy prior jest silny lub próba mała, HDI „ściąga‟ się w stronę priora,
         a CI pozostaje oparty wyłącznie na danych.")
    ),

    div(class = "chapter-transition",
      p("Teorii wystarczy. Wchodzimy w konkret — te same zastosowania, co we wnioskowaniu,
         ale z bayesowskim odpowiednikiem obok."),
      actionButton("ch3_next",
                   "Dalej: Jedna próba →",
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
           subtitle = "Przedział zbudowany metodą, która w 95% przypadków pokrywa prawdę",
           x = "Wartość", y = "Liczność") +
      theme_educational()
  })

  output$ch3_freq_result <- renderUI({
    r <- bf_result()
    ci <- r$ci_freq
    div(class = "callout-info",
      tags$b("Średnia próby: "), round(r$mean_x, 3), tags$br(),
      tags$b("95% CI: "), "[", round(ci[1], 3), ", ", round(ci[2], 3), "]", tags$br(),
      tags$em("Interpretacja: jeśli powtórzymy eksperyment wiele razy,
               95% takich przedziałów pokryje μ.")
    )
  })

  # PRAWA - HDI z posterior
  output$ch3_bayes_plot <- renderPlot({
    r <- bf_result()
    plot_posterior_density(
      r$posterior_mu,
      hdi = r$hdi,
      ref_value = 0,
      x_label = "μ (posterior)",
      title = "Posterior dla μ + 95% HDI",
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
      tags$em("Interpretacja: przy danych i priorze z prawdopodobieństwem 95%
               μ leży w tym przedziale.")
    )
  })

  output$ch3_comparison_narrative <- renderUI({
    r <- bf_result()
    ci_width  <- r$ci_freq[2] - r$ci_freq[1]
    hdi_width <- r$hdi["upper"] - r$hdi["lower"]
    ratio <- hdi_width / ci_width
    tagList(
      tags$b("Porównanie szerokości: "),
      "CI = ", round(ci_width, 3),
      "  |  HDI = ", round(hdi_width, 3),
      "  |  stosunek HDI/CI = ", round(ratio, 2),
      tags$br(),
      tags$em("Przy nieinformatywnym priorze i dostatecznie dużej próbie te dwa przedziały
               są niemal tożsame numerycznie — różnią się tylko interpretacją.")
    )
  })
}
