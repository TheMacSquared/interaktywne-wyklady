# ============================================================================
# CHAPTER 4: Jedna proba - t-test vs ttestBF + posterior mu
# ============================================================================

ch4_ui <- tabPanel("4. Jedna próba",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Pierwsze zastosowanie praktyczne: próbujemy rozstrzygnąć, czy średnia w populacji
       różni się od pewnej wartości referencyjnej μ₀."
    ),

    div(class = "section-title", "Typowa sytuacja"),

    div(class = "narrative",
      p("Producent deklaruje: „średnia waga paczki = 500 g‟. Mamy próbę 30 paczek
         i chcemy sprawdzić, czy rzeczywiście średnia to 500."),
      p(tags$b("Częstościowo: "), "test t jednej próby z H₀: μ = μ₀. Liczymy p-wartość."),
      p(tags$b("Bayesowsko: "), "ttestBF zwraca BF₁₀; dodatkowo dostajemy posterior dla μ —
         a więc nie tylko „cośś się różni‟, ale też „o ile, z jakim HDI‟.")
    ),

    div(class = "widget-block",
      h4("Test jednej próby: te same dane, dwa paradygmaty"),

      fluidRow(column(12,
        fluidRow(
          column(3,
            sliderInput("ch4_n", "n:", min = 5, max = 200, value = 30, step = 5)
          ),
          column(3,
            sliderInput("ch4_true_mean", "Prawdziwa średnia:",
                        min = 495, max = 510, value = 502, step = 0.5)
          ),
          column(3,
            sliderInput("ch4_sd", "SD populacji:",
                        min = 2, max = 20, value = 8, step = 1)
          ),
          column(3,
            sliderInput("ch4_mu0", "μ₀ (H₀):",
                        min = 495, max = 510, value = 500, step = 0.5)
          )
        ),
        fluidRow(column(12,
          actionButton("ch4_draw", "↻ Nowa próba",
                       class = "btn-primary", width = "200px")
        ))
      )),

      br(),
      fluidRow(
        column(6,
          div(class = "panel-frequentist",
            h5("Test t jednej próby"),
            plotOutput("ch4_freq_plot", height = "300px"),
            uiOutput("ch4_freq_result")
          )
        ),
        column(6,
          div(class = "panel-bayesian",
            h5("ttestBF + posterior μ"),
            plotOutput("ch4_bayes_plot", height = "300px"),
            uiOutput("ch4_bayes_result")
          )
        )
      ),

      div(class = "callout-info",
        uiOutput("ch4_comparison")
      )
    ),

    div(class = "callout-success",
      tags$strong("Co daje Bayes dodatkowo: "),
      "nie tylko „jest różnica vs brak‟ (binarna decyzja jak w testście), ale też
       ", tags$em("rozkład możliwych wartości μ"), " — możesz spytać:
       jaka jest szansa, że prawdziwa średnia ≥ 503? (policzysz ją bezpośrednio z posterior.)"
    ),

    div(class = "chapter-transition",
      p("Porównaliśmy jedną średnią z referencją.
         Teraz: dwie grupy — najczęstszy problem w badaniach empirycznych."),
      actionButton("ch4_next",
                   "Dalej: Dwie grupy →",
                   class = "btn-primary btn-lg")
    )

  )) # column, fluidRow
)

ch4_server <- function(input, output, session) {

  sample_data <- reactiveVal(NULL)

  observe({
    if (is.null(sample_data())) {
      x <- rnorm(input$ch4_n, mean = input$ch4_true_mean, sd = input$ch4_sd)
      sample_data(x)
    }
  })

  observeEvent(list(input$ch4_draw, input$ch4_n, input$ch4_true_mean,
                    input$ch4_sd), {
    x <- rnorm(input$ch4_n, mean = input$ch4_true_mean, sd = input$ch4_sd)
    sample_data(x)
  }, ignoreInit = TRUE)

  result <- reactive({
    x <- sample_data()
    req(x)
    compute_bf_one_sample(x, mu0 = input$ch4_mu0)
  })

  output$ch4_freq_plot <- renderPlot({
    x <- sample_data()
    req(x)
    plot_sample_data(x, mu0 = input$ch4_mu0,
                     title = paste0("Próba (n = ", length(x), ")"),
                     col_freq = col_frequentist)
  })

  output$ch4_freq_result <- renderUI({
    r <- result()
    p_info <- format_pval_pl(r$p_value)
    ci <- r$ci_freq
    div(class = "callout-info",
      tags$b("t = "), round(r$t_statistic, 3),
      " | df = ", round(r$df, 1), tags$br(),
      HTML(p_info$decision), tags$br(),
      tags$b("Średnia próby: "), round(r$mean_x, 2), tags$br(),
      tags$b("95% CI: "), "[", round(ci[1], 2), ", ", round(ci[2], 2), "]"
    )
  })

  output$ch4_bayes_plot <- renderPlot({
    r <- result()
    plot_posterior_density(
      r$posterior_mu,
      hdi = r$hdi,
      ref_value = input$ch4_mu0,
      x_label = "μ (posterior)",
      title = "Posterior dla μ",
      col_posterior = col_posterior,
      col_hdi = col_hdi
    )
  })

  output$ch4_bayes_result <- renderUI({
    r <- result()
    interp <- interpret_bf(r$bf10)
    hdi <- r$hdi
    prob_above_mu0 <- mean(r$posterior_mu > r$mu0)
    div(class = "callout-info",
      tags$b("BF₁₀ = "), format_bf(r$bf10),
      " (", interp$level, " ", interp$direction, ")", tags$br(),
      tags$b("Mediana posterior μ: "), round(r$posterior_median, 2), tags$br(),
      tags$b("95% HDI: "), "[", round(hdi["lower"], 2), ", ",
      round(hdi["upper"], 2), "]", tags$br(),
      tags$b("P(μ > μ₀ | dane) = "),
      paste0(round(prob_above_mu0 * 100, 1), "%")
    )
  })

  output$ch4_comparison <- renderUI({
    r <- result()
    direction <- if (r$mean_x > r$mu0) "większa" else "mniejsza"
    verdict <- if (r$p_value < 0.05 && r$bf10 > 3) {
      paste0("Oba podejścia zgodne: średnia w próbie jest ", direction,
             " od μ₀, różnica jest wyraźna.")
    } else if (r$p_value >= 0.05 && r$bf10 < 1/3) {
      paste0("Oba podejścia zgodne: brak przesłanek, żeby średnia różniła się od μ₀ = ",
             r$mu0, ".")
    } else if (r$p_value >= 0.05 && r$bf10 > 3) {
      paste0("Częstościowo nieistotny, bayesowsko umiarkowany-silny dowód ",
             interpret_bf(r$bf10)$direction,
             " — warto przyjrzeć się, ile mamy danych.")
    } else {
      paste0("Rezultaty mieszane; próba za mała lub efekt subtelny.")
    }
    tagList(
      tags$b("Werdykt: "), verdict
    )
  })
}
