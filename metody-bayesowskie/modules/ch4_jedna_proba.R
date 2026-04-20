# ============================================================================
# CHAPTER 4: Jedna proba - t-test vs ttestBF + posterior mu
# ============================================================================

ch4_ui <- tabPanel("4. Jedna pr\u00f3ba",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Pierwsze zastosowanie praktyczne: pr\u00f3bujemy rozstrzygn\u0105\u0107, czy \u015brednia w populacji
       r\u00f3\u017cni si\u0119 od pewnej warto\u015bci referencyjnej \u03bc\u2080."
    ),

    div(class = "section-title", "Typowa sytuacja"),

    div(class = "narrative",
      p("Producent deklaruje: \u201e\u015brednia waga paczki = 500 g\u201f. Mamy pr\u00f3b\u0119 30 paczek
         i chcemy sprawdzi\u0107, czy rzeczywi\u015bcie \u015brednia to 500."),
      p(tags$b("Cz\u0119sto\u015bciowo: "), "test t jednej pr\u00f3by z H\u2080: \u03bc = \u03bc\u2080. Liczymy p-warto\u015b\u0107."),
      p(tags$b("Bayesowsko: "), "ttestBF zwraca BF\u2081\u2080; dodatkowo dostajemy posterior dla \u03bc \u2014
         a wi\u0119c nie tylko \u201ecoś\u015b si\u0119 r\u00f3\u017cni\u201f, ale te\u017c \u201eo ile, z jakim HDI\u201f.")
    ),

    div(class = "widget-block",
      h4("Test jednej pr\u00f3by: te same dane, dwa paradygmaty"),

      fluidRow(column(12,
        fluidRow(
          column(3,
            sliderInput("ch4_n", "n:", min = 5, max = 200, value = 30, step = 5)
          ),
          column(3,
            sliderInput("ch4_true_mean", "Prawdziwa \u015brednia:",
                        min = 495, max = 510, value = 502, step = 0.5)
          ),
          column(3,
            sliderInput("ch4_sd", "SD populacji:",
                        min = 2, max = 20, value = 8, step = 1)
          ),
          column(3,
            sliderInput("ch4_mu0", "\u03bc\u2080 (H\u2080):",
                        min = 495, max = 510, value = 500, step = 0.5)
          )
        ),
        fluidRow(column(12,
          actionButton("ch4_draw", "\u21bb Nowa pr\u00f3ba",
                       class = "btn-primary", width = "200px")
        ))
      )),

      br(),
      fluidRow(
        column(6,
          div(class = "panel-frequentist",
            h5("Test t jednej pr\u00f3by"),
            plotOutput("ch4_freq_plot", height = "300px"),
            uiOutput("ch4_freq_result")
          )
        ),
        column(6,
          div(class = "panel-bayesian",
            h5("ttestBF + posterior \u03bc"),
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
      "nie tylko \u201ejest r\u00f3\u017cnica vs brak\u201f (binarna decyzja jak w test\u015bcie), ale te\u017c
       ", tags$em("rozk\u0142ad mo\u017cliwych warto\u015bci \u03bc"), " \u2014 mo\u017cesz spyta\u0107:
       jaka jest szansa, \u017ce prawdziwa \u015brednia \u2265 503? (policzysz j\u0105 bezpo\u015brednio z posterior.)"
    ),

    div(class = "chapter-transition",
      p("Por\u00f3wnali\u015bmy jedn\u0105 \u015bredni\u0105 z referencj\u0105.
         Teraz: dwie grupy \u2014 najcz\u0119stszy problem w badaniach empirycznych."),
      actionButton("ch4_next",
                   "Dalej: Dwie grupy \u2192",
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
                     title = paste0("Pr\u00f3ba (n = ", length(x), ")"),
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
      tags$b("\u015arednia pr\u00f3by: "), round(r$mean_x, 2), tags$br(),
      tags$b("95% CI: "), "[", round(ci[1], 2), ", ", round(ci[2], 2), "]"
    )
  })

  output$ch4_bayes_plot <- renderPlot({
    r <- result()
    plot_posterior_density(
      r$posterior_mu,
      hdi = r$hdi,
      ref_value = input$ch4_mu0,
      x_label = "\u03bc (posterior)",
      title = "Posterior dla \u03bc",
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
      tags$b("BF\u2081\u2080 = "), format_bf(r$bf10),
      " (", interp$level, " ", interp$direction, ")", tags$br(),
      tags$b("Mediana posterior \u03bc: "), round(r$posterior_median, 2), tags$br(),
      tags$b("95% HDI: "), "[", round(hdi["lower"], 2), ", ",
      round(hdi["upper"], 2), "]", tags$br(),
      tags$b("P(\u03bc > \u03bc\u2080 | dane) = "),
      paste0(round(prob_above_mu0 * 100, 1), "%")
    )
  })

  output$ch4_comparison <- renderUI({
    r <- result()
    direction <- if (r$mean_x > r$mu0) "wi\u0119ksza" else "mniejsza"
    verdict <- if (r$p_value < 0.05 && r$bf10 > 3) {
      paste0("Oba podej\u015bcia zgodne: \u015brednia w pr\u00f3bie jest ", direction,
             " od \u03bc\u2080, r\u00f3\u017cnica jest wyra\u017ana.")
    } else if (r$p_value >= 0.05 && r$bf10 < 1/3) {
      paste0("Oba podej\u015bcia zgodne: brak przes\u0142anek, \u017ceby \u015brednia r\u00f3\u017cni\u0142a si\u0119 od \u03bc\u2080 = ",
             r$mu0, ".")
    } else if (r$p_value >= 0.05 && r$bf10 > 3) {
      paste0("Cz\u0119sto\u015bciowo nieistotny, bayesowsko umiarkowany-silny dow\u00f3d ",
             interpret_bf(r$bf10)$direction,
             " \u2014 warto przyjrze\u0107 si\u0119, ile mamy danych.")
    } else {
      paste0("Rezultaty mieszane; pr\u00f3ba za ma\u0142a lub efekt subtelny.")
    }
    tagList(
      tags$b("Werdykt: "), verdict
    )
  })
}
