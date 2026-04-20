# ============================================================================
# CHAPTER 9: Regresja liniowa - lm() vs stan_glm()
# ============================================================================

ch9_ui <- tabPanel("9. Regresja liniowa",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Zamiast jednej liczby (r) albo \u015bredniej \u2014 model:
       y = \u03b2\u2080 + \u03b2\u2081 x + \u03b5. Kt\u00f3re \u03b2 s\u0105 istotne? Jakie maj\u0105 warto\u015bci?"
    ),

    div(class = "section-title", "lm() vs stan_glm()"),

    div(class = "narrative",
      p(tags$b("Cz\u0119sto\u015bciowo: "), "lm(y ~ x) estymuje \u03b2 metod\u0105 najmniejszych kwadrat\u00f3w,
         zwraca p-warto\u015bci dla ka\u017cdego wsp\u00f3\u0142czynnika i 95% CI."),
      p(tags$b("Bayesowsko: "), "stan_glm(y ~ x, family = gaussian) pr\u00f3bkuje posterior
         dla ka\u017cdego \u03b2. Dostajemy median\u0119 + 95% HDI \u2014 plausible range dla si\u0142y efektu.")
    ),

    div(class = "callout-warning",
      tags$b("Uwaga wydajno\u015bciowa: "),
      "stan_glm uruchamia sampler MCMC \u2014 pierwszy fit trwa 5-15 sekund.
       Kolejne fitowania tego samego modelu s\u0105 szybsze dzi\u0119ki cache."
    ),

    div(class = "widget-block",
      h4("Regresja prosta: ten sam model, dwa paradygmaty"),

      fluidRow(column(12,
        fluidRow(
          column(3,
            sliderInput("ch9_n", "n:",
                        min = 20, max = 200, value = 50, step = 10)
          ),
          column(3,
            sliderInput("ch9_slope", "Prawdziwy \u03b2\u2081 (nachylenie):",
                        min = -3, max = 3, value = 1.2, step = 0.1)
          ),
          column(3,
            sliderInput("ch9_sigma", "SD reszt:",
                        min = 1, max = 15, value = 5, step = 0.5)
          ),
          column(3,
            sliderInput("ch9_prior_scale",
                        "Skala priora (Normal(0, s)):",
                        min = 0.5, max = 10, value = 2.5, step = 0.5)
          )
        ),
        fluidRow(
          column(12,
            actionButton("ch9_draw", "\u21bb Nowa pr\u00f3ba",
                         class = "btn-primary", width = "200px"),
            actionButton("ch9_fit", "Dopasuj modele",
                         class = "btn-success", width = "200px")
          )
        )
      )),

      br(),
      plotOutput("ch9_scatter", height = "260px"),

      fluidRow(
        column(6,
          div(class = "panel-frequentist",
            h5("lm() \u2014 estymator OLS"),
            plotOutput("ch9_freq_forest", height = "180px"),
            uiOutput("ch9_freq_result")
          )
        ),
        column(6,
          div(class = "panel-bayesian",
            h5("stan_glm() \u2014 posterior \u03b2"),
            plotOutput("ch9_bayes_forest", height = "180px"),
            uiOutput("ch9_bayes_result")
          )
        )
      ),

      div(class = "callout-info",
        uiOutput("ch9_comparison")
      )
    ),

    div(class = "section-title", "Wp\u0142yw priora na estymacj\u0119"),

    div(class = "narrative",
      p("Suwak \u201eSkala priora\u201f kontroluje, jak \u201eoptymistyczny\u201f jest prior.
         Ma\u0142e warto\u015bci (np. 0.5) \u201e\u015bci\u0105gaj\u0105\u201f \u03b2 w kierunku zera
         \u2014 to ", tags$em("regularyzacja"), " bayesowska.
         Du\u017ce warto\u015bci (np. 10) \u2014 prior niemal nieinformatywny, wyniki podobne do lm()."),
      p("Dla ma\u0142ych pr\u00f3b i podejrzanie du\u017cych efekt\u00f3w regularyzacja jest zdrowa \u2014
         chroni przed nadinterpretacj\u0105 losowego szumu jako efektu.")
    ),

    div(class = "chapter-transition",
      p("Regresja liniowa do odpowiedzi ilo\u015bciowych.
         A dla binarnej odpowiedzi \u2014 regresja logistyczna."),
      actionButton("ch9_next",
                   "Dalej: Regresja logistyczna \u2192",
                   class = "btn-primary btn-lg")
    )

  )) # column, fluidRow
)

ch9_server <- function(input, output, session) {

  sample_data <- reactiveVal(NULL)
  fit_result  <- reactiveVal(NULL)

  observe({
    if (is.null(sample_data())) {
      d <- generate_regression_data(input$ch9_n,
                                    slope = input$ch9_slope,
                                    intercept = 2,
                                    sigma = input$ch9_sigma)
      sample_data(d)
    }
  })

  observeEvent(list(input$ch9_draw, input$ch9_n, input$ch9_slope,
                    input$ch9_sigma), {
    d <- generate_regression_data(input$ch9_n,
                                  slope = input$ch9_slope,
                                  intercept = 2,
                                  sigma = input$ch9_sigma)
    sample_data(d)
    fit_result(NULL)  # zresetuj fit po nowych danych
  }, ignoreInit = TRUE)

  observeEvent(input$ch9_fit, {
    d <- sample_data()
    req(d)
    withProgress(message = "Dopasowywanie modeli...",
                  detail = "stan_glm MCMC (chains=2, iter=1000)",
                  value = 0.1, {
      incProgress(0.3)
      res <- fit_bayes_lm(y ~ x, data = d,
                          chains = 2, iter = 1000,
                          prior_scale = input$ch9_prior_scale)
      incProgress(0.9)
      fit_result(res)
    })
  })

  output$ch9_scatter <- renderPlot({
    d <- sample_data()
    req(d)
    plot_scatter_with_fit(d, show_line = TRUE,
                           col_point = col_primary,
                           col_line = col_frequentist,
                           title = paste0("Dane (n = ", nrow(d), ")"))
  })

  output$ch9_freq_forest <- renderPlot({
    r <- fit_result()
    if (is.null(r)) {
      return(ggplot() + annotate("text", x = 0, y = 0,
        label = "Kliknij \u201eDopasuj modele\u201f \u2192") +
        theme_void())
    }
    plot_coef_forest(r$freq_coefs, "Cz\u0119sto\u015bciowo",
                     col_freq = col_frequentist,
                     col_bayes = col_bayesian)
  })

  output$ch9_freq_result <- renderUI({
    r <- fit_result()
    if (is.null(r)) return(div(class = "callout-warning",
                                 "Brak wynik\u00f3w \u2014 kliknij \u201eDopasuj modele\u201f."))
    fc <- r$freq_coefs
    slope_row <- fc[fc$term == "x", ]
    div(class = "callout-info",
      tags$b("Intercept (\u03b2\u2080): "), round(fc$estimate[1], 3),
      "  95% CI: [", round(fc$lower[1], 3), ", ",
      round(fc$upper[1], 3), "]", tags$br(),
      tags$b("Slope (\u03b2\u2081): "), round(slope_row$estimate, 3),
      "  95% CI: [", round(slope_row$lower, 3), ", ",
      round(slope_row$upper, 3), "]", tags$br(),
      tags$b("R\u00b2: "), round(r$r_squared, 3)
    )
  })

  output$ch9_bayes_forest <- renderPlot({
    r <- fit_result()
    if (is.null(r)) return(ggplot() + theme_void())
    plot_coef_forest(r$bayes_coefs, "Bayesowsko",
                     col_freq = col_frequentist,
                     col_bayes = col_bayesian)
  })

  output$ch9_bayes_result <- renderUI({
    r <- fit_result()
    if (is.null(r)) return(NULL)
    bc <- r$bayes_coefs
    slope_row <- bc[bc$term == "x", ]
    # Posterior P(slope > 0)
    post_slope <- r$posterior[, "x"]
    prob_positive <- mean(post_slope > 0)
    div(class = "callout-info",
      tags$b("Intercept (\u03b2\u2080): "), round(bc$estimate[1], 3),
      "  95% HDI: [", round(bc$lower[1], 3), ", ",
      round(bc$upper[1], 3), "]", tags$br(),
      tags$b("Slope (\u03b2\u2081): "), round(slope_row$estimate, 3),
      "  95% HDI: [", round(slope_row$lower, 3), ", ",
      round(slope_row$upper, 3), "]", tags$br(),
      tags$b("P(\u03b2\u2081 > 0 | dane) = "),
      paste0(round(prob_positive * 100, 1), "%")
    )
  })

  output$ch9_comparison <- renderUI({
    r <- fit_result()
    if (is.null(r)) return(tagList(
      tags$b("Oczekuje na fit..."), tags$br(),
      "Po klikni\u0119ciu \u201eDopasuj modele\u201f zobaczysz forest plot
       wsp\u00f3\u0142czynnik\u00f3w w obu paradygmatach."
    ))
    fc_slope <- r$freq_coefs[r$freq_coefs$term == "x", ]
    bc_slope <- r$bayes_coefs[r$bayes_coefs$term == "x", ]
    diff <- abs(fc_slope$estimate - bc_slope$estimate)
    pct_diff <- 100 * diff / abs(fc_slope$estimate)
    closeness <- if (pct_diff < 3) "niemal to\u017csame"
                 else if (pct_diff < 10) "bardzo zbli\u017cone"
                 else "wyra\u017anie r\u00f3\u017cne"
    tagList(
      tags$b("Werdykt: "),
      "Estymaty slope s\u0105 ", closeness,
      " (lm = ", round(fc_slope$estimate, 3),
      ", bayes = ", round(bc_slope$estimate, 3), ").",
      tags$br(),
      tags$em("Przy nieinformatywnym priorze i du\u017cej pr\u00f3bie
               oba podej\u015bcia daj\u0105 zwykle te same liczby \u2014 r\u00f3\u017cni je interpretacja.")
    )
  })
}
