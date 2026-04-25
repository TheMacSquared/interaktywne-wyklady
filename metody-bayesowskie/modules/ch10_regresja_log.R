# ============================================================================
# CHAPTER 10: Regresja logistyczna - glm(binomial) vs stan_glm(binomial)
# ============================================================================

ch10_ui <- lecture_chapter(
  id = "ch-regresja-log",
  num = "10",
  title = "Regresja logistyczna",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 10 · Metody bayesowskie",
      num    = "10",
      title  = "Regresja logistyczna",
      lead   = "Ilorazy szans i posterior prawdopodobieństw w modelu logistycznym."
    ),

    lc_feedback(type = "info",
      "Odpowiedź binarna (sukces/porażka). Model przewiduje prawdopodobieństwo,
       a współczynniki interpretujemy jako Odds Ratio (OR)."
    ),

    lc_h2("ch10-sec-01", "OR w dwóch szkołach"),

    tagList(
      p(tags$b("Częstościowo: "), "glm(y ~ x, family = binomial). Współczynniki na skali logit;
         po eksponencjacji dostajemy OR. 95% CI → 95% CI dla OR."),
      p(tags$b("Bayesowsko: "), "stan_glm(y ~ x, family = binomial). Posterior dla każdego β;
         eksponencjujemy → posterior dla OR. 95% HDI OR — wiarygodny przedział.")
    ),

    figure_panel(label = "Ryc. 10.1", title = "Regresja logistyczna prosta",

      fluidRow(column(12,
        fluidRow(
          column(3,
            sliderInput("ch10_n", "n:",
                        min = 30, max = 300, value = 100, step = 10)
          ),
          column(3,
            sliderInput("ch10_beta1",
                        "Prawdziwe β₁ (logit-skala):",
                        min = -2, max = 2, value = 1, step = 0.1)
          ),
          column(3,
            sliderInput("ch10_beta0", "Prawdziwe β₀:",
                        min = -2, max = 2, value = -0.5, step = 0.1)
          ),
          column(3,
            br(),
            actionButton("ch10_draw", "↻ Nowa próba",
                         class = "lc-btn-primary", width = "100%")
          )
        ),
        fluidRow(
          column(12,
            actionButton("ch10_fit", "Dopasuj modele",
                         class = "lc-btn-ok", width = "200px")
          )
        )
      )),

      br(),
      plotOutput("ch10_scatter", height = "260px"),

      fluidRow(
        column(6,
          div(class = "panel-frequentist",
            h5("glm(family = binomial)"),
            uiOutput("ch10_freq_result")
          )
        ),
        column(6,
          div(class = "panel-bayesian",
            h5("stan_glm(family = binomial) + posterior OR"),
            plotOutput("ch10_bayes_or", height = "200px"),
            uiOutput("ch10_bayes_result")
          )
        )
      ),

      lc_feedback(type = "info",
        uiOutput("ch10_comparison")
      )
    ),

    lc_feedback(type = "ok",
      tags$b("Bonus bayesowski: "),
      "z posteriora dla OR można liczyć odpowiedzi typu
       „P(OR > 2 | dane)‟ — jak prawdopodobne, że efekt jest przynajmniej dwukrotny.
       W częstościowym świecie takie pytanie nie ma prostej odpowiedzi."
    ),

    lc_chapter_next(
      num = "11",
      title = "Ściąga",
      lead = "najważniejsze reguły i dobór metod.",
      target_id = "ch-sciaga"
    )

  )
)

ch10_server <- function(input, output, session) {

  sample_data <- reactiveVal(NULL)
  fit_result  <- reactiveVal(NULL)

  observe({
    if (is.null(sample_data())) {
      d <- generate_logistic_data(bayes_input(input$ch10_n, 80),
                                  beta0 = bayes_input(input$ch10_beta0, -1),
                                  beta1 = bayes_input(input$ch10_beta1, 1.5))
      sample_data(d)
    }
  })

  observeEvent(list(input$ch10_draw, input$ch10_n, input$ch10_beta0,
                    input$ch10_beta1), {
    d <- generate_logistic_data(bayes_input(input$ch10_n, 80),
                                beta0 = bayes_input(input$ch10_beta0, -1),
                                beta1 = bayes_input(input$ch10_beta1, 1.5))
    sample_data(d)
    fit_result(NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$ch10_fit, {
    d <- sample_data()
    req(d)
    withProgress(message = "Dopasowywanie modeli logistycznych...",
                  detail = "stan_glm MCMC",
                  value = 0.1, {
      incProgress(0.3)
      res <- fit_bayes_glm_logistic(y ~ x, data = d,
                                    chains = 2, iter = 1000,
                                    prior_scale = 2.5)
      incProgress(0.9)
      fit_result(res)
    })
  })

  output$ch10_scatter <- renderPlot({
    d <- sample_data()
    req(d)
    ggplot(d, aes(x = x, y = y)) +
      geom_jitter(height = 0.05, width = 0, size = 2,
                   alpha = 0.5, color = bayes_primary) +
      geom_smooth(method = "glm", method.args = list(family = "binomial"),
                   se = TRUE, color = bayes_freq,
                   fill = bayes_freq, alpha = 0.15) +
      scale_y_continuous(breaks = c(0, 1), limits = c(-0.1, 1.1)) +
      labs(title = paste0("Dane (n = ", nrow(d), ", odpowiedź binarna)"),
           x = "x", y = "y (0 / 1)") +
      theme_upwr()
  })

  output$ch10_freq_result <- renderUI({
    r <- fit_result()
    if (is.null(r)) return(lc_feedback(type = "warning",
                                 "Brak wyników — kliknij „Dopasuj modele‟."))
    fc <- r$freq_coefs
    slope_row <- fc[fc$term == "x", ]
    lc_feedback(type = "info",
      tags$b("β₁ (logit): "), round(slope_row$estimate, 3),
      "  95% CI: [", round(slope_row$lower, 3), ", ",
      round(slope_row$upper, 3), "]", tags$br(),
      tags$b("OR: "), round(slope_row$or, 2),
      "  95% CI OR: [", round(slope_row$or_lower, 2), ", ",
      round(slope_row$or_upper, 2), "]"
    )
  })

  output$ch10_bayes_or <- renderPlot({
    r <- fit_result()
    if (is.null(r)) return(ggplot() + theme_void())
    # Posterior OR dla slope
    post_slope <- r$posterior[, "x"]
    or_samples <- exp(post_slope)
    hdi_or <- hdi_from_samples(or_samples, prob = 0.95)

    post_result <- list(
      or_samples     = or_samples,
      log_or_samples = post_slope,
      or_median      = median(or_samples),
      or_hdi         = hdi_or,
      log_or_hdi     = hdi_from_samples(post_slope, prob = 0.95),
      p_direction    = mean(post_slope > 0)
    )
    plot_posterior_or(post_result,
                      bayes_posterior = bayes_posterior, bayes_hdi = bayes_hdi)
  })

  output$ch10_bayes_result <- renderUI({
    r <- fit_result()
    if (is.null(r)) return(NULL)
    bc <- r$bayes_coefs
    slope_row <- bc[bc$term == "x", ]
    post_slope <- r$posterior[, "x"]
    prob_pos <- mean(post_slope > 0)
    prob_or2 <- mean(exp(post_slope) > 2)
    lc_feedback(type = "info",
      tags$b("β₁ (logit): "), round(slope_row$estimate, 3),
      "  95% HDI: [", round(slope_row$lower, 3), ", ",
      round(slope_row$upper, 3), "]", tags$br(),
      tags$b("OR: "), round(slope_row$or, 2),
      "  95% HDI OR: [", round(slope_row$or_lower, 2), ", ",
      round(slope_row$or_upper, 2), "]", tags$br(),
      tags$b("P(β₁ > 0 | dane) = "),
      paste0(round(prob_pos * 100, 1), "%"),
      "  |  P(OR > 2) = ", paste0(round(prob_or2 * 100, 1), "%")
    )
  })

  output$ch10_comparison <- renderUI({
    r <- fit_result()
    if (is.null(r)) return(tagList(
      tags$b("Oczekuje na fit..."), tags$br(),
      "Dopasuj modele — wtedy zobaczysz porównanie OR w obu paradygmatach."
    ))
    fc_slope <- r$freq_coefs[r$freq_coefs$term == "x", ]
    bc_slope <- r$bayes_coefs[r$bayes_coefs$term == "x", ]
    tagList(
      tags$b("Werdykt: "),
      "OR częstościowe = ", round(fc_slope$or, 2),
      " [", round(fc_slope$or_lower, 2), ", ",
      round(fc_slope$or_upper, 2), "]",
      " vs OR bayesowskie = ", round(bc_slope$or, 2),
      " [", round(bc_slope$or_lower, 2), ", ",
      round(bc_slope$or_upper, 2), "]",
      tags$br(),
      tags$em("Przy n ≥ 100 i Beta-nieinformatywnym priorze różnice numeryczne są niewielkie.
               Różni się interpretacja (CI vs HDI) oraz możliwość bezpośrednich pytań
               o wartości praktyczne.")
    )
  })
}
