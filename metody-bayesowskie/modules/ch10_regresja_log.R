# ============================================================================
# CHAPTER 10: Regresja logistyczna - glm(binomial) vs stan_glm(binomial)
# ============================================================================

ch10_ui <- tabPanel("10. Regresja logistyczna",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Odpowied\u017a binarna (sukces/pora\u017cka). Model przewiduje prawdopodobie\u0144stwo,
       a wsp\u00f3\u0142czynniki interpretujemy jako Odds Ratio (OR)."
    ),

    div(class = "section-title", "OR w dw\u00f3ch szko\u0142ach"),

    div(class = "narrative",
      p(tags$b("Cz\u0119sto\u015bciowo: "), "glm(y ~ x, family = binomial). Wsp\u00f3\u0142czynniki na skali logit;
         po eksponencjacji dostajemy OR. 95% CI \u2192 95% CI dla OR."),
      p(tags$b("Bayesowsko: "), "stan_glm(y ~ x, family = binomial). Posterior dla ka\u017cdego \u03b2;
         eksponencjujemy \u2192 posterior dla OR. 95% HDI OR \u2014 wiarygodny przedzia\u0142.")
    ),

    div(class = "widget-block",
      h4("Regresja logistyczna prosta"),

      fluidRow(column(12,
        fluidRow(
          column(3,
            sliderInput("ch10_n", "n:",
                        min = 30, max = 300, value = 100, step = 10)
          ),
          column(3,
            sliderInput("ch10_beta1",
                        "Prawdziwe \u03b2\u2081 (logit-skala):",
                        min = -2, max = 2, value = 1, step = 0.1)
          ),
          column(3,
            sliderInput("ch10_beta0", "Prawdziwe \u03b2\u2080:",
                        min = -2, max = 2, value = -0.5, step = 0.1)
          ),
          column(3,
            br(),
            actionButton("ch10_draw", "\u21bb Nowa pr\u00f3ba",
                         class = "btn-primary", width = "100%")
          )
        ),
        fluidRow(
          column(12,
            actionButton("ch10_fit", "Dopasuj modele",
                         class = "btn-success", width = "200px")
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

      div(class = "callout-info",
        uiOutput("ch10_comparison")
      )
    ),

    div(class = "callout-success",
      tags$b("Bonus bayesowski: "),
      "z posteriora dla OR mo\u017cna liczy\u0107 odpowiedzi typu
       \u201eP(OR > 2 | dane)\u201f \u2014 jak prawdopodobne, \u017ce efekt jest przynajmniej dwukrotny.
       W cz\u0119sto\u015bciowym \u015bwiecie takie pytanie nie ma prostej odpowiedzi."
    ),

    div(class = "chapter-transition",
      p("Prze\u015bli\u015bmy przez wszystkie typowe modele.
         Czas podsumowa\u0107: kiedy ktory paradygmat?"),
      actionButton("ch10_next",
                   "Dalej: \u015aci\u0105ga \u2192",
                   class = "btn-primary btn-lg")
    )

  )) # column, fluidRow
)

ch10_server <- function(input, output, session) {

  sample_data <- reactiveVal(NULL)
  fit_result  <- reactiveVal(NULL)

  observe({
    if (is.null(sample_data())) {
      d <- generate_logistic_data(input$ch10_n,
                                  beta0 = input$ch10_beta0,
                                  beta1 = input$ch10_beta1)
      sample_data(d)
    }
  })

  observeEvent(list(input$ch10_draw, input$ch10_n, input$ch10_beta0,
                    input$ch10_beta1), {
    d <- generate_logistic_data(input$ch10_n,
                                beta0 = input$ch10_beta0,
                                beta1 = input$ch10_beta1)
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
                   alpha = 0.5, color = col_primary) +
      geom_smooth(method = "glm", method.args = list(family = "binomial"),
                   se = TRUE, color = col_frequentist,
                   fill = col_frequentist, alpha = 0.15) +
      scale_y_continuous(breaks = c(0, 1), limits = c(-0.1, 1.1)) +
      labs(title = paste0("Dane (n = ", nrow(d), ", odpowied\u017a binarna)"),
           x = "x", y = "y (0 / 1)") +
      theme_educational()
  })

  output$ch10_freq_result <- renderUI({
    r <- fit_result()
    if (is.null(r)) return(div(class = "callout-warning",
                                 "Brak wynik\u00f3w \u2014 kliknij \u201eDopasuj modele\u201f."))
    fc <- r$freq_coefs
    slope_row <- fc[fc$term == "x", ]
    div(class = "callout-info",
      tags$b("\u03b2\u2081 (logit): "), round(slope_row$estimate, 3),
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
                      col_posterior = col_posterior, col_hdi = col_hdi)
  })

  output$ch10_bayes_result <- renderUI({
    r <- fit_result()
    if (is.null(r)) return(NULL)
    bc <- r$bayes_coefs
    slope_row <- bc[bc$term == "x", ]
    post_slope <- r$posterior[, "x"]
    prob_pos <- mean(post_slope > 0)
    prob_or2 <- mean(exp(post_slope) > 2)
    div(class = "callout-info",
      tags$b("\u03b2\u2081 (logit): "), round(slope_row$estimate, 3),
      "  95% HDI: [", round(slope_row$lower, 3), ", ",
      round(slope_row$upper, 3), "]", tags$br(),
      tags$b("OR: "), round(slope_row$or, 2),
      "  95% HDI OR: [", round(slope_row$or_lower, 2), ", ",
      round(slope_row$or_upper, 2), "]", tags$br(),
      tags$b("P(\u03b2\u2081 > 0 | dane) = "),
      paste0(round(prob_pos * 100, 1), "%"),
      "  |  P(OR > 2) = ", paste0(round(prob_or2 * 100, 1), "%")
    )
  })

  output$ch10_comparison <- renderUI({
    r <- fit_result()
    if (is.null(r)) return(tagList(
      tags$b("Oczekuje na fit..."), tags$br(),
      "Dopasuj modele \u2014 wtedy zobaczysz por\u00f3wnanie OR w obu paradygmatach."
    ))
    fc_slope <- r$freq_coefs[r$freq_coefs$term == "x", ]
    bc_slope <- r$bayes_coefs[r$bayes_coefs$term == "x", ]
    tagList(
      tags$b("Werdykt: "),
      "OR cz\u0119sto\u015bciowe = ", round(fc_slope$or, 2),
      " [", round(fc_slope$or_lower, 2), ", ",
      round(fc_slope$or_upper, 2), "]",
      " vs OR bayesowskie = ", round(bc_slope$or, 2),
      " [", round(bc_slope$or_lower, 2), ", ",
      round(bc_slope$or_upper, 2), "]",
      tags$br(),
      tags$em("Przy n \u2265 100 i Beta-nieinformatywnym priorze r\u00f3\u017cnice numeryczne s\u0105 niewielkie.
               R\u00f3\u017cni si\u0119 interpretacja (CI vs HDI) oraz mo\u017cliwo\u015b\u0107 bezpo\u015brednich pyta\u0144
               o warto\u015bci praktyczne.")
    )
  })
}
