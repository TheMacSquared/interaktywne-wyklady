# ============================================================================
# CHAPTER 9: Regresja liniowa - lm() vs stan_glm()
# ============================================================================

ch9_ui <- lecture_chapter(
  id = "ch-regresja-lin",
  num = "09",
  title = "Regresja liniowa",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 09 · Metody bayesowskie",
      num    = "09",
      title  = "Regresja liniowa",
      lead   = "Od lm() do stan_glm(): estymacja współczynników i rola priora."
    ),

    lc_feedback(type = "info",
      "Zamiast jednej liczby (r) albo średniej — model:
       y = β₀ + β₁ x + ε. Które β są istotne? Jakie mają wartości?"
    ),

    lc_h2("ch9-sec-01", "lm() vs stan_glm()"),

    tagList(
      p(tags$b("Częstościowo: "), "lm(y ~ x) estymuje β metodą najmniejszych kwadratów,
         zwraca p-wartości dla każdego współczynnika i 95% CI."),
      p(tags$b("Bayesowsko: "), "stan_glm(y ~ x, family = gaussian) próbkuje posterior
         dla każdego β. Dostajemy medianę + 95% HDI — plausible range dla siły efektu.")
    ),

    lc_feedback(type = "warning",
      tags$b("Uwaga wydajnościowa: "),
      "stan_glm uruchamia sampler MCMC — pierwszy fit trwa 5-15 sekund.
       Kolejne fitowania tego samego modelu są szybsze dzięki cache."
    ),

    figure_panel(label = "Ryc. 9.1", title = "Regresja prosta: ten sam model, dwa paradygmaty",

      fluidRow(column(12,
        fluidRow(
          column(3,
            sliderInput("ch9_n", "n:",
                        min = 20, max = 200, value = 50, step = 10)
          ),
          column(3,
            sliderInput("ch9_slope", "Prawdziwy β₁ (nachylenie):",
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
            actionButton("ch9_draw", "↻ Nowa próba",
                         class = "lc-btn-primary", width = "200px"),
            actionButton("ch9_fit", "Dopasuj modele",
                         class = "lc-btn-ok", width = "200px")
          )
        )
      )),

      br(),
      zoom_plot_ui("ch9_scatter", height = "260px"),

      fluidRow(
        column(6,
          div(class = "panel-frequentist",
            h5("lm() — estymator OLS"),
            zoom_plot_ui("ch9_freq_forest", height = "180px"),
            uiOutput("ch9_freq_result")
          )
        ),
        column(6,
          div(class = "panel-bayesian",
            h5("stan_glm() — posterior β"),
            zoom_plot_ui("ch9_bayes_forest", height = "180px"),
            uiOutput("ch9_bayes_result")
          )
        )
      ),

      lc_feedback(type = "info",
        uiOutput("ch9_comparison")
      )
    ),

    lc_h2("ch9-sec-02", "Wpływ priora na estymację"),

    tagList(
      p("Suwak „Skala priora‟ kontroluje, jak „optymistyczny‟ jest prior.
         Małe wartości (np. 0.5) „ściągają‟ β w kierunku zera
         — to ", tags$em("regularyzacja"), " bayesowska.
         Duże wartości (np. 10) — prior niemal nieinformatywny, wyniki podobne do lm()."),
      p("Dla małych prób i podejrzanie dużych efektów regularyzacja jest zdrowa —
         chroni przed nadinterpretacją losowego szumu jako efektu.")
    ),

    lc_chapter_next(
      num = "10",
      title = "Regresja logistyczna",
      lead = "bayesowski model dla zmiennej zero-jedynkowej.",
      target_id = "ch-regresja-log"
    )

  )
)

ch9_server <- function(input, output, session) {

  sample_data <- reactiveVal(NULL)
  fit_result  <- reactiveVal(NULL)

  observe({
    if (is.null(sample_data())) {
      d <- generate_regression_data(bayes_input(input$ch9_n, 60),
                                    slope = bayes_input(input$ch9_slope, 1.5),
                                    intercept = 2,
                                    sigma = bayes_input(input$ch9_sigma, 8))
      sample_data(d)
    }
  })

  observeEvent(list(input$ch9_draw, input$ch9_n, input$ch9_slope,
                    input$ch9_sigma), {
    d <- generate_regression_data(bayes_input(input$ch9_n, 60),
                                  slope = bayes_input(input$ch9_slope, 1.5),
                                  intercept = 2,
                                  sigma = bayes_input(input$ch9_sigma, 8))
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

  zoom_plot_server("ch9_scatter", reactive({
    d <- sample_data()
    req(d)
    plot_scatter_with_fit(d, show_line = TRUE,
                           col_point = bayes_primary,
                           col_line = bayes_freq,
                           title = paste0("Dane (n = ", nrow(d), ")"))
  }))

  zoom_plot_server("ch9_freq_forest", reactive({
    r <- fit_result()
    if (is.null(r)) {
      return(ggplot() + annotate("text", x = 0, y = 0,
        label = "Kliknij „Dopasuj modele‟ →") +
        theme_void())
    }
    plot_coef_forest(r$freq_coefs, "Częstościowo",
                     col_freq = bayes_freq,
                     col_bayes = bayes_bayes)
  }))

  output$ch9_freq_result <- renderUI({
    r <- fit_result()
    if (is.null(r)) return(lc_feedback(type = "warning",
                                 "Brak wyników — kliknij „Dopasuj modele‟."))
    fc <- r$freq_coefs
    slope_row <- fc[fc$term == "x", ]
    lc_feedback(type = "info",
      tags$b("Intercept (β₀): "), round(fc$estimate[1], 3),
      "  95% CI: [", round(fc$lower[1], 3), ", ",
      round(fc$upper[1], 3), "]", tags$br(),
      tags$b("Slope (β₁): "), round(slope_row$estimate, 3),
      "  95% CI: [", round(slope_row$lower, 3), ", ",
      round(slope_row$upper, 3), "]", tags$br(),
      tags$b("R²: "), round(r$r_squared, 3)
    )
  })

  zoom_plot_server("ch9_bayes_forest", reactive({
    r <- fit_result()
    if (is.null(r)) return(ggplot() + theme_void())
    plot_coef_forest(r$bayes_coefs, "Bayesowsko",
                     col_freq = bayes_freq,
                     col_bayes = bayes_bayes)
  }))

  output$ch9_bayes_result <- renderUI({
    r <- fit_result()
    if (is.null(r)) return(NULL)
    bc <- r$bayes_coefs
    slope_row <- bc[bc$term == "x", ]
    # Posterior P(slope > 0)
    post_slope <- r$posterior[, "x"]
    prob_positive <- mean(post_slope > 0)
    lc_feedback(type = "info",
      tags$b("Intercept (β₀): "), round(bc$estimate[1], 3),
      "  95% HDI: [", round(bc$lower[1], 3), ", ",
      round(bc$upper[1], 3), "]", tags$br(),
      tags$b("Slope (β₁): "), round(slope_row$estimate, 3),
      "  95% HDI: [", round(slope_row$lower, 3), ", ",
      round(slope_row$upper, 3), "]", tags$br(),
      tags$b("P(β₁ > 0 | dane) = "),
      paste0(round(prob_positive * 100, 1), "%")
    )
  })

  output$ch9_comparison <- renderUI({
    r <- fit_result()
    if (is.null(r)) return(tagList(
      tags$b("Oczekuje na fit..."), tags$br(),
      "Po kliknięciu „Dopasuj modele‟ zobaczysz forest plot
       współczynników w obu paradygmatach."
    ))
    fc_slope <- r$freq_coefs[r$freq_coefs$term == "x", ]
    bc_slope <- r$bayes_coefs[r$bayes_coefs$term == "x", ]
    diff <- abs(fc_slope$estimate - bc_slope$estimate)
    pct_diff <- 100 * diff / abs(fc_slope$estimate)
    closeness <- if (pct_diff < 3) "niemal tożsame"
                 else if (pct_diff < 10) "bardzo zbliżone"
                 else "wyraźnie różne"
    tagList(
      tags$b("Werdykt: "),
      "Estymaty slope są ", closeness,
      " (lm = ", round(fc_slope$estimate, 3),
      ", bayes = ", round(bc_slope$estimate, 3), ").",
      tags$br(),
      tags$em("Przy nieinformatywnym priorze i dużej próbie
               oba podejścia dają zwykle te same liczby — różni je interpretacja.")
    )
  })
}
