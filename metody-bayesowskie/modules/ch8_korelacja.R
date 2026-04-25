# ============================================================================
# CHAPTER 8: Korelacja - cor.test vs correlationBF + posterior rho
# ============================================================================

ch8_ui <- lecture_chapter(
  id = "ch-korelacja",
  num = "08",
  title = "Korelacja",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 08 · Metody bayesowskie",
      num    = "08",
      title  = "Korelacja",
      lead   = "Korelacja w dwóch paradygmatach: p-wartość, BF i niepewność efektu."
    ),

    lc_feedback(type = "info",
      "Dwie zmienne ilościowe: czy są powiązane liniowo?
       Częstościowo: test istotności r Pearsona. Bayesowsko: BF + posterior ρ."
    ),

    lc_h2("ch8-sec-01", "Dwa paradygmaty o związku"),

    tagList(
      p(tags$b("Częstościowo: "), "cor.test daje r, p-wartość i 95% CI dla ρ."),
      p(tags$b("Bayesowsko: "), "correlationBF zwraca BF₁₀ (czy jest związek)
         + posterior dla ρ (populacyjnego współczynnika korelacji).
         HDI mówi nam, jakie wartości ρ są zgodne z danymi.")
    ),

    figure_panel(label = "Ryc. 8.1", title = "Ten sam dataset, dwie odpowiedzi",

      fluidRow(column(12,
        fluidRow(
          column(4,
            sliderInput("ch8_n", "Wielkość próby n:",
                        min = 10, max = 200, value = 40, step = 5)
          ),
          column(4,
            sliderInput("ch8_true_r", "Prawdziwa korelacja ρ:",
                        min = -0.9, max = 0.9, value = 0.3, step = 0.05)
          ),
          column(4,
            br(),
            actionButton("ch8_draw", "↻ Nowa próba",
                         class = "lc-btn-primary", width = "100%")
          )
        )
      )),

      fluidRow(
        column(5,
          plotOutput("ch8_scatter", height = "300px")
        ),
        column(7,
          fluidRow(
            column(12,
              div(class = "panel-frequentist",
                h5("cor.test (Pearson)"),
                uiOutput("ch8_freq_result")
              )
            )
          ),
          fluidRow(
            column(12,
              div(class = "panel-bayesian",
                h5("correlationBF + posterior ρ"),
                plotOutput("ch8_bayes_plot", height = "180px"),
                uiOutput("ch8_bayes_result")
              )
            )
          )
        )
      ),

      lc_feedback(type = "info",
        uiOutput("ch8_comparison")
      )
    ),

    lc_feedback(type = "ok",
      tags$b("Zaleta posteriora ρ: "),
      "zamiast binarnej decyzji „istotna/nieistotna‟ widzimy cały rozkład
        możliwych wartości siły związku. Można zapytać:
       P(|ρ| > 0.3 | dane) — że związek ma praktyczną wielkość."
    ),

    lc_chapter_next(
      num = "09",
      title = "Regresja liniowa",
      lead = "model liniowy z priorem i posteriorem współczynników.",
      target_id = "ch-regresja-lin"
    )

  )
)

ch8_server <- function(input, output, session) {

  sample_data <- reactiveVal(NULL)

  observe({
    if (is.null(sample_data())) {
      d <- generate_bivariate_data(bayes_input(input$ch8_n, 40),
                                   true_r = bayes_input(input$ch8_true_r, 0.4))
      sample_data(d)
    }
  })

  observeEvent(list(input$ch8_draw, input$ch8_n, input$ch8_true_r), {
    d <- generate_bivariate_data(bayes_input(input$ch8_n, 40),
                                 true_r = bayes_input(input$ch8_true_r, 0.4))
    sample_data(d)
  }, ignoreInit = TRUE)

  result <- reactive({
    d <- sample_data()
    req(d)
    compute_bf_correlation(d)
  })

  output$ch8_scatter <- renderPlot({
    d <- sample_data()
    req(d)
    plot_scatter_with_fit(d, x_var = "x", y_var = "y",
                          show_line = TRUE,
                          col_point = bayes_primary,
                          col_line = bayes_freq,
                          title = paste0("Dane (n = ", nrow(d), ")"))
  })

  output$ch8_freq_result <- renderUI({
    r <- result()
    p_info <- format_pval_pl(r$p_value)
    ci <- r$ci_freq
    lc_feedback(type = "info",
      tags$b("r Pearsona = "), round(r$r_obs, 3),
      "  |  t = ", round(r$t_statistic, 2),
      "  |  df = ", r$df, tags$br(),
      HTML(p_info$decision), tags$br(),
      tags$b("95% CI dla ρ: "), "[", round(ci[1], 3),
      ", ", round(ci[2], 3), "]"
    )
  })

  output$ch8_bayes_plot <- renderPlot({
    r <- result()
    plot_posterior_density(
      r$posterior_rho,
      hdi = r$hdi,
      ref_value = 0,
      x_label = "ρ (korelacja populacyjna)",
      title = NULL,
      bayes_posterior = bayes_posterior,
      bayes_hdi = bayes_hdi
    )
  })

  output$ch8_bayes_result <- renderUI({
    r <- result()
    interp <- interpret_bf(r$bf10)
    prob_positive <- mean(r$posterior_rho > 0)
    prob_mid <- mean(abs(r$posterior_rho) > 0.3)
    lc_feedback(type = "info",
      tags$b("BF₁₀ = "), format_bf(r$bf10),
      " (", interp$level, " ", interp$direction, ")", tags$br(),
      tags$b("Mediana ρ: "), round(r$posterior_median, 3),
      "  |  95% HDI: [", round(r$hdi["lower"], 3), ", ",
      round(r$hdi["upper"], 3), "]", tags$br(),
      tags$b("P(ρ > 0 | dane) = "),
      paste0(round(prob_positive * 100, 1), "%"),
      "  |  P(|ρ| > 0.3) = ", paste0(round(prob_mid * 100, 1), "%")
    )
  })

  output$ch8_comparison <- renderUI({
    r <- result()
    direction <- if (r$r_obs > 0) "dodatni" else "ujemny"
    magnitude <- if (abs(r$r_obs) < 0.1) "znikomy"
                  else if (abs(r$r_obs) < 0.3) "słaby"
                  else if (abs(r$r_obs) < 0.5) "umiarkowany"
                  else if (abs(r$r_obs) < 0.7) "silny"
                  else "bardzo silny"
    verdict <- if (r$p_value < 0.05 && r$bf10 > 3) {
      paste0("Oba podejścia zgodne: związek ", direction, ", ", magnitude, ".")
    } else if (r$p_value >= 0.05 && r$bf10 < 1/3) {
      "Oba podejścia zgodne: brak przesłanek za istnieniem związku liniowego."
    } else {
      "Rezultaty mieszane — warto rozważyć większy n."
    }
    tagList(tags$b("Werdykt: "), verdict)
  })
}
