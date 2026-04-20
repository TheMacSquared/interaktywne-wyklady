# ============================================================================
# CHAPTER 8: Korelacja - cor.test vs correlationBF + posterior rho
# ============================================================================

ch8_ui <- tabPanel("8. Korelacja",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Dwie zmienne ilo\u015bciowe: czy s\u0105 powi\u0105zane liniowo?
       Cz\u0119sto\u015bciowo: test istotno\u015bci r Pearsona. Bayesowsko: BF + posterior \u03c1."
    ),

    div(class = "section-title", "Dwa paradygmaty o zwi\u0105zku"),

    div(class = "narrative",
      p(tags$b("Cz\u0119sto\u015bciowo: "), "cor.test daje r, p-warto\u015b\u0107 i 95% CI dla \u03c1."),
      p(tags$b("Bayesowsko: "), "correlationBF zwraca BF\u2081\u2080 (czy jest zwi\u0105zek)
         + posterior dla \u03c1 (populacyjnego wsp\u00f3\u0142czynnika korelacji).
         HDI m\u00f3wi nam, jakie warto\u015bci \u03c1 s\u0105 zgodne z danymi.")
    ),

    div(class = "widget-block",
      h4("Ten sam dataset, dwie odpowiedzi"),

      fluidRow(column(12,
        fluidRow(
          column(4,
            sliderInput("ch8_n", "Wielko\u015b\u0107 pr\u00f3by n:",
                        min = 10, max = 200, value = 40, step = 5)
          ),
          column(4,
            sliderInput("ch8_true_r", "Prawdziwa korelacja \u03c1:",
                        min = -0.9, max = 0.9, value = 0.3, step = 0.05)
          ),
          column(4,
            br(),
            actionButton("ch8_draw", "\u21bb Nowa pr\u00f3ba",
                         class = "btn-primary", width = "100%")
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
                h5("correlationBF + posterior \u03c1"),
                plotOutput("ch8_bayes_plot", height = "180px"),
                uiOutput("ch8_bayes_result")
              )
            )
          )
        )
      ),

      div(class = "callout-info",
        uiOutput("ch8_comparison")
      )
    ),

    div(class = "callout-success",
      tags$b("Zaleta posteriora \u03c1: "),
      "zamiast binarnej decyzji \u201eistotna/nieistotna\u201f widzimy ca\u0142y rozk\u0142ad
        mo\u017cliwych warto\u015bci si\u0142y zwi\u0105zku. Mo\u017cna zapyta\u0107:
       P(|\u03c1| > 0.3 | dane) \u2014 \u017ce zwi\u0105zek ma praktyczn\u0105 wielko\u015b\u0107."
    ),

    div(class = "chapter-transition",
      p("Korelacja to prosta liniowa zale\u017cno\u015b\u0107 mi\u0119dzy dwiema zmiennymi.
         Wchodzimy w pe\u0142n\u0105 regresj\u0119 \u2014 tam r\u00f3\u017cnica paradygmat\u00f3w jest najbardziej widoczna."),
      actionButton("ch8_next",
                   "Dalej: Regresja liniowa \u2192",
                   class = "btn-primary btn-lg")
    )

  )) # column, fluidRow
)

ch8_server <- function(input, output, session) {

  sample_data <- reactiveVal(NULL)

  observe({
    if (is.null(sample_data())) {
      d <- generate_bivariate_data(input$ch8_n, true_r = input$ch8_true_r)
      sample_data(d)
    }
  })

  observeEvent(list(input$ch8_draw, input$ch8_n, input$ch8_true_r), {
    d <- generate_bivariate_data(input$ch8_n, true_r = input$ch8_true_r)
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
                          col_point = col_primary,
                          col_line = col_frequentist,
                          title = paste0("Dane (n = ", nrow(d), ")"))
  })

  output$ch8_freq_result <- renderUI({
    r <- result()
    p_info <- format_pval_pl(r$p_value)
    ci <- r$ci_freq
    div(class = "callout-info",
      tags$b("r Pearsona = "), round(r$r_obs, 3),
      "  |  t = ", round(r$t_statistic, 2),
      "  |  df = ", r$df, tags$br(),
      HTML(p_info$decision), tags$br(),
      tags$b("95% CI dla \u03c1: "), "[", round(ci[1], 3),
      ", ", round(ci[2], 3), "]"
    )
  })

  output$ch8_bayes_plot <- renderPlot({
    r <- result()
    plot_posterior_density(
      r$posterior_rho,
      hdi = r$hdi,
      ref_value = 0,
      x_label = "\u03c1 (korelacja populacyjna)",
      title = NULL,
      col_posterior = col_posterior,
      col_hdi = col_hdi
    )
  })

  output$ch8_bayes_result <- renderUI({
    r <- result()
    interp <- interpret_bf(r$bf10)
    prob_positive <- mean(r$posterior_rho > 0)
    prob_mid <- mean(abs(r$posterior_rho) > 0.3)
    div(class = "callout-info",
      tags$b("BF\u2081\u2080 = "), format_bf(r$bf10),
      " (", interp$level, " ", interp$direction, ")", tags$br(),
      tags$b("Mediana \u03c1: "), round(r$posterior_median, 3),
      "  |  95% HDI: [", round(r$hdi["lower"], 3), ", ",
      round(r$hdi["upper"], 3), "]", tags$br(),
      tags$b("P(\u03c1 > 0 | dane) = "),
      paste0(round(prob_positive * 100, 1), "%"),
      "  |  P(|\u03c1| > 0.3) = ", paste0(round(prob_mid * 100, 1), "%")
    )
  })

  output$ch8_comparison <- renderUI({
    r <- result()
    direction <- if (r$r_obs > 0) "dodatni" else "ujemny"
    magnitude <- if (abs(r$r_obs) < 0.1) "znikomy"
                  else if (abs(r$r_obs) < 0.3) "s\u0142aby"
                  else if (abs(r$r_obs) < 0.5) "umiarkowany"
                  else if (abs(r$r_obs) < 0.7) "silny"
                  else "bardzo silny"
    verdict <- if (r$p_value < 0.05 && r$bf10 > 3) {
      paste0("Oba podej\u015bcia zgodne: zwi\u0105zek ", direction, ", ", magnitude, ".")
    } else if (r$p_value >= 0.05 && r$bf10 < 1/3) {
      "Oba podej\u015bcia zgodne: brak przes\u0142anek za istnieniem zwi\u0105zku liniowego."
    } else {
      "Rezultaty mieszane \u2014 warto rozwa\u017cy\u0107 wi\u0119kszy n."
    }
    tagList(tags$b("Werdykt: "), verdict)
  })
}
