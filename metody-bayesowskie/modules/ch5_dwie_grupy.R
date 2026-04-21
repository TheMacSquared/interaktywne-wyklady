# ============================================================================
# CHAPTER 5: Dwie grupy - t-test Welcha vs ttestBF + posterior roznicy
# ============================================================================

ch5_ui <- tabPanel("5. Dwie grupy",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Porównanie dwóch grup — chyba najczęstszy problem:
       czy nowa metoda działa lepiej, czy grupa leczona różni się od kontrolnej?"
    ),

    div(class = "section-title", "Welch vs ttestBF"),

    div(class = "narrative",
      p(tags$b("Częstościowo: "), "test t Welcha porównuje średnie dwóch grup,
         nie zakładając równych wariancji. Daje p-wartość + 95% CI dla różnicy."),
      p(tags$b("Bayesowsko: "), "ttestBF wylicza BF₁₀ (jak silny dowód ", tags$em("za"),
         " różnicą) oraz zwraca posterior dla różnicy średnich —
         bezpośrednio widzimy, ", tags$em("jak duża"),
         " ta różnica prawdopodobnie jest.")
    ),

    div(class = "widget-block",
      h4("Porównanie dwóch grup"),

      fluidRow(column(12,
        fluidRow(
          column(3,
            sliderInput("ch5_n", "n na grupę:",
                        min = 10, max = 100, value = 25, step = 5)
          ),
          column(3,
            sliderInput("ch5_effect", "Prawdziwa różnica (A → B):",
                        min = -10, max = 10, value = 3, step = 0.5)
          ),
          column(3,
            selectInput("ch5_dist", "Rozkład:",
              choices = c("Normalny" = "normal",
                          "Prawoskośny (Gamma)" = "skewed",
                          "Grube ogony (t)" = "heavy_tail"),
              selected = "normal")
          ),
          column(3,
            br(),
            actionButton("ch5_draw", "↻ Nowa próba",
                         class = "btn-primary", width = "100%")
          )
        )
      )),

      br(),
      plotOutput("ch5_data_plot", height = "220px"),

      fluidRow(
        column(6,
          div(class = "panel-frequentist",
            h5("t-test Welcha"),
            uiOutput("ch5_freq_result")
          )
        ),
        column(6,
          div(class = "panel-bayesian",
            h5("ttestBF + posterior różnicy"),
            plotOutput("ch5_bayes_plot", height = "260px"),
            uiOutput("ch5_bayes_result")
          )
        )
      ),

      div(class = "callout-info",
        uiOutput("ch5_comparison")
      )
    ),

    div(class = "section-title", "Zaleta posteriora dla różnicy"),

    div(class = "narrative",
      p("Z posterior możesz odczytać odpowiedzi, których p-wartość nie daje wprost:"),
      tags$ul(
        tags$li("P(B > A) — szansa, że grupa B jest wyższa"),
        tags$li("P(różnica > praktyczny próg, np. 2 jednostki)"),
        tags$li("95% HDI dla różnicy — plausible range")
      ),
      p("To są odpowiedzi na bezpośrednio interesujące pytania, nie zaś
         warunkowe „jak prawdopodobne są takie dane gdyby H₀ była prawdziwa‟.")
    ),

    div(class = "chapter-transition",
      p("Dwie grupy mamy. A trzy i więcej? Wchodzimy w ANOVA."),
      actionButton("ch5_next",
                   "Dalej: ANOVA →",
                   class = "btn-primary btn-lg")
    )

  )) # column, fluidRow
)

ch5_server <- function(input, output, session) {

  sample_data <- reactiveVal(NULL)

  observe({
    if (is.null(sample_data())) {
      d <- generate_two_groups_data(input$ch5_n, input$ch5_effect,
                                    dist = input$ch5_dist)
      sample_data(d)
    }
  })

  observeEvent(list(input$ch5_draw, input$ch5_n, input$ch5_effect,
                    input$ch5_dist), {
    d <- generate_two_groups_data(input$ch5_n, input$ch5_effect,
                                  dist = input$ch5_dist)
    sample_data(d)
  }, ignoreInit = TRUE)

  result <- reactive({
    d <- sample_data()
    req(d)
    compute_bf_two_sample(d)
  })

  output$ch5_data_plot <- renderPlot({
    d <- sample_data()
    req(d)
    plot_two_groups_box(d, col_a = col_primary, col_b = col_warning,
                        title = "Dane: dwie grupy")
  })

  output$ch5_freq_result <- renderUI({
    r <- result()
    p_info <- format_pval_pl(r$p_value)
    ci <- r$ci_freq
    div(class = "callout-info",
      tags$b("H₀: "), "μ_A = μ_B", tags$br(),
      tags$b("t Welcha = "), round(r$t_statistic, 3),
      " | df = ", round(r$df, 1), tags$br(),
      HTML(p_info$decision), tags$br(),
      tags$b("Obserwowana różnica (B - A): "), round(r$obs_diff, 2), tags$br(),
      tags$b("95% CI dla różnicy: "), "[", round(ci[1], 2), ", ",
      round(ci[2], 2), "]", tags$br(),
      tags$b("Cohen's d: "), round(r$cohen_d, 2)
    )
  })

  output$ch5_bayes_plot <- renderPlot({
    r <- result()
    plot_posterior_density(
      r$posterior_diff,
      hdi = r$hdi,
      ref_value = 0,
      x_label = "Różnica (B - A)",
      title = "Posterior różnicy średnich",
      col_posterior = col_posterior,
      col_hdi = col_hdi
    )
  })

  output$ch5_bayes_result <- renderUI({
    r <- result()
    interp <- interpret_bf(r$bf10)
    prob_b_greater <- mean(r$posterior_diff > 0)
    prob_practical <- mean(r$posterior_diff > 2)  # prog praktyczny
    div(class = "callout-info",
      tags$b("BF₁₀ = "), format_bf(r$bf10),
      " (", interp$level, " ", interp$direction, ")", tags$br(),
      tags$b("Mediana posterior różnicy: "),
      round(r$posterior_median, 2), tags$br(),
      tags$b("95% HDI: "), "[", round(r$hdi["lower"], 2), ", ",
      round(r$hdi["upper"], 2), "]", tags$br(),
      tags$b("P(B > A | dane) = "),
      paste0(round(prob_b_greater * 100, 1), "%"), tags$br(),
      tags$b("P(różnica > 2 | dane) = "),
      paste0(round(prob_practical * 100, 1), "%")
    )
  })

  output$ch5_comparison <- renderUI({
    r <- result()
    direction <- if (r$obs_diff > 0) "wyższą" else "niższą"
    magnitude <- if (abs(r$cohen_d) < 0.2) "bardzo mały"
                  else if (abs(r$cohen_d) < 0.5) "mały"
                  else if (abs(r$cohen_d) < 0.8) "średni"
                  else "duży"
    agreement <- (r$p_value < 0.05 && r$bf10 > 3) ||
                  (r$p_value >= 0.05 && r$bf10 < 1/3)
    verdict <- if (agreement && r$bf10 > 3) {
      paste0("Oba podejścia zgodne: grupa B ma ", direction,
             " średnią niż A, efekt ", magnitude, ".")
    } else if (agreement && r$bf10 < 1/3) {
      paste0("Oba podejścia zgodne: brak przesłanek do różnicowania grup.")
    } else {
      paste0("Rezultaty mieszane — próba może być za mała lub efekt subtelny.")
    }
    tagList(
      tags$b("Werdykt: "), verdict
    )
  })
}
