# ============================================================================
# CHAPTER 5: Dwie grupy - t-test Welcha vs ttestBF + posterior roznicy
# ============================================================================

ch5_ui <- tabPanel("5. Dwie grupy",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Porównanie dw\u00f3ch grup \u2014 chyba najcz\u0119stszy problem:
       czy nowa metoda dzia\u0142a lepiej, czy grupa leczona r\u00f3\u017cni si\u0119 od kontrolnej?"
    ),

    div(class = "section-title", "Welch vs ttestBF"),

    div(class = "narrative",
      p(tags$b("Cz\u0119sto\u015bciowo: "), "test t Welcha por\u00f3wnuje \u015brednie dw\u00f3ch grup,
         nie zak\u0142adaj\u0105c r\u00f3wnych wariancji. Daje p-warto\u015b\u0107 + 95% CI dla r\u00f3\u017cnicy."),
      p(tags$b("Bayesowsko: "), "ttestBF wylicza BF\u2081\u2080 (jak silny dow\u00f3d ", tags$em("za"),
         " r\u00f3\u017cnic\u0105) oraz zwraca posterior dla r\u00f3\u017cnicy \u015brednich \u2014
         bezpo\u015brednio widzimy, ", tags$em("jak du\u017ca"),
         " ta r\u00f3\u017cnica prawdopodobnie jest.")
    ),

    div(class = "widget-block",
      h4("Porównanie dw\u00f3ch grup"),

      fluidRow(column(12,
        fluidRow(
          column(3,
            sliderInput("ch5_n", "n na grup\u0119:",
                        min = 10, max = 100, value = 25, step = 5)
          ),
          column(3,
            sliderInput("ch5_effect", "Prawdziwa r\u00f3\u017cnica (A \u2192 B):",
                        min = -10, max = 10, value = 3, step = 0.5)
          ),
          column(3,
            selectInput("ch5_dist", "Rozk\u0142ad:",
              choices = c("Normalny" = "normal",
                          "Prawosko\u015bny (Gamma)" = "skewed",
                          "Grube ogony (t)" = "heavy_tail"),
              selected = "normal")
          ),
          column(3,
            br(),
            actionButton("ch5_draw", "\u21bb Nowa pr\u00f3ba",
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
            h5("ttestBF + posterior r\u00f3\u017cnicy"),
            plotOutput("ch5_bayes_plot", height = "260px"),
            uiOutput("ch5_bayes_result")
          )
        )
      ),

      div(class = "callout-info",
        uiOutput("ch5_comparison")
      )
    ),

    div(class = "section-title", "Zaleta posteriora dla r\u00f3\u017cnicy"),

    div(class = "narrative",
      p("Z posterior mo\u017cesz odczyta\u0107 odpowiedzi, kt\u00f3rych p-warto\u015b\u0107 nie daje wprost:"),
      tags$ul(
        tags$li("P(B > A) \u2014 szansa, \u017ce grupa B jest wy\u017csza"),
        tags$li("P(r\u00f3\u017cnica > praktyczny pr\u00f3g, np. 2 jednostki)"),
        tags$li("95% HDI dla r\u00f3\u017cnicy \u2014 plausible range")
      ),
      p("To s\u0105 odpowiedzi na bezpo\u015brednio interesuj\u0105ce pytania, nie za\u015b
         warunkowe \u201ejak prawdopodobne s\u0105 takie dane gdyby H\u2080 by\u0142a prawdziwa\u201f.")
    ),

    div(class = "chapter-transition",
      p("Dwie grupy mamy. A trzy i wi\u0119cej? Wchodzimy w ANOVA."),
      actionButton("ch5_next",
                   "Dalej: ANOVA \u2192",
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
      tags$b("H\u2080: "), "\u03bc_A = \u03bc_B", tags$br(),
      tags$b("t Welcha = "), round(r$t_statistic, 3),
      " | df = ", round(r$df, 1), tags$br(),
      HTML(p_info$decision), tags$br(),
      tags$b("Obserwowana r\u00f3\u017cnica (B - A): "), round(r$obs_diff, 2), tags$br(),
      tags$b("95% CI dla r\u00f3\u017cnicy: "), "[", round(ci[1], 2), ", ",
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
      x_label = "R\u00f3\u017cnica (B - A)",
      title = "Posterior r\u00f3\u017cnicy \u015brednich",
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
      tags$b("BF\u2081\u2080 = "), format_bf(r$bf10),
      " (", interp$level, " ", interp$direction, ")", tags$br(),
      tags$b("Mediana posterior r\u00f3\u017cnicy: "),
      round(r$posterior_median, 2), tags$br(),
      tags$b("95% HDI: "), "[", round(r$hdi["lower"], 2), ", ",
      round(r$hdi["upper"], 2), "]", tags$br(),
      tags$b("P(B > A | dane) = "),
      paste0(round(prob_b_greater * 100, 1), "%"), tags$br(),
      tags$b("P(r\u00f3\u017cnica > 2 | dane) = "),
      paste0(round(prob_practical * 100, 1), "%")
    )
  })

  output$ch5_comparison <- renderUI({
    r <- result()
    direction <- if (r$obs_diff > 0) "wy\u017csz\u0105" else "ni\u017csz\u0105"
    magnitude <- if (abs(r$cohen_d) < 0.2) "bardzo ma\u0142y"
                  else if (abs(r$cohen_d) < 0.5) "ma\u0142y"
                  else if (abs(r$cohen_d) < 0.8) "\u015bredni"
                  else "du\u017cy"
    agreement <- (r$p_value < 0.05 && r$bf10 > 3) ||
                  (r$p_value >= 0.05 && r$bf10 < 1/3)
    verdict <- if (agreement && r$bf10 > 3) {
      paste0("Oba podej\u015bcia zgodne: grupa B ma ", direction,
             " \u015bredni\u0105 ni\u017c A, efekt ", magnitude, ".")
    } else if (agreement && r$bf10 < 1/3) {
      paste0("Oba podej\u015bcia zgodne: brak przes\u0142anek do r\u00f3\u017cnicowania grup.")
    } else {
      paste0("Rezultaty mieszane \u2014 pr\u00f3ba mo\u017ce by\u0107 za ma\u0142a lub efekt subtelny.")
    }
    tagList(
      tags$b("Werdykt: "), verdict
    )
  })
}
