# ============================================================================
# CHAPTER 6: ANOVA - F-test vs anovaBF
# ============================================================================

ch6_ui <- tabPanel("6. ANOVA",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Trzy lub więcej grup. W częstościowej statystyce: F-test (jednoczynnikowa ANOVA).
       Tutaj: jej bayesowski odpowiednik — anovaBF."
    ),

    div(class = "section-title", "Idea dwoch podejsc"),

    div(class = "narrative",
      p(tags$b("Częstościowo: "), "F = wariancja między grupami / wariancja wewnątrz.
         Duże F i małe p → co najmniej jedna grupa różni się od pozostałych."),
      p(tags$b("Bayesowsko: "), "anovaBF porównuje dwa modele:"),
      tags$ul(
        tags$li("M₀: wartości pochodzą z ", tags$em("jednego"),
                 " rozkładu (grupa nie ma znaczenia)"),
        tags$li("M₁: każda grupa ma własną średnią")
      ),
      p("BF₁₀ mówi, ile razy bardziej prawdopodobne są dane pod M₁ niż pod M₀.")
    ),

    div(class = "widget-block",
      h4("ANOVA: te same dane, dwa paradygmaty"),

      fluidRow(column(12,
        fluidRow(
          column(3,
            sliderInput("ch6_n", "n na grupę:",
                        min = 10, max = 80, value = 25, step = 5)
          ),
          column(3,
            sliderInput("ch6_mean_a", "średnia A:",
                        min = 0, max = 20, value = 10, step = 0.5)
          ),
          column(3,
            sliderInput("ch6_mean_b", "średnia B:",
                        min = 0, max = 20, value = 12, step = 0.5)
          ),
          column(3,
            sliderInput("ch6_mean_c", "średnia C:",
                        min = 0, max = 20, value = 11, step = 0.5)
          )
        ),
        fluidRow(
          column(3,
            sliderInput("ch6_sd", "SD wewnątrzgrupowe:",
                        min = 1, max = 10, value = 4, step = 0.5)
          ),
          column(3,
            br(),
            actionButton("ch6_draw", "↻ Nowa próba",
                         class = "btn-primary", width = "100%")
          )
        )
      )),

      br(),
      plotOutput("ch6_data_plot", height = "240px"),

      fluidRow(
        column(6,
          div(class = "panel-frequentist",
            h5("F-test (ANOVA)"),
            uiOutput("ch6_freq_result")
          )
        ),
        column(6,
          div(class = "panel-bayesian",
            h5("anovaBF"),
            plotOutput("ch6_bayes_plot", height = "180px"),
            uiOutput("ch6_bayes_result")
          )
        )
      ),

      div(class = "callout-info",
        uiOutput("ch6_comparison")
      )
    ),

    div(class = "section-title", "Co zyskuję bayesowsko?"),

    div(class = "narrative",
      p("ANOVA częstościowa daje jedną liczbę (p) i każe iść na post-hoc testy. Bayes:"),
      tags$ul(
        tags$li("BF₁₀ = jasna skala siły dowodu (nie tylko binarne „istotny/nie‟)"),
        tags$li("Można porównywać różne modele — nie tylko „grupa ma znaczenie‟"),
        tags$li("Dowód ", tags$em("za H₀"), " (BF₁₀ < 1) jest możliwy — p-wartość nigdy nie mówi „brak efektu‟")
      )
    ),

    div(class = "chapter-transition",
      p("Porównywaliśmy średnie. A co z danymi jakościowymi?
         Tabele krzyżowe — też mają bayesowski odpowiednik."),
      actionButton("ch6_next",
                   "Dalej: Tabele krzyżowe →",
                   class = "btn-primary btn-lg")
    )

  )) # column, fluidRow
)

ch6_server <- function(input, output, session) {

  sample_data <- reactiveVal(NULL)

  observe({
    if (is.null(sample_data())) {
      d <- generate_multi_groups_data(
        n_per_group = input$ch6_n,
        means = c(input$ch6_mean_a, input$ch6_mean_b, input$ch6_mean_c),
        sd = input$ch6_sd
      )
      sample_data(d)
    }
  })

  observeEvent(list(input$ch6_draw, input$ch6_n, input$ch6_mean_a,
                    input$ch6_mean_b, input$ch6_mean_c, input$ch6_sd), {
    d <- generate_multi_groups_data(
      n_per_group = input$ch6_n,
      means = c(input$ch6_mean_a, input$ch6_mean_b, input$ch6_mean_c),
      sd = input$ch6_sd
    )
    sample_data(d)
  }, ignoreInit = TRUE)

  result <- reactive({
    d <- sample_data()
    req(d)
    compute_bf_anova(d)
  })

  output$ch6_data_plot <- renderPlot({
    d <- sample_data()
    req(d)
    ggplot(d, aes(x = group, y = value, fill = group)) +
      geom_jitter(width = 0.15, size = 2, alpha = 0.5,
                   aes(color = group), show.legend = FALSE) +
      geom_boxplot(alpha = 0.55, width = 0.5, outlier.shape = NA) +
      scale_fill_manual(values = c(col_primary, col_warning, col_teal),
                        guide = "none") +
      scale_color_manual(values = c(col_primary, col_warning, col_teal),
                         guide = "none") +
      labs(title = "Dane: trzy grupy", x = "Grupa", y = "Wartość") +
      theme_educational()
  })

  output$ch6_freq_result <- renderUI({
    r <- result()
    p_info <- format_pval_pl(r$p_value)
    gs <- r$group_stats
    means_str <- paste0(gs$group, " = ", round(gs$mean, 2),
                        " (SD ", round(gs$sd, 2), ")",
                        collapse = " | ")
    div(class = "callout-info",
      tags$b("F("), r$df1, ", ", r$df2, ") = ",
      round(r$f_statistic, 3), tags$br(),
      HTML(p_info$decision), tags$br(),
      tags$b("Średnie grup: "), means_str
    )
  })

  output$ch6_bayes_plot <- renderPlot({
    r <- result()
    plot_bf_scale(r$bf10)
  })

  output$ch6_bayes_result <- renderUI({
    r <- result()
    interp <- interpret_bf(r$bf10)
    div(class = "callout-info",
      tags$b("BF₁₀ (model grupowy vs null): "),
      format_bf(r$bf10), tags$br(),
      tags$b("Interpretacja: "), interp$short_summary, tags$br(),
      tags$em("M₁: każda grupa ma własną średnią | M₀: jedna wspólna średnia")
    )
  })

  output$ch6_comparison <- renderUI({
    r <- result()
    verdict <- if (r$p_value < 0.05 && r$bf10 > 3) {
      "Oba podejścia zgodne: grupy różnią się. Warto sprawdzić, ktore pary są wyjątkowe."
    } else if (r$p_value >= 0.05 && r$bf10 < 1/3) {
      "Oba podejścia zgodne: brak przesłanek, że grupy różnią się średnimi."
    } else {
      "Dowód niejednoznaczny — większy n mógłby rozstrzygnąć."
    }
    tagList(tags$b("Werdykt: "), verdict)
  })
}
