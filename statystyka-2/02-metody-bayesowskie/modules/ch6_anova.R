# ============================================================================
# CHAPTER 6: ANOVA - F-test vs anovaBF
# ============================================================================

ch6_ui <- lecture_chapter(
  id = "ch-anova",
  num = "06",
  title = "ANOVA",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 06 · Metody bayesowskie",
      num    = "06",
      title  = "ANOVA",
      lead   = "Bayesowska alternatywa dla ANOVA i interpretacja dowodu między modelami."
    ),

    lc_feedback(type = "info",
      "Trzy lub więcej grup. W częstościowej statystyce: F-test (jednoczynnikowa ANOVA).
       Tutaj: jej bayesowski odpowiednik — anovaBF."
    ),

    lc_h2("ch6-sec-01", "Idea dwoch podejsc"),

    tagList(
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

    figure_panel(label = "Ryc. 6.1", title = "ANOVA: te same dane, dwa paradygmaty",

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
                         class = "lc-btn-primary", width = "100%")
          )
        )
      )),

      br(),
      zoom_plot_ui("ch6_data_plot", height = "240px"),

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
            zoom_plot_ui("ch6_bayes_plot", height = "180px"),
            uiOutput("ch6_bayes_result")
          )
        )
      ),

      lc_feedback(type = "info",
        uiOutput("ch6_comparison")
      )
    ),

    lc_h2("ch6-sec-02", "Co zyskuję bayesowsko?"),

    tagList(
      p("ANOVA częstościowa daje jedną liczbę (p) i każe iść na post-hoc testy. Bayes:"),
      tags$ul(
        tags$li("BF₁₀ = jasna skala siły dowodu (nie tylko binarne „istotny/nie‟)"),
        tags$li("Można porównywać różne modele — nie tylko „grupa ma znaczenie‟"),
        tags$li("Dowód ", tags$em("za H₀"), " (BF₁₀ < 1) jest możliwy — p-wartość nigdy nie mówi „brak efektu‟")
      )
    ),

    lc_chapter_next(
      num = "07",
      title = "Tabele krzyżowe",
      lead = "Bayes Factor dla danych kategorycznych.",
      target_id = "ch-tabele"
    )

  )
)

ch6_server <- function(input, output, session) {

  sample_data <- reactiveVal(NULL)

  observe({
    if (is.null(sample_data())) {
      d <- generate_multi_groups_data(
        n_per_group = bayes_input(input$ch6_n, 20),
        means = c(bayes_input(input$ch6_mean_a, 50),
                  bayes_input(input$ch6_mean_b, 55),
                  bayes_input(input$ch6_mean_c, 60)),
        sd = bayes_input(input$ch6_sd, 10)
      )
      sample_data(d)
    }
  })

  observeEvent(list(input$ch6_draw, input$ch6_n, input$ch6_mean_a,
                    input$ch6_mean_b, input$ch6_mean_c, input$ch6_sd), {
    d <- generate_multi_groups_data(
      n_per_group = bayes_input(input$ch6_n, 20),
      means = c(bayes_input(input$ch6_mean_a, 50),
                bayes_input(input$ch6_mean_b, 55),
                bayes_input(input$ch6_mean_c, 60)),
      sd = bayes_input(input$ch6_sd, 10)
    )
    sample_data(d)
  }, ignoreInit = TRUE)

  result <- reactive({
    d <- sample_data()
    req(d)
    compute_bf_anova(d)
  })

  zoom_plot_server("ch6_data_plot", reactive({
    d <- sample_data()
    req(d)
    ggplot(d, aes(x = group, y = value, fill = group)) +
      geom_jitter(width = 0.15, size = 2, alpha = 0.5,
                   aes(color = group), show.legend = FALSE) +
      geom_boxplot(alpha = 0.55, width = 0.5, outlier.shape = NA) +
      scale_fill_manual(values = c(bayes_primary, bayes_warning, bayes_teal),
                        guide = "none") +
      scale_color_manual(values = c(bayes_primary, bayes_warning, bayes_teal),
                         guide = "none") +
      labs(x = "Grupa", y = "Wartość") +
      theme_upwr()
  }))

  output$ch6_freq_result <- renderUI({
    r <- result()
    p_info <- format_pval_pl(r$p_value)
    gs <- r$group_stats
    means_str <- paste0(gs$group, " = ", round(gs$mean, 2),
                        " (SD ", round(gs$sd, 2), ")",
                        collapse = " | ")
    lc_feedback(type = "info",
      tags$b("F("), r$df1, ", ", r$df2, ") = ",
      round(r$f_statistic, 3), tags$br(),
      HTML(p_info$decision), tags$br(),
      tags$b("Średnie grup: "), means_str
    )
  })

  zoom_plot_server("ch6_bayes_plot", reactive({
    r <- result()
    plot_bf_scale(r$bf10)
  }))

  output$ch6_bayes_result <- renderUI({
    r <- result()
    interp <- interpret_bf(r$bf10)
    lc_feedback(type = "info",
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
