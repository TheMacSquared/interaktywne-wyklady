# ============================================================================
# CHAPTER 6: ANOVA - F-test vs anovaBF
# ============================================================================

ch6_ui <- tabPanel("6. ANOVA",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Trzy lub wi\u0119cej grup. W cz\u0119sto\u015bciowej statystyce: F-test (jednoczynnikowa ANOVA).
       Tutaj: jej bayesowski odpowiednik \u2014 anovaBF."
    ),

    div(class = "section-title", "Idea dwoch podejsc"),

    div(class = "narrative",
      p(tags$b("Cz\u0119sto\u015bciowo: "), "F = wariancja mi\u0119dzy grupami / wariancja wewn\u0105trz.
         Du\u017ce F i ma\u0142e p \u2192 co najmniej jedna grupa r\u00f3\u017cni si\u0119 od pozosta\u0142ych."),
      p(tags$b("Bayesowsko: "), "anovaBF por\u00f3wnuje dwa modele:"),
      tags$ul(
        tags$li("M\u2080: warto\u015bci pochodz\u0105 z ", tags$em("jednego"),
                 " rozk\u0142adu (grupa nie ma znaczenia)"),
        tags$li("M\u2081: ka\u017cda grupa ma w\u0142asn\u0105 \u015bredni\u0105")
      ),
      p("BF\u2081\u2080 m\u00f3wi, ile razy bardziej prawdopodobne s\u0105 dane pod M\u2081 ni\u017c pod M\u2080.")
    ),

    div(class = "widget-block",
      h4("ANOVA: te same dane, dwa paradygmaty"),

      fluidRow(column(12,
        fluidRow(
          column(3,
            sliderInput("ch6_n", "n na grup\u0119:",
                        min = 10, max = 80, value = 25, step = 5)
          ),
          column(3,
            sliderInput("ch6_mean_a", "\u015brednia A:",
                        min = 0, max = 20, value = 10, step = 0.5)
          ),
          column(3,
            sliderInput("ch6_mean_b", "\u015brednia B:",
                        min = 0, max = 20, value = 12, step = 0.5)
          ),
          column(3,
            sliderInput("ch6_mean_c", "\u015brednia C:",
                        min = 0, max = 20, value = 11, step = 0.5)
          )
        ),
        fluidRow(
          column(3,
            sliderInput("ch6_sd", "SD wewn\u0105trzgrupowe:",
                        min = 1, max = 10, value = 4, step = 0.5)
          ),
          column(3,
            br(),
            actionButton("ch6_draw", "\u21bb Nowa pr\u00f3ba",
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

    div(class = "section-title", "Co zyskuj\u0119 bayesowsko?"),

    div(class = "narrative",
      p("ANOVA cz\u0119sto\u015bciowa daje jedn\u0105 liczb\u0119 (p) i ka\u017ce i\u015b\u0107 na post-hoc testy. Bayes:"),
      tags$ul(
        tags$li("BF\u2081\u2080 = jasna skala si\u0142y dowodu (nie tylko binarne \u201eistotny/nie\u201f)"),
        tags$li("Mo\u017cna por\u00f3wnywa\u0107 r\u00f3\u017cne modele \u2014 nie tylko \u201egrupa ma znaczenie\u201f"),
        tags$li("Dow\u00f3d ", tags$em("za H\u2080"), " (BF\u2081\u2080 < 1) jest mo\u017cliwy \u2014 p-warto\u015b\u0107 nigdy nie m\u00f3wi \u201ebrak efektu\u201f")
      )
    ),

    div(class = "chapter-transition",
      p("Por\u00f3wnywali\u015bmy \u015brednie. A co z danymi jako\u015bciowymi?
         Tabele krzy\u017cowe \u2014 te\u017c maj\u0105 bayesowski odpowiednik."),
      actionButton("ch6_next",
                   "Dalej: Tabele krzy\u017cowe \u2192",
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
      labs(title = "Dane: trzy grupy", x = "Grupa", y = "Warto\u015b\u0107") +
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
      tags$b("\u015arednie grup: "), means_str
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
      tags$b("BF\u2081\u2080 (model grupowy vs null): "),
      format_bf(r$bf10), tags$br(),
      tags$b("Interpretacja: "), interp$short_summary, tags$br(),
      tags$em("M\u2081: ka\u017cda grupa ma w\u0142asn\u0105 \u015bredni\u0105 | M\u2080: jedna wsp\u00f3lna \u015brednia")
    )
  })

  output$ch6_comparison <- renderUI({
    r <- result()
    verdict <- if (r$p_value < 0.05 && r$bf10 > 3) {
      "Oba podej\u015bcia zgodne: grupy r\u00f3\u017cni\u0105 si\u0119. Warto sprawdzi\u0107, ktore pary s\u0105 wyj\u0105tkowe."
    } else if (r$p_value >= 0.05 && r$bf10 < 1/3) {
      "Oba podej\u015bcia zgodne: brak przes\u0142anek, \u017ce grupy r\u00f3\u017cni\u0105 si\u0119 \u015brednimi."
    } else {
      "Dow\u00f3d niejednoznaczny \u2014 wi\u0119kszy n m\u00f3g\u0142by rozstrzygn\u0105\u0107."
    }
    tagList(tags$b("Werdykt: "), verdict)
  })
}
