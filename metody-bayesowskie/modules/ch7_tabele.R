# ============================================================================
# CHAPTER 7: Tabele krzyzowe - chi-kwadrat vs contingencyTableBF + OR
# ============================================================================

ch7_ui <- tabPanel("7. Tabele krzyżowe",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Dwie zmienne jakościowe — klasyczne pytanie: czy grupa A różni się od grupy B
       pod względem częstości pewnej cechy? (np. skuteczność leku vs placebo)."
    ),

    div(class = "section-title", "Chi-kwadrat vs BF dla tabeli"),

    div(class = "narrative",
      p(tags$b("Częstościowo: "), "test χ² niezależności porownuje obserwowane
         liczności z oczekiwanymi pod H₀ (niezależność zmiennych). Zwraca p-wartość."),
      p(tags$b("Bayesowsko: "), "contingencyTableBF zwraca BF₁₀ między modelem
         zależności a modelem niezależności. Dla 2×2 dodatkowo możemy policzyć
         posterior dla OR — widzimy nie tylko ", tags$em("czy"),
         " zależność istnieje, ale też ", tags$em("jak silna"), " i ", tags$em("w którą stronę"), ".")
    ),

    div(class = "callout-info",
      tags$b("Model próbkowania:"),
      " w BayesFactor wybieramy ", tags$em("sampleType"),
       ". Najczęściej „indepMulti‟ (grupy mają ustalone sumy wierszy —
       np. ustalone n w grupie leczonej i kontrolnej), czasem „poisson‟
       (żadne brzegi nie są ustalone)."
    ),

    div(class = "section-title", "Tabela 2x2: skuteczność leku vs placebo"),

    div(class = "widget-block",
      h4("Tabela 2×2: sukces / porażka w dwóch grupach"),

      fluidRow(column(12,
        fluidRow(
          column(3,
            h6("Grupa A (lek)"),
            numericInput("ch7_a_success", "Sukcesów:",
                         value = 40, min = 0, max = 500, step = 1),
            numericInput("ch7_a_fail", "Porażek:",
                         value = 60, min = 0, max = 500, step = 1)
          ),
          column(3,
            h6("Grupa B (placebo)"),
            numericInput("ch7_b_success", "Sukcesów:",
                         value = 25, min = 0, max = 500, step = 1),
            numericInput("ch7_b_fail", "Porażek:",
                         value = 75, min = 0, max = 500, step = 1)
          ),
          column(3,
            h6("Presety"),
            div(class = "preset-buttons",
              actionButton("ch7_preset_balanced", "Brak różnicy",
                           class = "btn-outline-secondary btn-sm"),
              actionButton("ch7_preset_moderate", "Umiarkowany efekt",
                           class = "btn-outline-secondary btn-sm"),
              actionButton("ch7_preset_strong", "Silny efekt",
                           class = "btn-outline-secondary btn-sm")
            )
          ),
          column(3,
            h6("Prior"),
            sliderInput("ch7_prior_alpha", "α, β (Beta prior):",
                        min = 0.5, max = 10, value = 1, step = 0.5),
            helpText("α = β = 1: Beta(1,1) = rozkład płaski, nieinformatywny")
          )
        )
      )),

      br(),
      plotOutput("ch7_data_plot", height = "220px"),

      fluidRow(
        column(6,
          div(class = "panel-frequentist",
            h5("Test χ² niezależności"),
            uiOutput("ch7_freq_result")
          )
        ),
        column(6,
          div(class = "panel-bayesian",
            h5("contingencyTableBF + posterior OR"),
            plotOutput("ch7_bayes_plot", height = "220px"),
            uiOutput("ch7_bayes_result")
          )
        )
      ),

      div(class = "callout-info",
        uiOutput("ch7_comparison")
      )
    ),

    div(class = "section-title", "Co zyskuję bayesowsko (dla 2×2)?"),

    div(class = "narrative",
      p("Klasyczny test χ² mówi „tak/nie‟ o zależności.
         Bayes — dla tabeli 2×2 — daje nam bezpośrednio:"),
      tags$ul(
        tags$li("BF₁₀: siłę dowodu za zależnością"),
        tags$li("Posterior OR: rozkład możliwych wartości „ilu razy skuteczniej‟"),
        tags$li("HDI OR: wiarygodny przedział dla efektu"),
        tags$li("P(OR > 1): prawdopodobieństwo, że grupa A jest lepsza")
      )
    ),

    div(class = "callout-warning",
      tags$b("Małe liczności: "),
      "gdy komorki mają < 5 oczekiwanych obserwacji, klasyczny χ² traci ważność
       (p-wartość aproksymacyjna). BF obliczony analitycznie nie ma tego problemu."
    ),

    div(class = "chapter-transition",
      p("Od tabel przechodzimy do zmiennych ilościowych związanych ze sobą liniowo
         — korelacja i jej bayesowski odpowiednik."),
      actionButton("ch7_next",
                   "Dalej: Korelacja →",
                   class = "btn-primary btn-lg")
    )

  )) # column, fluidRow
)

ch7_server <- function(input, output, session) {

  # Presety
  observeEvent(input$ch7_preset_balanced, {
    updateNumericInput(session, "ch7_a_success", value = 50)
    updateNumericInput(session, "ch7_a_fail",    value = 50)
    updateNumericInput(session, "ch7_b_success", value = 50)
    updateNumericInput(session, "ch7_b_fail",    value = 50)
  })
  observeEvent(input$ch7_preset_moderate, {
    updateNumericInput(session, "ch7_a_success", value = 40)
    updateNumericInput(session, "ch7_a_fail",    value = 60)
    updateNumericInput(session, "ch7_b_success", value = 25)
    updateNumericInput(session, "ch7_b_fail",    value = 75)
  })
  observeEvent(input$ch7_preset_strong, {
    updateNumericInput(session, "ch7_a_success", value = 70)
    updateNumericInput(session, "ch7_a_fail",    value = 30)
    updateNumericInput(session, "ch7_b_success", value = 20)
    updateNumericInput(session, "ch7_b_fail",    value = 80)
  })

  table_mat <- reactive({
    m <- matrix(c(input$ch7_a_success, input$ch7_a_fail,
                   input$ch7_b_success, input$ch7_b_fail),
                 nrow = 2, byrow = TRUE)
    rownames(m) <- c("Lek (A)", "Placebo (B)")
    colnames(m) <- c("Sukces", "Porażka")
    m
  })

  freq_res <- reactive({
    compute_bf_contingency(table_mat(), sampling = "indepMulti",
                            fixed_margin = "rows")
  })

  or_res <- reactive({
    posterior_2x2_or(table_mat(),
                     alpha_prior = input$ch7_prior_alpha,
                     beta_prior  = input$ch7_prior_alpha)
  })

  output$ch7_data_plot <- renderPlot({
    plot_contingency_table(table_mat(),
                            title = "Obserwowane liczności",
                            col_a = col_success, col_b = col_secondary)
  })

  output$ch7_freq_result <- renderUI({
    r <- freq_res()
    p_info <- format_pval_pl(r$p_value)
    tab <- table_mat()
    or_classical <- (tab[1,1] * tab[2,2]) / (tab[1,2] * tab[2,1])
    warning_msg <- if (r$low_expected_pct > 0) {
      paste0(" ⚠ ", round(r$low_expected_pct, 0),
             "% komórek ma oczekiwaną liczność < 5 — p może być niedokładne.")
    } else ""
    div(class = "callout-info",
      tags$b("χ² = "), round(r$chi_statistic, 3),
      " | df = ", r$df, tags$br(),
      HTML(p_info$decision), tags$br(),
      tags$b("OR obserwowany: "), round(or_classical, 2),
      warning_msg
    )
  })

  output$ch7_bayes_plot <- renderPlot({
    plot_posterior_or(or_res(),
                      col_posterior = col_posterior, col_hdi = col_hdi)
  })

  output$ch7_bayes_result <- renderUI({
    r <- freq_res()
    orr <- or_res()
    interp <- interpret_bf(r$bf10)
    div(class = "callout-info",
      tags$b("BF₁₀ (zależność vs niezależność): "),
      format_bf(r$bf10), tags$br(),
      tags$b("Interpretacja: "), interp$short_summary, tags$br(),
      tags$b("Mediana OR: "), round(orr$or_median, 2),
      "  |  95% HDI: [", round(orr$or_hdi["lower"], 2),
      ", ", round(orr$or_hdi["upper"], 2), "]", tags$br(),
      tags$b("P(OR > 1 | dane) = "),
      paste0(round(orr$p_direction * 100, 1), "%"),
      tags$em(" (tj. że lek lepszy niż placebo)")
    )
  })

  output$ch7_comparison <- renderUI({
    r <- freq_res()
    orr <- or_res()
    direction <- if (orr$or_median > 1) "większe szanse sukcesu" else "mniejsze szanse sukcesu"
    verdict <- if (r$p_value < 0.05 && r$bf10 > 3) {
      paste0("Oba podejścia zgodne: lek daje ", direction,
             " niż placebo. Mediana OR = ", round(orr$or_median, 2), ".")
    } else if (r$p_value >= 0.05 && r$bf10 < 1/3) {
      "Oba podejścia zgodne: brak przesłanek do różnicowania skuteczności lek vs placebo."
    } else {
      paste0("Dowód niejednoznaczny. Mediana OR = ",
             round(orr$or_median, 2),
             ", ale HDI obejmuje 1 — nie możemy wykluczyć braku różnicy.")
    }
    tagList(tags$b("Werdykt: "), verdict)
  })
}
