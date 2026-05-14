# ============================================================================
# CHAPTER 7: Tabele krzyzowe - chi-kwadrat vs contingencyTableBF + OR
# ============================================================================

ch7_ui <- lecture_chapter(
  id = "ch-tabele",
  num = "07",
  title = "Tabele krzyżowe",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 07 · Metody bayesowskie",
      num    = "07",
      title  = "Tabele krzyżowe",
      lead   = "Klasyczny chi-kwadrat kontra bayesowski dowód dla tabel 2x2."
    ),

    lc_feedback(type = "info",
      "Dwie zmienne jakościowe — klasyczne pytanie: czy grupa A różni się od grupy B
       pod względem częstości pewnej cechy? (np. skuteczność leku vs placebo)."
    ),

    lc_h2("ch7-sec-01", "Chi-kwadrat vs BF dla tabeli"),

    tagList(
      p(tags$b("Częstościowo: "), "test χ² niezależności porownuje obserwowane
         liczności z oczekiwanymi pod H₀ (niezależność zmiennych). Zwraca p-wartość."),
      p(tags$b("Bayesowsko: "), "contingencyTableBF zwraca BF₁₀ między modelem
         zależności a modelem niezależności. Dla 2×2 dodatkowo możemy policzyć
         posterior dla OR — widzimy nie tylko ", tags$em("czy"),
         " zależność istnieje, ale też ", tags$em("jak silna"), " i ", tags$em("w którą stronę"), ".")
    ),

    lc_feedback(type = "info",
      tags$b("Model próbkowania:"),
      " w BayesFactor wybieramy ", tags$em("sampleType"),
       ". Najczęściej „indepMulti‟ (grupy mają ustalone sumy wierszy —
       np. ustalone n w grupie leczonej i kontrolnej), czasem „poisson‟
       (żadne brzegi nie są ustalone)."
    ),

    lc_h2("ch7-sec-02", "Tabela 2x2: skuteczność leku vs placebo"),

    figure_panel(label = "Ryc. 7.1", title = "Tabela 2×2: sukces / porażka w dwóch grupach",

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
                           class = "lc-btn-secondary-outline lc-btn-sm"),
              actionButton("ch7_preset_moderate", "Umiarkowany efekt",
                           class = "lc-btn-secondary-outline lc-btn-sm"),
              actionButton("ch7_preset_strong", "Silny efekt",
                           class = "lc-btn-secondary-outline lc-btn-sm")
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
      zoom_plot_ui("ch7_data_plot", height = "220px"),

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
            zoom_plot_ui("ch7_bayes_plot", height = "220px"),
            uiOutput("ch7_bayes_result")
          )
        )
      ),

      lc_feedback(type = "info",
        uiOutput("ch7_comparison")
      )
    ),

    lc_h2("ch7-sec-03", "Co zyskuję bayesowsko (dla 2×2)?"),

    tagList(
      p("Klasyczny test χ² mówi „tak/nie‟ o zależności.
         Bayes — dla tabeli 2×2 — daje nam bezpośrednio:"),
      tags$ul(
        tags$li("BF₁₀: siłę dowodu za zależnością"),
        tags$li("Posterior OR: rozkład możliwych wartości „ilu razy skuteczniej‟"),
        tags$li("HDI OR: wiarygodny przedział dla efektu"),
        tags$li("P(OR > 1): prawdopodobieństwo, że grupa A jest lepsza")
      )
    ),

    lc_feedback(type = "warning",
      tags$b("Małe liczności: "),
      "gdy komorki mają < 5 oczekiwanych obserwacji, klasyczny χ² traci ważność
       (p-wartość aproksymacyjna). BF obliczony analitycznie nie ma tego problemu."
    ),

    lc_chapter_next(
      num = "08",
      title = "Korelacja",
      lead = "związek między zmiennymi jako BF i posterior.",
      target_id = "ch-korelacja"
    )

  )
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

  zoom_plot_server("ch7_data_plot", reactive({
    plot_contingency_table(table_mat(),
                            title = "Obserwowane liczności",
                            col_a = bayes_success, col_b = bayes_secondary)
  }))

  output$ch7_freq_result <- renderUI({
    r <- freq_res()
    p_info <- format_pval_pl(r$p_value)
    tab <- table_mat()
    or_classical <- (tab[1,1] * tab[2,2]) / (tab[1,2] * tab[2,1])
    warning_msg <- if (r$low_expected_pct > 0) {
      paste0(" ⚠ ", round(r$low_expected_pct, 0),
             "% komórek ma oczekiwaną liczność < 5 — p może być niedokładne.")
    } else ""
    lc_feedback(type = "info",
      tags$b("χ² = "), round(r$chi_statistic, 3),
      " | df = ", r$df, tags$br(),
      HTML(p_info$decision), tags$br(),
      tags$b("OR obserwowany: "), round(or_classical, 2),
      warning_msg
    )
  })

  zoom_plot_server("ch7_bayes_plot", reactive({
    plot_posterior_or(or_res(),
                      bayes_posterior = bayes_posterior, bayes_hdi = bayes_hdi)
  }))

  output$ch7_bayes_result <- renderUI({
    r <- freq_res()
    orr <- or_res()
    interp <- interpret_bf(r$bf10)
    lc_feedback(type = "info",
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
