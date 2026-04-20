# ============================================================================
# CHAPTER 7: Tabele krzyzowe - chi-kwadrat vs contingencyTableBF + OR
# ============================================================================

ch7_ui <- tabPanel("7. Tabele krzy\u017cowe",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Dwie zmienne jako\u015bciowe \u2014 klasyczne pytanie: czy grupa A r\u00f3\u017cni si\u0119 od grupy B
       pod wzgl\u0119dem cz\u0119sto\u015bci pewnej cechy? (np. skuteczno\u015b\u0107 leku vs placebo)."
    ),

    div(class = "section-title", "Chi-kwadrat vs BF dla tabeli"),

    div(class = "narrative",
      p(tags$b("Cz\u0119sto\u015bciowo: "), "test \u03c7\u00b2 niezale\u017cno\u015bci porownuje obserwowane
         liczno\u015bci z oczekiwanymi pod H\u2080 (niezale\u017cno\u015b\u0107 zmiennych). Zwraca p-warto\u015b\u0107."),
      p(tags$b("Bayesowsko: "), "contingencyTableBF zwraca BF\u2081\u2080 mi\u0119dzy modelem
         zale\u017cno\u015bci a modelem niezale\u017cno\u015bci. Dla 2\u00d72 dodatkowo mo\u017cemy policzy\u0107
         posterior dla OR \u2014 widzimy nie tylko ", tags$em("czy"),
         " zale\u017cno\u015b\u0107 istnieje, ale te\u017c ", tags$em("jak silna"), " i ", tags$em("w kt\u00f3r\u0105 stron\u0119"), ".")
    ),

    div(class = "callout-info",
      tags$b("Model pr\u00f3bkowania:"),
      " w BayesFactor wybieramy ", tags$em("sampleType"),
       ". Najcz\u0119\u015bciej \u201eindepMulti\u201f (grupy maj\u0105 ustalone sumy wierszy \u2014
       np. ustalone n w grupie leczonej i kontrolnej), czasem \u201epoisson\u201f
       (\u017cadne brzegi nie s\u0105 ustalone)."
    ),

    div(class = "section-title", "Tabela 2x2: skuteczno\u015b\u0107 leku vs placebo"),

    div(class = "widget-block",
      h4("Tabela 2\u00d72: sukces / pora\u017cka w dw\u00f3ch grupach"),

      fluidRow(column(12,
        fluidRow(
          column(3,
            h6("Grupa A (lek)"),
            numericInput("ch7_a_success", "Sukces\u00f3w:",
                         value = 40, min = 0, max = 500, step = 1),
            numericInput("ch7_a_fail", "Pora\u017cek:",
                         value = 60, min = 0, max = 500, step = 1)
          ),
          column(3,
            h6("Grupa B (placebo)"),
            numericInput("ch7_b_success", "Sukces\u00f3w:",
                         value = 25, min = 0, max = 500, step = 1),
            numericInput("ch7_b_fail", "Pora\u017cek:",
                         value = 75, min = 0, max = 500, step = 1)
          ),
          column(3,
            h6("Presety"),
            div(class = "preset-buttons",
              actionButton("ch7_preset_balanced", "Brak r\u00f3\u017cnicy",
                           class = "btn-outline-secondary btn-sm"),
              actionButton("ch7_preset_moderate", "Umiarkowany efekt",
                           class = "btn-outline-secondary btn-sm"),
              actionButton("ch7_preset_strong", "Silny efekt",
                           class = "btn-outline-secondary btn-sm")
            )
          ),
          column(3,
            h6("Prior"),
            sliderInput("ch7_prior_alpha", "\u03b1, \u03b2 (Beta prior):",
                        min = 0.5, max = 10, value = 1, step = 0.5),
            helpText("\u03b1 = \u03b2 = 1: Beta(1,1) = rozk\u0142ad p\u0142aski, nieinformatywny")
          )
        )
      )),

      br(),
      plotOutput("ch7_data_plot", height = "220px"),

      fluidRow(
        column(6,
          div(class = "panel-frequentist",
            h5("Test \u03c7\u00b2 niezale\u017cno\u015bci"),
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

    div(class = "section-title", "Co zyskuj\u0119 bayesowsko (dla 2\u00d72)?"),

    div(class = "narrative",
      p("Klasyczny test \u03c7\u00b2 m\u00f3wi \u201etak/nie\u201f o zale\u017cno\u015bci.
         Bayes \u2014 dla tabeli 2\u00d72 \u2014 daje nam bezpo\u015brednio:"),
      tags$ul(
        tags$li("BF\u2081\u2080: si\u0142\u0119 dowodu za zale\u017cno\u015bci\u0105"),
        tags$li("Posterior OR: rozk\u0142ad mo\u017cliwych warto\u015bci \u201eilu razy skuteczniej\u201f"),
        tags$li("HDI OR: wiarygodny przedzia\u0142 dla efektu"),
        tags$li("P(OR > 1): prawdopodobie\u0144stwo, \u017ce grupa A jest lepsza")
      )
    ),

    div(class = "callout-warning",
      tags$b("Ma\u0142e liczno\u015bci: "),
      "gdy komorki maj\u0105 < 5 oczekiwanych obserwacji, klasyczny \u03c7\u00b2 traci wa\u017cno\u015b\u0107
       (p-warto\u015b\u0107 aproksymacyjna). BF obliczony analitycznie nie ma tego problemu."
    ),

    div(class = "chapter-transition",
      p("Od tabel przechodzimy do zmiennych ilo\u015bciowych zwi\u0105zanych ze sob\u0105 liniowo
         \u2014 korelacja i jej bayesowski odpowiednik."),
      actionButton("ch7_next",
                   "Dalej: Korelacja \u2192",
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
    colnames(m) <- c("Sukces", "Pora\u017cka")
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
                            title = "Obserwowane liczno\u015bci",
                            col_a = col_success, col_b = col_secondary)
  })

  output$ch7_freq_result <- renderUI({
    r <- freq_res()
    p_info <- format_pval_pl(r$p_value)
    tab <- table_mat()
    or_classical <- (tab[1,1] * tab[2,2]) / (tab[1,2] * tab[2,1])
    warning_msg <- if (r$low_expected_pct > 0) {
      paste0(" \u26a0 ", round(r$low_expected_pct, 0),
             "% kom\u00f3rek ma oczekiwan\u0105 liczno\u015b\u0107 < 5 \u2014 p mo\u017ce by\u0107 niedok\u0142adne.")
    } else ""
    div(class = "callout-info",
      tags$b("\u03c7\u00b2 = "), round(r$chi_statistic, 3),
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
      tags$b("BF\u2081\u2080 (zale\u017cno\u015b\u0107 vs niezale\u017cno\u015b\u0107): "),
      format_bf(r$bf10), tags$br(),
      tags$b("Interpretacja: "), interp$short_summary, tags$br(),
      tags$b("Mediana OR: "), round(orr$or_median, 2),
      "  |  95% HDI: [", round(orr$or_hdi["lower"], 2),
      ", ", round(orr$or_hdi["upper"], 2), "]", tags$br(),
      tags$b("P(OR > 1 | dane) = "),
      paste0(round(orr$p_direction * 100, 1), "%"),
      tags$em(" (tj. \u017ce lek lepszy ni\u017c placebo)")
    )
  })

  output$ch7_comparison <- renderUI({
    r <- freq_res()
    orr <- or_res()
    direction <- if (orr$or_median > 1) "wi\u0119ksze szanse sukcesu" else "mniejsze szanse sukcesu"
    verdict <- if (r$p_value < 0.05 && r$bf10 > 3) {
      paste0("Oba podej\u015bcia zgodne: lek daje ", direction,
             " ni\u017c placebo. Mediana OR = ", round(orr$or_median, 2), ".")
    } else if (r$p_value >= 0.05 && r$bf10 < 1/3) {
      "Oba podej\u015bcia zgodne: brak przes\u0142anek do r\u00f3\u017cnicowania skuteczno\u015bci lek vs placebo."
    } else {
      paste0("Dow\u00f3d niejednoznaczny. Mediana OR = ",
             round(orr$or_median, 2),
             ", ale HDI obejmuje 1 \u2014 nie mo\u017cemy wykluczy\u0107 braku r\u00f3\u017cnicy.")
    }
    tagList(tags$b("Werdykt: "), verdict)
  })
}
