# ============================================================================
# CHAPTER 5: Dwie zmienne jakosciowe
# ============================================================================

ch5_ui <- tabPanel("6. Dwie zmienne jako\u015bciowe",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Badali\u015bmy korelacj\u0119 mi\u0119dzy zmiennymi ilo\u015bciowymi.
       A co, gdy obie zmienne s\u0105 jako\u015bciowe (kategorialne)?"
    ),

    div(class = "section-title", "Test \u03c7\u00b2 niezale\u017cno\u015bci"),

    div(class = "narrative",
      p("Pytanie: ", tags$b("Czy dwie zmienne jako\u015bciowe s\u0105 ze sob\u0105 powi\u0105zane?"), ""),
      p("Przyk\u0142ady: czy p\u0142e\u0107 wp\u0142ywa na wyb\u00f3r kierunku? Czy grupa krwi
        zale\u017cy od regionu?"),
      div(class = "formula-box",
        p(withMathJax("\\(H_0\\)"), ": zmienne s\u0105 niezale\u017cne"),
        p(withMathJax("\\(E_{ij} = \\frac{n_{i \\cdot} \\cdot n_{\\cdot j}}{n}\\)")),
        p(withMathJax("\\(\\chi^2 = \\sum \\frac{(O_{ij} - E_{ij})^2}{E_{ij}}\\)"))
      )
    ),

    # ========================================================================
    # WIDGET 1: Chi-kwadrat niezaleznosci
    # ========================================================================
    div(class = "section-title", "Test niezale\u017cno\u015bci w akcji"),

    div(class = "widget-block",
      h4("Test \u03c7\u00b2 niezale\u017cno\u015bci"),
      fluidRow(
        column(4,
          selectInput("ch5_scenario", "Scenariusz:",
            choices = c(
              "P\u0142e\u0107 vs kierunek" = "gender_field",
              "P\u0142e\u0107 vs zdanie egzaminu" = "gender_pass",
              "Kierunek vs zdanie egzaminu" = "field_pass"
            ),
            selected = "gender_field"
          ),
          sliderInput("ch5_n", "Wielko\u015b\u0107 pr\u00f3by (n):",
                      min = 50, max = 300, value = 150, step = 25),
          actionButton("ch5_run_chi", "Generuj i testuj",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch5_mosaic_plot", height = "350px"),
          uiOutput("ch5_chi_tables"),
          uiOutput("ch5_chi_result")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Cram\u00e9r's V:"),
      " Wielko\u015b\u0107 efektu dla \u03c7\u00b2. Zakres [0, 1]:
        < 0.1 pomijalny, 0.1\u20130.3 ma\u0142y, 0.3\u20130.5 \u015bredni, > 0.5 du\u017cy."
    ),

    # ========================================================================
    # WIDGET 2: Test Fishera
    # ========================================================================
    div(class = "section-title", "Test dok\u0142adny Fishera"),

    div(class = "narrative",
      p("Gdy oczekiwane liczno\u015bci s\u0105 ma\u0142e (< 5), test \u03c7\u00b2 jest niedok\u0142adny.
        Wtedy u\u017cywamy testu ", tags$b("dok\u0142adnego Fishera"), "."),
      p("Przyk\u0142ad: tabela 2\u00d72 z niewielk\u0105 pr\u00f3b\u0105.")
    ),

    div(class = "widget-block",
      h4("Fisher vs \u03c7\u00b2"),
      fluidRow(
        column(4,
          sliderInput("ch5_fisher_n", "Wielko\u015b\u0107 pr\u00f3by (n):",
                      min = 10, max = 100, value = 20, step = 5),
          checkboxInput("ch5_fisher_assoc", "Dodaj zwi\u0105zek mi\u0119dzy zmiennymi",
                        value = FALSE),
          actionButton("ch5_run_fisher", "Generuj i testuj",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          uiOutput("ch5_fisher_table"),
          uiOutput("ch5_fisher_result")
        )
      )
    ),

    div(class = "callout-warning",
      tags$strong("Regu\u0142a:"),
      " Gdy kt\u00f3rakolwiek oczekiwana liczno\u015b\u0107 < 5, u\u017cyj testu Fishera.
        W R: ", tags$code("fisher.test()"), ". Dla tabel wi\u0119kszych ni\u017c 2\u00d72
        mo\u017cna te\u017c u\u017cy\u0107 symulacji Monte Carlo: ",
      tags$code("chisq.test(simulate.p.value = TRUE)"), "."
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: por\u00f3wnywanie dw\u00f3ch grup (zmienna ilo\u015bciowa vs jako\u015bciowa)"),
      actionButton("ch5_next", "Dalej \u2192 7. Ilo\u015bciowa vs jako\u015bciowa",
                   class = "btn-primary btn-lg")
    )
  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch5_server <- function(input, output, session) {

  # --- Widget 1: Chi-kwadrat niezaleznosci ---
  ch5_chi_data <- reactiveVal(NULL)

  observeEvent(input$ch5_run_chi, {
    n <- input$ch5_n
    data <- generate_student_data(n)

    vars <- switch(input$ch5_scenario,
      "gender_field" = list(var1 = data$plec, var2 = data$kierunek,
                            lab1 = "P\u0142e\u0107", lab2 = "Kierunek"),
      "gender_pass"  = list(var1 = data$plec, var2 = data$zdal_egzamin,
                            lab1 = "P\u0142e\u0107", lab2 = "Zda\u0142 egzamin"),
      "field_pass"   = list(var1 = data$kierunek, var2 = data$zdal_egzamin,
                            lab1 = "Kierunek", lab2 = "Zda\u0142 egzamin")
    )

    tab <- table(vars$var1, vars$var2)
    test <- chisq.test(tab)

    # Cramers V
    k <- min(nrow(tab), ncol(tab))
    cramers_v <- sqrt(test$statistic / (n * (k - 1)))

    ch5_chi_data(list(
      tab = tab, test = test, expected = test$expected,
      var1_lab = vars$lab1, var2_lab = vars$lab2,
      cramers_v = cramers_v
    ))
  })

  output$ch5_mosaic_plot <- renderPlot({
    dd <- ch5_chi_data()
    if (is.null(dd)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Generuj i testuj'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      # Barplot of contingency table
      df <- as.data.frame(dd$tab)
      names(df) <- c("Var1", "Var2", "Freq")

      ggplot(df, aes(x = Var1, y = Freq, fill = Var2)) +
        geom_col(position = "dodge", alpha = 0.8) +
        labs(title = paste0(dd$var1_lab, " vs ", dd$var2_lab),
             x = dd$var1_lab, y = "Liczno\u015b\u0107", fill = dd$var2_lab) +
        scale_fill_brewer(palette = "Set2") +
        theme_test() +
        theme(legend.position = "top")
    }
  })

  output$ch5_chi_tables <- renderUI({
    dd <- ch5_chi_data()
    if (is.null(dd)) return(NULL)

    # Obserwowane i oczekiwane
    obs_html <- knitr::kable(dd$tab, format = "html",
                              caption = "Obserwowane liczno\u015bci")
    exp_html <- knitr::kable(round(dd$expected, 1), format = "html",
                              caption = "Oczekiwane liczno\u015bci (pod H\u2080)")

    div(
      HTML(obs_html),
      HTML(exp_html)
    )
  })

  output$ch5_chi_result <- renderUI({
    dd <- ch5_chi_data()
    if (is.null(dd)) return(NULL)

    test <- dd$test
    res <- format_test_result(test$p.value)

    # Ostrzezenie o malych oczekiwanych
    low_expected <- any(dd$expected < 5)

    tagList(
      div(class = "callout-info",
        p(tags$strong("Wynik testu \u03c7\u00b2 niezale\u017cno\u015bci:")),
        p(paste0("\u03c7\u00b2(", test$parameter, ") = ",
                 round(test$statistic, 3))),
        p(paste0("p = ", format.pval(test$p.value, digits = 4))),
        p(paste0("Cram\u00e9r's V = ", round(dd$cramers_v, 3),
                 " (", effect_size_label(dd$cramers_v), ")")),
        p(style = paste0("color:", res$color, "; font-weight: bold;"),
          res$decision)
      ),
      if (low_expected) {
        div(class = "callout-danger",
          tags$strong("Uwaga!"),
          " Niekt\u00f3re oczekiwane liczno\u015bci < 5. Rozwa\u017c test Fishera."
        )
      }
    )
  })

  # --- Widget 2: Fisher ---
  ch5_fisher_data <- reactiveVal(NULL)

  observeEvent(input$ch5_run_fisher, {
    n <- input$ch5_fisher_n

    if (input$ch5_fisher_assoc) {
      # Z asociacja
      plec <- sample(c("K", "M"), n, replace = TRUE)
      prob_pass <- ifelse(plec == "K", 0.8, 0.4)
      zdal <- ifelse(runif(n) < prob_pass, "Tak", "Nie")
    } else {
      plec <- sample(c("K", "M"), n, replace = TRUE)
      zdal <- sample(c("Tak", "Nie"), n, replace = TRUE, prob = c(0.6, 0.4))
    }

    tab <- table(plec, zdal)
    test_chi <- chisq.test(tab, correct = FALSE)
    test_fisher <- fisher.test(tab)
    expected <- test_chi$expected

    ch5_fisher_data(list(
      tab = tab, expected = expected,
      test_chi = test_chi, test_fisher = test_fisher
    ))
  })

  output$ch5_fisher_table <- renderUI({
    dd <- ch5_fisher_data()
    if (is.null(dd)) return(NULL)

    obs_html <- knitr::kable(dd$tab, format = "html",
                              caption = "Obserwowane")
    exp_html <- knitr::kable(round(dd$expected, 1), format = "html",
                              caption = "Oczekiwane")

    low <- any(dd$expected < 5)

    div(
      HTML(obs_html),
      HTML(exp_html),
      if (low) div(class = "callout-danger",
        tags$strong("Oczekiwane < 5!"),
        " Test \u03c7\u00b2 mo\u017ce by\u0107 niedok\u0142adny.")
    )
  })

  output$ch5_fisher_result <- renderUI({
    dd <- ch5_fisher_data()
    if (is.null(dd)) return(NULL)

    res_chi <- format_test_result(dd$test_chi$p.value)
    res_fisher <- format_test_result(dd$test_fisher$p.value)

    div(class = "callout-info",
      fluidRow(
        column(6,
          p(tags$strong("Test \u03c7\u00b2:")),
          p(paste0("p = ", format.pval(dd$test_chi$p.value, digits = 4))),
          p(style = paste0("color:", res_chi$color, "; font-weight: bold;"),
            res_chi$decision)
        ),
        column(6,
          p(tags$strong("Test Fishera:")),
          p(paste0("p = ", format.pval(dd$test_fisher$p.value, digits = 4))),
          p(style = paste0("color:", res_fisher$color, "; font-weight: bold;"),
            res_fisher$decision)
        )
      )
    )
  })
}
