# ============================================================================
# CHAPTER 6: Ilosciowa i jakosciowa (2 grupy)
# ============================================================================

ch6_ui <- tabPanel("7. Ilościowa i jakościowa",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Badaliśmy związki dwóch zmiennych tego samego typu.
       Teraz klasyczne pytanie: czy dwie grupy różnią się?"
    ),

    div(class = "section-title", "Test t dla dwóch prób niezależnych"),

    div(class = "narrative",
      p("Pytanie: Czy średnie w dwóch grupach różnią się istotnie?"),
      p("Przykład: czy mężczyźni i kobiety różnią się wzrostem?"),
      div(class = "formula-box",
        p(withMathJax("\\(H_0: \\mu_1 = \\mu_2\\)"), " — ",
          withMathJax("\\(H_a: \\mu_1 \\neq \\mu_2\\)")),
        p(withMathJax("\\(t = \\frac{\\bar{x}_1 - \\bar{x}_2}{SE}\\)"))
      )
    ),

    # ========================================================================
    # WIDGET 1: Test t niezalezny
    # ========================================================================
    div(class = "section-title", "Test t niezależny"),

    div(class = "widget-block",
      h4("Porównanie dwóch grup"),
      fluidRow(
        column(4,
          selectInput("ch6_ind_var", "Zmienna ilościowa:",
            choices = c(
              "Wzrost" = "wzrost",
              "Waga" = "waga",
              "Średnia ocen" = "srednia_ocen",
              "Czas dojazdu" = "czas_dojazdu"
            ),
            selected = "wzrost"
          ),
          sliderInput("ch6_ind_n", "n (na grupę):",
                      min = 15, max = 100, value = 40, step = 5),
          actionButton("ch6_run_ind_t", "Generuj i testuj",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch6_ind_boxplot", height = "300px"),
          uiOutput("ch6_ind_result")
        )
      )
    ),

    # ========================================================================
    # WIDGET 2: Test t parowy
    # ========================================================================
    div(class = "section-title", "Test t dla prób zależnych (parowy)"),

    div(class = "narrative",
      p("Gdy mierzymy tych samych osobników dwa razy
        (przed i po interwencji), używamy testu parowego."),
      p("Przykład: wyniki studentów przed i po korepetycjach."),
      p("Testujemy różnice: ", withMathJax("\\(d_i = x_{\\text{po},i} - x_{\\text{przed},i}\\)"),
        ". Pytamy, czy średnia różnic ≠ 0.")
    ),

    div(class = "widget-block",
      h4("Test parowy: przed i po"),
      fluidRow(
        column(4,
          sliderInput("ch6_paired_n", "Liczba studentów:",
                      min = 10, max = 50, value = 25, step = 5),
          sliderInput("ch6_paired_effect", "Efekt interwencji (pkt):",
                      min = 0, max = 15, value = 5, step = 1),
          actionButton("ch6_run_paired", "Generuj i testuj",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch6_paired_plot", height = "300px"),
          uiOutput("ch6_paired_result")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Uwaga: "),
      "gdy założenia testu t nie są spełnione (skrajne odstające, mocno skośny rozkład,
       małe n), stosuje się testy nieparametryczne — omówimy je w osobnym wykładzie."
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: a co gdy grup jest więcej niż dwie?"),
      actionButton("ch6_next", "Dalej → 8. ANOVA",
                   class = "btn-primary btn-lg")
    )
  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch6_server <- function(input, output, session) {

  # Shared independent data
  ch6_ind_data <- reactiveVal(NULL)

  observeEvent(input$ch6_run_ind_t, {
    n <- input$ch6_ind_n
    data <- generate_student_data(n * 2)
    ch6_ind_data(data)
  })

  # Shared paired data
  ch6_paired_data <- reactiveVal(NULL)

  observeEvent(input$ch6_run_paired, {
    ch6_paired_data(generate_paired_data(input$ch6_paired_n, input$ch6_paired_effect))
  })

  # --- Widget 1: Test t niezalezny ---
  output$ch6_ind_boxplot <- renderPlot({
    data <- ch6_ind_data()
    if (is.null(data)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Generuj i testuj'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      var <- input$ch6_ind_var
      var_label <- switch(var,
        "wzrost" = "Wzrost (cm)", "waga" = "Waga (kg)",
        "srednia_ocen" = "Średnia ocen", "czas_dojazdu" = "Czas dojazdu (min)")

      ggplot(data, aes(x = plec, y = .data[[var]], fill = plec)) +
        geom_boxplot(alpha = 0.6, outlier.alpha = 0.3) +
        geom_jitter(width = 0.15, alpha = 0.3, size = 1.5) +
        scale_fill_manual(values = c(col_h0, col_reject)) +
        labs(title = paste0(var_label, " według płci"),
             x = "Płeć", y = var_label) +
        theme_educational() +
        theme(legend.position = "none")
    }
  })

  output$ch6_ind_result <- renderUI({
    data <- ch6_ind_data()
    if (is.null(data)) return(NULL)

    var <- input$ch6_ind_var
    var_label <- switch(var,
      "wzrost" = "wzrost", "waga" = "waga",
      "srednia_ocen" = "średnia ocen", "czas_dojazdu" = "czas dojazdu", var)
    formula <- as.formula(paste(var, "~ plec"))

    result <- rstatix::t_test(data, formula)
    tidy_res <- as.data.frame(result)

    # Cohen's d
    d_res <- rstatix::cohens_d(data, formula)
    d_val <- as.data.frame(d_res)$effsize

    # Konkretny werdykt: która grupa wyższa, o ile
    means <- data %>% dplyr::group_by(plec) %>%
      dplyr::summarise(m = mean(.data[[var]], na.rm = TRUE), .groups = "drop")
    higher <- means$plec[which.max(means$m)]
    lower <- means$plec[which.min(means$m)]
    diff_val <- round(max(means$m) - min(means$m), 2)

    res <- format_test_result(tidy_res$p)

    div(class = "callout-info",
      p(tags$strong("Wynik testu t niezależnego:")),
      p(paste0("t(", round(tidy_res$df, 1), ") = ",
               round(tidy_res$statistic, 3))),
      p(paste0("p = ", format.pval(tidy_res$p, digits = 4))),
      p(paste0("Cohen's d = ", round(d_val, 3),
               " (efekt ", effect_size_label(d_val), ")")),
      p(tags$em(interpret_cohens_d(d_val))),
      p(style = paste0("color:", res$color, "; font-weight: bold;"),
        res$decision),
      p(tags$strong("Werdykt: "),
        "średnia ", var_label, " w grupie ", tags$b(as.character(higher)),
        " była wyższa od grupy ", tags$b(as.character(lower)),
        " o ", tags$b(diff_val), ".")
    )
  })

  # --- Widget 2: Test t parowy ---
  output$ch6_paired_plot <- renderPlot({
    data <- ch6_paired_data()
    if (is.null(data)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Generuj i testuj'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      # Connected dot plot
      long <- data %>%
        pivot_longer(cols = c(wynik_przed, wynik_po),
                     names_to = "moment", values_to = "wynik") %>%
        mutate(moment = factor(moment,
                               levels = c("wynik_przed", "wynik_po"),
                               labels = c("Przed", "Po")))

      ggplot(long, aes(x = moment, y = wynik)) +
        geom_line(aes(group = student), alpha = 0.3, color = col_paired) +
        geom_point(aes(color = moment), size = 2.5, alpha = 0.7) +
        scale_color_manual(values = c(col_h0, col_reject)) +
        labs(title = "Wyniki przed i po interwencji",
             x = "Moment", y = "Wynik") +
        theme_educational() +
        theme(legend.position = "none")
    }
  })

  output$ch6_paired_result <- renderUI({
    data <- ch6_paired_data()
    if (is.null(data)) return(NULL)

    long <- data %>%
      pivot_longer(cols = c(wynik_przed, wynik_po),
                   names_to = "moment", values_to = "wynik")
    long$moment <- factor(long$moment,
                          levels = c("wynik_przed", "wynik_po"))

    result <- rstatix::t_test(long, wynik ~ moment, paired = TRUE)
    tidy_res <- as.data.frame(result)

    d_val <- mean(data$wynik_po - data$wynik_przed) / sd(data$wynik_po - data$wynik_przed)
    mean_diff <- mean(data$wynik_po - data$wynik_przed)
    res <- format_test_result(tidy_res$p)
    direction <- if (mean_diff > 0) "wzrosły" else if (mean_diff < 0) "spadły" else "nie zmieniły się"

    div(class = "callout-info",
      p(tags$strong("Wynik testu t parowego:")),
      p(paste0("Średnia różnica: ", round(mean_diff, 2), " pkt")),
      p(paste0("t(", tidy_res$df, ") = ", round(tidy_res$statistic, 3))),
      p(paste0("p = ", format.pval(tidy_res$p, digits = 4))),
      p(paste0("Cohen's d = ", round(d_val, 3),
               " (efekt ", effect_size_label(d_val), ")")),
      p(tags$em(interpret_cohens_d(d_val))),
      p(style = paste0("color:", res$color, "; font-weight: bold;"),
        res$decision),
      p(tags$strong("Werdykt: "),
        "wyniki średnio ", tags$b(direction), " o ", tags$b(round(abs(mean_diff), 2)), " pkt.")
    )
  })

}
