# ============================================================================
# CHAPTER 6: Ilosciowa vs jakosciowa (2 grupy)
# ============================================================================

ch6_ui <- tabPanel("7. Ilo\u015bciowa vs jako\u015bciowa",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Badali\u015bmy zwi\u0105zki dw\u00f3ch zmiennych tego samego typu.
       Teraz klasyczne pytanie: czy dwie grupy r\u00f3\u017cni\u0105 si\u0119?"
    ),

    div(class = "section-title", "Test t dla dw\u00f3ch pr\u00f3b niezale\u017cnych"),

    div(class = "narrative",
      p("Pytanie: ", tags$b("Czy \u015brednie w dw\u00f3ch grupach r\u00f3\u017cni\u0105 si\u0119 istotnie?"), ""),
      p("Przyk\u0142ad: czy m\u0119\u017cczy\u017ani i kobiety r\u00f3\u017cni\u0105 si\u0119 wzrostem?"),
      div(class = "formula-box",
        p(withMathJax("\\(H_0: \\mu_1 = \\mu_2 \\quad\\text{vs}\\quad H_1: \\mu_1 \\neq \\mu_2\\)")),
        p(withMathJax("\\(t = \\frac{\\bar{x}_1 - \\bar{x}_2}{SE}\\)"))
      )
    ),

    # ========================================================================
    # WIDGET 1: Test t niezalezny
    # ========================================================================
    div(class = "section-title", "Test t niezale\u017cny"),

    div(class = "widget-block",
      h4("Por\u00f3wnanie dw\u00f3ch grup"),
      fluidRow(
        column(4,
          selectInput("ch6_ind_var", "Zmienna ilo\u015bciowa:",
            choices = c(
              "Wzrost" = "wzrost",
              "Waga" = "waga",
              "\u015arednia ocen" = "srednia_ocen",
              "Czas dojazdu" = "czas_dojazdu"
            ),
            selected = "wzrost"
          ),
          sliderInput("ch6_ind_n", "n (na grup\u0119):",
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
    div(class = "section-title", "Test t dla pr\u00f3b zale\u017cnych (parowy)"),

    div(class = "narrative",
      p("Gdy mierzymy ", tags$b("tych samych"), " osobnik\u00f3w dwa razy
        (przed i po interwencji), u\u017cywamy testu ", tags$b("parowego"), "."),
      p("Przyk\u0142ad: wyniki student\u00f3w przed i po korepetycjach."),
      p("Testujemy r\u00f3\u017cnice: ", withMathJax("\\(d_i = x_{\\text{po},i} - x_{\\text{przed},i}\\)"),
        ". Pytamy, czy \u015brednia r\u00f3\u017cnic \u2260 0.")
    ),

    div(class = "widget-block",
      h4("Test parowy: przed vs po"),
      fluidRow(
        column(4,
          sliderInput("ch6_paired_n", "Liczba student\u00f3w:",
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

    # ========================================================================
    # WIDGET 3: Mann-Whitney
    # ========================================================================
    div(class = "section-title", "Mann-Whitney U (nieparametryczny)"),

    div(class = "narrative",
      p("Odpowiednik nieparametryczny testu t niezale\u017cnego.
        Testuje, czy rozk\u0142ady w dw\u00f3ch grupach si\u0119 r\u00f3\u017cni\u0105 (ranga)."),
      p("U\u017cywaj, gdy dane nie s\u0105 normalne lub s\u0105 porz\u0105dkowe.")
    ),

    div(class = "widget-block",
      h4("Mann-Whitney U"),
      fluidRow(
        column(4,
          helpText("U\u017cywa tych samych danych co test t niezale\u017cny powy\u017cej."),
          actionButton("ch6_run_mw", "Testuj Mann-Whitneyem",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          uiOutput("ch6_mw_result")
        )
      )
    ),

    # ========================================================================
    # WIDGET 4: Wilcoxon par
    # ========================================================================
    div(class = "section-title", "Wilcoxon par znakowych (nieparametryczny parowy)"),

    div(class = "narrative",
      p("Nieparametryczny odpowiednik testu t parowego.
        Dzia\u0142a na rangach r\u00f3\u017cnic.")
    ),

    div(class = "widget-block",
      h4("Wilcoxon par znakowych"),
      fluidRow(
        column(4,
          helpText("U\u017cywa tych samych danych parowych co test t parowy powy\u017cej."),
          actionButton("ch6_run_wilcox_paired", "Testuj Wilcoxonem",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          uiOutput("ch6_wilcox_paired_result")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Podsumowanie:"),
      tags$table(class = "table table-bordered", style = "font-size: 14px;",
        tags$thead(
          tags$tr(tags$th("Typ"), tags$th("Parametryczny"), tags$th("Nieparametryczny"))
        ),
        tags$tbody(
          tags$tr(tags$td("Niezale\u017cne"), tags$td("Test t"), tags$td("Mann-Whitney U")),
          tags$tr(tags$td("Parowe"), tags$td("Test t parowy"), tags$td("Wilcoxon par znakowych"))
        )
      )
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: a co gdy grup jest wi\u0119cej ni\u017c dwie?"),
      actionButton("ch6_next", "Dalej \u2192 8. ANOVA",
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
        "srednia_ocen" = "\u015arednia ocen", "czas_dojazdu" = "Czas dojazdu (min)")

      ggplot(data, aes(x = plec, y = .data[[var]], fill = plec)) +
        geom_boxplot(alpha = 0.6, outlier.alpha = 0.3) +
        geom_jitter(width = 0.15, alpha = 0.3, size = 1.5) +
        scale_fill_manual(values = c(col_h0, col_reject)) +
        labs(title = paste0(var_label, " vs P\u0142e\u0107"),
             x = "P\u0142e\u0107", y = var_label) +
        theme_test() +
        theme(legend.position = "none")
    }
  })

  output$ch6_ind_result <- renderUI({
    data <- ch6_ind_data()
    if (is.null(data)) return(NULL)

    var <- input$ch6_ind_var
    formula <- as.formula(paste(var, "~ plec"))

    result <- rstatix::t_test(data, formula)
    tidy_res <- as.data.frame(result)

    # Cohen's d
    d_res <- rstatix::cohens_d(data, formula)
    d_val <- as.data.frame(d_res)$effsize

    res <- format_test_result(tidy_res$p)

    div(class = "callout-info",
      p(tags$strong("Wynik testu t niezale\u017cnego:")),
      p(paste0("t(", round(tidy_res$df, 1), ") = ",
               round(tidy_res$statistic, 3))),
      p(paste0("p = ", format.pval(tidy_res$p, digits = 4))),
      p(paste0("Cohen's d = ", round(d_val, 3),
               " (", effect_size_label(d_val), ")")),
      p(style = paste0("color:", res$color, "; font-weight: bold;"),
        res$decision)
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
        theme_test() +
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
    res <- format_test_result(tidy_res$p)

    div(class = "callout-info",
      p(tags$strong("Wynik testu t parowego:")),
      p(paste0("\u015arednia r\u00f3\u017cnica: ", round(mean(data$wynik_po - data$wynik_przed), 2), " pkt")),
      p(paste0("t(", tidy_res$df, ") = ", round(tidy_res$statistic, 3))),
      p(paste0("p = ", format.pval(tidy_res$p, digits = 4))),
      p(paste0("Cohen's d = ", round(d_val, 3),
               " (", effect_size_label(d_val), ")")),
      p(style = paste0("color:", res$color, "; font-weight: bold;"),
        res$decision)
    )
  })

  # --- Widget 3: Mann-Whitney ---
  output$ch6_mw_result <- renderUI({
    req(input$ch6_run_mw)
    data <- isolate(ch6_ind_data())
    if (is.null(data)) {
      return(div(class = "callout-warning",
        "Najpierw wygeneruj dane testem t niezale\u017cnym."))
    }

    var <- isolate(input$ch6_ind_var)
    formula <- as.formula(paste(var, "~ plec"))

    result <- rstatix::wilcox_test(data, formula)
    tidy_res <- as.data.frame(result)
    res <- format_test_result(tidy_res$p)

    div(class = "callout-info",
      p(tags$strong("Wynik testu Mann-Whitney U:")),
      p(paste0("U = ", round(tidy_res$statistic, 1))),
      p(paste0("p = ", format.pval(tidy_res$p, digits = 4))),
      p(style = paste0("color:", res$color, "; font-weight: bold;"),
        res$decision)
    )
  })

  # --- Widget 4: Wilcoxon parowy ---
  output$ch6_wilcox_paired_result <- renderUI({
    req(input$ch6_run_wilcox_paired)
    data <- isolate(ch6_paired_data())
    if (is.null(data)) {
      return(div(class = "callout-warning",
        "Najpierw wygeneruj dane testem t parowym."))
    }

    long <- data %>%
      pivot_longer(cols = c(wynik_przed, wynik_po),
                   names_to = "moment", values_to = "wynik")
    long$moment <- factor(long$moment,
                          levels = c("wynik_przed", "wynik_po"))

    result <- rstatix::wilcox_test(long, wynik ~ moment, paired = TRUE)
    tidy_res <- as.data.frame(result)
    res <- format_test_result(tidy_res$p)

    div(class = "callout-info",
      p(tags$strong("Wynik testu Wilcoxona par znakowych:")),
      p(paste0("V = ", round(tidy_res$statistic, 1))),
      p(paste0("p = ", format.pval(tidy_res$p, digits = 4))),
      p(style = paste0("color:", res$color, "; font-weight: bold;"),
        res$decision)
    )
  })
}
