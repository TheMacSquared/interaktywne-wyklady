# ============================================================================
# CHAPTER 7: ANOVA
# ============================================================================

ch7_ui <- tabPanel("8. ANOVA",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Por\u00f3wnywali\u015bmy dwie grupy. A co, gdy grup jest trzy lub wi\u0119cej?"
    ),

    div(class = "section-title", "ANOVA jednoczynnikowa"),

    div(class = "narrative",
      p("ANOVA (Analysis of Variance) to uog\u00f3lnienie testu t na ",
        tags$b("3 lub wi\u0119cej grup"), "."),
      p("Pytanie: ", tags$b("Czy \u015brednie w k grupach r\u00f3\u017cni\u0105 si\u0119 istotnie?"), ""),
      p("Przyk\u0142ad: czy \u015brednie oceny r\u00f3\u017cni\u0105 si\u0119 mi\u0119dzy 4 kierunkami studi\u00f3w?"),
      div(class = "formula-box",
        p(withMathJax("\\(H_0: \\mu_1 = \\mu_2 = \\ldots = \\mu_k\\)")),
        p(withMathJax("\\(F = \\frac{MS_{\\text{mi\u0119dzy}}}{MS_{\\text{wewn\u0105trz}}} = \\frac{\\text{zmienno\u015b\u0107 mi\u0119dzy grupami}}{\\text{zmienno\u015b\u0107 wewn\u0105trz grup}}\\)"))
      ),
      p("Du\u017ce F oznacza: grupy r\u00f3\u017cni\u0105 si\u0119 bardziej ni\u017c mogliby\u015bmy oczekiwa\u0107 przypadkiem.")
    ),

    # ========================================================================
    # WIDGET 1: ANOVA jednoczynnikowa
    # ========================================================================
    div(class = "section-title", "ANOVA w akcji"),

    div(class = "widget-block",
      h4("ANOVA jednoczynnikowa"),
      fluidRow(
        column(4,
          selectInput("ch7_var", "Zmienna zale\u017cna:",
            choices = c(
              "\u015arednia ocen" = "srednia_ocen",
              "Wzrost" = "wzrost",
              "Czas dojazdu" = "czas_dojazdu"
            ),
            selected = "srednia_ocen"
          ),
          sliderInput("ch7_n", "n (og\u00f3\u0142em):",
                      min = 80, max = 300, value = 160, step = 20),
          actionButton("ch7_run_anova", "Generuj i testuj",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch7_boxplot", height = "350px"),
          uiOutput("ch7_anova_result")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Dekompozycja wariancji:"),
      " Ca\u0142kowita zmienno\u015b\u0107 = zmienno\u015b\u0107 mi\u0119dzy grupami + zmienno\u015b\u0107 wewn\u0105trz grup.
        ANOVA testuje, czy ta \"mi\u0119dzy\" cz\u0119\u015b\u0107 jest istotnie du\u017ca."
    ),

    # ========================================================================
    # WIDGET 2: Post-hoc Tukey
    # ========================================================================
    div(class = "section-title", "Testy post-hoc (Tukey HSD)"),

    div(class = "narrative",
      p("ANOVA m\u00f3wi ", tags$em("\"grupy r\u00f3\u017cni\u0105 si\u0119\""),
        ", ale nie m\u00f3wi ", tags$em("\"kt\u00f3re\""), "."),
      p("Do tego s\u0142u\u017c\u0105 testy post-hoc. Tukey HSD por\u00f3wnuje ",
        tags$b("ka\u017cd\u0105 par\u0119 grup"),
        " z korekt\u0105 na wielokrotne por\u00f3wnania.")
    ),

    div(class = "widget-block",
      h4("Tukey HSD"),
      fluidRow(
        column(4,
          helpText("U\u017cywa danych z ANOVA powy\u017cej. Najpierw uruchom ANOVA!"),
          actionButton("ch7_run_tukey", "Testuj Tukeyem",
                       class = "btn-warning", width = "100%")
        ),
        column(8,
          plotOutput("ch7_tukey_plot", height = "300px"),
          uiOutput("ch7_tukey_result")
        )
      )
    ),

    div(class = "callout-warning",
      tags$strong("Wa\u017cne:"),
      " Testy post-hoc wykonujemy ", tags$b("tylko"), " gdy ANOVA jest istotna.
        Bez tego korekcja na wielokrotne por\u00f3wnania jest niepotrzebna."
    ),

    # ========================================================================
    # WIDGET 3: Kruskal-Wallis
    # ========================================================================
    div(class = "section-title", "Kruskal-Wallis (nieparametryczny)"),

    div(class = "narrative",
      p("Odpowiednik nieparametryczny ANOVA \u2014 test ",
        tags$b("Kruskala-Wallisa"), "."),
      p("Dzia\u0142a na rangach, nie wymaga normalno\u015bci.
        Post-hoc: test Dunna z korekt\u0105.")
    ),

    div(class = "widget-block",
      h4("Kruskal-Wallis + Dunn"),
      fluidRow(
        column(4,
          helpText("U\u017cywa tych samych danych."),
          actionButton("ch7_run_kw", "Testuj Kruskalem-Wallisem",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          uiOutput("ch7_kw_result"),
          uiOutput("ch7_dunn_result")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Podsumowanie:"),
      tags$table(class = "table table-bordered", style = "font-size: 14px;",
        tags$thead(
          tags$tr(tags$th(""), tags$th("Parametryczny"), tags$th("Nieparametryczny"))
        ),
        tags$tbody(
          tags$tr(tags$td("G\u0142\u00f3wny test"), tags$td("ANOVA"), tags$td("Kruskal-Wallis")),
          tags$tr(tags$td("Post-hoc"), tags$td("Tukey HSD"), tags$td("Test Dunna"))
        )
      )
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: podsumowanie wszystkich test\u00f3w"),
      actionButton("ch7_next", "Dalej \u2192 9. \u015aci\u0105ga",
                   class = "btn-primary btn-lg")
    )
  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch7_server <- function(input, output, session) {

  # Shared ANOVA data
  ch7_data <- reactiveVal(NULL)

  observeEvent(input$ch7_run_anova, {
    data <- generate_student_data(input$ch7_n)
    ch7_data(data)
  })

  # --- Widget 1: ANOVA ---
  output$ch7_boxplot <- renderPlot({
    data <- ch7_data()
    if (is.null(data)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Generuj i testuj'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      var <- input$ch7_var
      var_label <- switch(var,
        "srednia_ocen" = "\u015arednia ocen",
        "wzrost" = "Wzrost (cm)",
        "czas_dojazdu" = "Czas dojazdu (min)")

      ggplot(data, aes(x = kierunek, y = .data[[var]], fill = kierunek)) +
        geom_boxplot(alpha = 0.6, outlier.alpha = 0.3) +
        geom_jitter(width = 0.15, alpha = 0.2, size = 1) +
        scale_fill_brewer(palette = "Set2") +
        labs(title = paste0(var_label, " wed\u0142ug kierunku"),
             x = "Kierunek", y = var_label) +
        theme_educational() +
        theme(legend.position = "none")
    }
  })

  output$ch7_anova_result <- renderUI({
    data <- ch7_data()
    if (is.null(data)) return(NULL)

    var <- input$ch7_var
    formula <- as.formula(paste(var, "~ kierunek"))

    result <- rstatix::anova_test(data, formula)
    tidy_res <- as.data.frame(result)

    # Eta-squared jest w wyniku rstatix
    eta_sq <- tidy_res$ges  # generalized eta squared

    p_val <- tidy_res$p
    res <- format_test_result(p_val)

    div(class = "callout-info",
      p(tags$strong("Wynik ANOVA jednoczynnikowej:")),
      p(paste0("F(", tidy_res$DFn, ", ", tidy_res$DFd, ") = ",
               round(tidy_res$F, 3))),
      p(paste0("p = ", format.pval(p_val, digits = 4))),
      p(paste0("\u03b7\u00b2 = ", round(eta_sq, 3),
               " (", effect_size_label(sqrt(eta_sq)), ")")),
      p(style = paste0("color:", res$color, "; font-weight: bold;"),
        res$decision)
    )
  })

  # --- Widget 2: Tukey post-hoc ---
  output$ch7_tukey_plot <- renderPlot({
    req(input$ch7_run_tukey)
    data <- isolate(ch7_data())
    if (is.null(data)) return(NULL)

    var <- isolate(input$ch7_var)
    formula <- as.formula(paste(var, "~ kierunek"))

    tukey <- rstatix::tukey_hsd(data, formula)
    tukey_df <- as.data.frame(tukey)

    tukey_df$comparison <- paste0(tukey_df$group1, " vs\n", tukey_df$group2)
    tukey_df$significant <- tukey_df$p.adj < 0.05

    ggplot(tukey_df, aes(x = estimate, y = comparison, color = significant)) +
      geom_point(size = 3) +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
      geom_vline(xintercept = 0, linetype = "dashed", color = col_dark) +
      scale_color_manual(values = c("TRUE" = col_reject, "FALSE" = col_accept),
                         labels = c("TRUE" = "p < 0.05", "FALSE" = "p \u2265 0.05"),
                         name = NULL) +
      labs(title = "Tukey HSD: r\u00f3\u017cnice parowe z 95% CI",
           x = "R\u00f3\u017cnica \u015brednich", y = "Por\u00f3wnanie") +
      theme_educational() +
      theme(legend.position = "top")
  })

  output$ch7_tukey_result <- renderUI({
    req(input$ch7_run_tukey)
    data <- isolate(ch7_data())
    if (is.null(data)) {
      return(div(class = "callout-warning", "Najpierw uruchom ANOVA."))
    }

    var <- isolate(input$ch7_var)
    formula <- as.formula(paste(var, "~ kierunek"))
    tukey <- rstatix::tukey_hsd(data, formula)
    tukey_df <- as.data.frame(tukey)

    sig_pairs <- tukey_df[tukey_df$p.adj < 0.05, ]
    n_sig <- nrow(sig_pairs)

    if (n_sig == 0) {
      div(class = "callout-info",
        p(tags$strong("\u017badna para nie r\u00f3\u017cni si\u0119 istotnie"),
          " (po korekcji Tukeya)."))
    } else {
      div(class = "callout-success",
        p(tags$strong(paste0(n_sig, " istotna(e) r\u00f3\u017cnica(e):"))),
        tags$ul(
          lapply(1:n_sig, function(i) {
            tags$li(paste0(sig_pairs$group1[i], " vs ", sig_pairs$group2[i],
                           ": \u0394 = ", round(sig_pairs$estimate[i], 2),
                           ", p.adj = ", format.pval(sig_pairs$p.adj[i], digits = 3)))
          })
        )
      )
    }
  })

  # --- Widget 3: Kruskal-Wallis ---
  output$ch7_kw_result <- renderUI({
    req(input$ch7_run_kw)
    data <- isolate(ch7_data())
    if (is.null(data)) {
      return(div(class = "callout-warning", "Najpierw wygeneruj dane."))
    }

    var <- isolate(input$ch7_var)
    formula <- as.formula(paste(var, "~ kierunek"))

    result <- rstatix::kruskal_test(data, formula)
    tidy_res <- as.data.frame(result)

    res <- format_test_result(tidy_res$p)

    div(class = "callout-info",
      p(tags$strong("Wynik testu Kruskala-Wallisa:")),
      p(paste0("H(", tidy_res$df, ") = ", round(tidy_res$statistic, 3))),
      p(paste0("p = ", format.pval(tidy_res$p, digits = 4))),
      p(style = paste0("color:", res$color, "; font-weight: bold;"),
        res$decision)
    )
  })

  output$ch7_dunn_result <- renderUI({
    req(input$ch7_run_kw)
    data <- isolate(ch7_data())
    if (is.null(data)) return(NULL)

    var <- isolate(input$ch7_var)
    formula <- as.formula(paste(var, "~ kierunek"))

    result <- rstatix::kruskal_test(data, formula)
    if (as.data.frame(result)$p >= 0.05) {
      return(div(class = "callout-info",
        p("Test nieistotny \u2014 post-hoc niepotrzebny.")))
    }

    dunn <- rstatix::dunn_test(data, formula, p.adjust.method = "holm")
    dunn_df <- as.data.frame(dunn)
    sig <- dunn_df[dunn_df$p.adj < 0.05, ]

    if (nrow(sig) == 0) {
      div(class = "callout-info",
        p("Test Dunna: \u017cadna para nie jest istotna po korekcji Holma."))
    } else {
      div(class = "callout-success",
        p(tags$strong("Test Dunna (post-hoc):")),
        tags$ul(
          lapply(1:nrow(sig), function(i) {
            tags$li(paste0(sig$group1[i], " vs ", sig$group2[i],
                           ": p.adj = ", format.pval(sig$p.adj[i], digits = 3)))
          })
        )
      )
    }
  })
}
