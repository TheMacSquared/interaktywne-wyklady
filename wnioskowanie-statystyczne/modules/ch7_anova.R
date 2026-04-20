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
      p("ANOVA (Analysis of Variance) to uog\u00f3lnienie testu t na 3 lub wi\u0119cej grup."),
      p("Pytanie: Czy \u015brednie w k grupach r\u00f3\u017cni\u0105 si\u0119 istotnie?"),
      p("Przyk\u0142ad z technologii \u017cywno\u015bci: czy pH jogurtu r\u00f3\u017cni si\u0119 mi\u0119dzy trzema temperaturami fermentacji (20\u00b0C, 25\u00b0C, 30\u00b0C)?"),
      div(class = "formula-box",
        p(withMathJax("\\(H_0: \\mu_1 = \\mu_2 = \\ldots = \\mu_k\\) \u2014 wszystkie \u015brednie s\u0105 r\u00f3wne")),
        p("ANOVA testuje ", tags$b("jedn\u0105"), " hipotez\u0119 zerow\u0105 \u2014 \u017ce wszystkie \u015brednie s\u0105 r\u00f3wne.
           Odrzucenie H\u2080 m\u00f3wi ", tags$em("\u201eco najmniej jedna \u015brednia odstaje od pozosta\u0142ych\u201d"),
          ", ale nie m\u00f3wi ", tags$b("kt\u00f3ra"),
          " \u2014 do tego s\u0142u\u017cy post-hoc.")
      ),
      p("Du\u017ca statystyka F oznacza: r\u00f3\u017cnice mi\u0119dzy grupami s\u0105 wi\u0119ksze ni\u017c mogliby\u015bmy oczekiwa\u0107 przypadkiem.")
    ),

    # ========================================================================
    # WIDGET 1: ANOVA jednoczynnikowa
    # ========================================================================
    div(class = "section-title", "ANOVA w akcji"),

    div(class = "widget-block",
      h4("ANOVA jednoczynnikowa"),
      fluidRow(
        column(4,
          selectInput("ch7_scenario", "Scenariusz:",
            choices = c(
              "Fermentacja jogurtu (T\u017b)" = "fermentation",
              "Kierunki studi\u00f3w" = "students"
            ),
            selected = "fermentation"
          ),
          uiOutput("ch7_var_ui"),
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
    # Intuicja eta^2
    # ========================================================================
    div(class = "section-title", "Jak czyta\u0107 si\u0142\u0119 efektu (\u03b7\u00b2)?"),

    div(class = "narrative",
      p("P-warto\u015b\u0107 m\u00f3wi ", tags$em("czy"), " r\u00f3\u017cnice istniej\u0105.
        ", tags$b("\u03b7\u00b2 (eta kwadrat)"), " m\u00f3wi ", tags$em("jak du\u017ce"), " s\u0105
        \u2014 jaki u\u0142amek ca\u0142ej zmienno\u015bci wyja\u015bnia czynnik grupuj\u0105cy.")
    ),

    div(class = "callout-success",
      tags$strong("\u201eIle procent r\u00f3\u017cnic mi\u0119dzy pr\u00f3bkami t\u0142umaczy ten czynnik, a ile zostaje na inne rzeczy?\u201d"),
      tags$table(class = "table table-bordered", style = "font-size: 15px; margin-top: 10px;",
        tags$thead(
          tags$tr(tags$th("\u03b7\u00b2"), tags$th("Jak to czyta\u0107"), tags$th("Efekt"))
        ),
        tags$tbody(
          tags$tr(tags$td("0.01"), tags$td("~1% zmienno\u015bci wyja\u015bnione \u2014 wp\u0142yw symboliczny"), tags$td("ma\u0142y")),
          tags$tr(tags$td("0.06"), tags$td("~6% zmienno\u015bci wyja\u015bnione \u2014 czynnik liczy si\u0119"), tags$td("\u015bredni")),
          tags$tr(tags$td("0.14"), tags$td("~14% zmienno\u015bci wyja\u015bnione \u2014 czynnik dominuj\u0105cy"), tags$td("du\u017cy"))
        )
      ),
      p(tags$strong("Przyk\u0142ad (fermentacja):"),
        " \u03b7\u00b2 = 0.23 oznacza, \u017ce oko\u0142o 23% r\u00f3\u017cnic w pH jogurtu t\u0142umaczy temperatura fermentacji.
          Pozosta\u0142e 77% to inne czynniki: drobnoustroje startowe, czysto\u015b\u0107 surowca, czas, partia mleka.")
    ),

    div(class = "callout-info",
      tags$strong("Uwaga praktyczna: "),
      "ma\u0142e p przy ma\u0142ym \u03b7\u00b2 oznacza cz\u0119sto tylko ", tags$em("du\u017c\u0105 pr\u00f3b\u0119"),
      ", nie du\u017cy efekt. Zawsze raportuj ", tags$b("oba"), "."
    ),

    # ========================================================================
    # WIDGET 2: Post-hoc Tukey
    # ========================================================================
    div(class = "section-title", "Testy post-hoc (Tukey HSD)"),

    div(class = "narrative",
      p("ANOVA m\u00f3wi ", tags$em("\"grupy r\u00f3\u017cni\u0105 si\u0119\""),
        ", ale nie m\u00f3wi ", tags$em("\"kt\u00f3re\""), "."),
      p("Do tego s\u0142u\u017c\u0105 testy post-hoc. Tukey HSD por\u00f3wnuje ka\u017cd\u0105 par\u0119 grup
        z korekt\u0105 na wielokrotne por\u00f3wnania.")
    ),

    div(class = "callout-warning",
      tags$strong("Dlaczego nie testowa\u0107 par pojedynczo testem t?"),
      p("Gdy zrobisz wiele por\u00f3wna\u0144, ryzyko ", tags$em("fa\u0142szywego alarmu"),
        " (odrzucenia H\u2080 gdy nie powinno) narasta:"),
      tags$table(class = "table table-bordered", style = "font-size: 15px;",
        tags$thead(tags$tr(tags$th("Liczba grup"), tags$th("Liczba par"),
                           tags$th("Ryzyko \u2265 1 fa\u0142szywego alarmu"))),
        tags$tbody(
          tags$tr(tags$td("3"), tags$td("3"), tags$td("~14%")),
          tags$tr(tags$td("4"), tags$td("6"), tags$td("~26%")),
          tags$tr(tags$td("5"), tags$td("10"), tags$td("~40%"))
        )
      ),
      p("Tukey/Dunn trzymaj\u0105 to ryzyko na 5% dla ", tags$b("ca\u0142ej rodziny"),
        " por\u00f3wna\u0144 \u2014 to ich zadanie.")
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

    div(class = "callout-info",
      tags$strong("Kiedy Tukey, a kiedy Dunn?"),
      p("Do ka\u017cdego g\u0142\u00f3wnego testu pasuje ", tags$em("sw\u00f3j"), " post-hoc:"),
      tags$ul(
        tags$li(tags$strong("Po ANOVA \u2192 Tukey HSD"),
                " (parametryczny, por\u00f3wnuje \u015brednie, r\u00f3wnoliczne grupy)."),
        tags$li(tags$strong("Po Kruskalu-Wallisie \u2192 Test Dunna"),
                " (nieparametryczny, por\u00f3wnuje rangi, odporny na ma\u0142e pr\u00f3bki i sko\u015bno\u015b\u0107).")
      ),
      p("Prosta regu\u0142a: ", tags$em("\u201eTym samym narz\u0119dziem, kt\u00f3rym bada\u0142e\u015b ca\u0142o\u015b\u0107, sprawd\u017a pary.\u201d"))
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

  # Konfiguracja scenariuszy: jakie zmienne zale\u017cne, jaka kolumna grupuj\u0105ca, etykiety
  ch7_scenario_cfg <- function(scenario) {
    if (identical(scenario, "fermentation")) {
      list(
        group_col = "temperatura",
        group_label = "Temperatura fermentacji",
        vars = c("pH jogurtu" = "pH", "Kwasowo\u015b\u0107 miareczkowa (\u00b0SH)" = "kwasowosc_SH"),
        var_labels = c(pH = "pH jogurtu", kwasowosc_SH = "Kwasowo\u015b\u0107 (\u00b0SH)")
      )
    } else {
      list(
        group_col = "kierunek",
        group_label = "Kierunek",
        vars = c("\u015arednia ocen" = "srednia_ocen", "Wzrost" = "wzrost", "Czas dojazdu" = "czas_dojazdu"),
        var_labels = c(srednia_ocen = "\u015arednia ocen", wzrost = "Wzrost (cm)", czas_dojazdu = "Czas dojazdu (min)")
      )
    }
  }

  output$ch7_var_ui <- renderUI({
    cfg <- ch7_scenario_cfg(input$ch7_scenario)
    selectInput("ch7_var", "Zmienna zale\u017cna:",
                choices = cfg$vars,
                selected = unname(cfg$vars[1]))
  })

  observeEvent(input$ch7_scenario, {
    ch7_data(NULL)
  })

  observeEvent(input$ch7_run_anova, {
    if (identical(input$ch7_scenario, "fermentation")) {
      ch7_data(generate_fermentation_data(input$ch7_n))
    } else {
      ch7_data(generate_student_data(input$ch7_n))
    }
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
      cfg <- ch7_scenario_cfg(input$ch7_scenario)
      var <- input$ch7_var
      req(var %in% names(data))
      var_label <- cfg$var_labels[[var]]
      group_col <- cfg$group_col

      ggplot(data, aes(x = .data[[group_col]], y = .data[[var]], fill = .data[[group_col]])) +
        geom_boxplot(alpha = 0.6, outlier.alpha = 0.3) +
        geom_jitter(width = 0.15, alpha = 0.2, size = 1) +
        scale_fill_brewer(palette = "Set2") +
        labs(title = paste0(var_label, " wed\u0142ug: ", cfg$group_label),
             x = cfg$group_label, y = var_label) +
        theme_educational() +
        theme(legend.position = "none")
    }
  })

  output$ch7_anova_result <- renderUI({
    data <- ch7_data()
    if (is.null(data)) return(NULL)

    cfg <- ch7_scenario_cfg(input$ch7_scenario)
    var <- input$ch7_var
    req(var %in% names(data))
    formula <- as.formula(paste(var, "~", cfg$group_col))

    result <- rstatix::anova_test(data, formula)
    tidy_res <- as.data.frame(result)

    # Eta-squared jest w wyniku rstatix (generalized eta squared)
    eta_sq <- tidy_res$ges

    # Etykieta efektu dla \u03b7\u00b2 (progi Cohena: 0.01 ma\u0142y, 0.06 \u015bredni, 0.14 du\u017cy)
    eta_label <- if (eta_sq < 0.01) "pomijalny"
      else if (eta_sq < 0.06) "ma\u0142y"
      else if (eta_sq < 0.14) "\u015bredni"
      else "du\u017cy"

    p_val <- tidy_res$p
    res <- format_test_result(p_val)

    div(class = "callout-info",
      p(tags$strong("Wynik ANOVA jednoczynnikowej:")),
      p(paste0("F(", tidy_res$DFn, ", ", tidy_res$DFd, ") = ",
               round(tidy_res$F, 3))),
      p(paste0("p = ", format.pval(p_val, digits = 4))),
      p(paste0("\u03b7\u00b2 = ", round(eta_sq, 3),
               " (efekt ", eta_label, ", ok. ",
               round(eta_sq * 100), "% zmienno\u015bci wyja\u015bnione)")),
      p(style = paste0("color:", res$color, "; font-weight: bold;"),
        res$decision)
    )
  })

  # --- Widget 2: Tukey post-hoc ---
  output$ch7_tukey_plot <- renderPlot({
    req(input$ch7_run_tukey)
    data <- isolate(ch7_data())
    if (is.null(data)) return(NULL)

    cfg <- ch7_scenario_cfg(isolate(input$ch7_scenario))
    var <- isolate(input$ch7_var)
    req(var %in% names(data))
    formula <- as.formula(paste(var, "~", cfg$group_col))

    tukey <- rstatix::tukey_hsd(data, formula)
    tukey_df <- as.data.frame(tukey)

    tukey_df$comparison <- paste0(tukey_df$group1, "\n\u2014 ", tukey_df$group2)
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

    cfg <- ch7_scenario_cfg(isolate(input$ch7_scenario))
    var <- isolate(input$ch7_var)
    req(var %in% names(data))
    formula <- as.formula(paste(var, "~", cfg$group_col))
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
            tags$li(paste0(sig_pairs$group1[i], " \u2014 ", sig_pairs$group2[i],
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

    cfg <- ch7_scenario_cfg(isolate(input$ch7_scenario))
    var <- isolate(input$ch7_var)
    req(var %in% names(data))
    formula <- as.formula(paste(var, "~", cfg$group_col))

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

    cfg <- ch7_scenario_cfg(isolate(input$ch7_scenario))
    var <- isolate(input$ch7_var)
    req(var %in% names(data))
    formula <- as.formula(paste(var, "~", cfg$group_col))

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
            tags$li(paste0(sig$group1[i], " \u2014 ", sig$group2[i],
                           ": p.adj = ", format.pval(sig$p.adj[i], digits = 3)))
          })
        )
      )
    }
  })
}
