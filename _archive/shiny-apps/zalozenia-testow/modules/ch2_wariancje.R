# ============================================================================
# CHAPTER 2: Jednorodnosc wariancji
# ============================================================================

ch2_ui <- tabPanel("2. Jednorodne wariancje",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Normalno\u015b\u0107 to nie jedyne za\u0142o\u017cenie.
       Wiele test\u00f3w por\u00f3wnawczych wymaga r\u00f3wnych wariancji mi\u0119dzy grupami."
    ),

    div(class = "section-title", "Homoscedastyczno\u015b\u0107 \u2014 r\u00f3wne wariancje"),

    div(class = "narrative",
      p("Za\u0142o\u017cenie jednorodnych wariancji (homoscedastyczno\u015b\u0107) dotyczy:"),
      tags$ul(
        tags$li(tags$b("Test t niezale\u017cny"), " \u2014 wariancje w obu grupach powinny by\u0107 podobne"),
        tags$li(tags$b("ANOVA"), " \u2014 wariancje we wszystkich grupach por\u00f3wnywalne"),
        tags$li(tags$b("Regresja liniowa"), " \u2014 wariancja reszt sta\u0142a (homoscedastyczno\u015b\u0107 reszt)")
      )
    ),

    # ========================================================================
    # WIDGET 1: Wizualizacja
    # ========================================================================
    div(class = "section-title", "Jak wygl\u0105da naruszenie?"),

    div(class = "widget-block",
      h4("Dwie grupy o r\u00f3\u017cnej wariancji"),
      fluidRow(
        column(4,
          sliderInput("ch2_sd1", "SD grupy A:", min = 2, max = 30, value = 10, step = 1),
          sliderInput("ch2_sd2", "SD grupy B:", min = 2, max = 30, value = 10, step = 1),
          sliderInput("ch2_n_per", "n (na grup\u0119):", min = 15, max = 100, value = 40, step = 5),
          actionButton("ch2_gen", "Generuj dane",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch2_boxplot", height = "300px"),
          uiOutput("ch2_var_stats")
        )
      )
    ),

    # ========================================================================
    # WIDGET 2: Testy
    # ========================================================================
    div(class = "section-title", "Testy jednorodnych wariancji"),

    div(class = "narrative",
      p("Dwa popularne testy:"),
      tags$ul(
        tags$li(tags$b("Test Levene'a"), " \u2014 odporny na naruszenie normalno\u015bci, zalecany"),
        tags$li(tags$b("Test Bartletta"), " \u2014 mocniejszy, ale wra\u017cliwy na brak normalno\u015bci")
      ),
      p(withMathJax("\\(H_0\\)"), ": wariancje s\u0105 r\u00f3wne we wszystkich grupach.")
    ),

    div(class = "widget-block",
      h4("Levene i Bartlett"),
      fluidRow(
        column(4,
          helpText("U\u017cywa danych z widgetu powy\u017cej."),
          actionButton("ch2_test_var", "Testuj",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          uiOutput("ch2_test_results")
        )
      )
    ),

    # ========================================================================
    # WIDGET 3: Co robic?
    # ========================================================================
    div(class = "section-title", "Gdy wariancje s\u0105 nier\u00f3wne"),

    div(class = "narrative",
      p("Opcje:"),
      tags$ol(
        tags$li(tags$b("Test t Welcha"), " \u2014 domy\u015blny w R! Nie zak\u0142ada r\u00f3wnych wariancji.
                 W praktyce zawsze mo\u017cna u\u017cywa\u0107 Welcha zamiast klasycznego t."),
        tags$li(tags$b("Mann-Whitney U"), " \u2014 nieparametryczny, nie zak\u0142ada r\u00f3wnych wariancji"),
        tags$li(tags$b("ANOVA Welcha"), " \u2014 odpowiednik dla 3+ grup (", tags$code("oneway.test()"), ")"),
        tags$li(tags$b("Regresja: odporne b\u0142\u0119dy std."), " \u2014 ",
                tags$code("sandwich::vcovHC()"), " + ", tags$code("lmtest::coeftest()"))
      )
    ),

    div(class = "widget-block",
      h4("Test t klasyczny vs Welcha"),
      fluidRow(
        column(4,
          helpText("Por\u00f3wnanie wyniku: klasyczny test t (zak\u0142ada r\u00f3wne wariancje)
                    vs test Welcha (nie zak\u0142ada)."),
          actionButton("ch2_compare_t", "Por\u00f3wnaj testy",
                       class = "btn-warning", width = "100%")
        ),
        column(8,
          uiOutput("ch2_t_comparison")
        )
      )
    ),

    div(class = "callout-success",
      tags$strong("Praktyczna rada:"),
      " Zawsze u\u017cywaj testu Welcha (", tags$code("t.test(var.equal = FALSE)"),
      ") \u2014 to domy\u015blne zachowanie w R. Klasyczny test t z r\u00f3wnymi wariancjami
        ma sens tylko gdy masz pewno\u015b\u0107, \u017ce wariancje s\u0105 r\u00f3wne."
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: za\u0142o\u017cenia regresji"),
      actionButton("ch2_next", "Dalej \u2192 3. Za\u0142o\u017cenia regresji",
                   class = "btn-primary btn-lg")
    )
  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch2_server <- function(input, output, session) {

  ch2_data <- reactiveVal(NULL)

  observeEvent(input$ch2_gen, {
    ch2_data(generate_two_groups(
      n1 = input$ch2_n_per, n2 = input$ch2_n_per,
      sd1 = input$ch2_sd1, sd2 = input$ch2_sd2
    ))
  })

  output$ch2_boxplot <- renderPlot({
    df <- ch2_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Generuj dane'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      ggplot(df, aes(x = group, y = value, fill = group)) +
        geom_boxplot(alpha = 0.6) +
        geom_jitter(width = 0.15, alpha = 0.3) +
        scale_fill_manual(values = c(col_test, col_alt)) +
        labs(title = "Dwie grupy", x = "Grupa", y = "Warto\u015b\u0107") +
        theme_assumptions() +
        theme(legend.position = "none")
    }
  })

  output$ch2_var_stats <- renderUI({
    df <- ch2_data()
    if (is.null(df)) return(NULL)
    stats <- df %>% group_by(group) %>%
      summarise(sd = sd(value), var = var(value), .groups = "drop")
    ratio <- max(stats$var) / min(stats$var)

    tagList(
      div(class = "stat-box", style = paste0("background:", col_test, ";"),
          paste0("SD(A) = ", round(stats$sd[1], 2))),
      div(class = "stat-box", style = paste0("background:", col_alt, ";"),
          paste0("SD(B) = ", round(stats$sd[2], 2))),
      div(class = "stat-box",
          style = paste0("background:", if (ratio < 4) col_ok else col_fail, ";"),
          paste0("Ratio var: ", round(ratio, 2)))
    )
  })

  # --- Testy ---
  output$ch2_test_results <- renderUI({
    req(input$ch2_test_var)
    df <- isolate(ch2_data())
    if (is.null(df)) return(div(class = "callout-warning", "Najpierw wygeneruj dane."))

    lev <- rstatix::levene_test(df, value ~ group)
    bart <- bartlett.test(value ~ group, data = df)

    lev_color <- if (lev$p >= 0.05) col_ok else col_fail
    bart_color <- if (bart$p.value >= 0.05) col_ok else col_fail

    div(class = "callout-info",
      fluidRow(
        column(6,
          p(tags$strong("Test Levene'a:")),
          p(paste0("F = ", round(lev$statistic, 3))),
          p(paste0("p = ", format.pval(lev$p, digits = 4))),
          p(style = paste0("color:", lev_color, "; font-weight: bold;"),
            if (lev$p >= 0.05) "Wariancje jednorodne" else "Wariancje nier\u00f3wne!")
        ),
        column(6,
          p(tags$strong("Test Bartletta:")),
          p(paste0("\u03c7\u00b2 = ", round(bart$statistic, 3))),
          p(paste0("p = ", format.pval(bart$p.value, digits = 4))),
          p(style = paste0("color:", bart_color, "; font-weight: bold;"),
            if (bart$p.value >= 0.05) "Wariancje jednorodne" else "Wariancje nier\u00f3wne!")
        )
      )
    )
  })

  # --- Porownanie t ---
  output$ch2_t_comparison <- renderUI({
    req(input$ch2_compare_t)
    df <- isolate(ch2_data())
    if (is.null(df)) return(div(class = "callout-warning", "Najpierw wygeneruj dane."))

    t_classic <- t.test(value ~ group, data = df, var.equal = TRUE)
    t_welch <- t.test(value ~ group, data = df, var.equal = FALSE)

    div(class = "callout-info",
      fluidRow(
        column(6,
          p(tags$strong("Test t klasyczny"), " (var.equal=TRUE):"),
          p(paste0("t(", round(t_classic$parameter, 1), ") = ",
                   round(t_classic$statistic, 3))),
          p(paste0("p = ", format.pval(t_classic$p.value, digits = 4)))
        ),
        column(6,
          p(tags$strong("Test Welcha"), " (var.equal=FALSE):"),
          p(paste0("t(", round(t_welch$parameter, 1), ") = ",
                   round(t_welch$statistic, 3))),
          p(paste0("p = ", format.pval(t_welch$p.value, digits = 4)))
        )
      ),
      p(style = "margin-top: 10px;",
        tags$em("Przy nier\u00f3wnych wariancjach wyniki mog\u0105 si\u0119 r\u00f3\u017cni\u0107.
                 Welch jest bezpieczniejszy."))
    )
  })
}
