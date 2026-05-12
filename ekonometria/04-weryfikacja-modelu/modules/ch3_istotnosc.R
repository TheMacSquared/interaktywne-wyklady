# ============================================================================
# ROZDZIAŁ 3: Istotność statystyczna
# ============================================================================

ch3_ui <- lecture_chapter(
  id = "ch-istotnosc",
  num = "03",
  title = "Istotność statystyczna",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 04 · Weryfikacja",
      num = "03",
      title = "Czy efekt to coś więcej niż przypadek?",
      lead = "Test istotności pyta: czy efekt, który widzimy w b₁, jest większy niż przypadkowe wahanie? Albo równoważnie: czy dane pozwalają stwierdzić, że β₁ ≠ 0?"
    ),

    lc_h2("ch3-hipotezy", "Hipotezy"),
    lc_formula_box(
      withMathJax(helpText("$$H_0: \\beta_1 = 0 \\quad vs \\quad H_1: \\beta_1 \\neq 0$$")),
      p(strong("H₀:"), " zmienna X NIE wpływa na Y."),
      p(strong("H₁:"), " zmienna X wpływa na Y (kierunek dowolny).")
    ),
    lc_p("To jest test dwustronny — interesuje nas, czy X w ogóle ma związek z Y, niezależnie od kierunku. W praktyce w raporcie i tak podajemy znak b₁."),

    lc_h2("ch3-statystyka", "Statystyka testu"),
    lc_formula_box(
      withMathJax(helpText("$$t = \\frac{\\hat\\beta_1}{SE(\\hat\\beta_1)}$$")),
      p("Pod H₀ ta statystyka ma rozkład t z df = n − k.")
    ),
    lc_p("Intuicyjnie: t to ", tags$em("ile odchyleń standardowych"), " od zera leży nasza estymata. Jeśli t = 0.3, b₁ jest tak blisko zera, że trudno mówić o efekcie. Jeśli t = 5, b₁ jest tak daleko od zera, że trudno to wyjaśnić przypadkiem."),

    lc_h2("ch3-widget", "Ten sam efekt, dwa różne werdykty"),
    lc_p("Widget pokazuje dwa zbiory danych z TYM SAMYM prawdziwym nachyleniem β₁ = 1.5 i tą samą próbą obserwacji X. Różni je tylko poziom szumu — czyli to, jak głośno ε zagłusza systematyczną relację. Zobacz, jak zmienia się wniosek statystyczny mimo identycznego efektu."),

    figure_panel(
      label = "Ryc. 4.3",
      title = "Identyczne β₁, dwie różne historie",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          sliderInput("ch3_n", "Liczba obserwacji", min = 20, max = 200, value = 50, step = 10),
          sliderInput("ch3_sigma_low", "Szum: scenariusz A", min = 1, max = 30, value = 4, step = 1),
          sliderInput("ch3_sigma_high", "Szum: scenariusz B", min = 1, max = 30, value = 18, step = 1)
        ),
        column(
          8,
          plotOutput("ch3_plot", height = "380px"),
          uiOutput("ch3_stats"),
          uiOutput("ch3_verdict")
        )
      )
    ),

    inline_callout(
      label = "Zapamiętaj",
      color = "uwaga",
      open = TRUE,
      "p-wartość zależy od trzech rzeczy: rozmiaru efektu, wielkości próby i szumu. Mały efekt + duża próba = istotne. Duży efekt + mała próba = nieistotne. Patrz na całość, nie tylko na p."
    ),

    lc_chapter_next(
      num = "04",
      title = "Diagnoza krok po kroku",
      lead = "ćwiczenie",
      target_id = "ch-cwiczenie"
    )
  )
)

ch3_server <- function(input, output, session) {
  ch3_dataA <- reactive({
    eco_regression_data(n = input$ch3_n, beta0 = 10, beta1 = 1.5,
                        sigma = input$ch3_sigma_low, seed = 4001)
  })
  ch3_dataB <- reactive({
    eco_regression_data(n = input$ch3_n, beta0 = 10, beta1 = 1.5,
                        sigma = input$ch3_sigma_high, seed = 4001)
  })

  ch3_fitA <- reactive(lm(y ~ x, data = ch3_dataA()))
  ch3_fitB <- reactive(lm(y ~ x, data = ch3_dataB()))

  output$ch3_plot <- renderPlot({
    pA <- ggplot(ch3_dataA(), aes(x, y)) +
      geom_point(color = unname(upwr_cat["grafit"]), alpha = 0.65, size = 2) +
      geom_smooth(method = "lm", se = TRUE, color = upwr_accent,
                  fill = upwr_seq_burgundy[3], alpha = 0.18) +
      labs(x = "X", y = "Y", title = "Scenariusz A: niski szum") +
      theme_upwr()

    pB <- ggplot(ch3_dataB(), aes(x, y)) +
      geom_point(color = unname(upwr_cat["grafit"]), alpha = 0.65, size = 2) +
      geom_smooth(method = "lm", se = TRUE, color = unname(upwr_cat["terakota"]),
                  fill = upwr_seq_burgundy[3], alpha = 0.18) +
      labs(x = "X", y = "Y", title = "Scenariusz B: wysoki szum") +
      theme_upwr()

    if (requireNamespace("patchwork", quietly = TRUE)) pA + pB else pA
  })

  output$ch3_stats <- renderUI({
    cA <- broom::tidy(ch3_fitA())
    cB <- broom::tidy(ch3_fitB())
    rowA <- cA[cA$term == "x", ]
    rowB <- cB[cB$term == "x", ]
    lc_stat_grid(
      lc_stat_box("A: b₁",  eco_fmt(rowA$estimate, 2),  color = unname(upwr_cat["szalwia"])),
      lc_stat_box("A: SE",  eco_fmt(rowA$std.error, 2), color = unname(upwr_cat["niebo"])),
      lc_stat_box("A: p",   eco_fmt(rowA$p.value, 4),   color = upwr_accent),
      lc_stat_box("B: b₁",  eco_fmt(rowB$estimate, 2),  color = unname(upwr_cat["szalwia"])),
      lc_stat_box("B: SE",  eco_fmt(rowB$std.error, 2), color = unname(upwr_cat["niebo"])),
      lc_stat_box("B: p",   eco_fmt(rowB$p.value, 4),   color = unname(upwr_cat["terakota"])),
      columns = 3
    )
  })

  output$ch3_verdict <- renderUI({
    cA <- broom::tidy(ch3_fitA())
    cB <- broom::tidy(ch3_fitB())
    rowA <- cA[cA$term == "x", ]
    rowB <- cB[cB$term == "x", ]

    okA <- rowA$p.value < 0.05
    okB <- rowB$p.value < 0.05

    msgA <- if (okA) {
      paste0("Scenariusz A: efekt istotny — b₁ = ", eco_fmt(rowA$estimate, 2),
             ", p = ", eco_fmt(rowA$p.value, 4),
             ". Niski szum pozwala wyraźnie zobaczyć systematyczną relację.")
    } else {
      paste0("Scenariusz A: mimo niskiego szumu efekt nieistotny — b₁ = ",
             eco_fmt(rowA$estimate, 2), ", p = ", eco_fmt(rowA$p.value, 4),
             ". Próba zbyt mała, by potwierdzić zależność.")
    }
    msgB <- if (okB) {
      paste0(" Scenariusz B: mimo wysokiego szumu efekt też istotny — b₁ = ",
             eco_fmt(rowB$estimate, 2), ", p = ", eco_fmt(rowB$p.value, 4),
             ". Próba wystarczająco duża, by przebić się przez szum.")
    } else {
      paste0(" Scenariusz B: efekt taki sam, ale niepewny — b₁ = ",
             eco_fmt(rowB$estimate, 2), ", p = ", eco_fmt(rowB$p.value, 4),
             " — dane nie pozwalają potwierdzić wpływu mimo realnej zależności.")
    }

    lc_feedback(paste0(msgA, msgB), type = "info")
  })
}
