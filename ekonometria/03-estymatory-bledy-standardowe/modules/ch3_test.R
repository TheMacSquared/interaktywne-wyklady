# ============================================================================
# ROZDZIAŁ 3: Test t i przedział ufności
# ============================================================================

ch3_ui <- lecture_chapter(
  id = "ch-test",
  num = "03",
  title = "Test t i przedział ufności",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 03 · Estymacja",
      num = "03",
      title = "Od błędu standardowego do wniosku.",
      lead = "Skoro b₁ ma rozkład, to mamy też narzędzie: liczymy, jak daleko b₁ leży od ‚nudnej’ wartości β₁ = 0, w jednostkach SE. To jest statystyka t. A z niej — przedział ufności."
    ),

    lc_h2("ch3-formula-t", "Statystyka t"),
    lc_formula_box(
      withMathJax(helpText("$$t = \\frac{\\hat\\beta_1 - \\beta_{1,0}}{SE(\\hat\\beta_1)}$$")),
      p("Hipoteza zerowa to typowo β₁,₀ = 0 — czyli „X nie wpływa na Y”. Statystyka t mówi, o ile odchyleń standardowych nasza estymata b₁ leży od tej nudnej wartości. Im większe |t|, tym mocniejsza przesłanka, że X faktycznie ma znaczenie."),
      p("Pod hipotezą zerową t ma rozkład t-Studenta z df = n − k stopniami swobody (k to liczba szacowanych parametrów; w prostej regresji k = 2).")
    ),

    lc_h2("ch3-przedzial", "Przedział ufności"),
    lc_formula_box(
      withMathJax(helpText("$$\\hat\\beta_1 \\pm t_{\\alpha/2,\\,df} \\cdot SE(\\hat\\beta_1)$$")),
      p("Z prawdopodobieństwem 1 − α prawdziwe β₁ mieści się w tym przedziale. Wartość t_{α/2, df} to ‚tabelaryczna’ liczba zależna od poziomu ufności i liczby stopni swobody — np. dla 95% i dużej próby ≈ 1.96.")
    ),

    lc_h2("ch3-widget", "Jak poziom ufności zmienia szerokość przedziału?"),
    lc_p("Poniżej rozkład t-Studenta dla wybranej liczby stopni swobody. Zacieniowany obszar pokazuje centralną część rozkładu odpowiadającą wybranemu poziomowi ufności. Pionowe linie zaznaczają granice przedziału — punkty t_{α/2, df}, które używamy we wzorze."),
    figure_panel(
      label = "Ryc. 3.1",
      title = "Rozkład t i obszar ufności",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          sliderInput("ch3_level", "Poziom ufności (%)",
                      min = 80, max = 99, value = 95, step = 1),
          sliderInput("ch3_n", "Wielkość próby",
                      min = 20, max = 200, value = 60, step = 10)
        ),
        column(
          8,
          plotOutput("ch3_plot", height = "360px"),
          uiOutput("ch3_verdict")
        )
      )
    ),

    inline_callout(
      label = "Wskazówka",
      color = "wskazowka",
      open = TRUE,
      "Wzrost poziomu ufności (np. z 90% na 99%) NIE jest manipulacją — to zwykły wybór, jak ostrożni chcemy być. Wyższy poziom = szerszy przedział = mniejsze ryzyko, że pomyliliśmy się co do prawdziwej wartości. Niższy poziom = węższy przedział = wygodniejsza interpretacja, ale częściej myli się o pojedynczych próbach."
    ),

    lc_chapter_next(
      num = "04",
      title = "Czytanie tabeli wyników",
      lead = "ćwiczenie z interpretacji",
      target_id = "ch-cwiczenie"
    )
  )
)

ch3_server <- function(input, output, session) {
  ch3_t_data <- reactive({
    df <- input$ch3_n - 2
    alpha <- 1 - input$ch3_level / 100
    crit <- qt(1 - alpha / 2, df = df)
    grid <- seq(-4, 4, length.out = 400)
    list(
      df    = df,
      alpha = alpha,
      crit  = crit,
      data  = data.frame(t = grid, d = dt(grid, df = df))
    )
  })

  output$ch3_plot <- renderPlot({
    td <- ch3_t_data()
    shaded <- td$data |> dplyr::filter(t >= -td$crit & t <= td$crit)

    ggplot(td$data, aes(t, d)) +
      geom_area(data = shaded, aes(t, d),
                fill = unname(upwr_cat["niebo"]), alpha = 0.45) +
      geom_line(color = unname(upwr_cat["grafit"]), linewidth = 1) +
      geom_vline(xintercept = c(-td$crit, td$crit),
                 color = upwr_accent, linewidth = 1, linetype = "dashed") +
      annotate("text", x = 0, y = max(td$data$d) * 0.5,
               label = paste0(input$ch3_level, "%"),
               color = unname(upwr_cat["grafit"]), size = 6, fontface = "bold") +
      labs(x = "wartość statystyki t",
           y = "gęstość",
           title = paste0("Rozkład t-Studenta, df = ", td$df,
                          ", t krytyczne = ±", eco_fmt(td$crit, 3))) +
      theme_upwr()
  })

  output$ch3_verdict <- renderUI({
    td <- ch3_t_data()
    # Przykładowe b1 i SE — pokazane jako konkret w werdykcie
    b1 <- 1.50
    se <- 0.20
    lwr <- b1 - td$crit * se
    upr <- b1 + td$crit * se
    lc_feedback(
      type = "info",
      tags$p(
        "Dla próby n = ", strong(input$ch3_n),
        " (df = ", td$df, ") i poziomu ufności ", strong(paste0(input$ch3_level, "%")),
        " wartość krytyczna t to ", strong(eco_fmt(td$crit, 3)), "."
      ),
      tags$p(
        "Gdyby z danych wyszło b₁ = 1.50 z SE(b₁) = 0.20, to ",
        strong(paste0(input$ch3_level, "% przedział ufności")),
        " wynosiłby [", strong(eco_fmt(lwr, 2)), ", ", strong(eco_fmt(upr, 2)), "]. ",
        "Z prawdopodobieństwem ", input$ch3_level, "% prawdziwe nachylenie leży w tym przedziale. ",
        "Im wyższy poziom ufności, tym szerszy przedział — to cena ostrożności."
      )
    )
  })
}
