# ============================================================================
# ROZDZIAŁ 2: Miary dopasowania
# ============================================================================

ch2_ui <- lecture_chapter(
  id = "ch-dopasowanie",
  num = "02",
  title = "Miary dopasowania",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 04 · Weryfikacja",
      num = "02",
      title = "Jak dobrze model opisuje dane?",
      lead = "Po weryfikacji merytorycznej patrzymy, jak dobrze model opisuje DANE. Trzy podstawowe miary: R², SE reszt i wykres reszt — i zawsze patrzymy na obraz, nie tylko na liczby."
    ),

    lc_h2("ch2-r2", "R² — ile zmienności wyjaśniliśmy?"),
    lc_formula_box(
      withMathJax(helpText("$$R^2 = 1 - \\frac{\\sum (y_i - \\hat y_i)^2}{\\sum (y_i - \\bar y)^2}$$")),
      p("Zakres [0, 1]: 0 = model nic nie wyjaśnia, 1 = idealne dopasowanie.")
    ),
    lc_p("R² mierzy, jaką część wahań Y wytłumaczyliśmy zmienną X. Reszta to to, co poszło do ε. R² = 0.65 znaczy: 65% zmienności sprzedaży wyjaśnia nasz model, 35% pozostaje poza nim — losowe zaburzenia, pominięte czynniki, błędy pomiaru."),

    lc_h2("ch2-se", "SE reszt — przeciętna pomyłka modelu"),
    lc_formula_box(
      withMathJax(helpText("$$SE_e = \\sqrt{\\frac{\\sum (y_i - \\hat y_i)^2}{n - k}}$$")),
      p("Wyrażony w jednostkach Y. Mówi, o ile średnio model myli się in plus albo in minus.")
    ),
    lc_p("Jeśli Y to sprzedaż w tys. zł i SE_e = 5.2, to znaczy: typowa pomyłka modelu na pojedynczej obserwacji to około ±5.2 tys. zł. Łatwiej zinterpretować niż R², bo jest w naturalnych jednostkach."),

    lc_h2("ch2-zgodnosc", "Współczynnik zgodności"),
    lc_p("φ = SE_e / ȳ daje błąd w skali względnej. Pozwala porównywać modele opisujące różne wielkości. φ = 0.08 = przeciętna pomyłka stanowi 8% średniej wartości Y."),

    lc_h2("ch2-widget", "Diagnoza wzrokowa: cztery typowe scenariusze"),
    lc_p("Te same liczby (R², SE) mogą towarzyszyć BARDZO różnym sytuacjom. Dlatego zawsze patrzymy na wykres reszt vs wartości dopasowane — to tam widać problemy, których same statystyki nie pokażą."),

    figure_panel(
      label = "Ryc. 4.2",
      title = "Cztery typowe obrazy reszt",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          selectInput("ch2_kind", "Scenariusz danych:", c(
            "Poprawny obraz" = "ok",
            "Heteroskedastyczność" = "hetero",
            "Nieliniowość" = "nonlinear",
            "Obserwacje odstające" = "outliers"
          ))
        ),
        column(
          8,
          plotOutput("ch2_plot", height = "380px"),
          uiOutput("ch2_stats"),
          uiOutput("ch2_verdict")
        )
      )
    ),

    inline_callout(
      label = "Pułapka",
      color = "uwaga",
      open = TRUE,
      "Wysokie R² nie znaczy, że model jest dobry. Jeśli reszty mają wzorzec, R² zwodzi. Zawsze rysuj wykres reszt vs fitted."
    ),

    lc_chapter_next(
      num = "03",
      title = "Istotność statystyczna",
      lead = "test t dla nachylenia",
      target_id = "ch-istotnosc"
    )
  )
)

ch2_server <- function(input, output, session) {
  ch2_data <- reactive({
    eco_diagnostic_data(input$ch2_kind, seed = 44)
  })

  ch2_fit <- reactive(lm(y ~ x, data = ch2_data()))

  output$ch2_plot <- renderPlot({
    df <- ch2_data()
    df$fitted <- fitted(ch2_fit())
    df$resid  <- residuals(ch2_fit())

    p1 <- ggplot(df, aes(x, y)) +
      geom_point(color = unname(upwr_cat["grafit"]), alpha = 0.6, size = 2) +
      geom_smooth(method = "lm", se = FALSE, color = upwr_accent, linewidth = 1) +
      labs(x = "X", y = "Y", title = "Dane i prosta regresji") +
      theme_upwr()

    p2 <- ggplot(df, aes(fitted, resid)) +
      geom_hline(yintercept = 0, color = upwr_reference, linetype = "dashed") +
      geom_point(color = unname(upwr_cat["terakota"]), alpha = 0.65, size = 2) +
      geom_smooth(se = FALSE, color = unname(upwr_cat["niebo"]), linewidth = 1) +
      labs(x = "Wartości dopasowane", y = "Reszty", title = "Reszty vs dopasowane") +
      theme_upwr()

    if (requireNamespace("patchwork", quietly = TRUE)) p1 + p2 else p2
  })

  output$ch2_stats <- renderUI({
    g <- broom::glance(ch2_fit())
    phi <- g$sigma / mean(ch2_data()$y)
    lc_stat_grid(
      lc_stat_box("R²", eco_fmt(g$r.squared, 3),
                  caption = "część zmienności Y wyjaśniona modelem",
                  color = unname(upwr_cat["niebo"])),
      lc_stat_box("SE reszt", eco_fmt(g$sigma, 2),
                  caption = "przeciętna pomyłka w jednostkach Y",
                  color = unname(upwr_cat["terakota"])),
      lc_stat_box("p dla modelu", eco_fmt(g$p.value, 4),
                  caption = "test F dla całego modelu",
                  color = upwr_accent),
      columns = 3
    )
  })

  output$ch2_verdict <- renderUI({
    g <- broom::glance(ch2_fit())
    r2 <- eco_fmt(g$r.squared, 2)
    se <- eco_fmt(g$sigma, 1)

    msg <- switch(input$ch2_kind,
      ok = paste0("Reszty wyglądają losowo, brak systematycznego wzorca. Model dobrze opisuje dane — R² = ", r2,
                  ", przeciętna pomyłka ±", se, " jednostek Y."),
      hetero = paste0("Reszty rozszerzają się wraz z X — wariancja błędu zależy od X (heteroskedastyczność). R² = ",
                      r2, " wciąż wysoki, ale SE współczynników są zaniżone, p-wartości zawodne. Trzeba użyć błędów odpornych."),
      nonlinear = paste0("W resztach widać systematyczny wzorzec łukowy — to znak, że prawdziwa relacja nie jest liniowa. ",
                         "R² = ", r2, " sugeruje przyzwoite dopasowanie, ale to złudzenie. Trzeba dodać X² lub przekształcić zmienną."),
      outliers = paste0("Kilka punktów znacznie odstaje od reszty. Mogą zawyżać/zaniżać szacunki — sprawdź, ",
                        "czy to błędy danych czy realne obserwacje. SE reszt (= ", se, ") jest zawyżony przez te wartości.")
    )

    type <- switch(input$ch2_kind,
      ok = "ok",
      hetero = "warning",
      nonlinear = "warning",
      outliers = "warning"
    )

    lc_feedback(msg, type = type)
  })
}
