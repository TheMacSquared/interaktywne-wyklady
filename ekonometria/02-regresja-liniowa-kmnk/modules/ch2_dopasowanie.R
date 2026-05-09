# ============================================================================
# ROZDZIAŁ 2: Dopasowanie KMNK
# ============================================================================

ch2_ui <- lecture_chapter(
  id = "ch-dopasowanie",
  num = "02",
  title = "Dopasowanie KMNK",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 02 · KMNK",
      num = "02",
      title = "Dopasowanie metodą najmniejszych kwadratów.",
      lead = "Mając dane, jak wybrać konkretną prostą? KMNK mówi: tak, żeby suma kwadratów odchyleń od prostej była najmniejsza. Brzmi technicznie — ale intuicja jest prosta."
    ),

    lc_h2("ch2-idea", "Idea metody"),
    lc_p("Wyobraź sobie wykres rozrzutu: na osi poziomej X (np. nakłady reklamowe), na pionowej Y (sprzedaż). Punkty układają się w chmurę — z grubsza wznoszącą się, ale nieidealną. Możemy przez tę chmurę narysować dowolnie wiele prostych. Która jest ‚najlepsza‘?"),
    lc_p("Klasyczna metoda najmniejszych kwadratów (KMNK, ang. OLS) odpowiada konkretnie: dla każdej kandydującej prostej liczymy ", strong("reszty"), " — pionowe odległości od punktów do prostej. Te reszty podnosimy do kwadratu i sumujemy. Wybieramy tę prostą, dla której ta suma jest najmniejsza. Stąd nazwa."),
    lc_formula_box(
      withMathJax(helpText("$$\\min_{b_0,\\, b_1} \\sum_{i=1}^{n} \\bigl(Y_i - b_0 - b_1 X_i\\bigr)^2$$")),
      p("Pod znakiem sumy: ", withMathJax("\\(Y_i - b_0 - b_1 X_i\\)"), " to reszta dla obserwacji ", withMathJax("\\(i\\)"), " — różnica między rzeczywistą wartością Y a tym, co przewiduje prosta."),
      p("Litery ", withMathJax("\\(b_0, b_1\\)"), " (zamiast ", withMathJax("\\(\\beta_0, \\beta_1\\)"), ") oznaczają ", strong("oszacowania"), " parametrów liczone z konkretnej próby. Prawdziwe ", withMathJax("\\(\\beta\\)"), " są nieznane — szukamy ich.")
    ),

    lc_h2("ch2-dlaczego-kwadraty", "Dlaczego kwadraty, a nie wartości bezwzględne?"),
    tags$ol(
      tags$li(strong("Kara rośnie szybciej dla dużych pomyłek."), " Dwie reszty po 5 dają w sumie kwadratów 50; jedna reszta 10 daje 100. KMNK woli rozkładać błąd równomiernie, zamiast dopuszczać duże wpadki."),
      tags$li(strong("Jednoznaczne rozwiązanie matematyczne."), " Suma kwadratów jest funkcją gładką — pochodne się ładnie zerują, dostajemy wzory zamknięte na b₀ i b₁. Wartość bezwzględna ma w zerze ‚kant‘ i wymaga numerycznych tricków."),
      tags$li(strong("Kompatybilność z normalnością."), " Przy normalnym składniku losowym KMNK pokrywa się z metodą największej wiarygodności. To nie przypadek — kwadrat odpowiada logarytmowi gęstości normalnej.")
    ),

    lc_h2("ch2-widget", "Zobacz, jak działa KMNK"),
    lc_p("Poniżej symulator. Manipuluj liczebnością próby, prawdziwym nachyleniem relacji i wielkością szumu. Aplikacja każdorazowo dopasowuje prostą KMNK i pokazuje współczynniki. Zaznacz ‚Pokaż reszty‘, żeby zobaczyć, co dokładnie minimalizuje metoda."),
    figure_panel(
      label = "Ryc. 2.1",
      title = "Regresja prosta — symulator",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          sliderInput("ch2_n", "Liczba obserwacji", min = 20, max = 200, value = 80, step = 10),
          sliderInput("ch2_beta1", "Prawdziwe nachylenie β₁", min = -2, max = 4, value = 1.5, step = 0.25),
          sliderInput("ch2_sigma", "Odchylenie składnika losowego σ", min = 1, max = 25, value = 7, step = 1),
          checkboxInput("ch2_resid", "Pokaż reszty (pionowe odcinki)", value = FALSE)
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
      "KMNK jest wrażliwy na obserwacje odstające. Jeden punkt daleko od chmury — np. miesiąc z nietypową promocją — może wyraźnie przekrzywić prostą, bo jego reszta podniesiona do kwadratu dominuje w sumie. Zawsze warto rzucić okiem na wykres rozrzutu przed ufaniem współczynnikom."
    ),

    inline_callout(
      label = "Zapamiętaj",
      color = "wskazowka",
      "R² mierzy, jaką część zmienności Y wyjaśnia model. R² = 0 oznacza, że X w ogóle nie pomaga; R² = 1 — że model trafia idealnie. Wartości pomiędzy interpretujemy procentowo: R² = 0,72 to ‚72% zmienności sprzedaży tłumaczą wydatki na reklamę‘."
    ),

    lc_chapter_next(
      num = "03",
      title = "Założenia KMNK",
      lead = "warunki, których wymaga klasyczna metoda",
      target_id = "ch-zalozenia"
    )
  )
)

ch2_server <- function(input, output, session) {
  ch2_df <- reactive({
    eco_regression_data(
      n     = input$ch2_n,
      beta0 = 18,
      beta1 = input$ch2_beta1,
      sigma = input$ch2_sigma,
      seed  = 22
    )
  })

  ch2_fit <- reactive(lm(y ~ x, data = ch2_df()))

  output$ch2_plot <- renderPlot({
    d <- ch2_df()
    d$fitted <- fitted(ch2_fit())
    p <- ggplot(d, aes(x, y)) +
      geom_point(color = unname(upwr_cat["grafit"]), alpha = 0.65, size = 2) +
      geom_smooth(method = "lm", se = TRUE, color = upwr_accent,
                  fill = upwr_seq_burgundy[3], alpha = 0.18)
    if (isTRUE(input$ch2_resid)) {
      p <- p + geom_segment(aes(xend = x, yend = fitted),
                            color = unname(upwr_cat["terakota"]), alpha = 0.45)
    }
    p +
      labs(x = "X (jednostki umowne)", y = "Y (jednostki umowne)") +
      theme_upwr()
  })

  output$ch2_stats <- renderUI({
    g <- broom::glance(ch2_fit())
    b <- broom::tidy(ch2_fit())
    lc_stat_grid(
      lc_stat_box("b₀", eco_fmt(b$estimate[1], 2), caption = "wyraz wolny",
                  color = upwr_secondary),
      lc_stat_box("b₁", eco_fmt(b$estimate[2], 3), caption = "nachylenie",
                  color = unname(upwr_cat["szalwia"])),
      lc_stat_box("R²", eco_fmt(g$r.squared, 3), caption = "udział wyjaśnionej zmienności",
                  color = unname(upwr_cat["niebo"])),
      lc_stat_box("SE_e", eco_fmt(g$sigma, 2), caption = "typowy błąd reszty",
                  color = unname(upwr_cat["terakota"])),
      columns = 4
    )
  })

  output$ch2_verdict <- renderUI({
    g <- broom::glance(ch2_fit())
    b <- broom::tidy(ch2_fit())
    b0 <- b$estimate[1]
    b1 <- b$estimate[2]
    r2 <- g$r.squared
    kierunek <- if (b1 > 0) "rośnie" else if (b1 < 0) "spada" else "nie zmienia się"
    sila <- if (r2 >= 0.7) "silne" else if (r2 >= 0.4) "umiarkowane" else if (r2 >= 0.15) "słabe" else "bardzo słabe"
    lc_feedback(
      type = "info",
      "Z tej próby KMNK dopasowała prostą ", strong(paste0("ŷ = ", eco_fmt(b0, 2), " + ", eco_fmt(b1, 3), " · x")), ". ",
      "Gdy X rośnie o jednostkę, Y średnio ", kierunek, " o około ", strong(eco_fmt(abs(b1), 3)), " jednostki. ",
      "Zmienna X wyjaśnia ", strong(paste0(eco_fmt(100 * r2, 1), "%")), " zmienności Y — to dopasowanie ", strong(sila), "."
    )
  })
}
