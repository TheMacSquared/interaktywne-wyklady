# ============================================================================
# CHAPTER 5: Rozkład normalny
# ============================================================================

ch5_ui <- list(
  id = "ch-normalny", num = "05", title = "Rozkład normalny",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 05 · Rozkłady prawdopodobieństwa",
      num    = "05",
      title  = "Rozkład normalny.",
      lead   = "Wzrost, wyniki testów, błędy pomiarowe: wiele zmiennych ma rozkład
                w kształcie dzwonu. Opisuje go jeden wzór z dwoma parametrami,
                a każde pytanie o prawdopodobieństwo da się sprowadzić do jednego
                rozkładu wzorcowego N(0, 1)."
    ),

    lc_h2("ch5-intro", "Rozkład normalny — królowa rozkładów"),

    lc_p("W poprzednim rozdziale poznaliśmy kilka rozkładów ciągłych
      i nauczyliśmy się czytać prawdopodobieństwo jako pole pod krzywą gęstości.
      Jeden kształt pojawił się już wcześniej wiele razy. Histogram wzrostu
      z ankiety w wykładzie 01 był w przybliżeniu symetrycznym dzwonem,
      a reguła 68–95–99.7 sprawdzała się na nim bardzo dobrze. Teraz nadamy
      temu kształtowi wzór."),

    lc_p(gloss("rozkład normalny", "Rozkład normalny"), " (rozkład Gaussa) to
      rozkład ciągły o symetrycznej, dzwonowej ", gloss("funkcja gęstości", "gęstości"),
      ". Wyznaczają go dwa parametry: średnia \\(\\mu\\), która ustala położenie
      środka krzywej, i ", gloss("odchylenie standardowe"), " \\(\\sigma\\), które
      ustala jej szerokość. Zapisujemy to krótko \\(X \\sim N(\\mu, \\sigma)\\)."),

    lc_formula_box(withMathJax(
      "$$f(x) = \\frac{1}{\\sigma\\sqrt{2\\pi}} \\, e^{-\\frac{(x-\\mu)^2}{2\\sigma^2}} \\qquad E(X) = \\mu \\qquad Var(X) = \\sigma^2$$"
    )),

    lc_p("Parametry mają tu bezpośrednią interpretację. ",
      gloss("wartość oczekiwana", "Wartość oczekiwana"), " rozkładu jest równa
      \\(\\mu\\), a ", gloss("wariancja"), " jest równa \\(\\sigma^2\\), więc
      \\(\\sigma\\) to zwykłe SD. Część podręczników pisze \\(N(\\mu, \\sigma^2)\\),
      z wariancją zamiast SD, dlatego przy każdym zapisie warto sprawdzić,
      o który parametr chodzi."),

    lc_p("Dlaczego akurat ten rozkład jest tak ważny, wyjaśni następny rozdział
      o centralnym twierdzeniu granicznym. Najpierw zobaczmy, jak parametry
      zmieniają kształt krzywej."),

    # ========================================================================
    # WIDGET 1: Dwa parametry, nieskończone możliwości
    # ========================================================================
    lc_h2("ch5-parametry", "Dwa parametry — nieskończone możliwości"),

    lc_p("Panel rysuje gęstość N(μ, σ) dla wybranych parametrów i zaznacza pasy
      μ ± σ, μ ± 2σ i μ ± 3σ. Przyciski ustawiają trzy przykłady z życia:
      wzrost kobiet N(166, 6), iloraz inteligencji N(100, 15) i temperaturę
      ciała N(36.6, 0.4)."),

    figure_panel(
      label = "Ryc. 5.1",
      title = "Eksploracja N(μ, σ)",
      full_width = TRUE,
      fluidRow(
        column(4,
          lc_slider("ch5_mu", "μ (średnia)", -10, 10, 0, 0.5),
          lc_slider("ch5_sigma", "σ (odch. std.)", 0.5, 5, 1, 0.1),
          hr(),
          div(class = "preset-buttons",
            lc_action("ch5_preset_std", "N(0, 1)\nStandardowy", variant = "outline"),
            lc_action("ch5_preset_wzrost_k", "Wzrost\nkobiet", variant = "solid"),
            lc_action("ch5_preset_iq", "IQ", variant = "solid"),
            lc_action("ch5_preset_temp", "Temp.\nciała", variant = "solid")
          ),
          hr(),
          checkboxInput("ch5_show_empirical", "Pokaż regułę 68–95–99.7", value = TRUE)
        ),
        column(8,
          zoom_plot_ui("ch5_explore_plot", height = "400px"),
          uiOutput("ch5_explore_stats")
        )
      )
    ),

    lc_p("Zmiana μ przesuwa krzywą wzdłuż osi, nie zmieniając jej kształtu.
      Zmiana σ rozciąga ją albo ściska. Ponieważ całe pole pod gęstością zawsze
      wynosi 1, szersza krzywa musi być niższa: szczyt N(0, 1) ma wysokość
      0.399, a szczyt N(0, 2) tylko 0.199. Dla wzrostu kobiet pas μ ± σ to
      160–172 cm, dla IQ 85–115 punktów, a dla temperatury ciała 36.2–37.0 °C."),

    lc_p("Udział pola w każdym z pasów jest jednak zawsze taki sam, niezależnie
      od μ i σ. W pasie μ ± σ leży 68.27% prawdopodobieństwa, w pasie μ ± 2σ —
      95.45%, a w pasie μ ± 3σ — 99.73%. To jest ",
      gloss("reguła 68-95-99.7", "reguła 68–95–99.7"), " w wersji teoretycznej.
      W wykładzie 01 sprawdzaliśmy ją empirycznie na wzroście studentów
      i otrzymaliśmy 67.5%, 96.5% i 100%. Wtedy była to obserwacja o danych,
      teraz jest to własność rozkładu normalnego. Dane z ankiety zgadzały się
      z nią, bo rozkład wzrostu jest bliski normalnemu. Dlaczego udziały nie
      zależą od parametrów, pokaże sekcja o standaryzacji."),

    # ========================================================================
    # WIDGET 2: Porównanie rozkładów
    # ========================================================================
    lc_h2("ch5-porownanie", "Porównanie dwóch rozkładów normalnych"),

    lc_p("Dwa parametry wystarczają też do porównania grup. W wykładzie 01
      wykresy pudełkowe wzrostu kobiet i mężczyzn się nie nakładały, a mediany
      wynosiły 166.4 i 177.1 cm. Panel rysuje dwie krzywe normalne naraz;
      przycisk ustawia modele zbliżone do tych danych: N(166, 6) dla kobiet
      i N(178, 7) dla mężczyzn."),

    figure_panel(
      label = "Ryc. 5.2",
      title = "Dwie krzywe normalne",
      full_width = TRUE,
      fluidRow(
        column(3,
          h5("Rozkład A", style = "color: var(--upwr-cat-niebo);"),
          lc_slider("ch5_cmp_mu1", "μ₁", -5, 15, 5, 0.5),
          lc_slider("ch5_cmp_s1", "σ₁", 0.5, 5, 1.5, 0.1)
        ),
        column(3,
          h5("Rozkład B", style = "color: var(--upwr-accent);"),
          lc_slider("ch5_cmp_mu2", "μ₂", -5, 15, 8, 0.5),
          lc_slider("ch5_cmp_s2", "σ₂", 0.5, 5, 2, 0.1),
          hr(),
          lc_action("ch5_cmp_preset", "Mężczyźni vs\nkobiety (wzrost)", variant = "outline")
        ),
        column(6,
          zoom_plot_ui("ch5_compare_plot", height = "350px")
        )
      )
    ),

    lc_p("Przy ustawieniach startowych rozkład B, N(8, 2), leży na prawo od A,
      N(5, 1.5), i jest szerszy, więc jego szczyt jest niższy (0.199 wobec
      0.266). Dla wzrostu krzywe wyraźnie się nakładają, choć środkowe
      połowy grup są rozdzielone. Na wysokości 172 cm, w połowie między
      średnimi, model daje 15.9% kobiet wyższych od tej wartości i 19.6%
      mężczyzn niższych od niej. Kobiet wyższych niż średni mężczyzna
      (178 cm) jest 2.3%, a mężczyzn niższych niż średnia kobieta (166 cm)
      4.3%. Różnica średnich mówi, która grupa jest przeciętnie wyższa,
      ale o tym, jak często pojedyncze osoby z obu grup się mijają, decyduje
      także σ."),

    # ========================================================================
    # WIDGET 3: Standaryzacja (z-score)
    # ========================================================================
    lc_h2("ch5-standaryzacja", "Standaryzacja (z-score)"),

    lc_p("Porównanie dwóch rozkładów rodzi pytanie: jak zestawić wartości
      mierzone na różnych skalach? Wzrost 180 cm to u kobiety coś innego niż
      u mężczyzny. Odpowiedzią jest ", gloss("standaryzacja"), ". Od wartości
      odejmujemy średnią i dzielimy wynik przez odchylenie standardowe:"),

    lc_formula_box(withMathJax(
      "$$z = \\frac{x - \\mu}{\\sigma} \\qquad X \\sim N(\\mu, \\sigma) \\;\\Rightarrow\\; Z = \\frac{X - \\mu}{\\sigma} \\sim N(0, 1)$$"
    )),

    lc_p("Wynik z (z-score) mówi, o ile odchyleń standardowych wartość leży
      od średniej; znak mówi, po której stronie. Jeśli X ma rozkład normalny,
      to Z ma ", gloss("standardowy rozkład normalny", "standardowy rozkład normalny"),
      " N(0, 1), czyli rozkład o średniej 0 i SD 1. Kobieta o wzroście 180 cm
      ma z = (180 - 166)/6 = 2.33, a mężczyzna o tym samym wzroście
      z = (180 - 178)/7 = 0.29. Ta sama liczba centymetrów oznacza w pierwszej
      grupie wartość rzadką, a w drugiej przeciętną."),

    lc_p("Panel standaryzuje jedną wartość. Górny wykres pokazuje ją na
      oryginalnej skali, dolny na skali z. Domyślnie jest to wynik egzaminu
      80 punktów przy średniej 65 i SD 10."),

    figure_panel(
      label = "Ryc. 5.3",
      title = "Kalkulator z-score",
      full_width = TRUE,
      fluidRow(
        column(4,
          numericInput("ch5_z_mu", "μ (np. średnia egzaminu):", value = 65),
          numericInput("ch5_z_sigma", "σ (np. odch. std.):", value = 10, min = 0.1),
          numericInput("ch5_z_x", "x (wartość do standaryzacji):", value = 80),
          hr(),
          uiOutput("ch5_z_result")
        ),
        column(8,
          zoom_plot_ui("ch5_z_plot", height = "350px")
        )
      )
    ),

    lc_p("Wynik 80 punktów daje z = (80 - 65)/10 = 1.5: półtora odchylenia
      standardowego powyżej średniej. Oba wykresy mają identyczny kształt,
      a pionowa linia stoi w tym samym miejscu krzywej. Standaryzacja nie
      zmienia rozkładu, tylko opisuje oś w jednostkach σ, licząc od μ."),

    lc_p("Stąd bierze się uzasadnienie reguły 68–95–99.7. Pas μ ± σ na
      dowolnej skali to po standaryzacji zawsze przedział od -1 do 1, pas
      μ ± 2σ to przedział od -2 do 2. Każdy rozkład normalny przechodzi
      w ten sam rozkład N(0, 1), więc odsetki w pasach muszą być wszędzie
      takie same. Wystarczy je raz policzyć dla N(0, 1)."),

    # ========================================================================
    # WIDGET 4: Obliczanie prawdopodobieństw
    # ========================================================================
    lc_h2("ch5-prawdop", "Obliczanie prawdopodobieństw"),

    lc_p("Prawdopodobieństwo przedziału to pole pod gęstością, ale dla
      rozkładu normalnego pola tego nie da się zapisać prostym wzorem. Liczy
      się je numerycznie, a podstawą jest dystrybuanta F(z) = P(Z ≤ z),
      czyli pole na lewo od z. Pozostałe pytania wynikają z tego,
      że całe pole wynosi 1:"),

    lc_formula_box(withMathJax(
      "$$P(Z > a) = 1 - P(Z \\le a) \\qquad P(a < Z < b) = P(Z \\le b) - P(Z \\le a)$$"
    )),

    lc_p("Panel zaznacza szukane pole pod krzywą N(0, 1) i podaje wynik."),

    figure_panel(
      label = "Ryc. 5.4",
      title = "Kalkulator prawdopodobieństw N(0, 1)",
      full_width = TRUE,
      fluidRow(
        column(4,
          lc_segmented("ch5_prob_type", "Typ pytania", choices = c(
              "P(Z < a)" = "less",
              "P(Z > a)" = "greater",
              "P(a < Z < b)" = "between"
            ), selected = "between"),
          lc_slider("ch5_prob_a", "a", -4, 4, -1, 0.05),
          conditionalPanel(
            condition = "input.ch5_prob_type == 'between'",
            lc_slider("ch5_prob_b", "b", -4, 4, 1, 0.05)
          )
        ),
        column(8,
          zoom_plot_ui("ch5_prob_plot", height = "300px"),
          uiOutput("ch5_prob_result")
        )
      )
    ),

    lc_p("Ustawienie startowe, P(-1 < Z < 1) = 0.6827, to pierwsza liczba
      reguły 68–95–99.7. Wróćmy do egzaminu. Wynik powyżej 80 punktów
      odpowiada z > 1.5, więc P(X > 80) = 1 − P(Z ≤ 1.5) = 0.0668: taki wynik
      osiąga około 6.7% zdających. Podobnie IQ powyżej 130 punktów (z = 2) ma 2.3% populacji."),

    lc_p("Często pytanie jest odwrotne: znamy prawdopodobieństwo i szukamy
      wartości. Odpowiedzią jest ", gloss("percentyl"), " rozkładu, czyli wartość
      pozostawiającą na lewo pole p. Na przykład percentyl rzędu 0.975
      rozkładu N(0, 1) wynosi 1.96, więc środkowe 95% rozkładu normalnego leży dokładnie w pasie
      μ ± 1.96σ; reguła 68–95–99.7 zaokrągla to do 2σ. W modelu wzrostu
      kobiet N(166, 6) percentyl rzędu 0.9 wynosi 173.7 cm,
      więc 10% kobiet jest wyższych. Wartość 1.96 wróci w kolejnych
      wykładach przy przedziałach ufności."),

    lc_chapter_next(
      num       = "06",
      title     = "Centralne Twierdzenie Graniczne",
      lead      = "dlaczego rozkład normalny pojawia się wszędzie w naturze.",
      target_id = "ch-ctg"
    )
  )
)


# --------------------------------------------------------------------------
# Chapter 5 Server
# --------------------------------------------------------------------------

ch5_server <- function(input, output, session) {

  # --- Presety ---
  observeEvent(input$ch5_preset_std, {
    updateSliderInput(session, "ch5_mu", value = 0)
    updateSliderInput(session, "ch5_sigma", value = 1)
  })
  observeEvent(input$ch5_preset_wzrost_k, {
    updateSliderInput(session, "ch5_mu", value = 166, min = 140, max = 200)
    updateSliderInput(session, "ch5_sigma", value = 6, min = 1, max = 15)
  })
  observeEvent(input$ch5_preset_iq, {
    updateSliderInput(session, "ch5_mu", value = 100, min = 50, max = 150)
    updateSliderInput(session, "ch5_sigma", value = 15, min = 1, max = 30)
  })
  observeEvent(input$ch5_preset_temp, {
    updateSliderInput(session, "ch5_mu", value = 36.6, min = 34, max = 40)
    updateSliderInput(session, "ch5_sigma", value = 0.4, min = 0.1, max = 2)
  })

  # --- Widget 1: Eksploracja ---
  zoom_plot_server("ch5_explore_plot", reactive({
    mu <- input$ch5_mu
    sigma <- input$ch5_sigma
    show_emp <- input$ch5_show_empirical

    x_seq <- seq(mu - 4*sigma, mu + 4*sigma, length.out = 500)
    df <- data.frame(x = x_seq, y = dnorm(x_seq, mu, sigma))

    p <- ggplot(df, aes(x = x, y = y)) +
      geom_line(color = col_normal, linewidth = 1.5)

    if (show_emp) {
      # 1 SD
      shade1 <- data.frame(
        x = seq(mu - sigma, mu + sigma, length.out = 200),
        y = dnorm(seq(mu - sigma, mu + sigma, length.out = 200), mu, sigma)
      )
      p <- p + geom_area(data = shade1, aes(x = x, y = y),
                         fill = col_normal, alpha = 0.4)

      # 2 SD
      shade2l <- data.frame(
        x = seq(mu - 2*sigma, mu - sigma, length.out = 100),
        y = dnorm(seq(mu - 2*sigma, mu - sigma, length.out = 100), mu, sigma)
      )
      shade2r <- data.frame(
        x = seq(mu + sigma, mu + 2*sigma, length.out = 100),
        y = dnorm(seq(mu + sigma, mu + 2*sigma, length.out = 100), mu, sigma)
      )
      p <- p +
        geom_area(data = shade2l, aes(x = x, y = y), fill = col_normal, alpha = 0.25) +
        geom_area(data = shade2r, aes(x = x, y = y), fill = col_normal, alpha = 0.25)

      # 3 SD
      shade3l <- data.frame(
        x = seq(mu - 3*sigma, mu - 2*sigma, length.out = 100),
        y = dnorm(seq(mu - 3*sigma, mu - 2*sigma, length.out = 100), mu, sigma)
      )
      shade3r <- data.frame(
        x = seq(mu + 2*sigma, mu + 3*sigma, length.out = 100),
        y = dnorm(seq(mu + 2*sigma, mu + 3*sigma, length.out = 100), mu, sigma)
      )
      p <- p +
        geom_area(data = shade3l, aes(x = x, y = y), fill = col_normal, alpha = 0.12) +
        geom_area(data = shade3r, aes(x = x, y = y), fill = col_normal, alpha = 0.12)

      # Etykiety
      y_top <- dnorm(mu, mu, sigma)
      p <- p +
        annotate("text", x = mu, y = y_top * 0.6, label = "68%",
                 size = 5, fontface = "bold", color = upwr_secondary) +
        annotate("text", x = mu, y = y_top * 0.35, label = "95%",
                 size = 4.5, color = upwr_secondary) +
        annotate("text", x = mu, y = y_top * 0.15, label = "99.7%",
                 size = 4, color = upwr_reference)
    }

    p + geom_vline(xintercept = mu, color = upwr_secondary, linetype = "dashed") +
      labs(
           x = "x", y = "f(x)") +
      theme_upwr()
  }))

  output$ch5_explore_stats <- renderUI({
    mu <- input$ch5_mu
    sigma <- input$ch5_sigma
    lc_center(
      lc_stat_box("μ", mu, color = col_normal),
      lc_stat_box("σ", sigma, color = upwr_secondary),
      lc_stat_box("68%", paste0("[", round(mu - sigma, 1), ", ",
                                round(mu + sigma, 1), "]"),
                  color = unname(upwr_cat["bursztyn"]))
    )
  })

  # --- Widget 2: Porównanie ---
  observeEvent(input$ch5_cmp_preset, {
    updateSliderInput(session, "ch5_cmp_mu1", value = 166, min = 140, max = 200)
    updateSliderInput(session, "ch5_cmp_s1", value = 6, min = 1, max = 15)
    updateSliderInput(session, "ch5_cmp_mu2", value = 178, min = 140, max = 200)
    updateSliderInput(session, "ch5_cmp_s2", value = 7, min = 1, max = 15)
  })

  zoom_plot_server("ch5_compare_plot", reactive({
    mu1 <- input$ch5_cmp_mu1; s1 <- input$ch5_cmp_s1
    mu2 <- input$ch5_cmp_mu2; s2 <- input$ch5_cmp_s2

    x_min <- min(mu1 - 4*s1, mu2 - 4*s2)
    x_max <- max(mu1 + 4*s1, mu2 + 4*s2)
    x_seq <- seq(x_min, x_max, length.out = 500)

    df <- data.frame(
      x = rep(x_seq, 2),
      y = c(dnorm(x_seq, mu1, s1), dnorm(x_seq, mu2, s2)),
      group = rep(c("A", "B"), each = 500)
    )

    ggplot(df, aes(x = x, y = y, color = group, fill = group)) +
      geom_line(linewidth = 1.2) +
      geom_area(alpha = 0.15, position = "identity") +
      scale_color_manual(values = c("A" = unname(upwr_cat["niebo"]), "B" = unname(upwr_cat["terakota"])),
                         labels = c(paste0("A: N(", mu1, ", ", s1, ")"),
                                    paste0("B: N(", mu2, ", ", s2, ")")),
                         name = "") +
      scale_fill_manual(values = c("A" = unname(upwr_cat["niebo"]), "B" = unname(upwr_cat["terakota"])),
                        guide = "none") +
      labs(
           x = "x", y = "f(x)") +
      theme_upwr() +
      theme(legend.position = "top")
  }))

  # --- Widget 3: Z-score ---
  output$ch5_z_result <- renderUI({
    mu <- input$ch5_z_mu
    sigma <- input$ch5_z_sigma
    x <- input$ch5_z_x
    req(sigma > 0)

    z <- (x - mu) / sigma

    div(
      lc_stat_box("z", round(z, 2),
                  caption = paste0("(", x, " − ", mu, ") / ", sigma),
                  color = col_normal),
      lc_feedback(type = "info", style = "margin-top: 8px;",
        paste0("Wartość ", x, " leży ", round(abs(z), 2),
               " odchyleń standardowych ",
               if (z >= 0) "powyżej" else "poniżej", " średniej."))
    )
  })

  zoom_plot_server("ch5_z_plot", reactive({
    mu <- input$ch5_z_mu
    sigma <- input$ch5_z_sigma
    x <- input$ch5_z_x
    req(sigma > 0)

    z <- (x - mu) / sigma

    # Górny wykres: oryginalna skala
    x_seq <- seq(mu - 4*sigma, mu + 4*sigma, length.out = 500)
    df_orig <- data.frame(x = x_seq, y = dnorm(x_seq, mu, sigma))

    # Dolny wykres: standaryzowana skala
    z_seq <- seq(-4, 4, length.out = 500)
    df_std <- data.frame(x = z_seq, y = dnorm(z_seq))

    p1 <- ggplot(df_orig, aes(x = x, y = y)) +
      geom_line(color = unname(upwr_cat["niebo"]), linewidth = 1.2) +
      geom_vline(xintercept = x, color = unname(upwr_cat["terakota"]), linewidth = 1.2) +
      annotate("point", x = x, y = dnorm(x, mu, sigma),
               color = unname(upwr_cat["terakota"]), size = 4) +
      annotate("text", x = x, y = dnorm(x, mu, sigma) * 1.2,
               label = paste0("x = ", x), color = unname(upwr_cat["terakota"]),
               size = 4, fontface = "bold", vjust = -0.5) +
      labs(
           x = "x", y = "f(x)") +
      theme_upwr(base_size = 12)

    p2 <- ggplot(df_std, aes(x = x, y = y)) +
      geom_line(color = col_normal, linewidth = 1.2) +
      geom_vline(xintercept = z, color = unname(upwr_cat["terakota"]), linewidth = 1.2) +
      annotate("point", x = z, y = dnorm(z), color = unname(upwr_cat["terakota"]), size = 4) +
      annotate("text", x = z, y = dnorm(z) * 1.2,
               label = paste0("z = ", round(z, 2)), color = unname(upwr_cat["terakota"]),
               size = 4, fontface = "bold", vjust = -0.5) +
      labs(
           x = "z", y = "f(z)") +
      theme_upwr(base_size = 12)

    gridExtra::arrangeGrob(p1, p2, ncol = 1)
  }))

  # --- Widget 4: Kalkulator prawdopodobieństw ---
  zoom_plot_server("ch5_prob_plot", reactive({
    type <- input$ch5_prob_type
    a <- input$ch5_prob_a
    b <- if (type == "between") input$ch5_prob_b else NULL

    x_seq <- seq(-4, 4, length.out = 500)
    df <- data.frame(x = x_seq, y = dnorm(x_seq))

    p <- ggplot(df, aes(x = x, y = y)) +
      geom_line(color = upwr_secondary, linewidth = 1.2)

    if (type == "less") {
      shade <- data.frame(x = x_seq[x_seq <= a], y = dnorm(x_seq[x_seq <= a]))
      prob <- pnorm(a)
      p <- p + geom_area(data = shade, fill = unname(upwr_cat["niebo"]), alpha = 0.4) +
        geom_vline(xintercept = a, color = unname(upwr_cat["terakota"]), linetype = "dashed")
    } else if (type == "greater") {
      shade <- data.frame(x = x_seq[x_seq >= a], y = dnorm(x_seq[x_seq >= a]))
      prob <- 1 - pnorm(a)
      p <- p + geom_area(data = shade, fill = unname(upwr_cat["terakota"]), alpha = 0.4) +
        geom_vline(xintercept = a, color = unname(upwr_cat["terakota"]), linetype = "dashed")
    } else {
      shade <- data.frame(x = x_seq[x_seq >= a & x_seq <= b],
                          y = dnorm(x_seq[x_seq >= a & x_seq <= b]))
      prob <- pnorm(b) - pnorm(a)
      p <- p + geom_area(data = shade, fill = col_normal, alpha = 0.4) +
        geom_vline(xintercept = a, color = unname(upwr_cat["terakota"]), linetype = "dashed") +
        geom_vline(xintercept = b, color = unname(upwr_cat["terakota"]), linetype = "dashed")
    }

    p + annotate("text", x = 0, y = 0.2,
                 label = sprintf("P = %.4f", prob),
                 size = 6, fontface = "bold", color = upwr_secondary) +
      labs( x = "z", y = "f(z)") +
      theme_upwr()
  }))

  output$ch5_prob_result <- renderUI({
    type <- input$ch5_prob_type
    a <- input$ch5_prob_a
    b <- if (type == "between") input$ch5_prob_b else NULL

    prob <- switch(type,
      "less" = pnorm(a),
      "greater" = 1 - pnorm(a),
      "between" = pnorm(b) - pnorm(a)
    )

    label <- switch(type,
      "less" = paste0("P(Z < ", a, ")"),
      "greater" = paste0("P(Z > ", a, ")"),
      "between" = paste0("P(", a, " < Z < ", b, ")")
    )

    lc_center(
      lc_stat_box(label, sprintf("%.4f", prob), color = col_normal),
      lc_stat_box("Procent", sprintf("%.2f", prob * 100), "%",
                  color = upwr_secondary)
    )
  })

}
