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

    lc_h2("ch5-intro", "Rozkład normalny N(μ, σ) — królowa rozkładów"),

    lc_p("W poprzednim rozdziale poznaliśmy kilka rozkładów ciągłych
      i nauczyliśmy się czytać prawdopodobieństwo jako pole pod krzywą gęstości.
      Jeden kształt pojawił się już wcześniej wiele razy. ", gloss("histogram", "Histogram"), " wzrostu
      z ankiety w wykładzie 01 był w przybliżeniu symetrycznym dzwonem,
      a reguła 68–95–99.7 sprawdzała się na nim bardzo dobrze. Teraz nadamy
      temu kształtowi wzór."),

    lc_p(gloss("rozkład normalny", "Rozkład normalny"), " (rozkład Gaussa) to
      rozkład ciągły o symetrycznej, dzwonowej ", gloss("funkcja gęstości", "gęstości"),
      ". Wyznaczają go dwa ", gloss("parametr", "parametry"), ": ", gloss("średnia"), " \\(\\mu\\), która ustala położenie
      środka krzywej, i ", gloss("odchylenie standardowe"), " \\(\\sigma\\), które
      ustala jej szerokość. Zapisujemy to krótko \\(X \\sim N(\\mu, \\sigma)\\)."),

    lc_formula_annotated(
      "f(x) = \\frac{1}{⟦1|\\sigma⟧\\sqrt{2\\pi}} \\, e^{-\\frac{(x-⟦2|\\mu⟧)^2}{2⟦1|\\sigma⟧^2}} \\qquad E(X) = ⟦2|\\mu⟧ \\qquad Var(X) = ⟦1|\\sigma⟧^2",
      notes  = list("odchylenie standardowe", "średnia"),
      colors = c("wrzos", "accent"),
      above  = 2
    ),

    lc_p("Parametry mają tu bezpośrednią interpretację. ",
      gloss("wartość oczekiwana", "Wartość oczekiwana"), " rozkładu jest równa
      \\(\\mu\\), a ", gloss("wariancja"), " jest równa \\(\\sigma^2\\), więc
      \\(\\sigma\\) to zwykłe SD. Część podręczników pisze \\(N(\\mu, \\sigma^2)\\),
      z wariancją zamiast SD, dlatego przy każdym zapisie warto sprawdzić,
      o który parametr chodzi."),

    lc_p("Dlaczego akurat ten rozkład jest tak ważny, wyjaśni następny rozdział
      o centralnym twierdzeniu granicznym. Najpierw zobaczmy, jak parametry
      zmieniają kształt krzywej."),

    lc_h2("ch5-dzwon", "Skąd się bierze dzwon?"),

    lc_p("Zanim zobaczymy, jak parametry zmieniają krzywą, sprawdźmy, skąd się
      ten kształt bierze. Masa ciała zależy od wielu drobnych, niezależnych
      wpływów: genów, odżywiania, stylu życia, wieku. Każdy z nich dodaje
      do wyniku coś niewielkiego, a suma takich wpływów układa się w dzwon.
      Panel waży kolejne losowo wybrane osoby."),

    figure_panel(
      label = "Ryc. 5.1",
      width_mode = "text",
      exp_widget("ch5_waga", "Ważenie ludzi: od jednej osoby do rozkładu normalnego", "Zważ osobę",
        list(kind = "weight", mu = 70, sd = 12, lo = 30, hi = 110, width = 5,
             sub = "masa ciała tej osoby", xTitle = "X, czyli masa ciała (kg)",
             aria = "Ważenie losowych osób, masa ciała i histogram powtórzeń"))
    ),

    # ========================================================================
    # WIDGET 1: Dwa parametry, nieskończone możliwości
    # ========================================================================
    lc_h2("ch5-parametry", "Dwa parametry — nieskończone możliwości"),

    lc_p("Panel rysuje gęstość N(μ, σ) dla wybranych parametrów i zaznacza pasy
      μ ± σ, μ ± 2σ i μ ± 3σ. Przyciski ustawiają trzy przykłady z życia:
      wzrost kobiet N(166, 6), iloraz inteligencji N(100, 15) i temperaturę
      ciała N(36.6, 0.4)."),

    figure_panel(
      label = "Ryc. 5.2",
      title = "Eksploracja N(μ, σ)",
      full_width = TRUE,
      lc_toolbar(
        lc_slider("ch5_mu", "μ (średnia)", -10, 10, 0, 0.5),
        lc_slider("ch5_sigma", "σ (odch. std.)", 0.5, 5, 1, 0.1),
        lc_action("ch5_preset_std", "N(0, 1)", variant = "outline"),
        lc_action("ch5_preset_wzrost_k", "Wzrost kobiet", variant = "outline"),
        lc_action("ch5_preset_iq", "IQ", variant = "outline"),
        lc_action("ch5_preset_temp", "Temp. ciała", variant = "outline"),
        checkboxInput("ch5_show_empirical", "Reguła 68–95–99.7", value = TRUE),
        lc_readouts(uiOutput("ch5_explore_stats"))
      ),
      lc_plot("ch5_explore_plot", max_height = "400px")
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
      ", gloss("wykres pudełkowy", "wykresy pudełkowe"), " wzrostu kobiet i mężczyzn się nie nakładały, a ", gloss("mediana", "mediany"), "
      wynosiły 166.4 i 177.1 cm. Panel rysuje dwa modele zbliżone do tych danych:
      N(166, 6) dla kobiet i N(178, 7) dla mężczyzn. Uchwyt na szczycie krzywej
      przesuwa ją w bok (zmienia μ) albo w górę i w dół (zmienia σ)."),

    figure_panel(
      label = "Ryc. 5.3",
      title = "Dwie krzywe normalne",
      full_width = TRUE,
      tags$div(class = "lc-normals", `data-mu1` = "166", `data-sd1` = "6", `data-mu2` = "178", `data-sd2` = "7",
        `data-lo` = "130", `data-hi` = "215",
        lc_toolbar(
          tags$button(type = "button", class = "lc-action is-outline", `data-n-reset` = "1", tags$span("Od nowa")),
          lc_readouts(
            lc_readout("Kobiety", tags$span(`data-role` = "n1", "N(166.0, 6.0)"), color = "var(--upwr-cat-niebo)", swatch = TRUE),
            lc_readout("Mężczyźni", tags$span(`data-role` = "n2", "N(178.0, 7.0)"), color = "var(--upwr-cat-terakota)", swatch = TRUE),
            lc_readout("K > μ mężczyzn", tags$span(`data-role` = "a", "2.3%")),
            lc_readout("M < μ kobiet", tags$span(`data-role` = "b", "4.3%"))
          )
        ),
        tags$div(class = "lc-area-canvas",
          tags$canvas(`data-role` = "pdf", `aria-label` = "Gęstości wzrostu kobiet N(166, 6) i mężczyzn N(178, 7)"))
      )
    ),

    lc_p("Krzywe wyraźnie się nakładają, choć środkowe połowy grup są rozdzielone.
      Na wysokości 172 cm, w połowie między średnimi, model daje 15.9% kobiet
      wyższych od tej wartości i 19.6% mężczyzn niższych od niej. Kobiet wyższych
      niż średni mężczyzna (178 cm) jest 2.3%, a mężczyzn niższych niż średnia
      kobieta (166 cm) 4.3%. Szersza krzywa mężczyzn ma niższy szczyt (0.057
      wobec 0.066), bo pole pod każdą krzywą wynosi 1. Różnica średnich mówi,
      która grupa jest przeciętnie wyższa, ale o tym, jak często pojedyncze
      osoby z obu grup się mijają, decyduje także σ."),

    # ========================================================================
    # WIDGET 3: Standaryzacja (z-score)
    # ========================================================================
    lc_h2("ch5-standaryzacja", "Standaryzacja (z-score)"),

    lc_p("Porównanie dwóch rozkładów rodzi pytanie: jak zestawić wartości
      mierzone na różnych skalach? Wzrost 180 cm to u kobiety coś innego niż
      u mężczyzny. Odpowiedzią jest ", gloss("standaryzacja"), ". Od wartości
      odejmujemy średnią i dzielimy wynik przez odchylenie standardowe:"),

    lc_formula_annotated(
      "⟦1|z⟧ = \\frac{⟦2|x⟧ - ⟦3|\\mu⟧}{⟦4|\\sigma⟧} \\qquad X \\sim N(\\mu, \\sigma) \\;\\Rightarrow\\; Z = \\frac{X - \\mu}{\\sigma} \\sim N(0, 1)",
      notes  = list("wynik standaryzowany", "wartość", "średnia", "odchylenie standardowe"),
      colors = c("indygo", "niebo", "accent", "wrzos"),
      above  = c(2, 3)
    ),

    lc_p("Wynik z (z-score) mówi, o ile odchyleń standardowych wartość leży
      od średniej; znak mówi, po której stronie. Jeśli X ma rozkład normalny,
      to Z ma ", gloss("standardowy rozkład normalny", "standardowy rozkład normalny"),
      " N(0, 1), czyli rozkład o średniej 0 i SD 1. Kobieta o wzroście 180 cm
      ma z = (180 - 166)/6 = 2.33, a mężczyzna o tym samym wzroście
      z = (180 - 178)/7 = 0.29. Ta sama liczba centymetrów oznacza w pierwszej
      grupie wartość rzadką, a w drugiej przeciętną."),

    lc_p("Panel standaryzuje jedną wartość. Górny wykres pokazuje ją na
      oryginalnej skali, dolny na skali z. Domyślnie jest to wynik egzaminu
      80 punktów przy średniej 65 i SD 10. Wartość x można też przeciągnąć
      po górnym wykresie."),

    # Ryc. 5.4: prototyp 5.4-A (zaakceptowany 2026-10-29; Chart.js, zscore_chart.js)
    s3p_54a_ui,

    lc_p("Wynik 80 punktów daje z = (80 - 65)/10 = 1.5: półtora odchylenia
      standardowego powyżej średniej. Oba wykresy mają identyczny kształt,
      a pionowa linia stoi w tym samym miejscu krzywej. Standaryzacja nie
      zmienia rozkładu, tylko opisuje oś w jednostkach σ, licząc od μ."),

    lc_p("Stąd bierze się uzasadnienie reguły 68–95–99.7. Pas μ ± σ na
      dowolnej skali to po standaryzacji zawsze przedział od -1 do 1, pas
      μ ± 2σ to przedział od -2 do 2. Każdy rozkład normalny przechodzi
      w ten sam rozkład N(0, 1), więc odsetki w pasach muszą być wszędzie
      takie same. Wystarczy je raz policzyć dla N(0, 1)."),

    # Ryc. 5.5: prototyp 5c-C (zaakceptowany 2026-10-29)
    lc_p("Standaryzacja pozwala porównywać wartości mierzone na różnych skalach.
      Panel zestawia cztery takie pary. Przełącznik pokazuje je albo w ich
      własnych skalach, albo na wspólnej skali z."),

    s3p_5cc_ui,

    lc_p("Kobieta o wzroście 180 cm ma z = (180 − 166)/6 = 2.33, a mężczyzna
      o wzroście 190 cm z = (190 − 178)/7 = 1.71: mężczyzna jest wyższy
      o 10 cm, ale to kobieta jest rzadsza w swojej grupie. Wynik 90 punktów
      na egzaminie A (μ = 60, σ = 15) daje z = 2.00, a 680 punktów w teście B
      (μ = 500, σ = 100) z = 1.80, więc lepszy jest wynik z egzaminu A.
      Temperatura 37.4 °C i iloraz inteligencji 130 mają to samo z = 2,
      więc są tak samo rzadkie: wyższą wartość ma po 2.3% populacji. Ten sam
      koszykarz o wzroście 213 cm ma z = 5.00 wśród wszystkich mężczyzn, ale
      tylko z = 1.44 wśród zawodników NBA (parametry NBA są orientacyjne)."),

    # ========================================================================
    # WIDGET 4: Obliczanie prawdopodobieństw
    # ========================================================================
    lc_h2("ch5-prawdop", "Obliczanie prawdopodobieństw"),

    lc_p("Prawdopodobieństwo przedziału to pole pod gęstością, ale dla
      rozkładu normalnego pola tego nie da się zapisać prostym wzorem. Liczy
      się je numerycznie, a podstawą jest ", gloss("dystrybuanta"), " F(z) = P(Z ≤ z),
      czyli pole na lewo od z. Pozostałe pytania wynikają z tego,
      że całe pole wynosi 1:"),

    lc_formula_box(withMathJax(
      "$$P(Z > a) = 1 - P(Z \\le a) \\qquad P(a < Z < b) = P(Z \\le b) - P(Z \\le a)$$"
    )),

    lc_p("Panel zaznacza szukane pole pod krzywą N(0, 1) i podaje wynik."),

    figure_panel(
      label = "Ryc. 5.6",
      title = "Kalkulator prawdopodobieństw N(0, 1)",
      full_width = TRUE,
      lc_toolbar(
        lc_segmented("ch5_prob_type", "Typ pytania", choices = c(
            "P(Z < a)" = "less",
            "P(Z > a)" = "greater",
            "P(a < Z < b)" = "between"
          ), selected = "between"),
        lc_slider("ch5_prob_a", "a", -4, 4, -1, 0.05),
        conditionalPanel(
          condition = "input.ch5_prob_type == 'between'",
          lc_slider("ch5_prob_b", "b", -4, 4, 1, 0.05)
        ),
        lc_readouts(uiOutput("ch5_prob_result"))
      ),
      lc_plot("ch5_prob_plot", max_height = "300px")
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

    lc_p("Rozkład normalny pracuje w wielu dziedzinach. Galeria zestawia sześć
      przypadków z różnych kierunków studiów; próg można przesuwać po wykresie."),

    # Ryc. 5.7: prototyp 5a (zaakceptowany 2026-10-29; Chart.js, cases_chart.js)
    s3p_5a_ui,

    lc_p("Nie każda zmienna ma jednak rozkład normalny. Galeria porównuje zmienne
      z ankiety z krzywą normalną o tej samej średniej i tym samym odchyleniu
      standardowym."),

    # Ryc. 5.8: prototyp 5d (zaakceptowany 2026-10-29)
    s3p_5d_ui,

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

  # --- Ryc. 5.1: ważenie osób → rozkład normalny (experiment.js) ---
  X <- exp_X
  exp_texts(input, output, "ch5_waga", list(
    tagList("Losujemy osobę z dużej grupy dorosłych i ważymy ją. Waga pokazuje masę ciała
      w kilogramach, ale przed ważeniem nie wiemy, ile to będzie. Zważ kilka osób."),
    tagList(X(), " to masa ciała wylosowanej osoby w kilogramach. Może wynieść dowolną wartość
      z przedziału, nie tylko całkowitą, więc to zmienna losowa ciągła."),
    tagList("Ważymy kolejne osoby i zliczamy, ile mas trafia do przedziałów po 5 kg.
      Dokładaj po 10, 100 i 1000 osób."),
    tagList("Częstości w przedziałach układają się w dzwon. Krzywa to rozkład normalny
      N(μ, σ) z μ = 70 kg i σ = 12 kg. Taki kształt pojawia się, gdy cecha jest sumą wielu
      drobnych, niezależnych wpływów. Dlaczego tak się dzieje, wyjaśni następny rozdział.")
  ))

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
    tagList(
      lc_readout("μ", mu, color = col_normal),
      lc_readout("σ", sigma, color = upwr_secondary),
      lc_readout("68%", paste0("[", round(mu - sigma, 1), ", ",
                                round(mu + sigma, 1), "]"), color = unname(upwr_cat["bursztyn"]))
    )
  })

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

    tagList(
      lc_readout(label, sprintf("%.4f", prob), color = col_normal),
      lc_readout("Procent", paste0(sprintf("%.2f", prob * 100), "%"), color = upwr_secondary)
    )
  })

}
