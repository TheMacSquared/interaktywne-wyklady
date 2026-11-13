# ============================================================================
# CHAPTER 5: Rozkład normalny
# ============================================================================

# Sześć pytań i odpowiadające im rozkłady ciągłe z rozdziałów 4 i 5
# (Ryc. 5.15). Ten sam układ co mapa dyskretnych (Ryc. 3.13), ale miniatury
# to gładkie krzywe gęstości, każda w swojej skali.
ch5_continuous_map <- function() {
  items <- list(
    list(q = "Każda wartość z przedziału tak samo?", name = "jednostajny", par = "U(a, b)",
         x = seq(-0.25, 1.25, length.out = 300), f = function(x) dunif(x, 0, 1), col = "niebo"),
    list(q = "Ile czasu do zdarzenia?", name = "wykładniczy", par = "Exp(λ)",
         x = seq(0, 5, length.out = 300), f = function(x) dexp(x, 1), col = "szalwia"),
    list(q = "Suma wielu drobnych wpływów?", name = "normalny", par = "N(μ, σ)",
         x = seq(-3.5, 3.5, length.out = 300), f = function(x) dnorm(x), col = "terakota"),
    list(q = "Średnia z małej próby?", name = "t-Studenta", par = "t(df)",
         x = seq(-4.5, 4.5, length.out = 300), f = function(x) dt(x, 3), col = "indygo"),
    list(q = "Suma kwadratów?", name = "chi-kwadrat", par = "χ²(df)",
         x = seq(0, 14, length.out = 300), f = function(x) dchisq(x, 4), col = "wrzos"),
    list(q = "Dodatnie, z długim ogonem?", name = "log-normalny", par = "LogN(μ, σ)",
         x = seq(0, 5, length.out = 300), f = function(x) dlnorm(x, 0, 0.6), col = "bursztyn")
  )
  spark <- function(it) {
    y <- it$f(it$x)
    px <- 100 * (it$x - min(it$x)) / diff(range(it$x))
    py <- 42 - 38 * y / max(y)
    pts <- paste(sprintf("%.2f,%.2f", px, py), collapse = " ")
    tags$svg(class = "dmap-spark", viewBox = "0 0 100 44", preserveAspectRatio = "none",
      `aria-hidden` = "true",
      tags$polygon(class = "dmap-area", points = paste0("0,42 ", pts, " 100,42")),
      tags$polyline(class = "dmap-curve", points = pts),
      tags$line(x1 = 0, x2 = 100, y1 = 42.5, y2 = 42.5, class = "dmap-axis"))
  }
  tags$div(class = "dmap is-6",
    lapply(items, function(it) {
      tags$div(class = "dmap-card", style = paste0("--dm-c: var(--upwr-cat-", it$col, ");"),
        tags$div(class = "dmap-q", it$q),
        spark(it),
        tags$div(class = "dmap-name", it$name, tags$span(class = "dmap-par", it$par))
      )
    })
  )
}

# Scenariusze rozkładów opartych na normalnym (Ryc. 5.11, 5.12, 5.14)
ch5_t_defs <- list(
  t_1 = list(label = "t(df=1) — Cauchy", df = 1),
  t_2 = list(label = "t(df=3)", df = 3),
  t_3 = list(label = "t(df=5)", df = 5),
  t_4 = list(label = "t(df=30) ≈ normalny", df = 30)
)

ch5_chisq_defs <- list(
  chisq_1 = list(label = "χ²(df=2)", df = 2),
  chisq_2 = list(label = "χ²(df=5)", df = 5),
  chisq_3 = list(label = "χ²(df=10)", df = 10),
  chisq_4 = list(label = "χ²(df=20)", df = 20)
)

ch5_lnorm_defs <- list(
  lnorm_1 = list(label = "Czas reakcji: LogN(0, 0.3)", mu = 0, sigma = 0.3),
  lnorm_2 = list(label = "Ceny akcji: LogN(1, 0.5)", mu = 1, sigma = 0.5),
  lnorm_3 = list(label = "Dochody: LogN(2, 0.8)", mu = 2, sigma = 0.8),
  lnorm_4 = list(label = "Duża zmienność: LogN(1, 1)", mu = 1, sigma = 1)
)

# Podpisy pod rycinami scenariuszy (E(X), SD, mediana).
ch5_t_stat <- function(s) {
  mu_text <- if (s$df > 1) "E(X) = 0" else "E(X) = niezdef."
  sd_text <- if (s$df > 2) paste0("SD = ", round(sqrt(s$df / (s$df - 2)), 2)) else "SD = ∞"
  paste0(s$label, ":  ", mu_text, ",  ", sd_text)
}
ch5_chisq_stat <- function(s) {
  paste0(s$label, ":  E(X) = ", s$df, ",  SD = ", round(sqrt(2 * s$df), 2))
}
ch5_lnorm_stat <- function(s) {
  ev <- exp(s$mu + s$sigma^2 / 2)
  sd_val <- sqrt((exp(s$sigma^2) - 1) * exp(2 * s$mu + s$sigma^2))
  paste0(s$label, ":  E(X) = ", round(ev, 1), ",  Me = ", round(exp(s$mu), 1), ",  SD = ", round(sd_val, 1))
}


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
      lc_sf_figure("d03-normal", max_height = "400px",
        control = list(
          lc_sf_slider("ch5_mu", "μ (średnia)", -10, 10, 0, 0.5),
          lc_sf_slider("ch5_sigma", "σ (odch. std.)", 0.5, 5, 1, 0.1),
          checkboxInput("ch5_show_empirical", "Reguła 68–95–99.7", value = TRUE)),
        toolbar = tagList(
          lc_sf_action("std", "ch5_preset_std", "N(0, 1)", variant = "outline"),
          lc_sf_action("wzrost_k", "ch5_preset_wzrost_k", "Wzrost kobiet", variant = "outline"),
          lc_sf_action("iq", "ch5_preset_iq", "IQ", variant = "outline"),
          lc_sf_action("temp", "ch5_preset_temp", "Temp. ciała", variant = "outline"),
          lc_readouts(
            lc_readout("μ", lc_sf_value("mu", "0"), color = col_normal),
            lc_readout("σ", lc_sf_value("sigma", "1"), color = upwr_secondary),
            lc_readout("68%", lc_sf_value("i68", "[-1, 1]"), color = unname(upwr_cat["bursztyn"])))),
        # Presety: wartość, min, max i krok suwaków μ i σ.
        config = list(presets = list(
          std      = list(mu = c(-10, 10, 0, 0.5),    sigma = c(0.5, 5, 1, 0.1)),
          wzrost_k = list(mu = c(140, 200, 166, 1),   sigma = c(1, 15, 6, 0.5)),
          iq       = list(mu = c(50, 150, 100, 1),    sigma = c(1, 30, 15, 0.5)),
          temp     = list(mu = c(34, 40, 36.6, 0.1),  sigma = c(0.1, 2, 0.4, 0.1)))))
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
      przesuwa ją w bok (zmienia μ) albo w górę i w dół (zmienia σ).
      Można też wybrać szczyt Tabem i użyć strzałek; góra zwęża krzywą i podnosi szczyt."),

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
          ),
          lc_chart_zoom_button()
        ),
        tags$div(class = "lc-area-canvas",
          tags$div(class = "lc-ec", role = "img", `data-role` = "pdf", `aria-label` = "Gęstości wzrostu kobiet i mężczyzn; parametry w odczytach nad wykresem"))
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

    # Ryc. 5.4: kalkulator z-score (stops_ch5_calc.R, zscore_chart.js)
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

    # Ryc. 5.5: „Kto bardziej wyjątkowy?” (stops_ch5_z.R)
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
      lc_sf_figure("d03-zprob", max_height = "300px",
        control = list(
          lc_segmented("ch5_prob_type", "Typ pytania", choices = c(
              "P(Z < a)" = "less",
              "P(Z > a)" = "greater",
              "P(a < Z < b)" = "between"
            ), selected = "between"),
          lc_sf_slider("ch5_prob_a", "a", -4, 4, -1, 0.05),
          htmltools::tagAppendAttributes(lc_sf_slider("ch5_prob_b", "b", -4, 4, 1, 0.05), `data-role` = "b")),
        toolbar = lc_readouts(
          lc_readout(lc_sf_value("plabel", "P(-1 < Z < 1)"), lc_sf_value("p", "0.6827"), color = col_normal),
          lc_readout("Procent", lc_sf_value("pct", "68.27%"), color = upwr_secondary)))
    ),

    lc_p("Ustawienie startowe, P(-1 < Z < 1) = 0.6827, to pierwsza liczba
      reguły 68–95–99.7. Wróćmy do egzaminu. Wynik powyżej 80 punktów
      odpowiada z > 1.5, więc P(X > 80) = 1 − P(Z ≤ 1.5) = 0.0668: taki wynik
      osiąga około 6.7% zdających. Podobnie IQ powyżej 130 punktów (z = 2) ma 2.3% populacji."),

    s3p_e2_ui,

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

    # Ryc. 5.8: sześć kierunków (stops_ch5.R, cases_chart.js)
    s3p_5a_ui,

    lc_p("Nie każda zmienna ma jednak rozkład normalny. Galeria porównuje zmienne
      z ankiety z krzywą normalną o tej samej średniej i tym samym odchyleniu
      standardowym."),

    # Ryc. 5.9: „Normalny czy nie?” (stops_ch5.R)
    s3p_5d_ui,

    # ========================================================================
    # ROZKŁADY OPARTE NA ROZKŁADZIE NORMALNYM (przeniesione z rozdz. 4, 2026-11-06)
    # ========================================================================
    lc_h2("ch5-z-normalnego", "Rozkłady oparte na rozkładzie normalnym"),

    lc_p("Z rozkładu normalnego da się zbudować kolejne rozkłady. Wystarczy
      wziąć kilka niezależnych zmiennych N(0, 1) i coś z nimi zrobić:
      podnieść do kwadratu i zsumować, podzielić jedną przez drugą albo
      wziąć funkcję wykładniczą. Dwa z tych rozkładów, χ² i t-Studenta,
      opisują statystyki obliczane z próby i będą potrzebne przy przedziałach
      ufności i testach. Trzeci, log-normalny, opisuje zjawiska takie jak
      dochody czy ceny. Każdy najłatwiej zrozumieć przez to, jak powstaje
      z rozkładu normalnego, a nie przez wzór na gęstość."),

    lc_h3("Rozkład chi-kwadrat χ²(df)"),

    lc_p("Wiele wielkości mierzy, jak daleko coś leży od celu, i nigdy nie
      jest ujemnych. Dron dostawczy ustala pozycję z GPS, a GPS myli się
      trochę na każdej osi: na wschód i zachód, na północ i południe,
      a w powietrzu także w pionie. Przyjmijmy, że błąd na każdej osi ma
      rozkład normalny ze średnią 0 i odchyleniem standardowym 1 m. Dron
      najpierw ląduje z paczką na lądowisku, a potem zawisa przy balkonie,
      żeby ją podać."),

    figure_panel(
      label = "Ryc. 5.10",
      width_mode = "text",
      exp_widget("ch5_dron", "Dron z GPS: jak bardzo chybia?", "Wyślij drona",
        list(kind = "drone", n = 2, width = 0.5, xmax = 10,
             xTitle = "X, czyli kwadrat chybienia (m²)",
             aria = "Lądowania drona wokół celu, kwadrat chybienia i histogram powtórzeń"),
        options = list(label = "Gdzie", values = c("Lądowisko (2 osie)" = 2, "Balkon (3 osie)" = 3),
                       selected = 2))
    ),

    lc_p("Kwadrat chybienia to z twierdzenia Pitagorasa suma kwadratów błędów
      na poszczególnych osiach. Taka suma ma ",
      gloss("rozkład chi-kwadrat"), " χ²(df), gdzie df, czyli liczba stopni
      swobody, mówi, ile kwadratów sumujemy. Przy lądowaniu są dwie osie,
      więc χ²(2), przy balkonie trzy, więc χ²(3). Błąd tylko na jednej osi,
      na przykład odległość od linii, dałby χ²(1)."),

    lc_formula_annotated(
      "X = ⟦1|Z_1^2 + Z_2^2 + \\ldots + Z_{df}^2⟧, \\; ⟦2|Z_i⟧ \\sim N(0, 1)",
      notes  = list("suma kwadratów", "błąd na jednej osi"),
      colors = c("bursztyn", "niebo")
    ),

    lc_p("Każdy kwadrat Z² ma wartość oczekiwaną 1, więc średni kwadrat
      chybienia to po prostu liczba osi: 2 m² przy lądowaniu, 3 m² przy
      balkonie. Przy trzech osiach trafienie bardzo blisko celu jest rzadkie,
      bo trzeba mało chybić we wszystkich kierunkach naraz. Dlatego szczyt
      χ²(3) odsuwa się od zera, a χ²(2) maleje od samego zera."),

    lc_formula_annotated(
      "E(X) = ⟦1|df⟧, \\qquad Var(X) = 2 \\cdot df",
      notes  = list("liczba składników"),
      colors = c("szalwia")
    ),

    figure_panel(
      label = "Ryc. 5.11",
      title = "Rozkład χ²(df)",
      full_width = TRUE,
      d03_overlay("ch5_chisq_scenarios", ch5_chisq_defs, "chisq_2", ch5_chisq_stat, "chisq")
    ),

    lc_p("Panel pokazuje, co dzieje się przy większej liczbie składników. Dla
      χ²(5) wartość oczekiwana wynosi 5, SD = 3.16, a szczyt krzywej leży
      w punkcie 3. Wartości powyżej 11.07 pojawiają się tylko w 5% przypadków.
      Przy df = 2 krzywa
      opada od zera, a przy df = 20 jest już niemal symetryczna wokół 20.
      To nie przypadek: χ²(df) jest sumą df niezależnych składników, a suma
      wielu składników zbliża się do rozkładu normalnego. Dlaczego tak się
      dzieje, wyjaśni centralne twierdzenie graniczne w rozdziale 6."),

    lc_p("Intuicyjnie χ² mierzy, jak daleko dane leżą od stanu oczekiwanego.
      W ", gloss("test chi-kwadrat", "teście χ²"), " sumujemy kwadraty różnic
      między liczebnościami obserwowanymi a oczekiwanymi. Duża wartość tej
      sumy, leżąca w prawym ogonie rozkładu χ², wskazuje, że różnice są
      większe, niż wynikałoby z przypadku. Ten sam rozkład służy do
      wnioskowania o wariancji populacji."),

    lc_h3("Rozkład t-Studenta t(df)"),

    lc_p(gloss("rozkład t-Studenta", "Rozkład t-Studenta"), " t(df) pojawia się,
      gdy średnią z próby standaryzujemy, czyli odejmujemy od niej średnią
      populacji i dzielimy przez rozrzut, ale prawdziwego odchylenia
      standardowego populacji σ nie znamy i zastępujemy je odchyleniem
      standardowym z próby. Parametr df to ", gloss("stopnie swobody"), ".
      Przy średniej z n obserwacji df = n - 1."),

    lc_p("Konstrukcja mówi, skąd biorą się cięższe ogony. W liczniku stoi
      zmienna N(0, 1). W mianowniku zamiast prawdziwego rozrzutu, równego 1,
      stoi jego oszacowanie: pierwiastek z niezależnej zmiennej V ~ χ²(df) podzielonej przez df.
      Średnio wynosi ono około 1, ale się waha. Gdy przypadkiem wypadnie
      małe, iloraz robi się duży, dlatego wartości daleko od zera są
      częstsze niż w N(0, 1). Im więcej stopni swobody, tym oszacowanie
      dokładniejsze, mianownik bliższy 1 i rozkład bliższy N(0, 1)."),

    lc_formula_annotated(
      "T = \\frac{⟦1|Z⟧}{⟦2|\\sqrt{V / df}⟧}",
      notes  = list("zmienna N(0, 1)", "oszacowany rozrzut, średnio około 1"),
      colors = c("niebo", "bursztyn")
    ),

    lc_formula_box(withMathJax(
      "$$E(T) = 0 \\;\\text{ dla } df > 1, \\qquad Var(T) = \\frac{df}{df - 2} \\;\\text{ dla } df > 2$$"
    )),

    figure_panel(
      label = "Ryc. 5.12",
      title = "Rozkład t-Studenta t(df)",
      full_width = TRUE,
      d03_overlay("ch5_t_scenarios", ch5_t_defs, c("t_2", "t_4"), ch5_t_stat, "t",
        extra = checkboxInput("ch5_t_show_normal", "Pokaż N(0, 1) jako odniesienie", value = TRUE),
        normal = "ch5_t_show_normal")
    ),

    lc_p("Krzywa t(30) prawie pokrywa się z N(0, 1), a t(3) ma wyraźnie niższy
      szczyt i grubsze ogony. Różnicę widać w liczbach. Wartość dalej niż
      2 od zera ma w rozkładzie N(0, 1) prawdopodobieństwo 0.046, w t(30) —
      0.055, w t(5) — 0.102, a w t(3) już 0.139, czyli trzy razy więcej niż
      w rozkładzie normalnym. Odchylenie standardowe t(3) wynosi 1.73, a t(30) — 1.04. Skrajny
      przypadek t(1), zwany rozkładem Cauchy'ego, ma ogony tak ciężkie,
      że nie ma wartości oczekiwanej ani wariancji."),

    lc_p("Praktyczna konsekwencja: przy małej próbie, na przykład 4 obserwacjach
      (df = 3), granice, w których mieści się środkowe 95% rozkładu, leżą
      w ±3.18, a nie w ±1.96 jak dla N(0, 1). Wnioski z małych prób muszą
      więc być ostrożniejsze. Przy 31 obserwacjach (df = 30) granice to ±2.04
      i różnica staje się niewielka."),

    lc_h3("Rozkład log-normalny LogN(μ, σ)"),

    lc_p("Ostatni rozkład tej grupy opisuje zjawiska, nie statystyki. Wiele
      wielkości rośnie przez mnożenie, a nie dodawanie: pensja rośnie o kilka
      procent rocznie, cena akcji zmienia się o procent dziennie. Logarytm
      zamienia mnożenie w dodawanie, więc to logarytm takiej wielkości ma
      często rozkład normalny. Jeśli ln(X) ~ N(μ, σ), to X ma ",
      gloss("rozkład log-normalny"), " LogN(μ, σ). Zmienna jest zawsze dodatnia
      i ma prawy ogon. Parametry μ i σ to średnia i odchylenie standardowe
      logarytmu, a nie samej zmiennej X."),

    lc_formula_annotated(
      "X = e^{⟦1|Y⟧}, \\qquad Y \\sim N(⟦2|\\mu⟧, ⟦2|\\sigma⟧)",
      notes  = list("zmienna normalna", "parametry logarytmu, nie X"),
      colors = c("niebo", "accent")
    ),

    lc_p("Skąd bierze się taki ogon, widać na symulacji pensji."),

    # Ryc. 5.13: „Losowe podwyżki” (s3p_4f_ui, stops_ch4.R)
    s3p_4f_ui,

    lc_formula_annotated(
      "f(x) = \\frac{1}{⟦1|x⟧\\,\\sigma\\sqrt{2\\pi}} \\, e^{-\\frac{(⟦2|\\ln x⟧ - \\mu)^2}{2\\sigma^2}} \\;\\text{ dla } x > 0",
      notes  = list("czynnik z zamiany zmiennej", "logarytm zamiast x"),
      colors = c("terakota", "accent")
    ),
    lc_formula_annotated(
      "F(x) = ⟦1|\\Phi⟧\\left(⟦2|\\frac{\\ln x - \\mu}{\\sigma}⟧\\right) \\;\\text{ dla } x > 0",
      notes  = list("dystrybuanta N(0, 1)", "zestandaryzowany logarytm"),
      colors = c("niebo", "accent")
    ),

    lc_formula_annotated(
      "Me = e^{⟦1|\\mu⟧}, \\qquad E(X) = e^{⟦1|\\mu⟧ + ⟦2|\\sigma⟧^2/2}",
      notes  = list("średnia ln X", "odchylenie std. ln X"),
      colors = c("accent", "wrzos")
    ),
    lc_formula_box(withMathJax(
      "$$Var(X) = \\left(e^{\\sigma^2} - 1\\right) \\cdot e^{2\\mu + \\sigma^2}$$"
    )),

    figure_panel(
      label = "Ryc. 5.14",
      title = "Rozkład LogN(μ, σ)",
      full_width = TRUE,
      d03_overlay("ch5_lnorm_scenarios", ch5_lnorm_defs, "lnorm_2", ch5_lnorm_stat, "lnorm")
    ),

    lc_p("Prawy ogon sprawia, że wartość oczekiwana jest zawsze większa od mediany. Dla LogN(1, 0.5) mediana wynosi
      e¹ = 2.72, wartość oczekiwana 3.08, a SD = 1.64. Wartość oczekiwaną
      przekracza tylko 40% obserwacji. Im większe
      σ, tym dłuższy ogon i większa różnica. W scenariuszu dochodów
      LogN(2, 0.8) mediana to 7.39, a wartość oczekiwana 10.18, więc ponad
      średnią zarabia tylko 34% osób."),

    lc_p("To ten sam mechanizm, który w statystyce opisowej obserwowaliśmy na
      zarobkach w firmie: nieliczne bardzo duże wartości podnoszą średnią,
      a mediana zostaje przy typowej osobie. Dlatego dla dochodów, cen
      i czasów reakcji podaje się medianę obok średniej."),

    lc_note("Zasada", rule = TRUE,
      "Dane zawsze dodatnie, z długim prawym ogonem: zlogarytmuj je i obejrzyj
       histogram. Jeśli wygląda jak symetryczny dzwon, rozkład log-normalny
       jest dobrym kandydatem na model."
    ),

    lc_h2("ch5-porownanie-ciaglych", "Porównanie rozkładów ciągłych"),

    lc_p("Sześć rozkładów ciągłych z rozdziałów 4 i 5 odpowiada na sześć
      różnych pytań o dane."),

    figure_panel(
      label = "Ryc. 5.15",
      title = "Które pytanie, który rozkład",
      width_mode = "wide",
      ch5_continuous_map()
    ),

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

  # --- Ryc. 5.10: dron z GPS → rozkład χ² (experiment.js, kind = "drone") ---
  exp_texts(input, output, "ch5_dron", list(
    tagList("Dron leci do celu, ale GPS myli się o kilkadziesiąt centymetrów, czasem o metr
      lub dwa. Wyślij kilka lotów i zobacz, gdzie dron ląduje."),
    tagList(X(), " to kwadrat chybienia, czyli kwadrat odległości od celu. Z Pitagorasa to suma
      kwadratów błędów na osiach, widocznych w dzienniku."),
    tagList("Wysyłamy drona wiele razy i zliczamy kwadraty chybień w przedziałach po 0.5 m².
      Dokładaj po 10, 100 i 1000 lotów, potem przełącz na balkon."),
    tagList("Krzywa to model sumy kwadratów błędów normalnych. Średni kwadrat chybienia zbliża się
      do liczby osi: 2 przy lądowisku, 3 przy balkonie.")
  ))

  # Ryc. 5.2 i 5.6 (z presetami i odczytami) rysuje dist_charts.js w przeglądarce.

  # Ryc. 5.11, 5.12 i 5.14 rysuje dist_charts.js w przeglądarce.

}
