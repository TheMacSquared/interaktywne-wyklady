# ============================================================================
# CHAPTER 4: Rozkłady ciągłe
# ============================================================================

# Oś czasu z ustalonymi zdarzeniami (Ryc. 4.9).
# Nad osią liczba zdarzeń w każdej godzinie (Poisson, col_poisson), pod osią
# odstępy między kolejnymi zdarzeniami (wykładniczy, col_exponential).
ch4_event_times <- c(0.18, 0.55, 0.62, 1.30, 1.71, 2.05, 2.12, 2.38, 2.90,
                     3.47, 3.60, 3.92)
ch4_event_timeline <- function(t = ch4_event_times, hours = 4) {
  x0 <- 130; x1 <- 790; sx <- function(h) x0 + (x1 - x0) * h / hours
  y_axis <- 92; y_gap <- 122
  counts <- tabulate(floor(t) + 1, nbins = hours)
  tags$svg(class = "evt", viewBox = "0 0 800 150", role = "img",
    `aria-label` = paste0("Oś czasu z ", length(t), " zdarzeniami w ", hours,
      " godzinach. Liczba zdarzeń w kolejnych godzinach: ", paste(counts, collapse = ", "),
      ". Pod osią odstępy między kolejnymi zdarzeniami."),
    # pasy godzin
    lapply(seq_len(hours), function(h) tags$rect(class = paste("evt-hour", if (h %% 2 == 0) "is-alt"),
      x = sx(h - 1), y = 18, width = sx(h) - sx(h - 1), height = y_gap + 14 - 18)),
    # zliczenia nad osią
    lapply(seq_len(hours), function(h) tags$text(class = "evt-count",
      x = (sx(h - 1) + sx(h)) / 2, y = 56, `text-anchor` = "middle", counts[h])),
    # oś i podziałka godzin
    tags$line(class = "evt-axis", x1 = x0, x2 = x1, y1 = y_axis, y2 = y_axis),
    lapply(0:hours, function(h) tags$text(class = "evt-tick", x = sx(h), y = 148,
      `text-anchor` = "middle", paste0(h, " h"))),
    # odstępy pod osią między kolejnymi zdarzeniami
    lapply(seq_along(t)[-1], function(i) {
      a <- sx(t[i - 1]) + 3; b <- sx(t[i]) - 3
      tags$g(class = "evt-gap",
        tags$line(x1 = a, x2 = b, y1 = y_gap, y2 = y_gap),
        tags$line(x1 = a, x2 = a, y1 = y_gap - 5, y2 = y_gap + 5),
        tags$line(x1 = b, x2 = b, y1 = y_gap - 5, y2 = y_gap + 5))
    }),
    # zdarzenia
    lapply(t, function(e) tags$g(
      tags$line(class = "evt-drop", x1 = sx(e), x2 = sx(e), y1 = y_axis, y2 = y_gap),
      tags$circle(class = "evt-dot", cx = sx(e), cy = y_axis, r = 6))),
    # etykiety wierszy
    tags$text(class = "evt-lab is-count", x = 0, y = 56, "liczba w godzinie"),
    tags$text(class = "evt-lab is-gap", x = 0, y = y_gap + 5, "odstęp")
  )
}

ch4_unif_defs <- list(
  unif_1 = list(label = "U(0, 10)", a = 0, b = 10),
  unif_2 = list(label = "U(2, 8)", a = 2, b = 8),
  unif_3 = list(label = "U(0, 2)", a = 0, b = 2),
  unif_4 = list(label = "U(4, 6)", a = 4, b = 6)
)

ch4_exp_defs <- list(
  exp_1 = list(label = "Awarie: λ = 0.3/dzień", lambda = 0.3),
  exp_2 = list(label = "Wiadomości: λ = 1/godz", lambda = 1),
  exp_3 = list(label = "Zgłoszenia: λ = 2/godz", lambda = 2),
  exp_4 = list(label = "Połączenia: λ = 5/min", lambda = 5)
)


# Podpisy pod rycinami scenariuszy (E(X) i SD).
ch4_unif_stat <- function(s) {
  paste0(s$label, ":  E(X) = ", round((s$a + s$b) / 2, 1), ",  SD = ", round(sqrt((s$b - s$a)^2 / 12), 2))
}
ch4_exp_stat <- function(s) {
  mu <- 1 / s$lambda
  paste0(s$label, ":  E(X) = 1/λ = ", round(mu, 2), ",  SD = ", round(mu, 2))
}

ch4_ui <- list(
  id = "ch-ciagle", num = "04", title = "Rozkłady ciągłe",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 04 · Rozkłady prawdopodobieństwa",
      num    = "04",
      title  = "Rozkłady ciągłe.",
      lead   = "Wzrost, czas oczekiwania czy dochód mogą przyjąć dowolną wartość
                z przedziału. Dla takich zmiennych pojedyncza wartość ma
                prawdopodobieństwo zero, a prawdopodobieństwo przedziału
                odczytujemy jako pole pod krzywą gęstości."
    ),

    lc_p("W poprzednim rozdziale każdej wartości zmiennej przypisywaliśmy
      prawdopodobieństwo P(X = k). Taki opis, ",
      gloss("funkcja prawdopodobieństwa", "funkcja prawdopodobieństwa"),
      ", działa, gdy wartości da się wypisać: 0, 1, 2 i tak dalej. ",
      gloss("zmienna ciągła", "Zmienna ciągła"), " może przyjąć każdą wartość
      z przedziału, na przykład czas oczekiwania 1.7 min albo 1.7182 min.
      Takich wartości jest nieskończenie wiele, więc nie da się każdej z nich
      przypisać dodatniego prawdopodobieństwa tak, żeby suma wyniosła 1.
      W tym rozdziale zastąpimy słupki krzywą, a sumowanie słupków liczeniem
      pola."),

    # ========================================================================
    # WIDGET 1: Od histogramu do krzywej (krok po kroku)
    # ========================================================================
    lc_h2("ch4-histogram", "Od histogramu do krzywej gęstości"),

    lc_p("Punktem wyjścia jest narzędzie z wykładu o statystyce opisowej: ",
      gloss("histogram"), ". Dzieli on oś na przedziały i nad każdym rysuje
      słupek, którego wysokość to liczba obserwacji w przedziale. Gdy próba
      rośnie, a przedziały się zwężają, schodkowy histogram coraz bardziej
      przypomina gładką krzywą. Ta krzywa to ",
      gloss("funkcja gęstości", "funkcja gęstości prawdopodobieństwa"),
      " f(x), w skrócie PDF. Jest ciągłym odpowiednikiem funkcji
      prawdopodobieństwa z rozdziału 3."),

    lc_p("Panel losuje próbę z wybranego rozkładu, domyślnie 500 obserwacji
      z rozkładu normalnego o średniej 5 i odchyleniu standardowym 1.5,
      i w siedmiu krokach przechodzi od surowych danych do krzywej."),

    figure_panel(
      label = "Ryc. 4.1",
      full_width = TRUE,
      lc_step_widget("ch4_step",
        title = "Od histogramu do krzywej gęstości",
        steps = c("Surowe dane", "Histogram (5 binów)", "Więcej binów (15)",
                  "Jeszcze więcej (30)", "Skala gęstości", "Krzywa gęstości",
                  "Tylko PDF"),
        toolbar = lc_toolbar(
          selectInput("ch4_step_dist", "Rozkład źródłowy",
            choices = c("Normalny" = "normal", "Wykładniczy" = "exp",
                        "Jednostajny" = "unif"),
            selected = "normal"
          ),
          lc_slider("ch4_step_n", "Wielkość próby", 50, 10000, 500, 50)
        ),
        plot_id = "ch4_step_plot"
      )
    ),

    lc_p("Kluczowy jest krok piąty. Na osi Y nie ma już liczebności, tylko
      gęstość: liczebność przedziału dzielimy przez liczbę wszystkich obserwacji
      i przez szerokość przedziału. Kształt histogramu się nie zmienia, zmienia
      się sens pola. Pole słupka, czyli wysokość razy szerokość, to teraz
      ", gloss("częstość względna"), ", odsetek obserwacji w przedziale. Wszystkie słupki
      razem mają pole równe 1, bo obejmują wszystkie dane."),

    lc_p("Krzywa z kroków 6 i 7 jest wygładzeniem tej konkretnej próby, więc
      przy każdym losowaniu wygląda trochę inaczej. Przy rosnącej próbie
      wygładzenia kolejnych prób coraz mniej się od siebie różnią i zbliżają
      się do jednej krzywej. Ją właśnie nazywamy funkcją gęstości rozkładu.
      Tak jak w rozdziale 1 częstości względne zbliżały się do
      prawdopodobieństw, tak tutaj histogram w skali gęstości zbliża się do f(x).
      Zmień rozkład źródłowy na wykładniczy albo jednostajny: zasada jest ta
      sama, inny jest tylko kształt krzywej."),

    # ========================================================================
    # WIDGET 2: Prawdopodobieństwo = pole
    # ========================================================================
    lc_h2("ch4-pole", "Prawdopodobieństwo = pole pod krzywą"),

    lc_p("W histogramie w skali gęstości pole słupka było odsetkiem obserwacji
      w przedziale. Dla krzywej gęstości obowiązuje ta sama reguła:
      prawdopodobieństwo, że zmienna wpadnie do przedziału od a do b, to pole
      pod krzywą f(x) nad tym przedziałem. Pole pod krzywą liczy się całką."),

    lc_formula_annotated(
      "⟦1|P(a \\le X \\le b)⟧ = ⟦2|\\int_a^b⟧ ⟦3|f(x)⟧ \\, dx, \\qquad \\int_{-\\infty}^{\\infty} f(x) \\, dx = 1",
      notes  = list("prawdopodobieństwo przedziału", "pole od a do b", "gęstość"),
      colors = c("accent", "bursztyn", "indygo"),
      above  = 2
    ),

    lc_p("Z tej definicji wynikają dwie konsekwencje, które odróżniają rozkłady
      ciągłe od dyskretnych. Pierwsza: P(X = x) = 0 dla każdej pojedynczej
      wartości x, bo nad odcinkiem o szerokości zero pole jest zerowe. Nie
      pytamy więc, jakie jest prawdopodobieństwo czasu dokładnie 5.0 min, tylko
      czasu między 4.5 a 5.5 min. Z tego samego powodu nie ma różnicy między
      P(a ≤ X ≤ b) a P(a < X < b). Druga: wysokość krzywej f(x) nie jest
      prawdopodobieństwem. Gęstość może być większa od 1. Rozkład jednostajny
      na przedziale od 0 do 0.5 ma f(x) = 2 na całym przedziale, a mimo to pole
      pod nim wynosi 2 · 0.5 = 1."),

    # Ryc. 4.2: lupa w ECharts (zaakceptowana 2026-11-05, ec_proto.js)
    s3p_4aa_ui,

    lc_p("Wróćmy do wzrostu z Ryc. 4.2. Skoro pytanie o dokładnie 170 cm nie ma
      sensu, trzeba ustalić, co znaczy „około 170 cm”, na przykład 170 ± 5 cm.
      Panel zacienia pole pod krzywą ", gloss("rozkład normalny", "rozkładu normalnego"),
      " N(170, 8) między granicami a i b, które przeciąga się po wykresie."),

    figure_panel(
      label = "Ryc. 4.3",
      title = "Ile osób ma „około 170 cm”?",
      full_width = TRUE,
      tags$div(class = "lc-area", `data-mu` = "170", `data-sd` = "8", `data-lo` = "140", `data-hi` = "200",
        `data-a` = "165", `data-b` = "175",
        lc_toolbar(
          lc_caption("Przeciągnij granice lub użyj Tab i strzałek."),
          lc_readouts(
            lc_readout("a", tags$span(`data-role` = "a", "165.0")),
            lc_readout("b", tags$span(`data-role` = "b", "175.0")),
            lc_readout("P(a < X < b)", tags$span(`data-role` = "P", "0.468"),
                       color = "var(--upwr-accent)")
          ),
          lc_chart_zoom_button()
        ),
        tags$div(class = "lc-area-canvas",
          tags$div(class = "lc-ec", `data-role` = "pdf",
                   role = "img",
                   `aria-label` = "Gęstość wzrostu N(170, 8) z polem między granicami a i b"))
      )
    ),

    lc_p("Przy granicach 165 i 175 cm pole wynosi 0.468: mniej więcej co druga
      osoba ma „około 170 cm” w tym sensie. Węższy przedział, od 169 do 171 cm,
      obejmuje już tylko 0.099, a przedział od 169.5 do 170.5 cm około 0.050.
      Zbliżaj granice a i b do siebie: zacieniony pas zwęża się, a prawdopodobieństwo
      spada do zera, choć krzywa nad tym miejscem ma dodatnią wysokość. To ten
      sam efekt co na Ryc. 4.2, tylko odczytany jako pole."),

    # ========================================================================
    # Dystrybuanta (bez widgetu)
    # ========================================================================
    lc_h2("ch4-dystrybuanta", "Dystrybuanta"),

    lc_p("Liczenie całki przy każdym pytaniu o przedział byłoby uciążliwe.
      Wystarczy jednak znać jedną funkcję: pole pod krzywą od lewego końca
      osi do punktu x. Tę funkcję nazywamy ", gloss("dystrybuanta", "dystrybuantą"), " i oznaczamy F(x).
      Dystrybuanta podaje prawdopodobieństwo, że zmienna nie przekroczy
      wartości x."),

    lc_formula_annotated(
      "⟦1|F(x)⟧ = P(X \\le x) = ⟦2|\\int_{-\\infty}^{x}⟧ ⟦3|f(t)⟧ \\, dt, \\qquad P(a < X \\le b) = F(b) - F(a)",
      notes  = list("dystrybuanta", "pole na lewo od x", "gęstość"),
      colors = c("accent", "bursztyn", "indygo"),
      above  = 2
    ),

    figure_panel(
      label = "Ryc. 4.4",
      title = "Pole na lewo od x to wartość dystrybuanty",
      full_width = TRUE,
      tags$div(class = "lc-cdf", `data-x` = "1",
        lc_toolbar(
          lc_caption("Przesuń kursor po wykresie lub użyj Tab i strzałek."),
          lc_readouts(
            lc_readout("x", tags$span(`data-role` = "x", "1.00")),
            lc_readout("F(x)", tags$span(`data-role` = "F", "0.8413"),
                       color = "var(--upwr-accent)")
          ),
          lc_chart_zoom_button()
        ),
        tags$div(class = "lc-cdf-grid",
          tags$div(class = "lc-cdf-canvas",
            tags$div(class = "lc-ec", role = "img", `data-role` = "pdf",
                        `aria-label` = "Gęstość N(0, 1) z polem na lewo od x")),
          tags$div(class = "lc-cdf-canvas",
            tags$div(class = "lc-ec", role = "img", `data-role` = "cdf",
                        `aria-label` = "Dystrybuanta N(0, 1) z punktem (x, F(x))"))
        )
      )
    ),

    lc_p("Na lewym wykresie zacieniowane pole pod gęstością, na prawym ta
      sama liczba jako wysokość krzywej F w punkcie x. Gdy x przesuwa się
      w prawo, pole rośnie, a punkt wspina się po krzywej. Najszybciej rośnie
      tam, gdzie gęstość jest najwyższa, czyli przy średniej, a na krańcach,
      gdzie gęstość jest prawie zerowa, F prawie się nie zmienia."),

    lc_p("Prawdopodobieństwo przedziału to różnica dwóch pól: pola na lewo
      od b i pola na lewo od a. Dokładnie tak liczy je panel z Ryc. 4.3.
      Dla rozkładu N(0, 1) F(1) = 0.8413 i F(-1) = 0.1587, więc
      P(-1 < X ≤ 1) = 0.8413 - 0.1587 = 0.6827. Dystrybuanta rośnie od 0
      na lewym krańcu do 1 na prawym i nigdy nie maleje. Ma ją także każdy
      rozkład dyskretny: tam F(x) jest sumą słupków P(X = k) dla k ≤ x."),

    lc_p("Dystrybuantę można też czytać odwrotnie: zamiast pytać o pole na lewo
      od danej wartości, pytamy, przy jakiej wartości to pole osiąga zadany
      poziom. Taka wartość to ", gloss("kwantyl"), " rzędu q, czyli x, dla którego F(x) = q.
      ", gloss("mediana", "Mediana"), " jest kwantylem rzędu 0.5, a ", gloss("percentyl", "percentyle"), " z wykładu 01 to kwantyle
      wyrażone w procentach."),

    # Ryc. 4.5: prototyp 4b (zaakceptowany 2026-10-29)
    s3p_4b_ui,

    # ========================================================================
    # WIDGET 3: Jednostajny ciągły — scenariusze overlay
    # ========================================================================
    lc_h2("ch4-jednostajny", "Rozkład jednostajny ciągły U(a, b)"),

    lc_p("Mamy już wszystkie narzędzia, żeby opisywać konkretne rozkłady
      ciągłe. Każdy opiszemy tak samo jak dyskretne w rozdziale 3: sytuacja,
      w której się pojawia, funkcja gęstości, ", gloss("wartość oczekiwana"), " i ", gloss("wariancja"), ".
      Wzory na E(X) i Var(X) z rozdziału 2 przenosimy bez zmian w treści:
      sumę po wartościach zastępuje całka, a prawdopodobieństwo P(X = k) —
      gęstość f(x)."),

    lc_formula_box(withMathJax(
      "$$E(X) = \\int_{-\\infty}^{\\infty} x \\, f(x) \\, dx, \\qquad Var(X) = \\int_{-\\infty}^{\\infty} \\left(x - E(X)\\right)^2 f(x) \\, dx$$"
    )),

    lc_p("Najprostszy przypadek to ",
      gloss("rozkład jednostajny", "rozkład jednostajny ciągły"), " U(a, b).
      Zmienna przyjmuje wartości z przedziału od a do b i żaden fragment
      przedziału nie jest wyróżniony: odcinki tej samej długości mają to samo
      prawdopodobieństwo. Przykład: autobus odjeżdża co 10 minut, a pasażer
      przychodzi na przystanek, nie patrząc na rozkład jazdy. Czas
      oczekiwania ma rozkład U(0, 10). Gęstość jest stała na całym przedziale,
      więc wykres jest prostokątem. Jego wysokość wynika z warunku, że pole
      wynosi 1."),

    lc_formula_annotated(
      "f(x) = \\frac{1}{⟦1|b-a⟧} \\;\\text{ dla } a \\le x \\le b, \\qquad E(X) = ⟦2|\\frac{a+b}{2}⟧, \\qquad Var(X) = \\frac{(⟦1|b-a⟧)^2}{12}",
      notes  = list("szerokość przedziału", "środek przedziału"),
      colors = c("szalwia", "accent"),
      above  = 2
    ),

    figure_panel(
      label = "Ryc. 4.6",
      title = "Rozkład jednostajny U(a, b)",
      full_width = TRUE,
      d03_overlay("ch4_unif_scenarios", ch4_unif_defs, "unif_1", ch4_unif_stat, "unif", max_height = "350px")
    ),

    lc_p("Im szerszy przedział, tym niższy prostokąt: U(0, 10) ma wysokość 0.1,
      a U(0, 2) — 0.5. Pole zawsze wynosi 1. Wartość oczekiwana leży w środku
      przedziału, a odchylenie standardowe zależy tylko od jego szerokości.
      U(0, 2) i U(4, 6) to ten sam prostokąt przesunięty po osi: mają różne
      wartości oczekiwane (1 i 5), ale jednakowe SD równe 0.58."),

    lc_p("Wróćmy do autobusu. Dla U(0, 10) średni czas oczekiwania to
      E(X) = 5 min, wariancja 100/12 = 8.33, a SD = 2.89 min.
      Prawdopodobieństwo, że pasażer poczeka dłużej niż 7 minut, to pole prostokąta
      od 7 do 10: 3 · 0.1 = 0.3."),

    # ========================================================================
    # WIDGET 3b: Wykładniczy — scenariusze overlay
    # ========================================================================
    lc_h2("ch4-wykladniczy", "Rozkład wykładniczy Exp(λ)"),

    lc_p("W rozdziale 3 ", gloss("rozkład Poissona"), " liczył zdarzenia
      w ustalonym czasie, na przykład wiadomości w ciągu godziny. To samo
      zjawisko można opisać z drugiej strony: ile czasu mija między kolejnymi
      zdarzeniami. Gdy zdarzenia zachodzą niezależnie, ze stałym średnim
      tempem λ zdarzeń na jednostkę czasu, czas oczekiwania ma ",
      gloss("rozkład wykładniczy"), " Exp(λ). Jeśli liczba wiadomości na
      godzinę ma rozkład Poissona z λ = 1, to czas między wiadomościami ma
      rozkład Exp(1), mierzony w godzinach. To dwie strony tego samego
      procesu."),

    lc_p("Zamiast liczyć zdarzenia, mierzymy czas do jednego z nich. Pada deszcz
      i czekamy, aż się skończy. Deszcz trwa średnio 15 minut, ale każdy trwa
      inaczej, więc czas do jego końca jest zmienną losową. Może wynieść dowolną
      wartość dodatnią, więc jest zmienną ciągłą."),

    figure_panel(
      label = "Ryc. 4.7",
      width_mode = "text",
      exp_widget("ch4_expo", "Od czekania na koniec deszczu do rozkładu wykładniczego", "Zacznij deszcz",
        list(kind = "expo", lambda = 4, window = 60, width = 5, unit = "deszcz",
             sub = "czas do końca deszczu",
             xTitle = "X, czyli czas do końca deszczu (min)",
             aria = "Czas do końca deszczu i histogram powtórzeń"))
    ),

    lc_p("Gęstość ma największą wartość w zerze i maleje wykładniczo. Rozkład
      wykładniczy ma też prostą dystrybuantę, więc prawdopodobieństwa można
      liczyć bez całkowania."),

    lc_formula_annotated(
      "f(x) = ⟦1|\\lambda⟧ e^{-\\lambda ⟦2|x⟧}, \\quad F(x) = 1 - e^{-\\lambda x} \\;\\text{ dla } x \\ge 0",
      notes  = list("tempo zdarzeń", "czas oczekiwania"),
      colors = c("terakota", "niebo")
    ),
    lc_formula_annotated(
      "⟦1|E(X)⟧ = \\frac{1}{\\lambda}, \\quad Var(X) = \\frac{1}{\\lambda^2}",
      notes  = list("średni czas oczekiwania"),
      colors = c("accent")
    ),

    lc_p("Scenariusze w panelu mają różne jednostki czasu, podane w etykietach."),

    figure_panel(
      label = "Ryc. 4.8",
      title = "Rozkład wykładniczy Exp(λ)",
      full_width = TRUE,
      d03_overlay("ch4_exp_scenarios", ch4_exp_defs, "exp_2", ch4_exp_stat, "exp", max_height = "350px")
    ),

    lc_p("Krótkie czasy oczekiwania są najczęstsze, długie zdarzają się
      rzadko, ale się zdarzają. Im większe λ, tym krzywa startuje wyżej
      i szybciej opada, a średni czas oczekiwania 1/λ jest krótszy. Odchylenie
      standardowe jest równe wartości oczekiwanej, więc rozrzut jest duży:
      przy λ = 1 wiadomość na godzinę E(X) = 1 h i SD = 1 h."),

    lc_p("Dla tego scenariusza F(1) = 1 − e⁻¹ = 0.632. Oznacza to, że 63%
      odstępów jest krótszych od średniej. Mediana wynosi ln 2 / λ = 0.69 h,
      czyli około 42 minut, mniej niż średnia, bo długi prawy ogon podnosi
      średnią. Na wiadomość dłużej niż 2 godziny czeka się z prawdopodobieństwem
      e⁻² = 0.135."),

    lc_p("Rozkład wykładniczy ma nietypową własność, ",
      gloss("bezpamięciowość"), ". Załóżmy, że oczekiwanie na wiadomość trwa już
      2 godziny. Prawdopodobieństwo, że potrwa jeszcze co najmniej godzinę,
      wynosi P(X > 3 | X > 2) = e⁻³ / e⁻² = e⁻¹ = 0.368. To dokładnie tyle
      samo, ile prawdopodobieństwo czekania ponad godzinę od początku,
      P(X > 1) = 0.368. Czas, który już minął, nie skraca dalszego oczekiwania.
      Dlatego rozkład wykładniczy pasuje do zdarzeń, które nie mają pamięci,
      jak przychodzące wiadomości, a słabo do zużywających się elementów, jak
      starzejąca się maszyna."),

    # Te same zdarzenia jako zliczenia i jako odstępy
    figure_panel(
      label = "Ryc. 4.9",
      title = "Jedna oś czasu, dwie zmienne losowe",
      width_mode = "wide",
      ch4_event_timeline()
    ),

    lc_p("Na koniec wróćmy do związku z rozkładem Poissona. Liczby nad osią
      to wartości zmiennej o rozkładzie Poissona: ile
      zdarzeń wpadło do kolejnej godziny. Odcinki pod osią to wartości zmiennej
      o rozkładzie wykładniczym: ile czasu minęło od poprzedniego zdarzenia.
      Gdy zdarzeń w godzinie jest dużo, odstępy między nimi są krótkie."),

    lc_p("Bezpamięciowość ma zaskakujący skutek na przystanku. Porównajmy dwie
      linie, w których autobus odjeżdża średnio co 10 minut."),

    # Ryc. 4.10: prototyp 4c (zaakceptowany 2026-10-29)
    s3p_4c_ui,

    lc_p("Rozkład normalny pojawiał się w tym rozdziale kilka razy, na
      razie jako domyślny przykład gęstości. W następnym rozdziale zajmiemy
      się nim osobno, a na jego końcu zobaczymy trzy rozkłady, które z niego
      powstają: χ², t-Studenta i log-normalny."),

    lc_chapter_next(
      num       = "05",
      title     = "Rozkład normalny",
      lead      = "królowa rozkładów — dlaczego pojawia się wszędzie.",
      target_id = "ch-normalny"
    )
  )
)

# --------------------------------------------------------------------------
# Definicje scenariuszy
# --------------------------------------------------------------------------


# --------------------------------------------------------------------------
# Chapter 4 Server
# --------------------------------------------------------------------------

ch4_server <- function(input, output, session) {

  # --- Ryc. 4.7: czas oczekiwania → rozkład wykładniczy (experiment.js) ---
  X <- exp_X
  exp_texts(input, output, "ch4_expo", list(
    tagList("Zaczyna padać. Zegar rusza od zera i czekamy, aż deszcz się skończy: chmurka zamieni się
      w słoneczko. Czas jest za każdym razem inny, a przed deszczem go nie znamy."),
    tagList("Z każdego deszczu robimy jedną liczbę: ", X(), " to czas do jego końca w minutach.
      Może wynieść dowolną wartość dodatnią, nie tylko całkowitą, więc to zmienna losowa ciągła."),
    tagList("Powtarzamy deszcze i zliczamy, ile czasów trafia do przedziałów po 5 minut.
      Dla zmiennej ciągłej liczy się przedział, a nie pojedyncza wartość. Wykres urywa się na
      60 minutach: dłuższe deszcze są rzadkie."),
    tagList("Częstości w przedziałach układają się wzdłuż krzywej: najwięcej krótkich deszczów, potem
      wykładniczy spadek. To gęstość rozkładu wykładniczego ze średnim czasem 15 minut, czyli
      λ = 4 na godzinę.")
  ))

  # --- Widget 1: Krok po kroku ---
  # Krok widgetu (1..7) żyje w przeglądarce; zmiana rozkładu lub próby nie cofa kroku.
  ch4_step <- lc_step_server("ch4_step", input)$step

  ch4_sample_data <- reactive({
    req(input$ch4_step_n, input$ch4_step_dist)
    switch(input$ch4_step_dist,
      "normal" = rnorm(input$ch4_step_n, mean = 5, sd = 1.5),
      "exp"    = rexp(input$ch4_step_n, rate = 0.5),
      "unif"   = runif(input$ch4_step_n, min = 0, max = 10)
    )
  })

  # Granice dziedziny dla wygładzenia (kroki 6–7): krzywa nie wychodzi poza
  # możliwe wartości, a odbicie jądra poprawia jej wysokość przy brzegu.
  ch4_step_bounds <- reactive({
    switch(input$ch4_step_dist,
      "normal" = c(-Inf, Inf),
      "exp"    = c(0, Inf),
      "unif"   = c(0, 10)
    )
  })

  # Stała rama z pełnej próby: oś X wspólna dla kroków, oś Y wspólna dla
  # kroków na skali gęstości (5–7). Zapas 14% mieści skrajne słupki 5 binów.
  ch4_step_frame <- reactive({
    data <- ch4_sample_data()
    df <- data.frame(x = data)
    x_pad <- diff(range(data)) * 0.14
    hist_max <- function(bins, stat) {
      max(layer_data(ggplot(df, aes(x = x)) + geom_histogram(bins = bins))[[stat]])
    }
    kde_max <- max(layer_data(ggplot(df, aes(x = x)) +
                                geom_density(bounds = ch4_step_bounds()))$density)
    dens_max <- max(hist_max(30, "density"), kde_max)
    list(
      xlim = range(data) + c(-x_pad, x_pad),
      count_max = c(`5` = hist_max(5, "count"), `15` = hist_max(15, "count"),
                    `30` = hist_max(30, "count")),
      dens_max = dens_max
    )
  })

  zoom_plot_server("ch4_step_plot", reactive({
    step <- ch4_step()
    data <- ch4_sample_data()
    fr <- ch4_step_frame()

    df <- data.frame(x = data)
    count_frame <- function(bins) {
      step_frame(xlim = fr$xlim, ylim = c(0, fr$count_max[[as.character(bins)]] * 1.08))
    }
    dens_frame <- step_frame(xlim = fr$xlim, ylim = c(0, fr$dens_max * 1.08))

    if (step == 1) {
      ggplot(df, aes(x = x)) +
        step_layer(geom_rug, "data", alpha = 0.3) +
        scale_y_continuous() +
        labs(x = "Wartość", y = "") +
        step_frame(xlim = fr$xlim, ylim = c(0, 1), y_axis = FALSE)
    } else if (step == 2) {
      ggplot(df, aes(x = x)) +
        step_result(geom_histogram, bins = 5) +
        step_layer(geom_rug, "background") +
        labs(x = "Wartość", y = "Liczebność") +
        count_frame(5)
    } else if (step == 3) {
      ggplot(df, aes(x = x)) +
        step_result(geom_histogram, bins = 15) +
        labs(x = "Wartość", y = "Liczebność") +
        count_frame(15)
    } else if (step == 4) {
      ggplot(df, aes(x = x)) +
        step_result(geom_histogram, bins = 30) +
        labs(x = "Wartość", y = "Liczebność") +
        count_frame(30)
    } else if (step == 5) {
      ggplot(df, aes(x = x)) +
        step_result(geom_histogram, mapping = aes(y = after_stat(density)), bins = 30) +
        labs(x = "Wartość", y = "Gęstość") +
        dens_frame
    } else if (step == 6) {
      ggplot(df, aes(x = x)) +
        step_result(geom_histogram, mapping = aes(y = after_stat(density)), bins = 30,
                    alpha = 0.5) +
        step_layer(geom_density, "new", linewidth = 1.5,
                   bounds = ch4_step_bounds()) +
        labs(x = "Wartość", y = "Gęstość") +
        dens_frame
    } else {
      ggplot(df, aes(x = x)) +
        step_layer(geom_density, "known", fill = STEP_ROLES$data$colour,
                   linewidth = 1.2, alpha = 0.3, bounds = ch4_step_bounds()) +
        labs(x = "Wartość", y = "Gęstość f(x)") +
        dens_frame
    }
  }))

  output$ch4_step_text <- renderUI({
    step <- ch4_step()
    texts <- c(
      "Każda kreska to jedna obserwacja. Przy setkach kresek trudno ocenić, gdzie jest ich najwięcej.",
      "5 binów — widać ogólny zarys, ale mało szczegółów.",
      "15 binów — kształt staje się wyraźniejszy.",
      "30 binów — więcej szczegółów, ale słupki są nierówne.",
      "Oś Y w skali gęstości — łączne pole słupków wynosi 1.",
      "Gładka krzywa przybliża kształt histogramu.",
      "Zostaje sama krzywa gęstości f(x). Pole pod nią wynosi 1."
    )
    texts[step]
  })

  # Ryc. 4.6 i 4.8 rysuje dist_charts.js w przeglądarce.

}
