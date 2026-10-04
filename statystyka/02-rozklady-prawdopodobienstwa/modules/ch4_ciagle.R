# ============================================================================
# CHAPTER 4: Rozkłady ciągłe
# ============================================================================

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
      częstość względna, odsetek obserwacji w przedziale. Wszystkie słupki
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

    lc_formula_box(withMathJax(
      "$$P(a \\le X \\le b) = \\int_a^b f(x) \\, dx, \\qquad \\int_{-\\infty}^{\\infty} f(x) \\, dx = 1$$"
    )),

    lc_p("Z tej definicji wynikają dwie konsekwencje, które odróżniają rozkłady
      ciągłe od dyskretnych. Pierwsza: P(X = x) = 0 dla każdej pojedynczej
      wartości x, bo nad odcinkiem o szerokości zero pole jest zerowe. Nie
      pytamy więc, jakie jest prawdopodobieństwo czasu dokładnie 5.0 min, tylko
      czasu między 4.5 a 5.5 min. Z tego samego powodu nie ma różnicy między
      P(a ≤ X ≤ b) a P(a < X < b). Druga: wysokość krzywej f(x) nie jest
      prawdopodobieństwem. Gęstość może być większa od 1. Rozkład jednostajny
      na przedziale od 0 do 0.5 ma f(x) = 2 na całym przedziale, a mimo to pole
      pod nim wynosi 2 · 0.5 = 1."),

    lc_p("Panel zacienia pole między granicami a i b dla trzech rozkładów,
      które omówimy w tym i następnym rozdziale."),

    figure_panel(
      label = "Ryc. 4.2",
      title = "Zacieniuj przedział i odczytaj prawdopodobieństwo",
      full_width = TRUE,
      lc_toolbar(
        selectInput("ch4_area_dist", "Rozkład",
            choices = c("Normalny N(0, 1)" = "norm",
                        "Wykładniczy Exp(1)" = "exp",
                        "Jednostajny U(0, 10)" = "unif"),
            selected = "norm"
          ),
        lc_slider("ch4_area_a", "Dolna granica (a)", -4, 4, -1, 0.1),
        lc_slider("ch4_area_b", "Górna granica (b)", -4, 4, 1, 0.1),
        lc_readouts(uiOutput("ch4_area_stats"))
      ),
      lc_plot("ch4_area_plot", max_height = "350px")
    ),

    lc_p("Domyślnie panel pokazuje ", gloss("rozkład normalny"), " N(0, 1),
      któremu poświęcimy cały następny rozdział. Pole między -1 a 1 wynosi
      0.6827: około dwóch trzecich prawdopodobieństwa leży w tym przedziale.
      Dla rozkładu wykładniczego Exp(1) domyślny przedział od 0 do 2 obejmuje
      0.8647, a dla rozkładu jednostajnego U(0, 10) przedział od 2 do 7
      obejmuje dokładnie 0.5. W tym ostatnim przypadku pole jest prostokątem
      o szerokości 5 i wysokości 0.1. Zbliżaj suwaki a i b do siebie:
      zacieniony pas zwęża się, a prawdopodobieństwo spada do zera, choć
      krzywa nad tym miejscem ma dodatnią wysokość."),

    # ========================================================================
    # Dystrybuanta (bez widgetu)
    # ========================================================================
    lc_h2("ch4-dystrybuanta", "Dystrybuanta"),

    lc_p("Liczenie całki przy każdym pytaniu o przedział byłoby uciążliwe.
      Wystarczy jednak znać jedną funkcję: pole pod krzywą od lewego końca
      osi do punktu x. Tę funkcję nazywamy dystrybuantą i oznaczamy F(x).
      Dystrybuanta podaje prawdopodobieństwo, że zmienna nie przekroczy
      wartości x."),

    lc_formula_box(withMathJax(
      "$$F(x) = P(X \\le x) = \\int_{-\\infty}^{x} f(t) \\, dt, \\qquad P(a < X \\le b) = F(b) - F(a)$$"
    )),

    lc_p("Prawdopodobieństwo przedziału to różnica dwóch pól: pola na lewo
      od b i pola na lewo od a. Dokładnie tak liczy je panel z Ryc. 4.2.
      Dla rozkładu N(0, 1) F(1) = 0.8413 i F(-1) = 0.1587, więc
      P(-1 < X ≤ 1) = 0.8413 - 0.1587 = 0.6827. Dystrybuanta rośnie od 0
      na lewym krańcu do 1 na prawym i nigdy nie maleje. Ma ją także każdy
      rozkład dyskretny: tam F(x) jest sumą słupków P(X = k) dla k ≤ x."),

    lc_p("Dystrybuantę można też czytać odwrotnie: zamiast pytać o pole na lewo
      od danej wartości, pytamy, przy jakiej wartości to pole osiąga zadany
      poziom. Taka wartość to kwantyl rzędu q, czyli x, dla którego F(x) = q.
      Mediana jest kwantylem rzędu 0.5, a percentyle z wykładu 01 to kwantyle
      wyrażone w procentach."),

    # ========================================================================
    # WIDGET 3: Jednostajny ciągły — scenariusze overlay
    # ========================================================================
    lc_h2("ch4-jednostajny", "Rozkład jednostajny ciągły"),

    lc_p("Mamy już wszystkie narzędzia, żeby opisywać konkretne rozkłady
      ciągłe. Każdy opiszemy tak samo jak dyskretne w rozdziale 3: sytuacja,
      w której się pojawia, funkcja gęstości, wartość oczekiwana i wariancja.
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

    lc_formula_box(withMathJax(
      "$$f(x) = \\frac{1}{b-a} \\;\\text{ dla } a \\le x \\le b, \\qquad E(X) = \\frac{a+b}{2}, \\qquad Var(X) = \\frac{(b-a)^2}{12}$$"
    )),

    figure_panel(
      label = "Ryc. 4.3",
      title = "Rozkład jednostajny U(a, b)",
      full_width = TRUE,
      lc_toolbar(
        checkboxGroupInput("ch4_unif_scenarios", "Scenariusze",
            choices = c(
              "U(0, 10)" = "unif_1",
              "U(2, 8)" = "unif_2",
              "U(0, 2)" = "unif_3",
              "U(4, 6)" = "unif_4"
            ),
            selected = "unif_1"
          )
      ),
      lc_plot("ch4_unif_plot", max_height = "350px"),
      uiOutput("ch4_unif_stats")
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
    lc_h2("ch4-wykladniczy", "Rozkład wykładniczy"),

    lc_p("W rozdziale 3 ", gloss("rozkład Poissona"), " liczył zdarzenia
      w ustalonym czasie, na przykład wiadomości w ciągu godziny. To samo
      zjawisko można opisać z drugiej strony: ile czasu mija między kolejnymi
      zdarzeniami. Gdy zdarzenia zachodzą niezależnie, ze stałym średnim
      tempem λ zdarzeń na jednostkę czasu, czas oczekiwania ma ",
      gloss("rozkład wykładniczy"), " Exp(λ). Jeśli liczba wiadomości na
      godzinę ma rozkład Poissona z λ = 1, to czas między wiadomościami ma
      rozkład Exp(1), mierzony w godzinach. To dwie strony tego samego
      procesu."),

    lc_p("Gęstość ma największą wartość w zerze i maleje wykładniczo. Rozkład
      wykładniczy ma też prostą dystrybuantę, więc prawdopodobieństwa można
      liczyć bez całkowania."),

    lc_formula_box(withMathJax(
      "$$f(x) = \\lambda e^{-\\lambda x}, \\quad F(x) = 1 - e^{-\\lambda x} \\;\\text{ dla } x \\ge 0, \\qquad E(X) = \\frac{1}{\\lambda}, \\quad Var(X) = \\frac{1}{\\lambda^2}$$"
    )),

    lc_p("Scenariusze w panelu mają różne jednostki czasu, podane w etykietach."),

    figure_panel(
      label = "Ryc. 4.4",
      title = "Rozkład wykładniczy Exp(λ)",
      full_width = TRUE,
      lc_toolbar(
        checkboxGroupInput("ch4_exp_scenarios", "Scenariusze",
            choices = c(
              "Awarie: λ = 0.3/dzień" = "exp_1",
              "Wiadomości: λ = 1/godz" = "exp_2",
              "Zgłoszenia: λ = 2/godz" = "exp_3",
              "Połączenia: λ = 5/min" = "exp_4"
            ),
            selected = "exp_2"
          )
      ),
      lc_plot("ch4_exp_plot", max_height = "350px"),
      uiOutput("ch4_exp_stats")
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

    # ========================================================================
    # WIDGET 4: Rozkład t-Studenta — scenariusze overlay
    # ========================================================================
    lc_h2("ch4-t-studenta", "Rozkład t-Studenta"),

    lc_p("Rozkłady jednostajny i wykładniczy opisują zjawiska: czas na
      przystanku, odstępy między wiadomościami. Trzy kolejne rozkłady służą
      głównie czemu innemu. Opisują wartości statystyk obliczanych z próby
      i będą nam potrzebne przy przedziałach ufności i testach. Wszystkie trzy
      są zbudowane z rozkładu normalnego, który dokładnie poznamy w rozdziale 5.
      Tutaj wystarczy wiedzieć, że N(0, 1) to symetryczna krzywa w kształcie
      dzwonu, o środku w zerze."),

    lc_p(gloss("rozkład t-Studenta", "Rozkład t-Studenta"), " t(df) pojawia się,
      gdy średnią z próby standaryzujemy, czyli odejmujemy od niej średnią
      populacji i dzielimy przez rozrzut, ale prawdziwego odchylenia
      standardowego populacji σ nie znamy i zastępujemy je odchyleniem
      standardowym z próby. Ta dodatkowa niepewność sprawia, że gęstość
      t-Studenta ma kształt dzwonu jak N(0, 1), ale niższy szczyt i cięższe
      ogony: wartości daleko od zera są bardziej prawdopodobne. Parametr df
      to ", gloss("stopnie swobody"), ". Przy średniej z n obserwacji
      df = n - 1. Im więcej stopni swobody, tym rozkład bliższy N(0, 1)."),

    lc_formula_box(withMathJax(
      "$$E(X) = 0 \\;\\text{ dla } df > 1, \\qquad Var(X) = \\frac{df}{df - 2} \\;\\text{ dla } df > 2$$"
    )),

    figure_panel(
      label = "Ryc. 4.5",
      title = "Rozkład t-Studenta t(df)",
      full_width = TRUE,
      lc_toolbar(
        checkboxGroupInput("ch4_t_scenarios", "Scenariusze",
            choices = c(
              "t(df=1) — Cauchy" = "t_1",
              "t(df=3)" = "t_2",
              "t(df=5)" = "t_3",
              "t(df=30) ≈ normalny" = "t_4"
            ),
            selected = c("t_2", "t_4")
          ),
        checkboxInput("ch4_t_show_normal", "Pokaż N(0, 1) jako odniesienie", value = TRUE)
      ),
      lc_plot("ch4_t_plot", max_height = "400px"),
      uiOutput("ch4_t_stats")
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

    # ========================================================================
    # WIDGET 5: Rozkład chi-kwadrat — scenariusze overlay
    # ========================================================================
    lc_h2("ch4-chi-kwadrat", "Rozkład chi-kwadrat (χ²)"),

    lc_p("Rozkład t-Studenta opisuje statystyki, które mogą być ujemne lub
      dodatnie. Wiele statystyk mierzy jednak odległość, na przykład sumę
      kwadratów odchyleń, i nigdy nie jest ujemnych. Do nich służy ",
      gloss("rozkład chi-kwadrat"), " χ²(df). Powstaje jako suma kwadratów
      df niezależnych zmiennych o rozkładzie N(0, 1). Gęstość jest równa zeru
      dla wartości ujemnych, a dla dodatnich ma długi prawy ogon. Liczba
      stopni swobody df mówi, ile kwadratów sumujemy."),

    lc_formula_box(withMathJax(
      "$$X = Z_1^2 + Z_2^2 + \\ldots + Z_{df}^2, \\; Z_i \\sim N(0, 1), \\qquad E(X) = df, \\qquad Var(X) = 2 \\cdot df$$"
    )),

    figure_panel(
      label = "Ryc. 4.6",
      title = "Rozkład χ²(df)",
      full_width = TRUE,
      lc_toolbar(
        checkboxGroupInput("ch4_chisq_scenarios", "Scenariusze",
            choices = c(
              "χ²(df=2)" = "chisq_1",
              "χ²(df=5)" = "chisq_2",
              "χ²(df=10)" = "chisq_3",
              "χ²(df=20)" = "chisq_4"
            ),
            selected = "chisq_2"
          )
      ),
      lc_plot("ch4_chisq_plot", max_height = "400px"),
      uiOutput("ch4_chisq_stats")
    ),

    lc_p("Każdy składnik sumy ma wartość oczekiwaną 1, więc E(X) = df. Dla
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

    # ========================================================================
    # WIDGET 6: Rozkład log-normalny — scenariusze overlay
    # ========================================================================
    lc_h2("ch4-lognormalny", "Rozkład log-normalny"),

    lc_p("Ostatni rozkład znów opisuje zjawiska. Wiele wielkości rośnie
      przez mnożenie, a nie dodawanie: pensja rośnie o kilka procent rocznie,
      cena akcji zmienia się o procent dziennie. Logarytm zamienia mnożenie
      w dodawanie, więc to logarytm takiej wielkości ma często rozkład
      normalny. Jeśli ln(X) ~ N(μ, σ), to X ma ",
      gloss("rozkład log-normalny"), " LogN(μ, σ). Zmienna jest zawsze dodatnia
      i ma prawy ogon. Parametry μ i σ to średnia i odchylenie standardowe
      logarytmu, a nie samej zmiennej X."),

    lc_formula_box(withMathJax(
      "$$Me = e^{\\mu}, \\qquad E(X) = e^{\\mu + \\sigma^2/2}, \\qquad Var(X) = \\left(e^{\\sigma^2} - 1\\right) \\cdot e^{2\\mu + \\sigma^2}$$"
    )),

    figure_panel(
      label = "Ryc. 4.7",
      title = "Rozkład LogN(μ, σ)",
      full_width = TRUE,
      lc_toolbar(
        checkboxGroupInput("ch4_lnorm_scenarios", "Scenariusze",
            choices = c(
              "Czas reakcji: LogN(0, 0.3)" = "lnorm_1",
              "Ceny akcji: LogN(1, 0.5)" = "lnorm_2",
              "Dochody: LogN(2, 0.8)" = "lnorm_3",
              "Duża zmienność: LogN(1, 1)" = "lnorm_4"
            ),
            selected = "lnorm_2"
          )
      ),
      lc_plot("ch4_lnorm_plot", max_height = "400px"),
      uiOutput("ch4_lnorm_stats")
    ),

    lc_p("Prawy ogon sprawia, że wartość oczekiwana jest zawsze większa od ",
      gloss("mediana", "mediany"), ". Dla LogN(1, 0.5) mediana wynosi
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

    lc_p("Rozkład normalny pojawiał się w tym rozdziale wielokrotnie: jako
      domyślny przykład gęstości, jako punkt odniesienia dla t-Studenta,
      jako cegiełka rozkładu χ² i jako rozkład logarytmu w modelu
      log-normalnym. W następnym rozdziale zajmiemy się nim osobno."),

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

ch4_t_defs <- list(
  t_1 = list(label = "t(df=1) — Cauchy", df = 1),
  t_2 = list(label = "t(df=3)", df = 3),
  t_3 = list(label = "t(df=5)", df = 5),
  t_4 = list(label = "t(df=30) ≈ normalny", df = 30)
)

ch4_chisq_defs <- list(
  chisq_1 = list(label = "χ²(df=2)", df = 2),
  chisq_2 = list(label = "χ²(df=5)", df = 5),
  chisq_3 = list(label = "χ²(df=10)", df = 10),
  chisq_4 = list(label = "χ²(df=20)", df = 20)
)

ch4_lnorm_defs <- list(
  lnorm_1 = list(label = "Czas reakcji: LogN(0, 0.3)", mu = 0, sigma = 0.3),
  lnorm_2 = list(label = "Ceny akcji: LogN(1, 0.5)", mu = 1, sigma = 0.5),
  lnorm_3 = list(label = "Dochody: LogN(2, 0.8)", mu = 2, sigma = 0.8),
  lnorm_4 = list(label = "Duża zmienność: LogN(1, 1)", mu = 1, sigma = 1)
)

# --------------------------------------------------------------------------
# Chapter 4 Server
# --------------------------------------------------------------------------

ch4_server <- function(input, output, session) {

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

  # Stała rama z pełnej próby: oś X wspólna dla kroków, oś Y wspólna dla
  # kroków na skali gęstości (5–7). Zapas 14% mieści skrajne słupki 5 binów.
  ch4_step_frame <- reactive({
    data <- ch4_sample_data()
    df <- data.frame(x = data)
    x_pad <- diff(range(data)) * 0.14
    hist_max <- function(bins, stat) {
      max(layer_data(ggplot(df, aes(x = x)) + geom_histogram(bins = bins))[[stat]])
    }
    dens_max <- max(hist_max(30, "density"), max(density(data)$y))
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
        step_layer(geom_density, "new", linewidth = 1.5) +
        labs(x = "Wartość", y = "Gęstość") +
        dens_frame
    } else {
      ggplot(df, aes(x = x)) +
        step_layer(geom_density, "known", fill = STEP_ROLES$data$colour,
                   linewidth = 1.2, alpha = 0.3) +
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

  # --- Widget 2: Prawdopodobieństwo = pole ---
  observeEvent(input$ch4_area_dist, {
    if (input$ch4_area_dist == "norm") {
      updateSliderInput(session, "ch4_area_a", min = -4, max = 4, value = -1, step = 0.1)
      updateSliderInput(session, "ch4_area_b", min = -4, max = 4, value = 1, step = 0.1)
    } else if (input$ch4_area_dist == "exp") {
      updateSliderInput(session, "ch4_area_a", min = 0, max = 8, value = 0, step = 0.1)
      updateSliderInput(session, "ch4_area_b", min = 0, max = 8, value = 2, step = 0.1)
    } else {
      updateSliderInput(session, "ch4_area_a", min = 0, max = 10, value = 2, step = 0.1)
      updateSliderInput(session, "ch4_area_b", min = 0, max = 10, value = 7, step = 0.1)
    }
  })

  zoom_plot_server("ch4_area_plot", reactive({
    dist <- input$ch4_area_dist
    a <- input$ch4_area_a
    b <- input$ch4_area_b

    if (dist == "norm") {
      x_range <- c(-4, 4)
      dfn <- function(x) dnorm(x)
      prob <- pnorm(b) - pnorm(a)
    } else if (dist == "exp") {
      x_range <- c(0, 8)
      dfn <- function(x) dexp(x)
      prob <- pexp(b) - pexp(a)
    } else {
      x_range <- c(0, 10)
      dfn <- function(x) dunif(x, 0, 10)
      prob <- punif(b, 0, 10) - punif(a, 0, 10)
    }

    x_seq <- seq(x_range[1], x_range[2], length.out = 500)
    df_curve <- data.frame(x = x_seq, y = dfn(x_seq))

    shade_x <- seq(max(a, x_range[1]), min(b, x_range[2]), length.out = 300)
    shade_df <- data.frame(x = shade_x, y = dfn(shade_x))

    ggplot() +
      geom_area(data = shade_df, aes(x = x, y = y),
                fill = unname(upwr_cat["niebo"]), alpha = 0.35) +
      geom_line(data = df_curve, aes(x = x, y = y),
                color = upwr_secondary, linewidth = 1.2) +
      geom_vline(xintercept = a, color = unname(upwr_cat["terakota"]), linetype = "dashed") +
      geom_vline(xintercept = b, color = unname(upwr_cat["terakota"]), linetype = "dashed") +
      annotate("text", x = (a + b) / 2, y = max(dfn(x_seq)) * 0.5,
               label = sprintf("P = %.4f", prob),
               size = 6, fontface = "bold", color = upwr_secondary) +
      labs(
           x = "x", y = "f(x)") +
      theme_upwr()
  }))

  output$ch4_area_stats <- renderUI({
    dist <- input$ch4_area_dist
    a <- input$ch4_area_a
    b <- input$ch4_area_b

    if (dist == "norm") {
      prob <- pnorm(b) - pnorm(a)
    } else if (dist == "exp") {
      prob <- pexp(b) - pexp(a)
    } else {
      prob <- punif(b, 0, 10) - punif(a, 0, 10)
    }

    tagList(
      lc_readout(paste0("P(", a, " < X < ", b, ")"), sprintf("%.4f", max(0, prob)), color = unname(upwr_cat["niebo"])),
      lc_readout("Procent", paste0(sprintf("%.1f", max(0, prob) * 100), "%"), color = upwr_secondary)
    )
  })

  # --- Widget 3: Jednostajny — scenariusze overlay ---
  zoom_plot_server("ch4_unif_plot", reactive({
    selected <- input$ch4_unif_scenarios
    req(length(selected) > 0)

    dfs <- lapply(seq_along(selected), function(i) {
      s <- ch4_unif_defs[[selected[i]]]
      x_seq <- seq(-1, 12, length.out = 1000)
      y_seq <- dunif(x_seq, s$a, s$b)
      data.frame(x = x_seq, y = y_seq, scenario = s$label)
    })
    df <- do.call(rbind, dfs)
    df$scenario <- factor(df$scenario, levels = sapply(ch4_unif_defs[selected], `[[`, "label"))

    n_sel <- length(selected)
    colors <- setNames(upwr_cat_n(n_sel),
                       sapply(ch4_unif_defs[selected], `[[`, "label"))

    ggplot(df, aes(x = x, y = y, color = scenario, fill = scenario)) +
      geom_area(alpha = 0.15, position = "identity") +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = colors, name = NULL) +
      scale_fill_manual(values = colors, guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(
           x = "x", y = "f(x)") +
      theme_upwr() +
      theme(legend.position = "top", legend.text = element_text(size = 11))
  }))

  output$ch4_unif_stats <- renderUI({
    selected <- input$ch4_unif_scenarios
    req(length(selected) > 0)

    stats <- lapply(selected, function(id) {
      s <- ch4_unif_defs[[id]]
      mu <- (s$a + s$b) / 2
      sd_val <- sqrt((s$b - s$a)^2 / 12)
      paste0(s$label, ":  E(X) = ", round(mu, 1), ",  SD = ", round(sd_val, 2))
    })
    tags$ul(lapply(stats, tags$li))
  })

  # --- Widget 3b: Wykładniczy — scenariusze overlay ---
  zoom_plot_server("ch4_exp_plot", reactive({
    selected <- input$ch4_exp_scenarios
    req(length(selected) > 0)

    x_max <- max(sapply(selected, function(id) qexp(0.99, ch4_exp_defs[[id]]$lambda)))

    dfs <- lapply(seq_along(selected), function(i) {
      s <- ch4_exp_defs[[selected[i]]]
      x_seq <- seq(0, x_max, length.out = 500)
      y_seq <- dexp(x_seq, rate = s$lambda)
      data.frame(x = x_seq, y = y_seq, scenario = s$label)
    })
    df <- do.call(rbind, dfs)
    df$scenario <- factor(df$scenario, levels = sapply(ch4_exp_defs[selected], `[[`, "label"))

    n_sel <- length(selected)
    colors <- setNames(upwr_cat_n(n_sel),
                       sapply(ch4_exp_defs[selected], `[[`, "label"))

    ggplot(df, aes(x = x, y = y, color = scenario, fill = scenario)) +
      geom_area(alpha = 0.15, position = "identity") +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = colors, name = NULL) +
      scale_fill_manual(values = colors, guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(
           x = "x", y = "f(x)") +
      theme_upwr() +
      theme(legend.position = "top", legend.text = element_text(size = 11))
  }))

  output$ch4_exp_stats <- renderUI({
    selected <- input$ch4_exp_scenarios
    req(length(selected) > 0)

    stats <- lapply(selected, function(id) {
      s <- ch4_exp_defs[[id]]
      mu <- 1 / s$lambda
      paste0(s$label, ":  E(X) = 1/λ = ", round(mu, 2), ",  SD = ", round(mu, 2))
    })
    tags$ul(lapply(stats, tags$li))
  })

  # --- Widget 4: t-Studenta — scenariusze overlay ---
  zoom_plot_server("ch4_t_plot", reactive({
    selected <- input$ch4_t_scenarios
    show_normal <- input$ch4_t_show_normal
    req(length(selected) > 0 || show_normal)

    x_seq <- seq(-5, 5, length.out = 500)

    dfs <- lapply(seq_along(selected), function(i) {
      s <- ch4_t_defs[[selected[i]]]
      data.frame(x = x_seq, y = dt(x_seq, df = s$df), scenario = s$label)
    })
    df <- do.call(rbind, dfs)

    n_sel <- length(selected)
    colors <- setNames(upwr_cat_n(n_sel),
                       sapply(ch4_t_defs[selected], `[[`, "label"))

    if (show_normal) {
      df_norm <- data.frame(x = x_seq, y = dnorm(x_seq), scenario = "N(0, 1)")
      df <- rbind(df, df_norm)
      colors <- c(colors, "N(0, 1)" = "#999999")
    }

    df$scenario <- factor(df$scenario, levels = unique(df$scenario))

    ggplot(df, aes(x = x, y = y, color = scenario, fill = scenario)) +
      geom_area(alpha = 0.15, position = "identity") +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = colors, name = NULL) +
      scale_fill_manual(values = colors, guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(
           x = "x", y = "f(x)") +
      theme_upwr() +
      theme(legend.position = "top", legend.text = element_text(size = 11))
  }))

  output$ch4_t_stats <- renderUI({
    selected <- input$ch4_t_scenarios
    req(length(selected) > 0)

    stats <- lapply(selected, function(id) {
      s <- ch4_t_defs[[id]]
      mu_text <- if (s$df > 1) "E(X) = 0" else "E(X) = niezdef."
      sd_text <- if (s$df > 2) {
        paste0("SD = ", round(sqrt(s$df / (s$df - 2)), 2))
      } else "SD = ∞"
      paste0(s$label, ":  ", mu_text, ",  ", sd_text)
    })
    tags$ul(lapply(stats, tags$li))
  })

  # --- Widget 5: Chi-kwadrat — scenariusze overlay ---
  zoom_plot_server("ch4_chisq_plot", reactive({
    selected <- input$ch4_chisq_scenarios
    req(length(selected) > 0)

    x_max <- max(sapply(selected, function(id) qchisq(0.99, ch4_chisq_defs[[id]]$df)))
    x_seq <- seq(0.01, x_max, length.out = 500)

    dfs <- lapply(seq_along(selected), function(i) {
      s <- ch4_chisq_defs[[selected[i]]]
      data.frame(x = x_seq, y = dchisq(x_seq, df = s$df), scenario = s$label)
    })
    df <- do.call(rbind, dfs)
    df$scenario <- factor(df$scenario, levels = sapply(ch4_chisq_defs[selected], `[[`, "label"))

    n_sel <- length(selected)
    colors <- setNames(upwr_cat_n(n_sel),
                       sapply(ch4_chisq_defs[selected], `[[`, "label"))

    ggplot(df, aes(x = x, y = y, color = scenario, fill = scenario)) +
      geom_area(alpha = 0.15, position = "identity") +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = colors, name = NULL) +
      scale_fill_manual(values = colors, guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(
           x = "x", y = "f(x)") +
      theme_upwr() +
      theme(legend.position = "top", legend.text = element_text(size = 11))
  }))

  output$ch4_chisq_stats <- renderUI({
    selected <- input$ch4_chisq_scenarios
    req(length(selected) > 0)

    stats <- lapply(selected, function(id) {
      s <- ch4_chisq_defs[[id]]
      paste0(s$label, ":  E(X) = ", s$df, ",  SD = ", round(sqrt(2 * s$df), 2))
    })
    tags$ul(lapply(stats, tags$li))
  })

  # --- Widget 6: Log-normalny — scenariusze overlay ---
  zoom_plot_server("ch4_lnorm_plot", reactive({
    selected <- input$ch4_lnorm_scenarios
    req(length(selected) > 0)

    # Oblicz wspólny zakres x na podstawie wybranych scenariuszy
    x_max <- max(sapply(selected, function(id) {
      s <- ch4_lnorm_defs[[id]]
      qlnorm(0.99, s$mu, s$sigma)
    }))

    x_seq <- seq(0.001, x_max, length.out = 500)

    dfs <- lapply(seq_along(selected), function(i) {
      s <- ch4_lnorm_defs[[selected[i]]]
      data.frame(x = x_seq, y = dlnorm(x_seq, s$mu, s$sigma), scenario = s$label)
    })
    df <- do.call(rbind, dfs)
    df$scenario <- factor(df$scenario, levels = sapply(ch4_lnorm_defs[selected], `[[`, "label"))

    n_sel <- length(selected)
    colors <- setNames(upwr_cat_n(n_sel),
                       sapply(ch4_lnorm_defs[selected], `[[`, "label"))

    ggplot(df, aes(x = x, y = y, color = scenario, fill = scenario)) +
      geom_area(alpha = 0.15, position = "identity") +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = colors, name = NULL) +
      scale_fill_manual(values = colors, guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(
           x = "x", y = "f(x)") +
      theme_upwr() +
      theme(legend.position = "top", legend.text = element_text(size = 11))
  }))

  output$ch4_lnorm_stats <- renderUI({
    selected <- input$ch4_lnorm_scenarios
    req(length(selected) > 0)

    stats <- lapply(selected, function(id) {
      s <- ch4_lnorm_defs[[id]]
      ev <- exp(s$mu + s$sigma^2 / 2)
      med <- exp(s$mu)
      sd_val <- sqrt((exp(s$sigma^2) - 1) * exp(2 * s$mu + s$sigma^2))
      paste0(s$label, ":  E(X) = ", round(ev, 1),
             ",  Me = ", round(med, 1),
             ",  SD = ", round(sd_val, 1))
    })
    tags$ul(lapply(stats, tags$li))
  })

}
