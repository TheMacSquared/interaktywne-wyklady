# ============================================================================
# CHAPTER 6: Centralne Twierdzenie Graniczne
# ============================================================================

ch6_ui <- list(
  id = "ch-ctg", num = "06", title = "Centralne Twierdzenie Graniczne",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 06 · Rozkłady prawdopodobieństwa",
      num    = "06",
      title  = "Centralne Twierdzenie Graniczne.",
      lead   = "Rozkład normalny pojawia się w danych zbyt często, żeby był to przypadek.
                Wyjaśnia to centralne twierdzenie graniczne: średnia z wielu niezależnych
                obserwacji ma rozkład bliski normalnemu, nawet gdy pojedyncze obserwacje
                wyglądają zupełnie inaczej."
    ),

    lc_p("W poprzednim rozdziale poznaliśmy ", gloss("rozkład normalny"), " i nauczyliśmy się liczyć
      z nim prawdopodobieństwa. Rozkłady z rozdziałów 3 i 4 — dwumianowy, Poissona,
      wykładniczy — nie przypominają jednak dzwonu: są dyskretne, skośne albo
      ograniczone z jednej strony. Mimo to w praktyce rozkład normalny opisuje
      bardzo wiele zjawisk. W tym rozdziale zobaczymy, skąd się bierze: pojawia się
      za każdym razem, gdy uśredniamy lub sumujemy wiele niezależnych wartości."),

    lc_p("Zacznijmy od krótkiego filmu o smokach i królikach, który pokazuje,
      skąd bierze się dzwon. W kolejnych sekcjach rozłożymy to na części."),

    figure_panel(
      label = "Film",
      title = "Wideo wprowadzające",
      full_width = TRUE,
      div(style = "position: relative; padding-bottom: 56.25%; height: 0; overflow: hidden;",
        tags$iframe(
          src = "https://www.youtube.com/embed/jvoxEYmQHNM",
          style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; border: 0;",
          allowfullscreen = NA,
          allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
        )
      )
    ),

    # ========================================================================
    # Suma rozkładów normalnych: winda (scena przed CTG)
    # ========================================================================
    lc_h2("ch6-suma", "Suma rozkładów normalnych: winda"),

    lc_p("Zanim przejdziemy do średnich, zajmijmy się sumą. W windzie wisi tabliczka:
      udźwig 1000 kg lub 13 osób. Czy 13 dorosłych mieści się w tym limicie? Załóżmy,
      że waga jednej osoby ma rozkład normalny N(80, 12), czyli średnio 80 kg
      z odchyleniem standardowym 12 kg, a osoby wsiadają do windy niezależnie od siebie."),

    # Ryc. 6.1 — scena „Winda” (suma 13 wag normalnych, przeciążenie)
    figure_panel(
      label = "Ryc. 6.1",
      title = "Winda: 13 osób i limit 1000 kg",
      width_mode = "text",
      scene_widget("ch6_winda", NULL,
        steps = c("Jeden kurs", "Wiele kursów", "Model"),
        labels = c("Wpuść 13 osób", "Wpuść 13 osób", "Wpuść 13 osób"),
        more_from = 2,
        config = c(list(kind = "lift", height = 428,
                        aria = "Trzynaście osób o losowej wadze wsiada do windy o udźwigu 1000 kg; histogram łącznej wagi z kolejnych kursów"),
                   scene_lift[c("n", "mu", "sigma", "limit", "lo", "hi", "bin", "tick")]))
    ),

    lc_p("Przeciążenie nie jest rzadkim pechem. Zdarza się w ponad czterech kursach
      na pięć: ", paste0("P(S > 1000) = ", lc_fmt(scene_lift$p_over, 3), "."), " Powód widać już
      w średnich: 13 · 80 = 1040 kg, więc przeciętny komplet przekracza limit.
      Normy dźwigowe liczą 75 kg na osobę (1000 / 75 ≈ 13.3, stąd 13 osób),
      a przeciętny dorosły waży dziś więcej."),

    lc_p("Histogram sum układa się w dzwon, i nie jest to przybliżenie. Suma
      niezależnych zmiennych o rozkładach normalnych ma dokładnie rozkład normalny.
      Dla n osób o wadze N(μ, σ) jego parametry to:"),

    lc_formula_annotated(
      "S = X_1 + X_2 + \\dots + X_n \\sim N\\left(⟦1|n\\mu⟧,\\ ⟦2|\\sigma\\sqrt{n}⟧\\right)",
      notes  = list("wartość oczekiwana sumy", "odchylenie standardowe sumy"),
      colors = c("param", "wrzos")
    ),

    lc_more("Wyprowadzenie krok po kroku",

      lc_p("Krok 1. Wartość oczekiwana sumy to suma wartości oczekiwanych.
        Ta reguła obowiązuje zawsze, także dla zmiennych zależnych:"),
      lc_formula_box(withMathJax("$$E(S) = E(X_1) + \\dots + E(X_{13}) = 13 \\cdot 80 = 1040 \\text{ kg}$$")),

      lc_p("Krok 2. Dla zmiennych niezależnych dodają się wariancje, a nie
        odchylenia standardowe. Wariancja jednej wagi to 12² = 144 kg²:"),
      lc_formula_box(withMathJax("$$Var(S) = Var(X_1) + \\dots + Var(X_{13}) = 13 \\cdot 144 = 1872 \\ \\mathrm{kg}^2$$")),

      lc_p("Krok 3. Odchylenie standardowe to pierwiastek z wariancji, więc
        pierwiastek z n wychodzi przed σ:"),
      lc_formula_box(withMathJax("$$SD(S) = \\sqrt{13 \\cdot 12^2} = 12\\sqrt{13} = 43.3 \\text{ kg}$$")),

      lc_p("Krok 4. Suma zmiennych normalnych jest normalna. Obie liczby
        wstawiamy jako parametry rozkładu normalnego:"),
      lc_formula_annotated(
        "S \\sim N\\left(⟦1|13 \\cdot 80⟧,\\ ⟦2|12\\sqrt{13}⟧\\right) = N(1040,\\ 43.3)",
        notes  = list("E(S) z kroku 1", "SD(S) z kroku 3"),
        colors = c("param", "wrzos")
      ),

      lc_p("Krok 5. Standaryzujemy limit tak jak w rozdziale 5:"),
      lc_formula_box(withMathJax("$$z = \\frac{1000 - 1040}{43.3} = -0.92$$")),

      lc_p("Krok 6. Szukamy pola na prawo od z pod krzywą N(0, 1):"),
      lc_formula_box(withMathJax(paste0("$$P(S > 1000) = P(Z > -0.92) = ",
        lc_fmt(scene_lift$p_over, 3), "$$")))
    ),

    lc_p("To samo pytanie można zadać o średnią wagę w kabinie. Warunek S > 1000 kg
      oznacza to samo co X̄ = S / 13 > 76.9 kg. Po podzieleniu przez 13 dostajemy
      X̄ ~ N(80, 12 / √13) = N(80, 3.33) i to samo prawdopodobieństwo ",
      paste0(lc_fmt(scene_lift$p_over, 3), "."), " Odchylenie σ / √n wróci za chwilę jako błąd
      standardowy."),

    lc_p("Zostaje jedno założenie: wagi dorosłych nie mają dokładnie rozkładu
      normalnego, bo ich rozkład jest prawoskośny. Czy wynik dla windy nadal
      obowiązuje? Odpowiada na to centralne twierdzenie graniczne."),

    lc_h2("ch6-ctg", "Centralne Twierdzenie Graniczne (CTG)"),

    lc_p("W rozdziale 2 ", gloss("średnia"), " z próby x̄ była przybliżeniem ", gloss("wartość oczekiwana", "wartości oczekiwanej"), " E(X),
      które poprawia się wraz ze wzrostem próby. Średnia z konkretnej próby jest jednak
      wynikiem losowania: druga próba z tej samej ",
      gloss("populacja", "populacji"), " da trochę inną średnią, trzecia jeszcze inną.
      Średnia z próby jest więc ", gloss("zmienna losowa", "zmienną losową"), " i ma własny rozkład. Rozkład, w jaki
      układają się średnie z wielu prób tej samej wielkości n, nazywamy ",
      gloss("rozkład próbkowy", "rozkładem próbkowym"), " średniej."),

    lc_p("Zobaczmy to na ocenach prowadzącego."),

    # Ryc. 6.2 — scena „Zapytaj grupkę” (średnia z grupy, CTG)
    figure_panel(
      label = "Ryc. 6.2",
      title = "Zapytaj grupkę: od jednej oceny do rozkładu średniej",
      width_mode = "text",
      scene_widget("ch6_grupka", NULL,
        steps = c("Średnia X̄", "Powtarzamy", "Kształt"),
        labels = c("Zapytaj grupkę", "Zapytaj grupkę", "Zapytaj grupkę"),
        more_from = 2,
        config = list(kind = "group", n = 5, vals = I(scene_rating$vals), probs = I(scene_rating$probs),
                      mu = scene_rating$mu, sigma = scene_rating$sigma, height = 428,
                      aria = "Pięć losowych osób ocenia prowadzącego w skali 1–10; histogram średnich ocen z kolejnych grupek"))
    ),

    lc_p("Dwie własności tego rozkładu wynikają wprost z rachunku wartości oczekiwanej
      i ", gloss("wariancja", "wariancji"), ". Jeśli pojedyncza obserwacja ma wartość
      oczekiwaną μ i ", gloss("odchylenie standardowe"), " σ, a obserwacje w próbie są niezależne, to:"),

    lc_formula_annotated(
      "E(\\bar{X}) = \\mu, \\qquad ⟦1|SE⟧ = SD(\\bar{X}) = \\frac{⟦2|\\sigma⟧}{\\sqrt{⟦3|n⟧}}",
      notes  = list("błąd standardowy", "odchylenie std. obserwacji", "wielkość próby"),
      colors = c("indygo", "wrzos", "szalwia"),
      above  = 2
    ),

    lc_p("Średnie z prób skupiają się więc wokół tej samej wartości μ co pojedyncze
      obserwacje, ale są mniej rozproszone. Odchylenie standardowe średniej nazywamy ",
      gloss("błąd standardowy", "błędem standardowym"), " (SE). Maleje ono jak 1/√n,
      a nie jak 1/n: żeby zmniejszyć SE o połowę, trzeba czterokrotnie zwiększyć próbę.
      Oba wzory obowiązują dla każdego n i każdego rozkładu populacji."),

    lc_p("Wzory mówią, gdzie leży środek rozkładu średniej i jak jest szeroki, ale nie
      mówią nic o jego kształcie. Kształt opisuje ",
      gloss("centralne twierdzenie graniczne"), " (CTG). Jeśli obserwacje
      X₁, X₂, …, Xₙ są niezależne, pochodzą z tego samego rozkładu i ten rozkład ma
      skończoną wariancję σ², to wraz ze wzrostem n rozkład średniej X̄ zbliża się do
      rozkładu normalnego o średniej μ i odchyleniu standardowym σ/√n:"),

    lc_formula_annotated(
      "⟦1|\\bar{X}_n⟧ \\xrightarrow{d} ⟦2|N⟧\\left(\\mu, ⟦3|\\frac{\\sigma}{\\sqrt{n}}⟧\\right) \\quad \\text{dla } n \\to \\infty",
      notes  = list("średnia z próby", "rozkład normalny", "błąd standardowy"),
      colors = c("accent", "terakota", "indygo"),
      above  = 3
    ),

    lc_p("Najważniejsze jest to, czego twierdzenie nie wymaga: rozkład pojedynczej
      obserwacji może być dowolny — skośny, dyskretny, dwumodalny. Warunki dotyczą
      czego innego. Niezależność łamią na przykład pomiary tej samej osoby powtarzane
      w czasie. Skończona wariancja wyklucza rozkłady o tak ciężkich ogonach, że
      jedna obserwacja potrafi zdominować całą sumę; dane z pomiarów i ankiet spełniają
      ten warunek niemal zawsze."),

    lc_p("Sprawdźmy teraz twierdzenie w symulacjach."),

    # ========================================================================
    # WIDGET 1: Eksperyment CTG (kluczowy)
    # ========================================================================
    lc_h2("ch6-kostki", "Najpierw kostki: suma, która układa się w dzwon"),

    lc_p("Zanim wrócimy do średnich, zobaczmy to zjawisko na kostkach. Suma oczek
      z jednej kostki rozkłada się równomiernie, ale już z kilku kostek zaczyna
      przypominać dzwon. Średnia oczek to suma podzielona przez n, więc ma ten
      sam kształt, tylko inną skalę. Zmieniaj liczbę kostek i porównuj histogram
      z krzywą normalną."),

    figure_panel(
      label = "Ryc. 6.3",
      width_mode = "text",
      exp_widget("ch6_sumdice", "Suma oczek z n kostek", "Rzuć kostkami",
        list(kind = "sumdice", n = 1, xTitle = "X, czyli suma oczek",
             aria = "Suma oczek z n kostek i histogram powtórzeń"),
        options = list(label = "Liczba kostek", values = c(1, 2, 5, 10), selected = 1))
    ),

    lc_h2("ch6-eksperyment", "Eksperyment: średnie z dowolnego rozkładu"),

    lc_p("Twierdzenie najłatwiej sprawdzić, powtarzając losowanie wiele razy. Panel
      poniżej losuje próby z wybranej populacji, której rozkład widać na górnym
      wykresie. Z każdej próby liczy jedną średnią i odkłada ją na dolnym ", gloss("histogram", "histogramie"), ".
      Przerywana linia zaznacza μ, a od 30 zebranych średnich panel dorysowuje
      krzywą N(μ, σ/√n), czyli kształt przewidziany przez CTG. Zmiana rozkładu
      lub n czyści zebrane średnie."),

    figure_panel(
      label = "Ryc. 6.4",
      title = "Symulacja: średnie z dowolnego rozkładu → normalny",
      full_width = TRUE,
      lc_toolbar(
        selectInput("ch6_pop_dist", "Rozkład populacji",
          choices = c(
            "Jednostajny"         = "uniform",
            "Wykładniczy (skośny)" = "exponential",
            "Dwumodalny"          = "bimodal",
            "U-kształtny"          = "u_shape",
            "Kostka (dyskretny)"  = "die"
          ),
          selected = "exponential"
        ),
        lc_slider("ch6_sample_size", "Wielkość próby (n)", 1, 100, 5, 1),
        lc_action_group(ch6_take_1 = "1", ch6_take_100 = "100", ch6_take_1000 = "1000",
                        label = "Pobierz próby"),
        lc_action("ch6_reset", icon = "reset", variant = "ghost", aria_label = "Reset"),
        lc_readouts(uiOutput("ch6_sample_count"), uiOutput("ch6_means_stats"))
      ),
      lc_plot("ch6_pop_plot", ratio = "4/1", max_height = "180px"),
      lc_plot("ch6_means_plot", max_height = "300px")
    ),

    lc_p("Przy ustawieniach startowych populacja ma ", gloss("rozkład wykładniczy"), " z μ = 2
      i σ = 2, a próby liczą n = 5 obserwacji. Wzory przewidują, że średnie skupią się
      wokół 2, z odchyleniem standardowym 2/√5 ≈ 0.89. Po zebraniu 1000 średnich SD
      z symulacji wypada blisko tej wartości. Histogram średnich jest jednak nadal
      wyraźnie prawoskośny: przy tak skośnej populacji pięć obserwacji to za mało,
      żeby kształt był normalny. Gdy n rośnie, asymetria stopniowo słabnie."),

    lc_p("Najbardziej przekonujące są populacje, które z dzwonem nie mają nic wspólnego.
      W rozkładzie U-kształtnym wartości ze środka są najrzadsze, a mimo to średnie
      z prób gromadzą się właśnie w środku, bo do średniej trafiają naraz wartości
      z obu krańców. Dla populacji symetrycznych — jednostajnej, dwumodalnej,
      U-kształtnej i kostki — histogram średnich przypomina dzwon już przy małych
      próbach."),

    # ========================================================================
    # WIDGET 2: Wpływ wielkości próby
    # ========================================================================
    lc_h2("ch6-wielkosc-proby", "Wpływ wielkości próby"),

    lc_p("CTG opisuje granicę, do której zmierza rozkład średniej, gdy n rośnie.
      W praktyce mamy konkretne n i potrzebujemy wiedzieć, czy jest wystarczająco duże,
      żeby przybliżenie normalne było dobre. Odpowiedź zależy od kształtu rozkładu
      wyjściowego, przede wszystkim od jego ",
      gloss("skośność", "skośności"), ". Rycina poniżej pokazuje całą tę drogę naraz.
      Każdy wiersz mapy to osobny eksperyment: wiele razy losujemy n obserwacji,
      liczymy ich średnią i patrzymy z góry na histogram tych średnich. Wiersze się
      nie kumulują, wiersz 7 nie korzysta z wierszy 1–6. Pod mapą wybrany wiersz
      jako zwykły histogram 2000 średnich z krzywą normalną przewidzianą przez CTG."),

    s3p_e1_ui,

    lc_p("Na mapie widać oba skutki uśredniania. Rozkład się zwęża: dla czasu rozmowy
      na infolinii (μ = σ = 4 min) SE spada z 4 min dla pojedynczej rozmowy do 1.79
      dla n = 5, 0.89 dla n = 20 i 0.63 dla n = 40. I symetryzuje się: rozkład
      wykładniczy ma skośność 2, a skośność średniej maleje jak 2/√n, do 0.89 dla
      n = 5, 0.45 dla n = 20 i 0.32 dla n = 40. Asymetria słabnie więc powoli
      i najdłużej widać ją w ogonach. Dwa garby plonu z krzaka zlewają się w jeden już
      przy n = 5, a symetryczny błąd zaokrąglenia daje dzwon prawie od razu:
      przy n = 5 poza dzwonem leży 1.6% prawdopodobieństwa, dla infolinii jeszcze 11.2%."),

    lc_p("Dobrze to widać na konkretnym prawdopodobieństwie. W rozkładzie normalnym
      przedział μ ± 1.96·SE obejmuje dokładnie 95% wartości, po 2.5% zostaje
      w każdym ogonie (to dokładniejsza wersja reguły 68–95–99.7 z rozdziału 5).
      Dla średnich czasu rozmowy przy n = 5 powyżej górnej granicy leży 4.3% średnich,
      a poniżej dolnej tylko 0.04%. Przy n = 20 jest to 3.6% i 1.1%, przy n = 40 —
      3.3% i 1.6%. Łącznie poza przedziałem leży za każdym razem od 4.4% do 4.9%
      średnich, blisko 5%, ale podział między ogonami wyrównuje się dopiero przy
      dużych n."),

    lc_note("Zasada", rule = TRUE,
      "Nie ma jednej liczby obserwacji, od której przybliżenie normalne zaczyna
       działać. Im bardziej skośny rozkład wyjściowy, tym większej próby potrzeba:
       dla rozkładów symetrycznych wystarcza niewiele obserwacji, a przy silnej
       skośności (dochody, czasy oczekiwania) znacznie więcej."
    ),

    # ========================================================================
    # Dlaczego to działa — intuicja (obraz: mapa Ryc. 6.5)
    # ========================================================================
    lc_h2("ch6-dlaczego", "Dlaczego to działa? — intuicja"),

    lc_p("Mechanizm stojący za CTG jest prosty. Pojedyncza obserwacja z rozkładu
      wykładniczego bywa bardzo duża, bo prawy ogon jest długi. Żeby duża była średnia
      z kilku obserwacji, duże musiałyby być prawie wszystkie naraz, a to zdarza się
      rzadko. Zwykle wartości duże mieszają się z małymi i odchylenia w przeciwne
      strony się znoszą. Im więcej obserwacji uśredniamy, tym silniej to działa.
      Na mapie Ryc. 6.5 widać to jako prawy ogon, który blednie z wiersza na wiersz."),

    lc_p("To samo dotyczy sum, bo suma n obserwacji to n razy ich średnia. Dlatego
      rozkład normalny opisuje tak wiele zjawisk: wzrost człowieka, błąd pomiaru czy
      plon z pola są wynikiem wielu drobnych, w przybliżeniu niezależnych wpływów,
      które się sumują. Żaden z tych wpływów nie musi mieć rozkładu normalnego,
      normalny jest dopiero ich łączny efekt."),

    # PROTOTYP PUNKTU STOP (2026-10-29)
    s3p_6b_ui,

    lc_p("W praktyce mamy zwykle jedną próbę i jedną średnią, a μ nie znamy. CTG mówi,
      jak daleko ta średnia może leżeć od μ: w około 95% prób nie dalej niż 1.96·SE.
      Odwrócenie tego zdania — od średniej z próby do zakresu wiarygodnych wartości μ —
      daje ", gloss("przedział ufności"), ", któremu poświęcony jest wykład 04.
      Zobaczymy tam też, co zrobić, gdy σ również trzeba oszacować z danych."),

    # PROTOTYP PUNKTU STOP (2026-10-29)
    lc_h2("ch6-domkniecie", "Domknięcie wykładu"),

    s3p_d2_ui,

    lc_chapter_next(
      num       = "07",
      title     = "Quiz",
      lead      = "rozpoznawanie rozkładów w praktyce.",
      target_id = "ch-quiz"
    )
  )
)

# --------------------------------------------------------------------------
# Chapter 6 Server
# --------------------------------------------------------------------------

ch6_server <- function(input, output, session) {

  # --- Ryc. 6.3: suma n kostek → rozkład normalny (experiment.js) ---
  X <- exp_X
  exp_texts(input, output, "ch6_sumdice", list(
    tagList("Rzucamy n kostkami naraz i dodajemy oczka. Liczbę kostek można zmienić w każdej chwili
      (zmiana czyści zebrane rzuty). Zacznij od jednej."),
    tagList(X(), " to suma oczek ze wszystkich kostek. Przed rzutem jej nie znamy, więc to zmienna
      losowa. Dla jednej kostki zakres to 1–6, dla dziesięciu 10–60."),
    tagList("Powtarzamy rzuty i zliczamy sumy. Zmieniaj liczbę kostek i obserwuj kształt histogramu:
      jedna kostka daje płaski rozkład, kilka kostek coraz wyraźniejszy dzwon."),
    tagList("Częstości zbliżają się do rozkładu sumy, a przy większej liczbie kostek ten rozkład
      pokrywa się z krzywą normalną. Dla jednej kostki krzywa wyraźnie nie pasuje, a o tym mówi CTG:
      przybliżenie poprawia się wraz ze wzrostem n.")
  ))

  # --- Ryc. 6.1: scena „Winda” (suma rozkładów normalnych) ---
  scene_texts(input, output, "ch6_winda", list(
    tagList("Do windy wsiada 13 losowych osób. Waga każdej ma rozkład N(80, 12), a ",
      tags$code("S", .noWS = "outside"), " to łączna waga w kabinie. Wpuść kilka kompletów."),
    tagList("Każdy kurs spada żetonem nad swoją sumą. Pionowa linia to limit 1000 kg,
      a słupki za nią to kursy z przeciążeniem. Dołóż 100 i 1000."),
    tagList("Krzywa to rozkład normalny sumy wokół E(", tags$code("S", .noWS = "outside"),
      ") = 1040 kg. Pole pod krzywą na prawo od limitu to prawdopodobieństwo
      przeciążenia; porównaj je z odsetkiem z symulacji.")
  ))

  # --- PROTOTYP SCENY (2026-10-08): Zapytaj grupkę ---
  scene_texts(input, output, "ch6_grupka", list(
    tagList("Pytasz 5 losowych osób z roku: jak oceniasz prowadzącego w skali od 1 do 10? ",
      tags$code("X̄", .noWS = "outside"), " to średnia ocena grupki. Zapytaj kilka grupek."),
    tagList("Każda grupka spada żetonem nad swoją średnią. Dołóż 100 i 1000."),
    tagList("Tło to rozkład pojedynczych ocen: skośny, większość wysokich. Krzywa to rozkład normalny dla ",
      tags$code("X̄", .noWS = "outside"), ". Średnie z pięciu ocen układają się już prawie jak dzwon wokół μ = ",
      lc_fmt(scene_rating$mu, 1), ".")
  ))

  # --- Widget 1: Eksperyment CTG ---
  collected_means <- reactiveVal(numeric(0))

  take_samples <- function(k) {
    n <- input$ch6_sample_size
    dist <- input$ch6_pop_dist
    new_means <- replicate(k, {
      samp <- generate_population_sample(dist, n)
      mean(samp)
    })
    collected_means(c(collected_means(), new_means))
  }

  observeEvent(input$ch6_take_1, take_samples(1))
  observeEvent(input$ch6_take_100, take_samples(100))
  observeEvent(input$ch6_take_1000, take_samples(1000))
  observeEvent(input$ch6_reset, collected_means(numeric(0)))

  # Reset przy zmianie rozkładu lub n
  observeEvent(c(input$ch6_pop_dist, input$ch6_sample_size), {
    collected_means(numeric(0))
  })

  output$ch6_sample_count <- renderUI({
    n <- length(collected_means())
    lc_readout("Prób", n, color = unname(upwr_cat["niebo"]))
  })

  zoom_plot_server("ch6_pop_plot", reactive({
    dist <- input$ch6_pop_dist
    dist_label <- dist_names_pl[dist]

    if (dist == "die") {
      df <- data.frame(x = 1:6, prob = rep(1/6, 6))
      ggplot(df, aes(x = factor(x), y = prob)) +
        geom_col(fill = unname(upwr_cat["bursztyn"]), color = "white", alpha = 0.85, width = 0.6) +
        scale_y_continuous(limits = c(0, 0.3), expand = expansion(mult = c(0, 0))) +
        labs(x = "", y = "P(X = k)") +
        theme_upwr(base_size = 11)
    } else {
      data <- generate_population_sample(dist, 10000)
      df <- data.frame(x = data)
      ggplot(df, aes(x = x)) +
        geom_density(fill = unname(upwr_cat["bursztyn"]), color = upwr_secondary, alpha = 0.5, linewidth = 0.8) +
        labs(x = "", y = "Gęstość") +
        theme_upwr(base_size = 11)
    }
  }))

  zoom_plot_server("ch6_means_plot", reactive({
    means <- collected_means()

    if (length(means) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Pobierz próby przyciskiem powyżej",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      dist <- input$ch6_pop_dist
      n <- input$ch6_sample_size
      params <- get_population_params(dist)
      theo_mu <- params$mu
      theo_sd <- params$sigma / sqrt(n)

      df <- data.frame(x = means)

      p <- ggplot(df, aes(x = x))

      if (length(means) >= 5) {
        p <- p + geom_histogram(aes(y = after_stat(density)),
                                bins = min(50, max(10, length(means) / 5)),
                                fill = unname(upwr_cat["niebo"]), color = "white", alpha = 0.7)
      } else {
        p <- p + geom_dotplot(fill = unname(upwr_cat["niebo"]), alpha = 0.7, binwidth = theo_sd / 3)
      }

      if (length(means) >= 30 && theo_sd > 0) {
        x_range <- seq(min(means) - theo_sd, max(means) + theo_sd, length.out = 200)
        norm_df <- data.frame(x = x_range, y = dnorm(x_range, theo_mu, theo_sd))
        p <- p + geom_line(data = norm_df, aes(x = x, y = y),
                           color = unname(upwr_cat["terakota"]), linewidth = 1.5, linetype = "solid")
      }

      p + geom_vline(xintercept = theo_mu, color = unname(upwr_cat["terakota"]), linetype = "dashed") +
        labs(x = "Średnia z próby", y = "Gęstość") +
        theme_upwr()
    }
  }))

  output$ch6_means_stats <- renderUI({
    means <- collected_means()
    req(length(means) >= 2)

    dist <- input$ch6_pop_dist
    n <- input$ch6_sample_size
    params <- get_population_params(dist)
    theo_sd <- params$sigma / sqrt(n)

    tagList(
      lc_readout("Śr. średnich", round(mean(means), 3), color = unname(upwr_cat["niebo"])),
      lc_readout("SD średnich", round(sd(means), 3), color = upwr_secondary),
      lc_readout("Teoretyczne SD (σ/√n)", round(theo_sd, 3), color = unname(upwr_cat["bursztyn"]))
    )
  })

  # Ryc. 6.5 (mapa zbieżności) rysuje ec_proto.js z danych w data-config.

}
