# ============================================================================
# CHAPTER 3: Rozkłady dyskretne
# ============================================================================

ch3_ui <- list(
  id = "ch-dyskretne", num = "03", title = "Rozkłady dyskretne",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 03 · Rozkłady prawdopodobieństwa",
      num    = "03",
      title  = "Rozkłady dyskretne.",
      lead   = "Rzut kostką, orły w serii rzutów monetą, klienci w sklepie w ciągu
                godziny, próby do pierwszego sukcesu. Za większością zliczeń stoi
                jeden z czterech mechanizmów, a każdy z nich ma gotowy wzór
                na prawdopodobieństwa, wartość oczekiwaną i wariancję."
    ),

    lc_h2("ch3-intro", "Rozkłady dyskretne"),

    lc_p("W poprzednim rozdziale wartość oczekiwaną i wariancję liczyliśmy
      z tabeli: każdą możliwą wartość mnożyliśmy przez jej prawdopodobieństwo
      i sumowaliśmy. Taką tabelę trzeba jednak najpierw mieć. Na szczęście wiele
      zupełnie różnych sytuacji powstaje według tego samego schematu: liczba
      orłów w rzutach monetą i liczba wadliwych sztuk w partii towaru różnią się
      tylko liczbami, a nie mechanizmem. Wystarczy więc raz opisać mechanizm,
      a konkretną sytuację wskazać kilkoma liczbami, które nazywamy parametrami
      rozkładu."),

    lc_p("W tym rozdziale zajmujemy się zmiennymi, które przyjmują wartości
      oddzielone od siebie, najczęściej liczby całkowite 0, 1, 2, … W wykładzie
      o typach danych nazywaliśmy je ",
      gloss("zmienna dyskretna", "zmiennymi dyskretnymi"), ": powstają przez
      liczenie, a nie przez pomiar. Rozkład takiej zmiennej opisuje ",
      gloss("funkcja prawdopodobieństwa", "funkcja prawdopodobieństwa"),
      " P(X = k), która każdej możliwej wartości k przypisuje jej
      prawdopodobieństwo. Poznamy cztery klasyczne rozkłady: jednostajny,
      dwumianowy, Poissona i geometryczny. Dla każdego zapytamy, w jakiej
      sytuacji powstaje, jak wygląda jego funkcja prawdopodobieństwa i jak
      E(X) oraz Var(X) zależą od parametrów."),

    # ========================================================================
    # WIDGET 1: Rozkład jednostajny dyskretny
    # ========================================================================
    lc_h2("ch3-jednostajny", "Rozkład jednostajny dyskretny"),

    lc_p("Najprostsza sytuacja to taka, w której żaden wynik nie jest
      wyróżniony. Rzut symetryczną kostką, rzut monetą, losowanie numeru
      z urny: zmienna przyjmuje wartości 1, 2, …, n i każda z nich ma tę samą
      szansę. Taki rozkład nazywamy ",
      gloss("rozkład jednostajny", "jednostajnym"), ". Jedynym parametrem
      jest n, liczba możliwych wyników, a prawdopodobieństwo każdego wyniku
      to po prostu 1/n."),

    lc_formula_box(withMathJax(
      "$$P(X = k) = \\frac{1}{n}, \\quad E(X) = \\frac{n+1}{2}, \\quad Var(X) = \\frac{n^2 - 1}{12}$$"
    )),

    lc_p("Dla zwykłej kostki n = 6, więc każda ściana ma prawdopodobieństwo
      1/6 ≈ 0.167. Wartość oczekiwana wynosi (6 + 1)/2 = 3.5, czyli dokładnie
      środek zakresu, a wariancja (36 - 1)/12 ≈ 2.92, co daje SD ≈ 1.71.
      Panel poniżej
      symuluje serię rzutów i porównuje częstości względne z teoretycznym
      prawdopodobieństwem 1/n (linia przerywana)."),

    figure_panel(
      label = "Ryc. 3.1",
      title = "Symulacja: moneta i kostka",
      full_width = TRUE,
      lc_toolbar(
        lc_segmented("ch3_unif_type", "Eksperyment", choices = c("Moneta (2 wyniki)" = "coin",
                        "Kostka (6 wyników)" = "die",
                        "Kostka 12-ścienna" = "d12"), selected = "die"),
        lc_slider("ch3_unif_n", "Liczba prób", 10, 5000, 100, 10),
        lc_action("ch3_unif_sim", "Symuluj", variant = "solid")
      ),
      lc_plot("ch3_unif_plot", max_height = "350px")
    ),

    lc_p("Przy 100 rzutach słupki wyraźnie odstają od linii 1/6: jedne ściany
      wypadają częściej, inne rzadziej, choć kostka jest uczciwa. Przy kilku
      tysiącach rzutów wszystkie słupki układają się tuż przy linii. To ten sam
      mechanizm, który w pierwszym rozdziale prowadził od częstości do
      prawdopodobieństwa: rozkład teoretyczny opisuje, do czego zbliża się
      histogram, gdy prób jest coraz więcej. Parametr n zmienia tylko liczbę
      słupków i ich wysokość. Im więcej wyników, tym niższy każdy słupek
      (1/2 dla monety, 1/12 dla kostki dwunastościennej), a wartość oczekiwana
      przesuwa się do środka nowego zakresu: 1.5 dla monety z wynikami 1 i 2,
      6.5 dla kostki dwunastościennej."),

    # ========================================================================
    # WIDGET 2: Rozkład dwumianowy — scenariusze overlay
    # ========================================================================
    lc_h2("ch3-dwumianowy", "Rozkład dwumianowy (Binomial)"),

    lc_p("Rozkład jednostajny opisuje pojedynczy rzut. Częściej interesuje
      nas jednak wynik całej serii: ile orłów wypadnie w 10 rzutach monetą,
      ile osób z 20 zda egzamin, ile sztuk z partii 50 okaże się wadliwych.
      Każde pojedyncze doświadczenie ma tu dwa wyniki, sukces albo porażkę,
      i nazywamy je ", gloss("próba Bernoulliego", "próbą Bernoulliego"), ".
      Liczba sukcesów w serii prób ma ",
      gloss("rozkład dwumianowy", "rozkład dwumianowy"), " B(n, p), jeśli
      spełnione są cztery warunki: liczba prób n jest ustalona z góry, każda
      próba kończy się sukcesem albo porażką, prawdopodobieństwo sukcesu p
      jest w każdej próbie takie samo, a próby są od siebie niezależne."),

    lc_formula_box(withMathJax(
      "$$P(X = k) = \\binom{n}{k} p^k (1-p)^{n-k}, \\quad E(X) = np, \\quad Var(X) = np(1-p)$$"
    )),

    lc_p("Wzór czyta się w trzech kawałkach. Czynnik \\(p^k (1-p)^{n-k}\\) to
      prawdopodobieństwo jednego konkretnego ciągu k sukcesów i n − k porażek.
      Współczynnik \\(\\binom{n}{k}\\) liczy, na ile sposobów można rozmieścić
      te k sukcesów wśród n prób. Dla 10 rzutów monetą prawdopodobieństwo
      dokładnie 5 orłów wynosi \\(\\binom{10}{5} \\cdot 0.5^{10} = 252/1024 \\approx 0.246\\).
      Wartość oczekiwana to 10 · 0.5 = 5 orłów, wariancja 10 · 0.5 · 0.5 = 2.5,
      a SD ≈ 1.58. Na wykresie można nałożyć na siebie cztery scenariusze."),

    figure_panel(
      label = "Ryc. 3.2",
      title = "Rozkład dwumianowy B(n, p)",
      full_width = TRUE,
      lc_toolbar(
        checkboxGroupInput("ch3_binom_scenarios", "Scenariusze",
            choices = c(
              "Moneta: B(10, 0.5)" = "binom_1",
              "Egzamin: B(20, 0.25)" = "binom_2",
              "Jakość: B(50, 0.1)" = "binom_3",
              "Sukces: B(20, 0.7)" = "binom_4"
            ),
            selected = "binom_1"
          )
      ),
      lc_plot("ch3_binom_plot", max_height = "400px"),
      uiOutput("ch3_binom_stats")
    ),

    lc_p("Rozkład B(10, 0.5) jest symetryczny wokół 5, bo przy p = 0.5 sukces
      i porażka są zamienne. Trzy pierwsze scenariusze mają tę samą wartość
      oczekiwaną: 10 · 0.5 = 20 · 0.25 = 50 · 0.1 = 5. Mimo to ich kształty
      się różnią. Im mniejsze p, tym rozkład szerszy (SD rośnie od 1.58
      przez 1.94 do 2.12) i tym wyraźniej wydłuża się jego prawy ogon. Wynika
      to wprost ze wzoru na wariancję: przy tej samej wartości np czynnik
      (1 − p) jest bliższy 1, gdy p jest małe. Scenariusz egzaminu to student,
      który zgaduje odpowiedzi w teście z 20 pytaniami po 4 warianty. Zgadując,
      zdobędzie przeciętnie 5 punktów, a szansa na co najmniej 10 wynosi tylko
      1.4%. Scenariusz B(20, 0.7)
      pokazuje sytuację odwrotną: przy p powyżej 0.5 środek przesuwa się
      w prawo, do E(X) = 14, a dłuższy ogon pojawia się po lewej stronie."),

    # ========================================================================
    # WIDGET 3: Rozkład Poissona — scenariusze overlay
    # ========================================================================
    lc_h2("ch3-poisson", "Rozkład Poissona"),

    lc_p("Rozkład dwumianowy wymaga, żebyśmy znali liczbę prób n. W wielu
      zliczeniach jej nie ma. Ilu klientów wejdzie do sklepu w ciągu godziny?
      Ile literówek znajdzie się na stronie tekstu? Potencjalnych klientów
      są tysiące, a każdy z nich wchodzi z bardzo małym prawdopodobieństwem.
      Nie znamy ani n, ani p, znamy tylko średnią liczbę zdarzeń w danym
      przedziale. Tę jedną liczbę oznaczamy λ (lambda). Jeśli zdarzenia
      zachodzą niezależnie od siebie, pojedynczo i ze stałym średnim tempem,
      to liczba zdarzeń w ustalonym przedziale czasu lub przestrzeni ma ",
      gloss("rozkład Poissona", "rozkład Poissona"), " Pois(λ)."),

    lc_formula_box(withMathJax(
      "$$P(X = k) = \\frac{\\lambda^k e^{-\\lambda}}{k!}, \\quad E(X) = \\lambda, \\quad Var(X) = \\lambda$$"
    )),

    lc_p("Rozkład Poissona to granica rozkładu dwumianowego, gdy n jest bardzo
      duże, p bardzo małe, a iloczyn np = λ pozostaje stały. Dla B(1000, 0.002)
      prawdopodobieństwo dokładnie 2 sukcesów wynosi 0.2709, a dla Pois(2)
      0.2707. Rozkład nie ma górnej
      granicy, bo k może być dowolnie duże, ale prawdopodobieństwa dużych
      wartości szybko maleją. Dla λ = 2 szansa na zero zdarzeń wynosi
      e⁻² ≈ 0.135, na co najwyżej 3 zdarzenia 0.857, a na 5 lub więcej
      tylko 0.053."),

    figure_panel(
      label = "Ryc. 3.3",
      title = "Rozkład Poissona Pois(λ)",
      full_width = TRUE,
      lc_toolbar(
        checkboxGroupInput("ch3_pois_scenarios", "Scenariusze",
            choices = c(
              "Wypadki: λ = 0.5" = "pois_1",
              "Błędy: λ = 2" = "pois_2",
              "Klienci: λ = 5" = "pois_3",
              "Wiadomości: λ = 10" = "pois_4"
            ),
            selected = "pois_2"
          )
      ),
      lc_plot("ch3_pois_plot", max_height = "400px"),
      uiOutput("ch3_pois_stats")
    ),

    lc_p("Parametr λ jednocześnie przesuwa i poszerza rozkład, bo wartość
      oczekiwana i wariancja są równe λ. Przy λ = 0.5 najczęstszym wynikiem
      jest zero (prawdopodobieństwo 0.61), a rozkład jest silnie prawoskośny.
      Wraz ze wzrostem λ środek przesuwa się w prawo, SD = √λ rośnie od 0.71
      do 3.16, a rozkład staje się coraz bardziej symetryczny. Równość
      E(X) = Var(X) daje praktyczny test: jeśli w danych ze zliczeń średnia
      jest zbliżona do wariancji, model Poissona jest dobrym kandydatem.
      Jeśli wariancja jest wyraźnie większa, zdarzenia prawdopodobnie nie są
      niezależne, na przykład pojawiają się seriami."),

    lc_p("Związek z rozkładem dwumianowym widać, gdy obok scenariusza
      „Klienci: λ = 5” postawić B(50, 0.1) z poprzedniego wykresu. Oba
      rozkłady mają wartość oczekiwaną 5 i bardzo podobny kształt:
      P(X = 5) wynosi 0.185 dla dwumianowego i 0.175 dla Poissona. Różnica
      zmaleje, jeśli przy tym samym np = 5 zwiększymy n i zmniejszymy p."),

    # ========================================================================
    # WIDGET 4: Rozkład geometryczny — scenariusze overlay
    # ========================================================================
    lc_h2("ch3-geometryczny", "Rozkład geometryczny"),

    lc_p("Rozkład dwumianowy i rozkład Poissona odpowiadają na pytanie „ile
      sukcesów?”. Można zapytać odwrotnie: ile prób trzeba wykonać, żeby
      doczekać się pierwszego sukcesu? Ile rzutów kostką do pierwszej
      szóstki, ile wysłanych CV do pierwszego zaproszenia na rozmowę? Założenia
      są te same co w rozkładzie dwumianowym, czyli niezależne próby
      Bernoulliego ze stałym p, ale tym razem to liczba prób jest zmienną
      losową, a liczba sukcesów jest ustalona i wynosi 1. Numer próby,
      w której pada pierwszy sukces, ma ",
      gloss("rozkład geometryczny", "rozkład geometryczny"), " Geom(p)."),

    lc_formula_box(withMathJax(
      "$$P(X = k) = (1-p)^{k-1} \\cdot p, \\quad E(X) = \\frac{1}{p}, \\quad Var(X) = \\frac{1-p}{p^2}$$"
    )),

    lc_p("Żeby pierwszy sukces padł w próbie k, najpierw musi się zdarzyć
      k - 1 porażek, każda z prawdopodobieństwem 1 − p, a potem jeden sukces.
      Dla kostki p = 1/6, więc przeciętnie czekamy 1/p = 6 rzutów, przy
      SD ≈ 5.48. Szansa, że szóstka padnie w ciągu pierwszych sześciu rzutów,
      wynosi 1 − (5/6)⁶ ≈ 0.665, a że nie padnie przez 10 rzutów,
      (5/6)¹⁰ ≈ 0.162."),

    figure_panel(
      label = "Ryc. 3.4",
      title = "Rozkład geometryczny Geom(p)",
      full_width = TRUE,
      lc_toolbar(
        checkboxGroupInput("ch3_geom_scenarios", "Scenariusze",
            choices = c(
              "Rzadkie: p = 0.05" = "geom_1",
              "Szóstka: p = 1/6" = "geom_2",
              "Częste: p = 0.3" = "geom_3",
              "Moneta: p = 0.5" = "geom_4"
            ),
            selected = "geom_2"
          )
      ),
      lc_plot("ch3_geom_plot", max_height = "400px"),
      uiOutput("ch3_geom_stats")
    ),

    lc_p("Każdy rozkład geometryczny ma najwyższy słupek przy k = 1, a kolejne
      maleją, bo każda następna próba wymaga jeszcze jednej porażki więcej.
      Parametr p decyduje o tym, jak szybko. Przy p = 0.5 słupki spadają
      o połowę z każdym krokiem i przeciętnie czekamy 2 próby. Przy p = 0.05
      spadek jest powolny, rozkład ma bardzo długi prawy ogon, a wartość
      oczekiwana to 20 prób przy SD ≈ 19.5. Rzadkie zdarzenia oznaczają nie
      tylko długie czekanie, ale też bardzo nieprzewidywalne."),

    lc_p("Rozkład geometryczny ma też nieintuicyjną własność, którą nazywamy ",
      gloss("bezpamięciowość", "bezpamięciowością"), ". Jeśli w 10 rzutach
      nie wypadła szóstka, to szansa na szóstkę w następnym rzucie nadal
      wynosi 1/6, a oczekiwana liczba dalszych rzutów nadal wynosi 6.
      Kostka nie pamięta wcześniejszych porażek i nie jest nam winna
      szóstki."),

    # ========================================================================
    # WIDGET 5: Porównanie czterech rozkładów
    # ========================================================================
    lc_h2("ch3-porownanie", "Porównanie czterech rozkładów"),

    lc_p("Cztery rozkłady różnią się mechanizmem, więc różnią się też
      kształtem. Poniżej zestawiamy po jednym przedstawicielu każdego:
      kostkę, B(20, 0.3), Pois(4) i Geom(0.2). Wartość oczekiwaną
      i przedział ±1 SD można włączyć na wykresie."),

    figure_panel(
      label = "Ryc. 3.5",
      title = "Cztery rozkłady obok siebie",
      full_width = TRUE,
      checkboxInput("ch3_compare_show_ev", "Pokaż wartość oczekiwaną (linia)", value = FALSE),
      checkboxInput("ch3_compare_show_sd", "Pokaż ± odchylenie standardowe (pas)", value = FALSE),
      lc_plot("ch3_compare_plot", ratio = "1.8/1", max_height = "350px")
    ),

    lc_p("Rozkład jednostajny jest płaski: E(X) = 3.5 i SD ≈ 1.71.
      Dwumianowy B(20, 0.3) ma kształt dzwonu z lekko wydłużonym prawym
      ogonem, E(X) = 6 i SD ≈ 2.05. Poissona Pois(4) wygląda podobnie,
      E(X) = 4 i SD = 2, ale jego prawy ogon nie ma końca. Geometryczny
      Geom(0.2) maleje od pierwszej wartości. Ma E(X) = 5, ale SD ≈ 4.47,
      czyli rozrzut prawie tak duży jak sama wartość oczekiwana. Pas ±1 SD
      sięga przy nim poniżej 1, czyli poza możliwe wartości. To sygnał,
      że przy silnie skośnych rozkładach sama para E(X) i SD nie opisuje
      dobrze kształtu."),

    inline_callout(
      label = "Zasada",
      "„Ile z n prób?” — dwumianowy. „Ile razy w ciągu godziny, na stronie,
       w miesiącu?” — Poisson. „Ile prób aż do pierwszego sukcesu?” —
       geometryczny. „Każdy wynik tak samo prawdopodobny?” — jednostajny."
    ),

    lc_p("Wszystkie cztery rozkłady opisują zliczenia, więc ich wartości są
      liczbami całkowitymi. Czas oczekiwania, wzrost czy temperatura mogą
      jednak przyjąć dowolną wartość z przedziału i do nich potrzebujemy
      innego opisu."),

    lc_chapter_next(
      num       = "04",
      title     = "Rozkłady ciągłe",
      lead      = "gdy zmienna może przyjąć dowolną wartość z pewnego przedziału.",
      target_id = "ch-ciagle"
    )
  )
)

# --------------------------------------------------------------------------
# Chapter 3 Server
# --------------------------------------------------------------------------

# Definicje scenariuszy
ch3_binom_defs <- list(
  binom_1 = list(label = "Moneta: B(10, 0.5)", n = 10, p = 0.5),
  binom_2 = list(label = "Egzamin: B(20, 0.25)", n = 20, p = 0.25),
  binom_3 = list(label = "Jakość: B(50, 0.1)", n = 50, p = 0.1),
  binom_4 = list(label = "Sukces: B(20, 0.7)", n = 20, p = 0.7)
)

ch3_pois_defs <- list(
  pois_1 = list(label = "Wypadki: λ = 0.5", lambda = 0.5),
  pois_2 = list(label = "Błędy: λ = 2", lambda = 2),
  pois_3 = list(label = "Klienci: λ = 5", lambda = 5),
  pois_4 = list(label = "Wiadomości: λ = 10", lambda = 10)
)

ch3_geom_defs <- list(
  geom_1 = list(label = "Rzadkie: p = 0.05", p = 0.05),
  geom_2 = list(label = "Szóstka: p = 1/6", p = round(1/6, 4)),
  geom_3 = list(label = "Częste: p = 0.3", p = 0.3),
  geom_4 = list(label = "Moneta: p = 0.5", p = 0.5)
)

ch3_server <- function(input, output, session) {

  # --- Widget 1: Jednostajny dyskretny (bez zmian) ---
  ch3_unif_data <- reactive({
    input$ch3_unif_sim
    req(input$ch3_unif_type, input$ch3_unif_n)
    type <- input$ch3_unif_type
    k    <- switch(type, "coin" = 2, "die" = 6, "d12" = 12)
    list(obs = sample(1:k, input$ch3_unif_n, replace = TRUE), k = k, n = input$ch3_unif_n)
  })

  zoom_plot_server("ch3_unif_plot", reactive({
    d <- ch3_unif_data()

    df <- data.frame(x = factor(d$obs, levels = 1:d$k))
    freq_df <- df %>% count(x, .drop = FALSE) %>% mutate(rel = n / sum(n))

    ggplot(freq_df, aes(x = x, y = rel)) +
      geom_col(fill = col_uniform, color = "white", alpha = 0.7) +
      geom_hline(yintercept = 1/d$k, color = unname(upwr_cat["terakota"]), linewidth = 1, linetype = "dashed") +
      geom_point(aes(y = 1/d$k), color = unname(upwr_cat["terakota"]), size = 3) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(
           
           x = "Wynik", y = "Częstość względna") +
      theme_upwr()
  }))

  # --- Widget 2: Dwumianowy — scenariusze overlay ---
  zoom_plot_server("ch3_binom_plot", reactive({
    selected <- input$ch3_binom_scenarios
    req(length(selected) > 0)

    dfs <- lapply(seq_along(selected), function(i) {
      s <- ch3_binom_defs[[selected[i]]]
      x_vals <- 0:s$n
      probs <- dbinom(x_vals, s$n, s$p)
      data.frame(x = x_vals, prob = probs, scenario = s$label)
    })
    df <- do.call(rbind, dfs)
    df$scenario <- factor(df$scenario, levels = sapply(ch3_binom_defs[selected], `[[`, "label"))

    n_sel <- length(selected)
    colors <- setNames(upwr_cat_n(n_sel),
                       sapply(ch3_binom_defs[selected], `[[`, "label"))

    dodge <- if (n_sel > 1) position_dodge(width = 0.5) else "identity"

    ggplot(df, aes(x = x, y = prob, color = scenario)) +
      geom_point(size = 4, alpha = 0.85, position = dodge) +
      geom_segment(aes(xend = x, yend = 0), linewidth = 1, alpha = 0.6, position = dodge) +
      scale_color_manual(values = colors, name = NULL) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(
           x = "Liczba sukcesów (k)", y = "P(X = k)") +
      theme_upwr() +
      theme(legend.position = "top", legend.text = element_text(size = 11))
  }))

  output$ch3_binom_stats <- renderUI({
    selected <- input$ch3_binom_scenarios
    req(length(selected) > 0)

    stats <- lapply(selected, function(id) {
      s <- ch3_binom_defs[[id]]
      mu <- s$n * s$p
      sigma <- sqrt(s$n * s$p * (1 - s$p))
      paste0(s$label, ":  E(X) = ", round(mu, 1), ",  SD = ", round(sigma, 2))
    })
    tags$ul(lapply(stats, tags$li))
  })

  # --- Widget 3: Poissona — scenariusze overlay ---
  zoom_plot_server("ch3_pois_plot", reactive({
    selected <- input$ch3_pois_scenarios
    req(length(selected) > 0)

    # Wspólny zakres x dla wszystkich scenariuszy
    x_max <- max(sapply(selected, function(id) qpois(0.999, ch3_pois_defs[[id]]$lambda)))

    dfs <- lapply(seq_along(selected), function(i) {
      s <- ch3_pois_defs[[selected[i]]]
      x_vals <- 0:x_max
      probs <- dpois(x_vals, s$lambda)
      data.frame(x = x_vals, prob = probs, scenario = s$label)
    })
    df <- do.call(rbind, dfs)
    df$scenario <- factor(df$scenario, levels = sapply(ch3_pois_defs[selected], `[[`, "label"))

    n_sel <- length(selected)
    colors <- setNames(upwr_cat_n(n_sel),
                       sapply(ch3_pois_defs[selected], `[[`, "label"))
    dodge <- if (n_sel > 1) position_dodge(width = 0.5) else "identity"

    ggplot(df, aes(x = x, y = prob, color = scenario)) +
      geom_point(size = 4, alpha = 0.85, position = dodge) +
      geom_segment(aes(xend = x, yend = 0), linewidth = 1, alpha = 0.6, position = dodge) +
      scale_color_manual(values = colors, name = NULL) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(
           x = "Liczba zdarzeń (k)", y = "P(X = k)") +
      theme_upwr() +
      theme(legend.position = "top", legend.text = element_text(size = 11))
  }))

  output$ch3_pois_stats <- renderUI({
    selected <- input$ch3_pois_scenarios
    req(length(selected) > 0)

    stats <- lapply(selected, function(id) {
      s <- ch3_pois_defs[[id]]
      paste0(s$label, ":  E(X) = Var(X) = ", s$lambda,
             ",  SD = ", round(sqrt(s$lambda), 2))
    })
    tags$ul(lapply(stats, tags$li))
  })

  # --- Widget 4: Geometryczny — scenariusze overlay ---
  zoom_plot_server("ch3_geom_plot", reactive({
    selected <- input$ch3_geom_scenarios
    req(length(selected) > 0)

    # Wspólny zakres x, ograniczony do 40
    x_max <- min(40, max(sapply(selected, function(id) {
      qgeom(0.999, ch3_geom_defs[[id]]$p) + 1
    })))

    dfs <- lapply(seq_along(selected), function(i) {
      s <- ch3_geom_defs[[selected[i]]]
      x_vals <- 1:x_max
      probs <- dgeom(x_vals - 1, s$p)
      data.frame(x = x_vals, prob = probs, scenario = s$label)
    })
    df <- do.call(rbind, dfs)
    df$scenario <- factor(df$scenario, levels = sapply(ch3_geom_defs[selected], `[[`, "label"))

    n_sel <- length(selected)
    colors <- setNames(upwr_cat_n(n_sel),
                       sapply(ch3_geom_defs[selected], `[[`, "label"))
    dodge <- if (n_sel > 1) position_dodge(width = 0.5) else "identity"

    ggplot(df, aes(x = x, y = prob, color = scenario)) +
      geom_point(size = 4, alpha = 0.85, position = dodge) +
      geom_segment(aes(xend = x, yend = 0), linewidth = 1, alpha = 0.6, position = dodge) +
      scale_color_manual(values = colors, name = NULL) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(
           x = "Numer próby (k)", y = "P(X = k)") +
      theme_upwr() +
      theme(legend.position = "top", legend.text = element_text(size = 11))
  }))

  output$ch3_geom_stats <- renderUI({
    selected <- input$ch3_geom_scenarios
    req(length(selected) > 0)

    stats <- lapply(selected, function(id) {
      s <- ch3_geom_defs[[id]]
      mu <- 1 / s$p
      sigma <- sqrt((1 - s$p) / s$p^2)
      paste0(s$label, ":  E(X) = ", round(mu, 1), ",  SD = ", round(sigma, 2))
    })
    tags$ul(lapply(stats, tags$li))
  })

  # --- Widget 5: Porównanie (bez zmian) ---
  zoom_plot_server("ch3_compare_plot", reactive({
    show_ev <- input$ch3_compare_show_ev
    show_sd <- input$ch3_compare_show_sd

    # Jednostajny: kostka
    x1 <- 1:6; p1 <- rep(1/6, 6)
    mu1 <- 3.5; sd1 <- sqrt(35/12)
    df1 <- data.frame(x = x1, prob = p1, dist = "Jednostajny\n(kostka)")

    # Dwumianowy: B(20, 0.3)
    x2 <- 0:20; p2 <- dbinom(x2, 20, 0.3)
    mu2 <- 6; sd2 <- sqrt(20*0.3*0.7)
    keep2 <- p2 > 0.001
    df2 <- data.frame(x = x2[keep2], prob = p2[keep2], dist = "Dwumianowy\nB(20, 0.3)")

    # Poissona: Pois(4)
    x3 <- 0:15; p3 <- dpois(x3, 4)
    mu3 <- 4; sd3 <- 2
    keep3 <- p3 > 0.001
    df3 <- data.frame(x = x3[keep3], prob = p3[keep3], dist = "Poissona\nPois(4)")

    # Geometryczny: Geom(0.2)
    x4 <- 1:25; p4 <- dgeom(x4 - 1, 0.2)
    mu4 <- 1/0.2; sd4 <- sqrt((1 - 0.2) / 0.2^2)
    keep4 <- p4 > 0.001
    df4 <- data.frame(x = x4[keep4], prob = p4[keep4], dist = "Geometryczny\nGeom(0.2)")

    df_all <- rbind(df1, df2, df3, df4)
    df_all$dist <- factor(df_all$dist,
                          levels = c("Jednostajny\n(kostka)", "Dwumianowy\nB(20, 0.3)",
                                     "Poissona\nPois(4)", "Geometryczny\nGeom(0.2)"))

    stats_df <- data.frame(
      dist = levels(df_all$dist),
      mu = c(mu1, mu2, mu3, mu4),
      sd = c(sd1, sd2, sd3, sd4)
    )

    pl <- ggplot(df_all, aes(x = x, y = prob)) +
      geom_col(aes(fill = dist), color = "white", alpha = 0.85, width = 0.7, show.legend = FALSE) +
      facet_wrap(~dist, scales = "free_x", nrow = 2) +
      scale_fill_manual(values = c(col_uniform, col_binomial, col_poisson, col_geometric)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = "Wartość", y = "Prawdopodobieństwo") +
      theme_upwr(base_size = 13)

    if (show_ev) {
      pl <- pl + geom_vline(data = stats_df, aes(xintercept = mu),
                            color = unname(upwr_cat["terakota"]), linewidth = 1, linetype = "dashed")
    }
    if (show_sd) {
      pl <- pl + geom_rect(data = stats_df,
                           aes(xmin = mu - sd, xmax = mu + sd, ymin = 0, ymax = Inf),
                           inherit.aes = FALSE, fill = unname(upwr_cat["terakota"]), alpha = 0.08)
    }
    pl
  }))

}
