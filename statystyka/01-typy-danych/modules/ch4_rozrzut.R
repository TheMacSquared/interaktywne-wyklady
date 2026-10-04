# ============================================================================
# CHAPTER 4: Statystyki rozrzutu
# ============================================================================

ch4_ui <- list(
  id = "ch-rozrzut", num = "04", title = "Statystyki rozrzutu",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 04 · Statystyka opisowa",
      num    = "04",
      title  = "Statystyki rozrzutu.",
      lead   = "Średnia mówi, gdzie leży środek danych, ale milczy o tym, jak daleko
                od niego leżą pojedyncze obserwacje. Dwa zbiory z tą samą średnią
                mogą wyglądać zupełnie inaczej. Różnicę między nimi mierzą
                statystyki rozrzutu."
    ),

    uiOutput("tracker_ch4"),

    lc_p("W poprzednim rozdziale streszczaliśmy dane jedną liczbą opisującą
      położenie: ", gloss("średnia", "średnią"), ", ", gloss("mediana", "medianą"), " albo ", gloss("percentyl", "percentylem"), ". Taka liczba nie mówi
      jednak, czy ", gloss("obserwacja", "obserwacje"), " skupiają się ciasno wokół środka, czy są szeroko
      rozrzucone. W tym rozdziale poznamy miary, które to opisują, zbudujemy
      wykres pudełkowy, zapowiedziany przy percentylach, i sprawdzimy, które
      miary rozrzutu są odporne na wartości odstające."),

    # ====================================================================
    # WIDGET 1: Bus scenario - "Mean is not everything"
    # ====================================================================
    lc_h2("ch4-srednia", "Średnia to nie wszystko"),

    lc_p("Wyobraź sobie dwie linie autobusowe o tym samym średnim spóźnieniu,
      równym 2 minuty. Na linii A prawie każdy kurs przyjeżdża z niewielkim,
      podobnym opóźnieniem. Na linii B większość kursów jest niemal punktualna,
      ale co jakiś czas autobus spóźnia się bardzo. Panel pokazuje rozkłady
      spóźnień z 1000 symulowanych kursów każdej linii. Odczyty nad wykresem
      podają odchylenie standardowe, miarę rozrzutu, którą zdefiniujemy
      w następnej sekcji."),

    figure_panel(
      label = "Ryc. 4.1",
      lc_step_widget("ch4_spread",
        title = "Dwie linie autobusowe — ta sama średnia, inny rozrzut",
        steps = c("Dwie linie", "Inny rozrzut", "Wcześniejsze wyjście",
                  "Konsekwencje"),
        toolbar = lc_toolbar(
          lc_step_from(3,
            lc_slider("ch4_spread_buffer", "Wyjście wcześniej o (minuty)", 0, 15, 3, 1)
          ),
          lc_readouts(uiOutput("ch4_spread_reads"))
        ),
        plot_id = "ch4_spread_plot",
        ratio = "2/1"
      )
    ),

    lc_p("Krzywa linii A jest wąska i wysoka: prawie wszystkie spóźnienia
      mieszczą się między 0 a 4 minutami, a odchylenie standardowe wynosi
      0.7 min. Krzywa linii B ma ostry szczyt tuż przy zerze i długi prawy ogon,
      a jej odchylenie standardowe to 3.0 min. Na linii B 3.4% kursów spóźnia
      się o ponad 10 minut, średnio o 13.1 min. Na linii A takie spóźnienie
      się nie zdarza."),

    lc_p("Dla pasażera to różnica zasadnicza. Linia A jest przewidywalna: wiadomo,
      kiedy autobus przyjedzie. Na linii B zwykle czeka się krócej, ale trzeba
      liczyć się z tym, że raz na kilkadziesiąt kursów pasażer spóźni się na zajęcia.
      Widać to po zapasie czasu: przy wyjściu 3 minuty wcześniej linią A
      dojeżdża się na czas w 92% kursów, linią B w 77%. Żeby dojechać na czas
      w 99% kursów, na linii A wystarczą 4 minuty zapasu, na linii B potrzeba
      około 14. Średnia tej różnicy nie widzi. Potrzebujemy liczby, która ją zmierzy."),

    # ====================================================================
    # WIDGET 2: SD step-by-step
    # ====================================================================
    lc_h2("ch4-odchylenie", "Odchylenie standardowe krok po kroku"),

    lc_p("Rozrzut to odległość obserwacji od środka danych. Dla każdej obserwacji
      liczymy więc odchylenie od średniej \\(x_i - \\bar{x}\\). Samych odchyleń
      nie można po prostu uśrednić: dodatnie i ujemne zawsze sumują się do
      zera. Dlatego podnosimy je do kwadratu, sumujemy i dzielimy przez
      \\(n - 1\\). Wynik to ", gloss("wariancja"), " \\(s^2\\). Wariancja ma
      jednostki do kwadratu (dla wzrostu: cm²), więc na koniec wyciągamy z niej
      pierwiastek. Otrzymujemy ", gloss("odchylenie standardowe"), " \\(s\\),
      wyrażone w tych samych jednostkach co dane."),

    lc_formula_box(withMathJax(
      "$$s^2 = \\frac{1}{n-1} \\sum_{i=1}^{n} (x_i - \\bar{x})^2 \\qquad s = \\sqrt{s^2}$$"
    )),

    lc_p("Dzielimy przez \\(n - 1\\), a nie przez \\(n\\), bo odchylenia liczymy od
      średniej obliczonej z tej samej ", gloss("próba", "próby"), ". Średnia próby leży zawsze
      w środku tych konkretnych danych, więc suma kwadratów odchyleń wychodzi
      nieco mniejsza niż wokół prawdziwej średniej ", gloss("populacja", "populacji"), ". Dzielenie przez
      \\(n - 1\\) koryguje to zaniżenie. Przy dużych próbach różnica jest
      niewielka."),

    lc_p("Panel przeprowadza to obliczenie na 10 losowych pomiarach wzrostu.
      W kroku drugim strzałki pokazują odchylenia od średniej, a tabela
      ich kwadraty. W kroku trzecim suma kwadratów zamienia się w wariancję
      i odchylenie standardowe."),

    figure_panel(
      label = "Ryc. 4.2",
      lc_step_widget("ch4_sd",
        title = "Obliczanie odchylenia standardowego",
        steps = c("Dane", "Odchylenia od średniej", "Wariancja i SD"),
        toolbar = lc_toolbar(
          lc_action("ch4_sd_new", "Losuj nowy zestaw", variant = "outline")
        ),
        plot_id = "ch4_sd_plot",
        ratio = "2/1",
        extra = uiOutput("ch4_sd_table")
      )
    ),

    lc_p("Najdłuższe strzałki dają największy wkład do sumy, bo podnosimy je
      do kwadratu: obserwacja dwa razy dalej od średniej waży cztery razy
      więcej. Odchylenie standardowe można czytać jako typową odległość
      obserwacji od średniej. W naszej ankiecie średni wzrost to 171.1 cm,
      a odchylenie standardowe 8.1 cm, więc wzrost typowego studenta różni się
      od średniej o kilka do kilkunastu centymetrów."),

    # ====================================================================
    # WIDGET 2b: Empirical rule (68-95-99.7)
    # ====================================================================
    lc_h2("ch4-regula", "Reguła empiryczna (68–95–99.7)"),

    lc_p("Ostatni krok panelu zaznaczył pas od \\(\\bar{x} - s\\) do
      \\(\\bar{x} + s\\). Ile danych powinno się w nim zmieścić? Dla rozkładów
      symetrycznych, o kształcie dzwonu, odpowiedź daje ",
      gloss("reguła 68-95-99.7", "reguła 68–95–99.7"), ": około 68% obserwacji leży w odległości
      najwyżej jednego odchylenia standardowego od średniej, około 95% —
      dwóch, a 99.7% — trzech."),

    lc_p("Panel nakłada te trzy pasy na ", gloss("histogram"), " wybranej zmiennej z ankiety
      i podaje, jaki odsetek danych naprawdę w nich leży."),

    figure_panel(
      label = "Ryc. 4.3",
      title = "Reguła 68–95–99.7 — czy zawsze działa?",
      selectInput("ch4_emp_var", "Wybierz zmienną:",
        choices = c("Wzrost (cm)" = "wzrost",
                    "Waga (kg)" = "waga",
                    "Czas dojazdu (min)" = "czas_dojazdu",
                    "Średnia ocen" = "srednia_ocen"),
        selected = "wzrost"
      ),
      lc_plot("ch4_emp_plot", ratio = "1.6/1", max_height = "400px"),
      uiOutput("ch4_emp_text")
    ),

    lc_p("Dla wzrostu reguła sprawdza się bardzo dobrze: w pasie ±1 SD leży
      67.5% studentów, w pasie ±2 SD — 96.5%, a w pasie ±3 SD wszyscy.
      Podobnie jest dla średniej ocen (68.5% i 96%) i wagi (64% i 97.5%)."),

    lc_p("Ciekawszy jest czas dojazdu. W pasie ±1 SD leży 71% danych, więc
      łączny odsetek się zgadza, ale rozkład nie jest symetryczny: poniżej pasa
      leży 13% obserwacji, a powyżej 16%. Pas ±3 SD sięga od -13 do 85 minut.
      Jego lewy kraniec to wartość niemożliwa, a po prawej stronie i tak
      zostają dwie obserwacje. Średnia i odchylenie standardowe opisują
      rozkład dobrze tylko wtedy, gdy jest on w przybliżeniu symetryczny.
      Dla rozkładów skośnych lepiej sięgnąć po miary oparte na ", gloss("kwartyl", "kwartylach"), "."),

    # ====================================================================
    # WIDGET 3: Boxplot builder
    # ====================================================================
    lc_h2("ch4-boxplot", "Wykres pudełkowy od podstaw"),

    lc_p("Kwartyle poznaliśmy w poprzednim rozdziale: Q1 i Q3 ograniczają
      środkowe 50% danych, a ich odległość to ",
      gloss("rozstęp międzykwartylowy"), " (IQR). Na nim opiera się ",
      gloss("wykres pudełkowy"), " (boxplot), który widzieliśmy pod histogramem
      percentyli. Pudełko rozciąga się od Q1 do Q3, a kreska w środku to
      mediana. Wąsy sięgają do najdalszych obserwacji leżących nie dalej niż
      1.5 IQR od pudełka. Punkty poza tymi granicami rysujemy osobno jako ",
      gloss("wartość odstająca", "wartości odstające"), "."),

    lc_formula_box(withMathJax(
      "$$\\text{IQR} = Q_3 - Q_1 \\qquad \\text{granice wąsów: } Q_1 - 1.5 \\cdot \\text{IQR}, \\;\\; Q_3 + 1.5 \\cdot \\text{IQR}$$"
    )),

    lc_p("Panel buduje wykres krok po kroku na 30 pomiarach wzrostu. 27 z nich
      to losowe wartości wokół 170 cm, a trzy dodaliśmy celowo: 145, 198
      i 200 cm."),

    figure_panel(
      label = "Ryc. 4.4",
      lc_step_widget("ch4_bp",
        title = "Wykres pudełkowy — budowa krok po kroku",
        steps = c("Surowe dane", "Mediana", "Kwartyle i pudełko",
                  "Wąsy i odstające", "Gotowy wykres"),
        toolbar = lc_toolbar(
          lc_action("ch4_bp_new", "Losuj nowe dane", variant = "outline")
        ),
        plot_id = "ch4_bp_plot",
        ratio = "2/1"
      )
    ),

    lc_p("Trzy dodane wartości zwykle wypadają poza granice wąsów i zostają
      oznaczone jako wartości odstające. Pudełko ich nie zauważa: kwartyle,
      tak jak mediana, zależą tylko od kolejności obserwacji. W danych
      z ankiety wzrost ma IQR = 11.5 cm, a granice wąsów to 148.2 i 194.3 cm.
      Najniższy student ma 150 cm, najwyższy 191.2 cm, więc wykres pudełkowy
      wzrostu nie pokazuje żadnej wartości odstającej."),

    lc_p("Ostatni krok zestawia gotowy wykres z histogramem. Wykres pudełkowy
      streszcza rozkład w pięciu liczbach i zajmuje mało miejsca, ale gubi
      szczegóły kształtu. Na przykład dwa szczyty rozkładu są na nim
      niewidoczne. Jego największą zaletą jest porównywanie kilku rozkładów
      obok siebie."),

    # ====================================================================
    # WIDGET 3b: Group comparison -- side-by-side boxplots
    # ====================================================================
    lc_h2("ch4-porownanie", "Porównanie grup"),

    lc_p("Dotąd opisywaliśmy cały zbiór danych naraz. Jedno z najczęstszych
      pytań w analizie danych brzmi jednak: czy grupy się różnią? Wykresy
      pudełkowe ustawione obok siebie pozwalają porównać jednocześnie
      położenie i rozrzut. Panel rysuje je dla wybranej zmiennej z podziałem
      na płeć albo kierunek studiów; pod wykresem są statystyki każdej grupy."),

    figure_panel(
      label = "Ryc. 4.5",
      title = "Wykresy pudełkowe w grupach",
      lc_toolbar(
        selectInput("ch4_grp_var", "Zmienna ilościowa",
            choices = c("Wzrost (cm)" = "wzrost",
                        "Waga (kg)" = "waga",
                        "Czas dojazdu (min)" = "czas_dojazdu",
                        "Średnia ocen" = "srednia_ocen"),
            selected = "wzrost"
            ),
            lc_segmented("ch4_grp_by", "Grupuj wg",
              choices = c("Płeć" = "plec", "Kierunek" = "kierunek")),
            checkboxInput("ch4_grp_violin", "Pokaż wykres skrzypcowy", value = FALSE),
            checkboxInput("ch4_grp_points", "Pokaż punkty", value = TRUE)
            ),
      lc_plot("ch4_grp_plot", ratio = "1.6/1", max_height = "400px"),
      uiOutput("ch4_grp_table")
    ),

    lc_p("Przy podziale wzrostu według płci pudełka się nie nakładają: Q3 kobiet
      wynosi 170.5 cm, a Q1 mężczyzn 172.2 cm. Mediany to 166.4 i 177.1 cm.
      Zwróć uwagę na rozrzut. Odchylenie standardowe w grupach wynosi 6.0 cm
      u kobiet i 6.5 cm u mężczyzn, a w całej próbie 8.1 cm. Część rozrzutu
      całej próby bierze się z różnicy między grupami, a nie ze zmienności
      wewnątrz nich."),

    lc_p("Podział według kierunku daje inny obraz. Mediany wzrostu na czterech
      kierunkach mieszczą się między 169.3 a 171.3 cm, a pudełka w dużej
      części się pokrywają. Różnice między kierunkami są małe w porównaniu
      z rozrzutem wewnątrz każdego z nich. Wykres skrzypcowy dodaje do pudełek
      kształt rozkładu, podobnie jak wygładzony histogram."),

    # ====================================================================
    # WIDGET 4: Spread measures comparison
    # ====================================================================
    lc_h2("ch4-miary", "Porównanie miar rozrzutu"),

    lc_p("Mamy już trzy miary rozrzutu. Do odchylenia standardowego i IQR
      dołóżmy najprostszą: ", gloss("rozstęp"), ", czyli różnicę między
      największą a najmniejszą wartością. W poprzednim rozdziale porównywaliśmy
      ", gloss("odporność"), " średniej i mediany. Teraz zrobimy to samo dla
      miar rozrzutu. Panel startuje od wzrostu 200 studentów z ankiety. Każde
      kliknięcie dopisuje wartość około 30 cm większą od dotychczasowego
      maksimum."),

    figure_panel(
      label = "Ryc. 4.6",
      title = "Porównanie miar rozrzutu i ich odporności",
      lc_toolbar(
        lc_action("ch4_comp_add1", "Dodaj wartość odstającą (+30 cm)", variant = "solid"),
        lc_action("ch4_comp_add5", "Dodaj 5 wartości odstających", variant = "solid"),
        lc_action("ch4_comp_reset", icon = "reset", variant = "ghost", aria_label = "Reset"),
        lc_readouts(uiOutput("ch4_comp_n"))
      ),
      lc_plot("ch4_comp_plot", ratio = "1.8/1", max_height = "350px"),
      uiOutput("ch4_comp_table")
    ),

    lc_p("Na początku rozstęp wynosi 41.2 cm, IQR 11.5 cm, a odchylenie
      standardowe 8.1 cm. Jedna wartość odstająca (około 220 cm) podnosi
      rozstęp do około 71 cm, czyli o trzy czwarte. Odchylenie standardowe
      rośnie do 8.9 cm, a IQR prawie się nie zmienia. Po dodaniu pięciu takich
      wartości odchylenie standardowe sięga około 11 cm, a IQR wciąż wynosi
      około 11.8 cm."),

    lc_p("Rozstęp zależy tylko od dwóch skrajnych obserwacji, więc jedna
      nietypowa wartość wystarczy, żeby go zmienić. Odchylenie standardowe
      uwzględnia wszystkie dane, a odległe punkty, podniesione do kwadratu,
      ważą w nim szczególnie dużo. IQR, oparty na kwartylach, jest z tych
      trzech miar najbardziej odporny."),

    lc_note("Zasada", rule = TRUE,
      "Rozkład symetryczny bez wartości odstających opisuj średnią i odchyleniem
       standardowym, a rozkład skośny lub z wartościami odstającymi — medianą i IQR."
    ),

    # ====================================================================
    # WIDGET 5: Coefficient of Variation
    # ====================================================================
    lc_h2("ch4-cv", "Współczynnik zmienności (CV)"),

    lc_p("Odchylenie standardowe ma jednostki danych. Odchylenia wzrostu w cm
      i wagi w kg nie da się porównać, podobnie jak minut z ocenami. Liczy się
      też skala: rozrzut 8 cm przy średniej 171 cm to co innego niż rozrzut
      8 minut przy średniej 36 minut. Do porównań między zmiennymi służy ",
      gloss("współczynnik zmienności"), ". Wyraża on odchylenie standardowe
      jako procent średniej, więc nie ma jednostek."),

    lc_formula_box(withMathJax(
      "$$\\text{CV} = \\frac{s}{\\bar{x}} \\cdot 100\\%$$"
    )),

    lc_p("CV ma sens tylko dla zmiennych mierzonych na skali ilorazowej,
      z naturalnym zerem i wartościami dodatnimi, takich jak wzrost, waga
      czy czas. Panel zestawia odchylenia standardowe czterech zmiennych
      z ankiety (po lewej) z ich współczynnikami zmienności (po prawej)."),

    figure_panel(
      label = "Ryc. 4.7",
      title = "Porównanie zmienności między zmiennymi",
      lc_plots(
        tags$div(
          tags$h4("SD: wartości nieporównywalne"),
          lc_plot("ch4_sd_compare_plot", max_height = "350px")
        ),
        tags$div(
          tags$h4("CV: wartości porównywalne"),
          lc_plot("ch4_cv_plot", max_height = "350px")
        )
      ),
      uiOutput("ch4_cv_table")
    ),

    lc_p("Według samego odchylenia standardowego kolejność to: czas dojazdu
      (16.3 min), waga (13.7 kg), wzrost (8.1 cm) i średnia ocen (0.6). Te liczby
      mają jednak różne jednostki, więc ich porównanie nic nie mówi.
      Współczynnik zmienności czasu dojazdu wynosi 45.6%, wagi 19.2%, średniej
      ocen 15.7%, a wzrostu tylko 4.8%. Średnia ocen, z najmniejszym
      odchyleniem standardowym, okazuje się względnie trzy razy bardziej
      zmienna niż wzrost. Wzrost studentów jest najbardziej jednorodną
      z czterech zmiennych."),

    lc_chapter_next(
      num       = "05",
      title     = "Kształt rozkładu",
      lead      = "dwa rozkłady z tą samą średnią i SD mogą mieć zupełnie inny kształt — asymetrię i ciężkość ogonów.",
      target_id = "ch-ksztalt"
    ),

    # Bottom spacing
    lc_spacer("md")

  )
)

# --------------------------------------------------------------------------
# Chapter 4 Server
# --------------------------------------------------------------------------

ch4_server <- function(input, output, session) {

  # --- Widget 1: Bus scenario ---

  # Krok widgetu (1..4) żyje w przeglądarce; suwak działa od kroku 3.
  # Zapas = o ile minut wcześniej pasażer wychodzi; dojedzie na czas,
  # gdy spóźnienie autobusu nie przekracza zapasu.
  ch4_spread_step <- lc_step_server("ch4_spread", input)$step

  # Helper: generate bus delay data (deterministic seed)
  ch4_bus_data <- function() {
    set.seed(123)
    data_a <- rgamma(1000, shape = 8, scale = 0.25) - 0.3
    data_b <- rgamma(1000, shape = 0.4, scale = 5)  - 0.3
    data_a <- data_a - mean(data_a) + 2
    data_b <- data_b - mean(data_b) + 2
    list(a = data_a, b = data_b,
         sd_a = round(sd(data_a), 1), sd_b = round(sd(data_b), 1))
  }

  # Odczyty SD zastępują legendę: kolor odczytu = kolor linii.
  output$ch4_spread_reads <- renderUI({
    bus <- ch4_bus_data()
    tagList(
      lc_readout("Linia A", paste0("SD = ", lc_fmt(bus$sd_a, 1), " min"),
                 color = STEP_ROLES$data$colour, swatch = TRUE),
      lc_readout("Linia B", paste0("SD = ", lc_fmt(bus$sd_b, 1), " min"),
                 color = STEP_ROLES$group$colour, swatch = TRUE)
    )
  })

  zoom_plot_server("ch4_spread_plot", reactive({
    step <- ch4_spread_step()

    buffer <- input$ch4_spread_buffer
    req(!is.null(buffer))
    bus <- ch4_bus_data()

    dens_a <- density(bus$a, from = -3, to = 30, n = 500)
    dens_b <- density(bus$b, from = -3, to = 30, n = 500)
    df_a <- data.frame(x = dens_a$x, y = dens_a$y)
    df_b <- data.frame(x = dens_b$x, y = dens_b$y)
    # Stała rama: oś Y z obu krzywych, wspólna dla kroków.
    y_hi <- max(df_a$y, df_b$y) * 1.08

    p <- ggplot(mapping = aes(x = x, y = y)) +
      step_line("known", xintercept = 2) +
      step_layer(geom_vline, "known", xintercept = 0, linewidth = 0.5, alpha = 0.5)

    if (step >= 3) {
      cutoff <- buffer
      shade_a <- df_a[df_a$x <= cutoff, ]
      shade_b <- df_b[df_b$x <= cutoff, ]

      p <- p +
        geom_area(data = shade_a, fill = STEP_ROLES$data$colour, alpha = 0.25) +
        geom_area(data = shade_b, fill = STEP_ROLES$group$colour, alpha = 0.15) +
        step_line(step_role(step, 3), xintercept = cutoff)
    }

    p +
      step_layer(geom_line, "data", data = df_a, linewidth = 1.2, alpha = 1) +
      step_layer(geom_line, "group", data = df_b, linewidth = 1.2, alpha = 1) +
      labs(x = "Spóźnienie (minuty)    ← za wcześnie | za późno →",
           y = "Gęstość") +
      step_frame(xlim = c(-3, 25), ylim = c(0, y_hi))
  }))

  output$ch4_spread_text <- renderUI({
    step <- ch4_spread_step()
    buffer <- input$ch4_spread_buffer
    bus <- ch4_bus_data()

    if (step == 1) {
      "Obie linie mają średnie spóźnienie 2 minuty. Patrząc tylko na średnią,
       są identyczne."
    } else if (step == 2) {
      pct_10_a <- round(mean(bus$a > 10) * 100, 1)
      pct_10_b <- round(mean(bus$b > 10) * 100, 1)
      mean_late_b <- if (any(bus$b > 10)) round(mean(bus$b[bus$b > 10]), 1) else 0
      tagList(
        tags$strong("Spóźnienia ponad 10 min:"),
        paste0(" linia A — ", lc_fmt(pct_10_a, 1), "% kursów; linia B — ",
               lc_fmt(pct_10_b, 1), "% kursów",
               if (pct_10_b > 0) paste0(" (średnio ", lc_fmt(mean_late_b, 1), " min)") else "",
               ".")
      )
    } else if (step == 3) {
      lbl <- if (buffer == 0) "bez zapasu" else paste0(buffer, " min wcześniej")
      paste0("Wyjście ", lbl,
             ". Zacieniowany obszar to kursy spóźnione najwyżej o tyle, ",
             "ile wynosi zapas — z nimi pasażer dojedzie na czas.")
    } else if (step == 4) {
      prob_a <- mean(bus$a <= buffer)
      prob_b <- mean(bus$b <= buffer)
      pct_10_b <- round(mean(bus$b > 10) * 100, 1)
      mean_late_b <- if (any(bus$b > 10)) round(mean(bus$b[bus$b > 10]), 1) else 0
      lbl <- if (buffer == 0) "bez zapasu" else paste0(buffer, " min wcześniej")
      tagList(
        paste0("Wyjście ", lbl, ". Na czas dojedzie się linią A w ",
               lc_fmt(prob_a * 100, 1), "% kursów, linią B w ",
               lc_fmt(prob_b * 100, 1), "%."),
        if (pct_10_b > 0) tagList(
          tags$br(),
          paste0("Gdy linia B spóźnia się ponad 10 min (", lc_fmt(pct_10_b, 1),
                 "% kursów), czeka się średnio ", lc_fmt(mean_late_b, 1), " min.")
        )
      )
    }
  })

  # --- Widget 2: SD step-by-step ---

  # Krok widgetu (1..3) żyje w przeglądarce; nowy zestaw nie cofa kroku.
  ch4_sd_step <- lc_step_server("ch4_sd", input)$step
  ch4_sd_data <- reactiveVal(round(rnorm(10, mean = 170, sd = 8), 1))

  observeEvent(input$ch4_sd_new, {
    set.seed(NULL)
    ch4_sd_data(round(rnorm(10, mean = 170, sd = 8), 1))
  })

  zoom_plot_server("ch4_sd_plot", reactive({
    step <- ch4_sd_step()

    vals <- ch4_sd_data()
    n <- length(vals)
    x_bar <- mean(vals)
    s <- sd(vals)

    # Stała rama: dane i pas średnia ± SD, wspólne dla kroków.
    x_rng <- range(vals, x_bar - s, x_bar + s)
    x_pad <- diff(x_rng) * 0.08
    frame <- step_frame(xlim = x_rng + c(-x_pad, x_pad), ylim = c(0, n + 1.2),
                        y_axis = FALSE)

    if (step == 1) {
      # Krok 1: punkty na osi liczbowej
      df <- data.frame(x = vals)
      p <- ggplot(df, aes(x = x, y = (n + 1) / 2)) +
        step_layer(geom_point, "data", size = 4, alpha = 1) +
        labs(x = "Wzrost (cm)", y = "") +
        frame

    } else {
      # Kroki 2-3: punkty jedna pod drugą, posortowane wg odległości od średniej
      deviations <- vals - x_bar
      ord <- order(abs(deviations), decreasing = TRUE)
      df <- data.frame(
        x = vals[ord],
        dev = deviations[ord],
        y = seq(n, 1)  # najdalszy na górze
      )
      dev_role <- step_role(step, 2)

      p <- ggplot(df, aes(x = x, y = y))

      if (step >= 3) {
        p <- p +
          annotate("rect", xmin = x_bar - s, xmax = x_bar + s,
                   ymin = 0, ymax = n + 0.3, fill = STEP_ROLES$new$colour, alpha = 0.08) +
          step_line("new", xintercept = x_bar - s) +
          step_line("new", xintercept = x_bar + s) +
          step_label(x_bar - s, 0.3, paste0("śr. − SD\n", round(x_bar - s, 1)),
                     role = "new", hjust = 0.5) +
          step_label(x_bar + s, 0.3, paste0("śr. + SD\n", round(x_bar + s, 1)),
                     role = "new", hjust = 0.5) +
          step_label(x_bar, 0.5, paste0("SD = ", round(s, 2), " cm"),
                     role = "new", hjust = 0.5, vjust = 0.5, size = 4.2)
      }

      p <- p +
        step_line(dev_role, xintercept = x_bar) +
        step_layer(geom_segment, dev_role,
                   mapping = aes(x = x_bar, xend = x, y = y, yend = y),
                   arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
        step_layer(geom_point, "data", size = 4, alpha = 1) +
        step_label(x_bar, n + 0.8, paste0("średnia = ", round(x_bar, 2)),
                   role = dev_role, hjust = 0.5, vjust = 0.5) +
        labs(x = "Wzrost (cm)", y = "") +
        frame
    }

    p
  }))

  output$ch4_sd_table <- renderUI({
    step <- ch4_sd_step()
    if (step < 2) return(NULL)

    vals <- ch4_sd_data()
    n <- length(vals)
    x_bar <- mean(vals)

    deviations <- vals - x_bar
    sq_deviations <- deviations^2

    df <- data.frame(
      i = as.character(1:n),
      x = vals,
      dev = round(deviations, 2),
      sq = round(sq_deviations, 2)
    )
    # Od kroku 3: wiersz sumy kwadratów odchyleń.
    foot <- if (step >= 3) list(i = "SUMA", x = "", dev = "",
                                sq = round(sum(sq_deviations), 2))

    lc_table(df, cols = list(
      lc_col("i", "i", "row"),
      lc_col("x", "xᵢ", digits = 1),
      lc_col("dev", "xᵢ − x̄", digits = 2),
      lc_col("sq", "(xᵢ − x̄)²", digits = 2)
    ), foot = foot)
  })

  output$ch4_sd_text <- renderUI({
    step <- ch4_sd_step()

    if (step == 1) {
      "Dziesięć pomiarów wzrostu na osi liczbowej. Każdy punkt to jedna obserwacja."
    } else if (step == 2) {
      vals <- ch4_sd_data()
      x_bar <- mean(vals)
      paste0("Średnia wynosi ", round(x_bar, 2),
             " cm. Strzałki to odchylenia od średniej, tabela podaje ich kwadraty.")
    } else if (step == 3) {
      vals <- ch4_sd_data()
      n <- length(vals)
      x_bar <- mean(vals)
      deviations <- vals - x_bar
      sq_deviations <- deviations^2
      variance <- sum(sq_deviations) / (n - 1)
      s <- sqrt(variance)
      withMathJax(
        paste0("Wariancja \\(s^2\\) = ", round(sum(sq_deviations), 2), " / ", n - 1,
               " = ", round(variance, 2), " cm². ",
               "Odchylenie standardowe \\(s = \\sqrt{", round(variance, 2), "} = ",
               round(s, 2), "\\) cm. Pas na wykresie to \\(\\bar{x} \\pm s\\).")
      )
    }
  })

  # --- Widget 2b: Empirical rule (68-95-99.7) ---

  zoom_plot_server("ch4_emp_plot", reactive({
    var_name <- input$ch4_emp_var
    req(var_name)
    vals <- student_data[[var_name]]
    m <- mean(vals)
    s <- sd(vals)

    band_alphas <- c(0.22, 0.14, 0.07)

    p <- ggplot(data.frame(x = vals), aes(x = x))

    # Pasy w tle (od najszerszego do najwęższego, żeby ±1 SD był na wierzchu)
    for (k in 3:1) {
      p <- p + annotate("rect",
        xmin = m - k * s, xmax = m + k * s,
        ymin = -Inf, ymax = Inf,
        fill = upwr_accent, alpha = band_alphas[k]
      )
    }

    # Histogram na wierzchu pasów
    p <- p +
      geom_histogram(aes(y = after_stat(density)),
                     bins = 25, fill = upwr_cat["niebo"], color = "white", alpha = 0.85) +
      geom_vline(xintercept = m, color = upwr_secondary, linewidth = 1.2, linetype = "solid") +
      annotate("text", x = m, y = Inf, label = paste0("x̄ = ", round(m, 1)),
               vjust = 1.5, hjust = -0.1, color = upwr_secondary, fontface = "bold", size = 4.5) +
      annotate("text",
               x = c(m - s, m + s, m - 2 * s, m + 2 * s, m - 3 * s, m + 3 * s),
               y = -Inf,
               label = c("-1 SD", "+1 SD", "-2 SD", "+2 SD", "-3 SD", "+3 SD"),
               vjust = -0.5, hjust = c(1.1, -0.1, 1.1, -0.1, 1.1, -0.1),
               size = 3.2, color = upwr_secondary, fontface = "italic") +
      labs(
        x = variable_meta[[var_name]]$label,
        y = "Gęstość"
      ) +
      theme()

    p
  }))

  output$ch4_emp_text <- renderUI({
    var_name <- input$ch4_emp_var
    req(var_name)
    vals <- student_data[[var_name]]
    m <- mean(vals)
    s <- sd(vals)

    pct_in <- sapply(1:3, function(k) {
      round(mean(vals >= m - k * s & vals <= m + k * s) * 100, 1)
    })

    diff_1sd <- abs(pct_in[1] - 68)
    pct_txt <- paste0(" W pasach ±1, ±2 i ±3 SD leży ", lc_fmt(pct_in[1], 1), "%, ",
                      lc_fmt(pct_in[2], 1), "% i ", lc_fmt(pct_in[3], 1),
                      "% danych (reguła: 68%, 95% i 99.7%).")

    if (diff_1sd < 5) {
      lc_status(
        tags$strong("Dobra zgodność z regułą."),
        pct_txt
      )
    } else {
      lc_status(
        lc_verdict(tags$strong("Słaba zgodność z regułą."), type = "warning"),
        pct_txt
      )
    }
  })

  # --- Widget 3: Boxplot builder ---

  # Krok widgetu (1..5) żyje w przeglądarce; nowe dane nie cofają kroku.
  ch4_bp_step <- lc_step_server("ch4_bp", input)$step
  ch4_bp_data <- reactiveVal(round(c(rnorm(27, 170, 8), 145, 198, 200), 1))

  observeEvent(input$ch4_bp_new, {
    set.seed(NULL)
    ch4_bp_data(round(c(rnorm(27, 170, 8), 145, 198, 200), 1))
  })

  zoom_plot_server("ch4_bp_plot", reactive({
    step <- ch4_bp_step()

    vals <- ch4_bp_data()
    sorted_vals <- sort(vals)
    med <- median(vals)
    q1 <- quantile(vals, 0.25)
    q3 <- quantile(vals, 0.75)
    iqr_val <- q3 - q1
    lower_fence <- q1 - 1.5 * iqr_val
    upper_fence <- q3 + 1.5 * iqr_val
    whisker_low <- min(vals[vals >= lower_fence])
    whisker_high <- max(vals[vals <= upper_fence])
    outliers <- vals[vals < lower_fence | vals > upper_fence]

    # Stała rama osi X z pełnych danych, wspólna dla kroków.
    x_lim <- range(vals) + c(-1, 1) * diff(range(vals)) * 0.06
    frame <- step_frame(xlim = x_lim, ylim = c(-0.9, 0.9), y_axis = FALSE)

    if (step == 5) {
      # Final: clean boxplot + histogram for comparison
      df <- data.frame(x = vals)
      p_box <- ggplot(df, aes(x = x, y = "")) +
        step_result(geom_boxplot, outlier.colour = STEP_ROLES$known$colour,
                    outlier.size = 3, width = 0.4) +
        step_layer(geom_jitter, "known", width = 0, height = 0.05, alpha = 0.4, size = 2) +
        labs(x = "", y = "") +
        step_frame(xlim = x_lim, ylim = c(0.5, 1.5), y_axis = FALSE)

      p_hist <- ggplot(df, aes(x = x)) +
        step_result(geom_histogram, bins = 15)
      count_max <- max(layer_data(p_hist)$count)
      p_hist <- p_hist +
        step_line("known", xintercept = med, helper = FALSE) +
        step_line("known", xintercept = q1) +
        step_line("known", xintercept = q3) +
        labs(x = "Wzrost (cm)", y = "Liczebność") +
        step_frame(xlim = x_lim, ylim = c(0, count_max * 1.08))

      return(gridExtra::arrangeGrob(p_box, p_hist, nrow = 2, heights = c(1, 1.2)))
    }

    # Steps 1-4: manual construction
    df <- data.frame(x = vals, y = 0)

    if (step == 1) {
      # Jittered raw data
      set.seed(42)
      df$y_jit <- runif(nrow(df), -0.3, 0.3)

      ggplot(df, aes(x = x, y = y_jit)) +
        step_layer(geom_point, "data", size = 3) +
        labs(x = "Wzrost (cm)", y = "") +
        frame

    } else if (step == 2) {
      set.seed(42)
      df$y_jit <- runif(nrow(df), -0.3, 0.3)

      ggplot(df, aes(x = x, y = y_jit)) +
        step_layer(geom_point, "data", size = 3) +
        step_line("new", xintercept = med, helper = FALSE) +
        step_label(med, 0.65, "Mediana", role = "new", hjust = 0.5, vjust = 0.5) +
        labs(x = "Wzrost (cm)", y = "") +
        frame

    } else if (step == 3) {
      set.seed(42)
      df$y_jit <- runif(nrow(df), -0.3, 0.3)

      ggplot(df, aes(x = x, y = y_jit)) +
        # IQR box
        step_result(geom_rect, data = data.frame(xmin = q1, xmax = q3),
                    mapping = aes(xmin = xmin, xmax = xmax, ymin = -0.5, ymax = 0.5),
                    inherit.aes = FALSE, alpha = 0.2) +
        step_layer(geom_point, "data", size = 3) +
        step_line("known", xintercept = med, helper = FALSE) +
        step_line("new", xintercept = q1) +
        step_line("new", xintercept = q3) +
        step_label(med, 0.7, "Mediana", role = "known", hjust = 0.5, vjust = 0.5) +
        step_label(q1, -0.65, "Q1", role = "new", hjust = 0.5, vjust = 0.5) +
        step_label(q3, -0.65, "Q3", role = "new", hjust = 0.5, vjust = 0.5) +
        step_label((q1 + q3) / 2, 0.7, "IQR", role = "new", vjust = 0.5,
                   hjust = ifelse(abs(med - (q1 + q3) / 2) < 3, 2, 0.5)) +
        labs(x = "Wzrost (cm)", y = "") +
        frame

    } else if (step == 4) {
      is_outlier <- vals < lower_fence | vals > upper_fence
      df$outlier <- is_outlier
      set.seed(42)
      df$y_jit <- runif(nrow(df), -0.2, 0.2)

      whiskers <- data.frame(
        x    = c(whisker_low, whisker_low, q3, whisker_high),
        xend = c(q1, whisker_low, whisker_high, whisker_high),
        y    = c(0, -0.2, 0, -0.2),
        yend = c(0, 0.2, 0, 0.2)
      )

      p <- ggplot(df) +
        # IQR box
        step_result(geom_rect, data = data.frame(xmin = q1, xmax = q3),
                    mapping = aes(xmin = xmin, xmax = xmax, ymin = -0.4, ymax = 0.4),
                    alpha = 0.2) +
        # Median line inside box
        step_layer(geom_segment, "known",
                   data = data.frame(x = med, xend = med, y = -0.4, yend = 0.4),
                   mapping = aes(x = x, xend = xend, y = y, yend = yend),
                   linewidth = 1.2) +
        # Whiskers
        step_layer(geom_segment, "new", data = whiskers,
                   mapping = aes(x = x, xend = xend, y = y, yend = yend)) +
        # Points: normal
        step_layer(geom_point, "data", data = df[!df$outlier, ],
                   mapping = aes(x = x, y = y_jit), size = 2.5, alpha = 0.5) +
        # Points: outliers
        step_layer(geom_point, "new", data = df[df$outlier, ],
                   mapping = aes(x = x, y = y_jit), size = 4, shape = 18) +
        labs(x = "Wzrost (cm)", y = "") +
        frame

      if (length(outliers) > 0) {
        p <- p +
          step_label(mean(outliers), 0.55, paste0("wartości odstające: ", length(outliers)),
                     role = "new", hjust = 0.5, vjust = 0.5)
      }

      p
    }
  }))

  output$ch4_bp_text <- renderUI({
    step <- ch4_bp_step()
    if (step == 1) {
      "30 pomiarów wzrostu na osi liczbowej. Widać zakres, ale trudno coś szybko odczytać."
    } else if (step == 2) {
      vals <- ch4_bp_data()
      paste0("Mediana = ", round(median(vals), 1),
             " cm dzieli posortowane dane na dwie połowy.")
    } else if (step == 3) {
      vals <- ch4_bp_data()
      q1 <- quantile(vals, 0.25)
      q3 <- quantile(vals, 0.75)
      paste0("Q1 = ", round(q1, 1), " cm, Q3 = ", round(q3, 1),
             " cm. Pudełko obejmuje środkowe 50% danych, IQR = ",
             round(q3 - q1, 1), " cm.")
    } else if (step == 4) {
      vals <- ch4_bp_data()
      q1 <- quantile(vals, 0.25)
      q3 <- quantile(vals, 0.75)
      iqr_val <- q3 - q1
      lower_fence <- q1 - 1.5 * iqr_val
      upper_fence <- q3 + 1.5 * iqr_val
      outliers <- vals[vals < lower_fence | vals > upper_fence]
      paste0("Granice wąsów: ", round(lower_fence, 1), " i ",
             round(upper_fence, 1), " cm. ",
             if (length(outliers) > 0) {
               paste0("Wartości odstające: ",
                      paste(round(sort(outliers), 1), collapse = ", "), " cm.")
             } else {
               "Brak wartości odstających."
             })
    } else if (step == 5) {
      "Gotowy wykres pudełkowy (u góry) obok histogramu tych samych danych (u dołu)."
    }
  })

  # --- Widget 3b: Group comparison ---

  zoom_plot_server("ch4_grp_plot", reactive({
    var_name <- input$ch4_grp_var
    grp_name <- input$ch4_grp_by
    req(var_name, grp_name)

    df <- data.frame(
      value = student_data[[var_name]],
      group = student_data[[grp_name]]
    )

    var_label <- names(which(c("wzrost" = "Wzrost (cm)", "waga" = "Waga (kg)",
      "czas_dojazdu" = "Czas dojazdu (min)", "srednia_ocen" = "Średnia ocen") == var_name))
    if (length(var_label) == 0) var_label <- var_name

    grp_label <- ifelse(grp_name == "plec", "Płeć", "Kierunek")

    p <- ggplot(df, aes(x = group, y = value, fill = group))

    if (isTRUE(input$ch4_grp_violin)) {
      p <- p + geom_violin(alpha = 0.4, color = NA) +
        geom_boxplot(width = 0.2, alpha = 0.8, outlier.shape = NA)
    } else {
      p <- p + geom_boxplot(alpha = 0.7, outlier.color = upwr_accent, outlier.size = 3)
    }

    if (isTRUE(input$ch4_grp_points)) {
      p <- p + geom_jitter(width = 0.15, alpha = 0.3, size = 1.5)
    }

    p + scale_fill_upwr() +
      labs(x = grp_label, y = var_label) +
            theme(legend.position = "none")
  }))

  output$ch4_grp_table <- renderUI({
    var_name <- input$ch4_grp_var
    grp_name <- input$ch4_grp_by
    req(var_name, grp_name)

    df <- data.frame(
      value = student_data[[var_name]],
      group = student_data[[grp_name]]
    )

    stats <- df %>%
      group_by(group) %>%
      summarise(
        n = n(),
        mean = round(mean(value), 2),
        median = round(median(value), 2),
        sd = round(sd(value), 2),
        iqr = round(IQR(value), 2),
        .groups = "drop"
      ) %>%
      mutate(group = as.character(group))

    lc_table_split(stats,
      cols = list(
        lc_col("group", "Grupa", "row"),
        lc_col("n", "n"),
        lc_col("mean", "Średnia", digits = 2),
        lc_col("median", "Mediana", digits = 2),
        lc_col("sd", "SD", digits = 2),
        lc_col("iqr", "IQR", digits = 2)
      ),
      groups = list(c("n", "mean", "median"), c("sd", "iqr")),
      label = "Statystyki opisowe w grupach"
    )
  })

  # --- Widget 4: Spread measures comparison ---

  ch4_comp_data <- reactiveVal(NULL)

  observe({
    if (is.null(ch4_comp_data())) {
      ch4_comp_data(student_data$wzrost)
    }
  })

  observeEvent(input$ch4_comp_add1, {
    set.seed(NULL)
    current <- ch4_comp_data()
    outlier <- max(current) + 30 + runif(1, -5, 5)
    ch4_comp_data(c(current, round(outlier, 1)))
  })

  observeEvent(input$ch4_comp_add5, {
    set.seed(NULL)
    current <- ch4_comp_data()
    outliers <- sapply(1:5, function(i) max(current) + 30 + runif(1, -5, 5))
    ch4_comp_data(c(current, round(outliers, 1)))
  })

  observeEvent(input$ch4_comp_reset, {
    ch4_comp_data(student_data$wzrost)
  })

  zoom_plot_server("ch4_comp_plot", reactive({
    vals <- ch4_comp_data()
    if (is.null(vals)) return(NULL)

    df <- data.frame(x = vals)
    data_range <- range(vals)
    q1 <- quantile(vals, 0.25)
    q3 <- quantile(vals, 0.75)
    iqr_val <- q3 - q1

    ggplot(df, aes(x = x)) +
      geom_histogram(bins = 30, fill = upwr_cat["niebo"], color = "white", alpha = 0.7) +
      # Range
      annotate("segment", x = data_range[1], xend = data_range[2],
               y = -2, yend = -2, color = upwr_accent, linewidth = 2) +
      annotate("text",
               x = (data_range[1] + data_range[2]) / 2, y = -3.5,
               label = paste0("Rozstęp = ", round(diff(data_range), 1)),
               color = upwr_accent, size = 4, fontface = "bold") +
      # IQR
      annotate("segment", x = q1, xend = q3, y = -6, yend = -6,
               color = upwr_cat["szalwia"], linewidth = 2) +
      annotate("text", x = (q1 + q3) / 2, y = -7.5,
               label = paste0("IQR = ", round(iqr_val, 1)),
               color = upwr_cat["szalwia"], size = 4, fontface = "bold") +
      labs(x = "Wzrost (cm)", y = "Liczebność") +
            coord_cartesian(clip = "off") +
      theme(plot.margin = margin(10, 10, 50, 10))
  }))

  output$ch4_comp_n <- renderUI({
    vals <- ch4_comp_data()
    if (is.null(vals)) return(NULL)
    lc_readout("n", length(vals))
  })

  output$ch4_comp_table <- renderUI({
    vals <- ch4_comp_data()
    if (is.null(vals)) return(NULL)

    df <- data.frame(
      measure = c("Rozstęp", "IQR (rozstęp międzykwartylowy)",
                  "Odchylenie standardowe (SD)",
                  "Współczynnik zmienności (CV)"),
      value = c(diff(range(vals)), IQR(vals), sd(vals), sd(vals) / mean(vals) * 100),
      notes = c(
        "Bardzo wrażliwy na wartości odstające — zależy tylko od min i max",
        "Odporny na wartości odstające — oparty na kwartylach",
        "Umiarkowanie wrażliwe — uwzględnia wszystkie dane",
        "Bezjednostkowy — pozwala porównywać zmienność różnych zmiennych"
      ),
      stringsAsFactors = FALSE
    )

    lc_table(df,
      cols = list(
        lc_col("measure", "Miara", "row"),
        lc_col("value", "Wartość", digits = c(1, 1, 2, 1),
               suffix = c(" cm", " cm", " cm", "%")),
        lc_col("notes", "Własności", "text")
      ),
      narrow = "stack-last"
    )
  })

  # --- Widget 5: Coefficient of Variation ---

  zoom_plot_server("ch4_sd_compare_plot", reactive({
    vars <- c("wzrost", "waga", "czas_dojazdu", "srednia_ocen")
    labels <- c("Wzrost (cm)", "Waga (kg)", "Czas dojazdu (min)", "Średnia ocen")

    stats <- data.frame(
      Zmienna = factor(labels, levels = rev(labels)),
      SD = sapply(vars, function(v) sd(student_data[[v]]))
    )

    ggplot(stats, aes(x = Zmienna, y = SD, fill = SD)) +
      geom_col(alpha = 0.85, width = 0.6) +
      scale_fill_upwr_seq(variant = "burgundy", guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      coord_flip() +
      labs(x = NULL, y = "Odchylenie standardowe (oryg. jednostki)") +
      theme()
  }))

  zoom_plot_server("ch4_cv_plot", reactive({
    vars <- c("wzrost", "waga", "czas_dojazdu", "srednia_ocen")
    labels <- c("Wzrost (cm)", "Waga (kg)", "Czas dojazdu (min)", "Średnia ocen")

    stats <- data.frame(
      Zmienna = factor(labels, levels = rev(labels)),
      CV = sapply(vars, function(v) sd(student_data[[v]]) / mean(student_data[[v]]) * 100)
    )

    ggplot(stats, aes(x = Zmienna, y = CV, fill = CV)) +
      geom_col(alpha = 0.85, width = 0.6) +
      scale_fill_upwr_seq(variant = "burgundy", guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      coord_flip() +
      labs(x = NULL, y = "Współczynnik zmienności (%)") +
      theme()
  }))

  output$ch4_cv_table <- renderUI({
    vars <- c("wzrost", "waga", "czas_dojazdu", "srednia_ocen")
    labels <- c("Wzrost (cm)", "Waga (kg)", "Czas dojazdu (min)", "Średnia ocen")

    lc_table(
      data.frame(
        var = labels,
        mean = sapply(vars, function(v) mean(student_data[[v]])),
        sd = sapply(vars, function(v) sd(student_data[[v]])),
        cv = sapply(vars, function(v) sd(student_data[[v]]) / mean(student_data[[v]]) * 100)
      ),
      cols = list(
        lc_col("var", "Zmienna", "row"),
        lc_col("mean", "Średnia", digits = 2),
        lc_col("sd", "SD", digits = 2),
        lc_col("cv", "CV (%)", digits = 1)
      ),
      label = "Współczynnik zmienności czterech zmiennych"
    )
    })

}
