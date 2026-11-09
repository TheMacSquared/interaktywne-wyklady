# ============================================================================
# CHAPTER 4: Statystyki rozrzutu
# ============================================================================

# Świat autobusów (scena, Ryc. 4.1): 1000 kursów każdej linii,
# średnie spóźnienie 2 min. Ziarno ustawiane lokalnie, bez ruszania globalnego RNG.
ch4_bus_world <- function() {
  old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv())
  on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()))
  set.seed(123)
  data_a <- rgamma(1000, shape = 8, scale = 0.25) - 0.3
  data_b <- rgamma(1000, shape = 0.4, scale = 5)  - 0.3
  data_a <- data_a - mean(data_a) + 2
  data_b <- data_b - mean(data_b) + 2
  list(a = data_a, b = data_b,
       sd_a = round(sd(data_a), 1), sd_b = round(sd(data_b), 1))
}
ch4_bus <- ch4_bus_world()
ch4_bus_limit <- 5  # zapas: autobus o 7:45, jazda 10 min, zajęcia o 8:00

# Ryc. 4.4, 4.7 i 4.8 rysuje stat_charts.js. Opisy dla danych ankiety liczymy
# tu z góry; wartości po dopisaniu obserwacji odstających wpisuje przeglądarka.
ch4_emp_choices <- c("Wzrost (cm)" = "wzrost", "Waga (kg)" = "waga",
                     "Czas dojazdu (min)" = "czas_dojazdu", "Średnia ocen" = "srednia_ocen",
                     "Wydatki miesięczne (zł)" = "wydatki")

ch4_emp_notes <- function() {
  lapply(stats::setNames(nm = unname(ch4_emp_choices)), function(v) {
    vals <- student_data[[v]]; m <- mean(vals); s <- sd(vals)
    pct_in <- sapply(1:3, function(k) round(mean(vals >= m - k * s & vals <= m + k * s) * 100, 1))
    pct_txt <- paste0(" W pasach ±1, ±2 i ±3 SD leży ", lc_fmt(pct_in[1], 1), "%, ",
                      lc_fmt(pct_in[2], 1), "% i ", lc_fmt(pct_in[3], 1),
                      "% danych (reguła: 68%, 95% i 99.7%).")
    if (abs(pct_in[1] - 68) < 5) lc_status(tags$strong("Dobra zgodność z regułą."), pct_txt)
    else lc_status(lc_verdict(tags$strong("Słaba zgodność z regułą."), type = "warning"), pct_txt)
  })
}

# Tabela Ryc. 4.7: wartości wpisuje stat_charts.js po każdej zmianie danych.
ch4_comp_table_ui <- function() {
  x <- student_data$wzrost
  start <- c(range = paste(lc_fmt(diff(range(x)), 1), "cm"), iqr = paste(lc_fmt(IQR(x), 1), "cm"),
             sd = paste(lc_fmt(sd(x), 2), "cm"), cv = paste0(lc_fmt(sd(x) / mean(x) * 100, 1), "%"))
  df <- data.frame(
    measure = c("Rozstęp", "IQR (rozstęp międzykwartylowy)",
                "Odchylenie standardowe (SD)", "Współczynnik zmienności (CV)"),
    notes = c("Bardzo wrażliwy na wartości odstające — zależy tylko od min i max",
              "Odporny na wartości odstające — oparty na kwartylach",
              "Umiarkowanie wrażliwe — uwzględnia wszystkie dane",
              "Bezjednostkowy — pozwala porównywać zmienność różnych zmiennych"),
    stringsAsFactors = FALSE)
  df$value <- lapply(names(start), function(k) lc01_stat_value(k, start[[k]]))
  lc_table(df[c("measure", "value", "notes")],
    cols = list(lc_col("measure", "Miara", "row"), lc_col("value", "Wartość"),
                lc_col("notes", "Własności", "text")),
    narrow = "stack-last")
}

ch4_cv_table_ui <- function() {
  vars <- c("wzrost", "waga", "czas_dojazdu", "srednia_ocen")
  labels <- c("Wzrost (cm)", "Waga (kg)", "Czas dojazdu (min)", "Średnia ocen")
  lc_table(
    data.frame(var = labels,
      mean = sapply(vars, function(v) mean(student_data[[v]])),
      sd = sapply(vars, function(v) sd(student_data[[v]])),
      cv = sapply(vars, function(v) sd(student_data[[v]]) / mean(student_data[[v]]) * 100)),
    cols = list(lc_col("var", "Zmienna", "row"), lc_col("mean", "Średnia", digits = 2),
                lc_col("sd", "SD", digits = 2), lc_col("cv", "CV (%)", digits = 1)),
    label = "Współczynnik zmienności czterech zmiennych")
}

ch4_grp_choices <- c("Wzrost (cm)" = "wzrost", "Waga (kg)" = "waga",
                     "Czas dojazdu (min)" = "czas_dojazdu", "Średnia ocen" = "srednia_ocen")

# Tabele Ryc. 4.6 dla każdej pary zmienna × podział (klucz „zmienna|grupy”).
ch4_grp_notes <- function() {
  keys <- expand.grid(v = unname(ch4_grp_choices), g = c("plec", "kierunek"), stringsAsFactors = FALSE)
  notes <- lapply(seq_len(nrow(keys)), function(i) {
    df <- data.frame(value = student_data[[keys$v[i]]], group = student_data[[keys$g[i]]])
    stats <- df %>% group_by(group) %>%
      summarise(n = n(), mean = round(mean(value), 2), median = round(median(value), 2),
                sd = round(sd(value), 2), iqr = round(IQR(value), 2), .groups = "drop") %>%
      mutate(group = as.character(group))
    lc_table_split(stats,
      cols = list(lc_col("group", "Grupa", "row"), lc_col("n", "n"),
                  lc_col("mean", "Średnia", digits = 2), lc_col("median", "Mediana", digits = 2),
                  lc_col("sd", "SD", digits = 2), lc_col("iqr", "IQR", digits = 2)),
      groups = list(c("n", "mean", "median"), c("sd", "iqr")),
      label = "Statystyki opisowe w grupach")
  })
  stats::setNames(notes, paste(keys$v, keys$g, sep = "|"))
}

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

    lc_p("Zacznijmy od przystanku, na którym codziennie czekasz na autobus na zajęcia.
      Autobus odjeżdża według rozkładu o 7:45, jazda trwa 10 minut, a zajęcia
      zaczynają się o 8:00. Pasażer ma więc 5 minut zapasu: spóźnienie autobusu
      x do 5 minut nic nie zmienia, każda minuta ponad to jest minutą spóźnienia
      na zajęcia. Do przystanku dojeżdżają dwie linie, A i K, o tym samym
      średnim spóźnieniu, równym 2 minuty. Scena losuje kursy z 1000
      symulowanych przyjazdów każdej linii."),

    # Scena: czekanie na autobus A/K
    figure_panel(
      label = "Ryc. 4.1",
      width_mode = "text",
      scene_widget("ch4_bus_scene",
        title = "Czekanie na autobus — ta sama średnia, inne ryzyko",
        steps = c("Przystanek", "Powtarzamy", "Ryzyko"),
        labels = c("Czekaj na autobus", "Czekaj jeszcze raz", "Czekaj jeszcze raz"),
        more_from = 2,
        options = list(list(name = "line", label = "Linia",
                            values = c("A" = "A", "K" = "B"),
                            selected = "A", from = 1)),
        extra = lc_step_from(3,
          lc_slider("ch4_bus_buffer", "Zapas (minuty)", 0, 15, ch4_bus_limit, 1)
        ),
        config = list(
          kind = "bus", height = 512,
          a = round(ch4_bus$a, 2), b = round(ch4_bus$b, 2),
          dep = 7 * 60 + 45, limit = ch4_bus_limit, limit_input = "ch4_bus_buffer",
          mean = round(mean(ch4_bus$a), 1), xmax = 25,
          aria = "Student czeka na przystanku na autobus linii A albo K; każde spóźnienie trafia do histogramu swojej linii."
        )
      )
    ),

    lc_p("Odczyty pod histogramami podają odchylenie standardowe, miarę rozrzutu,
      którą zdefiniujemy w następnej sekcji. Na linii A prawie każdy kurs
      przyjeżdża z podobnym, niewielkim opóźnieniem: odchylenie standardowe wynosi 0.7 min, a najgorszy z 1000
      kursów spóźnia się o 4.9 min. Pasażer linii A zawsze zdąży. Linia K
      zwykle przyjeżdża szybciej: połowa kursów spóźnia się najwyżej o 0.7 min.
      Ma jednak długi prawy ogon i odchylenie standardowe 3.0 min. Spóźnienie
      ponad 5 minut zdarza się w 12.3% kursów, czyli mniej więcej raz na osiem
      dni, a pasażer dociera wtedy na zajęcia średnio 3.7 min po czasie."),

    lc_p("Ryzyko zależy też od zapasu, który w ostatnim kroku sceny zmienia
      suwak. Gdyby pasażer jeździł kursem o 7:40, z 10 minutami zapasu, linia K
      spóźniałaby go w 3.4% dni, a kursem o 7:35 w 0.7%. Na linii A zapas
      można za to skrócić: przy 3 minutach student zdąży w 92% dni, a linią K
      w 77%. Żeby dojechać na czas w 99% dni, na linii A wystarczą 4 minuty
      zapasu, na linii K potrzeba około 14. Średnia tej różnicy nie widzi.
      Potrzebujemy liczby, która ją zmierzy."),

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

    lc_formula_annotated(
      "⟦1|s^2⟧ = \\frac{1}{⟦2|n - 1⟧} ⟦3|\\sum_{i=1}^{n}⟧ (⟦4|x_i⟧ - ⟦5|\\bar{x}⟧)^2 \\qquad ⟦6|s⟧ = \\sqrt{s^2}",
      notes = list(
        "wariancja",
        "liczba obserwacji minus 1",
        "suma",
        "i-ta obserwacja",
        "średnia",
        "odchylenie standardowe"
      ),
      colors = c("wrzos", "szalwia", "bursztyn", "niebo", "accent", "wrzos"),
      above = c(3, 4, 6)
    ),

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
      lc01_stat_figure("sd", bare = TRUE, body = lc_step_widget("ch4_sd",
        title = "Obliczanie odchylenia standardowego",
        steps = c("Dane", "Odchylenia od średniej", "Wariancja i SD"),
        toolbar = lc_toolbar(
          lc01_stat_action("new", "ch4_sd_new", "Losuj nowy zestaw", variant = "outline"),
          lc_chart_zoom_button()
        ),
        body = lc01_stat_chart(0, ratio = "2/1", max_height = "420px"),
        extra = tags$div(`data-role` = "table")
      ))
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

    # Wzorcowa krzywa dzwonowa z pasami ±1, ±2, ±3 SD
    figure_panel(
      label = "Ryc. 4.3",
      title = "Reguła 68–95–99.7 na rozkładzie normalnym",
      width_mode = "text",
      lc01_stat_figure("regula", ratio = "2.2/1", max_height = "300px")
    ),

    lc_p("Panel nakłada te trzy pasy na ", gloss("histogram"), " wybranej zmiennej z ankiety
      i podaje, jaki odsetek danych naprawdę w nich leży."),

    figure_panel(
      label = "Ryc. 4.4",
      title = "Reguła 68–95–99.7 — czy zawsze działa?",
      lc01_stat_figure("emp", ratio = "1.7/1", max_height = "400px",
        control = lc_group("Zmienna", lc_select("ch4_emp_var", ch4_emp_choices, aria_label = "Zmienna")),
        selected = "wzrost", notes = ch4_emp_notes())
    ),

    lc_p("Dla wzrostu reguła sprawdza się bardzo dobrze: w pasie ±1 SD leży
      67.5% studentów, w pasie ±2 SD — 96.5%, a w pasie ±3 SD wszyscy.
      Podobnie jest dla średniej ocen (68.5% i 96%) i wagi (64% i 97.5%)."),

    lc_p("Ciekawszy jest czas dojazdu. W pasie ±1 SD leży 71% danych, więc
      łączny odsetek się zgadza, ale rozkład nie jest symetryczny: poniżej pasa
      leży 13% obserwacji, a powyżej 16%. Pas ±3 SD sięga od -13 do 85 minut.
      Jego lewy kraniec to wartość niemożliwa, a po prawej stronie i tak
      zostają dwie obserwacje."),

    lc_p("Najwyraźniej regułę łamią miesięczne wydatki. W pasie ±1 SD leży
      88% danych zamiast 68%: większość studentów wydaje zbliżone kwoty,
      a kilka bardzo wysokich wartości zawyża odchylenie standardowe. Poniżej
      pasa leży tylko 3% obserwacji, powyżej 9%. Pas ±2 SD zaczyna się od
      -652 zł, czyli od wartości niemożliwej. Średnia i odchylenie
      standardowe opisują rozkład dobrze tylko wtedy, gdy jest on
      w przybliżeniu symetryczny.
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

    lc_formula_annotated(
      "⟦1|\\text{IQR}⟧ = ⟦2|Q_3⟧ - ⟦3|Q_1⟧ \\qquad {\\small \\text{granice wąsów: }} Q_1 - ⟦4|1.5 \\cdot \\text{IQR}⟧, \\;\\; Q_3 + 1.5 \\cdot \\text{IQR}",
      notes = list(
        "rozstęp międzykwartylowy",
        "trzeci kwartyl (75%)",
        "pierwszy kwartyl (25%)",
        "odległość granicy od pudełka"
      ),
      colors = c("accent", "indygo", "indygo", "terakota"),
      above = 2
    ),

    lc_p("Panel buduje wykres krok po kroku na 30 pomiarach wzrostu. 27 z nich
      to losowe wartości wokół 170 cm, a trzy dodaliśmy celowo: 145, 198
      i 200 cm."),

    figure_panel(
      label = "Ryc. 4.5",
      # Krok 6: niskie pudełko (wykres 0) nad histogramem (wykres 1).
      tags$style(HTML("
        #ch4_bp[data-lc-step='6'] .lc01-stat-chart[data-k='0'] { aspect-ratio: 5 / 1 !important; min-height: 6em; }
      ")),
      lc01_stat_figure("bp", bare = TRUE, body = lc_step_widget("ch4_bp",
        title = "Wykres pudełkowy — budowa krok po kroku",
        steps = c("Surowe dane", "Mediana", "Kwartyle i pudełko",
                  "Granice 1.5 IQR", "Wąsy i odstające", "Gotowy wykres"),
        toolbar = lc_toolbar(
          lc01_stat_action("new", "ch4_bp_new", "Losuj nowe dane", variant = "outline"),
          lc_chart_zoom_button()
        ),
        body = tagList(lc01_stat_chart(0, ratio = "2/1", max_height = "420px"),
                       lc01_stat_chart(1, ratio = "2.4/1", max_height = "340px"))
      ))
    ),

    lc_p("Trzy dodane wartości zwykle wypadają poza granice i zostają
      oznaczone jako wartości odstające. Pudełko ich nie zauważa: kwartyle,
      tak jak mediana, zależą tylko od kolejności obserwacji. W danych
      z ankiety wzrost ma IQR = 11.5 cm, a granice wartości odstających to 148.2 i 194.3 cm.
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
      label = "Ryc. 4.6",
      title = "Wykresy pudełkowe w grupach",
      lc01_stat_figure("groups", ratio = "1.6/1", max_height = "400px",
        control = lc_group("Zmienna ilościowa", lc_select("ch4_grp_var", ch4_grp_choices,
                                                          aria_label = "Zmienna ilościowa")),
        toolbar = tagList(
          htmltools::tagAppendAttributes(lc_segmented("ch4_grp_by", "Grupuj wg",
            choices = c("Płeć" = "plec", "Kierunek" = "kierunek")), class = "lc01-stat-ctl"),
          htmltools::tagAppendAttributes(checkboxInput("ch4_grp_violin", "Pokaż wykres skrzypcowy", value = FALSE),
                                         class = "lc01-stat-ctl"),
          htmltools::tagAppendAttributes(checkboxInput("ch4_grp_points", "Pokaż punkty", value = TRUE),
                                         class = "lc01-stat-ctl")
        ),
        selected = "wzrost|plec", notes = ch4_grp_notes())
    ),

    lc_p("Przy podziale wzrostu według płci pudełka kobiet i mężczyzn się
      nie nakładają: Q3 kobiet wynosi 170.1 cm, a Q1 mężczyzn 172.1 cm.
      Mediany to 166.0 i 177.1 cm. Zwróć uwagę na rozrzut. Odchylenie
      standardowe w grupach wynosi 6.1 cm u kobiet i 6.4 cm u mężczyzn,
      a w całej próbie 8.1 cm. Część rozrzutu całej próby bierze się z różnicy
      między grupami, a nie ze zmienności wewnątrz nich. Pudełka osób
      niebinarnych (13) i osób, które nie podały płci (6), powstały z kilku
      pomiarów, więc ich kwartyle są mało pewne."),

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
      label = "Ryc. 4.7",
      title = "Porównanie miar rozrzutu i ich odporności",
      lc01_stat_figure("comp", ratio = "1.8/1", max_height = "380px",
        toolbar = tagList(
          lc01_stat_action("add1", "ch4_comp_add1", "Dodaj wartość odstającą (+30 cm)", variant = "solid"),
          lc01_stat_action("add5", "ch4_comp_add5", "Dodaj 5 wartości odstających", variant = "solid"),
          lc01_stat_action("reset", "ch4_comp_reset", icon = "reset", variant = "ghost", aria_label = "Reset"),
          lc_readouts(lc_readout("n", lc01_stat_value("n", nrow(student_data))))
        ),
        after = ch4_comp_table_ui())
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

    lc_formula_annotated(
      "⟦1|\\text{CV}⟧ = ⟦2|s⟧ \\, / \\, ⟦3|\\bar{x}⟧ \\cdot 100\\%",
      notes = list(
        "współczynnik zmienności",
        "odchylenie standardowe",
        "średnia"
      ),
      colors = c("indygo", "wrzos", "accent"),
      above = 2
    ),

    lc_p("CV ma sens tylko dla zmiennych mierzonych na skali ilorazowej,
      z naturalnym zerem i wartościami dodatnimi, takich jak wzrost, waga
      czy czas. Panel zestawia odchylenia standardowe czterech zmiennych
      z ankiety (po lewej) z ich współczynnikami zmienności (po prawej)."),

    figure_panel(
      label = "Ryc. 4.8",
      title = "Porównanie zmienności między zmiennymi",
      lc01_stat_figure("spread", charts = 2, ratio = "1.4/1", max_height = "350px",
        headings = list("SD: wartości nieporównywalne", "CV: wartości porównywalne"),
        after = ch4_cv_table_ui())
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

  # --- Widget 1: Bus scene ---

  # Scena: teksty kroków liczone z tego samego świata autobusów.
  local({
    scene_texts(input, output, "ch4_bus_scene", list(
      tagList("Odjazd wg rozkładu 7:45, jazda 10 minut, zajęcia o 8:00. Poczekaj kilka
        razy na linii A, potem na K. Spóźnienie x liczymy od godziny z rozkładu."),
      tagList("Każde czekanie trafia do histogramu swojej linii (każdy ma własną skalę
        wysokości). Zbierz po kilkaset kursów na obu liniach i porównaj kształty."),
      tagList("Przerywana linia to średnia, wspólna dla obu linii. Na prawo od
        granicy zapasu nie zdążysz na zajęcia. Suwak zmienia zapas: większy zapas
        to wcześniejszy kurs. Porównaj odsetek spóźnień pod wykresem: średnia
        nie mówi, czy zdążysz.")
    ))
  })

  # Ryc. 4.2 (SD krok po kroku) i 4.3 (reguła 68–95–99.7) rysuje stat_charts.js.

  # --- Widget 2b: Empirical rule (68-95-99.7) ---

  # Ryc. 4.4 rysuje stat_charts.js; opisy w UI (ch4_emp_notes).

  # Ryc. 4.5 (pudełko krok po kroku) rysuje stat_charts.js.

  # --- Widget 3b: Group comparison ---

  # Ryc. 4.6 rysuje stat_charts.js; tabele dla każdej pary w UI (ch4_grp_notes).


  # --- Widget 4: Spread measures comparison ---

  # Ryc. 4.7 i 4.8 rysuje stat_charts.js; tabele w UI.

  # --- Widget 5: Coefficient of Variation ---


}
