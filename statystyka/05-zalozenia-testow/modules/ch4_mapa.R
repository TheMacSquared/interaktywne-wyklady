# ============================================================================
# CHAPTER 4: Mapa metod - kompletna tablica założenia -> alternatywa
# ============================================================================

ch4_ui <- lecture_chapter(
  id = "ch-mapa",
  num = "04",
  title = "Mapa metod",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 04 · Założenia testów",
      num    = "04",
      title  = "Mapa metod.",
      lead   = "Każda metoda z wykładu 04 opiera się na innym zestawie założeń
                i ma inną drogę wyjścia, gdy założenia zawodzą. Ten rozdział
                zbiera je w jednej mapie, do której można wracać przy każdej
                analizie."
    ),

    lc_h2("ch4-kompletna-mapa", "Kompletna mapa: metoda → założenia → alternatywa"),

    lc_p("Trzy poprzednie rozdziały omawiały założenia po kolei: kształt rozkładu,
      równy rozrzut w porównywanych grupach, liczebności w tabelach i kształt
      związku w korelacji. Każda metoda z wykładu 04 korzysta z innej kombinacji
      tych założeń. Test t dla par nie pyta o wariancje, test χ² nie pyta
      o normalność, a korelacja Pearsona wymaga liniowości, której nie wymaga
      żaden test porównujący grupy. Poniższe tabele zestawiają te kombinacje
      w jednym miejscu."),

    lc_p("Jedno założenie powtarza się we wszystkich wierszach: ",
      gloss("niezależność obserwacji"), ". Każda osoba lub jednostka powinna
      wnosić do analizy jedną obserwację, a wynik jednej nie powinien wpływać
      na wynik drugiej. Tego założenia nie sprawdza się wykresem ani testem.
      Wynika z projektu badania: z tego, jak dobrano jednostki i jak zbierano
      pomiary. Gdy jest naruszone, na przykład gdy te same osoby zmierzono dwa
      razy, nie pomaga żadna alternatywa z kolumny po prawej. Trzeba wybrać
      metodę, która tę zależność uwzględnia, jak test dla par."),

    lc_p("Tabele czyta się od lewej do prawej. Kolumna z założeniami mówi, czego
      metoda potrzebuje, kolumna diagnostyki — czym to ocenić, a ostatnia
      kolumna — po co sięgnąć, gdy założenie wyraźnie zawodzi. Diagnostykę
      zawsze zaczynamy od wykresu. Test formalny, taki jak Shapiro-Wilk czy
      Levene, jest tylko pomocą: przy małej próbie ma małą moc i przeoczy
      nawet wyraźne odchylenie, a przy dużej wykryje odchylenia bez znaczenia
      dla wniosków. Brak podstaw do odrzucenia H₀ normalności nie oznacza,
      że rozkład jest normalny."),

    # ========================================================================
    # Testy parametryczne
    # ========================================================================
    lc_h2("ch4-parametryczne", "Testy parametryczne"),

    lc_p(gloss("test parametryczny", "Testy parametryczne"), " z wykładu 04
      wnioskują o parametrach populacji: średniej, różnicy średnich albo
      współczynniku korelacji. P-wartość biorą z rozkładu t lub F. Ten rozkład
      jest dokładny, gdy dane pochodzą z rozkładu normalnego, a w pozostałych
      przypadkach jest przybliżeniem. Założenie normalności dotyczy więc
      w gruncie rzeczy rozkładu średniej z próby, a nie surowych danych. Z ",
      gloss("centralne twierdzenie graniczne", "centralnego twierdzenia granicznego"),
      " z wykładu 02 wiemy, że rozkład średniej zbliża się do normalnego wraz
      ze wzrostem próby. Im bardziej skośny rozkład danych i im więcej wartości
      odstających, tym większej próby potrzeba, żeby to przybliżenie było dobre."),

    lc_p("Równe wariancje to osobna sprawa. Wymaga ich tylko klasyczny test t
      Studenta i klasyczna ANOVA. ", gloss("test t Welcha", "Test t Welcha"),
      ", którego używają wszystkie panele kursu, tego założenia nie potrzebuje.
      Dla trzech i więcej grup odpowiednikiem jest ANOVA Welcha."),

    lc_p("Wersję Welcha warto traktować jako wybór domyślny, a nie jako ratunek
      po nieudanym teście Levene'a. Procedura dwuetapowa, w której najpierw
      testujemy równość wariancji, a potem od wyniku uzależniamy wybór testu,
      dziedziczy słabości testu formalnego: przy małych grupach Levene przeoczy
      różnicę wariancji, przy dużych wykryje nieistotną. Wersja Studenta jest
      w porządku przy równych licznościach grup albo wtedy, gdy równe wariancje
      wynikają z wiedzy o badanym zjawisku, a nie z braku istotności testu."),

    lc_table(
      data.frame(
        c1 = c(
          "Test t jednej próby",
          "Test t dla prób niezależnych",
          "Test t dla par",
          "ANOVA jednoczynnikowa",
          "Korelacja Pearsona"
        ),
        c2 = c(
          "Niezależne obserwacje; średnia z próby w przybliżeniu normalna:
           dane bez silnej skośności i wartości odstających albo odpowiednio
           duża próba",
          "Niezależne obserwacje i grupy; średnie w obu grupach
           w przybliżeniu normalne. Równe wariancje tylko w wersji Studenta",
          "Niezależne pary; średnia różnic w przybliżeniu normalna
           (dotyczy rozkładu różnic, nie obu pomiarów osobno)",
          "Niezależne obserwacje; reszty (odchylenia od średniej grupy)
           w przybliżeniu normalne; w wersji klasycznej równe wariancje",
          "Niezależne pary; związek liniowy; brak silnych wartości
           odstających; dla testu i przedziału ufności rozkład obu
           zmiennych łącznie zbliżony do normalnego"
        ),
        c3 = c(
          "Wykres Q-Q; pomocniczo Shapiro-Wilk",
          "Wykresy Q-Q w grupach; porównanie odchyleń standardowych
           w grupach (opisowo, nie jako podstawa wyboru testu)",
          "Wykres Q-Q różnic; pomocniczo Shapiro-Wilk na różnicach",
          "Wykres Q-Q reszt; porównanie odchyleń standardowych w grupach
           (opisowo, nie jako podstawa wyboru testu)",
          "Wykres rozrzutu (najpierw); pomocniczo wykresy Q-Q obu zmiennych"
        ),
        c4 = c(
          "Wilcoxon jednej próby, gdy rozkład jest symetryczny; przy silnej
           skośności najpierw wróć do pytania: czy pytanie dotyczy średniej,
           czy typowej wartości",
          "Welch jako wybór domyślny; Mann–Whitney, gdy pytanie dotyczy tego, czy wartości
           w jednej grupie bywają większe, a nie średnich. Mann–Whitney
           nie rozwiązuje problemu nierównych wariancji",
          "Wilcoxon dla par, gdy rozkład różnic jest symetryczny",
          "ANOVA Welcha jako wybór domyślny, z post hoc Games-Howella; Kruskal-Wallis z testem Dunna przy silnej
           skośności w małych grupach lub danych porządkowych",
          "Spearman przy związku monotonicznym, wartościach odstających
           lub danych porządkowych; tau Kendalla przy małych próbach
           i wielu remisach"
        )
      ),
      cols = list(
        lc_col("c1", "Metoda", "row"),
        lc_col("c2", "Założenia", "text"),
        lc_col("c3", "Jak sprawdzić", "text"),
        lc_col("c4", "Gdy naruszone → alternatywa", "text")
      ),
      narrow = "cards",
      prose = TRUE
    ),

    lc_p("Para ANOVA Welcha i Games-Howell jest spójna: ani test ogólny, ani
      porównania parami nie zakładają równych wariancji. Panel ANOVA w wykładzie
      04 łączył klasyczną ANOVA z testem ",
      gloss("test Games-Howella", "Games-Howella"), ". Przy podobnych wariancjach
      obie wersje ANOVA dają prawie ten sam wynik, przy wyraźnie różnych
      bezpieczniej użyć wersji Welcha."),

    lc_p("Po alternatywę z ostatniej kolumny sięgamy w trzech sytuacjach. Pierwsza:
      próba jest mała, a wykres pokazuje silną skośność albo wartości odstające,
      więc nie można liczyć na centralne twierdzenie graniczne. Druga: dane są
      porządkowe, na przykład odpowiedzi na skali Likerta, i średnia nie ma
      dobrej interpretacji. Trzecia: rozkład jest tak skośny, że średnia
      przestaje opisywać typową wartość, a pytanie badawcze i tak dotyczy
      czegoś innego niż średnia. Łagodne odchylenia od normalności, zwłaszcza
      przy podobnych liczebnościach grup, testy t i ANOVA zwykle znoszą dobrze."),

    # ========================================================================
    # Testy nieparametryczne
    # ========================================================================
    lc_h2("ch4-nieparametryczne", "Testy nieparametryczne"),

    lc_p(gloss("test nieparametryczny", "Testy nieparametryczne"), " z ostatniej
      kolumny nie są wersjami testu t pozbawionymi założeń. Zastępują wartości
      ich ", gloss("ranga", "rangami"), ", czyli pozycjami w uporządkowanych
      danych, i dlatego testują inną hipotezę niż ich parametryczne odpowiedniki.
      Ani ", gloss("test Manna-Whitneya", "test Manna–Whitneya"), ", ani ",
      gloss("test Kruskala-Wallisa", "test Kruskala-Wallisa"), " nie porównuje
      średnich. Hipoteza zerowa mówi, że wszystkie grupy mają ten sam rozkład,
      a test wykrywa przede wszystkim tendencję wartości z jednej grupy do
      bycia większymi od wartości z drugiej. Jako porównanie median wynik można
      czytać tylko wtedy, gdy rozkłady w grupach mają podobny kształt i różnią
      się jedynie przesunięciem. Z tego samego powodu test Manna–Whitneya
      nie jest lekarstwem na nierówne wariancje. Gdy grupy mają równe średnie,
      ale różny rozrzut i różne liczebności, test odrzuca H₀ częściej, niż
      wynikałoby z przyjętego poziomu α."),

    lc_p(gloss("test Wilcoxona", "Test Wilcoxona"), " dla jednej próby i dla par
      ma inne założenie: symetrię rozkładu wokół badanej wartości. Przy
      symetrycznym rozkładzie środek symetrii jest jednocześnie medianą
      i średnią, więc test odpowiada na pytanie o położenie. Przy silnej
      skośności tego założenia nie ma, a przejście na rangi problemu nie usuwa.
      Testy rangowe nadal wymagają też niezależnych obserwacji."),

    lc_table(
      data.frame(
        c1 = c(
          "Wilcoxon jednej próby",
          "Wilcoxon dla par",
          "Mann–Whitney (suma rang)",
          "Kruskal-Wallis",
          "Spearman"
        ),
        c2 = c(
          "Niezależne obserwacje; symetria rozkładu wokół badanej wartości",
          "Niezależne pary; symetria rozkładu różnic w parach",
          "Niezależne grupy i obserwacje; dane co najmniej porządkowe",
          "Niezależne grupy i obserwacje; dane co najmniej porządkowe",
          "Niezależne pary; związek monotoniczny"
        ),
        c3 = c(
          "H₀: rozkład jest symetryczny wokół zadanej wartości. Przy symetrii
           jest ona również medianą; silna skośność nie znika po użyciu rang.",
          "H₀: rozkład różnic jest symetryczny wokół zera. Obliczamy różnice
           w parach, a nie mieszamy wszystkich pomiarów.",
          "H₀: obie grupy mają ten sam rozkład; test wykrywa tendencję do
           większych wartości w jednej grupie. Nie porównuje średnich.
           Interpretacja jako przesunięcie median wymaga tego samego
           kształtu rozkładów; symetria nie jest wymagana.",
          "H₀: wszystkie grupy mają ten sam rozkład. Nie porównuje średnich;
           porównanie median tylko przy podobnych kształtach rozkładów.
           Post hoc: test Dunna z korektą na porównania wielokrotne.",
          "Działa na rangach, więc jest mniej wrażliwy na wartości odstające
           niż Pearson; nie wykrywa związków niemonotonicznych (np. w kształcie U)."
        )
      ),
      cols = list(
        lc_col("c1", "Metoda", "row"),
        lc_col("c2", "Założenia", "text"),
        lc_col("c3", "Uwagi", "text")
      ),
      narrow = "cards",
      prose = TRUE
    ),

    lc_p("Konsekwencja dla raportu jest prosta: wynik testu rangowego opisujemy
      jako różnicę w rozkładach albo tendencję jednej grupy do wyższych
      wartości, a nie jako różnicę średnich. Gdy pytanie badawcze naprawdę
      dotyczy średnich, na przykład średniego kosztu na osobę, test
      nieparametryczny na nie nie odpowie, nawet jeśli założenia testu t są
      wątpliwe. Wtedy lepiej zostać przy teście Welcha i ocenić, czy próba
      jest wystarczająco duża jak na skośność danych."),

    # ========================================================================
    # Testy dla zmiennych jakościowych
    # ========================================================================
    lc_h2("ch4-jakosciowe", "Testy dla zmiennych jakościowych"),

    lc_p("Testy dla zmiennych jakościowych nie pytają o normalność ani wariancje,
      bo pracują na liczebnościach kategorii. Ich założenia są dwa: niezależne
      obserwacje (każda osoba trafia do tabeli raz) oraz liczebności, a nie
      procenty, w komórkach. Test χ² ma trzecie, omówione w rozdziale 03:
      rozkład χ² jest tylko przybliżeniem rozkładu statystyki, więc ",
      gloss("liczebność oczekiwana", "liczebności oczekiwane"), " nie mogą być
      zbyt małe. Często podawana orientacyjna reguła to co najmniej 5 w każdej
      komórce. Nie jest to ostra granica, tylko sygnał, że wynik warto sprawdzić
      metodą, która z przybliżenia nie korzysta."),

    lc_p("Takimi metodami są ", gloss("test dokładny Fishera"), " i ",
      gloss("test dwumianowy"), ". Liczą p-wartość dokładnie, więc założenie
      o wielkości próby ich nie dotyczy. Drugą możliwością jest p-wartość
      z symulacji Monte Carlo, w której komputer losuje wiele tabel zgodnych
      z H₀ zamiast korzystać z rozkładu χ²."),

    lc_table(
      data.frame(
        c1 = c("χ² zgodności", "χ² niezależności", "Test Fishera", "Test dwumianowy"),
        c2 = c(
          "Niezależne obserwacje; liczebności oczekiwane niezbyt małe
           (orientacyjnie co najmniej 5 w każdej kategorii)",
          "Niezależne obserwacje; liczebności oczekiwane niezbyt małe
           (orientacyjnie co najmniej 5 w każdej komórce)",
          "Niezależne obserwacje",
          "Niezależne obserwacje; dwie kategorie; to samo prawdopodobieństwo
           sukcesu dla każdej obserwacji"
        ),
        c3 = c(
          "Test dwumianowy (2 kategorie); p-wartość z symulacji Monte Carlo",
          "Test dokładny Fishera; p-wartość z symulacji Monte Carlo",
          "Metoda dokładna, nie wymaga dużej próby. Niezależności nie
           zastąpi: danych sparowanych (te same osoby dwa razy) nie
           analizuje się tym testem",
          "Metoda dokładna, nie wymaga dużej próby"
        )
      ),
      cols = list(
        lc_col("c1", "Metoda", "row"),
        lc_col("c2", "Założenia", "text"),
        lc_col("c3", "Gdy naruszone", "text")
      ),
      narrow = "cards",
      prose = TRUE
    ),

    # ========================================================================
    # Regresja
    # ========================================================================
    lc_h2("ch4-regresja", "Regresja"),

    lc_p("Ostatnia grupa wybiega w przyszłość, do wykładu 06. ",
      gloss("regresja liniowa", "Regresja liniowa"), " ma założenia podobne do
      ANOVA, ale formułuje się je dla ", gloss("reszta", "reszt"), ", czyli
      różnic między obserwowaną a przewidywaną wartością, a nie dla surowych
      danych. Zmienna zależna nie musi mieć rozkładu normalnego. Normalne
      w przybliżeniu powinny być reszty, a i to ma znaczenie głównie przy
      małych próbach. Ważniejsze są liniowość związku i ",
      gloss("homoskedastyczność"), ", czyli podobny rozrzut reszt dla wszystkich
      wartości predyktora. W regresji wielorakiej dochodzi ",
      gloss("współliniowość"), " predyktorów. Tabela służy na razie jako
      zapowiedź: każdą z tych diagnostyk omówimy na przykładach."),

    lc_table(
      data.frame(
        c1 = c("Regresja liniowa", "Regresja logistyczna"),
        c2 = c(
          "Liniowość, niezależność reszt, homoskedastyczność, reszty
           w przybliżeniu normalne, brak silnej współliniowości",
          "Liniowa zależność logitu od predyktorów, niezależne obserwacje,
           brak silnej współliniowości, wystarczająco dużo zdarzeń
           rzadszej kategorii na każdy predyktor (orientacyjnie około 10)"
        ),
        c3 = c(
          "Reszty względem wartości przewidywanych, wykres Q-Q reszt,
           Scale-Location; pomocniczo Breusch-Pagan, Durbin-Watson
           (dane uporządkowane w czasie), VIF",
          "Test Hosmera-Lemeshowa, reszty dewiancji, VIF"
        ),
        c4 = c(
          "Transformacje, odporne błędy standardowe (HC), ważona MNK (WLS),
           GLM, GAM, bootstrap",
          "Regresja Firtha (przy rzadkich zdarzeniach i separacji),
           regularyzacja, drzewa decyzyjne"
        )
      ),
      cols = list(
        lc_col("c1", "Metoda", "row"),
        lc_col("c2", "Założenia", "text"),
        lc_col("c3", "Diagnostyka", "text"),
        lc_col("c4", "Alternatywy", "text")
      ),
      narrow = "cards",
      prose = TRUE
    ),

    # ========================================================================
    # WIDGET: Interaktywny selektor
    # ========================================================================
    lc_h2("ch4-selektor", "Selektor: mam tę metodę — co sprawdzić?"),

    lc_p("Tabele dobrze się przegląda, ale przy konkretnej analizie zwykle wychodzi
      się od jednej metody. Selektor poniżej zbiera dla niej w jednym miejscu
      założenia, diagnostykę i alternatywy."),

    figure_panel(
      label = "Ryc. 4.1",
      title = "Sprawdzarka założeń",
      lc_toolbar(
        selectInput("ch4_method", "Metoda",
            choices = c(
              "Test t jednej próby" = "t_one",
              "Test t dla prób niezależnych" = "t_ind",
              "Test t dla par" = "t_paired",
              "ANOVA" = "anova",
              "Korelacja Pearsona" = "pearson",
              "Korelacja Spearmana" = "spearman",
              "Mann–Whitney" = "mann_whitney",
              "Kruskal-Wallis" = "kruskal",
              "χ² niezależności" = "chi_sq",
              "Test Fishera" = "fisher",
              "Regresja liniowa" = "lm",
              "Regresja logistyczna" = "glm"
            ),
            selected = "t_ind"
          )
      ),
      uiOutput("ch4_method_info")
    ),

    lc_p("Domyślnie wybrany test t dla prób niezależnych dobrze pokazuje, jak
      czytać selektor. Założenie równych wariancji jest opisane jako dotyczące
      tylko wersji Studenta, a pierwsza alternatywa to test Welcha, który
      ten kurs przyjmuje jako wybór domyślny. Druga alternatywa, test Manna–Whitneya,
      ma dopisek, że porównuje rangi, a nie średnie, i nie pomaga przy
      nierównych wariancjach. Wybór między nimi zależy
      od pytania badawczego, a nie od tego, który test daje mniejszą p-wartość."),

    lc_note("Zasada", rule = TRUE,
      "Niezależność wynika z projektu badania. Pozostałe założenia oceniaj
       najpierw na wykresie, a test formalny traktuj pomocniczo. Alternatywa
       nieparametryczna odpowiada na inne pytanie niż test, który zastępuje."
    ),

    # ========================================================================
    # Domknięcie wykładu
    # ========================================================================
    lc_p("Ten wykład uzupełnił wykład 04 o pytanie, kiedy wynikowi testu można
      ufać. Normalność nie jest celem samym w sobie: liczy się rozkład
      średniej, różnic w parach albo reszt, a wykres surowych danych pomaga
      ocenić, czy przybliżenie jest wiarygodne. Przy większych próbach
      i umiarkowanej skośności centralne twierdzenie graniczne robi większość
      pracy. Problem nierównych wariancji w porównaniu grup najprościej
      rozwiązuje wersja Welcha. Dla tabel kluczowe są liczebności
      oczekiwane, a gdy są małe, zostaje test Fishera. W korelacji najpierw
      patrzy się na wykres rozrzutu, bo to on pokazuje, czy związek jest
      liniowy, czy tylko monotoniczny."),

    lc_p("Następny wykład, poświęcony regresji, przenosi te same pomysły na
      modele. Tam założenia dotyczą reszt, a nie samych zmiennych, i ocenia
      się je tymi samymi narzędziami: wykresem Q-Q, porównaniem rozrzutu
      i wykresem reszt względem wartości przewidywanych. Zanim do niego
      przejdziemy, rozdział 05 zbiera cały ten wykład w zwięzłej ściądze."),

    lc_chapter_next(
      num = "05",
      title = "Ściąga",
      lead = "kompaktowa referencja do diagnostyki i alternatyw.",
      target_id = "ch-sciaga"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch4_server <- function(input, output, session) {

  method_info <- list(
    t_one = list(
      name = "Test t jednej próby",
      assumptions = c("Niezależne obserwacje",
                      "Średnia z próby w przybliżeniu normalna: dane bez silnej skośności i wartości odstających albo odpowiednio duża próba"),
      checks = c("Wykres Q-Q (najpierw)", "Pomocniczo: test Shapiro-Wilka"),
      alternatives = c("Przy symetrii: test Wilcoxona dla jednej próby",
                       "Przy silnej skośności Wilcoxon nie jest automatycznym zamiennikiem; najpierw ustal, czy pytanie dotyczy średniej")
    ),
    t_ind = list(
      name = "Test t dla prób niezależnych",
      assumptions = c("Niezależne obserwacje i grupy",
                      "Średnie w obu grupach w przybliżeniu normalne (brak silnej skośności lub odpowiednio duże grupy)",
                      "Równe wariancje — tylko w wersji Studenta"),
      checks = c("Wykresy Q-Q w grupach (najpierw)", "Pomocniczo: test Shapiro-Wilka",
                 "Odchylenia standardowe w grupach (opisowo); wyboru między Welchem a Studentem nie uzależniaj od testu Levene'a"),
      alternatives = c("Test t Welcha jako wybór domyślny",
                       "Test Manna–Whitneya: porównuje rangi, nie średnie, i nie rozwiązuje problemu nierównych wariancji")
    ),
    t_paired = list(
      name = "Test t dla par",
      assumptions = c("Niezależne pary",
                      "Średnia różnic w przybliżeniu normalna (rozkład różnic, nie obu pomiarów osobno)"),
      checks = c("Wykres Q-Q różnic (najpierw)", "Pomocniczo: test Shapiro-Wilka na różnicach"),
      alternatives = c("Przy symetrii różnic: test Wilcoxona dla par")
    ),
    anova = list(
      name = "ANOVA jednoczynnikowa",
      assumptions = c("Niezależne obserwacje",
                      "Reszty (odchylenia od średniej grupy) w przybliżeniu normalne",
                      "Równe wariancje — tylko w wersji klasycznej"),
      checks = c("Wykres Q-Q reszt (najpierw)", "Odchylenia standardowe w grupach (opisowo); wyboru wersji ANOVA nie uzależniaj od testu Levene'a"),
      alternatives = c("ANOVA Welcha + post hoc Games-Howella",
                       "Test Kruskala-Wallisa + post hoc Dunna; nie porównuje średnich")
    ),
    pearson = list(
      name = "Korelacja Pearsona",
      assumptions = c("Niezależne pary", "Związek liniowy", "Brak silnych wartości odstających",
                      "Dla testu: rozkład obu zmiennych łącznie zbliżony do normalnego"),
      checks = c("Wykres rozrzutu (najpierw)", "Pomocniczo: wykresy Q-Q obu zmiennych"),
      alternatives = c("Korelacja Spearmana",
                       "Tau Kendalla")
    ),
    spearman = list(
      name = "Korelacja Spearmana",
      assumptions = c("Niezależne pary", "Związek monotoniczny"),
      checks = c("Wykres rozrzutu"),
      alternatives = c("Tau Kendalla przy małych próbach i wielu remisach")
    ),
    mann_whitney = list(
      name = "Test Manna–Whitneya",
      assumptions = c("Niezależne grupy i obserwacje", "Dane co najmniej porządkowe",
                      "H₀: oba rozkłady identyczne; to nie jest test średnich, a porównanie median wymaga tego samego kształtu rozkładów"),
      checks = c("Projekt badania (niezależność)", "Histogramy lub wykresy pudełkowe w grupach (kształt)"),
      alternatives = c("Test t Welcha, gdy pytanie dotyczy średnich", "Test permutacyjny")
    ),
    kruskal = list(
      name = "Test Kruskala-Wallisa",
      assumptions = c("Niezależne grupy i obserwacje", "Dane co najmniej porządkowe",
                      "H₀: wszystkie rozkłady identyczne; porównanie median tylko przy podobnych kształtach"),
      checks = c("Projekt badania (niezależność)", "Wykresy pudełkowe, histogramy w grupach (kształt)"),
      alternatives = c("Post hoc: test Dunna z korektą", "ANOVA Welcha, gdy pytanie dotyczy średnich",
                       "Test permutacyjny")
    ),
    chi_sq = list(
      name = "Test χ² niezależności",
      assumptions = c("Niezależne obserwacje", "W komórkach liczebności, nie procenty",
                      "Liczebności oczekiwane niezbyt małe (orientacyjnie co najmniej 5)"),
      checks = c("Tabela liczebności oczekiwanych"),
      alternatives = c("Test dokładny Fishera",
                       "P-wartość z symulacji Monte Carlo")
    ),
    fisher = list(
      name = "Test dokładny Fishera",
      assumptions = c("Niezależne obserwacje"),
      checks = c("Projekt badania (niezależność)"),
      alternatives = c("Metoda dokładna, nie wymaga dużej próby",
                       "Dane sparowane (te same osoby dwa razy) wymagają innego testu")
    ),
    lm = list(
      name = "Regresja liniowa",
      assumptions = c("Liniowość związku", "Niezależność reszt", "Homoskedastyczność reszt",
                      "Reszty w przybliżeniu normalne", "Brak silnej współliniowości (w regresji wielorakiej)"),
      checks = c("Reszty względem wartości przewidywanych", "Wykres Q-Q reszt", "Scale-Location",
                 "Test Breuscha-Pagana",
                 "Test Durbina-Watsona (dane w czasie)", "VIF"),
      alternatives = c("Transformacja Y lub X", "Odporne błędy standardowe (HC)",
                       "Ważona MNK (WLS)", "GLM, GAM, bootstrap")
    ),
    glm = list(
      name = "Regresja logistyczna",
      assumptions = c("Liniowa zależność logitu od predyktorów", "Niezależne obserwacje",
                      "Brak silnej współliniowości",
                      "Wystarczająco dużo zdarzeń rzadszej kategorii na predyktor (orientacyjnie około 10)"),
      checks = c("Test Hosmera-Lemeshowa", "VIF",
                 "Liczba zdarzeń na predyktor"),
      alternatives = c("Regresja Firtha", "Regularyzacja",
                       "Drzewa decyzyjne")
    )
  )

  output$ch4_method_info <- renderUI({
    info <- method_info[[input$ch4_method]]
    if (is.null(info)) return(NULL)

    tagList(
      h4(info$name),
      lc_status(
        lc_verdict(tags$strong("Założenia:"), type = "warning"),
        tags$ul(lapply(info$assumptions, tags$li))
      ),
      lc_status(
        tags$strong("Jak sprawdzić:"),
        tags$ul(lapply(info$checks, tags$li))
      ),
      lc_status(
        lc_verdict(tags$strong("Alternatywy:"), type = "ok"),
        tags$ul(lapply(info$alternatives, tags$li))
      )
    )
  })
}
