# ============================================================================
# CHAPTER 5: Regresja logistyczna
# ============================================================================

ch5_ui <- list(
  id    = "ch-logistyczna",
  num   = "05",
  title = "Regresja logistyczna",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 05 · Regresja",
      num   = "05",
      title  = "Regresja logistyczna.",
      lead   = "Zdał albo nie zdał, kupił albo nie kupił. Gdy wynik ma tylko dwie
                wartości, prosta przestaje wystarczać. Model ma wtedy mówić,
                jak prawdopodobne jest zdarzenie, i nigdy nie wychodzić poza
                przedział od 0 do 1."
    ),

    lc_p("Wszystkie modele z poprzednich rozdziałów przewidywały liczbę: wynik
      testu, ocenę, masę ciała. Wiele pytań dotyczy jednak zdarzeń, które
      zachodzą albo nie: student zdał albo nie zdał, klient kliknął albo nie,
      pacjent przeżył albo nie. ", gloss("regresja liniowa", "Regresję liniową"), " da się wtedy policzyć, ale jej
      odpowiedzi przestają mieć sens. W tym rozdziale zobaczymy dlaczego
      i poznamy model zbudowany do takich danych."),

    lc_h2("ch5-od-ciaglej-do-binarnej", "Od wyniku ciągłego do zmiennej 0/1"),

    lc_p(gloss("zmienna", "Zmienna"), ", która przyjmuje tylko wartości 0 i 1, nie jest dla nas nowa.
      W wykładzie 03 każdą taką ", gloss("obserwacja", "obserwację"), " nazywaliśmy ",
      gloss("próba Bernoulliego", "próbą Bernoulliego"), ": sukces zachodzi
      z prawdopodobieństwem p, a liczba sukcesów w n niezależnych próbach ma ",
      gloss("rozkład dwumianowy"), ". W wykładach 04 i 05 szacowaliśmy p jako
      odsetek sukcesów w próbie i budowaliśmy dla niego przedziały i testy.
      Zawsze było to jedno p dla całej ", gloss("populacja", "populacji"), "."),

    lc_p(gloss("regresja logistyczna", "Regresja logistyczna"), " robi krok
      dalej: pozwala, żeby prawdopodobieństwo sukcesu zależało od ", gloss("predyktor", "predyktorów"), ".
      Zamiast pytać, jaki odsetek okręgów osiąga dobry wynik, pytamy, jak ten
      odsetek zmienia się z dochodem albo z udziałem uczniów uczących się
      angielskiego. ", gloss("zmienna zależna", "Zmienną zależną"), " jest
      zdarzenie 0/1, a model opisuje prawdopodobieństwo, że Y = 1."),

    lc_p("Żeby przejście było widoczne, wrócimy do danych CASchools z poprzednich
      rozdziałów. Wynik z czytania jest liczbą, więc sami tworzymy z niego
      zdarzenie: okręg „zdał”, jeśli jego średni wynik osiąga ustalony próg.
      To zabieg demonstracyjny, a nie zalecenie. Zamiana wyniku punktowego
      na 0/1 wyrzuca informację: okręg tuż pod progiem i okręg daleko pod nim
      stają się nieodróżnialne. Jeśli wynik liczbowy odpowiada na ",
      gloss("pytanie badawcze"), ", lepiej modelować go bezpośrednio.
      Regresja logistyczna jest naturalnym wyborem wtedy, gdy samo zdarzenie
      jest binarne."),

    lc_p("Górny wykres w panelu pokazuje oryginalne wyniki i próg, czyli
      jeszcze nie model, tylko definicję Y. Dolny pokazuje prawdopodobieństwa
      zdania, które model logistyczny przypisał okręgom na podstawie dochodu,
      odsetka uczniów z dotacją do obiadu i odsetka uczniów uczących się
      angielskiego jako drugiego języka."),

    figure_panel(
      label = "Ryc. 5.1", title = "Od wyniku punktowego do prawdopodobieństwa zdania",
      full_width = TRUE,
      # Model logistyczny dla progu liczy przeglądarka (modules/figures.js, rycina s07_caslogit).
      lc_sf_figure("s07_caslogit", config = ch5_cas_config, legend = TRUE,
        control = lc_sf_slider("ch5_cas_y_cut", "Próg zaliczenia: zdał od", 630, 680, 656, 1),
        toolbar = lc_readouts(tags$div(`data-role` = "stats", style = "display: contents")),
        body = tagList(
          tags$h4("Krok 1: wynik ciągły i próg zaliczenia"),
          lc_sf_chart(0, "2.2/1", "280px"),
          tags$h4("Krok 2: model logistyczny daje prawdopodobieństwo klasy 1"),
          lc_sf_chart(1, "2/1", "310px")),
        after = tagList(tags$div(`data-role` = "table"), tags$div(`data-role` = "note")))
    ),

    lc_p("Domyślny próg 656 pkt leży niemal dokładnie w ", gloss("mediana", "medianie"), " wyników
      (655.75 pkt), więc „zdaje” 208 z 420 okręgów, czyli 49.5%. Na dolnym
      wykresie każdy okręg dostaje własne prawdopodobieństwo. Okręgi zamożne
      i z małym odsetkiem dotacji do obiadu mają prawdopodobieństwa bliskie 1,
      biedniejsze bliskie 0, a środek skali zajmują okręgi, o których model
      nie ma pewności. Kolor punktu mówi, jak było naprawdę: wśród okręgów
      z wysokim prawdopodobieństwem zdarzają się takie, które progu nie
      osiągnęły, i odwrotnie."),

    lc_p("Przesunięcie progu zmienia samą zmienną zależną, więc zmieniają się
      też współczynniki. Przy progu 630 pkt zdaje 374 okręgi (89%), przy
      680 pkt tylko 48 (11%). To dwa różne pytania i dwa różne modele. Z tego
      samego powodu ", gloss("AIC"), " i ", gloss("BIC"), " pod wykresem można porównywać tylko między
      modelami dla tej samej definicji Y, a nie między progami. Tabelę ilorazów
      szans omówimy za chwilę, gdy będzie jasne, czym są szanse."),

    lc_h2("ch5-dlaczego", "Regresja liniowa na danych 0/1"),

    lc_p("Skoro Y przyjmuje wartości 0 i 1, można spróbować najprostszego
      rozwiązania: dopasować zwykłą prostą metodą z rozdziału 01 i czytać jej
      wartości przewidywane jako prawdopodobieństwa. Panel porównuje to podejście
      z modelem logistycznym na stałym przykładzie: 180 studentów, od 0 do 40
      godzin nauki, egzamin zdało 90 z nich. Suwak wybiera liczbę godzin,
      a odczyty podają prawdopodobieństwo zdania przewidywane przez oba
      modele."),

    figure_panel(
      label = "Ryc. 5.2", title = "Liniowy vs logistyczny na danych binarnych",
      full_width = TRUE,
      # Krzywe z R przy budowaniu, odczyt dla suwaka w przeglądarce (rycina s07_linlog).
      lc_sf_figure("s07_linlog", config = ch5_linlog_config,
        control = lc_sf_slider("ch5_lin_log_x", "Godziny nauki", 0, 40, 2, 1),
        toolbar = lc_readouts(tags$div(`data-role` = "stats", style = "display: contents")),
        ratio = "2.2/1", max_height = "320px"),
      lc_caption("Stały przykład: zdanie egzaminu (0/1) a liczba godzin nauki.")
    ),

    lc_p("Prosta przecina zero przy około 5.7 godziny nauki i przekracza
      jedynkę przy około 34.3 godziny. Dla studenta, który nie uczył się wcale,
      przewiduje prawdopodobieństwo zdania -0.20, a dla tego, który uczył się
      40 godzin, 1.20. Takich liczb nie da się czytać jako prawdopodobieństw,
      a w tym przykładzie poza przedziałem [0, 1] wypada 28.9% wartości
      przewidywanych."),

    lc_p("Warto zauważyć, czego panel nie pokazuje. Jeśli oba modele zamienić
      na decyzję „zda / nie zda” przy granicy 0.5, każdy trafia w 91.1%
      przypadków. Problemem prostej nie jest więc klasyfikacja, tylko
      prawdopodobieństwa. Prosta zakłada, że każda godzina zmienia szansę
      zdania o tyle samo, niezależnie od punktu startu. Tymczasem student,
      który i tak prawie na pewno zda, nie może zyskać tyle, co ktoś, kto stoi
      na granicy. Potrzebujemy krzywej, która w środku rośnie szybko, a przy
      0 i 1 się wypłaszcza."),

    lc_h2("ch5-krzywa", "Krzywa logistyczna"),

    lc_p("Taką krzywą otrzymamy, jeśli liniowo będziemy modelować nie samo
      prawdopodobieństwo, lecz wielkość, która nie ma górnej ani dolnej
      granicy. Pierwszym krokiem są szanse: stosunek prawdopodobieństwa
      sukcesu do prawdopodobieństwa porażki, p/(1 − p). Gdy p = 0.8, szanse
      wynoszą 0.8/0.2 = 4, czyli „4 do 1”: na cztery sukcesy przypada przeciętnie
      jedna porażka. Gdy p = 0.5, szanse wynoszą 1. Szanse przyjmują dowolne
      wartości dodatnie. Po zlogarytmowaniu dostajemy ", gloss("log-szanse", "logit"), ", który może być
      dowolną liczbą, ujemną dla p < 0.5 i dodatnią dla p > 0.5. Regresja
      logistyczna zakłada, że to logit zależy od predyktora liniowo:"),

    # Logit ze znacznikami symboli
    lc_formula_annotated(
      "⟦1|\\text{logit}(p)⟧ = \\ln⟦2|\\frac{p}{1 - p}⟧ = ⟦3|\\beta_0⟧ + ⟦4|\\beta_1⟧ x",
      notes  = list("log-szanse", "szanse", "wyraz wolny", "nachylenie"),
      colors = c("accent", "bursztyn", "terakota", "indygo"),
      above  = c(2, 4)
    ),

    lc_p("Po przekształceniu tego równania względem p otrzymujemy ",
      gloss("funkcja logistyczna", "funkcję logistyczną"), ", nazywaną też
      sigmoidą od kształtu litery S:"),

    # Sigmoida ze znacznikami symboli
    lc_formula_annotated(
      "⟦1|p⟧ = P(Y = 1) = \\frac{1}{1 + e^{-(⟦2|\\beta_0 + \\beta_1 x⟧)}}",
      notes  = list("prawdopodobieństwo sukcesu", "logit"),
      colors = c("niebo", "accent"),
      above  = c(2)
    ),

    lc_p("Niezależnie od tego, jak duże albo małe jest β₀ + β₁x, wynik leży
      między 0 a 1. Przy wielu predyktorach w wykładniku pojawia się po prostu
      więcej składników, tak jak w ", gloss("regresja wieloraka", "regresji wielorakiej"), ". Panel rysuje sigmoidę
      dla wybranych wartości β₀ i β₁."),

    figure_panel(
      label = "Ryc. 5.3", title = "Sigmoida w akcji",
      full_width = TRUE,
      # Krzywą i gotowe ustawienia obsługuje przeglądarka (rycina s07_sigmoid).
      lc_sf_figure("s07_sigmoid",
        control = list(lc_sf_slider("ch5_b0", "β₀ (wyraz wolny)", -10, 10, -4, 0.5),
                       lc_sf_slider("ch5_b1", "β₁ (nachylenie)", -3, 3, 0.2, 0.05)),
        toolbar = lc_sf_action_group("Ustawienia", steep = "Stromy", flat = "Płaski", neg = "Odwrotny"),
        ratio = "2.2/1", max_height = "350px")
    ),

    lc_p("Przy ustawieniu startowym, β₀ = -4 i β₁ = 0.2, krzywa przechodzi
      przez 0.5 w punkcie x = −β₀/β₁ = 20. Dla x = 0 prawdopodobieństwo wynosi
      około 0.02, a dla x = 40 około 0.98. Oba parametry mają czytelne role.
      Współczynnik β₁ decyduje o kierunku i stromości: im większy co do wartości
      bezwzględnej, tym szybsze przejście od 0 do 1, a ujemny odwraca krzywą,
      jak w ustawieniu „Odwrotny”. ", gloss("wyraz wolny", "Wyraz wolny"), " β₀ przesuwa krzywą w poziomie.
      Ustawienie „Płaski” ma ten sam środek co startowe, ale w całym
      narysowanym zakresie prawdopodobieństwo zmienia się tylko od 0.22
      do 0.78."),

    lc_p("Krzywa nie ma jednego nachylenia, tak jak prosta. Najszybciej rośnie
      w środku, przy p = 0.5, gdzie wzrost x o jednostkę zmienia
      prawdopodobieństwo o około β₁/4. Przy ustawieniu startowym to
      0.05, czyli 5 punktów procentowych na jednostkę. Bliżej 0 i 1 ten sam
      wzrost x zmienia prawdopodobieństwo coraz mniej. Dlatego współczynnika
      w regresji logistycznej nie da się przeczytać jako stałej zmiany
      prawdopodobieństwa i potrzebujemy innej interpretacji."),

    lc_h2("ch5-model-dane", "Model logistyczny na danych"),

    lc_p("W praktyce β₀ i β₁ nie ustawiamy suwakami, tylko szacujemy z danych.
      ", gloss("metoda najmniejszych kwadratów", "Metoda najmniejszych kwadratów"), " z rozdziału 01 nie pasuje do wyników 0/1,
      więc używa się metody największej wiarygodności: wybiera się takie
      współczynniki, przy których zaobserwowany układ zer i jedynek jest
      najbardziej prawdopodobny. To ten sam rachunek prawdopodobieństwa prób
      Bernoulliego co w wykładzie 03, tylko p każdej osoby zależy od jej
      predyktorów."),

    lc_p("Panel losuje dane studentów z modelu, którego współczynniki znamy:
      logit = -4 + 0.08 · godziny nauki + 1.2 · średnia ocen. Godzin jest od 0
      do 40, średnia ocen ma średnio 3.5, a egzamin zdaje przeciętnie około 81%
      studentów. Każde dopasowanie to nowa próba, więc oszacowania za każdym
      razem nieco się różnią. Wykres pokazuje krzywą dla wybranego predyktora
      przy drugim ustalonym na średniej, a pole pod suwakami podaje
      przewidywane prawdopodobieństwo zdania dla nowego studenta."),

    figure_panel(
      label = "Ryc. 5.4", title = "Predykcja zdania egzaminu",
      full_width = TRUE,
      # Losowanie i model logistyczny (IRLS) liczy przeglądarka (rycina s07_logit).
      lc_sf_figure("s07_logit",
        control = list(lc_sf_slider("ch5_n", "n", 50, 300, 150, 25),
          selectInput("ch5_predictor", "Prezentowany predyktor", selectize = FALSE,
            choices = c("Godziny nauki" = "godziny_nauki", "Średnia ocen" = "srednia_ocen"), selected = "godziny_nauki")),
        toolbar = tagList(lc_sf_action("fit", "ch5_fit", "Dopasuj model", variant = "solid"),
                          lc_readouts(tags$div(`data-role` = "stats", style = "display: contents"))),
        ratio = "2.2/1", max_height = "350px",
        after = tagList(
          tags$h4("Predykcja dla nowego studenta"),
          lc_toolbar(
            lc_group(NULL, grow = TRUE, lc_sf_ctl(numericInput("ch5_pred_hours", "Godziny nauki", value = 20, min = 0, max = 40))),
            lc_group(NULL, grow = TRUE, lc_sf_ctl(numericInput("ch5_pred_gpa", "Średnia ocen", value = 3.5, min = 2, max = 5, step = 0.1))),
            lc_readouts(tags$div(`data-role` = "pred", style = "display: contents")))))
    ),

    lc_p("Dla studenta, który uczył się 20 godzin i ma średnią 3.5, prawdziwy
      model daje logit -4 + 1.6 + 4.2 = 1.8, czyli prawdopodobieństwo zdania
      około 0.86. Oszacowanie z panelu leży zwykle blisko tej wartości,
      a przy mniejszym n rozrzut między kolejnymi próbami jest większy.
      Punkty na wykresie leżą wyłącznie na wysokości 0 albo 1, a krzywa
      przechodzi między nimi. Model nie przewiduje więc, czy konkretna osoba
      zda, tylko jak często zdawaliby studenci o takich samych wartościach
      predyktorów."),

    lc_h2("ch5-iloraz-szans", "Interpretacja: iloraz szans"),

    lc_p("Współczynnik β₁ opisuje zmianę logitu, a logarytm szans trudno sobie
      wyobrazić. Wystarczy jednak wrócić z logarytmu do szans. Jeśli x rośnie
      o 1, logit rośnie o β₁, więc szanse mnożą się przez \\(e^{\\beta_1}\\). Tę liczbę
      nazywamy ", gloss("iloraz szans", "ilorazem szans"), " (OR, od ang.
      odds ratio): to stosunek szans przy x + 1 do szans przy x."),

    # Iloraz szans ze znacznikami symboli
    lc_formula_annotated(
      "⟦1|OR⟧ = \\frac{\\text{szanse przy } x + 1}{\\text{szanse przy } x} = e^{⟦2|\\beta_1⟧}",
      notes  = list("iloraz szans", "nachylenie logitu"),
      colors = c("indygo", "indygo"),
      above  = c(2)
    ),

    lc_p("OR = 1 oznacza brak związku, OR > 1 wzrost szans, a OR < 1 ich spadek.
      W modelu CASchools z progiem 656 pkt iloraz szans dla dochodu wynosi 1.18.
      Każdy dodatkowy tysiąc dolarów dochodu mnoży szanse zdania przez 1.18,
      czyli podnosi je o 18%, przy tych samych odsetkach dotacji do obiadu
      i uczniów uczących się angielskiego. Dla dotacji do obiadu OR wynosi 0.93, a dla
      angielskiego 0.92: każdy dodatkowy punkt procentowy obniża szanse
      zdania o około 7–8%. Jak w każdej regresji na ", gloss("dane obserwacyjne", "danych obserwacyjnych"), ", to
      opis związku, a nie dowód, że dochód sam podnosi wyniki."),

    lc_p("Najczęstszy błąd polega na czytaniu „szanse rosną o 18%” jako
      „prawdopodobieństwo rośnie o 18%”. Ten sam iloraz szans oznacza różne
      zmiany prawdopodobieństwa w zależności od punktu startu. Okręg
      z prawdopodobieństwem 0.5 ma szanse 1. Po pomnożeniu przez 1.18 szanse
      wynoszą 1.18, a prawdopodobieństwo 0.54. Okręg z prawdopodobieństwem 0.9
      ma szanse 9, po pomnożeniu 10.6, a prawdopodobieństwo rośnie tylko do
      0.91. Ilorazy szans się mnożą: wzrost dochodu o 10 tys. USD mnoży szanse
      przez 1.18¹⁰ ≈ 5.3, a nie przez 1 + 10 · 0.18."),

    lc_note("Zasada", rule = TRUE,
      "Iloraz szans mnoży szanse, a nie prawdopodobieństwo. Żeby powiedzieć,
       o ile zmienia się prawdopodobieństwo, trzeba podać punkt startu."
    ),

    lc_p("Tabela poniżej pokazuje współczynniki modelu dopasowanego w panelu
      Ryc. 5.4: β na skali logitu, iloraz szans, jego 95% ", gloss("przedział ufności"), "
      i ", gloss("p-wartość"), "."),

    figure_panel(
      label = "Ryc. 5.5", title = "Ilorazy szans",
      full_width = TRUE,
      # Tabela z modelu z Ryc. 5.4 (modules/figures.js, rycina s07_or).
      s07_reads_figure("s07_or", tags$div(`data-role` = "table")),
      lc_caption("Używa modelu dopasowanego w Ryc. 5.4.")
    ),

    lc_p("Prawdziwe ilorazy szans w modelu, z którego panel losuje dane, to
      \\(e^{0.08} \\approx 1.08\\) dla godziny nauki i \\(e^{1.2} \\approx 3.32\\) dla punktu średniej
      ocen. Oszacowania z panelu wahają się wokół tych wartości. Większy OR
      dla średniej nie znaczy, że średnia ocen jest ważniejsza: jeden punkt
      średniej to duża różnica, a jedna godzina nauki mała. Dziesięć godzin
      daje OR = 1.08¹⁰ ≈ 2.23. Iloraz szans zależy od jednostki, w której
      mierzymy predyktor."),

    lc_p("Przedział ufności czytamy tak jak w wykładzie 04, tylko punktem
      odniesienia jest 1, a nie 0. Przedział, który nie obejmuje 1, odpowiada
      p-wartości poniżej 0.05. Przedział obejmujący 1 oznacza, że dane
      są zgodne z brakiem związku między predyktorem a szansami zdania."),

    lc_h2("ch5-prog", "Próg klasyfikacji i macierz pomyłek"),

    lc_p("Model logistyczny zwraca prawdopodobieństwo, a w praktyce często
      trzeba podjąć decyzję: dopuścić do poprawki, zaproponować kurs
      wyrównawczy, wysłać przypomnienie. Służy do tego ",
      gloss("próg klasyfikacji"), ": obserwacje z przewidywanym
      prawdopodobieństwem co najmniej równym progowi przypisujemy do klasy 1.
      To inny próg niż ten z początku rozdziału. Próg 656 pkt definiował
      zdarzenie, zanim powstał model. Próg klasyfikacji zamienia wynik modelu
      na decyzję, gdy model jest już dopasowany."),

    lc_p("Skutki decyzji zestawia ", gloss("macierz pomyłek"), ": w wierszach
      rzeczywistość, w kolumnach predykcja. Dokładność to odsetek wszystkich
      trafień. ", gloss("czułość", "Czułość"), " to odsetek rzeczywistych sukcesów, które model
      rozpoznał, a ", gloss("swoistość"), " to odsetek rzeczywistych porażek, które
      rozpoznał. Panel buduje macierz dla modelu z Ryc. 5.4 i wybranego
      progu."),

    figure_panel(
      label = "Ryc. 5.6", title = "Próg klasyfikacji i macierz pomyłek",
      full_width = TRUE,
      # Macierz pomyłek dla progu (modules/figures.js, rycina s07_threshold).
      lc_sf_figure("s07_threshold",
        control = lc_sf_slider("ch5_threshold", "Próg decyzji", 0.1, 0.9, 0.5, 0.05),
        toolbar = lc_readouts(tags$div(`data-role` = "stats", style = "display: contents")),
        ratio = "2.4/1", max_height = "280px"),
      lc_caption("Używa modelu dopasowanego w Ryc. 5.4.")
    ),

    lc_p("Obniżenie progu sprawia, że więcej osób trafia do klasy „zda”:
      czułość rośnie, a swoistość spada. Podniesienie progu działa odwrotnie.
      W danych tego panelu zdaje około 81% studentów, więc przy progu 0.5
      prawie wszyscy dostają predykcję „zda”. Gdyby znać prawdziwy model,
      przy tym progu dokładność wyniosłaby około 82%, czułość około 97%,
      a swoistość tylko około 19%. Model, który każdemu przewiduje zdanie,
      bez patrzenia na dane miałby dokładność 81%. Wysoka dokładność może
      więc wynikać głównie z tego, że jedna klasa jest dużo liczniejsza.
      Przy progu 0.8 czułość i swoistość wynoszą po około 70%."),

    lc_p("Który próg jest właściwy, zależy od kosztów błędów, a nie od samego
      modelu. Jeśli przeoczenie studenta zagrożonego niezdaniem jest
      kosztowne, a zbędne zaproszenie na konsultacje tanie, warto zaakceptować
      niższą dokładność w zamian za lepsze wykrywanie porażek. Do oceny
      samego modelu nie używa się R² w sensie liniowym. Modele dla tej samej
      zmiennej zależnej porównuje się przez AIC i BIC, tak jak w rozdziale 04,
      a trafność decyzji ocenia się macierzą pomyłek."),

    lc_h2("ch5-zalozenia", "Kiedy logistyczna może zawieść"),

    lc_p("Regresja logistyczna nie zakłada normalności ani stałej wariancji
      reszt, więc wykres Q-Q i wykres reszt z rozdziału 02 nie są tu głównymi
      narzędziami. Ma jednak własne warunki. Część z nich wynika z projektu
      badania, część z liczby i układu danych."),

    lc_p("Zmienna zależna ma być naprawdę binarna, a obserwacje niezależne.
      Jeśli ten sam student, klient albo zakład pojawia się w danych wiele
      razy, potrzebny jest model uwzględniający powtórzenia. Dla predyktorów
      ilościowych związek ma być w przybliżeniu liniowy na skali logitu,
      a nie samego prawdopodobieństwa. Tak jak w regresji wielorakiej, silnie
      skorelowane predyktory (", gloss("współliniowość"), ") utrudniają
      rozdzielenie ich wpływu i poszerzają przedziały ufności."),

    lc_p("Dwa problemy są specyficzne dla danych 0/1. Pierwszy to rzadkie
      zdarzenia. O współczynnikach decyduje przede wszystkim liczba
      obserwacji w mniej licznej klasie, a nie wielkość całej próby. Tysiąc
      osób, wśród których zdarzenie wystąpiło 15 razy, niesie niewiele
      informacji o tym, od czego zależy zdarzenie. Często podaje się orientacyjnie
      około 10 zdarzeń na każdy predyktor w modelu. To tylko punkt wyjścia
      do oceny, a nie granica, po której przekroczeniu model staje się
      wiarygodny. W CASchools przy skrajnych progach mniej liczna klasa liczy
      46 albo 48 okręgów na trzy predyktory, więc problem jeszcze nie
      występuje, ale przy kilkudziesięciu okręgach już by się pojawił."),

    lc_p("Drugi problem to separacja. Jeśli predyktor idealnie oddziela zera
      od jedynek, na przykład wszyscy, którzy uczyli się ponad 20 godzin,
      zdali, a wszyscy pozostali nie, metoda największej wiarygodności nie
      ma skończonego rozwiązania. Najlepsze dopasowanie daje coraz bardziej
      stroma sigmoida, więc współczynnik i jego ", gloss("błąd standardowy"), " rosną bez
      ograniczeń, a program zwraca ogromne liczby albo ostrzeżenie. Przy
      rzadkich zdarzeniach i separacji pomaga prostsza specyfikacja, więcej
      danych albo estymacja z karą, która odsuwa współczynniki od wartości
      skrajnych. Najczęściej stosowaną odmianą jest regresja Firtha."),

    lc_table(
      data.frame(
        c1 = c(
          "Y jest binarne",
          "Niezależne obserwacje",
          "Liniowość logitu",
          "Umiarkowana współliniowość",
          "Dość zdarzeń",
          "Brak separacji"
        ),
        c2 = c(
          "modelujemy zdarzenie 0/1: zdał/nie zdał, kupił/nie kupił",
          "ten sam student, klient lub zakład nie powinien pojawiać się wiele razy bez modelu z powtórzeniami",
          "dla predyktorów ilościowych zależność ma być mniej więcej liniowa na skali logarytmu szans",
          "tak jak w regresji wielorakiej: predyktory nie powinny powtarzać tej samej informacji",
          "liczy się mniej liczna klasa; orientacyjnie około 10 zdarzeń na predyktor, bez sztywnej granicy",
          "gdy predyktor idealnie oddziela 0 od 1, współczynniki uciekają do nieskończoności; pomaga estymacja z karą (regresja Firtha)"
        )
      ),
      cols = list(
        lc_col("c1", "Warunek", "row"),
        lc_col("c2", "Co oznacza w praktyce", "text")
      ),
      prose = TRUE
    ),

    # ========================================================================
    # Domknięcie wykładu i kursu
    # ========================================================================
    lc_p("Ten wykład zaczął się od prostej dopasowanej do chmury punktów,
      a skończył na modelu dla zdarzeń 0/1. Po drodze reszty pokazały, czy
      model pasuje do danych, R² i RMSE opisały, ile wyjaśnia i jak bardzo się
      myli, regresja wieloraka pozwoliła oddzielić wpływ predyktorów,
      a przykład pingwinów pokazał, że pominięta zmienna potrafi odwrócić
      kierunek związku. Porównanie modeli nauczyło, że więcej predyktorów nie
      znaczy lepiej. Regresja logistyczna przenosi te same pomysły na
      zdarzenia: zamiast średniej Y modelujemy prawdopodobieństwo,
      a współczynniki czytamy przez ilorazy szans."),

    lc_p("Ten wykład zamyka też część kursu poświęconą metodom. Wykład 01
      nauczył opisywać dane, a wykład 03 opisywać losowość rozkładami
      prawdopodobieństwa. Wykład 04 pokazał, jak z próby oszacować parametr
      razem z niepewnością, wykład 05, jak testować hipotezy, a wykład 06,
      kiedy wynikom testów można ufać. Regresja łączy te wątki: jej
      współczynniki to estymatory z przedziałami ufności i testami, a założenia
      sprawdza się na wykresach. Następne wykłady wykorzystują te narzędzia
      do oceny jakości danych i całych analiz. Ćwiczenia w rozdziale 06 pozwalają
      przećwiczyć samodzielną interpretację modelu."),

    lc_chapter_next(
      num       = "06",
      title     = "Ćwiczenia",
      lead      = "zadania interpretacyjne i porównywanie modeli",
      target_id = "ch-cwiczenia"
    )
  )
)


# ============================================================================
# SERVER
# ============================================================================

# Wszystkie ryciny rozdziału liczy przeglądarka albo R przy budowaniu strony.
ch5_server <- function(input, output, session) {}
