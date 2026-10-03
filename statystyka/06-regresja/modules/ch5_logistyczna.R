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
      pacjent przeżył albo nie. Regresję liniową da się wtedy policzyć, ale jej
      odpowiedzi przestają mieć sens. W tym rozdziale zobaczymy dlaczego
      i poznamy model zbudowany do takich danych."),

    lc_h2("ch5-od-ciaglej-do-binarnej", "Od wyniku ciągłego do zmiennej 0/1"),

    lc_p("Zmienna, która przyjmuje tylko wartości 0 i 1, nie jest dla nas nowa.
      W wykładzie 02 każdą taką obserwację nazywaliśmy ",
      gloss("próba Bernoulliego", "próbą Bernoulliego"), ": sukces zachodzi
      z prawdopodobieństwem p, a liczba sukcesów w n niezależnych próbach ma ",
      gloss("rozkład dwumianowy"), ". W wykładach 03 i 04 szacowaliśmy p jako
      odsetek sukcesów w próbie i budowaliśmy dla niego przedziały i testy.
      Zawsze było to jedno p dla całej populacji."),

    lc_p(gloss("regresja logistyczna", "Regresja logistyczna"), " robi krok
      dalej: pozwala, żeby prawdopodobieństwo sukcesu zależało od predyktorów.
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
      label = "Ryc. 5.0", title = "Od wyniku punktowego do prawdopodobieństwa zdania",
      full_width = TRUE,
      fluidRow(
        column(4,
          lc_slider("ch5_cas_y_cut", "Próg zaliczenia: zdał od", 630, 680, 656, 1),
          uiOutput("ch5_cas_threshold_note")
        ),
        column(8,
          tags$h4("Krok 1: wynik ciągły i próg zaliczenia"),
          lc_plot_fullscreen("ch5_cas_continuous_plot", height = "280px"),
          tags$h4("Krok 2: model logistyczny daje prawdopodobieństwo klasy 1"),
          lc_plot_fullscreen("ch5_cas_logit_plot", height = "310px"),
          uiOutput("ch5_cas_model_table"),
          uiOutput("ch5_cas_model_metrics")
        )
      )
    ),

    lc_p("Domyślny próg 656 pkt leży niemal dokładnie w medianie wyników
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
      samego powodu AIC i BIC pod wykresem można porównywać tylko między
      modelami dla tej samej definicji Y, a nie między progami. Tabelę ilorazów
      szans omówimy za chwilę, gdy będzie jasne, czym są szanse."),

    lc_h2("ch5-dlaczego", "Regresja liniowa na danych 0/1"),

    lc_p("Skoro Y przyjmuje wartości 0 i 1, można spróbować najprostszego
      rozwiązania: dopasować zwykłą prostą metodą z rozdziału 01 i czytać jej
      wartości przewidywane jako prawdopodobieństwa. Panel porównuje to podejście
      z modelem logistycznym na stałym przykładzie: 180 studentów, od 0 do 40
      godzin nauki, egzamin zdało 90 z nich."),

    figure_panel(
      label = "Ryc. 5.1", title = "Liniowy vs logistyczny na danych binarnych",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Stały przykład: zdanie egzaminu (0/1) a liczba godzin nauki.")
        ),
        column(8,
          zoom_plot_ui("ch5_lin_log_plot", height = "300px"),
          uiOutput("ch5_lin_log_stats")
        )
      )
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
      wartości dodatnie. Po zlogarytmowaniu dostajemy logit, który może być
      dowolną liczbą, ujemną dla p < 0.5 i dodatnią dla p > 0.5. Regresja
      logistyczna zakłada, że to logit zależy od predyktora liniowo:"),

    lc_formula_box(withMathJax(
      "$$\\text{logit}(p) = \\ln\\frac{p}{1 - p} = \\beta_0 + \\beta_1 x$$"
    )),

    lc_p("Po przekształceniu tego równania względem p otrzymujemy ",
      gloss("funkcja logistyczna", "funkcję logistyczną"), ", nazywaną też
      sigmoidą od kształtu litery S:"),

    lc_formula_box(withMathJax(
      "$$p = P(Y = 1) = \\frac{1}{1 + e^{-(\\beta_0 + \\beta_1 x)}}$$"
    )),

    lc_p("Niezależnie od tego, jak duże albo małe jest β₀ + β₁x, wynik leży
      między 0 a 1. Przy wielu predyktorach w wykładniku pojawia się po prostu
      więcej składników, tak jak w regresji wielorakiej. Panel rysuje sigmoidę
      dla wybranych wartości β₀ i β₁."),

    figure_panel(
      label = "Ryc. 5.2", title = "Sigmoida w akcji",
      full_width = TRUE,
      fluidRow(
        column(4,
          lc_slider("ch5_b0", "β₀ (wyraz wolny)", -10, 10, -4, 0.5),
          lc_slider("ch5_b1", "β₁ (nachylenie)", -3, 3, 0.2, 0.05),
          hr(),
          div(class = "preset-buttons",
            lc_action("ch5_preset_steep", "Stromy", variant = "outline"),
            lc_action("ch5_preset_flat", "Płaski", variant = "outline"),
            lc_action("ch5_preset_neg", "Odwrotny", variant = "solid")
          )
        ),
        column(8,
          zoom_plot_ui("ch5_sigmoid_plot", height = "350px")
        )
      )
    ),

    lc_p("Przy ustawieniu startowym, β₀ = -4 i β₁ = 0.2, krzywa przechodzi
      przez 0.5 w punkcie x = −β₀/β₁ = 20. Dla x = 0 prawdopodobieństwo wynosi
      około 0.02, a dla x = 40 około 0.98. Oba parametry mają czytelne role.
      Współczynnik β₁ decyduje o kierunku i stromości: im większy co do wartości
      bezwzględnej, tym szybsze przejście od 0 do 1, a ujemny odwraca krzywą,
      jak w ustawieniu „Odwrotny”. Wyraz wolny β₀ przesuwa krzywą w poziomie.
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
      Metoda najmniejszych kwadratów z rozdziału 01 nie pasuje do wyników 0/1,
      więc używa się metody największej wiarygodności: wybiera się takie
      współczynniki, przy których zaobserwowany układ zer i jedynek jest
      najbardziej prawdopodobny. To ten sam rachunek prawdopodobieństwa prób
      Bernoulliego co w wykładzie 02, tylko p każdej osoby zależy od jej
      predyktorów."),

    lc_p("Panel losuje dane studentów z modelu, którego współczynniki znamy:
      logit = -4 + 0.08 · godziny nauki + 1.2 · średnia ocen. Godzin jest od 0
      do 40, średnia ocen ma średnio 3.5, a egzamin zdaje przeciętnie około 81%
      studentów. Każde dopasowanie to nowa próba, więc oszacowania za każdym
      razem nieco się różnią. Wykres pokazuje krzywą dla wybranego predyktora
      przy drugim ustalonym na średniej, a pole pod suwakami podaje
      przewidywane prawdopodobieństwo zdania dla nowego studenta."),

    figure_panel(
      label = "Ryc. 5.3", title = "Predykcja zdania egzaminu",
      full_width = TRUE,
      fluidRow(
        column(4,
          lc_slider("ch5_n", "n", 50, 300, 150, 25),
          selectInput("ch5_predictor", "Prezentowany predyktor:",
            choices = c(
              "Godziny nauki" = "godziny_nauki",
              "Średnia ocen"  = "srednia_ocen"
            ),
            selected = "godziny_nauki"
          ),
          lc_action("ch5_fit", "Dopasuj model", variant = "solid"),
          hr(),
          h5("Predykcja dla nowego studenta:"),
          numericInput("ch5_pred_hours", "Godziny nauki:", value = 20, min = 0, max = 40),
          numericInput("ch5_pred_gpa", "Średnia ocen:", value = 3.5, min = 2, max = 5, step = 0.1),
          uiOutput("ch5_prediction")
        ),
        column(8,
          zoom_plot_ui("ch5_logit_plot", height = "350px"),
          uiOutput("ch5_model_summary")
        )
      )
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

    lc_formula_box(withMathJax(
      "$$OR = \\frac{\\text{szanse przy } x + 1}{\\text{szanse przy } x} = e^{\\beta_1}$$"
    )),

    lc_p("OR = 1 oznacza brak związku, OR > 1 wzrost szans, a OR < 1 ich spadek.
      W modelu CASchools z progiem 656 pkt iloraz szans dla dochodu wynosi 1.18.
      Każdy dodatkowy tysiąc dolarów dochodu mnoży szanse zdania przez 1.18,
      czyli podnosi je o 18%, przy tych samych odsetkach dotacji do obiadu
      i uczniów uczących się angielskiego. Dla dotacji do obiadu OR wynosi 0.93, a dla
      angielskiego 0.92: każdy dodatkowy punkt procentowy obniża szanse
      zdania o około 7–8%. Jak w każdej regresji na danych obserwacyjnych, to
      opis związku, a nie dowód, że dochód sam podnosi wyniki."),

    lc_p("Najczęstszy błąd polega na czytaniu „szanse rosną o 18%” jako
      „prawdopodobieństwo rośnie o 18%”. Ten sam iloraz szans oznacza różne
      zmiany prawdopodobieństwa w zależności od punktu startu. Okręg
      z prawdopodobieństwem 0.5 ma szanse 1. Po pomnożeniu przez 1.18 szanse
      wynoszą 1.18, a prawdopodobieństwo 0.54. Okręg z prawdopodobieństwem 0.9
      ma szanse 9, po pomnożeniu 10.6, a prawdopodobieństwo rośnie tylko do
      0.91. Ilorazy szans się mnożą: wzrost dochodu o 10 tys. USD mnoży szanse
      przez 1.18¹⁰ ≈ 5.3, a nie przez 1 + 10 · 0.18."),

    inline_callout(label = "Zasada",
      "Iloraz szans mnoży szanse, a nie prawdopodobieństwo. Żeby powiedzieć,
       o ile zmienia się prawdopodobieństwo, trzeba podać punkt startu."
    ),

    lc_p("Tabela poniżej pokazuje współczynniki modelu dopasowanego w panelu
      Ryc. 5.3: β na skali logitu, iloraz szans, jego 95% przedział ufności
      i p-wartość."),

    figure_panel(
      label = "Ryc. 5.4", title = "Ilorazy szans",
      full_width = TRUE,
      helpText("Używa modelu dopasowanego w Ryc. 5.3."),
      uiOutput("ch5_odds_ratios")
    ),

    lc_p("Prawdziwe ilorazy szans w modelu, z którego panel losuje dane, to
      \\(e^{0.08} \\approx 1.08\\) dla godziny nauki i \\(e^{1.2} \\approx 3.32\\) dla punktu średniej
      ocen. Oszacowania z panelu wahają się wokół tych wartości. Większy OR
      dla średniej nie znaczy, że średnia ocen jest ważniejsza: jeden punkt
      średniej to duża różnica, a jedna godzina nauki mała. Dziesięć godzin
      daje OR = 1.08¹⁰ ≈ 2.23. Iloraz szans zależy od jednostki, w której
      mierzymy predyktor."),

    lc_p("Przedział ufności czytamy tak jak w wykładzie 03, tylko punktem
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
      trafień. Czułość to odsetek rzeczywistych sukcesów, które model
      rozpoznał, a swoistość to odsetek rzeczywistych porażek, które
      rozpoznał. Panel buduje macierz dla modelu z Ryc. 5.3 i wybranego
      progu."),

    figure_panel(
      label = "Ryc. 5.5", title = "Próg klasyfikacji i macierz pomyłek",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Używa modelu dopasowanego w Ryc. 5.3."),
          lc_slider("ch5_threshold", "Próg decyzji", 0.1, 0.9, 0.5, 0.05)
        ),
        column(8,
          zoom_plot_ui("ch5_threshold_plot", height = "280px"),
          uiOutput("ch5_threshold_info")
        )
      )
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
      stroma sigmoida, więc współczynnik i jego błąd standardowy rosną bez
      ograniczeń, a program zwraca ogromne liczby albo ostrzeżenie. Przy
      rzadkich zdarzeniach i separacji pomaga prostsza specyfikacja, więcej
      danych albo estymacja z karą, która odsuwa współczynniki od wartości
      skrajnych. Najczęściej stosowaną odmianą jest regresja Firtha."),

    tags$table(class = "lc-table lc-table-bordered lc-table-striped",
      style = "font-size: 14px;",
      tags$thead(
        tags$tr(tags$th("Warunek"), tags$th("Co oznacza w praktyce"))
      ),
      tags$tbody(
        tags$tr(
          tags$td(tags$strong("Y jest binarne")),
          tags$td("modelujemy zdarzenie 0/1: zdał/nie zdał, kupił/nie kupił")
        ),
        tags$tr(
          tags$td(tags$strong("Niezależne obserwacje")),
          tags$td("ten sam student, klient lub zakład nie powinien pojawiać się wiele razy bez modelu z powtórzeniami")
        ),
        tags$tr(
          tags$td(tags$strong("Liniowość logitu")),
          tags$td("dla predyktorów ilościowych zależność ma być mniej więcej liniowa na skali logarytmu szans")
        ),
        tags$tr(
          tags$td(tags$strong("Umiarkowana współliniowość")),
          tags$td("tak jak w regresji wielorakiej: predyktory nie powinny powtarzać tej samej informacji")
        ),
        tags$tr(
          tags$td(tags$strong("Dość zdarzeń")),
          tags$td("liczy się mniej liczna klasa; orientacyjnie około 10 zdarzeń na predyktor, bez sztywnej granicy")
        ),
        tags$tr(
          tags$td(tags$strong("Brak separacji")),
          tags$td("gdy predyktor idealnie oddziela 0 od 1, współczynniki uciekają do nieskończoności; pomaga estymacja z karą (regresja Firtha)")
        )
      )
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
      nauczył opisywać dane, a wykład 02 opisywać losowość rozkładami
      prawdopodobieństwa. Wykład 03 pokazał, jak z próby oszacować parametr
      razem z niepewnością, wykład 04, jak testować hipotezy, a wykład 05,
      kiedy wynikom testów można ufać. Regresja łączy te wątki: jej
      współczynniki to estymatory z przedziałami ufności i testami, a założenia
      sprawdza się na wykresach. Następne wykłady wykorzystują te narzędzia
      do oceny jakości danych i całych analiz. Ściąga w rozdziale 06 zbiera
      wzory i zasady regresji, a ćwiczenia w rozdziale 07 pozwalają sprawdzić,
      czy potrafisz samodzielnie zinterpretować model."),

    lc_chapter_next(
      num       = "06",
      title     = "Ściąga",
      lead      = "podsumowanie wzorów i zasad",
      target_id = "ch-sciaga"
    )
  )
)


# ============================================================================
# SERVER
# ============================================================================

ch5_server <- function(input, output, session) {

  ch5_fmt <- function(x, digits = 3) {
    ifelse(is.na(x), "", formatC(x, digits = digits, format = "f"))
  }

  ch5_p <- function(x) {
    ifelse(is.na(x), "", ifelse(x < 0.001, "< 0.001", ch5_fmt(x, 3)))
  }

  ch5_cas_data <- reactive({
    df <- .cas_data
    y_cut <- input$ch5_cas_y_cut
    if (is.null(y_cut)) y_cut <- median(df$read, na.rm = TRUE)
    df$zdal_read <- as.integer(df$read >= y_cut)
    df
  })

  ch5_cas_model <- reactive({
    glm(zdal_read ~ income + lunch + english,
        data = ch5_cas_data(), family = binomial)
  })

  output$ch5_cas_threshold_note <- renderUI({
    df <- ch5_cas_data()
    lc_caption(
      "Y = 1 („zdał”) oznacza wynik czytania od ",
      ch5_fmt(input$ch5_cas_y_cut, 0),
      " pkt: ",
      sum(df$zdal_read),
      " z ",
      nrow(df),
      " okręgów.",
      tone = "ok"
    )
  })

  output$ch5_cas_continuous_plot <- renderPlot({
    df <- ch5_cas_data()

    ggplot(df, aes(income, read, color = factor(zdal_read))) +
      geom_point(alpha = 0.62, size = 2) +
      geom_hline(yintercept = input$ch5_cas_y_cut,
                 color = upwr_accent, linewidth = 1.05, linetype = "dashed") +
      annotate(
        "label",
        x = min(df$income, na.rm = TRUE),
        y = input$ch5_cas_y_cut,
        hjust = 0,
        vjust = -0.45,
        label = paste0("próg zaliczenia: ", input$ch5_cas_y_cut, " pkt"),
        color = upwr_accent,
        fill = "white",
        linewidth = 0
      ) +
      scale_color_manual(
        values = c("0" = unname(upwr_cat["grafit"]), "1" = unname(upwr_cat["szalwia"])),
        labels = c("0" = "nie zdał", "1" = "zdał"),
        name = "Klasa"
      ) +
      labs(
        x = "Dochód okręgu (tys. USD)",
        y = "Wynik z czytania"
      ) +
      theme_upwr()
  })

  output$ch5_cas_logit_plot <- renderPlot({
    df <- ch5_cas_data()
    mod <- ch5_cas_model()
    df$prob <- fitted(mod)

    ggplot(df, aes(income, prob, color = factor(zdal_read))) +
      geom_point(alpha = 0.7) +
      scale_color_manual(
        values = c("0" = unname(upwr_cat["grafit"]), "1" = unname(upwr_cat["szalwia"])),
        labels = c("0" = "nie zdał", "1" = "zdał"),
        name = "Klasa"
      ) +
      labs(x = "Dochód okręgu (tys. USD)", y = "Prawdopodobieństwo zdania") +
      theme_upwr()
  })

  output$ch5_cas_model_table <- renderUI({
    tb <- broom::tidy(ch5_cas_model(), exponentiate = TRUE, conf.int = TRUE)
    labels <- c(
      "(Intercept)" = "Stała (szanse wyjściowe, nie OR)",
      "income" = "Dochód okręgu (tys. USD)",
      "lunch" = "Uczniowie z dotacją do obiadu (%)",
      "english" = "Angielski jako drugi język (%)"
    )
    df <- data.frame(
      term = ifelse(!is.na(labels[tb$term]), unname(labels[tb$term]), tb$term),
      estimate = tb$estimate,
      se = tb$std.error,
      p = lc_pval(tb$p.value),
      sig = ifelse(tb$p.value < 0.05, "tak", "nie")
    )

    lc_table_split(df,
      cols = list(
        lc_col("term", "Zmienna", "row"),
        lc_col("estimate", "Iloraz szans (OR)", digits = 3),
        lc_col("se", "Błąd stand.", digits = 3),
        lc_col("p", "p"),
        lc_col("sig", "p < 0.05?", "text")
      ),
      groups = list(c("estimate", "se"), c("p", "sig")),
      label = "Model logistyczny: ilorazy szans"
    )
  })

  output$ch5_cas_model_metrics <- renderUI({
    model <- ch5_cas_model()
    p_hat <- fitted(model)
    y <- model$y
    rmse <- sqrt(mean((y - p_hat)^2))

    lc_stat_grid(
      lc_stat_box("AIC", ch5_fmt(AIC(model), 1), color = unname(upwr_cat["wrzos"])),
      lc_stat_box("BIC", ch5_fmt(BIC(model), 1), color = upwr_secondary),
      lc_stat_box("RMSE prawdop.", ch5_fmt(rmse, 3),
                  caption = "dla przewidywanych prawdopodobieństw",
                  color = unname(upwr_cat["bursztyn"])),
      columns = 3
    )
  })

  # --- Widget: Liniowy vs logistyczny (przeniesiony z ch4) ---
  ch5_lin_log_data <- reactive({
    x <- seq(0, 40, length.out = 180)
    p <- 1 / (1 + exp(-(-7 + 0.35 * x)))
    u <- ((seq_along(x) * 37) %% 100) / 100
    y <- as.integer(u < p)
    y_jitter <- ifelse(y == 1, 1, 0) + sin(seq_along(x) * 1.7) * 0.025

    data.frame(
      godziny_nauki = x,
      zdal_num = y,
      zdal_plot = y_jitter,
      zdal = factor(y, levels = c(0, 1), labels = c("Nie", "Tak"))
    )
  })

  zoom_plot_server("ch5_lin_log_plot", reactive({
    df <- ch5_lin_log_data()

    ggplot(df, aes(x = godziny_nauki, y = zdal_num)) +
      geom_point(aes(y = zdal_plot), alpha = 0.34, color = upwr_secondary, size = 1.6) +
      geom_smooth(method = "lm", se = FALSE, color = unname(upwr_cat["niebo"]),
                  linewidth = 1, linetype = "dashed") +
      geom_smooth(method = "glm", method.args = list(family = "binomial"),
                  se = FALSE, color = unname(upwr_cat["wrzos"]), linewidth = 1.2) +
      geom_hline(yintercept = c(0, 1), linetype = "dotted", color = upwr_rule) +
      annotate("text", x = 13, y = 0.28, label = "Logistyczny", color = unname(upwr_cat["wrzos"]),
               fontface = "bold") +
      annotate("text", x = 34, y = 1.12, label = "Liniowy", color = unname(upwr_cat["niebo"]),
               fontface = "bold") +
      labs(
           x = "Godziny nauki", y = "P(zdanie)") +
      coord_cartesian(ylim = c(-0.2, 1.2)) +
      theme_upwr()
  }))

  output$ch5_lin_log_stats <- renderUI({
    df <- ch5_lin_log_data()

    lin <- lm(zdal_num ~ godziny_nauki, data = df)
    log <- glm(zdal_num ~ godziny_nauki, data = df, family = binomial)

    lin_pred <- ifelse(fitted(lin) >= 0.5, 1, 0)
    log_pred <- ifelse(fitted(log) >= 0.5, 1, 0)
    acc_lin <- mean(lin_pred == df$zdal_num) * 100
    acc_log <- mean(log_pred == df$zdal_num) * 100

    outside <- mean(fitted(lin) < 0 | fitted(lin) > 1) * 100

    lc_stat_grid(columns = 3,
      lc_stat_box("Liniowy", round(acc_lin, 1), "%", color = unname(upwr_cat["niebo"])),
      lc_stat_box("Logistyczny", round(acc_log, 1), "%", color = unname(upwr_cat["wrzos"])),
      lc_stat_box("Liniowy poza [0, 1]", round(outside, 1), "%", color = unname(upwr_cat["terakota"]))
    )
  })

  # --- Widget 1: Sigmoida ---
  observeEvent(input$ch5_preset_steep, {
    updateSliderInput(session, "ch5_b0", value = -5)
    updateSliderInput(session, "ch5_b1", value = 0.5)
  })
  observeEvent(input$ch5_preset_flat, {
    updateSliderInput(session, "ch5_b0", value = -1)
    updateSliderInput(session, "ch5_b1", value = 0.05)
  })
  observeEvent(input$ch5_preset_neg, {
    updateSliderInput(session, "ch5_b0", value = 5)
    updateSliderInput(session, "ch5_b1", value = -0.3)
  })

  zoom_plot_server("ch5_sigmoid_plot", reactive({
    b0 <- input$ch5_b0
    b1 <- input$ch5_b1
    x <- seq(-5, 45, length.out = 500)
    p <- 1 / (1 + exp(-(b0 + b1 * x)))

    ggplot(data.frame(x = x, p = p), aes(x = x, y = p)) +
      geom_line(color = unname(upwr_cat["wrzos"]), linewidth = 1.5) +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = upwr_secondary, alpha = 0.5) +
      labs(
           x = "X", y = "P(Y = 1)") +
      ylim(0, 1) +
      theme_upwr()
  }))

  # --- Widget 2: Model logistyczny ---
  ch5_data <- reactiveVal(NULL)
  ch5_model <- reactiveVal(NULL)

  observeEvent(input$ch5_fit, {
    df <- generate_logistic_data(input$ch5_n)
    ch5_data(df)
    model <- glm(zdal_num ~ godziny_nauki + srednia_ocen,
                 data = df, family = binomial)
    ch5_model(model)
  })

  zoom_plot_server("ch5_logit_plot", reactive({
    df <- ch5_data()
    model <- ch5_model()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij „Dopasuj model”",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      pred_var <- input$ch5_predictor
      pred_label <- if (pred_var == "godziny_nauki") "Godziny nauki" else "Średnia ocen"

      # Predykcja dla wykresu (trzymając drugi predyktor na średniej)
      other_var <- setdiff(c("godziny_nauki", "srednia_ocen"), pred_var)
      other_mean <- mean(df[[other_var]])

      x_seq <- seq(min(df[[pred_var]]), max(df[[pred_var]]), length.out = 200)
      newdata <- data.frame(x_seq, other_mean)
      names(newdata) <- c(pred_var, other_var)
      newdata$pred_prob <- predict(model, newdata, type = "response")

      ggplot() +
        geom_jitter(data = df, aes(x = .data[[pred_var]], y = .data[["zdal_num"]]),
                    height = 0.03, alpha = 0.3, color = upwr_secondary) +
        geom_line(data = newdata, aes(x = .data[[pred_var]], y = .data[["pred_prob"]]),
                  color = unname(upwr_cat["wrzos"]), linewidth = 1.5) +
        geom_hline(yintercept = 0.5, linetype = "dashed", color = unname(upwr_cat["bursztyn"])) +
        labs(
             x = pred_label, y = "P(zdanie egzaminu)") +
        ylim(-0.05, 1.05) +
        theme_upwr()
    }
  }))

  output$ch5_model_summary <- renderUI({
    model <- ch5_model()
    if (is.null(model)) return(NULL)

    g <- broom::glance(model)
    coefs <- broom::tidy(model)

    # Confusion matrix
    df <- ch5_data()
    pred_class <- ifelse(predict(model, type = "response") >= 0.5, 1, 0)
    accuracy <- mean(pred_class == df$zdal_num) * 100

    tagList(
      lc_stat_box("AIC", round(g$AIC, 1), color = unname(upwr_cat["wrzos"])),
      lc_stat_box("BIC", round(g$BIC, 1), color = upwr_secondary),
      lc_stat_box("Dokładność", round(accuracy, 1), "%", color = unname(upwr_cat["szalwia"]))
    )
  })

  output$ch5_prediction <- renderUI({
    model <- ch5_model()
    if (is.null(model)) return(NULL)

    newdata <- data.frame(
      godziny_nauki = input$ch5_pred_hours,
      srednia_ocen = input$ch5_pred_gpa
    )
    prob <- predict(model, newdata, type = "response")

    color <- if (prob >= 0.5) unname(upwr_cat["szalwia"]) else unname(upwr_cat["terakota"])
    decision <- if (prob >= 0.5) "Prawdopodobnie zda" else "Raczej nie zda"

    lc_stat_box("P(zdanie)", round(prob, 3), caption = decision, color = color)
  })

  # --- Widget 3: Odds ratios ---
  output$ch5_odds_ratios <- renderUI({
    model <- ch5_model()
    if (is.null(model)) {
      return(lc_caption(
               "Najpierw dopasuj model."
             ))
    }

    coefs <- broom::tidy(model, conf.int = TRUE)
    coefs$or <- exp(coefs$estimate)
    coefs$or_low <- exp(coefs$conf.low)
    coefs$or_high <- exp(coefs$conf.high)

    labels_pl <- c(
      "(Intercept)" = "Wyraz wolny",
      "godziny_nauki" = "Godziny nauki (+1h)",
      "srednia_ocen" = "Średnia ocen (+1 pkt)"
    )

    coefs$term_pl <- ifelse(coefs$term %in% names(labels_pl),
                             labels_pl[coefs$term], coefs$term)

    rows <- lapply(2:nrow(coefs), function(i) {  # pomijamy intercept
      tags$tr(
        tags$td(coefs$term_pl[i]),
        tags$td(round(coefs$estimate[i], 3)),
        tags$td(tags$strong(round(coefs$or[i], 3))),
        tags$td(paste0("[", round(coefs$or_low[i], 3), " ; ",
                        round(coefs$or_high[i], 3), "]")),
        tags$td(format_p_value(coefs$p.value[i]))
      )
    })

    tags$table(class = "lc-table lc-table-bordered",
      style = "font-size: 14px;",
      tags$thead(
        tags$tr(tags$th("Zmienna"), tags$th("β"), tags$th("OR"),
                tags$th("95% CI (OR)"), tags$th("p"))
      ),
      tags$tbody(rows)
    )
  })

  # --- Widget: próg klasyfikacji ---
  zoom_plot_server("ch5_threshold_plot", reactive({
    model <- ch5_model()
    df <- ch5_data()
    if (is.null(model) || is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Najpierw dopasuj model w Ryc. 5.3",
                 size = 5.5, color = upwr_reference) +
        theme_void()
    } else {
      probs <- predict(model, type = "response")
      pred <- ifelse(probs >= input$ch5_threshold, 1, 0)
      cm <- as.data.frame(table(
        Rzeczywiste = factor(df$zdal_num, levels = c(0, 1), labels = c("Nie", "Tak")),
        Predykcja = factor(pred, levels = c(0, 1), labels = c("Nie", "Tak"))
      ))
      ggplot(cm, aes(x = Predykcja, y = Rzeczywiste, fill = Freq)) +
        geom_tile(color = "white", linewidth = 1) +
        geom_text(aes(label = Freq), size = 7, fontface = "bold", color = "white") +
        scale_fill_gradient(low = unname(upwr_cat["niebo"]), high = upwr_secondary) +
        labs(x = "Predykcja modelu", y = "Rzeczywistość") +
        theme_upwr() +
        theme(legend.position = "none")
    }
  }))

  output$ch5_threshold_info <- renderUI({
    model <- ch5_model()
    df <- ch5_data()
    if (is.null(model) || is.null(df)) return(NULL)
    probs <- predict(model, type = "response")
    pred <- ifelse(probs >= input$ch5_threshold, 1, 0)
    tp <- sum(pred == 1 & df$zdal_num == 1)
    tn <- sum(pred == 0 & df$zdal_num == 0)
    fp <- sum(pred == 1 & df$zdal_num == 0)
    fn <- sum(pred == 0 & df$zdal_num == 1)
    accuracy <- (tp + tn) / length(pred)
    sensitivity <- ifelse(tp + fn == 0, NA, tp / (tp + fn))
    specificity <- ifelse(tn + fp == 0, NA, tn / (tn + fp))
    tagList(
      lc_stat_box("Dokładność", paste0(round(accuracy * 100, 1), "%"), color = unname(upwr_cat["szalwia"])),
      lc_stat_box("Czułość", paste0(round(sensitivity * 100, 1), "%"), caption = "wykrywa Tak", color = unname(upwr_cat["niebo"])),
      lc_stat_box("Swoistość", paste0(round(specificity * 100, 1), "%"), caption = "wykrywa Nie", color = unname(upwr_cat["bursztyn"]))
    )
  })
}
