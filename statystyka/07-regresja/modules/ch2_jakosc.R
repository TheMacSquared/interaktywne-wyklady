# ============================================================================
# CHAPTER 2: Jakość modelu
# ============================================================================

# Scenariusze widgetu reszt vs fitted — dobrane tak, by pokazać różne wzorce.
.ch2_resid_specs <- list(
  read_lunch = list(
    label = "Czytanie ~ dotacje do obiadu (model dobrze działa)",
    x = "lunch", y = "read",
    verdict = "ok",
    title = "Reszty bez wzorca",
    comment = "Reszty leżą równą chmurą wokół zera, a Q-Q biegnie wzdłuż
              prostej. Prosta dobrze opisuje tę zależność."
  ),
  read_income = list(
    label = "Czytanie ~ dochód (krzywizna)",
    x = "income", y = "read",
    verdict = "warning",
    title = "Łuk w resztach",
    comment = "W środku zakresu reszty są głównie dodatnie, na obu krańcach
              ujemne. Zależność czytania od dochodu jest krzywa."
  ),
  read_str = list(
    label = "Czytanie ~ uczniowie na nauczyciela (słaby model, bez wzorca)",
    x = "student_teacher_ratio", y = "read",
    verdict = "info",
    title = "Słaby model bez wzorca",
    comment = "Reszty są prawie tak duże jak rozrzut samego wyniku, ale nie
              układają się w żaden kształt. Model niewiele wyjaśnia,
              choć niczego nie zniekształca."
  ),
  math_english = list(
    label = "Matematyka ~ angielski jako 2. język (wachlarz)",
    x = "english", y = "math",
    verdict = "info",
    title = "Bez łuku, z lekkim wachlarzem",
    comment = "Linia trendu reszt jest prawie płaska, ale rozrzut reszt rośnie
              w prawej części wykresu, gdzie leżą okręgi z małym udziałem
              uczniów uczących się angielskiego."
  )
)

.ch2_resid_choices <- setNames(
  names(.ch2_resid_specs),
  vapply(.ch2_resid_specs, `[[`, character(1), "label")
)

# Scenariusze widgetu RMSE — modele różnej jakości, ten sam zbiór.
.ch2_rmse_specs <- list(
  read_lunch = list(
    label = "Czytanie ~ dotacje do obiadu (najlepsze dopasowanie)",
    x = "lunch", y = "read"
  ),
  read_income = list(
    label = "Czytanie ~ dochód okręgu",
    x = "income", y = "read"
  ),
  math_english = list(
    label = "Matematyka ~ angielski jako 2. język",
    x = "english", y = "math"
  ),
  read_str = list(
    label = "Czytanie ~ uczniowie na nauczyciela",
    x = "student_teacher_ratio", y = "read"
  )
)

.ch2_rmse_choices <- setNames(
  names(.ch2_rmse_specs),
  vapply(.ch2_rmse_specs, `[[`, character(1), "label")
)

ch2_ui <- list(
  id    = "ch-jakosc",
  num   = "02",
  title = "Jakość modelu",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 02 · Regresja",
      num    = "02",
      title  = "Jakość modelu.",
      lead   = "Prostą można dopasować do każdej chmury punktów, także takiej,
                która wcale nie układa się wzdłuż prostej. O jakości modelu
                mówi dopiero to, co po nim zostaje: reszty, odsetek wyjaśnionej
                zmienności i wielkość typowej pomyłki."
    ),

    lc_p("W rozdziale 01 dopasowaliśmy prostą ", gloss("metoda najmniejszych kwadratów", "metodą najmniejszych kwadratów"), ",
      odczytaliśmy z tabeli wyników współczynniki i ich p-wartości i użyliśmy
      równania do przewidywania. Wszystkie te wyniki mają sens pod warunkiem,
      że sam model jest sensowny: że zależność rzeczywiście jest liniowa,
      a reszty zachowują się tak, jak zakłada metoda. Ten rozdział sprawdza
      ten warunek."),

    lc_p("Ocena modelu rozkłada się na trzy pytania, a każdemu odpowiada inne
      narzędzie. Czy prosta nie myli się systematycznie? Odpowiada na to wzorzec
      reszt. Jaką część zmienności Y model wyjaśnia? Odpowiada \\(R^2\\). Jak
      duże są typowe pomyłki w jednostkach Y? Odpowiada RMSE. Wszystkie trzy
      dotyczą jednego modelu. Porównywaniem kilku modeli zajmie się
      rozdział 04."),

    lc_h2("ch2-reszty", "Wzorzec reszt"),

    lc_p("W rozdziale 01 ", gloss("reszta", "reszty"), " były pomocniczym pojęciem:
      różnicami \\(e_i = y_i - \\hat{y}_i\\), których sumę kwadratów metoda
      najmniejszych kwadratów czyni jak najmniejszą. Teraz stają się głównym
      narzędziem oceny modelu. Reszta to ta część wyniku, której model nie
      przewidział, więc w resztach widać to, czego model nie uchwycił."),

    lc_p("Jeśli model dobrze opisuje dane, reszty są losowym szumem: tworzą
      chmurę wokół zera bez trendu i bez zmian rozrzutu. Każde odstępstwo od
      tego obrazu coś znaczy:"),

    tags$ul(
      tags$li(strong("Łuk:"), " zależność jest krzywa, a dopasowaliśmy prostą.
        W jednym zakresie X prosta systematycznie zaniża przewidywania,
        w innym je zawyża."),
      tags$li(strong("Wachlarz:"), " rozrzut reszt rośnie albo maleje wraz
        z przewidywaną wartością. Narusza to założenie stałej wariancji reszt,
        czyli ", gloss("homoskedastyczność", "homoskedastyczności"), "."),
      tags$li(strong("Pojedynczy punkt daleko od chmury:"), " ",
        gloss("wartość odstająca", "wartość odstająca"), ", która może
        przyciągać prostą do siebie.")
    ),

    lc_p("Podstawowym narzędziem jest wykres reszt względem ",
      gloss("wartość przewidywana", "wartości przewidywanych"), ": na osi
      poziomej leży \\(\\hat{y}_i\\), na osi pionowej \\(e_i\\). W regresji
      prostej \\(\\hat{y}\\) jest liniową funkcją X, więc ten wykres to
      w istocie wykres rozrzutu przechylony tak, by prosta regresji stała się
      poziomą linią zera. Wzorce, które w chmurze punktów łatwo przeoczyć,
      stają się wtedy wyraźne. Drugim narzędziem jest ",
      gloss("wykres kwantyl-kwantyl", "wykres Q-Q"), " reszt, ten sam co
      w wykładzie 06, tylko zastosowany do reszt zamiast do surowych danych.
      Punkty wzdłuż prostej oznaczają reszty w przybliżeniu normalne."),

    lc_p("Panel pokazuje cztery proste modele dopasowane do danych z 420 okręgów
      szkolnych w Kalifornii (zbiór CASchools). Dla każdego widać dane
      z prostą, reszty względem wartości przewidywanych z wygładzoną linią
      trendu reszt oraz wykres Q-Q reszt."),

    figure_panel(
      label = "Ryc. 2.1", title = "Reszty na danych CASchools",
      full_width = TRUE,
      # Opisy wykresów liczy R przy budowaniu (modules/figures.R).
      ch2_resid_fig(),
      lc_caption("Te same 420 okręgów szkolnych, cztery różne pary zmiennych.")
    ),

    lc_p("Model czytania zależnego od dochodu okręgu pokazuje łuk. Średnia
      reszta w okręgach o dochodzie do 10 tys. USD wynosi -7.8 punktu,
      w okręgach o dochodzie 15–20 tys. USD +4.7 punktu, a powyżej 30 tys. USD
      znów -7.3 punktu. Prosta zawyża więc wyniki na obu krańcach i zaniża je
      w środku. Wyniki rosną z dochodem coraz wolniej, a prosta tego nie
      potrafi oddać. Wykres Q-Q tego samego modelu wygląda dobrze, co
      przypomina, że normalność reszt nie gwarantuje poprawnego kształtu
      zależności."),

    lc_p("Model z odsetkiem uczniów z dotacją do obiadu nie zostawia
      wzorca: reszty leżą równą chmurą, a ich ", gloss("odchylenie standardowe"), "
      w dolnej, środkowej i górnej trzeciej części wartości przewidywanych
      wynosi 9.5, 9.6 i 9.8 punktu. Model z liczbą uczniów na nauczyciela też
      nie ma wzorca, ale jego reszty są prawie tak duże jak rozrzut samych
      wyników czytania. Brak wzorca nie znaczy więc, że model dużo wyjaśnia,
      tylko że nie myli się systematycznie. Ile wyjaśnia, mierzą \\(R^2\\)
      i RMSE w kolejnych sekcjach."),

    lc_p("Ostatni model, matematyki zależnej od odsetka uczniów uczących się
      angielskiego jako drugiego języka, ma prawie płaską linię trendu reszt,
      ale ich rozrzut rośnie w prawo: odchylenie standardowe reszt w trzech
      kolejnych częściach zakresu wartości przewidywanych wynosi 12.9, 16.1
      i 17.1 punktu. To łagodny wachlarz. Pionowy pas punktów przy prawej
      krawędzi to 49 okręgów, w których nikt nie uczy się angielskiego jako
      drugiego języka; wszystkie mają tę samą wartość przewidywaną."),

    lc_h2("ch2-zalozenia", "Założenia, które widać w resztach"),

    lc_p("Wykład 06 zapowiadał, że w regresji założenia dotyczą reszt, a nie
      samych zmiennych. To bezpośrednie przedłużenie tego, co znamy z ", gloss("test t", "testu t"), "
      i ", gloss("ANOVA"), ". Tam resztami były odchylenia obserwacji od średniej
      ich grupy, a założenia mówiły o ich rozkładzie i rozrzucie w każdej
      grupie. W regresji resztami są odchylenia od prostej, a „grupy”
      zastępuje ciągły zakres wartości przewidywanych. ", gloss("zmienna zależna", "Zmienna zależna"), " nie
      musi mieć ", gloss("rozkład normalny", "rozkładu normalnego"), "; w przybliżeniu normalne powinny być
      reszty."),

    lc_p("Narzędzia są te same co w wykładzie 06. Normalność oceniamy wykresem
      Q-Q reszt. Jednorodność wariancji oceniamy, porównując rozrzut reszt
      w różnych częściach zakresu, tak jak w wykładzie 06 porównywaliśmy
      odchylenia standardowe w grupach. Liniowość i stałą wariancję naraz
      pokazuje wykres reszt względem wartości przewidywanych. Tabela zbiera
      typowe założenia z sygnałami problemu i możliwymi reakcjami."),

    lc_table(
      data.frame(
        c1 = c(
          "Liniowość",
          "Stała wariancja",
          "Normalność reszt",
          "Brak obserwacji wpływowych"
        ),
        c2 = I(list(
          "reszty względem wartości przewidywanych",
          "reszty względem wartości przewidywanych, wykres Scale-Location",
          "wykres Q-Q reszt",
          tagList("reszty standaryzowane, dźwignia, ", gloss("odległość Cooka"))
        )),
        c3 = c(
          "łuk, fala, systematyczny wzorzec",
          "wachlarz, rosnący lub malejący rozrzut",
          "grube ogony, łuk, punkty daleko od prostej",
          "pojedynczy punkt zmienia nachylenie"
        ),
        c4 = c(
          "transformacja, składnik kwadratowy, model nieliniowy",
          "transformacja Y, odporne błędy standardowe, ważona MNK (WLS)",
          "sprawdź wartości odstające, przedział ufności bootstrap, inny model dla Y",
          "zweryfikuj pomiar, pokaż analizę z punktem i bez niego"
        )
      ),
      cols = list(
        lc_col("c1", "Założenie", "row"),
        lc_col("c2", "Co sprawdzić", "text"),
        lc_col("c3", "Sygnał problemu", "text"),
        lc_col("c4", "Co wtedy", "text")
      ),
      narrow = "cards",
      prose = TRUE
    ),

    lc_p("Założenia nie są równie ważne. Liniowość jest najważniejsza, bo przy
      krzywej zależności błędne są same współczynniki, a nie tylko ich
      p-wartości. Stała wariancja wpływa na błędy standardowe, a przez nie na
      p-wartości i ", gloss("przedział ufności", "przedziały ufności"), ". Normalność reszt ma znaczenie głównie
      w małych próbach, bo w dużych rozkład współczynników jest w przybliżeniu
      normalny niezależnie od kształtu rozkładu reszt. Osobnym założeniem jest
      niezależność reszt; wykresy z tego rozdziału jej nie pokazują, ocenia
      się ją na podstawie sposobu zbierania danych, np. pomiarów powtarzanych
      w czasie. ", gloss("obserwacja wpływowa", "Obserwacje wpływowe"),
      " nie są założeniem w ścisłym sensie, ale pojedynczy punkt potrafi
      zmienić nachylenie całej prostej, więc warto je wykryć."),

    lc_p("Nie każdy nietypowy punkt jest wpływowy. Panel pokazuje 14 studentów
      z wyraźnym związkiem: im więcej godzin nauki w tygodniu, tym wyższy wynik
      egzaminu. Do tych danych dochodzi jeden dodatkowy student, w trzech
      różnych miejscach. Linia przerywana to prosta bez niego, ciągła to
      prosta po jego dodaniu."),

    figure_panel(
      label = "Ryc. 2.2", title = "Kiedy jeden punkt przestawia prostą",
      full_width = TRUE,
      ch2_infl_fig()
    ),

    lc_p("Student, który uczył się przeciętnie długo, a dostał 92 punkty, leży
      daleko od prostej, ale prawie jej nie przechyla: nachylenie zmienia się
      z 2.33 na 2.31 punktu na godzinę. Prosta tylko lekko się podnosi, a
      \\(R^2\\) spada z 0.85 do 0.32, bo model gorzej trafia w tego jednego
      studenta. Student z 24 godzinami nauki leży daleko od pozostałych na osi
      X, ale jego wynik, 96 punktów, pasuje do trendu, więc prosta prawie się
      nie zmienia (2.48). Dopiero ten sam student z wynikiem 40 punktów, czyli
      daleko w X i wbrew trendowi, spłaszcza prostą do 0.37 punktu na
      godzinę."),

    lc_p("Obserwacja jest wpływowa, gdy łączy dwie cechy: leży daleko od
      pozostałych na osi X, czyli ma dużą ", gloss("dźwignia", "dźwignię"), ",
      i nie pasuje do trendu, który wyznaczają pozostałe punkty. Odległość
      Cooka z tabeli powyżej mierzy właśnie to połączenie. Nie usuwa się takiego
      punktu tylko dlatego, że przeszkadza. Najpierw sprawdza się, czy nie jest
      błędem pomiaru, a jeśli nie jest, raport podaje wynik z nim i bez
      niego."),

    lc_p("Testy formalne, np. ", gloss("test Shapiro-Wilka"), " dla reszt albo
      test Breuscha-Pagana dla ",
      gloss("heteroskedastyczność", "heteroskedastyczności"), ", są dodatkiem
      do wykresu, tak jak w wykładzie 06. W dużych próbach wykrywają odchylenia
      bez praktycznego znaczenia, w małych często ich nie wykrywają, a brak
      podstaw do odrzucenia H₀ nie dowodzi, że założenie jest spełnione.
      Przykład z naszych danych: dla modelu z liczbą uczniów na nauczyciela
      test Shapiro-Wilka daje p = 0.02, choć wykres Q-Q odchyla się od prostej
      tylko łagodnie na obu końcach, a przy 420 okręgach takie odchylenie
      niczemu nie zagraża. Dla modelu matematyki test Breuscha-Pagana daje
      p < 0.001, co potwierdza wachlarz widoczny na wykresie reszt. W raporcie
      najpierw opisujemy wzorzec reszt, a test podajemy co najwyżej obok."),

    lc_h2("ch2-r2", "R²: ile zmienności wyjaśnia model"),

    lc_p("Wzorzec reszt mówi, czy prosta nie myli się systematycznie, ale nie
      mówi, ile model wyjaśnia. Model z liczbą uczniów na nauczyciela był
      wolny od wzorca, a mimo to jego reszty były prawie tak duże jak rozrzut
      samych wyników. Potrzebujemy miary, która porówna wielkość reszt
      z tym, jak bardzo Y zmienia się w ogóle."),

    lc_p("Tą miarą jest ", gloss("współczynnik determinacji"), " \\(R^2\\).
      Punktem odniesienia jest najprostsza prognoza, jaką można zrobić bez
      żadnego ", gloss("predyktor", "predyktora"), ": średnia \\(\\bar{y}\\) dla wszystkich obserwacji.
      Jej błędy sumują się do całkowitej sumy kwadratów \\(SS_{tot}\\).
      Model z predyktorem zostawia mniejsze błędy, czyli resztową sumę
      kwadratów \\(SS_{res}\\), tę samą, którą minimalizuje metoda najmniejszych
      kwadratów. \\(R^2\\) mówi, jaką część \\(SS_{tot}\\) model usunął."),

    # R² ze znacznikami symboli
    lc_formula_annotated(
      "R^2 = 1 - \\frac{⟦1|SS_{res}⟧}{⟦2|SS_{tot}⟧} = 1 - \\frac{\\sum_{i=1}^{n}(y_i - ⟦3|\\hat{y}_i⟧)^2}{\\sum_{i=1}^{n}(y_i - ⟦4|\\bar{y}⟧)^2}",
      notes  = list("suma kwadratów reszt", "całkowita suma kwadratów",
                    "wartość przewidywana", "średnia Y"),
      colors = c("bursztyn", "szalwia", "wrzos", "accent"),
      above  = c(1, 3)
    ),

    lc_p("\\(R^2\\) przyjmuje wartości od 0 do 1. Zero oznacza, że model
      przewiduje nie lepiej niż średnia, jedynka, że wszystkie punkty leżą
      dokładnie na prostej. W regresji prostej \\(R^2\\) jest równe kwadratowi
      współczynnika ", gloss("korelacja Pearsona", "korelacji Pearsona"), " z wykładu 02, który już tam nazwaliśmy
      współczynnikiem determinacji. Dla czytania i odsetka uczniów
      z dotacją do obiadu \\(r = -0.88\\), więc \\(R^2 = 0.77\\): model
      wyjaśnia 77% zmienności wyników czytania między okręgami."),

    lc_p("Panel pokazuje trzy zbiory danych z tą samą prawdziwą prostą
      o nachyleniu 2.2. Różnią się tylko wielkością losowego szumu wokół
      niej."),

    figure_panel(
      label = "Ryc. 2.3", title = "To samo X i Y, różna siła wyjaśniania",
      full_width = TRUE,
      ch2_r2_compare_fig()
    ),

    lc_p("Przy dużym szumie \\(R^2 = 0.18\\), przy średnim 0.54, przy małym
      0.95. Nachylenie we wszystkich trzech panelach jest takie samo, zmienia
      się tylko to, jak ciasno punkty trzymają się prostej. To ta sama
      lekcja co w wykładzie 05: siła związku to coś innego niż nachylenie.
      \\(R^2\\) nie mówi, jak bardzo Y zmienia się z X, tylko jak dużo
      zmienności zostaje poza modelem."),

    lc_p("Nie istnieje uniwersalna granica „dobrego” \\(R^2\\). W naukach
      społecznych, w edukacji czy w badaniach bezpieczeństwa pracy zjawiska
      zależą od wielu czynników naraz i \\(R^2\\) rzędu 0.3 bywa cenną
      informacją. Model czytania zależnego od liczby uczniów na nauczyciela
      ma \\(R^2 = 0.06\\), a mimo to związek jest istotny statystycznie
      (p < 0.001): wielkość klas ma znaczenie, tylko tłumaczy niewielką część
      różnic między okręgami. \\(R^2\\) opisuje siłę związku w tych
      konkretnych danych, a nie wartość modelu w ogóle."),

    lc_p("To samo \\(R^2\\) nie oznacza też tych samych danych. Klasyczny
      przykład to kwartet Anscombe'a, znany z wykładu 05: cztery zestawy po
      11 punktów, dla których regresja daje praktycznie tę samą prostą,
      \\(\\hat{y} = 3.0 + 0.50x\\), i to samo \\(R^2 = 0.67\\). Panel
      pokazuje każdy zestaw z prostą i wykresem reszt, tak jak Ryc. 2.1."),

    figure_panel(
      label = "Ryc. 2.4", title = "Ta sama prosta, to samo R², cztery różne historie",
      full_width = TRUE,
      ch2_ansc_fig()
    ),

    lc_p("Tylko w zestawie 1 prosta jest dobrym opisem: reszty tworzą
      bezładną chmurę wokół zera. W zestawie 2 reszty układają się w łuk,
      bo zależność jest krzywa. W zestawie 3 dziesięć punktów leży niemal dokładnie
      na prostej o nachyleniu 0.35, a jeden odstający punkt podnosi je do
      0.50. W zestawie 4 dziesięć punktów ma tę samą wartość X, a nachylenie
      wyznacza w całości jeden punkt o dużej dźwigni, jak w Ryc. 2.2. Bez
      niego prostej nie dałoby się w ogóle dopasować. Tabela współczynników
      i \\(R^2\\) są we wszystkich czterech zestawach identyczne; różnicę
      widać dopiero na wykresach."),

    lc_p("Wysokie \\(R^2\\) nie gwarantuje więc dobrego modelu. Zestaw 2 ma
      \\(R^2 = 0.67\\), a mimo to prosta systematycznie się myli.
      Druga pułapka jest poważniejsza: model może dopasować się do
      przypadkowych szczegółów próby tak mocno, że świetnie wygląda na danych,
      na których go dopasowano (", gloss("zbiór treningowy", "zbiorze
      treningowym"), "), a słabo przewiduje nowe obserwacje. Nazywa się to ",
      gloss("przeuczenie", "przeuczeniem"), "."),

    lc_h3("Jak wygląda przeuczenie"),

    lc_p("Przeuczenie pojawia się zwykle w kilku typowych sytuacjach:"),

    tags$ul(
      tags$li(strong("Dużo predyktorów przy małej próbie:"), " model
        z 20 zmiennymi dla 40 obserwacji może przypadkiem „wyjaśnić” szum,
        a nie zjawisko."),
      tags$li(strong("Zbyt elastyczna krzywa:"), " wielomian wysokiego stopnia
        przechodzi blisko każdego punktu, ale między punktami faluje bez
        sensu."),
      tags$li(strong("Wielokrotne dobieranie modelu do tej samej próby:"),
        " sprawdzamy wiele wariantów i wybieramy ten, który wygląda najlepiej,
        choć wygrał przypadkiem."),
      tags$li(strong("Wyciek informacji:"), " wśród predyktorów jest zmienna,
        której w praktycznej predykcji jeszcze byśmy nie znali, np. wynik
        po egzaminie użyty do przewidywania zdania egzaminu.")
    ),

    lc_p("Żeby przeuczenie zobaczyć, trzeba mieć dane, których model nie
      widział. Panel dopasowuje do 30 punktów treningowych trzy wielomiany:
      stopnia 1, 4 i 12. Punkty powstały z krzywej przypominającej falę
      z losowym szumem o odchyleniu standardowym 0.9. Jasne punkty to 180
      nowych obserwacji z tego samego źródła (", gloss("zbiór testowy"),
      "). Tabela pod wykresem podaje RMSE, czyli typową wielkość błędu, osobno
      na danych treningowych i testowych; miarę tę zdefiniujemy dokładnie
      w następnej sekcji."),

    figure_panel(
      label = "Ryc. 2.5", title = "Przeuczenie: dopasowanie kontra generalizacja",
      full_width = TRUE,
      ch2_overfit_fig()
    ),

    lc_p("Prosta (stopień 1) jest zbyt sztywna: myli się podobnie na obu
      zbiorach, z RMSE 2.99 na treningu i 3.67 na teście. Wielomian stopnia 4
      łapie kształt fali: 1.06 na treningu i 1.42 na teście, blisko wielkości
      samego szumu. Wielomian stopnia 12 ma na treningu najmniejszy błąd ze
      wszystkich, 0.52, mniejszy nawet niż szum, który do danych dodaliśmy.
      To znak, że dopasował się do szumu. Na nowych danych jego błąd rośnie do
      3.92, więcej niż przy zwykłej prostej. Przeuczenie rozpoznajemy właśnie
      po tym rozjechaniu się błędu treningowego i testowego: model dobrze
      pamięta punkty, które widział, ale gorzej przewiduje nowe."),

    figure_panel(
      label = "Miniściąga", title = "Jak ograniczać przeuczenie",
      full_width = TRUE,
      lc_table(
        data.frame(
          c1 = c(
            "Model za złożony",
            "Dopasowanie do szumu",
            "Dodawanie kolejnych X tylko pod R²",
            "Niestabilne współczynniki"
          ),
          c2 = c(
            "R² wysokie, ale interpretacja chaotyczna",
            "Błąd na danych treningowych mały, na nowych duży",
            "R² rośnie po każdym dodatku",
            "Mała zmiana danych mocno zmienia tabelę regresji"
          ),
          c3 = c(
            "Uprościć model; usuwać predyktory bez uzasadnienia teoretycznego",
            "Sprawdzić model na zbiorze testowym albo walidacją krzyżową",
            "Patrzeć na skorygowane R², AIC, BIC i sens merytoryczny",
            "Zebrać więcej danych, ograniczyć liczbę zmiennych, sprawdzić współliniowość"
          )
        ),
        cols = list(
          lc_col("c1", "Problem", "row"),
          lc_col("c2", "Objaw", "text"),
          lc_col("c3", "Co zrobić", "text")
        ),
        narrow = "cards"
      )
    ),

    lc_p("Zwykłe \\(R^2\\) nigdy nie maleje po dodaniu predyktora, nawet
      zupełnie przypadkowego. Dlatego przy porównywaniu modeli używa się ",
      gloss("skorygowany R²", "skorygowanego R²"), ", które karze za
      zbędne predyktory. Wrócimy do niego w rozdziale 04, razem z podziałem
      na zbiór treningowy i testowy."),

    lc_h2("ch2-rmse", "RMSE: typowa wielkość pomyłki"),

    lc_p("\\(R^2\\) jest miarą względną: mówi, jaką część zmienności wyjaśniono,
      ale nie mówi, o ile punktów model się myli. Dla kogoś, kto chce użyć
      modelu do przewidywania, to często ważniejsze pytanie: czy prognoza
      wyniku testu chybia o 5 punktów, czy o 50?"),

    lc_p("Odpowiada na nie ", gloss("RMSE"), " (ang. root mean squared error),
      pierwiastek ze średniego kwadratu reszt:"),

    # RMSE ze znacznikami symboli
    lc_formula_annotated(
      "⟦1|RMSE⟧ = \\sqrt{\\frac{1}{⟦2|n⟧}\\sum_{i=1}^{n}(⟦3|y_i⟧ - ⟦4|\\hat{y}_i⟧)^2}",
      notes  = list("typowa pomyłka", "liczba obserwacji", "obserwacja",
                    "wartość przewidywana"),
      colors = c("accent", "szalwia", "niebo", "wrzos"),
      above  = c(3, 4)
    ),

    lc_p("RMSE ma jednostki Y, więc czyta się je wprost jako typową wielkość
      pomyłki modelu. Sama liczba nic jednak nie mówi bez odniesienia do skali
      Y. Naturalnym odniesieniem jest odchylenie standardowe Y: to RMSE
      prognozy, która dla każdego okręgu podaje po prostu średnią. Dobry model
      powinien mieć RMSE wyraźnie mniejsze. Panel pokazuje pasmo ±RMSE wokół
      prostej i porównuje RMSE z zakresem Y."),

    figure_panel(
      label = "Ryc. 2.6", title = "RMSE i zakres Y na danych CASchools",
      full_width = TRUE,
      ch2_rmse_fig(),
      lc_caption("Te same 420 okręgów szkolnych, cztery modele.")
    ),

    lc_p("Wyniki czytania mają zakres od 604.5 do 704 punktów i odchylenie
      standardowe około 20 punktów. Model z dotacją do obiadu ma
      RMSE 9.6 punktu, mniej niż połowę tego odchylenia: znajomość jednej
      zmiennej o połowę zmniejsza typową pomyłkę. Model z liczbą uczniów na
      nauczyciela ma RMSE 19.5, prawie tyle, ile prognoza samą średnią. Obie
      miary są ze sobą powiązane: RMSE jest równe odchyleniu standardowemu Y
      pomnożonemu przez \\(\\sqrt{1 - R^2}\\), jeśli oba liczymy z dzielnikiem
      n. \\(R^2\\) i RMSE mówią więc o tym samym dopasowaniu, tylko
      w innych jednostkach: względnych i w punktach testu."),

    lc_p("Gdy reszty mają rozkład zbliżony do normalnego, w paśmie ±RMSE wokół
      prostej mieści się około dwóch trzecich obserwacji. W modelu z dotacjami do obiadu
      jest to 297 z 420 okręgów, czyli 71%. Czy RMSE jest wystarczająco małe,
      zależy od zastosowania: inna dokładność wystarcza do opisu ogólnej
      zależności, a inna do prognozy dla konkretnej szkoły."),

    lc_h2("ch2-ekstrapolacja", "Ekstrapolacja: poza zakresem danych"),

    lc_p("Wszystkie dotychczasowe miary oceniają model w zakresie danych, na
      których go dopasowano. Równanie prostej da jednak liczbę dla dowolnego X,
      także leżącego daleko poza tym zakresem. Taka prognoza to ",
      gloss("ekstrapolacja"), ". Model nie ma żadnych informacji o tym, czy
      poza zakresem danych zależność nadal jest liniowa, a reszty, \\(R^2\\)
      i RMSE niczego tam nie sprawdzają."),

    lc_p("Panel wraca do modelu czytania zależnego od dochodu okręgu. Dochody
      w danych mieszczą się w przedziale od 5.3 do 55.3 tys. USD (szary pas).
      Suwak wybiera dochód, dla którego liczymy prognozę."),

    figure_panel(
      label = "Ryc. 2.7", title = "Ekstrapolacja poza zakres danych",
      full_width = TRUE,
      # Prognozę dla suwaka liczy przeglądarka (modules/figures.js, rycina s07_extrap).
      lc_sf_figure("s07_extrap", config = ch2_extrap_config,
        control = lc_sf_slider("ch2_extrap_x", "Dochód okręgu (tys. USD)", 1, 80, 20, 1),
        toolbar = lc_readouts(tags$div(`data-role` = "stats", style = "display: contents")),
        ratio = "2/1", max_height = "320px",
        after = tags$div(`data-role` = "verdict")),
      lc_caption("Szary pas na wykresie to zakres dochodów w danych.")
    ),

    lc_p("Dla dochodu 20 tys. USD model przewiduje 664.1 punktu, w środku
      chmury danych. Dla 80 tys. USD przewiduje 780.6 punktu, o ponad 75
      punktów więcej niż najlepszy wynik czytania w całym zbiorze (704).
      Prosta rośnie bez końca, a z wykresu reszt wiemy już, że wyniki rosną
      z dochodem coraz wolniej. Nawet na samym brzegu danych, przy 55 tys.
      USD, prognoza 732 punktów jest wyższa niż jakikolwiek obserwowany wynik.
      Ekstrapolacja przenosi błąd kształtu modelu tam, gdzie nie ma już danych,
      które by go zdradziły."),

    lc_note("Zasada", rule = TRUE,
      "Prognozuj tylko w zakresie X, na którym model był dopasowany. Im dalej
       od danych, tym mniej prognoza jest warta."
    ),

    lc_h2("ch2-co-dalej", "Co dalej"),

    lc_p("Mamy trzy narzędzia oceny pojedynczego modelu. Wzorzec reszt pokazuje,
      czy prosta nie myli się systematycznie i czy spełnione są założenia.
      \\(R^2\\) mówi, jaką część zmienności Y model wyjaśnia. RMSE mówi, jak
      duże są typowe pomyłki w jednostkach Y. Razem pozwalają ocenić, czy
      danemu modelowi można ufać."),

    lc_p("Wszystkie modele w tym rozdziale miały tylko jeden predyktor.
      Wyniki szkół zależą jednak od wielu czynników naraz, a pojedynczy
      predyktor może przejmować wpływ innych, pominiętych zmiennych.
      Następny rozdział rozszerza ", gloss("regresja prosta", "regresję prostą"),
      " na wieloraką. Porównywaniem modeli zajmie się rozdział 04, a modelami
      dla zmiennej zależnej binarnej, takiej jak zdał / nie zdał, rozdział 05."),

    lc_chapter_next(
      num       = "03",
      title     = "Regresja wieloraka",
      lead      = "wiele zmiennych objaśniających naraz",
      target_id = "ch-wieloraka"
    )
  )
)


# ============================================================================
# SERVER
# ============================================================================

# Wszystkie ryciny rozdziału liczy przeglądarka albo R przy budowaniu strony.
ch2_server <- function(input, output, session) {}
