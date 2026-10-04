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
      w wykładzie 05, tylko zastosowany do reszt zamiast do surowych danych.
      Punkty wzdłuż prostej oznaczają reszty w przybliżeniu normalne."),

    lc_p("Panel pokazuje cztery proste modele dopasowane do danych z 420 okręgów
      szkolnych w Kalifornii (zbiór CASchools). Dla każdego widać dane
      z prostą, reszty względem wartości przewidywanych z wygładzoną linią
      trendu reszt oraz wykres Q-Q reszt."),

    figure_panel(
      label = "Ryc. 2.1", title = "Reszty na danych CASchools",
      full_width = TRUE,
      lc_toolbar(
        selectInput("ch2_resid_case", "Model",
            choices = .ch2_resid_choices,
            selected = "read_income"
          ),
        lc_readouts(uiOutput("ch2_resid_stats"))
      ),
      lc_plot("ch2_resid_plot", max_height = "340px"),
      uiOutput("ch2_resid_verdict"),
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

    lc_p("Wykład 05 zapowiadał, że w regresji założenia dotyczą reszt, a nie
      samych zmiennych. To bezpośrednie przedłużenie tego, co znamy z ", gloss("test t", "testu t"), "
      i ", gloss("ANOVA"), ". Tam resztami były odchylenia obserwacji od średniej
      ich grupy, a założenia mówiły o ich rozkładzie i rozrzucie w każdej
      grupie. W regresji resztami są odchylenia od prostej, a „grupy”
      zastępuje ciągły zakres wartości przewidywanych. ", gloss("zmienna zależna", "Zmienna zależna"), " nie
      musi mieć ", gloss("rozkład normalny", "rozkładu normalnego"), "; w przybliżeniu normalne powinny być
      reszty."),

    lc_p("Narzędzia są te same co w wykładzie 05. Normalność oceniamy wykresem
      Q-Q reszt. Jednorodność wariancji oceniamy, porównując rozrzut reszt
      w różnych częściach zakresu, tak jak w wykładzie 05 porównywaliśmy
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

    lc_p("Testy formalne, np. ", gloss("test Shapiro-Wilka"), " dla reszt albo
      test Breuscha-Pagana dla ",
      gloss("heteroskedastyczność", "heteroskedastyczności"), ", są dodatkiem
      do wykresu, tak jak w wykładzie 05. W dużych próbach wykrywają odchylenia
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

    lc_formula_box(withMathJax(
      "$$R^2 = 1 - \\frac{SS_{res}}{SS_{tot}} = 1 - \\frac{\\sum_{i=1}^{n}(y_i - \\hat{y}_i)^2}{\\sum_{i=1}^{n}(y_i - \\bar{y})^2}$$"
    )),

    lc_p("\\(R^2\\) przyjmuje wartości od 0 do 1. Zero oznacza, że model
      przewiduje nie lepiej niż średnia, jedynka, że wszystkie punkty leżą
      dokładnie na prostej. W regresji prostej \\(R^2\\) jest równe kwadratowi
      współczynnika ", gloss("korelacja Pearsona", "korelacji Pearsona"), " z wykładu 04, który już tam nazwaliśmy
      współczynnikiem determinacji. Dla czytania i odsetka uczniów
      z dotacją do obiadu \\(r = -0.88\\), więc \\(R^2 = 0.77\\): model
      wyjaśnia 77% zmienności wyników czytania między okręgami."),

    lc_p("Panel pokazuje trzy zbiory danych z tą samą prawdziwą prostą
      o nachyleniu 2.2. Różnią się tylko wielkością losowego szumu wokół
      niej."),

    figure_panel(
      label = "Ryc. 2.2", title = "To samo X i Y, różna siła wyjaśniania",
      full_width = TRUE,
      lc_plot("ch2_r2_compare_plot", ratio = "1.7/1", max_height = "360px")
    ),

    lc_p("Przy dużym szumie \\(R^2 = 0.18\\), przy średnim 0.54, przy małym
      0.95. Nachylenie we wszystkich trzech panelach jest takie samo, zmienia
      się tylko to, jak ciasno punkty trzymają się prostej. To ta sama
      lekcja co w wykładzie 04: siła związku to coś innego niż nachylenie.
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

    lc_p("Wysokie \\(R^2\\) też nie gwarantuje dobrego modelu. Model z łukiem
      w resztach może mieć wysokie \\(R^2\\), a mimo to systematycznie się
      mylić. Druga pułapka jest poważniejsza: model może dopasować się do
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
      label = "Ryc. 2.2b", title = "Przeuczenie: dopasowanie kontra generalizacja",
      full_width = TRUE,
      lc_plot("ch2_overfit_plot", ratio = "1.6/1", max_height = "380px"),
      uiOutput("ch2_overfit_stats")
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

    lc_formula_box(withMathJax(
      "$$RMSE = \\sqrt{\\frac{1}{n}\\sum_{i=1}^{n}(y_i - \\hat{y}_i)^2}$$"
    )),

    lc_p("RMSE ma jednostki Y, więc czyta się je wprost jako typową wielkość
      pomyłki modelu. Sama liczba nic jednak nie mówi bez odniesienia do skali
      Y. Naturalnym odniesieniem jest odchylenie standardowe Y: to RMSE
      prognozy, która dla każdego okręgu podaje po prostu średnią. Dobry model
      powinien mieć RMSE wyraźnie mniejsze. Panel pokazuje pasmo ±RMSE wokół
      prostej i porównuje RMSE z zakresem Y."),

    figure_panel(
      label = "Ryc. 2.3", title = "RMSE i zakres Y na danych CASchools",
      full_width = TRUE,
      lc_toolbar(
        selectInput("ch2_rmse_case", "Model",
            choices = .ch2_rmse_choices,
            selected = "read_lunch"
          ),
        lc_readouts(uiOutput("ch2_rmse_stats"))
      ),
      lc_plot("ch2_rmse_plot", max_height = "320px"),
      uiOutput("ch2_rmse_interpretation"),
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
      label = "Ryc. 2.4", title = "Ekstrapolacja poza zakres danych",
      full_width = TRUE,
      lc_toolbar(
        lc_slider("ch2_extrap_x", "Dochód okręgu (tys. USD)", 1, 80, 20, 1)
      ),
      lc_plot("ch2_extrap_plot", max_height = "320px"),
      uiOutput("ch2_extrap_stats"),
      uiOutput("ch2_extrap_verdict"),
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

ch2_server <- function(input, output, session) {

  # --- Widget: Reszty vs fitted na CASchools ---
  ch2_resid_spec <- reactive({
    case <- input$ch2_resid_case
    if (is.null(case)) case <- "read_income"
    .ch2_resid_specs[[case]]
  })

  ch2_resid_model <- reactive({
    spec <- ch2_resid_spec()
    form <- as.formula(paste(spec$y, "~", spec$x))
    lm(form, data = .cas_data)
  })

  zoom_plot_server("ch2_resid_plot", reactive({
    spec <- ch2_resid_spec()
    model <- ch2_resid_model()

    df_scatter <- data.frame(
      x = .cas_data[[spec$x]],
      y = .cas_data[[spec$y]]
    )
    df_resid <- data.frame(
      fitted = fitted(model),
      resid  = residuals(model)
    )

    p_left <- ggplot(df_scatter, aes(x = x, y = y)) +
      geom_point(color = upwr_secondary, alpha = 0.4, size = 1.7) +
      geom_smooth(method = "lm", se = FALSE,
                  color = unname(upwr_cat["niebo"]), linewidth = 1.2) +
      labs(x = unname(.cas_labels[spec$x]), y = unname(.cas_labels[spec$y])) +
      theme_upwr()

    p_right <- ggplot(df_resid, aes(x = fitted, y = resid)) +
      geom_point(color = upwr_secondary, alpha = 0.4, size = 1.7) +
      geom_hline(yintercept = 0, color = upwr_reference,
                 linetype = "dashed", linewidth = 0.8) +
      geom_smooth(method = "loess", se = FALSE,
                  color = unname(upwr_cat["terakota"]), linewidth = 1.2) +
      labs(x = expression(hat(Y)), y = expression(e[i] == y[i] - hat(y)[i])) +
      theme_upwr()

    p_qq <- ggplot(df_resid, aes(sample = resid)) +
      stat_qq(color = upwr_secondary, alpha = 0.4, size = 1.7) +
      stat_qq_line(color = unname(upwr_cat["niebo"]), linewidth = 1.2) +
      labs(x = "Kwantyle teoretyczne", y = "Kwantyle próbki") +
      theme_upwr()

    if (requireNamespace("patchwork", quietly = TRUE)) {
      patchwork::wrap_plots(p_left, p_right, p_qq, ncol = 3)
    } else if (requireNamespace("gridExtra", quietly = TRUE)) {
      gridExtra::arrangeGrob(p_left, p_right, p_qq, ncol = 3)
    } else {
      df_combined <- rbind(
        data.frame(panel = "Dane + linia regresji",
                   x = df_scatter$x, y = df_scatter$y),
        data.frame(panel = "Reszty względem wartości przewidywanych",
                   x = df_resid$fitted, y = df_resid$resid)
      )
      ggplot(df_combined, aes(x = x, y = y)) +
        geom_point(color = upwr_secondary, alpha = 0.4, size = 1.7) +
        facet_wrap(~ panel, scales = "free", ncol = 2) +
        theme_upwr()
    }
  }))

  output$ch2_resid_verdict <- renderUI({
    spec <- ch2_resid_spec()
    lc_status(
      lc_verdict(tags$strong(spec$title), type = spec$verdict),
      p(spec$comment)
    )
  })

  output$ch2_resid_stats <- renderUI({
    spec <- ch2_resid_spec()
    model <- ch2_resid_model()
    g <- broom::glance(model)

    tagList(
      lc_readout("R²", round(g$r.squared, 3), color = unname(upwr_cat["niebo"])),
      lc_readout("RMSE", round(sqrt(mean(residuals(model)^2)), 2), color = unname(upwr_cat["bursztyn"])),
      lc_readout("n", nrow(.cas_data), color = upwr_secondary)
    )
  })

  # --- Widget: R² compare (przeniesiony z ch4) ---
  zoom_plot_server("ch2_r2_compare_plot", reactive({
    set.seed(103)
    make_panel <- function(label, sigma) {
      x <- seq(-3, 3, length.out = 70)
      y <- 10 + 2.2 * x + rnorm(length(x), 0, sigma)
      data.frame(wariant = label, x = x, y = y)
    }
    df <- rbind(
      make_panel("Niskie R²", 12.0),
      make_panel("Średnie R²", 4.0),
      make_panel("Wysokie R²", 0.9)
    )

    r2_levels <- c("Niskie R²", "Średnie R²", "Wysokie R²")
    stats <- df %>%
      group_by(wariant) %>%
      summarise(r2 = summary(lm(y ~ x))$r.squared, .groups = "drop")
    df$wariant <- factor(df$wariant, levels = r2_levels)
    stats$wariant <- factor(stats$wariant, levels = r2_levels)

    ggplot(df, aes(x = x, y = y)) +
      geom_point(color = upwr_secondary, alpha = 0.5, size = 1.9) +
      geom_smooth(method = "lm", se = FALSE,
                  color = unname(upwr_cat["niebo"]), linewidth = 1.1) +
      geom_text(
        data = stats,
        aes(x = -2.8, y = Inf, label = paste0("R² = ", round(r2, 2))),
        inherit.aes = FALSE, hjust = 0, vjust = 1.6,
        color = upwr_secondary, fontface = "bold"
      ) +
      facet_wrap(~ wariant, nrow = 1) +
      labs(x = "X", y = "Y") +
      theme_upwr()
  }))

  # --- Widget: intuicja przeuczenia ---
  ch2_overfit_sets <- local({
    set.seed(26)
    f <- function(x) 4.8 * sin(x)
    train_x <- sort(runif(30, 0, 10))
    test_x <- sort(runif(180, 0, 10))
    list(
      train = data.frame(
        set = "Trening",
        x = train_x,
        y = f(train_x) + rnorm(length(train_x), 0, 0.9)
      ),
      test = data.frame(
        set = "Test",
        x = test_x,
        y = f(test_x) + rnorm(length(test_x), 0, 0.9)
      )
    )
  })

  ch2_overfit_metrics <- reactive({
    train <- ch2_overfit_sets$train
    test <- ch2_overfit_sets$test
    degrees <- c(1, 4, 12)
    do.call(rbind, lapply(degrees, function(degree) {
      model <- lm(y ~ poly(x, degree), data = train)
      data.frame(
        degree = degree,
        train_rmse = sqrt(mean((train$y - predict(model, train))^2)),
        test_rmse = sqrt(mean((test$y - predict(model, test))^2))
      )
    }))
  })

  zoom_plot_server("ch2_overfit_plot", reactive({
    train <- ch2_overfit_sets$train
    test <- ch2_overfit_sets$test
    degrees <- c(1, 4, 12)
    labels <- c(
      "1" = "Zbyt prosty",
      "4" = "Rozsądnie elastyczny",
      "12" = "Przeuczony"
    )

    grid <- do.call(rbind, lapply(degrees, function(degree) {
      model <- lm(y ~ poly(x, degree), data = train)
      x_grid <- seq(0, 10, length.out = 260)
      data.frame(
        degree = factor(degree, levels = degrees, labels = labels[as.character(degrees)]),
        x = x_grid,
        y = predict(model, newdata = data.frame(x = x_grid))
      )
    }))

    train_plot <- train
    test_plot <- test
    train_plot$set <- "Trening"
    test_plot$set <- "Test"
    points <- do.call(rbind, lapply(labels[as.character(degrees)], function(lab) {
      tmp <- rbind(train_plot, test_plot)
      tmp$degree <- factor(lab, levels = labels[as.character(degrees)])
      tmp
    }))

    ggplot() +
      geom_point(data = points[points$set == "Test", ],
                 aes(x = x, y = y), color = unname(upwr_cat["bursztyn"]),
                 alpha = 0.22, size = 1.6) +
      geom_point(data = points[points$set == "Trening", ],
                 aes(x = x, y = y), color = upwr_secondary,
                 alpha = 0.72, size = 2.1) +
      geom_line(data = grid, aes(x = x, y = y),
                color = unname(upwr_cat["niebo"]), linewidth = 1.05) +
      facet_wrap(~ degree, nrow = 1) +
      labs(x = "X", y = "Y", caption = "Ciemne punkty = trening; jasne bursztynowe = nowe dane testowe") +
      coord_cartesian(ylim = c(-8, 8)) +
      theme_upwr()
  }))

  output$ch2_overfit_stats <- renderUI({
    metrics <- ch2_overfit_metrics()
    labels <- c(
      "1" = "Zbyt prosty",
      "4" = "Rozsądnie elastyczny",
      "12" = "Przeuczony"
    )
    metrics$model <- unname(labels[as.character(metrics$degree)])

    lc_table(as.data.frame(metrics),
      cols = list(
        lc_col("model", "Model", "row"),
        lc_col("degree", "Stopień"),
        lc_col("train_rmse", "RMSE trening", digits = 2),
        lc_col("test_rmse", "RMSE test", digits = 2)
      )
    )
  })

  # --- Widget: RMSE i zakres Y na CASchools ---
  ch2_rmse_spec <- reactive({
    case <- input$ch2_rmse_case
    if (is.null(case)) case <- "read_lunch"
    .ch2_rmse_specs[[case]]
  })

  ch2_rmse_model <- reactive({
    spec <- ch2_rmse_spec()
    form <- as.formula(paste(spec$y, "~", spec$x))
    lm(form, data = .cas_data)
  })

  zoom_plot_server("ch2_rmse_plot", reactive({
    spec <- ch2_rmse_spec()
    model <- ch2_rmse_model()
    rmse <- sqrt(mean(residuals(model)^2))
    y_vals <- .cas_data[[spec$y]]
    y_mean <- mean(y_vals)

    df <- data.frame(
      x = .cas_data[[spec$x]],
      y = y_vals,
      fitted = fitted(model)
    )

    ggplot(df, aes(x = x, y = y)) +
      geom_point(color = upwr_secondary, alpha = 0.42, size = 1.8) +
      geom_smooth(method = "lm", se = FALSE,
                  color = unname(upwr_cat["niebo"]), linewidth = 1.2) +
      geom_ribbon(
        data = local({
          ord <- order(df$x)
          data.frame(x = df$x[ord], ymin = df$fitted[ord] - rmse, ymax = df$fitted[ord] + rmse)
        }),
        aes(x = x, ymin = ymin, ymax = ymax),
        inherit.aes = FALSE,
        fill = unname(upwr_cat["bursztyn"]), alpha = 0.18
      ) +
      annotate("label", x = min(df$x), y = max(df$y),
               hjust = 0, vjust = 1,
               label = paste0("Pasmo ±RMSE = ±", round(rmse, 1)),
               color = unname(upwr_cat["bursztyn"]),
               fill = "white", linewidth = 0) +
      labs(
        x = unname(.cas_labels[spec$x]),
        y = unname(.cas_labels[spec$y])
      ) +
      theme_upwr()
  }))

  output$ch2_rmse_stats <- renderUI({
    spec <- ch2_rmse_spec()
    model <- ch2_rmse_model()
    g <- broom::glance(model)
    rmse <- sqrt(mean(residuals(model)^2))
    y_vals <- .cas_data[[spec$y]]
    y_range <- diff(range(y_vals))
    rmse_ratio <- rmse / y_range

    tagList(
      lc_readout("R²", round(g$r.squared, 3), color = unname(upwr_cat["niebo"])),
      lc_readout("RMSE", round(rmse, 2), color = unname(upwr_cat["bursztyn"])),
      lc_readout("Zakres Y (max − min)", round(y_range, 1), color = upwr_secondary),
      lc_readout("RMSE / zakres", paste0(round(rmse_ratio * 100, 1), "%"), color = unname(upwr_cat["terakota"]))
    )
  })

  # --- Widget: Ekstrapolacja ---
  .ch2_extrap_model <- lm(read ~ income, data = .cas_data)
  .ch2_extrap_x_range <- range(.cas_data$income)

  zoom_plot_server("ch2_extrap_plot", reactive({
    x_val <- input$ch2_extrap_x
    if (is.null(x_val)) x_val <- 20
    x_obs <- .cas_data$income
    y_obs <- .cas_data$read
    x_range <- .ch2_extrap_x_range
    x_grid <- seq(min(1, x_val - 2), max(80, x_val + 2), length.out = 300)
    df_line <- data.frame(
      income = x_grid,
      read   = predict(.ch2_extrap_model, newdata = data.frame(income = x_grid)),
      outside = x_grid < x_range[1] | x_grid > x_range[2]
    )
    y_pred <- predict(.ch2_extrap_model, newdata = data.frame(income = x_val))
    in_range <- x_val >= x_range[1] & x_val <= x_range[2]
    point_color <- if (in_range) unname(upwr_cat["niebo"]) else unname(upwr_cat["terakota"])

    ggplot() +
      annotate("rect",
        xmin = x_range[1], xmax = x_range[2],
        ymin = -Inf, ymax = Inf,
        fill = upwr_secondary, alpha = 0.08) +
      geom_point(data = data.frame(x = x_obs, y = y_obs),
                 aes(x = x, y = y),
                 color = upwr_secondary, alpha = 0.35, size = 1.6) +
      geom_line(data = df_line[!df_line$outside, ],
                aes(x = income, y = read),
                color = unname(upwr_cat["niebo"]), linewidth = 1.1) +
      geom_line(data = df_line[df_line$outside, ],
                aes(x = income, y = read),
                color = unname(upwr_cat["niebo"]), linewidth = 1.1,
                linetype = "dashed") +
      geom_vline(xintercept = x_val, color = point_color,
                 linetype = "dotted", linewidth = 0.9) +
      geom_point(data = data.frame(x = x_val, y = y_pred),
                 aes(x = x, y = y),
                 color = point_color, size = 4, shape = 18) +
      labs(
        x = "Dochód okręgu (tys. USD)",
        y = "Wynik testu czytania",
        caption = "Szary pas = zakres danych treningowych"
      ) +
      theme_upwr()
  }))

  output$ch2_extrap_verdict <- renderUI({
    x_val <- input$ch2_extrap_x
    if (is.null(x_val)) x_val <- 20
    x_range <- .ch2_extrap_x_range
    y_pred <- predict(.ch2_extrap_model, newdata = data.frame(income = x_val))
    in_range <- x_val >= x_range[1] & x_val <= x_range[2]
    dist_pct <- min(abs(x_val - x_range[1]), abs(x_val - x_range[2])) /
                diff(x_range) * 100

    if (in_range) {
      lc_status(
        lc_verdict(tags$strong("W zakresie danych"), type = "ok"),
        p(sprintf("Predykcja: %.1f pkt. Jesteśmy wewnątrz zakresu danych — predykcja ma sens.", y_pred))
      )
    } else {
      lc_status(
        lc_verdict(tags$strong("Ekstrapolacja"), type = "warning"),
        p(sprintf("Predykcja: %.1f pkt. Jesteśmy %.0f%% zakresu danych poza granicą — brak gwarancji.", y_pred, dist_pct))
      )
    }
  })

  output$ch2_extrap_stats <- renderUI({
    x_val <- input$ch2_extrap_x
    if (is.null(x_val)) x_val <- 20
    x_range <- .ch2_extrap_x_range
    y_pred <- predict(.ch2_extrap_model, newdata = data.frame(income = x_val))

    tagList(
      lc_readout("X podany", paste(x_val, "tys. USD"), color = unname(upwr_cat["niebo"])),
      lc_readout("Predykcja", paste(round(y_pred, 1), "pkt"), color = unname(upwr_cat["bursztyn"])),
      lc_readout("Zakres X danych", paste0(round(x_range[1], 0), "–", round(x_range[2], 0), " tys. USD"), color = upwr_secondary)
    )
  })

  output$ch2_rmse_interpretation <- renderUI({
    spec <- ch2_rmse_spec()
    model <- ch2_rmse_model()
    rmse <- sqrt(mean(residuals(model)^2))
    y_vals <- .cas_data[[spec$y]]
    y_range <- diff(range(y_vals))
    y_label <- unname(.cas_labels[spec$y])
    y_sd <- sd(y_vals)

    lc_caption(
      sprintf("Typowa pomyłka modelu to ±%.1f w skali „%s” (zakres %.0f, SD %.1f).",
                rmse, y_label, y_range, y_sd),
      tone = "info"
    )
  })
}
