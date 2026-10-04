# Tab 1: Katalog — 7 typowych problemów w danych z wizualizacjami

ch1_ui <- lecture_chapter(id = "ch1", num = "1", title = "Katalog", content = tagList(

  lc_chapter_hero(
    kicker = "Rozdział 01 · Co czyni dobry zbiór danych?",
    num    = "01",
    title  = "Katalog problemów.",
    lead   = "Siedem typowych usterek, które potrafią zepsuć analizę:
              za mało danych, brak zmienności, błędy, niejasne zmienne,
              braki, brak niezależności i zła struktura."
  ),

  lc_h2("sec-01", "Katalog problemów w danych"),

  lc_p("Metody z wykładów 01–06 zakładały milcząco, że dane są w porządku:
    obserwacji jest dość, zmienne mają ustalone znaczenie, wartości są
    wpisane poprawnie, a każdy wiersz to osobna, niezależna obserwacja.
    W prawdziwych zbiorach każde z tych założeń może zawieść. Ten rozdział
    zbiera siedem najczęstszych problemów w jeden katalog, do którego
    będziemy się odwoływać przy ocenie dziesięciu zbiorów w rozdziałach 2–11."),

  lc_p("Każdy problem opisujemy w tym samym porządku. Najpierw definicja,
    potem mały przykład pokazany na dwa sposoby: jako tabela, tak jak
    wygląda w arkuszu albo programie statystycznym, i jako wykres. Dalej
    pytamy, czym problem grozi w analizie i co można z nim zrobić. Przy
    kilku problemach przełącznik nad panelem pokazuje te same dane przed
    naprawą i po niej."),

  lc_p("Problemy nie są równie groźne. Za mało danych, brak zmienności,
    brak niezależności i zła struktura zwykle dyskwalifikują zbiór albo
    wymagają innych metod niż te z kursu. Błędy, niejasne zmienne i braki
    danych da się często naprawić, choć naprawa kosztuje czas i część
    informacji."),

  # --- Problem 1: Za mało danych ---
  lc_h2("sec-p1", "1", "Za mało danych"),

  lc_p("Każda statystyka policzona z próby jest tylko oszacowaniem wartości
    w populacji. Jak bardzo to oszacowanie może się mylić, mówi ",
    gloss("błąd standardowy"), ". Dla średniej jest to:"),

  lc_formula_box(withMathJax(
    "$$SE = \\frac{s}{\\sqrt{n}}$$"
  )),

  lc_p("Błąd standardowy maleje dopiero z pierwiastkiem z liczby obserwacji.
    Przy małym \\(n\\) oszacowania są tak niepewne, że prawdziwego efektu
    nie da się odróżnić od przypadkowych wahań. O zbiorze z za małą liczbą
    obserwacji mówimy, że ma za mało danych do planowanej analizy."),

  lc_p("Przykład: kolega przepytał sześcioro znajomych (cztery kobiety
    i dwóch mężczyzn) i chce ", gloss("test t", "testem t"), " sprawdzić, czy kobiety i mężczyźni
    różnią się średnią ocen. Panel pokazuje jego dane i ", gloss("histogram", "histogram"), " średnich ocen."),

  figure_panel(
    label = "Ryc. 1.1",
    title = "Sześć ankiet",
    lc_plots(
      uiOutput("cat1_table"),
      lc_plot("cat1_plot", max_height = "280px")
    )
  ),

  lc_p("Histogram z sześciu wartości ma cztery słupki o wysokości jednej
    do trzech obserwacji. Nie da się z niego odczytać kształtu rozkładu,
    więc nie da się też ocenić normalności, o której była mowa w wykładzie 05.
    Średnia ocen wynosi 3.92, ale 95% ", gloss("przedział ufności"), " dla niej
    rozciąga się od 3.30 do 4.53, czyli obejmuje ponad jeden pełny stopień
    oceny."),

  lc_p("Jeszcze gorzej wygląda porównanie grup. Przy czterech kobietach
    i dwóch mężczyznach ", gloss("moc testu"), " t dla średniego efektu
    (", gloss("d Cohena", "d Cohena"), " = 0.5) wynosi około 7%, a dla dużego (d = 0.8) około 11%. Nawet
    jeśli różnica w populacji istnieje, test przeoczy ją w zdecydowanej
    większości takich badań i popełni ",
    gloss("błąd drugiego rodzaju"), ". Wynik nieistotny przy tak małej
    próbie nie jest dowodem, że różnicy nie ma."),

  lc_p("Za mało danych nie da się naprawić po fakcie. Jedyne lekarstwo to
    więcej obserwacji, a ile ich potrzeba, warto oszacować przed badaniem
    na podstawie spodziewanej ", gloss("wielkość efektu", "wielkości efektu"),
    ". Dla ilustracji: żeby test t wykrył duży efekt z mocą 80%, potrzeba
    około 26 osób w każdej grupie, a dla średniego efektu około 64.
    Liczy się liczebność w każdej porównywanej grupie, nie w całym zbiorze."),

  # --- Problem 2: Brak zmienności ---
  lc_h2("sec-p2", "2", "Brak zmienności"),

  lc_p("Statystyka zajmuje się zmiennością. Korelacja i regresja pytają,
    czy różnice w jednej zmiennej idą w parze z różnicami w drugiej, a testy
    porównują grupy. Zmienna, która prawie się nie zmienia, nie niesie
    takiej informacji: jej ", gloss("odchylenie standardowe"), " jest bliskie
    zera albo niemal wszystkie obserwacje trafiają do jednej kategorii.
    Taki stan nazywamy brakiem zmienności."),

  lc_p("Przykład: firma przeprowadziła wśród pracowników działu IT ankietę
    zadowolenia, a wszyscy wiedzieli, że odpowiedzi czyta szef. W tym samym
    pliku zapisano staż pracy i wynagrodzenie. Panel pokazuje rozkład ocen
    zadowolenia i wykres rozrzutu stażu i wynagrodzenia."),

  figure_panel(
    label = "Ryc. 1.2",
    title = "Ankieta zadowolenia w dziale IT",
    lc_plots(
      uiOutput("cat2_table"),
      tags$div(
        lc_plot("cat2_plot_zadowolenie", max_height = "200px"),
        lc_plot("cat2_plot", max_height = "200px")
      )
    )
  ),

  lc_p("Wszystkie 12 ocen zadowolenia to 4 albo 5 (trzy czwórki i dziewięć
    piątek), a niższe wartości skali nie pojawiają się wcale. Nie da się
    więc sprawdzić, co różni zadowolonych od niezadowolonych, bo tych
    drugich w danych nie ma. Podobnie ze stażem: wszyscy pracują od 2.8 do
    3.2 roku, odchylenie standardowe wynosi 0.12 roku. Wynagrodzenia
    różnią się mocno, od 3200 do 7500 PLN, ale na wykresie punkty tworzą
    pionowy pas. Korelacja stażu z wynagrodzeniem wynosi 0.20, tyle że
    opiera się na różnicach stażu rzędu kilku miesięcy i nic nie mówi
    o pracownikach z rocznym czy dziesięcioletnim stażem. Dział ma jedną
    wartość, IT, więc działów też nie porównamy."),

  lc_p("Brak zmienności grozi dwojako. Albo analiza nic nie wykaże, bo
    nie ma czego wyjaśniać, albo pokaże coś przypadkowego: nachylenie
    prostej regresji (wykład 06) szacowane na bardzo wąskim zakresie
    \\(X\\) ma ogromny błąd standardowy i łatwo je przenieść poza zakres
    danych, gdzie nie ma żadnego uzasadnienia. Tego problemu nie naprawi
    żadna metoda. Potrzebne są dane z szerszym zakresem wartości, a przy
    ankietach także warunki, w których respondenci mogą odpowiadać
    szczerze, na przykład anonimowość. Przed analizą warto sprawdzić
    zakres i odchylenie standardowe każdej zmiennej ilościowej oraz
    liczebności kategorii każdej zmiennej jakościowej."),

  # --- Problem 3: Błędy i literówki ---
  lc_h2("sec-p3", "3", "Błędy i literówki w danych"),

  lc_p("Jeśli zakres zmiennej jest szeroki, warto zapytać, czy wszystkie
    wartości są prawdziwe. Błędy i literówki w danych to wartości
    niemożliwe albo niewiarygodne, które powstały przy wpisywaniu lub
    kopiowaniu: brakujące lub nadmiarowe zera, zły znak, przesunięty
    przecinek, sklejone cyfry. Trzeba je odróżniać od prawdziwych ",
    gloss("wartość odstająca", "wartości odstających"), ", czyli obserwacji
    rzadkich, ale możliwych. Do tego rozróżnienia wrócimy w rozdziale 9."),

  lc_p("Przykład: dwanaście ogłoszeń z portalu nieruchomości skopiowanych
    do arkusza. Na pierwszy rzut oka wszystko wygląda dobrze. W surowych
    danych podejrzane komórki są podświetlone, a przełącznik pokazuje
    tabelę i wykres ceny względem powierzchni przed poprawkami i po nich."),

  figure_panel(
    label = "Ryc. 1.3",
    title = "Ogłoszenia mieszkań",
    lc_toolbar(
      lc_segmented("cat3_view", NULL,
        choices = c("Surowe" = "raw", "Oczyszczone" = "clean"))
    ),
    lc_plots(
      uiOutput("cat3_table"),
      lc_plot("cat3_plot", max_height = "280px")
    )
  ),

  lc_p("W czterech wierszach jest pięć błędów: cena 45 PLN zamiast 450 000
    (zgubione zera), cena -300 000 (zły znak), cena 5 500 000 zamiast
    550 000 (nadmiarowe zero), powierzchnia 1200 m² zamiast 120 m²
    i 42 pokoje zamiast 4. Średnia cena w surowych danych wynosi około
    727 500 PLN, a ", gloss("mediana", "mediana"), " 375 000 PLN. Po poprawkach średnia spada do
    402 500 PLN i niemal zrównuje się z medianą (405 000 PLN). Na
    regresję błędy działają jeszcze mocniej: w surowych danych
    nachylenie prostej jest ujemne (około -270 PLN za metr kwadratowy),
    jakby większe mieszkania były tańsze. Po poprawkach nachylenie wynosi
    około +1190 PLN za metr kwadratowy. Związek pozostaje słaby
    (R² = 0.09), bo dwanaście mieszkań z różnych dzielnic to mało danych,
    ale jego kierunek jest wreszcie sensowny."),

  lc_p("Takie błędy psują wszystko, co liczy się z sumy wartości: średnią
    i odchylenie standardowe (wykład 01), korelację i regresję (wykłady
    04 i 06). Pojedynczy punkt daleko od reszty potrafi odwrócić kierunek
    prostej. Błąd można poprawić tylko wtedy, gdy wiadomo, jaka jest
    prawdziwa wartość, na przykład ze źródła danych. W przeciwnym razie
    bezpieczniej oznaczyć ją jako brak. Każdą poprawkę warto zapisać, żeby
    analizę dało się odtworzyć."),

  lc_note("Zasada", rule = TRUE,
    "Przed rozpoczęciem analizy sprawdź minimum i maksimum każdej zmiennej
    ilościowej i zapytaj, czy takie wartości są w ogóle możliwe."
  ),

  # --- Problem 4: Źle zdefiniowane zmienne ---
  lc_h2("sec-p4", "4", "Źle zdefiniowane zmienne"),

  lc_p("Żeby sprawdzić zakres zmiennej, trzeba najpierw wiedzieć, co ona
    mierzy i w jakich jednostkach. W wykładzie 01 każda zmienna miała
    jasny typ: ilościowa albo jakościowa, z ustaloną jednostką lub listą
    kategorii. Zmienna jest źle zdefiniowana, gdy tego brakuje: respondent
    sam decyduje o jednostce, skali i sposobie zapisu, więc odpowiedzi nie
    są ze sobą porównywalne."),

  lc_p("Przykład: student przygotował ankietę z pytaniami otwartymi
    o czas nauki, ocenę kursu i aktywność fizyczną. Każdy odpowiedział
    po swojemu. W surowych danych wykres pokazuje, ile odpowiedzi
    o czas nauki program statystyczny rozpozna jako liczbę, a po
    oczyszczeniu — histogram odzyskanych wartości."),

  figure_panel(
    label = "Ryc. 1.4",
    title = "Ankieta z pytaniami otwartymi",
    lc_toolbar(
      lc_segmented("cat4_view", NULL,
        choices = c("Surowe" = "raw", "Oczyszczone" = "clean"))
    ),
    lc_plots(
      uiOutput("cat4_table"),
      lc_plot("cat4_plot", max_height = "280px")
    )
  ),

  lc_p("Z dziesięciu odpowiedzi o czas nauki tylko dwie („5” i „3”) są
    liczbami. Pozostałe to tekst: „3-4h”, „ok. 2 godziny”, „cały dzień”,
    „weekendy”. Program statystyczny nie wie, co z nimi zrobić, więc
    zmienna, która miała być ilościowa, staje się tekstowa. Ręczne
    czyszczenie odzyskuje po pięć wartości w każdej z trzech zmiennych,
    czyli połowa odpowiedzi zamienia się w braki danych. Każda decyzja
    przy czyszczeniu jest przy tym arbitralna: czy „3-4h” to 3.5 godziny?
    Czy „6h dziennie” to 6, czy 42 godziny tygodniowo? Czy ocena „4” jest
    w skali od 1 do 5, czy od 1 do 10?"),

  lc_p("Źle zdefiniowana zmienna grozi utratą danych i wynikami, które
    zależą bardziej od decyzji osoby czyszczącej niż od odpowiedzi
    respondentów. Część takich zmiennych da się uratować, sprowadzając
    odpowiedzi do kilku kategorii (wrócimy do tego w rozdziale 8), ale
    wtedy zmienna ilościowa staje się jakościowa i traci część informacji.
    Najskuteczniej zapobiegać: zadawać pytania zamknięte, podawać jednostkę
    w treści pytania i używać spójnych skal, na przykład ",
    gloss("skala Likerta", "skali Likerta"), "."),

  lc_note("Zasada", rule = TRUE,
    "Jednostkę, skalę i listę odpowiedzi ustal przed zbieraniem danych
    i sprawdź je w pilotażu ankiety na kilku osobach."
  ),

  # --- Problem 5: Braki danych ---
  lc_h2("sec-p5", "5", "Braki danych (NA)"),

  lc_p("Czyszczenie z poprzedniego przykładu zostawiło puste komórki.
    W prawdziwych danych pojawiają się one także dlatego, że ktoś pominął
    pytanie, przyrząd się zepsuł albo pomiaru nie dało się wykonać. ",
    gloss("braki danych", "Braki danych"), " to komórki bez wartości;
    programy statystyczne oznaczają je zwykle jako NA. Przy ocenie liczą
    się dwie rzeczy: ile ich jest i dlaczego się pojawiły."),

  lc_p("Przykład: ankieta wśród dwunastu studentów, w której nie każdy
    odpowiedział na wszystkie pytania. Wykres pokazuje odsetek braków
    w każdej zmiennej."),

  figure_panel(
    label = "Ryc. 1.5",
    title = "Ankieta z pominiętymi pytaniami",
    lc_plots(
      uiOutput("cat5_table"),
      lc_plot("cat5_plot", max_height = "280px")
    )
  ),

  lc_p("W wieku i kierunku brakuje po 3 z 12 wartości (25%), w stresie
    i ocenach po 4 (33%). Każda zmienna osobno wygląda na uszkodzoną,
    ale nie zniszczoną. Braki leżą jednak w różnych wierszach, więc
    komplet odpowiedzi mają tylko 2 z 12 osób. Gdybyśmy usunęli każdy
    wiersz z jakimkolwiek brakiem, z dwunastu ankiet zostałyby dwie."),

  lc_p("Braki zmniejszają liczebność próby, a więc i moc testu (wykład 04).
    Groźniejsze jest jednak ", gloss("obciążenie", "obciążenie"), ". Braki rzadko pojawiają się
    losowo: jeśli o swoje oceny nie chcą mówić głównie osoby ze słabymi
    wynikami, średnia z pozostałych będzie zawyżona i żadne zwiększenie
    próby tego nie naprawi. Dlatego przed analizą warto policzyć braki
    w każdej zmiennej i liczbę kompletnych wierszy, a potem zastanowić się
    nad ich przyczyną. Gdy braków jest niewiele i nie widać powodu, by
    dotyczyły konkretnej grupy, usunięcie niepełnych wierszy jest zwykle
    bezpieczne. Przy większym odsetku można rozważyć ",
    gloss("imputacja", "imputację"), ", czyli uzupełnienie braków
    oszacowanymi wartościami, albo zrezygnować z najbardziej dziurawej
    zmiennej. Braki bywają też ukryte pod liczbą, na przykład zerem albo
    999 wpisanym zamiast pustej komórki. Taki przypadek zobaczymy
    w rozdziale 9."),

  # --- Problem 6: Brak niezależności ---
  lc_h2("sec-p6", "6", "Brak niezależności obserwacji"),

  lc_p("Dotąd zakładaliśmy, że każdy wiersz to nowa, osobna informacja.
    Testy z wykładów 04 i 05, korelacja i regresja wymagają ",
    gloss("niezależność obserwacji", "niezależności obserwacji"), ":
    wartość jednej obserwacji nie powinna nic mówić o wartości innej.
    Założenie to łamią pomiary powtarzane w czasie (dzień po dniu),
    w przestrzeni (sąsiednie działki) i w grupach (uczniowie z tej samej
    klasy). Takie dane mają w tabeli wiele wierszy, ale mniej
    niezależnej informacji, niż wskazuje liczba wierszy."),

  lc_p("Przykład: dzienna temperatura powietrza przez pół roku, od października
    2023 do marca 2024. W tabeli dane wyglądają zwyczajnie. Wykres pokazuje
    je w kolejności pomiarów, a przełącznik zamienia dni na średnie miesięczne."),

  figure_panel(
    label = "Ryc. 1.6",
    title = "Dzienna temperatura przez pół roku",
    lc_toolbar(
      lc_segmented("cat6_view", NULL,
        choices = c("Dzienne (surowe)" = "daily", "Miesięczne (agregat)" = "monthly"))
    ),
    lc_plots(
      uiOutput("cat6_table"),
      lc_plot("cat6_plot", max_height = "280px")
    )
  ),

  lc_p("W tabeli są 183 wiersze, ale wykres liniowy zdradza wyraźną ",
    gloss("sezonowość"), ": średnia temperatura spada z 7.8 °C w październiku
    do -4.2 °C w styczniu i wraca do 2.4 °C w marcu. Kolejne dni są do
    siebie podobne. Korelacja temperatury z temperaturą dnia poprzedniego
    wynosi 0.79, więc znając dzisiejszy pomiar, dobrze przewidzimy jutrzejszy.
    Taką zależność obserwacji od poprzednich nazywamy ",
    gloss("autokorelacja", "autokorelacją"), "."),

  lc_p("Test, który traktuje 183 dni jak 183 niezależne obserwacje,
    zakłada więcej informacji, niż jest w danych. Błąd standardowy wychodzi
    za mały, przedziały ufności za wąskie, a p-wartości za małe, więc
    łatwo o fałszywe odkrycie. Jednym wyjściem jest ",
    gloss("agregacja", "agregacja"), " do większych jednostek. Po
    przejściu na średnie miesięczne znika zależność w obrębie miesiąca,
    ale zostaje tylko 6 obserwacji, a średnie z kolejnych miesięcy wciąż
    układają się w gładką krzywą sezonową. Agregacja jest więc wyborem
    z kosztem, a nie darmową naprawą. Drugim wyjściem są metody dla ",
    gloss("szereg czasowy", "szeregów czasowych"), " i danych pogrupowanych,
    które wykraczają poza ten kurs."),

  lc_note("Przykład", title = "Ten sam problem w danych satelitarnych",
    " sąsiednie piksele często mają podobną temperaturę, wilgotność czy NDVI.
      Obraz z milionem pikseli nie musi więc zawierać miliona niezależnych
      informacji. Dodatkowo braki wywołane zachmurzeniem mają strukturę
      przestrzenną i sezonową — usunięcie ich nie zawsze jest neutralne."
  ),

  # --- Problem 7: Zła struktura ---
  lc_h2("sec-p7", "7", "Zła struktura danych"),

  lc_p("Szczególnie częsty przypadek zależności powstaje wtedy, gdy wiersz
    tabeli nie odpowiada temu, o co pyta analiza. ",
    gloss("jednostka obserwacji", "Jednostką obserwacji"), " nazywamy
    obiekt, o którym chcemy wnioskować: osobę, firmę, okręg szkolny, dzień.
    Zła struktura danych oznacza, że wiersze opisują coś innego, najczęściej
    zdarzenia (wizyty, transakcje, wypowiedzi), a pytanie dotyczy osób
    albo obiektów, do których te zdarzenia należą."),

  lc_p("Przykład: pomiary ciśnienia skurczowego u 30 pacjentów, z których
    każdy był na kilku wizytach. Każdy wiersz to jedna wizyta, a nie jeden
    pacjent. Przełącznik zamienia tabelę wizyt na tabelę pacjentów, a wykres
    pokazuje, ile obserwacji mamy w każdej z wersji."),

  figure_panel(
    label = "Ryc. 1.7",
    title = "Wizyty pacjentów",
    lc_toolbar(
      lc_segmented("cat7_view", NULL,
        choices = c("Wizyty (surowe)" = "events", "Pacjenci (agregat)" = "agg"))
    ),
    lc_plots(
      uiOutput("cat7_table"),
      lc_plot("cat7_plot", max_height = "280px")
    )
  ),

  lc_p("Tabela ma ", nrow(cat_patients_visits), " wierszy, ale opisują one
    tylko ", nrow(cat_patients_agg), " pacjentów: każdy był na 3–6 wizytach.
    Jeśli pytamy o różnicę ciśnienia między kobietami a mężczyznami,
    jednostką obserwacji jest pacjent. Na poziomie wizyt porównalibyśmy
    83 wiersze kobiet z 45 wierszami mężczyzn, choć w rzeczywistości to
    20 kobiet i 10 mężczyzn. Test t na wizytach traktowałby każdą kolejną
    wizytę tej samej osoby jak nowego pacjenta i dawałby wynik pewniejszy,
    niż uzasadniają dane. To ten sam brak niezależności co przy
    temperaturze, tylko ukryty w strukturze tabeli."),

  lc_p("Rozwiązaniem jest przekształcenie danych do właściwej jednostki
    obserwacji, na przykład policzenie średniego ciśnienia każdego
    pacjenta. Po takiej agregacji mamy n = ", nrow(cat_patients_agg),
    ", a nie ", nrow(cat_patients_visits), ". Jeśli po przekształceniu
    zostaje zbyt mało obserwacji, wracamy do problemu nr 1. Tak będzie
    z filmami Tarantino w rozdziale 5."),

  lc_note("Zasada", rule = TRUE,
    "Przed analizą odpowiedz, co jest jednostką obserwacji: osoba, firma,
    dzień czy zdarzenie. Wiersz w tabeli nie zawsze jest obserwacją."
  ),

  # --- Podsumowanie: lista kontrolna ---
  lc_h2("sec-02", "Podsumowanie: lista kontrolna jakości danych"),

  lc_p("Siedem problemów z katalogu da się zamienić w listę pytań, które
    warto zadać każdemu zbiorowi, zanim zacznie się analizę. Lista ma dwie
    części. Kryteria krytyczne obejmują dopasowanie danych do ", gloss("pytanie badawcze", "pytania badawczego"), ", liczebność, typy zmiennych, zmienność, strukturę
    i niezależność obserwacji. Jeśli zbiór ich nie spełnia, lepiej szukać
    innego. Kryteria naprawialne dotyczą braków danych, definicji zmiennych
    i błędów: wymagają pracy, ale da się je spełnić po oczyszczeniu."),

  lc_p("Panel poniżej pozwala przejść przez listę dla dowolnego zbioru.
    Pasek pod listą podsumowuje zaznaczone kryteria. Jeśli którekolwiek
    kryterium krytyczne nie jest spełnione, werdykt jest negatywny bez
    względu na kryteria naprawialne."),

  figure_panel(
    label = "Lista kontrolna",
    title = "Lista kontrolna jakości danych",
    tags$p(lc_verdict(tags$strong("Krytyczne:"), type = "danger"),
      " jeśli zbiór ich nie spełnia, poszukaj innego"),
    checkboxGroupInput("intro_critical", NULL,
      choices = c(
        "Dane odpowiadają hipotezie badawczej (mierzą badane zjawisko)" = "hyp",
        "Wystarczająca liczba obserwacji w każdej porównywanej grupie" = "n",
        "Różne typy zmiennych (ilościowe i jakościowe)" = "mix",
        "Zmienność w danych (nie wszystko takie samo)" = "var",
        "Struktura danych pasuje do planowanych analiz" = "fit",
        "Niezależność obserwacji (albo możliwość agregacji)" = "indep"
      )
    ),
    tags$p(lc_verdict(tags$strong("Naprawialne:"), type = "warning"),
      " wymagają pracy, ale się da"),
    checkboxGroupInput("intro_fixable", NULL,
      choices = c(
        "Niewiele braków danych" = "missing",
        "Jednoznaczne definicje zmiennych" = "def",
        "Brak błędów i podejrzanych wartości" = "errors"
      )
    ),
    uiOutput("intro_thermometer")
  ),

  lc_p("Kryteria naprawialne nie rekompensują krytycznych. Zbiór bez
    braków i błędów, ale z sześcioma obserwacjami albo z jedną wartością
    w kluczowej zmiennej, nadal nie odpowie na pytanie badawcze. W kolejnych
    dziesięciu rozdziałach tę samą listę przyłożymy do konkretnych zbiorów
    danych."),

  lc_chapter_next(
    num = "02",
    title = "Szkoły w Kalifornii",
    lead = "Katalog sprawdzimy na dziesięciu zbiorach danych, zaczynając od wzorcowego.",
    target_id = "ch2"
  )
))

ch1_server <- function(input, output, session) {

  # --- Problem 1: Za mało danych ---
  output$cat1_table <- renderUI({
    dd_data_table(cat_small,
      types = c("id", "nominalna", "ciągła", "porządkowa", "ciągła"))
  })

  zoom_plot_server("cat1_plot", reactive({
    ggplot(cat_small, aes(x = oceny)) +
      geom_histogram(bins = 4, fill = data_bad, color = "white", alpha = 0.8) +
      geom_vline(xintercept = mean(cat_small$oceny), linetype = "dashed", color = data_reference, linewidth = 1) +
      annotate("text", x = mean(cat_small$oceny) + 0.15, y = 2.2,
               label = paste0("M = ", round(mean(cat_small$oceny), 2)), hjust = 0, size = 4.5) +
      scale_y_continuous(breaks = 0:3) +
      labs(x = "Średnia ocen", y = "Liczebność") +
      theme_upwr(base_size = 14)
  }))

  # --- Problem 2: Brak zmienności ---
  output$cat2_table <- renderUI({
    dd_data_table(cat_novar,
      types = c("id", "porządkowa", "ciągła", "ciągła", "nominalna"))
  })

  zoom_plot_server("cat2_plot_zadowolenie", reactive({
    ggplot(cat_novar, aes(x = factor(zadowolenie))) +
      geom_bar(fill = data_bad, alpha = 0.85) +
      scale_x_discrete(limits = c("1","2","3","4","5")) +
      labs(x = "Ocena (1–5)", y = "Liczba") +
      theme_upwr(base_size = 13)
  }))

  zoom_plot_server("cat2_plot", reactive({
    ggplot(cat_novar, aes(x = staz, y = wynagrodzenie)) +
      geom_point(size = 3, alpha = 0.6, color = data_bad) +
      scale_x_continuous(limits = c(1, 10)) +
      labs(x = "Staż pracy (lata)", y = "Wynagrodzenie (PLN)") +
      theme_upwr(base_size = 13)
  }))

  # --- Problem 3: Błędy i literówki (przełącznik) ---
  cat3_view <- reactive({
    v <- input$cat3_view
    if (is.null(v)) "raw" else v
  })

  output$cat3_table <- renderUI({
    raw <- cat3_view() == "raw"
    d <- if (raw) cat_errors else cat_errors_clean
    # Podświetlenie podejrzanych wartości tylko w surowych danych.
    errors <- if (raw) list(
      cena = ifelse(d$cena <= 0 | d$cena > 1000000, "is-target", NA),
      powierzchnia = ifelse(d$powierzchnia > 500, "is-target", NA),
      pokoje = ifelse(d$pokoje > 10, "is-target", NA)
    )
    dd_data_table(d,
      types = c("id", "ciągła", "ciągła", "dyskretna", "nominalna"),
      cell_class = errors)
  })

  zoom_plot_server("cat3_plot", reactive({
    if (cat3_view() == "raw") {
      d <- cat_errors
      col <- data_bad
    } else {
      d <- cat_errors_clean
      col <- data_good
    }
    ggplot(d, aes(x = powierzchnia, y = cena)) +
      geom_point(size = 3, alpha = 0.7, color = data_reference) +
      geom_smooth(method = "lm", color = col, se = TRUE) +
      scale_y_continuous(labels = scales::label_number(big.mark = " ")) +
      labs(x = "Powierzchnia (m²)", y = "Cena (PLN)") +
      theme_upwr(base_size = 14)
  }))

  # --- Problem 4: Źle zdefiniowane zmienne (przełącznik) ---
  cat4_view <- reactive({
    v <- input$cat4_view
    if (is.null(v)) "raw" else v
  })

  output$cat4_table <- renderUI({
    if (cat4_view() == "raw") {
      dd_data_table(cat_messy,
        types = c("id", "tekst?!", "tekst?!", "tekst?!"))
    } else {
      dd_data_table(cat_messy_clean,
        types = c("id", "ciągła", "ciągła", "ciągła"))
    }
  })

  zoom_plot_server("cat4_plot", reactive({
    if (cat4_view() == "raw") {
      nums <- suppressWarnings(as.numeric(cat_messy$czas_nauki))
      n_ok <- sum(!is.na(nums))
      n_fail <- sum(is.na(nums))
      df <- data.frame(
        status = c("Rozpoznane\njako liczba", "Nie da się\nprzeczytać"),
        n = c(n_ok, n_fail)
      )
      ggplot(df, aes(x = status, y = n, fill = status)) +
        geom_col(width = 0.6) +
        scale_fill_manual(values = c(data_good, data_bad)) +
        geom_text(aes(label = n), vjust = -0.5, size = 6, fontface = "bold") +
        labs(x = NULL, y = "Liczba odpowiedzi") +
        theme_upwr(base_size = 14) +
        theme(legend.position = "none") +
        ylim(0, max(df$n) + 1)
    } else {
      d <- cat_messy_clean[!is.na(cat_messy_clean$czas_nauki_h), ]
      ggplot(d, aes(x = czas_nauki_h)) +
        geom_histogram(bins = 5, fill = data_good, color = "white", alpha = 0.8) +
        labs(
             
             x = "Godziny nauki/tydzień", y = "Liczebność") +
        theme_upwr(base_size = 14)
    }
  }))

  # --- Problem 5: Braki danych ---
  output$cat5_table <- renderUI({
    dd_data_table(cat_missing,
      types = c("id", "ciągła", "porządkowa", "ciągła", "nominalna"))
  })

  zoom_plot_server("cat5_plot", reactive({
    miss_pct <- sapply(cat_missing[, -1], function(x) mean(is.na(x)) * 100)
    df_miss <- data.frame(variable = names(miss_pct), pct = miss_pct)

    ggplot(df_miss, aes(x = reorder(variable, -pct), y = pct)) +
      geom_col(width = 0.6, fill = data_bad) +
      geom_text(aes(label = paste0(round(pct), "%")), vjust = -0.5, size = 5, fontface = "bold") +
      labs(x = NULL, y = "% braków (NA)") +
      theme_upwr(base_size = 14) +
      ylim(0, 35)
  }))

  # --- Problem 6: Brak niezależności (przełącznik) ---
  cat6_view <- reactive({
    v <- input$cat6_view
    if (is.null(v)) "daily" else v
  })

  output$cat6_table <- renderUI({
    if (cat6_view() == "daily") {
      df_show <- cat_timeseries
      df_show$data <- format(df_show$data, "%Y-%m-%d")
      dd_data_table(df_show, page_size = 10, page = input$cat6_table_page, page_input = "cat6_table_page",
        types = c("data", "nominalna", "ciągła"))
    } else {
      dd_data_table(cat_timeseries_monthly,
        types = c("nominalna", "ciągła", "dyskretna"))
    }
  })

  zoom_plot_server("cat6_plot", reactive({
    if (cat6_view() == "daily") {
      ggplot(cat_timeseries, aes(x = data, y = temperatura)) +
        geom_line(color = data_bad, linewidth = 0.8) +
        geom_point(color = data_bad, size = 1.2, alpha = 0.6) +
        labs(
             
             x = "Data", y = "Temperatura (°C)") +
        theme_upwr(base_size = 14)
    } else {
      df_m <- cat_timeseries_monthly
      df_m$idx <- seq_len(nrow(df_m))
      ggplot(df_m, aes(x = idx, y = srednia_temp)) +
        geom_line(color = data_bad, linewidth = 1) +
        geom_point(color = data_bad, size = 4) +
        geom_text(aes(label = paste0(srednia_temp, "°C")),
                  vjust = -1.2, size = 4.5, fontface = "bold") +
        scale_x_continuous(breaks = df_m$idx, labels = df_m$miesiac) +
        labs(
             
             x = NULL, y = "Średnia temperatura (°C)") +
        theme_upwr(base_size = 14) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
    }
  }))

  # --- Problem 7: Zła struktura (przełącznik) ---
  cat7_view <- reactive({
    v <- input$cat7_view
    if (is.null(v)) "events" else v
  })

  output$cat7_table <- renderUI({
    if (cat7_view() == "events") {
      df_show <- cat_patients_visits
      df_show$data_wizyty <- format(df_show$data_wizyty, "%Y-%m-%d")
      dd_data_table(df_show, page_size = 10, page = input$cat7_table_page, page_input = "cat7_table_page",
        types = c("id", "nominalna", "data", "ciągła"))
    } else {
      dd_data_table(cat_patients_agg, page_size = 10, page = input$cat7_table_page, page_input = "cat7_table_page",
        types = c("id", "nominalna", "ciągła", "dyskretna"))
    }
  })

  zoom_plot_server("cat7_plot", reactive({
    if (cat7_view() == "events") {
      df <- data.frame(label = "Wiersze\nw tabeli", n = nrow(cat_patients_visits))
      ggplot(df, aes(x = label, y = n)) +
        geom_col(fill = data_mixed, width = 0.4) +
        geom_text(aes(label = paste0("n = ", n)), vjust = -0.5, size = 7, fontface = "bold") +
        labs(x = NULL, y = NULL) +
        ylim(0, ceiling(nrow(cat_patients_visits) * 1.15)) +
        theme_upwr(base_size = 14) +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    } else {
      df <- data.frame(label = "Pacjenci\n(obserwacje)", n = nrow(cat_patients_agg))
      ggplot(df, aes(x = label, y = n)) +
        geom_col(fill = data_bad, width = 0.4) +
        geom_text(aes(label = paste0("n = ", n)), vjust = -0.5, size = 7, fontface = "bold",
                  color = data_bad) +
        labs(x = NULL, y = NULL) +
        ylim(0, 40) +
        theme_upwr(base_size = 14) +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    }
  }))
}
