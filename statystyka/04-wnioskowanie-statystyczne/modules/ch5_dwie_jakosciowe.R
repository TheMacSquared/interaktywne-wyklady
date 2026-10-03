# ============================================================================
# CHAPTER 7: Dwie zmienne jakościowe (chi-kwadrat, Fisher)
# ============================================================================

ch5_ui <- list(
  id = "ch-dwie-jakosciowe", num = "07", title = "Test χ² niezależności",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 07 · Testowanie hipotez",
      num    = "07",
      title  = "Test χ² niezależności.",
      lead   = "Płeć a kierunek studiów, rodzaj opakowania a pleśń, środki ochrony
                a ciężkość urazu: gdy obie zmienne są jakościowe, ich związek
                zapisujemy w tabeli liczebności. Test χ² porównuje tę tabelę z tabelą,
                jaką dałaby niezależność, i rozstrzyga, czy różnica jest większa
                niż losowa."
    ),

    lc_p("W poprzednim rozdziale obie zmienne były ilościowe, a związek mierzyliśmy
      współczynnikiem korelacji. Dla zmiennych jakościowych ta droga jest zamknięta:
      kierunków studiów nie da się dodawać ani mnożyć, nie ma więc średnich ani
      wykresu rozrzutu. Pozostaje policzyć, ile obserwacji wpada do każdej
      kombinacji kategorii, i sprawdzić, czy te liczebności układają się tak,
      jak przy braku związku."),

    # ========================================================================
    # Wprowadzenie
    # ========================================================================
    lc_h2("ch5-intro", "Tabela kontyngencji i test χ²"),

    lc_p("Taką tabelę znamy z wykładu 01. ",
      gloss("tabela kontyngencji", "Tabela kontyngencji"), " (krzyżowa) płci
      i kierunku studiów w ankiecie 200 studentów pokazała, że rozkłady kierunków
      u kobiet i mężczyzn są podobne, choć nie identyczne: Informatykę studiuje
      30.3% kobiet i 29.7% mężczyzn, Psychologię 18.3% kobiet i 22.0% mężczyzn.
      Wtedy wystarczyło stwierdzić, że w tej próbie wybór kierunku niewiele zależy
      od płci. Teraz pytamy o populację: czy takie różnice mogły powstać przez
      przypadek przy losowaniu próby, czy świadczą o rzeczywistym związku."),

    lc_p("Dwie ", gloss("zmienna jakościowa", "zmienne jakościowe"), " są niezależne,
      jeśli rozkład jednej jest taki sam w każdej kategorii drugiej. W języku
      tabeli oznacza to, że procenty wierszowe są w populacji identyczne we
      wszystkich wierszach. W próbie nigdy nie wyjdą dokładnie równe, bo każda
      próba różni się od populacji losowo. ",
      gloss("test chi-kwadrat", "Test χ²"), " niezależności sprawdza, czy
      obserwowane różnice są większe od tych, które zwykle daje sam przypadek.
      Hipotezy mają zawsze tę samą postać:"),

    lc_formula_box(
      p(withMathJax("\\(H_0:\\)"), " zmienne są niezależne"),
      p(withMathJax("\\(H_a:\\)"), " zmienne są powiązane")
    ),

    lc_p("Punktem odniesienia jest tabela, jakiej spodziewalibyśmy się przy
      prawdziwej H₀. Jeśli zmienne są niezależne, odsetek obserwacji w danej
      kolumnie powinien być w każdym wierszu taki sam jak w całej próbie. ",
      gloss("liczebność oczekiwana", "Liczebność oczekiwana"), " komórki to więc
      liczba obserwacji w jej wierszu pomnożona przez odsetek jej kolumny w całej
      próbie. Po uproszczeniu:"),

    lc_formula_box(withMathJax(
      "$$E_{ij} = \\frac{n_{i\\cdot} \\cdot n_{\\cdot j}}{n}$$"
    )),

    lc_p("gdzie \\(n_{i\\cdot}\\) to suma i-tego wiersza, \\(n_{\\cdot j}\\) suma
      j-tej kolumny, a n liczba wszystkich obserwacji. Statystyka testowa zbiera
      rozbieżności między liczebnościami obserwowanymi \\(O_{ij}\\) a oczekiwanymi
      \\(E_{ij}\\) ze wszystkich komórek tabeli. Różnice podnosimy do kwadratu,
      żeby nadwyżki i niedobory się nie znosiły, i dzielimy przez \\(E_{ij}\\),
      bo odchylenie o 10 znaczy więcej w komórce, w której spodziewamy się
      20 obserwacji, niż w tej, w której spodziewamy się 200:"),

    lc_formula_box(withMathJax(
      "$$\\chi^2 = \\sum_{i,j} \\frac{(O_{ij} - E_{ij})^2}{E_{ij}}, \\qquad df = (r - 1)(c - 1)$$"
    )),

    lc_p("Gdy H₀ jest prawdziwa, a próba dostatecznie duża, statystyka ta ma
      w przybliżeniu ", gloss("rozkład chi-kwadrat", "rozkład χ²"), " znany
      z wykładu 02 (rozdz. 4), o df = (r - 1)(c - 1) ",
      gloss("stopnie swobody", "stopniach swobody"), ", gdzie r i c to liczby
      wierszy i kolumn. Tyle komórek tabeli można wypełnić dowolnie, zanim sumy
      wierszy i kolumn wyznaczą resztę. Pełna niezależność w próbie dałaby χ² = 0,
      a każde odstępstwo od niej, w dowolną stronę, powiększa χ². Dlatego H₀
      odrzucamy tylko przy dużych wartościach statystyki, a p-wartość to pole pod
      krzywą rozkładu χ² na prawo od obliczonej wartości."),

    # ========================================================================
    # WIDGET 0: Budowanie intuicji — co to znaczy niezależność?
    # ========================================================================
    lc_h2("ch5-intuicja", "Budowanie intuicji: co to znaczy „niezależność”?"),

    lc_p("Zanim zastosujemy wzory do większych tabel, prześledźmy je na
      najprostszej tabeli 2 × 2. Mamy dane z 200 kontroli drogowych i pytamy,
      czy szansa dostania mandatu jest niezależna od płci. Panel pokazuje
      w trzech krokach tabelę obserwowaną, tabelę oczekiwaną przy założeniu
      niezależności i porównanie obu tabel."),

    figure_panel(
      label = "Ryc. 7.1",
      title = "Przykład: czy płeć wpływa na dostawanie mandatów?",

      lc_step_widget("ch5_narr",
        steps = c("Pokaż dane", "Załóżmy niezależność", "Porównaj"),
        body = uiOutput("ch5_narr_body")
      )
    ),

    lc_p("Krok drugi to sedno testu. Tabela oczekiwana nie pochodzi z danych
      o poszczególnych płciach, tylko z łącznego odsetka mandatów w całej próbie:
      tak wyglądałaby tabela, gdyby płeć nie miała znaczenia. Krok trzeci mierzy,
      jak daleko dane od niej odeszły. W tabeli 2 × 2 przy ustalonych sumach
      wierszy i kolumn wszystkie cztery komórki odbiegają od oczekiwań o tę samą
      liczbę obserwacji, dlatego wystarcza jeden stopień swobody. Te same
      odchylenia ważą jednak różnie: komórki z mniejszą liczebnością oczekiwaną,
      tu komórki z mandatem, wnoszą do χ² więcej."),

    lc_p("Przy poziomie istotności α = 0.05, ustalonym jak zwykle przed
      spojrzeniem na dane, wartość krytyczna rozkładu χ² z jednym stopniem
      swobody wynosi 3.84. Obliczone 8.33 leży daleko za nią, a p-wartość
      wynosi 0.004. Gdyby płeć nie miała związku
      z mandatami, rozbieżność co najmniej tak duża jak w tych danych zdarzałaby
      się mniej więcej w 4 próbach na 1000. Odrzucamy H₀. Test nie mówi natomiast,
      skąd ten związek się bierze. To dane obserwacyjne, więc nie wiemy, czy chodzi
      o płeć, czy na przykład o to, że mężczyźni więcej jeżdżą."),

    # ========================================================================
    # Ćwiczenie: sformułuj hipotezy
    # ========================================================================
    lc_h2("ch5-cwiczenie", "Ćwiczenie: sformułuj hipotezy"),

    lc_p("Pierwszym krokiem każdego testu jest zapis hipotez. Przy każdym
      z trzech pytań poniżej nazwij najpierw obie zmienne jakościowe i ich
      kategorie, potem zapisz H₀ i Hₐ, a dopiero na końcu porównaj swój zapis
      z odpowiedzią."),

    hypothesis_practice("ch5", list(
      list(
        question = "Czy wybór kierunku studiów zależy od płci?",
        h0 = "\\(H_0:\\) kierunek i płeć są niezależne",
        ha = "\\(H_a:\\) kierunek i płeć są powiązane",
        note = "Test χ² niezależności zawsze zestawia niezależność ze związkiem i nie mówi nic o kierunku zależności."
      ),
      list(
        question = "Czy typ opakowania (szkło / plastik / karton) ma związek
                    z występowaniem pleśni w sokach?",
        h0 = "\\(H_0:\\) ryzyko pojawienia się pleśni jest takie samo dla wszystkich rodzajów opakowań",
        ha = "\\(H_a:\\) dla przynajmniej jednego rodzaju opakowania ryzyko pleśni jest inne",
        note = "Choć merytorycznie spodziewamy się kierunku (niektóre opakowania pleśnieją częściej), Hₐ w teście χ² nie ma kierunku: statystyka rośnie przy każdym odstępstwie od niezależności."
      ),
      list(
        question = "Czy preferencje konsumentów (lubi / nie lubi) zależą od regionu
                    Polski (płd. / pn. / centr. / wsch. / zach.)?",
        h0 = "\\(H_0:\\) rozkład preferencji jest taki sam we wszystkich regionach",
        ha = "\\(H_a:\\) rozkład preferencji różni się między przynajmniej dwoma regionami",
        note = "Tabela 2 × 5. Test χ² działa dla tabeli kontyngencji o dowolnych wymiarach."
      )
    )),

    lc_p("Pytania różnią się liczbą kategorii, a więc wymiarem tabeli. Wymiar
      zmienia liczbę stopni swobody, ale nie sposób liczenia statystyki χ².
      Ten rachunek przejdziemy teraz krok po kroku."),

    # ========================================================================
    # WIDGET 1: Chi-kwadrat krokowy
    # ========================================================================
    lc_h2("ch5-krok", "Test χ² niezależności — krok po kroku"),

    lc_p("Panel losuje próbę o zadanej wielkości z jednego z trzech scenariuszy.
      Każda tabela ma sześć komórek (3 × 2 albo 2 × 3), więc df = 2. Kolejne
      kroki prowadzą od liczebności obserwowanych przez procenty wierszowe,
      znane z wykładu 01, i tabelę oczekiwaną do p-wartości i decyzji. Procenty
      wierszowe są tu właściwym wyborem, bo pytamy, czy rozkład drugiej zmiennej
      jest taki sam w każdej kategorii pierwszej."),

    figure_panel(
      label = "Ryc. 7.2",
      title = "Test χ² niezależności — krok po kroku",
      uiOutput("ch5_hypothesis_panel"),
      lc_step_widget("ch5_test",
        steps = c("Tabela obserwowana", "Procenty w wierszach",
                  "Oczekiwane i χ²", "p-wartość i decyzja"),
        toolbar = lc_toolbar(
          selectInput("ch5_scenario", "Scenariusz",
            choices = c(
              "Opakowanie a pleśń (TŻ)" = "packaging",
              "Typ gleby a kategoria plonu (R)" = "soil",
              "Środki ochrony a uraz (IB)" = "ppe_accident"
            ),
            selected = "packaging"
          ),
          lc_slider("ch5_n", "Wielkość próby (n)", 50, 300, 120, 10),
          lc_action("ch5_new_sample", "Losuj próbę", icon = "shuffle", variant = "solid")
        ),
        # Tabela kroku nad wykresem; kolory kategorii w nagłówkach zastępują legendę.
        above = uiOutput("ch5_test_table"),
        plot_id = "ch5_step_plot"
      )
    ),

    lc_p("We wszystkich scenariuszach populacja jest ustawiona tak, że zmienne
      są powiązane, czyli H₀ jest fałszywa. W domyślnym scenariuszu pleśń pojawia
      się w 5% opakowań szklanych, 12% plastikowych i 20% kartonowych. Mimo to
      przy n = 120 test nie zawsze odrzuca H₀: w symulacji 2000 prób zrobił to
      tylko w około 44% losowań, a przy n = 300 w około 85%. To ",
      gloss("moc testu"), " z rozdziału 03 w działaniu. Przy słabym związku
      i małej próbie wynik „brak podstaw do odrzucenia H₀” jest częsty i nie
      oznacza, że zmienne są niezależne. W scenariuszach gleby i środków ochrony
      różnice między wierszami są większe, więc już przy n = 120 test wykrywa
      związek w ponad 90% losowań."),

    lc_p("Wróćmy do ankiety z wykładu 01. Tabela płci i czterech kierunków ma
      2 × 4 komórki, więc df = 3. Liczebności obserwowane leżą bardzo blisko
      oczekiwanych, na przykład Informatykę studiują 33 kobiety, a przy
      niezależności oczekiwalibyśmy 32.7. Statystyka wynosi χ² = 0.47, daleko
      poniżej wartości krytycznej 7.81, a p-wartość 0.92. Nie ma podstaw do
      odrzucenia H₀. To nie dowodzi, że płeć i kierunek są niezależne, tylko
      że dane nie przemawiają przeciw niezależności."),

    lc_p("Dla tabel 2 × 2 część programów domyślnie stosuje poprawkę Yatesa
      na ciągłość, która nieco zmniejsza statystykę: dla danych o mandatach
      daje χ² = 7.52 i p = 0.006 zamiast 8.33 i 0.004 ze wzoru. Porównując
      wynik z obliczeniem ręcznym, sprawdź więc, czy poprawka została
      zastosowana. Sam test mówi tylko,
      czy związek istnieje. W którą stronę przebiega, pokazują procenty wierszowe,
      a jak jest silny, mierzy ", gloss("V Cramera"), " omówione w rozdziale 10."),

    # ========================================================================
    # WIDGET 2: Chi-kwadrat vs Fisher (porównanie)
    # ========================================================================
    lc_h2("ch5-fisher", "Test χ² a test Fishera"),

    lc_p("Test χ² ma założenia, które dokładnie omówimy w wykładzie 05.
      Obserwacje muszą być niezależne, czyli każda osoba trafia do tabeli tylko
      raz, a w komórkach stoją liczebności, nie procenty. Trzecie założenie
      dotyczy wielkości próby. Rozkład χ² jest tylko przybliżeniem rozkładu
      statystyki i sprawdza się, gdy ",
      gloss("liczebność oczekiwana", "liczebności oczekiwane"), " nie są zbyt małe.
      Często podawana orientacyjna reguła wymaga co najmniej 5 obserwacji
      oczekiwanych w każdej komórce. Nie jest to ostra granica, tylko sygnał,
      że wynik warto sprawdzić inną metodą."),

    lc_p("Taką metodą jest ", gloss("test dokładny Fishera"), ". Zamiast korzystać
      z przybliżenia, rozważa wszystkie tabele o tych samych sumach wierszy
      i kolumn co obserwowana. Dla każdej liczy dokładne prawdopodobieństwo
      przy H₀, a p-wartość to suma prawdopodobieństw tych tabel, które są
      nie bardziej prawdopodobne niż obserwowana. To ta sama idea co w teście
      dwumianowym z rozdziału 05, który liczył p-wartość wprost z rozkładu, bez
      przybliżenia normalnego. Panel stosuje oba testy do próby wylosowanej
      w panelu powyżej."),

    figure_panel(
      label = "Ryc. 7.3",
      title = "Porównanie: χ² vs Fisher",
      lc_toolbar(lc_action("ch5_compare", "Porównaj χ² i Fishera (na tych samych danych)", variant = "solid")),
      uiOutput("ch5_compare_result")
    ),

    lc_p("W domyślnym scenariuszu przy n = 120 na każdy rodzaj opakowania
      przypada średnio 40 prób, a pleśń pojawia się łącznie w około 12% z nich.
      Oczekiwana liczba spleśniałych opakowań w wierszu wynosi więc około 4.9
      i ostrzeżenie o małych liczebnościach oczekiwanych pojawia się często,
      w symulacji w trzech losowaniach na cztery. Mimo to oba testy prowadzą
      zwykle do tej samej decyzji (w symulacji w 96% prób), a ich p-wartości
      różnią się typowo o około 0.01. Przy n = 50 różnice są kilkakrotnie
      większe, a przy n = 300 praktycznie znikają."),

    lc_p("Najważniejsze różnice między testami zbiera tabela:"),

    lc_table(
      data.frame(
        c1 = c("Metoda", "Warunek", "Duże n", "Małe n"),
        c2 = c("Przybliżony (rozkład χ²)", "Liczebności oczekiwane niezbyt małe (orientacyjnie ≥ 5)", "Szybki, praktycznie identyczny wynik", "Może być niedokładny"),
        c3 = c("Dokładny (kombinatoryka)", "Działa zawsze", "Działa, ale wolniejszy", "Bezpieczny wybór")
      ),
      cols = list(
        lc_col("c1", "", "row"),
        lc_col("c2", "Test χ²", "text"),
        lc_col("c3", "Test Fishera", "text")
      ),
      cell_class = list(c2 = c(NA, NA, "is-best", "is-base"), c3 = c(NA, NA, NA, "is-best")),
      narrow = "cards",
      prose = TRUE
    ),

    lc_p("Oba testy stosuje się do tej samej tabeli. Przy dużych próbach dają
      praktycznie ten sam wynik, więc wybór nie ma znaczenia. Przy małych próbach albo rzadkich kategoriach bezpieczniej
      oprzeć decyzję na teście Fishera."),

    # ========================================================================
    # Ćwiczenia CASchools
    # ========================================================================
    lc_h2("ch5-cas", "Ćwiczenia", "CASchools — test χ² niezależności"),

    lc_p("Na koniec dwa zadania na danych o szkołach w Kalifornii. W obu
      przynajmniej jedna zmienna jest ilościowa, więc przed testem trzeba ją
      podzielić na dwie kategorie według podanego progu. Powstaje tabela 2 × 2
      i test χ² z jednym stopniem swobody."),

    lc_note("Dane",
      p("420 okręgów szkolnych Kalifornii (1998–1999). Plik: ",
        tags$code("dane/caschools.csv"), "."),
      p("Zmienne w zadaniach: ", tags$code("grades"),
        " (typ szkoły: KK-06/KK-08), ",
        tags$code("english"), " (% uczniów ELL), ",
        tags$code("student_teacher_ratio"), " (STR), ",
        tags$code("lunch"), " (% uczniów z dotacją — wskaźnik ubóstwa).")
    ),

    figure_panel(label = "Ćwiczenie",
      h4("Zadanie 8 — Czy typ szkoły wiąże się z wysokim odsetkiem uczniów ELL?"),
      p("Utwórz zmienną binarną: ",
        tags$code("high_english = (english > 20)"),
        ". Zbuduj tabelę krzyżową ", tags$code("grades"), " × ",
        tags$code("high_english"),
        " i wykonaj test χ² niezależności.
        Zapisz: χ², df, p. Co wynika? Czy typ szkoły jest niezależny
        od odsetka uczniów uczących się angielskiego?"),
      lc_more("Rozwiązanie", uiOutput("cas_ch5_sol8"))
    ),

    figure_panel(label = "Ćwiczenie",
      h4("Zadanie 9 — Czy przeładowane klasy idą w parze z ubóstwem?"),
      p("Utwórz dwie zmienne binarne: ",
        tags$code("high_str = (student_teacher_ratio > 20)"),
        " i ", tags$code("high_lunch = (lunch > 50)"),
        ". Wykonaj test χ² niezależności. Czy STR i ubóstwo są ze sobą powiązane?
        Co sugeruje wynik dla interpretacji zadania 5 z korelacji?"),
      lc_more("Rozwiązanie", uiOutput("cas_ch5_sol9"))
    ),

    lc_p("Porównując rozwiązania, zwróć uwagę na cenę podziału zmiennej ilościowej
      na dwie kategorie. Próg jest arbitralny, a obserwacje leżące tuż pod nim
      i tuż nad nim trafiają do różnych klas, choć prawie się nie różnią. Gdy obie
      zmienne są z natury ilościowe, test korelacji z rozdziału 06 zwykle lepiej
      wykorzystuje informację zawartą w danych."),

    lc_chapter_next(
      num       = "08",
      title     = "Test t dwóch grup",
      lead      = "porównanie średnich między dwiema grupami — czy różnica jest realna?",
      target_id = "ch-dwie-grupy"
    )
  )
)

# ============================================================================
# DANE — CASchools (wczytane raz przy ladowaniu modulu)
# ============================================================================

.ch5_cas <- read.csv(file.path(app_dir, "dane", "caschools.csv"),
                     stringsAsFactors = FALSE)

# ============================================================================
# SERVER
# ============================================================================

ch5_server <- function(input, output, session) {

  # --- Parametry scenariuszy ---
  scenario_params <- list(
    packaging = list(
      lab1 = "Opakowanie", lab2 = "Pleśń",
      cats1 = c("Szkło", "Plastik", "Karton"),
      cats2 = c("Tak", "Nie"),
      probs = matrix(c(0.05, 0.95, 0.12, 0.88, 0.20, 0.80), nrow = 3, byrow = TRUE),
      question = "Czy typ opakowania wpływa na występowanie pleśni?",
      h0_text = "\\(H_0:\\) typ opakowania i występowanie pleśni są niezależne",
      h1_text = "\\(H_a:\\) typ opakowania i występowanie pleśni są powiązane"),
    atmosphere = list(
      lab1 = "Atmosfera pakowania", lab2 = "Ocena świeżości po 7 dniach",
      cats1 = c("Powietrze", "MAP (modyfikowana)", "Próżnia"),
      cats2 = c("Świeże", "Średniej jakości", "Zepsute"),
      probs = matrix(c(0.15, 0.40, 0.45,
                        0.55, 0.35, 0.10,
                        0.70, 0.25, 0.05), nrow = 3, byrow = TRUE),
      question = "Czy atmosfera pakowania wpływa na świeżość mięsa po 7 dniach?",
      h0_text = "\\(H_0:\\) atmosfera pakowania i ocena świeżości są niezależne",
      h1_text = "\\(H_a:\\) atmosfera pakowania i ocena świeżości są powiązane"),
    soil = list(
      lab1 = "Typ gleby", lab2 = "Plon",
      cats1 = c("Piaszczysta", "Gliniasta", "Czarnoziemna"),
      cats2 = c("Niski", "Wysoki"),
      probs = matrix(c(0.65, 0.35, 0.45, 0.55, 0.25, 0.75), nrow = 3, byrow = TRUE),
      question = "Czy typ gleby wpływa na kategorię plonu?",
      h0_text = "\\(H_0:\\) typ gleby i kategoria plonu są niezależne",
      h1_text = "\\(H_a:\\) typ gleby i kategoria plonu są powiązane"),
    pasteurization = list(
      lab1 = "Metoda pasteryzacji", lab2 = "Liczba bakterii po 7 dniach",
      cats1 = c("Niska (63°C, 30 min)", "Wysoka (72°C, 15 s)", "UHT (135°C, 2 s)"),
      cats2 = c("Niska (< norma)", "Średnia", "Wysoka (> norma)"),
      probs = matrix(c(0.35, 0.40, 0.25,
                        0.60, 0.30, 0.10,
                        0.90, 0.08, 0.02), nrow = 3, byrow = TRUE),
      question = "Czy metoda pasteryzacji mleka wpływa na liczebność bakterii po 7 dniach?",
      h0_text = "\\(H_0:\\) metoda pasteryzacji i liczebność bakterii są niezależne",
      h1_text = "\\(H_a:\\) metoda pasteryzacji i liczebność bakterii są powiązane"),
    ppe_accident = list(
      lab1 = "Środki ochrony (SOI)", lab2 = "Ciężkość wypadku",
      cats1 = c("Niepełne", "Pełne"),
      cats2 = c("Brak urazu", "Uraz lekki", "Uraz ciężki"),
      probs = matrix(c(0.45, 0.35, 0.20,
                        0.80, 0.17, 0.03), nrow = 2, byrow = TRUE),
      question = "Czy stosowanie pełnych środków ochrony indywidualnej wpływa na ciężkość wypadku przy pracy?",
      h0_text = "\\(H_0:\\) stosowanie SOI i ciężkość wypadku są niezależne",
      h1_text = "\\(H_a:\\) stosowanie SOI i ciężkość wypadku są powiązane")
  )

  # --- Wspoldzielone dane ---
  # Tabela jest losowana dla konkretnego scenariusza i n. Po zmianie tych
  # inputow wymaga ponownego losowania, zamiast udawac aktualne dane.
  ch5_tab_state <- reactiveVal(NULL)
  ch5_tab <- reactive({
    state <- ch5_tab_state()
    if (is.null(state)) return(NULL)
    req(input$ch5_scenario, input$ch5_n)

    if (!identical(state$scenario, input$ch5_scenario) ||
        !isTRUE(state$n == input$ch5_n)) {
      return(NULL)
    }

    state$tab
  })

  # Krok widgetu (1..4) żyje w przeglądarce; nowa próba ani zmiana
  # scenariusza nie cofa kroku.
  ch5_step <- lc_step_server("ch5_test", input)$step

  observeEvent(input$ch5_new_sample, {
    req(input$ch5_scenario, input$ch5_n)
    par <- scenario_params[[input$ch5_scenario]]
    req(!is.null(par))
    n <- input$ch5_n
    n_per_cat1 <- rmultinom(1, n, rep(1, length(par$cats1)))

    rows <- list()
    for (i in seq_along(par$cats1)) {
      cats2_draws <- sample(par$cats2, n_per_cat1[i], replace = TRUE, prob = par$probs[i, ])
      rows[[i]] <- data.frame(var1 = par$cats1[i], var2 = cats2_draws)
    }
    df <- do.call(rbind, rows)
    df$var1 <- factor(df$var1, levels = par$cats1)
    df$var2 <- factor(df$var2, levels = par$cats2)

    ch5_tab_state(list(
      scenario = input$ch5_scenario,
      n = n,
      tab = table(df$var1, df$var2)
    ))
  }, ignoreInit = TRUE)

  # --- Widget 0: Narracja niezaleznosci (mandaty) ---
  # Stale dane do narracji (nie losowane)
  narr_tab <- matrix(c(30, 70, 50, 50), nrow = 2, byrow = TRUE,
    dimnames = list(c("Kobiety", "Mężczyźni"),
                    c("Mandat", "Brak mandatu")))
  narr_exp <- matrix(c(40, 60, 40, 60), nrow = 2, byrow = TRUE,
    dimnames = dimnames(narr_tab))
  ch5_narr_step <- lc_step_server("ch5_narr", input)$step

  # Tabele kroku: 1 obserwowana, 2 obserwowana | oczekiwana, 3 różnice.
  output$ch5_narr_body <- renderUI({
    obs <- tags$div(
      tags$p(class = "lc-tbl-lead", tags$b("Obserwowane")),
      lc_crosstab(narr_tab, measure = "n", lead = FALSE, label = "Dane z 200 kontroli")
    )
    switch(as.character(ch5_narr_step()),
      "1" = obs,
      "2" = tags$div(class = "lc-tbl-row",
        obs,
        tags$div(
          tags$p(class = "lc-tbl-lead", tags$b("Oczekiwane przy H₀")),
          lc_crosstab(narr_exp, measure = "n", lead = FALSE, label = "Tabela oczekiwana")
        )
      ),
      "3" = lc_table(
        data.frame(group = rownames(narr_tab), obs = narr_tab[, "Mandat"],
                   exp = narr_exp[, "Mandat"],
                   diff = narr_tab[, "Mandat"] - narr_exp[, "Mandat"]),
        cols = list(
          lc_col("group", "", "row"),
          lc_col("obs", "Mandat (obs.)"),
          lc_col("exp", "Mandat (oczek.)"),
          lc_col("diff", "Różnica")
        )
      )
    )
  })

  output$ch5_narr_text <- renderUI({
    switch(as.character(ch5_narr_step()),
      "1" = tagList("Mandat: ", step_num("30"), " ze 100 kobiet i ", step_num("50"),
        " ze 100 mężczyzn; łącznie 80 z 200, czyli 40%."),
      "2" = tagList("Przy niezależności 40% dotyczy obu płci: oczekujemy po ",
        step_num("40"), " mandatów i po ", step_num("60"),
        " kontroli bez mandatu w każdej grupie."),
      "3" = tagList("Każda komórka odbiega o 10: χ² = 10²/40 + 10²/60 + 10²/40 + 10²/60 = ",
        step_num("8.33"), " (df = 1).")
    )
  })

  # --- Panel hipotezy ---
  output$ch5_hypothesis_panel <- renderUI({
    par <- scenario_params[[input$ch5_scenario]]
    tab <- ch5_tab()
    tagList(
      lc_status(
        p(tags$b("Pytanie potoczne:")),
        p(tags$em(paste0("„", par$question, "”")))
      ),
      lc_formula_box(
        p(tags$b("Hipoteza formalna:")),
        p(withMathJax(par$h0_text)),
        p(withMathJax(par$h1_text))
      ),
      if (is.null(tab)) {
        lc_empty("Kliknij „Losuj próbę”")
      }
    )
  })

  # Kolory kategorii zmiennej w kolumnach (kroki 1–2): dane, grupa, trzecia.
  ch5_cat_colours <- function(n_cat) {
    c(STEP_ROLES$data$colour, STEP_ROLES$group$colour,
      unname(upwr_cat["szalwia"]))[seq_len(n_cat)]
  }

  # --- Krokowy wykres ---
  zoom_plot_server("ch5_step_plot", reactive({
    tab <- ch5_tab()
    step <- ch5_step()
    par <- scenario_params[[input$ch5_scenario]]

    if (is.null(tab)) return(NULL)

    if (step <= 2) {
      df <- as.data.frame(tab)
      names(df) <- c("Var1", "Var2", "Freq")
      df <- df %>%
        group_by(Var1) %>%
        mutate(pct = round(Freq / sum(Freq) * 100, 1)) %>%
        ungroup()
      # Krok 1: liczności; krok 2: procenty w obrębie wiersza.
      df$value <- if (step == 1) df$Freq else df$pct
      df$label <- if (step == 1) df$Freq else paste0(df$pct, "%")
      y_top <- if (step == 1) max(df$Freq) * 1.15 else 110

      # Wypełnienie z kategorii (aes), więc krawędź wyniku podana wprost:
      # step_result() ustawia stałe wypełnienie.
      ggplot(df, aes(x = Var1, y = value, fill = Var2)) +
        geom_col(position = position_dodge(width = 0.9), width = 0.85,
                 alpha = STEP_ROLES$data$alpha, colour = STEP_EDGE$colour,
                 linewidth = STEP_EDGE$linewidth) +
        geom_text(aes(label = label), position = position_dodge(width = 0.9),
                  vjust = -0.3, size = 4, family = lc_mono_family,
                  colour = STEP_ROLES$known$colour) +
        scale_fill_manual(values = ch5_cat_colours(ncol(tab))) +
        labs(x = par$lab1, y = if (step == 1) "Liczebność" else "Procent") +
        step_frame(xlim = c(0.4, nrow(tab) + 0.6), ylim = c(0, y_top))
    } else {
      # Krok 3: statystyka χ²; krok 4: obszar odrzucenia i decyzja
      test <- chisq.test(tab)
      step_null_plot(as.numeric(test$statistic), df = as.numeric(test$parameter),
                     type = "chisq", phase = if (step == 3) "stat" else "decision")
    }
  }))

  # --- Opis kroku ---
  output$ch5_test_text <- renderUI({
    tab <- ch5_tab()
    step <- ch5_step()

    if (is.null(tab)) return(NULL)

    test <- chisq.test(tab)
    chi_stat <- as.numeric(test$statistic)
    df_val <- as.numeric(test$parameter)

    switch(as.character(step),
      "1" = "To liczebności obserwowane. Grupy mogą mieć różne rozmiary,
        więc same liczby trudno porównać.",
      "2" = tagList(
        "Przy niezależności procenty w populacji byłyby takie same w każdym wierszu;
        w próbie różnią się także przez przypadek."
      ),
      "3" = tagList(
        "χ² = ", step_num(lc_fmt(chi_stat, 3)), paste0(" (df = ", df_val, ") — "),
        "łączna rozbieżność między tabelą obserwowaną a oczekiwaną.",
        if (any(test$expected < 5)) tagList(" ",
          lc_verdict("Uwaga: niektóre liczebności oczekiwane są mniejsze od 5", type = "danger"))
      ),
      "4" = tagList(
        paste0("Wynik testu χ² niezależności: χ²(", df_val, ") = "),
        step_num(lc_fmt(chi_stat, 3)), ". ", step_verdict(test$p.value)
      )
    )
  })

  # --- Tabele kroków pod wykresem ---
  output$ch5_test_table <- renderUI({
    tab <- ch5_tab()
    step <- ch5_step()
    par <- scenario_params[[input$ch5_scenario]]

    if (is.null(tab) || step == 4) return(NULL)

    tab <- as.matrix(unclass(tab))
    switch(as.character(step),
      "1" = lc_crosstab(tab, measure = "n", row_name = par$lab1, lead = FALSE,
                        col_name = par$lab2, col_colours = ch5_cat_colours(ncol(tab)),
                        label = paste0("Tabela krzyżowa: ", par$lab1, " × ", par$lab2)),
      "2" = lc_crosstab(tab, measure = "row", row_name = par$lab1,
                        col_name = par$lab2, col_colours = ch5_cat_colours(ncol(tab)),
                        label = "Procenty w każdej grupie (wierszu)"),
      "3" = {
        expected <- chisq.test(tab)$expected
        keys <- paste0("c", seq_len(ncol(expected)))
        df <- data.frame(group = rownames(expected), check.names = FALSE)
        for (j in seq_along(keys)) df[[keys[j]]] <- expected[, j]
        lc_table(df,
          cols = c(list(lc_col("group", par$lab1, "row")),
                   Map(function(k, lab) lc_col(k, lab, digits = 1),
                       keys, colnames(expected))),
          lead = "Liczebności oczekiwane (gdyby H₀ była prawdziwa):")
      }
    )
  })

  # --- Widget 2: Porownanie chi-kwadrat vs Fisher ---
  output$ch5_compare_result <- renderUI({
    req(input$ch5_compare)
    tab <- isolate(ch5_tab())

    if (is.null(tab)) {
      return(lc_caption(
               "Najpierw wylosuj próbę w widgecie powyżej."
             ))
    }

    test_chi <- chisq.test(tab)
    test_fisher <- tryCatch(
      fisher.test(tab),
      error = function(e) fisher.test(tab, simulate.p.value = TRUE, B = 2000)
    )

    low_exp <- any(test_chi$expected < 5)
    n_low <- sum(test_chi$expected < 5)

    div(
      lc_table(
        data.frame(
          row = c("p-wartość", "Decyzja"),
          chi = c(format_p_value(test_chi$p.value),
                  format_test_result(test_chi$p.value)$decision),
          fisher = c(format_p_value(test_fisher$p.value),
                     format_test_result(test_fisher$p.value)$decision)
        ),
        cols = list(
          lc_col("row", "", "row"),
          lc_col("chi", "Test χ²", "text"),
          lc_col("fisher", "Test Fishera", "text")
        ),
        cell_class = list(
          chi = c(NA, if (test_chi$p.value < 0.05) "is-base" else NA),
          fisher = c(NA, if (test_fisher$p.value < 0.05) "is-base" else NA)
        )
      ),
      lc_status(
        p(lc_verdict(tags$b("Liczebności oczekiwane poniżej 5:"), type = if (low_exp) "danger" else "ok"),
          if (low_exp) paste0(" tak (komórki: ", n_low, ") — przybliżenie χ² może być
            niedokładne, bezpieczniejszy jest wynik testu Fishera.")
          else " nie — przybliżenie χ² powinno być wystarczające.")
      )
    )
  })

  # --- Ćwiczenia CASchools ---

  .cas_chisq <- function(tab) {
    ct <- chisq.test(tab, correct = FALSE)
    n  <- sum(tab)
    k  <- min(nrow(tab), ncol(tab))
    v  <- sqrt(ct$statistic / (n * (k - 1)))
    list(chi2 = unname(ct$statistic), df = unname(ct$parameter),
         p = ct$p.value, tab = tab, v = unname(v), n = n)
  }

  # Liczba z przecinkiem dziesiętnym (polski zapis).
  .cas_num <- function(x, digits = 3) {
    formatC(x, format = "f", digits = digits)
  }

  .cas_result_lines <- function(r) {
    tagList(
      tags$li(paste0("χ²(", r$df, ") = ", .cas_num(r$chi2), ", ", format_p(r$p))),
      tags$li(paste0("Cramér's V = ", .cas_num(r$v)))
    )
  }

  .cas_verdict <- function(r) {
    lc_verdict(tags$strong(if (r$p < 0.05) "Odrzucamy H₀" else "Brak podstaw do odrzucenia H₀"),
               type = if (r$p < 0.05) "danger" else "ok")
  }

  output$cas_ch5_sol8 <- renderUI({
    r <- local({
      high_eng <- .ch5_cas$english > 20
      .cas_chisq(table(grades = .ch5_cas$grades, high_english = high_eng))
    })
    tab <- r$tab
    pct_high <- 100 * tab[, "TRUE"] / rowSums(tab)
    tagList(
      p(tags$b("H₀:"), " typ szkoły i high_english są niezależne · ",
        tags$b("Hₐ:"), " zmienne są zależne"),
      lc_crosstab(tab, row_name = "grades", col_name = "high_english", lead = FALSE),
      tags$ul(
        .cas_result_lines(r),
        lapply(rownames(tab), function(g) {
          tags$li(paste0("Odsetek high_english w ", g, ": ",
                         .cas_num(pct_high[[g]], 1), "%"))
        })
      ),
      .cas_verdict(r),
      p(tags$b("Interpretacja:"),
        " Test χ² rozstrzyga tylko, czy dane przemawiają przeciw niezależności.
        Kierunek pokazuje porównanie odsetków high_english w obu typach szkół,
        a siłę związku — Cramér's V.",
        if (r$p >= 0.05) " Brak podstaw do odrzucenia H₀ nie dowodzi, że typ
        szkoły i odsetek uczniów ELL są niezależne: różnica w odsetkach mogła
        powstać przez przypadek, ale mogła też być zbyt mała, by test ją wykrył.")
    )
  })

  output$cas_ch5_sol9 <- renderUI({
    r <- local({
      high_str   <- .ch5_cas$student_teacher_ratio > 20
      high_lunch <- .ch5_cas$lunch > 50
      .cas_chisq(table(high_str = high_str, high_lunch = high_lunch))
    })
    tab <- r$tab
    p_hi_str_poor <- tab["TRUE",  "TRUE"] / sum(tab["TRUE", ])
    p_lo_str_poor <- tab["FALSE", "TRUE"] / sum(tab["FALSE", ])
    r_cont <- cor.test(.ch5_cas$student_teacher_ratio, .ch5_cas$lunch)
    tagList(
      p(tags$b("H₀:"), " high_str i high_lunch są niezależne · ",
        tags$b("Hₐ:"), " zmienne są zależne"),
      lc_crosstab(tab, row_name = "high_str", col_name = "high_lunch", lead = FALSE),
      tags$ul(
        .cas_result_lines(r),
        tags$li(paste0("Odsetek high_lunch wśród STR > 20: ",
                       .cas_num(100 * p_hi_str_poor, 1), "%")),
        tags$li(paste0("Odsetek high_lunch wśród STR ≤ 20: ",
                       .cas_num(100 * p_lo_str_poor, 1), "%")),
        tags$li(paste0("Dla porównania korelacja Pearsona ciągłych STR i lunch: r = ",
                       .cas_num(unname(r_cont$estimate)), ", ",
                       format_p(r_cont$p.value)))
      ),
      .cas_verdict(r),
      p(tags$b("Wniosek:"),
        if (r$p < 0.05) " Okręgi z przeładowanymi klasami mają istotnie wyższy
        odsetek ubogich uczniów."
        else " Okręgi z przeładowanymi klasami mają nieco wyższy odsetek ubogich
        uczniów, ale po podziale obu zmiennych na dwie klasy test nie daje podstaw
        do odrzucenia H₀. Korelacja ciągłych zmiennych jest natomiast
        istotna, choć słaba: podział na dwie klasy wyrzucił część informacji.",
        " Słaby związek STR z ubóstwem oznacza, że ubóstwo może tłumaczyć
        korelację STR–read z zadania 5 tylko częściowo. Żeby to sprawdzić,
        potrzebny jest model uwzględniający obie zmienne naraz (regresja wieloraka,
        wykład 06).")
    )
  })
}
