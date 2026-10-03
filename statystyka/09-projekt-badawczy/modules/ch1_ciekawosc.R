ch1_ui <- lecture_chapter(id = "ch1", num = "1", title = "Od ciekawości do celu", content = tagList(
  lc_chapter_hero(
    kicker = "Rozdział 01 · Start badania",
    num = "01",
    title = "Od ciekawości do celu.",
    lead = "Projekt badawczy zaczyna się przed pierwszym testem: od celu
            i od kilku konkurujących wyjaśnień, które razem pozwalają ten
            cel ocenić."
  ),

  lc_h2("sec-01", "Zaczynamy od sytuacji, nie od metody"),

  lc_p("Wykład 08 przeprowadził jedną analizę od pytania do wniosku. Na końcu
    padła odpowiedź ostrożna: dane są obserwacyjne, efekt podaliśmy
    w jednostkach praktycznych razem z przedziałem ufności, a obok niego
    listę ograniczeń. Pytanie było tam jednak dane z góry. We własnym
    projekcie pytanie trzeba postawić samodzielnie i to ten etap, a nie
    wybór testu, najczęściej decyduje o tym, czy z analizy wyjdzie
    coś sensownego."),

  lc_p("Ten wykład pokazuje całą drogę na jednym zbiorze danych. Opisuje on
    463 kursy prowadzone w latach 2000–2002 na University of Texas at
    Austin przez 94 wykładowców. Dla każdego kursu mamy ocenę z ankiety
    studenckiej, kilka cech prowadzącego (płeć, wiek, ocenę wyglądu,
    język ojczysty) i kilka informacji o samym kursie. To nie jest jeszcze
    projekt badawczy. To materiał, z którego da się zbudować wiele różnych
    historii, i pierwszym zadaniem jest wybrać jedną."),

  lc_p("Kolejne rozdziały idą w tej samej kolejności, w jakiej powstaje
    prawdziwy projekt. W tym rozdziale z luźnej ciekawości robimy cel
    badania. W rozdziale 2 zamieniamy go na wiązkę ",
    gloss("hipoteza badawcza", "hipotez badawczych"), ", w rozdziale 3
    sprawdzamy, co naprawdę mierzą zmienne, a w rozdziale 4 składamy
    z tego konspekt. Dopiero rozdział 5 sięga po testy. Rozdziały 6 i 7
    sprawdzają, czy pierwsze wyniki nie są zasługą innych zmiennych,
    a rozdział 8 kończy projekt wnioskiem."),

  lc_p("Zanim sformułujemy jakiekolwiek pytanie, warto obejrzeć samą tabelę
    i ustalić, co jest w niej jedną ",
    gloss("jednostka obserwacji", "jednostką obserwacji"), ", jakiego typu
    są zmienne (wykład 01) i czego w danych nie ma. Panel pokazuje po
    dziesięć wierszy; pod tabelą jest opis wszystkich kolumn."),

  figure_panel(
    label = "Ryc. 1.1",
    title = "Podgląd danych",
    lc_toolbar(
      numericInput("ch1_row_from", "Pokaż od wiersza", value = 1,
                   min = 1, max = nrow(tr_data), step = 10),
      selectInput("ch1_col_view", "Zakres kolumn",
          choices = c("Kluczowe zmienne" = "key", "Wszystkie zmienne" = "all"),
          selected = "key"
          ),
          selectInput("ch1_sort_var", "Sortuj wg",
          choices = setNames(
            c("none", "eval", "beauty", "response.rate", "age"),
            c("bez sortowania", tr_labels[c("eval", "beauty", "response.rate", "age")])
          ),
          selected = "none"
          )
          ),
          uiOutput("ch1_data_view"),
    lc_table(
      data.frame(
        var = c("eval", "beauty", "gender, age", "minority", "native, tenure", "division, credits", "students, allstudents", "response.rate", "prof"),
        desc = c(
        "ocena kursu z ankiety studenckiej, uśredniona po osobach, które ją wypełniły; skala od 1 (bardzo źle) do 5 (znakomicie)",
        "ocena wyglądu prowadzącego wystawiona przez panel sześciu studentów, uśredniona i przesunięta tak, by średnia w zbiorze wynosiła 0",
        "płeć i wiek prowadzącego",
        "czy prowadzący należy do mniejszości (w oryginalnym opisie danych: osoba niebiała)",
        "czy angielski jest językiem ojczystym prowadzącego; czy prowadzący jest na ścieżce stałego zatrudnienia (tenure track)",
        "poziom kursu (niższy to głównie duże kursy pierwszych lat) i czy jest to jednopunktowy kurs fakultatywny, np. joga albo taniec",
        "liczba osób, które wypełniły ankietę, i liczba osób zapisanych na kurs",
        "odsetek zapisanych, którzy wypełnili ankietę (w %); policzony z dwóch poprzednich kolumn",
        "identyfikator prowadzącego; ta sama osoba prowadzi zwykle kilka kursów"
        )
      ),
      cols = list(
        lc_col("var", "Kolumna", "row"),
        lc_col("desc", "Co opisuje", "text")
      ),
      narrow = "cards", prose = TRUE
    )
  ),

  lc_p("Jednym wierszem jest kurs, a nie prowadzący ani student. Ma to dwie
    konsekwencje. Po pierwsze, eval jest już podsumowaniem: średnią z ankiet
    wszystkich osób, które odpowiedziały. Po drugie, 94 prowadzących
    rozkłada się na 463 kursy nierówno, od jednego do trzynastu kursów na
    osobę, przy medianie 4. Cechy prowadzącego, w tym ocena wyglądu i wiek,
    powtarzają się więc identycznie we wszystkich jego kursach. Wiersze nie
    są w pełni niezależne, a to problem, który w wykładzie 07 (rozdział 1)
    opisaliśmy jako brak niezależności obserwacji."),

  lc_p("Posortowanie tabeli po ocenie kursu pokazuje, że studenci korzystają
    głównie z górnej części skali. Średnia eval wynosi 4.00, mediana 4.0, a wartości leżą między 2.1 a 5.0.
    Tylko 18 kursów dostało ocenę niższą niż 3, a 56% kursów co najmniej 4.
    Wśród zmiennych opisujących prowadzącego większość ma tylko dwie
    kategorie i są to kategorie bardzo nierówne: 64 kursy prowadzą osoby
    z mniejszości, a 28 kursów osoby, dla których angielski nie jest językiem
    ojczystym. Te liczby wrócą, gdy będziemy porównywać grupy."),

  lc_h2("sec-02", "Od pomysłu do celu badania"),

  lc_p("Tabela podsuwa wiele pytań naraz: czy atrakcyjni prowadzący dostają
    lepsze oceny, czy kobiety są oceniane surowiej, czy w dużych kursach
    ankietę wypełniają tylko niezadowoleni. Każde z nich może stać się ",
    gloss("pytanie badawcze", "pytaniem badawczym"), ", ale samo w sobie
    nie jest jeszcze celem badania. Cel to jedno zdanie,
    które mówi, czego chcemy się dowiedzieć i po co, a pojedyncze pytania
    porządkuje jako jego części. Droga od pomysłu do planu ma trzy szczeble."),

  tags$ol(
    tags$li(b_("Pomysł badawczy."), " Zaczynamy od przypuszczenia: ocena z ankiety może mieć kilka źródeł."),
    tags$li(b_("Cel badania."), " Formułujemy pytanie, które da się rozważyć na danych."),
    tags$li(b_("Plan analizy."), " Zamieniamy cel na hipotezy, zmienne i porównania.")
  ),

  lc_p("Na tych danych cel może brzmieć następująco."),

  lc_note("Cel badawczy", p(tags$em(tr_goal))),

  lc_p("Na tak postawione pytanie nie odpowie pojedynczy test, bo w danych
    nie ma osobnej miary jakości nauczania, z którą można by porównać eval.
    Da się natomiast sprawdzić, czy ocena zależy od rzeczy, które z jakością
    zajęć nie powinny mieć nic wspólnego: od wyglądu prowadzącego, jego płci,
    języka ojczystego, przynależności do mniejszości albo od tego, jaka
    część grupy wypełniła ankietę. Jeśli zależy, to ocena miesza jakość
    z czymś innym. Każde takie podejrzenie nazywamy w tym wykładzie tropem."),

  lc_h2("sec-03", "Wiązka tropów, którą będziemy śledzić"),

  lc_p("Trop to robocze przypuszczenie o jednym możliwym składniku oceny
    z ankiety. Zestaw tropów podporządkowanych wspólnemu celowi nazywamy
    wiązką. Żaden trop nie odpowiada na cel samodzielnie: wynik dla
    wyglądu mówi tylko o wyglądzie. Dopiero zestawienie kilku tropów
    pozwala powiedzieć coś o tym, co mierzy eval. Nasza wiązka ma pięć
    tropów."),

  uiOutput("ch1_tropy_bundle"),

  lc_p("Cztery pierwsze tropy dotyczą cech prowadzącego, za które nie
    powinien być ani nagradzany, ani karany. Piąty jest innego rodzaju:
    dotyczy samego pomiaru, czyli tego, czy ankieta wypełniona przez część
    grupy mówi coś o całej grupie. Na tym etapie żaden trop nie ma jeszcze
    przypisanego testu. Metodę dobierzemy w rozdziale 5, gdy będzie
    wiadomo, jakiego typu są zmienne i jakie porównanie odpowiada na
    pytanie."),

  lc_note("Zasada", rule = TRUE,
    "Najpierw cel i tropy, potem metoda. Test jest narzędziem do sprawdzenia
     tropu, a nie punktem wyjścia badania."
  ),

  lc_h2("sec-04", "Tablica tropów"),

  lc_p("Wyniki będziemy zbierać w jednej tablicy, do której wraca cały wykład.
    Każdy wiersz to jeden trop z pytaniem badawczym. Kolumny z narzędziem,
    miarą efektu i werdyktem są na razie puste, bo nie mieliśmy jeszcze
    kontaktu z danymi."),

  figure_panel(
    label = "Ryc. 1.2",
    title = "Tablica tropów (stan początkowy)",
    tr_board_ui(reveal = character(0), show_verdict = TRUE)
  ),

  lc_p("Tablica zapełni się w rozdziale 5, po pierwszych testach. W rozdziale 6
    odczytamy ją w całości i zapytamy, co cała wiązka mówi o celu,
    a w rozdziale 8 wróci jako podsumowanie projektu. Puste pola mają też
    znaczenie same w sobie: pytania zostały zapisane, zanim zobaczyliśmy
    jakikolwiek wynik, więc nie da się ich później dopasować do tego,
    co akurat wyszło."),

  lc_h2("sec-05", "Tropy poza naszą wiązką"),

  lc_p("Pięć tropów to wybór na potrzeby tego wykładu, a nie pełna lista.
    Ten sam cel można badać wieloma innymi pytaniami. Kilka przykładów
    zebrano w tabeli."),

  uiOutput("ch1_extra_tropy"),

  lc_p("Część z tych tropów da się sprawdzić na naszych danych: wielkość
    kursu opisuje liczba zapisanych (od 8 do 581 osób), a identyfikator
    prowadzącego pozwala porównać oceny tej samej osoby na różnych kursach.
    Pory zajęć, trudności kursu ani dyscypliny w tabeli nie ma; poziom
    kursu to co innego niż wydział. W projekcie warto dążyć do tego, żeby
    wiązka obejmowała możliwie wszystkie rozsądne wyjaśnienia, a te,
    których nie da się sprawdzić, zapisać jako ograniczenia."),

  lc_chapter_next("02", "Hipotezy jako tropy",
    "Mamy cel i wiązkę. Teraz każdy trop dostaje postać hipotezy badawczej z alternatywnymi wyjaśnieniami.",
    "ch2")
  )
)

ch1_server <- function(input, output, session) {
  output$ch1_tropy_bundle <- renderUI({
    tags$ul(lapply(tr_trop_order, function(id) {
      tr <- tr_tropy[[id]]
      tags$li(b_(paste0(tr$short, ":")), " ", tr$question)
    }))
  })

  output$ch1_extra_tropy <- renderUI({
    extra <- list(
      c("Wielkość kursu", "Czy bardzo duże grupy są oceniane inaczej niż kameralne?"),
      c("Pora i dzień zajęć", "Czy zajęcia o poranku albo w piątek dostają niższe oceny?"),
      c("Trudność i obciążenie", "Czy łatwiejsze kursy dostają wyższe oceny niezależnie od jakości?"),
      c("Dyscyplina / wydział", "Czy kursy ścisłe są oceniane surowiej niż humanistyczne?"),
      c("Powtarzalność prowadzącego", "Czy ten sam prowadzący dostaje podobne oceny na różnych kursach?")
    )
    lc_table(
      data.frame(
        trop = vapply(extra, `[[`, character(1), 1),
        question = vapply(extra, `[[`, character(1), 2),
        stringsAsFactors = FALSE
      ),
      cols = list(
        lc_col("trop", "Trop", "row"),
        lc_col("question", "Przykładowe pytanie", "text")
      ),
      narrow = "stack-last"
    )
  })

  output$ch1_data_view <- renderUI({
    key_cols <- c(
      "eval", "beauty", "gender", "age", "minority", "native",
      "division", "credits", "students", "allstudents",
      "response.rate"
    )
    cols <- if (identical(input$ch1_col_view, "all")) names(tr_data) else key_cols
    show <- tr_data[, cols, drop = FALSE]

    if (!is.null(input$ch1_sort_var) && !identical(input$ch1_sort_var, "none")) {
      show <- show[order(show[[input$ch1_sort_var]], decreasing = TRUE), , drop = FALSE]
    }

    start <- min(max(1, input$ch1_row_from), nrow(show))
    idx <- start:min(nrow(show), start + 9)
    out <- show[idx, , drop = FALSE]
    out <- cbind(data.frame(row = rownames(out), stringsAsFactors = FALSE), out)
    # Jak wcześniej w renderTable(): liczby całkowite bez miejsc po kropce,
    # pozostałe liczby z 2 miejscami.
    cols <- c(
      list(lc_col("row", "", "row")),
      lapply(names(out)[-1], function(key) {
        x <- out[[key]]
        if (is.numeric(x)) {
          lc_col(key, key, "num", digits = if (is.integer(x)) 0 else 2)
        } else {
          lc_col(key, key, "text")
        }
      })
    )
    lc_table(out, cols, scroll = TRUE, sticky_first = TRUE,
             label = "Podgląd danych TeachingRatings")
  })
}
