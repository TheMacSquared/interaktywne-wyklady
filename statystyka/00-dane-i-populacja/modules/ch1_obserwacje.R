# ============================================================================
# CHAPTER 1: Obserwacja i zmienna
# ============================================================================

ch1_ui <- list(
  id    = "ch-obserwacje",
  num   = "01",
  title = "Obserwacja i zmienna",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 01 · Dane i populacja",
      num    = "01",
      title  = "Każdy wiersz to ktoś albo coś.",
      lead   = "Zanim cokolwiek policzymy, trzeba wiedzieć, kogo albo co opisuje
                jeden wiersz tabeli i co opisuje jedna kolumna. Te dwie rzeczy
                nazywamy obserwacją i zmienną. Od nich zależy, co znaczy liczba
                n, którą zobaczymy przy każdym wyniku w tym kursie."
    ),

    lc_p("Statystyka zaczyna się od tabeli. Ankieta, pomiary w laboratorium,
      rejestr sprzedaży w sklepie czy dane z czujnika pogodowego wyglądają
      różnie, ale przed analizą sprowadzamy je do tej samej postaci:
      prostokąta, w którym wiersze odpowiadają temu, co badamy, a kolumny
      temu, co o tym zapisaliśmy. Ten rozdział nazywa elementy takiej tabeli
      i pokazuje, że wybór, co jest wierszem, jest decyzją, a nie oczywistością."),

    lc_h2("ch1-anatomia", "Anatomia zbioru danych"),

    lc_p("Jeden wiersz tabeli to ", gloss("obserwacja"), ": wszystko, co
      zapisaliśmy o jednej osobie, jednym obiekcie albo jednym zdarzeniu.
      To, czego dotyczy wiersz, nazywamy ",
      gloss("jednostka obserwacji", "jednostką obserwacji"), ". W ankiecie
      studenckiej jednostką jest student, w badaniu gospodarstw rolnych
      gospodarstwo, w danych o pogodzie jeden dzień w jednym mieście."),

    lc_p("Jedna kolumna to ", gloss("zmienna"), ": cecha, którą zapisaliśmy
      dla każdej obserwacji i która może przyjmować różne wartości. Rok
      studiów, czas dojazdu na uczelnię czy to, czy ktoś pracuje zarobkowo,
      to zmienne. Pojedyncza komórka tabeli to wartość zmiennej dla jednej
      obserwacji, na przykład czas dojazdu jednej konkretnej studentki.
      Liczbę obserwacji oznaczamy literą n."),

    lc_table(
      data.frame(
        id       = faculty$id[c(1, 700, 1300, 2100)],
        rok      = faculty$rok[c(1, 700, 1300, 2100)],
        akademik = ifelse(faculty$akademik[c(1, 700, 1300, 2100)], "tak", "nie"),
        dojazd   = faculty$dojazd[c(1, 700, 1300, 2100)],
        praca    = ifelse(faculty$praca[c(1, 700, 1300, 2100)], "tak", "nie"),
        stringsAsFactors = FALSE
      ),
      list(
        lc_col("id", "Nr", "row"),
        lc_col("rok", "Rok studiów"),
        lc_col("akademik", "Akademik", "text"),
        lc_col("dojazd", "Dojazd (min)"),
        lc_col("praca", "Praca", "text")
      ),
      prose = TRUE,
      caption = "Cztery wiersze z danych o studentach wydziału. Wiersz to
                 obserwacja (jeden student), kolumna to zmienna, komórka to
                 wartość zmiennej dla jednej osoby."
    ),

    lc_p("Ta mała tabela pochodzi z danych, które będą nam towarzyszyć przez
      cały wykład: wszystkich ", lc_fmt(pop_N), " studentów jednego wydziału.
      Dla każdej osoby znamy rok studiów, to, czy mieszka w akademiku, czas
      dojazdu na zajęcia w minutach i to, czy pracuje zarobkowo. Każda
      z tych zmiennych jest innego rodzaju: jedne przyjmują liczby, inne
      kategorie. Rodzajom zmiennych poświęcony jest pierwszy rozdział
      wykładu 01. Tutaj wystarczy, że zmienna to kolumna, a obserwacja
      to wiersz."),

    lc_note("Zasada", rule = TRUE,
      "Wiersz to jedna obserwacja, kolumna to jedna zmienna, komórka to jedna
       wartość. Dane w takim układzie da się opisać i analizować bez
       dodatkowych przekształceń."
    ),

    lc_h2("ch1-jednostka", "Co jest jednostką obserwacji"),

    lc_p("Jednostka obserwacji nie wynika z samych danych, tylko z pytania,
      które zadajemy. Wyobraźmy sobie, że pięć osób zapisywało czas dojazdu
      na uczelnię przez trzy dni. Te same piętnaście liczb można ułożyć
      na dwa sposoby. W pierwszym wierszem jest osoba, a pomiary z kolejnych
      dni stoją obok siebie w osobnych kolumnach. W drugim wierszem jest
      pojedynczy przejazd: osoba, dzień i zmierzony czas."),

    figure_panel(
      label = "Ryc. 1.1",
      title = "Te same pomiary, dwie jednostki obserwacji",
      width_mode = "text",
      lc_toolbar(
        lc_segmented("ch1_layout", "Wiersz to", choices = c(
          "Osoba"    = "wide",
          "Przejazd" = "long"
        ), selected = "wide"),
        lc_readouts(uiOutput("ch1_layout_reads"))
      ),
      uiOutput("ch1_layout_table"),
      uiOutput("ch1_layout_caption")
    ),

    lc_p("W obu układach jest dokładnie ta sama informacja, ale liczba
      obserwacji jest różna: 5 albo 15. Który układ jest właściwy, zależy
      od pytania. Jeśli pytamy, czy studenci Biologii dojeżdżają krócej niż
      studenci Ekonomii, jednostką jest osoba i n = 5, bo trzy przejazdy
      tej samej osoby nie są trzema niezależnymi świadectwami. Jeśli pytamy,
      czy w środy jeździ się dłużej niż w poniedziałki, jednostką jest
      przejazd, a dzień tygodnia staje się zmienną."),

    lc_warn("Pułapka",
      "Policzenie każdego pomiaru jako osobnej osoby sztucznie zawyża n.
       Piętnaście przejazdów pięciu osób to wciąż informacja o pięciu
       osobach. Wiele błędów w analizie zaczyna się od pomylenia jednostki
       obserwacji z pojedynczym pomiarem."
    ),

    lc_p("Ustalenie jednostki obserwacji to pierwszy krok każdej analizy.
      Drugi to pytanie, o kim chcemy coś powiedzieć: tylko o tych osobach,
      które są w tabeli, czy o znacznie większej grupie. Tym zajmuje się
      następny rozdział."),

    lc_chapter_next(
      num       = "02",
      title     = "Populacja i próba",
      lead      = "o kim mówią dane, a o kim chcemy mówić",
      target_id = "ch-populacja"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch1_server <- function(input, output, session) {

  ch1_layout <- reactive(input$ch1_layout %||% "wide")

  output$ch1_layout_reads <- renderUI({
    wide <- ch1_layout() == "wide"
    tagList(
      lc_readout("obserwacje (n)", if (wide) nrow(commute_people) else nrow(commute_long),
                 color = col_sample),
      lc_readout("zmienne", if (wide) ncol(commute_people) else ncol(commute_long),
                 color = upwr_secondary)
    )
  })

  output$ch1_layout_table <- renderUI({
    if (ch1_layout() == "wide") {
      lc_table(commute_people, list(
        lc_col("osoba", "Osoba", "row"),
        lc_col("kierunek", "Kierunek", "text"),
        lc_col("pon", "Pon (min)"),
        lc_col("wt", "Wt (min)"),
        lc_col("sr", "Śr (min)")
      ), scroll = TRUE, label = "Czas dojazdu: wiersz to osoba")
    } else {
      lc_table(commute_long, list(
        lc_col("osoba", "Osoba", "row"),
        lc_col("kierunek", "Kierunek", "text"),
        lc_col("dzien", "Dzień", "text"),
        lc_col("czas", "Czas (min)")
      ), scroll = TRUE, label = "Czas dojazdu: wiersz to przejazd")
    }
  })

  output$ch1_layout_caption <- renderUI({
    if (ch1_layout() == "wide") {
      lc_caption("Jednostką obserwacji jest osoba: 5 wierszy, a dni tygodnia
                  są rozpisane na trzy kolumny.")
    } else {
      lc_caption("Jednostką obserwacji jest przejazd: 15 wierszy, a dzień
                  tygodnia jest zwykłą zmienną w jednej kolumnie.")
    }
  })
}
