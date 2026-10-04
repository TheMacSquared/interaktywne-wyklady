# Tab 5: Tarantino — dane eventowe, zła struktura

ch5_ui <- lecture_chapter(id = "ch5", num = "5", title = "Tarantino", content = tagList(

  lc_chapter_hero(
    kicker = "Rozdział 05 · Co czyni dobry zbiór danych?",
    num    = "05",
    title  = "Filmy Tarantino.",
    lead   = "Ciekawy temat nie wystarczy. Prawie dwa tysiące wierszy może
              oznaczać zaledwie siedem obserwacji, jeśli każdy wiersz
              opisuje zdarzenie, a nie jednostkę, którą chcemy porównywać."
  ),

  lc_h2("sec-01", "Opis"),

  lc_p("Ten zbiór pochodzi z serwisu FiveThirtyEight, który policzył każde
    przekleństwo i każdą śmierć na ekranie w siedmiu filmach Quentina
    Tarantino, od „Wściekłych psów” po „Django”. Tabela ma 1894 wiersze:
    1704 przekleństwa i 190 śmierci. Temat jest chwytliwy i łatwo sobie
    wyobrazić pytanie do projektu: czy w filmach, w których więcej się
    przeklina, ginie też więcej postaci? Zanim zaczniemy szukać
    odpowiedzi, sprawdźmy, czy te dane w ogóle pozwalają je zadać."),

  lc_p("Zbiór ma cztery zmienne:"),

  tags$ul(
    tags$li(tags$code("movie"), " — tytuł filmu,"),
    tags$li(tags$code("type"), " — rodzaj zdarzenia: przekleństwo (",
      tags$code("word"), ") albo śmierć (", tags$code("death"), "),"),
    tags$li(tags$code("word"), " — wypowiedziane słowo (puste, gdy zdarzeniem jest śmierć),"),
    tags$li(tags$code("minutes_in"), " — minuta filmu, w której zdarzenie nastąpiło.")
  ),

  lc_h2("sec-02", "Podgląd danych"),

  lc_p("Przeglądając tabelę, zwróć uwagę przede wszystkim na to, co opisuje
    jeden wiersz, czyli co jest tu ",
    gloss("jednostka obserwacji", "jednostką obserwacji"), "."),

  figure_panel(
    label = "Tab. 5.1",
    title = "Filmy Tarantino: 1894 zdarzenia",
    uiOutput("tab4_table")
  ),

  lc_p("Kolejne wiersze to kolejne zdarzenia z tego samego filmu. Film nie
    ma własnego wiersza z cechami, takimi jak
    długość, budżet czy rok premiery. Jest tylko etykietą powtarzaną setki
    razy. To układ typowy dla danych zdarzeniowych: świetnie nadają się do
    opisu przebiegu filmu, ale nie przypominają tabeli „jeden wiersz — jedna
    osoba”, z którą pracowaliśmy w poprzednich wykładach."),

  lc_h2("sec-03", "Eksploracja"),

  lc_p("Przy eksploracji zwróć uwagę, czy wykresy porównują jednostki, czy
    tylko liczą zdarzenia."),

  figure_panel(
    label = "Ryc. 5.1",
    title = "Kiedy w filmach pojawiają się zdarzenia",
    lc_toolbar(
      lc_segmented("tab4_view", NULL,
        choices = c("Histogram: minutes_in" = "hist", "Porównanie filmów" = "bar"))
    ),
    lc_plot("tab4_explore_plot", ratio = "1.8/1", max_height = "350px")
  ),

  lc_p("", gloss("histogram", "Histogram"), " minuty zdarzenia wrzuca do jednego worka siedem filmów
    o różnej długości: w najkrótszym ostatnie zdarzenie przypada na
    95. minutę, w najdłuższym na 160. Słupki z końca osi, powyżej
    150. minuty, pochodzą już tylko z „Django”. Taki wykres opisuje więc
    mieszankę filmów, a nie żaden z nich. Porównanie filmów
    jest bardziej wymowne. „Pulp Fiction” ma 469 przekleństw i 7 śmierci,
    „Kill Bill: Vol. 1” — 57 przekleństw i 63 śmierci. Różnice między filmami
    są ogromne, ale każda z nich to porównanie jednej liczby z jedną liczbą."),

  lc_p("Tak wygląda w praktyce problem z katalogu nazwany zła struktura
    danych. Wiersz w tabeli nie jest obserwacją w sensie, którego wymagają
    testy. Zdarzenia z tego samego filmu nie są też od siebie niezależne:
    dzielą scenariusz, reżyserię i postacie, więc dochodzi drugi problem
    z katalogu, brak niezależności obserwacji."),

  lc_h2("sec-04", "Próba analiz"),

  lc_p("Przed przejściem dalej zastanów się, którą z poznanych analiz dałoby
    się tu zastosować do tabeli w obecnej postaci."),

  figure_panel(
    label = "Ryc. 5.2",
    title = "Jaka analiza tu pasuje?",
    uiOutput("tab4_quiz_options"),
    uiOutput("tab4_quiz_result")
  ),

  lc_p("", gloss("test t", "Test t"), ", ", gloss("korelacja", "korelacja"), " i regresja zakładają, że każdy wiersz to niezależna
    obserwacja z cechami mierzonymi na tej samej jednostce. Tu tego nie ma.
    Jedyna zmienna liczbowa, ", tags$code("minutes_in"), ", opisuje moment
    zdarzenia, a nie cechę filmu, więc nie ma czego z nią korelować.
    Naturalnym ratunkiem jest ", gloss("agregacja"), ": zamiast zdarzeń
    liczymy, ile przekleństw i ile śmierci przypada na każdy film. Wtedy
    jednostką obserwacji staje się film."),

  figure_panel(
    label = "Ryc. 5.3",
    title = "Agregacja do poziomu filmów",
    lc_action("tab4_aggregate", "Zagreguj dane", variant = "solid"),
    uiOutput("tab4_agg_result")
  ),

  lc_p("Po agregacji struktura jest poprawna, ale z 1894 wierszy zostaje
    7 obserwacji, czyli problem z katalogu nazwany za mało danych.
    Współczynnik korelacji między liczbą przekleństw a liczbą śmierci
    wynosi w tych siedmiu filmach -0.67, co wygląda na wyraźną zależność.
    Jego 95-procentowy ", gloss("przedział ufności", "przedział ufności"), " sięga jednak od -0.95 do 0.17,
    a p = 0.10. Przy n = 7 dane są zgodne zarówno z silną ujemną
    zależnością, jak i z jej brakiem, a ",
    gloss("moc testu"), " jest tak niska, że nawet duży efekt łatwo
    przeoczyć. To ten sam mechanizm, który w wykładach 03 i 04 widzieliśmy
    przy małych próbach."),

  lc_h2("sec-05", "Werdykt"),

  lc_p("Zbiór dyskwalifikują dwie rzeczy naraz. W surowej postaci ma złą
    strukturę: wiersz to zdarzenie, a zdarzenia z jednego filmu są od siebie
    zależne. Po agregacji struktura jest poprawna, ale siedem filmów to za
    mało, żeby cokolwiek wnioskować o populacji filmów, a dodatkowych
    zmiennych opisujących film w tym zbiorze nie ma. Nie da się tego
    naprawić czyszczeniem, bo brakuje po prostu jednostek."),

  lc_p("Dane nie są przy tym bezużyteczne. Nadają się do ", gloss("statystyka opisowa", "statystyki opisowej"), "
    z wykładu 01: ", gloss("tabela częstości", "tabel częstości"), " słów, porównania filmów na ", gloss("wykres słupkowy", "wykresie słupkowym"), " czy opisu, jak zdarzenia rozkładają się w czasie trwania
    jednego filmu. Nie nadają się do testów i modeli, które uogólniają
    wynik poza te siedem tytułów."),

  lc_note("Werdykt",
    "Zły zbiór do klasycznej statystyki: dane zdarzeniowe o złej strukturze,
    a po agregacji zostaje tylko 7 obserwacji."),

  lc_chapter_next(
    num = "06",
    title = "Hotel",
    lead = "Zbiór może mieć rozsądną liczbę obserwacji i właściwą strukturę,
            a mimo to nie nadawać się do analizy, bo jego zmienne prawie
            się nie różnią.",
    target_id = "ch6"
  )
))

ch5_server <- function(input, output, session) {

  output$tab4_table <- renderUI({
    dd_data_table(round_df(tarantino), page_size = 10, page = input$tab4_table_page, page_input = "tab4_table_page")
  })

  zoom_plot_server("tab4_explore_plot", reactive({
    if (identical(input$tab4_view, "bar")) {
      tarantino %>%
        count(movie, type) %>%
        ggplot(aes(x = reorder(movie, n), y = n, fill = type)) +
        geom_col(position = "dodge", alpha = 0.8) +
        scale_fill_manual(values = c("death" = data_bad, "word" = data_mixed),
                          labels = c("death" = "śmierć", "word" = "przekleństwo")) +
        coord_flip() +
        labs(x = NULL, y = "Liczba zdarzeń", fill = "Typ") +
        theme_upwr(base_size = 14)
    } else {
      ggplot(tarantino, aes(x = minutes_in)) +
        geom_histogram(bins = 30, fill = data_primary, color = "white", alpha = 0.8) +
        labs( x = "Minuta filmu", y = "Liczba zdarzeń") +
        theme_upwr(base_size = 14)
    }
  }))

  tab4_quiz_answered <- reactiveVal(FALSE)
  tab4_quiz_selected <- reactiveVal(NULL)

  tab4_quiz_choices <- list(
    list(letter = "A", value = "Test t", text = "Test t"),
    list(letter = "B", value = "Korelacja", text = "Korelacja"),
    list(letter = "C", value = "Regresja", text = "Regresja"),
    list(letter = "D", value = "Zadna z klasycznych", text = "Żadna z klasycznych")
  )

  output$tab4_quiz_options <- renderUI({
    if (tab4_quiz_answered()) return(NULL)
    div(class = "quiz-tiles quiz-cols-4",
      lapply(tab4_quiz_choices, function(opt) {
        actionButton(paste0("tab4_tile_", gsub(" ", "_", opt$value)),
          tagList(
            div(class = "tile-letter", opt$letter),
            div(class = "tile-text", opt$text)
          ),
          class = "quiz-tile"
        )
      })
    )
  })

  observe({
    for (opt in tab4_quiz_choices) {
      local({
        val <- opt$value
        btn_id <- paste0("tab4_tile_", gsub(" ", "_", val))
        observeEvent(input[[btn_id]], {
          if (tab4_quiz_answered()) return()
          tab4_quiz_selected(val)
          tab4_quiz_answered(TRUE)
        }, ignoreInit = TRUE)
      })
    }
  })

  output$tab4_quiz_result <- renderUI({
    req(tab4_quiz_answered())
    answer <- tab4_quiz_selected()
    if (answer == "Zadna z klasycznych") {
      lc_status(
        lc_verdict(tags$strong("Dokładnie."), type = "ok"),
        " Wiersz to zdarzenie, a nie niezależna obserwacja, więc żaden
        z klasycznych testów nie pasuje do tabeli w tej postaci."
      )
    } else {
      lc_status(
        lc_verdict(tags$strong("Nie do końca."), type = "danger"),
        paste0(" ", answer, " wymaga niezależnych obserwacji i zmiennych
        mierzonych na tej samej jednostce. Tu wiersz to jedno przekleństwo
        albo jedna śmierć. Poprawna odpowiedź: „Żadna z klasycznych”.")
      )
    }
  })

  output$tab4_agg_result <- renderUI({
    req(input$tab4_aggregate > 0)
    agg <- tarantino %>%
      group_by(movie) %>%
      summarise(
        n_profanity = sum(type == "word", na.rm = TRUE),
        n_deaths = sum(type == "death", na.rm = TRUE),
        .groups = "drop"
      )

    tagList(
      dd_data_table(round_df(agg), n = 10, label = "Dane po agregacji"),
      lc_caption(paste0("Po agregacji: n = ", nrow(agg), " filmów zamiast ",
                        nrow(tarantino), " wierszy."))
    )
  })

}
