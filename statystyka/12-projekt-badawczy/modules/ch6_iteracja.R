ch6_ui <- lecture_chapter(id = "ch6", num = "6", title = "Wynik nie kończy badania", content = tagList(
  fluidRow(column(8, offset = 2,
    lc_chapter_hero(
      kicker = "Rozdział 06 · Iteracja",
      num = "06",
      title = "Wynik nie kończy badania.",
      lead = "Mamy pełną tablicę tropów. Teraz najważniejsze pytanie wykładu:
              co cała wiązka mówi o naszym celu — i co robimy dalej?"
    ),

    div(class = "lc-feedback lc-feedback-info",
      tags$strong("Przypomnienie celu:"),
      p(tags$em(tr_goal))
    ),

    lc_h2("sec-01", "Cała wiązka naraz: co wiemy o celu?"),

    div(class = "lc-prose",
      p("Pojedynczy wynik nie odpowiada na cel. Dopiero zebrane razem tropy
        zaczynają coś mówić: część się wzmocniła, część osłabła. To jest moment,
        w którym z osobnych testów składamy jeden obraz.")
    ),

    div(class = "lc-figure-panel",
      h4("Tablica tropów — pełny obraz"),
      tr_board_ui(reveal = tr_trop_order, show_verdict = TRUE)
    ),

    uiOutput("ch6_goal_readout"),

    lc_h2("sec-02", "Co robimy z każdym tropem?"),

    div(class = "lc-prose",
      p("Werdykt „wzmocniony\" nie znaczy „udowodniony\", a „osłabiony\" nie znaczy
        „temat zamknięty\". Każdy trop generuje następne pytania — poniżej dla
        całej wiązki naraz.")
    ),

    uiOutput("ch6_next_steps"),

    lc_h2("sec-03", "Czego brakuje w danych?"),

    div(class = "lc-prose",
      p("Wiązka pokazała też, czego nie ma w danych. To nie porażka — to lista
        zakupów do lepszego badania. Tu analiza danych przechodzi w projektowanie
        badania.")
    ),

    div(class = "lc-feedback lc-feedback-info",
      tags$strong("Brakujące elementy, które najbardziej zmieniłyby interpretację:"),
      tags$ul(
        tags$li("Efekty uczenia się przed i po kursie — czy studenci faktycznie się nauczyli."),
        tags$li("Trudność kursu i obciążenie pracą."),
        tags$li("Oczekiwana ocena / łatwość zaliczenia."),
        tags$li("Obowiązkowość kursu."),
        tags$li("Styl prowadzenia i jakość materiałów."),
        tags$li("Powody braku odpowiedzi w ankiecie.")
      ),
      p("Zadanie dla grupy: wybierzcie jeden brak, który najmocniej podważyłby
        wasz obecny odczyt celu.")
    ),

    div(class = "lc-feedback lc-feedback-warning",
      tags$strong("Zdanie badacza:"),
      p(tags$em("\"Ten wynik nie kończy tematu. On mówi nam, które kolejne pytanie
        jest teraz najbardziej sensowne.\""))
    ),

    lc_chapter_next("07", "Jak zaprojektować lepsze badanie?",
      "Skoro dane obserwacyjne mają ograniczenia, projektujemy mocniejszy kolejny krok.",
      "ch_projekt"),
    div(style = "height: 40px;")
  )))
)

ch6_server <- function(input, output, session) {
  output$ch6_goal_readout <- renderUI({
    supported <- tr_board_summary$short[tr_board_summary$supported]
    weak      <- tr_board_summary$short[!tr_board_summary$supported]
    fmt <- function(x) if (length(x) == 0) "—" else paste(x, collapse = ", ")
    div(class = "lc-feedback lc-feedback-warning",
      tags$strong("Co tablica mówi o celu:"),
      p("Tropy wzmocnione przez dane: ", tags$strong(fmt(supported)), "."),
      p("Tropy osłabione: ", tags$strong(fmt(weak)), "."),
      p("Żaden pojedynczy trop nie rozstrzyga, czy `eval` mierzy jakość. Ale wiązka
        pokazuje, że w ocenie z ankiety siedzi więcej niż sama jakość nauczania —
        i że trzeba sprawdzić te tropy jednocześnie, w jednym modelu.")
    )
  })

  output$ch6_next_steps <- renderUI({
    cases <- list(
      beauty = c(
        "Czy związek zostaje po uwzględnieniu płci, wieku i typu kursu?",
        "Czy beauty jest proxy czegoś innego: wieku, pewności siebie, stylu prowadzenia?",
        "Jak zaprojektować badanie, które oddzieli wygląd od jakości materiałów?"
      ),
      gender = c(
        "Czy kobiety i mężczyźni prowadzą podobne typy kursów?",
        "Czy różnica wygląda tak samo dla niższych i wyższych kursów?",
        "Czy response rate różni się między tymi grupami?"
      ),
      native = c(
        "Czy native speakerzy prowadzą inne kursy niż pozostali?",
        "Czy studenci oceniają jakość nauczania, czy łatwość komunikacji?",
        "Jak zebrać dane o języku prowadzenia, jasności wyjaśnień i typie zajęć?"
      ),
      minority = c(
        "Czy grupa minority jest wystarczająco liczna na stabilny wynik?",
        "Czy różnice ujawniają się tylko w wybranych typach kursów?",
        "Czy potrzebujemy lepszego pomiaru doświadczeń prowadzących i studentów?"
      ),
      response = c(
        "Czy kursy z niskim response rate są większe albo trudniejsze?",
        "Czy bardziej niezadowoleni studenci chętniej odpowiadają?",
        "Jak w projekcie zadbać o reprezentatywność ankiety?"
      )
    )
    cards <- lapply(tr_trop_order, function(id) {
      tr  <- tr_tropy[[id]]
      row <- tr_board_row(id)
      badge_cls <- if (row$supported) "tropy-verdict tropy-verdict-on"
                   else "tropy-verdict tropy-verdict-off"
      div(class = "trop-card",
        h4(tr$short, " ", tags$span(class = badge_cls, row$verdict)),
        p(tags$strong("Następne pytania:")),
        tags$ul(class = "trop-alt", lapply(cases[[id]], tags$li))
      )
    })
    div(class = "trop-stack", cards)
  })
}
