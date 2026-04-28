# Tab 0: Wprowadzenie — wstęp do wykładu o jakości danych

ch0_ui <- lecture_chapter(id = "ch0", num = "0", title = "Wprowadzenie", content = tagList(
  fluidRow(column(8, offset = 2,

    lc_h2("sec-01", "Od pomysłu do danych"),

    div(class = "lc-prose",
      p("Każda analiza zaczyna się od pomysłu i pytań — jeszcze zanim otworzycie
        jakikolwiek plik. Musicie wiedzieć co chcecie zbadać i jak to opisać:
        czy szukamy związku między dwiema rzeczami? Porównujemy grupy? Sprawdzamy
        czy coś się zmienia w czasie? To nie musi być formalna hipoteza statystyczna
        — wystarczy jasny pomysł w języku potocznym."),
      p("Zachęcam do wybierania tematów, które Was naprawdę interesują.",
        " Jeśli piszecie pracę o czymś, na czym Wam zależy, naturalnie zadajecie
        lepsze pytania, szybciej wyłapujecie absurdalne wyniki, łatwiej tworzycie
        sensowne hipotezy. Analiza zyska niuans i dojrzałość, której nie da żaden
        podręcznik — bo będziecie rozumieć kontekst.")
    ),

    lc_h2("sec-02", "Drugi krok: dane"),

    div(class = "lc-prose",
      p("Kiedy macie już pomysł, trzeba znaleźć (albo zebrać) dane. I tu zaczyna się
        pierwsza pułapka: nie każdy zbiór danych nadaje się do planowanej analizy.
        Na tym wykładzie pokażę Wam na co zwracać uwagę — co dyskwalifikuje dane
        od razu, a co można naprawić.")
    ),

    div(class = "lc-feedback lc-feedback-warning",
      tags$strong("Pytanie do grupy:"),
      " Wyobraźcie sobie, że otworzyliście zbiór danych w jamovi.",
      " Na co zwracacie uwagę? Co może pójść nie tak?",
      tags$br(), tags$br(),
      tags$em("(Porozmawiajmy o tym, a potem pokażę Wam katalog typowych problemów.)")
    ),

    div(class = "lc-feedback lc-feedback-info",
      tags$strong("Plan wykładu:"),
      tags$br(),
      "1. Katalog problemów — 7 typów błędów w danych (jak wyglądają w tabeli i na wykresie)",
      tags$br(),
      "2. Case studies — 10 zbiorów do samodzielnej oceny",
      tags$br(),
      "3. Ściąga — checklist i podsumowanie"
    ),

    lc_chapter_next(
      num = "01",
      title = "Katalog problemów",
      lead = "Zobaczmy, co może pójść nie tak z danymi.",
      target_id = "ch1"
    ),

    div(style = "height: 40px;")
  ))))

ch0_server <- function(input, output, session) {

  output$intro_thermometer <- renderUI({
    n_critical <- length(input$intro_critical)
    n_fixable <- length(input$intro_fixable)
    n_total <- n_critical + n_fixable
    pct <- n_total / 9 * 100

    # Krytyczne decyduja o kolorze
    if (n_critical <= 3) {
      color <- data_bad
      label <- "Dane wymagają pracy - problemy krytyczne!"
    } else if (n_critical <= 4 || n_total <= 6) {
      color <- data_mixed
      label <- "Dane OK z zastrzeżeniami"
    } else {
      color <- data_good
      label <- "Dane gotowe do analizy!"
    }

    tagList(
      div(style = "background: var(--upwr-rule); border-radius: 10px; height: 30px; margin-top: 15px;",
        div(style = paste0("background: ", color, "; height: 30px; border-radius: 10px; width: ", pct, "%;
                            transition: width 0.3s; text-align: center; line-height: 30px; color: white; font-weight: bold;"),
          paste0(n_total, "/9")
        )
      ),
      div(style = paste0("text-align: center; margin-top: 8px; font-weight: bold; color: ", color, ";"), label),
      if (n_critical < 6 && n_fixable > 0)
        div(style = "text-align: center; margin-top: 4px; font-size: 13px; color: var(--upwr-reference);",
          "Naprawialne kryteria nie ratują krytycznych problemów!")
    )
  })
}
