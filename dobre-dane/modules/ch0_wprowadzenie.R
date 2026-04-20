# Tab 0: Wprowadzenie — wstęp do wykładu o jakości danych

ch0_ui <- tabPanel("0. Wprowadzenie",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Od pomysłu do danych"),

    div(class = "narrative",
      p("Każda analiza zaczyna się od pomysłu i pytań \u2014 jeszcze zanim otworzycie
        jakikolwiek plik. Musicie wiedzieć co chcecie zbadać i jak to opisać:
        czy szukamy związku między dwiema rzeczami? Porównujemy grupy? Sprawdzamy
        czy coś się zmienia w czasie? To nie musi być formalna hipoteza statystyczna
        \u2014 wystarczy jasny pomysł w języku potocznym."),
      p("Zachęcam do wybierania tematów, które Was naprawdę interesują.",
        " Jeśli piszecie pracę o czymś, na czym Wam zależy, naturalnie zadajecie
        lepsze pytania, szybciej wyłapujecie absurdalne wyniki, łatwiej tworzycie
        sensowne hipotezy. Analiza zyska niuans i dojrzałość, której nie da żaden
        podręcznik \u2014 bo będziecie rozumieć kontekst.")
    ),

    div(class = "section-title", "Drugi krok: dane"),

    div(class = "narrative",
      p("Kiedy macie już pomysł, trzeba znaleźć (albo zebrać) dane. I tu zaczyna się
        pierwsza pułapka: nie każdy zbiór danych nadaje się do planowanej analizy.
        Na tym wykładzie pokażę Wam na co zwracać uwagę \u2014 co dyskwalifikuje dane
        od razu, a co można naprawić.")
    ),

    div(class = "callout-warning",
      tags$strong("Pytanie do grupy:"),
      " Wyobraźcie sobie, że otworzyliście zbiór danych w jamovi.",
      " Na co zwracacie uwagę? Co może pójść nie tak?",
      tags$br(), tags$br(),
      tags$em("(Porozmawiajmy o tym, a potem pokażę Wam katalog typowych problemów.)")
    ),

    div(class = "callout-info",
      tags$strong("Plan wykładu:"),
      tags$br(),
      "1. Katalog problemów \u2014 7 typów błędów w danych (jak wyglądają w tabeli i na wykresie)",
      tags$br(),
      "2. Case studies \u2014 10 zbiorów do samodzielnej oceny",
      tags$br(),
      "3. Ściąga \u2014 checklist i podsumowanie"
    ),

    div(class = "chapter-transition",
      p("Zobaczmy, co może pójść nie tak z danymi."),
      actionButton("ch0_next", "Dalej: 1. Katalog problemów \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  )))

ch0_server <- function(input, output, session) {

  output$intro_thermometer <- renderUI({
    n_critical <- length(input$intro_critical)
    n_fixable <- length(input$intro_fixable)
    n_total <- n_critical + n_fixable
    pct <- n_total / 9 * 100

    # Krytyczne decyduja o kolorze
    if (n_critical <= 3) {
      color <- col_bad
      label <- "Dane wymagają pracy - problemy krytyczne!"
    } else if (n_critical <= 4 || n_total <= 6) {
      color <- col_mixed
      label <- "Dane OK z zastrzeżeniami"
    } else {
      color <- col_good
      label <- "Dane gotowe do analizy!"
    }

    tagList(
      div(style = "background: #ecf0f1; border-radius: 10px; height: 30px; margin-top: 15px;",
        div(style = paste0("background: ", color, "; height: 30px; border-radius: 10px; width: ", pct, "%;
                            transition: width 0.3s; text-align: center; line-height: 30px; color: white; font-weight: bold;"),
          paste0(n_total, "/9")
        )
      ),
      div(style = paste0("text-align: center; margin-top: 8px; font-weight: bold; color: ", color, ";"), label),
      if (n_critical < 6 && n_fixable > 0)
        div(style = "text-align: center; margin-top: 4px; font-size: 13px; color: #7f8c8d;",
          "Naprawialne kryteria nie ratują krytycznych problemów!")
    )
  })
}
