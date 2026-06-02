ch8_ui <- lecture_chapter(id = "ch8", num = "8", title = "Od konspektu do wniosku", content = tagList(
  fluidRow(column(8, offset = 2,
    lc_chapter_hero(
      kicker = "Rozdział 08 · Domknięcie projektu",
      num = "08",
      title = "Od konspektu do wniosku.",
      lead = "Konspekt powstał przed analizą. Po sprawdzeniach wracamy do niego
              i dopisujemy, co dane zmieniły w interpretacji celu."
    ),

    div(class = "lc-feedback lc-feedback-info",
      tags$strong("Cel, który ciągnęliśmy przez cały wykład:"),
      p(tags$em(tr_goal))
    ),

    lc_h2("sec-01", "Co dopisać po analizie?"),

    div(class = "lc-prose",
      p("Po pierwszych sprawdzeniach konspekt nie znika. Staje się szkieletem
        raportu: przy każdym tropie dopisujemy wynik, interpretację, ograniczenie
        i pytanie, które wynika z analizy.")
    ),

    div(class = "proposal-skeleton",
      div(class = "proposal-step",
        span(class = "proposal-step-num", "1"),
        div(
          h4("Wyniki przy tropach"),
          p("Dopisz, które tropy dane wzmacniają, a które osłabiają. Nie zmieniaj
            celu tylko dlatego, że jeden wynik jest ciekawy."),
          div(class = "proposal-example",
            p(tags$strong("U nas: "), "część tropów pomaga interpretować ", tags$code("eval"),
              ", ale żaden pojedynczy wynik nie rozstrzyga całego celu.")
          )
        )
      ),
      div(class = "proposal-step",
        span(class = "proposal-step-num", "2"),
        div(
          h4("Interpretacja celu"),
          p("Zbierz tropy razem i napisz, co cała wiązka mówi o głównym pytaniu
            badawczym."),
          div(class = "proposal-example",
            p(tags$strong("U nas: "), "ocena z ankiety wygląda raczej jak wskaźnik
              mieszany niż czysta miara jakości nauczania.")
          )
        )
      ),
      div(class = "proposal-step",
        span(class = "proposal-step-num", "3"),
        div(
          h4("Ograniczenia"),
          p("Nazwij, czego dane nie pozwalają stwierdzić. To część jakości projektu,
            nie porażka analizy."),
          div(class = "proposal-example",
            p(tags$strong("U nas: "), "dane obserwacyjne pokazują współwystępowanie,
              ale nie pozwalają rozstrzygnąć przyczynowości.")
          )
        )
      ),
      div(class = "proposal-step",
        span(class = "proposal-step-num", "4"),
        div(
          h4("Następny krok"),
          p("Dopisz, jak rozwinąć projekt: jakie dane, pomiary albo porównania
            byłyby potrzebne po pierwszej analizie."),
          div(class = "proposal-example",
            p(tags$strong("U nas: "), "przydałyby się komentarze z ankiet, dane o typie zajęć
              i lepszy pomiar jakości nauczania.")
          )
        )
      )
    ),

    div(class = "lc-feedback lc-feedback-warning",
      tags$strong("Zdanie, które warto mieć w raporcie:"),
      p(tags$em("\"Nasze dane wspierają interpretację X, ale nie pozwalają
        rozstrzygnąć przyczynowości, ponieważ...\"")),
      p("To nie osłabia pracy. To pokazuje, że autorzy rozumieją granice własnego badania.")
    ),

    div(class = "lc-figure-panel",
      h4("Tak wygląda domknięty projekt: cel + wiązka + werdykty"),
      div(class = "lc-prose",
        p("Cała droga w jednym kadrze: cel badawczy, tropy, dane i ostrożne werdykty.
          Wasz raport ma wracać do konspektu, który powstał przed analizą.")
      ),
      tr_board_ui(reveal = tr_trop_order, show_verdict = TRUE)
    ),

    div(style = "height: 40px;")
  )))
)

ch8_server <- function(input, output, session) {
}
