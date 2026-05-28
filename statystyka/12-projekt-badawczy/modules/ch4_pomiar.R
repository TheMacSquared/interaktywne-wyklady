ch4_ui <- lecture_chapter(id = "ch4", num = "4", title = "Co właściwie mierzymy?", content = tagList(
  fluidRow(column(8, offset = 2,
    lc_chapter_hero(
      kicker = "Rozdział 04 · Operacjonalizacja",
      num = "04",
      title = "Co właściwie mierzymy?",
      lead = "Między pojęciem a kolumną w danych zawsze jest szczelina.
              Dobra analiza umie ją nazwać."
    ),

    lc_h2("sec-01", "Pojęcie → wskaźnik → zmienna → ograniczenie"),

    div(class = "lc-figure-panel",
      h4("Mapa pomiaru"),
      selectInput("ch4_construct", "Wybierz pojęcie:",
        choices = c(
          "Jakość nauczania" = "quality",
          "Atrakcyjność" = "beauty",
          "Sprawiedliwość ocen" = "fairness",
          "Reprezentatywność opinii" = "response"
        ),
        selected = "quality"
      ),
      uiOutput("ch4_construct_map")
    ),

    div(class = "lc-feedback lc-feedback-info",
      tags$strong("Ważne rozróżnienie:"),
      p("Możemy testować różnice w `eval`, ale `eval` nie jest tym samym co
        jakość nauczania. Wniosek musi mówić językiem zmiennych, a nie tylko
        językiem naszych ambicji badawczych.")
    ),

    tr_discussion_box("Pytania kontrolne do własnych projektów:",
      tags$li("Czy zmienna naprawdę mierzy to, o czym mówi hipoteza?"),
      tags$li("Kto i kiedy wykonał pomiar?"),
      tags$li("Jakie zjawisko mogłoby dać taki sam wynik w danych?")
    ),

    lc_chapter_next("05", "Pierwsze sprawdzenia w danych",
      "Dopiero teraz wybieramy testy i wykresy, bo wiemy już, co próbujemy sprawdzić.",
      "ch5"),
    div(style = "height: 40px;")
  )))
)

ch4_server <- function(input, output, session) {
  output$ch4_construct_map <- renderUI({
    maps <- list(
      quality = list(
        c("Pojęcie", "Jakość nauczania: czy zajęcia realnie pomagają studentom uczyć się."),
        c("Wskaźnik", "Ogólna ocena kursu wystawiona przez studentów."),
        c("Zmienna", "`eval`: skala 1-5."),
        c("Ograniczenie", "Może mierzyć satysfakcję, łatwość, sympatię lub oczekiwaną ocenę.")
      ),
      beauty = list(
        c("Pojęcie", "Atrakcyjność jako możliwe źródło obciążenia ocen."),
        c("Wskaźnik", "Średnia ocena wyglądu przez panel studentów."),
        c("Zmienna", "`beauty`: wystandaryzowana ocena atrakcyjności."),
        c("Ograniczenie", "To ocena społeczna, nie obiektywna cecha osoby.")
      ),
      fairness = list(
        c("Pojęcie", "Sprawiedliwość oceniania prowadzących."),
        c("Wskaźnik", "Porównanie ocen między grupami prowadzących."),
        c("Zmienna", "`gender`, `native`, `minority`, `tenure`."),
        c("Ograniczenie", "Różnice grupowe nie wyjaśniają automatycznie mechanizmu.")
      ),
      response = list(
        c("Pojęcie", "Reprezentatywność opinii studentów."),
        c("Wskaźnik", "Odsetek zapisanych osób, które wypełniły ankietę."),
        c("Zmienna", "`response.rate`."),
        c("Ograniczenie", "Nie wiemy, kto nie odpowiedział i dlaczego.")
      )
    )
    cells <- lapply(maps[[input$ch4_construct]], function(item) {
      div(class = "construct-cell",
        h4(item[[1]]),
        p(HTML(gsub("`([^`]+)`", "<code>\\1</code>", item[[2]])))
      )
    })
    div(class = "construct-map", cells)
  })
}
