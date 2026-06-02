ch3_ui <- lecture_chapter(id = "ch3", num = "3", title = "Co właściwie mierzymy?", content = tagList(
  fluidRow(column(8, offset = 2,
    lc_chapter_hero(
      kicker = "Rozdział 03 · Operacjonalizacja",
      num = "03",
      title = "Co właściwie mierzymy?",
      lead = "Pojęcie z hipotezy i zmienna w danych to nie to samo. Trzeba nazwać
              różnicę między nimi, zanim zinterpretujemy wynik."
    ),

    div(class = "lc-feedback lc-feedback-info",
      tags$strong("Przypomnienie celu:"),
      p(tags$em(tr_goal)),
      p("Cel mówi o ", tags$em("jakości nauczania"), ", ale w danych mamy tylko
        ", tags$code("eval"), ". Zanim ruszymy z testami, nazwijmy tę szczelinę.")
    ),

    lc_h2("sec-01", "Pojęcie → wskaźnik → zmienna → ograniczenie"),

    div(class = "lc-prose",
      p("Każde pojęcie z naszego celu i z wiązki tropów trzeba przełożyć na
        konkretną zmienną. Po drodze coś gubimy — i właśnie to ograniczenie
        musi później wrócić we wniosku. Poniżej cztery kluczowe pojęcia naraz.")
    ),

    uiOutput("ch3_construct_maps"),

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

    lc_chapter_next("04", "Konspekt pracy badawczej",
      "Mamy cel, tropy i pomiar — teraz składamy z nich pełny konspekt przed analizą.",
      "ch4"),
    div(style = "height: 40px;")
  )))
)

ch3_server <- function(input, output, session) {
  output$ch3_construct_maps <- renderUI({
    maps <- list(
      list(name = "Jakość nauczania", cells = list(
        c("Pojęcie", "Jakość nauczania: czy zajęcia realnie pomagają studentom uczyć się."),
        c("Wskaźnik", "Ogólna ocena kursu wystawiona przez studentów."),
        c("Zmienna", "`eval`: skala 1-5."),
        c("Ograniczenie", "Może mierzyć satysfakcję, łatwość, sympatię lub oczekiwaną ocenę.")
      )),
      list(name = "Atrakcyjność", cells = list(
        c("Pojęcie", "Atrakcyjność jako możliwe źródło obciążenia ocen."),
        c("Wskaźnik", "Średnia ocena wyglądu przez panel studentów."),
        c("Zmienna", "`beauty`: wystandaryzowana ocena atrakcyjności."),
        c("Ograniczenie", "To ocena społeczna, nie obiektywna cecha osoby.")
      )),
      list(name = "Sprawiedliwość ocen", cells = list(
        c("Pojęcie", "Sprawiedliwość oceniania prowadzących."),
        c("Wskaźnik", "Porównanie ocen między grupami prowadzących."),
        c("Zmienna", "`gender`, `native`, `minority`, `tenure`."),
        c("Ograniczenie", "Przynależność do grupy to etykieta, nie pomiar samego traktowania.")
      )),
      list(name = "Reprezentatywność opinii", cells = list(
        c("Pojęcie", "Reprezentatywność opinii studentów."),
        c("Wskaźnik", "Odsetek zapisanych osób, które wypełniły ankietę."),
        c("Zmienna", "`response.rate`."),
        c("Ograniczenie", "Nie wiemy, kto nie odpowiedział i dlaczego.")
      ))
    )
    blocks <- lapply(maps, function(m) {
      cells <- lapply(m$cells, function(item) {
        div(class = "construct-cell",
          h4(item[[1]]),
          p(HTML(gsub("`([^`]+)`", "<code>\\1</code>", item[[2]])))
        )
      })
      tagList(
        tags$h4(style = "margin: 14px 0 6px 0;", m$name),
        div(class = "construct-map", cells)
      )
    })
    div(blocks)
  })

}
