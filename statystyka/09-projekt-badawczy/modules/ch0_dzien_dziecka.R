ch0_ui <- lecture_chapter(id = "ch0", num = "0", title = "Dzień Dziecka", content = tagList(
  fluidRow(column(8, offset = 2,
    lc_chapter_hero(
      kicker = "Rozdział 00 · Dzień Dziecka",
      num = "00",
      title = "Dzień Dziecka.",
      lead = "Z okazji Dnia Dziecka uruchamiamy specjalne losowanie nagród
              dydaktycznych. Regulamin jest prosty: klikamy, wierzymy w los,
              a potem sprawdzamy, co los wie o statystyce."
    ),

    lc_h2("sec-00", "Losowanie nagrody"),

    div(class = "lc-prose",
      p("Na kole są same znakomite możliwości: punkty z aktywności,
        dodatkowe punkty, zaliczenia i inne powody do radości. Jest też
        jedno zupełnie niewinne pole kontrolne. Proszę nacisnąć przycisk
        i zachować naukowy spokój.")
    ),

    div(class = "child-wheel-panel",
      div(class = "child-wheel-stage",
        div(class = "child-wheel-pointer", `aria-hidden` = "true"),
        div(
          class = "child-wheel",
          id = "child-wheel",
          `aria-label` = "Dzień Dziecka z nagrodami",
          div(class = "child-wheel-label child-wheel-label-0", "kartkówka"),
          div(class = "child-wheel-label child-wheel-label-1", "1 pkt aktywności"),
          div(class = "child-wheel-label child-wheel-label-2", "2 pkt aktywności"),
          div(class = "child-wheel-label child-wheel-label-3", "zaliczenie"),
          div(class = "child-wheel-label child-wheel-label-4", "1 pkt"),
          div(class = "child-wheel-label child-wheel-label-5", "2 pkt"),
          div(class = "child-wheel-label child-wheel-label-6", "mniejszy projekt"),
          div(class = "child-wheel-label child-wheel-label-7", "zaliczenie"),
          div(class = "child-wheel-label child-wheel-label-8", "bonus za obecność"),
          div(class = "child-wheel-label child-wheel-label-9", "uśmiech prowadzącego"),
          div(class = "child-wheel-hub", "0")
        )
      ),
      div(class = "child-wheel-controls",
        actionButton("child_spin_button", "Zakręć kołem", class = "child-spin-button"),
        div(
          class = "child-wheel-result",
          id = "child-wheel-result",
          role = "status",
          `aria-live` = "polite",
          "Koło czeka na pierwszy eksperyment."
        )
      )
    ),

    div(class = "lc-feedback lc-feedback-info child-wheel-note",
      tags$strong("Hipoteza przed losowaniem:"),
      p("Nagrody są przydzielane losowo, niezależnie i z jednakowym prawdopodobieństwem.
        To piękna hipoteza. Bardzo szkoda, że zaraz spotka się z danymi.")
    ),

    lc_chapter_next("01", "Od ciekawości do celu",
      "Po rozgrzewce wracamy do prawdziwego projektu badawczego.",
      "ch1"),
    div(style = "height: 40px;")
  )))
)

ch0_server <- function(input, output, session) {
}
