# Tab 10: Studenci — wzorcowa ankieta studencka, dobry zbiór

ch10_ui <- lecture_chapter(id = "ch10", num = "10", title = "Studenci", content = tagList(
  fluidRow(column(8, offset = 2,

    lc_h2("sec-01", "Ankieta studencka"),

    div(class = "lc-prose",
      p("Wyobraź sobie, że projektujesz ankietę do projektu końcowego.
        Oto przykład dobrze zaprojektowanej ankiety z 150 respondentami.")
    ),

    lc_h2("sec-02", "Podgląd danych"),

    div(class = "lc-figure-panel",
      DT::dataTableOutput("tab9_table")
    ),

    div(class = "lc-feedback lc-feedback-info",
      tags$strong("Zmienne i ich typy:"),
      tags$br(),
      "plec (nominalna) | kierunek (nominalna) | rok_studiow (porządkowa)",
      tags$br(),
      "godziny_nauki (ciągła) | stres (porządkowa/Likert 1-10) | srednia_ocen (ciągła) | liczba_kursow (dyskretna)"
    ),

    lc_h2("sec-03", "Werdykt"),

    div(class = "lc-feedback lc-feedback-ok",
      tags$strong("Wzorcowa ankieta!"),
      tags$br(),
      "Zamknięte pytania, spójne skale, jasne kodowanie.",
      tags$br(),
      "n = 150, mix typów zmiennych, każda analiza z kursu jest możliwa.",
      tags$br(),
      tags$em("Porównaj z Trudną ankietą (tab 7) - te same tematy, ale świat różnic w jakości!")
    ),

    lc_chapter_next(
      num = "11",
      title = "Jakość powietrza",
      lead = "Ostatni zbiór - wygląda dobrze, ale ma ukryty problem...",
      target_id = "ch11"
    ),

    div(style = "height: 40px;")
  ))))

ch10_server <- function(input, output, session) {

  output$tab9_table <- DT::renderDataTable({
    datatable(round_df(survey_data), options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

}
