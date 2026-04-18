# Tab 10: Studenci — wzorcowa ankieta studencka, dobry zbiór

ch10_ui <- tabPanel("10. Studenci",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Ankieta studencka"),

    div(class = "narrative",
      p("Wyobraź sobie, że projektujesz ankietę do projektu końcowego.
        Oto przykład dobrze zaprojektowanej ankiety z 150 respondentami.")
    ),

    div(class = "section-title", "Podgląd danych"),

    div(class = "widget-block",
      DT::dataTableOutput("tab9_table")
    ),

    div(class = "callout-info",
      tags$strong("Zmienne i ich typy:"),
      tags$br(),
      "plec (nominalna) | kierunek (nominalna) | rok_studiow (porządkowa)",
      tags$br(),
      "godziny_nauki (ciągła) | stres (porządkowa/Likert 1-10) | srednia_ocen (ciągła) | liczba_kursow (dyskretna)"
    ),

    div(class = "section-title", "Werdykt"),

    div(class = "callout-success",
      tags$strong("Wzorcowa ankieta!"),
      tags$br(),
      "Zamknięte pytania, spójne skale, jasne kodowanie.",
      tags$br(),
      "n = 150, mix typów zmiennych, każda analiza z kursu jest możliwa.",
      tags$br(),
      tags$em("Porównaj z Trudną ankietą (tab 7) - te same tematy, ale świat różnic w jakości!")
    ),

    uiOutput("tab9_verdict"),

    div(class = "chapter-transition",
      p("Ostatni zbiór - wygląda dobrze, ale ma ukryty problem..."),
      actionButton("ch9_next", "Dalej: 11. Jakość powietrza \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  )))

ch10_server <- function(input, output, session) {

  output$tab9_table <- DT::renderDataTable({
    datatable(round_df(survey_data), options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  output$tab9_verdict <- renderUI({
    # hipoteza, n, mix, zmiennosc, struktura, niezaleznosc | braki, definicje, bledy
    render_verdict(rep("yes", 9), "good")
  })
}
