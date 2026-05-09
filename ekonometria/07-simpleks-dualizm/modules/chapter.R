ch1_ui <- lecture_chapter(
  id = "ch-simpleks",
  num = "01",
  title = "Idea simpleksu",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdzial 07 · Simpleks",
      num = "01",
      title = "Wierzcholki zamiast calego obszaru.",
      lead = "Metoda simpleks przechodzi miedzy rozwiazaniami bazowymi, szukajac coraz lepszej wartosci funkcji celu."
    ),
    lc_h2("intuicja", "Intuicja"),
    lc_p("W liniowym zadaniu maksimum, jesli istnieje skonczone optimum, znajduje sie ono w jednym z wierzcholkow obszaru dopuszczalnego. Simpleks jest systematycznym sposobem przechodzenia po takich wierzcholkach."),
    lc_chapter_next("02", "Tablica simpleksowa", "startowy placeholder pod rachunki", "ch-tablica")
  )
)

ch2_ui <- lecture_chapter(
  id = "ch-tablica",
  num = "02",
  title = "Tablica simpleksowa",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdzial 07 · Simpleks",
      num = "02",
      title = "Pierwsza tablica.",
      lead = "Na razie pokazujemy strukture tablicy i wybor zmiennej wchodzacej. Rachunki mozna pozniej rozbudowac iteracyjnie."
    ),
    figure_panel(
      label = "Tabela 7.1",
      title = "Schemat tablicy",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("c1", "Wspolczynnik celu przy x1", 1, 12, 6, step = 1),
          sliderInput("c2", "Wspolczynnik celu przy x2", 1, 12, 4, step = 1)
        ),
        column(8, uiOutput("simplex_table"), uiOutput("simplex_hint"))
      )
    ),
    lc_chapter_next("03", "Dualizm", "interpretacja cen dualnych", "ch-dualizm")
  )
)

ch3_ui <- lecture_chapter(
  id = "ch-dualizm",
  num = "03",
  title = "Dualizm",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdzial 07 · Dualizm",
      num = "03",
      title = "Problem primalny i dualny.",
      lead = "Dualizm pozwala patrzec na ograniczenia jak na zasoby z cenami granicznymi."
    ),
    lc_formula_box(
      withMathJax(helpText("$$\\max\\; c^Tx \\quad przy \\quad Ax \\le b,\\; x \\ge 0$$")),
      withMathJax(helpText("$$\\min\\; b^Ty \\quad przy \\quad A^Ty \\ge c,\\; y \\ge 0$$"))
    ),
    inline_callout("Do rozbudowy", color = "wskazowka", open = TRUE,
      "Dobry kolejny widget: zmiana zasobu b i pokazanie, kiedy cena dualna opisuje przyrost funkcji celu.")
  )
)

chapter_server <- function(input, output, session) {
  output$simplex_table <- renderUI({
    entering <- if (input$c1 >= input$c2) "x1" else "x2"
    tags$table(
      class = "table",
      tags$thead(tags$tr(
        tags$th("Baza"), tags$th("x1"), tags$th("x2"), tags$th("s1"), tags$th("s2"), tags$th("RHS")
      )),
      tags$tbody(
        tags$tr(tags$td("s1"), tags$td(2), tags$td(1), tags$td(1), tags$td(0), tags$td(100)),
        tags$tr(tags$td("s2"), tags$td(1), tags$td(2), tags$td(0), tags$td(1), tags$td(90)),
        tags$tr(tags$td("z"), tags$td(-input$c1), tags$td(-input$c2), tags$td(0), tags$td(0), tags$td(0))
      ),
      tags$caption(paste("Kandydat na zmienna wchodzaca:", entering))
    )
  })

  output$simplex_hint <- renderUI({
    entering <- if (input$c1 >= input$c2) "x1" else "x2"
    lc_feedback(type = "info",
      p("W wersji startowej wybieramy kolumne z najbardziej ujemnym wspolczynnikiem w wierszu z."),
      p(tags$strong("Zmienna wchodzaca: "), entering)
    )
  })
}
