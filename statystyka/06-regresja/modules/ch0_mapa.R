# ============================================================================
# CHAPTER 0: MAPA JEDNEGO, PEŁNEGO WYKŁADU
# ============================================================================

.regression_topics <- data.frame(
  order = 1:12,
  topic = c(
    "Po co model regresji?",
    "Regresja liniowa w praktyce",
    "Jak czytać output",
    "Reszty, R² i RMSE",
    "Założenia i granice predykcji",
    "Regresja wieloraka",
    "Predyktory jakościowe",
    "Pominięta zmienna i paradoks Simpsona",
    "Interakcje",
    "Porównywanie modeli",
    "Regresja logistyczna",
    "Ściąga i ćwiczenia"
  ),
  chapter = c(
    "01", "01", "01", "02", "02", "03",
    "03B", "03B", "03B", "04", "05", "06–07"
  ),
  level = c(
    "rdzeń", "rdzeń", "rdzeń", "rdzeń", "pogłębienie", "rdzeń",
    "pogłębienie", "pogłębienie", "pogłębienie", "rdzeń", "rdzeń", "rdzeń"
  ),
  minutes = c(4, 8, 7, 10, 12, 10, 10, 15, 15, 10, 12, 5),
  stringsAsFactors = FALSE
)

ch0_map_ui <- list(
  id = "ch-map",
  num = "00",
  title = "Mapa wykładu",
  duration = "5–10 min",
  content = tagList(
    lc_chapter_hero(
      kicker = "Regresja · jeden materiał, różne wybory prowadzącego",
      num = "00",
      title = "Najpierw wybierz cel zajęć.",
      lead = paste(
        "Aplikacja zawiera jeden pełny materiał. Nie ma osobnej wersji light:",
        "krótsze zajęcia oznaczają pominięcie pogłębień, a nie inną aplikację."
      )
    ),

    lc_h2("mapa-zasada", "Jedno źródło prawdy"),

    p(
      "Kręgosłup prowadzi od pytania i modelu liniowego, przez czytanie outputu,",
      "jakość dopasowania i model wieloraki, aż do porównania modeli oraz",
      "regresji logistycznej. Pingwiny pojawiają się tylko tam, gdzie naturalne",
      "grupy szczególnie dobrze pokazują kontekst, zmienne jakościowe i interakcje."
    ),

    lc_feedback(
      type = "info",
      tags$strong("Jak korzystać:"),
      " na zajęciach wybieraj rozdziały i sekcje według celu. Materiał oznaczony",
      " jako pogłębienie można ominąć bez utraty głównej historii."
    ),

    lc_h2("mapa-plan", "Planowanie konkretnego spotkania"),

    figure_panel(
      label = "Narzędzie prowadzącego",
      title = "Co zmieści się w dostępnym czasie?",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          radioButtons(
            "ch0_available_time",
            "Czas dostępny na dane spotkanie:",
            choices = c(
              "około 45 minut" = "45",
              "około 90 minut" = "90",
              "kilka spotkań — pełny materiał" = "full"
            ),
            selected = "90"
          ),
          lc_feedback(
            type = "warning",
            tags$strong("To nie są wersje aplikacji."),
            " Lista jedynie pomaga podjąć decyzję, które elementy omówić teraz,",
            " a do których wrócić później."
          )
        ),
        column(
          8,
          tableOutput("ch0_route_table"),
          uiOutput("ch0_route_note")
        )
      )
    ),

    lc_h2("mapa-przypadki", "Dwa przypadki, dwie funkcje"),

    tags$table(
      class = "lc-table lc-table-bordered lc-table-striped",
      tags$thead(tags$tr(
        tags$th("Przypadek"),
        tags$th("Najlepiej pokazuje"),
        tags$th("Dlaczego pozostaje w kursie")
      )),
      tags$tbody(
        tags$tr(
          tags$td("CASchools"),
          tags$td("regresję prostą, diagnostykę, model wieloraki i kontekst społeczny"),
          tags$td("wynik wymaga ostrożnej interpretacji i dobrze otwiera rozmowę o przyczynowości")
        ),
        tags$tr(
          tags$td("Palmer Penguins"),
          tags$td("predyktory jakościowe, paradoks Simpsona i interakcje"),
          tags$td("trzy naturalne grupy tworzą czytelny mechanizm wizualny")
        )
      )
    ),

    lc_chapter_next(
      num = "01",
      title = "Regresja liniowa",
      lead = "Zaczynamy od pytania, prostej i interpretacji współczynników.",
      target_id = "ch-1"
    )
  )
)

ch0_map_server <- function(input, output, session) {
  selected_topics <- reactive({
    if (identical(input$ch0_available_time, "full")) {
      return(.regression_topics)
    }

    available <- as.numeric(input$ch0_available_time)
    # Zostawiamy około 15 minut w spotkaniu 90-minutowym i około 8 minut
    # w spotkaniu 45-minutowym na pracę w jamovi, pytania oraz dyskusję.
    budget <- if (available >= 90) 75 else 37
    topics <- .regression_topics

    # Najpierw rdzeń w kolejności narracji, potem pogłębienia, jeśli zostaje czas.
    priority <- order(topics$level != "rdzeń", topics$order)
    topics <- topics[priority, , drop = FALSE]
    topics$include <- cumsum(topics$minutes) <= budget
    topics[order(topics$order), , drop = FALSE]
  })

  output$ch0_route_table <- renderTable({
    topics <- selected_topics()
    if (!"include" %in% names(topics)) topics$include <- TRUE
    data.frame(
      Nr = topics$order,
      Temat = topics$topic,
      Rozdział = topics$chapter,
      Poziom = topics$level,
      `Na to spotkanie` = ifelse(topics$include, "omów", "zostaw na później"),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$ch0_route_note <- renderUI({
    topics <- selected_topics()
    if (!"include" %in% names(topics)) {
      return(lc_feedback(
        type = "ok",
        tags$strong("Pełna ścieżka:"),
        paste(" około", sum(topics$minutes), "minut materiału bez ćwiczeń i dyskusji.")
      ))
    }
    used <- sum(topics$minutes[topics$include])
    lc_feedback(
      type = "ok",
      tags$strong("Sugerowany rdzeń:"),
      paste(" około", used, "minut narracji; pozostały czas zostaw na przykład w jamovi i pytania.")
    )
  })
}
