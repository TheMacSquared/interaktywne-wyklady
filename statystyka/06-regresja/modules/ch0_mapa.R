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

    lc_h2("mapa-tematy", "Mapa tematów"),

    tags$table(
      class = "lc-table lc-table-bordered lc-table-striped",
      tags$thead(tags$tr(
        tags$th("Nr"),
        tags$th("Temat"),
        tags$th("Rozdział"),
        tags$th("Poziom")
      )),
      tags$tbody(lapply(seq_len(nrow(.regression_topics)), function(index) {
        tags$tr(
          tags$td(.regression_topics$order[[index]]),
          tags$td(.regression_topics$topic[[index]]),
          tags$td(.regression_topics$chapter[[index]]),
          tags$td(.regression_topics$level[[index]])
        )
      }))
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
  # Rozdział jest statyczną mapą — nie ma elementów reaktywnych.
  invisible(NULL)
}
