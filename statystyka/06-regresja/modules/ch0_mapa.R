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
      "jakość dopasowania i model wieloraki, aż do porównania modeli oraz ",
      gloss("regresja logistyczna", "regresji logistycznej"),
      ". Pingwiny pojawiają się tylko tam, gdzie naturalne",
      "grupy szczególnie dobrze pokazują kontekst, ",
      gloss("zmienna jakościowa", "zmienne jakościowe"),
      " i interakcje."
    ),

    lc_note("Jak korzystać",
      "Na zajęciach wybieraj rozdziały i sekcje według celu. Materiał oznaczony",
      " jako pogłębienie można ominąć bez utraty głównej historii."
    ),

    lc_h2("mapa-tematy", "Mapa tematów"),

    lc_table(.regression_topics[, c("order", "topic", "chapter", "level")],
      cols = list(
        lc_col("order", "Nr", "row"),
        lc_col("topic", "Temat", "text"),
        lc_col("chapter", "Rozdział", "text"),
        lc_col("level", "Poziom", "text")
      ),
      narrow = "cards",
      prose = TRUE
    ),

    lc_h2("mapa-przypadki", "Dwa przypadki, dwie funkcje"),

    lc_table(
      data.frame(
        c1 = c("CASchools", "Palmer Penguins"),
        c2 = c(
          "regresję prostą, diagnostykę, model wieloraki i kontekst społeczny",
          "predyktory jakościowe, paradoks Simpsona i interakcje"
        ),
        c3 = c(
          "wynik wymaga ostrożnej interpretacji i dobrze otwiera rozmowę o przyczynowości",
          "trzy naturalne grupy tworzą czytelny mechanizm wizualny"
        )
      ),
      cols = list(
        lc_col("c1", "Przypadek", "row"),
        lc_col("c2", "Najlepiej pokazuje", "text"),
        lc_col("c3", "Dlaczego pozostaje w kursie", "text")
      ),
      narrow = "cards",
      prose = TRUE
    ),

    lc_chapter_next(
      num = "01",
      title = "Regresja liniowa",
      lead = "Zaczynamy od pytania, prostej i interpretacji współczynników.",
      target_id = "ch-liniowa"
    )
  )
)

ch0_map_server <- function(input, output, session) {
  # Rozdział jest statyczną mapą — nie ma elementów reaktywnych.
  invisible(NULL)
}
