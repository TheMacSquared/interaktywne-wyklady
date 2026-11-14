# ============================================================================
# CHAPTER 7: Quiz — populacja, próba, parametr, statystyka
# ============================================================================

# Pytania w JSON: scenario, question, options (4), correct (numer od 1), explanation.
.load_quiz_questions_dane <- function() {
  json_path <- file.path(app_dir, "modules", "quiz_dane_populacja.json")
  jsonlite::fromJSON(json_path, simplifyDataFrame = FALSE)$questions
}

QUIZ_DANE_MAX_QUESTIONS <- 10

ch7_ui <- list(
  id    = "ch-quiz",
  num   = "06",
  title = "Quiz",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 06 · Dane i populacja",
      num    = "06",
      title  = "Quiz.",
      lead   = "Każde pytanie opisuje krótko jedno badanie. Wskaż populację,
                próbę, parametr, statystykę albo sposób doboru próby. Quiz
                losuje 10 pytań z puli 20; możesz go powtarzać."
    ),

    figure_panel(
      label = "Ryc. 7.1",
      title = "Quiz",
      # Quiz prowadzi przeglądarka (shared/lc_quiz.js): losowanie, ocena, wynik.
      lc_tile_quiz("ch7", .load_quiz_questions_dane(), n = QUIZ_DANE_MAX_QUESTIONS,
        summary = c(
          "90" = "Świetnie, pojęcia są opanowane.",
          "70" = "Dobry wynik.",
          "50" = "Nieźle, ale warto wrócić do rozdziałów 02–04.",
          "0"  = "Wróć do rozdziałów 02–04 i spróbuj ponownie."
        ))
    ),

    lc_p("Następny krok to wykład 01, Statystyka opisowa: rodzaje zmiennych
      i liczby, którymi streszczamy próbę.")
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch7_server <- function(input, output, session) {
  # Rozdział nie ma logiki serwera: quiz działa w przeglądarce.
}
