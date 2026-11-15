# ============================================================================
# CHAPTER 7: Quiz - rozpoznaj typ zmiennej
# ============================================================================

# Wczytaj pytania z JSON
.load_quiz_questions_typy <- function() {
  json_path <- file.path(app_dir, "modules", "quiz_typy_zmiennych.json")
  jsonlite::fromJSON(json_path, simplifyDataFrame = FALSE)$questions
}

# Opcje (labele) dla typow zmiennych — zawsze te same 4
QUIZ_TYPE_OPTIONS <- list(
  ilosciowa_ciagla    = "Ilościowa ciągła",
  ilosciowa_dyskretna = "Ilościowa dyskretna",
  jakosciowa_porzadkowa = "Jakościowa porządkowa",
  jakosciowa_nominalna  = "Jakościowa nominalna"
)

QUIZ_TYPE_COLORS <- list(
  ilosciowa_ciagla      = type_colors["ilosciowa_ciagla"],
  ilosciowa_dyskretna   = type_colors["ilosciowa_dyskretna"],
  jakosciowa_porzadkowa = type_colors["porzadkowa"],
  jakosciowa_nominalna  = type_colors["nominalna"]
)

QUIZ_TYPE_MAX_QUESTIONS <- 15

# ============================================================================
# UI
# ============================================================================

ch7_ui <- list(
  id = "ch-quiz", num = "06", title = "Quiz",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 06 · Statystyka opisowa",
      num    = "06",
      title  = "Quiz.",
      lead   = "Przeczytaj opis zmiennej i wybierz jej typ. Każde pytanie ma
                cztery opcje — dokładnie jedną poprawną. Quiz losuje 15 pytań
                z puli 75; można go powtarzać wielokrotnie."
    ),

    # Legenda typow
    figure_panel(
      label = "Ryc. 7.1",
      title = "Przypomnienie typów zmiennych",
      # Kolor typu jak na kafelkach odpowiedzi.
      tags$ul(
        lapply(list(
          c("nominalna", "Nominalna:", " kategorie bez porządku."),
          c("porzadkowa", "Porządkowa:", " kategorie z porządkiem."),
          c("ilosciowa_dyskretna", "Dyskretna:", " liczby całkowite."),
          c("ilosciowa_ciagla", "Ciągła:", " pomiary z dokładnością.")
        ), function(x) tags$li(
          tags$i(class = "lc-th-swatch", style = paste0("--lc-sw:", type_colors[[x[1]]])),
          b_(x[2]), x[3]
        ))
      )
    ),

    # --- Quiz widget ---
    figure_panel(
      label = "Ryc. 7.2",
      title = "Quiz",

      # Quiz prowadzi przeglądarka (shared/lc_quiz.js): losowanie, ocena, wynik.
      lc_tile_quiz("ch7", .load_quiz_questions_typy(), n = QUIZ_TYPE_MAX_QUESTIONS,
        options = lapply(names(QUIZ_TYPE_OPTIONS), function(v) {
          list(value = v, label = QUIZ_TYPE_OPTIONS[[v]], color = unname(QUIZ_TYPE_COLORS[[v]]))
        }),
        tiles_class = "quiz-cols-4",
        labels = list(question = "Pytanie", score = "Wynik", result = "Wynik",
                      correct = "Poprawne odpowiedzi"),
        colors = list(question = upwr_secondary, correct = upwr_secondary,
          score_levels = list(start = upwr_secondary, ok = type_colors[["ilosciowa_ciagla"]],
                              mid = type_colors[["porzadkowa"]], low = type_colors[["nominalna"]])),
        summary = c(
          "90" = "Ćwiczenie zakończone celująco!",
          "70" = "Dobry wynik!",
          "50" = "Nieźle, ale warto powtórzyć.",
          "0"  = "Powtórz materiał z wcześniejszych rozdziałów."
        ))
    ),

    lc_chapter_next(
      num       = "07",
      title     = "Ćwiczenia",
      lead      = "czas na praktykę — zadania z typów danych i statystyki opisowej.",
      target_id = "ch-cwiczenia"
    ),

    br(), br()
  )
) # end ch7_ui
