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
  id = "ch-quiz", num = "07", title = "Quiz",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 07 · Statystyka opisowa",
      num    = "07",
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

      # Start / status bar
      lc_toolbar(
        lc_action("ch7_start", "Rozpocznij quiz", variant = "solid"),
        lc_readouts(uiOutput("ch7_progress"))
      ),

      # Pytanie
      uiOutput("ch7_question_ui"),

      # Opcje odpowiedzi (4 przyciski)
      uiOutput("ch7_options_ui"),

      # Feedback
      uiOutput("ch7_feedback_ui"),

      # Podsumowanie
      uiOutput("ch7_summary_ui")
    ),

    lc_chapter_next(
      num       = "08",
      title     = "Ćwiczenia",
      lead      = "czas na praktykę — zadania z typów danych i statystyki opisowej.",
      target_id = "ch-cwiczenia"
    ),

    br(), br()
  )
) # end ch7_ui

# ============================================================================
# SERVER
# ============================================================================

ch7_server <- function(input, output, session) {

  # --- Stan quizu ---
  quiz_state <- reactiveValues(
    active       = FALSE,
    questions    = list(),
    current_idx  = 0,
    total        = 0,
    correct      = 0,
    wrong        = 0,
    answered     = FALSE,
    current_options = list(),
    finished     = FALSE
  )

  all_questions <- NULL

  # --- Start quizu ---
  observeEvent(input$ch7_start, {
    if (is.null(all_questions)) {
      all_questions <<- .load_quiz_questions_typy()
    }

    n <- min(QUIZ_TYPE_MAX_QUESTIONS, length(all_questions))
    selected <- sample(all_questions, n)

    quiz_state$questions <- selected
    quiz_state$total <- n
    quiz_state$current_idx <- 1
    quiz_state$correct <- 0
    quiz_state$wrong <- 0
    quiz_state$answered <- FALSE
    quiz_state$finished <- FALSE
    quiz_state$active <- TRUE

    prepare_options()
  })

  # Przygotuj 4 opcje (zawsze te same, w losowej kolejnosci)
  prepare_options <- function() {
    all_vals <- names(QUIZ_TYPE_OPTIONS)
    opts_order <- sample(all_vals)
    quiz_state$current_options <- lapply(opts_order, function(v) {
      list(value = v, label = QUIZ_TYPE_OPTIONS[[v]])
    })
  }

  # --- Pasek postepu ---
  output$ch7_progress <- renderUI({
    if (!quiz_state$active) return(NULL)

    answered <- quiz_state$correct + quiz_state$wrong
    total <- quiz_state$total
    pct <- round(answered / total * 100)

    score_color <- if (answered == 0) upwr_secondary
                   else if (quiz_state$correct / answered >= 0.7) type_colors[["ilosciowa_ciagla"]]
                   else if (quiz_state$correct / answered >= 0.5) type_colors[["porzadkowa"]]
                   else type_colors[["nominalna"]]
    tagList(
      lc_readout("Pytanie", paste0(quiz_state$current_idx, " / ", total), color = upwr_secondary),
      lc_readout("Wynik", paste0(quiz_state$correct, " / ", answered), color = score_color)
    )
  })

  # --- Pytanie ---
  output$ch7_question_ui <- renderUI({
    if (!quiz_state$active || quiz_state$finished) return(NULL)

    q <- quiz_state$questions[[quiz_state$current_idx]]

    tags$h4(q$question)
  })

  # --- Opcje (4 przyciski w grid 2x2) ---
  output$ch7_options_ui <- renderUI({
    if (!quiz_state$active || quiz_state$finished) return(NULL)
    if (quiz_state$answered) return(NULL)

    opts <- quiz_state$current_options
    letters <- c("A", "B", "C", "D")

    div(class = "quiz-tiles quiz-cols-4",
      lapply(seq_along(opts), function(i) {
        btn_id <- paste0("ch7_answer_", i)
        type_color <- QUIZ_TYPE_COLORS[[opts[[i]]$value]]
        actionButton(btn_id,
          tagList(
            div(class = "tile-letter", style = paste0("background:", type_color, ";"),
                letters[i]),
            div(class = "tile-text", opts[[i]]$label)
          ),
          class = "quiz-tile"
        )
      })
    )
  })

  # --- Obsluga klikniec odpowiedzi ---
  observe({
    for (i in 1:4) {
      local({
        idx <- i
        btn_id <- paste0("ch7_answer_", idx)
        observeEvent(input[[btn_id]], {
          if (quiz_state$answered) return()

          q <- quiz_state$questions[[quiz_state$current_idx]]
          selected_val <- quiz_state$current_options[[idx]]$value
          is_correct <- (selected_val == q$correct)

          if (is_correct) {
            quiz_state$correct <- quiz_state$correct + 1
          } else {
            quiz_state$wrong <- quiz_state$wrong + 1
          }

          quiz_state$answered <- TRUE
          quiz_state$last_correct <- is_correct
          quiz_state$last_selected <- selected_val
          quiz_state$last_explanation <- q$explanation
          quiz_state$last_correct_val <- q$correct
        }, ignoreInit = TRUE)
      })
    }
  })

  # --- Feedback po odpowiedzi ---
  output$ch7_feedback_ui <- renderUI({
    if (!quiz_state$active || !quiz_state$answered || quiz_state$finished) return(NULL)

    is_correct <- quiz_state$last_correct
    explanation <- quiz_state$last_explanation
    correct_label <- QUIZ_TYPE_OPTIONS[[quiz_state$last_correct_val]]

    tagList(
      lc_status(
        lc_verdict(tags$strong(if (is_correct) "Dobrze!" else "Nie tym razem."), type = if (is_correct) "ok" else "danger"),
        if (!is_correct) {
          tagList(" Poprawna odpowiedź: ", b_(correct_label))
        },
        p(explanation)
      ),

      if (quiz_state$current_idx < quiz_state$total) {
        lc_action("ch7_next", "Następne pytanie", variant = "solid")
      } else {
        lc_action("ch7_finish", "Zobacz wynik", variant = "solid")
      }
    )
  })

  # --- Nastepne pytanie ---
  observeEvent(input$ch7_next, {
    quiz_state$current_idx <- quiz_state$current_idx + 1
    quiz_state$answered <- FALSE
    prepare_options()
  })

  # --- Zakonczenie ---
  observeEvent(input$ch7_finish, {
    quiz_state$finished <- TRUE
    quiz_state$answered <- FALSE
  })

  # --- Podsumowanie ---
  output$ch7_summary_ui <- renderUI({
    if (!quiz_state$finished) return(NULL)

    total <- quiz_state$total
    correct <- quiz_state$correct
    pct <- round(correct / total * 100)

    result_type <- if (pct >= 70) "ok" else if (pct >= 50) "warning" else "danger"

    result_text <- if (pct >= 90) "Ćwiczenie zakończone celująco!"
                   else if (pct >= 70) "Dobry wynik!"
                   else if (pct >= 50) "Nieźle, ale warto powtórzyć."
                   else "Powtórz materiał z wcześniejszych rozdziałów."

    tagList(
      lc_readouts(
        lc_readout("Wynik", paste0(pct, "%"), color = upwr_accent),
        lc_readout("Poprawne odpowiedzi", paste0(correct, " / ", total), color = upwr_secondary)
      ),
      lc_status(lc_verdict(tags$strong(result_text), type = result_type)),
      lc_toolbar(
        lc_action("ch7_start", "Spróbuj ponownie", variant = "solid"),
        lc_action("ch7_back_to_ch6", "Wróć do ściągi", variant = "outline")
      )
    )
  })

  # Nawigacja powrotna
  observeEvent(input$ch7_back_to_ch6, {
    session$sendCustomMessage("switchToChapter", "ch-sciaga")
  })

}
