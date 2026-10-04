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
  num   = "07",
  title = "Quiz",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 07 · Dane i populacja",
      num    = "07",
      title  = "Quiz.",
      lead   = "Każde pytanie opisuje krótko jedno badanie. Wskaż populację,
                próbę, parametr, statystykę albo sposób doboru próby. Quiz
                losuje 10 pytań z puli 20; możesz go powtarzać."
    ),

    figure_panel(
      label = "Ryc. 7.1",
      title = "Quiz",
      lc_toolbar(
        lc_action("ch7_start", "Rozpocznij quiz", variant = "solid"),
        lc_readouts(uiOutput("ch7_progress"))
      ),
      uiOutput("ch7_question_ui"),
      uiOutput("ch7_options_ui"),
      uiOutput("ch7_feedback_ui"),
      uiOutput("ch7_summary_ui")
    ),

    lc_p("Następny krok to wykład 01, Statystyka opisowa: rodzaje zmiennych
      i liczby, którymi streszczamy próbę.")
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch7_server <- function(input, output, session) {

  quiz_state <- reactiveValues(
    active = FALSE, finished = FALSE, answered = FALSE,
    questions = list(), current_idx = 0, total = 0,
    correct = 0, wrong = 0, order = 1:4,
    last_correct = FALSE, last_explanation = "", last_correct_label = ""
  )

  all_questions <- NULL

  observeEvent(input$ch7_start, {
    if (is.null(all_questions)) all_questions <<- .load_quiz_questions_dane()
    n <- min(QUIZ_DANE_MAX_QUESTIONS, length(all_questions))
    quiz_state$questions   <- sample(all_questions, n)
    quiz_state$total       <- n
    quiz_state$current_idx <- 1
    quiz_state$correct     <- 0
    quiz_state$wrong       <- 0
    quiz_state$answered    <- FALSE
    quiz_state$finished    <- FALSE
    quiz_state$active      <- TRUE
    quiz_state$order       <- sample(4)
  })

  current_question <- reactive({
    req(quiz_state$active, quiz_state$current_idx > 0)
    quiz_state$questions[[quiz_state$current_idx]]
  })

  output$ch7_progress <- renderUI({
    if (!quiz_state$active) return(NULL)
    answered <- quiz_state$correct + quiz_state$wrong
    tagList(
      lc_readout("pytanie", paste0(quiz_state$current_idx, " / ", quiz_state$total)),
      lc_readout("wynik", paste0(quiz_state$correct, " / ", answered),
                 color = upwr_accent)
    )
  })

  output$ch7_question_ui <- renderUI({
    if (!quiz_state$active || quiz_state$finished) return(NULL)
    q <- current_question()
    tagList(
      lc_status(q$scenario, live = FALSE),
      tags$h4(q$question)
    )
  })

  output$ch7_options_ui <- renderUI({
    if (!quiz_state$active || quiz_state$finished || quiz_state$answered) return(NULL)
    q <- current_question()
    letters_abcd <- c("A", "B", "C", "D")
    div(class = "quiz-tiles quiz-cols-2",
      lapply(seq_along(quiz_state$order), function(i) {
        actionButton(paste0("ch7_answer_", i),
          tagList(
            div(class = "tile-letter", style = paste0("background:", upwr_secondary, ";"),
                letters_abcd[i]),
            div(class = "tile-text", q$options[[quiz_state$order[i]]])
          ),
          class = "quiz-tile"
        )
      })
    )
  })

  lapply(1:4, function(i) {
    observeEvent(input[[paste0("ch7_answer_", i)]], {
      if (!quiz_state$active || quiz_state$answered) return()
      q <- current_question()
      chosen <- quiz_state$order[i]
      ok <- chosen == q$correct
      if (ok) quiz_state$correct <- quiz_state$correct + 1
      else quiz_state$wrong <- quiz_state$wrong + 1
      quiz_state$answered           <- TRUE
      quiz_state$last_correct       <- ok
      quiz_state$last_explanation   <- q$explanation
      quiz_state$last_correct_label <- q$options[[q$correct]]
    }, ignoreInit = TRUE)
  })

  output$ch7_feedback_ui <- renderUI({
    if (!quiz_state$active || !quiz_state$answered || quiz_state$finished) return(NULL)
    ok <- quiz_state$last_correct
    tagList(
      lc_status(
        lc_verdict(tags$strong(if (ok) "Dobrze!" else "Nie tym razem."),
                   type = if (ok) "ok" else "danger"),
        if (!ok) tagList(" Poprawna odpowiedź: ", b_(quiz_state$last_correct_label), "."),
        p(quiz_state$last_explanation)
      ),
      if (quiz_state$current_idx < quiz_state$total) {
        lc_action("ch7_next", "Następne pytanie", variant = "solid")
      } else {
        lc_action("ch7_finish", "Zobacz wynik", variant = "solid")
      }
    )
  })

  observeEvent(input$ch7_next, {
    quiz_state$current_idx <- quiz_state$current_idx + 1
    quiz_state$answered <- FALSE
    quiz_state$order <- sample(4)
  })

  observeEvent(input$ch7_finish, {
    quiz_state$finished <- TRUE
    quiz_state$answered <- FALSE
  })

  output$ch7_summary_ui <- renderUI({
    if (!quiz_state$finished) return(NULL)
    pct <- round(quiz_state$correct / quiz_state$total * 100)
    type <- if (pct >= 70) "ok" else if (pct >= 50) "warning" else "danger"
    text <- if (pct >= 90) "Świetnie, pojęcia są opanowane."
            else if (pct >= 70) "Dobry wynik."
            else if (pct >= 50) "Nieźle, ale warto wrócić do ściągi."
            else "Wróć do rozdziałów 02–04 i spróbuj ponownie."
    tagList(
      lc_readouts(
        lc_readout("wynik", paste0(pct, "%"), color = upwr_accent),
        lc_readout("poprawne", paste0(quiz_state$correct, " / ", quiz_state$total))
      ),
      lc_status(lc_verdict(tags$strong(text), type = type)),
      lc_toolbar(
        lc_action("ch7_start", "Spróbuj ponownie", variant = "solid"),
        lc_action("ch7_back", "Wróć do ściągi", variant = "outline")
      )
    )
  })

  observeEvent(input$ch7_back, {
    session$sendCustomMessage("switchToChapter", "ch-sciaga")
  })
}
