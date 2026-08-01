# ==========================================================================
# ROZDZIAŁ 7: QUIZ
# ==========================================================================

.quiz_question_ui <- function(question, index) {
  figure_panel(
    label = paste("Pytanie", index),
    radioButtons(
      inputId = paste0("ch7_", question$id),
      label = question$question,
      choices = quiz_choices(question),
      selected = character(0),
      width = "100%"
    )
  )
}

ch7_ui <- lecture_chapter(
  id = "ch-quiz",
  num = "07",
  title = "Quiz",
  duration = "10 min",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 07 · Język ryzyka",
      num = "07",
      title = "Czy mianownik się zgadza?",
      lead = "Dziesięć krótkich pytań sprawdza rozpoznawanie pojęć i granic
              modelu. Rachunki są tu mniej ważne niż precyzyjna interpretacja."
    ),

    tagList(lapply(seq_along(quiz_questions), function(i) {
      .quiz_question_ui(quiz_questions[[i]], i)
    })),

    actionButton(
      "ch7_check",
      "Sprawdź odpowiedzi",
      class = "lc-btn-primary",
      width = "100%"
    ),
    uiOutput("ch7_feedback"),

    lc_chapter_next(
      num = "08",
      title = "Ćwiczenie decyzyjne",
      lead = "Popraw niepełny komunikat o bezpieczeństwie.",
      target_id = "ch-cwiczenia"
    )
  )
)

ch7_server <- function(input, output, session) {
  check_count <- reactiveVal(0L)

  observeEvent(input$ch7_check, {
    check_count(check_count() + 1L)
  })

  output$ch7_feedback <- renderUI({
    req(check_count() > 0)

    answers <- vapply(quiz_questions, function(question) {
      value <- input[[paste0("ch7_", question$id)]]
      if (is.null(value)) "" else value
    }, character(1))
    correct <- vapply(seq_along(quiz_questions), function(i) {
      identical(answers[[i]], quiz_questions[[i]]$correct)
    }, logical(1))
    answered <- nzchar(answers)

    details <- lapply(seq_along(quiz_questions), function(i) {
      question <- quiz_questions[[i]]
      tags$li(
        tags$strong(paste0("Pytanie ", i, ": ")),
        if (!answered[[i]]) {
          "Brak odpowiedzi. "
        } else if (correct[[i]]) {
          "Dobrze. "
        } else {
          paste0("Nie tym razem. Poprawna odpowiedź: ",
                 question$options[[question$correct]], ". ")
        },
        question$explanation
      )
    })

    lc_feedback(
      type = if (all(correct)) "ok" else if (sum(correct) >= 4) "info" else "warning",
      tags$strong(sprintf("Wynik: %d/%d.", sum(correct), length(correct))),
      if (!all(answered)) " Nie udzielono odpowiedzi na wszystkie pytania.",
      tags$ol(details)
    )
  })
}
