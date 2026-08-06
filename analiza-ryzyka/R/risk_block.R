# ==========================================================================
# KOMPONENTY PEŁNYCH BLOKÓW KURSU ANALIZY RYZYKA
# Treść pozostaje w katalogu wykładu; ten plik dostarcza tylko wspólny język UI.
# ==========================================================================

`%||%` <- function(x, fallback) if (is.null(x)) fallback else x

risk_format_probability <- function(x, digits = 3L) {
  if (length(x) != 1L || is.na(x) || !is.finite(x)) {
    return("—")
  }
  paste0(
    gsub("\\.", ",", sprintf(paste0("%.", digits, "f"), x)),
    " (", gsub("\\.", ",", sprintf("%.1f", 100 * x)), "%)"
  )
}

risk_natural_frequency <- function(p, population = 1000L) {
  if (is.na(p) || !is.finite(p)) {
    return("—")
  }
  sprintf(
    "około %d na %s", round(p * population),
    format(population, big.mark = " ", scientific = FALSE)
  )
}

risk_widget_panel <- function(label = "Eksperyment", title, controls,
                              plot_id = NULL, stats_id = NULL, note = NULL,
                              height = "430px") {
  body <- list()
  if (!is.null(plot_id)) {
    body <- list(
      fluidRow(
        column(
          4,
          controls,
          if (!is.null(stats_id)) uiOutput(stats_id),
          if (!is.null(note)) lc_feedback(type = "info", note)
        ),
        column(8, zoom_plot_ui(plot_id, height = height))
      )
    )
  } else {
    body <- list(controls, if (!is.null(stats_id)) uiOutput(stats_id))
  }
  do.call(figure_panel, c(list(label = label, title = title, full_width = TRUE), body))
}

risk_vote_panel <- function(input_id, output_id, question, choices) {
  figure_panel(
    label = "Najpierw zdecyduj",
    title = question,
    radioButtons(input_id, NULL, choices = choices, selected = character(0)),
    actionButton(paste0(input_id, "_check"), "Sprawdź intuicję",
      class = "lc-btn-primary"
    ),
    uiOutput(output_id),
    full_width = TRUE
  )
}

risk_quiz_questions <- function(quiz) {
  if (!is.null(quiz$questions)) {
    return(quiz$questions)
  }
  list(
    quiz,
    list(
      question = "Czy wynik prawdopodobieństwa bez jednostki ekspozycji i horyzontu jest kompletny?",
      choices = c("Nie" = "no", "Tak" = "yes"), correct = "no",
      explanation = "Prawdopodobieństwo trzeba przypisać do określonej ekspozycji i czasu."
    ),
    list(
      question = "Czy niezależność można przyjąć bez sprawdzenia wspólnych mechanizmów?",
      choices = c("Nie" = "no", "Tak" = "yes"), correct = "no",
      explanation = "Niezależność jest założeniem wymagającym uzasadnienia."
    ),
    list(
      question = "Czy najmniejsze ryzyko liczbowe automatycznie wskazuje najlepszą decyzję?",
      choices = c("Nie" = "no", "Tak" = "yes"), correct = "no",
      explanation = "Decyzja uwzględnia również konsekwencje, koszt i wykonalność."
    ),
    list(
      question = "Czy raport powinien wskazywać przypadek, w którym model może zawieść?",
      choices = c("Tak" = "yes", "Nie" = "no"), correct = "yes",
      explanation = "Jawne ograniczenie ułatwia bezpieczne użycie wyniku."
    )
  )
}

risk_assessment_ui <- function(prefix, quiz, exercises) {
  questions <- risk_quiz_questions(quiz)
  tagList(
    lc_h2(paste0(prefix, "-quiz"), "Krótki quiz"),
    figure_panel(
      label = "Sprawdź rozumienie",
      title = "Pięć pytań: mechanizm i audyt modelu",
      tags$ol(lapply(seq_along(questions), function(index) {
        question <- questions[[index]]
        tags$li(
          tags$p(question$question),
          radioButtons(paste0(prefix, "_quiz_", index), NULL,
            choices = question$choices, selected = character(0)
          )
        )
      })),
      actionButton(paste0(prefix, "_quiz_check"), "Sprawdź wszystkie",
        class = "lc-btn-primary"
      ),
      uiOutput(paste0(prefix, "_quiz_feedback")),
      full_width = TRUE
    ),
    lc_h2(paste0(prefix, "-cwiczenia"), "Ćwiczenia"),
    figure_panel(
      label = "Praca własna",
      title = "Od rachunku do decyzji",
      tags$ol(lapply(exercises, tags$li)),
      full_width = TRUE
    )
  )
}

risk_assessment_server <- function(prefix, quiz, input, output) {
  questions <- risk_quiz_questions(quiz)
  checked <- reactiveVal(FALSE)
  observeEvent(input[[paste0(prefix, "_quiz_check")]], checked(TRUE))
  output[[paste0(prefix, "_quiz_feedback")]] <- renderUI({
    req(checked())
    answers <- vapply(seq_along(questions), function(index) {
      input[[paste0(prefix, "_quiz_", index)]] %||% ""
    }, character(1))
    correct <- vapply(seq_along(questions), function(index) {
      identical(answers[[index]], questions[[index]]$correct)
    }, logical(1))
    score <- sum(correct)
    missing <- sum(!nzchar(answers))
    lc_feedback(
      type = if (score == length(questions)) "ok" else "warning",
      tags$strong(sprintf("Wynik: %d/%d.", score, length(questions))),
      if (missing) {
        paste0(" Bez odpowiedzi: ", missing, ".")
      } else if (score == length(questions)) {
        " Wszystkie odpowiedzi są poprawne."
      } else {
        " Wróć do pytania merytorycznego i listy kontrolnej założeń."
      }
    )
  })
}

risk_chapter_from_config <- function(block, chapter, index, next_chapter = NULL) {
  content <- tagList(
    lc_chapter_hero(
      kicker = paste0("Rozdział ", sprintf("%02d", index), " · ", block$title),
      num = sprintf("%02d", index),
      title = paste0(chapter$title, "."),
      lead = chapter$lead
    )
  )

  for (section in chapter$sections %||% list()) {
    content <- tagAppendChildren(
      content,
      lc_h2(paste0(block$id, "-", chapter$id, "-", section$id), section$title),
      if (!is.null(section$text)) lc_p(section$text),
      if (!is.null(section$bullets)) tags$ul(lapply(section$bullets, tags$li))
    )
  }
  if (!is.null(chapter$formula)) {
    content <- tagAppendChildren(content, lc_formula_box(withMathJax(
      paste0("$$", chapter$formula, "$$")
    )))
  }
  if (!is.null(chapter$widget)) content <- tagAppendChildren(content, chapter$widget)
  if (!is.null(chapter$decision)) {
    content <- tagAppendChildren(
      content,
      lc_feedback(type = "ok", tags$strong("Decyzja:"), paste0(" ", chapter$decision))
    )
  }
  if (!is.null(chapter$pitfall)) {
    content <- tagAppendChildren(
      content,
      lc_feedback(type = "warning", tags$strong("Pułapka:"), paste0(" ", chapter$pitfall))
    )
  }
  if (isTRUE(chapter$extension)) {
    content <- tagAppendChildren(
      content,
      lc_feedback(
        type = "info", tags$strong("Rozszerzenie:"),
        " tę część można pominąć podczas krótszego wariantu zajęć."
      )
    )
  }
  if (!is.null(next_chapter)) {
    content <- tagAppendChildren(
      content,
      lc_chapter_next(
        num = sprintf("%02d", index + 1L),
        title = next_chapter$title,
        lead = next_chapter$lead,
        target_id = paste0("ch-", next_chapter$id)
      )
    )
  }

  lecture_chapter(
    id = paste0("ch-", chapter$id),
    num = sprintf("%02d", index),
    title = chapter$title,
    duration = chapter$duration %||% "10–15 min",
    content = content
  )
}

risk_block_chapters <- function(block) {
  lapply(seq_along(block$chapters), function(index) {
    next_chapter <- if (index < length(block$chapters)) block$chapters[[index + 1L]] else NULL
    risk_chapter_from_config(block, block$chapters[[index]], index, next_chapter)
  })
}
