# ==========================================================================
# KOMPONENTY PEŁNYCH BLOKÓW KURSU ANALIZY RYZYKA
# Treść pozostaje w katalogu wykładu; ten plik dostarcza tylko wspólny język UI.
# ==========================================================================

`%||%` <- function(x, fallback) if (is.null(x)) fallback else x

risk_format_probability <- function(x, digits = 3L) {
  if (length(x) != 1L || is.na(x) || !is.finite(x)) {
    return("—")
  }
  if (x > 0 && x < .01) digits <- max(digits, min(12L, ceiling(-log10(x)) + 2L))
  percent_digits <- max(1L, digits - 2L)
  paste0(
    gsub("\\.", ",", sprintf(paste0("%.", digits, "f"), x)),
    " (", gsub("\\.", ",", sprintf(paste0("%.", percent_digits, "f"), 100 * x)), "%)"
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
  if (is.null(quiz$questions) || !length(quiz$questions)) {
    stop("Quiz musi zawierać jawny zestaw pytań tematycznych.")
  }
  quiz$questions
}

risk_assessment_ui <- function(prefix, quiz, exercises) {
  questions <- risk_quiz_questions(quiz)
  tagList(
    lc_h2(paste0(prefix, "-quiz"), "Krótki quiz"),
    figure_panel(
      label = "Sprawdź rozumienie",
      title = paste(length(questions), "pytań: mechanizm i audyt modelu"),
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
  submitted <- reactiveVal(NULL)
  observeEvent(input[[paste0(prefix, "_quiz_check")]], {
    submitted(vapply(seq_along(questions), function(index) {
      input[[paste0(prefix, "_quiz_", index)]] %||% ""
    }, character(1)))
  })
  output[[paste0(prefix, "_quiz_feedback")]] <- renderUI({
    answers <- submitted()
    req(!is.null(answers))
    correct <- vapply(seq_along(questions), function(index) {
      identical(answers[[index]], questions[[index]]$correct)
    }, logical(1))
    score <- sum(correct)
    missing <- sum(!nzchar(answers))
    tagList(
      lc_feedback(
        type = if (score == length(questions)) "ok" else "warning",
        tags$strong(sprintf("Wynik: %d/%d.", score, length(questions))),
        if (missing) paste0(" Bez odpowiedzi: ", missing, ".") else " Poniżej omówienie każdej odpowiedzi."
      ),
      tags$ol(lapply(seq_along(questions), function(i) {
        question <- questions[[i]]
        answer_label <- names(question$choices)[match(question$correct, unname(question$choices))]
        tags$li(
          lc_feedback(type = if (correct[i]) "ok" else "warning",
            tags$strong(if (correct[i]) "Poprawnie:" else if (!nzchar(answers[i])) "Brak odpowiedzi:" else "Do poprawy:"),
            paste0(" ", question$question, " Poprawna odpowiedź: ", answer_label, ". ", question$explanation))
        )
      }))
    )
  })
}

risk_prose <- function(text) {
  # Akapit lub kilka akapitów: wektor znakowy renderuje się jako kolejne lc_p.
  tagList(lapply(text, lc_p))
}

risk_callout <- function(callout) {
  if (is.null(callout)) {
    return(NULL)
  }
  margin_callout(
    label = callout$label,
    callout$text,
    color = callout$color %||% "wskazowka"
  )
}

# Wspólne dodatki rozdziału albo sekcji: wzór → widget → takeaway → decyzja → pułapka
# → nota o rozszerzeniu. Każde pole jest opcjonalne, więc stare konfiguracje renderują
# się bez zmian, a scalony rozdział może mieć te elementy w każdej sekcji z osobna.
risk_config_extras <- function(x) {
  tagList(
    if (!is.null(x$formula)) {
      lc_formula_box(withMathJax(paste0("$$", x$formula, "$$")))
    },
    x$widget,
    if (!is.null(x$takeaway)) risk_prose(x$takeaway),
    if (!is.null(x$decision)) {
      lc_feedback(type = "ok", tags$strong("Decyzja:"), paste0(" ", x$decision))
    },
    if (!is.null(x$pitfall)) {
      lc_feedback(type = "warning", tags$strong("Pułapka:"), paste0(" ", x$pitfall))
    },
    if (isTRUE(x$extension)) {
      lc_feedback(
        type = "info", tags$strong("Rozszerzenie:"),
        " tę część można pominąć podczas krótszego wariantu zajęć."
      )
    }
  )
}

# Sekcja rozdziału: nagłówek wykrywany przez TOC, tekst, callout, lista punktów
# oraz te same dodatki, które ma rozdział. Dawny mały rozdział przenosi się do sekcji
# przez zmianę `intro` na `text`, bez przepisywania treści.
risk_section <- function(block, chapter, section) {
  tagList(
    lc_h2(paste0(block$id, "-", chapter$id, "-", section$id), section$title),
    if (!is.null(section$text)) risk_prose(section$text),
    risk_callout(section$callout),
    if (!is.null(section$bullets)) tags$ul(lapply(section$bullets, tags$li)),
    risk_config_extras(section)
  )
}

risk_chapter_from_config <- function(block, chapter, index, next_chapter = NULL) {
  sections <- chapter$sections %||% list()
  section_ids <- vapply(sections, function(section) section$id %||% "", character(1))
  if (anyDuplicated(section_ids)) {
    stop(sprintf(
      "Rozdział „%s” w bloku „%s” ma powtórzone id sekcji: %s",
      chapter$id, block$id, paste(unique(section_ids[duplicated(section_ids)]), collapse = ", ")
    ))
  }

  content <- tagList(
    lc_chapter_hero(
      kicker = paste0("Rozdział ", sprintf("%02d", index), " · ", block$title),
      num = sprintf("%02d", index),
      title = paste0(chapter$title, "."),
      lead = chapter$lead
    ),
    if (!is.null(chapter$intro)) risk_prose(chapter$intro),
    risk_callout(chapter$callout),
    lapply(sections, function(section) risk_section(block, chapter, section)),
    risk_config_extras(chapter)
  )
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
    duration = chapter$duration,
    content = content
  )
}

risk_block_chapters <- function(block) {
  lapply(seq_along(block$chapters), function(index) {
    next_chapter <- if (index < length(block$chapters)) block$chapters[[index + 1L]] else NULL
    risk_chapter_from_config(block, block$chapters[[index]], index, next_chapter)
  })
}
