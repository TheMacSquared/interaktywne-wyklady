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

# `quiz$intro` i `quiz$outro` (opcjonalne) to akapity przed panelem quizu i po nim;
# `exercises_intro` (opcjonalne) — akapity pod nagłówkiem „Ćwiczenia”.
risk_assessment_ui <- function(prefix, quiz, exercises, exercises_intro = NULL) {
  questions <- risk_quiz_questions(quiz)
  tagList(
    lc_h2(paste0(prefix, "-quiz"), "Krótki quiz"),
    if (!is.null(quiz$intro)) risk_prose(quiz$intro),
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
    if (!is.null(quiz$outro)) risk_prose(quiz$outro),
    lc_h2(paste0(prefix, "-cwiczenia"), "Ćwiczenia"),
    if (!is.null(exercises_intro)) risk_prose(exercises_intro),
    figure_panel(
      label = "Praca własna",
      title = "Od rachunku do decyzji",
      tags$ol(lapply(exercises, function(exercise) {
        # Ćwiczenie to tekst albo lista z polami task i answer (odpowiedź zwinięta).
        # `task` może zawierać kontrolki; ich ocena należy wtedy do serwera bloku.
        if (is.character(exercise)) {
          return(tags$li(exercise))
        }
        tags$li(
          exercise$task,
          if (!is.null(exercise$answer)) {
            tags$details(
              class = "lc-exercise-answer",
              tags$summary("Odpowiedź"),
              lapply(exercise$answer, function(x) if (inherits(x, "shiny.tag")) x else tags$p(x))
            )
          }
        )
      })),
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

# --------------------------------------------------------------------------
# ELEMENTY SKRYPTU
# Sekcja może mieć pole `body`: listę elementów renderowanych dokładnie w podanej
# kolejności. Wektor znakowy to akapity prozy; pozostałe elementy to gotowe tagi
# (definicja, wzór, przykład, pytanie kontrolne, widget). Dzięki temu tekst
# prowadzi rozumowanie, a widget ilustruje jego wybrany krok.
# --------------------------------------------------------------------------

risk_script_dependency <- function() {
  htmltools::htmlDependency(
    name = "risk-script", version = "1.0.0",
    src = c(file = file.path(.LC_PROJ_ROOT, "R", "assets")),
    script = "risk_script.js"
  )
}

# Tablica wyników detektora 2×2: wiersze to stan rzeczywisty, kolumny wynik
# detektora; trafienia (TP, TN) zielone, pomyłki (FN, FP) czerwone.
risk_confusion_matrix <- function() {
  cell <- function(kind, name, abbr, desc) {
    tags$div(
      class = paste("lc-cm-cell", paste0("lc-cm-", kind)),
      tags$div(class = "lc-cm-abbr", abbr),
      tags$div(class = "lc-cm-name", name),
      tags$div(class = "lc-cm-desc", desc)
    )
  }
  tags$div(
    class = "lc-cm",
    role = "table",
    `aria-label` = "Tablica wyników detektora: cztery wyniki",
    tags$div(class = "lc-cm-corner"),
    tags$div(class = "lc-cm-head", "Alarm (+)"),
    tags$div(class = "lc-cm-head", "Brak alarmu (−)"),
    tags$div(class = "lc-cm-side", "Awaria (A)"),
    cell("ok", "prawdziwie dodatni", "TP", "awaria i alarm"),
    cell("bad", "fałszywie ujemny", "FN", "awaria bez alarmu"),
    tags$div(class = "lc-cm-side", "Brak awarii (¬A)"),
    cell("bad", "fałszywie dodatni", "FP", "brak awarii, ale alarm"),
    cell("ok", "prawdziwie ujemny", "TN", "brak awarii i brak alarmu")
  )
}

# Wzór z liczbami i opisami: symbole w pierwszym rzędzie, te same wyrazy z liczbami
# w drugim, a pod nimi opisy połączone z wyrazami kreską. `items` to lista
# list(symbol, value, note, color); `ops` to znaki między wyrazami (o jeden mniej).
risk_annotated_formula <- function(items, ops) {
  n <- length(items)
  stopifnot(length(ops) == n - 1L)
  cols <- paste(rep(c("minmax(0, 1fr)", "auto"), length.out = 2L * n - 1L), collapse = " ")
  op_cell <- function(k, class) tags$div(class = paste("lc-annot-op", class), ops[[k]])
  row_cells <- function(field, class) {
    unlist(lapply(seq_len(n), function(i) {
      cell <- tags$div(
        class = paste("lc-annot-cell", class),
        style = paste0("--annot-color:", items[[i]]$color %||% "#6b1a2a"),
        items[[i]][[field]]
      )
      if (i < n) list(cell, op_cell(i, class)) else list(cell)
    }), recursive = FALSE)
  }
  note_cells <- unlist(lapply(seq_len(n), function(i) {
    cell <- tags$div(
      class = "lc-annot-cell lc-annot-note",
      style = paste0("--annot-color:", items[[i]]$color %||% "#6b1a2a"),
      items[[i]]$note
    )
    if (i < n) list(cell, tags$div()) else list(cell)
  }), recursive = FALSE)
  lc_formula_box(
    class = "lc-annot",
    style = paste0("grid-template-columns:", cols),
    row_cells("symbol", "lc-annot-symbol"),
    row_cells("value", "lc-annot-value"),
    note_cells
  )
}

risk_body <- function(items) {
  if (is.null(items)) {
    return(NULL)
  }
  if (is.character(items)) {
    return(risk_prose(items))
  }
  tagList(lapply(items, function(item) {
    if (is.character(item)) risk_prose(item) else item
  }))
}

risk_definition <- function(num, term, text) {
  tags$div(
    class = "lc-def",
    tags$div(class = "lc-def-label", paste0("Definicja ", num, " · ", term)),
    tags$div(class = "lc-def-body", lapply(text, tags$p))
  )
}

# Wzór z numerem i objaśnieniem symboli. `legend` to nazwany wektor:
# nazwa = symbol w TeX-u, wartość = znaczenie.
risk_formula <- function(tex, num = NULL, legend = NULL) {
  lc_formula_box(
    class = "lc-formula-numbered",
    tags$div(
      class = "lc-formula-row",
      tags$div(class = "lc-formula-tex", withMathJax(paste0("$$", tex, "$$"))),
      if (!is.null(num)) tags$div(class = "lc-formula-num", paste0("(", num, ")"))
    ),
    if (!is.null(legend)) {
      tags$div(
        class = "lc-formula-legend",
        "gdzie: ",
        tagList(lapply(seq_along(legend), function(i) {
          tagList(
            if (i > 1) "; ",
            paste0("\\(", names(legend)[i], "\\)"), " — ", legend[[i]]
          )
        })),
        "."
      )
    }
  )
}

# Przykład z rozwiązaniem krok po kroku. Rozwiązanie jest zwinięte, żeby czytelnik
# mógł najpierw spróbować sam. W krokach używaj zapisu Unicode, nie MathJax:
# treść zwiniętego elementu nie zawsze jest poprawnie składana.
# Podpunkty a), b), … jako lista. Każdy argument to jeden podpunkt (tekst albo
# tagi); znaczniki rysuje CSS (`lc-example-list`), więc treść nie zawiera „(a)”.
# Wynik wstawiamy do `problem`, `steps` lub `answer` jako element `list(...)`,
# nigdy `c(...)` — c() rozbija tag na części i wypisuje nazwy atrybutów jako tekst.
risk_parts <- function(...) {
  tags$ol(
    class = "lc-example-list", type = "a",
    lapply(list(...), tags$li)
  )
}

risk_example <- function(num, title, problem, steps, answer = NULL, steps_type = NULL) {
  tags$div(
    class = "lc-example",
    tags$div(class = "lc-example-label", paste0("Przykład ", num, " · ", title)),
    tags$div(
      class = "lc-example-body",
      lapply(problem, function(x) if (inherits(x, "shiny.tag")) x else tags$p(x))
    ),
    tags$details(
      class = "lc-example-solution",
      tags$summary("Rozwiązanie"),
      tags$ol(
        type = steps_type,
        class = if (!is.null(steps_type)) "lc-example-list",
        lapply(steps, tags$li)
      ),
      if (!is.null(answer)) tags$p(tags$strong("Odpowiedź:"), paste0(" ", answer))
    )
  )
}

# Zwinięte wyprowadzenie lub uzasadnienie — dla czytelnika, który chce zobaczyć,
# skąd bierze się wzór; na zajęciach można je pominąć.
risk_derivation <- function(title, text, lines = NULL) {
  inline_callout(
    label = paste0("Skąd to się bierze: ", title),
    lapply(text, tags$p),
    if (!is.null(lines)) tags$pre(class = "lc-derivation-lines", paste(lines, collapse = "\n")),
    color = "ok"
  )
}

# Pytanie kontrolne w toku tekstu. Działa w przeglądarce (bez serwera): po wyborze
# błędnej odpowiedzi pokazuje wskazówkę, po poprawnej — wyjaśnienie.
risk_check <- function(id, question, choices, correct, explanation, hints = NULL) {
  stopifnot(correct %in% unname(choices))
  tags$div(
    class = "lc-check", `data-correct` = correct,
    tags$div(class = "lc-check-label", "Sprawdź się"),
    tags$p(class = "lc-check-question", question),
    tags$div(
      class = "lc-check-options",
      lapply(seq_along(choices), function(i) {
        value <- unname(choices[[i]])
        tags$label(
          class = "lc-check-option",
          tags$input(
            type = "radio", name = paste0("lc_check_", id), value = value,
            `data-hint` = if (!is.null(hints) && value %in% names(hints)) hints[[value]]
          ),
          " ", names(choices)[i]
        )
      })
    ),
    tags$div(class = "lc-check-feedback", role = "status", `aria-live` = "polite"),
    tags$div(class = "lc-check-explanation", hidden = NA, explanation),
    risk_script_dependency()
  )
}

# Instrukcja przed widgetem: co zmienić i na co patrzeć.
risk_try <- function(text) {
  tags$div(class = "lc-try", tags$strong("Do zrobienia:"), paste0(" ", text))
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
    risk_body(section$body),
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
      title = paste0(chapter$hook %||% chapter$title, "."),
      lead = chapter$lead
    ),
    if (!is.null(chapter$intro)) risk_prose(chapter$intro),
    risk_callout(chapter$callout),
    risk_body(chapter$body),
    lapply(sections, function(section) risk_section(block, chapter, section)),
    risk_config_extras(chapter)
  )
  if (!is.null(next_chapter)) {
    content <- tagAppendChildren(
      content,
      lc_chapter_next(
        num = sprintf("%02d", index + 1L),
        title = next_chapter$title,
        # `teaser` to krótsza zapowiedź na karcie „Dalej”; domyślnie lead rozdziału.
        lead = next_chapter$teaser %||% next_chapter$lead,
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
