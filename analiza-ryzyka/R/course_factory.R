# ==========================================================================
# FABRYKA PEŁNYCH WYKŁADÓW — GLOBALNY RZUT KURSU
# ==========================================================================

risk_chapter <- function(id, title, lead, points, formula = NULL, case = NULL,
                         pitfall = NULL, widget = FALSE) {
  list(id = id, title = title, lead = lead, points = points, formula = formula,
       case = case, pitfall = pitfall, widget = widget)
}

risk_format_widget_value <- function(value, format = "probability") {
  if (identical(format, "probability")) {
    decimal <- gsub("\\.", ",", sprintf("%.3f", value))
    return(paste0(decimal, " (", round(100 * value, 1), "%)"))
  }
  if (identical(format, "count")) return(sprintf("%.1f", value))
  sprintf("%.3f", value)
}

risk_widget_ui <- function(widget) {
  figure_panel(
    label = "Interakcja kluczowa",
    title = widget$title,
    full_width = TRUE,
    fluidRow(
      column(
        4,
        sliderInput(
          "risk_widget_x", widget$input_label,
          min = widget$min, max = widget$max, value = widget$value,
          step = widget$step
        ),
        uiOutput("risk_widget_stats"),
        lc_feedback(type = "info", widget$note)
      ),
      column(8, zoom_plot_ui("risk_widget_plot", height = "440px"))
    )
  )
}

risk_build_chapter <- function(config, chapter, index) {
  next_chapter <- if (index < length(config$chapters)) config$chapters[[index + 1L]] else NULL

  content <- tagList(
    lc_chapter_hero(
      kicker = paste0("Rozdział ", sprintf("%02d", index), " · ", config$title),
      num = sprintf("%02d", index),
      title = paste0(chapter$title, "."),
      lead = chapter$lead
    ),
    lc_h2(paste0("risk-", chapter$id, "-sedno"), "Sedno rozdziału"),
    tags$ul(lapply(chapter$points, tags$li))
  )

  if (!is.null(chapter$formula)) {
    content <- tagAppendChildren(
      content,
      lc_h2(paste0("risk-", chapter$id, "-zapis"), "Minimalny zapis"),
      lc_formula_box(withMathJax(paste0("$$", chapter$formula, "$$")))
    )
  }
  if (!is.null(chapter$case)) {
    content <- tagAppendChildren(
      content,
      lc_h2(paste0("risk-", chapter$id, "-bananpol"), "Bananpol"),
      lc_p(chapter$case)
    )
  }
  if (isTRUE(chapter$widget)) {
    content <- tagAppendChildren(content, risk_widget_ui(config$widget))
  }
  if (!is.null(chapter$pitfall)) {
    content <- tagAppendChildren(
      content,
      lc_feedback(type = "warning", tags$strong("Pułapka:"), paste0(" ", chapter$pitfall))
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
  } else {
    content <- tagAppendChildren(
      content,
      lc_h2(paste0("risk-", chapter$id, "-quiz"), "Pytanie sprawdzające"),
      radioButtons(
        "risk_quiz", config$quiz$question,
        choices = config$quiz$choices, selected = character(0)
      ),
      actionButton("risk_quiz_check", "Sprawdź", class = "lc-btn-primary"),
      uiOutput("risk_quiz_feedback"),
      lc_h2(paste0("risk-", chapter$id, "-cwiczenia"), "Zadania do dyskusji"),
      tags$ol(lapply(config$exercises, tags$li)),
      lc_feedback(
        type = "info",
        tags$strong("Status:"),
        " kompletna struktura wykładu do przeglądu globalnego; kolejną warstwą
          będzie rozwinięcie ćwiczeń i dodatkowych widgetów."
      )
    )
  }

  lecture_chapter(
    id = paste0("ch-", chapter$id),
    num = sprintf("%02d", index),
    title = chapter$title,
    duration = "10–15 min",
    content = content
  )
}

risk_lecture_chapters <- function(config) {
  lapply(seq_along(config$chapters), function(index) {
    risk_build_chapter(config, config$chapters[[index]], index)
  })
}

risk_lecture_server <- function(config, input, output, session) {
  quiz_checked <- reactiveVal(FALSE)
  observeEvent(input$risk_quiz_check, quiz_checked(TRUE))

  output$risk_quiz_feedback <- renderUI({
    req(quiz_checked())
    answer <- input$risk_quiz
    if (is.null(answer)) answer <- ""
    correct <- identical(answer, config$quiz$correct)
    lc_feedback(
      type = if (correct) "ok" else "warning",
      tags$strong(if (correct) "Dobrze." else "Sprawdź jeszcze raz."),
      paste0(" ", config$quiz$explanation)
    )
  })

  widget_plot <- reactive({
    req(input$risk_widget_x)
    widget <- config$widget
    x <- seq(widget$min, widget$max, length.out = 240)
    data <- data.frame(x = x, y = widget$compute(x))
    current_y <- widget$compute(input$risk_widget_x)

    ggplot(data, aes(x = x, y = y)) +
      geom_line(linewidth = 1, colour = upwr_accent) +
      geom_point(
        data = data.frame(x = input$risk_widget_x, y = current_y),
        size = 3.5, colour = upwr_cat[["niebo"]]
      ) +
      labs(
        title = widget$plot_title,
        x = widget$x_label,
        y = widget$y_label
      ) +
      coord_cartesian(ylim = widget$ylim)
  })

  output$risk_widget_stats <- renderUI({
    req(input$risk_widget_x)
    widget <- config$widget
    value <- widget$compute(input$risk_widget_x)
    lc_stat_grid(
      lc_stat_box(widget$input_short, format(input$risk_widget_x, trim = TRUE)),
      lc_stat_box(widget$output_label,
                  risk_format_widget_value(value, widget$format),
                  color = upwr_accent),
      columns = 1
    )
  })

  zoom_plot_server(
    "risk_widget_plot", widget_plot,
    alt = config$widget$alt
  )
}
