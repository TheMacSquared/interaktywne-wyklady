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
  ilosciowa_ciagla      = col_continuous,
  ilosciowa_dyskretna   = col_discrete,
  jakosciowa_porzadkowa = col_ordinal,
  jakosciowa_nominalna  = col_nominal
)

QUIZ_TYPE_MAX_QUESTIONS <- 15

# ============================================================================
# UI
# ============================================================================

ch7_ui <- list(
  id = "ch-quiz", num = "07", title = "Quiz",
  content = tagList(

    div(class = "chapter-recap",
      "Poprzednio: ściąga ze statystyki opisowej"
    ),

    h2(id = "ch7-quiz", class = "section-title", "Quiz: rozpoznaj typ zmiennej"),

    div(class = "narrative",
      p("Przeczytaj opis zmiennej i wybierz jej typ. Każde pytanie ma ",
        tags$b("4 opcje"), " — dokładnie jedną poprawną."),
      p("Quiz losuje ", tags$b("15 pytań"), " z puli 75. Możesz go powtarzać
        wielokrotnie — za każdym razem dostaniesz inny zestaw.")
    ),

    # Legenda typow
    div(class = "widget-block",
      h4("Przypomnienie typów zmiennych"),
      fluidRow(
        column(3, div(class = "type-badge",
          style = paste0("background: ", col_nominal, ";"), "Nominalna")),
        column(3, div(class = "type-badge",
          style = paste0("background: ", col_ordinal, ";"), "Porządkowa")),
        column(3, div(class = "type-badge",
          style = paste0("background: ", col_discrete, ";"), "Dyskretna")),
        column(3, div(class = "type-badge",
          style = paste0("background: ", col_continuous, ";"), "Ciągła"))
      ),
      p(style = "margin-top: 10px; font-size: 13px; color: #777;",
        "Nominalna = kategorie bez porządku | Porządkowa = kategorie z porządkiem | ",
        "Dyskretna = liczby całkowite | Ciągła = pomiary z dokładnością")
    ),

    # --- Quiz widget ---
    div(class = "widget-block",
      h4("Quiz"),

      # Start / status bar
      fluidRow(
        column(6,
          actionButton("ch7_start", "Rozpocznij quiz",
                       class = "btn-primary btn-lg", width = "100%")
        ),
        column(6,
          uiOutput("ch7_progress")
        )
      ),

      hr(),

      # Pytanie
      uiOutput("ch7_question_ui"),

      # Opcje odpowiedzi (4 przyciski)
      uiOutput("ch7_options_ui"),

      # Feedback
      uiOutput("ch7_feedback_ui"),

      hr(),

      # Podsumowanie
      uiOutput("ch7_summary_ui")
    ),

    # --- Przejscie do cwiczen ---
    div(class = "chapter-transition",
      p("Czas na praktykę! Przejdź do ćwiczeń z typów danych."),
      actionButton("ch7_to_ch8", "Dalej: Ćwiczenia →", class = "btn-primary")
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

    tagList(
      div(style = "display: flex; justify-content: space-between; margin-bottom: 4px;",
        span(paste0("Pytanie ", quiz_state$current_idx, " / ", total)),
        span(paste0("Wynik: ", quiz_state$correct, " / ", answered),
             style = paste0("font-weight: bold; color: ",
                            if (answered == 0) col_dark
                            else if (quiz_state$correct / answered >= 0.7) col_continuous
                            else if (quiz_state$correct / answered >= 0.5) col_ordinal
                            else col_nominal))
      ),
      div(style = "background: #e9ecef; border-radius: 6px; height: 8px; overflow: hidden;",
        div(style = paste0(
          "background: ", col_discrete, "; height: 100%; width: ", pct, "%;",
          "border-radius: 6px; transition: width 0.3s;"
        ))
      )
    )
  })

  # --- Pytanie ---
  output$ch7_question_ui <- renderUI({
    if (!quiz_state$active || quiz_state$finished) return(NULL)

    q <- quiz_state$questions[[quiz_state$current_idx]]

    div(
      style = "font-size: 18px; font-weight: 500; color: #2c3e50;
               padding: 20px; background: white; border-radius: 8px;
               border-left: 4px solid #3498db; margin: 15px 0;",
      q$question
    )
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
            div(class = "tile-letter", style = paste0("background: ", type_color, ";"),
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
    correct_color <- QUIZ_TYPE_COLORS[[quiz_state$last_correct_val]]

    tagList(
      div(class = if (is_correct) "callout-success" else "callout-danger",
        tags$strong(if (is_correct) "Dobrze!" else "Nie tym razem."),
        if (!is_correct) {
          tagList(
            " Poprawna odpowiedź: ",
            span(style = paste0("font-weight: bold; color: ", correct_color, ";"),
                 correct_label)
          )
        }
      ),

      div(style = "background: #f8f9fa; border-radius: 6px; padding: 12px 16px;
                   margin: 10px 0; font-size: 14px; color: #555;",
        explanation
      ),

      if (quiz_state$current_idx < quiz_state$total) {
        actionButton("ch7_next", "Następne pytanie →",
                     class = "btn-primary", width = "100%",
                     style = "margin-top: 10px;")
      } else {
        actionButton("ch7_finish", "Zobacz wynik",
                     class = "btn-success btn-lg", width = "100%",
                     style = "margin-top: 10px;")
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

    result_color <- if (pct >= 70) col_continuous
                    else if (pct >= 50) col_ordinal
                    else col_nominal

    result_text <- if (pct >= 90) "Ćwiczenie zakończone celująco!"
                   else if (pct >= 70) "Dobry wynik!"
                   else if (pct >= 50) "Nieźle, ale warto powtórzyć."
                   else "Powtórz materiał z wcześniejszych rozdziałów."

    div(style = "text-align: center; padding: 30px;",
      div(style = paste0(
        "font-size: 64px; font-weight: bold; color: ", result_color, ";"
      ), paste0(pct, "%")),

      div(style = "font-size: 18px; color: #555; margin: 10px 0;",
        paste0("Poprawne odpowiedzi: ", correct, " / ", total)
      ),

      div(style = "max-width: 300px; margin: 15px auto; background: #e9ecef;
                   border-radius: 10px; height: 16px; overflow: hidden;",
        div(style = paste0(
          "background: ", result_color, "; height: 100%; width: ", pct, "%;
           border-radius: 10px; transition: width 0.5s;"
        ))
      ),

      div(style = paste0("font-size: 16px; font-weight: bold; color: ", result_color, ";
                          margin: 10px 0 20px 0;"),
        result_text
      ),

      div(style = "display: flex; gap: 10px; justify-content: center;",
        actionButton("ch7_start", "Spróbuj ponownie",
                     class = "btn-primary btn-lg"),
        actionButton("ch7_back_to_ch6", "Wróć do ściągi",
                     class = "btn-outline-secondary btn-lg")
      )
    )
  })

  # Nawigacja powrotna
  observeEvent(input$ch7_back_to_ch6, {
    session$sendCustomMessage("switchToChapter", "ch-sciaga")
  })

}
