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
  ilosciowa_ciagla    = "Ilo\u015bciowa ci\u0105g\u0142a",
  ilosciowa_dyskretna = "Ilo\u015bciowa dyskretna",
  jakosciowa_porzadkowa = "Jako\u015bciowa porz\u0105dkowa",
  jakosciowa_nominalna  = "Jako\u015bciowa nominalna"
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

ch7_ui <- tabPanel("7. Quiz",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Poprzednio: \u015bci\u0105ga ze statystyki opisowej"
    ),

    div(class = "section-title", "Quiz: rozpoznaj typ zmiennej"),

    div(class = "narrative",
      p("Przeczytaj opis zmiennej i wybierz jej typ. Ka\u017cde pytanie ma ",
        tags$b("4 opcje"), " \u2014 dok\u0142adnie jedn\u0105 poprawn\u0105."),
      p("Quiz losuje ", tags$b("15 pyta\u0144"), " z puli 75. Mo\u017cesz go powtarza\u0107
        wielokrotnie \u2014 za ka\u017cdym razem dostaniesz inny zestaw.")
    ),

    # Legenda typow
    div(class = "widget-block",
      h4("Przypomnienie typ\u00f3w zmiennych"),
      fluidRow(
        column(3, div(class = "type-badge",
          style = paste0("background: ", col_nominal, ";"), "Nominalna")),
        column(3, div(class = "type-badge",
          style = paste0("background: ", col_ordinal, ";"), "Porz\u0105dkowa")),
        column(3, div(class = "type-badge",
          style = paste0("background: ", col_discrete, ";"), "Dyskretna")),
        column(3, div(class = "type-badge",
          style = paste0("background: ", col_continuous, ";"), "Ci\u0105g\u0142a"))
      ),
      p(style = "margin-top: 10px; font-size: 13px; color: #777;",
        "Nominalna = kategorie bez porz\u0105dku | Porz\u0105dkowa = kategorie z porz\u0105dkiem | ",
        "Dyskretna = liczby ca\u0142kowite | Ci\u0105g\u0142a = pomiary z dok\u0142adno\u015bci\u0105")
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
      p("Czas na praktyk\u0119! Przejd\u017a do \u0107wicze\u0144 z typ\u00f3w danych."),
      actionButton("ch7_to_ch8", "Dalej: \u0106wiczenia \u2192", class = "btn-primary")
    ),

    br(), br()
  ))
)

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
            " Poprawna odpowied\u017a: ",
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
        actionButton("ch7_next", "Nast\u0119pne pytanie \u2192",
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

    result_text <- if (pct >= 90) "\u0106wiczenie zako\u0144czone celuj\u0105co!"
                   else if (pct >= 70) "Dobry wynik!"
                   else if (pct >= 50) "Nie\u017ale, ale warto powt\u00f3rzy\u0107."
                   else "Powt\u00f3rz materia\u0142 z wcze\u015bniejszych rozdzia\u0142\u00f3w."

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
        actionButton("ch7_start", "Spr\u00f3buj ponownie",
                     class = "btn-primary btn-lg"),
        actionButton("ch7_back_to_ch6", "Wr\u00f3\u0107 do \u015bci\u0105gi",
                     class = "btn-outline-secondary btn-lg")
      )
    )
  })

  # Nawigacja powrotna
  observeEvent(input$ch7_back_to_ch6, {
    updateNavbarPage(session, "main_nav", selected = "6. \u015aci\u0105ga")
  })

}
