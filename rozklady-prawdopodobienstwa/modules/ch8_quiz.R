# ============================================================================
# CHAPTER 8: Quiz - dopasuj rozklad do zjawiska
# ============================================================================

# Wczytaj pytania z JSON
.load_quiz_questions <- function() {
  json_path <- file.path(app_dir, "modules", "quiz_rozklady.json")
  jsonlite::fromJSON(json_path, simplifyDataFrame = FALSE)$questions
}

# Opcje (labele) dla rozkladow
QUIZ_OPTIONS <- list(
  dwumianowy   = "Rozkład dwumianowy",
  poissona     = "Rozkład Poissona",
  normalny     = "Rozkład normalny",
  geometryczny = "Rozkład geometryczny",
  lognormalny  = "Rozkład log-normalny",
  wykladniczy  = "Rozkład wykładniczy"
)

QUIZ_MAX_QUESTIONS <- 10

# ============================================================================
# UI
# ============================================================================

ch8_ui <- list(
  id = "ch-quiz", num = "08", title = "Quiz",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 08 · Rozkłady prawdopodobieństwa",
      num    = "08",
      title  = "Quiz.",
      lead   = "Czas sprawdzić, czy potrafisz rozpoznać rozkłady w praktyce!"
    ),

    h2(id = "ch8-quiz", class = "section-title", "Quiz: dopasuj rozkład do zjawiska"),

    div(class = "narrative",
      p("Przeczytaj opis zjawiska i wybierz rozkład, który najlepiej je modeluje.
        W każdym pytaniu zobaczysz ", tags$b("3 opcje"),
        " — jedną poprawną i dwie losowo wybrane błędne."),
      p("Quiz losuje ", tags$b("10 pytań"), " z puli 60. Możesz go powtarzać
        wielokrotnie — za każdym razem dostaniesz inny zestaw.")
    ),

    figure_panel(
      label = "Quiz",
      title = "Dopasuj rozkład do zjawiska",
      full_width = TRUE,

      # Start / status bar
      fluidRow(
        column(6,
          actionButton("ch8_start", "Rozpocznij quiz",
                       class = "btn-primary btn-lg", width = "100%")
        ),
        column(6,
          uiOutput("ch8_progress")
        )
      ),

      hr(),

      # Pytanie
      uiOutput("ch8_question_ui"),

      # Opcje odpowiedzi (3 przyciski)
      uiOutput("ch8_options_ui"),

      # Feedback
      uiOutput("ch8_feedback_ui"),

      hr(),

      # Podsumowanie
      uiOutput("ch8_summary_ui")
    ),

    lc_chapter_next(
      num       = "09",
      title     = "Ćwiczenia",
      lead      = "zadania z rozkładów prawdopodobieństwa do samodzielnego rozwiązania.",
      target_id = "ch-cwiczenia"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch8_server <- function(input, output, session) {

  # --- Stan quizu ---
  quiz_state <- reactiveValues(
    active       = FALSE,
    questions    = list(),    # wylosowane pytania (lista)
    current_idx  = 0,         # ktore pytanie (1-based)
    total        = 0,
    correct      = 0,
    wrong        = 0,
    answered     = FALSE,     # czy odpowiedziano na biezace
    current_options = list(), # 3 opcje {value, label}
    finished     = FALSE
  )

  all_questions <- NULL

  # --- Start quizu ---
  observeEvent(input$ch8_start, {
    # Wczytaj pytania (leniwe ladowanie)
    if (is.null(all_questions)) {
      all_questions <<- .load_quiz_questions()
    }

    # Losuj 10 pytan
    n <- min(QUIZ_MAX_QUESTIONS, length(all_questions))
    selected <- sample(all_questions, n)

    quiz_state$questions <- selected
    quiz_state$total <- n
    quiz_state$current_idx <- 1
    quiz_state$correct <- 0
    quiz_state$wrong <- 0
    quiz_state$answered <- FALSE
    quiz_state$finished <- FALSE
    quiz_state$active <- TRUE

    # Przygotuj opcje dla pierwszego pytania
    prepare_options()
  })

  # Przygotuj 3 opcje: 1 poprawna + 2 losowe bledne
  prepare_options <- function() {
    q <- quiz_state$questions[[quiz_state$current_idx]]
    correct_val <- q$correct
    all_vals <- names(QUIZ_OPTIONS)
    wrong_vals <- setdiff(all_vals, correct_val)
    selected_wrong <- sample(wrong_vals, 2)
    opts <- sample(c(correct_val, selected_wrong))
    quiz_state$current_options <- lapply(opts, function(v) {
      list(value = v, label = QUIZ_OPTIONS[[v]])
    })
  }

  # --- Pasek postepu ---
  output$ch8_progress <- renderUI({
    if (!quiz_state$active) return(NULL)

    answered <- quiz_state$correct + quiz_state$wrong
    total <- quiz_state$total
    pct <- round(answered / total * 100)

    tagList(
      div(style = "display: flex; justify-content: space-between; margin-bottom: 4px;",
        span(paste0("Pytanie ", quiz_state$current_idx, " / ", total)),
        span(paste0("Wynik: ", quiz_state$correct, " / ", answered),
             style = paste0("font-weight: bold; color: ",
                            if (answered == 0) upwr_secondary
                            else if (quiz_state$correct / answered >= 0.7) unname(upwr_cat["szalwia"])
                            else if (quiz_state$correct / answered >= 0.5) unname(upwr_cat["bursztyn"])
                            else unname(upwr_cat["terakota"])))
      ),
      div(style = "background: #e9ecef; border-radius: 6px; height: 8px; overflow: hidden;",
        div(style = paste0(
          "background: ", unname(upwr_cat["niebo"]), "; height: 100%; width: ", pct, "%;",
          "border-radius: 6px; transition: width 0.3s;"
        ))
      )
    )
  })

  # --- Pytanie ---
  output$ch8_question_ui <- renderUI({
    if (!quiz_state$active || quiz_state$finished) return(NULL)

    q <- quiz_state$questions[[quiz_state$current_idx]]

    div(
      style = "font-size: 18px; font-weight: 500; color: #2c3e50;
               padding: 20px; background: white; border-radius: 8px;
               border-left: 4px solid #3498db; margin: 15px 0;",
      q$question
    )
  })

  # --- Opcje (3 przyciski) ---
  output$ch8_options_ui <- renderUI({
    if (!quiz_state$active || quiz_state$finished) return(NULL)
    if (quiz_state$answered) return(NULL)

    opts <- quiz_state$current_options
    letters <- c("A", "B", "C")

    div(class = "quiz-tiles quiz-cols-3",
      lapply(seq_along(opts), function(i) {
        btn_id <- paste0("ch8_answer_", i)
        actionButton(btn_id,
          tagList(
            div(class = "tile-letter", letters[i]),
            div(class = "tile-text", opts[[i]]$label)
          ),
          class = "quiz-tile"
        )
      })
    )
  })

  # --- Obsluga klikniec odpowiedzi ---
  observe({
    for (i in 1:3) {
      local({
        idx <- i
        btn_id <- paste0("ch8_answer_", idx)
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
  output$ch8_feedback_ui <- renderUI({
    if (!quiz_state$active || !quiz_state$answered || quiz_state$finished) return(NULL)

    is_correct <- quiz_state$last_correct
    explanation <- quiz_state$last_explanation
    correct_label <- QUIZ_OPTIONS[[quiz_state$last_correct_val]]

    tagList(
      # Wynik
      div(class = if (is_correct) "callout-success" else "callout-danger",
        tags$strong(if (is_correct) "Dobrze!" else "Nie tym razem."),
        if (!is_correct) {
          paste0(" Poprawna odpowiedź: ", correct_label)
        }
      ),

      # Wyjasnienie
      div(style = "background: #f8f9fa; border-radius: 6px; padding: 12px 16px;
                   margin: 10px 0; font-size: 14px; color: #555;",
        explanation
      ),

      # Przycisk dalej
      if (quiz_state$current_idx < quiz_state$total) {
        actionButton("ch8_next", "Następne pytanie →",
                     class = "btn-primary", width = "100%",
                     style = "margin-top: 10px;")
      } else {
        actionButton("ch8_finish", "Zobacz wynik",
                     class = "btn-success btn-lg", width = "100%",
                     style = "margin-top: 10px;")
      }
    )
  })

  # --- Nastepne pytanie ---
  observeEvent(input$ch8_next, {
    quiz_state$current_idx <- quiz_state$current_idx + 1
    quiz_state$answered <- FALSE
    prepare_options()
  })

  # --- Zakonczenie ---
  observeEvent(input$ch8_finish, {
    quiz_state$finished <- TRUE
    quiz_state$answered <- FALSE
  })

  # --- Podsumowanie ---
  output$ch8_summary_ui <- renderUI({
    if (!quiz_state$finished) return(NULL)

    total <- quiz_state$total
    correct <- quiz_state$correct
    pct <- round(correct / total * 100)

    result_color <- if (pct >= 70) unname(upwr_cat["szalwia"])
                    else if (pct >= 50) unname(upwr_cat["bursztyn"])
                    else unname(upwr_cat["terakota"])

    result_text <- if (pct >= 90) "Ćwiczenie zakończone celująco!"
                   else if (pct >= 70) "Dobry wynik!"
                   else if (pct >= 50) "Nieźle, ale warto powtórzyć."
                   else "Powtórz materiał z wcześniejszych rozdziałów."

    div(style = "text-align: center; padding: 30px;",
      # Procent
      div(style = paste0(
        "font-size: 64px; font-weight: bold; color: ", result_color, ";"
      ), paste0(pct, "%")),

      # Szczegoly
      div(style = "font-size: 18px; color: #555; margin: 10px 0;",
        paste0("Poprawne odpowiedzi: ", correct, " / ", total)
      ),

      # Pasek wyniku
      div(style = "max-width: 300px; margin: 15px auto; background: #e9ecef;
                   border-radius: 10px; height: 16px; overflow: hidden;",
        div(style = paste0(
          "background: ", result_color, "; height: 100%; width: ", pct, "%;
           border-radius: 10px; transition: width 0.5s;"
        ))
      ),

      # Tekst oceny
      div(style = paste0("font-size: 16px; font-weight: bold; color: ", result_color, ";
                          margin: 10px 0 20px 0;"),
        result_text
      ),

      # Przyciski
      div(style = "display: flex; gap: 10px; justify-content: center;",
        actionButton("ch8_start", "Spróbuj ponownie",
                     class = "btn-primary btn-lg"),
        actionButton("ch8_back_to_ch7", "Wróć do ściągi",
                     class = "btn-outline-secondary btn-lg")
      )
    )
  })

  # Nawigacja powrotna
  observeEvent(input$ch8_back_to_ch7, {
    updateNavbarPage(session, "main_nav", selected = "7. Ściąga")
  })

}
