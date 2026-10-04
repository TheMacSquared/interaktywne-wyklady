# ============================================================================
# FUNKCJE POMOCNICZE - Case studies
# ============================================================================

case_explore   <- unname(upwr_cat["niebo"])
case_test      <- unname(upwr_cat["wrzos"])
case_model     <- unname(upwr_cat["szalwia"])
case_conclude  <- unname(upwr_cat["bursztyn"])
case_highlight <- upwr_accent
case_reference <- upwr_secondary
case_muted     <- upwr_reference


# Formatowanie wyniku testu
format_decision <- function(p_value, alpha = 0.05) {
  if (p_value < alpha) {
    list(text = "Odrzucamy H₀", color = case_highlight, icon = "✗")
  } else {
    list(text = "Brak podstaw do odrzucenia H₀", color = case_model, icon = "✓")
  }
}


# Pytanie z kartami odpowiedzi (lc-choices): poprawna karta zielenieje po
# wyborze, pod kartami pojawia się wyjaśnienie z case_quiz_server().
case_quiz_ui <- function(id, title, question, choices, correct) {
  figure_panel(
    label = "Pytanie", title = title,
    p(question),
    tags$div(class = "lc-choices", `data-correct` = correct,
      radioButtons(id, NULL, choices = choices, selected = character(0))
    ),
    uiOutput(paste0(id, "_fb"))
  )
}

# feedback: lista nazwana wartościami odpowiedzi; tekst po werdykcie.
case_quiz_server <- function(input, output, id, correct, feedback) {
  output[[paste0(id, "_fb")]] <- renderUI({
    choice <- input[[id]]
    if (is.null(choice) || identical(choice, character(0))) return(NULL)
    ok <- identical(choice, correct)
    lc_status(
      lc_verdict(tags$strong(if (ok) "Tak." else "Nie."),
                 type = if (ok) "ok" else "danger"),
      " ", feedback[[choice]]
    )
  })
}
