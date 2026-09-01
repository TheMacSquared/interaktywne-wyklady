# ==========================================================================
# ROZDZIAŁ 1: OD ŻARTU DO PRECYZYJNEGO OPISU
# ==========================================================================

.risk_term_choices <- stats::setNames(
  names(risk_term_labels),
  unname(risk_term_labels)
)

.risk_item_ui <- function(item_id, item_text) {
  fluidRow(
    column(
      8,
      tags$p(class = "lc-p", style = "margin-top:0.55rem;", item_text)
    ),
    column(
      4,
      selectInput(
        inputId = paste0("ch1_term_", item_id),
        label = paste("Kategoria dla:", item_text),
        choices = c("— wybierz —" = "", .risk_term_choices),
        selected = "",
        width = "100%"
      )
    )
  )
}

ch1_ui <- lecture_chapter(
  id = "ch-sytuacja",
  num = "01",
  title = "Co tu jest ryzykiem?",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 01 · Język ryzyka",
      num = "01",
      title = "Skórka to jeszcze nie wypadek.",
      lead = "W korytarzu Bananpolu znaleziono skórkę od banana. Brzmi jak
              dowcip, ale pozwala precyzyjnie oddzielić zagrożenie, ekspozycję,
              zdarzenie, skutek i zabezpieczenie."
    ),

    lc_h2("ch1-bananpol", "Witamy w Bananpolu"),
    lc_p(
      "Bananpol jest fikcyjnym importerem bananów. Firma ma rampę rozładunkową,
       dojrzewalnię z chłodnią, magazyn wysokiego składowania, linię sortowania
       i pakowania, wózki widłowe oraz instalację chłodniczą z alarmami.
       Właśnie zaczynasz tu pracę jako inspektor bezpieczeństwa."
    ),
    lc_p(
      "Przez całą serię wykładów będziesz uzupełniać mapę ryzyka tej firmy: od
       dzisiejszej skórki na korytarzu, przez alarmy i awarie urządzeń, aż po
       drzewo błędów całej instalacji w finale. Każde nowe pojęcie dostanie
       swoje miejsce w tym samym zakładzie, więc wyniki z kolejnych wykładów
       będą do siebie pasować."
    ),

    margin_callout(
      label = "Dane fikcyjne",
      "Wszystkie liczby w kursie są wymyślone na potrzeby dydaktyki i nie
       opisują żadnej prawdziwej firmy. Prawdziwe są tylko metody.",
      color = "uwaga"
    ),

    margin_callout(
      label = "Pytanie na start",
      "Czy obecność skórki oznacza, że doszło do wypadku? Najpierw odpowiedz
       intuicyjnie, dopiero potem uporządkuj historię.",
      color = "wskazowka"
    ),

    lc_h2("ch1-slownik", "Pięć różnych elementów jednej historii"),
    lc_p(
      "W analizie ryzyka podobne słowa bywają używane zamiennie. Tutaj każde
       ma osobną rolę. Zagrożenie może spowodować szkodę, ekspozycja tworzy
       kontakt z zagrożeniem, zdarzenie opisuje to, co zaszło, a skutek mówi o
       następstwie. Zabezpieczenie ma przerwać ten łańcuch."
    ),

    lc_stat_grid(
      lc_stat_box("Zagrożenie", "Źródło możliwej szkody", color = upwr_cat[["terakota"]]),
      lc_stat_box("Ekspozycja", "Kontakt w danych warunkach", color = upwr_cat[["bursztyn"]]),
      lc_stat_box("Zdarzenie", "To, co faktycznie zaszło", color = upwr_accent),
      lc_stat_box("Skutek", "Następstwo zdarzenia", color = upwr_cat[["wrzos"]]),
      lc_stat_box("Zabezpieczenie", "Element przerywający łańcuch", color = upwr_cat[["szalwia"]]),
      columns = 3
    ),

    lc_h2("ch1-klasyfikacja", "Uporządkuj incydent Bananpolu"),
    lc_p(
      "Przypisz każdemu zdaniu jedną rolę. Ta wersja używa list wyboru zamiast
       przeciągania kart, dzięki czemu działa również z klawiatury."
    ),

    figure_panel(
      label = "Interakcja 1",
      title = "Od zagrożenia do zabezpieczenia",
      full_width = TRUE,
      tagList(lapply(seq_len(nrow(risk_scenario_items)), function(i) {
        .risk_item_ui(risk_scenario_items$id[[i]], risk_scenario_items$text[[i]])
      })),
      actionButton(
        "ch1_check",
        "Sprawdź klasyfikację",
        class = "lc-btn-primary"
      ),
      uiOutput("ch1_feedback")
    ),

    lc_feedback(
      type = "warning",
      tags$strong("Pułapka:"),
      " zagrożenie nie jest zdarzeniem, a prawdopodobieństwo zdarzenia nie
        opisuje jeszcze dotkliwości skutku."
    ),

    lc_chapter_next(
      num = "02",
      title = "Częstość i prawdopodobieństwo",
      lead = "Sprawdzimy, dlaczego jeden miesiąc obserwacji potrafi mylić.",
      target_id = "ch-czestosc"
    )
  )
)

ch1_server <- function(input, output, session) {
  checked <- reactiveVal(FALSE)

  observeEvent(input$ch1_check, {
    checked(TRUE)
  })

  output$ch1_feedback <- renderUI({
    req(checked())

    answers <- stats::setNames(
      vapply(risk_scenario_items$id, function(item_id) {
        value <- input[[paste0("ch1_term_", item_id)]]
        if (is.null(value)) "" else value
      }, character(1)),
      risk_scenario_items$id
    )
    result <- score_risk_classification(answers)

    details <- lapply(seq_len(nrow(risk_scenario_items)), function(i) {
      selected <- answers[[risk_scenario_items$id[[i]]]]
      correct_code <- risk_scenario_items$correct[[i]]
      is_correct <- result$correct[[i]]
      selected_label <- if (nzchar(selected)) risk_term_labels[[selected]] else "brak odpowiedzi"

      tags$li(
        tags$strong(paste0(i, ". ")),
        if (is_correct) {
          paste0("Dobrze: ", risk_term_labels[[correct_code]], ". ")
        } else {
          paste0(
            "Wybrano: ", selected_label, "; poprawnie: ",
            risk_term_labels[[correct_code]], ". "
          )
        },
        risk_scenario_items$explanation[[i]]
      )
    })

    lc_feedback(
      type = if (result$score == result$total) "ok" else "warning",
      tags$strong(sprintf("Wynik: %d/%d.", result$score, result$total)),
      if (result$score == result$total) {
        " Historia jest uporządkowana — można teraz zdefiniować zdarzenie do obliczeń."
      } else {
        " Sprawdź różnicę między źródłem szkody, kontaktem, zdarzeniem i następstwem."
      },
      tags$ul(details)
    )
  })
}
