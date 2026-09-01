# ==========================================================================
# ROZDZIAŁ 8: ĆWICZENIA
# ==========================================================================

.required_report_fields <- c("definition", "exposure", "period", "consequence")

ch8_ui <- lecture_chapter(
  id = "ch-cwiczenia",
  num = "08",
  title = "Ćwiczenia",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 08 · Język ryzyka",
      num = "08",
      title = "Trzy wypadki. I co z tego?",
      lead = "Dyrektor Bananpolu dostał komunikat: „W czerwcu mieliśmy trzy
              wypadki, a drugi magazyn pięć, więc jesteśmy bezpieczniejsi”.
              Twoim zadaniem jest zatrzymać zbyt szybki wniosek."
    ),

    lc_h2("ch8-diagnoza", "Uzupełnij informację przed decyzją"),
    lc_p(
      "Zaznacz dane, których potrzebujesz, aby porównać częstość, a następnie
       pełne ryzyko obu magazynów. Wszystkie wybrane informacje powinny mieć tę
       samą definicję po obu stronach porównania."
    ),

    figure_panel(
      label = "Ćwiczenie 1",
      title = "Notatka dla dyrektora — czego brakuje?",
      full_width = TRUE,
      checkboxGroupInput(
        "ch8_fields",
        "Czego brakuje?",
        choices = c(
          "Jednoznacznej definicji zdarzenia i zasad rejestracji" = "definition",
          "Liczby porównywalnych ekspozycji, np. przejść lub pracownikogodzin" = "exposure",
          "Wspólnego okresu i informacji o warunkach pracy" = "period",
          "Informacji o rodzaju oraz dotkliwości skutków" = "consequence"
        )
      ),
      radioButtons(
        "ch8_conclusion",
        "Który wniosek jest teraz uzasadniony?",
        choices = c(
          "Bananpol jest bezpieczniejszy, bo 3 < 5" = "safer",
          "Magazyny są równie bezpieczne" = "equal",
          "Na podstawie samych liczników nie da się ich porównać" = "insufficient"
        ),
        selected = character(0)
      ),
      actionButton(
        "ch8_check",
        "Sprawdź rekomendację",
        class = "lc-btn-primary"
      ),
      uiOutput("ch8_feedback")
    ),

    lc_h2("ch8-zbiory", "Policz działania na zdarzeniach"),
    figure_panel(
      label = "Ćwiczenie 2",
      title = "Sto kontroli rampy",
      full_width = TRUE,
      lc_p(
        "W 100 kontrolach rampy zdarzenie A — zastawione przejście — wystąpiło
         28 razy. Zdarzenie B — brak oznakowania — wystąpiło 17 razy. Oba
         zdarzenia wystąpiły jednocześnie 6 razy."
      ),
      tags$ol(
        tags$li("Ile kontroli należało do A ∩ B?"),
        tags$li("Ile kontroli należało do A ∪ B?"),
        tags$li("Ile kontroli nie należało ani do A, ani do B?"),
        tags$li("Czy A i B są rozłączne? Uzasadnij jednym zdaniem.")
      ),
      actionButton("ch8_sets_solution", "Pokaż tok rozwiązania", class = "lc-btn-ok-outline"),
      uiOutput("ch8_sets_feedback")
    ),

    lc_h2("ch8-model", "Rozpoznaj punkt startu"),
    lc_p(
      "Dla każdej sytuacji wybierz: definicja klasyczna, częstość empiryczna
       albo potrzeba dalszego modelu i danych."
    ),
    figure_panel(
      label = "Ćwiczenie 3",
      title = "Nie każdy ułamek znaczy to samo",
      full_width = TRUE,
      selectInput(
        "ch8_model_1",
        "1. Spośród 30 ponumerowanych palet losujemy jedną; 4 mają uszkodzone zabezpieczenie.",
        choices = c(
          "— wybierz —" = "",
          "Definicja klasyczna" = "classical",
          "Częstość empiryczna" = "empirical",
          "Dalszy model i dane" = "model"
        )
      ),
      selectInput(
        "ch8_model_2",
        "2. W rejestrze 8 ze 100 porównywalnych zmian zawierało zdarzenie.",
        choices = c(
          "— wybierz —" = "",
          "Definicja klasyczna" = "classical",
          "Częstość empiryczna" = "empirical",
          "Dalszy model i dane" = "model"
        )
      ),
      selectInput(
        "ch8_model_3",
        "3. Chcemy przewidzieć jutrzejsze ryzyko przy deszczu, większym ruchu i nowej procedurze sprzątania.",
        choices = c(
          "— wybierz —" = "",
          "Definicja klasyczna" = "classical",
          "Częstość empiryczna" = "empirical",
          "Dalszy model i dane" = "model"
        )
      ),
      actionButton("ch8_models_check", "Sprawdź dobór", class = "lc-btn-primary"),
      uiOutput("ch8_models_feedback")
    ),

    lc_h2("ch8-transfer", "Przenieś język poza Bananpol"),
    figure_panel(
      label = "Ćwiczenie 4",
      title = "Alarm gazowy w laboratorium",
      full_width = TRUE,
      lc_p(
        "W dwóch zdaniach zdefiniuj zagrożenie, ekspozycję, zdarzenie, skutek i
         zabezpieczenie dla sytuacji: czujnik sygnalizuje wzrost stężenia gazu
         w laboratorium, w którym pracują trzy osoby. Dodaj jednostkę i okres,
         względem których można byłoby obserwować częstość zdarzenia."
      ),
      textAreaInput(
        "ch8_transfer_text",
        "Twoja odpowiedź",
        rows = 6,
        placeholder = "Zagrożenie: ... Ekspozycja: ... Zdarzenie: ..."
      ),
      actionButton("ch8_transfer_rubric", "Pokaż kryteria samooceny", class = "lc-btn-ok-outline"),
      uiOutput("ch8_transfer_feedback")
    ),

    lc_h2("ch8-wzorzec", "Wzorzec poprawionego komunikatu"),
    lc_feedback(
      type = "ok",
      "„W czerwcu magazyn A zgłosił 3 zdarzenia, a magazyn B — 5. Przed
        porównaniem potrzebujemy wspólnej definicji zdarzenia, porównywalnej
        ekspozycji i danych o skutkach. Same liczniki nie uzasadniają rankingu
        bezpieczeństwa.”"
    ),

    lc_h2("ch8-most", "Co zmieni dodatkowa informacja?"),
    lc_p(
      "W tym wykładzie ustaliliśmy mianownik i język zdarzeń. W następnym
       sprawdzimy, jak informacja o warunkach — mokrej posadzce, natężeniu ruchu
       albo niesprawnym sprzątaniu — zmienia ocenę prawdopodobieństwa."
    ),

    lc_feedback(
      type = "info",
      tags$strong("Pytanie wyjściowe:"),
      " Jakiego jednego zdania zabrakło w ostatnim raporcie o bezpieczeństwie,
        który czytałeś lub przygotowywałeś?"
    )
  )
)

ch8_server <- function(input, output, session) {
  check_count <- reactiveVal(0L)
  sets_revealed <- reactiveVal(FALSE)
  models_check_count <- reactiveVal(0L)
  rubric_revealed <- reactiveVal(FALSE)

  observeEvent(input$ch8_check, {
    check_count(check_count() + 1L)
  })

  observeEvent(input$ch8_sets_solution, sets_revealed(TRUE))
  observeEvent(input$ch8_models_check, models_check_count(models_check_count() + 1L))
  observeEvent(input$ch8_transfer_rubric, rubric_revealed(TRUE))

  output$ch8_feedback <- renderUI({
    req(check_count() > 0)
    selected_fields <- input$ch8_fields
    if (is.null(selected_fields)) selected_fields <- character()
    selected_conclusion <- input$ch8_conclusion
    if (is.null(selected_conclusion)) selected_conclusion <- ""

    missing_fields <- setdiff(.required_report_fields, selected_fields)
    extra_fields <- setdiff(selected_fields, .required_report_fields)
    fields_ok <- length(missing_fields) == 0 && length(extra_fields) == 0
    conclusion_ok <- identical(selected_conclusion, "insufficient")
    all_ok <- fields_ok && conclusion_ok

    missing_labels <- c(
      definition = "definicja zdarzenia",
      exposure = "mianownik ekspozycji",
      period = "wspólny okres i warunki",
      consequence = "rodzaj skutków"
    )

    lc_feedback(
      type = if (all_ok) "ok" else "warning",
      tags$strong(if (all_ok) "Rekomendacja jest kompletna." else "Wstrzymaj decyzję."),
      if (!fields_ok) {
        paste0(
          " Brakuje: ",
          paste(unname(missing_labels[missing_fields]), collapse = ", "),
          "."
        )
      },
      if (!conclusion_ok) {
        " Same liczniki 3 i 5 nie pozwalają jeszcze ustalić, który magazyn jest bezpieczniejszy."
      },
      if (all_ok) {
        " Najpierw ujednolicamy definicje i ekspozycję, potem porównujemy częstości i skutki."
      }
    )
  })

  output$ch8_sets_feedback <- renderUI({
    req(sets_revealed())
    union_count <- 28 + 17 - 6
    neither_count <- 100 - union_count

    lc_feedback(
      type = "ok",
      tags$ol(
        tags$li("A ∩ B zawiera 6 kontroli — tę liczbę podano w treści."),
        tags$li(sprintf("A ∪ B zawiera 28 + 17 − 6 = %d kontroli.", union_count)),
        tags$li(sprintf("Ani A, ani B: 100 − %d = %d kontroli.", union_count, neither_count)),
        tags$li("Zdarzenia nie są rozłączne, ponieważ ich część wspólna zawiera 6 wyników.")
      )
    )
  })

  output$ch8_models_feedback <- renderUI({
    req(models_check_count() > 0)
    answers <- c(input$ch8_model_1, input$ch8_model_2, input$ch8_model_3)
    answers[is.na(answers)] <- ""
    correct <- c("classical", "empirical", "model")
    score <- sum(answers == correct)

    lc_feedback(
      type = if (score == 3) "ok" else "warning",
      tags$strong(sprintf("Wynik: %d/3.", score)),
      tags$ol(
        tags$li("Losowanie palety: definicja klasyczna, jeśli procedura zapewnia równe szanse."),
        tags$li("Rejestr zmian: częstość empiryczna z konkretnych obserwacji."),
        tags$li("Prognoza przy nowych warunkach: potrzebny dalszy model i dane o warunkach.")
      )
    )
  })

  output$ch8_transfer_feedback <- renderUI({
    req(rubric_revealed())
    lc_feedback(
      type = "info",
      tags$strong("Sprawdź, czy odpowiedź zawiera:"),
      tags$ul(
        tags$li("źródło możliwej szkody, a nie tylko nazwę wypadku;"),
        tags$li("osoby i warunki ekspozycji;"),
        tags$li("jedno obserwowalne zdarzenie;"),
        tags$li("możliwy skutek oraz barierę;"),
        tags$li("mianownik, np. zmianę laboratoryjną, i jednoznaczny okres.")
      )
    )
  })
}
