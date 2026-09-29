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
      title = "Trzy wypadki to jeszcze nie wniosek.",
      lead = "Dyrektor Bananpolu dostał komunikat: „W czerwcu mieliśmy trzy
              wypadki, a drugi magazyn pięć, więc jesteśmy bezpieczniejsi”.
              Twoim zadaniem jest zatrzymać zbyt szybki wniosek."
    ),

    lc_p(
      "Ćwiczenia sprawdzają trzy umiejętności z tego wykładu. Pierwsza to
       zatrzymanie wniosku, który opiera się na samym liczniku — to problem
       mianownika z rozdziału 02. Druga to rachunek na zdarzeniach według wzorów
       (1.2)–(1.6). Trzecia to rozpoznanie, skąd w danej sytuacji bierze się
       prawdopodobieństwo: z symetrii, z rejestru czy dopiero z modelu. Zadania 5–7
       na końcu rozdziału mają odpowiedzi zwinięte pod treścią."
    ),

    lc_h2("ch8-diagnoza", "Uzupełnij informację przed decyzją"),
    lc_p(
      "Zaznacz dane, których potrzebujesz, aby porównać częstość, a następnie
       pełne ryzyko obu magazynów. Wszystkie wybrane informacje powinny mieć tę
       samą definicję po obu stronach porównania."
    ),

    figure_panel(
      label = "Zadanie 1",
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
      label = "Zadanie 2",
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
      label = "Zadanie 3",
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
      label = "Zadanie 4",
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

    lc_h2("ch8-dodatkowe", "Zadania do samodzielnego rozwiązania"),
    figure_panel(
      label = "Zadania 5–7",
      title = "Przestrzeń, aksjomaty i częstość",
      full_width = TRUE,
      tags$ol(
        start = 5,
        tags$li(
          "Audytor losuje jedną zmianę z 15 par dzień–zmiana (przykład 1.3).
           C — wylosowano zmianę ranną, D — wylosowano poniedziałek lub wtorek.
           Oblicz P(C ∪ D) oraz prawdopodobieństwo, że nie zaszło ani C, ani D.",
          tags$details(
            class = "lc-exercise-answer",
            tags$summary("Odpowiedź"),
            tags$p("|C| = 5, |D| = 2 · 3 = 6, C ∩ D = {(pon, ranna), (wt, ranna)}, więc |C ∩ D| = 2."),
            tags$p("Ze wzoru (1.5): P(C ∪ D) = 5/15 + 6/15 − 2/15 = 9/15 = 0,6."),
            tags$p("Z praw de Morgana (1.6) i wzoru (1.4): P(Cᶜ ∩ Dᶜ) = 1 − 0,6 = 0,4. Sprawdzenie: 3 dni (śr–pt) × 2 zmiany nieranne = 6 wyników, 6/15 = 0,4.")
          )
        ),
        tags$li(
          "W arkuszu oceny czujnika gazu w chłodni wpisano prawdopodobieństwa
           czterech wyników, które wykluczają się i wyczerpują wszystkie możliwości
           w ciągu jednej zmiany: brak alarmu 0,70; alarm fałszywy 0,20; alarm
           prawdziwy 0,15; awaria czujnika 0,02. Czy takie przypisanie jest
           dopuszczalne?",
          tags$details(
            class = "lc-exercise-answer",
            tags$summary("Odpowiedź"),
            tags$p("Nie. Wyniki są rozłączne i razem tworzą Ω, więc z aksjomatów (1.7) ich prawdopodobieństwa muszą sumować się do P(Ω) = 1. Tymczasem 0,70 + 0,20 + 0,15 + 0,02 = 1,07."),
            tags$p("Arkusz jest wewnętrznie sprzeczny niezależnie od danych: co najmniej jedna wartość jest błędna. Trzeba wrócić do źródła każdej liczby, a nie „przeskalować” wszystkie tak, żeby suma wyszła 1.")
          )
        ),
        tags$li(
          "W rejestrze korytarza przy pakowni 12 ze 150 zmian zawierało
           poślizgnięcie. Oblicz częstość empiryczną. Przyjmując p = 0,08, oszacuj
           typowe odchylenie częstości w serii 150 zmian i oceń, czy wynik różny o
           0,01 od poprzedniego roku jest mocnym sygnałem zmiany.",
          tags$details(
            class = "lc-exercise-answer",
            tags$summary("Odpowiedź"),
            tags$p("Ze wzoru (1.1): p̂ = 12/150 = 0,08."),
            tags$p("Typowe odchylenie: √(0,08 · 0,92 / 150) ≈ 0,022. Różnica 0,01 jest ponad dwa razy mniejsza niż typowe wahanie częstości przy tej liczbie zmian, więc nie jest mocnym sygnałem zmiany. Potrzeba dłuższej serii albo informacji o zmianie warunków.")
          )
        )
      )
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
