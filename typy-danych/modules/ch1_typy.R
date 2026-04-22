# ============================================================================
# CHAPTER 1: Typy danych
# ============================================================================

ch1_ui <- list(
  id = "ch-typy", num = "01", title = "Typy danych",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 01 · Statystyka opisowa",
      num    = "01",
      title  = "Typy danych.",
      lead   = "Pierwszym krokiem w analizie danych jest rozpoznanie typu zmiennej,
               z którą mamy do czynienia. To nie jest formalność — typ zmiennej
               determinuje jakie statystyki możemy obliczyć, jakie wykresy narysować
               i jakie testy statystyczne zastosować."
    ),

    div(class = "narrative",
      p("Błędne rozpoznanie typu zmiennej prowadzi do błędnych analiz.
        Na przykład, obliczanie średniej z kodów pocztowych nie ma sensu,
        mimo że są to liczby.")
    ),

    margin_callout(
      label = "Zasada",
      "Zanim zaczniesz analizę — zawsze określ typ każdej zmiennej.",
      color = "uwaga"
    ),

    # --- Widget 1: Taxonomy tree ---
    h2(id = "ch1-taksonomia", class = "section-title", "Taksonomia typów danych"),

    div(class = "narrative",
      p("Ponizszy diagram przedstawia hierarchie typow danych.
        Kliknij na liscie drzewa (najnizszy poziom), aby odkryc
        przyklady zmiennych każdego typu z naszego zbioru danych.")
    ),

    figure_panel(
      label = "Ryc. 1.1",
      title = "Taksonomia typów danych",
      div(class = "taxonomy-tree",
        tags$ul(
          tags$li(
            div(class = "tax-node", "Dane"),
            tags$ul(
              tags$li(
                div(class = "tax-node", HTML("Ilościowe<br><small>(liczbowe)</small>")),
                tags$ul(
                  tags$li(
                    div(class = "tax-leaf", id = "ch1_leaf_ciagla",
                      style = paste0("background:", type_colors["ilosciowa_ciagla"], ";"),
                      onclick = "Shiny.setInputValue('ch1_leaf_click', 'ciagla', {priority:'event'})",
                      "Ciągłe"
                    )
                  ),
                  tags$li(
                    div(class = "tax-leaf", id = "ch1_leaf_dyskretna",
                      style = paste0("background:", type_colors["ilosciowa_dyskretna"], ";"),
                      onclick = "Shiny.setInputValue('ch1_leaf_click', 'dyskretna', {priority:'event'})",
                      "Dyskretne"
                    )
                  )
                )
              ),
              tags$li(
                div(class = "tax-node", HTML("Jakościowe<br><small>(kategoryczne)</small>")),
                tags$ul(
                  tags$li(
                    div(class = "tax-leaf", id = "ch1_leaf_porzadkowa",
                      style = paste0("background:", type_colors["porzadkowa"], ";"),
                      onclick = "Shiny.setInputValue('ch1_leaf_click', 'porzadkowa', {priority:'event'})",
                      "Porządkowe"
                    )
                  ),
                  tags$li(
                    div(class = "tax-leaf", id = "ch1_leaf_nominalna",
                      style = paste0("background:", type_colors["nominalna"], ";"),
                      onclick = "Shiny.setInputValue('ch1_leaf_click', 'nominalna', {priority:'event'})",
                      "Nominalne"
                    )
                  )
                )
              )
            )
          )
        )
      ),
      p(style = "text-align: center; font-size: 13px; color: var(--upwr-reference); margin: 8px 0 0;",
        "Kliknij na kolorowy liść drzewa, aby zobaczyć przykłady"),
      uiOutput("ch1_leaf_detail")
    ),

    margin_callout(
      label = "Uwaga",
      "Granica między typami nie zawsze jest ostra. Na przykład ocena wykładowcy
       w skali 1–10 może być traktowana jako porządkowa lub dyskretna,
       w zależności od kontekstu i celu analizy.",
      color = "uwaga"
    ),

    # --- Widget 2: Examples gallery ---
    h2(id = "ch1-przyklady", class = "section-title", "Przykłady typów zmiennych"),

    div(class = "narrative",
      p("Zobaczmy jak wyglada każdy typ zmiennej w praktyce.
        Kazdy typ ma swoje charakterystyczne cechy i wymaga
        odpowiednich narzedzi wizualizacji."),
      p("Wlacz opcje poniżej, aby zobaczyc co sie stanie,
        gdy uzyjemy nieodpowiedniego wykresu.")
    ),

    figure_panel(
      label = "Ryc. 1.2",
      title = "Cztery typy zmiennych — jeden wykres na typ",
      checkboxInput("ch1_show_bad",
                    "Pokaż nieodpowiednie wykresy (przykład czego NIE robić)",
                    value = FALSE),
      fluidRow(
        column(6,
          div(class = "example-card",
              style = paste0("border-color: ", type_colors["nominalna"], ";"),
            span(class = "type-badge",
                 style = paste0("background: ", type_colors["nominalna"], ";"),
                 "Jakościowa nominalna"),
            tags$h4("Płeć"),
            tags$p(style = "color: var(--upwr-ink-soft); font-size: 13px;",
              "Kategorie bez naturalnego porzadku. Mozemy liczyc ile jest
               obserwacji w każdej kategorii, ale nie mozemy ich uporządkowac
               ani uśredniać."),
            plotOutput("ch1_ex1_plot", height = "280px")
          )
        ),
        column(6,
          div(class = "example-card",
              style = paste0("border-color: ", type_colors["porzadkowa"], ";"),
            span(class = "type-badge",
                 style = paste0("background: ", type_colors["porzadkowa"], ";"),
                 "Jakościowa porządkowa"),
            tags$h4("Zadowolenie ze studiów"),
            tags$p(style = "color: var(--upwr-ink-soft); font-size: 13px;",
              "Kategorie z naturalnym porzadkiem. Wiemy ze 'Bardzo zadowolony'
               jest wyzej niż 'Zadowolony', ale nie znamy dokladnych odleglosci
               miedzy kategoriami."),
            plotOutput("ch1_ex2_plot", height = "280px")
          )
        )
      ),
      fluidRow(
        column(6,
          div(class = "example-card",
              style = paste0("border-color: ", type_colors["ilosciowa_dyskretna"], ";"),
            span(class = "type-badge",
                 style = paste0("background: ", type_colors["ilosciowa_dyskretna"], ";"),
                 "Ilościowa dyskretna"),
            tags$h4("Liczba kursów"),
            tags$p(style = "color: var(--upwr-ink-soft); font-size: 13px;",
              "Wartości liczbowe, ale tylko całkowite. Mozemy obliczać srednia
               i odchylenie standardowe. Wykres słupkowy jest tu odpowiedni,
               bo mamy skończoną liczbę wartości."),
            plotOutput("ch1_ex3_plot", height = "280px")
          )
        ),
        column(6,
          div(class = "example-card",
              style = paste0("border-color: ", type_colors["ilosciowa_ciagla"], ";"),
            span(class = "type-badge",
                 style = paste0("background: ", type_colors["ilosciowa_ciagla"], ";"),
                 "Ilościowa ciągła"),
            tags$h4("Wzrost (cm)"),
            tags$p(style = "color: var(--upwr-ink-soft); font-size: 13px;",
              "Wartości liczbowe, ktore mogą przyjmowac dowolne wartości
               z pewnego przedzialu (takze ulamkowe). Histogram grupuje
               wartości w przedziały, gęstość wygładza rozkład."),
            plotOutput("ch1_ex4_plot", height = "280px")
          )
        )
      )
    ),

    # --- Widget 4: Dataset preview ---
    h2(id = "ch1-dane", class = "section-title", "Nasze dane — ankieta studencka"),

    div(class = "narrative",
      p("A tak wyglądają nasze dane - ankieta 200 studentow.
        To z tego zbioru beda pochodzic wszystkie przyklady
        w dalszej czesci kursu. Ponizej pierwszych 10 obserwacji.")
    ),

    figure_panel(
      label = "Ryc. 1.3",
      title = "Pierwsze 10 obserwacji — nasz zbiór danych",
      div(style = "overflow-x: auto; font-size: 12px;",
        tableOutput("ch1_data_preview")
      )
    ),

    margin_callout(
      label = "Zwróć uwagę",
      "Zbiór zawiera zmienne wszystkich czterech typów. W kolejnych
       rozdziałach nauczymy się, jak je podsumowywać i wizualizować."
    ),

    margin_code_note(
      code = "str(student_data)\nsummary(student_data)",
      description = "Dwa podstawowe narzędzia do podglądu struktury zbioru w R."
    ),

    # --- Variable tracker selector ---
    figure_panel(
      label = "Narzędzie",
      title = "🔍 Śledź zmienną przez cały kurs",
      color = upwr_single_alt,
      p(style = "font-family: var(--upwr-serif); font-size: 15px; color: var(--upwr-ink-soft); margin-bottom: 14px;",
        "Wybierz jedną zmienną ilościową. W każdym kolejnym rozdziale zobaczysz,
         jakie nowe informacje dają Ci kolejne narzędzia statystyczne zastosowane
         do tej samej zmiennej."),
      selectInput("tracked_var", "Wybierz zmienną do śledzenia:",
        choices = c("Wzrost (cm)" = "wzrost",
                    "Waga (kg)" = "waga",
                    "Czas dojazdu (min)" = "czas_dojazdu",
                    "Średnia ocen" = "srednia_ocen"),
        selected = "wzrost", width = "300px"
      )
    ),

    lc_chapter_next(
      num       = "02",
      title     = "Zmienne jakościowe",
      lead      = "Jak podsumować kategorie — są prostsze i stanowią naturalny punkt wyjścia.",
      target_id = "ch-jakosciowe"
    ),

    # Bottom spacing
    div(style = "height: 40px;")

  )
)

# --------------------------------------------------------------------------
# Chapter 1 Server
# --------------------------------------------------------------------------

ch1_server <- function(input, output, session) {

  ch1_selected_leaf <- reactiveVal(NULL)

  # --- Widget 1: Taxonomy tree (HTML) ---

  .leaf_info <- list(
    ciagla = list(
      label = "Ciągłe", color = type_colors["ilosciowa_ciagla"],
      desc = "Wartości liczbowe, które mogą przyjmować dowolną wartość z przedziału (także ułamkowe).",
      examples = "wzrost (cm), waga (kg), czas dojazdu (min), średnia ocen"
    ),
    dyskretna = list(
      label = "Dyskretne", color = type_colors["ilosciowa_dyskretna"],
      desc = "Wartości liczbowe, ale tylko całkowite — można je policzyć.",
      examples = "liczba kursów, liczba nieobecności"
    ),
    porzadkowa = list(
      label = "Porządkowe", color = type_colors["porzadkowa"],
      desc = "Kategorie z naturalnym porządkiem, ale odległości między nimi nie są znane.",
      examples = "rok studiów (1 < 2 < 3 < ...), zadowolenie ze studiów, ocena wykładowcy"
    ),
    nominalna = list(
      label = "Nominalne", color = type_colors["nominalna"],
      desc = "Kategorie bez naturalnego porządku — można je tylko liczyć.",
      examples = "płeć, kierunek studiów, grupa krwi"
    )
  )

  observeEvent(input$ch1_leaf_click, {
    leaf_id <- input$ch1_leaf_click
    if (identical(ch1_selected_leaf(), leaf_id)) {
      ch1_selected_leaf(NULL)
    } else {
      ch1_selected_leaf(leaf_id)
    }
  })

  output$ch1_leaf_detail <- renderUI({
    sel <- ch1_selected_leaf()
    if (is.null(sel)) return(NULL)
    info <- .leaf_info[[sel]]
    div(class = "tax-detail",
      style = paste0("border-left: 4px solid ", info$color, ";"),
      tags$strong(style = paste0("color: ", info$color, "; font-size: 16px;"),
        info$label),
      p(style = "margin: 6px 0 4px; font-size: 14px;", info$desc),
      p(style = "margin: 0; font-size: 13px; color: var(--upwr-ink-soft);",
        tags$em("W naszych danych: "), info$examples)
    )
  })

  # --- Widget 2: Examples gallery ---

  output$ch1_ex1_plot <- renderPlot({
    if (input$ch1_show_bad) {
      render_bad_plot(student_data$plec, "Płeć", "nominalna")
    } else {
      render_good_plot(student_data$plec, "Płeć", "nominalna")
    }
  })

  output$ch1_ex2_plot <- renderPlot({
    if (input$ch1_show_bad) {
      render_bad_plot(student_data$zadowolenie, "Zadowolenie", "porzadkowa")
    } else {
      render_good_plot(student_data$zadowolenie, "Zadowolenie", "porzadkowa")
    }
  })

  output$ch1_ex3_plot <- renderPlot({
    if (input$ch1_show_bad) {
      render_bad_plot(student_data$liczba_kursow, "Liczba kursów", "ilosciowa_dyskretna")
    } else {
      render_good_plot(student_data$liczba_kursow, "Liczba kursów", "ilosciowa_dyskretna")
    }
  })

  output$ch1_ex4_plot <- renderPlot({
    if (input$ch1_show_bad) {
      render_bad_plot(student_data$wzrost, "Wzrost (cm)", "ilosciowa_ciagla")
    } else {
      render_good_plot(student_data$wzrost, "Wzrost (cm)", "ilosciowa_ciagla")
    }
  })

  # --- Widget 4: Dataset preview ---

  output$ch1_data_preview <- renderTable({
    head(student_data, 10)
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

}
