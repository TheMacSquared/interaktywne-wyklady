# ============================================================================
# CHAPTER 1: Typy danych
# ============================================================================

ch1_ui <- list(
  id = "ch-typy", num = "01", title = "Typy danych",
  content = tagList(

    # --- Section: Introduction ---
    div(class = "section-title", "Typy danych - fundament analizy statystycznej"),

    div(class = "narrative",
      p("Pierwszym krokiem w analizie danych jest rozpoznanie typu zmiennej,
        z która mamy do czynienia. To nie jest formalność - typ zmiennej
        determinuje jakie statystyki mozemy obliczyć, jakie wykresy narysowac
        i jakie testy statystyczne zastosować."),
      p("Błędne rozpoznanie typu zmiennej prowadzi do błędnych analiz.
        Na przykład, obliczanie średniej z kodow pocztowych nie ma sensu,
        mimo ze są to liczby.")
    ),

    div(class = "callout-info",
      tags$strong("Zasada:"),
      " Zanim zaczniesz analize - zawsze okresl typ każdej zmiennej."
    ),

    # --- Widget 1: Taxonomy tree ---
    h2(id = "ch1-taksonomia", class = "section-title", "Taksonomia typów danych"),

    div(class = "narrative",
      p("Ponizszy diagram przedstawia hierarchie typow danych.
        Kliknij na liscie drzewa (najnizszy poziom), aby odkryc
        przyklady zmiennych każdego typu z naszego zbioru danych.")
    ),

    div(class = "widget-block",
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
                      style = paste0("background:", col_continuous, ";"),
                      onclick = "Shiny.setInputValue('ch1_leaf_click', 'ciagla', {priority:'event'})",
                      "Ciągłe"
                    )
                  ),
                  tags$li(
                    div(class = "tax-leaf", id = "ch1_leaf_dyskretna",
                      style = paste0("background:", col_discrete, ";"),
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
                      style = paste0("background:", col_ordinal, ";"),
                      onclick = "Shiny.setInputValue('ch1_leaf_click', 'porzadkowa', {priority:'event'})",
                      "Porządkowe"
                    )
                  ),
                  tags$li(
                    div(class = "tax-leaf", id = "ch1_leaf_nominalna",
                      style = paste0("background:", col_nominal, ";"),
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
      p(style = "text-align: center; font-size: 13px; color: #bdc3c7; margin: 8px 0 0;",
        "Kliknij na kolorowy liść drzewa, aby zobaczyć przykłady"),
      uiOutput("ch1_leaf_detail")
    ),

    div(class = "callout-warning",
      tags$strong("Uwaga:"),
      " Granica miedzy typami nie zawsze jest ostra. Na przykład ocena wykładowcy
      w skali 1-10 moze byc traktowana jako porządkowa lub dyskretna,
      w zależności od kontekstu i celu analizy."
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

    div(class = "widget-block",
      checkboxInput("ch1_show_bad",
                    "Pokaz nieodpowiednie wykresy (przykład czego NIE robic)",
                    value = FALSE),
      fluidRow(
        column(6,
          div(class = "example-card",
              style = paste0("border-color: ", col_nominal, ";"),
            span(class = "type-badge",
                 style = paste0("background: ", col_nominal, ";"),
                 "Jakościowa nominalna"),
            tags$h4("Płeć"),
            tags$p(style = "color: #7f8c8d; font-size: 13px;",
              "Kategorie bez naturalnego porzadku. Mozemy liczyc ile jest
               obserwacji w każdej kategorii, ale nie mozemy ich uporządkowac
               ani uśredniać."),
            plotOutput("ch1_ex1_plot", height = "280px")
          )
        ),
        column(6,
          div(class = "example-card",
              style = paste0("border-color: ", col_ordinal, ";"),
            span(class = "type-badge",
                 style = paste0("background: ", col_ordinal, ";"),
                 "Jakościowa porządkowa"),
            tags$h4("Zadowolenie ze studiów"),
            tags$p(style = "color: #7f8c8d; font-size: 13px;",
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
              style = paste0("border-color: ", col_discrete, ";"),
            span(class = "type-badge",
                 style = paste0("background: ", col_discrete, ";"),
                 "Ilościowa dyskretna"),
            tags$h4("Liczba kursów"),
            tags$p(style = "color: #7f8c8d; font-size: 13px;",
              "Wartości liczbowe, ale tylko całkowite. Mozemy obliczać srednia
               i odchylenie standardowe. Wykres słupkowy jest tu odpowiedni,
               bo mamy skończoną liczbę wartości."),
            plotOutput("ch1_ex3_plot", height = "280px")
          )
        ),
        column(6,
          div(class = "example-card",
              style = paste0("border-color: ", col_continuous, ";"),
            span(class = "type-badge",
                 style = paste0("background: ", col_continuous, ";"),
                 "Ilościowa ciągła"),
            tags$h4("Wzrost (cm)"),
            tags$p(style = "color: #7f8c8d; font-size: 13px;",
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

    div(class = "widget-block",
      div(style = "overflow-x: auto; font-size: 12px;",
        tableOutput("ch1_data_preview")
      )
    ),

    div(class = "callout-info",
      tags$strong("Zwroc uwage:"),
      " Zbior zawiera zmienne wszystkich czterech typow. W kolejnych
      rozdzialach nauczymy sie jak je podsumowywać i wizualizować."
    ),

    # --- Variable tracker selector ---
    div(class = "widget-block", style = "background: #eaf4fc; border: 2px solid #3498db;",
      h4("\U0001F50D Sledz zmienna przez caly kurs"),
      p(style = "font-size: 14px; color: #2c3e50;",
        "Wybierz jedna zmienna ilościowa. W każdym kolejnym rozdziale zobaczysz,
         jakie nowe informacje daja Ci kolejne narzedzia statystyczne zastosowane
         do tej samej zmiennej."),
      selectInput("tracked_var", "Wybierz zmienna do sledzenia:",
        choices = c("Wzrost (cm)" = "wzrost",
                    "Waga (kg)" = "waga",
                    "Czas dojazdu (min)" = "czas_dojazdu",
                    "Średnia ocen" = "srednia_ocen"),
        selected = "wzrost", width = "300px"
      )
    ),

    div(class = "chapter-transition",
      p("Wiemy jakie typy zmiennych mamy w naszych danych. Zaczynamy od zmiennych
        jakościowych -- są prostsze i stanowia naturalny punkt wyjscia."),
      actionButton("ch1_next", "Dalej: 2. Zmienne jakościowe →",
                   class = "btn-primary btn-lg")
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
      label = "Ciągłe", color = col_continuous,
      desc = "Wartości liczbowe, które mogą przyjmować dowolną wartość z przedziału (także ułamkowe).",
      examples = "wzrost (cm), waga (kg), czas dojazdu (min), średnia ocen"
    ),
    dyskretna = list(
      label = "Dyskretne", color = col_discrete,
      desc = "Wartości liczbowe, ale tylko całkowite — można je policzyć.",
      examples = "liczba kursów, liczba nieobecności"
    ),
    porzadkowa = list(
      label = "Porządkowe", color = col_ordinal,
      desc = "Kategorie z naturalnym porządkiem, ale odległości między nimi nie są znane.",
      examples = "rok studiów (1 < 2 < 3 < ...), zadowolenie ze studiów, ocena wykładowcy"
    ),
    nominalna = list(
      label = "Nominalne", color = col_nominal,
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
      p(style = "margin: 0; font-size: 13px; color: #7f8c8d;",
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
