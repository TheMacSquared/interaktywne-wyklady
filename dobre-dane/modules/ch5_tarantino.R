# Tab 5: Tarantino — dane eventowe, zła struktura

ch5_ui <- lecture_chapter(id = "ch5", num = "5", title = "Tarantino", content = tagList(
  fluidRow(column(8, offset = 2,

    lc_h2("sec-01", "Filmy Tarantino"),

    div(class = "lc-prose",
      p("Kolega znalazł ciekawy zbiór danych o filmach Quentina Tarantino.
        Zawiera informacje o każdym przekleństwie i każdej śmierci w jego filmach.
        'Super temat na projekt!' - mówi."),
      p("Źródło: pakiet fivethirtyeight w R.")
    ),

    lc_h2("sec-02", "Podgląd danych"),

    div(class = "lc-figure-panel",
      DT::dataTableOutput("tab4_table")
    ),

    div(class = "lc-feedback lc-feedback-info",
      tags$strong("Zmienne:"),
      " movie (tytuł filmu), type ('word' lub 'death'), ",
      "word (konkretne słowo, jeśli type='word'), minutes_in (minuta filmu)."
    ),

    lc_h2("sec-03", "Eksploracja"),

    div(class = "lc-figure-panel",
      fluidRow(
        column(6, actionButton("tab4_hist", "Histogram: minutes_in", class = "lc-btn-outline", width = "100%")),
        column(6, actionButton("tab4_bar", "Porównanie filmów", class = "lc-btn-outline", width = "100%"))
      ),
      plotOutput("tab4_explore_plot", height = "350px")
    ),

    lc_h2("sec-04", "Próba analiz"),

    div(class = "lc-figure-panel",
      h4("Jaka analiza tu pasuje?"),
      uiOutput("tab4_quiz_options"),
      uiOutput("tab4_quiz_result")
    ),

    div(class = "lc-figure-panel",
      h4("Może agregacja pomoże?"),
      div(class = "lc-prose",
        p("Każdy wiersz to jedno zdarzenie (przekleństwo lub śmierć). Aby używać klasycznej
          statystyki, musielibyśmy zagregować dane do poziomu filmów.")
      ),
      actionButton("tab4_aggregate", "Zagreguj dane", class = "lc-btn-warning"),
      uiOutput("tab4_agg_result")
    ),

    lc_h2("sec-05", "Werdykt"),

    div(class = "lc-feedback lc-feedback-danger",
      "Zły zbiór do klasycznej statystyki!",
      tags$br(),
      tags$strong("Problem 1:"), " Dane eventowe - każdy wiersz to zdarzenie, nie obserwacja w sensie statystycznym.",
      tags$br(),
      tags$strong("Problem 2:"), " Po agregacji do poziomu filmów mamy n = 7. To za mało na jakąkolwiek analizę.",
      tags$br(),
      tags$strong("Problem 3:"), " Brak zmiennych ilościowych do korelacji/regresji."
    ),

    lc_chapter_next(
      num = "06",
      title = "Ankieta firmowa",
      lead = "Czasem dane mają odpowiednią wielkość, ale inny problem...",
      target_id = "ch6"
    ),

    div(style = "height: 40px;")
  ))))

ch5_server <- function(input, output, session) {

  output$tab4_table <- DT::renderDataTable({
    datatable(round_df(tarantino), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  observeEvent(input$tab4_hist, {
    output$tab4_explore_plot <- renderPlot({
      ggplot(tarantino, aes(x = minutes_in)) +
        geom_histogram(bins = 30, fill = data_primary, color = "white", alpha = 0.8) +
        labs( x = "Minuta filmu", y = "Liczba zdarzeń") +
        theme_upwr(base_size = 14)
    })
  })

  observeEvent(input$tab4_bar, {
    output$tab4_explore_plot <- renderPlot({
      tarantino %>%
        count(movie, type) %>%
        ggplot(aes(x = reorder(movie, n), y = n, fill = type)) +
        geom_col(position = "dodge", alpha = 0.8) +
        scale_fill_manual(values = c("death" = data_bad, "word" = data_mixed)) +
        coord_flip() +
        labs(x = NULL, y = "Liczba", fill = "Typ") +
        theme_upwr(base_size = 14)
    })
  })

  tab4_quiz_answered <- reactiveVal(FALSE)
  tab4_quiz_selected <- reactiveVal(NULL)

  tab4_quiz_choices <- list(
    list(letter = "A", value = "Test t", text = "Test t"),
    list(letter = "B", value = "Korelacja", text = "Korelacja"),
    list(letter = "C", value = "Regresja", text = "Regresja"),
    list(letter = "D", value = "Zadna z klasycznych", text = "Żadna z klasycznych")
  )

  output$tab4_quiz_options <- renderUI({
    if (tab4_quiz_answered()) return(NULL)
    div(class = "quiz-tiles quiz-cols-4",
      lapply(tab4_quiz_choices, function(opt) {
        actionButton(paste0("tab4_tile_", gsub(" ", "_", opt$value)),
          tagList(
            div(class = "tile-letter", opt$letter),
            div(class = "tile-text", opt$text)
          ),
          class = "quiz-tile"
        )
      })
    )
  })

  observe({
    for (opt in tab4_quiz_choices) {
      local({
        val <- opt$value
        btn_id <- paste0("tab4_tile_", gsub(" ", "_", val))
        observeEvent(input[[btn_id]], {
          if (tab4_quiz_answered()) return()
          tab4_quiz_selected(val)
          tab4_quiz_answered(TRUE)
        }, ignoreInit = TRUE)
      })
    }
  })

  output$tab4_quiz_result <- renderUI({
    req(tab4_quiz_answered())
    answer <- tab4_quiz_selected()
    if (answer == "Zadna z klasycznych") {
      div(class = "lc-feedback lc-feedback-ok", style = "margin-top: 10px;",
        tags$strong("Dokładnie!"),
        " Dane eventowe nie nadają się do klasycznych testów.",
        " Każdy wiersz to zdarzenie, nie niezależna obserwacja."
      )
    } else {
      div(class = "lc-feedback lc-feedback-danger", style = "margin-top: 10px;",
        tags$strong("Nie do końca."),
        paste0(" ", answer, " wymaga zmiennych odpowiedniego typu i niezależnych obserwacji. "),
        "Tutaj mamy dane eventowe - każdy wiersz to jedno przekleństwo lub śmierć w filmie. ",
        "Poprawna odpowiedź: 'Żadna z klasycznych'."
      )
    }
  })

  output$tab4_agg_result <- renderUI({
    req(input$tab4_aggregate > 0)
    agg <- tarantino %>%
      group_by(movie) %>%
      summarise(
        n_profanity = sum(type == "word", na.rm = TRUE),
        n_deaths = sum(type == "death", na.rm = TRUE),
        .groups = "drop"
      )

    tagList(
      div(style = "margin-top: 15px;",
        DT::renderDataTable({
          datatable(round_df(agg), options = list(dom = 't', pageLength = 10), rownames = FALSE)
        })
      ),
      div(class = "lc-feedback lc-feedback-danger", style = "margin-top: 15px;",
        tags$strong("Problem:"),
        paste0(" Po agregacji mamy n = ", nrow(agg), " filmów. "),
        "To zdecydowanie za mało na jakąkolwiek analizę statystyczną.",
        tags$br(),
        "Korelacja n_profanity vs n_deaths przy n=7 nie ma mocy statystycznej."
      )
    )
  })

}
