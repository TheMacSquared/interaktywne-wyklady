# ==========================================================================
# ROZDZIAŁ 4: DZIAŁANIA NA ZDARZENIACH
# ==========================================================================

ch4_ui <- lecture_chapter(
  id = "ch-zbiory",
  num = "04",
  title = "Zdarzenia się łączą",
  duration = "20 min",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 04 · Język ryzyka",
      num = "04",
      title = "„Lub”, „i” oraz „nie” zmieniają zdarzenie.",
      lead = "W raportach bezpieczeństwa jedno słowo potrafi zmienić licznik.
              Zobaczymy sumę, część wspólną i dopełnienie na stu kontrolach
              korytarza Bananpolu."
    ),

    margin_callout(
      label = "Dwa zdarzenia",
      tags$div("A — podczas kontroli znaleziono skórkę na przejściu."),
      tags$div("B — podczas kontroli posadzka była mokra."),
      color = "wskazowka"
    ),

    lc_h2("ch4-jezyk", "Przetłumacz zdanie na zbiór"),
    lc_p(
      "Zdarzenie A ∪ B zachodzi, gdy wystąpiło A lub B, włącznie z sytuacją,
       gdy wystąpiły oba. Zdarzenie A ∩ B wymaga obu warunków naraz. Dopełnienie
       Aᶜ obejmuje wszystkie wyniki, w których A nie zaszło."
    ),

    lc_formula_box(
      withMathJax("$$P(A\\cup B)=P(A)+P(B)-P(A\\cap B)$$"),
      tags$p("Część wspólną odejmujemy, ponieważ przy dodawaniu została policzona dwa razy.")
    ),

    lc_h2("ch4-siatka", "Zbuduj dwa zdarzenia na 100 kontrolach"),
    lc_p(
      "Zmieniaj liczebności A i B oraz ich część wspólną. Aplikacja pilnuje, by
       wybrane zbiory mogły zmieścić się w przestrzeni 100 wyników."
    ),

    figure_panel(
      label = "Interakcja 4",
      title = "Suma, iloczyn i dopełnienie zdarzeń",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          sliderInput("ch4_n_a", "Liczba kontroli ze zdarzeniem A", 0, 80, 30, 1),
          sliderInput("ch4_n_b", "Liczba kontroli ze zdarzeniem B", 0, 80, 20, 1),
          sliderInput("ch4_overlap", "Liczba kontroli z A i B", 0, 20, 8, 1),
          uiOutput("ch4_stats")
        ),
        column(
          8,
          zoom_plot_ui("ch4_event_grid", height = "480px")
        )
      )
    ),

    lc_h2("ch4-pulapka", "Rozłączne nie znaczy niezależne"),
    lc_p(
      "Zdarzenia rozłączne nie mogą zajść razem, więc ich część wspólna jest
       pusta. Zdarzenia niezależne mogą zajść razem, ale informacja o jednym nie
       zmienia prawdopodobieństwa drugiego. Dwa niezerowe zdarzenia rozłączne
       nie są niezależne: gdy A zaszło, wiemy na pewno, że B nie zaszło."
    ),

    lc_feedback(
      type = "warning",
      tags$strong("Pułapka językowa:"),
      " w rachunku prawdopodobieństwa „A lub B” obejmuje także przypadek
        „A i B”, chyba że wyraźnie mówimy o alternatywie wykluczającej."
    ),

    lc_chapter_next(
      num = "05",
      title = "Od prawdopodobieństwa do decyzji",
      lead = "Dwa zdarzenia o podobnej częstości mogą mieć zupełnie inne skutki.",
      target_id = "ch-decyzja"
    )
  )
)

ch4_server <- function(input, output, session) {
  observeEvent(list(input$ch4_n_a, input$ch4_n_b), {
    lower <- max(0L, input$ch4_n_a + input$ch4_n_b - 100L)
    upper <- min(input$ch4_n_a, input$ch4_n_b)
    current <- input$ch4_overlap
    if (is.null(current)) current <- lower
    value <- min(max(current, lower), upper)

    updateSliderInput(
      session,
      "ch4_overlap",
      min = lower,
      max = upper,
      value = value
    )
  })

  event_values <- reactive({
    req(input$ch4_n_a, input$ch4_n_b, input$ch4_overlap)
    lower <- max(0L, input$ch4_n_a + input$ch4_n_b - 100L)
    upper <- min(input$ch4_n_a, input$ch4_n_b)
    overlap <- min(max(input$ch4_overlap, lower), upper)
    list(
      n_a = as.integer(input$ch4_n_a),
      n_b = as.integer(input$ch4_n_b),
      overlap = as.integer(overlap)
    )
  })

  output$ch4_stats <- renderUI({
    values <- event_values()
    union <- values$n_a + values$n_b - values$overlap

    lc_stat_grid(
      lc_stat_box("P(A ∩ B)", format_probability_pl(values$overlap / 100),
                  color = upwr_cat[["wrzos"]]),
      lc_stat_box("P(A ∪ B)", format_probability_pl(union / 100),
                  color = upwr_accent),
      lc_stat_box("P(Aᶜ)", format_probability_pl(1 - values$n_a / 100),
                  color = upwr_cat[["szalwia"]]),
      lc_stat_box("Ani A, ani B", format_probability_pl((100 - union) / 100),
                  color = upwr_reference),
      columns = 2
    )
  })

  event_grid_plot <- reactive({
    values <- event_values()
    data <- build_event_grid(
      total = 100L,
      n_a = values$n_a,
      n_b = values$n_b,
      overlap = values$overlap,
      columns = 10L
    )

    ggplot(data, aes(x = column, y = -row, fill = status)) +
      geom_tile(colour = "white", linewidth = 0.7, width = 0.95, height = 0.95) +
      scale_fill_manual(values = c(
        "A i B" = upwr_cat[["wrzos"]],
        "Tylko A" = upwr_cat[["terakota"]],
        "Tylko B" = upwr_cat[["niebo"]],
        "Ani A, ani B" = upwr_reference
      )) +
      coord_equal() +
      scale_x_continuous(breaks = NULL) +
      scale_y_continuous(breaks = NULL) +
      labs(
        title = "Sto kontroli korytarza",
        subtitle = "Każdy kwadrat to jeden wynik doświadczenia",
        x = NULL,
        y = NULL,
        fill = NULL
      ) +
      theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom"
      )
  })

  zoom_plot_server(
    "ch4_event_grid",
    event_grid_plot,
    alt = paste(
      "Siatka stu kontroli podzielonych na zdarzenie A, zdarzenie B,",
      "ich część wspólną oraz wyniki nienależące do żadnego zdarzenia."
    )
  )
}
