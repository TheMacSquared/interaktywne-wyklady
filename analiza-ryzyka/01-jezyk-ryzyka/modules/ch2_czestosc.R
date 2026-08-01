# ==========================================================================
# ROZDZIAŁ 2: CZĘSTOŚĆ EMPIRYCZNA I PRAWDOPODOBIEŃSTWO
# ==========================================================================

ch2_ui <- lecture_chapter(
  id = "ch-czestosc",
  num = "02",
  title = "Od obserwacji do modelu",
  duration = "20 min",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 02 · Język ryzyka",
      num = "02",
      title = "Jeden miesiąc może kłamać.",
      lead = "Częstość obserwowana zmienia się od serii do serii. Dopiero wraz
              z liczbą porównywalnych okresów zaczyna odsłaniać stabilny wzorzec."
    ),

    margin_callout(
      label = "Jednostka obserwacji",
      "Jedna próba oznacza jedną 8-godzinną zmianę w konkretnym korytarzu.
       Zdarzenie: co najmniej jedno poślizgnięcie podczas tej zmiany.",
      color = "ok"
    ),

    lc_h2("ch2-mianownik", "Najpierw ustal mianownik"),
    lc_p(
      "Zdanie „były trzy poślizgnięcia” nie pozwala porównać dwóch magazynów.
       Potrzebujemy wiedzieć, w ilu porównywalnych zmianach mogły wystąpić, jak
       zdefiniowano zdarzenie i czy obserwacje dotyczą tych samych warunków."
    ),

    lc_formula_box(
      withMathJax("$$\\text{częstość empiryczna} =
                   \\frac{\\text{liczba zmian ze zdarzeniem}}
                   {\\text{liczba obserwowanych zmian}}$$")
    ),

    lc_h2("ch2-symulacja", "Zobacz stabilizację częstości"),
    lc_p(
      "Ustaw modelowe prawdopodobieństwo, a następnie dodawaj kolejne fikcyjne
       zmiany. Małe serie mogą wyglądać dramatycznie albo podejrzanie dobrze.
       Duże serie zwykle zbliżają się do wartości przyjętej w modelu."
    ),

    figure_panel(
      label = "Interakcja 2",
      title = "Teoria kontra kolejne zmiany w Bananpolu",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          lc_stack(
            sliderInput(
              "ch2_probability",
              "Modelowe P(zdarzenia w jednej zmianie)",
              min = 0.01,
              max = 0.30,
              value = bananpol$events$corridor_slip$illustrative_probability,
              step = 0.01
            ),
            actionButton("ch2_add_1", "Dodaj 1 zmianę", class = "lc-btn-primary", width = "100%"),
            actionButton("ch2_add_10", "Dodaj 10 zmian", class = "lc-btn-primary", width = "100%"),
            actionButton("ch2_add_100", "Dodaj 100 zmian", class = "lc-btn-primary", width = "100%"),
            actionButton("ch2_add_1000", "Dodaj 1000 zmian", class = "lc-btn-primary", width = "100%"),
            actionButton("ch2_reset", "Reset", class = "lc-btn-secondary-outline", width = "100%")
          ),
          uiOutput("ch2_stats")
        ),
        column(
          8,
          zoom_plot_ui("ch2_convergence", height = "440px")
        )
      )
    ),

    lc_feedback(
      type = "info",
      tags$strong("Aha:"),
      " prawdopodobieństwo jest własnością modelu, a częstość jest wynikiem
        konkretnej serii obserwacji. Nie oczekujemy, że w każdej małej serii
        będą identyczne."
    ),

    lc_feedback(
      type = "warning",
      tags$strong("Ważne:"),
      " stabilizacja częstości nie naprawia złej definicji zdarzenia, zmiany
        warunków ani błędów rejestracji. Więcej danych nie zastępuje dobrego
        modelu obserwacji."
    ),

    lc_chapter_next(
      num = "03",
      title = "Przestrzeń zdarzeń",
      lead = "Zobaczymy, kiedy wolno liczyć przypadki sprzyjające.",
      target_id = "ch-przestrzen"
    )
  )
)

ch2_server <- function(input, output, session) {
  history <- reactiveVal(integer())

  add_days <- function(n) {
    req(input$ch2_probability)
    history(append_bernoulli_history(history(), n, input$ch2_probability))
  }

  observeEvent(input$ch2_add_1, add_days(1L))
  observeEvent(input$ch2_add_10, add_days(10L))
  observeEvent(input$ch2_add_100, add_days(100L))
  observeEvent(input$ch2_add_1000, add_days(1000L))
  observeEvent(input$ch2_reset, history(integer()))
  observeEvent(input$ch2_probability, history(integer()), ignoreInit = TRUE)

  output$ch2_stats <- renderUI({
    req(input$ch2_probability)
    observed <- history()
    n <- length(observed)
    events <- sum(observed)
    frequency <- if (n == 0) NA_real_ else mean(observed)

    lc_stat_grid(
      lc_stat_box("Obserwowane zmiany", format(n, big.mark = " ")),
      lc_stat_box("Zmiany ze zdarzeniem", format(events, big.mark = " ")),
      lc_stat_box(
        "Częstość empiryczna",
        if (is.na(frequency)) "—" else format_probability_pl(frequency),
        color = upwr_cat[["niebo"]]
      ),
      lc_stat_box(
        "Prawdopodobieństwo modelowe",
        format_probability_pl(input$ch2_probability),
        color = upwr_accent
      ),
      columns = 2
    )
  })

  convergence_plot <- reactive({
    req(input$ch2_probability)
    data <- cumulative_frequency(history())

    plot <- ggplot(data, aes(x = trial, y = frequency)) +
      geom_hline(
        yintercept = input$ch2_probability,
        colour = upwr_accent,
        linewidth = 0.9,
        linetype = "dashed"
      ) +
      coord_cartesian(ylim = c(0, 1)) +
      labs(
        title = "Częstość poślizgnięć w kolejnych zmianach",
        subtitle = "Linia przerywana: prawdopodobieństwo przyjęte w modelu",
        x = "Liczba obserwowanych zmian",
        y = "Skumulowana częstość zdarzenia"
      )

    if (nrow(data) == 0) {
      plot +
        annotate(
          "text",
          x = 1,
          y = 0.55,
          label = "Dodaj pierwsze obserwacje",
          colour = upwr_secondary,
          size = 5
        ) +
        scale_x_continuous(limits = c(0, 2))
    } else {
      plot + geom_line(linewidth = 0.8, colour = upwr_cat[["niebo"]])
    }
  })

  zoom_plot_server(
    "ch2_convergence",
    convergence_plot,
    alt = paste(
      "Wykres skumulowanej częstości poślizgnięć w kolejnych zmianach",
      "z linią modelowego prawdopodobieństwa."
    )
  )
}
