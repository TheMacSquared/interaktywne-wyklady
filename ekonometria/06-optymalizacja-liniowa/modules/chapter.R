ch1_ui <- lecture_chapter(
  id = "ch-decyzja",
  num = "01",
  title = "Problem decyzyjny",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdzial 06 · Optymalizacja",
      num = "01",
      title = "Od decyzji do modelu.",
      lead = "Programowanie liniowe porzadkuje decyzje: co wybieramy, co maksymalizujemy i jakie mamy ograniczenia."
    ),
    lc_h2("elementy", "Elementy zadania"),
    tags$ul(
      tags$li("zmienne decyzyjne - wielkosci, ktore wybieramy,"),
      tags$li("funkcja celu - zysk, koszt, czas albo inna miara do optymalizacji,"),
      tags$li("ograniczenia - zasoby, normy, budzet, limity technologiczne,"),
      tags$li("warunki nieujemnosci - zwykle nie produkujemy ujemnej ilosci dobra.")
    ),
    lc_chapter_next("02", "Model liniowy", "graficzny start dla dwoch zmiennych", "ch-graf")
  )
)

ch2_ui <- lecture_chapter(
  id = "ch-graf",
  num = "02",
  title = "Model liniowy",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdzial 06 · Optymalizacja",
      num = "02",
      title = "Obszar dopuszczalny.",
      lead = "Dla dwoch zmiennych mozemy zobaczyc ograniczenia i kandydatow na optimum."
    ),
    figure_panel(
      label = "Ryc. 6.1",
      title = "Graficzna metoda rozwiazania",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("z1", "Zysk z x1", 5, 60, 30, step = 5),
          sliderInput("z2", "Zysk z x2", 5, 60, 25, step = 5),
          sliderInput("b1", "Zasob A", 40, 160, 100, step = 10),
          sliderInput("b2", "Zasob B", 40, 160, 90, step = 10)
        ),
        column(8, plotOutput("lp_plot", height = "380px"), uiOutput("lp_stats"))
      )
    ),
    lc_chapter_next("03", "Postac zadania", "klasyczna i standardowa postac PM", "ch-postac")
  )
)

ch3_ui <- lecture_chapter(
  id = "ch-postac",
  num = "03",
  title = "Postac zadania",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdzial 06 · Optymalizacja",
      num = "03",
      title = "Klasyczna i standardowa postac.",
      lead = "Ten fragment przygotowuje grunt pod tablice simpleksowe."
    ),
    lc_formula_box(
      withMathJax(helpText("$$\\max z = c_1x_1 + c_2x_2$$")),
      withMathJax(helpText("$$a_{11}x_1 + a_{12}x_2 \\le b_1, \\quad a_{21}x_1 + a_{22}x_2 \\le b_2, \\quad x_1,x_2 \\ge 0$$"))
    ),
    inline_callout("Do rozbudowy", color = "wskazowka", open = TRUE,
      "Mozna tu dodac generator zadania tekstowego i automatyczne tlumaczenie go na funkcje celu oraz ograniczenia.")
  )
)

chapter_server <- function(input, output, session) {
  lp <- reactive(eco_lp_vertices(b1 = input$b1, b2 = input$b2, z1 = input$z1, z2 = input$z2))

  output$lp_plot <- renderPlot({
    x <- seq(0, 90, length.out = 300)
    boundary <- data.frame(
      x = x,
      y = pmin(pmax(0, (input$b1 - 2 * x) / 1), pmax(0, (input$b2 - x) / 2))
    )
    verts <- lp()
    best <- verts[which.max(verts$value), ]
    ggplot(boundary, aes(x, y)) +
      geom_area(fill = upwr_seq_burgundy[2], alpha = 0.75) +
      geom_line(color = upwr_accent, linewidth = 1) +
      geom_point(data = verts, aes(x, y), color = upwr_secondary, size = 3) +
      geom_point(data = best, aes(x, y), color = unname(upwr_cat["szalwia"]), size = 5) +
      coord_cartesian(xlim = c(0, 80), ylim = c(0, 80)) +
      labs(x = "x1", y = "x2") +
      theme_upwr()
  })

  output$lp_stats <- renderUI({
    best <- lp()[which.max(lp()$value), ]
    lc_stat_grid(
      lc_stat_box("x1*", eco_fmt(best$x, 2), color = unname(upwr_cat["niebo"])),
      lc_stat_box("x2*", eco_fmt(best$y, 2), color = unname(upwr_cat["szalwia"])),
      lc_stat_box("z max", eco_fmt(best$value, 2), color = upwr_accent),
      columns = 3
    )
  })
}
