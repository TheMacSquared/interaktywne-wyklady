ch1_ui <- lecture_chapter(
  id = "ch-estymatory",
  num = "01",
  title = "Estymatory parametrow",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdzial 03 · Estymacja",
      num = "01",
      title = "Od parametru do estymatora.",
      lead = "Parametry beta sa nieznane. Estymatory b0 i b1 liczymy z proby."
    ),
    lc_h2("wzory", "W modelu prostym"),
    lc_formula_box(
      withMathJax(helpText("$$\\hat\\beta_1 = \\frac{\\sum (x_i - \\bar{x})(y_i - \\bar{y})}{\\sum (x_i - \\bar{x})^2}$$")),
      withMathJax(helpText("$$\\hat\\beta_0 = \\bar{y} - \\hat\\beta_1\\bar{x}$$"))
    ),
    lc_p("W praktyce najwazniejsze jest nie tylko policzenie wspolczynnikow, ale zrozumienie ich niepewnosci."),
    lc_chapter_next("02", "Bledy standardowe", "jak bardzo wyniki zmienialyby sie miedzy probami", "ch-se")
  )
)

ch2_ui <- lecture_chapter(
  id = "ch-se",
  num = "02",
  title = "Bledy standardowe",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdzial 03 · Estymacja",
      num = "02",
      title = "Niepewnosc estymatorow.",
      lead = "Ten widget pokazuje, ze ten sam mechanizm populacyjny daje rozne nachylenia w roznych probach."
    ),
    figure_panel(
      label = "Ryc. 3.1",
      title = "Rozklad estymatora nachylenia",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("n", "Liczba obserwacji w probie", 20, 250, 60, step = 10),
          sliderInput("sigma", "Szum w danych", 1, 25, 8, step = 1),
          sliderInput("reps", "Liczba symulacji", 50, 600, 250, step = 50)
        ),
        column(8, plotOutput("se_plot", height = "360px"), uiOutput("se_stats"))
      )
    ),
    lc_chapter_next("03", "Przedzial i test", "naturalny most do weryfikacji istotnosci", "ch-test")
  )
)

ch3_ui <- lecture_chapter(
  id = "ch-test",
  num = "03",
  title = "Przedzial i test",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdzial 03 · Estymacja",
      num = "03",
      title = "Od bledu standardowego do wniosku.",
      lead = "Blad standardowy jest budulcem statystyki t, p-wartosci i przedzialu ufnosci."
    ),
    lc_formula_box(
      withMathJax(helpText("$$t = \\frac{\\hat\\beta_j - \\beta_{j,0}}{SE(\\hat\\beta_j)}$$")),
      withMathJax(helpText("$$\\hat\\beta_j \\pm t_{\\alpha/2, df}\\,SE(\\hat\\beta_j)$$"))
    ),
    inline_callout("Do rozbudowy", color = "wskazowka", open = TRUE,
      "W tym miejscu warto dodac docelowa tabele wynikow modelu i cwiczenie: ktore wspolczynniki sa istotne i jak je interpretowac.")
  )
)

chapter_server <- function(input, output, session) {
  slopes <- reactive({
    set.seed(303)
    replicate(input$reps, {
      d <- eco_regression_data(input$n, beta0 = 10, beta1 = 1.5,
                               sigma = input$sigma, seed = sample.int(1e6, 1))
      coef(lm(y ~ x, data = d))[2]
    })
  })

  output$se_plot <- renderPlot({
    data.frame(b1 = slopes()) |>
      ggplot(aes(b1)) +
      geom_histogram(bins = 28, fill = unname(upwr_cat["niebo"]), color = "white", alpha = 0.9) +
      geom_vline(xintercept = 1.5, color = upwr_accent, linewidth = 1) +
      labs(x = "Oszacowane nachylenie b1", y = "Liczba prob") +
      theme_upwr()
  })

  output$se_stats <- renderUI({
    s <- slopes()
    lc_stat_grid(
      lc_stat_box("Srednia b1", eco_fmt(mean(s), 3), color = unname(upwr_cat["szalwia"])),
      lc_stat_box("SD b1", eco_fmt(sd(s), 3), caption = "symulacyjny blad standardowy", color = upwr_accent),
      lc_stat_box("Prawdziwe beta1", "1.500", color = upwr_secondary),
      columns = 3
    )
  })
}
