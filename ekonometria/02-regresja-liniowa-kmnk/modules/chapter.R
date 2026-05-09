ch1_ui <- lecture_chapter(
  id = "ch-rownanie",
  num = "01",
  title = "Rownanie regresji",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdzial 02 · KMNK",
      num = "01",
      title = "Regresja liniowa z jedna zmienna.",
      lead = "Tu zaczyna sie pierwszy liczony model: prosta opisujaca zaleznosc Y od X."
    ),
    lc_h2("model", "Model"),
    lc_formula_box(
      withMathJax(helpText("$$Y_i = \\beta_0 + \\beta_1 X_i + \\varepsilon_i$$")),
      p(withMathJax("\\(\\beta_1\\)"), " interpretujemy jako przecietna zmiane Y po wzroscie X o jedna jednostke.")
    ),
    lc_h2("intuicja", "Intuicja estymacji"),
    lc_p("KMNK wybiera taka prosta, dla ktorej suma kwadratow reszt jest najmniejsza. Kwadratowanie sprawia, ze duze pomylki sa karane mocniej niz male."),
    lc_chapter_next("02", "Dopasowanie KMNK", "interaktywny model na symulowanych danych", "ch-widget")
  )
)

ch2_ui <- lecture_chapter(
  id = "ch-widget",
  num = "02",
  title = "Dopasowanie KMNK",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdzial 02 · KMNK",
      num = "02",
      title = "Dopasowanie prostej.",
      lead = "Sterujemy sila relacji i szumem, a aplikacja przelicza model."
    ),
    figure_panel(
      label = "Ryc. 2.1",
      title = "Regresja prosta",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("n", "Liczba obserwacji", 20, 200, 80, step = 10),
          sliderInput("beta1", "Nachylenie", -2, 4, 1.5, step = 0.25),
          sliderInput("sigma", "Szum", 1, 25, 7, step = 1),
          checkboxInput("resid", "Pokaz reszty", FALSE)
        ),
        column(8, plotOutput("reg_plot", height = "380px"), uiOutput("reg_stats"))
      )
    ),
    lc_chapter_next("03", "Zalozenia KMNK", "warunki, ktore stoja za interpretacja modelu", "ch-zalozenia")
  )
)

ch3_ui <- lecture_chapter(
  id = "ch-zalozenia",
  num = "03",
  title = "Zalozenia KMNK",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdzial 02 · KMNK",
      num = "03",
      title = "Zalozenia klasycznej MNK.",
      lead = "Na tym etapie chodzi o liste kontrolna, do ktorej bedziemy wracac przy weryfikacji modelu."
    ),
    lc_h2("lista", "Lista startowa"),
    tags$ul(
      tags$li("liniowosc wzgledem parametrow,"),
      tags$li("losowy skladnik o sredniej zero,"),
      tags$li("stala wariancja skladnika losowego,"),
      tags$li("brak autokorelacji skladnikow losowych,"),
      tags$li("brak scislej wspolliniowosci w modelach wielorakich,"),
      tags$li("normalnosc reszt, gdy korzystamy z klasycznych testow w malej probie.")
    ),
    inline_callout("Do rozbudowy", color = "wskazowka", open = TRUE,
      "Tu warto dodac docelowy przyklad z cwiczen laboratoryjnych: Excel/Gretl/R i interpretacje tabeli wynikow.")
  )
)

chapter_server <- function(input, output, session) {
  df <- reactive(eco_regression_data(input$n, beta0 = 18, beta1 = input$beta1,
                                     sigma = input$sigma, seed = 22))
  fit <- reactive(lm(y ~ x, data = df()))

  output$reg_plot <- renderPlot({
    d <- df()
    d$fitted <- fitted(fit())
    p <- ggplot(d, aes(x, y)) +
      geom_point(color = unname(upwr_cat["grafit"]), alpha = 0.65, size = 2) +
      geom_smooth(method = "lm", se = TRUE, color = upwr_accent,
                  fill = upwr_seq_burgundy[3], alpha = 0.18)
    if (isTRUE(input$resid)) {
      p <- p + geom_segment(aes(xend = x, yend = fitted),
                            color = unname(upwr_cat["terakota"]), alpha = 0.35)
    }
    p + labs(x = "X", y = "Y") + theme_upwr()
  })

  output$reg_stats <- renderUI({
    g <- broom::glance(fit())
    b <- broom::tidy(fit())
    lc_stat_grid(
      lc_stat_box("b0", eco_fmt(b$estimate[1], 2), caption = "wyraz wolny", color = upwr_secondary),
      lc_stat_box("b1", eco_fmt(b$estimate[2], 3), caption = "nachylenie", color = unname(upwr_cat["szalwia"])),
      lc_stat_box("R2", eco_fmt(g$r.squared, 3), caption = "dopasowanie", color = unname(upwr_cat["niebo"])),
      lc_stat_box("SE", eco_fmt(g$sigma, 2), caption = "blad reszt", color = unname(upwr_cat["terakota"])),
      columns = 4
    )
  })
}
