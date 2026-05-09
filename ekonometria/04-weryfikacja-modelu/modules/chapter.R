ch1_ui <- lecture_chapter(
  id = "ch-merytoryczna",
  num = "01",
  title = "Weryfikacja merytoryczna",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdzial 04 · Weryfikacja",
      num = "01",
      title = "Czy model ma sens ekonomiczny?",
      lead = "Zanim ocenimy p-wartosci, sprawdzamy znak, skale i interpretowalnosc wspolczynnikow."
    ),
    lc_h2("pytania", "Pytania kontrolne"),
    tags$ul(
      tags$li("Czy znak parametru jest zgodny z teoria lub intuicja ekonomiczna?"),
      tags$li("Czy jednostki zmiennych sa poprawnie opisane?"),
      tags$li("Czy model nie pomija oczywistego czynnika zaklocajacego?"),
      tags$li("Czy wynik da sie obronic przed osoba znajaca badane zjawisko?")
    ),
    lc_chapter_next("02", "Dopasowanie", "R2, odchylenie standardowe reszt i zgodnosc modelu", "ch-dopasowanie")
  )
)

ch2_ui <- lecture_chapter(
  id = "ch-dopasowanie",
  num = "02",
  title = "Dopasowanie modelu",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdzial 04 · Weryfikacja",
      num = "02",
      title = "Miary dopasowania.",
      lead = "R2 i blad reszt mowia o dopasowaniu, ale nie zastepuja diagnozy reszt."
    ),
    figure_panel(
      label = "Ryc. 4.1",
      title = "Typowe problemy widoczne w resztach",
      full_width = TRUE,
      fluidRow(
        column(4,
          selectInput("kind", "Scenariusz danych", c(
            "poprawny obraz" = "ok",
            "heteroskedastycznosc" = "hetero",
            "nieliniowosc" = "nonlinear",
            "obserwacje odstajace" = "outliers"
          ))
        ),
        column(8, plotOutput("diag_plot", height = "380px"), uiOutput("diag_stats"))
      )
    ),
    lc_chapter_next("03", "Istotnosc zmiennej", "test t dla zmiennej objasniajacej", "ch-istotnosc")
  )
)

ch3_ui <- lecture_chapter(
  id = "ch-istotnosc",
  num = "03",
  title = "Istotnosc zmiennej",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdzial 04 · Weryfikacja",
      num = "03",
      title = "Czy zmienna wnosi informacje?",
      lead = "Test istotnosci nachylenia sprawdza, czy dane sa zgodne z hipoteza beta1 = 0."
    ),
    lc_formula_box(
      withMathJax(helpText("$$H_0: \\beta_1 = 0 \\quad vs \\quad H_1: \\beta_1 \\neq 0$$")),
      withMathJax(helpText("$$t = \\frac{\\hat\\beta_1}{SE(\\hat\\beta_1)}$$"))
    ),
    inline_callout("Do rozbudowy", color = "wskazowka", open = TRUE,
      "Dalszy krok: dodac automatyczny werdykt dla tabeli wspolczynnikow i cwiczenie z interpretacji R2, SE reszt oraz p-wartosci.")
  )
)

chapter_server <- function(input, output, session) {
  d <- reactive(eco_diagnostic_data(input$kind, seed = 44))
  fit <- reactive(lm(y ~ x, data = d()))

  output$diag_plot <- renderPlot({
    df <- d()
    df$fitted <- fitted(fit())
    df$resid <- residuals(fit())
    p1 <- ggplot(df, aes(x, y)) +
      geom_point(color = unname(upwr_cat["grafit"]), alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE, color = upwr_accent) +
      labs(x = "X", y = "Y") +
      theme_upwr()
    p2 <- ggplot(df, aes(fitted, resid)) +
      geom_hline(yintercept = 0, color = upwr_reference, linetype = "dashed") +
      geom_point(color = unname(upwr_cat["terakota"]), alpha = 0.65) +
      geom_smooth(se = FALSE, color = unname(upwr_cat["niebo"])) +
      labs(x = "Wartosci dopasowane", y = "Reszty") +
      theme_upwr()
    if (requireNamespace("patchwork", quietly = TRUE)) p1 + p2 else p2
  })

  output$diag_stats <- renderUI({
    g <- broom::glance(fit())
    lc_stat_grid(
      lc_stat_box("R2", eco_fmt(g$r.squared, 3), color = unname(upwr_cat["niebo"])),
      lc_stat_box("SE reszt", eco_fmt(g$sigma, 2), color = unname(upwr_cat["terakota"])),
      lc_stat_box("p dla modelu", eco_fmt(g$p.value, 4), color = upwr_accent),
      columns = 3
    )
  })
}
