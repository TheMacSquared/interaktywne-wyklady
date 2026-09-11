# ============================================================================
# ROZDZIAŁ 2: Zmienne w modelu
# ============================================================================

ch2_ui <- lecture_chapter(
  id = "ch-zmienne",
  num = "02",
  title = "Zmienne w modelu",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 01 · Model ekonometryczny",
      num = "02",
      title = "Zmienne w modelu.",
      lead = "Ten sam zbiór danych można opisać na kilka sposobów. Ekonometria zaczyna się od decyzji, która zmienna pełni jaką rolę."
    ),

    lc_h2("ch2-role", "Role zmiennych"),
    tagList(
      p("W modelu jedna zmienna zwykle pełni rolę wyniku, a pozostałe — rolę czynników, które ten wynik tłumaczą. To jest decyzja merytoryczna, nie tylko techniczna. Ten sam zestaw zmiennych można poukładać inaczej, jeśli zadamy inne pytanie."),
      p("Przykład: w danych mamy ", strong("cenę mieszkania"), ", ", strong("metraż"), " i ", strong("dzielnicę"), ". Jeśli pytamy o wycenę nieruchomości — Y to cena, a metraż i dzielnica to X. Jeśli pytamy o segregację społeczną w mieście — bardziej naturalne jest patrzenie na cenę i metraż jako zmienne wyjaśniające skład dzielnicy."),
      figure_panel(
        label = "Tabela 2.1",
        title = "Najczęstsze role zmiennych",
        tags$table(
          class = "table",
          tags$thead(tags$tr(
            tags$th("Rola"),
            tags$th("Pytanie"),
            tags$th("Przykład")
          )),
          tags$tbody(
            tags$tr(tags$td("Zmienna objaśniana"), tags$td("Co chcemy wyjaśnić?"), tags$td("sprzedaż, koszt, stopa bezrobocia")),
            tags$tr(tags$td("Zmienna objaśniająca"), tags$td("Czym to wyjaśniamy?"), tags$td("cena, dochód, nakłady, czas")),
            tags$tr(tags$td("Zmienna kontrolna"), tags$td("Co trzeba uwzględnić, żeby nie pomylić efektów?"), tags$td("region, sezon, wielkość firmy")),
            tags$tr(tags$td("Składnik losowy"), tags$td("Co zostaje poza modelem?"), tags$td("czynniki pominięte, błąd pomiaru, przypadkowość"))
          )
        )
      )
    ),

    lc_h2("ch2-przyklad", "Mini-przykład: nakłady reklamowe i sprzedaż"),
    lc_p("Spójrzmy na sztuczne dane, które naśladują sytuację firmy obserwującej, jak miesięczne nakłady reklamowe (X) wiążą się z miesięczną sprzedażą (Y). Zmień siłę relacji i poziom szumu, żeby zobaczyć, jak zmienia się obraz."),
    figure_panel(
      label = "Ryc. 2.1",
      title = "Sprzedaż i nakłady reklamowe",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          sliderInput("ch2_n", "Liczba obserwacji", min = 20, max = 160, value = 70, step = 10),
          sliderInput("ch2_beta1", "Siła relacji X → Y", min = -1, max = 4, value = 1.4, step = 0.2),
          sliderInput("ch2_sigma", "Niepewność poza modelem", min = 1, max = 18, value = 6, step = 1)
        ),
        column(
          8,
          plotOutput("ch2_plot", height = "360px"),
          uiOutput("ch2_comment")
        )
      )
    ),

    inline_callout(
      label = "Wskazówka",
      color = "ok",
      "Im mniejszy szum (i im większa próba), tym wyraźniej widać związek między X a Y. Pierwsza wersja modelu może wyglądać dobrze przy niskim szumie, a rozsypywać się przy realistycznych danych — to jeden z powodów, dla których ekonometria nie kończy się na narysowaniu prostej."
    ),

    lc_chapter_next(
      num = "03",
      title = "Składnik losowy",
      lead = "dlaczego model nie trafia idealnie",
      target_id = "ch-losowy"
    )
  )
)

ch2_server <- function(input, output, session) {
  ch2_data <- reactive({
    eco_make_regression_data(
      n = input$ch2_n,
      beta0 = 20,
      beta1 = input$ch2_beta1,
      sigma = input$ch2_sigma,
      seed = 1001
    )
  })

  output$ch2_plot <- renderPlot({
    ggplot(ch2_data(), aes(naklady, sprzedaz)) +
      geom_point(color = unname(upwr_cat["grafit"]), alpha = 0.65, size = 2) +
      geom_smooth(method = "lm", se = TRUE, color = upwr_accent,
                  fill = upwr_seq_burgundy[3], alpha = 0.18) +
      labs(x = "X: nakłady reklamowe", y = "Y: sprzedaż") +
      theme_upwr()
  })

  output$ch2_comment <- renderUI({
    fit <- lm(sprzedaz ~ naklady, data = ch2_data())
    g <- broom::glance(fit)
    coefs <- broom::tidy(fit)
    lc_stat_grid(
      eco_metric("Nachylenie", round(coefs$estimate[2], 2), "zmiana Y przy wzroście X o 1", unname(upwr_cat["szalwia"])),
      eco_metric("R²", round(g$r.squared, 3), "część zmienności Y opisana przez X", unname(upwr_cat["niebo"])),
      eco_metric("Reszta", "Y − Ŷ", "to, czego model nie wyjaśnił", unname(upwr_cat["terakota"])),
      columns = 3
    )
  })
}
