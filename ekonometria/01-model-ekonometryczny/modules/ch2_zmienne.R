# ============================================================================
# CHAPTER 2: Zmienne w modelu
# ============================================================================

ch2_ui <- lecture_chapter(
  id = "ch-zmienne",
  num = "02",
  title = "Zmienne w modelu",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdzial 01 · Model ekonometryczny",
      num = "02",
      title = "Zmienne w modelu.",
      lead = "Ten sam zbior danych mozna opisac na kilka sposobow. Ekonometria zaczyna sie od decyzji, ktora zmienna pelni jaka role."
    ),

    lc_h2("ch2-role", "Role zmiennych"),
    tagList(
      p("W modelu jedna zmienna zwykle pelni role wyniku, a pozostale role czynnikow, ktore maja ten wynik tlumaczyc. To jest decyzja merytoryczna, nie tylko techniczna."),
      figure_panel(
        label = "Tabela 2.1",
        title = "Najczestsze role zmiennych",
        tags$table(
          class = "table",
          tags$thead(tags$tr(
            tags$th("Rola"),
            tags$th("Pytanie"),
            tags$th("Przyklad")
          )),
          tags$tbody(
            tags$tr(tags$td("Zmienna objasniana"), tags$td("Co chcemy wyjasnic?"), tags$td("sprzedaz, koszt, stopa bezrobocia")),
            tags$tr(tags$td("Zmienna objasniajaca"), tags$td("Czym to wyjasniamy?"), tags$td("cena, dochod, naklady, czas")),
            tags$tr(tags$td("Zmienna kontrolna"), tags$td("Co trzeba uwzglednic, zeby nie pomylic efektow?"), tags$td("region, sezon, wielkosc firmy")),
            tags$tr(tags$td("Skladnik losowy"), tags$td("Co zostaje poza modelem?"), tags$td("czynniki pominiete, blad pomiaru, przypadkowosc"))
          )
        )
      )
    ),

    lc_h2("ch2-przyklad", "Mini-przyklad"),
    figure_panel(
      label = "Ryc. 2.1",
      title = "Sprzedaz i naklady reklamowe",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          sliderInput("ch2_n", "Liczba obserwacji", min = 20, max = 160, value = 70, step = 10),
          sliderInput("ch2_beta1", "Sila relacji X -> Y", min = -1, max = 4, value = 1.4, step = 0.2),
          sliderInput("ch2_sigma", "Niepewnosc poza modelem", min = 1, max = 18, value = 6, step = 1)
        ),
        column(
          8,
          plotOutput("ch2_plot", height = "360px"),
          uiOutput("ch2_comment")
        )
      )
    ),

    inline_callout(
      label = "Notatka metodyczna",
      color = "ok",
      "To jeszcze nie jest pelny wyklad o KMNK. Widget ma pokazac intuicje relacji Y-X i przygotowac grunt pod nastepny rozdzial o regresji liniowej."
    ),

    lc_chapter_next(
      num = "03",
      title = "Skladnik losowy",
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
      labs(x = "X: naklady reklamowe", y = "Y: sprzedaz") +
      theme_upwr()
  })

  output$ch2_comment <- renderUI({
    fit <- lm(sprzedaz ~ naklady, data = ch2_data())
    g <- broom::glance(fit)
    coefs <- broom::tidy(fit)
    lc_stat_grid(
      eco_metric("Nachylenie", round(coefs$estimate[2], 2), "zmiana Y przy wzroscie X o 1", unname(upwr_cat["szalwia"])),
      eco_metric("R2", round(g$r.squared, 3), "czesc zmiennosci Y opisana przez X", unname(upwr_cat["niebo"])),
      eco_metric("Reszta", "Y - Y_hat", "to, czego model nie wyjasnil", unname(upwr_cat["terakota"])),
      columns = 3
    )
  })
}
