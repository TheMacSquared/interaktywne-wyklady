# ============================================================================
# CHAPTER 3: Skladnik losowy
# ============================================================================

ch3_ui <- lecture_chapter(
  id = "ch-losowy",
  num = "03",
  title = "Skladnik losowy",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdzial 01 · Model ekonometryczny",
      num = "03",
      title = "Rola skladnika losowego.",
      lead = "Skladnik losowy nie jest smietnikiem. To formalny zapis tego, ze model opisuje zjawisko z niepewnoscia."
    ),

    lc_h2("ch3-co-zawiera", "Co trafia do epsilon?"),
    tags$ul(
      tags$li("czynniki, ktorych nie obserwujemy albo nie umiemy dobrze zmierzyc,"),
      tags$li("bledy pomiaru w danych,"),
      tags$li("losowe zaburzenia zachowan ludzi, firm i instytucji,"),
      tags$li("niedopasowanie postaci modelu, np. prosta zamiast relacji krzywoliniowej.")
    ),

    lc_h2("ch3-widget", "Jak szum zmienia obraz zaleznosci?"),
    figure_panel(
      label = "Ryc. 3.1",
      title = "Ta sama relacja, rozny skladnik losowy",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          sliderInput("ch3_sigma", "Odchylenie skladnika losowego", min = 1, max = 25, value = 8, step = 1),
          sliderInput("ch3_beta1", "Nachylenie relacji", min = 0, max = 3, value = 1.2, step = 0.2),
          checkboxInput("ch3_true", "Pokaz prawdziwa zaleznosc", value = TRUE)
        ),
        column(8, plotOutput("ch3_plot", height = "360px"))
      )
    ),

    lc_h2("ch3-wniosek", "Wniosek praktyczny"),
    inline_callout(
      label = "Zapamietaj",
      color = "uwaga",
      open = TRUE,
      "Im wiekszy skladnik losowy wzgledem systematycznej czesci modelu, tym trudniej zobaczyc relacje w danych i tym ostrozniej interpretujemy parametry."
    ),

    lc_chapter_next(
      num = "04",
      title = "Postepowanie ekonometryczne",
      lead = "od pytania do interpretacji",
      target_id = "ch-postepowanie"
    )
  )
)

ch3_server <- function(input, output, session) {
  output$ch3_plot <- renderPlot({
    df <- eco_make_regression_data(
      n = 100,
      beta0 = 20,
      beta1 = input$ch3_beta1,
      sigma = input$ch3_sigma,
      seed = 2027
    )

    p <- ggplot(df, aes(naklady, sprzedaz)) +
      geom_point(color = unname(upwr_cat["grafit"]), alpha = 0.6, size = 2) +
      geom_smooth(method = "lm", se = FALSE, color = upwr_accent, linewidth = 1) +
      labs(x = "X", y = "Y") +
      theme_upwr()

    if (isTRUE(input$ch3_true)) {
      p <- p + geom_line(aes(y = fitted_true), color = unname(upwr_cat["niebo"]),
                         linewidth = 1, linetype = "dashed")
    }

    p
  })
}
