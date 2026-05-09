# ============================================================================
# ROZDZIAŁ 3: Składnik losowy
# ============================================================================

ch3_ui <- lecture_chapter(
  id = "ch-losowy",
  num = "03",
  title = "Składnik losowy",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 01 · Model ekonometryczny",
      num = "03",
      title = "Rola składnika losowego.",
      lead = "Składnik losowy nie jest śmietnikiem. To formalny zapis tego, że model opisuje zjawisko z niepewnością."
    ),

    lc_h2("ch3-co-zawiera", "Co trafia do ε?"),
    lc_p("Każdy model — nawet najlepszy — coś pomija. Składnik losowy ε to formalna nazwa dla wszystkiego, czego model nie opisał. Nie jest to „błąd” w sensie pomyłki. Jest to różnica między tym, co model przewiduje, a tym, co rzeczywiście obserwujemy."),
    tags$ul(
      tags$li("czynniki, których nie obserwujemy albo nie umiemy dobrze zmierzyć (np. nastrój konsumenta, lokalna konkurencja),"),
      tags$li("błędy pomiaru w danych (zaokrąglenia, błędy w sprawozdaniach finansowych, niedokładność ankiet),"),
      tags$li("losowe zaburzenia zachowań ludzi, firm i instytucji,"),
      tags$li("niedopasowanie postaci modelu — np. próbujemy opisać prostą relację, która naprawdę jest krzywoliniowa.")
    ),

    lc_h2("ch3-widget", "Jak szum zmienia obraz zależności?"),
    lc_p("Poniżej ta sama systematyczna zależność (ta sama prawdziwa prosta — niebieska, przerywana) z różnym poziomem szumu. Czerwona linia to prosta dopasowana do konkretnej próby. Zauważ, że im większy szum, tym mocniej dopasowana prosta odbiega od prawdziwej, mimo że mechanizm generujący dane się nie zmienił."),
    figure_panel(
      label = "Ryc. 3.1",
      title = "Ta sama relacja, różny składnik losowy",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          sliderInput("ch3_sigma", "Odchylenie składnika losowego", min = 1, max = 25, value = 8, step = 1),
          sliderInput("ch3_beta1", "Nachylenie relacji", min = 0, max = 3, value = 1.2, step = 0.2),
          checkboxInput("ch3_true", "Pokaż prawdziwą zależność", value = TRUE)
        ),
        column(8, plotOutput("ch3_plot", height = "360px"))
      )
    ),

    lc_h2("ch3-wniosek", "Wniosek praktyczny"),
    inline_callout(
      label = "Zapamiętaj",
      color = "uwaga",
      open = TRUE,
      "Im większy składnik losowy względem systematycznej części modelu, tym trudniej zobaczyć relację w danych i tym ostrożniej interpretujemy parametry. Mała próba + duży szum = łatwo o przypadkowy obraz."
    ),

    lc_chapter_next(
      num = "04",
      title = "Postępowanie ekonometryczne",
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
      labs(x = "X: nakłady", y = "Y: sprzedaż") +
      theme_upwr()

    if (isTRUE(input$ch3_true)) {
      p <- p + geom_line(aes(y = fitted_true), color = unname(upwr_cat["niebo"]),
                         linewidth = 1, linetype = "dashed")
    }

    p
  })
}
