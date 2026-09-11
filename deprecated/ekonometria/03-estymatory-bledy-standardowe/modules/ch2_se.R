# ============================================================================
# ROZDZIAŁ 2: Błędy standardowe
# ============================================================================

ch2_ui <- lecture_chapter(
  id = "ch-se",
  num = "02",
  title = "Błędy standardowe",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 03 · Estymacja",
      num = "02",
      title = "Błędy standardowe.",
      lead = "Skąd wiemy, jak duża jest niepewność b₁? Symulujemy: generujemy wiele prób z tego samego mechanizmu i patrzymy, jak rozrzucone są oszacowania. To rozrzucenie ma swoją nazwę — błąd standardowy."
    ),

    lc_h2("ch2-idea", "Idea błędu standardowego"),
    lc_p("Błąd standardowy SE(b₁) mierzy, jak bardzo b₁ wahałoby się, gdybyśmy losowali kolejne próby z tej samej populacji. To nie jest „błąd” w sensie pomyłki ani niedbalstwa analityka. To miara wrodzonej niepewności wynikającej z tego, że pracujemy z próbą, a nie z całą populacją."),
    lc_p("Formalnie SE(b₁) to odchylenie standardowe rozkładu estymatora — czyli rozkładu, który widzielibyśmy, gdyby udało się powtarzać badanie wielokrotnie. W realnym świecie mamy tylko jedną próbę, więc SE liczymy wzorem analitycznym. Ale w symulacji możemy zobaczyć ten rozkład wprost — i to robimy poniżej."),

    lc_h2("ch2-symulacja", "Eksperyment z wieloma próbami"),
    lc_p("Generujemy dane z tego samego prawdziwego modelu (β₀ = 10, β₁ = 1.5). Powtarzamy losowanie próby wiele razy, za każdym razem dopasowując regresję i zapisując oszacowane nachylenie b₁. Histogram poniżej pokazuje, jak te nachylenia są rozrzucone wokół prawdziwego β₁ = 1.5."),
    figure_panel(
      label = "Ryc. 2.1",
      title = "Rozkład estymatora nachylenia",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          sliderInput("ch2_n", "Liczba obserwacji w jednej próbie",
                      min = 20, max = 250, value = 60, step = 10),
          sliderInput("ch2_sigma", "Szum w danych (σ)",
                      min = 1, max = 25, value = 8, step = 1),
          sliderInput("ch2_reps", "Liczba symulowanych prób",
                      min = 50, max = 600, value = 250, step = 50)
        ),
        column(
          8,
          plotOutput("ch2_plot", height = "360px"),
          uiOutput("ch2_stats"),
          uiOutput("ch2_verdict")
        )
      )
    ),

    lc_h2("ch2-co-wplywa", "Co zwiększa, a co zmniejsza SE?"),
    tagList(
      tags$ul(
        tags$li(strong("Większa próba (n ↑)"), " → mniejszy SE. Każda dodatkowa obserwacja stabilizuje oszacowanie. Ruszanie suwakiem n w górę zauważalnie ścieśnia histogram."),
        tags$li(strong("Mniejszy szum (σ ↓)"), " → mniejszy SE. Mniej zaszumione dane łatwiej dopasować — punkty leżą blisko prawdziwej linii, więc nachylenie liczy się stabilniej."),
        tags$li(strong("Większy rozrzut X"), " → mniejszy SE. Dane rozciągnięte w X dają więcej „dźwigni” dla nachylenia. Tu w symulacji X jest losowane z tego samego zakresu, więc ten efekt nie jest sterowany suwakiem — ale w praktyce ma duże znaczenie.")
      )
    ),

    inline_callout(
      label = "Pułapka",
      color = "uwaga",
      open = TRUE,
      "Mały SE to nie to samo co duże β₁. Można mieć b₁ = 0.01 z SE = 0.001 (efekt jest niezerowy, ale ekonomicznie znikomy) i b₁ = 5 z SE = 4 (efekt jest duży, ale niepewny). Patrz zawsze na obie liczby — sama estymata ani sam SE niczego nie rozstrzyga."
    ),

    lc_chapter_next(
      num = "03",
      title = "Test t i przedział ufności",
      lead = "od SE do wnioskowania",
      target_id = "ch-test"
    )
  )
)

ch2_server <- function(input, output, session) {
  ch2_slopes <- reactive({
    set.seed(303)
    seeds <- sample.int(1e6, input$ch2_reps)
    vapply(seeds, function(s) {
      d <- eco_regression_data(input$ch2_n, beta0 = 10, beta1 = 1.5,
                               sigma = input$ch2_sigma, seed = s)
      unname(coef(lm(y ~ x, data = d))[2])
    }, numeric(1))
  })

  output$ch2_plot <- renderPlot({
    s <- ch2_slopes()
    data.frame(b1 = s) |>
      ggplot(aes(b1)) +
      geom_histogram(bins = 28, fill = unname(upwr_cat["niebo"]),
                     color = "white", alpha = 0.9) +
      geom_vline(xintercept = 1.5, color = upwr_accent, linewidth = 1) +
      geom_vline(xintercept = mean(s), color = unname(upwr_cat["szalwia"]),
                 linewidth = 1, linetype = "dashed") +
      labs(x = "Oszacowane nachylenie b₁",
           y = "Liczba prób",
           title = "Rozkład b₁ w wielu próbach (czerwona = prawdziwe β₁)") +
      theme_upwr()
  })

  output$ch2_stats <- renderUI({
    s <- ch2_slopes()
    lc_stat_grid(
      lc_stat_box("Średnia b₁", eco_fmt(mean(s), 3),
                  caption = "centrum rozkładu próby",
                  color = unname(upwr_cat["szalwia"])),
      lc_stat_box("SD b₁", eco_fmt(sd(s), 3),
                  caption = "symulacyjny błąd standardowy",
                  color = upwr_accent),
      lc_stat_box("Prawdziwe β₁", "1.500",
                  caption = "wartość, której szukamy",
                  color = upwr_secondary),
      columns = 3
    )
  })

  output$ch2_verdict <- renderUI({
    s <- ch2_slopes()
    lc_feedback(
      type = "info",
      tags$p(
        "Przy n = ", strong(input$ch2_n),
        ", σ = ", strong(input$ch2_sigma),
        " i ", strong(input$ch2_reps), " powtórzeniach: ",
        "oszacowane nachylenie wahało się od ", strong(eco_fmt(min(s), 2)),
        " do ", strong(eco_fmt(max(s), 2)),
        ", średnio ", strong(eco_fmt(mean(s), 3)),
        " z odchyleniem ±", strong(eco_fmt(sd(s), 3)),
        ". Prawdziwy parametr β₁ = 1.500 leży w środku rozkładu — symulacja pokazuje, że estymator KMNK trafia w cel ‚średnio’, choć pojedyncza próba może odbiegać."
      )
    )
  })
}
