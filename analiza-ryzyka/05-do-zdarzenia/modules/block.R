# Blok 05: Ile prób do zdarzenia -----------------------------------------

dozd_quiz <- list(
  question = "Co jest ustalone w modelu ujemnym dwumianowym?",
  choices = c("Liczba oczekiwanych zdarzeń r" = "r", "Łączna liczba prób n" = "n", "Dokładny moment ostatniego zdarzenia" = "time"),
  correct = "r", explanation = "Eksperyment trwa do r-tego zdarzenia, więc liczba prób jest losowa."
)
dozd_exercises <- c(
  "Bananpol: przy p=0,10 wyznacz średnią liczbę kontroli do znalezienia trzech wadliwych zabezpieczeń i P(ukończenia do 40. kontroli).",
  "Diagnostyka: jakość zmienia się między partiami. Wyjaśnij, dlaczego model ze stałym p może zaniżyć niepewność planu.",
  "Transfer: zaplanuj liczbę audytów potrzebną do zaobserwowania dwóch naruszeń procedury przy podanym p."
)

dozd_block <- list(id = "dozd", title = "Ile prób do zdarzenia", chapters = list(
  list(
    id = "regula", title = "Co pozostaje stałe?", lead = "Dwumianowy zatrzymuje się po n próbach; ujemny dwumianowy po r zdarzeniach.",
    widget = risk_vote_panel("d5_vote", "d5_vote_feedback", "Chcemy znaleźć trzy wadliwe zabezpieczenia. Który element eksperymentu jest stały?", c("r=3 znalezione wady" = "r", "n — liczba kontroli" = "n", "odsetek wad w zebranej próbie" = "share"))
  ),
  list(
    id = "pierwsze", title = "Do pierwszego wykrycia", lead = "Rozkład geometryczny ma długi ogon: sukces może nadejść szybko albo bardzo późno.",
    formula = "P(X=x)=(1-p)^{x-1}p",
    widget = risk_widget_panel("Symulacja", "Ile kontroli do pierwszej wady?", tagList(sliderInput("d5_geo_p", "p", .01, .5, .1, .01), actionButton("d5_geo_run", "Losuj ponownie", class = "lc-btn-primary")), "d5_geo", "d5_geo_stats")
  ),
  list(
    id = "rte", title = "Do r-tego wykrycia", lead = "Suma oczekiwania na kolejne wykrycia daje liczbę wszystkich prób.",
    widget = risk_widget_panel("Rozkład", "Łączna liczba kontroli", tagList(sliderInput("d5_p", "p wykrycia", .01, .5, .1, .01), sliderInput("d5_r", "r", 1, 10, 3, 1)), "d5_nb", "d5_nb_stats")
  ),
  list(
    id = "parametryzacje", title = "Dwie parametryzacje", lead = "Oprogramowanie może zwracać niepowodzenia przed r-tym sukcesem zamiast wszystkich prób.",
    sections = list(list(id = "os", title = "Ta sama realizacja", text = "Jeżeli znaleziono r zdarzeń po X wszystkich próbach, liczba wcześniejszych niepowodzeń wynosi X−r.")),
    formula = "X_{wszystkie}=Y_{niepowodzenia}+r", pitfall = "Bez nazwania parametryzacji wynik może różnić się dokładnie o r."
  ),
  list(
    id = "zasoby", title = "Średnia kontra plan zasobów", lead = "Średnia r/p nie gwarantuje ukończenia przed limitem.",
    formula = "E(X)=\\frac{r}{p}",
    widget = figure_panel(label = "Kalkulator", title = "Limit liczby kontroli", sliderInput("d5_limit", "Limit", 3, 200, 40, 1), uiOutput("d5_plan"), full_width = TRUE),
    decision = "Planuj na podstawie prawdopodobieństwa ukończenia lub kwantyla, a nie tylko średniej."
  ),
  list(
    id = "zawodzi", title = "Kiedy model zawodzi", lead = "Stałe p i niezależność są założeniami operacyjnymi.",
    widget = risk_widget_panel("Porównanie", "Stałe p kontra partie o różnej jakości", sliderInput("d5_variation", "Zmienność p między partiami", 0, .09, .04, .005), "d5_failure", "d5_failure_stats"),
    pitfall = "Uczenie kontrolera, grupowanie wad i zmiana dostawy mogą zmieniać p w czasie."
  ),
  list(
    id = "decyzja", title = "Ile kontroli zaplanować?", lead = "Plan powinien podać cel, limit i ryzyko niedokończenia.",
    sections = list(list(id = "raport", title = "Minimalny raport", bullets = c("cel r i definicja wykrycia", "p i jego źródło", "limit zasobów", "P(ukończenia przed limitem)", "reakcja, gdy limit zostanie przekroczony"))),
    decision = "Oddziel oczekiwaną liczbę kontroli od bezpiecznego zapasu planistycznego."
  ),
  list(
    id = "sprawdzenie", title = "Ściąga i sprawdzenie", lead = "Reguła zatrzymania → p i r → rozkład → limit → decyzja.",
    sections = list(list(id = "sciaga", title = "Ściąga", bullets = c("Pytanie: ile prób do r-tego zdarzenia?", "Model: geometryczny dla r=1, ujemny dwumianowy dla r>1", "Założenia: stałe p i niezależność", "Wynik: rozkład liczby prób", "Interpretacja: zasoby potrzebne do osiągnięcia celu"))),
    widget = risk_assessment_ui("d5", dozd_quiz, dozd_exercises), duration = "15–20 min"
  )
))

dozd_chapters <- risk_block_chapters(dozd_block)

dozd_server <- function(input, output, session) {
  vote <- reactiveVal(FALSE)
  observeEvent(input$d5_vote_check, vote(TRUE))
  output$d5_vote_feedback <- renderUI({
    req(vote())
    lc_feedback(type = if (identical(input$d5_vote, "r")) "ok" else "warning", tags$strong("Stały jest cel:"), " r=3; liczba kontroli pozostaje losowa.")
  })
  geo_sample <- eventReactive(input$d5_geo_run,
    {
      set.seed(as.integer(Sys.time()))
      rgeom(400, input$d5_geo_p) + 1
    },
    ignoreNULL = FALSE
  )
  geo_plot <- reactive(ggplot(data.frame(x = geo_sample()), aes(x)) +
    geom_histogram(binwidth = 1, boundary = .5, fill = upwr_secondary, colour = "white") +
    coord_cartesian(xlim = c(1, min(80, max(geo_sample())))) +
    labs(title = "Długi prawy ogon oczekiwania", x = "Liczba prób do pierwszego wykrycia", y = "Powtórzenia") +
    theme_upwr())
  zoom_plot_server("d5_geo", geo_plot, alt = "Histogram liczby prób potrzebnych do pierwszego wykrycia.")
  output$d5_geo_stats <- renderUI(lc_stat_grid(lc_stat_box("Średnia teoretyczna", round(1 / input$d5_geo_p, 1)), lc_stat_box("90. percentyl", qgeom(.9, input$d5_geo_p) + 1, color = upwr_accent), columns = 1))
  nb_plot <- reactive({
    maxx <- max(input$d5_r + 10, qnbinom(.995, input$d5_r, input$d5_p) + input$d5_r)
    x <- input$d5_r:maxx
    ggplot(data.frame(x, p = vapply(x, risk_negative_binomial_total_pmf, numeric(1), r = input$d5_r, p = input$d5_p)), aes(x, p)) +
      geom_col(fill = upwr_accent) +
      labs(title = "Liczba wszystkich prób do r-tego zdarzenia", x = "Wszystkie próby", y = "Prawdopodobieństwo") +
      theme_upwr()
  })
  zoom_plot_server("d5_nb", nb_plot, alt = "Rozkład liczby wszystkich prób do osiągnięcia ustalonej liczby wykryć.")
  output$d5_nb_stats <- renderUI(lc_stat_grid(lc_stat_box("E(X)=r/p", round(input$d5_r / input$d5_p, 1)), lc_stat_box("Niepowodzenia średnio", round(input$d5_r * (1 - input$d5_p) / input$d5_p, 1)), columns = 1))
  output$d5_plan <- renderUI({
    pfinish <- risk_negative_binomial_finish(input$d5_limit, input$d5_r, input$d5_p)
    lc_stat_grid(lc_stat_box("Średnia", round(input$d5_r / input$d5_p, 1)), lc_stat_box("P(ukończenia do limitu)", risk_format_probability(pfinish), color = upwr_accent), lc_stat_box("95. percentyl", qnbinom(.95, input$d5_r, input$d5_p) + input$d5_r), columns = 1)
  })
  failure_plot <- reactive({
    set.seed(505)
    stable <- rnbinom(1000, size = 3, prob = .1) + 3
    ps <- pmin(.95, pmax(.005, rnorm(1000, .1, input$d5_variation)))
    mixed <- vapply(ps, function(p) rnbinom(1, 3, p) + 3, numeric(1))
    dat <- data.frame(x = c(stable, mixed), model = rep(c("Stałe p", "Zmienne p między partiami"), each = 1000))
    ggplot(dat, aes(x, fill = model)) +
      geom_histogram(binwidth = 3, position = "identity", alpha = .55) +
      coord_cartesian(xlim = c(3, 150)) +
      scale_fill_manual(values = upwr_cat_n(2)) +
      labs(title = "Zmienność jakości poszerza rozkład", x = "Liczba kontroli", y = "Powtórzenia", fill = NULL) +
      theme_upwr()
  })
  zoom_plot_server("d5_failure", failure_plot, alt = "Nakładające się histogramy stałego i zmiennego prawdopodobieństwa wykrycia.")
  output$d5_failure_stats <- renderUI(lc_feedback(type = "warning", "Model ze stałym p nie pokazuje ryzyka wyjątkowo słabej partii."))
  risk_assessment_server("d5", dozd_quiz, input, output)
}
