# Blok 06: Zmienność i próg ---------------------------------------------

prog_quiz <- list(question = "Która zmiana bezpośrednio zmniejsza P(T>85°C), gdy próg pozostaje stały?", choices = c("Obniżenie średniej lub odchylenia standardowego" = "both", "Zwiększenie średniej" = "mean", "Ignorowanie ogona rozkładu" = "ignore"), correct = "both", explanation = "Położenie i rozrzut rozkładu wspólnie wyznaczają pole za progiem.")
prog_exercises <- c("Bananpol: dla T~N(82,3) policz P(T>85°C) i naturalną częstość na 1000 zmian.", "Diagnostyka: porównaj histogram i wykres kwantylowy; wskaż, co podważa model normalny.", "Transfer: dla obciążenia i wytrzymałości konstrukcji policz ryzyko jako P(L>S), nie jako pole nakładania krzywych.")

prog_block <- list(id = "prog", title = "Zmienność i próg", chapters = list(
  list(id = "glosowanie", title = "Średnia poniżej progu", lead = "Bez informacji o zmienności średnia nie odpowiada na pytanie o przekroczenie.", widget = risk_vote_panel("z6_vote", "z6_vote_feedback", "Średnia temperatura wynosi 82°C, próg 85°C. Czy ryzyko jest pomijalne?", c("Tak" = "yes", "Nie — potrzebujemy rozrzutu" = "sd", "Zawsze wynosi 50%" = "half"))),
  list(id = "histogram", title = "Od histogramu do pola", lead = "Histogram z wielu zmian przybliża kształt rozkładu, a prawdopodobieństwo jest polem.", widget = risk_widget_panel("Symulacja", "Histogram stabilizuje się wraz z liczebnością", sliderInput("z6_sample", "Liczba obserwacji", 30, 5000, 200, 10), "z6_hist", "z6_hist_stats")),
  list(id = "parametry", title = "Parametry μ i σ", lead = "μ przesuwa środek, σ rozszerza lub zwęża rozkład.", formula = "T\\sim N(\\mu,\\sigma),\\qquad z=\\frac{t-\\mu}{\\sigma}", widget = risk_widget_panel("Model", "Przesuń i rozszerz krzywą", tagList(sliderInput("z6_mean", "μ (°C)", 75, 90, 82, .5), sliderInput("z6_sd", "σ (°C)", .5, 8, 3, .25)), "z6_normal", "z6_normal_stats")),
  list(id = "standaryzacja", title = "Wspólna linijka z", lead = "Standaryzacja mówi, ile odchyleń standardowych dzieli wynik od średniej.", formula = "z=(x-\\mu)/\\sigma", sections = list(list(id = "jednostki", title = "Bez jednostki", text = "Po standaryzacji można porównywać temperaturę, ciśnienie i drgania, ale tylko w ramach sensownego modelu."))),
  list(id = "ogon", title = "Część B — ryzyko przekroczenia", lead = "Próg dzieli rozkład na wyniki akceptowalne i przekroczenia.", widget = risk_widget_panel("Ogon", "Próg temperatury łożyska", sliderInput("z6_threshold", "Próg (°C)", 78, 95, 85, .5), "z6_tail", "z6_tail_stats"), decision = "Raportuj pole ogona oraz naturalną częstość w ustalonym horyzoncie."),
  list(id = "dzialania", title = "Trzy działania", lead = "Możemy obniżyć średnią, ograniczyć zmienność albo zmienić próg konstrukcyjny.", widget = figure_panel(label = "Porównanie", title = "Która interwencja najbardziej zmienia ogon?", selectInput("z6_action", "Działanie", c("Stan bazowy" = "base", "Chłodzenie: μ−2°C" = "mean", "Stabilizacja: σ−1°C" = "sd", "Wyższy próg: +2°C" = "threshold")), uiOutput("z6_action_result"), full_width = TRUE)),
  list(id = "obciazenie", title = "Obciążenie–wytrzymałość", lead = "Awaria zachodzi wtedy, gdy obciążenie L przekracza wytrzymałość S.", formula = "P(awarii)=P(L>S)=P(D<0),\\quad D=S-L", widget = risk_widget_panel("Symulacja", "Pary obciążenie–wytrzymałość", tagList(sliderInput("z6_load", "Średnie L", 60, 110, 85, 1), sliderInput("z6_strength", "Średnie S", 70, 120, 95, 1)), "z6_ls", "z6_ls_stats"), pitfall = "Pole nakładania dwóch gęstości nie jest prawdopodobieństwem L>S."),
  list(id = "nienormalny", title = "Kiedy normalny zawodzi", lead = "Skośność i ciężki ogon mogą silnie zmienić ryzyko progowe mimo podobnej średniej i odchylenia.", widget = risk_widget_panel("Rozszerzenie", "Trzy rozkłady o podobnym centrum", selectInput("z6_shape", "Kształt", c("Symetryczny" = "normal", "Skośny" = "skew", "Ciężki ogon" = "heavy")), "z6_shapes", "z6_shapes_stats"), extension = TRUE, pitfall = "Dopasowanie środka wykresu nie gwarantuje dobrego opisu ekstremów."),
  list(id = "decyzja", title = "Decyzja progowa", lead = "Wynik powinien wskazywać mechanizm, horyzont i działanie.", sections = list(list(id = "sciaga", title = "Ściąga", bullets = c("Pytanie: jaka część wyników przekracza próg?", "Model: rozkład zmiennej ciągłej", "Założenia: stabilność, kształt ogona, jednostki", "Wynik: P(X>c) i naturalna częstość", "Interpretacja: oczekiwane przekroczenia w porównywalnych ekspozycjach"))), decision = "Najpierw redukuj mechanizm ryzyka; podniesienie progu wymaga uzasadnienia konstrukcyjnego."),
  list(id = "sprawdzenie", title = "Quiz i ćwiczenia", lead = "Połącz wykres, rachunek i sens inżynierski.", widget = risk_assessment_ui("z6", prog_quiz, prog_exercises), duration = "15–20 min")
))
prog_chapters <- risk_block_chapters(prog_block)

prog_server <- function(input, output, session) {
  v <- reactiveVal(FALSE)
  observeEvent(input$z6_vote_check, v(TRUE))
  output$z6_vote_feedback <- renderUI({
    req(v())
    lc_feedback(type = if (identical(input$z6_vote, "sd")) "ok" else "warning", tags$strong("Potrzebujemy σ:"), " przy σ=3°C przekroczenie dotyczy około 16% zmian.")
  })
  sample_values <- reactive({
    set.seed(606)
    rnorm(input$z6_sample, 82, 3)
  })
  hist_plot <- reactive(ggplot(data.frame(t = sample_values()), aes(t)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, fill = upwr_secondary, colour = "white") +
    stat_function(fun = dnorm, args = list(mean = 82, sd = 3), colour = upwr_accent, linewidth = 1) +
    labs(title = "Histogram i model gęstości", x = "Temperatura (°C)", y = "Gęstość") +
    theme_upwr())
  zoom_plot_server("z6_hist", hist_plot, alt = "Histogram temperatur z nałożoną krzywą normalną.")
  output$z6_hist_stats <- renderUI(lc_stat_grid(lc_stat_box("Średnia próby", round(mean(sample_values()), 2)), lc_stat_box("SD próby", round(sd(sample_values()), 2)), columns = 1))
  normal_plot <- reactive({
    x <- seq(65, 105, length.out = 400)
    ggplot(data.frame(x, p = dnorm(x, input$z6_mean, input$z6_sd)), aes(x, p)) +
      geom_line(colour = upwr_accent, linewidth = 1.1) +
      geom_vline(xintercept = input$z6_mean, linetype = 2) +
      labs(title = "Położenie i szerokość rozkładu", x = "Temperatura (°C)", y = "Gęstość") +
      theme_upwr()
  })
  zoom_plot_server("z6_normal", normal_plot, alt = "Krzywa normalna sterowana średnią i odchyleniem standardowym.")
  output$z6_normal_stats <- renderUI(lc_stat_grid(lc_stat_box("z dla 85°C", round((85 - input$z6_mean) / input$z6_sd, 2)), columns = 1))
  tail_plot <- reactive({
    x <- seq(input$z6_mean - 4 * input$z6_sd, input$z6_mean + 5 * input$z6_sd, length.out = 500)
    d <- data.frame(x, p = dnorm(x, input$z6_mean, input$z6_sd))
    ggplot(d, aes(x, p)) +
      geom_area(data = d[d$x >= input$z6_threshold, ], fill = upwr_accent, alpha = .55) +
      geom_line(colour = upwr_secondary, linewidth = 1) +
      geom_vline(xintercept = input$z6_threshold, linetype = 2) +
      labs(title = "Pole za progiem", x = "Temperatura (°C)", y = "Gęstość") +
      theme_upwr()
  })
  zoom_plot_server("z6_tail", tail_plot, alt = "Krzywa normalna z zacieniowanym obszarem temperatur powyżej progu.")
  output$z6_tail_stats <- renderUI({
    p <- risk_normal_exceedance(input$z6_threshold, input$z6_mean, input$z6_sd)
    lc_stat_grid(lc_stat_box("P(przekroczenia)", risk_format_probability(p), color = upwr_accent), lc_stat_box("Częstość", risk_natural_frequency(p)), columns = 1)
  })
  output$z6_action_result <- renderUI({
    pars <- switch(input$z6_action,
      base = c(82, 3, 85),
      mean = c(80, 3, 85),
      sd = c(82, 2, 85),
      threshold = c(82, 3, 87)
    )
    p <- risk_normal_exceedance(pars[3], pars[1], pars[2])
    lc_stat_grid(lc_stat_box("μ / σ / próg", paste(pars, collapse = " / ")), lc_stat_box("P(przekroczenia)", risk_format_probability(p), color = upwr_accent), columns = 1)
  })
  ls_plot <- reactive({
    set.seed(607)
    n <- 700
    l <- rnorm(n, input$z6_load, 8)
    s <- rnorm(n, input$z6_strength, 7)
    dat <- data.frame(l, s, fail = ifelse(l > s, "Awaria: L>S", "Rezerwa: S≥L"))
    ggplot(dat, aes(l, s, colour = fail, shape = fail)) +
      geom_point(alpha = .55) +
      geom_abline(slope = 1, intercept = 0, linetype = 2) +
      scale_colour_manual(values = c("Awaria: L>S" = upwr_accent, "Rezerwa: S≥L" = upwr_secondary)) +
      labs(title = "Każdy punkt to para L i S", x = "Obciążenie L", y = "Wytrzymałość S", colour = NULL, shape = NULL) +
      theme_upwr()
  })
  zoom_plot_server("z6_ls", ls_plot, alt = "Punkty obciążenia i wytrzymałości po obu stronach linii równości.")
  output$z6_ls_stats <- renderUI(lc_stat_grid(lc_stat_box("P(L>S)", risk_format_probability(risk_stress_strength_normal(input$z6_load, 8, input$z6_strength, 7)), color = upwr_accent), columns = 1))
  shapes_plot <- reactive({
    set.seed(608)
    gamma_scale <- 3 / sqrt(3)
    x <- switch(input$z6_shape,
      normal = rnorm(5000, 82, 3),
      skew = 82 - 3 * gamma_scale + rgamma(5000, shape = 3, scale = gamma_scale),
      heavy = 82 + 3 * rt(5000, df = 3) / sqrt(3)
    )
    ggplot(data.frame(x), aes(x)) +
      geom_histogram(bins = 60, fill = upwr_secondary, colour = "white") +
      geom_vline(xintercept = 85, colour = upwr_accent, linewidth = 1) +
      coord_cartesian(xlim = c(65, 105)) +
      labs(title = "Kształt ogona ma znaczenie", x = "Temperatura (°C)", y = "Liczba obserwacji") +
      theme_upwr()
  })
  zoom_plot_server("z6_shapes", shapes_plot, alt = "Histogram wybranego rozkładu z zaznaczonym progiem.")
  output$z6_shapes_stats <- renderUI(lc_feedback(type = "info", "Porównuj prawdopodobieństwo przekroczenia, nie tylko średnią i odchylenie."))
  risk_assessment_server("z6", prog_quiz, input, output)
}
