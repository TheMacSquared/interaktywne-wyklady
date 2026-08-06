# Blok 04: Wiele prób -----------------------------------------------------

proby_quiz <- list(
  question = "Który warunek jest konieczny dla prostego modelu dwumianowego?",
  choices = c(
    "Stała liczba prób i to samo p w każdej próbie" = "fixed",
    "Rosnące p po każdej awarii" = "growing",
    "Co najmniej trzy możliwe wyniki próby" = "three"
  ),
  correct = "fixed",
  explanation = "Model wymaga ustalonego n, dwóch wyników, stałego p i niezależności prób."
)
proby_exercises <- c(
  "Bananpol: dla n=100 i p=0,02 policz P(X=0), P(X=2) i P(X≥1).",
  "Diagnostyka: partia pochodzi z dwóch dostaw o różnej jakości. Które założenie modelu dwumianowego jest zagrożone?",
  "Transfer: zdefiniuj próbę, sukces i n dla kontroli 30 mocowań rusztowania."
)

proby_block <- list(id = "proby", title = "Wiele prób", chapters = list(
  list(
    id = "jednostka", title = "Co jest pojedynczą próbą?",
    lead = "Najpierw ustalamy jednostkę ekspozycji i wynik 0/1.",
    widget = risk_vote_panel(
      "p4_vote", "p4_vote_feedback", "Która definicja tworzy porównywalne próby?",
      c(
        "Każdy skontrolowany zawór; wynik: sprawny/niesprawny" = "valve",
        "Każdy dzień; wynik: dowolna liczba usterek" = "day",
        "Cała fabryka; wynik: wszystkie obserwacje" = "factory"
      )
    )
  ),
  list(
    id = "bernoulli", title = "Linia Bernoulliego",
    lead = "Pojedyncze wyniki są losowe, choć długookresowa częstość jest stabilna.",
    widget = figure_panel(
      label = "Symulacja", title = "Seria kontroli zaworów",
      sliderInput("p4_series_n", "Liczba kontroli", 10, 200, 100, 10),
      actionButton("p4_run", "Uruchom serię", class = "lc-btn-primary"),
      verbatimTextOutput("p4_sequence"), uiOutput("p4_series_stats"), full_width = TRUE
    )
  ),
  list(
    id = "zalozenia", title = "Cztery założenia",
    lead = "Dwumianowy jest modelem sytuacji, nie tylko wzorem.",
    sections = list(list(
      id = "karty", title = "Lista kontrolna",
      bullets = c("n jest ustalone przed obserwacją", "każda próba ma dwa rozłączne wyniki", "p jest stałe", "wyniki prób są niezależne")
    )),
    widget = figure_panel(
      label = "Diagnoza", title = "Scenariusz partii",
      selectInput("p4_scenario", "Sytuacja", c("Jedna stabilna linia" = "stable", "Dwie dostawy o różnym p" = "mixture", "Uszkodzenie zwiększa ryzyko następnego" = "dependent")),
      uiOutput("p4_scenario_feedback"), full_width = TRUE
    ),
    pitfall = "Duża partia nie naprawia złej definicji próby ani zmiennego p."
  ),
  list(
    id = "rozklad", title = "Rozkład liczby awarii",
    lead = "Rozkład odpowiada na pytania dokładnie, co najmniej i najwyżej.",
    formula = "X\\sim Bin(n,p),\\quad P(X=k)={n\\choose k}p^k(1-p)^{n-k}",
    widget = risk_widget_panel(
      "Rozkład", "Liczba niesprawnych zaworów",
      tagList(
        sliderInput("p4_n", "n", 10, 300, 100, 10), sliderInput("p4_p", "p", .001, .10, .02, .001),
        sliderInput("p4_k", "k", 0, 20, 2, 1),
        selectInput("p4_query", "Pytanie", c("Dokładnie k" = "exactly", "Co najmniej k" = "at_least", "Najwyżej k" = "at_most"))
      ),
      "p4_binom", "p4_binom_stats"
    )
  ),
  list(
    id = "srednia", title = "Średnia nie jest prognozą",
    lead = "np opisuje środek wielu partii, nie wynik jednej konkretnej partii.",
    formula = "E(X)=np,\\qquad Var(X)=np(1-p)",
    widget = risk_widget_panel(
      "Powtórzenia", "Wiele partii przy tych samych parametrach",
      sliderInput("p4_batches", "Liczba partii", 50, 2000, 500, 50), "p4_batches_plot", "p4_batches_stats"
    )
  ),
  list(
    id = "co-najmniej-jedna", title = "Co najmniej jedna",
    lead = "Łatwiej policzyć zdarzenie przeciwne: ani jednej niesprawności.",
    formula = "P(X\\ge 1)=1-P(X=0)=1-(1-p)^n",
    widget = risk_widget_panel(
      "Krzywa", "Ryzyko wraz z liczbą prób",
      sliderInput("p4_curve_p", "p niesprawności", .001, .10, .02, .001), "p4_one", "p4_one_stats"
    )
  ),
  list(
    id = "decyzja", title = "Decyzja kontrolna",
    lead = "Plan kontroli łączy ryzyko partii z regułą akceptacji.",
    sections = list(list(
      id = "plan", title = "Co należy zapisać?",
      bullets = c("wielkość losowanej próby", "dopuszczalna liczba niesprawnych", "p reprezentujące jakość partii", "konsekwencję odrzucenia i przeoczenia")
    )),
    decision = "Porównaj kilka jakości partii, zanim wybierzesz n i limit akceptacji."
  ),
  list(
    id = "sprawdzenie", title = "Ściąga i sprawdzenie",
    lead = "Jednostka → założenia → Bin(n,p) → pytanie ogonowe → decyzja.",
    sections = list(list(id = "sciaga", title = "Ściąga", bullets = c("Pytanie: ile zdarzeń w n próbach?", "Model: dwumianowy", "Założenia: stałe n i p, dwa wyniki, niezależność", "Wynik: prawdopodobieństwo liczby zdarzeń", "Interpretacja: dotyczy powtarzalnych partii"))),
    widget = risk_assessment_ui("p4", proby_quiz, proby_exercises), duration = "15–20 min"
  )
))

proby_chapters <- risk_block_chapters(proby_block)

proby_server <- function(input, output, session) {
  vote <- reactiveVal(FALSE)
  observeEvent(input$p4_vote_check, vote(TRUE))
  output$p4_vote_feedback <- renderUI({
    req(vote())
    lc_feedback(type = if (identical(input$p4_vote, "valve")) "ok" else "warning", tags$strong("Jednostka:"), " jeden zawór i dwa rozłączne wyniki.")
  })
  series <- eventReactive(input$p4_run, rbinom(input$p4_series_n, 1, .02), ignoreNULL = FALSE)
  output$p4_sequence <- renderText(paste(ifelse(series() == 1, "×", "·"), collapse = " "))
  output$p4_series_stats <- renderUI(lc_stat_grid(lc_stat_box("Niesprawne", sum(series()), color = upwr_accent), lc_stat_box("Oczekiwano średnio", round(length(series()) * .02, 1)), columns = 1))
  output$p4_scenario_feedback <- renderUI({
    messages <- c(stable = "Założenia są wiarygodne, jeśli kontrole nie wpływają na siebie.", mixture = "Zmienia się p: rozważ warstwy dostaw.", dependent = "Zagrożona jest niezależność prób.")
    lc_feedback(type = if (identical(input$p4_scenario, "stable")) "ok" else "warning", messages[[input$p4_scenario]])
  })
  binom_plot <- reactive({
    kmax <- max(15, qbinom(.999, input$p4_n, input$p4_p))
    x <- 0:kmax
    dat <- data.frame(x, p = dbinom(x, input$p4_n, input$p4_p))
    selected <- switch(input$p4_query,
      exactly = x == input$p4_k,
      at_least = x >= input$p4_k,
      at_most = x <= input$p4_k
    )
    dat$part <- ifelse(selected, "Odpowiedź", "Pozostałe")
    ggplot(dat, aes(x, p, fill = part)) +
      geom_col() +
      scale_fill_manual(values = c(Odpowiedź = upwr_accent, Pozostałe = upwr_reference)) +
      labs(title = "Rozkład liczby niesprawnych", x = "Liczba niesprawnych", y = "Prawdopodobieństwo", fill = NULL) +
      theme_upwr()
  })
  zoom_plot_server("p4_binom", binom_plot, alt = "Słupkowy rozkład dwumianowy z wyróżnionym zakresem odpowiedzi.")
  output$p4_binom_stats <- renderUI({
    k <- min(input$p4_k, input$p4_n)
    lc_stat_grid(lc_stat_box("Prawdopodobieństwo", risk_format_probability(risk_binomial_probability(input$p4_n, input$p4_p, k, input$p4_query)), color = upwr_accent), columns = 1)
  })
  batches <- reactive({
    set.seed(2404)
    rbinom(input$p4_batches, input$p4_n, input$p4_p)
  })
  batches_plot <- reactive(ggplot(data.frame(x = batches()), aes(x)) +
    geom_histogram(binwidth = 1, boundary = -.5, fill = upwr_secondary, colour = "white") +
    geom_vline(xintercept = input$p4_n * input$p4_p, colour = upwr_accent, linewidth = 1) +
    labs(title = "Wyniki wielu partii", x = "Liczba niesprawnych", y = "Liczba partii") +
    theme_upwr())
  zoom_plot_server("p4_batches_plot", batches_plot, alt = "Histogram liczby niesprawnych w wielu partiach z linią wartości oczekiwanej.")
  output$p4_batches_stats <- renderUI(lc_stat_grid(lc_stat_box("E(X)", round(input$p4_n * input$p4_p, 2)), lc_stat_box("Zakres w symulacji", paste(range(batches()), collapse = "–")), columns = 1))
  one_plot <- reactive({
    n <- 1:300
    ggplot(data.frame(n, p = vapply(n, risk_at_least_one, numeric(1), p = input$p4_curve_p)), aes(n, p)) +
      geom_line(colour = upwr_accent, linewidth = 1.1) +
      labs(title = "Im więcej ekspozycji, tym większa szansa co najmniej jednego zdarzenia", x = "Liczba prób", y = "P(X ≥ 1)") +
      theme_upwr()
  })
  zoom_plot_server("p4_one", one_plot, alt = "Rosnąca krzywa prawdopodobieństwa co najmniej jednej niesprawności.")
  output$p4_one_stats <- renderUI(lc_stat_grid(lc_stat_box("Dla n=100", risk_format_probability(risk_at_least_one(100, input$p4_curve_p)), color = upwr_accent), columns = 1))
  risk_assessment_server("p4", proby_quiz, input, output)
}
