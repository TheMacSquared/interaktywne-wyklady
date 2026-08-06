# Blok 09: Analiza drzewa błędów ----------------------------------------

fta_quiz <- list(question = "Czy dla bramki OR wolno zawsze dodać prawdopodobieństwa wejść?", choices = c("Nie — suma podwójnie liczy część wspólną" = "no", "Tak — OR z definicji jest sumą" = "yes", "Tylko gdy wartości są większe od 0,5" = "large"), correct = "no", explanation = "Dla niezależnych wejść używamy 1−∏(1−p_i); prosta suma jest co najwyżej przybliżeniem dla rzadkich, rozłącznych zdarzeń.")
fta_exercises <- c("Bananpol: policz P(top) dla inicjacji 0,005 oraz OR awarii detekcji 0,05 i tłumienia 0,08.", "Diagnostyka: znajdź powtórzone zdarzenie bazowe i wyjaśnij ryzyko podwójnego liczenia.", "Transfer: zbuduj małe drzewo utraty zasilania aparatury medycznej, oddzielając wspólną przyczynę.")

fta_block <- list(id = "fta", title = "Analiza drzewa błędów", chapters = list(
  list(id = "top", title = "Dobre zdarzenie szczytowe", lead = "Top event musi opisywać konkretny niepożądany stan, system i horyzont.", widget = risk_vote_panel("f9_vote", "f9_vote_feedback", "Która definicja jest audytowalna?", c("Nieopanowany pożar magazynu w ciągu roku" = "good", "Problem z bezpieczeństwem" = "vague", "Awaria" = "failure"))),
  list(id = "poziomy", title = "Zdarzenia szczytowe, pośrednie i bazowe", lead = "Poziomy drzewa rozdzielają skutek, logikę mechanizmu i zdarzenia bez dalszego rozwijania.", sections = list(list(id = "role", title = "Role w drzewie", bullets = c("szczytowe: analizowany niepożądany stan", "pośrednie: wynik bramki lub podsystemu", "bazowe: przyczyna z przypisanym stanem albo prawdopodobieństwem")))),
  list(id = "konstruktor", title = "Kierowany konstruktor", lead = "Pytanie operacyjne brzmi: czy wystarczy jedna przyczyna, czy potrzebna jest kombinacja?", widget = figure_panel(label = "Budowa", title = "Utrata kontroli nad zapłonem", selectInput("f9_gate", "Logika", c("Wystarczy jedna przyczyna — OR" = "or", "Potrzebna kombinacja — AND" = "and")), checkboxGroupInput("f9_causes", "Przyczyny", c("Brak detekcji" = "detect", "Brak tłumienia" = "suppress", "Utrata zasilania" = "power"), selected = c("detect", "suppress")), uiOutput("f9_structure"), full_width = TRUE)),
  list(id = "bramki", title = "Bramki AND i OR bez liczb", lead = "Przełączanie stanów liści pokazuje, która ścieżka aktywuje zdarzenie nadrzędne.", widget = figure_panel(label = "Logika", title = "Aktywuj zdarzenia bazowe", checkboxGroupInput("f9_states", "Aktywne liście", c("Inicjacja" = "init", "Brak detekcji" = "detect", "Brak tłumienia" = "suppress"), selected = character(0)), uiOutput("f9_state_result"), full_width = TRUE)),
  list(id = "rachunek", title = "Od liści do korzenia", lead = "Najpierw obliczamy bramkę OR zabezpieczeń, potem łączymy ją przez AND z inicjacją.", formula = "P(top)=P(I)\\,[1-(1-P(D))(1-P(S))]", widget = risk_widget_panel("Obliczenia", "Parametry małego drzewa", tagList(sliderInput("f9_init", "P(inicjacji)", 0, .03, .005, .001), sliderInput("f9_detect", "P(braku detekcji)", 0, .3, .05, .01), sliderInput("f9_suppress", "P(braku tłumienia)", 0, .3, .08, .01)), "f9_tree_plot", "f9_tree_stats"), pitfall = "OR nie jest automatycznie sumą prawdopodobieństw."),
  list(id = "przekroje", title = "Część B — minimalne przekroje", lead = "Minimalny przekrój to najmniejszy zestaw zdarzeń bazowych wystarczający do top event.", sections = list(list(id = "sets", title = "Dwa przekroje drzewa Bananpolu", bullets = c("{inicjacja, brak detekcji}", "{inicjacja, brak tłumienia}"))), widget = figure_panel(label = "Podświetlenie", title = "Wybierz przekrój", radioButtons("f9_cut", NULL, c("I + D" = "id", "I + S" = "is"), selected = "id"), uiOutput("f9_cut_text"), full_width = TRUE)),
  list(id = "powtorzenie", title = "Powtórzone zdarzenie bazowe", lead = "Ten sam brak zasilania może pojawić się w wielu gałęziach, ale pozostaje jednym zdarzeniem.", widget = figure_panel(label = "Pułapka", title = "Dwa wystąpienia, jedno źródło", sliderInput("f9_repeat", "P(utraty wspólnego zasilania)", 0, .2, .05, .01), uiOutput("f9_repeat_result"), full_width = TRUE), pitfall = "Traktowanie powtórzeń jako niezależnych zaniża lub zawyża wynik zależnie od struktury."),
  list(id = "wspolna", title = "Wspólna przyczyna zmienia strukturę", lead = "Zasilanie wspólne umieszczamy jako jawny liść prowadzący do obu niesprawności.", sections = list(list(id = "model", title = "Zmiana modelu", text = "Nie wystarczy skorygować liczby. Trzeba pokazać wspólny mechanizm w drzewie i ponownie ocenić minimalne przekroje.")), extension = TRUE),
  list(id = "ranking", title = "Ranking potencjalnej redukcji", lead = "Poprawiamy po kolei każdy liść i obserwujemy spadek P(top).", widget = risk_widget_panel("Wrażliwość", "Ta sama redukcja względna każdego liścia", sliderInput("f9_reduction", "Redukcja parametru", 0, .9, .5, .05), "f9_rank_plot", "f9_rank_stats"), decision = "Ranking jest wskazówką do rozmowy o kosztach i wykonalności, nie automatycznym wyborem."),
  list(id = "granice", title = "Granice FTA", lead = "Dokładny rachunek nie naprawia niekompletnego drzewa ani słabych danych.", sections = list(list(id = "audit", title = "Przegląd ekspercki", bullets = c("Czy top event jest jednoznaczny?", "Czy lista przyczyn jest wystarczająca?", "Gdzie założono niezależność?", "Czy jednostki i horyzonty są zgodne?", "Które dane są fikcyjne lub niepewne?")))),
  list(id = "sciaga", title = "Ściąga", lead = "Top event → logika → liście → zależności → redukcja → decyzja.", sections = list(list(id = "lista", title = "Reguła", bullets = c("Najpierw logika, potem liczby", "AND: potrzebne wszystkie wejścia", "OR: wystarczy co najmniej jedno wejście", "Powtórzony liść pozostaje tym samym zdarzeniem", "Wynik zależy od kompletności drzewa")))),
  list(id = "sprawdzenie", title = "Quiz i ćwiczenia", lead = "Audytuj zarówno rachunek, jak i strukturę.", widget = risk_assessment_ui("f9", fta_quiz, fta_exercises), duration = "15–20 min")
))
fta_chapters <- risk_block_chapters(fta_block)

fta_server <- function(input, output, session) {
  v <- reactiveVal(FALSE)
  observeEvent(input$f9_vote_check, v(TRUE))
  output$f9_vote_feedback <- renderUI({
    req(v())
    lc_feedback(type = if (identical(input$f9_vote, "good")) "ok" else "warning", tags$strong("Dobra definicja:"), " nieopanowany pożar magazynu w ciągu jednego roku.")
  })
  output$f9_structure <- renderUI({
    n <- length(input$f9_causes)
    lc_feedback(type = if (n >= 2) "info" else "warning", paste0("Wybrano bramkę ", toupper(input$f9_gate), " i ", n, " wejść. "), if (input$f9_gate == "or") "Każde wejście może wystarczyć." else "Wszystkie wybrane wejścia są potrzebne.")
  })
  output$f9_state_result <- renderUI({
    s <- input$f9_states
    active <- "init" %in% s && ("detect" %in% s || "suppress" %in% s)
    lc_feedback(type = if (active) "warning" else "ok", tags$strong(if (active) "Top event aktywny." else "Top event nieaktywny."), " Logika: inicjacja AND (brak detekcji OR brak tłumienia).")
  })
  tree_value <- reactive(risk_fta_top(input$f9_init, input$f9_detect, input$f9_suppress))
  tree_plot <- reactive({
    nodes <- data.frame(x = c(2, 1, 3, .5, 1.5, 2.5), y = c(3, 2, 2, 1, 1, 1), label = c("TOP", "Inicjacja", "OR", "Brak detekcji", "Brak tłumienia", ""), type = c("Szczytowe", "Bazowe", "Bramka", "Bazowe", "Bazowe", ""))
    edges <- data.frame(x = c(2, 2, 3, 3), y = c(3, 3, 2, 2), xend = c(1, 3, .5, 1.5), yend = c(2, 2, 1, 1))
    ggplot() +
      geom_segment(data = edges, aes(x, y, xend = xend, yend = yend), colour = upwr_reference) +
      geom_point(data = nodes[nodes$label != "", ], aes(x, y, shape = type, colour = type), size = 7) +
      geom_text(data = nodes[nodes$label != "", ], aes(x, y - .25, label = label), size = 3) +
      scale_colour_manual(values = upwr_cat_n(3)) +
      coord_equal(xlim = c(0, 3.5), ylim = c(.5, 3.4)) +
      labs(title = "Małe drzewo Bananpolu", x = NULL, y = NULL, shape = NULL, colour = NULL) +
      theme_upwr() +
      theme(axis.text = element_blank(), axis.ticks = element_blank())
  })
  zoom_plot_server("f9_tree_plot", tree_plot, alt = "Drzewo błędów z inicjacją połączoną przez AND z bramką OR dwóch niesprawności zabezpieczeń.")
  output$f9_tree_stats <- renderUI(lc_stat_grid(lc_stat_box("P(OR zabezpieczeń)", risk_format_probability(risk_gate_or(c(input$f9_detect, input$f9_suppress)))), lc_stat_box("P(top)", risk_format_probability(tree_value()), color = upwr_accent), columns = 1))
  output$f9_cut_text <- renderUI(lc_feedback(type = "info", if (input$f9_cut == "id") "Inicjacja + brak detekcji wystarczają do TOP." else "Inicjacja + brak tłumienia wystarczają do TOP."))
  output$f9_repeat_result <- renderUI({
    q <- input$f9_repeat
    wrong <- risk_gate_or(c(q, q))
    lc_stat_grid(lc_stat_box("Jedno wspólne zdarzenie", risk_format_probability(q), color = upwr_accent), lc_stat_box("Błędnie jako dwa niezależne", risk_format_probability(wrong)), columns = 1)
  })
  rank_plot <- reactive({
    base <- c(init = .005, detect = .05, suppress = .08)
    top0 <- do.call(risk_fta_top, unname(as.list(base)))
    gains <- vapply(names(base), function(n) {
      x <- base
      x[n] <- x[n] * (1 - input$f9_reduction)
      top0 - do.call(risk_fta_top, unname(as.list(x)))
    }, numeric(1))
    dat <- data.frame(element = factor(c("Inicjacja", "Detekcja", "Tłumienie"), levels = c("Inicjacja", "Detekcja", "Tłumienie")), gain = gains)
    ggplot(dat, aes(element, gain, fill = element)) +
      geom_col() +
      scale_fill_manual(values = upwr_cat_n(3), guide = "none") +
      labs(title = "Spadek P(top) po poprawie", x = NULL, y = "Redukcja P(top)") +
      theme_upwr()
  })
  zoom_plot_server("f9_rank_plot", rank_plot, alt = "Słupki redukcji prawdopodobieństwa zdarzenia szczytowego po poprawie każdego liścia.")
  output$f9_rank_stats <- renderUI(lc_feedback(type = "info", "Porównanie dotyczy modelu bazowego i jednakowej redukcji względnej."))
  risk_assessment_server("f9", fta_quiz, input, output)
}
