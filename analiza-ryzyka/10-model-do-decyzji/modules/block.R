# Blok 10: Od modelu do decyzji -----------------------------------------

integracja_quiz <- list(question = "Który element powinien znaleźć się w czterozdaniowej rekomendacji?", choices = c("Wynik, naturalna częstość, kluczowe założenie i rekomendacja" = "four", "Tylko najdokładniejszy wynik liczbowy" = "number", "Automatycznie wybrana najtańsza interwencja" = "cheap"), correct = "four", explanation = "Decydent potrzebuje skali wyniku, kontekstu, ograniczenia i proponowanego działania.")
integracja_exercises <- c("Bananpol: przeprowadź bazowy rachunek alarmu, kontroli i niezawodności dla parametrów z teczki.", "Diagnostyka: wskaż trzy miejsca, w których założenie niezależności może zawieść w złożonym modelu.", "Transfer zespołowy: przygotuj czterozdaniową rekomendację dla wybranego systemu bezpieczeństwa i obroń ją w krótkiej prezentacji.")

integracja_block <- list(id = "integracja", title = "Od modelu do decyzji", chapters = list(
  list(id = "teczka", title = "Teczka przypadku", lead = "Analiza zaczyna się od danych, jednostek, okresów i jawnych braków — nie od wyboru wzoru.", widget = tagList(
    risk_vote_panel("i10_vote", "i10_vote_feedback", "Od czego zacząć analizę przypadku?", c("Od definicji i audytu danych" = "audit", "Od najbardziej zaawansowanego modelu" = "model", "Od wyboru najtańszej interwencji" = "cost")),
    figure_panel(label = "Audyt danych", title = "Co wiemy o Bananpolu?", checkboxGroupInput("i10_fields", "Elementy sprawdzone", c("Jednostki" = "unit", "Horyzonty" = "horizon", "Źródła" = "source", "Braki i niepewność" = "missing")), uiOutput("i10_dossier"), full_width = TRUE)
  )),
  list(id = "definicje", title = "Zdarzenie, ekspozycja, skutek i misja", lead = "Cztery definicje utrzymują spójne mianowniki między kartami analizy.", sections = list(list(id = "contract", title = "Kontrakt przypadku", bullets = c("zdarzenie: co dokładnie uznajemy za awarię", "ekspozycja: czego dotyczy jedna możliwość zdarzenia", "skutek: który niepożądany stan oceniamy", "czas misji: wspólny horyzont elementów i systemu"))), pitfall = "Łączenie prawdopodobieństwa na zmianę z niezawodnością na 1000 godzin bez konwersji jest błędem jednostek."),
  list(id = "mapa", title = "Mapa wyboru modelu", lead = "Forma pytania prowadzi do rodziny modelu.", widget = figure_panel(label = "Nawigacja", title = "Wybierz pytanie", selectInput("i10_question", "Pytanie", c("Warunek zmienia ocenę" = "conditional", "Co oznacza alarm" = "bayes", "Ile zdarzeń w n próbach" = "binomial", "Ile prób do r zdarzeń" = "negative", "Czy element dotrwa do czasu t" = "survival", "Czy system zadziała" = "system")), uiOutput("i10_model"), full_width = TRUE)),
  list(id = "alarm", title = "Karta alarmu", lead = "Bayes zamienia parametry detektora i częstość bazową na wiarygodność alarmu.", widget = figure_panel(label = "Karta 1", title = "Detektor przegrzania", sliderInput("i10_prev", "P(awarii)", .001, .1, .01, .001), sliderInput("i10_sens", "Czułość", .5, 1, .95, .01), sliderInput("i10_fpr", "FPR", 0, .2, .05, .005), uiOutput("i10_alarm_result"), full_width = TRUE)),
  list(id = "kontrola", title = "Karta kontroli", lead = "Model dwumianowy opisuje liczbę niesprawnych zaworów w ustalonej próbie.", widget = figure_panel(label = "Karta 2", title = "Partia zaworów", sliderInput("i10_n", "Liczba kontroli", 10, 300, 100, 10), sliderInput("i10_p", "p niesprawności", .001, .1, .02, .001), uiOutput("i10_inspection_result"), full_width = TRUE)),
  list(id = "utrzymanie", title = "Karta utrzymania", lead = "Wybór wykładniczy/Weibull wynika z hipotezy o hazardzie.", widget = risk_widget_panel("Karta 3", "Niezawodność wentylatora", tagList(selectInput("i10_life_model", "Model", c("Wykładniczy — stały hazard" = "exp", "Weibull — zużycie" = "weibull")), sliderInput("i10_time", "Czas misji (h)", 100, 3000, 1000, 50)), "i10_life_plot", "i10_life_stats")),
  list(id = "system", title = "Część B — układ zabezpieczeń", lead = "Łączymy sterownik szeregowo z dwoma wentylatorami równoległymi i jawnym zasilaniem.", formula = "R_{sys}=R_P R_C[1-(1-R_A)(1-R_B)]", widget = figure_panel(label = "Redukcja", title = "System Bananpolu", sliderInput("i10_power", "R zasilania", .7, 1, .98, .01), sliderInput("i10_controller", "R sterownika", .7, 1, .95, .01), sliderInput("i10_fan", "R wentylatora", .7, 1, .92, .01), uiOutput("i10_system_result"), full_width = TRUE)),
  list(id = "fta", title = "Końcowe FTA", lead = "Parametry z wcześniejszych kart stają się liśćmi jawnej logiki zdarzenia szczytowego.", widget = risk_widget_panel("Integracja", "Inicjacja AND utrata zabezpieczeń", sliderInput("i10_init", "P(inicjacji)", 0, .03, .005, .001), "i10_fta_plot", "i10_fta_stats"), pitfall = "Nie mnożymy kart tylko dlatego, że wszystkie są dostępne; połączenie musi wynikać z logiki systemu."),
  list(id = "interwencje", title = "Porównanie czterech interwencji", lead = "Każda opcja zmienia wynik, koszt i wykonalność — aplikacja nie wybiera za użytkownika.", widget = risk_widget_panel("Opcje", "Efekt względem modelu bazowego", selectInput("i10_intervention", "Interwencja", setNames(bananpol$interventions$id, bananpol$interventions$label)), "i10_interventions_plot", "i10_intervention_stats"), decision = "Traktuj ranking liczbowy jako wejście do decyzji wielokryterialnej."),
  list(id = "scenariusze", title = "Analiza scenariuszy", lead = "Wynik bazowy, optymistyczny i ostrożny pokazuje konsekwencje niepewności parametrów.", widget = risk_widget_panel("Niepewność", "Trzy jawne scenariusze", sliderInput("i10_uncertainty", "Zakres zmiany parametrów", 0, .5, .2, .05), "i10_scenarios", "i10_scenarios_stats")),
  list(id = "notatka", title = "Czterozdaniowa notatka", lead = "Krótka rekomendacja łączy wynik z warunkami jego ważności.", widget = figure_panel(label = "Generator", title = "Wynik → częstość → założenie → rekomendacja", selectInput("i10_recommend", "Rekomendowane działanie", setNames(bananpol$interventions$id, bananpol$interventions$label)), uiOutput("i10_memo"), full_width = TRUE), decision = "Człowiek zatwierdza rekomendację po ocenie wykonalności i skutków ubocznych."),
  list(id = "audyt", title = "Quiz audytowy i ćwiczenie zespołowe", lead = "Zespół kończy analizę krótką prezentacją decyzji oraz jej ograniczeń.", sections = list(list(id = "sciaga", title = "Ściąga końcowa", bullets = c("Pytanie i jednostka poprzedzają model", "Każde p ma horyzont i źródło", "Niezależność jest jawna", "Wynik tłumaczymy na naturalną częstość", "Rekomendacja zawiera kluczowe założenie"))), widget = risk_assessment_ui("i10", integracja_quiz, integracja_exercises), duration = "20–25 min")
))
integracja_chapters <- risk_block_chapters(integracja_block)

integracja_server <- function(input, output, session) {
  vote <- reactiveVal(FALSE)
  observeEvent(input$i10_vote_check, vote(TRUE))
  output$i10_vote_feedback <- renderUI({
    req(vote())
    lc_feedback(type = if (identical(input$i10_vote, "audit")) "ok" else "warning", tags$strong("Najpierw kontrakt analizy:"), " definicje, jednostki, horyzonty, źródła i braki.")
  })
  output$i10_dossier <- renderUI({
    done <- length(input$i10_fields)
    lc_feedback(type = if (done == 4) "ok" else "warning", tags$strong(paste(done, "z 4 pól sprawdzonych.")), if (done == 4) " Teczka ma minimalny kontrakt danych." else " Uzupełnij metadane przed rachunkiem.")
  })
  output$i10_model <- renderUI({
    models <- c(conditional = "Prawdopodobieństwo warunkowe i całkowite", bayes = "Bayes / naturalne częstości", binomial = "Rozkład dwumianowy", negative = "Rozkład ujemny dwumianowy", survival = "R(t) i h(t)", system = "Logika szeregowa/równoległa i FTA")
    lc_feedback(type = "info", tags$strong("Model:"), paste0(" ", models[[input$i10_question]], "."))
  })
  output$i10_alarm_result <- renderUI({
    p <- risk_bayes(input$i10_prev, input$i10_sens, input$i10_fpr)
    lc_stat_grid(lc_stat_box("P(awaria | alarm)", risk_format_probability(p), color = upwr_accent), lc_stat_box("Naturalna częstość alarmów prawdziwych", risk_natural_frequency(p)), columns = 1)
  })
  output$i10_inspection_result <- renderUI({
    pone <- risk_at_least_one(input$i10_n, input$i10_p)
    lc_stat_grid(lc_stat_box("E(X)", round(input$i10_n * input$i10_p, 2)), lc_stat_box("P(co najmniej jednej)", risk_format_probability(pone), color = upwr_accent), columns = 1)
  })
  life_r <- reactive(if (input$i10_life_model == "exp") exp(-input$i10_time / 1500) else risk_weibull(input$i10_time, 2, 1700)$reliability)
  life_plot <- reactive({
    t <- seq(0, 3000, length.out = 400)
    r <- if (input$i10_life_model == "exp") exp(-t / 1500) else exp(-(t / 1700)^2)
    ggplot(data.frame(t, r), aes(t, r)) +
      geom_line(colour = upwr_accent, linewidth = 1.1) +
      geom_vline(xintercept = input$i10_time, linetype = 2) +
      labs(title = "Karta czasu życia", x = "Czas (h)", y = "R(t)") +
      theme_upwr()
  })
  zoom_plot_server("i10_life_plot", life_plot, alt = "Krzywa niezawodności wentylatora z linią czasu misji.")
  output$i10_life_stats <- renderUI(lc_stat_grid(lc_stat_box("R(t)", risk_format_probability(life_r()), color = upwr_accent), columns = 1))
  sys_r <- reactive(input$i10_power * input$i10_controller * risk_parallel_reliability(c(input$i10_fan, input$i10_fan)))
  output$i10_system_result <- renderUI(lc_stat_grid(lc_stat_box("R części równoległej", risk_format_probability(risk_parallel_reliability(c(input$i10_fan, input$i10_fan)))), lc_stat_box("R systemu", risk_format_probability(sys_r()), color = upwr_accent), columns = 1))
  top_p <- reactive(risk_fta_top(input$i10_init, 1 - input$i10_sens, 1 - sys_r()))
  fta_plot <- reactive({
    vals <- c(`Inicjacja` = input$i10_init, `Nieskuteczna informacja` = 1 - risk_bayes(input$i10_prev, input$i10_sens, input$i10_fpr), `Utrata funkcji systemu` = 1 - sys_r(), `Top event` = top_p())
    ggplot(data.frame(node = factor(names(vals), levels = names(vals)), p = vals), aes(node, p, fill = node)) +
      geom_col() +
      scale_fill_manual(values = upwr_cat_n(4), guide = "none") +
      labs(title = "Parametry kart w logice FTA", x = NULL, y = "Prawdopodobieństwo") +
      theme_upwr() +
      theme(axis.text.x = element_text(angle = 18, hjust = 1))
  })
  zoom_plot_server("i10_fta_plot", fta_plot, alt = "Słupki parametrów wejściowych oraz końcowego zdarzenia szczytowego.")
  output$i10_fta_stats <- renderUI(lc_stat_grid(lc_stat_box("P(top)", risk_format_probability(top_p()), color = upwr_accent), lc_stat_box("Naturalna częstość", risk_natural_frequency(top_p(), 10000)), columns = 1))
  interventions_plot <- reactive({
    d <- bananpol$interventions
    d$result <- top_p() * (1 - d$relative_reduction)
    ggplot(d, aes(reorder(label, result), result, fill = feasibility)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = upwr_cat_n(length(unique(d$feasibility)))) +
      labs(title = "Wynik po interwencji", x = NULL, y = "P(top)", fill = "Wykonalność") +
      theme_upwr()
  })
  zoom_plot_server("i10_interventions_plot", interventions_plot, alt = "Poziome słupki prawdopodobieństwa zdarzenia szczytowego po czterech interwencjach.")
  output$i10_intervention_stats <- renderUI({
    d <- bananpol$interventions[bananpol$interventions$id == input$i10_intervention, ]
    lc_stat_grid(lc_stat_box("P(top) po zmianie", risk_format_probability(top_p() * (1 - d$relative_reduction)), color = upwr_accent), lc_stat_box("Koszt demonstracyjny", d$cost_index), lc_stat_box("Wykonalność", d$feasibility), columns = 1)
  })
  scenarios_plot <- reactive({
    u <- input$i10_uncertainty
    base <- top_p()
    dat <- data.frame(scenario = factor(c("Optymistyczny", "Bazowy", "Ostrożny"), levels = c("Optymistyczny", "Bazowy", "Ostrożny")), p = pmin(1, c(base * (1 - u), base, base * (1 + u))))
    ggplot(dat, aes(scenario, p, fill = scenario)) +
      geom_col() +
      scale_fill_manual(values = upwr_cat_n(3), guide = "none") +
      labs(title = "Jawny przedział scenariuszy", x = NULL, y = "P(top)") +
      theme_upwr()
  })
  zoom_plot_server("i10_scenarios", scenarios_plot, alt = "Trzy słupki scenariusza optymistycznego, bazowego i ostrożnego.")
  output$i10_scenarios_stats <- renderUI(lc_feedback(type = "info", "Scenariusze nie są przedziałem ufności; pokazują konsekwencje jawnych zmian założeń."))
  output$i10_memo <- renderUI({
    d <- bananpol$interventions[bananpol$interventions$id == input$i10_recommend, ]
    after <- top_p() * (1 - d$relative_reduction)
    tags$ol(tags$li(paste0("W modelu bazowym P(top) = ", risk_format_probability(top_p()), ".")), tags$li(paste0("To odpowiada ", risk_natural_frequency(top_p(), 10000), " porównywalnych okresów.")), tags$li("Kluczowe założenie: wejścia OR i karty systemu są niezależne poza jawnymi wspólnymi przyczynami."), tags$li(paste0("Rekomendacja do oceny: ", d$label, "; wynik po zmianie ", risk_format_probability(after), ", koszt ", d$cost_index, ", wykonalność ", d$feasibility, ".")))
  })
  risk_assessment_server("i10", integracja_quiz, input, output)
}
