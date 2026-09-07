# Blok 10: Od modelu do decyzji -----------------------------------------

integracja_quiz <- list(questions = list(
  list(question = "Czy roczna awaria choć raz oznacza niedostępność podczas zapotrzebowania?", choices = c("Nie, urządzenie mogło zostać naprawione" = "no", "Tak, to ten sam horyzont" = "yes", "Tak, gdy p jest małe" = "rare"), correct = "no", explanation = "Stan podczas zapotrzebowania i awaria w dowolnym momencie roku to różne zdarzenia."),
  list(question = "Która liczba z karty detekcji trafia do FTA jako przeoczenie?",
    choices = c("P(I | alarm)" = "a", "FPR" = "b", "1−czułość" = "c"), correct = "c",
    explanation = "Przeoczenie to brak alarmu przy zapotrzebowaniu; posterior odpowiada na inne pytanie."),
  list(question = "Zmienia się czas misji. Co należy przeliczyć w karcie systemu?",
    choices = c("Tylko prawdopodobieństwo inicjacji" = "a", "R(t) wszystkich elementów" = "b", "Tylko etykietę czasu" = "c"), correct = "b",
    explanation = "Elementy muszą mieć wspólny czas; tu R zasilania i sterownika przeliczamy modelem wykładniczym."),
  list(question = "Co odróżnia R(3000) od R(1000)³ dla Weibulla?",
    choices = c("Ciągłe starzenie od trzech misji nowych urządzeń" = "a", "Tylko zaokrąglenie" = "b", "Nic, zawsze są równe" = "c"), correct = "a",
    explanation = "Potęgowanie identycznego R zakłada identyczny stan początkowy kolejnych misji, np. odnowę."),
  list(question = "Najlepsze działanie w budżecie przekracza limit w ostrożnym scenariuszu. Co raportujemy?",
    choices = c("Zatwierdzamy, bo jest najlepsze" = "a", "Ukrywamy ostrożny scenariusz" = "b", "Kryterium nie jest spełnione; potrzebny inny projekt lub zakres" = "c"), correct = "c",
    explanation = "Ranking i spełnienie kryterium to odrębne sprawdzenia; potrzebne jest jawne ryzyko po działaniu.")
))
integracja_exercises <- c(
  "Bananpol: dla misji 1000 h, p inicjacji 0,005, czułości 0,95, R zasilania 0,98 i sterownika 0,95 policz P(top) dla obu modeli wentylatora. Wyjaśnij, które parametry są warunkowe.",
  "Czas: porównaj R(3000), R(1000)³ i R(500)⁶ dla Weibulla β=2, η=1700 h. Nazwij różne polityki odnowy; sprawdź, czy podział czasu bez wymiany może zmienić wynik.",
  "Decyzja: przy budżecie 2 i demonstracyjnym limicie P(top)=0,002 porównaj dopuszczalne działania w trzech scenariuszach. Sprawdź wynik po działaniu; jeśli żadne nie spełnia kryterium, nie deklaruj akceptowalności.",
  "Transfer: zbuduj scenariusz dla innego systemu, wskaż człowieka, procedurę, barierę techniczną i możliwy skutek. Oddaj rekomendację z ryzykiem po działaniu, właścicielem i terminem kontroli."
)

integracja_block <- list(id = "integracja", title = "Od modelu do decyzji", chapters = list(
  list(
    id = "teczka", title = "Teczka przypadku",
    lead = "Najpierw opisujemy misję i dostępne dane, potem wybieramy model.",
    intro = c(
      "Bananpol uruchamia partię w komorze. Na początku misji może wystąpić stan wymagający aktywnego chłodzenia przez cały zadany czas. Ochrona wymaga wykrycia tego stanu i ciągłej pracy układu chłodzenia. Analizujemy utratę wymaganej ochrony termicznej; nie utożsamiamy jej automatycznie z pożarem ani urazem.",
      "Wszystkie liczby są fikcyjne. W tym uproszczonym scenariuszu nie ma napraw podczas misji, a każdy element zaczyna sprawny i nowy. Jeden wentylator wystarcza do wymaganej wydajności; oba pracują, a awaria jednego nie zmienia charakterystyki drugiego."
    ),
    callout = list(label = "Dane fikcyjne", text = "Misja bazowa: 1000 h. P zapotrzebowania na początku misji: 0,005; czułość: 0,95; FPR: 0,05. Parametry czasu życia wentylatora pochodzą z bloku 07. Zasilanie i sterownik mają wykładnicze czasy życia z R(1000 h)=0,98 i 0,95.", color = "uwaga"),
    widget = tagList(
      risk_vote_panel("i10_vote", "i10_vote_feedback", "Od czego zacząć analizę przypadku?", c("Od definicji i audytu danych" = "audit", "Od wzoru" = "model", "Od najtańszego działania" = "cost")),
      figure_panel(label = "Audyt", title = "Co sprawdzono?", checkboxGroupInput("i10_fields", "Elementy teczki", c("Definicje zdarzeń" = "event", "Czas i stan początkowy" = "time", "Źródła i warunki pomiaru" = "source", "Niepewność i braki" = "missing")), uiOutput("i10_dossier"), full_width = TRUE)
    )
  ),
  list(
    id = "definicje", title = "Kontrakt zdarzeń",
    lead = "Roczne prawdopodobieństwo, wynik misji i odpowiedź na zapotrzebowanie nie są zamienne.",
    intro = "I oznacza potrzebę chłodzenia na początku misji. D to niewykrycie tej potrzeby, a S — niezdolność układu do utrzymania chłodzenia przez misję. Definiujemy TOP jako I ∩ (D ∪ S). Obie funkcje są wymagane: skuteczna detekcja nie zastępuje chłodzenia, a układ nie zostanie uruchomiony bez sygnału.",
    sections = list(
      list(id = "warunki", title = "Co znaczy każda liczba?", bullets = c("P(I): udział porównywalnych misji z zapotrzebowaniem na początku", "P(D | I)=1−czułość: przeoczenia wśród rzeczywistych zapotrzebowań", "P(S | I)=1−R_sys(t): awaria układu podczas wymaganej pracy", "D i S są niezależne warunkowo przy I; pomiar detekcji ma osobne zasilanie", "Parametry czasu życia dotyczą właśnie pracy pod wymaganym obciążeniem")),
      list(id = "granica", title = "Granica modelu", text = "Awaria naprawiona przed zapotrzebowaniem nie jest niedostępnością podczas zapotrzebowania. Gdy potrzeba chłodzenia pojawia się w losowej chwili, trzeba modelować stan systemu w tej chwili oraz dalszą pracę. Nasza misja zaczyna się od ewentualnego zapotrzebowania i nie obejmuje napraw.")
    ),
    pitfall = "Ujednolicenie etykiety czasu nie naprawia pomieszania zdarzeń. Najpierw nazwij warunki, potem łącz liczby."
  ),
  list(
    id = "mapa", title = "Mapa wyboru modelu",
    lead = "Pytanie wskazuje model i zakres potrzebnych danych.",
    intro = "Prześledź drogę od rejestru do decyzji. Losowość wyników przy znanym p różni się od niewiedzy o p, a obie różnią się od niepewności, czy wybrano właściwą logikę barier.",
    widget = figure_panel(label = "Nawigacja", title = "Wybierz pytanie", selectInput("i10_question", "Pytanie", c("Warunek zmienia ocenę" = "conditional", "Co oznacza alarm" = "bayes", "Ile zdarzeń w n próbach" = "binomial", "Ile prób do pierwszego zdarzenia" = "geometric", "Ile prób do r zdarzeń" = "negative", "Jak często przekraczamy próg" = "threshold", "Czy element dotrwa do czasu t" = "survival", "Czy system spełni funkcję" = "system")), uiOutput("i10_model"), full_width = TRUE)
  ),
  list(
    id = "karta-alarm", title = "Karta 1: detekcja",
    lead = "Posterior służy interpretacji alarmu; do drzewa trafia prawdopodobieństwo przeoczenia.",
    intro = "Detektor ocenia stan na początku misji. Bayes odpowiada, czy za alarmem stoi rzeczywista potrzeba chłodzenia. Do FTA trafi inna liczba z tej samej karty: 1−czułość. Zmiana FPR zmienia wiarygodność alarmu i koszt zbędnych reakcji, lecz w naszym modelu nie zmienia ryzyka przeoczenia. Skutki zbędnego uruchomienia są poza analizowanym TOP.",
    widget = figure_panel(label = "Karta 1", title = "Detektor potrzeby chłodzenia", sliderInput("i10_init", "P(I): zapotrzebowanie na początku misji", .001, .03, bananpol$integration$initiation, .001), sliderInput("i10_sens", "Czułość P(alarm | I)", .5, 1, bananpol$integration$sensitivity, .01), sliderInput("i10_fpr", "FPR P(alarm | brak I)", 0, .2, bananpol$integration$false_positive_rate, .005), uiOutput("i10_alarm_result"), full_width = TRUE)
  ),
  list(
    id = "karta-kontrola", title = "Karta 2: kontrola partii",
    lead = "Zerowy licznik awarii nie dowodzi zerowego prawdopodobieństwa.",
    intro = "Partia zaworów jest osobnym problemem odbiorczym. Dla zadanego p liczymy rozkład liczby wad. Gdy p jest nieznane, wnioskujemy z próby: zero wad w n niezależnych kontrolach daje oszacowanie punktowe zero, ale dodatnią górną granicę ufności. Nie wkładamy wyniku tej karty do drzewa ochrony termicznej, bo nie opisuje jego liścia.",
    formula = "p_{górne}=1-0{,}05^{1/n}\\quad (0\\text{ zdarzeń},\\ 95\\%\\text{ jednostronnie})",
    widget = figure_panel(label = "Karta 2", title = "Partia zaworów i niepewność p", sliderInput("i10_n", "Liczba niezależnych kontroli n", 10, 1000, 100, 10), sliderInput("i10_p", "Zadane p do prognozy liczby wad", .001, .1, .02, .001), uiOutput("i10_inspection_result"), full_width = TRUE),
    pitfall = "Granica ufności opisuje procedurę wnioskowania przy ustalonym n. Nie jest stwierdzeniem, że po obejrzeniu próby stały parametr ma 95% szans leżeć w przedziale."
  ),
  list(
    id = "karta-utrzymanie", title = "Karta 3: czas życia",
    lead = "Wybrany model wentylatora zasila kartę systemu i końcowy wynik.",
    intro = "Wybierz hipotezę o czasie życia wentylatora i długość misji. Model wykładniczy ma MTTF=1500 h; Weibull β=2 i η=1700 h ma MTTF około 1507 h. Ich średnie są zbliżone, ale krzywe różne. Wynik R(t) tej karty trafia bezpośrednio do obu gałęzi systemu.",
    sections = list(list(id = "odnowa", title = "Podział czasu nie odmładza", text = "Dla jednego urządzenia bez wymiany przetrwanie 3000 h to R(3000). Trzy misje nowych urządzeń po 1000 h dają R(1000)³; sześć po 500 h — R(500)⁶. To różne polityki odnowy. Przy kontynuacji pracy kolejne prawdopodobieństwa są warunkowe: R(2000)/R(1000), a nie ponownie R(1000).")),
    widget = risk_widget_panel("Karta 3", "Niezawodność wentylatora", tagList(selectInput("i10_life_model", "Model", c("Wykładniczy" = "exp", "Weibull — zużycie" = "weibull")), sliderInput("i10_time", "Czas misji (h)", 100, 3000, 1000, 50)), "i10_life_plot", "i10_life_stats")
  ),
  list(
    id = "system", title = "Część B — układ chłodzenia",
    lead = "Zasilanie i sterownik są wymagane zawsze; wystarcza jeden z dwóch pracujących wentylatorów.",
    intro = "Wentylatory mają R(t) z poprzedniej karty. Dla zasilania i sterownika zakładamy wykładniczy czas życia, więc R(t)=R(1000)^(t/1000). Wszystkie elementy liczymy dla wspólnego czasu misji. Zasilanie jest jawnym wspólnym zasobem obu wentylatorów; poza nim zakładamy niezależność elementów.",
    formula = "R_{sys}(t)=R_P(t)R_C(t)[1-(1-R_A(t))(1-R_B(t))]",
    widget = figure_panel(label = "Redukcja", title = "Elementy w tej samej misji", sliderInput("i10_power", "R zasilania na 1000 h", .7, 1, bananpol$integration$power_r1000, .01), sliderInput("i10_controller", "R sterownika na 1000 h", .7, 1, bananpol$integration$controller_r1000, .01), uiOutput("i10_system_result"), full_width = TRUE)
  ),
  list(
    id = "fta", title = "Końcowe FTA",
    lead = "Inicjacja i niepowodzenie wymaganej ochrony tworzą wspólny scenariusz.",
    intro = "TOP wystąpi, gdy jest zapotrzebowanie i zabraknie detekcji lub ciągłego chłodzenia. Iloczyn P(I) i prawdopodobieństwa warunkowego nie wymaga niezależności od I. Dopełnienie iloczynu wewnątrz nawiasu wymaga natomiast niezależności D i S przy ustalonym I. Zależność detektora od wspólnego zasilania wymagałaby przebudowy drzewa.",
    formula = "P(TOP)=P(I)\\,[1-(1-P(D\\mid I))(1-P(S\\mid I))]",
    widget = risk_widget_panel("Integracja", "Utrata ochrony termicznej w misji", tags$p("Parametry zmieniasz na kartach detekcji, czasu życia i systemu."), "i10_fta_plot", "i10_fta_stats"),
    pitfall = "P(TOP) jest prawdopodobieństwem utraty wymaganej ochrony. Do prawdopodobieństwa szkody materialnej lub urazu potrzebny jest dalszy model skutków."
  ),
  list(
    id = "interwencje", title = "Cztery interwencje",
    lead = "Porównujemy efekt, budżet i wykonalność przy tej samej misji.",
    intro = "Lepszy czujnik zmniejsza przeoczenia, ograniczenie źródła ciepła zmniejsza częstość zapotrzebowania, niezależne zasilanie dodaje drugą gałąź zasilania, a dodatkowy wentylator trzecią gałąź chłodzenia. Bazowa redukcja przeoczeń lub inicjacji o 50% jest fikcyjną hipotezą skuteczności działania, wymagającą danych z pilotażu. Nie wynika z samego częstszego przeglądu.",
    widget = risk_widget_panel("Opcje", "Ryzyko po działaniu", tagList(selectInput("i10_intervention", "Interwencja", setNames(bananpol$interventions$id, bananpol$interventions$label)), sliderInput("i10_budget", "Budżet w jednostkach demonstracyjnych", 1, 4, 2, 1)), "i10_interventions_plot", "i10_intervention_stats"),
    decision = "Budżet ogranicza zbiór opcji; pozostałe ryzyko porównaj z jawnym kryterium. Koszty są umownymi jednostkami, nie cenami rynkowymi."
  ),
  list(
    id = "scenariusze", title = "Odporność rekomendacji",
    lead = "Każdą interwencję przeliczamy w każdym scenariuszu.",
    intro = "Mnożnik m=1±u skaluje P(I), prawdopodobieństwo przeoczenia oraz skumulowane hazardy elementów; prawdopodobieństwa ograniczamy do 1. Skuteczność czujnika i ograniczenia źródła ciepła wynosi 0,5(2−m): w ostrożnym scenariuszu jest niższa. Te same założenia stosujemy do wszystkich opcji przed ich porównaniem. Redundancja zakłada niezależność dodanej gałęzi także w scenariuszach.",
    widget = risk_widget_panel("Niepewność", "Opcje w trzech scenariuszach", sliderInput("i10_uncertainty", "Zakres u", 0, .5, .2, .05), "i10_scenarios", "i10_scenarios_stats"),
    takeaway = "Scenariusze są jawnym eksperymentem na założeniach, a nie przedziałem ufności ani dowodem odporności na wszystkie możliwe błędy modelu."
  ),
  list(
    id = "notatka", title = "Rekomendacja i ryzyko po działaniu",
    lead = "Mniejsze prawdopodobieństwo nie musi spełniać przyjętego kryterium.",
    intro = "Wybierz działanie i demonstracyjny limit dla utraty ochrony w jednej misji. Notatka sprawdza budżet i najgorszy z rozpatrywanych scenariuszy. Limit służy wyłącznie ćwiczeniu, nie jest normą bezpieczeństwa. Uzgodnienie rzeczywistego kryterium wymaga także oceny skutków i narażenia.",
    widget = figure_panel(label = "Notatka", title = "Wynik, działanie, kryterium i kontrola", selectInput("i10_recommend", "Rozważane działanie", setNames(bananpol$interventions$id, bananpol$interventions$label)), sliderInput("i10_target", "Demonstracyjny limit P(TOP) na misję", .0001, .01, .002, .0001), uiOutput("i10_memo"), full_width = TRUE),
    decision = "Jeżeli żadna dopuszczalna opcja nie spełnia kryterium, wróć do zakresu misji, budżetu lub projektu barier. Nie zmieniaj kryterium tylko po to, by zatwierdzić wynik."
  ),
  list(
    id = "audyt", title = "Quiz i obrona rekomendacji",
    lead = "Oddaj rachunek wraz z założeniami, skutkami i planem sprawdzenia działania.",
    intro = "Zespół objaśnia każdy liść i każdą bramkę. Recenzent pyta o brakujący scenariusz, błąd człowieka, zależność i źródło skuteczności działania. Na koniec trzeba nazwać ryzyko pozostałe po interwencji oraz to, czego model nie obejmuje.",
    sections = list(list(id = "sciaga", title = "Ściąga", bullets = c("Definicje i warunki poprzedzają rachunek", "Model czasu życia zasila system dla tego samego t", "Nie utożsamiamy posterioru z przeoczeniem ani awaryjności z niedostępnością", "Porównujemy opcje w każdym scenariuszu i w budżecie", "Notatka zawiera kryterium, ryzyko po działaniu, właściciela i termin przeglądu"))),
    widget = risk_assessment_ui("i10", integracja_quiz, integracja_exercises)
  )
))
integracja_chapters <- risk_block_chapters(integracja_block)

integracja_server <- function(input, output, session) {
  vote <- reactiveVal(FALSE)
  observeEvent(input$i10_vote_check, vote(TRUE))
  output$i10_vote_feedback <- renderUI({
    req(vote())
    lc_feedback(type = if (identical(input$i10_vote, "audit")) "ok" else "warning", "Najpierw definicje, czas, warunki i źródła danych.")
  })
  output$i10_dossier <- renderUI(lc_feedback(type = "info", paste(length(input$i10_fields), "z 4 pól oznaczono jako sprawdzone. Samo zaznaczenie nie zastępuje uzasadnienia.")))
  output$i10_model <- renderUI({
    models <- c(conditional = "Warunkowe i całkowite", bayes = "Bayes", binomial = "Dwumianowy", geometric = "Geometryczny", negative = "Ujemny dwumianowy", threshold = "Rozkład ciągły i ogon", survival = "Funkcje czasu życia", system = "Funkcja struktury i FTA")
    lc_feedback(type = "info", models[[input$i10_question]])
  })
  output$i10_alarm_result <- renderUI({
    p <- risk_bayes(input$i10_init, input$i10_sens, input$i10_fpr)
    lc_stat_grid(lc_stat_box("P(I | alarm)", risk_format_probability(p)), lc_stat_box("P(D | I) — wejście do FTA", risk_format_probability(1 - input$i10_sens)), columns = 1)
  })
  output$i10_inspection_result <- renderUI(lc_stat_grid(
    lc_stat_box("E(X) przy zadanym p", round(input$i10_n * input$i10_p, 2)),
    lc_stat_box("P(co najmniej jednej wady)", risk_format_probability(risk_at_least_one(input$i10_n, input$i10_p))),
    lc_stat_box("Jeśli zaobserwowano zero: górna granica 95% dla p", risk_format_probability(risk_zero_failure_upper(input$i10_n))), columns = 1
  ))
  evaluate <- function(id = "none", stress = 1) risk_mission_analysis(
    input$i10_time, input$i10_life_model, input$i10_power,
    input$i10_controller, input$i10_init, input$i10_sens, id, stress
  )
  base <- reactive(evaluate())
  life_r <- reactive(base()$fan_r)
  sys_r <- reactive(base()$system_r)
  top_p <- reactive(base()$top)
  life_plot <- reactive({
    t <- seq(0, 3000, length.out = 400)
    r <- if (input$i10_life_model == "exp") exp(-t / 1500) else exp(-(t / 1700)^2)
    ggplot(data.frame(t, r), aes(t, r)) + geom_line(colour = upwr_accent, linewidth = 1) +
      geom_vline(xintercept = input$i10_time, linetype = 2) +
      labs(title = "Niezawodność pojedynczego wentylatora", x = "Czas (h)", y = "R(t)") + theme_upwr()
  })
  zoom_plot_server("i10_life_plot", life_plot, alt = "Krzywa wybranego modelu czasu życia wentylatora z zaznaczonym czasem misji.")
  output$i10_life_stats <- renderUI(lc_stat_grid(lc_stat_box("R(t) przekazane do systemu", risk_format_probability(life_r())), columns = 1))
  output$i10_system_result <- renderUI({
    b <- base()
    lc_stat_grid(lc_stat_box("Czas misji", paste(input$i10_time, "h")), lc_stat_box("R zasilania w misji", risk_format_probability(b$power_r)), lc_stat_box("R sterownika w misji", risk_format_probability(b$controller_r)), lc_stat_box("R gałęzi równoległych", risk_format_probability(b$parallel_r)), lc_stat_box("R systemu | I", risk_format_probability(sys_r())), columns = 1)
  })
  fta_plot <- reactive({
    b <- base()
    d <- data.frame(node = c("P(I)", "P(D | I)", "P(S | I)", "P(TOP)"), p = c(b$initiation, b$miss, b$cooling_failure, b$top))
    ggplot(d, aes(node, p, fill = node)) + geom_col() + scale_fill_manual(values = upwr_cat_n(4), guide = "none") + labs(title = "Zdarzenie inicjujące i warunkowe niepowodzenia", x = NULL, y = "Prawdopodobieństwo") + theme_upwr()
  })
  zoom_plot_server("i10_fta_plot", fta_plot, alt = "Prawdopodobieństwo inicjacji, warunkowe prawdopodobieństwa niepowodzeń oraz wynik na jedną misję.")
  output$i10_fta_stats <- renderUI(lc_stat_grid(lc_stat_box("P(TOP) na misję", risk_format_probability(top_p())), lc_stat_box("Na 10 000 porównywalnych misji", risk_natural_frequency(top_p(), 10000)), columns = 1))
  intervention_top <- function(id, stress = 1) evaluate(id, stress)$top
  interventions_plot <- reactive({
    d <- bananpol$interventions
    d$result <- vapply(d$id, intervention_top, numeric(1))
    d$budget <- ifelse(d$cost_index <= input$i10_budget, "W budżecie", "Poza budżetem")
    ggplot(d, aes(reorder(label, result), result, fill = budget)) + geom_col() + coord_flip() + scale_fill_manual(values = upwr_cat_n(length(unique(d$budget)))) + labs(title = "Wynik po interwencji", x = NULL, y = "P(TOP) na misję", fill = NULL) + theme_upwr()
  })
  zoom_plot_server("i10_interventions_plot", interventions_plot, alt = "Porównanie ryzyka po czterech działaniach z oznaczeniem dostępności w budżecie.")
  output$i10_intervention_stats <- renderUI({
    d <- bananpol$interventions[bananpol$interventions$id == input$i10_intervention, ]
    lc_stat_grid(lc_stat_box("P(TOP) po zmianie", risk_format_probability(intervention_top(d$id))), lc_stat_box("Koszt umowny", d$cost_index), lc_stat_box("Wykonalność", d$feasibility), columns = 1)
  })
  scenario_results <- reactive({
    scenarios <- c("Optymistyczny", "Bazowy", "Ostrożny")
    multipliers <- c(1 - input$i10_uncertainty, 1, 1 + input$i10_uncertainty)
    do.call(rbind, lapply(seq_along(scenarios), function(i) {
      d <- bananpol$interventions
      d$scenario <- scenarios[i]
      d$result <- vapply(d$id, intervention_top, numeric(1), stress = multipliers[i])
      d
    }))
  })
  scenarios_plot <- reactive({
    d <- scenario_results()
    d$scenario <- factor(d$scenario, levels = c("Optymistyczny", "Bazowy", "Ostrożny"))
    ggplot(d, aes(label, result, fill = scenario)) + geom_col(position = "dodge") + coord_flip() + scale_fill_manual(values = upwr_cat_n(3)) + labs(title = "Każda opcja w każdym scenariuszu", x = NULL, y = "P(TOP) po działaniu", fill = "Scenariusz") + theme_upwr()
  })
  zoom_plot_server("i10_scenarios", scenarios_plot, alt = "Trzy scenariusze ryzyka po każdej interwencji.")
  output$i10_scenarios_stats <- renderUI({
    d <- scenario_results()
    d <- d[d$cost_index <= input$i10_budget, ]
    winners <- lapply(split(d, d$scenario), function(x) {
      best <- x$label[abs(x$result - min(x$result)) < 1e-12]
      paste(best, collapse = " / ")
    })
    lc_feedback(type = "info", paste(paste(names(winners), unlist(winners), sep = ": "), collapse = "; "), ". Ranking minimalnego P(TOP) w budżecie; równe wyniki pokazano razem. To nie jest przedział ufności.")
  })
  output$i10_memo <- renderUI({
    d <- bananpol$interventions[bananpol$interventions$id == input$i10_recommend, ]
    results <- scenario_results()
    worst <- max(results$result[results$id == d$id])
    affordable <- d$cost_index <= input$i10_budget
    meets <- worst <= input$i10_target
    tags$ol(
      tags$li(paste0("Przy misji ", input$i10_time, " h bazowe P utraty ochrony wynosi ", risk_format_probability(top_p()), "; to ", risk_natural_frequency(top_p(), 10000), " porównywalnych misji.")),
      tags$li(paste0("Rozważamy: ", d$label, "; koszt ", d$cost_index, ", ", if (affordable) "w budżecie" else "poza budżetem", "; P(TOP) po działaniu bazowo ", risk_format_probability(intervention_top(d$id)), ", w najgorszym rozpatrzonym scenariuszu ", risk_format_probability(worst), ".")),
      tags$li(paste0("Demonstracyjny limit ", risk_format_probability(input$i10_target), if (meets) " jest spełniony w badanych scenariuszach" else " nie jest spełniony", "; ", if (affordable && meets) "wariant można przekazać do oceny skutków i wdrożenia" else "potrzebna jest inna opcja, projekt lub budżet", ". Założenia: brak napraw, niezależność detekcji od chłodzenia i skuteczność działań zgodna ze scenariuszem.")),
      tags$li("Właściciel proponowanej kontroli: kierownik utrzymania; weryfikacja detekcji, awarii i skuteczności działania po pierwszej misji oraz po każdej zmianie instalacji. Oddzielnie oceniamy skutki utraty ochrony i scenariusze pominięte w modelu.")
    )
  })
  risk_assessment_server("i10", integracja_quiz, input, output)
}
