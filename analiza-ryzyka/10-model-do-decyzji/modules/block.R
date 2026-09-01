# Blok 10: Od modelu do decyzji -----------------------------------------

integracja_quiz <- list(question = "Który element powinien znaleźć się w czterozdaniowej rekomendacji?", choices = c("Wynik, naturalna częstość, kluczowe założenie i rekomendacja" = "four", "Tylko najdokładniejszy wynik liczbowy" = "number", "Automatycznie wybrana najtańsza interwencja" = "cheap"), correct = "four", explanation = "Decydent potrzebuje skali wyniku, kontekstu, ograniczenia i proponowanego działania.")
integracja_exercises <- c("Bananpol: przeprowadź bazowy rachunek alarmu, kontroli i niezawodności dla parametrów z teczki.", "Audyt jednostek: aplikacja przelicza zawodność systemu na horyzont roczny jako 1−R_sys³ dla trzech misji po 1000 h. Policz P(top) dla wariantu z sześcioma krótszymi misjami oraz dla błędnego wariantu bez konwersji (1−R_sys) i porównaj wszystkie trzy wyniki.", "Diagnostyka: wskaż trzy miejsca, w których założenie niezależności może zawieść w złożonym modelu.", "Transfer zespołowy: przygotuj czterozdaniową rekomendację dla wybranego systemu bezpieczeństwa i obroń ją w krótkiej prezentacji.")

integracja_block <- list(id = "integracja", title = "Od modelu do decyzji", chapters = list(
  list(
    id = "teczka", title = "Teczka przypadku",
    lead = "Analiza zaczyna się od danych, jednostek, okresów i jawnych braków — nie od wyboru wzoru.",
    intro = c(
      "Po serii analiz na biurku inspektora leży kompletna teczka Bananpolu: rejestry alarmów z chłodni, protokoły kontroli zaworów i historia napraw wentylatorów. Zarząd oczekuje jednej odpowiedzi: którą barierę poprawić najpierw. Zanim padnie jakikolwiek wzór, trzeba sprawdzić, czy te dane w ogóle nadają się do rachunku.",
      "Ten wykład niczego nowego nie wprowadza — i to jest jego treść. Wszystkie narzędzia już masz; zadaniem jest złożenie ich w jedną, uczciwą rekomendację. Największe błędy analiz integracyjnych nie powstają w rachunkach, lecz w szwach między nimi: w niezgodnych jednostkach, pomieszanych horyzontach i cichych założeniach."
    ),
    callout = list(
      label = "Dane fikcyjne",
      text = "Cała teczka — parametry detektora, partii zaworów, wentylatorów i drzewa pożaru — pochodzi ze wspólnego, jawnie fikcyjnego rejestru parametrów Bananpolu. Każda liczba ma tam jednostkę, horyzont i źródło.",
      color = "uwaga"
    ),
    widget = tagList(
      risk_vote_panel("i10_vote", "i10_vote_feedback", "Od czego zacząć analizę przypadku?", c("Od definicji i audytu danych" = "audit", "Od najbardziej zaawansowanego modelu" = "model", "Od wyboru najtańszej interwencji" = "cost")),
      figure_panel(label = "Audyt danych", title = "Co wiemy o Bananpolu?", checkboxGroupInput("i10_fields", "Elementy sprawdzone", c("Jednostki" = "unit", "Horyzonty" = "horizon", "Źródła" = "source", "Braki i niepewność" = "missing")), uiOutput("i10_dossier"), full_width = TRUE)
    )
  ),
  list(
    id = "definicje", title = "Zdarzenie, ekspozycja, skutek i misja",
    lead = "Cztery definicje utrzymują spójne mianowniki między kartami analizy.",
    intro = "W pojedynczym wykładzie definicje pilnowały jednego rachunku. W studium integracyjnym pilnują czegoś więcej: zgodności między rachunkami. Prawdopodobieństwo alarmu liczone na zmianę, niezawodność na tysiąc godzin i drzewo błędów na rok pracy magazynu spotkają się w jednym modelu — i spotkanie skończy się katastrofą jednostek, jeśli kontrakt przypadku nie zostanie spisany na początku.",
    sections = list(list(id = "contract", title = "Kontrakt przypadku", bullets = c("zdarzenie: co dokładnie uznajemy za awarię", "ekspozycja: czego dotyczy jedna możliwość zdarzenia", "skutek: który niepożądany stan oceniamy", "czas misji: wspólny horyzont elementów i systemu"))),
    pitfall = "Łączenie prawdopodobieństwa na zmianę z niezawodnością na 1000 godzin bez konwersji jest błędem jednostek."
  ),
  list(
    id = "mapa", title = "Mapa wyboru modelu",
    lead = "Forma pytania prowadzi do rodziny modelu.",
    intro = c(
      "Przez dziewięć wykładów każde pytanie dostawało własny model: warunek, alarm, licznik zdarzeń, czas oczekiwania, czas życia, system. Teraz odwracamy nawyk: zaczynamy od pytania i dopiero ono wskazuje model — nigdy odwrotnie, od ulubionego modelu, do którego dopasowuje się dane.",
      "Nawigator poniżej to cały kurs w jednej liście rozwijanej. Warto przećwiczyć go w obie strony: od pytania do modelu, ale też od danych, które masz, do pytań, na które te dane naprawdę potrafią odpowiedzieć."
    ),
    widget = figure_panel(label = "Nawigacja", title = "Wybierz pytanie", selectInput("i10_question", "Pytanie", c("Warunek zmienia ocenę" = "conditional", "Co oznacza alarm" = "bayes", "Ile zdarzeń w n próbach" = "binomial", "Ile prób do pierwszego zdarzenia" = "geometric", "Ile prób do r zdarzeń" = "negative", "Jak często przekraczamy próg" = "threshold", "Czy element dotrwa do czasu t" = "survival", "Czy system zadziała" = "system")), uiOutput("i10_model"), full_width = TRUE)
  ),
  list(
    id = "karta-alarm", title = "Karta 1: alarm",
    lead = "Bayes zamienia parametry detektora i częstość bazową na wiarygodność alarmu.",
    intro = c(
      "Zamiast liczyć wszystko naraz, rozbijamy teczkę na trzy karty obliczeniowe. Każda karta ma własne pytanie, własny model z wcześniejszych wykładów i własny wynik z jednostką — dopiero komplet kart pozwoli sensownie rozmawiać o integracji.",
      "Karta pierwsza wraca do wykładu o alarmie i prawdzie: detektor przegrzania w chłodni ma znaną czułość i odsetek fałszywych alarmów, a rejestr podpowiada częstość bazową awarii. Wynik — wiarygodność alarmu — za chwilę okaże się potrzebny w zupełnie innym miejscu analizy."
    ),
    widget = figure_panel(label = "Karta 1", title = "Detektor przegrzania", sliderInput("i10_prev", "P(awarii)", .001, .1, .01, .001), sliderInput("i10_sens", "Czułość", .5, 1, .95, .01), sliderInput("i10_fpr", "FPR", 0, .2, .05, .005), uiOutput("i10_alarm_result"), full_width = TRUE)
  ),
  list(
    id = "karta-kontrola", title = "Karta 2: kontrola partii",
    lead = "Model dwumianowy opisuje liczbę niesprawnych zaworów w ustalonej próbie.",
    intro = c(
      "Karta druga to wykład o wielu próbach w wersji roboczej: partia zaworów, ustalona liczba kontroli, stałe p niesprawności. Wynik odpowiada na pytanie o jakość dostawy i zasila decyzję odbiorczą wobec dostawcy.",
      "Ta karta pełni w studium rolę specjalną: jest przykładem wyniku, który do końcowego drzewa nie wejdzie. Jakość partii zaworów to osobny problem decyzyjny — ważny, ale niebędący liściem logiki pożaru magazynu. Umiejętność odłożenia poprawnego rachunku na bok jest częścią integracji."
    ),
    widget = figure_panel(label = "Karta 2", title = "Partia zaworów", sliderInput("i10_n", "Liczba kontroli", 10, 300, 100, 10), sliderInput("i10_p", "p niesprawności", .001, .1, .02, .001), uiOutput("i10_inspection_result"), full_width = TRUE),
    pitfall = "Karta kontroli odpowiada na osobne pytanie decyzyjne — jakość partii zaworów — i celowo nie wchodzi do końcowego FTA. Integracja nie oznacza mnożenia wszystkich dostępnych wyników."
  ),
  list(
    id = "karta-utrzymanie", title = "Karta 3: utrzymanie",
    lead = "Wybór wykładniczy/Weibull wynika z hipotezy o hazardzie.",
    intro = c(
      "Karta trzecia sięga do wykładu o czasie życia: ile wytrzyma wentylator? Odpowiedź zależy od hipotezy o mechanizmie — stały hazard modelu wykładniczego czy zużycie opisane Weibullem — a nie tylko od parametrów.",
      "Przełącz oba modele przy tym samym czasie misji i porównaj R(t). Różnica między nimi to cena hipotezy: dokładnie o tyle mylisz się w ocenie ryzyka misji, jeśli wybierzesz zły mechanizm przy poprawnych średnich."
    ),
    widget = risk_widget_panel("Karta 3", "Niezawodność wentylatora", tagList(selectInput("i10_life_model", "Model", c("Wykładniczy — stały hazard" = "exp", "Weibull — zużycie" = "weibull")), sliderInput("i10_time", "Czas misji (h)", 100, 3000, 1000, 50)), "i10_life_plot", "i10_life_stats")
  ),
  list(
    id = "system", title = "Część B — układ zabezpieczeń",
    lead = "Łączymy sterownik szeregowo z dwoma wentylatorami równoległymi i jawnym zasilaniem.",
    intro = c(
      "Karty opisały elementy; czas na architekturę. Instalacja chłodzenia Bananpolu to zasilanie i sterownik w szeregu z redundantną parą wentylatorów — dokładnie układ mieszany z wykładu o niezawodności systemu, tym razem z liczbami z teczki.",
      "Zwróć uwagę, że wszystkie trzy suwaki są niezawodnościami dla wspólnego czasu misji 1000 godzin. To nie przypadek, lecz wymóg kontraktu przypadku z początku wykładu — bez niego iloczyn nie miałby sensu."
    ),
    formula = "R_{sys}=R_P R_C[1-(1-R_A)(1-R_B)]",
    widget = figure_panel(label = "Redukcja", title = "System Bananpolu", sliderInput("i10_power", "R zasilania", .7, 1, .98, .01), sliderInput("i10_controller", "R sterownika", .7, 1, .95, .01), sliderInput("i10_fan", "R wentylatora (wspólne dla A i B)", .7, 1, .92, .01), uiOutput("i10_system_result"), full_width = TRUE)
  ),
  list(
    id = "fta", title = "Końcowe FTA",
    lead = "Parametry z wcześniejszych kart stają się liśćmi jawnej logiki zdarzenia szczytowego.",
    intro = c(
      "Zwieńczenie studium: drzewo błędów pożaru magazynu, którego liście nie są już danymi z sufitu, lecz wynikami wcześniejszych kart. Zawodna detekcja pochodzi z karty alarmu (1−czułość), utrata funkcji chłodzenia z karty systemu — po konwersji horyzontu — a logika bramek z poprzedniego wykładu.",
      "Konwersja horyzontu jest najdelikatniejszym szwem: system liczyliśmy na misję 1000 godzin, a drzewo pyta o rok. Przyjmujemy trzy porównywalne, niezależne misje rocznie i przeliczamy zawodność jako 1−R³ — to założenie jest zapisane jawnie, bo ćwiczenie każe je zakwestionować."
    ),
    formula = "q_{sys,rok}=1-R_{sys}^{\\,3}",
    widget = risk_widget_panel("Integracja", "Inicjacja AND utrata zabezpieczeń", sliderInput("i10_init", "P(inicjacji)", 0, .03, .005, .001), "i10_fta_plot", "i10_fta_stats", note = "Wszystkie liście interpretujemy w horyzoncie rocznym. Rok magazynu to trzy porównywalne misje po 1000 h, więc zawodność systemu wchodzi do drzewa po konwersji jako 1−R_sys³ — przy założeniu, że misje są porównywalne i niezależne. W ćwiczeniu sprawdzisz, jak wynik zmienia inna liczba misji."),
    takeaway = "W tym drzewie spotykają się wybrane wyniki wcześniejszych kart: czułość detektora z karty alarmu, niezawodność systemu po konwersji horyzontu i logika bramek z analizy drzewa błędów. Integracja oznacza wybór parametrów, które faktycznie należą do logiki zdarzenia szczytowego — a nie zsumowanie wszystkiego, co udało się policzyć.",
    pitfall = "Nie mnożymy kart tylko dlatego, że wszystkie są dostępne; połączenie musi wynikać z logiki systemu, a każde wejście musi mieć ten sam horyzont."
  ),
  list(
    id = "interwencje", title = "Cztery interwencje",
    lead = "Każda opcja zmienia konkretny parametr albo strukturę modelu — aplikacja nie wybiera za użytkownika.",
    intro = c(
      "Zarząd rozważa cztery interwencje: lepszy czujnik, częstszy przegląd, niezależne zasilanie i dodatkowy wentylator. Każda działa w innym miejscu modelu — jedna podnosi czułość, inna obniża inicjację, jeszcze inna przebudowuje architekturę — więc każdą można uczciwie przeliczyć na nową wartość P(top).",
      "Obok wyniku w tabeli stoją koszt i wykonalność. Ranking liczbowy odpowiada tylko na pytanie „co najbardziej obniża ryzyko?”; decyzja odpowiada na pytanie „co najbardziej obniża ryzyko za dostępne pieniądze i w realnym czasie?” — a to są różne pytania."
    ),
    widget = risk_widget_panel("Opcje", "Efekt względem modelu bazowego", selectInput("i10_intervention", "Interwencja", setNames(bananpol$interventions$id, bananpol$interventions$label)), "i10_interventions_plot", "i10_intervention_stats", note = "Każda interwencja zmienia konkretny parametr albo strukturę modelu, więc redukcję P(top) można zweryfikować rachunkiem z wcześniejszych bloków."),
    decision = "Traktuj ranking liczbowy jako wejście do decyzji wielokryterialnej."
  ),
  list(
    id = "scenariusze", title = "Analiza scenariuszy",
    lead = "Wynik bazowy, optymistyczny i ostrożny pokazuje konsekwencje niepewności parametrów.",
    intro = c(
      "Wszystkie liczby w teczce są szacunkami, a szacunki bywają mylne. Zamiast udawać precyzję, pokazujemy niepewność jawnie: przeskalowujemy wszystkie liście drzewa o wybrany zakres w górę i w dół i liczymy P(top) w trzech scenariuszach.",
      "Test rekomendacji brzmi: czy przewaga wybranej interwencji przetrwa ostrożny scenariusz? Rekomendacja, która wygrywa tylko w scenariuszu bazowym, jest zakładem o dokładność danych — i decydent ma prawo o tym wiedzieć."
    ),
    widget = risk_widget_panel("Niepewność", "Trzy jawne scenariusze", sliderInput("i10_uncertainty", "Zakres zmiany parametrów", 0, .5, .2, .05), "i10_scenarios", "i10_scenarios_stats"),
    takeaway = "Scenariusze nie są przedziałem ufności — są jawnym eksperymentem na założeniach. Ich siła polega na tym, że każdy może je powtórzyć i zakwestionować konkretną liczbę, a nie ogólne poczucie niepewności."
  ),
  list(
    id = "notatka", title = "Czterozdaniowa notatka",
    lead = "Krótka rekomendacja łączy wynik z warunkami jego ważności.",
    intro = c(
      "Najlepszy rachunek, którego nikt nie zrozumie, nie zmieni żadnej decyzji. Cztery zdania — wynik, jego skala w naturalnych częstościach, kluczowe założenie i rekomendacja — to format, który decydent przeczyta między dwoma spotkaniami i na który może odpowiedzieć.",
      "Zwróć uwagę na zdanie trzecie. Kluczowe założenie w notatce to nie asekuracja, lecz instrukcja obsługi wyniku: mówi, kiedy rekomendacja przestaje obowiązywać i co trzeba sprawdzić, zanim użyje się jej ponownie."
    ),
    widget = figure_panel(label = "Generator", title = "Wynik → częstość → założenie → rekomendacja", selectInput("i10_recommend", "Rekomendowane działanie", setNames(bananpol$interventions$id, bananpol$interventions$label)), uiOutput("i10_memo"), full_width = TRUE),
    decision = "Człowiek zatwierdza rekomendację po ocenie wykonalności i skutków ubocznych."
  ),
  list(
    id = "audyt", title = "Quiz audytowy i ćwiczenie zespołowe",
    lead = "Zespół kończy analizę krótką prezentacją decyzji oraz jej ograniczeń.",
    intro = "Na koniec kursu audyt zatacza koło: wracamy do pytań z pierwszego wykładu — o zdarzenie, mianownik, jednostkę i skutek — tym razem zadanych całej analizie naraz. Ćwiczenie zespołowe symuluje prawdziwy finał pracy inspektora: obronę rekomendacji przed ludźmi, którzy mają prawo pytać o każde założenie.",
    sections = list(list(id = "sciaga", title = "Ściąga końcowa", bullets = c("Pytanie i jednostka poprzedzają model", "Każde p ma horyzont i źródło", "Niezależność jest jawna", "Wynik tłumaczymy na naturalną częstość", "Rekomendacja zawiera kluczowe założenie"))),
    widget = risk_assessment_ui("i10", integracja_quiz, integracja_exercises)
  )
))
integracja_chapters <- risk_block_chapters(integracja_block)

integracja_server <- function(input, output, session) {
  vote <- reactiveVal(FALSE)
  observeEvent(input$i10_vote_check, vote(TRUE))
  output$i10_vote_feedback <- renderUI({
    req(vote())
    if (is.null(input$i10_vote)) {
      return(lc_feedback(type = "info", "Najpierw zaznacz jedną z odpowiedzi."))
    }
    lc_feedback(type = if (identical(input$i10_vote, "audit")) "ok" else "warning", tags$strong("Najpierw kontrakt analizy:"), " definicje, jednostki, horyzonty, źródła i braki.")
  })
  output$i10_dossier <- renderUI({
    done <- length(input$i10_fields)
    lc_feedback(type = if (done == 4) "ok" else "warning", tags$strong(paste(done, "z 4 pól sprawdzonych.")), if (done == 4) " Teczka ma minimalny kontrakt danych." else " Uzupełnij metadane przed rachunkiem.")
  })
  output$i10_model <- renderUI({
    models <- c(conditional = "Prawdopodobieństwo warunkowe i całkowite", bayes = "Bayes / naturalne częstości", binomial = "Rozkład dwumianowy", geometric = "Rozkład geometryczny", negative = "Rozkład ujemny dwumianowy", threshold = "Rozkład normalny: pole za progiem", survival = "R(t) i h(t)", system = "Logika szeregowa/równoległa i FTA")
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
  missions_per_year <- 3L
  annual_q <- function(r_mission) 1 - r_mission^missions_per_year
  top_p <- reactive(risk_fta_top(input$i10_init, 1 - input$i10_sens, annual_q(sys_r())))
  intervention_mechanisms <- c(
    detector = "połowa przeoczonych awarii mniej: czułość s → 1−(1−s)/2",
    inspection = "P(inicjacji) zmniejszone o połowę",
    power = "redundantne zasilanie: R_P → 1−(1−R_P)²",
    fan = "trzeci wentylator w gałęzi równoległej"
  )
  intervention_top <- function(id) {
    init <- input$i10_init
    sens <- input$i10_sens
    power <- input$i10_power
    fans <- rep(input$i10_fan, 2)
    switch(id,
      detector = sens <- 1 - (1 - sens) / 2,
      inspection = init <- init / 2,
      power = power <- 1 - (1 - power)^2,
      fan = fans <- rep(input$i10_fan, 3)
    )
    system <- power * input$i10_controller * risk_parallel_reliability(fans)
    risk_fta_top(init, 1 - sens, annual_q(system))
  }
  fta_plot <- reactive({
    vals <- c(`Inicjacja` = input$i10_init, `Zawodna detekcja (1−czułość)` = 1 - input$i10_sens, `Utrata funkcji systemu (rok)` = annual_q(sys_r()), `Top event` = top_p())
    ggplot(data.frame(node = factor(names(vals), levels = names(vals)), p = vals), aes(node, p, fill = node)) +
      geom_col() +
      scale_fill_manual(values = upwr_cat_n(4), guide = "none") +
      labs(title = "Parametry kart w logice FTA", x = NULL, y = "Prawdopodobieństwo") +
      theme_upwr() +
      theme(axis.text.x = element_text(angle = 18, hjust = 1))
  })
  zoom_plot_server("i10_fta_plot", fta_plot, alt = "Słupki parametrów wejściowych oraz końcowego zdarzenia szczytowego.")
  output$i10_fta_stats <- renderUI(lc_stat_grid(
    lc_stat_box("Inicjacja (suwak obok)", risk_format_probability(input$i10_init)),
    lc_stat_box("Zawodna detekcja — karta alarmu (1−czułość)", risk_format_probability(1 - input$i10_sens)),
    lc_stat_box("Utrata funkcji — karta systemu (1−R_sys³, horyzont roczny)", risk_format_probability(annual_q(sys_r()))),
    lc_stat_box("P(top) w ciągu roku", risk_format_probability(top_p()), color = upwr_accent),
    lc_stat_box("Naturalna częstość", risk_natural_frequency(top_p(), 10000)),
    columns = 1
  ))
  interventions_plot <- reactive({
    d <- bananpol$interventions
    d$result <- vapply(d$id, intervention_top, numeric(1))
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
    lc_stat_grid(
      lc_stat_box("Mechanizm", intervention_mechanisms[[input$i10_intervention]]),
      lc_stat_box("P(top) po zmianie", risk_format_probability(intervention_top(input$i10_intervention)), color = upwr_accent),
      lc_stat_box("Koszt demonstracyjny", d$cost_index),
      lc_stat_box("Wykonalność", d$feasibility),
      columns = 1
    )
  })
  scenario_top <- function(multiplier) {
    risk_fta_top(
      min(1, input$i10_init * multiplier),
      min(1, (1 - input$i10_sens) * multiplier),
      min(1, annual_q(sys_r()) * multiplier)
    )
  }
  scenarios_plot <- reactive({
    u <- input$i10_uncertainty
    dat <- data.frame(scenario = factor(c("Optymistyczny", "Bazowy", "Ostrożny"), levels = c("Optymistyczny", "Bazowy", "Ostrożny")), p = c(scenario_top(1 - u), top_p(), scenario_top(1 + u)))
    ggplot(dat, aes(scenario, p, fill = scenario)) +
      geom_col() +
      scale_fill_manual(values = upwr_cat_n(3), guide = "none") +
      labs(title = "Jawny przedział scenariuszy", x = NULL, y = "P(top)") +
      theme_upwr()
  })
  zoom_plot_server("i10_scenarios", scenarios_plot, alt = "Trzy słupki scenariusza optymistycznego, bazowego i ostrożnego.")
  output$i10_scenarios_stats <- renderUI(lc_feedback(type = "info", "Scenariusze skalują jednocześnie wszystkie liście drzewa o wybrany zakres i przeliczają P(top) od nowa. Nie są przedziałem ufności; pokazują konsekwencje jawnych zmian założeń."))
  output$i10_memo <- renderUI({
    d <- bananpol$interventions[bananpol$interventions$id == input$i10_recommend, ]
    after <- intervention_top(input$i10_recommend)
    tags$ol(tags$li(paste0("W modelu bazowym roczne P(top) = ", risk_format_probability(top_p()), ".")), tags$li(paste0("To odpowiada ", risk_natural_frequency(top_p(), 10000), " porównywalnych lat pracy magazynu.")), tags$li("Kluczowe założenie: wejścia OR i karty systemu są niezależne poza jawnymi wspólnymi przyczynami."), tags$li(paste0("Rekomendacja do oceny: ", d$label, " (", intervention_mechanisms[[input$i10_recommend]], "); wynik po zmianie ", risk_format_probability(after), ", koszt ", d$cost_index, ", wykonalność ", d$feasibility, ".")))
  })
  risk_assessment_server("i10", integracja_quiz, input, output)
}
