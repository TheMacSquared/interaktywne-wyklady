# Blok 05: Ile prób do zdarzenia -----------------------------------------

dozd_quiz <- list(questions = list(
  list(
  question = "Co jest ustalone w modelu ujemnym dwumianowym?",
  choices = c("Liczba oczekiwanych zdarzeń r" = "r", "Łączna liczba prób n" = "n", "Dokładny moment ostatniego zdarzenia" = "time"),
  correct = "r", explanation = "Eksperyment trwa do r-tego zdarzenia, więc liczba prób jest losowa."
),
  list(question = "Przy p=0,1 ile wynosi średnia liczba wszystkich prób do trzeciej wady?",
    choices = c("3" = "a", "27" = "b", "30" = "c"), correct = "c",
    explanation = "E(X)=r/p=30; 27 to średnia liczba niepowodzeń przed trzecim sukcesem."),
  list(question = "R zwrócił 27 niepowodzeń przed trzecim sukcesem. Ile było wszystkich prób?",
    choices = c("24" = "a", "30" = "b", "27" = "c"), correct = "b",
    explanation = "Dodajemy trzy próby zakończone sukcesem: X=Y+r."),
  list(question = "Po 20 próbach bez wady, przy stałym p i niezależności, szansa wady w następnej próbie…",
    choices = c("nadal wynosi p" = "a", "rośnie do 1" = "b", "spada do zera" = "c"), correct = "a",
    explanation = "Brak pamięci nie oznacza, że zdarzenie musi wkrótce nastąpić."),
  list(question = "Między seriami p jest losowe. Jak wyznaczyć średnią liczbę prób do r zdarzeń?",
    choices = c("r/E(p) zawsze" = "a", "E(p)/r" = "b", "r·E(1/p)" = "c"), correct = "c",
    explanation = "Warunkowo średnia wynosi r/p; następnie uśredniamy po rozkładzie p. Zmienność zmienia także średnią oczekiwania.")
))
dozd_exercises <- c(
  "Bananpol: przy p=0,10 wyznacz średnią liczbę kontroli do znalezienia trzech wadliwych zabezpieczeń i P(ukończenia do 40. kontroli).",
  "Diagnostyka: jakość zmienia się między partiami. Wyjaśnij, dlaczego model ze stałym p może zaniżyć niepewność planu.",
  "Transfer: zaplanuj liczbę audytów potrzebną do zaobserwowania dwóch naruszeń procedury przy podanym p."
)

dozd_sciaga_widget <- tagList(
  figure_panel(
    label = "Ściąga 5.1",
    title = "Trzy rozkłady schematu Bernoulliego",
    full_width = TRUE,
    tags$table(
      class = "lc-table lc-table-striped lc-table-bordered",
      tags$thead(tags$tr(
        tags$th("Rozkład"), tags$th("Co jest stałe"), tags$th("Co jest losowe"), tags$th("Pytanie inspektora")
      )),
      tags$tbody(
        tags$tr(tags$td("Dwumianowy"), tags$td("liczba prób n"), tags$td("liczba zdarzeń"), tags$td("Ile wad w partii 100 zaworów?")),
        tags$tr(tags$td("Geometryczny"), tags$td("cel: 1 zdarzenie"), tags$td("liczba prób"), tags$td("Ile kontroli do pierwszej wady?")),
        tags$tr(tags$td("Ujemny dwumianowy"), tags$td("cel: r zdarzeń"), tags$td("liczba prób"), tags$td("Ile kontroli do trzeciej wady?"))
      )
    )
  ),
  risk_assessment_ui("d5", dozd_quiz, dozd_exercises)
)

dozd_block <- list(id = "dozd", title = "Ile prób do zdarzenia", chapters = list(
  list(
    id = "regula", title = "Co pozostaje stałe?", lead = "Dwumianowy zatrzymuje się po n próbach; ujemny dwumianowy po r zdarzeniach.",
    intro = c(
      "Audytor w Bananpolu nie pyta, ile wadliwych zabezpieczeń znajdzie w pięćdziesięciu kontrolach. Pyta, ile kontroli potrwa, zanim znajdzie trzy — bo na tyle musi zabudżetować czas i ludzi. To odwrócenie zmienia rozkład: losowa przestaje być liczba zdarzeń, a staje się liczba prób.",
      "Pojedyncze próby są dokładnie te same, co w poprzednim wykładzie — schemat Bernoulliego ze stałym p i niezależnością. Zmienia się wyłącznie reguła zatrzymania eksperymentu. Rozpoznanie, co jest stałe, a co losowe, jest pierwszym i najważniejszym krokiem doboru rozkładu."
    ),
    callout = list(
      label = "Dane Bananpolu",
      text = "Audyt zabezpieczeń ładunku: prawdopodobieństwo, że losowo wybrana paleta ma wadliwe zabezpieczenie, wynosi 0,10; cel audytu to r = 3 wykryte wady. Jednostka: kontrola jednej palety; horyzont: jedna seria audytowa. Liczby są fikcyjne.",
      color = "uwaga"
    ),
    widget = risk_vote_panel("d5_vote", "d5_vote_feedback", "Chcemy znaleźć trzy wadliwe zabezpieczenia. Który element eksperymentu jest stały?", c("r=3 znalezione wady" = "r", "n — liczba kontroli" = "n", "odsetek wad w zebranej próbie" = "share"))
  ),
  list(
    id = "geometryczny", title = "Do pierwszego wykrycia", lead = "Rozkład geometryczny ma długi ogon: sukces może nadejść szybko albo bardzo późno.",
    intro = c(
      "Najprostsza wersja pytania: ile kontroli do pierwszej wady? Zanim padnie jakikolwiek wzór, zbuduj wyczucie — uruchom symulację kilka razy i obserwuj kształt histogramu: gdzie jest szczyt, jak długo ciągnie się ogon, jak często seria kończy się już przy pierwszych kontrolach.",
      "Dwie rzeczy powinny zwrócić uwagę. Najbardziej prawdopodobna jest zawsze pierwsza kontrola, a każda kolejna coraz mniej — mimo to średnia bywa myląca: przy p = 0,10 średnio czekamy 10 kontroli, ale co dziesiąta seria przekroczy 22 kontrole."
    ),
    widget = tagList(
      risk_widget_panel("Symulacja", "Ile kontroli do pierwszej wady?", tagList(sliderInput("d5_geo_p", "p", .01, .5, .1, .01), actionButton("d5_geo_run", "Losuj ponownie", class = "lc-btn-primary")), "d5_geo", "d5_geo_stats"),
      lc_p(
        "Kształt, który widzisz, ma prostą historię. Żeby pierwsza wada pojawiła
         się dokładnie w x-tej kontroli, potrzeba x−1 kontroli bez wykrycia,
         a potem jednego wykrycia — mnożymy wzdłuż drogi, dokładnie jak
         w wykładzie o warunkach:"
      ),
      lc_formula_box(
        withMathJax("$$P(X=x)=(1-p)^{x-1}p,\\qquad E(X)=\\frac{1}{p}$$"),
        tags$p("Każdy kolejny słupek histogramu jest poprzednim pomnożonym przez (1−p) — stąd opadający kształt i długi ogon.")
      ),
      lc_h2("dozd-geometryczny-pamiec", "Brak pamięci"),
      lc_p(
        "Rozkład geometryczny nie pamięta porażek: po dwudziestu kontrolach bez
         wykrycia rozkład dalszego oczekiwania wygląda dokładnie tak samo jak na
         początku. Seria „bez wady” nie zwiastuje bliskiego wykrycia — to
         dyskretna wersja własności, którą w wykładzie o czasie życia spotkamy
         pod nazwą stałego hazardu."
      )
    ),
    takeaway = "Brak wykrycia po trzydziestu kontrolach nie dowodzi, że wad nie ma. Model geometryczny pozwala policzyć, jak prawdopodobne jest tak długie oczekiwanie przy przyjętym p; dopiero jawne kryterium decyzyjne mówi, kiedy przerwać kontrolę."
  ),
  list(
    id = "rte", title = "Do r-tego wykrycia", lead = "Łączna liczba prób jest sumą czasów oczekiwania na kolejne wykrycia.",
    intro = c(
      "Audytor potrzebuje trzech wykrytych wad, nie jednej. Oczekiwanie na trzecią wadę to trzy sklejone oczekiwania geometryczne: do pierwszej, potem do drugiej, potem do trzeciej. Suma tych trzech czasów ma rozkład ujemny dwumianowy.",
      "Współczynnik we wzorze zlicza układy: ostatnia, x-ta kontrola musi zakończyć się wykryciem, a wcześniejsze r−1 wykryć może rozmieścić się dowolnie wśród x−1 poprzednich kontroli. Porównaj kształt rozkładu z geometrycznym: im większe r, tym rozkład bardziej symetryczny i dalszy od zera."
    ),
    formula = "P(X=x)=\\binom{x-1}{r-1}p^{r}(1-p)^{x-r},\\qquad E(X)=\\frac{r}{p}",
    widget = risk_widget_panel("Rozkład", "Łączna liczba kontroli", tagList(sliderInput("d5_p", "p wykrycia", .01, .5, .1, .01), sliderInput("d5_r", "r", 1, 10, 3, 1)), "d5_nb", "d5_nb_stats"),
    takeaway = "Dla r = 1 ujemny dwumianowy pokrywa się z geometrycznym — warto to sprawdzić suwakiem. Uogólnienie nie dodaje nowych założeń: nadal stałe p i niezależne próby, zmienia się tylko cel."
  ),
  list(
    id = "parametryzacje", title = "Dwie parametryzacje", lead = "Oprogramowanie może zwracać niepowodzenia przed r-tym sukcesem zamiast wszystkich prób.",
    intro = "Podręczniki i biblioteki liczą ten sam rozkład na dwa sposoby: jako łączną liczbę prób X albo jako liczbę niepowodzeń Y przed r-tym sukcesem. Funkcje R z rodziny nbinom używają drugiej konwencji — dlatego w kodzie tego kursu do wyniku dodaje się r. Obie wersje opisują tę samą serię kontroli; różnią się tylko tym, co liczą.",
    sections = list(list(id = "os", title = "Ta sama realizacja", text = "Jeżeli znaleziono r zdarzeń po X wszystkich próbach, liczba wcześniejszych niepowodzeń wynosi X−r. Przeliczenie jest trywialne, ale tylko wtedy, gdy wiadomo, którą wielkość podaje źródło — w raporcie zawsze nazwij, co oznacza oś.")),
    formula = "X_{wszystkie}=Y_{niepowodzenia}+r", pitfall = "Bez nazwania parametryzacji wynik może różnić się dokładnie o r."
  ),
  list(
    id = "zasoby", title = "Średnia kontra plan zasobów", lead = "Średnia r/p nie gwarantuje ukończenia przed limitem.",
    intro = c(
      "Przy p = 0,10 i celu r = 3 średnia liczba kontroli wynosi 30. Czy zaplanowanie dokładnie 30 kontroli wystarczy? Kalkulator poniżej pokazuje, że szansa ukończenia audytu w 30 kontrolach to tylko około połowa — rozkład jest skośny i długa seria pechowych kontroli wcale nie jest rzadka.",
      "Plan zasobów buduje się więc na kwantylu, nie na średniej: limit kontroli dobieramy tak, żeby prawdopodobieństwo ukończenia audytu przed limitem osiągnęło uzgodniony poziom, na przykład 95%. Różnica między średnią a kwantylem to właśnie zapas planistyczny."
    ),
    formula = "E(X)=\\frac{r}{p}",
    widget = figure_panel(label = "Kalkulator", title = "Limit liczby kontroli", sliderInput("d5_limit", "Limit", 3, 200, 40, 1), uiOutput("d5_plan"), full_width = TRUE),
    decision = "Planuj na podstawie prawdopodobieństwa ukończenia lub kwantyla, a nie tylko średniej."
  ),
  list(
    id = "zawodzi", title = "Kiedy model zawodzi", lead = "Stałe p i niezależność są założeniami operacyjnymi.",
    intro = c(
      "Stałe p brzmi niewinnie, ale w praktyce oznacza: każda kontrolowana paleta pochodzi z tej samej populacji jakości. Gdy dostawy przychodzą od różnych dostawców albo jakość dryfuje w czasie, p zmienia się między partiami — a rozkład liczby kontroli robi się szerszy, niż obiecuje model.",
      "Symulacja porównuje świat stałego p ze światem, w którym p losuje się osobno dla każdej partii. Zmienia się także średnia: przy losowym p wynosi r·E(1/p), a nie r/E(p). Funkcja 1/p jest wypukła, więc przy tej samej średniej p zmienność wydłuża przeciętne oczekiwanie. Różni się również ogon — czyli dokładnie ta część rozkładu, na której opiera się plan zasobów. Niedoszacowany ogon to audyt, który „niespodziewanie” trwa dwa razy dłużej."
    ),
    sections = list(list(
      id = "transfer", title = "Przykład transferowy: poszukiwania i rekrutacja",
      text = "Model „ile prób do r-tego sukcesu” pojawia się wszędzie tam, gdzie szuka się rzadkich obiektów: liczba odwiertów do drugiego złoża, liczba rozmów rekrutacyjnych do trzeciego zatrudnienia, liczba testów do wykrycia r-tej usterki oprogramowania. We wszystkich tych zastosowaniach ta sama pułapka: sukcesy zmieniają proces (uczenie, wyczerpanie puli), więc stałość p trzeba sprawdzić, zanim rozkład stanie się planem."
    )),
    widget = risk_widget_panel("Porównanie", "Stałe p kontra partie o różnej jakości", sliderInput("d5_variation", "Odchylenie p przed ograniczeniem do [0,005; 0,95]", 0, .09, .04, .005), "d5_failure", "d5_failure_stats"),
    pitfall = "Uczenie kontrolera, grupowanie wad i zmiana dostawy mogą zmieniać p w czasie."
  ),
  list(
    id = "decyzja", title = "Ile kontroli zaplanować?", lead = "Plan powinien podać cel, limit i ryzyko niedokończenia.",
    intro = "Wynik tego wykładu trafia do jednego dokumentu: planu audytu. Dobry plan nie obiecuje, że audyt się uda — podaje, z jakim prawdopodobieństwem uda się w ramach przyznanych zasobów, i co się stanie, jeśli limit zostanie osiągnięty bez ukończenia celu. Ta ostatnia pozycja jest najczęściej pomijana, a to ona decyduje, czy przekroczenie limitu będzie kontrolowaną decyzją, czy improwizacją.",
    sections = list(list(id = "raport", title = "Minimalny raport", bullets = c("cel r i definicja wykrycia", "p i jego źródło", "limit zasobów", "P(ukończenia przed limitem)", "reakcja, gdy limit zostanie przekroczony"))),
    decision = "Oddziel oczekiwaną liczbę kontroli od bezpiecznego zapasu planistycznego."
  ),
  list(
    id = "sprawdzenie", title = "Ściąga i sprawdzenie", lead = "Reguła zatrzymania → p i r → rozkład → limit → decyzja.",
    intro = "Masz teraz komplet trzech rozkładów zbudowanych na schemacie Bernoulliego. Ściąga zestawia je obok siebie — w quizie i ćwiczeniach najważniejsze będzie rozpoznanie, które pytanie prowadzi do którego rozkładu.",
    sections = list(list(id = "sciaga", title = "Ściąga", bullets = c("Pytanie: ile prób do r-tego zdarzenia?", "Model: geometryczny dla r=1, ujemny dwumianowy dla r>1", "Założenia: stałe p i niezależność", "Wynik: rozkład liczby prób", "Interpretacja: zasoby potrzebne do osiągnięcia celu")), list(id = "most", title = "Co dalej", text = "Geometryczny i ujemny dwumianowy liczą dyskretne próby. W wykładzie o czasie życia to samo pytanie — jak długo czekamy na zdarzenie — zadamy w czasie ciągłym, a odpowiedzą rozkład wykładniczy i gamma.")),
    widget = dozd_sciaga_widget
  )
))

dozd_chapters <- risk_block_chapters(dozd_block)

dozd_server <- function(input, output, session) {
  vote <- reactiveVal(FALSE)
  observeEvent(input$d5_vote_check, vote(TRUE))
  output$d5_vote_feedback <- renderUI({
    req(vote())
    if (is.null(input$d5_vote)) {
      return(lc_feedback(type = "info", "Najpierw zaznacz jedną z odpowiedzi."))
    }
    lc_feedback(type = if (identical(input$d5_vote, "r")) "ok" else "warning", tags$strong("Stały jest cel:"), " r=3; liczba kontroli pozostaje losowa.")
  })
  geo_sample <- reactive({
    input$d5_geo_run
    rgeom(400, input$d5_geo_p) + 1
  })
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
  failure_data <- reactive({
    set.seed(505)
    stable <- rnbinom(1000, size = 3, prob = .1) + 3
    ps <- pmin(.95, pmax(.005, rnorm(1000, .1, input$d5_variation)))
    mixed <- vapply(ps, function(p) rnbinom(1, 3, p) + 3, numeric(1))
    data.frame(x = c(stable, mixed), model = rep(c("Stałe p", "Zmienne p między partiami"), each = 1000))
  })
  failure_plot <- reactive({
    ggplot(failure_data(), aes(x, fill = model)) +
      geom_histogram(binwidth = 3, position = "identity", alpha = .55) +
      coord_cartesian(xlim = c(3, 150)) +
      scale_fill_manual(values = upwr_cat_n(2)) +
      labs(title = "Zmienność jakości poszerza rozkład", x = "Liczba kontroli", y = "Powtórzenia", fill = NULL) +
      theme_upwr()
  })
  zoom_plot_server("d5_failure", failure_plot, alt = "Nakładające się histogramy stałego i zmiennego prawdopodobieństwa wykrycia.")
  output$d5_failure_stats <- renderUI({
    dat <- failure_data()
    means <- tapply(dat$x, dat$model, mean)
    lc_stat_grid(lc_stat_box("Średnia symulowana — stałe p", round(means[["Stałe p"]], 1)), lc_stat_box("Średnia symulowana — zmienne p", round(means[["Zmienne p między partiami"]], 1)), columns = 1)
  })
  risk_assessment_server("d5", dozd_quiz, input, output)
}
