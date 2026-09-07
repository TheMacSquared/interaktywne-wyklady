# Blok 07: Czas życia elementu ------------------------------------------

zycie_quiz <- list(questions = list(
  list(question = "Co oznacza stały hazard rozkładu wykładniczego?", choices = c("Chwilowe tempo awarii nie zależy od wieku działającego elementu" = "constant", "Każdy element żyje dokładnie tyle samo" = "same", "Ryzyko awarii zawsze rośnie" = "grow"), correct = "constant", explanation = "Brak pamięci dotyczy warunkowego ryzyka dalszego życia, nie identycznych czasów awarii."),
  list(question = "MTTF=1500 h w modelu wykładniczym. Ile wynosi R(1000)?",
    choices = c("Około 0,667" = "a", "1500" = "b", "Około 0,513" = "c"), correct = "c",
    explanation = "R(1000)=exp(−1000/1500)."),
  list(question = "Element działa na końcu obserwacji po 1200 h. Co zapisujemy?",
    choices = c("Usuwamy element z danych" = "a", "Czas 1200 h i znacznik cenzorowania" = "b", "Awarię dokładnie po 1200 h" = "c"), correct = "b",
    explanation = "Wiemy, że T>1200 h; nie znamy dokładnego czasu przyszłej awarii."),
  list(question = "Hazard wynosi 0,002 na godzinę. Co przybliża 0,002×0,1?",
    choices = c("Szansę awarii w najbliższej 0,1 h wśród działających" = "a", "Szansę awarii od uruchomienia do teraz" = "b", "Niezawodność 0,1 h każdego modelu" = "c"), correct = "a",
    explanation = "Dla małego Δt hazard razy Δt przybliża warunkowe prawdopodobieństwo zdarzenia."),
  list(question = "Co opisuje czas do trzeciego zdarzenia jednorodnego procesu Poissona?",
    choices = c("Zawsze Weibull z β=3" = "a", "Rozkład dwumianowy" = "b", "Erlang, czyli gamma z k=3" = "c"), correct = "c",
    explanation = "Sumujemy trzy niezależne wykładnicze czasy o tej samej intensywności.")
))
zycie_exercises <- c("Bananpol: dla MTTF=1500 h policz R(1000) w modelu wykładniczym.", "Diagnostyka: wskaż, dlaczego widoczne tylko zakończone awarie zaniżają oszacowany czas życia.", "Transfer: wybierz sensowny kształt Weibulla dla elementu zużywającego się i uzasadnij znak zmiany hazardu.")

zycie_functions_table <- figure_panel(
  label = "Słownik",
  title = "Cztery funkcje, cztery pytania",
  full_width = TRUE,
  tags$table(
    class = "lc-table lc-table-striped lc-table-bordered",
    tags$thead(tags$tr(
      tags$th("Funkcja"), tags$th("Definicja"), tags$th("Pytanie inspektora")
    )),
    tags$tbody(
      tags$tr(tags$td("Gęstość f(t)"), tags$td("rozkład momentów awarii"), tags$td("Kiedy awarie są najgęstsze?")),
      tags$tr(tags$td("Dystrybuanta F(t)"), tags$td("P(T ≤ t)"), tags$td("Jaka część elementów zawiedzie do chwili t?")),
      tags$tr(tags$td("Niezawodność R(t)"), tags$td("P(T > t) = 1 − F(t)"), tags$td("Jaka część dotrwa poza t?")),
      tags$tr(tags$td("Hazard h(t)"), tags$td("f(t) / R(t)"), tags$td("Jak ryzykowna jest najbliższa chwila dla elementu, który wciąż działa?"))
    )
  )
)

zycie_block <- list(id = "zycie", title = "Czas życia elementu", chapters = list(
  list(
    id = "mttf", title = "Dwa urządzenia z tym samym MTTF",
    lead = "Ta sama średnia nie gwarantuje tej samej niezawodności w czasie misji.",
    intro = c(
      "Dwa wentylatory z kart katalogowych mają identyczny średni czas życia. Po roku eksploatacji jeden park maszyn wygląda wyraźnie lepiej od drugiego. Średnia nie mówi, jak awarie rozkładają się w czasie — a właśnie od tego zależy, czy element dotrwa do końca misji.",
      "Czas do awarii jest zmienną ciągłą, więc cały warsztat poprzedniego wykładu — gęstość, pola, ogony — działa również tutaj. Dochodzi jedno nowe pojęcie, które zmieni sposób myślenia o starzeniu: hazard, czyli chwilowe tempo awarii elementu, który wciąż działa."
    ),
    callout = list(
      label = "Dane Bananpolu",
      text = "Wentylatory dojrzewalni: model wykładniczy o MTTF 1500 h oraz model Weibulla o kształcie β = 2 i skali η = 1700 h. Jednostka: godzina pracy; horyzont: czas do awarii wentylatora. Liczby są fikcyjne.",
      color = "uwaga"
    ),
    sections = list(list(
      id = "srednia", title = "Czym jest MTTF",
      text = "MTTF (mean time to failure) to wartość oczekiwana czasu życia E(T) — jedna liczba z całego rozkładu, dokładnie tak jak E(X) = np w wykładzie o partiach. Dwa rozkłady o tej samej średniej mogą mieć zupełnie różne udziały wczesnych awarii, a dla planu utrzymania liczą się właśnie one."
    )),
    widget = risk_vote_panel("c7_vote", "c7_vote_feedback", "Czy ten sam MTTF oznacza takie samo R(1000 h)?", c("Tak" = "yes", "Nie — znaczenie ma cały rozkład" = "distribution", "Tylko dla Weibulla" = "weibull"))
  ),
  list(
    id = "cenzorowanie", title = "Oś obserwacji i cenzorowanie",
    lead = "Element nadal działający na końcu badania wnosi informację: jego czas życia jest co najmniej tak długi.",
    intro = c(
      "Badanie trwałości wentylatorów zakończyło się po 1200 godzinach, a spora część egzemplarzy wciąż działała. Usunięcie ich z danych jest poważnym błędem: ich czas życia nie jest całkiem nieznany — wiemy, że przekroczył moment zakończenia obserwacji, i ta informacja musi zostać w analizie.",
      "Przesuń koniec obserwacji na osi czasu i zobacz, jak zmienia się bilans awarii i obserwacji uciętych. Gdyby policzyć „średni czas życia” wyłącznie z zakończonych awarii, każdy wcześniejszy koniec badania dawałby krótszy wynik — nie dlatego, że wentylatory są gorsze, lecz dlatego, że najdłużej żyjące egzemplarze jeszcze nie zdążyły się zepsuć."
    ),
    sections = list(list(
      id = "transfer", title = "Przykład transferowy: badania przeżycia",
      text = "Cenzorowanie to codzienność badań klinicznych: pacjenci, u których zdarzenie nie wystąpiło do końca obserwacji, wnoszą informację „co najmniej tyle”. Metody analizy przeżycia — z krzywą Kaplana–Meiera na czele — powstały właśnie po to, żeby tej informacji nie wyrzucać. Inżynieria niezawodności i medycyna używają tu tego samego aparatu."
    )),
    widget = risk_widget_panel("Oś czasu", "Awarie i obserwacje ucięte", sliderInput("c7_follow", "Koniec obserwacji (h)", 300, 2500, 1200, 50), "c7_timeline", "c7_timeline_stats"),
    pitfall = "Usunięcie działających elementów z danych systematycznie skraca obraz czasu życia."
  ),
  list(
    id = "jezyk", title = "f(t), F(t), R(t) i h(t)",
    lead = "Cztery funkcje odpowiadają na różne pytania o ten sam czas życia.",
    intro = "Cztery funkcje brzmią groźnie, ale to cztery spojrzenia na jeden rozkład — znając jedną, można wyprowadzić pozostałe. Nowością jest hazard: dzieli gęstość przez niezawodność, więc pyta o ryzyko najbliższej chwili wśród elementów, które dożyły do t. To warunkowe spojrzenie — mianownik R(t) robi tu dokładnie to, co warunek B w wykładzie drugim.",
    sections = list(list(id = "zmienna", title = "Zmienna losowa T", text = "Czas życia elementu opisujemy zmienną losową T: czasem od uruchomienia do awarii. Wszystkie cztery funkcje mówią o tym samym T — dystrybuanta F(t)=P(T≤t) to prawdopodobieństwo awarii do chwili t, a niezawodność R(t)=P(T>t) to prawdopodobieństwo przetrwania poza t. O tę zmienną pytaliśmy już w głosowaniu o MTTF: średnia E(T) jest tylko jedną liczbą z całego rozkładu.")),
    formula = "R(t)=1-F(t),\\qquad h(t)=\\frac{f(t)}{R(t)}",
    widget = tagList(
      zycie_functions_table,
      lc_p("F(t) i R(t) są bezwymiarowymi prawdopodobieństwami. Gęstość f(t) i hazard h(t) mają jednostkę 1/h, gdy czas mierzymy w godzinach. Hazard nie jest prawdopodobieństwem i może być większy od 1/h: dopiero h(t)Δt przybliża prawdopodobieństwo awarii w krótkim przedziale, warunkowo dla działającego elementu."),
      lc_formula_box(withMathJax("$$P(t<T\\le t+\\Delta t\\mid T>t)\\approx h(t)\\Delta t$$")),
      lc_p("Niezawodność R(t) pyta o przetrwanie całej misji bez awarii. Gotowość pyta, czy funkcja jest dostępna w danej chwili, także po naprawach. Liczba kolejnych awarii na godzinę w systemie naprawialnym opisuje proces zliczający; nie jest automatycznie hazardem czasu do pierwszej awarii. MTTF dotyczy pierwszej awarii, MTBF odstępów między awariami; nie mieszaj tych wielkości."),
      risk_widget_panel("Synchronizacja", "Wspólny suwak czasu", sliderInput("c7_time", "Czas t (h)", 0, 4000, 1000, 50), "c7_functions", "c7_functions_stats", note = "Dla rozkładu wykładniczego f(t) jest proporcjonalna do R(t), dlatego obie krzywe mają ten sam kształt, a przeskalowany hazard jest poziomą linią. To cecha tego modelu, nie ogólna reguła.")
    )
  ),
  list(
    id = "wykladniczy", title = "Rozkład wykładniczy",
    lead = "Stały hazard daje model bez pamięci — użyteczny, lecz mechanicznie wymagający.",
    intro = c(
      "Najprostsza hipoteza o hazardzie brzmi: jest stały. Element nie dociera się i nie zużywa — psuje się od losowych zaburzeń, które w każdej godzinie są tak samo prawdopodobne. Ta hipoteza wyznacza dokładnie jeden rozkład: wykładniczy, ciągły odpowiednik geometrycznego z wykładu piątego.",
      "Konsekwencją stałego hazardu jest brak pamięci: wentylator pracujący od 1000 godzin ma przed sobą dokładnie taki sam rozkład dalszego życia jak fabrycznie nowy. Jeśli dane pokazują, że stare egzemplarze psują się częściej niż nowe, model wykładniczy jest z góry wykluczony — żaden dobór λ tego nie naprawi."
    ),
    formula = "R(t)=e^{-\\lambda t},\\qquad MTTF=1/\\lambda",
    widget = risk_widget_panel("Model", "Stały hazard", sliderInput("c7_mttf", "MTTF (h)", 300, 4000, 1500, 50), "c7_exp", "c7_exp_stats"),
    pitfall = "Brak pamięci nie pasuje do wyraźnego docierania ani zużycia."
  ),
  list(
    id = "gamma", title = "Rozkład gamma i przypadek Erlanga",
    lead = "Gamma opisuje czas oczekiwania o elastycznym kształcie; dla całkowitego k jest to czas do k-tego zdarzenia.",
    intro = "W wykładzie piątym czekaliśmy na r-te wykrycie, licząc dyskretne próby; gamma robi to samo w czasie ciągłym. W jednorodnym procesie Poissona o intensywności λ czas do k-tego zdarzenia jest sumą k niezależnych czasów wykładniczych o tej samej intensywności — tak jak ujemny dwumianowy był sumą k oczekiwań geometrycznych. Ta paralela to nie przypadek, lecz ta sama konstrukcja w dwóch skalach czasu.",
    sections = list(list(id = "rodzina", title = "Erlang to szczególny przypadek", text = "Erlang jest rozkładem gamma o całkowitym parametrze kształtu k: sumą k niezależnych etapów o wykładniczych czasach — na przykład czasem do k-tej awarii w jednorodnym procesie Poissona. Ogólny rozkład gamma dopuszcza dowolne k>0. Kształt niecałkowity traci interpretację etapów, ale pozwala modelować hazard rosnący (k>1) albo malejący (k<1) i dopasowywać rozkład do danych bez sztucznego zaokrąglania.")),
    formula = "f(t)=\\frac{\\lambda^{k}t^{k-1}e^{-\\lambda t}}{\\Gamma(k)},\\qquad E(T)=k/\\lambda",
    takeaway = "Most przez Poissona: przy stałej intensywności, niezależnych przyrostach i pojedynczych zdarzeniach liczba zdarzeń N(t) ma rozkład Poissona o średniej λt. Czas do pierwszego jest wykładniczy, do k-tego — Erlanga. Stała średnia liczba zgłoszeń nie wystarcza, jeśli zgłoszenia przychodzą grupami albo zależą od wcześniejszych. To krótki kontekst dla gamma, nie dodatkowy rozbudowany dział.",
    widget = risk_widget_panel("Model", "Czas oczekiwania o kształcie k", sliderInput("c7_k", "Parametr kształtu k", .5, 8, 3, .5), "c7_gamma", "c7_gamma_stats", note = "Dla całkowitego k suwak pokazuje rozkłady Erlanga; wartości pośrednie należą do ogólnej rodziny gamma.")
  ),
  list(
    id = "weibull", title = "Część B — mechanizm Weibulla",
    lead = "Parametr β opisuje kierunek zmiany hazardu, a η skalę czasu.",
    intro = c(
      "Weibull jest domyślnym językiem inżynierii niezawodności, bo jednym parametrem odpowiada na najważniejsze pytanie diagnostyczne: co dzieje się z hazardem. β < 1 oznacza hazard malejący (wczesne defekty odsiewają się z parku), β = 1 odtwarza rozkład wykładniczy, a β > 1 — hazard rosnący, charakterystyczny dla zużycia.",
      "Drugi parametr, η, jest czystą skalą czasu: mówi, kiedy rzeczy się dzieją, a nie jak. Przy każdym β niezawodność w chwili t = η wynosi e⁻¹ ≈ 0,37 — to punkt orientacyjny, po którym łatwo czytać wykresy."
    ),
    formula = "R(t)=\\exp[-(t/\\eta)^\\beta]",
    widget = risk_widget_panel("Model", "R(t) i h(t) reagują razem", tagList(sliderInput("c7_beta", "β", .4, 4, 2, .1), sliderInput("c7_eta", "η (h)", 300, 4000, 1700, 50)), "c7_weibull", "c7_weibull_stats"),
    takeaway = "Dobór β nie jest kosmetyką statystyczną, lecz hipotezą o mechanizmie awarii. Zanim dopasujesz parametry do danych, zapytaj inżyniera utrzymania: czy ten element się dociera, zużywa, czy psuje losowo?"
  ),
  list(
    id = "same-mttf", title = "Ten sam MTTF, inne R(t)",
    lead = "Kształt rozkładu wpływa na ryzyko misji, nawet gdy średnie czasy są zgodne.",
    intro = c(
      "Wracamy do głosowania z początku wykładu, tym razem z rachunkiem. Trzy modele Weibulla — o hazardzie malejącym, stałym i rosnącym — skalibrowano tak, żeby wszystkie miały MTTF równy dokładnie 1500 godzin. Karta katalogowa nie odróżni ich od siebie.",
      "Przesuń czas misji i odczytaj trzy wartości R(t) na pionowej linii. Dla krótkich misji najlepszy jest model zużyciowy (β > 1): awarie przychodzą późno, ale zbiorowo. Dla długich misji przewaga się odwraca. Wniosek praktyczny: porównywanie urządzeń po MTTF bez czasu misji jest porównywaniem nieporównywalnego."
    ),
    widget = risk_widget_panel("Porównanie", "Modele skalibrowane do MTTF=1500 h", sliderInput("c7_mission", "Czas misji (h)", 100, 3000, 1000, 50), "c7_same_mean", "c7_same_mean_stats"),
    decision = "Wybieraj urządzenie pod konkretny czas misji: porównuj R(t) w horyzoncie eksploatacji, nie sam MTTF z katalogu."
  ),
  list(
    id = "wanna", title = "Krzywa wannowa to złożenie mechanizmów",
    lead = "Wczesne defekty, okres stabilny i zużycie tworzą trzy składowe hazardu.",
    intro = "Podręcznikowa krzywa wannowa — wysoki hazard na początku, płaski środek, wznoszący koniec — bywa błędnie przedstawiana jako „kształt rozkładu Weibulla”. Tymczasem pojedynczy Weibull ma hazard monotoniczny: może odtworzyć jedno ramię wanny, nigdy całą. Wanna powstaje z nałożenia trzech mechanizmów, z których każdy ma własny przebieg i własne lekarstwo.",
    sections = list(list(
      id = "mechanizmy", title = "Trzy mechanizmy, trzy interwencje",
      bullets = c(
        "wczesne defekty (hazard malejący) — może pomagać docieranie i kontrola odbiorcza; skuteczność wymaga sprawdzenia mechanizmu;",
        "awarie losowe (hazard stały) — pomaga redundancja i ochrona przed zaburzeniami zewnętrznymi;",
        "zużycie (hazard rosnący) — może pomagać wymiana profilaktyczna we właściwym momencie."
      )
    )),
    widget = risk_widget_panel("Mechanizmy", "Suma trzech składowych", sliderInput("c7_wear", "Nasilenie zużycia", .2, 2, 1, .1), "c7_bathtub", "c7_bathtub_stats"),
    takeaway = "Wanna jest kształtem hazardu, który można uzyskać przez nałożenie trzech mechanizmów: wczesnych defektów, awarii losowych i zużycia. Dlatego plan przeglądów oparty na jednym dopasowanym modelu może być trafny w środku życia elementu, a mylny na jego początku i końcu.",
    pitfall = "Pojedynczy Weibull ma hazard monotoniczny; nie tworzy pełnej krzywej wannowej."
  ),
  list(
    id = "przeglad", title = "Plan przeglądu",
    lead = "Czas interwencji wynika z wymaganego R(t), kosztów i mechanizmu awarii.",
    intro = c(
      "Pytanie utrzymaniowe brzmi konkretnie: po ilu godzinach zaplanować przegląd wentylatora, żeby ryzyko awarii przed przeglądem pozostało akceptowalne? Suwak poniżej liczy R(t) dla modelu zużyciowego Weibulla (β = 2, η = 1700 h) — przesuwaj czas przeglądu i obserwuj, jak rośnie ryzyko.",
      "Zauważ, że sensowność wymiany profilaktycznej zależy od mechanizmu: przy hazardzie rosnącym wcześniejsza wymiana naprawdę redukuje ryzyko, ale przy stałym hazardzie wymiana sprawnego elementu na nowy niczego nie zmienia — nowy ma dokładnie ten sam hazard co stary. Plan przeglądów bez hipotezy o hazardzie jest strzałem w ciemno."
    ),
    widget = figure_panel(label = "Decyzja", title = "Czy wentylator dotrwa do końca misji?", sliderInput("c7_plan_time", "Czas do przeglądu (h)", 100, 3000, 1000, 50), uiOutput("c7_plan"), full_width = TRUE),
    decision = "Podaj model, czas misji i prawdopodobieństwo dotrwania. Przegląd sam nie odnawia elementu: trzeba określić, co wykrywa i czy prowadzi do wymiany lub naprawy. MTTF samo nie wyznacza harmonogramu."
  ),
  list(
    id = "sciaga", title = "Ściąga",
    lead = "Czas → cenzorowanie → R(t) i h(t) → mechanizm → plan.",
    intro = "Zanim przejdziesz do quizu, sprawdź, czy umiesz odpowiedzieć na pięć pytań poniżej dla dowolnego elementu ze swojego otoczenia — od baterii w laptopie po pasek rozrządu. To one, a nie wzory, są szkieletem analizy czasu życia.",
    sections = list(list(id = "lista", title = "Pięć pytań", bullets = c("Co rozpoczyna i kończy czas życia?", "Jaki jest wspólny czas misji?", "Czy obserwacje działające są cenzorowane?", "Czy hazard jest stały, rośnie czy maleje?", "Jak wynik zmienia decyzję utrzymaniową?")), list(id = "most", title = "Co dalej", text = "Dotąd badaliśmy pojedynczy element. W następnym wykładzie połączymy funkcje niezawodności R_i(t) kilku elementów w niezawodność całego systemu — i okaże się, że wynik zależy nie tylko od elementów, ale i od architektury."))
  ),
  list(
    id = "sprawdzenie", title = "Quiz i ćwiczenia",
    lead = "Interpretuj funkcje czasu życia bez estymacji parametrów.",
    intro = "Quiz pyta o interpretacje — zwłaszcza o to, co naprawdę znaczy stały hazard — a ćwiczenia prowadzą od rachunku R(t) przez diagnozę cenzorowania po dobór kształtu Weibulla do mechanizmu.",
    widget = risk_assessment_ui("c7", zycie_quiz, zycie_exercises)
  )
))
zycie_chapters <- risk_block_chapters(zycie_block)

zycie_server <- function(input, output, session) {
  v <- reactiveVal(FALSE)
  observeEvent(input$c7_vote_check, v(TRUE))
  output$c7_vote_feedback <- renderUI({
    req(v())
    if (is.null(input$c7_vote)) {
      return(lc_feedback(type = "info", "Najpierw zaznacz jedną z odpowiedzi."))
    }
    lc_feedback(type = if (identical(input$c7_vote, "distribution")) "ok" else "warning", tags$strong("Nie."), " Rozkłady o tym samym MTTF mogą mieć odmienne R(t).")
  })
  times <- c(220, 480, 760, 990, 1350, 1750, 2300, 3100)
  timeline_plot <- reactive({
    obs <- pmin(times, input$c7_follow)
    status <- ifelse(times <= input$c7_follow, "Awaria", "Nadal działa — cenzorowanie")
    dat <- data.frame(id = factor(seq_along(times)), obs, status)
    ggplot(dat, aes(x = 0, xend = obs, y = id, yend = id, colour = status)) +
      geom_segment(linewidth = 2) +
      geom_point(aes(x = obs, shape = status), size = 3) +
      scale_colour_manual(values = c("Awaria" = upwr_accent, "Nadal działa — cenzorowanie" = upwr_secondary)) +
      labs(title = "Każdy element wnosi informację", x = "Czas (h)", y = "Element", colour = NULL, shape = NULL) +
      theme_upwr()
  })
  zoom_plot_server("c7_timeline", timeline_plot, alt = "Osiem linii czasu zakończonych awarią lub znacznikiem cenzorowania.")
  output$c7_timeline_stats <- renderUI(lc_stat_grid(lc_stat_box("Awarie", sum(times <= input$c7_follow)), lc_stat_box("Cenzorowane", sum(times > input$c7_follow)), columns = 1))
  functions_plot <- reactive({
    t <- seq(0, 4000, length.out = 400)
    e <- risk_exponential(t, 1 / 1500)
    dat <- rbind(data.frame(t, value = e$density * 3000, fun = "f(t) × 3000"), data.frame(t, value = e$cdf, fun = "F(t)"), data.frame(t, value = e$reliability, fun = "R(t)"), data.frame(t, value = e$hazard * 1500, fun = "h(t) × 1500"))
    ggplot(dat, aes(t, value, colour = fun)) +
      geom_line(linewidth = 1) +
      geom_vline(xintercept = input$c7_time, linetype = 2) +
      scale_colour_manual(values = upwr_cat_n(4)) +
      labs(title = "Cztery perspektywy na czas życia", x = "Czas (h)", y = "Wartość przeskalowana", colour = NULL) +
      theme_upwr()
  })
  zoom_plot_server("c7_functions", functions_plot, alt = "Cztery zsynchronizowane funkcje czasu życia ze wspólną linią czasu.")
  output$c7_functions_stats <- renderUI({
    e <- risk_exponential(input$c7_time, 1 / 1500)
    lc_stat_grid(lc_stat_box("F(t)", risk_format_probability(e$cdf)), lc_stat_box("R(t)", risk_format_probability(e$reliability), color = upwr_accent), columns = 1)
  })
  exp_plot <- reactive({
    t <- seq(0, 5000, length.out = 400)
    r <- risk_exponential(t, 1 / input$c7_mttf)$reliability
    ggplot(data.frame(t, r), aes(t, r)) +
      geom_line(colour = upwr_accent, linewidth = 1.1) +
      labs(title = "Niezawodność wykładnicza", x = "Czas (h)", y = "R(t)") +
      theme_upwr()
  })
  zoom_plot_server("c7_exp", exp_plot, alt = "Malejąca wykładnicza krzywa niezawodności.")
  output$c7_exp_stats <- renderUI(lc_stat_grid(lc_stat_box("R(1000 h)", risk_format_probability(exp(-1000 / input$c7_mttf)), color = upwr_accent), columns = 1))
  gamma_plot <- reactive({
    t <- seq(0, 6000, length.out = 400)
    ggplot(data.frame(t, p = dgamma(t, shape = input$c7_k, rate = 1 / 500)), aes(t, p)) +
      geom_line(colour = upwr_secondary, linewidth = 1.1) +
      labs(title = "Gęstość gamma: kształt k, skala 500 h", x = "Czas (h)", y = "Gęstość") +
      theme_upwr()
  })
  zoom_plot_server("c7_gamma", gamma_plot, alt = "Gęstość rozkładu gamma dla wybranego parametru kształtu.")
  output$c7_gamma_stats <- renderUI(lc_stat_grid(lc_stat_box("Średni czas E(T)", paste(input$c7_k * 500, "h")), lc_stat_box("Interpretacja kształtu", if (input$c7_k %% 1 == 0) paste0("Erlang: czas do ", input$c7_k, ". zdarzenia") else "ogólna gamma (bez etapów)"), columns = 1))
  weib_plot <- reactive({
    t <- seq(1, 5000, length.out = 500)
    w <- risk_weibull(t, input$c7_beta, input$c7_eta)
    dat <- rbind(data.frame(t, value = w$reliability, fun = "R(t)"), data.frame(t, value = w$hazard * input$c7_eta, fun = "h(t) × η"))
    ggplot(dat, aes(t, value, colour = fun)) +
      geom_line(linewidth = 1.05) +
      scale_colour_manual(values = upwr_cat_n(2)) +
      labs(title = "Niezawodność i hazard", x = "Czas (h)", y = "Wartość", colour = NULL) +
      theme_upwr()
  })
  zoom_plot_server("c7_weibull", weib_plot, alt = "Krzywe niezawodności i hazardu Weibulla sterowane parametrami beta i eta.")
  output$c7_weibull_stats <- renderUI(lc_stat_grid(lc_stat_box("Kierunek hazardu", if (input$c7_beta < 1) "maleje" else if (input$c7_beta > 1) "rośnie" else "stały"), lc_stat_box("R(1000 h)", risk_format_probability(risk_weibull(1000, input$c7_beta, input$c7_eta)$reliability), color = upwr_accent), columns = 1))
  same_plot <- reactive({
    t <- seq(0, 3500, length.out = 400)
    shapes <- c(.7, 1, 2.5)
    scales <- 1500 / gamma(1 + 1 / shapes)
    dat <- do.call(rbind, lapply(seq_along(shapes), function(i) data.frame(t, r = exp(-(t / scales[i])^shapes[i]), model = paste0("β=", shapes[i]))))
    ggplot(dat, aes(t, r, colour = model)) +
      geom_line(linewidth = 1) +
      geom_vline(xintercept = input$c7_mission, linetype = 2) +
      scale_colour_manual(values = upwr_cat_n(3)) +
      labs(title = "Ten sam MTTF, inne R(t)", x = "Czas (h)", y = "R(t)", colour = NULL) +
      theme_upwr()
  })
  zoom_plot_server("c7_same_mean", same_plot, alt = "Trzy krzywe Weibulla o tym samym średnim czasie życia i różnych kształtach.")
  output$c7_same_mean_stats <- renderUI(lc_feedback(type = "info", "Odczytaj trzy różne wartości na pionowej linii czasu misji."))
  bathtub_plot <- reactive({
    t <- seq(1, 4000, length.out = 500)
    early <- 1.2 * exp(-t / 350)
    stable <- rep(.12, length(t))
    wear <- input$c7_wear * (t / 4000)^3
    dat <- data.frame(t, early, stable, wear, total = early + stable + wear)
    long <- reshape(dat, varying = c("early", "stable", "wear", "total"), v.names = "hazard", timevar = "mechanizm", times = c("Wczesne defekty", "Losowe awarie", "Zużycie", "Suma"), direction = "long")
    ggplot(long, aes(t, hazard, colour = mechanizm)) +
      geom_line(aes(linewidth = mechanizm == "Suma")) +
      scale_linewidth_manual(values = c(`TRUE` = 1.3, `FALSE` = .7), guide = "none") +
      scale_colour_manual(values = upwr_cat_n(4)) +
      labs(title = "Wanna jako suma mechanizmów", x = "Czas", y = "Względny hazard", colour = NULL) +
      theme_upwr()
  })
  zoom_plot_server("c7_bathtub", bathtub_plot, alt = "Krzywa hazardu w kształcie wanny i jej trzy składowe.")
  output$c7_bathtub_stats <- renderUI(lc_feedback(type = "warning", "Zmiana mechanizmu wymaga innej interwencji utrzymaniowej."))
  output$c7_plan <- renderUI({
    r <- risk_weibull(input$c7_plan_time, 2, 1700)$reliability
    lc_stat_grid(lc_stat_box("R(t)", risk_format_probability(r), color = upwr_accent), lc_stat_box("Ryzyko awarii", risk_format_probability(1 - r)), columns = 1)
  })
  risk_assessment_server("c7", zycie_quiz, input, output)
}
