# Blok 06: Zmienność i próg ---------------------------------------------

prog_quiz <- list(question = "Która zmiana bezpośrednio zmniejsza P(T>85°C), gdy próg pozostaje stały?", choices = c("Obniżenie średniej lub odchylenia standardowego" = "both", "Zwiększenie średniej" = "mean", "Ignorowanie ogona rozkładu" = "ignore"), correct = "both", explanation = "Położenie i rozrzut rozkładu wspólnie wyznaczają pole za progiem.")
prog_exercises <- c("Bananpol: dla T~N(82,3) policz P(T>85°C) i naturalną częstość na 1000 zmian.", "Diagnostyka: porównaj histogram i wykres kwantylowy; wskaż, co podważa model normalny.", "Transfer: dla obciążenia i wytrzymałości konstrukcji policz ryzyko jako P(L>S), nie jako pole nakładania krzywych.")

prog_block <- list(id = "prog", title = "Zmienność i próg", chapters = list(
  list(
    id = "glosowanie", title = "Średnia poniżej progu",
    lead = "Bez informacji o zmienności średnia nie odpowiada na pytanie o przekroczenie.",
    intro = c(
      "Raport z dojrzewalni wygląda uspokajająco: średnia temperatura łożyska wentylatora to 82°C, a wewnętrzny próg ostrzegawczy ustalono na 85°C. Trzy stopnie zapasu — czy sprawa jest zamknięta? Zanim odpowiesz, przypomnij sobie, że średnia to jedna liczba opisująca setki pomiarów, z których każdy wypadł trochę inaczej.",
      "Ten wykład wprowadza zmienne ciągłe i rozkład normalny, ale jego prawdziwym tematem jest zmienność: dlaczego bez miary rozrzutu nie da się odpowiedzieć na żadne pytanie o przekroczenie progu."
    ),
    callout = list(
      label = "Dane Bananpolu",
      text = "Temperatura łożyska wentylatora: średnia 82°C, odchylenie standardowe 3°C, wewnętrzny próg ostrzegawczy 85°C. Jednostka: °C; pomiar w ustalonym trybie pracy. Próg jest demonstracyjny, a liczby fikcyjne.",
      color = "uwaga"
    ),
    widget = tagList(
      risk_vote_panel("z6_vote", "z6_vote_feedback", "Średnia temperatura wynosi 82°C, próg 85°C. Czy ryzyko jest pomijalne?", c("Tak" = "yes", "Nie — potrzebujemy rozrzutu" = "sd", "Zawsze wynosi 50%" = "half")),
      lc_h2("z6-histogram", "Od histogramu do pola"),
      lc_p("Histogram z wielu zmian przybliża kształt rozkładu, a prawdopodobieństwo przekroczenia jest polem — zobacz, jak obraz stabilizuje się wraz z liczbą obserwacji. To ciągły odpowiednik stabilizacji częstości z pierwszego wykładu: tam stabilizowała się jedna liczba, tutaj stabilizuje się cały kształt."),
      risk_widget_panel("Symulacja", "Histogram stabilizuje się wraz z liczebnością", sliderInput("z6_sample", "Liczba obserwacji", 30, 5000, 200, 10), "z6_hist", "z6_hist_stats")
    )
  ),
  list(
    id = "ciagla", title = "Zmienna dyskretna a ciągła",
    lead = "Temperatura nie jest liczbą zdarzeń — wymaga innego rodzaju rozkładu.",
    intro = "W wykładach o próbach zmienne losowe zliczały zdarzenia: zero, jedna, dwie wady. Temperatura łożyska nie zlicza niczego — może wynieść 82,1°C, 82,14°C albo dowolną wartość pomiędzy. To wymusza zmianę narzędzi: zamiast prawdopodobieństw pojedynczych wartości pracujemy z gęstością, a prawdopodobieństwa czytamy z pól pod krzywą.",
    sections = list(
      list(id = "kontrast", title = "Dwa rodzaje zmiennych", text = "Zmienna dyskretna, jak liczba niesprawnych czujników z wykładów 04–05, przyjmuje policzalne wartości i każdej z nich można przypisać dodatnie prawdopodobieństwo. Zmienna ciągła, jak temperatura łożyska, może przyjąć dowolną wartość z przedziału — wyników jest nieprzeliczalnie wiele."),
      list(id = "zero", title = "Dlaczego P(X=x)=0", text = "Dla zmiennej ciągłej prawdopodobieństwo trafienia dokładnie jednej wartości wynosi zero: pojedynczy punkt nie ma szerokości, więc pole nad nim znika. Sens mają dopiero prawdopodobieństwa przedziałów i przekroczeń, liczone jako pole pod krzywą gęstości.")
    ),
    formula = "P(a<X\\le b)=\\int_a^b f(x)\\,dx,\\qquad P(X=x)=0",
    pitfall = "Pytanie „jakie jest prawdopodobieństwo, że temperatura wyniesie dokładnie 85°C” nie ma użytecznej odpowiedzi; pytaj o przedział albo przekroczenie progu."
  ),
  list(
    id = "parametry", title = "Parametry μ i σ",
    lead = "μ przesuwa środek, σ rozszerza lub zwęża rozkład.",
    intro = c(
      "Rozkład normalny jest opisany dwiema liczbami o czytelnych rolach: μ mówi, gdzie leży środek, a σ — jak szeroko wyniki rozrzucają się wokół niego. Praktyczna linijka: około 68% wyników mieści się w przedziale μ±σ, około 95% w μ±2σ, a wyniki poza μ±3σ są rzadkością.",
      "Pobaw się suwakami i obserwuj wskaźnik z dla progu 85°C. Zauważ, że tę samą odległość od progu można osiągnąć chłodzeniem (mniejsze μ) albo stabilizacją pracy (mniejsze σ) — rozróżnienie, które wróci przy decyzjach."
    ),
    sections = list(list(id = "konwencja", title = "Konwencja zapisu", text = "W tym kursie zapis T~N(μ, σ) oznacza, że drugim parametrem jest odchylenie standardowe σ. W wielu podręcznikach ten sam rozkład zapisuje się jako N(μ, σ²) z wariancją na drugim miejscu — przed podstawieniem liczb zawsze sprawdź, którą konwencję przyjmuje źródło.")),
    formula = "T\\sim N(\\mu,\\sigma),\\qquad z=\\frac{t-\\mu}{\\sigma}",
    widget = risk_widget_panel("Model", "Przesuń i rozszerz krzywą", tagList(sliderInput("z6_mean", "μ (°C)", 75, 90, 82, .5), sliderInput("z6_sd", "σ (°C)", .5, 8, 3, .25)), "z6_normal", "z6_normal_stats")
  ),
  list(
    id = "standaryzacja", title = "Wspólna linijka z",
    lead = "Standaryzacja mówi, ile odchyleń standardowych dzieli wynik od średniej.",
    intro = c(
      "Czy 85°C przy średniej 82°C i σ = 3°C to dużo? A 62 bary ciśnienia przy średniej 56 i σ = 2? Porównanie surowych liczb z różnych światów jest niemożliwe — dopóki obu nie przełożymy na wspólną jednostkę: liczbę odchyleń standardowych od średniej.",
      "Dla progu łożyska z = (85−82)/3 = 1: próg leży jedno odchylenie nad średnią, co w modelu normalnym oznacza około 16% przekroczeń. Dla ciśnienia z = 3 — przekroczenia są rzadkością. Standaryzacja porządkuje priorytety, zanim padnie jakakolwiek decyzja."
    ),
    formula = "z=(x-\\mu)/\\sigma",
    sections = list(list(id = "jednostki", title = "Bez jednostki", text = "Po standaryzacji można porównywać temperaturę, ciśnienie i drgania, ale tylko w ramach sensownego modelu. Wynik z = 1 znaczy „jedno odchylenie nad średnią” zawsze; przełożenie tego na prawdopodobieństwo wymaga już założenia o kształcie rozkładu."))
  ),
  list(
    id = "ogon", title = "Część B — ryzyko przekroczenia",
    lead = "Próg dzieli rozkład na wyniki akceptowalne i przekroczenia.",
    intro = c(
      "Wracamy do pytania z głosowania, tym razem z pełnym warsztatem. Prawdopodobieństwo przekroczenia progu to pole pod gęstością na prawo od progu — dla T~N(82, 3) i progu 85°C około 0,16. W naturalnych częstościach: mniej więcej 159 na 1000 porównywalnych pomiarów.",
      "Zanim zapiszemy to wzorem, pobaw się progiem i obserwuj, jak pole reaguje nieliniowo: w okolicy średniej każda zmiana progu o pół stopnia silnie zmienia wynik, a daleko w ogonie te same pół stopnia znaczy niewiele. Ta nieliniowość to znak rozpoznawczy ogonów rozkładu normalnego."
    ),
    widget = tagList(
      risk_widget_panel("Ogon", "Próg temperatury łożyska", sliderInput("z6_threshold", "Próg (°C)", 78, 95, 85, .5), "z6_tail", "z6_tail_stats"),
      lc_p("To, co robił suwak — odcinał pole na prawo od progu — zapisujemy jedną linijką, korzystając ze standaryzacji z poprzedniego rozdziału:"),
      lc_formula_box(
        withMathJax("$$P(T>c)=1-\\Phi\\!\\left(\\frac{c-\\mu}{\\sigma}\\right)$$"),
        tags$p("Φ jest dystrybuantą standardowego rozkładu normalnego, a (c−μ)/σ to wynik z progu — odległość od średniej we wspólnej linijce odchyleń.")
      )
    ),
    takeaway = "Wynik progowy zawsze raportuj podwójnie: jako pole ogona i jako naturalną częstość w ustalonym horyzoncie. „P = 0,16” i „około 159 zmian na 1000” to ta sama liczba, ale tylko druga wersja uruchamia wyobraźnię decydenta."
  ),
  list(
    id = "dzialania", title = "Trzy działania na ogonie",
    lead = "Chłodzenie przesuwa średnią, stabilizacja zwęża rozkład, zmiana progu przesuwa granicę.",
    intro = c(
      "Pole ogona można zmniejszyć na trzy sposoby: obniżyć średnią, ograniczyć zmienność albo zmienić próg. Fizycznie to trzy zupełnie różne interwencje — lepsze chłodzenie, wyrównanie obciążenia i warunków pracy albo decyzja konstrukcyjna o nowej granicy.",
      "Porównaj skuteczność interwencji w widgecie. Zwróć uwagę, że wynik zależy od punktu wyjścia: gdy próg leży blisko średniej, więcej daje przesunięcie μ; gdy daleko w ogonie — zwężenie σ. Nie ma uniwersalnego zwycięzcy, jest rachunek."
    ),
    sections = list(list(
      id = "hierarchia", title = "Hierarchia interwencji",
      text = "Dwie pierwsze interwencje zmieniają mechanizm — po ich wdrożeniu instalacja naprawdę pracuje chłodniej albo stabilniej. Trzecia zmienia tylko definicję problemu: przekroczeń „ubywa”, choć fizycznie nic się nie poprawiło. Podniesienie progu bywa zasadne, ale wymaga dowodu konstrukcyjnego, że wyższa temperatura jest bezpieczna — nigdy samej potrzeby poprawienia statystyk."
    )),
    widget = figure_panel(label = "Porównanie", title = "Która interwencja najbardziej zmienia ogon?", selectInput("z6_action", "Działanie", c("Stan bazowy" = "base", "Chłodzenie: μ−2°C" = "mean", "Stabilizacja: σ−1°C" = "sd", "Wyższy próg: +2°C" = "threshold")), uiOutput("z6_action_result"), full_width = TRUE),
    decision = "Raportuj pole ogona oraz naturalną częstość w ustalonym horyzoncie; najpierw redukuj mechanizm, podniesienie progu wymaga uzasadnienia konstrukcyjnego."
  ),
  list(
    id = "obciazenie", title = "Obciążenie–wytrzymałość",
    lead = "Awaria zachodzi wtedy, gdy obciążenie L przekracza wytrzymałość S.",
    intro = c(
      "W konstrukcjach i instalacjach granica bezpieczeństwa rzadko jest stałą: wytrzymałość liny zmienia się z partią i zużyciem, a obciążenie z ładunkiem i pogodą. Pytanie o awarię staje się pytaniem o wyścig dwóch zmiennych losowych.",
      "Najpierw obejrzyj ten wyścig w symulacji: każdy punkt to jedna para obciążenie–wytrzymałość, a przekątna dzieli świat na pary bezpieczne i awarie. Przesuwaj obie średnie i obserwuj, jak chmura punktów przelewa się przez linię L = S."
    ),
    sections = list(
      list(id = "most", title = "Próg też bywa zmienny", text = "Dotąd próg był stałą konstrukcyjną, a zmienna była tylko temperatura. Teraz sam próg — wytrzymałość S — również jest zmienną losową, więc porównujemy dwie zmienne naraz i pytamy o prawdopodobieństwo, że jedna przewyższy drugą.")
    ),
    widget = tagList(
      risk_widget_panel("Symulacja", "Pary obciążenie–wytrzymałość", tagList(sliderInput("z6_load", "Średnie L", 60, 110, 85, 1), sliderInput("z6_strength", "Średnie S", 70, 120, 95, 1)), "z6_ls", "z6_ls_stats"),
      lc_p("Chmura punktów podpowiada właściwą miarę ryzyka: to udział par poniżej przekątnej, czyli prawdopodobieństwo, że różnica D = S − L wypadnie ujemna:"),
      lc_formula_box(
        withMathJax("$$P(\\text{awarii})=P(L>S)=P(D<0),\\qquad D=S-L$$"),
        tags$p("Dla niezależnych rozkładów normalnych różnica D też jest normalna: o średniej μ_S − μ_L i wariancji σ_S² + σ_L². Rachunek progowy z poprzednich rozdziałów stosuje się wtedy do D i progu zero.")
      ),
      lc_h2("prog-obciazenie-transfer", "Przykład transferowy: zawiesie dźwigu"),
      lc_p(
        "Zawiesie o średniej wytrzymałości 95 kN pracuje z ładunkami o średnim
         obciążeniu 85 kN. Dziesięć kilonewtonów zapasu wygląda solidnie, ale
         o ryzyku decydują rozrzuty obu wielkości: wystarczy ciężka partia
         ładunków i osłabiona partia zawiesi, żeby pary L > S przestały być
         teoretyczną ciekawostką. Normy konstrukcyjne mówią językiem kwantyli
         i współczynników bezpieczeństwa właśnie dlatego, że średnie nie
         wystarczają."
      )
    ),
    pitfall = "Pole nakładania dwóch gęstości nie jest prawdopodobieństwem L>S."
  ),
  list(
    id = "nienormalny", title = "Kiedy normalny zawodzi",
    lead = "Skośność i ciężki ogon mogą silnie zmienić ryzyko progowe mimo podobnej średniej i odchylenia.",
    intro = c(
      "Model normalny jest wygodny, ale nie jest prawem przyrody. Procesy z naturalną dolną granicą bywają skośne, a procesy z rzadkimi zaburzeniami mają ogony cięższe, niż przewiduje krzywa dzwonowa. Trzy rozkłady w widgecie mają zbliżone centrum — i wyraźnie różne ryzyko przekroczenia progu.",
      "Do diagnozy służy wykres kwantylowy: punkty na prostej oznaczają zgodność z modelem normalnym, a zagięcia na końcach — ogony inne niż normalne. To najtańsze narzędzie kontroli jakości modelu przed rachunkiem progowym."
    ),
    widget = risk_widget_panel("Rozszerzenie", "Trzy rozkłady o podobnym centrum", tagList(selectInput("z6_shape", "Kształt", c("Symetryczny" = "normal", "Skośny" = "skew", "Ciężki ogon" = "heavy")), radioButtons("z6_view", "Widok", c("Histogram" = "hist", "Wykres kwantylowy (Q–Q)" = "qq"))), "z6_shapes", "z6_shapes_stats"),
    extension = TRUE,
    pitfall = "Dopasowanie środka wykresu nie gwarantuje dobrego opisu ekstremów."
  ),
  list(
    id = "decyzja", title = "Decyzja progowa",
    lead = "Wynik powinien wskazywać mechanizm, horyzont i działanie.",
    intro = "Kompletny komunikat progowy mieści się w trzech zdaniach: jaka część wyników przekracza próg i w jakim horyzoncie, jaki mechanizm odpowiada za ogon, które działanie — chłodzenie, stabilizacja czy rewizja progu — rekomendujesz i dlaczego. Liczba bez mechanizmu nie wskazuje działania; działanie bez liczby nie ma uzasadnienia.",
    sections = list(list(id = "sciaga", title = "Ściąga", bullets = c("Pytanie: jaka część wyników przekracza próg?", "Model: rozkład zmiennej ciągłej", "Założenia: stabilność, kształt ogona, jednostki", "Wynik: P(X>c) i naturalna częstość", "Interpretacja: oczekiwane przekroczenia w porównywalnych ekspozycjach")), list(id = "most", title = "Co dalej", text = "Następny wykład zastosuje ten sam język gęstości, pola i ogona do szczególnej zmiennej ciągłej: czasu do awarii elementu.")),
    decision = "Najpierw redukuj mechanizm ryzyka; podniesienie progu wymaga uzasadnienia konstrukcyjnego."
  ),
  list(
    id = "sprawdzenie", title = "Quiz i ćwiczenia",
    lead = "Połącz wykres, rachunek i sens inżynierski.",
    intro = "Quiz sprawdza rozumienie mechanizmu — co naprawdę zmniejsza pole ogona — a ćwiczenia prowadzą przez pełny rachunek: od parametrów, przez standaryzację, po naturalną częstość i diagnozę modelu.",
    widget = risk_assessment_ui("z6", prog_quiz, prog_exercises)
  )
))
prog_chapters <- risk_block_chapters(prog_block)

prog_server <- function(input, output, session) {
  v <- reactiveVal(FALSE)
  observeEvent(input$z6_vote_check, v(TRUE))
  output$z6_vote_feedback <- renderUI({
    req(v())
    if (is.null(input$z6_vote)) {
      return(lc_feedback(type = "info", "Najpierw zaznacz jedną z odpowiedzi."))
    }
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
    if (identical(input$z6_view, "qq")) {
      ggplot(data.frame(x), aes(sample = x)) +
        stat_qq(colour = upwr_secondary, alpha = .4) +
        stat_qq_line(colour = upwr_accent, linewidth = 1) +
        labs(title = "Wykres kwantylowy względem rozkładu normalnego", x = "Kwantyle teoretyczne (normalne)", y = "Kwantyle próby (°C)") +
        theme_upwr()
    } else {
      ggplot(data.frame(x), aes(x)) +
        geom_histogram(bins = 60, fill = upwr_secondary, colour = "white") +
        geom_vline(xintercept = 85, colour = upwr_accent, linewidth = 1) +
        coord_cartesian(xlim = c(65, 105)) +
        labs(title = "Kształt ogona ma znaczenie", x = "Temperatura (°C)", y = "Liczba obserwacji") +
        theme_upwr()
    }
  })
  zoom_plot_server("z6_shapes", shapes_plot, alt = "Histogram albo wykres kwantylowy wybranego rozkładu względem modelu normalnego.")
  output$z6_shapes_stats <- renderUI(lc_feedback(type = "info", "Punkty układające się wzdłuż prostej na wykresie kwantylowym oznaczają zgodność z modelem normalnym; zagięcia w ogonach ostrzegają, że ocena przekroczeń może być błędna. Porównuj prawdopodobieństwo przekroczenia, nie tylko średnią i odchylenie."))
  risk_assessment_server("z6", prog_quiz, input, output)
}
