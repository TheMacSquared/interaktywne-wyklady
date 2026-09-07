# Blok 04: Wiele prób -----------------------------------------------------

proby_quiz <- list(questions = list(
  list(
  question = "Który warunek jest konieczny dla prostego modelu dwumianowego?",
  choices = c(
    "Stała liczba prób i to samo p w każdej próbie" = "fixed",
    "Rosnące p po każdej awarii" = "growing",
    "Co najmniej trzy możliwe wyniki próby" = "three"
  ),
  correct = "fixed",
  explanation = "Model wymaga ustalonego n, dwóch wyników, stałego p i niezależności prób."
),
  list(question = "Dla n=100 i p=0,02 ile wynosi E(X)?",
    choices = c("0,02" = "a", "98" = "b", "2" = "c"), correct = "c",
    explanation = "E(X)=np=2; pojedyncza partia może dać inny wynik."),
  list(question = "Jak policzyć co najmniej jedną wadę?",
    choices = c("p^n" = "a", "1−(1−p)^n" = "b", "np w każdym przypadku" = "c"), correct = "b",
    explanation = "Przez dopełnienie zdarzenia, że wszystkie próby zakończą się bez wady."),
  list(question = "Co oznacza P(X≤2) w planie akceptacji c=2?",
    choices = c("Prawdopodobieństwo przyjęcia partii" = "a", "Prawdopodobieństwo dokładnie dwóch wad" = "b", "Prawdopodobieństwo odrzucenia partii" = "c"), correct = "a",
    explanation = "Przyjmujemy partię, gdy liczba wad nie przekracza limitu."),
  list(question = "Zero wad w 100 niezależnych próbach przy ustalonym n. Jaki wniosek jest poprawny?",
    choices = c("Udowodniono p=0" = "a", "Następna partia na pewno nie ma wad" = "b", "Górna jednostronna granica 95% dla p wynosi około 0,0295" = "c"), correct = "c",
    explanation = "Niezaobserwowanie wady pozostawia niepewność parametru; granica wynika z (1−p)^100=0,05.")
))
proby_exercises <- c(
  "Bananpol: dla n=100 i p=0,02 policz P(X=0), P(X=2) i P(X≥1).",
  "Diagnostyka: partia pochodzi z dwóch dostaw o różnej jakości. Które założenie modelu dwumianowego jest zagrożone?",
  "Transfer: zdefiniuj próbę, sukces i n dla kontroli 30 mocowań rusztowania."
)

proby_variable_widget <- figure_panel(
  label = "Słownik",
  title = "Jedna kontrola zaworu jako zmienna losowa",
  full_width = TRUE,
  tags$table(
    class = "lc-table lc-table-striped lc-table-bordered",
    tags$thead(tags$tr(
      tags$th("Wynik kontroli"), tags$th("Wartość Xᵢ"), tags$th("Prawdopodobieństwo")
    )),
    tags$tbody(
      tags$tr(tags$td("Zawór niesprawny"), tags$td("1"), tags$td("p = 0,02")),
      tags$tr(tags$td("Zawór sprawny"), tags$td("0"), tags$td("1 − p = 0,98"))
    )
  ),
  lc_stat_grid(
    lc_stat_box("E(Xᵢ)", "p = 0,02", caption = "średni wynik jednej próby", color = upwr_accent),
    lc_stat_box("Suma X₁ + … + X₁₀₀", "licznik niesprawnych", caption = "zmienna, o którą naprawdę pytamy", color = upwr_cat[["niebo"]]),
    columns = 2
  ),
  lc_feedback(
    type = "info",
    tags$strong("Po co ta konstrukcja:"),
    " zdarzeń nie da się dodawać, ale liczby już tak. Zapisanie wyniku próby
      jako 0/1 pozwala sumować kontrole, liczyć średnie i budować rozkłady —
      cała dalsza część kursu stoi na tym pomoście."
  )
)

proby_sciaga_widget <- tagList(
  figure_panel(
    label = "Ściąga 4.1",
    title = "Pytania i odpowiedzi modelu dwumianowego",
    full_width = TRUE,
    tags$table(
      class = "lc-table lc-table-striped lc-table-bordered",
      tags$thead(tags$tr(tags$th("Pytanie"), tags$th("Zapis"), tags$th("Narzędzie"))),
      tags$tbody(
        tags$tr(tags$td("Dokładnie k niesprawnych"), tags$td("P(X = k)"), tags$td("wzór dwumianowy")),
        tags$tr(tags$td("Co najwyżej k"), tags$td("P(X ≤ k)"), tags$td("suma od 0 do k")),
        tags$tr(tags$td("Co najmniej k"), tags$td("P(X ≥ k) = 1 − P(X ≤ k−1)"), tags$td("dopełnienie")),
        tags$tr(tags$td("Co najmniej jedna"), tags$td("P(X ≥ 1) = 1 − (1−p)ⁿ"), tags$td("dopełnienie zera zdarzeń")),
        tags$tr(tags$td("Typowa liczba"), tags$td("E(X) = np"), tags$td("środek rozkładu, nie prognoza"))
      )
    )
  ),
  risk_assessment_ui("p4", proby_quiz, proby_exercises)
)

proby_block <- list(id = "proby", title = "Wiele prób", chapters = list(
  list(
    id = "jednostka", title = "Co jest pojedynczą próbą?",
    lead = "Najpierw ustalamy jednostkę ekspozycji i wynik 0/1.",
    intro = c(
      "Nowy dostawca przysłał do Bananpolu partię stu zaworów do instalacji chłodniczej. Zanim policzysz cokolwiek, musisz zdecydować, co jest pojedynczą próbą i jaki wynik uznajesz za zdarzenie — od tej decyzji zależy każda dalsza liczba w analizie.",
      "Dotychczas pytaliśmy o pojedynczą zmianę albo pojedynczy alarm. Dziś zmieniamy skalę: interesuje nas cała seria porównywalnych prób i liczba zdarzeń, które się w niej pojawią. Ale seria jest dobra tylko wtedy, gdy jej elementy naprawdę są porównywalne — dlatego zaczynamy od definicji próby, nie od wzoru."
    ),
    callout = list(
      label = "Dane Bananpolu",
      text = "Kontrola przyjęcia dostawy: partia 100 zaworów, prawdopodobieństwo niesprawności pojedynczego zaworu 0,02. Jednostka: kontrola jednego elementu; horyzont: jedna partia kontrolna. Liczby są fikcyjne.",
      color = "uwaga"
    ),
    sections = list(list(
      id = "kryteria", title = "Dobra próba ma trzy cechy",
      bullets = c(
        "jest jednoznacznie wyodrębniona — wiadomo, gdzie kończy się jedna, a zaczyna druga;",
        "ma dokładnie dwa rozłączne wyniki, nazwane przed obserwacją;",
        "jest porównywalna z pozostałymi — ten sam typ elementu, ta sama procedura kontroli."
      )
    )),
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
    id = "bernoulli", title = "Linia Bernoulliego i cztery założenia",
    lead = "Pojedyncze wyniki są losowe, choć długookresowa częstość jest stabilna — o ile sytuacja spełnia założenia modelu.",
    intro = c(
      "Serię prób o dwóch wynikach, stałym p i wzajemnej niezależności nazywamy schematem Bernoulliego. To najprostszy generator losowości w tym kursie — i fundament trzech rozkładów, które poznasz w tym i następnym wykładzie.",
      "Uruchom serię kontroli kilka razy. Wzór kropek za każdym razem będzie inny: czasem niesprawne zawory pojawią się parami, czasem długo nie będzie żadnego. Losowość lokalna i stabilność globalna nie wykluczają się — to dwie strony tego samego schematu."
    ),
    widget = tagList(
      figure_panel(
        label = "Symulacja", title = "Seria kontroli zaworów",
        sliderInput("p4_series_n", "Liczba kontroli", 10, 200, 100, 10),
        actionButton("p4_run", "Uruchom serię", class = "lc-btn-primary"),
        verbatimTextOutput("p4_sequence"), uiOutput("p4_series_stats"), full_width = TRUE
      ),
      lc_h2("p4-zalozenia", "Cztery założenia"),
      lc_p("Dwumianowy jest modelem sytuacji, nie tylko wzorem. Zanim go użyjesz, sprawdź listę kontrolną:"),
      tags$ul(
        tags$li("n jest ustalone przed obserwacją"),
        tags$li("każda próba ma dwa rozłączne wyniki"),
        tags$li("p jest stałe"),
        tags$li("wyniki prób są niezależne")
      ),
      lc_p(
        "Każde z tych założeń psuje się w rozpoznawalny sposób. Dwie dostawy o
         różnej jakości wymieszane w jednej partii łamią stałość p. Wada, która
         uszkadza sąsiednie zawory w transporcie, łamie niezależność. Kontroler,
         który po znalezieniu wady zaczyna sprawdzać dokładniej, zmienia samą
         definicję próby w trakcie serii. Wybierz scenariusz poniżej i sprawdź
         diagnozę."
      ),
      figure_panel(
        label = "Diagnoza", title = "Scenariusz partii",
        selectInput("p4_scenario", "Sytuacja", c("Jedna stabilna linia" = "stable", "Dwie dostawy o różnym p" = "mixture", "Uszkodzenie zwiększa ryzyko następnego" = "dependent")),
        uiOutput("p4_scenario_feedback"), full_width = TRUE
      )
    ),
    pitfall = "Duża partia nie naprawia złej definicji próby ani zmiennego p."
  ),
  list(
    id = "zmienna", title = "Zmienna losowa: od zdarzeń do liczb",
    lead = "Funkcja przypisująca wynikom liczby jest pomostem między „co może się zdarzyć” a „ile tego będzie”.",
    intro = c(
      "W pierwszym wykładzie zdarzenia były zbiorami: podzbiorami przestrzeni wyników. Zbiorów nie da się jednak dodawać ani uśredniać, a inspektor chce właśnie tego — policzyć niesprawne zawory w partii i porównać partie między sobą. Potrzebny jest pomost od zdarzeń do arytmetyki.",
      "Tym pomostem jest zmienna losowa: funkcja, która każdemu wynikowi doświadczenia przypisuje liczbę. Dla jednej kontroli zaworu przypisujemy 1, gdy zawór jest niesprawny, i 0, gdy sprawny. Rozkład zmiennej losowej mówi, które wartości i z jakim prawdopodobieństwem może ona przyjąć — dla pojedynczej próby to najprostszy rozkład tego kursu, rozkład Bernoulliego."
    ),
    sections = list(list(
      id = "suma", title = "Licznik jest sumą zer i jedynek",
      text = "Zapis 0/1 wygląda niepozornie, ale robi całą robotę: liczba niesprawnych zaworów w partii to po prostu suma X = X₁ + … + X₁₀₀. Pytanie „ile zdarzeń w n próbach?” stało się pytaniem o rozkład sumy zmiennych losowych — i na to pytanie odpowie następny rozdział."
    )),
    formula = "P(X_i=1)=p,\\qquad P(X_i=0)=1-p,\\qquad E(X_i)=p",
    widget = proby_variable_widget,
    takeaway = "Zmienna losowa nie jest ani zmienną z algebry, ani niewiadomą — jest funkcją na przestrzeni wyników. Jej wartość poznajemy dopiero po doświadczeniu, ale jej rozkład znamy przed nim."
  ),
  list(
    id = "rozklad", title = "Rozkład liczby awarii",
    lead = "Rozkład odpowiada na pytania dokładnie, co najmniej i najwyżej.",
    intro = c(
      "Suma stu prób Bernoulliego o stałym p i wzajemnej niezależności ma rozkład dwumianowy. Współczynnik dwumianowy zlicza, na ile sposobów k niesprawnych zaworów może rozmieścić się wśród n kontroli — a reszta wzoru to znany z wykładu o warunkach iloczyn wzdłuż drogi.",
      "W praktyce inspektora rzadko potrzebne jest „dokładnie k”. Pytania decyzyjne mają formę ogonową: co najmniej jedna wada (czy partia jest podejrzana?), co najwyżej dwie (czy mieścimy się w limicie akceptacji?). Przełącznik pytania w widgecie zmienia zaznaczony obszar rozkładu — obserwuj, jak zmienia się wynik."
    ),
    sections = list(list(
      id = "most", title = "Od jednej próby do licznika",
      text = "Poprzedni rozdział zbudował pomost: wynik każdej kontroli to zmienna Xᵢ o wartościach 0/1, a licznik niesprawnych w partii to suma X = X₁ + … + Xₙ. Właśnie ta suma — licznik zdarzeń w n niezależnych próbach o stałym p — ma rozkład dwumianowy."
    )),
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
    intro = c(
      "Ile wad będzie w najbliższej partii stu zaworów przy p = 0,02? Kusząca odpowiedź — „dwie, przecież 100 razy 0,02 to 2” — jest błędna w sposób, który najlepiej zobaczyć na własne oczy: pojedyncza partia równie dobrze może mieć zero wad, co pięć.",
      "Symulacja poniżej losuje setki partii przy tych samych parametrach. Zanim spojrzysz na jakikolwiek wzór, obejrzyj histogram: gdzie leży jego środek, jak szeroko rozrzucają się wyniki i jak często zdarza się dokładnie ta „oczekiwana” liczba wad."
    ),
    widget = tagList(
      risk_widget_panel(
        "Powtórzenia", "Wiele partii przy tych samych parametrach",
        sliderInput("p4_batches", "Liczba partii", 50, 2000, 500, 50), "p4_batches_plot", "p4_batches_stats"
      ),
      lc_p("Środek tego histogramu i jego szerokość mają zwięzły zapis:"),
      lc_formula_box(
        withMathJax("$$E(X)=np,\\qquad Var(X)=np(1-p)$$"),
        tags$p("Wartość oczekiwana opisuje środek ciężkości wielu porównywalnych partii; wariancja — rozrzut wyników wokół tego środka.")
      )
    ),
    takeaway = "Wynik jednej partii może wyraźnie różnić się od wartości oczekiwanej: przy stałym p raz zobaczysz zero niesprawnych, innym razem kilka. E(X)=np opisuje środek tego histogramu, nie obietnicę dla konkretnej partii. Dopiero rozkład wyników wielu porównywalnych partii pokazuje, które liczebności są typowe, a które powinny skłonić do sprawdzenia modelu."
  ),
  list(
    id = "co-najmniej-jedna", title = "Co najmniej jedna",
    lead = "Łatwiej policzyć zdarzenie przeciwne: ani jednej niesprawności.",
    intro = c(
      "Pytanie „czy w partii jest co najmniej jedna wada?” obejmuje mnóstwo scenariuszy: jedna wada, dwie, trzy… aż po sto. Zamiast sumować je wszystkie, liczymy jedno zdarzenie przeciwne — ani jednej wady — i odejmujemy od jedności. To najczęstszy trik rachunkowy analizy ryzyka.",
      "Krzywa poniżej pokazuje konsekwencję, którą łatwo przeoczyć: nawet bardzo małe p przy dużej liczbie prób daje niemal pewne zdarzenie. Rzadkość pojedynczej próby nie chroni długiej serii."
    ),
    sections = list(list(
      id = "transfer", title = "Przykład transferowy: ekspozycja skumulowana",
      text = "Ta sama krzywa opisuje każdą powtarzaną ekspozycję. Codzienny przejazd o ryzyku kolizji 0,0001 na przejazd daje po dziesięciu latach pracy (około 2500 przejazdów) ponad 20% szans co najmniej jednej kolizji. Wniosek dla profilaktyki: komunikaty „to zdarza się rzadko” trzeba zawsze uzupełniać horyzontem — rzadko na próbę nie znaczy rzadko w karierze."
    )),
    formula = "P(X\\ge 1)=1-P(X=0)=1-(1-p)^n",
    widget = risk_widget_panel(
      "Krzywa", "Ryzyko wraz z liczbą prób",
      sliderInput("p4_curve_p", "p niesprawności", .001, .10, .02, .001), "p4_one", "p4_one_stats"
    )
  ),
  list(
    id = "decyzja", title = "Decyzja kontrolna",
    lead = "Plan kontroli łączy ryzyko partii z regułą akceptacji.",
    intro = c(
      "Rachunek dwumianowy staje się decyzją w planie odbioru partii: losujemy n elementów i akceptujemy dostawę, jeżeli liczba niesprawnych nie przekracza limitu c. Para (n, c) wyznacza dwie krzywe ryzyka — szansę odrzucenia dobrej partii i szansę przyjęcia złej.",
      "Nie ma planu doskonałego: zaostrzenie limitu chroni magazyn, ale częściej odrzuca przyzwoite dostawy; złagodzenie działa odwrotnie. Dlatego plan kontroli jest decyzją negocjowaną z dostawcą i zapisaną przed pierwszą kontrolą, a nie dobieraną po obejrzeniu wyników."
    ),
    sections = list(list(
      id = "plan", title = "Co należy zapisać?",
      bullets = c("wielkość losowanej próby", "dopuszczalna liczba niesprawnych", "p reprezentujące jakość partii", "konsekwencję odrzucenia i przeoczenia")
    )),
    widget = figure_panel(label = "Od danych do modelu", title = "Zero wad w stu kontrolach", full_width = TRUE,
      lc_p("Jeśli p nie podano, szacujemy je z próby. Zero wad w 100 niezależnych kontrolach daje oszacowanie punktowe 0, ale dokładna jednostronna górna granica ufności 95% wynosi 1−0,05^(1/100)≈0,0295. Przy tej wartości szansa zobaczenia zera wynosi jeszcze 5%. Założenia obejmują stałe p i ustaloną z góry liczebność próby."),
      lc_p("Dla następnej partii 100 elementów podstawienie oszacowania p=0 daje prognozę P(co najmniej jednej wady)=0, natomiast podstawienie górnej granicy daje 0,95. To wrażliwość prognozy na niepewność p, a nie 95-procentowe prawdopodobieństwo awarii partii. Losowość nowej partii i niepewność oszacowania to dwa różne źródła niepewności.")
    ),
    decision = "Porównaj kilka jakości partii, zanim wybierzesz n i limit akceptacji."
  ),
  list(
    id = "sprawdzenie", title = "Ściąga i sprawdzenie",
    lead = "Jednostka → założenia → Bin(n,p) → pytanie ogonowe → decyzja.",
    intro = "Model dwumianowy jest pierwszym „gotowym” rozkładem w kursie i łatwo go nadużyć: wystarczy przeoczyć zmienne p albo zależność prób. Ściąga zbiera pytania i zapisy; quiz oraz ćwiczenia sprawdzają, czy potrafisz zarówno policzyć wynik, jak i zauważyć, kiedy liczyć nie wolno.",
    sections = list(list(id = "sciaga", title = "Ściąga", bullets = c("Pytanie: ile zdarzeń w n próbach?", "Model: dwumianowy", "Założenia: stałe n i p, dwa wyniki, niezależność", "Wynik: prawdopodobieństwo liczby zdarzeń", "Interpretacja: dotyczy powtarzalnych partii")), list(
      id = "most", title = "Co dalej",
      text = "Dwumianowy zatrzymuje się po ustalonej liczbie prób i pyta o liczbę zdarzeń. W następnym wykładzie odwrócimy regułę zatrzymania: ustalimy liczbę zdarzeń, a losowa stanie się liczba prób potrzebnych, żeby je zaobserwować."
    )),
    widget = proby_sciaga_widget
  )
))

proby_chapters <- risk_block_chapters(proby_block)

proby_server <- function(input, output, session) {
  vote <- reactiveVal(FALSE)
  observeEvent(input$p4_vote_check, vote(TRUE))
  output$p4_vote_feedback <- renderUI({
    req(vote())
    if (is.null(input$p4_vote)) {
      return(lc_feedback(type = "info", "Najpierw zaznacz jedną z odpowiedzi."))
    }
    lc_feedback(type = if (identical(input$p4_vote, "valve")) "ok" else "warning", tags$strong("Jednostka:"), " jeden zawór i dwa rozłączne wyniki.")
  })
  series <- reactive({
    input$p4_run
    rbinom(input$p4_series_n, 1, .02)
  })
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
