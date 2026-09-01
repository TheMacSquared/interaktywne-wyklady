# Blok 08: Niezawodność systemu -----------------------------------------

system_quiz <- list(question = "Dla dwóch niezależnych gałęzi równoległych system zawodzi, gdy…", choices = c("zawiodą obie gałęzie" = "both", "zawiedzie dowolna jedna" = "one", "zawsze po średnim czasie życia" = "mean"), correct = "both", explanation = "W układzie równoległym sukces wymaga co najmniej jednej działającej gałęzi.")
system_exercises <- c("Bananpol: policz R systemu szeregowego dla R1=0,92, R2=0,95 i R3=0,98.", "Diagnostyka: wskaż, dlaczego wspólne zasilanie podważa zwykły rachunek redundancji.", "Transfer: zapisz logikę sukcesu systemu hamulcowego z dwiema niezależnymi gałęziami i wspólnym sterownikiem.")

system_block <- list(id = "system", title = "Niezawodność systemu", chapters = list(
  list(
    id = "intuicja", title = "Te same elementy, trzy odpowiedzi",
    lead = "Niezawodność systemu zależy od logiki sukcesu, nie tylko od listy części.",
    intro = c(
      "Nocna awaria chłodzenia dojrzewalni. Rano trzy osoby podają trzy różne wartości niezawodności instalacji — i każda potrafi obronić swoją liczbę. To nie są trzy odpowiedzi dla tego samego systemu, lecz odpowiedzi dla trzech różnych definicji sukcesu.",
      "Do tej pory każdy element — wentylator, czujnik, zasilanie — miał własną niezawodność R(t). Ten wykład skleja te liczby w jedną: niezawodność systemu. Zaskoczenie polega na tym, że wynik zależy od architektury co najmniej tak mocno jak od jakości części."
    ),
    callout = list(
      label = "Dane Bananpolu",
      text = "Instalacja chłodzenia dojrzewalni: R wentylatora 0,92, R czujnika 0,95, R zasilania 0,98 — wszystkie dla wspólnego czasu misji 1000 h. P utraty wspólnego zasilania: 0,01. Liczby są fikcyjne.",
      color = "uwaga"
    ),
    widget = risk_vote_panel("s8_vote", "s8_vote_feedback", "Dwa elementy mają R=0,9. Czy R systemu wynosi 0,81, 0,9 czy 0,99?", c("0,81" = "series", "0,90" = "single", "0,99" = "parallel"))
  ),
  list(
    id = "definicja", title = "Sukces i wspólny czas misji",
    lead = "Najpierw definiujemy, co system ma zrobić i przez jak długi czas.",
    intro = c(
      "„System działa” to zdanie bez treści, dopóki nie powiemy, co dokładnie ma robić: utrzymywać temperaturę poniżej progu? podnieść alarm w ciągu minuty? pracować bez przerwy przez tysiąc godzin? Każda definicja sukcesu wyznacza inny zbiór wymaganych elementów — i inną liczbę na końcu rachunku.",
      "Drugi filar to wspólny czas misji. Niezawodność 0,92 „na tysiąc godzin” i 0,95 „na rok” nie są liczbami z tej samej analizy; ich iloczyn nie znaczy nic. Zanim pomnożysz cokolwiek, sprowadź wszystkie R_i do jednego horyzontu."
    ),
    sections = list(list(id = "check", title = "Kontrakt modelu", bullets = c("jednoznaczna funkcja systemu", "ten sam horyzont dla wszystkich R_i", "stany elementów adekwatne do funkcji", "jawne zależności i wspólne zasoby"))),
    pitfall = "Nie wolno mnożyć niezawodności podanych dla różnych czasów misji."
  ),
  list(
    id = "schemat", title = "Schemat blokowy systemu",
    lead = "Schemat niezawodnościowy pokazuje drogi sukcesu: system działa, gdy istnieje ciągła droga od wejścia do wyjścia przez działające bloki.",
    intro = c(
      "Zanim pojawią się wzory, narysujmy logikę. Schemat blokowy niezawodności (RBD) czyta się jednym pytaniem: czy da się przejść od lewej do prawej krawędzi wyłącznie przez działające bloki? Bloki ustawione w szereg muszą działać wszystkie; bloki w równoległych gałęziach zastępują się nawzajem.",
      "Przełącz trzy architektury zbudowane z tych samych elementów. Fizycznie nic się nie zmienia — te same urządzenia stoją w tej samej hali. Zmienia się wyłącznie to, które z nich są wymagane naraz, a które mogą się zastąpić."
    ),
    widget = risk_widget_panel("Schemat", "Trzy architektury tych samych elementów", selectInput("s8_diagram", "Układ", c("Szeregowy" = "series", "Równoległy" = "parallel", "Mieszany" = "mixed")), plot_id = "s8_diagram_plot", note = "Blok to element, linia to wymaganie drogi sukcesu; schemat opisuje logikę niezawodności, nie fizyczne połączenia."),
    pitfall = "Schemat niezawodnościowy nie musi pokrywać się ze schematem instalacji: dwa fizycznie odległe urządzenia mogą tworzyć jeden szereg logiczny."
  ),
  list(
    id = "szereg", title = "Układ szeregowy",
    lead = "System działa tylko wtedy, gdy działają wszystkie wymagane elementy.",
    intro = "Czujnik wykrywa przegrzanie, sterownik przetwarza sygnał, wentylator chłodzi. Wystarczy, że zawiedzie jedno ogniwo, a funkcja chłodzenia znika — to definicja układu szeregowego. Zanim pojawi się wzór, sprawdź w przełączniku stanów, które kombinacje utrzymują system przy życiu.",
    widget = tagList(
      figure_panel(label = "Stany", title = "Wszystkie muszą działać", checkboxGroupInput("s8_series_states", "Działające elementy", choices = c("Czujnik" = "sensor", "Sterownik" = "controller", "Wentylator" = "fan"), selected = c("sensor", "controller", "fan")), uiOutput("s8_series_state"), full_width = TRUE),
      lc_p("Kombinacja wygrywająca jest dokładnie jedna: wszyscy działają. Skoro system wymaga wszystkich elementów naraz, a awarie są niezależne, prawdopodobieństwa działania mnożą się wzdłuż łańcucha:"),
      lc_formula_box(withMathJax("$$R_s=\\prod_i R_i$$")),
      lc_p(
        "Iloczyn liczb mniejszych od jedności maleje z każdym czynnikiem, więc
         długie szeregi są bezlitosne: dziesięć elementów po R = 0,95 daje
         systemowe R ≈ 0,60. W układzie szeregowym system jest zawsze gorszy od
         najsłabszego elementu — a dotkliwość tej straty rośnie z długością
         łańcucha."
      )
    ),
    pitfall = "Iloczyn R_i zakłada niezależność elementów; wspólne zasoby i zależności trzeba dodać do modelu jawnie — wrócimy do tego przy wspólnej przyczynie."
  ),
  list(
    id = "rownolegle", title = "Układ równoległy",
    lead = "Liczymy przez zdarzenie przeciwne: awarię wszystkich gałęzi.",
    intro = "Drugi wentylator zamontowany obok pierwszego niczego nie chłodzi lepiej — czeka. System równoległy działa, dopóki działa co najmniej jedna gałąź, więc zawodzi tylko wtedy, gdy zawiodą wszystkie naraz. Przełącz stany gałęzi i znajdź jedyną kombinację, która kładzie system.",
    widget = tagList(
      figure_panel(label = "Stany", title = "Co najmniej jedna gałąź musi działać", checkboxGroupInput("s8_parallel_states", "Działające wentylatory", choices = c("A" = "a", "B" = "b"), selected = c("a", "b")), uiOutput("s8_parallel_state"), full_width = TRUE),
      lc_p("Tym razem przegrywająca kombinacja jest jedna — i to jest zaproszenie do triku z dopełnieniem, znanego z wykładu o wielu próbach: zamiast wielu scenariuszy sukcesu liczymy jeden scenariusz porażki."),
      lc_formula_box(withMathJax("$$R_p=1-\\prod_i(1-R_i)$$")),
      lc_p(
        "Przy niezależnych gałęziach prawdopodobieństwa awarii się mnożą —
         dwie gałęzie po R = 0,9 dają awarię systemu 0,1 · 0,1 = 0,01,
         czyli R = 0,99."
      )
    ),
    pitfall = "Wzór 1−∏(1−R_i) zakłada niezależność awarii gałęzi; wspólna przyczyna potrafi zniweczyć redundancję."
  ),
  list(
    id = "przelacznik", title = "Przełącznik architektury",
    lead = "Dla tych samych elementów zmienia się logika, wzór i wynik.",
    intro = "Ten widget to eksperyment kontrolowany: dwa elementy o ustalonych niezawodnościach i jeden przełącznik logiki. Zanim klikniesz, oszacuj: o ile system równoległy będzie lepszy od szeregowego przy R₁ = 0,92 i R₂ = 0,95? Potem sprawdź, jak różnica reaguje na pogorszenie jednego z elementów.",
    widget = risk_widget_panel("Architektura", "Te same R, inny system", tagList(selectInput("s8_arch", "Układ", c("Szeregowy" = "series", "Równoległy" = "parallel")), sliderInput("s8_r1", "R₁", .5, 1, .92, .01), sliderInput("s8_r2", "R₂", .5, 1, .95, .01)), "s8_arch_plot", "s8_arch_stats"),
    takeaway = "Suwaki się nie zmieniły — zmieniła się tylko logika sukcesu. To najważniejsza obserwacja tego wykładu: lista części nie wyznacza niezawodności, dopóki nie powiemy, które z nich naprawdę muszą działać razem."
  ),
  list(
    id = "mieszany", title = "Część B — układ mieszany i systemowa R(t)",
    lead = "Redukujemy najpierw gałęzie równoległe, a potem łączymy wynik z elementem szeregowym.",
    intro = c(
      "Prawdziwe instalacje rzadko są czystym szeregiem albo czystą redundancją. Chłodzenie dojrzewalni to sterownik (wymagany zawsze) i dwa wentylatory (zastępowalne). Takie układy liczy się przez redukcję: zwiń każdą grupę równoległą do jednego zastępczego bloku, a potem pomnóż powstały szereg.",
      "Redukcja działa także w czasie: podstawiając R_i(t) z wykładu o czasie życia, otrzymujemy krzywą niezawodności całego systemu. Zwróć uwagę na jej położenie — system nigdy nie jest lepszy od najsłabszego wymaganego szeregu, a przewaga redundancji topnieje z czasem misji."
    ),
    widget = tagList(
      figure_panel(label = "Krok po kroku", title = "Sterownik C oraz wentylatory A/B", actionButton("s8_step", "Pokaż następny krok", class = "lc-btn-primary"), uiOutput("s8_reduction"), full_width = TRUE),
      lc_p("Trzy kroki redukcji, które właśnie przeszliśmy, składają się w gotowy wzór całego układu:"),
      lc_formula_box(
        withMathJax("$$R_{sys}=R_C\\,[1-(1-R_A)(1-R_B)]$$"),
        tags$p("Nawias kwadratowy to zredukowany blok równoległy wentylatorów; sterownik mnoży go szeregowo.")
      ),
      lc_h2("s8-czas", "Systemowa R(t)"),
      lc_p("Ta sama redukcja działa w czasie: krzywe elementów i całego systemu muszą używać wspólnego czasu misji, a krzywa systemu zawsze leży poniżej najsłabszego wymaganego szeregu."),
      risk_widget_panel("Czas", "Elementy i system mieszany", sliderInput("s8_mission", "Czas misji (h)", 100, 3000, 1000, 50), "s8_time_plot", "s8_time_stats")
    )
  ),
  list(
    id = "struktura", title = "Funkcja struktury i systemy koherentne",
    lead = "Logikę sukcesu, którą klikaliśmy w poprzednich rozdziałach, można zapisać jedną funkcją.",
    intro = "Checkboksy z poprzednich rozdziałów wykonywały w tle prostą matematykę: brały wektor stanów elementów i zwracały stan systemu. Ta operacja ma nazwę — funkcja struktury φ — i zapisuje architekturę bez ani jednego prawdopodobieństwa. Rozdzielenie logiki (φ) od liczb (R_i) to porządek, który za wykład wróci w drzewach błędów.",
    sections = list(
      list(id = "definicja", title = "Zapis funkcji struktury", text = "Stan elementu i zapisujemy jako x_i: 1 gdy działa, 0 gdy zawiódł. Funkcja struktury φ przypisuje wektorowi stanów elementów stan całego systemu. Przełączniki stanów w rozdziałach o układach robiły dokładnie to: dla szeregu φ(x)=x₁x₂⋯xₙ, dla układu równoległego φ(x)=1−(1−x₁)⋯(1−xₙ), a dla naszego układu mieszanego φ(x)=x_C·[1−(1−x_A)(1−x_B)]."),
      list(id = "koherentnosc", title = "System koherentny", bullets = c("naprawa elementu nigdy nie pogarsza stanu systemu — φ jest niemalejąca względem każdego x_i", "każdy element jest istotny — istnieje układ stanów pozostałych elementów, w którym jego stan rozstrzyga o wyniku", "wszystkie układy z tego wykładu: szeregowy, równoległy i mieszany, są koherentne"))
    ),
    formula = "\\varphi(x_1,\\ldots,x_n)\\in\\{0,1\\},\\qquad x_i\\in\\{0,1\\}",
    pitfall = "Element, którego stan nigdy nie wpływa na φ, łamie koherentność i zwykle sygnalizuje błąd modelu: albo element jest zbędny w schemacie, albo pominęliśmy drogę, na której ma znaczenie."
  ),
  list(
    id = "wspolna", title = "Jawna wspólna przyczyna",
    lead = "Utrata wspólnego zasilania jest osobnym zdarzeniem w architekturze.",
    intro = c(
      "Obietnica z wykładu drugiego zostaje spełniona: wspólne zasilanie wraca w pełnej skali. Dwa wentylatory na papierze dają R = 0,996 — ale oba wpięte są w tę samą rozdzielnicę. Utrata zasilania wyłącza obie gałęzie naraz, więc nie jest szumem w danych, lecz osobnym zdarzeniem, które trzeba dopisać do architektury.",
      "Model jest prosty: z prawdopodobieństwem q pada wspólny zasób i system nie działa niezależnie od stanu gałęzi; z prawdopodobieństwem 1−q obowiązuje zwykły rachunek redundancji. Krzywa poniżej pokazuje, jak szybko nawet małe q zjada obiecany zysk z drugiego wentylatora."
    ),
    formula = "R=(1-q)R_{bez\\ wspólnej\\ awarii}",
    widget = risk_widget_panel("Zależność", "Wspólne zasilanie", sliderInput("s8_common", "P(utraty wspólnego zasilania)", 0, .15, .01, .005), "s8_common_plot", "s8_common_stats"),
    pitfall = "Suwak korelacji nie zastępuje opisu mechanizmu wspólnej przyczyny."
  ),
  list(
    id = "redundancja", title = "Malejąca korzyść redundancji",
    lead = "Kolejna gałąź poprawia R, lecz wnosi koszt i coraz mniejszy przyrost.",
    intro = c(
      "Skoro drugi wentylator tak pomaga, czemu nie zamontować czterech? Rachunek odpowiada krzywą nasycenia: pierwsza dodatkowa gałąź redukuje ryzyko dziesięciokrotnie, następna znowu dziesięciokrotnie — ale to już redukcja z 0,01 do 0,001, podczas gdy koszt każdej gałęzi jest taki sam.",
      "W praktyce granicę opłacalności wyznaczają dwa czynniki, których krzywa nie pokazuje: wspólne przyczyny (od pewnego momentu to one dominują ryzyko i kolejne gałęzie nie pomagają wcale) oraz koszty pośrednie — miejsce, obsługa, dodatkowe punkty awarii."
    ),
    sections = list(list(
      id = "transfer", title = "Przykład transferowy: kopie zapasowe",
      text = "Ta sama krzywa opisuje kopie zapasowe danych. Druga kopia radykalnie zmniejsza ryzyko utraty, trzecia już umiarkowanie — a wszystkie trzy trzymane w tej samej serwerowni dzielą wspólną przyczynę: pożar, zalanie, ransomware. Reguła 3-2-1 (trzy kopie, dwa nośniki, jedna poza lokalizacją) to inżynieria wspólnych przyczyn, nie mnożenie gałęzi."
    )),
    widget = risk_widget_panel("Trade-off", "Liczba gałęzi i koszt", tagList(sliderInput("s8_branches", "Liczba gałęzi", 1, 6, 2, 1), sliderInput("s8_branch_r", "R jednej gałęzi", .5, .99, .9, .01)), "s8_redundancy", "s8_redundancy_stats"),
    takeaway = "Przy niezależnych, jednakowych gałęziach każda kolejna redukuje coraz mniejszą część pozostałego ryzyka, a koszt rośnie liniowo. Wielkość korzyści zależy jednak od niezawodności gałęzi i od wspólnych przyczyn: zależne zasilanie potrafi odebrać redundancji większość obiecanego zysku."
  ),
  list(
    id = "poprawa", title = "Który element poprawić?",
    lead = "Ta sama poprawa elementu może mieć różną wartość systemową.",
    intro = c(
      "Budżet pozwala poprawić jeden element o dwie setne niezawodności. Który wybrać? Intuicja podpowiada najsłabszy — i w układzie szeregowym zwykle ma rację, ale nie zawsze i nie z definicji. Wartość poprawy zależy od miejsca elementu w architekturze: wzmacnianie gałęzi równoległej, którą i tak ktoś zastępuje, daje ułamek tego, co wzmocnienie wąskiego gardła w szeregu.",
      "Porównanie poniżej liczy dokładnie to: systemowy zysk z identycznej poprawy w trzech różnych miejscach. To pierwsza wersja analizy wrażliwości, która w wykładzie o drzewach błędów stanie się rankingiem interwencji."
    ),
    widget = figure_panel(label = "Porównanie", title = "Spadek ryzyka po poprawie R o 0,02", uiOutput("s8_improvement"), full_width = TRUE),
    decision = "Porównuj zmianę wyniku systemowego, koszt i wykonalność; nie wybieraj automatycznie najsłabszego elementu."
  ),
  list(
    id = "sciaga", title = "Ściąga",
    lead = "Funkcja → misja → architektura → zależności → wynik.",
    intro = "Rachunek systemowy sprowadza się do dwóch wzorów i jednej dyscypliny: iloczyn dla szeregu, dopełnienie iloczynu dla redundancji, i bezwzględny wymóg wspólnego czasu misji oraz jawnych wspólnych przyczyn. Pięć kroków poniżej wystarcza do audytu każdej analizy — własnej i cudzej.",
    sections = list(list(id = "lista", title = "Pięć kroków", bullets = c("Zdefiniuj sukces systemu", "Ustal wspólny czas misji", "Zredukuj logikę etapami", "Dodaj jawne wspólne przyczyny", "Sprawdź wrażliwość na interwencje")), list(
      id = "most", title = "Co dalej",
      text = "Opisaliśmy logikę sukcesu: kiedy system działa. Następny wykład odwróci perspektywę i zapyta, jakie kombinacje przyczyn prowadzą do awarii — to ta sama algebra, ale czytana od strony zdarzenia szczytowego."
    ))
  ),
  list(
    id = "sprawdzenie", title = "Quiz i ćwiczenia",
    lead = "Rachunek ma odzwierciedlać fizyczną architekturę.",
    intro = "Quiz sprawdza logikę sukcesu i porażki w obu układach; ćwiczenia prowadzą od rachunku szeregowego przez diagnozę wspólnego zasilania po zapis logiki systemu hamulcowego — czyli transfer całego warsztatu poza chłodnię.",
    widget = risk_assessment_ui("s8", system_quiz, system_exercises)
  )
))
system_chapters <- risk_block_chapters(system_block)

system_server <- function(input, output, session) {
  v <- reactiveVal(FALSE)
  observeEvent(input$s8_vote_check, v(TRUE))
  output$s8_vote_feedback <- renderUI({
    req(v())
    if (is.null(input$s8_vote)) {
      return(lc_feedback(type = "info", "Najpierw zaznacz jedną z odpowiedzi."))
    }
    lc_feedback(type = "info", tags$strong("Każda odpowiedź może być poprawna:"), " 0,81 dla szeregu, 0,90 dla pojedynczego wymagania i 0,99 dla redundancji równoległej.")
  })
  output$s8_series_state <- renderUI({
    ok <- length(input$s8_series_states) == 3
    lc_feedback(type = if (ok) "ok" else "warning", tags$strong(if (ok) "System działa." else "System nie działa."), " Układ szeregowy wymaga wszystkich elementów.")
  })
  output$s8_parallel_state <- renderUI({
    ok <- length(input$s8_parallel_states) >= 1
    lc_feedback(type = if (ok) "ok" else "warning", tags$strong(if (ok) "System działa." else "System nie działa."), " Wystarcza co najmniej jedna gałąź.")
  })
  diagram_plot <- reactive({
    if (input$s8_diagram == "series") {
      boxes <- data.frame(x = c(2, 4.5, 7), y = 0, w = 1, label = c("Czujnik", "Sterownik", "Wentylator"))
      lines <- data.frame(xs = c(.3, 3, 5.5, 8), xe = c(1, 3.5, 6, 8.7), ys = 0, ye = 0)
      title <- "Układ szeregowy: jedna droga przez wszystkie bloki"
      limits <- list(x = c(0, 9), y = c(-1.5, 1.5))
    } else if (input$s8_diagram == "parallel") {
      boxes <- data.frame(x = 4.5, y = c(.9, -.9), w = 1.4, label = c("Wentylator A", "Wentylator B"))
      lines <- data.frame(
        xs = c(.5, 2, 2, 2, 2, 5.9, 5.9, 7, 7, 7),
        xe = c(2, 2, 2, 3.1, 3.1, 7, 7, 7, 7, 8.5),
        ys = c(0, 0, 0, .9, -.9, .9, -.9, .9, -.9, 0),
        ye = c(0, .9, -.9, .9, -.9, .9, -.9, 0, 0, 0)
      )
      title <- "Układ równoległy: wystarczy jedna droga"
      limits <- list(x = c(0, 9), y = c(-1.8, 1.8))
    } else {
      boxes <- data.frame(
        x = c(1.9, 6.4, 6.4), y = c(0, .9, -.9), w = c(1.2, 1.4, 1.4),
        label = c("Sterownik C", "Wentylator A", "Wentylator B")
      )
      lines <- data.frame(
        xs = c(0, 3.1, 4.2, 4.2, 4.2, 4.2, 7.8, 7.8, 8.6, 8.6, 8.6),
        xe = c(.7, 4.2, 4.2, 4.2, 5, 5, 8.6, 8.6, 8.6, 8.6, 9.5),
        ys = c(0, 0, 0, 0, .9, -.9, .9, -.9, .9, -.9, 0),
        ye = c(0, 0, .9, -.9, .9, -.9, .9, -.9, 0, 0, 0)
      )
      title <- "Układ mieszany: szereg C z redundancją A/B"
      limits <- list(x = c(-.2, 9.7), y = c(-1.8, 1.8))
    }
    ggplot() +
      geom_segment(data = lines, aes(x = xs, xend = xe, y = ys, yend = ye), colour = upwr_reference, linewidth = 1) +
      geom_rect(data = boxes, aes(xmin = x - w, xmax = x + w, ymin = y - .45, ymax = y + .45), fill = upwr_secondary, colour = "white") +
      geom_text(data = boxes, aes(x = x, y = y, label = label), colour = "white", fontface = "bold", size = 3.6) +
      coord_equal(xlim = limits$x, ylim = limits$y) +
      labs(title = title, x = NULL, y = NULL) +
      theme_upwr() +
      theme(
        axis.text = element_blank(), axis.ticks = element_blank(),
        axis.line = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  zoom_plot_server("s8_diagram_plot", diagram_plot, alt = "Schemat blokowy niezawodności: bloki elementów połączone liniami dróg sukcesu dla wybranej architektury.")
  arch_value <- reactive(if (input$s8_arch == "series") risk_series_reliability(c(input$s8_r1, input$s8_r2)) else risk_parallel_reliability(c(input$s8_r1, input$s8_r2)))
  arch_plot <- reactive({
    dat <- data.frame(element = c("Element 1", "Element 2", "System"), r = c(input$s8_r1, input$s8_r2, arch_value()))
    ggplot(dat, aes(element, r, fill = element)) +
      geom_col(width = .65) +
      coord_cartesian(ylim = c(0, 1)) +
      scale_fill_manual(values = upwr_cat_n(3), guide = "none") +
      labs(title = paste("Architektura", if (input$s8_arch == "series") "szeregowa" else "równoległa"), x = NULL, y = "Niezawodność") +
      theme_upwr()
  })
  zoom_plot_server("s8_arch_plot", arch_plot, alt = "Słupki niezawodności dwóch elementów i systemu dla wybranej architektury.")
  output$s8_arch_stats <- renderUI(lc_stat_grid(lc_stat_box("R systemu", risk_format_probability(arch_value()), color = upwr_accent), columns = 1))
  step <- reactiveVal(0L)
  observeEvent(input$s8_step, step((step() + 1L) %% 3L))
  output$s8_reduction <- renderUI({
    texts <- c("1. Zdefiniuj sukces: C działa oraz A lub B działa.", "2. Zredukuj A/B: R_AB=1−(1−R_A)(1−R_B).", "3. Połącz szeregowo: R_sys=R_C·R_AB.")
    lc_feedback(type = "info", texts[[step() + 1L]])
  })
  time_plot <- reactive({
    t <- seq(0, 3000, length.out = 400)
    ra <- exp(-t / 1800)
    rb <- exp(-t / 2000)
    rc <- exp(-t / 2500)
    rs <- rc * (1 - (1 - ra) * (1 - rb))
    dat <- rbind(data.frame(t, r = ra, name = "Wentylator A"), data.frame(t, r = rb, name = "Wentylator B"), data.frame(t, r = rc, name = "Sterownik"), data.frame(t, r = rs, name = "System"))
    ggplot(dat, aes(t, r, colour = name)) +
      geom_line(linewidth = 1) +
      geom_vline(xintercept = input$s8_mission, linetype = 2) +
      scale_colour_manual(values = upwr_cat_n(4)) +
      labs(title = "Wspólny czas dla elementów i systemu", x = "Czas (h)", y = "R(t)", colour = NULL) +
      theme_upwr()
  })
  zoom_plot_server("s8_time_plot", time_plot, alt = "Krzywe niezawodności trzech elementów i systemu mieszanego.")
  output$s8_time_stats <- renderUI({
    t <- input$s8_mission
    rs <- exp(-t / 2500) * (1 - (1 - exp(-t / 1800)) * (1 - exp(-t / 2000)))
    lc_stat_grid(lc_stat_box("R systemu", risk_format_probability(rs), color = upwr_accent), columns = 1)
  })
  common_plot <- reactive({
    q <- seq(0, .15, length.out = 200)
    base <- risk_parallel_reliability(c(.92, .95))
    ggplot(data.frame(q, r = (1 - q) * base), aes(q, r)) +
      geom_line(colour = upwr_accent, linewidth = 1.1) +
      geom_point(data = data.frame(q = input$s8_common, r = (1 - input$s8_common) * base), colour = upwr_secondary, size = 3) +
      labs(title = "Wspólna przyczyna ogranicza redundancję", x = "P(wspólnej awarii)", y = "R systemu") +
      theme_upwr()
  })
  zoom_plot_server("s8_common_plot", common_plot, alt = "Malejąca niezawodność układu redundantnego wraz ze wzrostem wspólnej przyczyny.")
  output$s8_common_stats <- renderUI(lc_stat_grid(lc_stat_box("R z przyczyną wspólną", risk_format_probability(risk_common_cause_reliability(risk_parallel_reliability(c(.92, .95)), input$s8_common)), color = upwr_accent), columns = 1))
  redundancy_plot <- reactive({
    n <- 1:6
    r <- 1 - (1 - input$s8_branch_r)^n
    dat <- data.frame(n, r, cost = n * 100)
    ggplot(dat, aes(n, r)) +
      geom_line(colour = upwr_accent, linewidth = 1.1) +
      geom_point() +
      geom_point(data = dat[dat$n == input$s8_branches, ], colour = upwr_secondary, size = 4) +
      labs(title = "Przyrost niezawodności maleje", x = "Liczba gałęzi", y = "R systemu") +
      theme_upwr()
  })
  zoom_plot_server("s8_redundancy", redundancy_plot, alt = "Krzywa niezawodności równoległej względem liczby gałęzi.")
  output$s8_redundancy_stats <- renderUI({
    r <- 1 - (1 - input$s8_branch_r)^input$s8_branches
    lc_stat_grid(lc_stat_box("R", risk_format_probability(r), color = upwr_accent), lc_stat_box("Koszt demonstracyjny", paste(input$s8_branches * 100, "jedn.")), columns = 1)
  })
  output$s8_improvement <- renderUI({
    base <- c(.92, .95, .98)
    sys0 <- risk_series_reliability(base)
    gains <- vapply(seq_along(base), function(i) {
      x <- base
      x[i] <- min(1, x[i] + .02)
      risk_series_reliability(x) - sys0
    }, numeric(1))
    lc_stat_grid(lc_stat_box("Czujnik", risk_format_probability(gains[1])), lc_stat_box("Sterownik", risk_format_probability(gains[2])), lc_stat_box("Zasilanie", risk_format_probability(gains[3])), columns = 1)
  })
  risk_assessment_server("s8", system_quiz, input, output)
}
