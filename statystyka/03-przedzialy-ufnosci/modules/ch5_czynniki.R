# ============================================================================
# CHAPTER 5: Co wpływa na szerokość przedziału?
# ============================================================================

ch5_ui <- list(
  id    = "ch-czynniki",
  num   = "05",
  title = "Co wpływa na szerokość?",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 05 · Przedziały ufności",
      num    = "05",
      title  = "Co wpływa na szerokość przedziału?",
      lead   = "Szerokość przedziału zależy od trzech rzeczy: liczby obserwacji,
                zmienności danych i tego, jak dużej pewności żądamy. Realny wpływ
                mamy głównie na pierwszą z nich, a każde zawężenie przedziału
                o połowę kosztuje czterokrotnie więcej danych."
    ),

    lc_p("W dwóch poprzednich rozdziałach budowaliśmy przedziały dla średniej
      i dla proporcji. Za każdym razem wynik miał tę samą postać: estymata
      punktowa plus minus pewna odległość. Od tej odległości zależy, czy
      przedział jest użyteczny. Przedział „wzrost studentów leży między 150
      a 190 cm” jest poprawny, ale nic nie mówi. W tym rozdziale sprawdzimy,
      co tę odległość wydłuża i skraca oraz jak zaplanować badanie, żeby
      wynik był wystarczająco precyzyjny."),

    lc_h2("ch5-czynniki", "Trzy czynniki szerokości przedziału"),

    lc_p("Odległość od estymaty do każdej z granic przedziału nazywamy ",
      gloss("margines błędu", "marginesem błędu"), " (ME, od ang. margin of
      error). Cały przedział ma szerokość 2·ME. Dla średniej margines błędu
      to iloczyn wartości krytycznej rozkładu t i błędu standardowego:"),

    lc_formula_box(withMathJax(
      "$$ME = t^* \\cdot \\frac{s}{\\sqrt{n}}$$"
    )),

    lc_p("We wzorze widać trzy czynniki. Pierwszy to ",
      gloss("wielkość próby", "wielkość próby"), " n: stoi w mianowniku pod
      pierwiastkiem, więc im więcej obserwacji, tym węższy przedział. Drugi to ",
      gloss("poziom ufności", "poziom ufności"), ", który wyznacza wartość
      krytyczną t*: im większej pewności żądamy, tym większe t* i szerszy
      przedział. Trzeci to zmienność danych mierzona ",
      gloss("odchylenie standardowe", "odchyleniem standardowym"), " s:
      im bardziej rozproszone obserwacje, tym mniej precyzyjna średnia."),

    lc_p("Ta sama logika obowiązuje dla proporcji. Wzór ma postać
      z*·√(p̂(1 − p̂)/n), a rolę s pełni √(p̂(1 − p̂)), które jest największe
      przy p̂ = 0,5. Wszystko, co dalej powiemy o średniej, przenosi się
      więc na proporcje."),

    lc_h2("ch5-eksploracja", "Jak szybko maleje margines błędu"),

    lc_p("Panel poniżej pozwala zmieniać każdy z trzech czynników osobno.
      Górny wykres pokazuje, jak margines błędu maleje wraz z n przy
      ustalonym poziomie ufności i ustalonym s, a punkt zaznacza bieżące n.
      Dolny pokazuje odpowiadający mu przedział na stałej osi, więc łatwo
      porównać jego szerokość między ustawieniami."),

    figure_panel(
      label = "Ryc. 5.1", title = "Jak zmienia się szerokość przedziału?",
      full_width = TRUE,
      fluidRow(
        column(4,
          lc_slider("ch5_n", "Wielkość próby (n)", 5, 100, 30, 1),
          lc_slider("ch5_conf", "Poziom ufności", 0.80, 0.99, 0.95, 0.01),
          lc_slider("ch5_s", "Odchylenie std. (s)", 1, 12, 8, 1),
          hr(),
          uiOutput("ch5_me_display")
        ),
        column(8,
          zoom_plot_ui("ch5_factors_plot", height = "480px")
        )
      )
    ),

    lc_p("Przy ustawieniach początkowych (n = 30, s = 8, poziom ufności 95%)
      wartość krytyczna wynosi t* = 2,045, a margines błędu 2,99, więc przedział
      ma szerokość około 6. Każdy z suwaków działa na tę liczbę inaczej."),

    lc_p("Odchylenie standardowe działa proporcjonalnie: przy s = 4 margines
      spada dokładnie o połowę, do 1,49. Poziom ufności działa przez t*:
      przy 90% margines wynosi 2,48, przy 99% już 4,03. Najciekawsza jest
      krzywa dla n. Na początku opada stromo: przy n = 5 margines wynosi
      9,93, przy n = 30 już tylko 2,99. Dalej spłaszcza się i kolejne
      obserwacje dają coraz mniej."),

    lc_p("To ten sam mechanizm, który poznaliśmy w wykładzie 02: błąd standardowy
      maleje jak 1/√n, więc żeby zmniejszyć go o połowę, trzeba czterokrotnie
      większej próby. Margines błędu dziedziczy tę zależność. Przejście z n = 25
      do n = 100 skraca go z 3,30 do 1,59, czyli nieco ponad dwukrotnie,
      bo przy większym n maleje też t*. Kolejne czterokrotne powiększenie
      próby, do 400 obserwacji, znowu skróci przedział mniej więcej o połowę.
      Każde następne zawężenie przedziału jest więc droższe od poprzedniego."),

    lc_h2("ch5-planowanie", "Planowanie wielkości próby"),

    lc_p("Skoro wiemy, jak margines błędu zależy od n, możemy odwrócić pytanie.
      Zamiast liczyć, jak precyzyjny wynik dała próba, którą już mamy,
      ustalamy z góry, jakiej precyzji potrzebujemy, i liczymy, ile obserwacji
      trzeba zebrać. Wystarczy rozwiązać wzór na margines błędu względem n:"),

    lc_formula_box(withMathJax(
      "$$n = \\left(\\frac{z^* \\cdot s}{ME_{\\text{max}}}\\right)^2$$"
    )),

    lc_p("Wynik zaokrąglamy zawsze w górę. Zamiast t* używamy tu z* z rozkładu
      normalnego, bo t* zależy od n, którego jeszcze nie znamy. Przy próbach,
      jakie zwykle wychodzą z tego wzoru, różnica między t* a z* jest niewielka.
      Najtrudniejsze jest s: przed badaniem nie mamy danych, więc odchylenie
      standardowe trzeba założyć na podstawie badania pilotażowego, wcześniejszych
      publikacji albo rozsądnego szacunku zakresu wartości."),

    figure_panel(
      label = "Ryc. 5.2", title = "Kalkulator wielkości próby",
      full_width = TRUE,
      fluidRow(
        column(4,
          numericInput("ch5_plan_me", "Pożądany margines błędu:",
                       value = 2, min = 0.1, step = 0.1),
          numericInput("ch5_plan_s", "Spodziewane s:",
                       value = 10, min = 0.1, step = 0.5),
          lc_slider("ch5_plan_conf", "Poziom ufności", 0.80, 0.99, 0.95, 0.01)
        ),
        column(8,
          uiOutput("ch5_plan_result"),
          zoom_plot_ui("ch5_plan_plot", height = "440px")
        )
      )
    ),

    lc_p("Przy domyślnych ustawieniach (margines 2, s = 10, poziom ufności 95%)
      wzór daje (1,96 · 10 / 2)² = 96,04, czyli potrzeba 97 obserwacji. Żądanie
      dwa razy większej precyzji, czyli marginesu 1, podnosi wymaganą próbę
      do 385 osób, prawie czterokrotnie. Poziom ufności też kosztuje: przy 90%
      wystarczy 68 obserwacji, przy 99% potrzeba 166."),

    lc_p("Dla proporcji rachunek jest analogiczny, z p(1 − p) w miejscu s².
      Gdy nie wiemy nic o spodziewanej proporcji, przyjmujemy najgorszy
      przypadek p = 0,5. Margines 3 punktów procentowych przy poziomie ufności
      95% wymaga wtedy 1068 respondentów. Stąd biorą się typowe sondaże
      na około tysiącu osób."),

    lc_h2("ch5-porownanie", "Ten sam zbiór, trzy poziomy ufności"),

    lc_p("W kalkulatorze wybór poziomu ufności był częścią planu badania.
      Teraz spójrzmy na niego od strony gotowych danych. Panel poniżej liczy
      z jednej próby trzy przedziały: 90%, 95% i 99%. Dane i ich środek
      są za każdym razem te same, zmienia się tylko t*."),

    figure_panel(
      label = "Ryc. 5.3", title = "Trzy poziomy ufności",
      full_width = TRUE,
      fluidRow(
        column(4,
          selectInput("ch5_cmp_data", "Dane:",
            choices = list(
              "Przykłady ogólne" = c(
                "Wzrost studentów (n=30)" = "height",
                "Czas dojazdu (n=50)" = "commute",
                "Oceny z egzaminu (n=40)" = "grades"
              ),
              "Dane kierunkowe" = c(
                "IB: wskaźnik wypadków (n=320)" = "ib_wypadki",
                "ROL: plon pszenicy (n=280)" = "rol_plon",
                "TZ: zawartość białka (n=350)" = "tz_bialko"
              )
            ),
            selected = "height"
          ),
          lc_action("ch5_cmp_calc", "Oblicz 3 przedziały", variant = "solid"),
          br(), br(),
          uiOutput("ch5_cmp_stats")
        ),
        column(8,
          zoom_plot_ui("ch5_cmp_plot", height = "250px")
        )
      )
    ),

    lc_p("Dla próby wzrostu 30 studentów (średnia 170,69 cm, s = 12,55 cm)
      przedział 90% to [166,79; 174,58], 95% to [166,00; 175,37], a 99% to
      [164,37; 177,00]. Przedział 99% jest o ponad 60% szerszy niż 90%.
      Przy danych kierunkowych, gdzie próby liczą około 300 obserwacji,
      wszystkie trzy przedziały są wąskie i różnice między nimi stają się
      niewielkie w porównaniu ze skalą zmiennej."),

    lc_p("Wybór poziomu ufności to więc kompromis między pewnością a precyzją.
      Przedział 99% rzadziej mija prawdziwą wartość, ale jest szeroki.
      Przedział 90% jest węższy, za to częściej się myli. Poziom 95% przyjął się
      jako rozsądny środek i jest domyślny w większości programów, ale to umowa,
      a nie prawo statystyki."),

    lc_h2("ch5-edge-case", "Przypadek graniczny: poziom ufności zmienia wniosek"),

    lc_p("Kompromis między pewnością a precyzją ma praktyczne konsekwencje,
      gdy pytamy, czy parametr przekracza jakiś próg. Jeśli cały przedział leży
      powyżej progu, wszystkie wiarygodne wartości parametru go przekraczają
      i możemy to stwierdzić. Jeśli przedział przecina próg, dane są zgodne
      zarówno z wartościami powyżej, jak i poniżej. Nie wystarczy więc, że
      sama średnia czy proporcja z próby leży nad granicą. Liczy się położenie
      całego przedziału, a jego szerokość zależy od wybranego poziomu ufności."),

    lc_p("W trzech przykładach poniżej ta sama próba jest oceniana przy
      poziomach 90%, 95% i 99%. Po wybraniu poziomu wykres pokazuje przedział
      na tle obszaru hipotezy. Zanim odsłonisz werdykt, oceń sam, czy przedział
      pozwala ją przyjąć."),

    tags$details(class = "case-study", open = NA,
      tags$summary(
        span(class = "case-icon", "\U0001f697"),
        "Przykład 1. Czas dojazdu — czy średni czas przekracza 26 min?"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Zmierzono czas dojazdu 40 pracowników. Średnia z próby wynosi ",
            withMathJax("\\(\\bar{x} = 28{,}5\\)"), " min, odchylenie standardowe ", withMathJax("\\(s = 8\\)"), " min.
            Hipoteza: średni czas dojazdu w populacji przekracza 26 min.")
        ),
        uiOutput("ch5_edge1_buttons"),
        lc_plot("ch5_edge1_plot", ratio = "2.6/1", max_height = "240px"),
        uiOutput("ch5_edge1_explain")
      )
    ),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f5f3️"),
        "Przykład 2. Sondaż — czy poparcie przekracza 50%?"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Pracownia sondażowa zapytała 1000 wyborców, czy poprą partię X.
            Odpowiedzi TAK udzieliło 540 osób (", withMathJax("\\(\\hat{p} = 0{,}54\\)"), ").
            Hipoteza: poparcie w populacji przekracza próg 50%.")
        ),
        uiOutput("ch5_edge2_buttons"),
        lc_plot("ch5_edge2_plot", ratio = "2.6/1", max_height = "240px"),
        uiOutput("ch5_edge2_explain")
      )
    ),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f4d8"),
        "Przykład 3. Wynik szkolenia — czy średnia przekracza 65 pkt?"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Po szkoleniu BHP 20 pracowników uzyskało średni wynik ",
            withMathJax("\\(\\bar{x} = 68\\)"), " pkt
            (na 100), ", withMathJax("\\(s = 10\\)"), " pkt.
            Hipoteza: średni wynik w populacji przekracza próg 65 pkt.")
        ),
        uiOutput("ch5_edge3_buttons"),
        lc_plot("ch5_edge3_plot", ratio = "2.6/1", max_height = "240px"),
        uiOutput("ch5_edge3_explain")
      )
    ),

    lc_p("Gdy przedział leży blisko progu, sama zmiana poziomu ufności może
      przesunąć jego granicę na drugą stronę i zmienić wniosek. Bywa też
      odwrotnie: próba jest tak mała, że przedział obejmuje próg przy każdym
      rozsądnym poziomie ufności. Wtedy zmiana poziomu nie pomoże, a
      rozstrzygnięcie wymaga większej próby, czyli powrotu do planowania
      z poprzednich sekcji."),

    lc_p("Takie wyniki często wydają się podejrzane: skoro przy 90% wniosek
      jest pozytywny, a przy 95% już nie, to jak jest naprawdę? Obie odpowiedzi
      są poprawne, bo odpowiadają na różne pytania. Zdanie „nie możemy tego
      stwierdzić przy ufności 95%, ale możemy przy 90%” nie jest sprzecznością,
      tylko precyzyjnym opisem siły dowodów. Potoczne myślenie zna tylko
      „pewne”, „prawdopodobne” i „wątpliwe”, a statystyka pozwala tę pewność
      zmierzyć."),

    lc_p("Wybór poziomu ufności powinien wynikać z kosztu pomyłki. Przy wstępnej
      eksploracji, gdy błąd niewiele kosztuje, można przyjąć 90%. W badaniach
      medycznych czy kontroli jakości uzasadnione bywa 99%. Ten swobodny wybór
      ma jedno ograniczenie: poziom trzeba ustalić przed obejrzeniem wyników.
      Kto najpierw patrzy na przedziały, a potem wybiera poziom, przy którym
      wniosek wychodzi „po jego myśli”, przestaje mierzyć siłę dowodów."),

    inline_callout(label = "Zasada",
      "Poziom ufności ustal przed analizą danych i zawsze podawaj go w raporcie
       razem z przedziałem."
    ),

    lc_p("Ten rozdział zamyka wykład o przedziałach ufności. Zaczęliśmy od
      pytania, jak z jednej próby powiedzieć coś o populacji, a skończyliśmy
      na przedziale, którego szerokość umiemy wyjaśnić i zaplanować.
      Przykłady z ostatniej sekcji zadawały jednak pytanie innego rodzaju:
      nie „gdzie leży parametr?”, tylko „czy przekracza konkretną wartość?”.
      Takie pytania są tematem wykładu 04 o testowaniu hipotez. Zobaczymy tam,
      że sprawdzenie, czy przedział obejmuje wartość progową, jest blisko
      spokrewnione z testem statystycznym, a rolę poziomu ufności przejmie ",
      gloss("poziom istotności", "poziom istotności"), "."),

    lc_chapter_next(
      num       = "06",
      title     = "Ściąga",
      lead      = "podsumowanie wzorów i zasad",
      target_id = "ch-sciaga"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch5_server <- function(input, output, session) {

  # --- Widget 1: Trzy suwaki ---
  zoom_plot_server("ch5_factors_plot", reactive({
    n <- input$ch5_n
    conf <- input$ch5_conf
    s <- input$ch5_s
    t_star <- qt(1 - (1 - conf) / 2, df = n - 1)
    me <- t_star * s / sqrt(n)
    xbar <- 170  # arbitralny srodek (np. wzrost)

    # ---- GORNY PANEL: krzywa ME(n) ----
    n_seq <- seq(5, 100, by = 1)
    me_seq <- qt(1 - (1 - conf) / 2, df = pmax(n_seq - 1, 1)) * s / sqrt(n_seq)
    df <- data.frame(n = n_seq, me = me_seq)

    p_top <- ggplot(df, aes(x = n, y = me)) +
      geom_line(color = col_ci, linewidth = 1.2) +
      geom_point(aes(x = !!n, y = !!me), color = col_estimate, size = 4) +
      geom_hline(yintercept = me, color = col_estimate, linetype = "dotted") +
      annotate("text", x = n + 4, y = me + 0.3,
               label = "ME",
               color = col_estimate, fontface = "bold", size = 4.5) +
      labs(
           x = "Wielkość próby (n)",
           y = "Margines błędu (ME)") +
      theme_upwr()

    # ---- DOLNY PANEL: sam pasek CI na fixed osi X ----
    # Worst-case ME (n=5, conf=0.99, s=12) -> ustala stale granice osi X
    max_me_worst <- qt(0.995, df = 4) * 12 / sqrt(5)
    xlims <- c(xbar - max_me_worst * 1.05, xbar + max_me_worst * 1.05)

    p_bot <- ggplot() +
      xlim(xlims) +
      ylim(-0.6, 0.6) +
      labs(x = "Wartość (np. wzrost w cm)", y = NULL) +
      theme_upwr() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      geom_vline(xintercept = xbar, color = upwr_reference,
                 linetype = "dashed", linewidth = 0.6) +
      annotate("text", x = xbar, y = 0.5, label = "środek",
               color = upwr_reference, size = 4, hjust = -0.1) +
      geom_point(aes(x = xbar, y = 0), color = col_estimate,
                 size = 7, shape = 18) +
      geom_errorbarh(aes(xmin = xbar - me, xmax = xbar + me, y = 0),
                     height = 0.18, color = col_ci, linewidth = 2.4, alpha = 0.7) +
      annotate("text", x = xbar, y = -0.42,
               label = paste0(round(conf * 100), "% CI"),
               color = col_ci, fontface = "bold", size = 4.8)

    library(patchwork)
    (p_top / p_bot) + plot_layout(heights = c(2, 1))
  }))

  output$ch5_me_display <- renderUI({
    n <- input$ch5_n
    conf <- input$ch5_conf
    s <- input$ch5_s
    t_star <- qt(1 - (1 - conf) / 2, df = n - 1)
    me <- t_star * s / sqrt(n)
    width <- 2 * me

    tagList(
      lc_stat_box("ME", round(me, 2), color = col_ci),
      lc_stat_box("Szer.", round(width, 2), color = upwr_secondary),
      lc_stat_box("t*", round(t_star, 3), color = col_estimate)
    )
  })

  # --- Widget 2: Planowanie n ---
  output$ch5_plan_result <- renderUI({
    me_max <- input$ch5_plan_me
    s <- input$ch5_plan_s
    conf <- input$ch5_plan_conf
    z_star <- qnorm(1 - (1 - conf) / 2)
    n_req <- ceiling((z_star * s / me_max)^2)

    lc_feedback(type = "ok",
      p(tags$strong("Wymagana wielkość próby:")),
      p(withMathJax(paste0(
        "\\(n = \\left(\\frac{", round(z_star, 3), " \\cdot ", s, "}{",
        me_max, "}\\right)^2 = ", round((z_star * s / me_max)^2, 1),
        " \\approx \\mathbf{", n_req, "}\\)"
      )))
    )
  })

  zoom_plot_server("ch5_plan_plot", reactive({
    me_max <- input$ch5_plan_me
    s <- input$ch5_plan_s
    conf <- input$ch5_plan_conf
    z_star <- qnorm(1 - (1 - conf) / 2)
    n_req <- ceiling((z_star * s / me_max)^2)
    me_actual <- z_star * s / sqrt(n_req)  # ME osiagniete przy n_req (zwykle ~ me_max)
    center <- 100  # arbitralny srodek

    # ---- GORNY PANEL: krzywa ME vs n ----
    n_seq <- seq(5, max(n_req * 2, 100), by = 1)
    me_seq <- z_star * s / sqrt(n_seq)
    df <- data.frame(n = n_seq, me = me_seq)

    p_top <- ggplot(df, aes(x = n, y = me)) +
      geom_line(color = col_ci, linewidth = 1.2) +
      geom_hline(yintercept = me_max, color = col_miss, linetype = "dashed",
                 linewidth = 1) +
      geom_point(aes(x = n_req, y = me_max), color = col_hit, size = 5) +
      annotate("text", x = n_req, y = me_max + 0.3,
               label = paste0("n = ", n_req),
               color = col_hit, fontface = "bold", size = 5) +
      labs(
           x = "n", y = "Margines błędu") +
      theme_upwr()

    # ---- DOLNY PANEL: pasek CI przy n_req, z dopuszczalna strefa ----
    xlims <- c(center - 3 * me_max, center + 3 * me_max)

    p_bot <- ggplot() +
      xlim(xlims) +
      ylim(-0.6, 0.6) +
      labs(x = "Wartość (jednostki dowolne)", y = NULL) +
      theme_upwr() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      annotate("rect",
               xmin = center - me_max, xmax = center + me_max,
               ymin = -Inf, ymax = Inf,
               fill = upwr_rule, alpha = 0.4) +
      geom_vline(xintercept = center, color = upwr_reference,
                 linetype = "dashed", linewidth = 0.6) +
      geom_point(aes(x = center, y = 0), color = col_estimate,
                 size = 7, shape = 18) +
      geom_errorbarh(aes(xmin = center - me_actual, xmax = center + me_actual, y = 0),
                     height = 0.18, color = col_hit, linewidth = 2.4, alpha = 0.8) +
      annotate("text", x = center, y = -0.42,
               label = "Osiągnięte ME ≤ wymagane ✓",
               color = col_hit, fontface = "bold", size = 4.8)

    library(patchwork)
    (p_top / p_bot) + plot_layout(heights = c(2, 1))
  }))

  # --- Widget 3: Porównanie 90/95/99 ---
  ch5_cmp_data <- reactiveVal(NULL)

  observeEvent(input$ch5_cmp_calc, {
    set.seed(42)
    samp <- switch(input$ch5_cmp_data,
      "height"     = rnorm(30, mean = 170, sd = 10),
      "commute"    = rgamma(50, shape = 4, scale = 7.5),
      "grades"     = pmin(pmax(rnorm(40, mean = 3.5, sd = 0.7), 2), 5),
      "ib_wypadki" = read.csv("dane/bhp_zaklady.csv")$wskaznik_wypadkow,
      "rol_plon"   = read.csv("dane/rolnictwo_pola.csv")$plon_pszenicy,
      "tz_bialko"  = read.csv("dane/zywnosc_partie.csv")$zawartosc_bialka
    )
    xbar <- mean(samp)
    s <- sd(samp)
    n <- length(samp)

    levels <- c(0.90, 0.95, 0.99)
    results <- lapply(levels, function(conf) {
      t_star <- qt(1 - (1 - conf) / 2, df = n - 1)
      me <- t_star * s / sqrt(n)
      data.frame(
        conf = paste0(conf * 100, "%"),
        xbar = xbar, lower = xbar - me, upper = xbar + me,
        me = me, width = 2 * me
      )
    })
    ch5_cmp_data(do.call(rbind, results))
  })

  zoom_plot_server("ch5_cmp_plot", reactive({
    df <- ch5_cmp_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij „Oblicz 3 przedziały”",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      df$y <- c(3, 2, 1)
      colors <- c(col_estimate, col_ci, col_true)

      ggplot(df, aes(y = y)) +
        geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.3,
                       color = colors, linewidth = 2) +
        geom_point(aes(x = xbar), color = col_estimate, size = 4, shape = 18) +
        scale_y_continuous(breaks = c(1, 2, 3),
                           labels = c("99%", "95%", "90%")) +
        annotate("text", x = df$upper + 0.1, y = df$y,
                 label = paste0("[", round(df$lower, 2), " ; ",
                                round(df$upper, 2), "]"),
                 hjust = 0, size = 4) +
        labs(
             x = "Wartość", y = "Poziom ufności") +
        theme_upwr()
    }
  }))

  output$ch5_cmp_stats <- renderUI({
    df <- ch5_cmp_data()
    if (is.null(df)) return(NULL)
    tagList(
      lapply(1:3, function(i) {
        lc_stat_box(df$conf[i], "±", round(df$me[i], 2),
                    color = c(col_estimate, col_ci, col_true)[i])
      })
    )
  })

  # ==========================================================================
  # WIDGET 4: Przypadki graniczne — poziom ufności zmienia werdykt
  # ==========================================================================
  col_hyp <- "#8e44ad"

  # ---- Helpery ----
  ci_mean_local <- function(xbar, s, n, conf) {
    t_star <- qt(1 - (1 - conf) / 2, df = n - 1)
    me <- t_star * s / sqrt(n)
    list(lower = xbar - me, upper = xbar + me, me = me, t_star = t_star)
  }
  ci_prop_local <- function(x, n, conf) {
    phat <- x / n
    z_star <- qnorm(1 - (1 - conf) / 2)
    se <- sqrt(phat * (1 - phat) / n)
    me <- z_star * se
    list(phat = phat, lower = phat - me, upper = phat + me, me = me, z_star = z_star)
  }
  hypothesis_verdict_edge <- function(lower, upper, bound, dir) {
    if (dir == "gt") {
      if (lower > bound)      "yes"
      else if (upper < bound) "no"
      else                    "maybe"
    } else {
      if (upper < bound)      "yes"
      else if (lower > bound) "no"
      else                    "maybe"
    }
  }
  verdict_class_edge <- function(v) {
    switch(v, "yes" = "ok", "no" = "danger",
           "maybe" = "warning")
  }
  verdict_label_edge <- function(v) {
    switch(v, "yes" = "TAK", "no" = "NIE", "maybe" = "NIEPEWNE")
  }

  # ---- Konfiguracja edge case'ow ----
  edge_cases <- list(
    edge1 = list(
      kind = "mean",
      data = list(xbar = 28.5, s = 8, n = 40),
      hypothesis = list(text = "Średni czas dojazdu przekracza 26 min",
                        bound = 26, dir = "gt"),
      xlab = "Średni czas dojazdu (min)"
    ),
    edge2 = list(
      kind = "prop",
      data = list(x = 540, n = 1000),
      hypothesis = list(text = "Poparcie dla partii X przekracza 50%",
                        bound = 0.50, dir = "gt"),
      xlab = "Poparcie dla partii X"
    ),
    edge3 = list(
      kind = "mean",
      data = list(xbar = 68, s = 10, n = 20),
      hypothesis = list(text = "Średni wynik szkolenia przekracza 65 pkt",
                        bound = 65, dir = "gt"),
      xlab = "Średni wynik (pkt)"
    )
  )

  # State per case: lista (conf, revealed)
  #   conf:     NA (nic nie wybrane) lub 0.90 / 0.95 / 0.99
  #   revealed: FALSE (tylko CI + treść hipotezy) / TRUE (z werdyktem)
  ch5_edge_state <- reactiveValues()
  for (cid in names(edge_cases)) {
    ch5_edge_state[[cid]] <- list(conf = NA_real_, revealed = FALSE)
  }

  # ---- Compute CI for given case at given conf ----
  compute_edge_ci <- function(case_id, conf) {
    cfg <- edge_cases[[case_id]]
    if (cfg$kind == "mean") {
      ci <- ci_mean_local(cfg$data$xbar, cfg$data$s, cfg$data$n, conf)
      list(center = cfg$data$xbar, lower = ci$lower, upper = ci$upper, me = ci$me)
    } else {
      ci <- ci_prop_local(cfg$data$x, cfg$data$n, conf)
      list(center = ci$phat, lower = ci$lower, upper = ci$upper, me = ci$me)
    }
  }

  # ---- Generator przyciskow conf level + reveal ----
  edge_buttons_ui <- function(case_id) {
    state <- ch5_edge_state[[case_id]]
    current_conf <- state$conf
    revealed <- state$revealed
    levels <- c(0.90, 0.95, 0.99)
    btns <- lapply(levels, function(lv) {
      is_active <- !is.na(current_conf) && abs(current_conf - lv) < 1e-9
      btn_class <- if (is_active) "lc-btn-warning" else "lc-btn-warning-outline"
      actionButton(paste0("ch5_", case_id, "_conf", round(lv * 100)),
                   paste0(round(lv * 100), "%"), class = btn_class)
    })

    # Drugi rzad: przycisk "Pokaz werdykt" - tylko gdy conf wybrany i jeszcze nie odkryty
    reveal_row <- if (!is.na(current_conf) && !revealed) {
      div(class = "step-buttons lc-mt-xs",
        lc_action(paste0("ch5_", case_id, "_reveal"), "\U0001f50d Pokaż werdykt", variant = "solid"))
    } else {
      NULL
    }

    tagList(
      div(class = "step-buttons", btns),
      reveal_row
    )
  }

  # ---- Plot dla edge case'a (jeden panel: pasek CI + obszar hipotezy) ----
  render_edge_plot <- function(case_id) {
    cfg <- edge_cases[[case_id]]
    conf <- ch5_edge_state[[case_id]]$conf

    # Najszerszy mozliwy CI (przy 99%) -> ustala stale granice osi X
    ci_max <- compute_edge_ci(case_id, 0.99)
    ci_min <- compute_edge_ci(case_id, 0.90)
    bound <- cfg$hypothesis$bound
    center <- ci_max$center

    # Zakres X obejmujacy wszystkie 3 poziomy CI + bound + troche marginesu
    xrange <- range(c(ci_max$lower, ci_max$upper, bound))
    pad <- diff(xrange) * 0.20
    xlims <- c(xrange[1] - pad, xrange[2] + pad)

    p <- ggplot() +
      xlim(xlims) +
      ylim(-0.65, 0.65) +
      labs(x = cfg$xlab, y = NULL) +
      theme_upwr() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    # Obszar hipotezy (zawsze widoczny)
    if (cfg$hypothesis$dir == "gt") {
      p <- p + annotate("rect",
                        xmin = bound, xmax = Inf,
                        ymin = -Inf, ymax = Inf,
                        fill = col_hyp, alpha = 0.15)
    } else {
      p <- p + annotate("rect",
                        xmin = -Inf, xmax = bound,
                        ymin = -Inf, ymax = Inf,
                        fill = col_hyp, alpha = 0.15)
    }
    p <- p +
      geom_vline(xintercept = bound, color = col_hyp,
                 linewidth = 1, linetype = "solid") +
      annotate("text", x = bound, y = 0.55,
               label = paste0(if (cfg$hypothesis$dir == "gt") "≥ " else "≤ ",
                              bound),
               color = col_hyp, fontface = "bold", size = 4.5, hjust = -0.1)

    # Punkt centralny (zawsze)
    p <- p +
      geom_point(aes(x = center, y = 0), color = col_estimate,
                 size = 7, shape = 18) +
      annotate("text", x = center, y = -0.22,
               label = paste0(if (cfg$kind == "mean") "x̄ = " else "p̂ = ",
                              round(center, 3)),
               color = col_estimate, fontface = "bold", size = 4.5)

    # Pasek CI - tylko jezeli wybrany conf
    if (!is.na(conf)) {
      ci <- compute_edge_ci(case_id, conf)
      p <- p +
        geom_errorbarh(aes(xmin = ci$lower, xmax = ci$upper, y = 0),
                       height = 0.18, color = col_ci, linewidth = 2.4, alpha = 0.7) +
        annotate("text", x = center, y = -0.45,
                 label = paste0(round(conf * 100), "% CI: [",
                                round(ci$lower, 3), " ; ", round(ci$upper, 3), "]"),
                 color = col_ci, fontface = "bold", size = 4.8)
    } else {
      p <- p + annotate("text", x = mean(xlims), y = 0.35,
                        label = "Wybierz poziom ufności powyżej",
                        color = upwr_reference, size = 4.5, fontface = "italic")
    }

    p
  }

  # ---- Render werdyktu dla edge case'a ----
  render_edge_explain <- function(case_id) {
    cfg <- edge_cases[[case_id]]
    state <- ch5_edge_state[[case_id]]
    conf <- state$conf
    revealed <- state$revealed

    if (is.na(conf)) {
      return(lc_feedback(type = "info",
        p(tags$strong("Hipoteza: "), cfg$hypothesis$text),
        p(tags$em("Wybierz poziom ufności (90%, 95% lub 99%), żeby zobaczyć
                  przedział."))
      ))
    }

    # Faza 1: tylko CI + treść hipotezy, czas na zastanowienie
    if (!revealed) {
      return(lc_feedback(type = "info",
        p(tags$strong("Hipoteza: "), cfg$hypothesis$text),
        p(tags$strong("Poziom ufności:"), " ", round(conf * 100), "%"),
        p(tags$em("Gdzie leży przedział względem granicy hipotezy? Przycisk
                  „Pokaż werdykt” odsłoni odpowiedź."))
      ))
    }

    # Faza 2: werdykt
    ci <- compute_edge_ci(case_id, conf)
    verdict <- hypothesis_verdict_edge(ci$lower, ci$upper, cfg$hypothesis$bound,
                                       cfg$hypothesis$dir)
    cls <- verdict_class_edge(verdict)
    label <- verdict_label_edge(verdict)

    body <- if (verdict == "yes") {
      p("Cały przedział ", round(conf * 100), "% leży w obszarze hipotezy.
        Przy tym poziomie ufności możemy stwierdzić: ",
        paste0(tolower(substr(cfg$hypothesis$text, 1, 1)),
               substring(cfg$hypothesis$text, 2)), ".")
    } else if (verdict == "no") {
      p("Cały przedział ", round(conf * 100), "% leży poza obszarem hipotezy.
        Dane przemawiają przeciwko niej.")
    } else {
      p("Przedział ", round(conf * 100), "% przecina granicę hipotezy (",
        round(cfg$hypothesis$bound, 3), "). Dane są zgodne z wartościami
        po obu stronach progu, więc przy tym poziomie ufności nie możemy
        hipotezy ani przyjąć, ani odrzucić. Sprawdź pozostałe poziomy.")
    }

    lc_feedback(type = cls,
      p(tags$strong("Hipoteza: "), cfg$hypothesis$text),
      p(tags$strong("Werdykt przy ", round(conf * 100), "% ufności: ", label)),
      body
    )
  }

  # ---- Rejestracja outputow + observerow dla kazdego edge case'a ----
  register_edge_case <- function(case_id) {
    levels <- c(0.90, 0.95, 0.99)
    # Klikniecie poziomu ufnosci -> wybiera conf, RESETUJE revealed na FALSE
    lapply(levels, function(lv) {
      force(lv)
      observeEvent(input[[paste0("ch5_", case_id, "_conf", round(lv * 100))]], {
        ch5_edge_state[[case_id]] <- list(conf = lv, revealed = FALSE)
      }, ignoreInit = TRUE)
    })

    # Przycisk "Pokaz werdykt" -> ustawia revealed = TRUE (zachowujac obecny conf)
    observeEvent(input[[paste0("ch5_", case_id, "_reveal")]], {
      current <- ch5_edge_state[[case_id]]
      if (!is.na(current$conf) && !current$revealed) {
        ch5_edge_state[[case_id]] <- list(conf = current$conf, revealed = TRUE)
      }
    }, ignoreInit = TRUE)

    output[[paste0("ch5_", case_id, "_buttons")]] <- renderUI({
      ch5_edge_state[[case_id]]
      edge_buttons_ui(case_id)
    })
    zoom_plot_server(paste0("ch5_", case_id, "_plot"), reactive({
      ch5_edge_state[[case_id]]
      render_edge_plot(case_id)
    }))
    output[[paste0("ch5_", case_id, "_explain")]] <- renderUI({
      ch5_edge_state[[case_id]]
      render_edge_explain(case_id)
    })
  }

  for (cid in names(edge_cases)) {
    register_edge_case(cid)
  }
}
