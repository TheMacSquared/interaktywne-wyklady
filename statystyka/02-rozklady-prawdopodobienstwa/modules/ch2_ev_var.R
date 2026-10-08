# ============================================================================
# CHAPTER 2: Wartość oczekiwana i wariancja
# ============================================================================

ch2_ev_var_ui <- list(
  id = "ch-ev-var", num = "02", title = "Wartość oczekiwana i wariancja",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 02 · Rozkłady prawdopodobieństwa",
      num    = "02",
      title  = "Wartość oczekiwana i wariancja.",
      lead   = "Rozkład prawdopodobieństwa wymienia wszystkie możliwe wyniki, ale do
                porównań potrzebny jest krótszy opis. Dane streszczaliśmy średnią
                i odchyleniem standardowym. Rozkład streszczają ich odpowiedniki:
                wartość oczekiwana mówi, na jaki wynik można liczyć w długim okresie,
                a wariancja — jak daleko od niego wypadają pojedyncze wyniki."
    ),

    lc_h2("ch2-ev-intro", "Od średniej do wartości oczekiwanej"),

    lc_p("W poprzednim rozdziale ", gloss("rozkład prawdopodobieństwa"), " opisywał
      ", gloss("zmienna losowa", "zmienną losową"), " w całości: podawał jej możliwe wartości i prawdopodobieństwo
      każdej z nich. Taki opis jest kompletny, ale niewygodny, gdy chcemy porównać
      dwa rozkłady. Ten sam problem mieliśmy w wykładzie 01 z danymi. ", gloss("histogram", "Histogram"), "
      pokazywał cały rozkład, a do porównań streszczaliśmy go dwiema liczbami:
      ", gloss("średnia", "średnią"), " i odchyleniem standardowym. Teraz zrobimy
      to samo z rozkładem prawdopodobieństwa."),

    lc_p("Średnią z danych rozumieliśmy jako punkt równowagi. Każda obserwacja
      to jednakowy ciężarek na linijce, a linijka balansuje w punkcie średniej.
      Rozkład nie składa się z obserwacji, tylko z wartości i ich
      prawdopodobieństw. Wystarczy więc, że ciężarek postawiony przy wartości x
      waży tyle, ile wynosi P(X = x). Punkt równowagi tak obciążonej linijki
      to ", gloss("wartość oczekiwana"), " E(X): suma wszystkich wartości
      pomnożonych przez ich prawdopodobieństwa."),

    lc_formula_box(withMathJax(
      "$$E(X) = \\sum_x x \\cdot P(X = x)$$"
    )),

    lc_p("Wartość oczekiwana jest więc średnią ważoną: wartości prawdopodobne
      ważą w niej dużo, mało prawdopodobne — mało. Nie musi przy tym być wartością,
      którą zmienna może przyjąć. Dla rzutu kostką każda liczba oczek ma
      prawdopodobieństwo 1/6, więc E(X) = (1 + 2 + 3 + 4 + 5 + 6) · 1/6 = 3.5,
      choć 3.5 oczka nigdy nie wypada."),

    # ========================================================================
    # WIDGET 1: Loterie — symulacja wartości oczekiwanej
    # ========================================================================
    lc_h2("ch2-loterie", "Czego się spodziewać? — gra w loterie"),

    lc_p("Zacznijmy od zdrapki kupionej w kiosku."),

    # PROTOTYP SCENY (2026-10-08): Zdrap los (E(X) na dłuższą metę, ryzyko jako rozrzut)
    figure_panel(
      label = "Prototyp sceny",
      width_mode = "text",
      scene_widget("ch2_zdrapka", "Zdrap los: od jednego losu do wartości oczekiwanej",
        steps = c("Zdrapka", "Bilans X", "Powtarzamy", "E(X) i rozrzut"),
        labels = c("Kup i zdrap los", "Kup i zdrap los", "Kup i zdrap los", "Kup i zdrap los"),
        options = list(list(name = "ticket", label = "Los",
                            values = c("Zdrapka" = "main", "Pewne 4 zł" = "sure", "10% na 40 zł" = "risky"),
                            selected = "main", from = 3)),
        config = list(kind = "scratch", ticket = "main", price = scene_ticket_price, height = 436,
                      tickets = lapply(scene_tickets, function(t) {
                        t$prizes <- I(t$prizes); t$probs <- I(t$probs); t }),
                      aria = "Kupujący zdrapuje los z kiosku; histogram bilansów (wygrana minus cena) i bieżący średni bilans na tle wartości oczekiwanej"))
    ),

    lc_p("Nazwa „oczekiwana” bierze się z gier losowych. Wartość oczekiwana
      wygranej mówi, ile średnio przynosi jedna gra, jeśli gramy wiele razy.
      Panel pozwala zagrać w jedną z czterech loterii. Dla każdej znamy wygrane
      i ich prawdopodobieństwa, więc E(X) liczymy ze wzoru. Dla loterii A
      to 0.5 · 10 + 0.5 · 0 = 5 zł, dla B pewne 4 zł, dla C 0.1 · 100 + 0.9 · 0
      = 10 zł, a dla D 0.6 · 8 + 0.4 · (-5) = 2.8 zł. Wykres pokazuje średnią
      wygraną ze wszystkich dotychczasowych gier (linia ciągła) na tle E(X)
      (linia przerywana)."),

    figure_panel(
      label = "Ryc. 2.1",
      title = "Gra w loterie",
      full_width = TRUE,
      lc_toolbar(
        lc_group(NULL, grow = TRUE,
          selectInput("ch2ev_lottery", "Loteria",
            choices = c(
              "A: 50% → 10 zł, 50% → 0 zł"  = "A",
              "B: 100% → 4 zł (pewna)"       = "B",
              "C: 10% → 100 zł, 90% → 0 zł" = "C",
              "D: 60% → 8 zł, 40% → -5 zł"  = "D"
            ),
            selected = "A"
          )
        ),
        lc_action_group(ch2ev_play_1 = "1×", ch2ev_play_10 = "10×",
                        ch2ev_play_100 = "100×", ch2ev_play_1000 = "1000×",
                        label = "Graj"),
        lc_action("ch2ev_reset_lottery", icon = "reset", variant = "ghost", aria_label = "Reset"),
        lc_readouts(uiOutput("ch2ev_play_count"), uiOutput("ch2ev_lottery_stats"))
      ),
      lc_plot("ch2ev_convergence_plot", max_height = "300px")
    ),

    lc_p("Po kilku grach średnia skacze. W loterii C pierwsza gra daje średnią
      100 zł albo 0 zł, nigdy 10 zł. Z każdą kolejną grą pojedynczy wynik waży
      w średniej coraz mniej, wahania słabną, a linia ciągła zbliża się do
      przerywanej. To ", gloss("prawo wielkich liczb"), " z poprzedniego rozdziału,
      tym razem dla średniej zamiast częstości: średnia z wielu powtórzeń zbliża
      się do wartości oczekiwanej. W loterii C dzieje się to zwykle wolniej niż
      w A, bo jej wygrane są bardziej rozrzucone. Loteria B nie ma czego
      stabilizować: każda gra daje 4 zł, więc średnia od początku leży na E(X)."),

    lc_p("Jeśli liczy się tylko średni zysk, najbardziej opłaca się loteria C:
      10 zł na grę, dwa razy więcej niż A i dwa i pół raza więcej niż pewne 4 zł
      z B. Mimo to wiele osób wybrałoby B, bo w loterii C dziewięć gier na dziesięć
      kończy się niczym. Wartość oczekiwana tej różnicy nie widzi. Wrócimy do niej
      za chwilę, przy wariancji."),

    # ========================================================================
    # WIDGET 2: Punkt równowagi
    # ========================================================================
    lc_h2("ch2-rownowaga", "E(X) jako punkt równowagi"),

    lc_p("Wzór na E(X) to dosłownie przepis na punkt równowagi z początku
      rozdziału. Panel pokazuje go dla zmiennej przyjmującej wartości 1, 3, 5
      i 9, z prawdopodobieństwami wpisywanymi w pola. Słupki są ciężarkami,
      trójkąt pod osią to punkt podparcia, a pod wykresem widać pełne obliczenie.
      Prawdopodobieństwa muszą sumować się do 1. Licznik ∑P pokazuje sumę
      wpisanych liczb, a gdy różni się od 1, panel dzieli każdą z nich przez tę
      sumę."),

    figure_panel(
      label = "Ryc. 2.2",
      title = "Punkt równowagi rozkładu",
      full_width = TRUE,
      lc_toolbar(
        lc_group(NULL, numericInput("ch2ev_bal_p1", "P(X = 1)", 0.25, min = 0, max = 1, step = 0.05)),
        lc_group(NULL, numericInput("ch2ev_bal_p2", "P(X = 3)", 0.25, min = 0, max = 1, step = 0.05)),
        lc_group(NULL, numericInput("ch2ev_bal_p3", "P(X = 5)", 0.25, min = 0, max = 1, step = 0.05)),
        lc_group(NULL, numericInput("ch2ev_bal_p4", "P(X = 9)", 0.25, min = 0, max = 1, step = 0.05)),
        lc_action_group(ch2ev_bal_sym = "Symetryczny", ch2ev_bal_skew = "Skośny",
                        ch2ev_bal_bimod = "Dwumodalny", label = "Ustawienia"),
        lc_readouts(uiOutput("ch2ev_bal_sum"))
      ),
      lc_plot("ch2ev_balance_plot", max_height = "350px"),
      uiOutput("ch2ev_balance_text")
    ),

    lc_p("Przy równych prawdopodobieństwach E(X) = 4.5. To zwykła średnia
      czterech wartości, bo każda waży tyle samo. Gdyby największą wartością
      było 7, a nie 9, punkt równowagi wypadłby w 4. Odległa wartość ciągnie
      E(X) w swoją stronę tak samo, jak ", gloss("wartość odstająca"), " ciągnie średnią
      z danych."),

    lc_p("Ustawienie „Skośny” przenosi ciężar na wysokie wartości:
      P(X = 9) = 0.5, a E(X) rośnie do 6.5. Ustawienie „Dwumodalny” kładzie
      po 0.4 na skrajne wartości 1 i 9 i po 0.1 na środkowe. E(X) = 4.8 wypada
      wtedy między dwoma szczytami, w miejscu, którego zmienna w ogóle nie
      przyjmuje. O położeniu E(X) decydują więc dwie rzeczy naraz: jak
      prawdopodobna jest wartość i jak daleko leży od pozostałych. Sama E(X)
      nie mówi natomiast nic o kształcie rozkładu."),

    # ========================================================================
    # WIDGET 3: Ryzyko a rozrzut — intuicja wariancji
    # ========================================================================
    lc_h2("ch2-wariancja", "Wariancja — rozrzut wokół oczekiwania"),

    lc_p("Loterie A i C różniły się nie tylko wartością oczekiwaną. W A każda
      wygrana leży 5 zł od E(X), w C wynik 100 zł leży aż 90 zł od E(X) = 10 zł.
      W wykładzie 01 taki rozrzut danych wokół średniej mierzyliśmy wariancją
      i odchyleniem standardowym. Dla rozkładu robimy to samo, tylko kwadraty
      odchyleń od E(X) ważymy prawdopodobieństwami, a nie dzielimy przez liczbę
      obserwacji. Tak powstaje ", gloss("wariancja"), " Var(X). Jej pierwiastek
      to ", gloss("odchylenie standardowe"), " SD(X), wyrażone w tych samych
      jednostkach co X."),

    lc_formula_box(withMathJax(
      "$$Var(X) = \\sum_x \\big(x - E(X)\\big)^2 \\cdot P(X = x) \\qquad SD(X) = \\sqrt{Var(X)}$$"
    )),

    lc_p("Dla loterii A: Var(X) = 0.5 · (10 - 5)² + 0.5 · (0 - 5)² = 25 zł²,
      więc SD(X) = 5 zł. Dla loterii C: Var(X) = 0.1 · (100 - 10)² +
      0.9 · (0 - 10)² = 900 zł², więc SD(X) = 30 zł. Loteria B ma wariancję 0,
      bo jej jedyny wynik równa się E(X). Teraz widać, co odróżnia C od B:
      C ma wyższą wartość oczekiwaną, ale jej odchylenie standardowe jest sześć
      razy większe niż w A, a B nie ma rozrzutu wcale."),

    lc_p("Żeby oddzielić rozrzut od położenia, panel porównuje trzy loterie
      o tej samej wartości oczekiwanej, E(X) = 50 zł. Loteria A daje zawsze
      50 zł. Loteria B daje 0 albo 100 zł, każdą kwotę z prawdopodobieństwem 0.5.
      Loteria C wypłaca dowolną kwotę od 0 do 100 zł, a każda jest równie
      prawdopodobna. Panel symuluje wybraną liczbę gier i rysuje histogram wygranych
      każdej loterii."),

    figure_panel(
      label = "Ryc. 2.3",
      title = "Trzy loterie, jedno E(X), różne ryzyko",
      full_width = TRUE,
      lc_toolbar(
        lc_slider("ch2ev_var_n", "Ile razy zagrać?", 10, 2000, 200, 10),
        lc_action("ch2ev_var_sim", "Symuluj", variant = "solid")
      ),
      lc_plot("ch2ev_var_plot", max_height = "400px"),
      uiOutput("ch2ev_var_summary")
    ),

    lc_p("Trzy histogramy mają ten sam środek, a zupełnie różny kształt.
      W loterii A wszystkie wygrane trafiają w jeden słupek nad 50 zł: Var(X) = 0.
      W B wygrane tworzą dwa słupki na krańcach, każda leży 50 zł od E(X), więc
      Var(X) = 2500 zł², a SD(X) = 50 zł. W C wygrane rozkładają się równomiernie
      od 0 do 100 zł. To zmienna ciągła, którą poznamy w rozdziale 4; jej wariancja
      wynosi około 833 zł², a SD(X) ≈ 28.9 zł. Średnie i odchylenia standardowe
      w tabeli obok zmieniają się z każdą symulacją, ale przy setkach gier trzymają
      się blisko tych wartości teoretycznych."),

    lc_p("Przy grach losowych i inwestycjach odchylenie standardowe czyta się jako
      ryzyko. Mała wariancja oznacza wyniki skupione blisko E(X), duża — wyniki,
      które często i daleko od niej odbiegają. Zerowa wariancja oznacza brak
      losowości: wynik jest pewny. Pełny opis zmiennej losowej wymaga więc
      co najmniej dwóch liczb: E(X) mówi, gdzie leży środek, a SD(X), jak szeroko
      rozkładają się wokół niego wyniki."),

    lc_note("Zasada", rule = TRUE,
      "Wariancja ma jednostkę do kwadratu (zł²), odchylenie standardowe — tę samą
       co X (zł). Do opisu rozrzutu używaj SD, wariancja przydaje się w obliczeniach."
    ),

    # ========================================================================
    # Podsumowanie
    # ========================================================================
    lc_h2("ch2-od-danych", "Od danych do modelu"),

    lc_p("Każde pojęcie z tego rozdziału ma odpowiednik w ", gloss("statystyka opisowa", "statystyce opisowej"), "
      z wykładu 01. Wzory też są analogiczne: w średniej z próby każda obserwacja
      ma wagę 1/n, a w E(X) wartość x ma wagę P(X = x). Różnica leży w źródle
      liczb: statystyki z lewej kolumny liczymy z zebranych danych, parametry
      z prawej — z modelu, czyli z rozkładu prawdopodobieństwa."),

    lc_table(
      data.frame(
        what  = c("Środek", "Rozrzut (kwadrat)", "Rozrzut (jednostki)", "Źródło liczb"),
        data  = c("Średnia z próby x̄", "Wariancja z próby s²", "Odchylenie standardowe s",
                  "Zebrane dane"),
        model = c("Wartość oczekiwana E(X)", "Wariancja Var(X)", "Odchylenie standardowe SD(X)",
                  "Rozkład prawdopodobieństwa"),
        stringsAsFactors = FALSE
      ),
      cols = list(
        lc_col("what", "", "row"),
        lc_col("data", "Dane", "text"),
        lc_col("model", "Model", "text")
      ),
      prose = TRUE,
      label = "Porównanie statystyki opisowej i modelu",
      caption = "Statystyka opisowa liczy wielkości z danych, rachunek prawdopodobieństwa wyznacza ich odpowiedniki z modelu."
    ),

    lc_p("Obie kolumny łączy prawo wielkich liczb, które widzieliśmy przy
      loteriach: im więcej obserwacji, tym bliżej x̄ leży E(X). Dlatego
      statystyki z próby mogą służyć do szacowania parametrów rozkładu, z którego
      dane pochodzą."),

    lc_chapter_next(
      num       = "03",
      title     = "Rozkłady dyskretne",
      lead      = "jak E(X) i Var(X) zależą od parametrów konkretnych rozkładów.",
      target_id = "ch-dyskretne"
    )
  )
)

# --------------------------------------------------------------------------
# Chapter 2 Server
# --------------------------------------------------------------------------

ch2_ev_var_server <- function(input, output, session) {

  # --- PROTOTYP SCENY (2026-10-08): Zdrap los ---
  tk <- scene_tickets$main
  scene_texts(input, output, "ch2_zdrapka", list(
    tagList("W kiosku los kosztuje ", lc_fmt(scene_ticket_price), " zł. Na zdrapce można wygrać 4, 10 albo 100 zł,
      ale najczęściej pod srebrną farbą nie ma nic. Kup i zdrap kilka losów."),
    tagList("Liczymy, ile naprawdę zyskaliśmy na jednym losie: wygraną minus cenę. Ten bilans oznaczamy ",
      tags$code("X", .noWS = "outside"), ". Przed zdrapaniem go nie znamy, więc to zmienna losowa o czterech
      możliwych wartościach: -5, -1, 5 i 95 zł. Pusty los to nie 0, tylko -5 zł."),
    tagList("Lewy wykres zlicza bilanse, prawy pokazuje średni bilans ze wszystkich dotąd kupionych losów.
      Na początku średnia skacze, zwłaszcza po trafieniu 100 zł. Dołóż 100 i 1000 losów: linia się uspokaja,
      i to poniżej zera. Potem zmień los na pewne 4 zł albo 10% na 40 zł."),
    tagList("Średnia na dłuższą metę to wartość oczekiwana: dla zdrapki E(X) = ", sprintf("%.2f", scene_ticket_ev(tk)),
      " zł, czyli na każdym losie średnio tracisz ", sprintf("%.2f", -scene_ticket_ev(tk)), " zł.
      Odchylenie standardowe mówi, jak daleko od E(X) wypadają pojedyncze bilanse. Los pewny i los 10% na 40 zł
      mają tę samą E(X) = ", sprintf("%.2f", scene_ticket_ev(scene_tickets$sure)), " zł, ale pierwszy ma SD = 0,
      a drugi SD = ", lc_fmt(scene_ticket_sd(scene_tickets$risky), 0), " zł: to jest ryzyko.")
  ))

  # --- Definicje loterii ---
  lottery_defs <- list(
    A = list(outcomes = c(10, 0), probs = c(0.5, 0.5), ev = 5,
             label = "A: 50/50 na 10 zł lub 0 zł"),
    B = list(outcomes = c(4), probs = c(1), ev = 4,
             label = "B: Pewne 4 zł"),
    C = list(outcomes = c(100, 0), probs = c(0.1, 0.9), ev = 10,
             label = "C: 10% na 100 zł"),
    D = list(outcomes = c(8, -5), probs = c(0.6, 0.4), ev = 2.8,
             label = "D: 60% na 8 zł, 40% na -5 zł")
  )

  # --- Widget 1: Loterie ---
  lottery_results <- reactiveVal(numeric(0))

  play_lottery <- function(n) {
    lot <- lottery_defs[[input$ch2ev_lottery]]
    idx <- sample.int(length(lot$outcomes), n, replace = TRUE, prob = lot$probs)
    new_results <- lot$outcomes[idx]
    lottery_results(c(lottery_results(), new_results))
  }

  observeEvent(input$ch2ev_play_1, { play_lottery(1) })
  observeEvent(input$ch2ev_play_10, { play_lottery(10) })
  observeEvent(input$ch2ev_play_100, { play_lottery(100) })
  observeEvent(input$ch2ev_play_1000, { play_lottery(1000) })
  observeEvent(input$ch2ev_reset_lottery, lottery_results(numeric(0)))
  observeEvent(input$ch2ev_lottery, lottery_results(numeric(0)))

  output$ch2ev_play_count <- renderUI({
    n <- length(lottery_results())
    lc_readout("Gier", n, color = unname(upwr_cat["niebo"]))
  })

  zoom_plot_server("ch2ev_convergence_plot", reactive({
    results <- lottery_results()
    lot <- lottery_defs[[input$ch2ev_lottery]]

    if (length(results) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij „Graj”, aby rozpocząć",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      # Średnia krocząca
      running_mean <- cumsum(results) / seq_along(results)
      df <- data.frame(n = seq_along(results), mean = running_mean)

      ggplot(df, aes(x = n, y = mean)) +
        geom_line(color = unname(upwr_cat["niebo"]), linewidth = 1) +
        geom_hline(yintercept = lot$ev, color = unname(upwr_cat["terakota"]),
                   linewidth = 1.2, linetype = "dashed") +
        annotate("text", x = max(df$n) * 0.95, y = lot$ev,
                 label = paste0("E(X) = ", lot$ev),
                 color = unname(upwr_cat["terakota"]), fontface = "bold", size = 5,
                 vjust = -1) +
        scale_y_continuous(limits = c(
          min(min(running_mean), lot$ev) - abs(lot$ev) * 0.3,
          max(max(running_mean), lot$ev) + abs(lot$ev) * 0.3
        )) +
        labs(
             x = "Liczba gier", y = "Średnia wygrana (zł)") +
        theme_upwr()
    }
  }))

  output$ch2ev_lottery_stats <- renderUI({
    results <- lottery_results()
    lot <- lottery_defs[[input$ch2ev_lottery]]
    req(length(results) > 0)

    obs_mean <- round(mean(results), 2)
    diff <- abs(obs_mean - lot$ev)

    tagList(
      lc_readout("Śr. dotychczasowa", paste0(obs_mean, " zł"), color = unname(upwr_cat["niebo"])),
      lc_readout("E(X)", paste0(lot$ev, " zł"), color = unname(upwr_cat["terakota"])),
      lc_readout("Różnica", paste0(round(diff, 2), " zł"), color = if (diff < 0.5) unname(upwr_cat["szalwia"]) else unname(upwr_cat["bursztyn"]))
    )
  })

  # --- Widget 2: Punkt równowagi ---
  observeEvent(input$ch2ev_bal_sym, {
    updateNumericInput(session, "ch2ev_bal_p1", value = 0.25)
    updateNumericInput(session, "ch2ev_bal_p2", value = 0.25)
    updateNumericInput(session, "ch2ev_bal_p3", value = 0.25)
    updateNumericInput(session, "ch2ev_bal_p4", value = 0.25)
  })
  observeEvent(input$ch2ev_bal_skew, {
    updateNumericInput(session, "ch2ev_bal_p1", value = 0.05)
    updateNumericInput(session, "ch2ev_bal_p2", value = 0.15)
    updateNumericInput(session, "ch2ev_bal_p3", value = 0.30)
    updateNumericInput(session, "ch2ev_bal_p4", value = 0.50)
  })
  observeEvent(input$ch2ev_bal_bimod, {
    updateNumericInput(session, "ch2ev_bal_p1", value = 0.40)
    updateNumericInput(session, "ch2ev_bal_p2", value = 0.10)
    updateNumericInput(session, "ch2ev_bal_p3", value = 0.10)
    updateNumericInput(session, "ch2ev_bal_p4", value = 0.40)
  })

  # Wpisane liczby (puste i ujemne pola liczą się jako 0) i rozkład po
  # przeskalowaniu do sumy 1.
  ch2ev_bal <- reactive({
    typed <- c(input$ch2ev_bal_p1, input$ch2ev_bal_p2,
               input$ch2ev_bal_p3, input$ch2ev_bal_p4)
    typed <- vapply(seq_len(4), function(i) {
      v <- typed[i]
      if (length(v) == 0 || is.na(v) || v < 0) 0 else v
    }, numeric(1))
    total <- sum(typed)
    probs <- if (total > 0) typed / total else rep(0.25, 4)
    list(typed = typed, total = total, probs = probs)
  })

  output$ch2ev_bal_sum <- renderUI({
    s <- ch2ev_bal()$total
    if (abs(s - 1) < 0.005) {
      lc_readout("∑P", paste0(sprintf("%.2f", s), " ✔"), color = unname(upwr_cat["szalwia"]))
    } else {
      lc_readout("∑P", paste0(sprintf("%.2f", s), " ≠ 1"), color = unname(upwr_cat["terakota"]))
    }
  })

  zoom_plot_server("ch2ev_balance_plot", reactive({
    x_vals <- c(1, 3, 5, 9)
    probs <- ch2ev_bal()$probs

    ev <- sum(x_vals * probs)

    df <- data.frame(x = x_vals, prob = probs)

    ggplot(df, aes(x = x, y = prob)) +
      geom_col(fill = unname(upwr_cat["niebo"]), color = "white", alpha = 0.85, width = 0.6) +
      geom_text(aes(label = sprintf("%.2f", prob)), vjust = -0.5, size = 4.5) +
      # Oś belki
      geom_segment(aes(x = 0, xend = 10, y = -0.01, yend = -0.01),
                   color = upwr_secondary, linewidth = 1.5) +
      # Trójkąt — punkt równowagi
      annotate("point", x = ev, y = -0.03,
               shape = 17, size = 6, color = unname(upwr_cat["terakota"])) +
      annotate("text", x = ev, y = -0.06,
               label = paste0("E(X) = ", round(ev, 2)),
               color = unname(upwr_cat["terakota"]), fontface = "bold", size = 5) +
      scale_y_continuous(limits = c(-0.08, max(probs) * 1.3),
                         expand = expansion(mult = c(0, 0.05))) +
      scale_x_continuous(breaks = x_vals, limits = c(0, 10)) +
      labs(
           x = "Wartość (x)", y = "Prawdopodobieństwo P(X = x)") +
      theme_upwr()
  }))

  output$ch2ev_balance_text <- renderUI({
    x_vals <- c(1, 3, 5, 9)
    bal <- ch2ev_bal()
    probs <- bal$probs
    ev <- sum(x_vals * probs)

    calc_parts <- paste(
      sapply(seq_along(x_vals), function(i) {
        paste0(x_vals[i], "·", sprintf("%.2f", probs[i]))
      }),
      collapse = " + "
    )

    lc_status(
      tags$strong("Obliczenie:"),
      paste0(" ", "E(X) = ", calc_parts, " = ", round(ev, 2)),
      if (abs(bal$total - 1) >= 0.005 && bal$total > 0) {
        tags$p(paste0("Wpisane liczby sumują się do ", sprintf("%.2f", bal$total),
                      ", więc każdą podzielono przez tę sumę."))
      } else if (bal$total == 0) {
        tags$p("Wszystkie pola są puste lub zerowe, więc panel przyjął równe prawdopodobieństwa.")
      }
    )
  })

  # --- Widget 3: Ryzyko a rozrzut ---
  ch2ev_var_data <- reactive({
    input$ch2ev_var_sim
    req(input$ch2ev_var_n)
    n <- input$ch2ev_var_n
    list(
      a  = rep(50, n),
      b  = sample(c(0, 100), n, replace = TRUE),
      c  = runif(n, 0, 100),
      n  = n
    )
  })

  zoom_plot_server("ch2ev_var_plot", reactive({
    d <- ch2ev_var_data()

    df <- data.frame(
      value = c(d$a, d$b, d$c),
      lottery = rep(c("A: Pewne 50 zł\n(Var = 0)",
                       "B: 0 lub 100 zł\n(Var = 2500)",
                       "C: Losowe 0–100 zł\n(Var ≈ 833)"),
                    each = d$n)
    )
    df$lottery <- factor(df$lottery, levels = unique(df$lottery))

    ggplot(df, aes(x = value)) +
      geom_histogram(bins = 30, fill = unname(upwr_cat["niebo"]), color = "white", alpha = 0.7) +
      geom_vline(xintercept = 50, color = unname(upwr_cat["terakota"]), linewidth = 1.2, linetype = "dashed") +
      facet_wrap(~lottery, ncol = 3) +
      annotate("text", x = 50, y = Inf, label = "E(X) = 50",
               color = unname(upwr_cat["terakota"]), fontface = "bold", size = 4, vjust = 2) +
      labs(
           x = "Wygrana (zł)", y = "Liczebność") +
      theme_upwr(base_size = 12)
  }))

  output$ch2ev_var_summary <- renderUI({
    d <- ch2ev_var_data()

    lc_table(
      data.frame(
        lottery = c("A: pewna", "B: 0/100", "C: losowa"),
        sd = c(sd(d$a), sd(d$b), sd(d$c)),
        mean = c(mean(d$a), mean(d$b), mean(d$c))
      ),
      cols = list(
        lc_col("lottery", "Loteria", "row"),
        lc_col("sd", "SD", digits = 1),
        lc_col("mean", "Średnia", digits = 1)
      ),
      fit = TRUE, label = "Rozrzut wygranych w trzech loteriach"
    )
  })

}
