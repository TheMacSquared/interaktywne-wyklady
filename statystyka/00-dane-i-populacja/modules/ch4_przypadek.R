# ============================================================================
# CHAPTER 4: Przypadek czy wzorzec?
# ============================================================================

# Plansza lineup: 9 wykresów rozrzutu, jeden z prawdziwymi danymi,
# osiem z przetasowaną zmienną y (związek zniszczony losowo).
make_lineup_board <- function(n, r) {
  x <- rnorm(n)
  y <- r * x + sqrt(1 - r^2) * rnorm(n)
  pos <- sample(9, 1)
  panels <- lapply(1:9, function(k) {
    data.frame(panel = k, x = x, y = if (k == pos) y else sample(y))
  })
  list(data = do.call(rbind, panels), pos = pos, n = n, r = r)
}

ch4_ui <- list(
  id    = "ch-przypadek",
  num   = "04",
  title = "Przypadek czy wzorzec?",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 04 · Dane i populacja",
      num    = "04",
      title  = "Szum też układa się we wzory.",
      lead   = "Dwie zmienne w małej próbie potrafią wyglądać na związane,
                choć w populacji nie mają ze sobą nic wspólnego. Zanim
                nauczymy się to sprawdzać wzorami, wypróbujemy to na własnym
                oku: wskaż wykres z prawdziwymi danymi wśród ośmiu
                podróbek."
    ),

    lc_p("W poprzednim rozdziale odsetek p̂ zmieniał się od próby do próby.
      To samo dotyczy każdej innej cechy próby, także wzorca między dwiema
      zmiennymi. Jeśli wylosujemy 15 studentów i narysujemy czas nauki
      przeciw wynikowi kolokwium, punkty prawie nigdy nie ułożą się
      w idealnie płaską chmurę, nawet gdy w całej populacji nauka
      z wynikiem nie ma żadnego związku. Oko widzi wtedy trend, którego
      nie ma."),

    lc_h2("ch4-lineup", "Znajdź prawdziwe dane"),

    lc_p("Panel poniżej rysuje dziewięć wykresów rozrzutu. W jednym z nich
      leżą prawdziwe dane, w których zmienna na osi y częściowo zależy od x.
      W ośmiu pozostałych te same wartości y zostały losowo przetasowane
      między punktami, więc związek między x a y jest w nich zniszczony.
      Wykresy wyglądają tak samo, bo mają dokładnie te same wartości,
      tylko inaczej połączone w pary. Wybierz numer wykresu, który według
      Ciebie pokazuje prawdziwe dane, i sprawdź."),

    figure_panel(
      label = "Ryc. 4.1",
      title = "Który z dziewięciu wykresów pokazuje prawdziwe dane?",
      width_mode = "wide",
      lc_toolbar(
        lc_slider("ch4_n", "Liczebność próby (n)", 10, 200, 30, 5),
        lc_slider("ch4_r", "Siła związku (r)", 0.1, 0.9, 0.4, 0.1),
        lc_segmented("ch4_guess", "Twój typ", choices = c(
          "–" = "0", "1" = "1", "2" = "2", "3" = "3", "4" = "4",
          "5" = "5", "6" = "6", "7" = "7", "8" = "8", "9" = "9"
        ), selected = "0"),
        lc_action("ch4_check", label = "Sprawdź", variant = "solid"),
        lc_action("ch4_new", icon = "shuffle", variant = "ghost",
                  aria_label = "Nowa plansza"),
        lc_readouts(uiOutput("ch4_reads"))
      ),
      lc_plot("ch4_lineup_plot", ratio = "1.5/1", max_height = "560px"),
      uiOutput("ch4_feedback")
    ),

    lc_p("Siła związku r to współczynnik korelacji: 0 oznacza brak związku,
      a 1 idealną linię prostą rosnącą. Dokładnie omówimy go w wykładzie 06,
      tu wystarczy, że większe r daje wyraźniejszy trend. Dobrze
      jest wypróbować kilka ustawień. Przy n = 15 i r = 0.3 prawidłowy wykres
      wskazuje się trafnie mniej więcej raz na trzy plansze, a przy
      n = 100 i tym samym r już w prawie 9 planszach na 10. Przy
      n = 15 i r = 0.6 trafność wynosi około 70%. Tych liczb nie trzeba
      pamiętać, wystarczy wrażenie, że to samo r raz jest ledwo widoczne,
      a innym razem oczywiste, i że decyduje o tym liczebność próby."),

    lc_note("Zasada", rule = TRUE,
      "Wzorzec w próbie to jeszcze nie wzorzec w populacji. Zanim uznamy,
       że coś widać, trzeba sprawdzić, jak często podobny obraz wychodzi
       z samego przypadku."
    ),

    lc_h2("ch4-szum", "Ile r potrafi wyprodukować sam przypadek"),

    lc_p("Wykresy z tasowaniem to dobry sposób, żeby zobaczyć, że przypadek
      daje wzorce. Można też policzyć, jak często i jak silne. Wyobraźmy
      sobie populację, w której dwie zmienne naprawdę nie mają ze sobą
      związku, czyli r = 0. Losujemy z niej próbę o liczebności n
      i liczymy r z próby, a potem powtarzamy to tysiąc razy. Wykres
      pokazuje, jak rozrzucone są te tysiąc wartości."),

    figure_panel(
      label = "Ryc. 4.2",
      title = "Wartości r z prób, gdy w populacji nie ma żadnego związku",
      width_mode = "wide",
      lc_toolbar(
        lc_slider("ch4b_n", "Liczebność próby (n)", 10, 200, 20, 5),
        lc_slider("ch4b_thr", "Próg |r|", 0.1, 0.8, 0.3, 0.05),
        lc_readouts(uiOutput("ch4b_reads"))
      ),
      lc_plot("ch4b_plot", ratio = "2.4/1"),
      uiOutput("ch4b_caption")
    ),

    lc_p("W populacji bez związku próby nadal dają r różne od zera.
      Rozrzut jest mniej więcej taki sam po obu stronach zera i kurczy się,
      gdy n rośnie. Przy n = 10 próbę z |r| ≥ 0.3 dostajemy w około
      ", lc_fmt(100 * 2 * pt(-0.3 * sqrt(8) / sqrt(1 - 0.09), 8), 0), "% przypadków, przy n = 30
      w około ", lc_fmt(100 * 2 * pt(-0.3 * sqrt(28) / sqrt(1 - 0.09), 28), 0), "%, a przy n = 100 w ułamku procenta."),

    lc_p("To jest ta sama zmienność próbkowa, którą widzieliśmy dla p̂,
      tylko opisująca inną statystykę. Wniosek też jest ten sam: jedna
      próba mówi o populacji tylko tyle, ile pozwala jej rozrzut, który
      zależy od n."),

    lc_h2("ch4-test", "Skąd wziąć regułę zamiast oka"),

    lc_p("Gra z lineupem to w istocie test statystyczny, tylko bez wzoru.
      Przypuszczamy, że związku nie ma, i generujemy kilka obrazów, jakie
      dałby sam przypadek. Potem sprawdzamy, czy prawdziwe dane wyróżniają
      się z tłumu. Jeśli ktoś wskaże właściwy wykres spośród dziewięciu,
      to gdyby związku nie było, trafiłby przypadkiem z szansą 1/9,
      czyli około 11%. Im bardziej wyraźny trend, tym mniejsza szansa,
      że to przypadek."),

    lc_p("Wykład 04 rozwija ten pomysł i zastępuje dziewięć wykresów
      jedną liczbą: prawdopodobieństwem, że sam przypadek dałby wynik
      równie wyraźny jak ten, który mamy. Przedziały ufności z wykładu 03
      odpowiadają na pokrewne pytanie z drugiej strony: w jakim zakresie
      może leżeć prawdziwa wartość parametru."),

    lc_warn("Pułapka",
      "Duża liczba zmiennych zwiększa szansę, że któraś para wygląda na
       związaną przypadkiem. Badacz, który przejrzy sto par zmiennych,
       znajdzie kilka „ciekawych” związków nawet w danych losowych."
    ),

    lc_chapter_next(
      num       = "05",
      title     = "Opis i wnioskowanie",
      lead      = "dwa zadania statystyki i mapa kursu",
      target_id = "ch-opis-wnioskowanie"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch4_server <- function(input, output, session) {

  board <- reactiveVal(make_lineup_board(30, 0.4))
  state <- reactiveValues(checked = FALSE, guess = 0L, hits = 0L, tries = 0L)

  new_board <- function() {
    board(make_lineup_board(input$ch4_n %||% 30, input$ch4_r %||% 0.4))
    state$checked <- FALSE
  }

  observeEvent(input$ch4_new, new_board())
  observeEvent(list(input$ch4_n, input$ch4_r), {
    new_board()
    state$hits <- 0L
    state$tries <- 0L
  }, ignoreInit = TRUE)

  observeEvent(input$ch4_check, {
    g <- as.integer(input$ch4_guess %||% 0)
    if (state$checked || g == 0L) return()
    state$checked <- TRUE
    state$guess <- g
    state$tries <- state$tries + 1L
    if (g == board()$pos) state$hits <- state$hits + 1L
  })

  output$ch4_reads <- renderUI({
    tagList(
      lc_readout("trafień", paste0(state$hits, " z ", state$tries)),
      lc_readout("losowe trafienie", "1 z 9")
    )
  })

  output$ch4_feedback <- renderUI({
    b <- board()
    if (!state$checked) {
      return(lc_caption("Wybierz numer wykresu w pasku nad rysunkiem
        i kliknij Sprawdź."))
    }
    rs <- vapply(1:9, function(k) {
      d <- b$data[b$data$panel == k, ]
      abs(cor(d$x, d$y))
    }, numeric(1))
    max_null <- max(rs[-b$pos])
    ok <- state$guess == b$pos
    lc_feedback(
      type = if (ok) "ok" else "warning",
      if (ok) "Trafiony. " else paste0("Nie tym razem. Prawdziwe dane
        to wykres ", b$pos, ". "),
      paste0("W prawdziwych danych |r| = ", lc_fmt(rs[b$pos], 2),
             ", a największe |r| wśród ośmiu podróbek wyniosło ",
             lc_fmt(max_null, 2), ". ",
             if (rs[b$pos] <= max_null)
               "Sam przypadek dał więc trend wyraźniejszy niż prawdziwy związek."
             else
               "Przetasowane dane też potrafią dać wyraźny trend, choć słabszy.")
    )
  })

  zoom_plot_server("ch4_lineup_plot", reactive({
    b <- board()
    d <- b$data
    d$label <- factor(d$panel, levels = 1:9)
    d$is_real <- state$checked & d$panel == b$pos
    d$is_guess <- state$checked & d$panel == state$guess
    strip_lab <- vapply(1:9, function(k) {
      if (!state$checked) as.character(k)
      else if (k == b$pos) paste0(k, " · prawdziwe dane")
      else if (k == state$guess) paste0(k, " · Twój typ")
      else as.character(k)
    }, character(1))
    names(strip_lab) <- 1:9
    ggplot(d, aes(x, y, color = is_real)) +
      geom_point(size = 1.8, alpha = 0.8) +
      facet_wrap(~panel, nrow = 3, labeller = as_labeller(strip_lab)) +
      scale_color_manual(values = c(`FALSE` = col_pop, `TRUE` = col_stat),
                         guide = "none") +
      labs(x = "Czas nauki (standaryzowany)",
           y = "Wynik kolokwium (standaryzowany)") +
      theme(axis.text = element_blank(), axis.ticks = element_blank())
  }), alt = "Dziewięć wykresów rozrzutu, w jednym z nich prawdziwe dane")

  # --- Rozkład r przy braku związku ------------------------------------------
  null_r <- reactive({
    n <- input$ch4b_n %||% 20
    set.seed(40 + n)
    replicate(1000, cor(rnorm(n), rnorm(n)))
  })

  output$ch4b_reads <- renderUI({
    r <- null_r()
    thr <- input$ch4b_thr %||% 0.3
    tagList(
      lc_readout("prób", length(r)),
      lc_readout(paste0("|r| ≥ ", lc_fmt(thr, 2)),
                 paste0(lc_fmt(100 * mean(abs(r) >= thr), 1), "%"),
                 color = col_stat, swatch = TRUE)
    )
  })

  output$ch4b_caption <- renderUI({
    r <- null_r()
    thr <- input$ch4b_thr %||% 0.3
    lc_caption(sprintf(
      "W %s z 1000 prób r przekroczyło co do modułu %s, choć w populacji
       związku nie ma.",
      sum(abs(r) >= thr), lc_fmt(thr, 2)
    ))
  })

  zoom_plot_server("ch4b_plot", reactive({
    r <- null_r()
    thr <- input$ch4b_thr %||% 0.3
    d <- data.frame(r = r, extreme = abs(r) >= thr)
    ggplot(d, aes(r, fill = extreme)) +
      geom_histogram(breaks = seq(-1, 1, 0.05), color = "white") +
      geom_vline(xintercept = c(-thr, thr), linetype = "dashed",
                 color = col_param, linewidth = 0.9) +
      scale_fill_manual(values = c(`FALSE` = col_pop, `TRUE` = col_stat),
                        guide = "none") +
      scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.25)) +
      labs(x = "Współczynnik korelacji w próbie, r", y = "Liczba prób")
  }), alt = "Histogram wartości r z prób z populacji bez związku")
}
