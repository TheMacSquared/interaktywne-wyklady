# ============================================================================
# CHAPTER 1: Od danych do prawdopodobieństwa
# ============================================================================

ch1_ui <- list(
  id = "ch-most", num = "01", title = "Od danych do prawdopodobieństwa",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 01 · Rozkłady prawdopodobieństwa",
      num    = "01",
      title  = "Od danych do prawdopodobieństwa.",
      lead   = "Histogram z ankiety opisuje dwustu konkretnych studentów. Żeby
                powiedzieć coś o następnym, potrzebujemy modelu: reguły, która
                mówi, jak często pojawia się każdy wynik. Taką regułą jest rozkład
                prawdopodobieństwa, a częstości z danych są jego przybliżeniem."
    ),

    lc_p("W poprzednim wykładzie opisywaliśmy dane z ankiety: liczyliśmy
      częstości względne kategorii, rysowaliśmy histogramy, obliczaliśmy
      średnią i odchylenie standardowe. Wszystkie te liczby dotyczą jednej
      konkretnej próby. Inna grupa studentów dałaby trochę inne wyniki.
      W tym rozdziale przechodzimy od opisu danych do modelu, który je
      wytwarza. Zaczniemy od rzutów kostką, bo tam model znamy z góry
      i możemy sprawdzić, jak dane się do niego zbliżają."),

    # ========================================================================
    # WIDGET 1: Stabilizacja częstości (rzut kostką)
    # ========================================================================
    lc_h2("ch1-pwl", "Prawo wielkich liczb w akcji"),

    lc_p("Wynik rzutu kostką oznaczmy literą \\(X\\). To ",
      gloss("zmienna losowa"), ": jej wartość zależy od przypadku, a przed
      rzutem znamy tylko możliwe wyniki, od 1 do 6. Dla uczciwej kostki każda
      ścianka ma tę samą szansę, więc \\(P(X = k) = 1/6 \\approx 0.167\\)
      dla każdego \\(k\\). Po \\(n\\) rzutach możemy policzyć, ile razy wypadła
      ścianka \\(k\\), i obliczyć ",
      gloss("częstość względna", "częstość względną"), " \\(n_k / n\\),
      dokładnie tak jak w tabeli częstości z poprzedniego wykładu.
      ", gloss("prawo wielkich liczb", "Prawo wielkich liczb"), " mówi, że
      wraz ze wzrostem liczby rzutów częstość względna zbliża się do
      prawdopodobieństwa."),

    lc_formula_box(withMathJax(
      "$$\\frac{n_k}{n} \\;\\longrightarrow\\; P(X = k) \\qquad \\text{gdy } n \\to \\infty$$"
    )),

    lc_p("Panel rzuca wirtualną kostką. Lewy wykres pokazuje częstości
      względne wszystkich ścianek na tle linii 1/6, prawy — jak zmieniała
      się częstość każdej ścianki w miarę dokładania rzutów."),

    figure_panel(
      label = "Ryc. 1.1",
      title = "Rzuty kostką — stabilizacja częstości",
      width_mode = "wide",
      lc_toolbar(
        lc_action_group(label = "Rzuć kostką",
          ch1_roll_1 = "+1", ch1_roll_10 = "+10",
          ch1_roll_100 = "+100", ch1_roll_1000 = "+1000"),
        lc_action("ch1_roll_reset", icon = "reset", variant = "ghost",
                  aria_label = "Wyzeruj rzuty"),
        lc_readouts(uiOutput("ch1_roll_count"))
      ),
      conditionalPanel("!output.ch1_has_rolls",
        lc_empty("Rzuć kostką, żeby zobaczyć częstości")),
      conditionalPanel("output.ch1_has_rolls",
        lc_plots(
          lc_plot("ch1_freq_bar"),
          lc_plot("ch1_conv_plot")
        )
      )
    ),

    lc_p("Po kilku rzutach częstości są chaotyczne: jedna ścianka może nie
      wypaść ani razu, inna kilka razy z rzędu. Linie na prawym wykresie
      skaczą najpierw gwałtownie, a potem coraz spokojniej zbliżają się do
      1/6. Wielkość tych wahań da się policzyć. Przy 10 rzutach częstość
      ścianki odchyla się od 1/6 typowo o około 0.12, czyli prawie o tyle,
      ile wynosi samo prawdopodobieństwo. Przy 100 rzutach typowe odchylenie
      spada do 0.037, przy 1000 do 0.012, a przy 10 000 do 0.004. Stukrotnie
      więcej rzutów daje dziesięciokrotnie mniejszy błąd."),

    lc_p("Prawo wielkich liczb nie mówi, że kostka „wyrównuje” wyniki. Jeśli
      szóstka długo nie wypadała, w kolejnym rzucie nadal ma szansę 1/6.
      Częstość zbliża się do prawdopodobieństwa dlatego, że początkowe
      nadwyżki i niedobory toną w coraz większej liczbie rzutów, a nie
      dlatego, że ktoś je odrabia."),

    # ========================================================================
    # WIDGET 0: Rozkład empiryczny vs teoretyczny
    # ========================================================================
    lc_h2("ch1-rozklad-emp", "Rozkład empiryczny vs teoretyczny"),

    lc_p("Częstości policzone z danych, czy to dla ścianek kostki, grup krwi,
      czy przedziałów wzrostu, tworzą ", gloss("rozkład empiryczny"), ": opis
      tego, jak często każda wartość pojawiła się w zebranej próbie. Jego
      odpowiednikiem po stronie modelu jest ", gloss("rozkład teoretyczny"),
      ": reguła, która każdej możliwej wartości przypisuje prawdopodobieństwo.
      Model może dotyczyć zmiennej dowolnego typu. Dla uczciwej kostki są to
      prawdopodobieństwa 1/6, dla grupy krwi udziały grup w populacji, a dla
      wzrostu krzywa opisana wzorem i kilkoma parametrami."),

    lc_p("Dla zmiennych o kilku wartościach oba rozkłady porównujemy słupek po
      słupku, tak jak przy kostce. Zmienne ciągłe, takie jak wzrost czy czas
      dojazdu, przyjmują wartości z całego przedziału. Rozkład empiryczny
      pokazuje dla nich ", gloss("histogram"), ", a teoretyczny gładka krzywa
      gęstości. Żeby dały się porównać, oba rysujemy w skali gęstości: łączne
      pole słupków histogramu wynosi 1 i tak samo pole pod krzywą. Wtedy pole
      nad dowolnym przedziałem odpowiada odsetkowi obserwacji w tym przedziale.
      Krzywym gęstości przyjrzymy się dokładniej w rozdziale 4."),

    lc_p("Panel losuje próbę z jednego z trzech modeli i rysuje jej
      histogram. Krzywą modelu można nałożyć na histogram."),

    figure_panel(
      label = "Ryc. 1.2",
      title = "Histogram (dane) vs krzywa gęstości (model)",
      full_width = TRUE,
      lc_toolbar(
        selectInput("ch1_emp_dist", "Rozkład źródłowy",
            choices = c(
              "Wzrost studentów (normalny)" = "normal",
              "Czas dojazdu (skośny)"       = "skewed",
              "Ocena losowa (jednostajny)"   = "uniform"
            ),
            selected = "normal"
          ),
        lc_slider("ch1_emp_n", "Wielkość próby", 20, 5000, 200, 20),
        lc_action("ch1_emp_resample", "Losuj nową próbę", icon = "shuffle", variant = "solid"),
        checkboxInput("ch1_show_hist", "Histogram (dane empiryczne)", value = TRUE),
        checkboxInput("ch1_show_density", "Krzywa gęstości (model teoretyczny)", value = FALSE)
      ),
      lc_plot("ch1_emp_vs_theo", max_height = "380px")
    ),

    lc_p("Wzrost losujemy z rozkładu normalnego o średniej 170 cm
      i odchyleniu standardowym 8 cm. To wartości bliskie tym z ankiety,
      gdzie średni wzrost wynosił 171.1 cm, a odchylenie standardowe 8.1 cm.
      Według modelu 68% osób ma od 162 do 178 cm. Czas dojazdu pochodzi
      z rozkładu skośnego, tego samego, z którego wygenerowano dane ankiety.
      Model ma średnią 35 min i medianę 31.7 min, a w ankiecie wyszło
      35.7 i 32.9 min. Model przewiduje, że 8.8% osób dojeżdża dłużej niż
      godzinę. W ankiecie takich osób było 19 na 200, czyli 9.5%."),

    lc_p("Przy próbie liczącej 200 obserwacji histogram ma zarys krzywej,
      ale jest poszarpany, a każde nowe losowanie zmienia wysokość słupków.
      Przy kilku tysiącach obserwacji słupki układają się niemal dokładnie
      pod krzywą i kolejne losowania prawie się od siebie nie różnią. To samo
      prawo wielkich liczb co przy kostce, tylko zastosowane do przedziałów
      zamiast pojedynczych ścianek. Rozkład teoretyczny jest ideałem, a dane
      są jego niedoskonałym odbiciem, tym wierniejszym, im większa próba."),

    # ========================================================================
    # WIDGET 2: Częstości vs prawdopodobieństwo
    # ========================================================================
    lc_h2("ch1-czestosci", "Częstości vs prawdopodobieństwo"),

    lc_p("Histogram z krzywą porównywaliśmy na oko. Dla zmiennej o kilku
      wynikach zgodność danych z modelem da się zmierzyć jedną liczbą:
      największą różnicą między częstością względną a prawdopodobieństwem.
      Model nie musi przy tym przypisywać wszystkim wynikom tej samej szansy.
      Obok uczciwej kostki panel pokazuje kostkę obciążoną, na której
      szóstka wypada z prawdopodobieństwem 0.5, a każda z pozostałych ścianek
      z prawdopodobieństwem 0.1, oraz rzut monetą."),

    lc_p("Słupki pokazują częstości z wylosowanych obserwacji, punkty
      połączone linią — prawdopodobieństwa z modelu. Pod wykresem panel
      podaje największą różnicę między nimi."),

    figure_panel(
      label = "Ryc. 1.3",
      title = "Porównanie: teoria vs obserwacja",
      full_width = TRUE,
      lc_toolbar(
        lc_segmented("ch1_scenario", "Scenariusz", choices = c(
              "Uczciwa kostka"   = "fair",
              "Obciążona kostka" = "loaded",
              "Moneta"           = "coin"
            ), selected = "fair"),
        lc_slider("ch1_n_obs", "Liczba obserwacji", 10, 5000, 100, 10),
        lc_action("ch1_resample", "Losuj ponownie", icon = "shuffle", variant = "solid")
      ),
      lc_plot("ch1_freq_vs_prob", max_height = "350px"),
      uiOutput("ch1_freq_vs_prob_text")
    ),

    lc_p("Przy 100 rzutach uczciwą kostką największa różnica przekracza 0.05
      mniej więcej w dwóch losowaniach na trzy. Przy 500 rzutach zdarza się
      to już tylko w około 2 losowaniach na 100. Dla monety przy 100 rzutach
      próg 0.05 zostaje przekroczony rzadziej, w około jednym losowaniu
      na trzy, bo największą różnicę wybieramy spośród dwóch wyników,
      a nie sześciu."),

    lc_p("Obciążona kostka pokazuje drugą stronę tej zależności. Przy 100
      rzutach częstość szóstek waha się typowo o 0.05 wokół 0.5, więc nie da
      się jej pomylić z wartością 1/6 ≈ 0.167, jakiej oczekiwalibyśmy od
      uczciwej kostki. Częstości nie tylko przybliżają znany model, ale
      pozwalają też odróżnić jeden model od drugiego. Na tym pomyśle opiera
      się wnioskowanie statystyczne, któremu poświęcimy kolejne wykłady."),

    # ========================================================================
    # WIDGET 3: Czym jest rozkład?
    # ========================================================================
    lc_h2("ch1-rozklad", "Czym jest rozkład prawdopodobieństwa?"),

    lc_p("Każdy z modeli w tym rozdziale przypisywał możliwym wynikom ich
      prawdopodobieństwa. Taki kompletny opis nazywamy ",
      gloss("rozkład prawdopodobieństwa", "rozkładem prawdopodobieństwa"),
      ". Dla zmiennej o skończonej liczbie wyników jest to po prostu lista
      wartości \\(k\\) i prawdopodobieństw \\(P(X = k)\\). Nie każda lista
      liczb jest rozkładem. Prawdopodobieństwa muszą być nieujemne i muszą
      sumować się do 1, bo któryś z wyników na pewno wystąpi."),

    lc_formula_box(withMathJax(
      "$$P(X = k) \\geq 0 \\quad \\text{dla każdego } k, \\qquad \\sum_{k} P(X = k) = 1$$"
    )),

    lc_p("Panel pozwala ustawić prawdopodobieństwa czterech wyników.
      Słupki są zielone tylko wtedy, gdy suma wynosi 1."),

    figure_panel(
      label = "Ryc. 1.4",
      title = "Zbuduj własny rozkład",
      full_width = TRUE,
      lc_toolbar(
        lc_slider("ch1_p1", "P(Wynik A)", 0, 1, 0.25, 0.01),
        lc_slider("ch1_p2", "P(Wynik B)", 0, 1, 0.25, 0.01),
        lc_slider("ch1_p3", "P(Wynik C)", 0, 1, 0.25, 0.01),
        lc_slider("ch1_p4", "P(Wynik D)", 0, 1, 0.25, 0.01),
        lc_readouts(uiOutput("ch1_sum_check"))
      ),
      lc_plot("ch1_custom_dist", max_height = "300px")
    ),

    lc_p("Ustawienie startowe, cztery razy 0.25, to rozkład, w którym każdy
      wynik jest równie prawdopodobny, jak przy kostce z czterema ściankami.
      Po zwiększeniu jednego prawdopodobieństwa suma przekroczy 1 i rozkład
      przestanie być poprawny, dopóki inne nie zostanie zmniejszone. Prawdopodobieństwa
      w rozkładzie konkurują ze sobą: pula wynosi zawsze 1 i można ją tylko
      inaczej podzielić. Ten sam warunek obowiązuje dla zmiennych ciągłych.
      Tam rolę sumy przejmuje pole pod krzywą gęstości, które również
      wynosi 1."),

    lc_note("Zasada", rule = TRUE,
      "Rozkład prawdopodobieństwa opisuje model, a nie dane. Częstości
       z próby przybliżają ten model tym lepiej, im większa jest próba."
    ),

    lc_p("Rozkład zawiera całą informację o zmiennej losowej, ale tak jak
      histogram jest za obszerny, żeby go streścić w jednym zdaniu.
      W statystyce opisowej streszczaliśmy dane średnią i odchyleniem
      standardowym. Ich odpowiedniki dla rozkładów poznamy w następnym
      rozdziale."),

    lc_chapter_next(
      num       = "02",
      title     = "Wartość oczekiwana i wariancja",
      lead      = "czego się spodziewać i jak mierzyć rozrzut wyników.",
      target_id = "ch-ev-var"
    )
  )
)

# --------------------------------------------------------------------------
# Chapter 1 Server
# --------------------------------------------------------------------------

ch1_server <- function(input, output, session) {

  # --- Widget 0: Rozkład empiryczny vs teoretyczny ---
  emp_resample_trigger <- reactiveVal(0)
  observeEvent(input$ch1_emp_resample, emp_resample_trigger(emp_resample_trigger() + 1))

  ch1_emp_data <- reactive({
    emp_resample_trigger()
    req(input$ch1_emp_n, input$ch1_emp_dist)
    n <- input$ch1_emp_n
    dist <- input$ch1_emp_dist
    data <- switch(dist,
      "normal"  = rnorm(n, mean = 170, sd = 8),
      "skewed"  = rgamma(n, shape = 3, scale = 10) + 5,
      "uniform" = runif(n, min = 1, max = 6)
    )
    list(data = data, dist = dist, n = n)
  })

  zoom_plot_server("ch1_emp_vs_theo", reactive({
    d <- ch1_emp_data()

    show_hist <- input$ch1_show_hist
    show_dens <- input$ch1_show_density

    if (!show_hist && !show_dens) {
      return(ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Włącz przynajmniej jedną warstwę",
                 size = 6, color = upwr_reference) +
        theme_void())
    }

    df <- data.frame(x = d$data)

    # Zakres teoretyczny osi X
    theo_xlim <- switch(d$dist,
      "normal"  = c(170 - 4*8, 170 + 4*8),   # mu +/- 4*sigma
      "skewed"  = c(0, 5 + qgamma(0.999, shape = 3, scale = 10)),
      "uniform" = c(0.5, 6.5)
    )
    # Zakres empiryczny
    emp_xlim <- range(d$data)
    # Weź szerszy z dwóch
    x_lo <- min(theo_xlim[1], emp_xlim[1])
    x_hi <- max(theo_xlim[2], emp_xlim[2])
    x_margin <- (x_hi - x_lo) * 0.05
    fixed_xlim <- c(x_lo - x_margin, x_hi + x_margin)

    x_seq <- seq(fixed_xlim[1], fixed_xlim[2], length.out = 500)

    theo_y <- switch(d$dist,
      "normal"  = dnorm(x_seq, mean = 170, sd = 8),
      "skewed"  = dgamma(x_seq - 5, shape = 3, scale = 10),
      "uniform" = dunif(x_seq, min = 1, max = 6)
    )
    theo_y[is.na(theo_y) | theo_y < 0] <- 0
    df_theo <- data.frame(x = x_seq, y = theo_y)

    # Stałe breaks oparte na danych (niezależne od osi)
    n_bins <- min(50, max(10, d$n / 10))
    bin_breaks <- seq(min(d$data), max(d$data), length.out = n_bins + 1)

    # Zakres Y: max z gęstości teoretycznej i histogramu
    theo_ymax <- max(theo_y)
    hist_obj <- hist(d$data, breaks = bin_breaks, plot = FALSE)
    hist_ymax <- max(hist_obj$density)
    fixed_ymax <- max(theo_ymax, hist_ymax) * 1.08

    dist_label <- switch(d$dist,
      "normal"  = "Rozkład normalny N(170, 8)",
      "skewed"  = "Rozkład gamma (skośny)",
      "uniform" = "Rozkład jednostajny U(1, 6)"
    )

    p <- ggplot()

    if (show_hist) {
      p <- p + geom_histogram(data = df, aes(x = x, y = after_stat(density)),
                               breaks = bin_breaks,
                               fill = unname(upwr_cat["niebo"]), color = "white", alpha = 0.6)
    }

    if (show_dens) {
      p <- p + geom_line(data = df_theo, aes(x = x, y = y),
                          color = unname(upwr_cat["terakota"]), linewidth = 1.8) +
        geom_area(data = df_theo, aes(x = x, y = y),
                  fill = unname(upwr_cat["terakota"]), alpha = 0.1)
    }

    p + coord_cartesian(xlim = fixed_xlim, ylim = c(0, fixed_ymax)) +
    labs(
      
      
      x = "Wartość", y = "Gęstość"
    ) +
    theme_upwr()
  }))

  # --- Widget 1: Rzuty kostką ---
  dice_rolls <- reactiveVal(integer(0))

  observeEvent(input$ch1_roll_1, {
    dice_rolls(c(dice_rolls(), sample(1:6, 1)))
  })
  observeEvent(input$ch1_roll_10, {
    dice_rolls(c(dice_rolls(), sample(1:6, 10, replace = TRUE)))
  })
  observeEvent(input$ch1_roll_100, {
    dice_rolls(c(dice_rolls(), sample(1:6, 100, replace = TRUE)))
  })
  observeEvent(input$ch1_roll_1000, {
    dice_rolls(c(dice_rolls(), sample(1:6, 1000, replace = TRUE)))
  })
  observeEvent(input$ch1_roll_reset, {
    dice_rolls(integer(0))
  })

  output$ch1_roll_count <- renderUI({
    n <- length(dice_rolls())
    lc_readout("Rzutów", n, color = unname(upwr_cat["niebo"]))
  })

  output$ch1_has_rolls <- reactive(length(dice_rolls()) > 0)
  outputOptions(output, "ch1_has_rolls", suspendWhenHidden = FALSE)

  zoom_plot_server("ch1_freq_bar", reactive({
    rolls <- dice_rolls()
    if (length(rolls) == 0) {
      NULL
    } else {
      df <- data.frame(face = factor(rolls, levels = 1:6))
      freq_df <- df %>% count(face, .drop = FALSE) %>%
        mutate(rel_freq = n / sum(n))
      ggplot(freq_df, aes(x = face, y = rel_freq)) +
        geom_col(fill = unname(upwr_cat["niebo"]), color = "white", alpha = 0.85) +
        geom_hline(yintercept = 1/6, color = unname(upwr_cat["terakota"]), linewidth = 1, linetype = "dashed") +
        geom_text(aes(label = sprintf("%.3f", rel_freq)), vjust = -0.5, size = 4) +
        scale_y_continuous(limits = c(0, max(0.35, max(freq_df$rel_freq) * 1.15)),
                           expand = expansion(mult = c(0, 0.05))) +
        labs(x = "Ścianka", y = "Częstość względna") +
        annotate("text", x = 6.3, y = 1/6, label = "1/6", color = unname(upwr_cat["terakota"]),
                 fontface = "bold", size = 4, hjust = 0) +
        theme_upwr()
    }
  }))

  zoom_plot_server("ch1_conv_plot", reactive({
    rolls <- dice_rolls()
    if (length(rolls) < 2) return(NULL)

    # Linia zbieżności dla każdej ścianki
    n_total <- length(rolls)
    # Wybierz punkty do wykreślenia (max 200 punktów dla wydajności)
    if (n_total <= 200) {
      indices <- seq_len(n_total)
    } else {
      indices <- unique(c(
        seq(1, min(50, n_total)),
        round(seq(51, n_total, length.out = 150))
      ))
    }

    conv_data <- do.call(rbind, lapply(indices, function(i) {
      tab <- table(factor(rolls[1:i], levels = 1:6)) / i
      data.frame(n = i, face = factor(1:6), rel_freq = as.numeric(tab))
    }))

    ggplot(conv_data, aes(x = n, y = rel_freq, color = face)) +
      geom_line(linewidth = 0.8, alpha = 0.7) +
      geom_hline(yintercept = 1/6, color = "gray40", linewidth = 0.8, linetype = "dashed") +
      scale_color_brewer(palette = "Set2", name = "Ścianka") +
      labs(
           x = "Liczba rzutów", y = "Częstość względna") +
      theme_upwr() +
      theme(legend.position = "right")
  }))

  # --- Widget 2: Częstości vs prawdopodobieństwo ---
  freq_data <- reactive({
    input$ch1_resample
    req(input$ch1_n_obs, input$ch1_scenario)
    n        <- input$ch1_n_obs
    scenario <- input$ch1_scenario
    if (scenario == "fair") {
      list(obs = sample(1:6, n, replace = TRUE), theo = rep(1/6, 6), labels = as.character(1:6))
    } else if (scenario == "loaded") {
      probs <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5)
      list(obs = sample(1:6, n, replace = TRUE, prob = probs), theo = probs, labels = as.character(1:6))
    } else {
      list(obs = sample(c(1, 2), n, replace = TRUE), theo = c(0.5, 0.5), labels = c("Orzeł", "Reszka"))
    }
  })

  zoom_plot_server("ch1_freq_vs_prob", reactive({
    fd <- freq_data()

    n_levels <- length(fd$labels)
    tab <- table(factor(fd$obs, levels = 1:n_levels)) / length(fd$obs)

    df_obs <- data.frame(
      outcome = factor(fd$labels, levels = fd$labels),
      value = as.numeric(tab)
    )
    df_theo <- data.frame(
      outcome = factor(fd$labels, levels = fd$labels),
      value = fd$theo
    )

    ggplot() +
      geom_col(data = df_obs, aes(x = outcome, y = value, fill = "Obserwowane"),
               alpha = 0.85, color = "white") +
      geom_line(data = df_theo, aes(x = outcome, y = value, color = "Teoretyczne", group = 1),
                linewidth = 1.2) +
      geom_point(data = df_theo, aes(x = outcome, y = value, color = "Teoretyczne"),
                 size = 4) +
      scale_fill_manual(values = c("Obserwowane" = unname(upwr_cat["niebo"])), name = "") +
      scale_color_manual(values = c("Teoretyczne" = unname(upwr_cat["terakota"])), name = "") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(
           x = "Wynik", y = "Proporcja / Prawdopodobieństwo") +
      theme_upwr() +
      theme(legend.position = "top")
  }))

  output$ch1_freq_vs_prob_text <- renderUI({
    fd <- freq_data()
    n <- length(fd$obs)
    max_diff <- max(abs(table(factor(fd$obs, levels = 1:length(fd$labels))) / n - fd$theo))

    lc_caption(
      paste0("Maksymalna różnica między częstością a prawdopodobieństwem: ",
             sprintf("%.3f", max_diff),
             if (max_diff < 0.05) " — dobra zgodność" else " — słabsza zgodność")
    )
  })

  # --- Widget 3: Zbuduj własny rozkład ---
  output$ch1_sum_check <- renderUI({
    s <- input$ch1_p1 + input$ch1_p2 + input$ch1_p3 + input$ch1_p4
    if (abs(s - 1) < 0.005) {
      lc_readout("∑", paste0(sprintf("%.2f", s), " ✔"), color = unname(upwr_cat["szalwia"]))
    } else {
      lc_readout("∑", paste0(sprintf("%.2f", s), " ≠ 1 ✘"), color = unname(upwr_cat["terakota"]))
    }
  })

  zoom_plot_server("ch1_custom_dist", reactive({
    probs <- c(input$ch1_p1, input$ch1_p2, input$ch1_p3, input$ch1_p4)
    s <- sum(probs)
    valid <- abs(s - 1) < 0.005

    df <- data.frame(
      outcome = c("A", "B", "C", "D"),
      prob = probs
    )

    ggplot(df, aes(x = outcome, y = prob)) +
      geom_col(fill = if (valid) unname(upwr_cat["szalwia"]) else upwr_reference,
               color = "white", alpha = 0.85, width = 0.6) +
      geom_text(aes(label = sprintf("%.2f", prob)), vjust = -0.5, size = 5) +
      scale_y_continuous(limits = c(0, 1.1), expand = expansion(mult = c(0, 0))) +
      labs(
           x = "Wynik", y = "Prawdopodobieństwo") +
      theme_upwr()
  }))

}
