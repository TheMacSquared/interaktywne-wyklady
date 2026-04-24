# ============================================================================
# CHAPTER 3: Rozklady dyskretne
# ============================================================================

ch3_ui <- list(
  id = "ch-dyskretne", num = "03", title = "Rozkłady dyskretne",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 03 · Rozkłady prawdopodobieństwa",
      num    = "03",
      title  = "Rozkłady dyskretne.",
      lead   = "Wiemy już, czym jest wartość oczekiwana i wariancja.
                Teraz poznamy cztery klasyczne rozkłady dyskretne i zobaczymy,
                jak ich E(X) i Var(X) zależą od parametrów."
    ),

    lc_h2("ch3-intro", "Rozkłady dyskretne"),

    tagList(
      p("Rozkład dyskretny opisuje zmienną, która przyjmuje ",
        "skończoną lub przeliczalną liczbę wartości
        (np. 0, 1, 2, 3...). Każdej wartości przypisujemy prawdopodobieństwo."),
      p("Poznamy trzy najważniejsze:")
    ),

    # ========================================================================
    # WIDGET 1: Rozklad jednostajny dyskretny
    # ========================================================================
    lc_h2("ch3-jednostajny", "Rozkład jednostajny dyskretny"),

    tagList(
      p("Najprostszy rozkład: każdy wynik jest jednakowo prawdopodobny.
        Przykłady: rzut kostką, losowanie cyfry, losowanie karty z talii.")
    ),

    figure_panel(
      label = "Ryc. 3.1",
      title = "Symulacja: moneta i kostka",
      full_width = TRUE,
      fluidRow(
        column(4,
          radioButtons("ch3_unif_type", "Eksperyment:",
            choices = c("Moneta (2 wyniki)" = "coin",
                        "Kostka (6 wyników)" = "die",
                        "Kostka 12-ścienna" = "d12"),
            selected = "die"
          ),
          sliderInput("ch3_unif_n", "Liczba prób:",
                      min = 10, max = 5000, value = 100, step = 10),
          actionButton("ch3_unif_sim", "Symuluj!",
                       class = "lc-btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch3_unif_plot", height = "350px")
        )
      ),
      lc_formula_box(
        withMathJax(helpText(
          "$$P(X = k) = \\frac{1}{n}, \\quad E(X) = \\frac{n+1}{2}, \\quad Var(X) = \\frac{n^2 - 1}{12}$$"
        ))
      )
    ),

    # ========================================================================
    # WIDGET 2: Rozklad dwumianowy — scenariusze overlay
    # ========================================================================
    lc_h2("ch3-dwumianowy", "Rozkład dwumianowy (Binomial)"),

    tagList(
      p("Powtarzamy n niezależnych prób, każda z prawdopodobieństwem
        sukcesu p. Liczymy, ile razy wystąpił sukces."),
      p("Przykłady: ile orłów w 10 rzutach monetą? Ile wadliwych produktów
        w partii? Ile poprawnych odpowiedzi na teście wielokrotnego wyboru?")
    ),

    figure_panel(
      label = "Ryc. 3.2",
      title = "Rozkład dwumianowy B(n, p)",
      full_width = TRUE,
      fluidRow(
        column(4,
          checkboxGroupInput("ch3_binom_scenarios", "Scenariusze:",
            choices = c(
              "Moneta: B(10, 0.5)" = "binom_1",
              "Egzamin: B(20, 0.25)" = "binom_2",
              "Jakość: B(50, 0.1)" = "binom_3",
              "Sukces: B(20, 0.7)" = "binom_4"
            ),
            selected = "binom_1"
          )
        ),
        column(8,
          plotOutput("ch3_binom_plot", height = "400px"),
          uiOutput("ch3_binom_stats")
        )
      ),
      lc_formula_box(
        withMathJax(helpText(
          "$$P(X = k) = \\binom{n}{k} p^k (1-p)^{n-k}, \\quad E(X) = np, \\quad Var(X) = np(1-p)$$"
        ))
      )
    ),

    margin_callout(
      label = "Obserwacja",
      "Porównaj scenariusze i zwróć uwagę, jak zmiana p przesuwa
       rozkład, a wzrost n sprawia, że staje się coraz bardziej 'dzwonowaty'."
    ),

    margin_callout(
      label = "Jak rozpoznać?",
      "Kiedy w zadaniu widzisz pytanie: ile z n... — mysl dwumianowy.
       Np. ile z 20 studentow zda egzamin? Kluczowe: ustalona liczba prob i dwa wyniki.",
      color = "uwaga"
    ),

    # ========================================================================
    # WIDGET 3: Rozklad Poissona — scenariusze overlay
    # ========================================================================
    lc_h2("ch3-poisson", "Rozkład Poissona"),

    tagList(
      p("Zliczamy zdarzenia zachodzące w ustalonym przedziale czasu lub przestrzeni.
        Parametr λ (lambda) mówi, ile średnio zdarzeń oczekujemy w danym przedziale."),
      p("Przykłady: liczba błędów na stronie, klientów w sklepie na godzinę,
        wypadków na skrzyżowaniu w miesiącu.")
    ),

    figure_panel(
      label = "Ryc. 3.3",
      title = "Rozkład Poissona Pois(λ)",
      full_width = TRUE,
      fluidRow(
        column(4,
          checkboxGroupInput("ch3_pois_scenarios", "Scenariusze:",
            choices = c(
              "Wypadki: λ = 0.5" = "pois_1",
              "Błędy: λ = 2" = "pois_2",
              "Klienci: λ = 5" = "pois_3",
              "Wiadomości: λ = 10" = "pois_4"
            ),
            selected = "pois_2"
          )
        ),
        column(8,
          plotOutput("ch3_pois_plot", height = "400px"),
          uiOutput("ch3_pois_stats")
        )
      ),
      lc_formula_box(
        withMathJax(helpText(
          "$$P(X = k) = \\frac{\\lambda^k e^{-\\lambda}}{k!}, \\quad E(X) = \\lambda, \\quad Var(X) = \\lambda$$"
        ))
      )
    ),

    margin_callout(
      label = "Ciekawostka",
      "W rozkładzie Poissona wartość oczekiwana = wariancja = λ.
       Jeśli w danych średnia ≈ wariancja, to dobry kandydat na model Poissona!"
    ),

    margin_callout(
      label = "Jak rozpoznać?",
      "Kiedy w zadaniu widzisz pytanie: ile razy w ciagu... — mysl Poisson.
       Kluczowe: zliczasz zdarzenia w ustalonym czasie lub przestrzeni, bez gornego limitu.",
      color = "uwaga"
    ),

    # ========================================================================
    # WIDGET 4: Rozklad geometryczny — scenariusze overlay
    # ========================================================================
    lc_h2("ch3-geometryczny", "Rozkład geometryczny"),

    tagList(
      p("Powtarzamy próby aż do pierwszego sukcesu. Pytamy: ile prób to zajmie?"),
      p("Przykłady: ile rzutów kostką do pierwszej szóstki?
        Ile losowań do trafienia nagrody? Ile prób egzaminu do zdania?")
    ),

    figure_panel(
      label = "Ryc. 3.4",
      title = "Rozkład geometryczny Geom(p)",
      full_width = TRUE,
      fluidRow(
        column(4,
          checkboxGroupInput("ch3_geom_scenarios", "Scenariusze:",
            choices = c(
              "Rzadkie: p = 0.05" = "geom_1",
              "Szóstka: p = 1/6" = "geom_2",
              "Częste: p = 0.3" = "geom_3",
              "Moneta: p = 0.5" = "geom_4"
            ),
            selected = "geom_2"
          )
        ),
        column(8,
          plotOutput("ch3_geom_plot", height = "400px"),
          uiOutput("ch3_geom_stats")
        )
      ),
      lc_formula_box(
        withMathJax(helpText(
          "$$P(X = k) = (1-p)^{k-1} \\cdot p, \\quad E(X) = \\frac{1}{p}, \\quad Var(X) = \\frac{1-p}{p^2}$$"
        ))
      )
    ),

    margin_callout(
      label = "Bezpamięciowość",
      "Im mniejsze p, tym dłużej (przeciętnie) czekamy na sukces.
       Rozkład geometryczny jest bezpamięciowy — szansa sukcesu w każdej próbie jest taka sama,
       niezależnie od liczby dotychczasowych porażek."
    ),

    margin_callout(
      label = "Jak rozpoznać?",
      "Kiedy w zadaniu widzisz pytanie: ile prob az do... — mysl geometryczny.
       Kluczowe: powtarzasz proby az do pierwszego sukcesu.",
      color = "uwaga"
    ),

    # ========================================================================
    # WIDGET 5: Porownanie czterech rozkladow
    # ========================================================================
    lc_h2("ch3-porownanie", "Porównanie czterech rozkładów"),

    tagList(
      p("Zobaczmy wszystkie cztery rozkłady obok siebie. Zwróć uwagę na
        różnice w kształtach i na to, kiedy rozkład dyskretny zaczyna
        wyglądać jak gładki 'dzwon'.")
    ),

    figure_panel(
      label = "Ryc. 3.5",
      title = "Cztery rozkłady obok siebie",
      full_width = TRUE,
      checkboxInput("ch3_compare_show_ev", "Pokaż wartość oczekiwaną (linia)", value = FALSE),
      checkboxInput("ch3_compare_show_sd", "Pokaż ± odchylenie standardowe (pas)", value = FALSE),
      plotOutput("ch3_compare_plot", height = "350px")
    ),

    lc_chapter_next(
      num       = "04",
      title     = "Rozkłady ciągłe",
      lead      = "gdy zmienna może przyjąć dowolną wartość z pewnego przedziału.",
      target_id = "ch-ciagle"
    )
  )
)

# --------------------------------------------------------------------------
# Chapter 3 Server
# --------------------------------------------------------------------------

# Definicje scenariuszy
ch3_binom_defs <- list(
  binom_1 = list(label = "Moneta: B(10, 0.5)", n = 10, p = 0.5),
  binom_2 = list(label = "Egzamin: B(20, 0.25)", n = 20, p = 0.25),
  binom_3 = list(label = "Jakość: B(50, 0.1)", n = 50, p = 0.1),
  binom_4 = list(label = "Sukces: B(20, 0.7)", n = 20, p = 0.7)
)

ch3_pois_defs <- list(
  pois_1 = list(label = "Wypadki: λ = 0.5", lambda = 0.5),
  pois_2 = list(label = "Błędy: λ = 2", lambda = 2),
  pois_3 = list(label = "Klienci: λ = 5", lambda = 5),
  pois_4 = list(label = "Wiadomości: λ = 10", lambda = 10)
)

ch3_geom_defs <- list(
  geom_1 = list(label = "Rzadkie: p = 0.05", p = 0.05),
  geom_2 = list(label = "Szóstka: p = 1/6", p = round(1/6, 4)),
  geom_3 = list(label = "Częste: p = 0.3", p = 0.3),
  geom_4 = list(label = "Moneta: p = 0.5", p = 0.5)
)

ch3_server <- function(input, output, session) {

  # --- Widget 1: Jednostajny dyskretny (bez zmian) ---
  ch3_unif_data <- reactive({
    input$ch3_unif_sim
    req(input$ch3_unif_type, input$ch3_unif_n)
    type <- input$ch3_unif_type
    k    <- switch(type, "coin" = 2, "die" = 6, "d12" = 12)
    list(obs = sample(1:k, input$ch3_unif_n, replace = TRUE), k = k, n = input$ch3_unif_n)
  })

  output$ch3_unif_plot <- renderPlot({
    d <- ch3_unif_data()

    df <- data.frame(x = factor(d$obs, levels = 1:d$k))
    freq_df <- df %>% count(x, .drop = FALSE) %>% mutate(rel = n / sum(n))

    ggplot(freq_df, aes(x = x, y = rel)) +
      geom_col(fill = col_uniform, color = "white", alpha = 0.7) +
      geom_hline(yintercept = 1/d$k, color = unname(upwr_cat["terakota"]), linewidth = 1, linetype = "dashed") +
      geom_point(aes(y = 1/d$k), color = unname(upwr_cat["terakota"]), size = 3) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(title = paste0("Rozkład jednostajny: ", d$n, " prób, ", d$k, " wyników"),
           subtitle = paste0("Linia: P(X=k) = 1/", d$k, " = ", round(1/d$k, 4)),
           x = "Wynik", y = "Częstość względna") +
      theme_upwr()
  })

  # --- Widget 2: Dwumianowy — scenariusze overlay ---
  output$ch3_binom_plot <- renderPlot({
    selected <- input$ch3_binom_scenarios
    req(length(selected) > 0)

    dfs <- lapply(seq_along(selected), function(i) {
      s <- ch3_binom_defs[[selected[i]]]
      x_vals <- 0:s$n
      probs <- dbinom(x_vals, s$n, s$p)
      data.frame(x = x_vals, prob = probs, scenario = s$label)
    })
    df <- do.call(rbind, dfs)
    df$scenario <- factor(df$scenario, levels = sapply(ch3_binom_defs[selected], `[[`, "label"))

    n_sel <- length(selected)
    colors <- setNames(upwr_cat_n(n_sel),
                       sapply(ch3_binom_defs[selected], `[[`, "label"))

    dodge <- if (n_sel > 1) position_dodge(width = 0.5) else "identity"

    ggplot(df, aes(x = x, y = prob, color = scenario)) +
      geom_point(size = 4, alpha = 0.85, position = dodge) +
      geom_segment(aes(xend = x, yend = 0), linewidth = 1, alpha = 0.6, position = dodge) +
      scale_color_manual(values = colors, name = NULL) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(title = "Rozkład dwumianowy B(n, p)",
           x = "Liczba sukcesów (k)", y = "P(X = k)") +
      theme_upwr() +
      theme(legend.position = "top", legend.text = element_text(size = 11))
  })

  output$ch3_binom_stats <- renderUI({
    selected <- input$ch3_binom_scenarios
    req(length(selected) > 0)

    stats <- lapply(selected, function(id) {
      s <- ch3_binom_defs[[id]]
      mu <- s$n * s$p
      sigma <- sqrt(s$n * s$p * (1 - s$p))
      paste0(s$label, ":  E(X) = ", round(mu, 1), ",  SD = ", round(sigma, 2))
    })
    div(style = "font-size: 13px; margin-top: 10px; line-height: 1.8;",
      lapply(stats, function(s) div(s))
    )
  })

  # --- Widget 3: Poissona — scenariusze overlay ---
  output$ch3_pois_plot <- renderPlot({
    selected <- input$ch3_pois_scenarios
    req(length(selected) > 0)

    # Wspolny zakres x dla wszystkich scenariuszy
    x_max <- max(sapply(selected, function(id) qpois(0.999, ch3_pois_defs[[id]]$lambda)))

    dfs <- lapply(seq_along(selected), function(i) {
      s <- ch3_pois_defs[[selected[i]]]
      x_vals <- 0:x_max
      probs <- dpois(x_vals, s$lambda)
      data.frame(x = x_vals, prob = probs, scenario = s$label)
    })
    df <- do.call(rbind, dfs)
    df$scenario <- factor(df$scenario, levels = sapply(ch3_pois_defs[selected], `[[`, "label"))

    n_sel <- length(selected)
    colors <- setNames(upwr_cat_n(n_sel),
                       sapply(ch3_pois_defs[selected], `[[`, "label"))
    dodge <- if (n_sel > 1) position_dodge(width = 0.5) else "identity"

    ggplot(df, aes(x = x, y = prob, color = scenario)) +
      geom_point(size = 4, alpha = 0.85, position = dodge) +
      geom_segment(aes(xend = x, yend = 0), linewidth = 1, alpha = 0.6, position = dodge) +
      scale_color_manual(values = colors, name = NULL) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(title = "Rozkład Poissona Pois(λ)",
           x = "Liczba zdarzeń (k)", y = "P(X = k)") +
      theme_upwr() +
      theme(legend.position = "top", legend.text = element_text(size = 11))
  })

  output$ch3_pois_stats <- renderUI({
    selected <- input$ch3_pois_scenarios
    req(length(selected) > 0)

    stats <- lapply(selected, function(id) {
      s <- ch3_pois_defs[[id]]
      paste0(s$label, ":  E(X) = Var(X) = ", s$lambda,
             ",  SD = ", round(sqrt(s$lambda), 2))
    })
    div(style = "font-size: 13px; margin-top: 10px; line-height: 1.8;",
      lapply(stats, function(s) div(s))
    )
  })

  # --- Widget 4: Geometryczny — scenariusze overlay ---
  output$ch3_geom_plot <- renderPlot({
    selected <- input$ch3_geom_scenarios
    req(length(selected) > 0)

    # Wspolny zakres x, ograniczony do 40
    x_max <- min(40, max(sapply(selected, function(id) {
      qgeom(0.999, ch3_geom_defs[[id]]$p) + 1
    })))

    dfs <- lapply(seq_along(selected), function(i) {
      s <- ch3_geom_defs[[selected[i]]]
      x_vals <- 1:x_max
      probs <- dgeom(x_vals - 1, s$p)
      data.frame(x = x_vals, prob = probs, scenario = s$label)
    })
    df <- do.call(rbind, dfs)
    df$scenario <- factor(df$scenario, levels = sapply(ch3_geom_defs[selected], `[[`, "label"))

    n_sel <- length(selected)
    colors <- setNames(upwr_cat_n(n_sel),
                       sapply(ch3_geom_defs[selected], `[[`, "label"))
    dodge <- if (n_sel > 1) position_dodge(width = 0.5) else "identity"

    ggplot(df, aes(x = x, y = prob, color = scenario)) +
      geom_point(size = 4, alpha = 0.85, position = dodge) +
      geom_segment(aes(xend = x, yend = 0), linewidth = 1, alpha = 0.6, position = dodge) +
      scale_color_manual(values = colors, name = NULL) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(title = "Rozkład geometryczny Geom(p)",
           x = "Numer próby (k)", y = "P(X = k)") +
      theme_upwr() +
      theme(legend.position = "top", legend.text = element_text(size = 11))
  })

  output$ch3_geom_stats <- renderUI({
    selected <- input$ch3_geom_scenarios
    req(length(selected) > 0)

    stats <- lapply(selected, function(id) {
      s <- ch3_geom_defs[[id]]
      mu <- 1 / s$p
      sigma <- sqrt((1 - s$p) / s$p^2)
      paste0(s$label, ":  E(X) = ", round(mu, 1), ",  SD = ", round(sigma, 2))
    })
    div(style = "font-size: 13px; margin-top: 10px; line-height: 1.8;",
      lapply(stats, function(s) div(s))
    )
  })

  # --- Widget 5: Porownanie (bez zmian) ---
  output$ch3_compare_plot <- renderPlot({
    show_ev <- input$ch3_compare_show_ev
    show_sd <- input$ch3_compare_show_sd

    # Jednostajny: kostka
    x1 <- 1:6; p1 <- rep(1/6, 6)
    mu1 <- 3.5; sd1 <- sqrt(35/12)
    df1 <- data.frame(x = x1, prob = p1, dist = "Jednostajny\n(kostka)")

    # Dwumianowy: B(20, 0.3)
    x2 <- 0:20; p2 <- dbinom(x2, 20, 0.3)
    mu2 <- 6; sd2 <- sqrt(20*0.3*0.7)
    keep2 <- p2 > 0.001
    df2 <- data.frame(x = x2[keep2], prob = p2[keep2], dist = "Dwumianowy\nB(20, 0.3)")

    # Poissona: Pois(4)
    x3 <- 0:15; p3 <- dpois(x3, 4)
    mu3 <- 4; sd3 <- 2
    keep3 <- p3 > 0.001
    df3 <- data.frame(x = x3[keep3], prob = p3[keep3], dist = "Poissona\nPois(4)")

    # Geometryczny: Geom(0.2)
    x4 <- 1:25; p4 <- dgeom(x4 - 1, 0.2)
    mu4 <- 1/0.2; sd4 <- sqrt((1 - 0.2) / 0.2^2)
    keep4 <- p4 > 0.001
    df4 <- data.frame(x = x4[keep4], prob = p4[keep4], dist = "Geometryczny\nGeom(0.2)")

    df_all <- rbind(df1, df2, df3, df4)
    df_all$dist <- factor(df_all$dist,
                          levels = c("Jednostajny\n(kostka)", "Dwumianowy\nB(20, 0.3)",
                                     "Poissona\nPois(4)", "Geometryczny\nGeom(0.2)"))

    stats_df <- data.frame(
      dist = levels(df_all$dist),
      mu = c(mu1, mu2, mu3, mu4),
      sd = c(sd1, sd2, sd3, sd4)
    )

    pl <- ggplot(df_all, aes(x = x, y = prob)) +
      geom_col(aes(fill = dist), color = "white", alpha = 0.85, width = 0.7, show.legend = FALSE) +
      facet_wrap(~dist, scales = "free_x", nrow = 2) +
      scale_fill_manual(values = c(col_uniform, col_binomial, col_poisson, col_geometric)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = "Wartość", y = "Prawdopodobieństwo") +
      theme_upwr(base_size = 13)

    if (show_ev) {
      pl <- pl + geom_vline(data = stats_df, aes(xintercept = mu),
                            color = unname(upwr_cat["terakota"]), linewidth = 1, linetype = "dashed")
    }
    if (show_sd) {
      pl <- pl + geom_rect(data = stats_df,
                           aes(xmin = mu - sd, xmax = mu + sd, ymin = 0, ymax = Inf),
                           inherit.aes = FALSE, fill = unname(upwr_cat["terakota"]), alpha = 0.08)
    }
    pl
  })

}
