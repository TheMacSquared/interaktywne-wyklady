# ============================================================================
# CHAPTER 3: Rozklady dyskretne
# ============================================================================

ch3_ui <- tabPanel("3. Rozk\u0142ady dyskretne",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Wiemy ju\u017c, czym jest warto\u015b\u0107 oczekiwana i wariancja.
       Teraz poznamy trzy klasyczne rozk\u0142ady dyskretne i zobaczymy,
       jak ich E(X) i Var(X) zale\u017c\u0105 od parametr\u00f3w."
    ),

    div(class = "section-title", "Rozk\u0142ady dyskretne"),

    div(class = "narrative",
      p("Rozk\u0142ad dyskretny opisuje zmienn\u0105, kt\u00f3ra przyjmuje ",
        tags$b("sko\u0144czon\u0105 lub przeliczaln\u0105"), " liczb\u0119 warto\u015bci
        (np. 0, 1, 2, 3...). Ka\u017cdej warto\u015bci przypisujemy prawdopodobie\u0144stwo."),
      p("Poznamy trzy najwa\u017cniejsze:")
    ),

    # ========================================================================
    # WIDGET 1: Rozklad jednostajny dyskretny
    # ========================================================================
    div(class = "section-title", "Rozk\u0142ad jednostajny dyskretny"),

    div(class = "narrative",
      p("Najprostszy rozk\u0142ad: ka\u017cdy wynik jest ",
        tags$b("jednakowo prawdopodobny"), ". Przyk\u0142ady: rzut kostk\u0105,
        losowanie cyfry, losowanie karty z talii.")
    ),

    div(class = "widget-block",
      h4("Symulacja: moneta i kostka"),
      fluidRow(
        column(4,
          radioButtons("ch3_unif_type", "Eksperyment:",
            choices = c("Moneta (2 wyniki)" = "coin",
                        "Kostka (6 wynik\u00f3w)" = "die",
                        "Kostka 12-\u015bcienna" = "d12"),
            selected = "die"
          ),
          sliderInput("ch3_unif_n", "Liczba pr\u00f3b:",
                      min = 10, max = 5000, value = 100, step = 10),
          actionButton("ch3_unif_sim", "Symuluj!",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch3_unif_plot", height = "350px")
        )
      ),
      div(class = "formula-box",
        withMathJax(helpText(
          "$$P(X = k) = \\frac{1}{n}, \\quad E(X) = \\frac{n+1}{2}, \\quad Var(X) = \\frac{n^2 - 1}{12}$$"
        ))
      )
    ),

    # ========================================================================
    # WIDGET 2: Rozklad dwumianowy — scenariusze overlay
    # ========================================================================
    div(class = "section-title", "Rozk\u0142ad dwumianowy (Binomial)"),

    div(class = "narrative",
      p("Powtarzamy ", tags$b("n niezale\u017cnych pr\u00f3b"), ", ka\u017cda z prawdopodobie\u0144stwem
        sukcesu ", tags$b("p"), ". Liczymy ",
        tags$b("ile razy wyst\u0105pi\u0142 sukces"), "."),
      p("Przyk\u0142ady: ile or\u0142\u00f3w w 10 rzutach monet\u0105? Ile wadliwych produkt\u00f3w
        w partii? Ile poprawnych odpowiedzi na te\u015bcie wielokrotnego wyboru?")
    ),

    div(class = "widget-block",
      h4("Rozk\u0142ad dwumianowy B(n, p)"),
      fluidRow(
        column(4,
          checkboxGroupInput("ch3_binom_scenarios", "Scenariusze:",
            choices = c(
              "Moneta: B(10, 0.5)" = "binom_1",
              "Egzamin: B(20, 0.25)" = "binom_2",
              "Jako\u015b\u0107: B(50, 0.1)" = "binom_3",
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
      div(class = "formula-box",
        withMathJax(helpText(
          "$$P(X = k) = \\binom{n}{k} p^k (1-p)^{n-k}, \\quad E(X) = np, \\quad Var(X) = np(1-p)$$"
        ))
      )
    ),

    div(class = "callout-info",
      tags$strong("Obserwacja:"),
      " Por\u00f3wnaj scenariusze i zwr\u00f3\u0107 uwag\u0119, jak zmiana p przesuwa
        rozk\u0142ad, a wzrost n sprawia, \u017ce staje si\u0119 coraz bardziej 'dzwonowaty'."
    ),

    div(class = "callout-warning",
      tags$strong("Jak rozpozna\u0107?"),
      " Kiedy w zadaniu widzisz pytanie typu ",
      tags$b("\u201eile z n...\u201d"), " \u2014 my\u015bl dwumianowy.
        Np. \u201eile z 20 student\u00f3w zda egzamin?\u201d, \u201eile z 50 produkt\u00f3w b\u0119dzie wadliwych?\u201d.
        Kluczowe: masz ", tags$b("ustalon\u0105 liczb\u0119 pr\u00f3b"), " i dwa wyniki (sukces/pora\u017cka)."
    ),

    # ========================================================================
    # WIDGET 3: Rozklad Poissona — scenariusze overlay
    # ========================================================================
    div(class = "section-title", "Rozk\u0142ad Poissona"),

    div(class = "narrative",
      p("Zliczamy ", tags$b("zdarzenia"), " zachodz\u0105ce w ustalonym przedziale czasu lub przestrzeni.
        Parametr \u03bb (lambda) m\u00f3wi, ile \u015brednio zdarze\u0144 oczekujemy w danym przedziale."),
      p("Przyk\u0142ady: liczba b\u0142\u0119d\u00f3w na stronie, klient\u00f3w w sklepie na godzin\u0119,
        wypadk\u00f3w na skrzy\u017cowaniu w miesi\u0105cu.")
    ),

    div(class = "widget-block",
      h4("Rozk\u0142ad Poissona Pois(\u03bb)"),
      fluidRow(
        column(4,
          checkboxGroupInput("ch3_pois_scenarios", "Scenariusze:",
            choices = c(
              "Wypadki: \u03bb = 0.5" = "pois_1",
              "B\u0142\u0119dy: \u03bb = 2" = "pois_2",
              "Klienci: \u03bb = 5" = "pois_3",
              "Wiadomo\u015bci: \u03bb = 10" = "pois_4"
            ),
            selected = "pois_2"
          )
        ),
        column(8,
          plotOutput("ch3_pois_plot", height = "400px"),
          uiOutput("ch3_pois_stats")
        )
      ),
      div(class = "formula-box",
        withMathJax(helpText(
          "$$P(X = k) = \\frac{\\lambda^k e^{-\\lambda}}{k!}, \\quad E(X) = \\lambda, \\quad Var(X) = \\lambda$$"
        ))
      )
    ),

    div(class = "callout-info",
      tags$strong("Ciekawostka:"),
      " W rozk\u0142adzie Poissona warto\u015b\u0107 oczekiwana = wariancja = \u03bb.
        Je\u015bli w danych \u015brednia \u2248 wariancja, to dobry kandydat na model Poissona!"
    ),

    div(class = "callout-warning",
      tags$strong("Jak rozpozna\u0107?"),
      " Kiedy w zadaniu widzisz pytanie typu ",
      tags$b("\u201eile razy w ci\u0105gu...\u201d"), " \u2014 my\u015bl Poisson.
        Np. \u201eile wiadomo\u015bci dostaniesz w ci\u0105gu godziny?\u201d, \u201eile b\u0142\u0119d\u00f3w na stronie?\u201d.
        Kluczowe: zliczasz zdarzenia w ", tags$b("ustalonym czasie lub przestrzeni"),
      ", bez g\u00f3rnego limitu."
    ),

    # ========================================================================
    # WIDGET 4: Rozklad geometryczny — scenariusze overlay
    # ========================================================================
    div(class = "section-title", "Rozk\u0142ad geometryczny"),

    div(class = "narrative",
      p("Powtarzamy pr\u00f3by a\u017c do ", tags$b("pierwszego sukcesu"),
        ". Pytamy: ile pr\u00f3b to zajmie?"),
      p("Przyk\u0142ady: ile rzut\u00f3w kostk\u0105 do pierwszej sz\u00f3stki?
        Ile losowa\u0144 do trafienia nagrody? Ile pr\u00f3b egzaminu do zdania?")
    ),

    div(class = "widget-block",
      h4("Rozk\u0142ad geometryczny Geom(p)"),
      fluidRow(
        column(4,
          checkboxGroupInput("ch3_geom_scenarios", "Scenariusze:",
            choices = c(
              "Rzadkie: p = 0.05" = "geom_1",
              "Sz\u00f3stka: p = 1/6" = "geom_2",
              "Cz\u0119ste: p = 0.3" = "geom_3",
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
      div(class = "formula-box",
        withMathJax(helpText(
          "$$P(X = k) = (1-p)^{k-1} \\cdot p, \\quad E(X) = \\frac{1}{p}, \\quad Var(X) = \\frac{1-p}{p^2}$$"
        ))
      )
    ),

    div(class = "callout-info",
      tags$strong("Uwaga:"),
      " Im mniejsze p, tym d\u0142u\u017cej (przeci\u0119tnie) czekamy na sukces.
        Rozk\u0142ad geometryczny jest ", tags$b("bezpami\u0119ciowy"),
      " \u2014 szansa sukcesu w ka\u017cdej pr\u00f3bie jest taka sama,
        niezale\u017cnie od tego ile pora\u017cek ju\u017c by\u0142o."
    ),

    div(class = "callout-warning",
      tags$strong("Jak rozpozna\u0107?"),
      " Kiedy w zadaniu widzisz pytanie typu ",
      tags$b("\u201eile pr\u00f3b a\u017c do...\u201d"), " \u2014 my\u015bl geometryczny.
        Np. \u201eile rzut\u00f3w kostk\u0105 do pierwszej sz\u00f3stki?\u201d,
        \u201eile CV trzeba wys\u0142a\u0107, \u017ceby dosta\u0107 zaproszenie na rozmow\u0119?\u201d.
        Kluczowe: powtarzasz pr\u00f3by a\u017c do ", tags$b("pierwszego sukcesu"), "."
    ),

    # ========================================================================
    # WIDGET 5: Porownanie czterech rozkladow
    # ========================================================================
    div(class = "section-title", "Por\u00f3wnanie czterech rozk\u0142ad\u00f3w"),

    div(class = "narrative",
      p("Zobaczmy wszystkie cztery rozk\u0142ady obok siebie. Zwr\u00f3\u0107 uwag\u0119 na
        r\u00f3\u017cnice w kszta\u0142tach i na to, kiedy rozk\u0142ad dyskretny zaczyna
        wygl\u0105da\u0107 jak g\u0142adki 'dzwon'.")
    ),

    div(class = "widget-block",
      h4("Cztery rozk\u0142ady obok siebie"),
      checkboxInput("ch3_compare_show_ev", "Poka\u017c warto\u015b\u0107 oczekiwan\u0105 (linia)", value = FALSE),
      checkboxInput("ch3_compare_show_sd", "Poka\u017c \u00b1 odchylenie standardowe (pas)", value = FALSE),
      plotOutput("ch3_compare_plot", height = "350px")
    ),

    # --- Transition ---
    div(class = "chapter-transition",
      p("Rozk\u0142ady dyskretne opisuj\u0105 wyniki, kt\u00f3re mo\u017cna policzy\u0107.
        Ale co, gdy zmienna mo\u017ce przyj\u0105\u0107 ", tags$b("dowoln\u0105 warto\u015b\u0107"),
        " z pewnego przedzia\u0142u? Wtedy potrzebujemy rozk\u0142ad\u00f3w ci\u0105g\u0142ych."),
      actionButton("ch3_next", "Dalej: 4. Rozk\u0142ady ci\u0105g\u0142e \u2192",
                   class = "btn-primary btn-lg")
    ),

    br(), br()
  ))
)

# --------------------------------------------------------------------------
# Chapter 3 Server
# --------------------------------------------------------------------------

# Definicje scenariuszy
ch3_binom_defs <- list(
  binom_1 = list(label = "Moneta: B(10, 0.5)", n = 10, p = 0.5),
  binom_2 = list(label = "Egzamin: B(20, 0.25)", n = 20, p = 0.25),
  binom_3 = list(label = "Jako\u015b\u0107: B(50, 0.1)", n = 50, p = 0.1),
  binom_4 = list(label = "Sukces: B(20, 0.7)", n = 20, p = 0.7)
)

ch3_pois_defs <- list(
  pois_1 = list(label = "Wypadki: \u03bb = 0.5", lambda = 0.5),
  pois_2 = list(label = "B\u0142\u0119dy: \u03bb = 2", lambda = 2),
  pois_3 = list(label = "Klienci: \u03bb = 5", lambda = 5),
  pois_4 = list(label = "Wiadomo\u015bci: \u03bb = 10", lambda = 10)
)

ch3_geom_defs <- list(
  geom_1 = list(label = "Rzadkie: p = 0.05", p = 0.05),
  geom_2 = list(label = "Sz\u00f3stka: p = 1/6", p = round(1/6, 4)),
  geom_3 = list(label = "Cz\u0119ste: p = 0.3", p = 0.3),
  geom_4 = list(label = "Moneta: p = 0.5", p = 0.5)
)

ch3_server <- function(input, output, session) {

  # --- Widget 1: Jednostajny dyskretny (bez zmian) ---
  ch3_unif_data <- reactiveVal(NULL)

  observeEvent(input$ch3_unif_sim, {
    n <- input$ch3_unif_n
    type <- input$ch3_unif_type
    k <- switch(type, "coin" = 2, "die" = 6, "d12" = 12)
    obs <- sample(1:k, n, replace = TRUE)
    ch3_unif_data(list(obs = obs, k = k, n = n))
  }, ignoreNULL = FALSE)

  observe({
    input$ch3_unif_type
    type <- input$ch3_unif_type
    k <- switch(type, "coin" = 2, "die" = 6, "d12" = 12)
    ch3_unif_data(list(obs = sample(1:k, input$ch3_unif_n, replace = TRUE),
                       k = k, n = input$ch3_unif_n))
  })

  output$ch3_unif_plot <- renderPlot({
    d <- ch3_unif_data()
    req(d)

    df <- data.frame(x = factor(d$obs, levels = 1:d$k))
    freq_df <- df %>% count(x, .drop = FALSE) %>% mutate(rel = n / sum(n))

    ggplot(freq_df, aes(x = x, y = rel)) +
      geom_col(fill = col_uniform, color = "white", alpha = 0.7) +
      geom_hline(yintercept = 1/d$k, color = col_secondary, linewidth = 1, linetype = "dashed") +
      geom_point(aes(y = 1/d$k), color = col_secondary, size = 3) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(title = paste0("Rozk\u0142ad jednostajny: ", d$n, " pr\u00f3b, ", d$k, " wynik\u00f3w"),
           subtitle = paste0("Linia: P(X=k) = 1/", d$k, " = ", round(1/d$k, 4)),
           x = "Wynik", y = "Cz\u0119sto\u015b\u0107 wzgl\u0119dna") +
      theme_educational()
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
    colors <- setNames(col_scenario[seq_len(n_sel)],
                       sapply(ch3_binom_defs[selected], `[[`, "label"))

    dodge <- if (n_sel > 1) position_dodge(width = 0.5) else "identity"

    ggplot(df, aes(x = x, y = prob, color = scenario)) +
      geom_point(size = 4, alpha = 0.85, position = dodge) +
      geom_segment(aes(xend = x, yend = 0), linewidth = 1, alpha = 0.6, position = dodge) +
      scale_color_manual(values = colors, name = NULL) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(title = "Rozk\u0142ad dwumianowy B(n, p)",
           x = "Liczba sukces\u00f3w (k)", y = "P(X = k)") +
      theme_educational() +
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
    colors <- setNames(col_scenario[seq_len(n_sel)],
                       sapply(ch3_pois_defs[selected], `[[`, "label"))
    dodge <- if (n_sel > 1) position_dodge(width = 0.5) else "identity"

    ggplot(df, aes(x = x, y = prob, color = scenario)) +
      geom_point(size = 4, alpha = 0.85, position = dodge) +
      geom_segment(aes(xend = x, yend = 0), linewidth = 1, alpha = 0.6, position = dodge) +
      scale_color_manual(values = colors, name = NULL) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(title = "Rozk\u0142ad Poissona Pois(\u03bb)",
           x = "Liczba zdarze\u0144 (k)", y = "P(X = k)") +
      theme_educational() +
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
    colors <- setNames(col_scenario[seq_len(n_sel)],
                       sapply(ch3_geom_defs[selected], `[[`, "label"))
    dodge <- if (n_sel > 1) position_dodge(width = 0.5) else "identity"

    ggplot(df, aes(x = x, y = prob, color = scenario)) +
      geom_point(size = 4, alpha = 0.85, position = dodge) +
      geom_segment(aes(xend = x, yend = 0), linewidth = 1, alpha = 0.6, position = dodge) +
      scale_color_manual(values = colors, name = NULL) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(title = "Rozk\u0142ad geometryczny Geom(p)",
           x = "Numer pr\u00f3by (k)", y = "P(X = k)") +
      theme_educational() +
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
      labs(x = "Warto\u015b\u0107", y = "Prawdopodobie\u0144stwo") +
      theme_educational(base_size = 13)

    if (show_ev) {
      pl <- pl + geom_vline(data = stats_df, aes(xintercept = mu),
                            color = col_secondary, linewidth = 1, linetype = "dashed")
    }
    if (show_sd) {
      pl <- pl + geom_rect(data = stats_df,
                           aes(xmin = mu - sd, xmax = mu + sd, ymin = 0, ymax = Inf),
                           inherit.aes = FALSE, fill = col_secondary, alpha = 0.08)
    }
    pl
  })

}
