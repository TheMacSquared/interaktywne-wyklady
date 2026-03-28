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
    # WIDGET 2: Rozklad dwumianowy
    # ========================================================================
    div(class = "section-title", "Rozk\u0142ad dwumianowy (Binomial)"),

    div(class = "narrative",
      p("Powtarzamy ", tags$b("n niezale\u017cnych pr\u00f3b"), ", ka\u017cda z prawdopodobie\u0144stwem
        sukcesu ", tags$b("p"), ". Liczymy ",
        tags$b("ile razy wyst\u0105pi\u0142 sukces"), "."),
      p("Przyk\u0142ady: ile or\u0142\u00f3w w 10 rzutach monet\u0105? Ile wadliwych produkt\u00f3w
        w partii 100? Ile poprawnych odpowiedzi na te\u015bcie wielokrotnego wyboru?")
    ),

    div(class = "widget-block",
      h4("Rozk\u0142ad dwumianowy B(n, p)"),
      fluidRow(
        column(4,
          sliderInput("ch3_binom_n", "n (liczba pr\u00f3b):",
                      min = 1, max = 100, value = 20, step = 1),
          sliderInput("ch3_binom_p", "p (prawdop. sukcesu):",
                      min = 0, max = 1, value = 0.5, step = 0.01),
          hr(),
          div(class = "preset-buttons",
            actionButton("ch3_binom_preset1", "Moneta\n(n=10, p=0.5)",
                         class = "btn-outline-primary"),
            actionButton("ch3_binom_preset2", "Jako\u015b\u0107\n(n=100, p=0.02)",
                         class = "btn-outline-warning"),
            actionButton("ch3_binom_preset3", "Egzamin\n(n=20, p=0.25)",
                         class = "btn-outline-success")
          ),
          hr(),
          checkboxInput("ch3_binom_show_stats", "Poka\u017c E(X) i SD", value = TRUE)
        ),
        column(8,
          plotOutput("ch3_binom_plot", height = "350px"),
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
      " Przesuwaj suwak p od 0 do 1 i obserwuj, jak kszta\u0142t rozk\u0142adu
        zmienia si\u0119 z prawosko\u015bnego (p\u2248 0) przez symetryczny (p=0.5)
        do lewosko\u015bnego (p\u2248 1). Przy du\u017cym n rozk\u0142ad staje si\u0119
        coraz bardziej 'dzwonowaty'."
    ),

    # ========================================================================
    # WIDGET 3: Rozklad Poissona
    # ========================================================================
    div(class = "section-title", "Rozk\u0142ad Poissona"),

    div(class = "narrative",
      p("Zliczamy ", tags$b("rzadkie zdarzenia"), " w ustalonym czasie lub przestrzeni.
        Parametr \u03bb (lambda) to \u015brednia liczba zdarze\u0144."),
      p("Przyk\u0142ady: liczba b\u0142\u0119d\u00f3w na stronie, klient\u00f3w w sklepie na godzin\u0119,
        wypadk\u00f3w na skrzy\u017cowaniu w miesi\u0105cu.")
    ),

    div(class = "widget-block",
      h4("Rozk\u0142ad Poissona Pois(\u03bb)"),
      fluidRow(
        column(4,
          sliderInput("ch3_pois_lambda", "\u03bb (\u015brednia liczba zdarze\u0144):",
                      min = 0.5, max = 20, value = 3, step = 0.5),
          hr(),
          div(class = "preset-buttons",
            actionButton("ch3_pois_preset1", "B\u0142\u0119dy\n(\u03bb=2)",
                         class = "btn-outline-primary"),
            actionButton("ch3_pois_preset2", "Klienci\n(\u03bb=8)",
                         class = "btn-outline-warning"),
            actionButton("ch3_pois_preset3", "Wypadki\n(\u03bb=0.5)",
                         class = "btn-outline-danger")
          ),
          hr(),
          checkboxInput("ch3_pois_show_stats", "Poka\u017c E(X) i SD", value = TRUE)
        ),
        column(8,
          plotOutput("ch3_pois_plot", height = "350px"),
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

    # ========================================================================
    # WIDGET 4: Porownanie trzech rozkladow
    # ========================================================================
    div(class = "section-title", "Por\u00f3wnanie trzech rozk\u0142ad\u00f3w"),

    div(class = "narrative",
      p("Zobaczmy wszystkie trzy rozk\u0142ady obok siebie. Zwr\u00f3\u0107 uwag\u0119 na
        r\u00f3\u017cnice w kszta\u0142tach i na to, kiedy rozk\u0142ad dyskretny zaczyna
        wygl\u0105da\u0107 jak g\u0142adki 'dzwon'.")
    ),

    div(class = "widget-block",
      h4("Trzy rozk\u0142ady obok siebie"),
      checkboxInput("ch3_compare_show_ev", "Poka\u017c warto\u015b\u0107 oczekiwan\u0105 (linia)", value = FALSE),
      checkboxInput("ch3_compare_show_sd", "Poka\u017c \u00b1 odchylenie standardowe (pas)", value = FALSE),
      plotOutput("ch3_compare_plot", height = "300px")
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

ch3_server <- function(input, output, session) {

  # --- Widget 1: Jednostajny dyskretny ---
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
      theme_prob()
  })

  # --- Widget 2: Dwumianowy ---
  observeEvent(input$ch3_binom_preset1, {
    updateSliderInput(session, "ch3_binom_n", value = 10)
    updateSliderInput(session, "ch3_binom_p", value = 0.5)
  })
  observeEvent(input$ch3_binom_preset2, {
    updateSliderInput(session, "ch3_binom_n", value = 100)
    updateSliderInput(session, "ch3_binom_p", value = 0.02)
  })
  observeEvent(input$ch3_binom_preset3, {
    updateSliderInput(session, "ch3_binom_n", value = 20)
    updateSliderInput(session, "ch3_binom_p", value = 0.25)
  })

  output$ch3_binom_plot <- renderPlot({
    n <- input$ch3_binom_n
    p <- input$ch3_binom_p
    show_stats <- input$ch3_binom_show_stats

    x_vals <- 0:n
    probs <- dbinom(x_vals, n, p)

    # Ogranicz zakres osi x do sensownych wartosci
    sig_range <- which(probs > 0.001)
    if (length(sig_range) > 0) {
      x_min <- max(0, min(sig_range) - 2)
      x_max <- min(n, max(sig_range) + 2)
    } else {
      x_min <- 0; x_max <- n
    }
    keep <- x_vals >= x_min & x_vals <= x_max

    df <- data.frame(x = x_vals[keep], prob = probs[keep])
    mu <- n * p
    sigma <- sqrt(n * p * (1 - p))

    pl <- ggplot(df, aes(x = x, y = prob)) +
      geom_col(fill = col_binomial, color = "white", alpha = 0.85, width = 0.7) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(title = paste0("B(n=", n, ", p=", p, ")"),
           x = "Liczba sukces\u00f3w (k)", y = "P(X = k)") +
      theme_prob()

    if (show_stats) {
      pl <- pl +
        geom_vline(xintercept = mu, color = col_secondary, linewidth = 1.2, linetype = "dashed") +
        annotate("rect", xmin = mu - sigma, xmax = mu + sigma,
                 ymin = 0, ymax = Inf, fill = col_secondary, alpha = 0.08)
    }
    pl
  })

  output$ch3_binom_stats <- renderUI({
    n <- input$ch3_binom_n
    p <- input$ch3_binom_p
    mu <- n * p
    sigma <- sqrt(n * p * (1 - p))

    div(style = "text-align: center; margin-top: 10px;",
      div(class = "stat-box", style = paste0("background: ", col_secondary, ";"),
          paste0("E(X) = np = ", round(mu, 2))),
      div(class = "stat-box", style = paste0("background: ", col_dark, ";"),
          paste0("SD = \u221a(np(1-p)) = ", round(sigma, 2)))
    )
  })

  # --- Widget 3: Poissona ---
  observeEvent(input$ch3_pois_preset1, {
    updateSliderInput(session, "ch3_pois_lambda", value = 2)
  })
  observeEvent(input$ch3_pois_preset2, {
    updateSliderInput(session, "ch3_pois_lambda", value = 8)
  })
  observeEvent(input$ch3_pois_preset3, {
    updateSliderInput(session, "ch3_pois_lambda", value = 0.5)
  })

  output$ch3_pois_plot <- renderPlot({
    lambda <- input$ch3_pois_lambda
    show_stats <- input$ch3_pois_show_stats

    x_max <- max(20, qpois(0.999, lambda))
    x_vals <- 0:x_max
    probs <- dpois(x_vals, lambda)

    sig <- which(probs > 0.001)
    x_show_max <- min(x_max, max(sig) + 2)
    keep <- x_vals <= x_show_max

    df <- data.frame(x = x_vals[keep], prob = probs[keep])
    sigma <- sqrt(lambda)

    pl <- ggplot(df, aes(x = x, y = prob)) +
      geom_col(fill = col_poisson, color = "white", alpha = 0.85, width = 0.7) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(title = paste0("Pois(\u03bb=", lambda, ")"),
           x = "Liczba zdarze\u0144 (k)", y = "P(X = k)") +
      theme_prob()

    if (show_stats) {
      pl <- pl +
        geom_vline(xintercept = lambda, color = col_secondary, linewidth = 1.2, linetype = "dashed") +
        annotate("rect", xmin = lambda - sigma, xmax = lambda + sigma,
                 ymin = 0, ymax = Inf, fill = col_secondary, alpha = 0.08)
    }
    pl
  })

  output$ch3_pois_stats <- renderUI({
    lambda <- input$ch3_pois_lambda
    div(style = "text-align: center; margin-top: 10px;",
      div(class = "stat-box", style = paste0("background: ", col_poisson, ";"),
          paste0("E(X) = \u03bb = ", lambda)),
      div(class = "stat-box", style = paste0("background: ", col_dark, ";"),
          paste0("SD = \u221a\u03bb = ", round(sqrt(lambda), 2))),
      div(class = "stat-box", style = paste0("background: ", col_warning, ";"),
          paste0("Var = \u03bb = ", lambda))
    )
  })

  # --- Widget 4: Porownanie ---
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

    df_all <- rbind(df1, df2, df3)
    df_all$dist <- factor(df_all$dist,
                          levels = c("Jednostajny\n(kostka)", "Dwumianowy\nB(20, 0.3)", "Poissona\nPois(4)"))

    stats_df <- data.frame(
      dist = levels(df_all$dist),
      mu = c(mu1, mu2, mu3),
      sd = c(sd1, sd2, sd3)
    )

    pl <- ggplot(df_all, aes(x = x, y = prob)) +
      geom_col(aes(fill = dist), color = "white", alpha = 0.85, width = 0.7, show.legend = FALSE) +
      facet_wrap(~dist, scales = "free_x") +
      scale_fill_manual(values = c(col_uniform, col_binomial, col_poisson)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = "Warto\u015b\u0107", y = "Prawdopodobie\u0144stwo") +
      theme_prob(base_size = 13)

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
