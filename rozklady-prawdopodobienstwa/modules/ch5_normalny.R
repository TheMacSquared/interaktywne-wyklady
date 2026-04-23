# ============================================================================
# CHAPTER 5: Rozklad normalny
# ============================================================================

ch5_ui <- list(
  id = "ch-normalny", num = "05", title = "Rozkład normalny",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 05 · Rozkłady prawdopodobieństwa",
      num    = "05",
      title  = "Rozkład normalny.",
      lead   = "Wiemy już, czym są rozkłady ciągłe i jak interpretować gęstość.
                Teraz poznamy najważniejszy ze wszystkich rozkładów."
    ),

    h2(id = "ch5-intro", class = "section-title", "Rozkład normalny — królowa rozkładów"),

    div(class = "narrative",
      p("Rozkład normalny (Gaussa) to ", tags$b("najczęściej spotykany"),
        " rozkład w statystyce. Opisuje go tylko ",
        tags$b("dwa parametry"), ": średnia μ (gdzie jest środek)
        i odchylenie standardowe σ (jak szerokie jest rozproszenie)."),
      p("Dlaczego jest aż tak ważny? Odpowiedź poznamy w następnym rozdziale (CLT).
        Na razie zbudujmy intuicję.")
    ),

    # ========================================================================
    # WIDGET 1: Dwa parametry, nieskonczone mozliwosci
    # ========================================================================
    h2(id = "ch5-parametry", class = "section-title", "Dwa parametry — nieskończone możliwości"),

    figure_panel(
      label = "Ryc. 5.1",
      title = "Eksploracja N(μ, σ)",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch5_mu", "μ (średnia):",
                      min = -10, max = 10, value = 0, step = 0.5),
          sliderInput("ch5_sigma", "σ (odch. std.):",
                      min = 0.5, max = 5, value = 1, step = 0.1),
          hr(),
          div(class = "preset-buttons",
            actionButton("ch5_preset_std", "N(0,1)\nStandardowy",
                         class = "btn-outline-primary"),
            actionButton("ch5_preset_wzrost_k", "Wzrost\nkobiet",
                         class = "btn-outline-success"),
            actionButton("ch5_preset_iq", "IQ",
                         class = "btn-outline-warning"),
            actionButton("ch5_preset_temp", "Temp.\nciała",
                         class = "btn-outline-danger")
          ),
          hr(),
          checkboxInput("ch5_show_empirical", "Pokaż regułę 68-95-99.7", value = TRUE)
        ),
        column(8,
          plotOutput("ch5_explore_plot", height = "400px"),
          uiOutput("ch5_explore_stats")
        )
      )
    ),

    margin_callout(
      label = "Reguła 68-95-99.7",
      "Około 68% danych leży w μ±σ, 95% w μ±2σ, a 99.7% w μ±3σ."
    ),

    # ========================================================================
    # WIDGET 2: Porownanie rozkladow
    # ========================================================================
    h2(id = "ch5-porownanie", class = "section-title", "Porównanie dwóch rozkładów normalnych"),

    figure_panel(
      label = "Ryc. 5.2",
      title = "Dwie krzywe normalne",
      full_width = TRUE,
      fluidRow(
        column(3,
          h5("Rozkład A", style = "color: #3498db;"),
          sliderInput("ch5_cmp_mu1", "μ₁:", min = -5, max = 15, value = 5, step = 0.5),
          sliderInput("ch5_cmp_s1", "σ₁:", min = 0.5, max = 5, value = 1.5, step = 0.1)
        ),
        column(3,
          h5("Rozkład B", style = "color: #e74c3c;"),
          sliderInput("ch5_cmp_mu2", "μ₂:", min = -5, max = 15, value = 8, step = 0.5),
          sliderInput("ch5_cmp_s2", "σ₂:", min = 0.5, max = 5, value = 2, step = 0.1),
          hr(),
          actionButton("ch5_cmp_preset", "Mężczyźni vs\nkobiety (wzrost)",
                       class = "btn-outline-primary", width = "100%")
        ),
        column(6,
          plotOutput("ch5_compare_plot", height = "350px")
        )
      )
    ),

    # ========================================================================
    # WIDGET 3: Standaryzacja (z-score)
    # ========================================================================
    h2(id = "ch5-standaryzacja", class = "section-title", "Standaryzacja (z-score)"),

    div(class = "narrative",
      p("Każdy rozkład normalny można sprowadzić do ",
        tags$b("standardowego N(0, 1)"), " za pomocą transformacji:"),
      div(class = "formula-box",
        withMathJax(helpText("$$z = \\frac{x - \\mu}{\\sigma}$$"))
      ),
      p("Z-score mówi nam: ", tags$b("ile odchyleń standardowych"),
        " dana wartość leży od średniej.")
    ),

    figure_panel(
      label = "Ryc. 5.3",
      title = "Kalkulator z-score",
      full_width = TRUE,
      fluidRow(
        column(4,
          numericInput("ch5_z_mu", "μ (np. średnia egzaminu):", value = 65),
          numericInput("ch5_z_sigma", "σ (np. odch. std.):", value = 10, min = 0.1),
          numericInput("ch5_z_x", "x (wartość do standaryzacji):", value = 80),
          hr(),
          uiOutput("ch5_z_result")
        ),
        column(8,
          plotOutput("ch5_z_plot", height = "350px")
        )
      )
    ),

    # ========================================================================
    # WIDGET 4: Obliczanie prawdopodobienstw
    # ========================================================================
    h2(id = "ch5-prawdop", class = "section-title", "Obliczanie prawdopodobieństw"),

    div(class = "narrative",
      p("Znając z-score, możemy obliczyć prawdopodobieństwo dowolnego
        przedziału. W R używamy funkcji ", tags$code("pnorm()"), ".")
    ),

    figure_panel(
      label = "Ryc. 5.4",
      title = "Kalkulator prawdopodobieństw N(0, 1)",
      full_width = TRUE,
      fluidRow(
        column(4,
          radioButtons("ch5_prob_type", "Typ pytania:",
            choices = c(
              "P(Z < a)" = "less",
              "P(Z > a)" = "greater",
              "P(a < Z < b)" = "between"
            ),
            selected = "between"
          ),
          sliderInput("ch5_prob_a", "a:", min = -4, max = 4, value = -1, step = 0.05),
          conditionalPanel(
            condition = "input.ch5_prob_type == 'between'",
            sliderInput("ch5_prob_b", "b:", min = -4, max = 4, value = 1, step = 0.05)
          )
        ),
        column(8,
          plotOutput("ch5_prob_plot", height = "300px"),
          uiOutput("ch5_prob_result")
        )
      )
    ),

    div(class = "formula-box",
      withMathJax(helpText(
        "$$f(x) = \\frac{1}{\\sigma\\sqrt{2\\pi}} e^{-\\frac{(x-\\mu)^2}{2\\sigma^2}}, \\quad E(X) = \\mu, \\quad Var(X) = \\sigma^2$$"
      ))
    ),

    lc_chapter_next(
      num       = "06",
      title     = "Centralne Twierdzenie Graniczne",
      lead      = "dlaczego rozkład normalny pojawia się wszędzie w naturze.",
      target_id = "ch-ctg"
    )
  )
)

# --------------------------------------------------------------------------
# Chapter 5 Server
# --------------------------------------------------------------------------

ch5_server <- function(input, output, session) {

  # --- Presety ---
  observeEvent(input$ch5_preset_std, {
    updateSliderInput(session, "ch5_mu", value = 0)
    updateSliderInput(session, "ch5_sigma", value = 1)
  })
  observeEvent(input$ch5_preset_wzrost_k, {
    updateSliderInput(session, "ch5_mu", value = 166, min = 140, max = 200)
    updateSliderInput(session, "ch5_sigma", value = 6, min = 1, max = 15)
  })
  observeEvent(input$ch5_preset_iq, {
    updateSliderInput(session, "ch5_mu", value = 100, min = 50, max = 150)
    updateSliderInput(session, "ch5_sigma", value = 15, min = 1, max = 30)
  })
  observeEvent(input$ch5_preset_temp, {
    updateSliderInput(session, "ch5_mu", value = 36.6, min = 34, max = 40)
    updateSliderInput(session, "ch5_sigma", value = 0.4, min = 0.1, max = 2)
  })

  # --- Widget 1: Eksploracja ---
  output$ch5_explore_plot <- renderPlot({
    mu <- input$ch5_mu
    sigma <- input$ch5_sigma
    show_emp <- input$ch5_show_empirical

    x_seq <- seq(mu - 4*sigma, mu + 4*sigma, length.out = 500)
    df <- data.frame(x = x_seq, y = dnorm(x_seq, mu, sigma))

    p <- ggplot(df, aes(x = x, y = y)) +
      geom_line(color = col_normal, linewidth = 1.5)

    if (show_emp) {
      # 1 SD
      shade1 <- data.frame(
        x = seq(mu - sigma, mu + sigma, length.out = 200),
        y = dnorm(seq(mu - sigma, mu + sigma, length.out = 200), mu, sigma)
      )
      p <- p + geom_area(data = shade1, aes(x = x, y = y),
                         fill = col_normal, alpha = 0.4)

      # 2 SD
      shade2l <- data.frame(
        x = seq(mu - 2*sigma, mu - sigma, length.out = 100),
        y = dnorm(seq(mu - 2*sigma, mu - sigma, length.out = 100), mu, sigma)
      )
      shade2r <- data.frame(
        x = seq(mu + sigma, mu + 2*sigma, length.out = 100),
        y = dnorm(seq(mu + sigma, mu + 2*sigma, length.out = 100), mu, sigma)
      )
      p <- p +
        geom_area(data = shade2l, aes(x = x, y = y), fill = col_normal, alpha = 0.25) +
        geom_area(data = shade2r, aes(x = x, y = y), fill = col_normal, alpha = 0.25)

      # 3 SD
      shade3l <- data.frame(
        x = seq(mu - 3*sigma, mu - 2*sigma, length.out = 100),
        y = dnorm(seq(mu - 3*sigma, mu - 2*sigma, length.out = 100), mu, sigma)
      )
      shade3r <- data.frame(
        x = seq(mu + 2*sigma, mu + 3*sigma, length.out = 100),
        y = dnorm(seq(mu + 2*sigma, mu + 3*sigma, length.out = 100), mu, sigma)
      )
      p <- p +
        geom_area(data = shade3l, aes(x = x, y = y), fill = col_normal, alpha = 0.12) +
        geom_area(data = shade3r, aes(x = x, y = y), fill = col_normal, alpha = 0.12)

      # Etykiety
      y_top <- dnorm(mu, mu, sigma)
      p <- p +
        annotate("text", x = mu, y = y_top * 0.6, label = "68%",
                 size = 5, fontface = "bold", color = upwr_secondary) +
        annotate("text", x = mu, y = y_top * 0.35, label = "95%",
                 size = 4.5, color = upwr_secondary) +
        annotate("text", x = mu, y = y_top * 0.15, label = "99.7%",
                 size = 4, color = "#7f8c8d")
    }

    p + geom_vline(xintercept = mu, color = upwr_secondary, linetype = "dashed") +
      labs(title = paste0("N(μ=", mu, ", σ=", sigma, ")"),
           x = "x", y = "f(x)") +
      theme_upwr()
  })

  output$ch5_explore_stats <- renderUI({
    mu <- input$ch5_mu
    sigma <- input$ch5_sigma
    div(style = "text-align: center; margin-top: 10px;",
      div(class = "stat-box", style = paste0("background: ", col_normal, ";"),
          paste0("μ = ", mu)),
      div(class = "stat-box", style = paste0("background: ", upwr_secondary, ";"),
          paste0("σ = ", sigma)),
      div(class = "stat-box", style = paste0("background: ", unname(upwr_cat["bursztyn"]), ";"),
          paste0("68%: [", round(mu - sigma, 1), ", ", round(mu + sigma, 1), "]"))
    )
  })

  # --- Widget 2: Porownanie ---
  observeEvent(input$ch5_cmp_preset, {
    updateSliderInput(session, "ch5_cmp_mu1", value = 166, min = 140, max = 200)
    updateSliderInput(session, "ch5_cmp_s1", value = 6, min = 1, max = 15)
    updateSliderInput(session, "ch5_cmp_mu2", value = 178, min = 140, max = 200)
    updateSliderInput(session, "ch5_cmp_s2", value = 7, min = 1, max = 15)
  })

  output$ch5_compare_plot <- renderPlot({
    mu1 <- input$ch5_cmp_mu1; s1 <- input$ch5_cmp_s1
    mu2 <- input$ch5_cmp_mu2; s2 <- input$ch5_cmp_s2

    x_min <- min(mu1 - 4*s1, mu2 - 4*s2)
    x_max <- max(mu1 + 4*s1, mu2 + 4*s2)
    x_seq <- seq(x_min, x_max, length.out = 500)

    df <- data.frame(
      x = rep(x_seq, 2),
      y = c(dnorm(x_seq, mu1, s1), dnorm(x_seq, mu2, s2)),
      group = rep(c("A", "B"), each = 500)
    )

    ggplot(df, aes(x = x, y = y, color = group, fill = group)) +
      geom_line(linewidth = 1.2) +
      geom_area(alpha = 0.15, position = "identity") +
      scale_color_manual(values = c("A" = unname(upwr_cat["niebo"]), "B" = unname(upwr_cat["terakota"])),
                         labels = c(paste0("A: N(", mu1, ", ", s1, ")"),
                                    paste0("B: N(", mu2, ", ", s2, ")")),
                         name = "") +
      scale_fill_manual(values = c("A" = unname(upwr_cat["niebo"]), "B" = unname(upwr_cat["terakota"])),
                        guide = "none") +
      labs(title = "Porównanie dwóch rozkładów normalnych",
           x = "x", y = "f(x)") +
      theme_upwr() +
      theme(legend.position = "top")
  })

  # --- Widget 3: Z-score ---
  output$ch5_z_result <- renderUI({
    mu <- input$ch5_z_mu
    sigma <- input$ch5_z_sigma
    x <- input$ch5_z_x
    req(sigma > 0)

    z <- (x - mu) / sigma

    div(
      div(class = "stat-box",
          style = paste0("background: ", col_normal, "; display: block; margin-bottom: 8px;"),
          paste0("z = (", x, " - ", mu, ") / ", sigma, " = ", round(z, 2))),
      div(class = "callout-info", style = "margin-top: 8px;",
        paste0("Wartość ", x, " leży ", round(abs(z), 2),
               " odchyleń standardowych ",
               if (z >= 0) "powyżej" else "poniżej", " średniej."))
    )
  })

  output$ch5_z_plot <- renderPlot({
    mu <- input$ch5_z_mu
    sigma <- input$ch5_z_sigma
    x <- input$ch5_z_x
    req(sigma > 0)

    z <- (x - mu) / sigma

    # Gorny wykres: oryginalna skala
    x_seq <- seq(mu - 4*sigma, mu + 4*sigma, length.out = 500)
    df_orig <- data.frame(x = x_seq, y = dnorm(x_seq, mu, sigma))

    # Dolny wykres: standaryzowana skala
    z_seq <- seq(-4, 4, length.out = 500)
    df_std <- data.frame(x = z_seq, y = dnorm(z_seq))

    p1 <- ggplot(df_orig, aes(x = x, y = y)) +
      geom_line(color = unname(upwr_cat["niebo"]), linewidth = 1.2) +
      geom_vline(xintercept = x, color = unname(upwr_cat["terakota"]), linewidth = 1.2) +
      annotate("point", x = x, y = dnorm(x, mu, sigma),
               color = unname(upwr_cat["terakota"]), size = 4) +
      annotate("text", x = x, y = dnorm(x, mu, sigma) * 1.2,
               label = paste0("x = ", x), color = unname(upwr_cat["terakota"]),
               size = 4, fontface = "bold", vjust = -0.5) +
      labs(title = paste0("Oryginalna skala: N(", mu, ", ", sigma, ")"),
           x = "x", y = "f(x)") +
      theme_upwr(base_size = 12)

    p2 <- ggplot(df_std, aes(x = x, y = y)) +
      geom_line(color = col_normal, linewidth = 1.2) +
      geom_vline(xintercept = z, color = unname(upwr_cat["terakota"]), linewidth = 1.2) +
      annotate("point", x = z, y = dnorm(z), color = unname(upwr_cat["terakota"]), size = 4) +
      annotate("text", x = z, y = dnorm(z) * 1.2,
               label = paste0("z = ", round(z, 2)), color = unname(upwr_cat["terakota"]),
               size = 4, fontface = "bold", vjust = -0.5) +
      labs(title = "Standaryzowana skala: N(0, 1)",
           x = "z", y = "f(z)") +
      theme_upwr(base_size = 12)

    gridExtra::grid.arrange(p1, p2, ncol = 1)
  })

  # --- Widget 4: Kalkulator prawdopodobienstw ---
  output$ch5_prob_plot <- renderPlot({
    type <- input$ch5_prob_type
    a <- input$ch5_prob_a
    b <- if (type == "between") input$ch5_prob_b else NULL

    x_seq <- seq(-4, 4, length.out = 500)
    df <- data.frame(x = x_seq, y = dnorm(x_seq))

    p <- ggplot(df, aes(x = x, y = y)) +
      geom_line(color = upwr_secondary, linewidth = 1.2)

    if (type == "less") {
      shade <- data.frame(x = x_seq[x_seq <= a], y = dnorm(x_seq[x_seq <= a]))
      prob <- pnorm(a)
      p <- p + geom_area(data = shade, fill = unname(upwr_cat["niebo"]), alpha = 0.4) +
        geom_vline(xintercept = a, color = unname(upwr_cat["terakota"]), linetype = "dashed")
    } else if (type == "greater") {
      shade <- data.frame(x = x_seq[x_seq >= a], y = dnorm(x_seq[x_seq >= a]))
      prob <- 1 - pnorm(a)
      p <- p + geom_area(data = shade, fill = unname(upwr_cat["terakota"]), alpha = 0.4) +
        geom_vline(xintercept = a, color = unname(upwr_cat["terakota"]), linetype = "dashed")
    } else {
      shade <- data.frame(x = x_seq[x_seq >= a & x_seq <= b],
                          y = dnorm(x_seq[x_seq >= a & x_seq <= b]))
      prob <- pnorm(b) - pnorm(a)
      p <- p + geom_area(data = shade, fill = col_normal, alpha = 0.4) +
        geom_vline(xintercept = a, color = unname(upwr_cat["terakota"]), linetype = "dashed") +
        geom_vline(xintercept = b, color = unname(upwr_cat["terakota"]), linetype = "dashed")
    }

    p + annotate("text", x = 0, y = 0.2,
                 label = sprintf("P = %.4f", prob),
                 size = 6, fontface = "bold", color = upwr_secondary) +
      labs(title = "Rozkład standardowy N(0, 1)", x = "z", y = "f(z)") +
      theme_upwr()
  })

  output$ch5_prob_result <- renderUI({
    type <- input$ch5_prob_type
    a <- input$ch5_prob_a
    b <- if (type == "between") input$ch5_prob_b else NULL

    prob <- switch(type,
      "less" = pnorm(a),
      "greater" = 1 - pnorm(a),
      "between" = pnorm(b) - pnorm(a)
    )

    label <- switch(type,
      "less" = paste0("P(Z < ", a, ")"),
      "greater" = paste0("P(Z > ", a, ")"),
      "between" = paste0("P(", a, " < Z < ", b, ")")
    )

    r_code <- switch(type,
      "less" = paste0("pnorm(", a, ")"),
      "greater" = paste0("1 - pnorm(", a, ")"),
      "between" = paste0("pnorm(", b, ") - pnorm(", a, ")")
    )

    div(style = "text-align: center; margin-top: 10px;",
      div(class = "stat-box", style = paste0("background: ", col_normal, ";"),
          paste0(label, " = ", sprintf("%.4f", prob))),
      div(class = "stat-box", style = paste0("background: ", upwr_secondary, ";"),
          paste0(sprintf("%.2f", prob * 100), "%")),
      div(style = "margin-top: 8px; font-size: 13px; color: #7f8c8d;",
          paste0("W R: ", r_code))
    )
  })

}
