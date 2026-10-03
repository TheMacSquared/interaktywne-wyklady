# ============================================================================
# CHAPTER 8: Modele AR — autoregresja
# ============================================================================

ch8_ui <- list(
  id    = "ch-ar",
  num   = "08",
  title = "Modele AR: autoregresja",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 08 · Szeregi czasowe",
      num    = "08",
      title  = "Modele AR.",
      lead   = "Autoregresja: jutrzejsza wartość to ważona suma wczorajszych wartości plus szum.
                AR(p) to pierwszy i najprostszy model rodziny ARIMA."
    ),

    lc_h2("ch8-intuicja", "Intuicja: 'pamiętam przeszłość'"),

    tagList(
      lc_p("Model AR(1) mówi: dzisiejsza wartość szeregu to φ razy wczorajsza wartość,
        plus losowy szum. Jeśli φ = 0.8, to 80% dzisiaj pochodzi z wczoraj.
        Jeśli φ = 0 — nie ma żadnej pamięci — to biały szum."),
      lc_formula_box(
        withMathJax(helpText("$$x_t = \\phi_1 x_{t-1} + \\varepsilon_t \\quad \\text{AR(1)}$$")),
        withMathJax(helpText("$$x_t = \\phi_1 x_{t-1} + \\phi_2 x_{t-2} + \\cdots + \\phi_p x_{t-p} + \\varepsilon_t \\quad \\text{AR(p)}$$")),
        p("gdzie ", withMathJax("\\(\\varepsilon_t \\sim N(0, \\sigma^2)\\)"), " — biały szum (niezależny, o stałej wariancji)")
      ),
      lc_more("Chcesz więcej matematyki?",
        p("Warunek stacjonarności AR(1): |φ₁| < 1."),
        p("AR(p) jest stacjonarny, jeśli pierwiastki wielomianu charakterystycznego ",
          withMathJax("\\(1 - \\phi_1 z - \\cdots - \\phi_p z^p = 0\\)"),
          " leżą poza kołem jednostkowym."),
        p("Wariancja AR(1): ",
          withMathJax("\\(\\text{Var}(x_t) = \\sigma^2 / (1 - \\phi_1^2)\\)"), ".")
      )
    ),

    lc_h2("ch8-phi-suwak", "Suwak φ₁ — co się dzieje z szeregiem?"),

    tagList(
      lc_p("Zmień wartość φ₁ i obserwuj, jak zmienia się charakter szeregu AR(1).
        Szczególnie zwróć uwagę na zachowanie przy |φ₁| bliskim 1 i przy wartościach ujemnych.")
    ),

    figure_panel(
      label = "Ryc. 8.1", title = "AR(1): co robi φ₁?",
      full_width = TRUE,
      fluidRow(
        column(4,
          lc_slider("ch8_phi", "φ₁", -0.99, 0.99, 0.8, 0.05),
          lc_slider("ch8_sigma", "Szum σ", 0.2, 3, 1, 0.1),
          numericInput("ch8_n", "n:", value = 200, min = 50, max = 500, step = 50),
          lc_action("ch8_new", "Nowa realizacja", variant = "solid"),
          uiOutput("ch8_phi_info")
        ),
        column(8,
          zoom_plot_ui("ch8_ts_plot", height = "250px"),
          zoom_plot_ui("ch8_phi_acf_plot", height = "180px")
        )
      )
    ),

    lc_h2("ch8-step-forecast", "Prognoza z AR(1) krok po kroku"),

    tagList(
      lc_p("Prognozowanie z AR(1) jest proste: podstawiamy ostatnią obserwację i wyliczamy
        przewidywaną wartość. Prognoza na więcej kroków: iterujemy.")
    ),

    figure_panel(
      label = "Ryc. 8.2",
      full_width = TRUE,
      lc_step_widget("ch8_fc",
        title = "Budowanie prognozy AR(1)",
        steps = c("Scenariusz syntetyczny", "Prognoza t+1",
                  "Prognoza t+2, t+3, ...", "Zanik pamięci"),
        toolbar = lc_toolbar(
          lc_slider("ch8_fc_phi", "φ₁", 0.5, 0.95, 0.8, 0.05)
        ),
        plot_id = "ch8_fc_plot"
      )
    ),

    lc_h2("ch8-estymacja", "Estymacja: prawdziwe φ₁ vs. estymowane"),

    tagList(
      lc_p("W praktyce nie znamy φ₁. Estymujemy go z danych metodą najmniejszych kwadratów
        (lub MLE). Sprawdźmy, jak dobrze estymacja działa przy różnych n.")
    ),

    figure_panel(
      label = "Ryc. 8.3", title = "φ_true vs φ_hat — błąd estymacji przy różnych n",
      full_width = TRUE,
      fluidRow(
        column(4,
          lc_slider("ch8_est_phi", "Prawdziwe φ₁", 0.3, 0.95, 0.75, 0.05),
          lc_slider("ch8_est_n", "Rozmiar próby n", 30, 500, 100, 10),
          numericInput("ch8_est_reps", "Liczba symulacji:", value = 500, min = 100, max = 2000, step = 100),
          lc_action("ch8_est_run", "Symuluj", variant = "solid"),
          uiOutput("ch8_est_stats")
        ),
        column(8,
          zoom_plot_ui("ch8_est_plot", height = "280px")
        )
      )
    ),

    lc_chapter_next(
      num       = "09",
      title     = "Modele MA i ARMA",
      lead      = "pamięć na błędy — kontrast z AR",
      target_id = "ch-ma-arma"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch8_server <- function(input, output, session) {

  ch8_seed <- reactiveVal(42)
  observeEvent(input$ch8_new, ch8_seed(ch8_seed() + 1))

  ch8_data <- reactive({
    phi   <- input$ch8_phi
    sigma <- input$ch8_sigma
    n     <- if (!is.null(input$ch8_n)) input$ch8_n else 200
    set.seed(ch8_seed())
    as.numeric(arima.sim(list(ar = phi), n = n, sd = sigma))
  })

  zoom_plot_server("ch8_ts_plot", reactive({
    x  <- ch8_data()
    phi <- input$ch8_phi
    df <- data.frame(t = seq_along(x), x = x)
    ggplot(df, aes(x = t, y = x)) +
      geom_line(color = upwr_secondary, linewidth = 0.7) +
      labs(x = "Czas", y = "x_t",
           title = paste0("AR(1) z φ₁ = ", phi)) +
      theme_upwr()
  }))

  zoom_plot_server("ch8_phi_acf_plot", reactive({
    x <- ch8_data()
    plot_acf_gg(x, lag.max = 20, title = "ACF szeregu AR(1)")
  }))

  output$ch8_phi_info <- renderUI({
    phi <- input$ch8_phi
    desc <- if (phi > 0.9) {
      "φ₁ bliski 1: szereg bardzo 'leniwy' — powolne zanikanie do średniej. Prawie niestacjonarny (random walk)."
    } else if (phi > 0.5) {
      "φ₁ umiarkowane: szereg wykazuje wyraźną autokorelację, ale wraca do średniej."
    } else if (phi > 0) {
      "φ₁ małe, dodatnie: słaba autokorelacja, szybki powrót do średniej."
    } else if (phi < -0.5) {
      "φ₁ ujemne, silne: szereg oscyluje — wartości naprzemiennie powyżej i poniżej średniej."
    } else {
      "φ₁ = 0: biały szum — brak autokorelacji."
    }
    lc_feedback(type = "info", p(desc))
  })

  # Krok widgetu (1..4) żyje w przeglądarce; zmiana φ₁ nie zmienia kroku.
  ch8_fc_step <- lc_step_server("ch8_fc", input)$step

  ch8_fc_hist <- reactive({
    set.seed(77)
    phi <- if (!is.null(input$ch8_fc_phi)) input$ch8_fc_phi else 0.8
    as.numeric(arima.sim(list(ar = phi), n = 40, sd = 1))
  })

  zoom_plot_server("ch8_fc_plot", reactive({
    step <- ch8_fc_step()
    phi  <- if (!is.null(input$ch8_fc_phi)) input$ch8_fc_phi else 0.8
    hist <- ch8_fc_hist()
    n    <- length(hist)
    n_fc <- 12

    fc_vals <- numeric(n_fc)
    fc_vals[1] <- phi * hist[n]
    for (i in 2:n_fc) fc_vals[i] <- phi * fc_vals[i-1]

    df_hist <- data.frame(t = seq_len(n), x = hist, type = "Historia")
    df_fc   <- data.frame(t = n + seq_len(n_fc), x = fc_vals, type = "Prognoza")

    # Stała rama: historia i cała prognoza, niezależnie od kroku.
    y_pad  <- diff(range(hist, fc_vals, 0)) * 0.08
    y_lims <- range(hist, fc_vals, 0) + c(-y_pad, y_pad)
    x_max  <- n + n_fc + 1
    y_off  <- diff(y_lims) * 0.07
    side   <- if (fc_vals[1] >= 0) 1 else -1

    p <- ggplot(df_hist, aes(x = t, y = x)) +
      step_layer(geom_line, "data") +
      step_line("known", yintercept = 0) +
      labs(x = "Czas", y = "x_t")

    if (step >= 2) {
      role <- step_role(step, 2)
      p <- p +
        step_layer(geom_segment, role,
                   data = data.frame(x0 = n, x1 = n + 1, y0 = hist[n], y1 = fc_vals[1]),
                   mapping = aes(x = x0, xend = x1, y = y0, yend = y1),
                   linetype = "22") +
        step_layer(geom_point, role, data = df_fc[1, ], mapping = aes(x = t, y = x),
                   size = 3.5) +
        # Etykieta po stronie z dala od zera (tam zmierza prognoza), dosunięta
        # do prawej krawędzi ramy. Zwykły krój: mono nie ma znaku x̂.
        annotate("text", x = x_max, y = fc_vals[1] + side * y_off,
                 label = paste0("x̂(t+1) = ", round(phi, 2), "·", round(hist[n], 2),
                                " = ", round(fc_vals[1], 2)),
                 hjust = 1, vjust = 0.5, colour = STEP_ROLES[[role]]$colour,
                 fontface = "bold", size = 3.5)
    }
    if (step >= 3) {
      role <- step_role(step, 3)
      p <- p +
        step_layer(geom_line, role, data = df_fc, mapping = aes(x = t, y = x),
                   linetype = "22") +
        step_layer(geom_point, role, data = df_fc, mapping = aes(x = t, y = x),
                   size = 2.5)
    }
    if (step >= 4) {
      p <- p + step_line("new", yintercept = 0) +
        step_label(x_max, -side * y_off, "Prognoza → E[x] = 0", role = "new",
                   hjust = 1, vjust = 0.5)
    }
    p + step_frame(xlim = c(0, x_max), ylim = y_lims)
  }))

  output$ch8_fc_text <- renderUI({
    step <- ch8_fc_step()
    phi  <- if (!is.null(input$ch8_fc_phi)) input$ch8_fc_phi else 0.8
    switch(as.character(step),
      "1" = "Scenariusz syntetyczny: ostatnia wartość x_t to punkt startowy.",
      "2" = paste0("Prognoza na 1 krok: x̂(t+1) = φ₁ · x_t = ", round(phi, 2), " · x_t."),
      "3" = paste0("Prognoza wielokrokowa: każdy kolejny krok iterujemy: x̂(t+k) = φ₁^k · x_t."),
      "4" = paste0("Zanik pamięci: przy φ₁ = ", phi,
                   " prognoza zmierza do 0 (średniej). Po ~",
                   ceiling(-3 / log10(phi)), " krokach jesteśmy blisko 0."),
      ""
    )
  })

  ch8_est_results <- reactiveVal(NULL)

  observeEvent(input$ch8_est_run, {
    phi  <- input$ch8_est_phi
    n    <- input$ch8_est_n
    reps <- input$ch8_est_reps
    ests <- vapply(seq_len(reps), function(i) {
      set.seed(i * 1000)
      x  <- as.numeric(arima.sim(list(ar = phi), n = n))
      fit <- ar(x, order.max = 1, method = "yule-walker", aic = FALSE)
      fit$ar[1]
    }, numeric(1))
    ch8_est_results(list(ests = ests, phi_true = phi, n = n))
  })

  zoom_plot_server("ch8_est_plot", reactive({
    res <- ch8_est_results()
    if (is.null(res)) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                                  label = "Kliknij 'Symuluj'",
                                  color = upwr_reference, size = 5) + theme_upwr())
    }
    df <- data.frame(phi_hat = res$ests)
    ggplot(df, aes(x = phi_hat)) +
      geom_histogram(fill = upwr_accent, color = upwr_bg, bins = 30, alpha = 0.85) +
      geom_vline(xintercept = res$phi_true, color = unname(upwr_cat["szalwia"]),
                 linewidth = 1.3, linetype = "dashed") +
      geom_vline(xintercept = mean(res$ests), color = unname(upwr_cat["terakota"]),
                 linewidth = 1.3) +
      annotate("text", x = res$phi_true, y = Inf, vjust = 1.4, hjust = -0.1,
               label = paste0("φ_true = ", res$phi_true),
               color = unname(upwr_cat["szalwia"]), fontface = "bold") +
      annotate("text", x = mean(res$ests), y = Inf, vjust = 2.8, hjust = -0.1,
               label = paste0("φ̂_avg = ", round(mean(res$ests), 3)),
               color = unname(upwr_cat["terakota"]), fontface = "bold") +
      labs(x = "Wyestymowane φ̂₁", y = "Liczba symulacji",
           title = paste0("Rozkład estymatorów (n=", res$n, ", ", nrow(df), " symulacji)")) +
      theme_upwr()
  }))

  output$ch8_est_stats <- renderUI({
    res <- ch8_est_results()
    if (is.null(res)) return(NULL)
    lc_stat_grid(
      lc_stat_box("φ_true",  res$phi_true,              color = unname(upwr_cat["szalwia"])),
      lc_stat_box("φ̂ średnia", round(mean(res$ests), 4), color = unname(upwr_cat["terakota"])),
      lc_stat_box("SD(φ̂)",  round(sd(res$ests), 4),    color = upwr_secondary),
      columns = 3
    )
  })
}
