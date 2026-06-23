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
      inline_callout(label = "Chcesz więcej matematyki?", color = "wskazowka", open = FALSE,
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
          sliderInput("ch8_phi", "φ₁:", min = -0.99, max = 0.99, value = 0.8, step = 0.05),
          sliderInput("ch8_sigma", "Szum σ:", min = 0.2, max = 3, value = 1, step = 0.1),
          numericInput("ch8_n", "n:", value = 200, min = 50, max = 500, step = 50),
          actionButton("ch8_new", "Nowa realizacja", class = "lc-btn-primary", width = "100%"),
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
      label = "Ryc. 8.2", title = "Budowanie prognozy AR(1)",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Klikaj kroki, żeby zobaczyć jak buduje się prognoza."),
          sliderInput("ch8_fc_phi", "φ₁:", min = 0.5, max = 0.95, value = 0.8, step = 0.05),
          actionButton("ch8_fc_step1", "1. Dane historyczne",       class = "lc-btn-outline", width = "100%"),
          actionButton("ch8_fc_step2", "2. Prognoza t+1",           class = "lc-btn-outline", width = "100%"),
          actionButton("ch8_fc_step3", "3. Prognoza t+2, t+3, ...", class = "lc-btn-outline", width = "100%"),
          actionButton("ch8_fc_step4", "4. Zanik pamięci",          class = "lc-btn-outline", width = "100%"),
          uiOutput("ch8_fc_info")
        ),
        column(8,
          zoom_plot_ui("ch8_fc_plot", height = "300px")
        )
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
          sliderInput("ch8_est_phi", "Prawdziwe φ₁:", min = 0.3, max = 0.95, value = 0.75, step = 0.05),
          sliderInput("ch8_est_n",   "Rozmiar próby n:", min = 30, max = 500, value = 100, step = 10),
          numericInput("ch8_est_reps", "Liczba symulacji:", value = 500, min = 100, max = 2000, step = 100),
          actionButton("ch8_est_run", "Symuluj", class = "lc-btn-primary", width = "100%"),
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

  ch8_fc_step <- reactiveVal(0)
  observeEvent(input$ch8_fc_step1, ch8_fc_step(1))
  observeEvent(input$ch8_fc_step2, ch8_fc_step(2))
  observeEvent(input$ch8_fc_step3, ch8_fc_step(3))
  observeEvent(input$ch8_fc_step4, ch8_fc_step(4))

  ch8_fc_hist <- reactive({
    set.seed(77)
    phi <- if (!is.null(input$ch8_fc_phi)) input$ch8_fc_phi else 0.8
    as.numeric(arima.sim(list(ar = phi), n = 40, sd = 1))
  })

  zoom_plot_server("ch8_fc_plot", reactive({
    step <- ch8_fc_step()
    if (step == 0) {
      return(ggplot() + labs(title = "Klikaj kroki po lewej") +
               annotate("text", x = 0.5, y = 0.5, label = "Klikaj kroki po lewej",
                        color = upwr_reference, size = 6) + theme_upwr())
    }
    phi  <- if (!is.null(input$ch8_fc_phi)) input$ch8_fc_phi else 0.8
    hist <- ch8_fc_hist()
    n    <- length(hist)
    n_fc <- 12

    fc_vals <- numeric(n_fc)
    fc_vals[1] <- phi * hist[n]
    for (i in 2:n_fc) fc_vals[i] <- phi * fc_vals[i-1]

    df_hist <- data.frame(t = seq_len(n), x = hist, type = "Historia")
    df_fc   <- data.frame(t = n + seq_len(n_fc), x = fc_vals, type = "Prognoza")
    df_all  <- rbind(df_hist, df_fc)

    p <- ggplot(df_hist, aes(x = t, y = x)) +
      geom_line(color = upwr_secondary, linewidth = 0.9) +
      geom_hline(yintercept = 0, color = upwr_reference, linetype = "dashed") +
      labs(x = "Czas", y = "x_t") +
      theme_upwr()

    if (step >= 2) {
      p <- p + annotate("point", x = n + 1, y = fc_vals[1],
                        color = upwr_accent, size = 3.5)
      p <- p + annotate("segment",
                        x = n, xend = n + 1, y = hist[n], yend = fc_vals[1],
                        color = upwr_accent, linewidth = 1.2, linetype = "dashed")
      p <- p + annotate("text", x = n + 1.2, y = fc_vals[1] + 0.2,
                        label = paste0("x̂(t+1) = ", round(phi, 2), "·", round(hist[n], 2),
                                       " = ", round(fc_vals[1], 2)),
                        hjust = 0, color = upwr_accent, size = 3.5)
    }
    if (step >= 3) {
      fc_shown <- if (step == 3) seq_len(n_fc) else seq_len(n_fc)
      p <- p + geom_line(data = df_fc[fc_shown, ], aes(x = t, y = x),
                         color = upwr_accent, linewidth = 1.2, linetype = "dashed") +
               geom_point(data = df_fc[fc_shown, ], aes(x = t, y = x),
                          color = upwr_accent, size = 2.5)
    }
    if (step >= 4) {
      mean_line <- data.frame(t = (n + 1):(n + n_fc), y = 0)
      p <- p + geom_hline(yintercept = 0,
                          color = unname(upwr_cat["szalwia"]),
                          linewidth = 1.2, linetype = "longdash") +
               annotate("text", x = n + 2, y = 0.3,
                        label = "Prognoza → E[x] = 0", hjust = 0,
                        color = unname(upwr_cat["szalwia"]), fontface = "bold")
    }
    p
  }))

  output$ch8_fc_info <- renderUI({
    step <- ch8_fc_step()
    phi  <- if (!is.null(input$ch8_fc_phi)) input$ch8_fc_phi else 0.8
    msgs <- list(
      "0" = NULL,
      "1" = "Dane historyczne: ostatnia wartość x_t to punkt startowy.",
      "2" = paste0("Prognoza na 1 krok: x̂(t+1) = φ₁ · x_t = ", round(phi, 2), " · x_t."),
      "3" = paste0("Prognoza wielokrokowa: każdy kolejny krok iterujemy: x̂(t+k) = φ₁^k · x_t."),
      "4" = paste0("Zanik pamięci: przy φ₁ = ", phi,
                   " prognoza zmierza do 0 (średniej). Po ~",
                   ceiling(-3 / log10(phi)), " krokach jesteśmy blisko 0.")
    )
    if (!is.null(msgs[[as.character(step)]])) {
      lc_feedback(type = "info", p(msgs[[as.character(step)]]))
    }
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
