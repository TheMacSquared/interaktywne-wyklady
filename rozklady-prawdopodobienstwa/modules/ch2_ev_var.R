# ============================================================================
# CHAPTER 2: Wartosc oczekiwana i wariancja
# ============================================================================

ch2_ev_var_ui <- list(
  id = "ch-ev-var", num = "02", title = "Wartość oczekiwana i wariancja",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 02 · Rozkłady prawdopodobieństwa",
      num    = "02",
      title  = "Wartość oczekiwana i wariancja.",
      lead   = "Wiemy już, czym jest rozkład prawdopodobieństwa. Teraz dwa kluczowe pytania:
                czego możemy się spodziewać i jak bardzo wyniki mogą się różnić?"
    ),

    lc_h2("ch2-ev-intro", "Wartość oczekiwana i wariancja"),

    tagList(
      p("Każdy rozkład prawdopodobieństwa można opisać dwoma kluczowymi
        liczbami:"),
      tags$ul(
        tags$li(tags$b("Wartość oczekiwana E(X)"),
          " — 'na co mogę liczyć w dłuższej perspektywie?'"),
        tags$li(tags$b("Wariancja / odchylenie standardowe"),
          " — 'jak bardzo wyniki rozpraszają się wokół oczekiwania?'")
      ),
      p("To rozszerzenie pojęć, które już znasz ze statystyki opisowej
        (średnia i odchylenie standardowe), ale teraz stosujemy je do
        modeli teoretycznych, a nie do danych.")
    ),

    # ========================================================================
    # WIDGET 1: Loterie -- symulacja wartosci oczekiwanej
    # ========================================================================
    lc_h2("ch2-loterie", "Czego się spodziewać? — gra w loterie"),

    tagList(
      p("Wyobraź sobie, że możesz grać w jedną z trzech loterii.
        Każda ma inne wygrane i szanse. Która opłaca się najbardziej?"),
      p("Zagraj wiele razy i obserwuj, jak średnia wygrana na grę
        stabilizuje się — to właśnie wartość oczekiwana.")
    ),

    figure_panel(
      label = "Ryc. 2.1",
      title = "Gra w loterie",
      full_width = TRUE,
      fluidRow(
        column(4,
          radioButtons("ch2ev_lottery", "Wybierz loterię:",
            choices = c(
              "A: 50% → 10 zł, 50% → 0 zł"     = "A",
              "B: 100% → 4 zł (pewna)"              = "B",
              "C: 10% → 100 zł, 90% → 0 zł"    = "C",
              "D: 60% → 8 zł, 40% → −5 zł"    = "D"
            ),
            selected = "A"
          ),
          hr(),
          lc_stack(gap = "md",
            actionButton("ch2ev_play_1", "Graj 1x",
                         class = "lc-btn-primary", width = "100%"),
            actionButton("ch2ev_play_10", "Graj 10x",
                         class = "lc-btn-primary", width = "100%"),
            actionButton("ch2ev_play_100", "Graj 100x",
                         class = "lc-btn-primary", width = "100%"),
            actionButton("ch2ev_play_1000", "Graj 1000x",
                         class = "lc-btn-warning", width = "100%"),
            hr(),
            actionButton("ch2ev_reset_lottery", "Reset",
                         class = "lc-btn-secondary-outline", width = "100%")
          ),
          br(),
          uiOutput("ch2ev_play_count")
        ),
        column(8,
          plotOutput("ch2ev_convergence_plot", height = "300px"),
          uiOutput("ch2ev_lottery_stats")
        )
      )
    ),

    inline_callout(
      label = "Wartość oczekiwana",
      "To 'długoterminowa średnia' — wynik, wokół którego oscyluje
       średnia po wielu powtorzeniach. Nie musi być równa żadnemu
       konkretnemu wynikowi! (np. E(kostki) = 3.5, choć 3.5 nigdy nie wypada)"
    ),

    # ========================================================================
    # WIDGET 2: Punkt rownowagi
    # ========================================================================
    lc_h2("ch2-rownowaga", "E(X) jako punkt równowagi"),

    tagList(
      p("Wartość oczekiwana to punkt równowagi rozkładu — gdybyś położył(a)
        słupki PMF na wadze, E(X) byłoby miejscem podparcia."),
      p("Spróbuj ustawić prawdopodobieństwa i obserwuj,
        jak przesuwa się punkt równowagi.")
    ),

    figure_panel(
      label = "Ryc. 2.2",
      title = "Punkt równowagi rozkładu",
      full_width = TRUE,
      fluidRow(
        column(4,
          h5("Prawdopodobieństwa:"),
          sliderInput("ch2ev_bal_p1", "P(X = 1):", min = 0, max = 1, value = 0.25, step = 0.01),
          sliderInput("ch2ev_bal_p2", "P(X = 3):", min = 0, max = 1, value = 0.25, step = 0.01),
          sliderInput("ch2ev_bal_p3", "P(X = 5):", min = 0, max = 1, value = 0.25, step = 0.01),
          sliderInput("ch2ev_bal_p4", "P(X = 9):", min = 0, max = 1, value = 0.25, step = 0.01),
          uiOutput("ch2ev_bal_sum"),
          hr(),
          div(class = "preset-buttons",
            actionButton("ch2ev_bal_sym", "Symetryczny",
                         class = "lc-btn-outline"),
            actionButton("ch2ev_bal_skew", "Skośny",
                         class = "lc-btn-warning-outline"),
            actionButton("ch2ev_bal_bimod", "Dwumodalny",
                         class = "lc-btn-ok-outline")
          )
        ),
        column(8,
          plotOutput("ch2ev_balance_plot", height = "350px"),
          uiOutput("ch2ev_balance_text")
        )
      )
    ),

    inline_callout(
      label = "Pamiętaj",
      "E(X) to średnia ważona prawdopodobieństwami. Wynik o dużym prawdopodobieństwie
       ciągnie E(X) w swoją stronę — podobnie jak ciężki przedmiot na wadze.",
      color = "uwaga"
    ),

    # ========================================================================
    # WIDGET 3: Ryzyko a rozrzut -- intuicja wariancji
    # ========================================================================
    lc_h2("ch2-wariancja", "Wariancja — rozrzut wokół oczekiwania"),

    tagList(
      p("Dwie loterie mogą mieć tę samą wartość oczekiwaną, ale zupełnie różne ryzyko.
        Wariancja (i odchylenie standardowe) mierzy właśnie to: jak bardzo wyniki
        rozpraszają się wokół E(X)."),
      p("Porównaj trzy loterie — wszystkie mają E(X) = 50 zł:")
    ),

    figure_panel(
      label = "Ryc. 2.3",
      title = "Trzy loterie, jedno E(X), różne ryzyko",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch2ev_var_n", "Ile razy zagrać?",
                      min = 10, max = 2000, value = 200, step = 10),
          actionButton("ch2ev_var_sim", "Symuluj!",
                       class = "lc-btn-primary", width = "100%"),
          br(), br(),
          uiOutput("ch2ev_var_summary")
        ),
        column(8,
          plotOutput("ch2ev_var_plot", height = "400px")
        )
      )
    ),

    inline_callout(
      label = "Kluczowa intuicja",
      tagList(
        tags$ul(
          tags$li(tags$b("Mała wariancja"), " = wyniki skupione blisko E(X), małe ryzyko"),
          tags$li(tags$b("Duża wariancja"), " = wyniki rozrzucone szeroko, duże ryzyko"),
          tags$li(tags$b("Wariancja = 0"), " = brak losowości, wynik pewny")
        ),
        "SD = √Var ma tę samą jednostkę co dane — łatwiejsza w interpretacji."
      ),
      color = "ok"
    ),

    # ========================================================================
    # Podsumowanie
    # ========================================================================
    lc_h2("ch2-od-danych", "Od danych do modelu"),

    tagList(
      p("Zwróć uwagę na analogię:"),
      tags$table(class = "lc-table lc-table-bordered", style = "font-size: 15px;",
        tags$thead(
          tags$tr(
            tags$th("Statystyka opisowa (dane)"),
            tags$th("Rachunek prawdopodobieństwa (model)")
          )
        ),
        tags$tbody(
          tags$tr(tags$td("Średnia z próby x̄"), tags$td("Wartość oczekiwana E(X)")),
          tags$tr(tags$td("Wariancja z próby s²"), tags$td("Wariancja Var(X)")),
          tags$tr(tags$td("Odchylenie standardowe s"), tags$td("Odchylenie standardowe SD(X)")),
          tags$tr(tags$td("Obliczane z danych"), tags$td("Obliczane z modelu (rozkładu)"))
        )
      ),
      p("Prawo wielkich liczb gwarantuje, że x̄ → E(X) wraz ze wzrostem próby.")
    ),

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

  # --- Definicje loterii ---
  lottery_defs <- list(
    A = list(outcomes = c(10, 0), probs = c(0.5, 0.5), ev = 5,
             label = "A: 50/50 na 10 zł lub 0 zł"),
    B = list(outcomes = c(4), probs = c(1), ev = 4,
             label = "B: Pewne 4 zł"),
    C = list(outcomes = c(100, 0), probs = c(0.1, 0.9), ev = 10,
             label = "C: 10% na 100 zł"),
    D = list(outcomes = c(8, -5), probs = c(0.6, 0.4), ev = 2.8,
             label = "D: 60% na 8 zł, 40% na −5 zł")
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
    lc_stat_box("Gier", n, color = unname(upwr_cat["niebo"]))
  })

  output$ch2ev_convergence_plot <- renderPlot({
    results <- lottery_results()
    lot <- lottery_defs[[input$ch2ev_lottery]]

    if (length(results) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij 'Graj', aby rozpocząć",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      # Srednia kroczaca
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
        labs(title = "Średnia wygrana na grę → E(X)",
             x = "Liczba gier", y = "Średnia wygrana (zł)") +
        theme_upwr()
    }
  })

  output$ch2ev_lottery_stats <- renderUI({
    results <- lottery_results()
    lot <- lottery_defs[[input$ch2ev_lottery]]
    req(length(results) > 0)

    obs_mean <- round(mean(results), 2)
    diff <- abs(obs_mean - lot$ev)

    lc_center(
      lc_stat_box("Śr. dotychczasowa", obs_mean, " zł",
                  color = unname(upwr_cat["niebo"])),
      lc_stat_box("E(X)", lot$ev, " zł",
                  color = unname(upwr_cat["terakota"])),
      lc_stat_box("Różnica", round(diff, 2), " zł",
                  color = if (diff < 0.5) unname(upwr_cat["szalwia"]) else unname(upwr_cat["bursztyn"]))
    )
  })

  # --- Widget 2: Punkt rownowagi ---
  observeEvent(input$ch2ev_bal_sym, {
    updateSliderInput(session, "ch2ev_bal_p1", value = 0.25)
    updateSliderInput(session, "ch2ev_bal_p2", value = 0.25)
    updateSliderInput(session, "ch2ev_bal_p3", value = 0.25)
    updateSliderInput(session, "ch2ev_bal_p4", value = 0.25)
  })
  observeEvent(input$ch2ev_bal_skew, {
    updateSliderInput(session, "ch2ev_bal_p1", value = 0.05)
    updateSliderInput(session, "ch2ev_bal_p2", value = 0.15)
    updateSliderInput(session, "ch2ev_bal_p3", value = 0.30)
    updateSliderInput(session, "ch2ev_bal_p4", value = 0.50)
  })
  observeEvent(input$ch2ev_bal_bimod, {
    updateSliderInput(session, "ch2ev_bal_p1", value = 0.40)
    updateSliderInput(session, "ch2ev_bal_p2", value = 0.10)
    updateSliderInput(session, "ch2ev_bal_p3", value = 0.10)
    updateSliderInput(session, "ch2ev_bal_p4", value = 0.40)
  })

  output$ch2ev_bal_sum <- renderUI({
    s <- input$ch2ev_bal_p1 + input$ch2ev_bal_p2 + input$ch2ev_bal_p3 + input$ch2ev_bal_p4
    if (abs(s - 1) < 0.005) {
      lc_stat_box("∑P", sprintf("%.2f", s), " ✔", color = unname(upwr_cat["szalwia"]))
    } else {
      lc_stat_box("∑P", sprintf("%.2f", s), " ≠ 1", color = unname(upwr_cat["terakota"]))
    }
  })

  output$ch2ev_balance_plot <- renderPlot({
    x_vals <- c(1, 3, 5, 9)
    probs <- c(input$ch2ev_bal_p1, input$ch2ev_bal_p2,
               input$ch2ev_bal_p3, input$ch2ev_bal_p4)
    s <- sum(probs)
    if (abs(s - 1) > 0.05) probs <- probs / s  # normalizuj

    ev <- sum(x_vals * probs)

    df <- data.frame(x = x_vals, prob = probs)

    ggplot(df, aes(x = x, y = prob)) +
      geom_col(fill = unname(upwr_cat["niebo"]), color = "white", alpha = 0.85, width = 0.6) +
      geom_text(aes(label = sprintf("%.2f", prob)), vjust = -0.5, size = 4.5) +
      # Os belki
      geom_segment(aes(x = 0, xend = 10, y = -0.01, yend = -0.01),
                   color = upwr_secondary, linewidth = 1.5) +
      # Trojkat - punkt rownowagi
      annotate("point", x = ev, y = -0.03,
               shape = 17, size = 6, color = unname(upwr_cat["terakota"])) +
      annotate("text", x = ev, y = -0.06,
               label = paste0("E(X) = ", round(ev, 2)),
               color = unname(upwr_cat["terakota"]), fontface = "bold", size = 5) +
      scale_y_continuous(limits = c(-0.08, max(probs) * 1.3),
                         expand = expansion(mult = c(0, 0.05))) +
      scale_x_continuous(breaks = x_vals, limits = c(0, 10)) +
      labs(title = "Rozkład na wadze — E(X) to punkt równowagi",
           x = "Wartość (x)", y = "Prawdopodobieństwo P(X=x)") +
      theme_upwr()
  })

  output$ch2ev_balance_text <- renderUI({
    x_vals <- c(1, 3, 5, 9)
    probs <- c(input$ch2ev_bal_p1, input$ch2ev_bal_p2,
               input$ch2ev_bal_p3, input$ch2ev_bal_p4)
    s <- sum(probs)
    if (abs(s - 1) > 0.05) probs <- probs / s
    ev <- sum(x_vals * probs)

    calc_parts <- paste(
      sapply(seq_along(x_vals), function(i) {
        paste0(x_vals[i], "·", sprintf("%.2f", probs[i]))
      }),
      collapse = " + "
    )

    lc_feedback(type = "info",
      tags$strong("Obliczenie: "),
      paste0("E(X) = ", calc_parts, " = ", round(ev, 2))
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

  output$ch2ev_var_plot <- renderPlot({
    d <- ch2ev_var_data()

    df <- data.frame(
      value = c(d$a, d$b, d$c),
      lottery = rep(c("A: Pewne 50 zł\n(Var = 0)",
                       "B: 0 lub 100 zł\n(Var = 2500)",
                       "C: Losowe 0-100 zł\n(Var ≈ 833)"),
                    each = d$n)
    )
    df$lottery <- factor(df$lottery, levels = unique(df$lottery))

    ggplot(df, aes(x = value)) +
      geom_histogram(bins = 30, fill = unname(upwr_cat["niebo"]), color = "white", alpha = 0.7) +
      geom_vline(xintercept = 50, color = unname(upwr_cat["terakota"]), linewidth = 1.2, linetype = "dashed") +
      facet_wrap(~lottery, ncol = 3) +
      annotate("text", x = 50, y = Inf, label = "E(X) = 50",
               color = unname(upwr_cat["terakota"]), fontface = "bold", size = 4, vjust = 2) +
      labs(title = paste0("Wyniki ", d$n, " gier — to samo E(X), różne ryzyko"),
           x = "Wygrana (zł)", y = "Liczebność") +
      theme_upwr(base_size = 12)
  })

  output$ch2ev_var_summary <- renderUI({
    d <- ch2ev_var_data()

    div(
      h5("Statystyki:"),
      tags$table(class = "lc-table lc-table-bordered lc-table-sm", style = "font-size: 13px;",
        tags$thead(tags$tr(tags$th("Loteria"), tags$th("SD"), tags$th("Śr."))),
        tags$tbody(
          tags$tr(
            tags$td("A: Pewna"),
            tags$td(round(sd(d$a), 1)),
            tags$td(round(mean(d$a), 1))
          ),
          tags$tr(
            tags$td("B: 0/100"),
            tags$td(round(sd(d$b), 1)),
            tags$td(round(mean(d$b), 1))
          ),
          tags$tr(
            tags$td("C: Losowa"),
            tags$td(round(sd(d$c), 1)),
            tags$td(round(mean(d$c), 1))
          )
        )
      )
    )
  })

}
