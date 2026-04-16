# ============================================================================
# CHAPTER 2: Wartosc oczekiwana i wariancja
# ============================================================================

ch2_ev_var_ui <- tabPanel("2. Wart. oczekiwana i wariancja",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Wiemy ju\u017c, czym jest rozk\u0142ad prawdopodobie\u0144stwa. Teraz dwa kluczowe pytania:
       czego mo\u017cemy si\u0119 spodziewa\u0107 i jak bardzo wyniki mog\u0105 si\u0119 r\u00f3\u017cni\u0107?"
    ),

    div(class = "section-title", "Warto\u015b\u0107 oczekiwana i wariancja"),

    div(class = "narrative",
      p("Ka\u017cdy rozk\u0142ad prawdopodobie\u0144stwa mo\u017cna opisa\u0107 dwoma kluczowymi
        liczbami:"),
      tags$ul(
        tags$li(tags$b("Warto\u015b\u0107 oczekiwana E(X)"),
          " \u2014 'na co mog\u0119 liczy\u0107 w d\u0142u\u017cszej perspektywie?'"),
        tags$li(tags$b("Wariancja / odchylenie standardowe"),
          " \u2014 'jak bardzo wyniki rozpraszaj\u0105 si\u0119 wok\u00f3\u0142 oczekiwania?'")
      ),
      p("To rozszerzenie poj\u0119\u0107, kt\u00f3re ju\u017c znasz ze statystyki opisowej
        (\u015brednia i odchylenie standardowe), ale teraz stosujemy je do ",
        tags$b("modeli teoretycznych"), ", a nie do danych.")
    ),

    # ========================================================================
    # WIDGET 1: Loterie -- symulacja wartosci oczekiwanej
    # ========================================================================
    div(class = "section-title", "Czego si\u0119 spodziewa\u0107? \u2014 gra w loterie"),

    div(class = "narrative",
      p("Wyobra\u017a sobie, \u017ce mo\u017cesz gra\u0107 w jedn\u0105 z trzech loterii.
        Ka\u017cda ma inne wygrane i szanse. Kt\u00f3ra op\u0142aca si\u0119 najbardziej?"),
      p("Zagraj wiele razy i obserwuj, jak \u015brednia wygrana na gr\u0119
        stabilizuje si\u0119 \u2014 to w\u0142a\u015bnie ", tags$b("warto\u015b\u0107 oczekiwana"), ".")
    ),

    div(class = "widget-block",
      h4("Gra w loterie"),
      fluidRow(
        column(4,
          radioButtons("ch2ev_lottery", "Wybierz loteri\u0119:",
            choices = c(
              "A: 50% \u2192 10 z\u0142, 50% \u2192 0 z\u0142"     = "A",
              "B: 100% \u2192 4 z\u0142 (pewna)"              = "B",
              "C: 10% \u2192 100 z\u0142, 90% \u2192 0 z\u0142"    = "C",
              "D: 60% \u2192 8 z\u0142, 40% \u2192 \u22125 z\u0142"    = "D"
            ),
            selected = "A"
          ),
          hr(),
          div(style = "display: flex; flex-direction: column; gap: 8px;",
            actionButton("ch2ev_play_1", "Graj 1x",
                         class = "btn-primary", width = "100%"),
            actionButton("ch2ev_play_10", "Graj 10x",
                         class = "btn-primary", width = "100%"),
            actionButton("ch2ev_play_100", "Graj 100x",
                         class = "btn-primary", width = "100%"),
            actionButton("ch2ev_play_1000", "Graj 1000x",
                         class = "btn-warning", width = "100%"),
            hr(),
            actionButton("ch2ev_reset_lottery", "Reset",
                         class = "btn-outline-secondary", width = "100%")
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

    div(class = "callout-info",
      tags$strong("Warto\u015b\u0107 oczekiwana:"),
      " To 'd\u0142ugoterminowa \u015brednia' \u2014 wynik, wok\u00f3\u0142 kt\u00f3rego oscyluje
        \u015brednia po wielu powtorzeniach. Nie musi by\u0107 r\u00f3wna \u017cadnemu
        konkretnemu wynikowi! (np. E(kostki) = 3.5, cho\u0107 3.5 nigdy nie wypada)"
    ),

    # ========================================================================
    # WIDGET 2: Punkt rownowagi
    # ========================================================================
    div(class = "section-title", "E(X) jako punkt r\u00f3wnowagi"),

    div(class = "narrative",
      p("Warto\u015b\u0107 oczekiwana to ", tags$b("punkt r\u00f3wnowagi"),
        " rozk\u0142adu \u2014 gdyby\u015b po\u0142o\u017cy\u0142(a) s\u0142upki PMF na wadze,
        E(X) by\u0142oby miejscem podparcia."),
      p("Spr\u00f3buj ustawi\u0107 prawdopodobie\u0144stwa i obserwuj,
        jak przesuwa si\u0119 punkt r\u00f3wnowagi.")
    ),

    div(class = "widget-block",
      h4("Punkt r\u00f3wnowagi rozk\u0142adu"),
      fluidRow(
        column(4,
          h5("Prawdopodobie\u0144stwa:"),
          sliderInput("ch2ev_bal_p1", "P(X = 1):", min = 0, max = 1, value = 0.25, step = 0.01),
          sliderInput("ch2ev_bal_p2", "P(X = 3):", min = 0, max = 1, value = 0.25, step = 0.01),
          sliderInput("ch2ev_bal_p3", "P(X = 5):", min = 0, max = 1, value = 0.25, step = 0.01),
          sliderInput("ch2ev_bal_p4", "P(X = 9):", min = 0, max = 1, value = 0.25, step = 0.01),
          uiOutput("ch2ev_bal_sum"),
          hr(),
          div(class = "preset-buttons",
            actionButton("ch2ev_bal_sym", "Symetryczny",
                         class = "btn-outline-primary"),
            actionButton("ch2ev_bal_skew", "Sko\u015bny",
                         class = "btn-outline-warning"),
            actionButton("ch2ev_bal_bimod", "Dwumodalny",
                         class = "btn-outline-success")
          )
        ),
        column(8,
          plotOutput("ch2ev_balance_plot", height = "350px"),
          uiOutput("ch2ev_balance_text")
        )
      )
    ),

    div(class = "callout-warning",
      tags$strong("Pami\u0119taj:"),
      " E(X) to \u015brednia ", tags$b("wa\u017cona prawdopodobie\u0144stwami"),
      ". Wynik o du\u017cym prawdopodobie\u0144stwie ci\u0105gnie E(X) w swoj\u0105
        stron\u0119 \u2014 podobnie jak ci\u0119\u017cki przedmiot na wadze."
    ),

    # ========================================================================
    # WIDGET 3: Ryzyko a rozrzut -- intuicja wariancji
    # ========================================================================
    div(class = "section-title", "Wariancja \u2014 rozrzut wok\u00f3\u0142 oczekiwania"),

    div(class = "narrative",
      p("Dwie loterie mog\u0105 mie\u0107 ", tags$b("t\u0119 sam\u0105 warto\u015b\u0107 oczekiwan\u0105"),
        ", ale zupe\u0142nie r\u00f3\u017cne ryzyko. Wariancja (i odchylenie standardowe)
        mierzy w\u0142a\u015bnie to: ", tags$b("jak bardzo wyniki rozpraszaj\u0105 si\u0119"),
        " wok\u00f3\u0142 E(X)."),
      p("Por\u00f3wnaj trzy loterie \u2014 wszystkie maj\u0105 E(X) = 50 z\u0142:")
    ),

    div(class = "widget-block",
      h4("Trzy loterie, jedno E(X), r\u00f3\u017cne ryzyko"),
      fluidRow(
        column(4,
          sliderInput("ch2ev_var_n", "Ile razy zagra\u0107?",
                      min = 10, max = 2000, value = 200, step = 10),
          actionButton("ch2ev_var_sim", "Symuluj!",
                       class = "btn-primary", width = "100%"),
          br(), br(),
          uiOutput("ch2ev_var_summary")
        ),
        column(8,
          plotOutput("ch2ev_var_plot", height = "400px")
        )
      )
    ),

    div(class = "callout-success",
      tags$strong("Kluczowa intuicja:"),
      tags$ul(
        tags$li(tags$b("Ma\u0142a wariancja"), " = wyniki skupione blisko E(X), ma\u0142e ryzyko"),
        tags$li(tags$b("Du\u017ca wariancja"), " = wyniki rozrzucone szeroko, du\u017ce ryzyko"),
        tags$li(tags$b("Wariancja = 0"), " = brak losowo\u015bci, wynik pewny")
      ),
      p("Odchylenie standardowe SD = \u221aVar ma t\u0119 sam\u0105 jednostk\u0119 co dane
        (np. z\u0142ot\u00f3wki), dlatego jest \u0142atwiejsze w interpretacji.")
    ),

    # ========================================================================
    # Podsumowanie
    # ========================================================================
    div(class = "section-title", "Od danych do modelu"),

    div(class = "narrative",
      p("Zwr\u00f3\u0107 uwag\u0119 na analogi\u0119:"),
      tags$table(class = "table table-bordered", style = "font-size: 15px;",
        tags$thead(
          tags$tr(
            tags$th("Statystyka opisowa (dane)"),
            tags$th("Rachunek prawdopodobie\u0144stwa (model)")
          )
        ),
        tags$tbody(
          tags$tr(tags$td("\u015arednia z pr\u00f3by x\u0304"), tags$td("Warto\u015b\u0107 oczekiwana E(X)")),
          tags$tr(tags$td("Wariancja z pr\u00f3by s\u00b2"), tags$td("Wariancja Var(X)")),
          tags$tr(tags$td("Odchylenie standardowe s"), tags$td("Odchylenie standardowe SD(X)")),
          tags$tr(tags$td("Obliczane z danych"), tags$td("Obliczane z modelu (rozk\u0142adu)"))
        )
      ),
      p("Prawo wielkich liczb gwarantuje, \u017ce x\u0304 \u2192 E(X) wraz ze wzrostem pr\u00f3by.")
    ),

    # --- Transition ---
    div(class = "chapter-transition",
      p("Wiemy ju\u017c, czym s\u0105 warto\u015b\u0107 oczekiwana i wariancja.
        Teraz poznamy konkretne rozk\u0142ady dyskretne i zobaczymy,
        jak E(X) i Var(X) zale\u017c\u0105 od ich parametr\u00f3w."),
      actionButton("ch2ev_next", "Dalej: 3. Rozk\u0142ady dyskretne \u2192",
                   class = "btn-primary btn-lg")
    ),

    br(), br()
  ))
)

# --------------------------------------------------------------------------
# Chapter 2 Server
# --------------------------------------------------------------------------

ch2_ev_var_server <- function(input, output, session) {

  # --- Definicje loterii ---
  lottery_defs <- list(
    A = list(outcomes = c(10, 0), probs = c(0.5, 0.5), ev = 5,
             label = "A: 50/50 na 10 z\u0142 lub 0 z\u0142"),
    B = list(outcomes = c(4), probs = c(1), ev = 4,
             label = "B: Pewne 4 z\u0142"),
    C = list(outcomes = c(100, 0), probs = c(0.1, 0.9), ev = 10,
             label = "C: 10% na 100 z\u0142"),
    D = list(outcomes = c(8, -5), probs = c(0.6, 0.4), ev = 2.8,
             label = "D: 60% na 8 z\u0142, 40% na \u22125 z\u0142")
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
    div(class = "stat-box", style = paste0("background: ", col_primary, ";"),
        paste0("Gier: ", n))
  })

  output$ch2ev_convergence_plot <- renderPlot({
    results <- lottery_results()
    lot <- lottery_defs[[input$ch2ev_lottery]]

    if (length(results) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij 'Graj', aby rozpocz\u0105\u0107",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      # Srednia kroczaca
      running_mean <- cumsum(results) / seq_along(results)
      df <- data.frame(n = seq_along(results), mean = running_mean)

      ggplot(df, aes(x = n, y = mean)) +
        geom_line(color = col_primary, linewidth = 1) +
        geom_hline(yintercept = lot$ev, color = col_secondary,
                   linewidth = 1.2, linetype = "dashed") +
        annotate("text", x = max(df$n) * 0.95, y = lot$ev,
                 label = paste0("E(X) = ", lot$ev),
                 color = col_secondary, fontface = "bold", size = 5,
                 vjust = -1) +
        scale_y_continuous(limits = c(
          min(min(running_mean), lot$ev) - abs(lot$ev) * 0.3,
          max(max(running_mean), lot$ev) + abs(lot$ev) * 0.3
        )) +
        labs(title = "\u015arednia wygrana na gr\u0119 \u2192 E(X)",
             x = "Liczba gier", y = "\u015arednia wygrana (z\u0142)") +
        theme_educational()
    }
  })

  output$ch2ev_lottery_stats <- renderUI({
    results <- lottery_results()
    lot <- lottery_defs[[input$ch2ev_lottery]]
    req(length(results) > 0)

    obs_mean <- round(mean(results), 2)
    diff <- abs(obs_mean - lot$ev)

    div(style = "text-align: center; margin-top: 10px;",
      div(class = "stat-box", style = paste0("background: ", col_primary, ";"),
          paste0("\u015ar. dotychczasowa = ", obs_mean, " z\u0142")),
      div(class = "stat-box", style = paste0("background: ", col_secondary, ";"),
          paste0("E(X) = ", lot$ev, " z\u0142")),
      div(class = "stat-box",
          style = paste0("background: ", if (diff < 0.5) col_success else col_warning, ";"),
          paste0("R\u00f3\u017cnica: ", round(diff, 2), " z\u0142"))
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
      div(class = "stat-box", style = paste0("background: ", col_success, ";"),
          paste0("\u2211P = ", sprintf("%.2f", s), " \u2714"))
    } else {
      div(class = "stat-box", style = paste0("background: ", col_secondary, ";"),
          paste0("\u2211P = ", sprintf("%.2f", s), " \u2260 1"))
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
      geom_col(fill = col_primary, color = "white", alpha = 0.85, width = 0.6) +
      geom_text(aes(label = sprintf("%.2f", prob)), vjust = -0.5, size = 4.5) +
      # Os belki
      geom_segment(aes(x = 0, xend = 10, y = -0.01, yend = -0.01),
                   color = col_dark, linewidth = 1.5) +
      # Trojkat - punkt rownowagi
      annotate("point", x = ev, y = -0.03,
               shape = 17, size = 6, color = col_secondary) +
      annotate("text", x = ev, y = -0.06,
               label = paste0("E(X) = ", round(ev, 2)),
               color = col_secondary, fontface = "bold", size = 5) +
      scale_y_continuous(limits = c(-0.08, max(probs) * 1.3),
                         expand = expansion(mult = c(0, 0.05))) +
      scale_x_continuous(breaks = x_vals, limits = c(0, 10)) +
      labs(title = "Rozk\u0142ad na wadze \u2014 E(X) to punkt r\u00f3wnowagi",
           x = "Warto\u015b\u0107 (x)", y = "Prawdopodobie\u0144stwo P(X=x)") +
      theme_educational()
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
        paste0(x_vals[i], "\u00b7", sprintf("%.2f", probs[i]))
      }),
      collapse = " + "
    )

    div(class = "callout-info",
      tags$strong("Obliczenie: "),
      paste0("E(X) = ", calc_parts, " = ", round(ev, 2))
    )
  })

  # --- Widget 3: Ryzyko a rozrzut ---
  ch2ev_var_data <- reactiveVal(NULL)

  generate_var_sim <- function() {
    n <- input$ch2ev_var_n

    # Loteria A: zawsze 50 zl (Var = 0)
    a <- rep(50, n)
    # Loteria B: 50% szans na 0 zl, 50% na 100 zl (Var = 2500)
    b <- sample(c(0, 100), n, replace = TRUE)
    # Loteria C: rownomiernie 0-100 (Var ~ 833)
    cc <- runif(n, 0, 100)

    ch2ev_var_data(list(a = a, b = b, c = cc, n = n))
  }

  observeEvent(input$ch2ev_var_sim, generate_var_sim())

  # Inicjalizacja
  observe({
    if (is.null(ch2ev_var_data())) {
      isolate(generate_var_sim())
    }
  })

  output$ch2ev_var_plot <- renderPlot({
    d <- ch2ev_var_data()
    req(d)

    df <- data.frame(
      value = c(d$a, d$b, d$c),
      lottery = rep(c("A: Pewne 50 z\u0142\n(Var = 0)",
                       "B: 0 lub 100 z\u0142\n(Var = 2500)",
                       "C: Losowe 0-100 z\u0142\n(Var \u2248 833)"),
                    each = d$n)
    )
    df$lottery <- factor(df$lottery, levels = unique(df$lottery))

    ggplot(df, aes(x = value)) +
      geom_histogram(bins = 30, fill = col_primary, color = "white", alpha = 0.7) +
      geom_vline(xintercept = 50, color = col_secondary, linewidth = 1.2, linetype = "dashed") +
      facet_wrap(~lottery, ncol = 3) +
      annotate("text", x = 50, y = Inf, label = "E(X) = 50",
               color = col_secondary, fontface = "bold", size = 4, vjust = 2) +
      labs(title = paste0("Wyniki ", d$n, " gier \u2014 to samo E(X), r\u00f3\u017cne ryzyko"),
           x = "Wygrana (z\u0142)", y = "Liczebno\u015b\u0107") +
      theme_educational(base_size = 12)
  })

  output$ch2ev_var_summary <- renderUI({
    d <- ch2ev_var_data()
    req(d)

    div(
      h5("Statystyki:"),
      tags$table(class = "table table-bordered table-sm", style = "font-size: 13px;",
        tags$thead(tags$tr(tags$th("Loteria"), tags$th("SD"), tags$th("\u015ar."))),
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
