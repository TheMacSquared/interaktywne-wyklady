# ============================================================================
# CHAPTER 5: Co wplywa na szerokosc przedzialu?
# ============================================================================

ch5_ui <- list(
  id    = "ch-czynniki",
  num   = "05",
  title = "Co wpływa na szerokość?",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 05 · Przedziały ufności",
      num    = "05",
      title  = "Co wpływa na szerokość przedziału?",
      lead   = "Umiemy już budować przedziały dla średniej i proporcji.
                Teraz zbadamy, co decyduje o ich precyzji."
    ),

    lc_h2("ch5-czynniki", "Trzy czynniki szerokości przedziału"),

    tagList(
      p("Margines błędu (a więc szerokość przedziału) zależy od trzech rzeczy:"),
      lc_formula_box(
        withMathJax(helpText(
          "$$ME = t^* \\cdot \\frac{s}{\\sqrt{n}}$$"
        ))
      ),
      tags$ol(
        tags$li(tags$b("Wielkość próby (n)"), " — więcej danych = węższy przedział"),
        tags$li(tags$b("Poziom ufności"), " — większa pewność = szerszy przedział"),
        tags$li(tags$b("Zmienność danych (s)"), " — większe rozproszenie = szerszy przedział")
      )
    ),

    lc_h2("ch5-eksploracja", "Interaktywna eksploracja"),

    figure_panel(
      label = "Ryc. 5.1", title = "Jak zmienia się szerokość przedziału?",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch5_n", "Wielkość próby (n):",
                      min = 5, max = 100, value = 30, step = 1),
          sliderInput("ch5_conf", "Poziom ufności:",
                      min = 0.80, max = 0.99, value = 0.95, step = 0.01),
          sliderInput("ch5_s", "Odchylenie std. (s):",
                      min = 1, max = 12, value = 8, step = 1),
          hr(),
          uiOutput("ch5_me_display")
        ),
        column(8,
          plotOutput("ch5_factors_plot", height = "480px")
        )
      )
    ),

    inline_callout(label = "Malejące korzyści", color = "wskazowka",
      "Zwiększenie n z 25 do 100 (4×) skraca przedział o połowę (2×).
       Ale z 100 do 400 (4×) też tylko o połowę. To efekt 1/√n."
    ),

    lc_h2("ch5-planowanie", "Planowanie wielkości próby"),

    tagList(
      p("Odwróćmy pytanie: ile obserwacji potrzebuję,
        żeby margines błędu był nie większy niż zakładany?"),
      lc_formula_box(
        withMathJax(helpText(
          "$$n = \\left(\\frac{z^* \\cdot s}{ME_{\\text{max}}}\\right)^2$$"
        ))
      )
    ),

    figure_panel(
      label = "Ryc. 5.2", title = "Kalkulator wielkości próby",
      full_width = TRUE,
      fluidRow(
        column(4,
          numericInput("ch5_plan_me", "Pożądany margines błędu:",
                       value = 2, min = 0.1, step = 0.1),
          numericInput("ch5_plan_s", "Spodziewane s:",
                       value = 10, min = 0.1, step = 0.5),
          sliderInput("ch5_plan_conf", "Poziom ufności:",
                      min = 0.80, max = 0.99, value = 0.95, step = 0.01)
        ),
        column(8,
          uiOutput("ch5_plan_result"),
          plotOutput("ch5_plan_plot", height = "440px")
        )
      )
    ),

    lc_h2("ch5-porownanie", "90% vs 95% vs 99%"),

    tagList(
      p("Zobaczmy jak wyglądają trzy przedziały z tych samych danych,
        ale przy różnych poziomach ufności.")
    ),

    figure_panel(
      label = "Ryc. 5.3", title = "Trzy poziomy ufności",
      full_width = TRUE,
      fluidRow(
        column(4,
          selectInput("ch5_cmp_data", "Dane:",
            choices = list(
              "Przykłady ogólne" = c(
                "Wzrost studentów (n=30)" = "height",
                "Czas dojazdu (n=50)" = "commute",
                "Oceny z egzaminu (n=40)" = "grades"
              ),
              "Dane kierunkowe" = c(
                "IB: wskaźnik wypadków (n=320)" = "ib_wypadki",
                "ROL: plon pszenicy (n=280)" = "rol_plon",
                "TZ: zawartość białka (n=350)" = "tz_bialko"
              )
            ),
            selected = "height"
          ),
          actionButton("ch5_cmp_calc", "Oblicz 3 przedziały",
                       class = "lc-btn-primary", width = "100%"),
          br(), br(),
          uiOutput("ch5_cmp_stats")
        ),
        column(8,
          plotOutput("ch5_cmp_plot", height = "250px")
        )
      )
    ),

    inline_callout(label = "Kompromis", color = "ok",
      "95% to standardowy wybór — rozsądna równowaga między pewnością
       a precyzją. 99% daje szerszy przedział (większa pewność, mniejsza
       precyzja), 90% węższy (mniej pewny, bardziej precyzyjny)."
    ),

    lc_h2("ch5-edge-case", "Edge case: kiedy poziom ufności zmienia wniosek"),

    tagList(
      p("Czasami ten sam zbiór danych pozwala stwierdzić hipotezę
        przy 90% ufności, a nie pozwala przy 95%. To jest często nieintuicyjne —
        student myśli, że skoro ", tags$em("p̂ jest powyżej granicy"),
        ", to wniosek jest oczywisty. Nie jest. Liczy się cały przedział
        względem granicy hipotezy, a szerokość przedziału zależy od poziomu ufności."),
      p("Poniżej trzy case'y. W każdym kliknij ", tags$b("90%"), ", ", tags$b("95%"),
        " i ", tags$b("99%"), " i obserwuj, jak werdykt się zmienia.")
    ),

    tags$details(class = "case-study", open = NA,
      tags$summary(
        span(class = "case-icon", "\U0001f697"),
        "Edge 1. Czas dojazdu — czy średni czas przekracza 26 min?"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Zmierzono czas dojazdu dla 40 pracowników. Średnia z próby ",
            withMathJax("\\(\\bar{x} = 28.5\\)"), " min,
            odchylenie standardowe ", withMathJax("\\(s = 8\\)"), " min.
            Hipoteza: średni czas dojazdu w populacji przekracza 26 min.")
        ),
        uiOutput("ch5_edge1_buttons"),
        plotOutput("ch5_edge1_plot", height = "240px"),
        uiOutput("ch5_edge1_explain")
      )
    ),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f5f3️"),
        "Edge 2. Sondaż — czy poparcie przekracza 50%?"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Pracownia sondażowa zapytała 1000 wyborców, czy poprze partię X.
            540 odpowiedzi TAK (", withMathJax("\\(\\hat{p} = 0.54\\)"), ").
            Hipoteza: poparcie w populacji przekracza próg 50%.")
        ),
        uiOutput("ch5_edge2_buttons"),
        plotOutput("ch5_edge2_plot", height = "240px"),
        uiOutput("ch5_edge2_explain")
      )
    ),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f4d8"),
        "Edge 3. Wynik szkolenia — czy średnia przekracza 65 pkt?"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("W szkoleniu BHP 20 pracowników uzyskało średni wynik ",
            withMathJax("\\(\\bar{x} = 68\\)"), " pkt
            (na 100), ", withMathJax("\\(s = 10\\)"), " pkt.
            Hipoteza: średni wynik w populacji przekracza próg 65 pkt.")
        ),
        uiOutput("ch5_edge3_buttons"),
        plotOutput("ch5_edge3_plot", height = "240px"),
        uiOutput("ch5_edge3_explain")
      )
    ),

    tagList(
      p(tags$strong("Dlaczego to jest nieintuicyjne?"),
        " Bo w codziennym myśleniu nie odróżniamy 95% od 93% — dla nas
        jest „dużo”, „średnio”, „mało”. Statystyka pozwala na precyzyjne
        kwantyfikowanie pewności i to jest jej moc, nie wada.
        Stwierdzenie ",
        tags$em("„nie możemy być pewni z 95%, ale możemy z 93%”"),
        " nie jest sprzecznością — to jest dokładnie ten poziom precyzji,
        do którego służy ten aparat matematyczny."),
      p(tags$strong("W praktyce:"),
        " 95% to umowny standard. Jeśli wiesz, że ", tags$em("Twój problem"),
        " toleruje więcej ryzyka (np. wstępna eksploracja, niskie koszty
        błędu), możesz legalnie użyć 90%. Jeśli mniej (np. badania
        medyczne, kontrola jakości), użyj 99%. Ważne jest tylko, żeby
        poziom ufności wybrać zanim spojrzysz na wyniki — i potem ten
        wybór jasno raportować.")
    ),

    lc_chapter_next(
      num       = "06",
      title     = "Ściąga",
      lead      = "podsumowanie wzorów i zasad",
      target_id = "ch-sciaga"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch5_server <- function(input, output, session) {

  # --- Widget 1: Trzy suwaki ---
  output$ch5_factors_plot <- renderPlot({
    n <- input$ch5_n
    conf <- input$ch5_conf
    s <- input$ch5_s
    t_star <- qt(1 - (1 - conf) / 2, df = n - 1)
    me <- t_star * s / sqrt(n)
    xbar <- 170  # arbitralny srodek (np. wzrost)

    # ---- GORNY PANEL: krzywa ME(n) ----
    n_seq <- seq(5, 100, by = 1)
    me_seq <- qt(1 - (1 - conf) / 2, df = pmax(n_seq - 1, 1)) * s / sqrt(n_seq)
    df <- data.frame(n = n_seq, me = me_seq)

    p_top <- ggplot(df, aes(x = n, y = me)) +
      geom_line(color = col_ci, linewidth = 1.2) +
      geom_point(aes(x = !!n, y = !!me), color = col_estimate, size = 4) +
      geom_hline(yintercept = me, color = col_estimate, linetype = "dotted") +
      annotate("text", x = n + 4, y = me + 0.3,
               label = paste0("ME = ", round(me, 2)),
               color = col_estimate, fontface = "bold", size = 4.5) +
      labs(title = paste0("Margines błędu w funkcji n ",
                          "(", round(conf * 100), "% CI, s = ", s, ")"),
           x = "Wielkość próby (n)",
           y = "Margines błędu (ME)") +
      theme_upwr()

    # ---- DOLNY PANEL: sam pasek CI na fixed osi X ----
    # Worst-case ME (n=5, conf=0.99, s=12) -> ustala stale granice osi X
    max_me_worst <- qt(0.995, df = 4) * 12 / sqrt(5)
    xlims <- c(xbar - max_me_worst * 1.05, xbar + max_me_worst * 1.05)

    p_bot <- ggplot() +
      xlim(xlims) +
      ylim(-0.6, 0.6) +
      labs(x = "Wartość (np. wzrost w cm)", y = NULL,
           title = "Twój 95% CI na stałej osi") +
      theme_upwr() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      geom_vline(xintercept = xbar, color = upwr_reference,
                 linetype = "dashed", linewidth = 0.6) +
      annotate("text", x = xbar, y = 0.5, label = paste0("środek = ", xbar),
               color = upwr_reference, size = 4, hjust = -0.1) +
      geom_point(aes(x = xbar, y = 0), color = col_estimate,
                 size = 7, shape = 18) +
      geom_errorbarh(aes(xmin = xbar - me, xmax = xbar + me, y = 0),
                     height = 0.18, color = col_ci, linewidth = 2.4, alpha = 0.7) +
      annotate("text", x = xbar, y = -0.42,
               label = paste0("CI: [", round(xbar - me, 2),
                              " ; ", round(xbar + me, 2), "]    szer. = ",
                              round(2 * me, 2)),
               color = col_ci, fontface = "bold", size = 4.8)

    library(patchwork)
    (p_top / p_bot) + plot_layout(heights = c(2, 1))
  })

  output$ch5_me_display <- renderUI({
    n <- input$ch5_n
    conf <- input$ch5_conf
    s <- input$ch5_s
    t_star <- qt(1 - (1 - conf) / 2, df = n - 1)
    me <- t_star * s / sqrt(n)
    width <- 2 * me

    tagList(
      lc_stat_box("ME", round(me, 2), color = col_ci),
      lc_stat_box("Szer.", round(width, 2), color = upwr_secondary),
      lc_stat_box("t*", round(t_star, 3), color = col_estimate)
    )
  })

  # --- Widget 2: Planowanie n ---
  output$ch5_plan_result <- renderUI({
    me_max <- input$ch5_plan_me
    s <- input$ch5_plan_s
    conf <- input$ch5_plan_conf
    z_star <- qnorm(1 - (1 - conf) / 2)
    n_req <- ceiling((z_star * s / me_max)^2)

    lc_feedback(type = "ok",
      p(tags$strong("Wymagana wielkość próby:")),
      p(withMathJax(paste0(
        "\\(n = \\left(\\frac{", round(z_star, 3), " \\cdot ", s, "}{",
        me_max, "}\\right)^2 = ", round((z_star * s / me_max)^2, 1),
        " \\approx \\mathbf{", n_req, "}\\)"
      )))
    )
  })

  output$ch5_plan_plot <- renderPlot({
    me_max <- input$ch5_plan_me
    s <- input$ch5_plan_s
    conf <- input$ch5_plan_conf
    z_star <- qnorm(1 - (1 - conf) / 2)
    n_req <- ceiling((z_star * s / me_max)^2)
    me_actual <- z_star * s / sqrt(n_req)  # ME osiagniete przy n_req (zwykle ~ me_max)
    center <- 100  # arbitralny srodek

    # ---- GORNY PANEL: krzywa ME vs n ----
    n_seq <- seq(5, max(n_req * 2, 100), by = 1)
    me_seq <- z_star * s / sqrt(n_seq)
    df <- data.frame(n = n_seq, me = me_seq)

    p_top <- ggplot(df, aes(x = n, y = me)) +
      geom_line(color = col_ci, linewidth = 1.2) +
      geom_hline(yintercept = me_max, color = col_miss, linetype = "dashed",
                 linewidth = 1) +
      geom_point(aes(x = n_req, y = me_max), color = col_hit, size = 5) +
      annotate("text", x = n_req, y = me_max + 0.3,
               label = paste0("n = ", n_req),
               color = col_hit, fontface = "bold", size = 5) +
      labs(title = "Margines błędu vs wielkość próby",
           x = "n", y = "Margines błędu") +
      theme_upwr()

    # ---- DOLNY PANEL: pasek CI przy n_req, z dopuszczalna strefa ----
    xlims <- c(center - 3 * me_max, center + 3 * me_max)

    p_bot <- ggplot() +
      xlim(xlims) +
      ylim(-0.6, 0.6) +
      labs(x = "Wartość (jednostki dowolne)", y = NULL,
           title = paste0("CI przy n = ", n_req,
                          "  —  szara strefa = dopuszczalny ME = ±", me_max)) +
      theme_upwr() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      annotate("rect",
               xmin = center - me_max, xmax = center + me_max,
               ymin = -Inf, ymax = Inf,
               fill = upwr_rule, alpha = 0.4) +
      geom_vline(xintercept = center, color = upwr_reference,
                 linetype = "dashed", linewidth = 0.6) +
      geom_point(aes(x = center, y = 0), color = col_estimate,
                 size = 7, shape = 18) +
      geom_errorbarh(aes(xmin = center - me_actual, xmax = center + me_actual, y = 0),
                     height = 0.18, color = col_hit, linewidth = 2.4, alpha = 0.8) +
      annotate("text", x = center, y = -0.42,
               label = paste0("Osiągnięte ME = ±", round(me_actual, 3),
                              "  ≤  ", me_max, " ✓"),
               color = col_hit, fontface = "bold", size = 4.8)

    library(patchwork)
    (p_top / p_bot) + plot_layout(heights = c(2, 1))
  })

  # --- Widget 3: Porownanie 90/95/99 ---
  ch5_cmp_data <- reactiveVal(NULL)

  observeEvent(input$ch5_cmp_calc, {
    set.seed(42)
    samp <- switch(input$ch5_cmp_data,
      "height"     = rnorm(30, mean = 170, sd = 10),
      "commute"    = rgamma(50, shape = 4, scale = 7.5),
      "grades"     = pmin(pmax(rnorm(40, mean = 3.5, sd = 0.7), 2), 5),
      "ib_wypadki" = read.csv("dane/bhp_zaklady.csv")$wskaznik_wypadkow,
      "rol_plon"   = read.csv("dane/rolnictwo_pola.csv")$plon_pszenicy,
      "tz_bialko"  = read.csv("dane/zywnosc_partie.csv")$zawartosc_bialka
    )
    xbar <- mean(samp)
    s <- sd(samp)
    n <- length(samp)

    levels <- c(0.90, 0.95, 0.99)
    results <- lapply(levels, function(conf) {
      t_star <- qt(1 - (1 - conf) / 2, df = n - 1)
      me <- t_star * s / sqrt(n)
      data.frame(
        conf = paste0(conf * 100, "%"),
        xbar = xbar, lower = xbar - me, upper = xbar + me,
        me = me, width = 2 * me
      )
    })
    ch5_cmp_data(do.call(rbind, results))
  })

  output$ch5_cmp_plot <- renderPlot({
    df <- ch5_cmp_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Oblicz'",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      df$y <- c(3, 2, 1)
      colors <- c(col_estimate, col_ci, col_true)

      ggplot(df, aes(y = y)) +
        geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.3,
                       color = colors, linewidth = 2) +
        geom_point(aes(x = xbar), color = col_estimate, size = 4, shape = 18) +
        scale_y_continuous(breaks = c(1, 2, 3),
                           labels = c("99%", "95%", "90%")) +
        annotate("text", x = df$upper + 0.1, y = df$y,
                 label = paste0("[", round(df$lower, 2), " ; ",
                                round(df$upper, 2), "]"),
                 hjust = 0, size = 4) +
        labs(title = "Ten sam zbiór — trzy poziomy ufności",
             x = "Wartość", y = "Poziom ufności") +
        theme_upwr()
    }
  })

  output$ch5_cmp_stats <- renderUI({
    df <- ch5_cmp_data()
    if (is.null(df)) return(NULL)
    tagList(
      lapply(1:3, function(i) {
        lc_stat_box(df$conf[i], "±", round(df$me[i], 2),
                    color = c(col_estimate, col_ci, col_true)[i])
      })
    )
  })

  # ==========================================================================
  # WIDGET 4: Edge case'y - poziom ufnosci zmienia werdykt
  # ==========================================================================
  col_hyp <- "#8e44ad"

  # ---- Helpery ----
  ci_mean_local <- function(xbar, s, n, conf) {
    t_star <- qt(1 - (1 - conf) / 2, df = n - 1)
    me <- t_star * s / sqrt(n)
    list(lower = xbar - me, upper = xbar + me, me = me, t_star = t_star)
  }
  ci_prop_local <- function(x, n, conf) {
    phat <- x / n
    z_star <- qnorm(1 - (1 - conf) / 2)
    se <- sqrt(phat * (1 - phat) / n)
    me <- z_star * se
    list(phat = phat, lower = phat - me, upper = phat + me, me = me, z_star = z_star)
  }
  hypothesis_verdict_edge <- function(lower, upper, bound, dir) {
    if (dir == "gt") {
      if (lower > bound)      "yes"
      else if (upper < bound) "no"
      else                    "maybe"
    } else {
      if (upper < bound)      "yes"
      else if (lower > bound) "no"
      else                    "maybe"
    }
  }
  verdict_class_edge <- function(v) {
    switch(v, "yes" = "ok", "no" = "danger",
           "maybe" = "warning")
  }
  verdict_label_edge <- function(v) {
    switch(v, "yes" = "TAK", "no" = "NIE", "maybe" = "NIEPEWNE")
  }

  # ---- Konfiguracja edge case'ow ----
  edge_cases <- list(
    edge1 = list(
      kind = "mean",
      data = list(xbar = 28.5, s = 8, n = 40),
      hypothesis = list(text = "Średni czas dojazdu przekracza 26 min",
                        bound = 26, dir = "gt"),
      xlab = "Średni czas dojazdu (min)"
    ),
    edge2 = list(
      kind = "prop",
      data = list(x = 540, n = 1000),
      hypothesis = list(text = "Poparcie dla partii X przekracza 50%",
                        bound = 0.50, dir = "gt"),
      xlab = "Poparcie dla partii X"
    ),
    edge3 = list(
      kind = "mean",
      data = list(xbar = 68, s = 10, n = 20),
      hypothesis = list(text = "Średni wynik szkolenia przekracza 65 pkt",
                        bound = 65, dir = "gt"),
      xlab = "Średni wynik (pkt)"
    )
  )

  # State per case: lista (conf, revealed)
  #   conf:     NA (nic nie wybrane) lub 0.90 / 0.95 / 0.99
  #   revealed: FALSE (tylko CI + treść hipotezy) / TRUE (z werdyktem)
  ch5_edge_state <- reactiveValues()
  for (cid in names(edge_cases)) {
    ch5_edge_state[[cid]] <- list(conf = NA_real_, revealed = FALSE)
  }

  # ---- Compute CI for given case at given conf ----
  compute_edge_ci <- function(case_id, conf) {
    cfg <- edge_cases[[case_id]]
    if (cfg$kind == "mean") {
      ci <- ci_mean_local(cfg$data$xbar, cfg$data$s, cfg$data$n, conf)
      list(center = cfg$data$xbar, lower = ci$lower, upper = ci$upper, me = ci$me)
    } else {
      ci <- ci_prop_local(cfg$data$x, cfg$data$n, conf)
      list(center = ci$phat, lower = ci$lower, upper = ci$upper, me = ci$me)
    }
  }

  # ---- Generator przyciskow conf level + reveal ----
  edge_buttons_ui <- function(case_id) {
    state <- ch5_edge_state[[case_id]]
    current_conf <- state$conf
    revealed <- state$revealed
    levels <- c(0.90, 0.95, 0.99)
    btns <- lapply(levels, function(lv) {
      is_active <- !is.na(current_conf) && abs(current_conf - lv) < 1e-9
      btn_class <- if (is_active) "lc-btn-warning" else "lc-btn-warning-outline"
      actionButton(paste0("ch5_", case_id, "_conf", round(lv * 100)),
                   paste0(round(lv * 100), "%"), class = btn_class)
    })

    # Drugi rzad: przycisk "Pokaz werdykt" - tylko gdy conf wybrany i jeszcze nie odkryty
    reveal_row <- if (!is.na(current_conf) && !revealed) {
      div(class = "step-buttons lc-mt-xs",
        actionButton(paste0("ch5_", case_id, "_reveal"),
                     "\U0001f50d Pokaż werdykt", class = "lc-btn-ok"))
    } else {
      NULL
    }

    tagList(
      div(class = "step-buttons", btns),
      reveal_row
    )
  }

  # ---- Plot dla edge case'a (jeden panel: pasek CI + obszar hipotezy) ----
  render_edge_plot <- function(case_id) {
    cfg <- edge_cases[[case_id]]
    conf <- ch5_edge_state[[case_id]]$conf

    # Najszerszy mozliwy CI (przy 99%) -> ustala stale granice osi X
    ci_max <- compute_edge_ci(case_id, 0.99)
    ci_min <- compute_edge_ci(case_id, 0.90)
    bound <- cfg$hypothesis$bound
    center <- ci_max$center

    # Zakres X obejmujacy wszystkie 3 poziomy CI + bound + troche marginesu
    xrange <- range(c(ci_max$lower, ci_max$upper, bound))
    pad <- diff(xrange) * 0.20
    xlims <- c(xrange[1] - pad, xrange[2] + pad)

    p <- ggplot() +
      xlim(xlims) +
      ylim(-0.65, 0.65) +
      labs(x = cfg$xlab, y = NULL) +
      theme_upwr() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    # Obszar hipotezy (zawsze widoczny)
    if (cfg$hypothesis$dir == "gt") {
      p <- p + annotate("rect",
                        xmin = bound, xmax = Inf,
                        ymin = -Inf, ymax = Inf,
                        fill = col_hyp, alpha = 0.15)
    } else {
      p <- p + annotate("rect",
                        xmin = -Inf, xmax = bound,
                        ymin = -Inf, ymax = Inf,
                        fill = col_hyp, alpha = 0.15)
    }
    p <- p +
      geom_vline(xintercept = bound, color = col_hyp,
                 linewidth = 1, linetype = "solid") +
      annotate("text", x = bound, y = 0.55,
               label = paste0(if (cfg$hypothesis$dir == "gt") "≥ " else "≤ ",
                              bound),
               color = col_hyp, fontface = "bold", size = 4.5, hjust = -0.1)

    # Punkt centralny (zawsze)
    p <- p +
      geom_point(aes(x = center, y = 0), color = col_estimate,
                 size = 7, shape = 18) +
      annotate("text", x = center, y = -0.22,
               label = paste0(if (cfg$kind == "mean") "x̄ = " else "p̂ = ",
                              round(center, 3)),
               color = col_estimate, fontface = "bold", size = 4.5)

    # Pasek CI - tylko jezeli wybrany conf
    if (!is.na(conf)) {
      ci <- compute_edge_ci(case_id, conf)
      p <- p +
        geom_errorbarh(aes(xmin = ci$lower, xmax = ci$upper, y = 0),
                       height = 0.18, color = col_ci, linewidth = 2.4, alpha = 0.7) +
        annotate("text", x = center, y = -0.45,
                 label = paste0(round(conf * 100), "% CI: [",
                                round(ci$lower, 3), " ; ", round(ci$upper, 3), "]"),
                 color = col_ci, fontface = "bold", size = 4.8)
    } else {
      p <- p + annotate("text", x = mean(xlims), y = 0.35,
                        label = "Wybierz poziom ufności powyżej",
                        color = upwr_reference, size = 4.5, fontface = "italic")
    }

    p
  }

  # ---- Render werdyktu dla edge case'a ----
  render_edge_explain <- function(case_id) {
    cfg <- edge_cases[[case_id]]
    state <- ch5_edge_state[[case_id]]
    conf <- state$conf
    revealed <- state$revealed

    if (is.na(conf)) {
      return(lc_feedback(type = "info",
        p(tags$strong("Hipoteza: "), cfg$hypothesis$text),
        p(tags$em("Kliknij jeden z przycisków 90% / 95% / 99% żeby zobaczyć
                  przedział ufności."))
      ))
    }

    # Faza 1: tylko CI + tresc hipotezy, czas na dyskusje
    if (!revealed) {
      return(lc_feedback(type = "info",
        p(tags$strong("Hipoteza: "), cfg$hypothesis$text),
        p("Wybrany poziom ufności: ", tags$b(round(conf * 100), "%")),
        p(tags$em("Spojrz na wykres: gdzie leży CI względem granicy hipotezy?
                  Co o tym sądzicie? Kliknięcie ", tags$b("Pokaż werdykt"),
                  " odsloni odpowiedź."))
      ))
    }

    # Faza 2: werdykt
    ci <- compute_edge_ci(case_id, conf)
    verdict <- hypothesis_verdict_edge(ci$lower, ci$upper, cfg$hypothesis$bound,
                                       cfg$hypothesis$dir)
    cls <- verdict_class_edge(verdict)
    label <- verdict_label_edge(verdict)

    body <- if (verdict == "yes") {
      p("Cały ", round(conf * 100), "% CI leży w obszarze hipotezy. Możemy ",
        tags$b("z ", round(conf * 100), "% pewnością"), " stwierdzić, że ",
        cfg$hypothesis$text, ".")
    } else if (verdict == "no") {
      p("Cały ", round(conf * 100), "% CI leży poza obszarem hipotezy. Z ",
        round(conf * 100), "% pewnością ", tags$b("nie możemy"),
        " stwierdzić hipotezy — dane przemawiają wręcz przeciwko niej.")
    } else {
      p(round(conf * 100), "% CI ", tags$b("przecina granicę hipotezy"),
        " (", round(cfg$hypothesis$bound, 3), "). Część przedziału jest w obszarze
        hipotezy, część poza. Z ", round(conf * 100), "% pewnością ",
        tags$b("nie możemy stwierdzić"),
        ", że hipoteza jest prawdziwa — ale też nie możemy jej odrzucić.
        Spróbuj zmienić poziom ufności i zobacz, jak werdykt się zmienia.")
    }

    lc_feedback(type = cls,
      p(tags$strong("Hipoteza: "), cfg$hypothesis$text),
      p(tags$strong("Werdykt przy ", round(conf * 100), "% ufności: ", label)),
      body
    )
  }

  # ---- Rejestracja outputow + observerow dla kazdego edge case'a ----
  register_edge_case <- function(case_id) {
    levels <- c(0.90, 0.95, 0.99)
    # Klikniecie poziomu ufnosci -> wybiera conf, RESETUJE revealed na FALSE
    lapply(levels, function(lv) {
      force(lv)
      observeEvent(input[[paste0("ch5_", case_id, "_conf", round(lv * 100))]], {
        ch5_edge_state[[case_id]] <- list(conf = lv, revealed = FALSE)
      }, ignoreInit = TRUE)
    })

    # Przycisk "Pokaz werdykt" -> ustawia revealed = TRUE (zachowujac obecny conf)
    observeEvent(input[[paste0("ch5_", case_id, "_reveal")]], {
      current <- ch5_edge_state[[case_id]]
      if (!is.na(current$conf) && !current$revealed) {
        ch5_edge_state[[case_id]] <- list(conf = current$conf, revealed = TRUE)
      }
    }, ignoreInit = TRUE)

    output[[paste0("ch5_", case_id, "_buttons")]] <- renderUI({
      ch5_edge_state[[case_id]]
      edge_buttons_ui(case_id)
    })
    output[[paste0("ch5_", case_id, "_plot")]] <- renderPlot({
      ch5_edge_state[[case_id]]
      render_edge_plot(case_id)
    })
    output[[paste0("ch5_", case_id, "_explain")]] <- renderUI({
      ch5_edge_state[[case_id]]
      render_edge_explain(case_id)
    })
  }

  for (cid in names(edge_cases)) {
    register_edge_case(cid)
  }
}
