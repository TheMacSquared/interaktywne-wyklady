# ============================================================================
# CHAPTER 3: Jedna zmienna ilosciowa — test t jednej proby
# ============================================================================

ch2_ui <- list(
  id = "ch-jedna-ilosciowa", num = "03", title = "Test t jednej próby",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 03 · Wnioskowanie statystyczne",
      num    = "03",
      title  = "Test t jednej próby.",
      lead   = "„Czy nasi studenci mają typowy poziom koncentracji?” Jeden pomiar na
                osobie, porównanie średniej z wartością referencyjną — od danych, przez
                statystykę testową, do p-wartości."
    ),

    # ========================================================================
    # Case study otwierajacy
    # ========================================================================
    h2(id = "ch2-pytanie", class = "section-title", "Od pytania do testu"),

    div(class = "narrative",
      p("Statystyk nie zaczyna od wzorów — zaczyna od pytania. Ktoś przychodzi i pyta w języku potocznym:"),
      div(class = "callout-info", style = "font-size: 18px; text-align: center;",
        tags$em("„Czy nasi studenci mają typowy poziom koncentracji?
        Bo wydaje mi się, że coś z nimi jest nie tak.”")
      ),
      p("Zadanie statystyka: przełożyć to na formalną hipotezę i dodać kontekst —
        typowy to ile? Mamy wartość referencyjną
        z pilotażu: średni wynik testu koncentracji w populacji = 70 pkt."),
      p("Więc pytanie potoczne zamienia się w:"),
      div(class = "formula-box",
        p(withMathJax("\\(H_0: \\mu = 70 \\quad\\text{(średnia jest typowa)}\\)")),
        p(withMathJax("\\(H_a: \\mu \\neq 70 \\quad\\text{(średnia odbiega od normy)}\\)"))
      ),
      p("Teraz potrzebujemy danych i wzoru na ", tags$b("test t jednej próby"), ":"),
      div(class = "formula-box",
        p(withMathJax("\\(t = \\frac{\\bar{x} - \\mu_0}{s / \\sqrt{n}}, \\quad df = n - 1\\)"))
      )
    ),

    # ========================================================================
    # WIDGET 1: Krokowy test t jednej proby
    # ========================================================================
    h2(id = "ch2-krok", class = "section-title", "Test t jednej próby — krok po kroku"),

    figure_panel(
      label = "Ryc. 3.1",
      title = "Test t jednej próby — krok po kroku",
      fluidRow(
        column(4,
          selectInput("ch2_scenario", "Scenariusz:",
            choices = c(
              "Koncentracja (μ₀ = 70 pkt)" = "concentration",
              "Zużycie wody (μ₀ = 150 l)" = "water",
              "Plon pszenicy (μ₀ = 5 t/ha)" = "yield",
              "Trwałość jogurtu (μ₀ = 14 dni)" = "yogurt"
            ),
            selected = "concentration"
          ),
          sliderInput("ch2_n", "Wielkość próby (n):",
                      min = 10, max = 100, value = 40, step = 5),
          actionButton("ch2_new_sample", "Losuj próbę",
                       class = "btn-primary", width = "100%"),
          hr(),
          h5("Kroki testu:"),
          div(style = "display: flex; flex-direction: column; gap: 6px;",
            actionButton("ch2_step1", "1. Dane",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch2_step2", "2. Statystyki opisowe",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch2_step3", "3. Statystyka testowa",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch2_step4", "4. p-wartość i decyzja",
                         class = "btn-outline-primary", width = "100%")
          )
        ),
        column(8,
          uiOutput("ch2_hypothesis_panel"),
          plotOutput("ch2_step_plot", height = "350px"),
          uiOutput("ch2_step_info")
        )
      )
    ),

    # ========================================================================
    # Interpretacja
    # ========================================================================
    margin_callout(
      label = "Co zrobiliśmy?",
      tagList(
        tags$ol(
          tags$li("Zebraliśmy dane (próbę)"),
          tags$li("Obliczyliśmy średnią i odchylenie standardowe"),
          tags$li("Policzyliśmy, jak daleko średnia z próby jest od μ₀ — to statystyka t"),
          tags$li("Sprawdziliśmy, czy taka wartość t jest zaskakująca (p-wartość)")
        ),
        tags$p("Jeśli p < 0.05, różnica między naszą próbą a wartością referencyjną
               jest zbyt duża, by ją wytłumaczyć przypadkiem.")
      )
    ),

    # ========================================================================
    # WIDGET 2: Test jednostronny — to samo pytanie, ale z kierunkiem
    # ========================================================================
    h2(id = "ch2-jednostronny", class = "section-title", "A jeśli znamy kierunek? Test jednostronny"),

    div(class = "narrative",
      p("W pierwszym teście pytaliśmy: „czy średnia różni się od μ₀?” (dwustronny, ≠).
        Ale czasem mamy silniejsze podejrzenie — nie tylko „czy różni się”,
        ale „czy jest większa / mniejsza”."),
      p("Użyjemy tych samych danych co powyżej, ale zmienimy pytanie na kierunkowe.
        Zobaczcie, jak zmienia się hipoteza i wykres.")
    ),

    figure_panel(
      label = "Ryc. 3.2",
      title = "Test t jednostronny — krok po kroku",
      fluidRow(
        column(4,
          helpText("Dane: te same co w teście dwustronnym powyżej."),
          hr(),
          h5("Kroki testu:"),
          div(style = "display: flex; flex-direction: column; gap: 6px;",
            actionButton("ch2b_step1", "1. Dane",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch2b_step2", "2. Statystyki opisowe",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch2b_step3", "3. Statystyka testowa",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch2b_step4", "4. p-wartość i decyzja",
                         class = "btn-outline-primary", width = "100%")
          )
        ),
        column(8,
          uiOutput("ch2b_hypothesis_panel"),
          plotOutput("ch2b_step_plot", height = "350px"),
          uiOutput("ch2b_step_info")
        )
      )
    ),

    margin_callout(
      label = "Dwustronny a jednostronny",
      tagList(
        tags$ul(
          tags$li(tags$b("Dwustronny (≠):"), " bezpieczniejszy, wykrywa efekt w obie strony.
            Punkt krytyczny dalej od zera — trudniej odrzucić H₀."),
          tags$li(tags$b("Jednostronny (> lub <):"), " mocniejszy w jednym kierunku,
            ale ", tags$em("ślepy"), " na efekt w drugim."),
          tags$li("Regułą: jednostronny decydujemy przed zbieraniem danych!")
        )
      ),
      color = "uwaga"
    ),

    lc_chapter_next(
      num       = "04",
      title     = "Test proporcji",
      lead      = "a co gdy pytamy nie o średnią, lecz o procent?",
      target_id = "ch-jedna-jakosciowa"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch2_server <- function(input, output, session) {

  # --- Dane scenariuszy ---
  scenario_params <- list(
    concentration = list(mu0 = 70, mu_true = 72, sd = 13,
                         xlab = "Wynik testu koncentracji (pkt)",
                         title = "Koncentracja studentów",
                         question = "Czy nasi studenci mają typowy poziom koncentracji?",
                         h0_text = "\\(H_0: \\mu = 70\\) (koncentracja jest typowa)",
                         h1_text = "\\(H_a: \\mu \\neq 70\\) (koncentracja odbiega od normy)"),
    water  = list(mu0 = 150, mu_true = 158, sd = 25,
                  xlab = "Zużycie wody (l/osobę/dobę)",
                  title = "Zużycie wody w gminie",
                  question = "Czy zużycie wody w naszej gminie spełnia normę projektową 150 l/osobę?",
                  h0_text = "\\(H_0: \\mu = 150\\) (zużycie zgodne z normą)",
                  h1_text = "\\(H_a: \\mu \\neq 150\\) (zużycie odbiega od normy)"),
    yield  = list(mu0 = 5, mu_true = 5.4, sd = 0.8,
                  xlab = "Plon pszenicy (t/ha)",
                  title = "Plony na poletku doświadczalnym",
                  question = "Czy średni plon pszenicy na naszych poletkach odpowiada średniej krajowej 5 t/ha?",
                  h0_text = "\\(H_0: \\mu = 5\\) (plon typowy dla kraju)",
                  h1_text = "\\(H_a: \\mu \\neq 5\\) (plon odbiega od średniej krajowej)"),
    yogurt = list(mu0 = 14, mu_true = 15.2, sd = 2.5,
                  xlab = "Trwałość (dni do przeterminowania)",
                  title = "Trwałość jogurtu naturalnego",
                  question = "Czy trwałość naszego jogurtu spełnia deklarowane 14 dni?",
                  h0_text = "\\(H_0: \\mu = 14\\) (trwałość zgodna z deklaracją)",
                  h1_text = "\\(H_a: \\mu \\neq 14\\) (trwałość odbiega od deklaracji)")
  )

  # --- Shared state ---
  ch2_sample <- reactiveVal(NULL)
  ch2_step   <- reactiveVal(0)

  observeEvent(input$ch2_new_sample, {
    par <- scenario_params[[input$ch2_scenario]]
    n <- input$ch2_n
    samp <- rnorm(n, mean = par$mu_true, sd = par$sd)
    ch2_sample(samp)
    ch2_step(0)
  })

  # Resetuj probke przy zmianie scenariusza
  observeEvent(input$ch2_scenario, {
    ch2_sample(NULL)
    ch2_step(0)
  })

  observeEvent(input$ch2_step1, ch2_step(1))
  observeEvent(input$ch2_step2, ch2_step(2))
  observeEvent(input$ch2_step3, ch2_step(3))
  observeEvent(input$ch2_step4, ch2_step(4))

  # --- Panel hipotezy (zawsze widoczny) ---
  output$ch2_hypothesis_panel <- renderUI({
    par <- scenario_params[[input$ch2_scenario]]
    samp <- ch2_sample()

    tagList(
      div(class = "callout-info", style = "font-size: 16px;",
        p(tags$b("Pytanie potoczne:")),
        p(tags$em(paste0("„", par$question, "”")))
      ),
      div(class = "formula-box",
        p(tags$b("Hipoteza formalna (dwustronna):")),
        p(withMathJax(par$h0_text)),
        p(withMathJax(par$h1_text))
      ),
      if (is.null(samp)) {
        div(style = "text-align: center; margin: 10px 0; color: var(--upwr-reference);",
          p(tags$em("Kliknij „Losuj próbę”, żeby zebrać dane"))
        )
      }
    )
  })

  # --- Krokowy wykres ---
  output$ch2_step_plot <- renderPlot({
    samp <- ch2_sample()
    step <- ch2_step()
    par <- scenario_params[[input$ch2_scenario]]
    mu0 <- par$mu0

    if (is.null(samp)) return(NULL)

    if (step == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Próba gotowa! Klikaj kroki po kolei.",
                 size = 5, color = upwr_reference) +
        theme_void()
    } else if (step <= 2) {
      # Krok 1-2: histogram danych
      p <- ggplot(data.frame(x = samp), aes(x = x)) +
        geom_histogram(bins = 15, fill = col_h0, alpha = 0.6, color = "white") +
        labs(title = par$title, x = par$xlab, y = "Liczba") +
        theme()

      if (step >= 2) {
        # Dodaj srednia i mu0
        p <- p +
          geom_vline(xintercept = mu0, color = col_reject, linewidth = 1.2,
                     linetype = "dashed") +
          geom_vline(xintercept = mean(samp), color = col_pvalue, linewidth = 1.2) +
          annotate("text", x = mu0, y = Inf, vjust = 2,
                   label = paste0("μ₀ = ", mu0), color = col_reject,
                   fontface = "bold") +
          annotate("text", x = mean(samp), y = Inf, vjust = 3.5,
                   label = paste0("x̄ = ", round(mean(samp), 2)),
                   color = col_pvalue, fontface = "bold")
      }
      p
    } else if (step == 3) {
      # Krok 3: rozklad t — tylko linia statystyki, bez zacienionego pola
      n <- length(samp)
      t_stat <- (mean(samp) - mu0) / (sd(samp) / sqrt(n))

      x <- seq(-4, 4, length.out = 500)
      y <- dt(x, df = n - 1)
      plot_df <- data.frame(x = x, y = y)

      ggplot(plot_df, aes(x = x, y = y)) +
        geom_line(color = col_h0, linewidth = 1.2) +
        geom_vline(xintercept = t_stat, color = col_reject,
                   linewidth = 1.2, linetype = "dashed") +
        annotate("text", x = t_stat, y = max(y) * 0.9,
                 label = paste0("t = ", round(t_stat, 3)),
                 hjust = if (t_stat > 0) -0.1 else 1.1,
                 color = col_reject, fontface = "bold") +
        labs(title = paste0("Rozkład pod H₀: t(", n - 1, ")"),
             x = "Statystyka testowa", y = "Gęstość") +
        theme()

    } else {
      # Krok 4: rozklad t z zacienionym polem p-wartosci
      n <- length(samp)
      t_stat <- (mean(samp) - mu0) / (sd(samp) / sqrt(n))
      plot_test_distribution(t_stat, df = n - 1, test_type = "t")
    }
  })

  # --- Krokowe info ---
  output$ch2_step_info <- renderUI({
    samp <- ch2_sample()
    step <- ch2_step()
    par <- scenario_params[[input$ch2_scenario]]
    mu0 <- par$mu0

    if (is.null(samp) || step == 0) return(NULL)

    n <- length(samp)
    x_bar <- mean(samp)
    s <- sd(samp)
    se <- s / sqrt(n)
    t_stat <- (x_bar - mu0) / se
    p_val <- 2 * pt(-abs(t_stat), df = n - 1)
    res <- format_test_result(p_val)

    info <- switch(as.character(step),
      "1" = tagList(
        div(class = "stat-box", style = paste0("background:", col_h0, ";"),
            paste0("n = ", n)),
        p("Mamy próbę ", n, " obserwacji. Chcemy sprawdzić, czy średnia różni się od μ₀ = ", mu0, ".")
      ),
      "2" = tagList(
        div(class = "stat-box", style = paste0("background:", col_pvalue, ";"),
            paste0("x̄ = ", round(x_bar, 2))),
        div(class = "stat-box", style = paste0("background:", col_h0, ";"),
            paste0("s = ", round(s, 2))),
        div(class = "stat-box", style = paste0("background:", upwr_secondary, ";"),
            paste0("SE = s/√n = ", round(se, 2))),
        p("Różnica między x̄ a μ₀: ", tags$b(round(x_bar - mu0, 2)),
          ". Ale czy to dużo? Musimy to odnieść do zmienności (SE).")
      ),
      "3" = tagList(
        div(class = "stat-box", style = paste0("background:", col_effect, ";"),
            paste0("t = (", round(x_bar, 2), " − ", mu0, ") / ",
                   round(se, 2), " = ", round(t_stat, 3))),
        p("Statystyka t mówi: średnia z próby jest ",
          tags$b(round(abs(t_stat), 1)), " błędów standardowych od μ₀.",
          if (abs(t_stat) > 2) " To sporo!" else " To niewiele.")
      ),
      "4" = tagList(
        div(class = "stat-box", style = paste0("background:", col_pvalue, ";"),
            paste0("p = ", format.pval(p_val, digits = 4))),
        p(style = paste0("color: ", res$color, "; font-weight: bold; font-size: 16px;"),
          res$decision),
        p(res$explanation)
      )
    )
    div(class = "callout-info", info)
  })

  # --- Widget 2: Test jednostronny (te same dane co Widget 1) ---
  scenario_params_1s <- list(
    concentration = list(alt = "less",
                         question = "Czy studenci mają niższą koncentrację niż norma 70 pkt?",
                         h0_text = "\\(H_0: \\mu \\geq 70\\) (koncentracja nie jest niższa)",
                         h1_text = "\\(H_a: \\mu < 70\\) (koncentracja jest niższa niż norma)"),
    water  = list(alt = "greater",
                  question = "Czy zużycie wody w gminie przekracza normę projektową 150 l/osobę?",
                  h0_text = "\\(H_0: \\mu \\leq 150\\) (zużycie nie przekracza normy)",
                  h1_text = "\\(H_a: \\mu > 150\\) (zużycie przekracza normę)"),
    yield  = list(alt = "greater",
                  question = "Czy nowa odmiana daje wyższy plon niż średnia krajowa 5 t/ha?",
                  h0_text = "\\(H_0: \\mu \\leq 5\\) (plon nie jest wyższy)",
                  h1_text = "\\(H_a: \\mu > 5\\) (plon jest wyższy niż średnia krajowa)"),
    yogurt = list(alt = "greater",
                  question = "Czy trwałość jogurtu jest dłuższa niż deklarowane 14 dni?",
                  h0_text = "\\(H_0: \\mu \\leq 14\\) (trwałość nie przekracza deklaracji)",
                  h1_text = "\\(H_a: \\mu > 14\\) (trwałość jest dłuższa niż deklarowana)")
  )

  ch2b_step <- reactiveVal(0)

  # Reset krokow Widget 2 gdy Widget 1 generuje nowa probke
  observeEvent(input$ch2_new_sample, { ch2b_step(0) })
  observeEvent(input$ch2_scenario,   { ch2b_step(0) })

  observeEvent(input$ch2b_step1, ch2b_step(1))
  observeEvent(input$ch2b_step2, ch2b_step(2))
  observeEvent(input$ch2b_step3, ch2b_step(3))
  observeEvent(input$ch2b_step4, ch2b_step(4))

  # Panel hipotezy (jednostronny) — zawsze widoczny jako naglowek
  output$ch2b_hypothesis_panel <- renderUI({
    par1s <- scenario_params_1s[[input$ch2_scenario]]
    samp <- ch2_sample()

    tagList(
      div(class = "callout-info", style = "font-size: 16px;",
        p(tags$b("Pytanie potoczne (kierunkowe):")),
        p(tags$em(paste0("„", par1s$question, "”")))
      ),
      div(class = "formula-box",
        p(tags$b("Hipoteza formalna (jednostronna!):")),
        p(withMathJax(par1s$h0_text)),
        p(withMathJax(par1s$h1_text))
      ),
      if (is.null(samp)) {
        div(style = "text-align: center; margin: 10px 0; color: var(--upwr-reference);",
          p(tags$em("Najpierw wylosuj próbę w teście dwustronnym powyżej"))
        )
      }
    )
  })

  # Krokowy wykres (jednostronny)
  output$ch2b_step_plot <- renderPlot({
    samp <- ch2_sample()
    step <- ch2b_step()
    par <- scenario_params[[input$ch2_scenario]]
    par1s <- scenario_params_1s[[input$ch2_scenario]]
    mu0 <- par$mu0

    if (is.null(samp) || step == 0) return(NULL)

    if (step <= 2) {
      p <- ggplot(data.frame(x = samp), aes(x = x)) +
        geom_histogram(bins = 15, fill = col_h0, alpha = 0.6, color = "white") +
        labs(title = par$title, x = par$xlab, y = "Liczba") +
        theme()

      if (step >= 2) {
        p <- p +
          geom_vline(xintercept = mu0, color = col_reject, linewidth = 1.2,
                     linetype = "dashed") +
          geom_vline(xintercept = mean(samp), color = col_pvalue, linewidth = 1.2) +
          annotate("text", x = mu0, y = Inf, vjust = 2,
                   label = paste0("μ₀ = ", mu0), color = col_reject,
                   fontface = "bold") +
          annotate("text", x = mean(samp), y = Inf, vjust = 3.5,
                   label = paste0("x̄ = ", round(mean(samp), 2)),
                   color = col_pvalue, fontface = "bold")
      }
      p
    } else if (step == 3) {
      n <- length(samp)
      t_stat <- (mean(samp) - mu0) / (sd(samp) / sqrt(n))

      x <- seq(-4, 4, length.out = 500)
      y <- dt(x, df = n - 1)
      plot_df <- data.frame(x = x, y = y)

      ggplot(plot_df, aes(x = x, y = y)) +
        geom_line(color = col_h0, linewidth = 1.2) +
        geom_vline(xintercept = t_stat, color = col_reject,
                   linewidth = 1.2, linetype = "dashed") +
        annotate("text", x = t_stat, y = max(y) * 0.9,
                 label = paste0("t = ", round(t_stat, 3)),
                 hjust = if (t_stat > 0) -0.1 else 1.1,
                 color = col_reject, fontface = "bold") +
        labs(title = paste0("Rozkład pod H₀: t(", n - 1, ")"),
             subtitle = "Test jednostronny — tylko jeden ogon!",
             x = "Statystyka testowa", y = "Gęstość") +
        theme()
    } else {
      n <- length(samp)
      t_stat <- (mean(samp) - mu0) / (sd(samp) / sqrt(n))
      plot_test_distribution(t_stat, df = n - 1, test_type = "t",
                             alternative = par1s$alt)
    }
  })

  # Krokowe info (jednostronny)
  output$ch2b_step_info <- renderUI({
    samp <- ch2_sample()
    step <- ch2b_step()
    par <- scenario_params[[input$ch2_scenario]]
    par1s <- scenario_params_1s[[input$ch2_scenario]]
    mu0 <- par$mu0

    if (is.null(samp) || step == 0) return(NULL)

    n <- length(samp)
    x_bar <- mean(samp)
    s <- sd(samp)
    se <- s / sqrt(n)
    t_stat <- (x_bar - mu0) / se

    # p-wartosc jednostronna
    p_val <- if (par1s$alt == "less") {
      pt(t_stat, df = n - 1)
    } else {
      pt(t_stat, df = n - 1, lower.tail = FALSE)
    }
    res <- format_test_result(p_val)

    dir_label <- if (par1s$alt == "less") "mniejsza" else "większa"

    info <- switch(as.character(step),
      "1" = tagList(
        div(class = "stat-box", style = paste0("background:", col_h0, ";"),
            paste0("n = ", n, " (te same dane co wyżej)")),
        p("Pytamy, czy średnia jest ",
          dir_label, " niż μ₀ = ", mu0, ".")
      ),
      "2" = tagList(
        div(class = "stat-box", style = paste0("background:", col_pvalue, ";"),
            paste0("x̄ = ", round(x_bar, 2))),
        div(class = "stat-box", style = paste0("background:", col_h0, ";"),
            paste0("s = ", round(s, 2))),
        div(class = "stat-box", style = paste0("background:", upwr_secondary, ";"),
            paste0("SE = s/√n = ", round(se, 2))),
        p("Statystyki takie same jak wyżej — dane się nie zmieniły.
          Zmieniło się tylko pytanie (kierunek).")
      ),
      "3" = tagList(
        div(class = "stat-box", style = paste0("background:", col_effect, ";"),
            paste0("t = ", round(t_stat, 3), " (taka sama wartość!)")),
        p("Statystyka t jest identyczna. Ale w teście jednostronnym patrzymy tylko na ",
          tags$b(if (par1s$alt == "less") "lewy" else "prawy"), " ogon rozkładu.")
      ),
      "4" = tagList(
        div(class = "stat-box", style = paste0("background:", col_pvalue, ";"),
            paste0("p = ", format.pval(p_val, digits = 4),
                   " (jednostronnie!)")),
        p(style = paste0("color: ", res$color, "; font-weight: bold; font-size: 16px;"),
          res$decision),
        p(res$explanation),
        p(tags$em("Porównaj z testem dwustronnym wyżej — te same dane,
          ten sam t, ale inna p-wartość!"))
      )
    )
    div(class = "callout-info", info)
  })
}
