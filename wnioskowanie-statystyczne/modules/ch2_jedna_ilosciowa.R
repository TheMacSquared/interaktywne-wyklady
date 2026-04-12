# ============================================================================
# CHAPTER 3: Jedna zmienna ilosciowa — test t jednej proby
# ============================================================================

ch2_ui <- tabPanel("3. Jedna zmienna ilo\u015bciowa",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Znamy logik\u0119 testowania i formu\u0142owanie hipotez.
       Czas na pierwszy konkretny test: czy \u015brednia r\u00f3\u017cni si\u0119 od zak\u0142adanej warto\u015bci?"
    ),

    # ========================================================================
    # Case study otwierajacy
    # ========================================================================
    div(class = "section-title", "Od pytania do testu"),

    div(class = "narrative",
      p("Statystyk nie zaczyna od wzor\u00f3w \u2014 zaczyna od ", tags$b("pytania"),
        ". Kto\u015b przychodzi i pyta w j\u0119zyku potocznym:"),
      div(class = "callout-info", style = "font-size: 18px; text-align: center;",
        tags$em("\u201eCzy nasi studenci maj\u0105 typowy poziom koncentracji?
        Bo wydaje mi si\u0119, \u017ce co\u015b z nimi jest nie tak.\u201d")
      ),
      p("Zadanie statystyka: prze\u0142o\u017cy\u0107 to na ", tags$b("formaln\u0105 hipotez\u0119"),
        " i doda\u0107 kontekst \u2014 typowy to ile? Mamy warto\u015b\u0107 referencyjn\u0105
        z pilota\u017cu: \u015bredni wynik testu koncentracji w populacji = 70 pkt."),
      p("Wi\u0119c pytanie potoczne zamienia si\u0119 w:"),
      div(class = "formula-box",
        p(withMathJax("\\(H_0: \\mu = 70 \\quad\\text{(\u015brednia jest typowa)}\\)")),
        p(withMathJax("\\(H_a: \\mu \\neq 70 \\quad\\text{(\u015brednia odbiega od normy)}\\)"))
      ),
      p("Teraz potrzebujemy danych i wzoru na ", tags$b("test t jednej pr\u00f3by"), ":"),
      div(class = "formula-box",
        p(withMathJax("\\(t = \\frac{\\bar{x} - \\mu_0}{s / \\sqrt{n}}, \\quad df = n - 1\\)"))
      )
    ),

    # ========================================================================
    # WIDGET 1: Krokowy test t jednej proby
    # ========================================================================
    div(class = "section-title", "Test t jednej pr\u00f3by \u2014 krok po kroku"),

    div(class = "widget-block",
      fluidRow(
        column(4,
          selectInput("ch2_scenario", "Scenariusz:",
            choices = c(
              "Koncentracja (\u03bc\u2080 = 70 pkt)" = "concentration",
              "Zu\u017cycie wody (\u03bc\u2080 = 150 l)" = "water",
              "Plon pszenicy (\u03bc\u2080 = 5 t/ha)" = "yield",
              "Trwa\u0142o\u015b\u0107 jogurtu (\u03bc\u2080 = 14 dni)" = "yogurt"
            ),
            selected = "concentration"
          ),
          sliderInput("ch2_n", "Wielko\u015b\u0107 pr\u00f3by (n):",
                      min = 10, max = 100, value = 40, step = 5),
          actionButton("ch2_new_sample", "Losuj pr\u00f3b\u0119",
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
            actionButton("ch2_step4", "4. p-warto\u015b\u0107 i decyzja",
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
    div(class = "callout-info",
      tags$strong("Co zrobili\u015bmy?"),
      tags$ol(
        tags$li("Zebrali\u015bmy dane (pr\u00f3b\u0119)"),
        tags$li("Obliczyli\u015bmy \u015bredni\u0105 i odchylenie standardowe"),
        tags$li("Policzylimy, jak daleko \u015brednia z pr\u00f3by jest od \u03bc\u2080 (w jednostkach b\u0142\u0119du standardowego) \u2014 to jest statystyka t"),
        tags$li("Sprawdzili\u015bmy, czy taka warto\u015b\u0107 t jest zaskakuj\u0105ca (p-warto\u015b\u0107)")
      ),
      p("Je\u015bli p < 0.05, r\u00f3\u017cnica mi\u0119dzy nasz\u0105 pr\u00f3b\u0105 a warto\u015bci\u0105 referencyjn\u0105
        jest zbyt du\u017ca, by j\u0105 wyt\u0142umaczy\u0107 przypadkiem.")
    ),

    # ========================================================================
    # WIDGET 2: Test jednostronny — to samo pytanie, ale z kierunkiem
    # ========================================================================
    div(class = "section-title", "A je\u015bli znamy kierunek? Test jednostronny"),

    div(class = "narrative",
      p("W pierwszym te\u015bcie pytali\u015bmy: \u201eczy \u015brednia ", tags$b("r\u00f3\u017cni si\u0119"),
        " od \u03bc\u2080?\u201d (dwustronny, \u2260). Ale czasem mamy silniejsze podejrzenie \u2014
        nie tylko \u201eczy r\u00f3\u017cni si\u0119\u201d, ale \u201eczy jest ",
        tags$b("wi\u0119ksza"), " / ", tags$b("mniejsza"), "\u201d."),
      p("U\u017cyjemy ", tags$b("tych samych danych"), " co powy\u017cej,
        ale zmienimy pytanie na kierunkowe. Zobaczcie, jak zmienia si\u0119
        hipoteza i wykres.")
    ),

    div(class = "widget-block",
      fluidRow(
        column(4,
          helpText("Dane: te same co w te\u015bcie dwustronnym powy\u017cej."),
          hr(),
          h5("Kroki testu:"),
          div(style = "display: flex; flex-direction: column; gap: 6px;",
            actionButton("ch2b_step1", "1. Dane",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch2b_step2", "2. Statystyki opisowe",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch2b_step3", "3. Statystyka testowa",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch2b_step4", "4. p-warto\u015b\u0107 i decyzja",
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

    div(class = "callout-warning",
      tags$strong("Dwustronny vs jednostronny:"),
      tags$ul(
        tags$li(tags$b("Dwustronny (\u2260):"), " bezpieczniejszy, wykrywa efekt w obie strony.
          Punkt krytyczny dalej od zera \u2014 trudniej odrzuci\u0107 H\u2080."),
        tags$li(tags$b("Jednostronny (> lub <):"), " mocniejszy w jednym kierunku, ale ",
          tags$em("\u015blepy"), " na efekt w drugim. Punkt krytyczny bli\u017cej zera \u2014 \u0142atwiej odrzuci\u0107 H\u2080."),
        tags$li("Regu\u0142a: test jednostronny decydujemy ", tags$b("przed"),
          " zbieraniem danych, nie po zobaczeniu wynik\u00f3w!")
      )
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: testy dla jednej zmiennej jako\u015bciowej"),
      actionButton("ch2_next", "Dalej \u2192 4. Jedna zmienna jako\u015bciowa",
                   class = "btn-primary btn-lg")
    )
  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch2_server <- function(input, output, session) {

  # --- Dane scenariuszy ---
  scenario_params <- list(
    concentration = list(mu0 = 70, mu_true = 72, sd = 13,
                         xlab = "Wynik testu koncentracji (pkt)",
                         title = "Koncentracja student\u00f3w",
                         question = "Czy nasi studenci maj\u0105 typowy poziom koncentracji?",
                         h0_text = "\\(H_0: \\mu = 70\\) (koncentracja jest typowa)",
                         h1_text = "\\(H_a: \\mu \\neq 70\\) (koncentracja odbiega od normy)"),
    water  = list(mu0 = 150, mu_true = 158, sd = 25,
                  xlab = "Zu\u017cycie wody (l/osob\u0119/dob\u0119)",
                  title = "Zu\u017cycie wody w gminie",
                  question = "Czy zu\u017cycie wody w naszej gminie spe\u0142nia norm\u0119 projektow\u0105 150 l/osob\u0119?",
                  h0_text = "\\(H_0: \\mu = 150\\) (zu\u017cycie zgodne z norm\u0105)",
                  h1_text = "\\(H_a: \\mu \\neq 150\\) (zu\u017cycie odbiega od normy)"),
    yield  = list(mu0 = 5, mu_true = 5.4, sd = 0.8,
                  xlab = "Plon pszenicy (t/ha)",
                  title = "Plony na poletku do\u015bwiadczalnym",
                  question = "Czy \u015bredni plon pszenicy na naszych poletkach odpowiada \u015bredniej krajowej 5 t/ha?",
                  h0_text = "\\(H_0: \\mu = 5\\) (plon typowy dla kraju)",
                  h1_text = "\\(H_a: \\mu \\neq 5\\) (plon odbiega od \u015bredniej krajowej)"),
    yogurt = list(mu0 = 14, mu_true = 15.2, sd = 2.5,
                  xlab = "Trwa\u0142o\u015b\u0107 (dni do przeterminowania)",
                  title = "Trwa\u0142o\u015b\u0107 jogurtu naturalnego",
                  question = "Czy trwa\u0142o\u015b\u0107 naszego jogurtu spe\u0142nia deklarowane 14 dni?",
                  h0_text = "\\(H_0: \\mu = 14\\) (trwa\u0142o\u015b\u0107 zgodna z deklaracj\u0105)",
                  h1_text = "\\(H_a: \\mu \\neq 14\\) (trwa\u0142o\u015b\u0107 odbiega od deklaracji)")
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
        p(tags$em(paste0("\u201e", par$question, "\u201d")))
      ),
      div(class = "formula-box",
        p(tags$b("Hipoteza formalna (dwustronna):")),
        p(withMathJax(par$h0_text)),
        p(withMathJax(par$h1_text))
      ),
      if (is.null(samp)) {
        div(style = "text-align: center; margin: 10px 0; color: #7f8c8d;",
          p(tags$em("Kliknij \u201eLosuj pr\u00f3b\u0119\u201d, \u017ceby zebra\u0107 dane"))
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
                 label = "Pr\u00f3ba gotowa! Klikaj kroki po kolei.",
                 size = 5, color = "#7f8c8d") +
        theme_void()
    } else if (step <= 2) {
      # Krok 1-2: histogram danych
      p <- ggplot(data.frame(x = samp), aes(x = x)) +
        geom_histogram(bins = 15, fill = col_h0, alpha = 0.6, color = "white") +
        labs(title = par$title, x = par$xlab, y = "Liczba") +
        theme_test()

      if (step >= 2) {
        # Dodaj srednia i mu0
        p <- p +
          geom_vline(xintercept = mu0, color = col_reject, linewidth = 1.2,
                     linetype = "dashed") +
          geom_vline(xintercept = mean(samp), color = col_pvalue, linewidth = 1.2) +
          annotate("text", x = mu0, y = Inf, vjust = 2,
                   label = paste0("\u03bc\u2080 = ", mu0), color = col_reject,
                   fontface = "bold") +
          annotate("text", x = mean(samp), y = Inf, vjust = 3.5,
                   label = paste0("x\u0304 = ", round(mean(samp), 2)),
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
        labs(title = paste0("Rozk\u0142ad pod H\u2080: t(", n - 1, ")"),
             x = "Statystyka testowa", y = "G\u0119sto\u015b\u0107") +
        theme_test()

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
        p("Mamy pr\u00f3b\u0119 ", n, " obserwacji. Chcemy sprawdzi\u0107, czy \u015brednia r\u00f3\u017cni si\u0119 od \u03bc\u2080 = ", mu0, ".")
      ),
      "2" = tagList(
        div(class = "stat-box", style = paste0("background:", col_pvalue, ";"),
            paste0("x\u0304 = ", round(x_bar, 2))),
        div(class = "stat-box", style = paste0("background:", col_h0, ";"),
            paste0("s = ", round(s, 2))),
        div(class = "stat-box", style = paste0("background:", col_dark, ";"),
            paste0("SE = s/\u221an = ", round(se, 2))),
        p("R\u00f3\u017cnica mi\u0119dzy x\u0304 a \u03bc\u2080: ", tags$b(round(x_bar - mu0, 2)),
          ". Ale czy to du\u017co? Musimy to odnie\u015b\u0107 do zmienno\u015bci (SE).")
      ),
      "3" = tagList(
        div(class = "stat-box", style = paste0("background:", col_effect, ";"),
            paste0("t = (", round(x_bar, 2), " \u2212 ", mu0, ") / ",
                   round(se, 2), " = ", round(t_stat, 3))),
        p("Statystyka t m\u00f3wi: \u015brednia z pr\u00f3by jest ",
          tags$b(round(abs(t_stat), 1)), " b\u0142\u0119d\u00f3w standardowych od \u03bc\u2080.",
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
                         question = "Czy studenci maj\u0105 ni\u017csz\u0105 koncentracj\u0119 ni\u017c norma 70 pkt?",
                         h0_text = "\\(H_0: \\mu \\geq 70\\) (koncentracja nie jest ni\u017csza)",
                         h1_text = "\\(H_a: \\mu < 70\\) (koncentracja jest ni\u017csza ni\u017c norma)"),
    water  = list(alt = "greater",
                  question = "Czy zu\u017cycie wody w gminie przekracza norm\u0119 projektow\u0105 150 l/osob\u0119?",
                  h0_text = "\\(H_0: \\mu \\leq 150\\) (zu\u017cycie nie przekracza normy)",
                  h1_text = "\\(H_a: \\mu > 150\\) (zu\u017cycie przekracza norm\u0119)"),
    yield  = list(alt = "greater",
                  question = "Czy nowa odmiana daje wy\u017cszy plon ni\u017c \u015brednia krajowa 5 t/ha?",
                  h0_text = "\\(H_0: \\mu \\leq 5\\) (plon nie jest wy\u017cszy)",
                  h1_text = "\\(H_a: \\mu > 5\\) (plon jest wy\u017cszy ni\u017c \u015brednia krajowa)"),
    yogurt = list(alt = "greater",
                  question = "Czy trwa\u0142o\u015b\u0107 jogurtu jest d\u0142u\u017csza ni\u017c deklarowane 14 dni?",
                  h0_text = "\\(H_0: \\mu \\leq 14\\) (trwa\u0142o\u015b\u0107 nie przekracza deklaracji)",
                  h1_text = "\\(H_a: \\mu > 14\\) (trwa\u0142o\u015b\u0107 jest d\u0142u\u017csza ni\u017c deklarowana)")
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
        p(tags$em(paste0("\u201e", par1s$question, "\u201d")))
      ),
      div(class = "formula-box",
        p(tags$b("Hipoteza formalna (jednostronna!):")),
        p(withMathJax(par1s$h0_text)),
        p(withMathJax(par1s$h1_text))
      ),
      if (is.null(samp)) {
        div(style = "text-align: center; margin: 10px 0; color: #7f8c8d;",
          p(tags$em("Najpierw wylosuj pr\u00f3b\u0119 w te\u015bcie dwustronnym powy\u017cej"))
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
        theme_test()

      if (step >= 2) {
        p <- p +
          geom_vline(xintercept = mu0, color = col_reject, linewidth = 1.2,
                     linetype = "dashed") +
          geom_vline(xintercept = mean(samp), color = col_pvalue, linewidth = 1.2) +
          annotate("text", x = mu0, y = Inf, vjust = 2,
                   label = paste0("\u03bc\u2080 = ", mu0), color = col_reject,
                   fontface = "bold") +
          annotate("text", x = mean(samp), y = Inf, vjust = 3.5,
                   label = paste0("x\u0304 = ", round(mean(samp), 2)),
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
        labs(title = paste0("Rozk\u0142ad pod H\u2080: t(", n - 1, ")"),
             subtitle = "Test jednostronny \u2014 tylko jeden ogon!",
             x = "Statystyka testowa", y = "G\u0119sto\u015b\u0107") +
        theme_test()
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

    dir_label <- if (par1s$alt == "less") "mniejsza" else "wi\u0119ksza"

    info <- switch(as.character(step),
      "1" = tagList(
        div(class = "stat-box", style = paste0("background:", col_h0, ";"),
            paste0("n = ", n, " (te same dane co wy\u017cej)")),
        p("Pytamy, czy \u015brednia jest ",
          dir_label, " ni\u017c \u03bc\u2080 = ", mu0, ".")
      ),
      "2" = tagList(
        div(class = "stat-box", style = paste0("background:", col_pvalue, ";"),
            paste0("x\u0304 = ", round(x_bar, 2))),
        div(class = "stat-box", style = paste0("background:", col_h0, ";"),
            paste0("s = ", round(s, 2))),
        div(class = "stat-box", style = paste0("background:", col_dark, ";"),
            paste0("SE = s/\u221an = ", round(se, 2))),
        p("Statystyki takie same jak wy\u017cej \u2014 dane si\u0119 nie zmieni\u0142y.
          Zmieni\u0142o si\u0119 tylko pytanie (kierunek).")
      ),
      "3" = tagList(
        div(class = "stat-box", style = paste0("background:", col_effect, ";"),
            paste0("t = ", round(t_stat, 3), " (taka sama warto\u015b\u0107!)")),
        p("Statystyka t jest identyczna. Ale w te\u015bcie jednostronnym patrzymy tylko na ",
          tags$b(if (par1s$alt == "less") "lewy" else "prawy"), " ogon rozk\u0142adu.")
      ),
      "4" = tagList(
        div(class = "stat-box", style = paste0("background:", col_pvalue, ";"),
            paste0("p = ", format.pval(p_val, digits = 4),
                   " (jednostronnie!)")),
        p(style = paste0("color: ", res$color, "; font-weight: bold; font-size: 16px;"),
          res$decision),
        p(res$explanation),
        p(tags$em("Por\u00f3wnaj z testem dwustronnym wy\u017cej \u2014 te same dane,
          ten sam t, ale inna p-warto\u015b\u0107!"))
      )
    )
    div(class = "callout-info", info)
  })
}
