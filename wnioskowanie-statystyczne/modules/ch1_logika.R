# ============================================================================
# CHAPTER 1: Logika testowania hipotez
# ============================================================================

ch1_ui <- tabPanel("1. Logika testowania",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Przedzia\u0142y ufno\u015bci mierz\u0105 niepewno\u015b\u0107 estymacji.
       Teraz przejdziemy do pytania: czy zaobserwowany efekt jest rzeczywisty?"
    ),

    # ========================================================================
    # SEKCJA 0: Case study otwierajacy
    # ========================================================================
    div(class = "section-title", "Czy telefon na biurku obni\u017ca koncentracj\u0119?"),

    div(class = "narrative",
      p("Wyobra\u017aci sobie nast\u0119puj\u0105cy eksperyment na waszej uczelni
        (inspirowany badaniem Ward et al., 2017):"),
      tags$ul(
        tags$li("80 student\u00f3w losowo przydzielonych do dw\u00f3ch grup po 40 os\u00f3b"),
        tags$li(tags$b("Grupa A:"), " telefon schowany w plecaku"),
        tags$li(tags$b("Grupa B:"), " telefon le\u017cy na biurku (ekranem w d\u00f3\u0142, wyciszony)"),
        tags$li("Wszyscy rozwi\u0105zuj\u0105 ten sam test koncentracji (0\u2013100 punkt\u00f3w)")
      ),
      p("Nikt nie u\u017cywa telefonu. R\u00f3\u017cnica jest ",
        tags$em("tylko"), " w tym, czy telefon le\u017cy w zasi\u0119gu wzroku.")
    ),

    div(class = "widget-block",
      h4("Wyniki eksperymentu"),
      fluidRow(
        column(4,
          actionButton("ch1_case_generate", "Przeprowad\u017a eksperyment",
                       class = "btn-primary", width = "100%"),
          br(), br(),
          uiOutput("ch1_case_stats")
        ),
        column(8,
          plotOutput("ch1_case_plot", height = "350px")
        )
      )
    ),

    div(class = "callout-warning",
      p(tags$b("Pytanie kluczowe:"), " \u015arednia w grupie \u201ebiurko\u201d jest ni\u017csza.
        Ale czy to nie mo\u017ce by\u0107 ", tags$b("przypadek"), "?"),
      p("Mo\u017ce gdyby\u015bmy powt\u00f3rzyli eksperyment z innymi 80 osobami,
        r\u00f3\u017cnica by\u0142aby w drug\u0105 stron\u0119? Mo\u017ce po prostu trafili\u015bmy na
        s\u0142abszych student\u00f3w w grupie B?"),
      p(tags$b("W\u0142a\u015bnie do tego s\u0142u\u017cy testowanie hipotez"),
        " \u2014 daje nam narz\u0119dzie do odpowiedzi na pytanie:
        czy obserwowana r\u00f3\u017cnica jest zbyt du\u017ca, \u017ceby by\u0107 przypadkiem?")
    ),

    # ========================================================================
    # SEKCJA 1: Logika testowania z odniesieniem do case study
    # ========================================================================
    div(class = "section-title", "Testowanie hipotez \u2014 logika rozumowania"),

    div(class = "narrative",
      p("Testowanie hipotez statystycznych przypomina ", tags$b("proces s\u0105dowy"), ":"),
      tags$table(class = "table table-bordered", style = "font-size: 15px;",
        tags$thead(
          tags$tr(tags$th("Element"), tags$th("S\u0105d"), tags$th("Nasz eksperyment z telefonem"))
        ),
        tags$tbody(
          tags$tr(
            tags$td(tags$b("H\u2080")),
            tags$td("Oskar\u017cony jest niewinny"),
            tags$td("Telefon NIE wp\u0142ywa na koncentracj\u0119 (r\u00f3\u017cnica = 0)")
          ),
          tags$tr(
            tags$td(tags$b("H\u2081")),
            tags$td("Oskar\u017cony jest winny"),
            tags$td("Telefon OBNI\u017bA koncentracj\u0119 (r\u00f3\u017cnica > 0)")
          ),
          tags$tr(
            tags$td(tags$b("Dane")),
            tags$td("Dowody z\u0142o\u017cone w s\u0105dzie"),
            tags$td("Wyniki testu 80 student\u00f3w")
          ),
          tags$tr(
            tags$td(tags$b("p-warto\u015b\u0107")),
            tags$td("Czy takie dowody mog\u0142yby powsta\u0107 przypadkiem, gdyby oskar\u017cony by\u0142 niewinny?"),
            tags$td("Czy tak\u0105 r\u00f3\u017cnic\u0119 mogliby\u015bmy zobaczy\u0107 przypadkiem, gdyby telefon nie mia\u0142 wp\u0142ywu?")
          ),
          tags$tr(
            tags$td(tags$b("Decyzja")),
            tags$td("Skazanie (je\u015bli dowody wystarczaj\u0105ce) lub uniewinnienie"),
            tags$td("Odrzucamy H\u2080 (je\u015bli p < \u03b1) lub brak podstaw do odrzucenia")
          )
        )
      ),
      p("Je\u015bli p-warto\u015b\u0107 jest mniejsza ni\u017c przyj\u0119ty ",
        tags$b("poziom istotno\u015bci \u03b1"), " (zwykle 0.05),
        odrzucamy H\u2080 na korzy\u015b\u0107 H\u2081.")
    ),

    # ========================================================================
    # WIDGET 1: Powtorzone eksperymenty pod H0
    # ========================================================================
    div(class = "section-title", "Co to jest p-warto\u015b\u0107?"),

    div(class = "narrative",
      p("Wyobra\u017a sobie, \u017ce telefon ", tags$em("naprawd\u0119 nie ma wp\u0142ywu"),
        " na koncentracj\u0119 i powtarzamy eksperyment z nowymi lud\u017ami."),
      p("Ka\u017cdy eksperyment da inn\u0105 r\u00f3\u017cnic\u0119 \u015brednich \u2014 czasem na plus,
        czasem na minus, zwykle niewielk\u0105. Je\u015bli r\u00f3\u017cnica z prawdziwego eksperymentu
        wypada daleko od tego, co powstaje przypadkiem \u2014 mamy pow\u00f3d, by odrzuci\u0107 H\u2080.")
    ),

    div(class = "widget-block",
      h4("Powt\u00f3rzone eksperymenty pod H\u2080"),
      fluidRow(
        column(4,
          sliderInput("ch1_sim_n", "Wielko\u015b\u0107 pr\u00f3by (n na grup\u0119):",
                      min = 10, max = 100, value = 40, step = 5),
          hr(),
          div(style = "display: flex; flex-direction: column; gap: 8px;",
            actionButton("ch1_sim_10", "Powt\u00f3rz 10 razy",
                         class = "btn-primary", width = "100%"),
            actionButton("ch1_sim_200", "Powt\u00f3rz 200 razy",
                         class = "btn-warning", width = "100%"),
            actionButton("ch1_sim_reset", "Reset",
                         class = "btn-outline-secondary", width = "100%")
          ),
          br(),
          uiOutput("ch1_sim_info")
        ),
        column(8,
          plotOutput("ch1_sim_plot", height = "350px"),
          uiOutput("ch1_sim_stats")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Jak to czyta\u0107:"),
      " Ka\u017cdy s\u0142upek to r\u00f3\u017cnica \u015brednich z jednego symulowanego eksperymentu,
      w kt\u00f3rym telefon nie ma wp\u0142ywu. Czerwona linia to r\u00f3\u017cnica z prawdziwego eksperymentu.
      ", tags$b("p-warto\u015b\u0107 = jaki odsetek tych s\u0142upk\u00f3w jest co najmniej tak daleko od zera"),
      " jak nasza czerwona linia?"
    ),

    # ========================================================================
    # WIDGET 2: Bledy I i II rodzaju
    # ========================================================================
    div(class = "section-title", "B\u0142\u0119dy I i II rodzaju"),

    div(class = "narrative",
      p("Ka\u017cda decyzja mo\u017ce by\u0107 b\u0142\u0119dna:"),
      tags$table(class = "table table-bordered", style = "font-size: 15px;",
        tags$thead(
          tags$tr(tags$th(""), tags$th("H\u2080 prawdziwa"),
                  tags$th("H\u2080 fa\u0142szywa"))
        ),
        tags$tbody(
          tags$tr(
            tags$td(tags$strong("Nie odrzucamy H\u2080")),
            tags$td(style = "background: #eafaf1;", "OK"),
            tags$td(style = "background: #fdedec;", "B\u0142\u0105d II rodzaju (\u03b2)")
          ),
          tags$tr(
            tags$td(tags$strong("Odrzucamy H\u2080")),
            tags$td(style = "background: #fdedec;", "B\u0142\u0105d I rodzaju (\u03b1)"),
            tags$td(style = "background: #eafaf1;", "OK (moc = 1\u2212\u03b2)")
          )
        )
      )
    ),

    div(class = "callout-info",
      p("W naszym eksperymencie z telefonem:"),
      tags$ul(
        tags$li(tags$b("B\u0142\u0105d I rodzaju:"), " stwierdzamy, \u017ce telefon rozprasza,
          cho\u0107 ", tags$em("naprawd\u0119 nie rozprasza"), ". Fa\u0142szywy alarm."),
        tags$li(tags$b("B\u0142\u0105d II rodzaju:"), " nie wykrywamy wp\u0142ywu telefonu,
          cho\u0107 ", tags$em("naprawd\u0119 rozprasza"), ". Przegapiony efekt.")
      )
    ),

    div(style = "text-align: center; margin: 15px 0;",
      tags$img(src = "assets/type-error.jpg", style = "width: 100%; border-radius: 8px;")
    ),

    div(class = "widget-block",
      h4("Moc testu i b\u0142\u0119dy"),
      fluidRow(
        column(4,
          sliderInput("ch1_alpha", "\u03b1 (poziom istotno\u015bci):",
                      min = 0.01, max = 0.20, value = 0.05, step = 0.01)
        ),
        column(4,
          sliderInput("ch1_effect", "R\u00f3\u017cnica \u015brednich (pkt):",
                      min = 0, max = 15, value = 7, step = 1)
        ),
        column(4,
          sliderInput("ch1_power_n", "Wielko\u015b\u0107 pr\u00f3by (n na grup\u0119):",
                      min = 10, max = 200, value = 40, step = 5)
        )
      ),
      plotOutput("ch1_power_plot", height = "380px"),
      uiOutput("ch1_power_stats")
    ),

    div(class = "callout-warning",
      tags$strong("Kompromis:"),
      " Zmniejszenie \u03b1 redukuje b\u0142\u0105d I rodzaju, ale zwi\u0119ksza b\u0142\u0105d II rodzaju.
        Jedyny spos\u00f3b na zmniejszenie obu naraz: ", tags$b("zwi\u0119kszenie n"), "!"
    ),

    # ========================================================================
    # WIDGET 3: Quiz - decyzja
    # ========================================================================
    div(class = "section-title", "Decyzja w praktyce"),

    div(class = "narrative",
      p("Zn\u00f3w wracamy do kluczowej regu\u0142y: ", tags$b("p < \u03b1 \u2192 odrzucamy H\u2080"),
        ". Spr\u00f3bujcie sami na kilku scenariuszach:")
    ),

    div(class = "widget-block",
      h4("Quiz: odrzuci\u0107 czy nie?"),
      uiOutput("ch1_quiz_scenario"),
      p("Twoja decyzja:"),
      uiOutput("ch1_quiz_options"),
      uiOutput("ch1_quiz_feedback"),
      actionButton("ch1_quiz_next", "Nowy scenariusz", class = "btn-outline-secondary")
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: jak formu\u0142owa\u0107 hipotezy?"),
      actionButton("ch1_next", "Dalej \u2192 2. Formu\u0142owanie hipotez",
                   class = "btn-primary btn-lg")
    )
  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch1_server <- function(input, output, session) {

  # --- Sekcja 0: Case study ---
  ch1_case_data <- reactiveVal(NULL)

  observeEvent(input$ch1_case_generate, {
    ch1_case_data(generate_phone_data(40))
  })

  # Generuj dane na starcie
  observe({
    if (is.null(ch1_case_data())) {
      ch1_case_data(generate_phone_data(40))
    }
  })

  output$ch1_case_plot <- renderPlot({
    d <- ch1_case_data()
    if (is.null(d)) return(NULL)

    ggplot(d, aes(x = grupa, y = koncentracja, fill = grupa)) +
      geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.5) +
      geom_jitter(width = 0.15, alpha = 0.4, size = 2) +
      scale_fill_manual(values = c(col_accept, col_reject)) +
      labs(title = "Wyniki testu koncentracji",
           x = NULL, y = "Wynik (0\u2013100 pkt)") +
      theme_test() +
      theme(legend.position = "none") +
      coord_cartesian(ylim = c(20, 100))
  })

  output$ch1_case_stats <- renderUI({
    d <- ch1_case_data()
    if (is.null(d)) return(NULL)

    stats <- d %>%
      group_by(grupa) %>%
      summarise(m = round(mean(koncentracja), 1),
                s = round(sd(koncentracja), 1),
                .groups = "drop")

    diff_val <- round(stats$m[1] - stats$m[2], 1)

    tagList(
      div(class = "stat-box", style = paste0("background:", col_accept, ";"),
          paste0("Plecak: ", stats$m[1], " pkt (s=", stats$s[1], ")")),
      div(class = "stat-box", style = paste0("background:", col_reject, ";"),
          paste0("Biurko: ", stats$m[2], " pkt (s=", stats$s[2], ")")),
      div(class = "stat-box", style = paste0("background:", col_dark, ";"),
          paste0("R\u00f3\u017cnica: ", diff_val, " pkt"))
    )
  })

  # Oblicz prawdziwa roznice z case study
  ch1_observed_diff <- reactive({
    d <- ch1_case_data()
    if (is.null(d)) return(0)
    means <- tapply(d$koncentracja, d$grupa, mean)
    unname(means["Telefon w plecaku"] - means["Telefon na biurku"])
  })

  # --- Widget 1: Histogram roznic z powtorzonych eksperymentow ---
  ch1_sim_diffs <- reactiveVal(numeric(0))

  do_simulations <- function(k) {
    n <- input$ch1_sim_n
    new_diffs <- sapply(seq_len(k), function(i) {
      g1 <- rnorm(n, mean = 70, sd = 13)
      g2 <- rnorm(n, mean = 70, sd = 13)
      mean(g1) - mean(g2)
    })
    ch1_sim_diffs(c(ch1_sim_diffs(), new_diffs))
  }

  observeEvent(input$ch1_sim_10, do_simulations(10))
  observeEvent(input$ch1_sim_200, do_simulations(200))
  observeEvent(input$ch1_sim_reset, ch1_sim_diffs(numeric(0)))

  output$ch1_sim_info <- renderUI({
    n_s <- length(ch1_sim_diffs())
    obs <- round(ch1_observed_diff(), 1)
    tagList(
      div(class = "stat-box", style = paste0("background:", col_h0, ";"),
          paste0("Eksperyment\u00f3w: ", n_s)),
      div(class = "stat-box", style = paste0("background:", col_reject, ";"),
          paste0("Obs. r\u00f3\u017cnica: ", obs, " pkt"))
    )
  })

  output$ch1_sim_plot <- renderPlot({
    diffs <- ch1_sim_diffs()
    obs <- ch1_observed_diff()

    if (length(diffs) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij \u201ePowt\u00f3rz\u201d \u2014\nsymulujemy eksperymenty bez efektu",
                 size = 5, color = "#7f8c8d") +
        theme_void()
    } else {
      df <- data.frame(diff = diffs, extreme = abs(diffs) >= abs(obs))
      ggplot(df, aes(x = diff, fill = extreme)) +
        geom_histogram(bins = 30, color = "white") +
        geom_vline(xintercept = obs, color = col_reject,
                   linewidth = 1.5, linetype = "solid") +
        geom_vline(xintercept = -obs, color = col_reject,
                   linewidth = 1, linetype = "dashed") +
        scale_fill_manual(values = c("TRUE" = col_pvalue, "FALSE" = col_h0),
                          labels = c("TRUE" = "co najmniej tak skrajne",
                                     "FALSE" = "bli\u017cej zera"),
                          name = NULL) +
        labs(title = "R\u00f3\u017cnice \u015brednich z symulowanych eksperyment\u00f3w (H\u2080 prawdziwa)",
             subtitle = "Czerwona linia = prawdziwa r\u00f3\u017cnica z eksperymentu",
             x = "R\u00f3\u017cnica \u015brednich (grupa A \u2212 grupa B)", y = "Liczba") +
        theme_test() +
        theme(legend.position = "top")
    }
  })

  output$ch1_sim_stats <- renderUI({
    diffs <- ch1_sim_diffs()
    if (length(diffs) == 0) return(NULL)
    obs <- ch1_observed_diff()
    n_extreme <- sum(abs(diffs) >= abs(obs))
    pval <- n_extreme / length(diffs)
    tagList(
      div(class = "stat-box", style = paste0("background:", col_pvalue, ";"),
          paste0("p \u2248 ", round(pval, 3),
                 " (", n_extreme, "/", length(diffs), " eksperyment\u00f3w co najmniej tak skrajnych)"))
    )
  })

  # --- Widget 2: Moc testu ---
  output$ch1_power_plot <- renderPlot({
    alpha <- input$ch1_alpha
    diff_means <- input$ch1_effect  # roznica srednich w punktach
    n <- input$ch1_power_n
    sigma <- 13  # stale odchylenie std (ukryte)
    mu0 <- 70
    mu1 <- mu0 + diff_means

    x <- seq(mu0 - 4 * sigma / sqrt(n), max(mu1, mu0) + 4 * sigma / sqrt(n), length.out = 500)
    se <- sigma / sqrt(n)

    y_h0 <- dnorm(x, mean = mu0, sd = se)
    y_h1 <- dnorm(x, mean = mu1, sd = se)

    crit <- mu0 + qnorm(1 - alpha / 2) * se

    df_plot <- data.frame(
      x = rep(x, 2),
      y = c(y_h0, y_h1),
      dist = rep(c("H\u2080: brak efektu", "H\u2081: telefon rozprasza"), each = 500)
    )

    p <- ggplot(df_plot, aes(x = x, y = y, color = dist)) +
      geom_line(linewidth = 1.2) +
      geom_vline(xintercept = crit, linetype = "dashed", color = col_dark) +
      scale_color_manual(values = c(col_h0, col_h1), name = "Rozk\u0142ad") +
      labs(title = paste0("Moc testu (n=", n, " na grup\u0119, r\u00f3\u017cnica=", diff_means, " pkt, \u03b1=", alpha, ")"),
           x = "\u015arednia koncentracja w pr\u00f3bie", y = "G\u0119sto\u015b\u0107") +
      theme_test() +
      theme(legend.position = "top")

    # Shade rejection region under H0
    shade_h0 <- data.frame(x = x[x >= crit], y = y_h0[x >= crit])
    if (nrow(shade_h0) > 0) {
      p <- p + geom_area(data = shade_h0, aes(x = x, y = y),
                         fill = col_reject, alpha = 0.2, inherit.aes = FALSE)
    }

    # Shade power under H1
    shade_h1 <- data.frame(x = x[x >= crit], y = y_h1[x >= crit])
    if (nrow(shade_h1) > 0) {
      p <- p + geom_area(data = shade_h1, aes(x = x, y = y),
                         fill = col_accept, alpha = 0.2, inherit.aes = FALSE)
    }

    p
  })

  output$ch1_power_stats <- renderUI({
    alpha <- input$ch1_alpha
    diff_means <- input$ch1_effect
    n <- input$ch1_power_n
    sigma <- 13
    se <- sigma / sqrt(n)
    power <- pnorm(diff_means / se - qnorm(1 - alpha / 2))

    tagList(
      div(class = "stat-box", style = paste0("background:", col_reject, ";"),
          paste0("B\u0142\u0105d I: ", alpha * 100, "%")),
      div(class = "stat-box", style = paste0("background:", col_accept, ";"),
          paste0("Moc: ", round(power * 100, 1), "%")),
      div(class = "stat-box", style = paste0("background:", col_dark, ";"),
          paste0("B\u0142\u0105d II: ", round((1 - power) * 100, 1), "%"))
    )
  })

  # --- Widget 3: Quiz (tiles) ---
  ch1_quiz_data <- reactiveVal(NULL)
  ch1_quiz_answered <- reactiveVal(FALSE)
  ch1_quiz_selected <- reactiveVal(NULL)

  generate_quiz <- function() {
    scenarios <- list(
      list(p = 0.003, alpha = 0.05,
           context = "Badanie wp\u0142ywu kawy na czas reakcji: p = 0.003, \u03b1 = 0.05"),
      list(p = 0.12, alpha = 0.05,
           context = "Czy notatki r\u0119czne daj\u0105 lepsze wyniki ni\u017c na laptopie? p = 0.12, \u03b1 = 0.05"),
      list(p = 0.048, alpha = 0.05,
           context = "Korelacja mi\u0119dzy ilo\u015bci\u0105 snu a ocen\u0105 z egzaminu: p = 0.048, \u03b1 = 0.05"),
      list(p = 0.06, alpha = 0.01,
           context = "Czy kierunek studi\u00f3w wp\u0142ywa na zarobki po 5 latach? ANOVA: p = 0.06, \u03b1 = 0.01"),
      list(p = 0.001, alpha = 0.01,
           context = "Czy p\u0142e\u0107 wp\u0142ywa na wyb\u00f3r specjalizacji? \u03c7\u00b2: p = 0.001, \u03b1 = 0.01"),
      list(p = 0.052, alpha = 0.05,
           context = "Por\u00f3wnanie skuteczno\u015bci dw\u00f3ch metod nauki: p = 0.052, \u03b1 = 0.05")
    )
    ch1_quiz_data(scenarios[[sample(length(scenarios), 1)]])
    ch1_quiz_answered(FALSE)
    ch1_quiz_selected(NULL)
  }

  observe({ generate_quiz() })
  observeEvent(input$ch1_quiz_next, { generate_quiz() })

  output$ch1_quiz_scenario <- renderUI({
    sc <- ch1_quiz_data()
    if (is.null(sc)) return(NULL)
    div(class = "callout-info",
      p(tags$strong("Scenariusz:"), sc$context)
    )
  })

  ch1_quiz_choices <- list(
    list(letter = "A", value = "reject", text = "Odrzucamy H\u2080"),
    list(letter = "B", value = "fail_to_reject", text = "Brak podstaw do odrzucenia H\u2080")
  )

  output$ch1_quiz_options <- renderUI({
    ch1_quiz_data()
    if (ch1_quiz_answered()) return(NULL)
    div(class = "quiz-tiles quiz-cols-2",
      lapply(ch1_quiz_choices, function(opt) {
        actionButton(paste0("ch1_tile_", opt$value),
          tagList(
            div(class = "tile-letter", opt$letter),
            div(class = "tile-text", opt$text)
          ),
          class = "quiz-tile"
        )
      })
    )
  })

  observe({
    for (opt in ch1_quiz_choices) {
      local({
        val <- opt$value
        observeEvent(input[[paste0("ch1_tile_", val)]], {
          if (ch1_quiz_answered()) return()
          ch1_quiz_selected(val)
          ch1_quiz_answered(TRUE)
        }, ignoreInit = TRUE)
      })
    }
  })

  output$ch1_quiz_feedback <- renderUI({
    req(ch1_quiz_answered())
    sc <- ch1_quiz_data()
    answer <- ch1_quiz_selected()
    if (is.null(sc) || is.null(answer)) return(NULL)

    correct <- if (sc$p < sc$alpha) "reject" else "fail_to_reject"
    if (answer == correct) {
      div(class = "callout-success",
        tags$strong("Poprawnie!"),
        p(paste0("p = ", sc$p, " ", ifelse(sc$p < sc$alpha, "<", "\u2265"),
                 " \u03b1 = ", sc$alpha))
      )
    } else {
      div(class = "callout-danger",
        tags$strong("Nie! "),
        p(paste0("p = ", sc$p, " ", ifelse(sc$p < sc$alpha, "<", "\u2265"),
                 " \u03b1 = ", sc$alpha, ". Zatem: ",
                 ifelse(correct == "reject", "odrzucamy H\u2080",
                        "brak podstaw do odrzucenia H\u2080"), "."))
      )
    }
  })
}
