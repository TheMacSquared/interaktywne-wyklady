# ============================================================================
# CHAPTER 1: Logika testowania hipotez
# ============================================================================

ch1_ui <- tabPanel("1. Logika testowania",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Przedzia\u0142y ufno\u015bci mierz\u0105 niepewno\u015b\u0107 estymacji.
       Teraz przejdziemy do pytania: czy zaobserwowany efekt jest rzeczywisty?"
    ),

    div(class = "section-title", "Testowanie hipotez \u2014 logika rozumowania"),

    div(class = "narrative",
      p("Testowanie hipotez statystycznych przypomina proces s\u0105dowy:"),
      tags$ul(
        tags$li(tags$b("H\u2080 (hipoteza zerowa)"), " = \"oskar\u017cony jest niewinny\" \u2014 brak efektu, brak r\u00f3\u017cnicy"),
        tags$li(tags$b("H\u2081 (hipoteza alternatywna)"), " = \"oskar\u017cony jest winny\" \u2014 efekt istnieje"),
        tags$li(tags$b("Dane"), " = dowody przedstawione w s\u0105dzie"),
        tags$li(tags$b("p-warto\u015b\u0107"), " = jak bardzo dane s\u0105 zaskakuj\u0105ce, je\u015bli H\u2080 jest prawdziwa")
      ),
      p("Je\u015bli p-warto\u015b\u0107 jest mniejsza ni\u017c przyj\u0119ty ",
        tags$b("poziom istotno\u015bci \u03b1"), " (zwykle 0.05),
        odrzucamy H\u2080 na korzy\u015b\u0107 H\u2081.")
    ),

    # ========================================================================
    # WIDGET 1: Symulacja p-wartosci
    # ========================================================================
    div(class = "section-title", "Co to jest p-warto\u015b\u0107?"),

    div(class = "narrative",
      p("P-warto\u015b\u0107 to prawdopodobie\u0144stwo uzyskania wyniku ",
        tags$b("co najmniej tak skrajnego"), " jak zaobserwowany,
        ", tags$b("zak\u0142adaj\u0105c, \u017ce H\u2080 jest prawdziwa"), "."),
      p("Zobaczmy to na symulacji: losujemy pr\u00f3by z populacji, w kt\u00f3rej
        H\u2080 jest prawdziwa, i obserwujemy rozk\u0142ad statystyki testowej.")
    ),

    div(class = "widget-block",
      h4("Symulacja pod H\u2080"),
      fluidRow(
        column(4,
          sliderInput("ch1_n", "Wielko\u015b\u0107 pr\u00f3by (n):",
                      min = 10, max = 100, value = 30, step = 5),
          helpText("Populacja: N(\u03bc=170, \u03c3=10). Testujemy H\u2080: \u03bc = 170."),
          hr(),
          div(style = "display: flex; flex-direction: column; gap: 8px;",
            actionButton("ch1_draw_1", "Pobierz 1 pr\u00f3b\u0119",
                         class = "btn-primary", width = "100%"),
            actionButton("ch1_draw_100", "Pobierz 100 pr\u00f3b",
                         class = "btn-warning", width = "100%"),
            actionButton("ch1_reset", "Reset",
                         class = "btn-outline-secondary", width = "100%")
          ),
          br(),
          uiOutput("ch1_sim_info")
        ),
        column(8,
          plotOutput("ch1_pval_plot", height = "350px"),
          uiOutput("ch1_pval_stats")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Obserwacja:"),
      " Gdy H\u2080 jest prawdziwa, p-warto\u015bci maj\u0105 rozk\u0142ad ",
      tags$b("jednostajny [0, 1]"), ". Oko\u0142o 5% pr\u00f3b da p < 0.05 \u2014
      to s\u0105 fa\u0142szywe alarmy (b\u0142\u0105d I rodzaju)!"
    ),

    # ========================================================================
    # WIDGET 2: Bledy I i II rodzaju
    # ========================================================================
    div(class = "section-title", "B\u0142\u0119dy I i II rodzaju"),

    div(class = "narrative",
      p("Ka\u017cda decyzja mo\u017ce by\u0107 b\u0142\u0119dna:"),
      tags$table(class = "table table-bordered", style = "font-size: 15px;",
        tags$thead(
          tags$tr(tags$th(""), tags$th("H\u2080 prawdziwa"), tags$th("H\u2080 fa\u0142szywa"))
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

    div(class = "widget-block",
      h4("Moc testu i b\u0142\u0119dy"),
      fluidRow(
        column(4,
          sliderInput("ch1_alpha", "\u03b1 (poziom istotno\u015bci):",
                      min = 0.01, max = 0.20, value = 0.05, step = 0.01),
          sliderInput("ch1_effect", "Wielko\u015b\u0107 efektu (d):",
                      min = 0, max = 2, value = 0.5, step = 0.1),
          sliderInput("ch1_power_n", "Wielko\u015b\u0107 pr\u00f3by (n):",
                      min = 10, max = 200, value = 30, step = 5)
        ),
        column(8,
          plotOutput("ch1_power_plot", height = "350px"),
          uiOutput("ch1_power_stats")
        )
      )
    ),

    div(class = "callout-warning",
      tags$strong("Kompromis:"),
      " Zmniejszenie \u03b1 redukuje b\u0142\u0105d I rodzaju, ale zwi\u0119ksza b\u0142\u0105d II rodzaju.
        Jedyny spos\u00f3b na zmniejszenie obu: zwi\u0119kszenie n!"
    ),

    # ========================================================================
    # WIDGET 3: Quiz - decyzja
    # ========================================================================
    div(class = "section-title", "Decyzja w praktyce"),

    div(class = "widget-block",
      h4("Quiz: odrzuci\u0107 czy nie?"),
      uiOutput("ch1_quiz_scenario"),
      radioButtons("ch1_quiz_answer", "Twoja decyzja:",
        choices = c(
          "Odrzucamy H\u2080" = "reject",
          "Brak podstaw do odrzucenia H\u2080" = "fail_to_reject"
        ),
        selected = character(0)
      ),
      actionButton("ch1_quiz_check", "Sprawd\u017a", class = "btn-primary"),
      actionButton("ch1_quiz_next", "Nowy scenariusz", class = "btn-outline-secondary"),
      br(), br(),
      uiOutput("ch1_quiz_feedback")
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

  # --- Widget 1: Symulacja p-wartosci ---
  ch1_pvalues <- reactiveVal(numeric(0))

  draw_pvalues <- function(k) {
    n <- input$ch1_n
    new_p <- sapply(seq_len(k), function(i) {
      samp <- rnorm(n, mean = 170, sd = 10)
      t.test(samp, mu = 170)$p.value
    })
    ch1_pvalues(c(ch1_pvalues(), new_p))
  }

  observeEvent(input$ch1_draw_1, draw_pvalues(1))
  observeEvent(input$ch1_draw_100, draw_pvalues(100))
  observeEvent(input$ch1_reset, ch1_pvalues(numeric(0)))

  output$ch1_sim_info <- renderUI({
    n_p <- length(ch1_pvalues())
    div(class = "stat-box", style = paste0("background:", col_h0, ";"),
        paste0("Pr\u00f3b: ", n_p))
  })

  output$ch1_pval_plot <- renderPlot({
    pvals <- ch1_pvalues()
    if (length(pvals) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Pobierz pr\u00f3b\u0119'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      df <- data.frame(p = pvals, significant = pvals < 0.05)
      ggplot(df, aes(x = p, fill = significant)) +
        geom_histogram(breaks = seq(0, 1, by = 0.05), color = "white") +
        geom_vline(xintercept = 0.05, color = col_reject, linewidth = 1.2,
                   linetype = "dashed") +
        scale_fill_manual(values = c("TRUE" = col_reject, "FALSE" = col_h0),
                          labels = c("TRUE" = "p < 0.05", "FALSE" = "p \u2265 0.05"),
                          name = NULL) +
        labs(title = "Rozk\u0142ad p-warto\u015bci (H\u2080 prawdziwa!)",
             x = "p-warto\u015b\u0107", y = "Liczba") +
        theme_test() +
        theme(legend.position = "top")
    }
  })

  output$ch1_pval_stats <- renderUI({
    pvals <- ch1_pvalues()
    if (length(pvals) == 0) return(NULL)
    n_sig <- sum(pvals < 0.05)
    pct <- round(n_sig / length(pvals) * 100, 1)
    tagList(
      div(class = "stat-box", style = paste0("background:", col_reject, ";"),
          paste0("p < 0.05: ", n_sig, " (", pct, "%)")),
      div(class = "stat-box", style = paste0("background:", col_accept, ";"),
          paste0("p \u2265 0.05: ", length(pvals) - n_sig)),
      div(class = "stat-box", style = paste0("background:", col_dark, ";"),
          paste0("Oczekiwane \u2248 5%"))
    )
  })

  # --- Widget 2: Moc testu ---
  output$ch1_power_plot <- renderPlot({
    alpha <- input$ch1_alpha
    d <- input$ch1_effect
    n <- input$ch1_power_n
    sigma <- 10
    mu0 <- 170
    mu1 <- mu0 + d * sigma  # efekt w oryginalnej skali

    x <- seq(mu0 - 4 * sigma / sqrt(n), mu1 + 4 * sigma / sqrt(n), length.out = 500)
    se <- sigma / sqrt(n)

    y_h0 <- dnorm(x, mean = mu0, sd = se)
    y_h1 <- dnorm(x, mean = mu1, sd = se)

    crit <- mu0 + qnorm(1 - alpha / 2) * se

    df_plot <- data.frame(
      x = rep(x, 2),
      y = c(y_h0, y_h1),
      dist = rep(c("H\u2080", "H\u2081"), each = 500)
    )

    p <- ggplot(df_plot, aes(x = x, y = y, color = dist)) +
      geom_line(linewidth = 1.2) +
      geom_vline(xintercept = crit, linetype = "dashed", color = col_dark) +
      scale_color_manual(values = c(col_h0, col_h1), name = "Rozk\u0142ad") +
      labs(title = paste0("Moc testu (n=", n, ", d=", d, ", \u03b1=", alpha, ")"),
           x = expression(bar(x)), y = "G\u0119sto\u015b\u0107") +
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
    d <- input$ch1_effect
    n <- input$ch1_power_n
    sigma <- 10
    se <- sigma / sqrt(n)
    crit <- qnorm(1 - alpha / 2) * se
    mu1_offset <- d * sigma
    power <- pnorm(mu1_offset / se - qnorm(1 - alpha / 2))

    tagList(
      div(class = "stat-box", style = paste0("background:", col_reject, ";"),
          paste0("B\u0142\u0105d I: ", alpha * 100, "%")),
      div(class = "stat-box", style = paste0("background:", col_accept, ";"),
          paste0("Moc: ", round(power * 100, 1), "%")),
      div(class = "stat-box", style = paste0("background:", col_dark, ";"),
          paste0("B\u0142\u0105d II: ", round((1 - power) * 100, 1), "%"))
    )
  })

  # --- Widget 3: Quiz ---
  ch1_quiz_data <- reactiveVal(NULL)

  generate_quiz <- function() {
    scenarios <- list(
      list(p = 0.003, alpha = 0.05, context = "Test t dla \u015bredniej wzrostu: p = 0.003, \u03b1 = 0.05"),
      list(p = 0.12, alpha = 0.05, context = "Test chi-kwadrat: p = 0.12, \u03b1 = 0.05"),
      list(p = 0.048, alpha = 0.05, context = "Test korelacji Pearsona: p = 0.048, \u03b1 = 0.05"),
      list(p = 0.06, alpha = 0.01, context = "ANOVA: p = 0.06, \u03b1 = 0.01"),
      list(p = 0.001, alpha = 0.01, context = "Test Wilcoxona: p = 0.001, \u03b1 = 0.01"),
      list(p = 0.052, alpha = 0.05, context = "Test t niezale\u017cny: p = 0.052, \u03b1 = 0.05")
    )
    ch1_quiz_data(scenarios[[sample(length(scenarios), 1)]])
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

  output$ch1_quiz_feedback <- renderUI({
    req(input$ch1_quiz_check)
    isolate({
      sc <- ch1_quiz_data()
      answer <- input$ch1_quiz_answer
      if (is.null(sc) || is.null(answer) || answer == "") return(NULL)

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
  })
}
