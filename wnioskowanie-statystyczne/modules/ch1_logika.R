# ============================================================================
# CHAPTER 1: Logika testowania hipotez
# ============================================================================

ch1_ui <- tabPanel("1. Logika testowania",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Przedziały ufności mierzą niepewność estymacji.
       Teraz przejdziemy do pytania: czy zaobserwowany efekt jest rzeczywisty?"
    ),

    # ========================================================================
    # SEKCJA 0: Case study otwierajacy
    # ========================================================================
    div(class = "section-title", "Czy telefon na biurku obniża koncentrację?"),

    div(class = "narrative",
      p("Wyobraźci sobie następujący eksperyment na waszej uczelni
        (inspirowany badaniem Ward et al., 2017):"),
      tags$ul(
        tags$li("80 studentów losowo przydzielonych do dwóch grup po 40 osób"),
        tags$li(tags$b("Grupa A:"), " telefon schowany w plecaku"),
        tags$li(tags$b("Grupa B:"), " telefon leży na biurku (ekranem w dół, wyciszony)"),
        tags$li("Wszyscy rozwiązują ten sam test koncentracji (0–100 punktów)")
      ),
      p("Nikt nie używa telefonu. Różnica jest ",
        tags$em("tylko"), " w tym, czy telefon leży w zasięgu wzroku.")
    ),

    div(class = "widget-block",
      h4("Wyniki eksperymentu"),
      fluidRow(
        column(4,
          actionButton("ch1_case_generate", "Przeprowadź eksperyment",
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
      p(tags$b("Pytanie kluczowe:"), " Średnia w grupie „biurko” jest niższa.
        Ale czy to nie może być przypadek?"),
      p("Może gdybyśmy powtórzyli eksperyment z innymi 80 osobami,
        różnica byłaby w drugą stronę? Może po prostu trafiliśmy na
        słabszych studentów w grupie B?"),
      p("Właśnie do tego służy testowanie hipotez — daje nam narzędzie do odpowiedzi na pytanie:
        czy obserwowana różnica jest zbyt duża, żeby być przypadkiem?")
    ),

    # ========================================================================
    # SEKCJA 1: Logika testowania z odniesieniem do case study
    # ========================================================================
    div(class = "section-title", "Testowanie hipotez — logika rozumowania"),

    div(class = "narrative",
      p("Testowanie hipotez statystycznych przypomina proces sądowy:"),
      tags$table(class = "table table-bordered", style = "font-size: 15px;",
        tags$thead(
          tags$tr(tags$th("Element"), tags$th("Sąd"), tags$th("Nasz eksperyment z telefonem"))
        ),
        tags$tbody(
          tags$tr(
            tags$td(tags$b("H₀")),
            tags$td("Oskarżony jest niewinny"),
            tags$td("Telefon NIE wpływa na koncentrację (różnica = 0)")
          ),
          tags$tr(
            tags$td(tags$b("H₁")),
            tags$td("Oskarżony jest winny"),
            tags$td("Telefon OBNIŻA koncentrację (różnica > 0)")
          ),
          tags$tr(
            tags$td(tags$b("Dane")),
            tags$td("Dowody złożone w sądzie"),
            tags$td("Wyniki testu 80 studentów")
          ),
          tags$tr(
            tags$td(tags$b("p-wartość")),
            tags$td("Czy takie dowody mogłyby powstać przypadkiem, gdyby oskarżony był niewinny?"),
            tags$td("Czy taką różnicę moglibyśmy zobaczyć przypadkiem, gdyby telefon nie miał wpływu?")
          ),
          tags$tr(
            tags$td(tags$b("Decyzja")),
            tags$td("Skazanie (jeśli dowody wystarczające) lub uniewinnienie"),
            tags$td("Odrzucamy H₀ (jeśli p < α) lub brak podstaw do odrzucenia")
          )
        )
      ),
      p("Jeśli p-wartość jest mniejsza niż przyjęty ",
        tags$b("poziom istotności α"), " (zwykle 0.05),
        odrzucamy H₀ na korzyść H₁.")
    ),

    # ========================================================================
    # WIDGET 1: Powtorzone eksperymenty pod H0
    # ========================================================================
    div(class = "section-title", "Co to jest p-wartość?"),

    div(class = "narrative",
      p("Wyobraź sobie, że telefon ", tags$em("naprawdę nie ma wpływu"),
        " na koncentrację i powtarzamy eksperyment z nowymi ludźmi."),
      p("Każdy eksperyment da inną różnicę średnich — czasem na plus,
        czasem na minus, zwykle niewielką. Jeśli różnica z prawdziwego eksperymentu
        wypada daleko od tego, co powstaje przypadkiem — mamy powód, by odrzucić H₀.")
    ),

    div(class = "widget-block",
      h4("Powtórzone eksperymenty pod H₀"),
      fluidRow(
        column(4,
          sliderInput("ch1_sim_n", "Wielkość próby (n na grupę):",
                      min = 10, max = 100, value = 40, step = 5),
          hr(),
          div(style = "display: flex; flex-direction: column; gap: 8px;",
            actionButton("ch1_sim_10", "Powtórz 10 razy",
                         class = "btn-primary", width = "100%"),
            actionButton("ch1_sim_200", "Powtórz 200 razy",
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
      tags$strong("Jak to czytać:"),
      " Każdy słupek to różnica średnich z jednego symulowanego eksperymentu,
      w którym telefon nie ma wpływu. Czerwona linia to różnica z prawdziwego eksperymentu.
      p-wartość = jaki odsetek tych słupków jest co najmniej tak daleko od zera jak nasza czerwona linia?"
    ),

    # ========================================================================
    # WIDGET 2: Bledy I i II rodzaju
    # ========================================================================
    div(class = "section-title", "Błędy I i II rodzaju"),

    div(class = "narrative",
      p("Każda decyzja może być błędna:"),
      tags$table(class = "table table-bordered", style = "font-size: 15px;",
        tags$thead(
          tags$tr(tags$th(""), tags$th("H₀ prawdziwa"),
                  tags$th("H₀ fałszywa"))
        ),
        tags$tbody(
          tags$tr(
            tags$td(tags$strong("Nie odrzucamy H₀")),
            tags$td(style = "background: var(--upwr-sage-tint);", "OK"),
            tags$td(style = "background: var(--upwr-accent-tint);", "Błąd II rodzaju (β)")
          ),
          tags$tr(
            tags$td(tags$strong("Odrzucamy H₀")),
            tags$td(style = "background: var(--upwr-accent-tint);", "Błąd I rodzaju (α)"),
            tags$td(style = "background: var(--upwr-sage-tint);", "OK (moc = 1−β)")
          )
        )
      )
    ),

    div(class = "callout-info",
      p("W naszym eksperymencie z telefonem:"),
      tags$ul(
        tags$li(tags$b("Błąd I rodzaju:"), " stwierdzamy, że telefon rozprasza,
          choć ", tags$em("naprawdę nie rozprasza"), ". Fałszywy alarm."),
        tags$li(tags$b("Błąd II rodzaju:"), " nie wykrywamy wpływu telefonu,
          choć ", tags$em("naprawdę rozprasza"), ". Przegapiony efekt.")
      )
    ),

    div(style = "text-align: center; margin: 15px 0;",
      tags$img(src = "assets/type-error.jpg", style = "width: 100%; border-radius: 8px;")
    ),

    div(class = "widget-block",
      h4("Moc testu i błędy"),
      fluidRow(
        column(4,
          sliderInput("ch1_alpha", "α (poziom istotności):",
                      min = 0.01, max = 0.20, value = 0.05, step = 0.01)
        ),
        column(4,
          sliderInput("ch1_effect", "Różnica średnich (pkt):",
                      min = 0, max = 15, value = 7, step = 1)
        ),
        column(4,
          sliderInput("ch1_power_n", "Wielkość próby (n na grupę):",
                      min = 10, max = 200, value = 40, step = 5)
        )
      ),
      plotOutput("ch1_power_plot", height = "380px"),
      uiOutput("ch1_power_stats")
    ),

    div(class = "callout-warning",
      tags$strong("Kompromis:"),
      " Zmniejszenie α redukuje błąd I rodzaju, ale zwiększa błąd II rodzaju.
        Jedyny sposób na zmniejszenie obu naraz: ", tags$b("zwiększenie n"), "!"
    ),

    # ========================================================================
    # WIDGET 3: Quiz - decyzja
    # ========================================================================
    div(class = "section-title", "Decyzja w praktyce"),

    div(class = "narrative",
      p("Znów wracamy do kluczowej reguły: ", tags$b("p < α → odrzucamy H₀"),
        ". Spróbujcie sami na kilku scenariuszach:")
    ),

    div(class = "widget-block",
      h4("Quiz: odrzucić czy nie?"),
      uiOutput("ch1_quiz_scenario"),
      p("Twoja decyzja:"),
      uiOutput("ch1_quiz_options"),
      uiOutput("ch1_quiz_feedback"),
      actionButton("ch1_quiz_next", "Nowy scenariusz", class = "btn-outline-secondary")
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: jak formułować hipotezy?"),
      actionButton("ch1_next", "Dalej → 2. Formułowanie hipotez",
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
           x = NULL, y = "Wynik (0–100 pkt)") +
      theme_educational() +
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
          paste0("Różnica: ", diff_val, " pkt"))
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
          paste0("Eksperymentów: ", n_s)),
      div(class = "stat-box", style = paste0("background:", col_reject, ";"),
          paste0("Obs. różnica: ", obs, " pkt"))
    )
  })

  output$ch1_sim_plot <- renderPlot({
    diffs <- ch1_sim_diffs()
    obs <- ch1_observed_diff()

    if (length(diffs) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij „Powtórz” —\nsymulujemy eksperymenty bez efektu",
                 size = 5, color = upwr_reference) +
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
                                     "FALSE" = "bliżej zera"),
                          name = NULL) +
        labs(title = "Różnice średnich z symulowanych eksperymentów (H₀ prawdziwa)",
             subtitle = "Czerwona linia = prawdziwa różnica z eksperymentu",
             x = "Różnica średnich (grupa A − grupa B)", y = "Liczba") +
        theme_educational() +
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
          paste0("p ≈ ", round(pval, 3),
                 " (", n_extreme, "/", length(diffs), " eksperymentów co najmniej tak skrajnych)"))
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
      dist = rep(c("H₀: brak efektu", "H₁: telefon rozprasza"), each = 500)
    )

    p <- ggplot(df_plot, aes(x = x, y = y, color = dist)) +
      geom_line(linewidth = 1.2) +
      geom_vline(xintercept = crit, linetype = "dashed", color = col_dark) +
      scale_color_manual(values = c(col_h0, col_h1), name = "Rozkład") +
      labs(title = paste0("Moc testu (n=", n, " na grupę, różnica=", diff_means, " pkt, α=", alpha, ")"),
           x = "Średnia koncentracja w próbie", y = "Gęstość") +
      theme_educational() +
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
          paste0("Błąd I: ", alpha * 100, "%")),
      div(class = "stat-box", style = paste0("background:", col_accept, ";"),
          paste0("Moc: ", round(power * 100, 1), "%")),
      div(class = "stat-box", style = paste0("background:", col_dark, ";"),
          paste0("Błąd II: ", round((1 - power) * 100, 1), "%"))
    )
  })

  # --- Widget 3: Quiz (tiles) ---
  ch1_quiz_data <- reactiveVal(NULL)
  ch1_quiz_answered <- reactiveVal(FALSE)
  ch1_quiz_selected <- reactiveVal(NULL)

  generate_quiz <- function() {
    scenarios <- list(
      list(p = 0.003, alpha = 0.05,
           context = "Badanie wpływu kawy na czas reakcji: p = 0.003, α = 0.05"),
      list(p = 0.12, alpha = 0.05,
           context = "Czy notatki ręczne dają lepsze wyniki niż na laptopie? p = 0.12, α = 0.05"),
      list(p = 0.048, alpha = 0.05,
           context = "Korelacja między ilością snu a oceną z egzaminu: p = 0.048, α = 0.05"),
      list(p = 0.06, alpha = 0.01,
           context = "Czy kierunek studiów wpływa na zarobki po 5 latach? ANOVA: p = 0.06, α = 0.01"),
      list(p = 0.001, alpha = 0.01,
           context = "Czy płeć wpływa na wybór specjalizacji? χ²: p = 0.001, α = 0.01"),
      list(p = 0.052, alpha = 0.05,
           context = "Porównanie skuteczności dwóch metod nauki: p = 0.052, α = 0.05")
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
    list(letter = "A", value = "reject", text = "Odrzucamy H₀"),
    list(letter = "B", value = "fail_to_reject", text = "Brak podstaw do odrzucenia H₀")
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
        p(paste0("p = ", sc$p, " ", ifelse(sc$p < sc$alpha, "<", "≥"),
                 " α = ", sc$alpha))
      )
    } else {
      div(class = "callout-danger",
        tags$strong("Nie! "),
        p(paste0("p = ", sc$p, " ", ifelse(sc$p < sc$alpha, "<", "≥"),
                 " α = ", sc$alpha, ". Zatem: ",
                 ifelse(correct == "reject", "odrzucamy H₀",
                        "brak podstaw do odrzucenia H₀"), "."))
      )
    }
  })
}
