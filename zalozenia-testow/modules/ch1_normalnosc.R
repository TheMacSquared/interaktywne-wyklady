# ============================================================================
# CHAPTER 1: Normalnosc rozkladu
# ============================================================================

ch1_ui <- tabPanel("1. Normalność rozkładu",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Wiele testów statystycznych zakłada normalność danych.
       Jak to sprawdzić i co zrobić, gdy założenie jest naruszone?"
    ),

    div(class = "section-title", "Które metody wymagają normalności?"),

    div(class = "narrative",
      p("Założenie normalności dotyczy:"),
      tags$ul(
        tags$li(tags$b("Test t"), " (jednej próby, niezależny, parowy) — normalność danych (lub reszt)"),
        tags$li(tags$b("ANOVA"), " — normalność reszt w każdej grupie"),
        tags$li(tags$b("Korelacja Pearsona"), " — rozkład dwuwymiarowy normalny"),
        tags$li(tags$b("Regresja liniowa"), " — normalność reszt (nie danych!)")
      ),
      p(tags$b("Ważne:"), " Przy dużych próbach (n > 30) testy t i ANOVA są
        odporne na odchylenia dzięki CTG. Normalność jest kluczowa głównie przy małych próbach.")
    ),

    # ========================================================================
    # WIDGET 1: Wizualne sprawdzanie normalnosci
    # ========================================================================
    div(class = "section-title", "Wizualne sprawdzanie normalności"),

    div(class = "widget-block",
      h4("Histogram + Q-Q plot"),
      fluidRow(
        column(4,
          selectInput("ch1_dist", "Rozkład danych:",
            choices = c(
              "Normalny" = "normal",
              "Prawoskośny" = "skewed",
              "Ciężkie ogony" = "heavy_tail",
              "Dwumodalny" = "bimodal",
              "Jednostajny" = "uniform"
            ),
            selected = "normal"
          ),
          sliderInput("ch1_n", "Wielkość próby (n):",
                      min = 10, max = 200, value = 50, step = 10),
          actionButton("ch1_gen", "Generuj dane",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch1_normality_plots", height = "350px")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Jak czytać Q-Q plot:"),
      " Jeśli punkty leżą na linii — dane są normalne.
        Odchylenia na końcach — ciężkie/lekkie ogony.
        Krzywa — skośność."
    ),

    # ========================================================================
    # WIDGET 2: Testy normalnosci
    # ========================================================================
    div(class = "section-title", "Testy formalne"),

    div(class = "narrative",
      p("Dwa najczęściej używane testy:"),
      tags$ul(
        tags$li(tags$b("Shapiro-Wilk"), " — najlepszy dla n < 50, najczęściej używany"),
        tags$li(tags$b("Kolmogorov-Smirnov"), " — działa dla dowolnego n, mniej mocny")
      ),
      p(withMathJax("\\(H_0\\)"), ": dane pochodzą z rozkładu normalnego. ",
        "p < 0.05 → odrzucamy normalność.")
    ),

    div(class = "widget-block",
      h4("Shapiro-Wilk i K-S"),
      fluidRow(
        column(4,
          helpText("Używa danych z widgetu powyżej."),
          actionButton("ch1_test_norm", "Testuj normalność",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          uiOutput("ch1_norm_results")
        )
      )
    ),

    div(class = "callout-warning",
      tags$strong("Problem z testami formalnymi:"),
      " Przy dużym n test Shapiro-Wilka odrzuci normalność nawet dla
        nieistotnych odchyleń. Przy małym n nie ma mocy. ",
      tags$strong("Zawsze łącz test z wizualizacją (Q-Q plot)!")
    ),

    # ========================================================================
    # WIDGET 3: Co robic gdy naruszone?
    # ========================================================================
    div(class = "section-title", "Gdy normalność jest naruszona"),

    div(class = "narrative",
      p("Opcje:"),
      tags$ol(
        tags$li(tags$b("Zignoruj"), " — przy dużym n (> 30) testy parametryczne są odporne"),
        tags$li(tags$b("Użyj alternatywy nieparametrycznej"), " — patrz tabela"),
        tags$li(tags$b("Transformuj dane"), " — log, sqrt, Box-Cox")
      )
    ),

    div(class = "widget-block",
      h4("Efekt transformacji logarytmicznej"),
      fluidRow(
        column(4,
          helpText("Generujemy dane prawoskośne i stosujemy log()."),
          sliderInput("ch1_trans_n", "n:", min = 30, max = 200, value = 80, step = 10),
          actionButton("ch1_transform", "Generuj i transformuj",
                       class = "btn-warning", width = "100%")
        ),
        column(8,
          plotOutput("ch1_transform_plots", height = "300px"),
          uiOutput("ch1_transform_results")
        )
      )
    ),

    div(class = "callout-success",
      tags$strong("Alternatywy nieparametryczne:"),
      tags$table(class = "table table-bordered", style = "font-size: 14px;",
        tags$tbody(
          tags$tr(tags$td("Test t jednej próby"), tags$td("→ Wilcoxon jednej próby")),
          tags$tr(tags$td("Test t niezależny"), tags$td("→ Mann-Whitney U")),
          tags$tr(tags$td("Test t parowy"), tags$td("→ Wilcoxon par znakowych")),
          tags$tr(tags$td("ANOVA"), tags$td("→ Kruskal-Wallis")),
          tags$tr(tags$td("Pearson"), tags$td("→ Spearman"))
        )
      )
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: założenie jednorodnych wariancji"),
      actionButton("ch1_next", "Dalej → 2. Jednorodne wariancje",
                   class = "btn-primary btn-lg")
    )
  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch1_server <- function(input, output, session) {

  ch1_data <- reactiveVal(NULL)

  observeEvent(input$ch1_gen, {
    ch1_data(generate_test_data(input$ch1_n, input$ch1_dist))
  })

  output$ch1_normality_plots <- renderPlot({
    x <- ch1_data()
    if (is.null(x)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Generuj dane'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      df <- data.frame(x = x)

      p1 <- ggplot(df, aes(x = x)) +
        geom_histogram(aes(y = after_stat(density)), bins = 20,
                       fill = col_test, alpha = 0.6, color = "white") +
        stat_function(fun = dnorm, args = list(mean = mean(x), sd = sd(x)),
                      color = col_ok, linewidth = 1.2, linetype = "dashed") +
        labs(title = "Histogram + krzywa normalna",
             x = "Wartość", y = "Gęstość") +
        theme_educational()

      p2 <- ggplot(df, aes(sample = x)) +
        stat_qq(color = col_test, alpha = 0.6) +
        stat_qq_line(color = col_ok, linewidth = 1) +
        labs(title = "Q-Q plot",
             x = "Kwantyle teoretyczne", y = "Kwantyle próbkowe") +
        theme_educational()

      gridExtra::grid.arrange(p1, p2, ncol = 2)
    }
  })

  # --- Widget 2: Testy normalnosci ---
  output$ch1_norm_results <- renderUI({
    req(input$ch1_test_norm)
    x <- isolate(ch1_data())
    if (is.null(x)) return(div(class = "callout-warning", "Najpierw wygeneruj dane."))

    sw <- shapiro_test(data.frame(value = x), value)
    ks <- ks.test(x, "pnorm", mean = mean(x), sd = sd(x))

    sw_color <- if (sw$p >= 0.05) col_ok else col_fail
    ks_color <- if (ks$p.value >= 0.05) col_ok else col_fail

    tagList(
      div(class = "callout-info",
        fluidRow(
          column(6,
            p(tags$strong("Shapiro-Wilk:")),
            p(paste0("W = ", round(sw$statistic, 4))),
            p(paste0("p = ", format.pval(sw$p, digits = 4))),
            p(style = paste0("color:", sw_color, "; font-weight: bold;"),
              if (sw$p >= 0.05) "Brak podstaw do odrzucenia normalności"
              else "Normalność odrzucona!")
          ),
          column(6,
            p(tags$strong("Kolmogorov-Smirnov:")),
            p(paste0("D = ", round(ks$statistic, 4))),
            p(paste0("p = ", format.pval(ks$p.value, digits = 4))),
            p(style = paste0("color:", ks_color, "; font-weight: bold;"),
              if (ks$p.value >= 0.05) "Brak podstaw do odrzucenia normalności"
              else "Normalność odrzucona!")
          )
        )
      )
    )
  })

  # --- Widget 3: Transformacja ---
  ch1_trans_data <- reactiveVal(NULL)

  observeEvent(input$ch1_transform, {
    x <- rgamma(input$ch1_trans_n, shape = 2, scale = 5) + 1
    ch1_trans_data(x)
  })

  output$ch1_transform_plots <- renderPlot({
    x <- ch1_trans_data()
    if (is.null(x)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Generuj i transformuj'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      log_x <- log(x)

      p1 <- ggplot(data.frame(x = x), aes(sample = x)) +
        stat_qq(color = col_fail, alpha = 0.5) +
        stat_qq_line(color = col_fail) +
        labs(title = "Oryginalne (prawoskośne)") +
        theme_educational()

      p2 <- ggplot(data.frame(x = log_x), aes(sample = x)) +
        stat_qq(color = col_ok, alpha = 0.5) +
        stat_qq_line(color = col_ok) +
        labs(title = "Po log()") +
        theme_educational()

      gridExtra::grid.arrange(p1, p2, ncol = 2)
    }
  })

  output$ch1_transform_results <- renderUI({
    x <- ch1_trans_data()
    if (is.null(x)) return(NULL)

    sw_orig <- shapiro_test(data.frame(value = x), value)
    sw_log <- shapiro_test(data.frame(value = log(x)), value)

    tagList(
      div(class = "stat-box",
          style = paste0("background:", if (sw_orig$p >= 0.05) col_ok else col_fail, ";"),
          paste0("Oryginalne: p = ", format.pval(sw_orig$p, digits = 3))),
      div(class = "stat-box",
          style = paste0("background:", if (sw_log$p >= 0.05) col_ok else col_fail, ";"),
          paste0("Po log(): p = ", format.pval(sw_log$p, digits = 3)))
    )
  })
}
