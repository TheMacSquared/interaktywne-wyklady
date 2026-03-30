# ============================================================================
# CHAPTER 1: Normalnosc rozkladu
# ============================================================================

ch1_ui <- tabPanel("1. Normalno\u015b\u0107 rozk\u0142adu",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Wiele test\u00f3w statystycznych zak\u0142ada normalno\u015b\u0107 danych.
       Jak to sprawdzi\u0107 i co zrobi\u0107, gdy za\u0142o\u017cenie jest naruszone?"
    ),

    div(class = "section-title", "Kt\u00f3re metody wymagaj\u0105 normalno\u015bci?"),

    div(class = "narrative",
      p("Za\u0142o\u017cenie normalno\u015bci dotyczy:"),
      tags$ul(
        tags$li(tags$b("Test t"), " (jednej pr\u00f3by, niezale\u017cny, parowy) \u2014 normalno\u015b\u0107 danych (lub reszt)"),
        tags$li(tags$b("ANOVA"), " \u2014 normalno\u015b\u0107 reszt w ka\u017cdej grupie"),
        tags$li(tags$b("Korelacja Pearsona"), " \u2014 rozk\u0142ad dwuwymiarowy normalny"),
        tags$li(tags$b("Regresja liniowa"), " \u2014 normalno\u015b\u0107 reszt (nie danych!)")
      ),
      p(tags$b("Wa\u017cne:"), " Przy du\u017cych pr\u00f3bach (n > 30) testy t i ANOVA s\u0105
        odporne na odchylenia dzi\u0119ki CTG. Normalno\u015b\u0107 jest kluczowa g\u0142\u00f3wnie przy ma\u0142ych pr\u00f3bach.")
    ),

    # ========================================================================
    # WIDGET 1: Wizualne sprawdzanie normalnosci
    # ========================================================================
    div(class = "section-title", "Wizualne sprawdzanie normalno\u015bci"),

    div(class = "widget-block",
      h4("Histogram + Q-Q plot"),
      fluidRow(
        column(4,
          selectInput("ch1_dist", "Rozk\u0142ad danych:",
            choices = c(
              "Normalny" = "normal",
              "Prawosko\u015bny" = "skewed",
              "Ci\u0119\u017ckie ogony" = "heavy_tail",
              "Dwumodalny" = "bimodal",
              "Jednostajny" = "uniform"
            ),
            selected = "normal"
          ),
          sliderInput("ch1_n", "Wielko\u015b\u0107 pr\u00f3by (n):",
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
      tags$strong("Jak czyta\u0107 Q-Q plot:"),
      " Je\u015bli punkty le\u017c\u0105 na linii \u2014 dane s\u0105 normalne.
        Odchylenia na ko\u0144cach \u2014 ci\u0119\u017ckie/lekkie ogony.
        Krzywa \u2014 sko\u015bno\u015b\u0107."
    ),

    # ========================================================================
    # WIDGET 2: Testy normalnosci
    # ========================================================================
    div(class = "section-title", "Testy formalne"),

    div(class = "narrative",
      p("Dwa najcz\u0119\u015bciej u\u017cywane testy:"),
      tags$ul(
        tags$li(tags$b("Shapiro-Wilk"), " \u2014 najlepszy dla n < 50, najcz\u0119\u015bciej u\u017cywany"),
        tags$li(tags$b("Kolmogorov-Smirnov"), " \u2014 dzia\u0142a dla dowolnego n, mniej mocny")
      ),
      p(withMathJax("\\(H_0\\)"), ": dane pochodz\u0105 z rozk\u0142adu normalnego. ",
        "p < 0.05 \u2192 odrzucamy normalno\u015b\u0107.")
    ),

    div(class = "widget-block",
      h4("Shapiro-Wilk i K-S"),
      fluidRow(
        column(4,
          helpText("U\u017cywa danych z widgetu powy\u017cej."),
          actionButton("ch1_test_norm", "Testuj normalno\u015b\u0107",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          uiOutput("ch1_norm_results")
        )
      )
    ),

    div(class = "callout-warning",
      tags$strong("Problem z testami formalnymi:"),
      " Przy du\u017cym n test Shapiro-Wilka odrzuci normalno\u015b\u0107 nawet dla
        nieistotnych odchyle\u0144. Przy ma\u0142ym n nie ma mocy.
        ", tags$b("Zawsze \u0142\u0105cz test z wizualizacj\u0105 (Q-Q plot)!"))
    ,

    # ========================================================================
    # WIDGET 3: Co robic gdy naruszone?
    # ========================================================================
    div(class = "section-title", "Gdy normalno\u015b\u0107 jest naruszona"),

    div(class = "narrative",
      p("Opcje:"),
      tags$ol(
        tags$li(tags$b("Zignoruj"), " \u2014 przy du\u017cym n (> 30) testy parametryczne s\u0105 odporne"),
        tags$li(tags$b("U\u017cyj alternatywy nieparametrycznej"), " \u2014 patrz tabela"),
        tags$li(tags$b("Transformuj dane"), " \u2014 log, sqrt, Box-Cox")
      )
    ),

    div(class = "widget-block",
      h4("Efekt transformacji logarytmicznej"),
      fluidRow(
        column(4,
          helpText("Generujemy dane prawosko\u015bne i stosujemy log()."),
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
          tags$tr(tags$td("Test t jednej pr\u00f3by"), tags$td("\u2192 Wilcoxon jednej pr\u00f3by")),
          tags$tr(tags$td("Test t niezale\u017cny"), tags$td("\u2192 Mann-Whitney U")),
          tags$tr(tags$td("Test t parowy"), tags$td("\u2192 Wilcoxon par znakowych")),
          tags$tr(tags$td("ANOVA"), tags$td("\u2192 Kruskal-Wallis")),
          tags$tr(tags$td("Pearson"), tags$td("\u2192 Spearman"))
        )
      )
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: za\u0142o\u017cenie jednorodnych wariancji"),
      actionButton("ch1_next", "Dalej \u2192 2. Jednorodne wariancje",
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
             x = "Warto\u015b\u0107", y = "G\u0119sto\u015b\u0107") +
        theme_assumptions()

      p2 <- ggplot(df, aes(sample = x)) +
        stat_qq(color = col_test, alpha = 0.6) +
        stat_qq_line(color = col_ok, linewidth = 1) +
        labs(title = "Q-Q plot",
             x = "Kwantyle teoretyczne", y = "Kwantyle pr\u00f3bkowe") +
        theme_assumptions()

      gridExtra::grid.arrange(p1, p2, ncol = 2)
    }
  })

  # --- Widget 2: Testy normalnosci ---
  output$ch1_norm_results <- renderUI({
    req(input$ch1_test_norm)
    x <- isolate(ch1_data())
    if (is.null(x)) return(div(class = "callout-warning", "Najpierw wygeneruj dane."))

    sw <- shapiro.test(x)
    ks <- ks.test(x, "pnorm", mean = mean(x), sd = sd(x))

    sw_color <- if (sw$p.value >= 0.05) col_ok else col_fail
    ks_color <- if (ks$p.value >= 0.05) col_ok else col_fail

    tagList(
      div(class = "callout-info",
        fluidRow(
          column(6,
            p(tags$strong("Shapiro-Wilk:")),
            p(paste0("W = ", round(sw$statistic, 4))),
            p(paste0("p = ", format.pval(sw$p.value, digits = 4))),
            p(style = paste0("color:", sw_color, "; font-weight: bold;"),
              if (sw$p.value >= 0.05) "Brak podstaw do odrzucenia normalno\u015bci"
              else "Normalno\u015b\u0107 odrzucona!")
          ),
          column(6,
            p(tags$strong("Kolmogorov-Smirnov:")),
            p(paste0("D = ", round(ks$statistic, 4))),
            p(paste0("p = ", format.pval(ks$p.value, digits = 4))),
            p(style = paste0("color:", ks_color, "; font-weight: bold;"),
              if (ks$p.value >= 0.05) "Brak podstaw do odrzucenia normalno\u015bci"
              else "Normalno\u015b\u0107 odrzucona!")
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
        labs(title = "Oryginalne (prawosko\u015bne)") +
        theme_assumptions()

      p2 <- ggplot(data.frame(x = log_x), aes(sample = x)) +
        stat_qq(color = col_ok, alpha = 0.5) +
        stat_qq_line(color = col_ok) +
        labs(title = "Po log()") +
        theme_assumptions()

      gridExtra::grid.arrange(p1, p2, ncol = 2)
    }
  })

  output$ch1_transform_results <- renderUI({
    x <- ch1_trans_data()
    if (is.null(x)) return(NULL)

    sw_orig <- shapiro.test(x)
    sw_log <- shapiro.test(log(x))

    tagList(
      div(class = "stat-box",
          style = paste0("background:", if (sw_orig$p.value >= 0.05) col_ok else col_fail, ";"),
          paste0("Oryginalne: p = ", format.pval(sw_orig$p.value, digits = 3))),
      div(class = "stat-box",
          style = paste0("background:", if (sw_log$p.value >= 0.05) col_ok else col_fail, ";"),
          paste0("Po log(): p = ", format.pval(sw_log$p.value, digits = 3)))
    )
  })
}
