# ============================================================================
# CHAPTER 4: Dwie zmienne ilosciowe (korelacja)
# ============================================================================

ch4_ui <- tabPanel("5. Dwie zmienne ilo\u015bciowe",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Dotychczas badali\u015bmy jedn\u0105 zmienn\u0105.
       Teraz pytamy: czy dwie zmienne ilo\u015bciowe s\u0105 ze sob\u0105 powi\u0105zane?"
    ),

    div(class = "section-title", "Korelacja Pearsona"),

    div(class = "narrative",
      p("Wsp\u00f3\u0142czynnik korelacji Pearsona ", withMathJax("\\(r\\)"),
        " mierzy ", tags$b("liniowy"), " zwi\u0105zek mi\u0119dzy dwiema zmiennymi."),
      div(class = "formula-box",
        p(withMathJax("\\(H_0: \\rho = 0\\)"), " (brak korelacji liniowej)"),
        p(withMathJax("\\(r = \\frac{\\sum (x_i - \\bar{x})(y_i - \\bar{y})}{\\sqrt{\\sum(x_i-\\bar{x})^2 \\cdot \\sum(y_i-\\bar{y})^2}}\\)"))
      )
    ),

    # ========================================================================
    # WIDGET 1: Korelacja Pearsona
    # ========================================================================
    div(class = "section-title", "Interaktywna korelacja"),

    div(class = "widget-block",
      h4("Korelacja Pearsona"),
      fluidRow(
        column(4,
          sliderInput("ch4_r", "Prawdziwe r:",
                      min = -1, max = 1, value = 0.7, step = 0.05),
          sliderInput("ch4_n", "Wielko\u015b\u0107 pr\u00f3by (n):",
                      min = 10, max = 200, value = 50, step = 10),
          actionButton("ch4_gen_pearson", "Generuj dane",
                       class = "btn-primary", width = "100%"),
          hr(),
          checkboxInput("ch4_show_reg", "Poka\u017c lini\u0119 regresji", value = FALSE)
        ),
        column(8,
          plotOutput("ch4_scatter", height = "350px"),
          uiOutput("ch4_pearson_result")
        )
      )
    ),

    # ========================================================================
    # WIDGET 2: Korelacja Spearmana
    # ========================================================================
    div(class = "section-title", "Korelacja Spearmana"),

    div(class = "narrative",
      p("Korelacja Spearmana mierzy ", tags$b("monotoniczno\u015b\u0107"),
        " zwi\u0105zku (niekoniecznie liniowy). Dzia\u0142a na ",
        tags$b("rangach"), " \u2014 jest odporna na outliery."),
      p("Zobaczmy dane, gdzie zwi\u0105zek jest monotoniczny, ale nie liniowy:")
    ),

    div(class = "widget-block",
      h4("Pearson vs Spearman"),
      fluidRow(
        column(4,
          selectInput("ch4_corr_type", "Typ danych:",
            choices = c(
              "Liniowy" = "linear",
              "Monotoniczny (log)" = "monotonic",
              "Brak zwi\u0105zku" = "none"
            ),
            selected = "monotonic"
          ),
          sliderInput("ch4_sp_n", "n:",
                      min = 20, max = 100, value = 50, step = 10),
          actionButton("ch4_gen_spearman", "Generuj dane",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch4_sp_scatter", height = "300px"),
          uiOutput("ch4_sp_comparison")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Kiedy kt\u00f3ry?"),
      tags$ul(
        tags$li(tags$b("Pearson"), " \u2014 zwi\u0105zek liniowy, dane ~normalne, brak ekstremalnych outlier\u00f3w"),
        tags$li(tags$b("Spearman"), " \u2014 zwi\u0105zek monotoniczny (niekoniecznie liniowy), dane porz\u0105dkowe, outliery")
      )
    ),

    # ========================================================================
    # WIDGET 3: Korelacja =/= przyczynowość
    # ========================================================================
    div(class = "section-title", "Korelacja \u2260 przyczynowo\u015b\u0107"),

    div(class = "narrative",
      p("Wysoka korelacja nie oznacza, \u017ce jedna zmienna ",
        tags$b("powoduje"), " zmian\u0119 drugiej. Zobaczmy, jak ",
        tags$b("jeden outlier"), " mo\u017ce sztucznie wytworzy\u0107 korelacj\u0119.")
    ),

    div(class = "widget-block",
      h4("Dodaj outliera!"),
      fluidRow(
        column(4,
          helpText("Dane: 50 punkt\u00f3w bez korelacji. Kliknij, aby doda\u0107 outliera."),
          actionButton("ch4_gen_outlier", "Nowe dane (brak korelacji)",
                       class = "btn-primary", width = "100%"),
          actionButton("ch4_add_outlier", "Dodaj outliera!",
                       class = "btn-danger", width = "100%"),
          br(), br(),
          uiOutput("ch4_outlier_r")
        ),
        column(8,
          plotOutput("ch4_outlier_plot", height = "350px")
        )
      )
    ),

    div(class = "callout-danger",
      tags$strong("Pami\u0119taj:"),
      " Przed interpretacj\u0105 korelacji ", tags$b("zawsze"), " obejrzyj wykres!
        Pojedynczy punkt mo\u017ce drastycznie zmieni\u0107 warto\u015b\u0107 r."
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: zwi\u0105zek mi\u0119dzy dwiema zmiennymi jako\u015bciowymi"),
      actionButton("ch4_next", "Dalej \u2192 6. Dwie zmienne jako\u015bciowe",
                   class = "btn-primary btn-lg")
    )
  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch4_server <- function(input, output, session) {

  # --- Widget 1: Pearson ---
  ch4_pearson_data <- reactiveVal(NULL)

  observeEvent(input$ch4_gen_pearson, {
    ch4_pearson_data(generate_correlation_data(input$ch4_n, input$ch4_r, "linear"))
  })

  output$ch4_scatter <- renderPlot({
    df <- ch4_pearson_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Generuj dane'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      p <- ggplot(df, aes(x = x, y = y)) +
        geom_point(color = col_h0, alpha = 0.6, size = 2.5) +
        labs(title = "Wykres rozrzutu", x = "X", y = "Y") +
        theme_test()

      if (input$ch4_show_reg) {
        p <- p + geom_smooth(method = "lm", se = TRUE,
                             color = col_reject, fill = col_reject, alpha = 0.1)
      }
      p
    }
  })

  output$ch4_pearson_result <- renderUI({
    df <- ch4_pearson_data()
    if (is.null(df)) return(NULL)

    result <- rstatix::cor_test(df, x, y, method = "pearson")
    tidy_res <- as.data.frame(result)
    res <- format_test_result(tidy_res$p)

    div(class = "callout-info",
      p(tags$strong("Korelacja Pearsona:")),
      p(paste0("r = ", round(tidy_res$cor, 3))),
      p(paste0("p = ", format.pval(tidy_res$p, digits = 4))),
      p(style = paste0("color:", res$color, "; font-weight: bold;"),
        res$decision)
    )
  })

  # --- Widget 2: Spearman ---
  ch4_spearman_data <- reactiveVal(NULL)

  observeEvent(input$ch4_gen_spearman, {
    ch4_spearman_data(generate_correlation_data(
      input$ch4_sp_n, 0.7, input$ch4_corr_type))
  })

  output$ch4_sp_scatter <- renderPlot({
    df <- ch4_spearman_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Generuj dane'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      ggplot(df, aes(x = x, y = y)) +
        geom_point(color = col_h0, alpha = 0.6, size = 2.5) +
        geom_smooth(method = "lm", se = FALSE, color = col_reject,
                    linetype = "dashed", alpha = 0.5) +
        labs(title = "Wykres rozrzutu", x = "X", y = "Y") +
        theme_test()
    }
  })

  output$ch4_sp_comparison <- renderUI({
    df <- ch4_spearman_data()
    if (is.null(df)) return(NULL)

    res_p <- rstatix::cor_test(df, x, y, method = "pearson")
    res_s <- rstatix::cor_test(df, x, y, method = "spearman")

    tagList(
      div(class = "stat-box", style = paste0("background:", col_h0, ";"),
          paste0("Pearson r = ", round(as.data.frame(res_p)$cor, 3))),
      div(class = "stat-box", style = paste0("background:", col_paired, ";"),
          paste0("Spearman \u03c1 = ", round(as.data.frame(res_s)$cor, 3))),
      div(class = "stat-box", style = paste0("background:", col_dark, ";"),
          paste0("p(Pearson) = ", format.pval(as.data.frame(res_p)$p, digits = 3))),
      div(class = "stat-box", style = paste0("background:", col_dark, ";"),
          paste0("p(Spearman) = ", format.pval(as.data.frame(res_s)$p, digits = 3)))
    )
  })

  # --- Widget 3: Outlier ---
  ch4_outlier_data <- reactiveVal(NULL)

  observeEvent(input$ch4_gen_outlier, {
    ch4_outlier_data(generate_correlation_data(50, 0, "none"))
  })

  observeEvent(input$ch4_add_outlier, {
    df <- ch4_outlier_data()
    if (is.null(df)) return()
    outlier <- data.frame(x = max(df$x) + 15, y = max(df$y) + 15)
    ch4_outlier_data(rbind(df, outlier))
  })

  output$ch4_outlier_plot <- renderPlot({
    df <- ch4_outlier_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Nowe dane'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      is_outlier <- c(rep(FALSE, nrow(df) - max(0, nrow(df) - 50)),
                       rep(TRUE, max(0, nrow(df) - 50)))
      if (length(is_outlier) != nrow(df)) {
        is_outlier <- rep(FALSE, nrow(df))
      }

      r_val <- cor(df$x, df$y)

      ggplot(df, aes(x = x, y = y)) +
        geom_point(color = ifelse(seq_len(nrow(df)) > 50, col_reject, col_h0),
                   size = ifelse(seq_len(nrow(df)) > 50, 4, 2.5),
                   alpha = 0.7) +
        geom_smooth(method = "lm", se = FALSE, color = col_reject, alpha = 0.5) +
        labs(title = paste0("r = ", round(r_val, 3)),
             x = "X", y = "Y") +
        theme_test()
    }
  })

  output$ch4_outlier_r <- renderUI({
    df <- ch4_outlier_data()
    if (is.null(df)) return(NULL)
    r_val <- cor(df$x, df$y)
    n_outliers <- max(0, nrow(df) - 50)
    tagList(
      div(class = "stat-box", style = paste0("background:", col_h0, ";"),
          paste0("r = ", round(r_val, 3))),
      div(class = "stat-box", style = paste0("background:", col_reject, ";"),
          paste0("Outlier\u00f3w: ", n_outliers))
    )
  })
}
