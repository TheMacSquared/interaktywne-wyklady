# ============================================================================
# CHAPTER 3: Jedna zmienna jakosciowa
# ============================================================================

ch3_ui <- tabPanel("4. Jedna zmienna jako\u015bciowa",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Testowali\u015bmy \u015bredni\u0105 jednej zmiennej ilo\u015bciowej.
       A co, gdy zmienna jest jako\u015bciowa (kategorialna)?"
    ),

    div(class = "section-title", "Test chi-kwadrat zgodno\u015bci"),

    div(class = "narrative",
      p("Pytanie: ", tags$b("Czy rozk\u0142ad obserwowany r\u00f3\u017cni si\u0119 od oczekiwanego?"), ""),
      p("Przyk\u0142ady: czy kostka jest uczciwa? Czy studenci wybieraj\u0105
        kierunki r\u00f3wnomiernie?"),
      div(class = "formula-box",
        p(withMathJax("\\(H_0\\)"), ": rozk\u0142ad zgodny z oczekiwanym"),
        p(withMathJax("\\(\\chi^2 = \\sum \\frac{(O_i - E_i)^2}{E_i}\\)")),
        p("gdzie ", withMathJax("\\(O_i\\)"), " = liczno\u015bci obserwowane, ",
          withMathJax("\\(E_i\\)"), " = liczno\u015bci oczekiwane")
      )
    ),

    # ========================================================================
    # WIDGET 1: Chi-kwadrat dopasowania
    # ========================================================================
    div(class = "section-title", "Czy kostka jest uczciwa?"),

    div(class = "widget-block",
      h4("Test \u03c7\u00b2 zgodno\u015bci"),
      fluidRow(
        column(4,
          sliderInput("ch3_n_rolls", "Liczba rzut\u00f3w:",
                      min = 30, max = 500, value = 60, step = 10),
          radioButtons("ch3_fair", "Typ kostki:",
            choices = c(
              "Uczciwa" = "fair",
              "Obci\u0105\u017cona (6 cz\u0119\u015bciej)" = "biased"
            ),
            selected = "fair", inline = TRUE
          ),
          actionButton("ch3_roll", "Rzu\u0107 kostk\u0105!",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch3_dice_plot", height = "300px"),
          uiOutput("ch3_dice_result")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Interpretacja:"),
      " Du\u017ca warto\u015b\u0107 \u03c7\u00b2 oznacza du\u017c\u0105 rozbie\u017cno\u015b\u0107 mi\u0119dzy tym,
        co zaobserwowali\u015bmy, a tym, czego oczekiwali\u015bmy. Je\u015bli p < 0.05,
        rozbie\u017cno\u015b\u0107 jest zbyt du\u017ca, by by\u0107 przypadkiem."
    ),

    # ========================================================================
    # WIDGET 2: Test dwumianowy
    # ========================================================================
    div(class = "section-title", "Test dwumianowy"),

    div(class = "narrative",
      p("Gdy zmienna ma dok\u0142adnie ", tags$b("dwie kategorie"),
        " (sukces/pora\u017cka), mo\u017cemy u\u017cy\u0107 testu dwumianowego."),
      p("Pytanie: ", tags$b("Czy proporcja sukces\u00f3w r\u00f3\u017cni si\u0119 od p\u2080?"), ""),
      p("Przyk\u0142ad: rzucamy monet\u0105 \u2014 czy jest uczciwa (p = 0.5)?")
    ),

    div(class = "widget-block",
      h4("Rzut monet\u0105"),
      fluidRow(
        column(4,
          sliderInput("ch3_coin_n", "Liczba rzut\u00f3w:",
                      min = 10, max = 200, value = 50, step = 5),
          sliderInput("ch3_coin_p0", "Testowane p\u2080:",
                      min = 0.1, max = 0.9, value = 0.5, step = 0.05),
          sliderInput("ch3_coin_true_p", "Prawdziwe p (orze\u0142):",
                      min = 0.1, max = 0.9, value = 0.5, step = 0.05),
          actionButton("ch3_coin_flip", "Rzu\u0107 monet\u0105!",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch3_coin_plot", height = "300px"),
          uiOutput("ch3_coin_result")
        )
      )
    ),

    div(class = "callout-warning",
      tags$strong("Uwaga:"),
      " Test dwumianowy jest ", tags$b("dok\u0142adny"), " (nie opiera si\u0119 na
        przybli\u017ceniu) \u2014 dzia\u0142a nawet przy bardzo ma\u0142ych pr\u00f3bach.
        Test \u03c7\u00b2 zgodno\u015bci wymaga, \u017ceby ka\u017cda oczekiwana liczno\u015b\u0107 \u2265 5."
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: zwi\u0105zek mi\u0119dzy dwiema zmiennymi ilo\u015bciowymi"),
      actionButton("ch3_next", "Dalej \u2192 5. Dwie zmienne ilo\u015bciowe",
                   class = "btn-primary btn-lg")
    )
  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch3_server <- function(input, output, session) {

  # --- Widget 1: Chi-kwadrat ---
  ch3_dice_data <- reactiveVal(NULL)

  observeEvent(input$ch3_roll, {
    n <- input$ch3_n_rolls
    probs <- if (input$ch3_fair == "fair") rep(1/6, 6) else c(rep(0.15, 5), 0.25)
    rolls <- sample(1:6, n, replace = TRUE, prob = probs)
    observed <- table(factor(rolls, levels = 1:6))
    expected <- n / 6  # zakladamy uczciwa kostke w H0

    test <- chisq.test(observed, p = rep(1/6, 6))

    ch3_dice_data(list(
      observed = as.numeric(observed),
      expected = rep(expected, 6),
      test = test
    ))
  })

  output$ch3_dice_plot <- renderPlot({
    dd <- ch3_dice_data()
    if (is.null(dd)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Rzu\u0107 kostk\u0105!'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      df <- data.frame(
        face = rep(1:6, 2),
        count = c(dd$observed, dd$expected),
        type = rep(c("Obserwowane", "Oczekiwane"), each = 6)
      )

      ggplot(df, aes(x = factor(face), y = count, fill = type)) +
        geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.8) +
        geom_text(aes(label = round(count, 1)),
                  position = position_dodge(width = 0.8), vjust = -0.3, size = 4) +
        scale_fill_manual(values = c(col_h0, col_pvalue), name = NULL) +
        labs(title = paste0(input$ch3_n_rolls, " rzut\u00f3w kostk\u0105"),
             x = "Oczko", y = "Liczno\u015b\u0107") +
        theme_test() +
        theme(legend.position = "top")
    }
  })

  output$ch3_dice_result <- renderUI({
    dd <- ch3_dice_data()
    if (is.null(dd)) return(NULL)

    test <- dd$test
    res <- format_test_result(test$p.value)

    div(class = "callout-info",
      p(tags$strong("Wynik testu \u03c7\u00b2 zgodno\u015bci:")),
      p(paste0("\u03c7\u00b2(", test$parameter, ") = ",
               round(test$statistic, 3))),
      p(paste0("p = ", format.pval(test$p.value, digits = 4))),
      p(style = paste0("color:", res$color, "; font-weight: bold;"),
        res$decision)
    )
  })

  # --- Widget 2: Test dwumianowy ---
  ch3_coin_data <- reactiveVal(NULL)

  observeEvent(input$ch3_coin_flip, {
    n <- input$ch3_coin_n
    true_p <- input$ch3_coin_true_p
    p0 <- input$ch3_coin_p0

    heads <- rbinom(1, n, true_p)
    test <- binom.test(heads, n, p = p0)

    ch3_coin_data(list(
      heads = heads, n = n, phat = heads / n,
      p0 = p0, true_p = true_p, test = test
    ))
  })

  output$ch3_coin_plot <- renderPlot({
    cd <- ch3_coin_data()
    if (is.null(cd)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Rzu\u0107 monet\u0105!'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      # Rozklad dwumianowy pod H0
      x <- 0:cd$n
      probs <- dbinom(x, cd$n, cd$p0)
      df <- data.frame(x = x, prob = probs)

      ggplot(df, aes(x = x, y = prob)) +
        geom_col(fill = col_h0, alpha = 0.5, width = 0.8) +
        geom_vline(xintercept = cd$heads, color = col_reject, linewidth = 1.2) +
        annotate("text", x = cd$heads, y = max(probs) * 0.9,
                 label = paste0("Obserwowane: ", cd$heads),
                 hjust = -0.1, color = col_reject, fontface = "bold") +
        labs(title = paste0("Rozk\u0142ad B(", cd$n, ", ", cd$p0,
                            ") pod H\u2080"),
             x = "Liczba or\u0142\u00f3w", y = "Prawdopodobie\u0144stwo") +
        theme_test()
    }
  })

  output$ch3_coin_result <- renderUI({
    cd <- ch3_coin_data()
    if (is.null(cd)) return(NULL)

    test <- cd$test
    res <- format_test_result(test$p.value)

    div(class = "callout-info",
      p(tags$strong("Wynik testu dwumianowego:")),
      p(paste0("Obserwowane or\u0142y: ", cd$heads, " / ", cd$n,
               " (\u015b = ", round(cd$phat, 3), ")")),
      p(paste0("p = ", format.pval(test$p.value, digits = 4))),
      p(paste0("95% CI: [", round(test$conf.int[1], 3), " ; ",
               round(test$conf.int[2], 3), "]")),
      p(style = paste0("color:", res$color, "; font-weight: bold;"),
        res$decision)
    )
  })
}
