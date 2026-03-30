# ============================================================================
# CHAPTER 5: Co wplywa na szerokosc przedzialu?
# ============================================================================

ch5_ui <- tabPanel("5. Co wp\u0142ywa na szeroko\u015b\u0107?",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Umiemy ju\u017c budowa\u0107 przedzia\u0142y dla \u015bredniej i proporcji.
       Teraz zbadamy, co decyduje o ich precyzji."
    ),

    div(class = "section-title", "Trzy czynniki szeroko\u015bci przedzia\u0142u"),

    div(class = "narrative",
      p("Margines b\u0142\u0119du (a wi\u0119c szeroko\u015b\u0107 przedzia\u0142u) zale\u017cy od trzech rzeczy:"),
      div(class = "formula-box",
        withMathJax(helpText(
          "$$ME = t^* \\cdot \\frac{s}{\\sqrt{n}}$$"
        ))
      ),
      tags$ol(
        tags$li(tags$b("Wielko\u015b\u0107 pr\u00f3by (n)"), " \u2014 wi\u0119cej danych = w\u0119\u017cszy przedzia\u0142"),
        tags$li(tags$b("Poziom ufno\u015bci"), " \u2014 wi\u0119ksza pewno\u015b\u0107 = szerszy przedzia\u0142"),
        tags$li(tags$b("Zmienno\u015b\u0107 danych (s)"), " \u2014 wi\u0119ksze rozproszenie = szerszy przedzia\u0142")
      )
    ),

    # ========================================================================
    # WIDGET 1: Trzy suwaki
    # ========================================================================
    div(class = "section-title", "Interaktywna eksploracja"),

    div(class = "widget-block",
      h4("Jak zmienia si\u0119 szeroko\u015b\u0107 przedzia\u0142u?"),
      fluidRow(
        column(4,
          sliderInput("ch5_n", "Wielko\u015b\u0107 pr\u00f3by (n):",
                      min = 5, max = 500, value = 30, step = 5),
          sliderInput("ch5_conf", "Poziom ufno\u015bci:",
                      min = 0.80, max = 0.99, value = 0.95, step = 0.01),
          sliderInput("ch5_s", "Odchylenie std. (s):",
                      min = 1, max = 30, value = 10, step = 1),
          hr(),
          uiOutput("ch5_me_display")
        ),
        column(8,
          plotOutput("ch5_factors_plot", height = "350px")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Malej\u0105ce korzy\u015bci:"),
      " Zwi\u0119kszenie n z 25 do 100 (4\u00d7) skraca przedzia\u0142 o po\u0142ow\u0119 (2\u00d7).
        Ale z 100 do 400 (4\u00d7) te\u017c tylko o po\u0142ow\u0119. To efekt ",
      withMathJax("\\(\\frac{1}{\\sqrt{n}}\\)"), "."
    ),

    # ========================================================================
    # WIDGET 2: Planowanie wielkosci proby
    # ========================================================================
    div(class = "section-title", "Planowanie wielko\u015bci pr\u00f3by"),

    div(class = "narrative",
      p("Odwr\u00f3\u0107my pytanie: ", tags$b("ile obserwacji potrzebuj\u0119"),
        ", \u017ceby margines b\u0142\u0119du by\u0142 nie wi\u0119kszy ni\u017c zak\u0142adany?"),
      div(class = "formula-box",
        withMathJax(helpText(
          "$$n = \\left(\\frac{z^* \\cdot s}{ME_{\\text{max}}}\\right)^2$$"
        ))
      )
    ),

    div(class = "widget-block",
      h4("Kalkulator wielko\u015bci pr\u00f3by"),
      fluidRow(
        column(4,
          numericInput("ch5_plan_me", "Po\u017c\u0105dany margines b\u0142\u0119du:",
                       value = 2, min = 0.1, step = 0.1),
          numericInput("ch5_plan_s", "Spodziewane s:",
                       value = 10, min = 0.1, step = 0.5),
          sliderInput("ch5_plan_conf", "Poziom ufno\u015bci:",
                      min = 0.80, max = 0.99, value = 0.95, step = 0.01)
        ),
        column(8,
          uiOutput("ch5_plan_result"),
          plotOutput("ch5_plan_plot", height = "300px")
        )
      )
    ),

    # ========================================================================
    # WIDGET 3: Porownanie przedzialow
    # ========================================================================
    div(class = "section-title", "90% vs 95% vs 99%"),

    div(class = "narrative",
      p("Zobaczmy jak wygl\u0105daj\u0105 trzy przedzia\u0142y z tych samych danych,
        ale przy r\u00f3\u017cnych poziomach ufno\u015bci.")
    ),

    div(class = "widget-block",
      h4("Trzy poziomy ufno\u015bci"),
      fluidRow(
        column(4,
          selectInput("ch5_cmp_data", "Dane:",
            choices = c(
              "Wzrost student\u00f3w (n=30)" = "height",
              "Czas dojazdu (n=50)" = "commute",
              "Oceny z egzaminu (n=40)" = "grades"
            ),
            selected = "height"
          ),
          actionButton("ch5_cmp_calc", "Oblicz 3 przedzia\u0142y",
                       class = "btn-primary", width = "100%"),
          br(), br(),
          uiOutput("ch5_cmp_stats")
        ),
        column(8,
          plotOutput("ch5_cmp_plot", height = "250px")
        )
      )
    ),

    div(class = "callout-success",
      tags$strong("Kompromis:"),
      " 95% to standardowy wyb\u00f3r \u2014 rozs\u0105dna r\u00f3wnowaga mi\u0119dzy
        pewno\u015bci\u0105 a precyzj\u0105. 99% daje szerszy przedzia\u0142 (wi\u0119ksza pewno\u015b\u0107,
        mniejsza precyzja), 90% w\u0119\u017cszy (mniej pewny, bardziej precyzyjny)."
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: podsumowanie wzor\u00f3w i zasad"),
      actionButton("ch5_next", "Dalej \u2192 6. \u015aci\u0105ga",
                   class = "btn-primary btn-lg")
    )
  ))
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
    xbar <- 170  # arbitrary center

    # Show how ME changes with n
    n_seq <- seq(5, 500, by = 5)
    me_seq <- qt(1 - (1 - conf) / 2, df = pmax(n_seq - 1, 1)) * s / sqrt(n_seq)

    df <- data.frame(n = n_seq, me = me_seq)

    ggplot(df, aes(x = n, y = me)) +
      geom_line(color = col_ci, linewidth = 1.2) +
      geom_point(aes(x = !!n, y = !!me), color = col_estimate, size = 4) +
      geom_hline(yintercept = me, color = col_estimate, linetype = "dotted") +
      annotate("text", x = n + 20, y = me + 0.5,
               label = paste0("ME = ", round(me, 2)),
               color = col_estimate, fontface = "bold", size = 4.5) +
      labs(title = paste0("Margines b\u0142\u0119du w funkcji n ",
                          "(", round(conf * 100), "% CI, s = ", s, ")"),
           x = "Wielko\u015b\u0107 pr\u00f3by (n)",
           y = "Margines b\u0142\u0119du (ME)") +
      theme_ci()
  })

  output$ch5_me_display <- renderUI({
    n <- input$ch5_n
    conf <- input$ch5_conf
    s <- input$ch5_s
    t_star <- qt(1 - (1 - conf) / 2, df = n - 1)
    me <- t_star * s / sqrt(n)
    width <- 2 * me

    tagList(
      div(class = "stat-box", style = paste0("background:", col_ci, ";"),
          paste0("ME = ", round(me, 2))),
      div(class = "stat-box", style = paste0("background:", col_dark, ";"),
          paste0("Szer. = ", round(width, 2))),
      div(class = "stat-box", style = paste0("background:", col_estimate, ";"),
          paste0("t* = ", round(t_star, 3)))
    )
  })

  # --- Widget 2: Planowanie n ---
  output$ch5_plan_result <- renderUI({
    me_max <- input$ch5_plan_me
    s <- input$ch5_plan_s
    conf <- input$ch5_plan_conf
    z_star <- qnorm(1 - (1 - conf) / 2)
    n_req <- ceiling((z_star * s / me_max)^2)

    div(class = "callout-success",
      p(tags$strong("Wymagana wielko\u015b\u0107 pr\u00f3by:")),
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

    n_seq <- seq(5, max(n_req * 2, 100), by = 1)
    me_seq <- z_star * s / sqrt(n_seq)

    df <- data.frame(n = n_seq, me = me_seq)

    ggplot(df, aes(x = n, y = me)) +
      geom_line(color = col_ci, linewidth = 1.2) +
      geom_hline(yintercept = me_max, color = col_secondary, linetype = "dashed",
                 linewidth = 1) +
      geom_point(aes(x = n_req, y = me_max), color = col_success, size = 5) +
      annotate("text", x = n_req, y = me_max + 0.3,
               label = paste0("n = ", n_req),
               color = col_success, fontface = "bold", size = 5) +
      labs(title = "Margines b\u0142\u0119du vs wielko\u015b\u0107 pr\u00f3by",
           x = "n", y = "Margines b\u0142\u0119du") +
      theme_ci()
  })

  # --- Widget 3: Porownanie 90/95/99 ---
  ch5_cmp_data <- reactiveVal(NULL)

  observeEvent(input$ch5_cmp_calc, {
    set.seed(42)
    samp <- switch(input$ch5_cmp_data,
      "height"  = rnorm(30, mean = 170, sd = 10),
      "commute" = rgamma(50, shape = 4, scale = 7.5),
      "grades"  = pmin(pmax(rnorm(40, mean = 3.5, sd = 0.7), 2), 5)
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
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      df$y <- c(3, 2, 1)
      colors <- c(col_warning, col_ci, col_purple)

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
        labs(title = "Ten sam zbi\u00f3r \u2014 trzy poziomy ufno\u015bci",
             x = "Warto\u015b\u0107", y = "Poziom ufno\u015bci") +
        theme_ci()
    }
  })

  output$ch5_cmp_stats <- renderUI({
    df <- ch5_cmp_data()
    if (is.null(df)) return(NULL)
    tagList(
      lapply(1:3, function(i) {
        div(class = "stat-box",
            style = paste0("background:", c(col_warning, col_ci, col_purple)[i], ";"),
            paste0(df$conf[i], ": \u00b1", round(df$me[i], 2)))
      })
    )
  })
}
