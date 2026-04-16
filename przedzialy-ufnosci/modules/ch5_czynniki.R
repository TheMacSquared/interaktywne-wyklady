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
                      min = 5, max = 100, value = 30, step = 1),
          sliderInput("ch5_conf", "Poziom ufno\u015bci:",
                      min = 0.80, max = 0.99, value = 0.95, step = 0.01),
          sliderInput("ch5_s", "Odchylenie std. (s):",
                      min = 1, max = 12, value = 8, step = 1),
          hr(),
          uiOutput("ch5_me_display")
        ),
        column(8,
          plotOutput("ch5_factors_plot", height = "480px")
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
          plotOutput("ch5_plan_plot", height = "440px")
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

    # ========================================================================
    # WIDGET 4: Edge case'y - jak poziom ufnosci zmienia werdykt
    # ========================================================================
    div(class = "section-title", "Edge case: kiedy poziom ufno\u015bci zmienia wniosek"),

    div(class = "narrative",
      p("Czasami ten sam zbi\u00f3r danych ", tags$b("pozwala stwierdzi\u0107 hipotez\u0119"),
        " przy 90% ufno\u015bci, a ", tags$b("nie pozwala"), " przy 95%. To jest cz\u0119sto
        nieintuicyjne \u2014 student my\u015bli, \u017ce skoro ", tags$em("p\u0302 jest powy\u017cej granicy"),
        ", to wniosek jest oczywisty. Nie jest. Liczy si\u0119 ", tags$b("ca\u0142y przedzia\u0142"),
        " wzgl\u0119dem granicy hipotezy, a szeroko\u015b\u0107 przedzia\u0142u zale\u017cy od poziomu ufno\u015bci."),
      p("Poni\u017cej trzy case'y. W ka\u017cdym kliknij ", tags$b("90%"), ", ", tags$b("95%"),
        " i ", tags$b("99%"), " i obserwuj, jak werdykt si\u0119 zmienia.")
    ),

    tags$details(class = "case-study", open = NA,
      tags$summary(
        span(class = "case-icon", "\U0001f697"),
        "Edge 1. Czas dojazdu \u2014 czy \u015bredni czas przekracza 26 min?"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Zmierzono czas dojazdu dla ", tags$b("40 pracownik\u00f3w"),
            ". \u015arednia z pr\u00f3by ", withMathJax("\\(\\bar{x} = 28.5\\)"), " min,
            odchylenie standardowe ", withMathJax("\\(s = 8\\)"), " min.
            Hipoteza: \u015bredni czas dojazdu w populacji przekracza 26 min.")
        ),
        uiOutput("ch5_edge1_buttons"),
        plotOutput("ch5_edge1_plot", height = "240px"),
        uiOutput("ch5_edge1_explain")
      )
    ),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f5f3\ufe0f"),
        "Edge 2. Sonda\u017c \u2014 czy poparcie przekracza 50%?"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Pracownia sonda\u017cowa zapyta\u0142a ", tags$b("1000 wyborc\u00f3w"),
            ", czy poprze parti\u0119 X. ", tags$b("540 odpowiedzi TAK"),
            " (", withMathJax("\\(\\hat{p} = 0.54\\)"), ").
            Hipoteza: poparcie w populacji przekracza pr\u00f3g 50%.")
        ),
        uiOutput("ch5_edge2_buttons"),
        plotOutput("ch5_edge2_plot", height = "240px"),
        uiOutput("ch5_edge2_explain")
      )
    ),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f4d8"),
        "Edge 3. Wynik szkolenia \u2014 czy \u015brednia przekracza 65 pkt?"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("W szkoleniu BHP ", tags$b("20 pracownik\u00f3w"),
            " uzyska\u0142o \u015bredni wynik ", withMathJax("\\(\\bar{x} = 68\\)"), " pkt
            (na 100), ", withMathJax("\\(s = 10\\)"), " pkt.
            Hipoteza: \u015bredni wynik w populacji przekracza pr\u00f3g 65 pkt.")
        ),
        uiOutput("ch5_edge3_buttons"),
        plotOutput("ch5_edge3_plot", height = "240px"),
        uiOutput("ch5_edge3_explain")
      )
    ),

    div(class = "callout-info",
      tags$strong("Dlaczego to jest nieintuicyjne?"),
      " Bo w codziennym my\u015bleniu nie odr\u00f3\u017cniamy 95% od 93% \u2014 dla nas jest
        \"du\u017co\", \"\u015brednio\", \"ma\u0142o\". Statystyka pozwala na ", tags$b("precyzyjne kwantyfikowanie pewno\u015bci"),
      " i to jest jej moc, nie wada. Stwierdzenie ",
      tags$em("\"nie mo\u017cemy by\u0107 pewni z 95%, ale mo\u017cemy z 93%\""),
      " nie jest sprzeczno\u015bci\u0105 \u2014 to jest dok\u0142adnie ten poziom precyzji,
        do kt\u00f3rego s\u0142u\u017cy ten aparat matematyczny. ",
      tags$br(), tags$br(),
      tags$strong("W praktyce:"),
      " 95% to umowny standard. Je\u015bli wiesz, \u017ce ", tags$em("Tw\u00f3j problem"),
      " toleruje wi\u0119cej ryzyka (np. wst\u0119pna eksploracja, niskie koszty b\u0142\u0119du), mo\u017cesz
        legalnie u\u017cy\u0107 90%. Je\u015bli mniej (np. badania medyczne, kontrola jako\u015bci),
        u\u017cyj 99%. Wa\u017cne jest tylko, \u017ceby poziom ufno\u015bci ", tags$b("wybra\u0107 zanim"),
      " spojrzysz na wyniki \u2014 i potem ten wyb\u00f3r jasno raportowa\u0107."
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
    xbar <- 170  # arbitralny srodek (np. wzrost)

    # ---- GORNY PANEL: krzywa ME(n) ----
    n_seq <- seq(5, 100, by = 1)
    me_seq <- qt(1 - (1 - conf) / 2, df = pmax(n_seq - 1, 1)) * s / sqrt(n_seq)
    df <- data.frame(n = n_seq, me = me_seq)

    p_top <- ggplot(df, aes(x = n, y = me)) +
      geom_line(color = col_ci, linewidth = 1.2) +
      geom_point(aes(x = !!n, y = !!me), color = col_estimate, size = 4) +
      geom_hline(yintercept = me, color = col_estimate, linetype = "dotted") +
      annotate("text", x = n + 4, y = me + 0.3,
               label = paste0("ME = ", round(me, 2)),
               color = col_estimate, fontface = "bold", size = 4.5) +
      labs(title = paste0("Margines b\u0142\u0119du w funkcji n ",
                          "(", round(conf * 100), "% CI, s = ", s, ")"),
           x = "Wielko\u015b\u0107 pr\u00f3by (n)",
           y = "Margines b\u0142\u0119du (ME)") +
      theme_educational()

    # ---- DOLNY PANEL: sam pasek CI na fixed osi X ----
    # Worst-case ME (n=5, conf=0.99, s=12) -> ustala stale granice osi X
    max_me_worst <- qt(0.995, df = 4) * 12 / sqrt(5)
    xlims <- c(xbar - max_me_worst * 1.05, xbar + max_me_worst * 1.05)

    p_bot <- ggplot() +
      xlim(xlims) +
      ylim(-0.6, 0.6) +
      labs(x = "Warto\u015b\u0107 (np. wzrost w cm)", y = NULL,
           title = "Tw\u00f3j 95% CI na sta\u0142ej osi") +
      theme_educational() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      geom_vline(xintercept = xbar, color = "#7f8c8d",
                 linetype = "dashed", linewidth = 0.6) +
      annotate("text", x = xbar, y = 0.5, label = paste0("\u015brodek = ", xbar),
               color = "#7f8c8d", size = 4, hjust = -0.1) +
      geom_point(aes(x = xbar, y = 0), color = col_estimate,
                 size = 7, shape = 18) +
      geom_errorbarh(aes(xmin = xbar - me, xmax = xbar + me, y = 0),
                     height = 0.18, color = col_ci, linewidth = 2.4, alpha = 0.7) +
      annotate("text", x = xbar, y = -0.42,
               label = paste0("CI: [", round(xbar - me, 2),
                              " ; ", round(xbar + me, 2), "]    szer. = ",
                              round(2 * me, 2)),
               color = col_ci, fontface = "bold", size = 4.8)

    library(patchwork)
    (p_top / p_bot) + plot_layout(heights = c(2, 1))
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
    me_actual <- z_star * s / sqrt(n_req)  # ME osiagniete przy n_req (zwykle ~ me_max)
    center <- 100  # arbitralny srodek

    # ---- GORNY PANEL: krzywa ME vs n ----
    n_seq <- seq(5, max(n_req * 2, 100), by = 1)
    me_seq <- z_star * s / sqrt(n_seq)
    df <- data.frame(n = n_seq, me = me_seq)

    p_top <- ggplot(df, aes(x = n, y = me)) +
      geom_line(color = col_ci, linewidth = 1.2) +
      geom_hline(yintercept = me_max, color = col_secondary, linetype = "dashed",
                 linewidth = 1) +
      geom_point(aes(x = n_req, y = me_max), color = col_success, size = 5) +
      annotate("text", x = n_req, y = me_max + 0.3,
               label = paste0("n = ", n_req),
               color = col_success, fontface = "bold", size = 5) +
      labs(title = "Margines b\u0142\u0119du vs wielko\u015b\u0107 pr\u00f3by",
           x = "n", y = "Margines b\u0142\u0119du") +
      theme_educational()

    # ---- DOLNY PANEL: pasek CI przy n_req, z dopuszczalna strefa ----
    xlims <- c(center - 3 * me_max, center + 3 * me_max)

    p_bot <- ggplot() +
      xlim(xlims) +
      ylim(-0.6, 0.6) +
      labs(x = "Warto\u015b\u0107 (jednostki dowolne)", y = NULL,
           title = paste0("CI przy n = ", n_req,
                          "  \u2014  szara strefa = dopuszczalny ME = \u00b1", me_max)) +
      theme_educational() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      annotate("rect",
               xmin = center - me_max, xmax = center + me_max,
               ymin = -Inf, ymax = Inf,
               fill = "#bdc3c7", alpha = 0.4) +
      geom_vline(xintercept = center, color = "#7f8c8d",
                 linetype = "dashed", linewidth = 0.6) +
      geom_point(aes(x = center, y = 0), color = col_estimate,
                 size = 7, shape = 18) +
      geom_errorbarh(aes(xmin = center - me_actual, xmax = center + me_actual, y = 0),
                     height = 0.18, color = col_success, linewidth = 2.4, alpha = 0.8) +
      annotate("text", x = center, y = -0.42,
               label = paste0("Osi\u0105gni\u0119te ME = \u00b1", round(me_actual, 3),
                              "  \u2264  ", me_max, " \u2713"),
               color = col_success, fontface = "bold", size = 4.8)

    library(patchwork)
    (p_top / p_bot) + plot_layout(heights = c(2, 1))
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
        theme_educational()
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

  # ==========================================================================
  # WIDGET 4: Edge case'y - poziom ufnosci zmienia werdykt
  # ==========================================================================
  col_hyp <- "#8e44ad"

  # ---- Helpery ----
  ci_mean_local <- function(xbar, s, n, conf) {
    t_star <- qt(1 - (1 - conf) / 2, df = n - 1)
    me <- t_star * s / sqrt(n)
    list(lower = xbar - me, upper = xbar + me, me = me, t_star = t_star)
  }
  ci_prop_local <- function(x, n, conf) {
    phat <- x / n
    z_star <- qnorm(1 - (1 - conf) / 2)
    se <- sqrt(phat * (1 - phat) / n)
    me <- z_star * se
    list(phat = phat, lower = phat - me, upper = phat + me, me = me, z_star = z_star)
  }
  hypothesis_verdict_edge <- function(lower, upper, bound, dir) {
    if (dir == "gt") {
      if (lower > bound)      "yes"
      else if (upper < bound) "no"
      else                    "maybe"
    } else {
      if (upper < bound)      "yes"
      else if (lower > bound) "no"
      else                    "maybe"
    }
  }
  verdict_class_edge <- function(v) {
    switch(v, "yes" = "callout-success", "no" = "callout-danger",
           "maybe" = "callout-warning")
  }
  verdict_label_edge <- function(v) {
    switch(v, "yes" = "TAK", "no" = "NIE", "maybe" = "NIEPEWNE")
  }

  # ---- Konfiguracja edge case'ow ----
  edge_cases <- list(
    edge1 = list(
      kind = "mean",
      data = list(xbar = 28.5, s = 8, n = 40),
      hypothesis = list(text = "\u015aredni czas dojazdu przekracza 26 min",
                        bound = 26, dir = "gt"),
      xlab = "\u015aredni czas dojazdu (min)"
    ),
    edge2 = list(
      kind = "prop",
      data = list(x = 540, n = 1000),
      hypothesis = list(text = "Poparcie dla partii X przekracza 50%",
                        bound = 0.50, dir = "gt"),
      xlab = "Poparcie dla partii X"
    ),
    edge3 = list(
      kind = "mean",
      data = list(xbar = 68, s = 10, n = 20),
      hypothesis = list(text = "\u015aredni wynik szkolenia przekracza 65 pkt",
                        bound = 65, dir = "gt"),
      xlab = "\u015aredni wynik (pkt)"
    )
  )

  # State per case: lista (conf, revealed)
  #   conf:     NA (nic nie wybrane) lub 0.90 / 0.95 / 0.99
  #   revealed: FALSE (tylko CI + tre\u015b\u0107 hipotezy) / TRUE (z werdyktem)
  ch5_edge_state <- reactiveValues()
  for (cid in names(edge_cases)) {
    ch5_edge_state[[cid]] <- list(conf = NA_real_, revealed = FALSE)
  }

  # ---- Compute CI for given case at given conf ----
  compute_edge_ci <- function(case_id, conf) {
    cfg <- edge_cases[[case_id]]
    if (cfg$kind == "mean") {
      ci <- ci_mean_local(cfg$data$xbar, cfg$data$s, cfg$data$n, conf)
      list(center = cfg$data$xbar, lower = ci$lower, upper = ci$upper, me = ci$me)
    } else {
      ci <- ci_prop_local(cfg$data$x, cfg$data$n, conf)
      list(center = ci$phat, lower = ci$lower, upper = ci$upper, me = ci$me)
    }
  }

  # ---- Generator przyciskow conf level + reveal ----
  edge_buttons_ui <- function(case_id) {
    state <- ch5_edge_state[[case_id]]
    current_conf <- state$conf
    revealed <- state$revealed
    levels <- c(0.90, 0.95, 0.99)
    btns <- lapply(levels, function(lv) {
      is_active <- !is.na(current_conf) && abs(current_conf - lv) < 1e-9
      btn_class <- if (is_active) "btn-warning" else "btn-outline-warning"
      actionButton(paste0("ch5_", case_id, "_conf", round(lv * 100)),
                   paste0(round(lv * 100), "%"), class = btn_class)
    })

    # Drugi rzad: przycisk "Pokaz werdykt" - tylko gdy conf wybrany i jeszcze nie odkryty
    reveal_row <- if (!is.na(current_conf) && !revealed) {
      div(class = "step-buttons", style = "margin-top: 4px;",
        actionButton(paste0("ch5_", case_id, "_reveal"),
                     "\U0001f50d Poka\u017c werdykt", class = "btn-success"))
    } else {
      NULL
    }

    tagList(
      div(class = "step-buttons", btns),
      reveal_row
    )
  }

  # ---- Plot dla edge case'a (jeden panel: pasek CI + obszar hipotezy) ----
  render_edge_plot <- function(case_id) {
    cfg <- edge_cases[[case_id]]
    conf <- ch5_edge_state[[case_id]]$conf

    # Najszerszy mozliwy CI (przy 99%) -> ustala stale granice osi X
    ci_max <- compute_edge_ci(case_id, 0.99)
    ci_min <- compute_edge_ci(case_id, 0.90)
    bound <- cfg$hypothesis$bound
    center <- ci_max$center

    # Zakres X obejmujacy wszystkie 3 poziomy CI + bound + troche marginesu
    xrange <- range(c(ci_max$lower, ci_max$upper, bound))
    pad <- diff(xrange) * 0.20
    xlims <- c(xrange[1] - pad, xrange[2] + pad)

    p <- ggplot() +
      xlim(xlims) +
      ylim(-0.65, 0.65) +
      labs(x = cfg$xlab, y = NULL) +
      theme_educational() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    # Obszar hipotezy (zawsze widoczny)
    if (cfg$hypothesis$dir == "gt") {
      p <- p + annotate("rect",
                        xmin = bound, xmax = Inf,
                        ymin = -Inf, ymax = Inf,
                        fill = col_hyp, alpha = 0.15)
    } else {
      p <- p + annotate("rect",
                        xmin = -Inf, xmax = bound,
                        ymin = -Inf, ymax = Inf,
                        fill = col_hyp, alpha = 0.15)
    }
    p <- p +
      geom_vline(xintercept = bound, color = col_hyp,
                 linewidth = 1, linetype = "solid") +
      annotate("text", x = bound, y = 0.55,
               label = paste0(if (cfg$hypothesis$dir == "gt") "\u2265 " else "\u2264 ",
                              bound),
               color = col_hyp, fontface = "bold", size = 4.5, hjust = -0.1)

    # Punkt centralny (zawsze)
    p <- p +
      geom_point(aes(x = center, y = 0), color = col_estimate,
                 size = 7, shape = 18) +
      annotate("text", x = center, y = -0.22,
               label = paste0(if (cfg$kind == "mean") "x\u0304 = " else "p\u0302 = ",
                              round(center, 3)),
               color = col_estimate, fontface = "bold", size = 4.5)

    # Pasek CI - tylko jezeli wybrany conf
    if (!is.na(conf)) {
      ci <- compute_edge_ci(case_id, conf)
      p <- p +
        geom_errorbarh(aes(xmin = ci$lower, xmax = ci$upper, y = 0),
                       height = 0.18, color = col_ci, linewidth = 2.4, alpha = 0.7) +
        annotate("text", x = center, y = -0.45,
                 label = paste0(round(conf * 100), "% CI: [",
                                round(ci$lower, 3), " ; ", round(ci$upper, 3), "]"),
                 color = col_ci, fontface = "bold", size = 4.8)
    } else {
      p <- p + annotate("text", x = mean(xlims), y = 0.35,
                        label = "Wybierz poziom ufno\u015bci powy\u017cej",
                        color = "#7f8c8d", size = 4.5, fontface = "italic")
    }

    p
  }

  # ---- Render werdyktu dla edge case'a ----
  render_edge_explain <- function(case_id) {
    cfg <- edge_cases[[case_id]]
    state <- ch5_edge_state[[case_id]]
    conf <- state$conf
    revealed <- state$revealed

    if (is.na(conf)) {
      return(div(class = "callout-info",
        p(tags$strong("Hipoteza: "), cfg$hypothesis$text),
        p(tags$em("Kliknij jeden z przycisk\u00f3w 90% / 95% / 99% \u017ceby zobaczy\u0107
                  przedzia\u0142 ufno\u015bci."))
      ))
    }

    # Faza 1: tylko CI + tresc hipotezy, czas na dyskusje
    if (!revealed) {
      return(div(class = "callout-info",
        p(tags$strong("Hipoteza: "), cfg$hypothesis$text),
        p("Wybrany poziom ufno\u015bci: ", tags$b(round(conf * 100), "%")),
        p(tags$em("Spojrz na wykres: gdzie le\u017cy CI wzgl\u0119dem granicy hipotezy?
                  Co o tym s\u0105dzicie? Klikni\u0119cie ", tags$b("Poka\u017c werdykt"),
                  " odsloni odpowied\u017a."))
      ))
    }

    # Faza 2: werdykt
    ci <- compute_edge_ci(case_id, conf)
    verdict <- hypothesis_verdict_edge(ci$lower, ci$upper, cfg$hypothesis$bound,
                                       cfg$hypothesis$dir)
    cls <- verdict_class_edge(verdict)
    label <- verdict_label_edge(verdict)

    body <- if (verdict == "yes") {
      p("Ca\u0142y ", round(conf * 100), "% CI le\u017cy w obszarze hipotezy. Mo\u017cemy ",
        tags$b("z ", round(conf * 100), "% pewno\u015bci\u0105"), " stwierdzi\u0107, \u017ce ",
        cfg$hypothesis$text, ".")
    } else if (verdict == "no") {
      p("Ca\u0142y ", round(conf * 100), "% CI le\u017cy poza obszarem hipotezy. Z ",
        round(conf * 100), "% pewno\u015bci\u0105 ", tags$b("nie mo\u017cemy"),
        " stwierdzi\u0107 hipotezy \u2014 dane przemawiaj\u0105 wr\u0119cz przeciwko niej.")
    } else {
      p(round(conf * 100), "% CI ", tags$b("przecina granic\u0119 hipotezy"),
        " (", round(cfg$hypothesis$bound, 3), "). Cz\u0119\u015b\u0107 przedzia\u0142u jest w obszarze
        hipotezy, cz\u0119\u015b\u0107 poza. Z ", round(conf * 100), "% pewno\u015bci\u0105 ",
        tags$b("nie mo\u017cemy stwierdzi\u0107"),
        ", \u017ce hipoteza jest prawdziwa \u2014 ale te\u017c nie mo\u017cemy jej odrzuci\u0107.
        Spr\u00f3buj zmieni\u0107 poziom ufno\u015bci i zobacz, jak werdykt si\u0119 zmienia.")
    }

    div(class = cls,
      p(tags$strong("Hipoteza: "), cfg$hypothesis$text),
      p(tags$strong("Werdykt przy ", round(conf * 100), "% ufno\u015bci: ", label)),
      body
    )
  }

  # ---- Rejestracja outputow + observerow dla kazdego edge case'a ----
  register_edge_case <- function(case_id) {
    levels <- c(0.90, 0.95, 0.99)
    # Klikniecie poziomu ufnosci -> wybiera conf, RESETUJE revealed na FALSE
    lapply(levels, function(lv) {
      force(lv)
      observeEvent(input[[paste0("ch5_", case_id, "_conf", round(lv * 100))]], {
        ch5_edge_state[[case_id]] <- list(conf = lv, revealed = FALSE)
      }, ignoreInit = TRUE)
    })

    # Przycisk "Pokaz werdykt" -> ustawia revealed = TRUE (zachowujac obecny conf)
    observeEvent(input[[paste0("ch5_", case_id, "_reveal")]], {
      current <- ch5_edge_state[[case_id]]
      if (!is.na(current$conf) && !current$revealed) {
        ch5_edge_state[[case_id]] <- list(conf = current$conf, revealed = TRUE)
      }
    }, ignoreInit = TRUE)

    output[[paste0("ch5_", case_id, "_buttons")]] <- renderUI({
      ch5_edge_state[[case_id]]
      edge_buttons_ui(case_id)
    })
    output[[paste0("ch5_", case_id, "_plot")]] <- renderPlot({
      ch5_edge_state[[case_id]]
      render_edge_plot(case_id)
    })
    output[[paste0("ch5_", case_id, "_explain")]] <- renderUI({
      ch5_edge_state[[case_id]]
      render_edge_explain(case_id)
    })
  }

  for (cid in names(edge_cases)) {
    register_edge_case(cid)
  }
}
