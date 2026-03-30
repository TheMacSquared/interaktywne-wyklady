# ============================================================================
# CHAPTER 2: Idea przedzialow ufnosci
# ============================================================================

ch2_ui <- tabPanel("2. Idea przedzia\u0142\u00f3w",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Estymata punktowa zmienia si\u0119 z pr\u00f3by na pr\u00f3b\u0119.
       Czas doda\u0107 do niej zakres niepewno\u015bci."
    ),

    div(class = "section-title", "Czym jest przedzia\u0142 ufno\u015bci?"),

    div(class = "narrative",
      p("Przedzia\u0142 ufno\u015bci (CI \u2014 confidence interval) to zakres warto\u015bci,
        kt\u00f3ry z okre\u015blonym ", tags$b("poziomem ufno\u015bci"),
        " (np. 95%) zawiera prawdziwy parametr populacji."),
      p("Kluczowa idea: gdyby\u015bmy powtarzali eksperyment wiele razy,
        to ", tags$b("95% skonstruowanych przedzia\u0142\u00f3w"),
        " zawiera\u0142oby prawdziwe ", withMathJax("\\(\\mu\\)"), ".")
    ),

    # ========================================================================
    # WIDGET 1: 100 przedzialow ufnosci
    # ========================================================================
    div(class = "section-title", "100 przedzia\u0142\u00f3w ufno\u015bci"),

    div(class = "narrative",
      p("To kluczowa wizualizacja. Ka\u017cdy poziomy odcinek to jeden przedzia\u0142
        ufno\u015bci \u2014 skonstruowany z osobnej pr\u00f3by. Zielone trafiaj\u0105 w ",
        withMathJax("\\(\\mu\\)"), ", czerwone \u2014 nie.")
    ),

    div(class = "widget-block",
      h4("Symulacja 100 przedzia\u0142\u00f3w"),
      fluidRow(
        column(4,
          selectInput("ch2_dist", "Rozk\u0142ad populacji:",
            choices = c(
              "Normalny (wzrost)"           = "normal",
              "Wyk\u0142adniczy (prawosko\u015bny)" = "exponential",
              "Jednostajny"                 = "uniform"
            ),
            selected = "normal"
          ),
          sliderInput("ch2_n", "Wielko\u015b\u0107 pr\u00f3by (n):",
                      min = 5, max = 100, value = 30, step = 5),
          sliderInput("ch2_conf", "Poziom ufno\u015bci:",
                      min = 0.80, max = 0.99, value = 0.95, step = 0.01),
          hr(),
          actionButton("ch2_sim", "Losuj 100 przedzia\u0142\u00f3w",
                       class = "btn-primary", width = "100%"),
          br(), br(),
          uiOutput("ch2_coverage_info")
        ),
        column(8,
          plotOutput("ch2_ci_plot", height = "500px")
        )
      )
    ),

    div(class = "callout-success",
      tags$strong("Aha-moment:"),
      " Przy 95% poziomie ufno\u015bci oko\u0142o 5 z 100 przedzia\u0142\u00f3w ",
      tags$b("nie trafi"), " w prawdziwe \u03bc. To nie b\u0142\u0105d \u2014 to w\u0142a\u015bnie ",
      "znaczenie \"95% ufno\u015bci\"!"
    ),

    # ========================================================================
    # WIDGET 2: Jeden przedzial krok po kroku
    # ========================================================================
    div(class = "section-title", "Budowa przedzia\u0142u \u2014 krok po kroku"),

    div(class = "narrative",
      p("Zobaczmy, jak powstaje jeden przedzia\u0142 ufno\u015bci dla \u015bredniej.
        Ka\u017cdy krok buduje kolejn\u0105 cz\u0119\u015b\u0107 wzoru:"),
      div(class = "formula-box",
        withMathJax(helpText(
          "$$\\bar{x} \\pm t^* \\cdot \\frac{s}{\\sqrt{n}}$$"
        ))
      )
    ),

    div(class = "widget-block",
      h4("Konstruowanie przedzia\u0142u"),
      div(class = "step-buttons",
        actionButton("ch2_step1", "1. Pr\u00f3ba", class = "btn-outline-primary"),
        actionButton("ch2_step2", "2. Statystyki", class = "btn-outline-primary"),
        actionButton("ch2_step3", "3. Wart. krytyczna", class = "btn-outline-primary"),
        actionButton("ch2_step4", "4. Margines b\u0142\u0119du", class = "btn-outline-primary"),
        actionButton("ch2_step5", "5. Przedzia\u0142!", class = "btn-outline-primary")
      ),
      plotOutput("ch2_step_plot", height = "300px"),
      uiOutput("ch2_step_explanation")
    ),

    # ========================================================================
    # WIDGET 3: Czesty blad interpretacji
    # ========================================================================
    div(class = "section-title", "Jak (nie) interpretowa\u0107 przedzia\u0142 ufno\u015bci"),

    div(class = "narrative",
      p("95% przedzia\u0142 ufno\u015bci [165, 175] dla \u015bredniej wzrostu.
        Kt\u00f3ra interpretacja jest poprawna?")
    ),

    div(class = "widget-block",
      h4("Quiz: interpretacja CI"),
      radioButtons("ch2_quiz", "Wybierz poprawn\u0105 interpretacj\u0119:",
        choices = c(
          "A) Jest 95% prawdopodobie\u0144stwa, \u017ce \u03bc le\u017cy w [165, 175]" = "A",
          "B) 95% danych z populacji le\u017cy w [165, 175]" = "B",
          "C) Gdyby\u015bmy powtarzali badanie, 95% tak skonstruowanych przedzia\u0142\u00f3w zawiera\u0142oby \u03bc" = "C",
          "D) Jeste\u015bmy w 95% pewni, \u017ce \u015brednia z pr\u00f3by le\u017cy w [165, 175]" = "D"
        ),
        selected = character(0)
      ),
      actionButton("ch2_check", "Sprawd\u017a", class = "btn-primary"),
      br(), br(),
      uiOutput("ch2_quiz_feedback")
    ),

    div(class = "callout-danger",
      tags$strong("Cz\u0119sty b\u0142\u0105d:"),
      " Przedzia\u0142 ufno\u015bci ", tags$b("nie m\u00f3wi"),
      " o prawdopodobie\u0144stwie, \u017ce parametr le\u017cy w konkretnym przedziale.
        Parametr jest sta\u0142y \u2014 to przedzia\u0142 jest losowy!
        Poprawnie: \"metoda daje przedzia\u0142y, kt\u00f3re w 95% przypadk\u00f3w trafaj\u0105\"."
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: konkretne wzory \u2014 przedzia\u0142 dla \u015bredniej"),
      actionButton("ch2_next", "Dalej \u2192 3. Przedzia\u0142 dla \u015bredniej",
                   class = "btn-primary btn-lg")
    )
  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch2_server <- function(input, output, session) {

  # --- Widget 1: 100 przedzialow ---
  ch2_sim_data <- reactiveVal(NULL)

  observeEvent(input$ch2_sim, {
    result <- simulate_coverage(
      dist_type = input$ch2_dist,
      n = input$ch2_n,
      conf_level = input$ch2_conf,
      n_sims = 100,
      method = "t"
    )
    ch2_sim_data(result)
  })

  output$ch2_ci_plot <- renderPlot({
    df <- ch2_sim_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Losuj 100 przedzia\u0142\u00f3w'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      params <- get_population_params(input$ch2_dist)
      df$color <- ifelse(df$covers, col_hit, col_miss)

      ggplot(df, aes(y = sim)) +
        geom_vline(xintercept = params$mu, color = col_true,
                   linewidth = 1.2, linetype = "dashed") +
        geom_segment(aes(x = lower, xend = upper, yend = sim, color = covers),
                     linewidth = 0.8) +
        geom_point(aes(x = xbar, color = covers), size = 1.5) +
        scale_color_manual(values = c("TRUE" = col_hit, "FALSE" = col_miss),
                           labels = c("TRUE" = "Trafiony", "FALSE" = "Chybiony"),
                           name = NULL) +
        labs(title = paste0("100 przedzia\u0142\u00f3w ufno\u015bci (",
                            round(input$ch2_conf * 100), "%)"),
             x = "Warto\u015b\u0107 parametru",
             y = "Numer pr\u00f3by") +
        theme_ci() +
        theme(legend.position = "top")
    }
  })

  output$ch2_coverage_info <- renderUI({
    df <- ch2_sim_data()
    if (is.null(df)) return(NULL)
    coverage <- mean(df$covers) * 100
    color <- if (abs(coverage - input$ch2_conf * 100) <= 5) col_hit else col_miss
    tagList(
      div(class = "stat-box", style = paste0("background:", color, ";"),
          paste0("Pokrycie: ", coverage, "%")),
      div(class = "stat-box", style = paste0("background:", col_dark, ";"),
          paste0("Oczekiwane: ", round(input$ch2_conf * 100), "%"))
    )
  })

  # --- Widget 2: Krok po kroku ---
  ch2_step <- reactiveVal(0)
  ch2_sample <- reactiveVal(NULL)

  observe({
    # Reset on any step 1 click
    if (input$ch2_step1 > 0) {
      ch2_step()  # track dependency
    }
  })

  observeEvent(input$ch2_step1, {
    ch2_step(1)
    ch2_sample(generate_population_sample("normal", 25))
  })
  observeEvent(input$ch2_step2, { ch2_step(2) })
  observeEvent(input$ch2_step3, { ch2_step(3) })
  observeEvent(input$ch2_step4, { ch2_step(4) })
  observeEvent(input$ch2_step5, { ch2_step(5) })

  output$ch2_step_plot <- renderPlot({
    step <- ch2_step()
    samp <- ch2_sample()
    params <- get_population_params("normal")

    if (step == 0 || is.null(samp)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij '1. Pr\u00f3ba' aby zacz\u0105\u0107",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      xbar <- mean(samp)
      s <- sd(samp)
      n <- length(samp)
      t_star <- qt(0.975, df = n - 1)
      me <- t_star * s / sqrt(n)

      xlims <- c(params$mu - 4 * params$sigma / sqrt(n),
                 params$mu + 4 * params$sigma / sqrt(n))

      p <- ggplot() +
        geom_vline(xintercept = params$mu, color = col_true,
                   linewidth = 1, linetype = "dashed", alpha = 0.5) +
        xlim(xlims) +
        labs(x = "Warto\u015b\u0107", y = "") +
        theme_ci() +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

      if (step >= 1) {
        p <- p + geom_point(aes(x = xbar, y = 0), color = col_estimate,
                            size = 5, shape = 18)
      }
      if (step >= 4) {
        p <- p + geom_errorbarh(aes(xmin = xbar - me, xmax = xbar + me, y = 0),
                                height = 0.1, color = col_ci, linewidth = 1.5)
      }
      if (step >= 5) {
        covers <- (xbar - me <= params$mu) & (params$mu <= xbar + me)
        ci_color <- if (covers) col_hit else col_miss
        p <- p +
          geom_errorbarh(aes(xmin = xbar - me, xmax = xbar + me, y = 0),
                         height = 0.1, color = ci_color, linewidth = 2) +
          annotate("text", x = xbar, y = 0.15,
                   label = paste0("[", round(xbar - me, 1), " ; ",
                                  round(xbar + me, 1), "]"),
                   size = 5, fontface = "bold", color = ci_color)
      }

      p + ggtitle(paste0("Krok ", step, " z 5"))
    }
  })

  output$ch2_step_explanation <- renderUI({
    step <- ch2_step()
    samp <- ch2_sample()
    if (step == 0 || is.null(samp)) return(NULL)

    xbar <- mean(samp)
    s <- sd(samp)
    n <- length(samp)
    t_star <- qt(0.975, df = n - 1)
    me <- t_star * s / sqrt(n)

    explanation <- switch(as.character(step),
      "1" = div(class = "callout-info",
        p(tags$strong("Krok 1:"), " Pobieramy pr\u00f3b\u0119 n = ", n, " obserwacji."),
        p("Punkt na wykresie to \u015brednia z pr\u00f3by: ",
          withMathJax(paste0("\\(\\bar{x} = ", round(xbar, 2), "\\)")))
      ),
      "2" = div(class = "callout-info",
        p(tags$strong("Krok 2:"), " Obliczamy statystyki z pr\u00f3by:"),
        p(withMathJax(paste0("\\(\\bar{x} = ", round(xbar, 2), "\\)"))),
        p(withMathJax(paste0("\\(s = ", round(s, 2), "\\)"))),
        p(withMathJax(paste0("\\(n = ", n, "\\)")))
      ),
      "3" = div(class = "callout-info",
        p(tags$strong("Krok 3:"), " Warto\u015b\u0107 krytyczna z rozk\u0142adu t:"),
        p(withMathJax(paste0("\\(t^* = t_{0.975, ", n - 1, "} = ",
                             round(t_star, 3), "\\)")))
      ),
      "4" = div(class = "callout-info",
        p(tags$strong("Krok 4:"), " Margines b\u0142\u0119du:"),
        p(withMathJax(paste0("\\(ME = t^* \\cdot \\frac{s}{\\sqrt{n}} = ",
                             round(t_star, 3), " \\cdot \\frac{",
                             round(s, 2), "}{\\sqrt{", n, "}} = ",
                             round(me, 2), "\\)")))
      ),
      "5" = {
        covers <- (xbar - me <= 170) & (170 <= xbar + me)
        div(class = if (covers) "callout-success" else "callout-danger",
          p(tags$strong("Krok 5: Przedzia\u0142 ufno\u015bci!")),
          p(withMathJax(paste0("\\([", round(xbar - me, 2), " \\;; \\; ",
                               round(xbar + me, 2), "]\\)"))),
          p(if (covers) "Przedzia\u0142 zawiera prawdziwe \u03bc = 170"
            else "Przedzia\u0142 NIE zawiera prawdziwego \u03bc = 170")
        )
      }
    )
    explanation
  })

  # --- Widget 3: Quiz ---
  output$ch2_quiz_feedback <- renderUI({
    req(input$ch2_check)
    isolate({
      answer <- input$ch2_quiz
      if (is.null(answer) || answer == "") {
        div(class = "callout-warning", "Wybierz odpowied\u017a!")
      } else if (answer == "C") {
        div(class = "callout-success",
          tags$strong("Poprawnie!"),
          p("Przedzia\u0142 ufno\u015bci opisuje ",
            tags$b("metod\u0119"), ", nie konkretny wynik.
            95% przedzia\u0142\u00f3w skonstruowanych t\u0105 metod\u0105 zawiera prawdziwe \u03bc.")
        )
      } else {
        feedback <- switch(answer,
          "A" = "To najcz\u0119stszy b\u0142\u0105d! \u03bc jest sta\u0142e, nie losowe. To przedzia\u0142 jest losowy, nie parametr.",
          "B" = "Nie \u2014 przedzia\u0142 dotyczy parametru (\u015bredniej), nie poszczeg\u00f3lnych obserwacji.",
          "D" = "Nie \u2014 \u015brednia z pr\u00f3by zawsze le\u017cy w \u015brodku przedzia\u0142u (jest punktem wyj\u015bcia)."
        )
        div(class = "callout-danger",
          tags$strong("Nie do ko\u0144ca!"),
          p(feedback),
          p("Poprawna odpowied\u017a to C.")
        )
      }
    })
  })
}
