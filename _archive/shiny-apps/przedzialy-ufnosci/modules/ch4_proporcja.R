# ============================================================================
# CHAPTER 4: Przedzial dla proporcji
# ============================================================================

ch4_ui <- tabPanel("4. Przedzia\u0142 dla proporcji",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Umiemy ju\u017c budowa\u0107 przedzia\u0142 dla \u015bredniej.
       A co, gdy interesuje nas odsetek (proporcja)?"
    ),

    div(class = "section-title", "Przedzia\u0142 ufno\u015bci dla proporcji"),

    div(class = "narrative",
      p("Cz\u0119sto chcemy oszacowa\u0107 ", tags$b("odsetek"),
        " \u2014 np. jaki procent student\u00f3w zda\u0142 egzamin, jaki odsetek wyborc\u00f3w
        g\u0142osuje na parti\u0119 X, jaki procent produkt\u00f3w jest wadliwy."),
      p("Estymator to proporcja z pr\u00f3by ",
        withMathJax("\\(\\hat{p} = \\frac{x}{n}\\)"),
        ", a przedzia\u0142 ufno\u015bci mo\u017cna skonstruowa\u0107 na dwa sposoby.")
    ),

    div(class = "formula-box",
      p(tags$strong("Przedzia\u0142 Walda:"),
        withMathJax("\\(\\hat{p} \\pm z^* \\sqrt{\\frac{\\hat{p}(1-\\hat{p})}{n}}\\)")),
      p(tags$strong("Przedzia\u0142 Wilsona"), " (lepszy dla ma\u0142ych n i skrajnych p) \u2014 koryguje wz\u00f3r uwzgl\u0119dniaj\u0105c niepewno\u015b\u0107.")
    ),

    # ========================================================================
    # WIDGET 1: Symulacja proporcji
    # ========================================================================
    div(class = "section-title", "Symulacja: 100 przedzia\u0142\u00f3w dla proporcji"),

    div(class = "narrative",
      p("Analogicznie do \u015bredniej \u2014 losujemy 100 pr\u00f3b,
        konstruujemy przedzia\u0142y i sprawdzamy pokrycie.
        Por\u00f3wnaj Walda z Wilsonem!")
    ),

    div(class = "widget-block",
      h4("Wald vs Wilson"),
      fluidRow(
        column(4,
          sliderInput("ch4_true_p", "Prawdziwe p:",
                      min = 0.01, max = 0.99, value = 0.5, step = 0.01),
          sliderInput("ch4_n", "Wielko\u015b\u0107 pr\u00f3by (n):",
                      min = 10, max = 200, value = 30, step = 5),
          sliderInput("ch4_conf", "Poziom ufno\u015bci:",
                      min = 0.80, max = 0.99, value = 0.95, step = 0.01),
          radioButtons("ch4_method", "Metoda:",
            choices = c("Wald" = "wald", "Wilson" = "wilson"),
            selected = "wald", inline = TRUE
          ),
          hr(),
          actionButton("ch4_sim", "Losuj 100 przedzia\u0142\u00f3w",
                       class = "btn-primary", width = "100%"),
          br(), br(),
          uiOutput("ch4_coverage_info")
        ),
        column(8,
          plotOutput("ch4_ci_plot", height = "500px")
        )
      )
    ),

    div(class = "callout-warning",
      tags$strong("Uwaga na Walda!"),
      " Ustaw p blisko 0 lub 1 (np. 0.05) i ma\u0142e n (np. 20).
        Zobaczysz, \u017ce pokrycie Walda spada znacznie poni\u017cej 95%.
        Wilson radzi sobie lepiej w tych warunkach."
    ),

    # ========================================================================
    # WIDGET 2: Heatmapa pokrycia
    # ========================================================================
    div(class = "section-title", "Kiedy aproksymacja normalna dzia\u0142a?"),

    div(class = "narrative",
      p("Przedzia\u0142 Walda opiera si\u0119 na przybli\u017ceniu normalnym.
        Dzia\u0142a dobrze, gdy ", withMathJax("\\(np \\geq 10\\)"),
        " i ", withMathJax("\\(n(1-p) \\geq 10\\)"),
        ". Poni\u017cej tych granic pokrycie jest za niskie."),
      p("Heatmapa pokazuje rzeczywiste pokrycie dla r\u00f3\u017cnych kombinacji n i p.")
    ),

    div(class = "widget-block",
      h4("Mapa pokrycia"),
      fluidRow(
        column(4,
          radioButtons("ch4_heat_method", "Metoda:",
            choices = c("Wald" = "wald", "Wilson" = "wilson"),
            selected = "wald", inline = TRUE
          ),
          helpText("Symulacja: 500 przedzia\u0142\u00f3w dla ka\u017cdej kombinacji (n, p).
                    95% poziom ufno\u015bci. Mo\u017ce potrwa\u0107 kilka sekund."),
          actionButton("ch4_heat_run", "Generuj heatmap\u0119",
                       class = "btn-warning", width = "100%")
        ),
        column(8,
          plotOutput("ch4_heat_plot", height = "400px")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Zasada:"),
      " Gdy ", withMathJax("\\(np < 10\\)"), " lub ",
      withMathJax("\\(n(1-p) < 10\\)"),
      ", u\u017cyj przedzia\u0142u Wilsona lub metody dok\u0142adnej (Cloppera-Pearsona)."
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: co decyduje o szeroko\u015bci przedzia\u0142u?"),
      actionButton("ch4_next", "Dalej \u2192 5. Co wp\u0142ywa na szeroko\u015b\u0107?",
                   class = "btn-primary btn-lg")
    )
  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch4_server <- function(input, output, session) {

  # --- Widget 1: 100 przedzialow dla proporcji ---
  ch4_sim_data <- reactiveVal(NULL)

  observeEvent(input$ch4_sim, {
    result <- simulate_coverage_prop(
      true_p = input$ch4_true_p,
      n = input$ch4_n,
      conf_level = input$ch4_conf,
      n_sims = 100,
      method = input$ch4_method
    )
    ch4_sim_data(result)
  })

  output$ch4_ci_plot <- renderPlot({
    df <- ch4_sim_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Losuj 100 przedzia\u0142\u00f3w'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      true_p <- input$ch4_true_p

      ggplot(df, aes(y = sim)) +
        geom_vline(xintercept = true_p, color = col_true,
                   linewidth = 1.2, linetype = "dashed") +
        geom_segment(aes(x = lower, xend = upper, yend = sim, color = covers),
                     linewidth = 0.8) +
        geom_point(aes(x = phat, color = covers), size = 1.5) +
        scale_color_manual(values = c("TRUE" = col_hit, "FALSE" = col_miss),
                           labels = c("TRUE" = "Trafiony", "FALSE" = "Chybiony"),
                           name = NULL) +
        xlim(0, 1) +
        labs(title = paste0("100 przedzia\u0142\u00f3w ",
                            ifelse(input$ch4_method == "wald", "Walda", "Wilsona"),
                            " (", round(input$ch4_conf * 100), "%)"),
             subtitle = paste0("Prawdziwe p = ", input$ch4_true_p),
             x = "Proporcja",
             y = "Numer pr\u00f3by") +
        theme_ci() +
        theme(legend.position = "top")
    }
  })

  output$ch4_coverage_info <- renderUI({
    df <- ch4_sim_data()
    if (is.null(df)) return(NULL)
    coverage <- mean(df$covers) * 100
    color <- if (abs(coverage - input$ch4_conf * 100) <= 5) col_hit else col_miss
    tagList(
      div(class = "stat-box", style = paste0("background:", color, ";"),
          paste0("Pokrycie: ", coverage, "%")),
      div(class = "stat-box", style = paste0("background:", col_dark, ";"),
          paste0("Oczekiwane: ", round(input$ch4_conf * 100), "%"))
    )
  })

  # --- Widget 2: Heatmapa pokrycia ---
  ch4_heat_data <- reactiveVal(NULL)

  observeEvent(input$ch4_heat_run, {
    method <- input$ch4_heat_method
    p_vals <- seq(0.05, 0.95, by = 0.05)
    n_vals <- c(10, 20, 30, 50, 75, 100, 150, 200)

    results <- expand.grid(p = p_vals, n = n_vals)
    results$coverage <- mapply(function(p, n) {
      sim <- simulate_coverage_prop(p, n, 0.95, 500, method)
      mean(sim$covers)
    }, results$p, results$n)

    ch4_heat_data(results)
  })

  output$ch4_heat_plot <- renderPlot({
    df <- ch4_heat_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Generuj heatmap\u0119'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      ggplot(df, aes(x = p, y = factor(n), fill = coverage)) +
        geom_tile(color = "white", linewidth = 0.5) +
        geom_text(aes(label = round(coverage * 100, 0)), size = 3.5) +
        scale_fill_gradient2(low = col_miss, mid = "white", high = col_hit,
                             midpoint = 0.95, limits = c(0.80, 1),
                             name = "Pokrycie") +
        labs(title = paste0("Pokrycie (%) \u2014 metoda ",
                            ifelse(input$ch4_heat_method == "wald", "Walda", "Wilsona")),
             x = "Prawdziwe p",
             y = "Wielko\u015b\u0107 pr\u00f3by (n)") +
        theme_ci() +
        theme(legend.position = "right")
    }
  })
}
