# Rozdział 2: Idea przedziałów ufności

ch2_ui <- tabPanel("2. Idea CI",
  withMathJax(),
  fluidRow(column(8, offset = 2,

    # ---- Część 1: Intro + opis ----
    include_content("ch2_idea", 1),

    # ---- Widget: ch2_ci_sim ----
    div(class = "widget-block",
      fluidRow(
        column(3, selectInput("ch2_dist", "Rozkład:", choices = c(
          "Normalny" = "normal", "Wykładniczy" = "exponential",
          "Jednostajny" = "uniform", "Skośny" = "skewed"
        ), selected = "normal")),
        column(3, sliderInput("ch2_n", "n:", min = 5, max = 100, value = 25, step = 5)),
        column(3, sliderInput("ch2_conf", "Poziom ufności:",
                               min = 0.80, max = 0.99, value = 0.95, step = 0.01)),
        column(3, actionButton("ch2_sim", "Symuluj 100 CI",
                               class = "btn-primary", width = "100%"))
      ),
      plotOutput("ch2_ci_plot", height = "500px"),
      uiOutput("ch2_coverage_info")
    ),

    # ---- Część 2: Budowa przedziału + błędy interpretacji ----
    include_content("ch2_idea", 2)

  ))
)

# ===========================================================================
# SERVER
# ===========================================================================

ch2_server <- function(input, output, session) {

  ch2_sim_data <- reactiveVal(NULL)

  observeEvent(input$ch2_sim, {
    results <- simulate_coverage(input$ch2_dist, input$ch2_n, input$ch2_conf, 100)
    ch2_sim_data(results)
  })

  output$ch2_ci_plot <- renderPlot({
    df <- ch2_sim_data()
    req(df)
    params <- get_population_params(input$ch2_dist)

    df$color <- ifelse(df$covers, col_hit, col_miss)

    ggplot(df, aes(y = sim)) +
      geom_vline(xintercept = params$mu, color = col_true,
                 linewidth = 1.2, linetype = "dashed") +
      geom_segment(aes(x = lower, xend = upper, yend = sim, color = covers),
                   linewidth = 0.8) +
      geom_point(aes(x = xbar), color = col_dark, size = 1) +
      scale_color_manual(values = c("TRUE" = col_hit, "FALSE" = col_miss),
                         labels = c("TRUE" = "Trafiony", "FALSE" = "Pudło"),
                         name = "") +
      labs(title = paste0("100 przedziałów ufności (", input$ch2_conf * 100, "%)"),
           x = "Wartość", y = "Numer próby") +
      theme_ci()
  })

  output$ch2_coverage_info <- renderUI({
    df <- ch2_sim_data()
    req(df)
    actual <- mean(df$covers) * 100
    expected <- input$ch2_conf * 100
    color <- if (abs(actual - expected) <= 5) col_hit else col_miss
    div(style = "text-align: center; margin-top: 10px;",
      stat_box(paste0("Pokrycie: ", actual, "% (oczekiwane: ", expected, "%)"), color)
    )
  })
}
