# Rozdział 1: Estymacja punktowa
# UI składa: HTML narracja (z Quarto) + widgety Shiny

ch1_ui <- tabPanel("1. Estymacja",
  withMathJax(),
  fluidRow(column(8, offset = 2,

    # ---- Część 1: Intro + "Estymator w akcji" ----
    include_content("ch1_estymacja", 1),

    # ---- Widget: ch1_estimator ----
    div(class = "widget-block",
      selectInput("ch1_dist", "Rozkład populacji:",
        choices = c("Normalny (wzrost)" = "normal",
                    "Wykładniczy" = "exponential",
                    "Jednostajny" = "uniform",
                    "Bimodalny" = "bimodal"),
        selected = "normal", width = "100%"
      ),
      sliderInput("ch1_n", "Wielkość próby (n):",
                  min = 5, max = 200, value = 30, step = 5, width = "100%"),
      fluidRow(
        column(4, actionButton("ch1_draw_1", "Losuj 1 próbę",
                               class = "btn-primary", width = "100%")),
        column(4, actionButton("ch1_draw_20", "Losuj 20 prób",
                               class = "btn-warning", width = "100%")),
        column(4, actionButton("ch1_reset", "Reset",
                               class = "btn-outline-secondary", width = "100%"))
      ),
      plotOutput("ch1_estimates_plot", height = "350px"),
      uiOutput("ch1_estimates_stats")
    ),

    # ---- Część 2: Callout + "Obciążenie vs trafność" ----
    include_content("ch1_estymacja", 2),

    # ---- Widget: ch1_bias ----
    div(class = "widget-block",
      selectInput("ch1_bias_dist", "Rozkład:", choices = c(
        "Normalny (wzrost)" = "normal", "Skośny (gamma)" = "skewed"
      ), width = "100%"),
      sliderInput("ch1_bias_n", "Wielkość próby:",
                  min = 5, max = 200, value = 30, step = 5, width = "100%"),
      actionButton("ch1_bias_run", "Symuluj 500 prób",
                   class = "btn-primary", width = "100%"),
      plotOutput("ch1_bias_plot", height = "300px")
    ),

    # ---- Część 3: "Dlaczego sam punkt nie wystarczy?" ----
    include_content("ch1_estymacja", 3)

  ))
)

# ===========================================================================
# SERVER
# ===========================================================================

ch1_server <- function(input, output, session) {

  # --- Estimator widget ---
  ch1_estimates <- reactiveVal(data.frame(i = integer(0), xbar = numeric(0)))

  draw_samples <- function(k) {
    dist <- input$ch1_dist
    n <- input$ch1_n
    old <- ch1_estimates()
    new_rows <- lapply(seq_len(k), function(j) {
      samp <- generate_population_sample(dist, n)
      data.frame(i = nrow(old) + j, xbar = mean(samp))
    })
    ch1_estimates(rbind(old, do.call(rbind, new_rows)))
  }

  observeEvent(input$ch1_draw_1, draw_samples(1))
  observeEvent(input$ch1_draw_20, draw_samples(20))
  observeEvent(input$ch1_reset, {
    ch1_estimates(data.frame(i = integer(0), xbar = numeric(0)))
  })

  observe({
    input$ch1_dist
    input$ch1_n
    ch1_estimates(data.frame(i = integer(0), xbar = numeric(0)))
  })

  output$ch1_estimates_plot <- renderPlot({
    est <- ch1_estimates()
    params <- get_population_params(input$ch1_dist)

    if (nrow(est) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij przycisk, aby losować próby",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      ggplot(est, aes(x = xbar)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30,
                       fill = col_ci, alpha = 0.6, color = "white") +
        geom_vline(xintercept = params$mu, color = col_true,
                   linewidth = 1.5, linetype = "dashed") +
        annotate("text", x = params$mu, y = Inf, vjust = 2,
                 label = paste0("\u03bc = ", params$mu),
                 color = col_true, fontface = "bold", size = 5) +
        labs(title = paste0("Rozkład estymat średniej (n = ", nrow(est), " prób)"),
             x = expression(bar(x)), y = "Gęstość") +
        theme_ci()
    }
  })

  output$ch1_estimates_stats <- renderUI({
    est <- ch1_estimates()
    if (nrow(est) < 2) return(NULL)
    params <- get_population_params(input$ch1_dist)
    div(style = "text-align: center; margin-top: 10px;",
      stat_box(paste0("Średnia estymat: ", round(mean(est$xbar), 2)), col_ci),
      stat_box(paste0("SD estymat: ", round(sd(est$xbar), 2)), col_dark),
      stat_box(paste0("\u03bc = ", params$mu), col_true)
    )
  })

  # --- Bias widget ---
  output$ch1_bias_plot <- renderPlot({
    input$ch1_bias_run
    isolate({
      dist <- input$ch1_bias_dist
      n <- input$ch1_bias_n
      params <- get_population_params(dist)

      samps <- replicate(500, generate_population_sample(dist, n), simplify = FALSE)
      means   <- sapply(samps, mean)
      medians <- sapply(samps, median)
      trimmed <- sapply(samps, function(x) mean(x, trim = 0.1))

      df <- data.frame(
        value = c(means, medians, trimmed),
        estymator = rep(c("Średnia", "Mediana", "Średnia obcięta (10%)"), each = 500)
      )

      ggplot(df, aes(x = value)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30,
                       fill = col_ci, alpha = 0.6, color = "white") +
        geom_vline(xintercept = params$mu, color = col_true,
                   linewidth = 1.2, linetype = "dashed") +
        facet_wrap(~estymator, scales = "free_y") +
        labs(title = "Porównanie trzech estymatorów (500 symulacji)",
             x = "Wartość estymatora", y = "Gęstość") +
        theme_ci(base_size = 12)
    })
  }) |> bindEvent(input$ch1_bias_run)
}
