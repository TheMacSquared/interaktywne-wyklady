# Wykład Regresja — Rozdział 1: Regresja liniowa prosta

ch_reg1_ui <- tabPanel("1. Liniowa prosta",
  withMathJax(),
  fluidRow(column(8, offset = 2,

    # ---- Część 1: Idea + dopasowanie ----
    include_content("ch_reg1_liniowa", 1),

    # ---- Widget: reg1_fit ----
    div(class = "widget-block",
      fluidRow(
        column(3, sliderInput("reg1_n", "Wielkość próby (n):",
                              min = 10, max = 200, value = 50, step = 10)),
        column(3, sliderInput("reg1_beta1", "Nachylenie (β₁):",
                              min = -3, max = 3, value = 2, step = 0.5)),
        column(3, sliderInput("reg1_sigma", "Szum (σ):",
                              min = 0.5, max = 5, value = 1, step = 0.5)),
        column(3, actionButton("reg1_gen", "Generuj dane",
                               class = "btn-primary", width = "100%"))
      ),
      plotOutput("reg1_fit_plot", height = "350px"),
      uiOutput("reg1_fit_stats")
    ),

    # ---- Część 2: Callout + Reszty ----
    include_content("ch_reg1_liniowa", 2),

    # ---- Widget: reg1_residuals ----
    div(class = "widget-block",
      plotOutput("reg1_resid_plot", height = "280px")
    ),

    # ---- Część 3: Callout o problemach ----
    include_content("ch_reg1_liniowa", 3)

  ))
)

# ===========================================================================
# SERVER
# ===========================================================================

ch_reg1_server <- function(input, output, session) {

  reg1_data <- reactiveVal(NULL)

  observeEvent(input$reg1_gen, {
    n <- input$reg1_n
    beta1 <- input$reg1_beta1
    sigma <- input$reg1_sigma
    x <- runif(n, 0, 10)
    y <- 2 + beta1 * x + rnorm(n, 0, sigma)
    reg1_data(data.frame(x = x, y = y))
  })

  output$reg1_fit_plot <- renderPlot({
    df <- reg1_data()
    req(df)

    model <- lm(y ~ x, data = df)

    ggplot(df, aes(x = x, y = y)) +
      geom_point(color = col_dark, alpha = 0.5, size = 2) +
      geom_smooth(method = "lm", se = TRUE,
                  color = col_primary, fill = col_primary, alpha = 0.1) +
      labs(title = "Regresja liniowa prosta",
           subtitle = paste0("y = ", round(coef(model)[1], 2), " + ",
                            round(coef(model)[2], 2), " · x"),
           x = "X", y = "Y") +
      theme_lecture()
  })

  output$reg1_fit_stats <- renderUI({
    df <- reg1_data()
    req(df)
    model <- lm(y ~ x, data = df)
    r2 <- summary(model)$r.squared
    div(style = "text-align: center; margin-top: 10px;",
      stat_box(paste0("β₀ = ", round(coef(model)[1], 2)), col_dark),
      stat_box(paste0("β₁ = ", round(coef(model)[2], 2)), col_primary),
      stat_box(paste0("R² = ", round(r2, 3)), col_success)
    )
  })

  output$reg1_resid_plot <- renderPlot({
    df <- reg1_data()
    req(df)
    model <- lm(y ~ x, data = df)
    resid_df <- data.frame(fitted = fitted(model), residuals = residuals(model))

    ggplot(resid_df, aes(x = fitted, y = residuals)) +
      geom_point(color = col_dark, alpha = 0.5, size = 2) +
      geom_hline(yintercept = 0, color = col_secondary,
                 linewidth = 1, linetype = "dashed") +
      labs(title = "Wykres reszt",
           x = "Wartości dopasowane", y = "Reszty") +
      theme_lecture()
  })
}
