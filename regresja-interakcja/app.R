# Regresja z Interakcją - Porównanie Modeli
# Interaktywna wizualizacja różnicy między modelem addytywnym a modelem z interakcją

library(shiny)
library(ggplot2)
library(dplyr)

# ============================================================================
# FUNKCJE POMOCNICZE
# ============================================================================

generate_regression_data <- function(n_per_group,
                                      intercept_a, slope_a,
                                      intercept_b, slope_b,
                                      noise_sd) {
  set.seed(NULL)

  # Grupa A
  x_a <- runif(n_per_group, 0, 10)
  y_a <- intercept_a + slope_a * x_a + rnorm(n_per_group, 0, noise_sd)


  # Grupa B
  x_b <- runif(n_per_group, 0, 10)
  y_b <- intercept_b + slope_b * x_b + rnorm(n_per_group, 0, noise_sd)

  data.frame(
    x = c(x_a, x_b),
    y = c(y_a, y_b),
    grupa = factor(rep(c("A", "B"), each = n_per_group))
  )
}

fit_models <- function(data) {
  # Model addytywny (bez interakcji) - równoległe linie
  model_additive <- lm(y ~ x + grupa, data = data)


  # Model z interakcją - różne nachylenia
  model_interaction <- lm(y ~ x * grupa, data = data)

  # Test porównania modeli (czy interakcja jest istotna?)
  anova_test <- anova(model_additive, model_interaction)

  list(
    additive = model_additive,
    interaction = model_interaction,
    anova = anova_test,
    interaction_pvalue = anova_test$`Pr(>F)`[2]
  )
}

get_model_lines <- function(model, model_type) {
  coefs <- coef(model)

  if (model_type == "additive") {
    # y = b0 + b1*x + b2*grupaB
    # Grupa A: y = b0 + b1*x
    # Grupa B: y = (b0 + b2) + b1*x
    data.frame(
      grupa = c("A", "B"),
      intercept = c(coefs[1], coefs[1] + coefs[3]),
      slope = c(coefs[2], coefs[2])
    )
  } else {
    # y = b0 + b1*x + b2*grupaB + b3*x:grupaB
    # Grupa A: y = b0 + b1*x
    # Grupa B: y = (b0 + b2) + (b1 + b3)*x
    data.frame(
      grupa = c("A", "B"),
      intercept = c(coefs[1], coefs[1] + coefs[3]),
      slope = c(coefs[2], coefs[2] + coefs[4])
    )
  }
}

format_equation <- function(intercept, slope, group_name) {
  sign <- ifelse(slope >= 0, "+", "-")
  sprintf("Grupa %s: y = %.2f %s %.2f x",
          group_name, intercept, sign, abs(slope))
}

# ============================================================================
# PREDEFINIOWANE SCENARIUSZE
# ============================================================================

scenarios <- list(
  parallel = list(
    name = "Linie równoległe (brak interakcji)",
    description = "Obie grupy mają takie samo nachylenie - interakcja NIE jest potrzebna",
    intercept_a = 2, slope_a = 1.5,
    intercept_b = 6, slope_b = 1.5
  ),
  crossing = list(
    name = "Linie przecinające się (silna interakcja)",
    description = "Grupy mają przeciwne nachylenia - interakcja KONIECZNA",
    intercept_a = 2, slope_a = 2,
    intercept_b = 12, slope_b = -1
  ),
  diverging = list(
    name = "Linie rozbieżne (umiarkowana interakcja)",
    description = "Różne nachylenia, ale ten sam kierunek - interakcja ZALECANA",
    intercept_a = 3, slope_a = 0.5,
    intercept_b = 2, slope_b = 2
  ),
  no_group_effect = list(
    name = "Brak efektu grupy",
    description = "Obie grupy zachowują się identycznie",
    intercept_a = 3, slope_a = 1.2,
    intercept_b = 3, slope_b = 1.2
  )
)

# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(
  titlePanel("Regresja z Interakcją - Porównanie Modeli"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      h4("Scenariusze"),
      selectInput("scenario", "Wybierz scenariusz:",
                  choices = c("Własne ustawienia" = "custom",
                              setNames(names(scenarios),
                                       sapply(scenarios, `[[`, "name")))),

      conditionalPanel(
        condition = "input.scenario != 'custom'",
        wellPanel(
          style = "background-color: #f8f9fa;",
          textOutput("scenario_description")
        )
      ),

      hr(),
      h4("Parametry danych"),

      sliderInput("n_per_group", "Liczba obserwacji (na grupę):",
                  min = 10, max = 100, value = 30, step = 5),

      sliderInput("noise_sd", "Poziom szumu:",
                  min = 0.5, max = 5, value = 2, step = 0.5),

      hr(),
      h4("Grupa A"),
      sliderInput("intercept_a", "Wyraz wolny (a\u2080):",
                  min = -5, max = 15, value = 2, step = 0.5),
      sliderInput("slope_a", "Nachylenie (a\u2081):",
                  min = -3, max = 3, value = 1.5, step = 0.1),

      hr(),
      h4("Grupa B"),
      sliderInput("intercept_b", "Wyraz wolny (b\u2080):",
                  min = -5, max = 15, value = 6, step = 0.5),
      sliderInput("slope_b", "Nachylenie (b\u2081):",
                  min = -3, max = 3, value = 1.5, step = 0.1),

      hr(),
      actionButton("regenerate", "Wygeneruj nowe dane",
                   class = "btn-primary", width = "100%")
    ),

    mainPanel(
      width = 9,

      fluidRow(
        column(6,
               h4("Model addytywny (bez interakcji)",
                  style = "text-align: center; color: #2c3e50;"),
               p("y = \u03B2\u2080 + \u03B2\u2081x + \u03B2\u2082grupa",
                 style = "text-align: center; font-style: italic; color: #7f8c8d;"),
               plotOutput("plot_additive", height = "350px")
        ),
        column(6,
               h4("Model z interakcją",
                  style = "text-align: center; color: #2c3e50;"),
               p("y = \u03B2\u2080 + \u03B2\u2081x + \u03B2\u2082grupa + \u03B2\u2083(x \u00D7 grupa)",
                 style = "text-align: center; font-style: italic; color: #7f8c8d;"),
               plotOutput("plot_interaction", height = "350px")
        )
      ),

      hr(),

      fluidRow(
        column(12,
               wellPanel(
                 style = "background-color: #ecf0f1;",
                 h4("Porównanie modeli", style = "color: #2c3e50;"),
                 fluidRow(
                   column(4,
                          h5("Test istotności interakcji"),
                          uiOutput("interaction_test")
                   ),
                   column(4,
                          h5("Model addytywny"),
                          verbatimTextOutput("additive_summary")
                   ),
                   column(4,
                          h5("Model z interakcją"),
                          verbatimTextOutput("interaction_summary")
                   )
                 )
               )
        )
      ),

      fluidRow(
        column(12,
               wellPanel(
                 h4("Interpretacja", style = "color: #2c3e50;"),
                 uiOutput("interpretation")
               )
        )
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  # Aktualizacja sliderów przy wyborze scenariusza
  observeEvent(input$scenario, {
    if (input$scenario != "custom" && input$scenario %in% names(scenarios)) {
      sc <- scenarios[[input$scenario]]
      updateSliderInput(session, "intercept_a", value = sc$intercept_a)
      updateSliderInput(session, "slope_a", value = sc$slope_a)
      updateSliderInput(session, "intercept_b", value = sc$intercept_b)
      updateSliderInput(session, "slope_b", value = sc$slope_b)
    }
  })

  # Opis scenariusza
  output$scenario_description <- renderText({
    req(input$scenario != "custom")
    if (input$scenario %in% names(scenarios)) {
      scenarios[[input$scenario]]$description
    }
  })

  # Reactive data - regeneruje przy zmianie parametrów lub przycisku
  data <- reactive({
    input$regenerate  # Trigger na przycisk

    generate_regression_data(
      n_per_group = input$n_per_group,
      intercept_a = input$intercept_a,
      slope_a = input$slope_a,
      intercept_b = input$intercept_b,
      slope_b = input$slope_b,
      noise_sd = input$noise_sd
    )
  })

  # Fitted models
  models <- reactive({
    fit_models(data())
  })

  # Kolory dla grup
  group_colors <- c("A" = "#3498db", "B" = "#e74c3c")

  # Wykres - model addytywny
  output$plot_additive <- renderPlot({
    df <- data()
    m <- models()
    lines_df <- get_model_lines(m$additive, "additive")

    ggplot(df, aes(x = x, y = y, color = grupa)) +
      geom_point(alpha = 0.6, size = 3) +
      geom_abline(data = lines_df,
                  aes(intercept = intercept, slope = slope, color = grupa),
                  linewidth = 1.2) +
      scale_color_manual(values = group_colors) +
      labs(x = "X", y = "Y", color = "Grupa") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom") +
      annotate("text", x = 0.5, y = max(df$y) * 0.95,
               label = sprintf("R² = %.3f", summary(m$additive)$r.squared),
               hjust = 0, size = 5, fontface = "bold") +
      coord_cartesian(xlim = c(0, 10))
  })

  # Wykres - model z interakcją
  output$plot_interaction <- renderPlot({
    df <- data()
    m <- models()
    lines_df <- get_model_lines(m$interaction, "interaction")

    ggplot(df, aes(x = x, y = y, color = grupa)) +
      geom_point(alpha = 0.6, size = 3) +
      geom_abline(data = lines_df,
                  aes(intercept = intercept, slope = slope, color = grupa),
                  linewidth = 1.2) +
      scale_color_manual(values = group_colors) +
      labs(x = "X", y = "Y", color = "Grupa") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom") +
      annotate("text", x = 0.5, y = max(df$y) * 0.95,
               label = sprintf("R² = %.3f", summary(m$interaction)$r.squared),
               hjust = 0, size = 5, fontface = "bold") +
      coord_cartesian(xlim = c(0, 10))
  })

  # Test interakcji
  output$interaction_test <- renderUI({
    m <- models()
    p_val <- m$interaction_pvalue

    if (is.na(p_val)) {
      return(p("Nie można obliczyć testu"))
    }

    significance <- if (p_val < 0.001) {
      list(text = "Wysoce istotna (p < 0.001)", color = "#c0392b", icon = "\u2717")
    } else if (p_val < 0.05) {
      list(text = sprintf("Istotna (p = %.3f)", p_val), color = "#e67e22", icon = "\u2717")
    } else {
      list(text = sprintf("Nieistotna (p = %.3f)", p_val), color = "#27ae60", icon = "\u2713")
    }

    div(
      style = sprintf("padding: 15px; border-radius: 5px; background-color: %s; color: white;",
                      significance$color),
      h5(paste(significance$icon, "Interakcja:", significance$text),
         style = "margin: 0; color: white;"),
      p(style = "margin: 5px 0 0 0; font-size: 0.9em;",
        if (p_val < 0.05) "Model z interakcją jest lepszy"
        else "Model addytywny jest wystarczający")
    )
  })

  # Podsumowanie modelu addytywnego
  output$additive_summary <- renderPrint({
    m <- models()
    lines_df <- get_model_lines(m$additive, "additive")

    cat(format_equation(lines_df$intercept[1], lines_df$slope[1], "A"), "\n")
    cat(format_equation(lines_df$intercept[2], lines_df$slope[2], "B"), "\n")
    cat("\nR² =", round(summary(m$additive)$r.squared, 3))
    cat("\nAIC =", round(AIC(m$additive), 1))
  })

  # Podsumowanie modelu z interakcją
  output$interaction_summary <- renderPrint({
    m <- models()
    lines_df <- get_model_lines(m$interaction, "interaction")

    cat(format_equation(lines_df$intercept[1], lines_df$slope[1], "A"), "\n")
    cat(format_equation(lines_df$intercept[2], lines_df$slope[2], "B"), "\n")
    cat("\nR² =", round(summary(m$interaction)$r.squared, 3))
    cat("\nAIC =", round(AIC(m$interaction), 1))
  })

  # Interpretacja
  output$interpretation <- renderUI({
    m <- models()
    p_val <- m$interaction_pvalue

    slope_diff <- abs(input$slope_a - input$slope_b)

    if (is.na(p_val)) {
      return(p("Wygeneruj dane, aby zobaczyć interpretację."))
    }

    # Porównanie R²
    r2_add <- summary(m$additive)$r.squared
    r2_int <- summary(m$interaction)$r.squared
    r2_diff <- r2_int - r2_add

    div(
      if (p_val < 0.05) {
        tagList(
          p(tags$strong("Interakcja jest istotna statystycznie."),
            "Oznacza to, że związek między X a Y różni się między grupami."),
          tags$ul(
            tags$li(sprintf("Grupa A: nachylenie = %.2f", input$slope_a)),
            tags$li(sprintf("Grupa B: nachylenie = %.2f", input$slope_b)),
            tags$li(sprintf("Różnica nachyleń: %.2f", slope_diff))
          ),
          p("Model z interakcją lepiej opisuje dane. ",
            sprintf("Dodanie interakcji zwiększa R² o %.3f.", r2_diff))
        )
      } else {
        tagList(
          p(tags$strong("Interakcja NIE jest istotna statystycznie."),
            "Związek między X a Y jest podobny w obu grupach."),
          p("Model addytywny (równoległe linie) jest wystarczający. ",
            "Dodanie interakcji nie poprawia istotnie dopasowania modelu."),
          p(tags$em("Zasada parsymonii: wybierz prostszy model, gdy złożony nie jest lepszy."))
        )
      }
    )
  })
}

# Uruchomienie aplikacji
shinyApp(ui = ui, server = server)
