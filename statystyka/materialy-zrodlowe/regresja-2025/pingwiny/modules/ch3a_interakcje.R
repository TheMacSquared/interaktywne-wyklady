# ============================================================================
# CHAPTER 3A: Interakcje w regresji
# ============================================================================

ch3a_ui <- list(
  id    = "ch-interakcje",
  num   = "03a",
  title = "Interakcje",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 03a · Regresja",
      num    = "03a",
      title  = "Interakcje.",
      lead   = "Czasem wpływ jednego predyktora zależy od poziomu drugiego.
                Wtedy model addytywny jest zbyt sztywny."
    ),

    tagList(
      p("W modelu addytywnym zakładamy, że każdy predyktor wnosi własny,
        stały składnik. Dłuższa płetwa może podnosić masę ciała, a gatunek
        może przesuwać całą masę w górę lub w dół — ale oba efekty działają
        niezależnie i każda gatunkowa linia ma takie samo nachylenie."),
      p("Interakcja pozwala zapytać o coś subtelniejszego: czy zależność
        między długością płetwy a masą ciała jest taka sama u wszystkich
        gatunków? Może u jednego gatunku każdy dodatkowy milimetr płetwy
        dokłada więcej gramów niż u innego.")
    ),

    lc_h2("ch3a-model", "Model z interakcją"),

    tagList(
      p("Porównamy dwa modele na danych palmerpenguins (zmienna ", tags$code("species"),
        " ma trzy poziomy: Adelie, Chinstrap, Gentoo):"),
      lc_formula_box(
        withMathJax(helpText(
          "$$\\text{masa} = \\beta_0 + \\beta_1\\,\\text{płetwa} + \\beta_2\\,\\text{gatunek} + \\varepsilon$$"
        )),
        withMathJax(helpText(
          "$$\\text{masa} = \\beta_0 + \\beta_1\\,\\text{płetwa} + \\beta_2\\,\\text{gatunek} + \\beta_3\\,\\text{płetwa}\\times\\text{gatunek} + \\varepsilon$$"
        ))
      ),
      p("Współczynniki przy interakcji mówią, czy nachylenie dla długości
        płetwy zmienia się między gatunkami. Na wykresie poniżej widać to
        jako linie o różnych nachyleniach.")
    ),

    figure_panel(
      label = "Ryc. 3a.1", title = "Długość płetwy, gatunek i masa ciała",
      full_width = TRUE,
      fluidRow(
        column(4,
          radioButtons("ch3a_model_type", "Model:",
            choices = c(
              "Addytywny: płetwa + gatunek" = "add",
              "Z interakcją: płetwa × gatunek" = "int"
            ),
            selected = "int"
          ),
          helpText("Linie pokazują predykcje modelu osobno dla każdego z trzech
                    gatunków.")
        ),
        column(8,
          zoom_plot_ui("ch3a_interaction_plot", height = "350px"),
          uiOutput("ch3a_model_stats"),
          uiOutput("ch3a_model_table")
        )
      )
    ),

    inline_callout(label = "Jak czytać", color = "wskazowka",
      "Jeśli linie są równoległe, model mówi: efekt długości płetwy jest
       podobny u każdego gatunku. Jeśli linie mają różne nachylenia, pojawia
       się interakcja: wpływ płetwy na masę ciała zależy od gatunku."
    ),

    figure_panel(
      label = "Ryc. 3a.2", title = "Addytywny kontra interakcyjny",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Ten wykres pokazuje, co dokładnie zmienia dodanie składnika
                    płetwa × gatunek: model addytywny wymusza równoległe linie,
                    model interakcyjny pozwala im zmienić nachylenie.")
        ),
        column(8,
          zoom_plot_ui("ch3a_compare_plot", height = "340px")
        )
      )
    ),

    lc_chapter_next(
      num       = "04",
      title     = "Jak porównywać modele?",
      lead      = "R²adj, AIC, BIC, train/test — gdy mamy kilku kandydatów",
      target_id = "ch-porownanie"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch3a_server <- function(input, output, session) {

  ch3a_labels <- c(
    flipper_length_mm = "Długość płetwy",
    speciesChinstrap = "Gatunek: Chinstrap",
    speciesGentoo = "Gatunek: Gentoo",
    `flipper_length_mm:speciesChinstrap` = "Płetwa × Chinstrap",
    `flipper_length_mm:speciesGentoo` = "Płetwa × Gentoo"
  )

  ch3a_species_cols <- c(
    "Adelie"    = unname(upwr_cat["szalwia"]),
    "Chinstrap" = unname(upwr_cat["bursztyn"]),
    "Gentoo"    = unname(upwr_cat["terakota"])
  )

  ch3a_data <- reactive({
    df <- .cas_data
    df$species <- factor(df$species, levels = c("Adelie", "Chinstrap", "Gentoo"))
    df
  })

  ch3a_model <- reactive({
    df <- ch3a_data()
    if (identical(input$ch3a_model_type, "add")) {
      lm(body_mass_g ~ flipper_length_mm + species, data = df)
    } else {
      lm(body_mass_g ~ flipper_length_mm * species, data = df)
    }
  })

  ch3a_prediction_grid <- function(df) {
    species_levels <- levels(df$species)
    grid <- expand.grid(
      flipper_length_mm = seq(min(df$flipper_length_mm, na.rm = TRUE),
                              max(df$flipper_length_mm, na.rm = TRUE),
                              length.out = 140),
      species = species_levels,
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    grid$species <- factor(grid$species, levels = species_levels)
    grid
  }

  ch3a_plot_model <- function(model, df, title = NULL) {
    grid <- ch3a_prediction_grid(df)
    grid$pred <- predict(model, newdata = grid)

    ggplot(df, aes(x = flipper_length_mm, y = body_mass_g)) +
      geom_point(aes(color = species), alpha = 0.55, size = 1.8) +
      geom_line(data = grid,
                aes(x = flipper_length_mm, y = pred, color = species),
                linewidth = 1.15) +
      scale_color_manual(values = ch3a_species_cols, name = "Gatunek") +
      labs(
        title = title,
        x = "Długość płetwy (mm)",
        y = "Masa ciała (g)"
      ) +
      theme_upwr() +
      theme(legend.position = "top")
  }

  zoom_plot_server("ch3a_interaction_plot", reactive({
    ch3a_plot_model(ch3a_model(), ch3a_data())
  }))

  output$ch3a_model_stats <- renderUI({
    model <- ch3a_model()
    metrics <- compute_model_metrics(model)
    lc_stat_grid(columns = 4,
      lc_stat_box("R²", round(metrics$r_squared, 3), color = unname(upwr_cat["niebo"])),
      lc_stat_box("adj.R²", round(metrics$adj_r_squared, 3), color = unname(upwr_cat["szalwia"])),
      lc_stat_box("AIC", round(metrics$aic, 1), color = unname(upwr_cat["bursztyn"])),
      lc_stat_box("RMSE", round(metrics$rmse, 1), color = unname(upwr_cat["terakota"]))
    )
  })

  output$ch3a_model_table <- renderUI({
    coefs <- broom::tidy(ch3a_model())
    labels_pl <- c("(Intercept)" = "Wyraz wolny", ch3a_labels)
    coefs$term_pl <- ifelse(coefs$term %in% names(labels_pl), labels_pl[coefs$term], coefs$term)

    rows <- lapply(seq_len(nrow(coefs)), function(i) {
      sig <- if (coefs$p.value[i] < 0.05) " *" else ""
      tags$tr(
        tags$td(coefs$term_pl[i]),
        tags$td(round(coefs$estimate[i], 3)),
        tags$td(round(coefs$std.error[i], 3)),
        tags$td(round(coefs$statistic[i], 3)),
        tags$td(paste0(format_p_value(coefs$p.value[i]), sig))
      )
    })

    tags$table(class = "lc-table lc-table-bordered lc-table-striped",
      style = "font-size: 14px;",
      tags$thead(
        tags$tr(tags$th("Zmienna"), tags$th("Estymata"), tags$th("SE"),
                tags$th("t"), tags$th("p"))
      ),
      tags$tbody(rows)
    )
  })

  zoom_plot_server("ch3a_compare_plot", reactive({
    df <- ch3a_data()
    add_model <- lm(body_mass_g ~ flipper_length_mm + species, data = df)
    int_model <- lm(body_mass_g ~ flipper_length_mm * species, data = df)

    add_grid <- ch3a_prediction_grid(df)
    add_grid$pred <- predict(add_model, newdata = add_grid)
    add_grid$model <- "Addytywny"

    int_grid <- ch3a_prediction_grid(df)
    int_grid$pred <- predict(int_model, newdata = int_grid)
    int_grid$model <- "Z interakcją"

    line_df <- rbind(add_grid, int_grid)

    ggplot(df, aes(x = flipper_length_mm, y = body_mass_g)) +
      geom_point(color = upwr_secondary, alpha = 0.35, size = 1.5) +
      geom_line(data = line_df,
                aes(x = flipper_length_mm, y = pred, color = species),
                linewidth = 1.05) +
      facet_wrap(~ model) +
      scale_color_manual(values = ch3a_species_cols, name = "Gatunek") +
      labs(x = "Długość płetwy (mm)", y = "Masa ciała (g)") +
      theme_upwr() +
      theme(legend.position = "top")
  }))
}
