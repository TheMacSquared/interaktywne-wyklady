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
        stały składnik. Dochód okręgu może podnosić wynik z matematyki,
        a odsetek uczniów z dofinansowaniem obiadów może go obniżać — ale
        oba efekty działają niezależnie."),
      p("Interakcja pozwala zapytać o coś subtelniejszego: czy relacja między
        dochodem okręgu a wynikiem z matematyki jest taka sama w okręgach
        z niskim i wysokim odsetkiem dofinansowanych obiadów?")
    ),

    lc_h2("ch3a-model", "Model z interakcją"),

    tagList(
      p("Porównamy dwa modele na danych CASchools:"),
      lc_formula_box(
        withMathJax(helpText(
          "$$\\text{math} = \\beta_0 + \\beta_1\\,\\text{income} + \\beta_2\\,\\text{lunch} + \\varepsilon$$"
        )),
        withMathJax(helpText(
          "$$\\text{math} = \\beta_0 + \\beta_1\\,\\text{income} + \\beta_2\\,\\text{lunch} + \\beta_3\\,\\text{income}\\times\\text{lunch} + \\varepsilon$$"
        ))
      ),
      p("Współczynnik przy interakcji mówi, czy nachylenie dla dochodu zmienia
        się wraz z poziomem dofinansowania obiadów. W wykresie poniżej widać
        to jako linie o różnych nachyleniach.")
    ),

    figure_panel(
      label = "Ryc. 3a.1", title = "Dochód, dofinansowanie obiadów i wynik z matematyki",
      full_width = TRUE,
      fluidRow(
        column(4,
          radioButtons("ch3a_model_type", "Model:",
            choices = c(
              "Addytywny: income + lunch" = "add",
              "Z interakcją: income × lunch" = "int"
            ),
            selected = "int"
          ),
          helpText("Linie pokazują predykcje modelu dla niskiego, średniego
                    i wysokiego poziomu dofinansowania obiadów.")
        ),
        column(8,
          zoom_plot_ui("ch3a_interaction_plot", height = "350px"),
          uiOutput("ch3a_model_stats"),
          uiOutput("ch3a_model_table")
        )
      )
    ),

    inline_callout(label = "Jak czytać", color = "wskazowka",
      "Jeśli linie są równoległe, model mówi: efekt dochodu jest podobny
       przy każdym poziomie dofinansowania obiadów. Jeśli linie mają różne
       nachylenia, pojawia się interakcja: wpływ dochodu zależy od kontekstu
       społecznego okręgu."
    ),

    figure_panel(
      label = "Ryc. 3a.2", title = "Addytywny kontra interakcyjny",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Ten wykres pokazuje, co dokładnie zmienia dodanie składnika
                    income × lunch: model addytywny wymusza równoległe linie,
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
    income_c = "Dochód okręgu",
    lunch_c = "Dofinansowanie obiadów",
    `income_c:lunch_c` = "Dochód × dofinansowanie"
  )

  ch3a_data <- reactive({
    df <- .cas_data
    df$income_c <- df$income - mean(df$income, na.rm = TRUE)
    df$lunch_c <- df$lunch - mean(df$lunch, na.rm = TRUE)
    df$lunch_group <- cut(
      df$lunch,
      breaks = as.numeric(quantile(df$lunch, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)),
      include.lowest = TRUE,
      labels = c("niskie", "średnie", "wysokie")
    )
    df
  })

  ch3a_model <- reactive({
    df <- ch3a_data()
    if (identical(input$ch3a_model_type, "add")) {
      lm(math ~ income_c + lunch_c, data = df)
    } else {
      lm(math ~ income_c * lunch_c, data = df)
    }
  })

  ch3a_prediction_grid <- function(df) {
    lunch_reps <- tapply(df$lunch, df$lunch_group, median, na.rm = TRUE)
    grid <- expand.grid(
      income = seq(min(df$income, na.rm = TRUE), max(df$income, na.rm = TRUE), length.out = 140),
      lunch_group = names(lunch_reps),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    grid$lunch <- as.numeric(lunch_reps[grid$lunch_group])
    grid$income_c <- grid$income - mean(df$income, na.rm = TRUE)
    grid$lunch_c <- grid$lunch - mean(df$lunch, na.rm = TRUE)
    grid
  }

  ch3a_plot_model <- function(model, df, title = NULL) {
    grid <- ch3a_prediction_grid(df)
    grid$pred <- predict(model, newdata = grid)

    ggplot(df, aes(x = income, y = math)) +
      geom_point(aes(color = lunch_group), alpha = 0.55, size = 1.8) +
      geom_line(data = grid,
                aes(x = income, y = pred, color = lunch_group),
                linewidth = 1.15) +
      scale_color_manual(
        values = c(
          "niskie" = unname(upwr_cat["szalwia"]),
          "średnie" = unname(upwr_cat["bursztyn"]),
          "wysokie" = unname(upwr_cat["terakota"])
        ),
        name = "Dofinansowanie obiadów"
      ) +
      labs(
        title = title,
        x = "Dochód okręgu (tys. USD)",
        y = "Wynik: matematyka"
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
      lc_stat_box("RMSE", round(metrics$rmse, 3), color = unname(upwr_cat["terakota"]))
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
        tags$td(round(coefs$estimate[i], 4)),
        tags$td(round(coefs$std.error[i], 4)),
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
    add_model <- lm(math ~ income_c + lunch_c, data = df)
    int_model <- lm(math ~ income_c * lunch_c, data = df)

    add_grid <- ch3a_prediction_grid(df)
    add_grid$pred <- predict(add_model, newdata = add_grid)
    add_grid$model <- "Addytywny"

    int_grid <- ch3a_prediction_grid(df)
    int_grid$pred <- predict(int_model, newdata = int_grid)
    int_grid$model <- "Z interakcją"

    line_df <- rbind(add_grid, int_grid)

    ggplot(df, aes(x = income, y = math)) +
      geom_point(color = upwr_secondary, alpha = 0.35, size = 1.5) +
      geom_line(data = line_df,
                aes(x = income, y = pred, color = lunch_group),
                linewidth = 1.05) +
      facet_wrap(~ model) +
      scale_color_manual(
        values = c(
          "niskie" = unname(upwr_cat["szalwia"]),
          "średnie" = unname(upwr_cat["bursztyn"]),
          "wysokie" = unname(upwr_cat["terakota"])
        ),
        name = "Dofinansowanie obiadów"
      ) +
      labs(x = "Dochód okręgu (tys. USD)", y = "Wynik: matematyka") +
      theme_upwr() +
      theme(legend.position = "top")
  }))
}
