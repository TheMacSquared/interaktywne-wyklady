# ============================================================================
# CHAPTER 3: Regresja wieloraka
# ============================================================================

ch3_ui <- list(
  id    = "ch-wieloraka",
  num   = "03",
  title = "Regresja wieloraka",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 03 · Regresja",
      num   = "03",
      title  = "Regresja wieloraka.",
      lead   = "Regresja prosta używała jednego predyktora.
                W rzeczywistości na Y wpływa wiele czynników jednocześnie."
    ),

    tagList(
      p("W rozdziale 1 mieliśmy jedno X. W rozdziale 2 nauczyliśmy się
        oceniać, ", tags$em("czy"), " dany model jest dobry — reszty, R², RMSE.
        Realne dane mają jednak wiele predyktorów naraz i czasem dopiero
        zobaczenie ich razem zmienia obraz."),
      p("Klasyczny przykład: u pingwinów dłuższy dziób wydaje się wiązać
        z ", tags$em("niższym"), " (mniejsza wysokość dzioba). Brzmi
        zaskakująco — ale ten obraz miesza trzy gatunki o różnej budowie.
        Co ", tags$em("naprawdę"), " łączy wymiary dzioba? Tego nie powie nam
        żadne z pojedynczych równań. Musimy zbudować model z wieloma X-ami naraz.")
    ),

    lc_h2("ch3-wiele-predyktorow", "Wiele predyktorów naraz"),

    tagList(
      p("Regresja wieloraka rozszerza model o k predyktorów:"),
      lc_formula_box(
        withMathJax(helpText(
          "$$Y = \\beta_0 + \\beta_1 X_1 + \\beta_2 X_2 + \\ldots + \\beta_k X_k + \\varepsilon$$"
        ))
      ),
      p("Każde ", withMathJax("\\(\\beta_j\\)"),
        " mówi: o ile zmieni się Y, gdy ", withMathJax("\\(X_j\\)"),
        " wzrośnie o 1, przy stałych pozostałych zmiennych."),
      p("To zastrzeżenie „przy stałych pozostałych\" jest sercem regresji
        wielorakiej. Bez niego ", withMathJax("\\(\\beta_j\\)"),
        " wyglądałoby tak samo jak w regresji prostej. Z nim — może być
        zupełnie inne, a czasem wręcz przeciwnego znaku.")
    ),

    lc_h2("ch3-budowanie", "Budowanie modelu wielorakiego"),

    tagList(
      p("Zobaczmy to na realnych danych palmerpenguins: masa ciała pingwina
        w zależności od jego wymiarów. Wybierz, które predyktory dodać — i zwróć
        uwagę nie tylko na same liczby w tabeli, ale na to, jak zmienia się ",
        tags$em("znak"), " i ", tags$em("istotność"),
        " współczynnika, gdy dokładamy kolejny X."),
      p("To są dane obserwacyjne, więc nie oczekujemy czystej sytuacji
        laboratoryjnej. Część zmiennych będzie wyraźna, część nieistotna,
        a część może działać inaczej w różnych podgrupach. Interakcje
        na razie świadomie ignorujemy — najpierw uczymy się modelu
        addytywnego: każdy predyktor wnosi własny składnik.")
    ),

    figure_panel(
      label = "Ryc. 3.1", title = "Pingwiny: model z wieloma predyktorami",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Dane: 333 pingwiny z archipelagu Palmer. Przełączaj
                    predyktory — model przelicza się na tych samych realnych
                    danych."),
          selectInput("ch3_outcome", "Zmienna zależna Y:",
            choices = c(
              "Masa ciała (g)" = "body_mass_g",
              "Długość dzioba (mm)" = "bill_length_mm"
            ),
            selected = "body_mass_g"
          ),
          tags$div(class = "form-group shiny-input-container",
            tags$label(class = "control-label", "Predyktor bazowy:"),
            tags$div(tags$strong("Długość płetwy (mm)"))
          ),
          checkboxGroupInput("ch3_predictors", "Dodaj predyktory:",
            choices = c(
              "Długość dzioba (mm)" = "bill_length_mm",
              "Wysokość dzioba (mm)" = "bill_depth_mm",
              "Gatunek (kategoria)" = "species",
              "Płeć (kategoria)" = "sex",
              "Wyspa (kategoria)" = "island"
            ),
            selected = "species"
          ),
          helpText(style = "margin-top: 8px; font-size: 12px;",
            "Model zawsze zaczyna od długości płetwy. Dokładaj predyktory —
             także jakościowe (gatunek, płeć, wyspa) — i obserwuj, jak zmienia
             się współczynnik długości płetwy, gdy część jej „zasługi” przejmuje
             gatunek.")
        ),
        column(8,
          lc_feedback(type = "info",
            p("Tabela pokazuje współczynniki pełnego modelu addytywnego,
              bez interakcji. Gwiazdka przy p-value oznacza p < 0.05.")
          ),
          uiOutput("ch3_model_coefs"),
          uiOutput("ch3_prediction_plot_ui"),
          uiOutput("ch3_model_stats")
        )
      )
    ),

    lc_h2("ch3-kontrola", "Co znaczy „przy stałych pozostałych zmiennych”?"),

    tagList(
      p("Słowo „kontrola\" w regresji znaczy: ", tags$em("usuwamy"),
        " z X-a informację, którą już niesie inny X. To, co zostaje, jest
        efektem unikalnym danej zmiennej — tym, czego nie da się wytłumaczyć pozostałymi."),
      p("Widget pokazuje to krok po kroku na pingwinach: najpierw policzymy
        prosty związek wysokości dzioba z jego długością (wyjdzie ", tags$em("ujemny"),
        "!), potem rozbijemy te same punkty na gatunki, a na końcu zobaczymy,
        co zostaje ze współczynnika długości dzioba, gdy kontrolujemy gatunek.")
    ),

    figure_panel(
      label = "Ryc. 3.2", title = "Paradoks Simpsona: ukryty gatunek",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Porównujemy model prosty i model z kontrolą gatunku na tych
                    samych 333 pingwinach."),
          h5("Kroki:"),
          actionButton("ch3_control_step1", "1. Wysokość ~ długość dzioba",
                       class = "lc-btn-outline", width = "100%"),
          actionButton("ch3_control_step2", "2. Rozbij na gatunki",
                       class = "lc-btn-outline", width = "100%"),
          actionButton("ch3_control_step3", "3. Model z kontrolą gatunku",
                       class = "lc-btn-outline", width = "100%")
        ),
        column(8,
          lc_plot_fullscreen("ch3_control_plot", height = "320px"),
          uiOutput("ch3_control_info")
        )
      )
    ),

    inline_callout(label = "Uwaga", color = "uwaga",
      "Współczynnik tej samej zmiennej w modelu prostym i wielorakim może
       być zupełnie różny — czasem nawet przeciwnego znaku. To zjawisko
       nazywa się paradoksem Simpsona i jest jedną z głównych motywacji
       do używania regresji wielorakiej."
    ),

    lc_h2("ch3-wspolliniowosc", "Multikolinearność"),

    tagList(
      p("A co, jeśli dwa nasze predyktory mówią ", tags$em("prawie to samo"),
        "? Powiedzmy: długość płetwy i masa ciała pingwina są silnie ze sobą
        skorelowane. Model w zasadzie nie wie, któremu przypisać efekt —
        i rozdmuchuje błędy standardowe obu. Współczynniki stają się niestabilne, p-value rosną."),
      p("Wskaźnikiem, który to wychwytuje, jest VIF
        — variance inflation factor. Im wyższy, tym bardziej zmienna
        powtarza informację z innych X-ów. VIF > 5 jest sygnałem
        ostrzegawczym, VIF > 10 — czerwoną flagą.")
    ),

    figure_panel(
      label = "Ryc. 3.3", title = "Gdy predyktory mówią prawie to samo",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch3_collin_rho", "Korelacja X₁–X₂:",
                      min = 0, max = 0.98, value = 0.8, step = 0.02),
          actionButton("ch3_collin_new", "Generuj i dopasuj",
                       class = "lc-btn-warning", width = "100%")
        ),
        column(8,
          lc_plot_fullscreen("ch3_collin_plot", height = "300px"),
          uiOutput("ch3_collin_info")
        )
      )
    ),

    inline_callout(label = "Przed interpretacją", color = "wskazowka",
      "W regresji wielorakiej VIF jest częścią diagnostyki modelu, nie
       ozdobną metryką. Jeśli VIF jest wysoki, współczynniki mogą mieć
       sensowny znak w jednym losowaniu i dziwny w następnym. Wtedy
       interpretuj ostrożnie, usuń jeden z redundantnych predyktorów albo
       połącz je w jedną miarę."
    ),

    lc_h2("ch3-co-dalej", "Co dalej"),

    tagList(
      p("Mamy teraz w arsenale modele z różną liczbą predyktorów: od
        jednego X aż po wszystkie naraz. Naturalne pytanie: który z nich wybrać?"),
      p("Można by chcieć po prostu wziąć ten o najwyższym R². Ale —
        jak zaraz zobaczymy w rozdziale 4 — R² zachowuje się w
        porównaniach zdradliwie: zawsze rośnie, gdy dodajemy kolejny
        predyktor, nawet bezsensowny. Trzeba poznać metryki, które karzą
        za złożoność."),
      p("Most do rozdziału 4: R²adj, AIC, BIC, podział train/test —
        i pierwszy widget pokazujący, dlaczego sam R² nie wystarcza.")
    ),

    lc_chapter_next(
      num       = "03a",
      title     = "Interakcje",
      lead      = "gdy wpływ jednego predyktora zależy od drugiego",
      target_id = "ch-interakcje"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch3_server <- function(input, output, session) {

  ch3_labels_pl <- c(
    "body_mass_g" = "Masa ciała (g)",
    "bill_length_mm" = "Długość dzioba (mm)",
    "bill_depth_mm" = "Wysokość dzioba (mm)",
    "flipper_length_mm" = "Długość płetwy (mm)",
    "species" = "Gatunek",
    "sex" = "Płeć",
    "island" = "Wyspa"
  )

  ch3_base_x <- "flipper_length_mm"

  ch3_selected_predictors <- reactive({
    outcome <- input$ch3_outcome
    if (is.null(outcome)) outcome <- "body_mass_g"
    # Predyktor bazowy zawsze obecny; wykluczamy zmienną zależną,
    # gdyby trafiła na listę checkboxów.
    setdiff(unique(c(ch3_base_x, input$ch3_predictors)), outcome)
  })

  # Model jako reactive: zależy od danych i wyboru predyktorów.
  # Dzięki temu przełączanie checkboxów porównuje modele na TYCH SAMYCH danych.
  ch3_model <- reactive({
    df <- .cas_data
    outcome <- input$ch3_outcome
    if (is.null(outcome)) outcome <- "body_mass_g"
    preds <- ch3_selected_predictors()
    formula <- as.formula(paste(outcome, "~", paste(preds, collapse = " + ")))
    lm(formula, data = df)
  })

  output$ch3_model_coefs <- renderUI({
    model <- ch3_model()
    if (is.null(model)) return(NULL)

    coefs <- broom::tidy(model)

    labels_pl <- c("(Intercept)" = "Wyraz wolny", ch3_labels_pl)

    # Terminy faktorowe (np. "speciesChinstrap") formatujemy jako "Gatunek: Chinstrap".
    cat_vars <- names(ch3_labels_pl)[vapply(names(ch3_labels_pl),
      function(v) v %in% names(.cas_data) && !is.numeric(.cas_data[[v]]), logical(1))]
    pretty_term <- function(t) {
      if (t %in% names(labels_pl)) return(unname(labels_pl[t]))
      for (v in cat_vars) {
        if (startsWith(t, v)) return(paste0(ch3_labels_pl[[v]], ": ", substring(t, nchar(v) + 1)))
      }
      t
    }
    coefs$term_pl <- vapply(coefs$term, pretty_term, character(1))

    rows <- lapply(1:nrow(coefs), function(i) {
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

  ch3_make_bins <- function(x, labels) {
    probs <- seq(0, 1, length.out = length(labels) + 1)
    breaks <- unique(as.numeric(quantile(x, probs = probs, na.rm = TRUE)))
    if (length(breaks) <= 2) {
      cut(x, breaks = 2, include.lowest = TRUE, labels = labels[seq_len(2)])
    } else {
      cut(x, breaks = breaks, include.lowest = TRUE, labels = labels[seq_len(length(breaks) - 1)])
    }
  }

  # Zwraca grupy (do koloru/fasety) i ich reprezentantów (do siatki predykcji).
  # Predyktor ciągły -> tercyle + mediany; jakościowy -> poziomy faktora (poziom = sam siebie).
  ch3_group_reps <- function(df, var, labels) {
    if (is.numeric(df[[var]])) {
      group <- ch3_make_bins(df[[var]], labels)
      reps <- tapply(df[[var]], group, median, na.rm = TRUE)
    } else {
      group <- factor(df[[var]])
      reps <- setNames(levels(group), levels(group))
    }
    list(group = group, reps = reps)
  }

  ch3_prediction_grid <- function(df, predictors, x_var) {
    extra <- setdiff(predictors, x_var)
    x_grid <- seq(min(df[[x_var]], na.rm = TRUE), max(df[[x_var]], na.rm = TRUE), length.out = 120)
    grid <- data.frame(x = x_grid)
    names(grid) <- x_var

    if (length(extra) >= 1) {
      color_info <- ch3_group_reps(df, extra[1], c("niskie", "średnie", "wysokie"))
      grid <- merge(
        grid,
        data.frame(color_group = names(color_info$reps), stringsAsFactors = FALSE),
        all = TRUE
      )
      grid[[extra[1]]] <- unname(color_info$reps[grid$color_group])
    }

    facet_vars <- extra[-1]
    if (length(facet_vars) >= 1) {
      levels_1 <- if (length(facet_vars) == 1) c("niskie", "średnie", "wysokie") else c("niższe", "wyższe")
      facet_info_1 <- ch3_group_reps(df, facet_vars[1], levels_1)
      grid <- merge(
        grid,
        data.frame(facet_1 = names(facet_info_1$reps), stringsAsFactors = FALSE),
        all = TRUE
      )
      grid[[facet_vars[1]]] <- unname(facet_info_1$reps[grid$facet_1])
    }

    if (length(facet_vars) >= 2) {
      facet_info_2 <- ch3_group_reps(df, facet_vars[2], c("niższe", "wyższe"))
      grid <- merge(
        grid,
        data.frame(facet_2 = names(facet_info_2$reps), stringsAsFactors = FALSE),
        all = TRUE
      )
      grid[[facet_vars[2]]] <- unname(facet_info_2$reps[grid$facet_2])
    }

    other_vars <- setdiff(predictors, names(grid))
    for (var in other_vars) {
      if (is.numeric(df[[var]])) {
        grid[[var]] <- mean(df[[var]], na.rm = TRUE)
      } else {
        grid[[var]] <- names(sort(table(df[[var]]), decreasing = TRUE))[1]
      }
    }

    grid
  }

  output$ch3_prediction_plot_ui <- renderUI({
    if (length(ch3_selected_predictors()) > 4) return(NULL)
    tagList(
      lc_plot_fullscreen("ch3_coef_plot", height = "320px"),
      if (length(ch3_selected_predictors()) > 1) {
        lc_plot_fullscreen("ch3_compare_plot", height = "230px")
      }
    )
  })

  output$ch3_coef_plot <- renderPlot({
    model <- ch3_model()
    if (is.null(model)) return(NULL)

    df <- .cas_data
    outcome <- input$ch3_outcome
    if (is.null(outcome)) outcome <- "body_mass_g"
    predictors <- ch3_selected_predictors()
    if (length(predictors) > 4) return(NULL)
    x_var <- ch3_base_x

    plot_df <- df
    extra <- setdiff(predictors, x_var)
    color_var <- extra[1]
    if (!is.na(color_var)) {
      plot_df$color_group <- ch3_group_reps(plot_df, color_var, c("niskie", "średnie", "wysokie"))$group
    }

    grid <- ch3_prediction_grid(df, predictors, x_var)
    grid$pred <- predict(model, newdata = grid)

    if (length(predictors) >= 3) {
      facet_vars <- extra[-1]
      levels_1 <- if (length(facet_vars) == 1) c("niskie", "średnie", "wysokie") else c("niższe", "wyższe")
      plot_df$facet_1 <- ch3_group_reps(plot_df, facet_vars[1], levels_1)$group

      if (length(facet_vars) >= 2) {
        plot_df$facet_2 <- ch3_group_reps(plot_df, facet_vars[2], c("niższe", "wyższe"))$group
      }
    }

    p <- ggplot(plot_df, aes(x = .data[[x_var]], y = .data[[outcome]])) +
      labs(
        x = ch3_labels_pl[[x_var]],
        y = ch3_labels_pl[[outcome]]
      ) +
      theme_upwr() +
      theme(legend.position = "top")

    if (length(predictors) == 1) {
      p <- p +
        geom_point(color = upwr_secondary, alpha = 0.45, size = 1.8) +
        geom_line(data = grid, aes(x = .data[[x_var]], y = pred),
                  color = unname(upwr_cat["niebo"]), linewidth = 1.05) +
        guides(color = "none", linetype = "none")
    } else {
      grp_levels <- levels(factor(plot_df$color_group))
      col_values <- if (is.numeric(df[[color_var]])) {
        c("niskie" = unname(upwr_cat["szalwia"]),
          "średnie" = unname(upwr_cat["bursztyn"]),
          "wysokie" = unname(upwr_cat["terakota"]))
      } else {
        setNames(upwr_cat_n(length(grp_levels)), grp_levels)
      }
      p <- p +
        geom_point(aes(color = color_group), alpha = 0.68, size = 1.9) +
        geom_line(data = grid, aes(x = .data[[x_var]], y = pred, color = color_group),
                  linewidth = 1.05) +
        scale_color_manual(values = col_values, name = ch3_labels_pl[[color_var]])
    }

    if (length(predictors) == 1) {
      p <- p
    } else if (length(predictors) == 3) {
      facet_label <- ch3_labels_pl[[extra[-1][1]]]
      p <- p + facet_grid(
        cols = vars(facet_1),
        labeller = labeller(facet_1 = function(x) paste(facet_label, x))
      )
    } else if (length(predictors) == 4) {
      facet_vars <- extra[-1]
      facet_label_1 <- ch3_labels_pl[[facet_vars[1]]]
      facet_label_2 <- ch3_labels_pl[[facet_vars[2]]]
      p <- p + facet_grid(
        rows = vars(facet_1),
        cols = vars(facet_2),
        labeller = labeller(
          facet_1 = function(x) paste(facet_label_1, x),
          facet_2 = function(x) paste(facet_label_2, x)
        )
      )
    }

    p
  })

  output$ch3_compare_plot <- renderPlot({
    model <- ch3_model()
    if (is.null(model)) return(NULL)

    df <- .cas_data
    outcome <- input$ch3_outcome
    if (is.null(outcome)) outcome <- "body_mass_g"
    predictors <- ch3_selected_predictors()
    if (length(predictors) <= 1 || length(predictors) > 4) return(NULL)
    x_var <- ch3_base_x

    x_grid <- seq(min(df[[x_var]], na.rm = TRUE), max(df[[x_var]], na.rm = TRUE), length.out = 120)
    simple_model <- lm(as.formula(paste(outcome, "~", x_var)), data = df)

    simple_grid <- data.frame(x = x_grid)
    names(simple_grid) <- x_var
    simple_grid$pred <- predict(simple_model, newdata = simple_grid)
    simple_grid$model <- "Regresja prosta"

    model_grid <- data.frame(x = x_grid)
    names(model_grid) <- x_var
    for (var in setdiff(predictors, x_var)) {
      if (is.numeric(df[[var]])) {
        model_grid[[var]] <- mean(df[[var]], na.rm = TRUE)
      } else {
        model_grid[[var]] <- names(sort(table(df[[var]]), decreasing = TRUE))[1]
      }
    }
    model_grid$pred <- predict(model, newdata = model_grid)
    model_grid$model <- "Aktualny model"

    line_cols <- c(x_var, "pred", "model")
    line_df <- rbind(simple_grid[line_cols], model_grid[line_cols])

    ggplot(df, aes(x = .data[[x_var]], y = .data[[outcome]])) +
      geom_point(color = upwr_secondary, alpha = 0.28, size = 1.5) +
      geom_line(data = line_df, aes(x = .data[[x_var]], y = pred, color = model, linetype = model),
                linewidth = 1.05) +
      scale_color_manual(
        values = c("Regresja prosta" = unname(upwr_cat["terakota"]),
                   "Aktualny model" = unname(upwr_cat["niebo"])),
        name = NULL
      ) +
      scale_linetype_manual(
        values = c("Regresja prosta" = "dashed", "Aktualny model" = "solid"),
        name = NULL
      ) +
      labs(x = ch3_labels_pl[[x_var]], y = ch3_labels_pl[[outcome]]) +
      theme_upwr() +
      theme(legend.position = "top")
  })

  output$ch3_model_stats <- renderUI({
    model <- ch3_model()
    if (is.null(model)) return(NULL)
    metrics <- compute_model_metrics(model)
    lc_stat_grid(columns = 4,
      lc_stat_box("R²", round(metrics$r_squared, 3), color = unname(upwr_cat["niebo"])),
      lc_stat_box("adj.R²", round(metrics$adj_r_squared, 3), color = unname(upwr_cat["szalwia"])),
      lc_stat_box("AIC", round(metrics$aic, 1), color = unname(upwr_cat["bursztyn"])),
      lc_stat_box("RMSE", round(metrics$rmse, 3), color = unname(upwr_cat["terakota"]))
    )
  })

  # --- Widget: paradoks Simpsona na pingwinach ---
  ch3_control_step <- reactiveVal(1)

  observeEvent(input$ch3_control_step1, ch3_control_step(1))
  observeEvent(input$ch3_control_step2, ch3_control_step(2))
  observeEvent(input$ch3_control_step3, ch3_control_step(3))

  ch3_species_cols <- c(
    "Adelie"    = unname(upwr_cat["szalwia"]),
    "Chinstrap" = unname(upwr_cat["bursztyn"]),
    "Gentoo"    = unname(upwr_cat["terakota"])
  )

  output$ch3_control_plot <- renderPlot({
    df <- .cas_data
    step <- ch3_control_step()
    if (step == 1) {
      ggplot(df, aes(x = bill_length_mm, y = bill_depth_mm)) +
        geom_point(color = upwr_secondary, alpha = 0.5) +
        geom_smooth(method = "lm", se = FALSE, color = unname(upwr_cat["niebo"])) +
        labs(x = "Długość dzioba (mm)", y = "Wysokość dzioba (mm)") +
        theme_upwr()
    } else if (step == 2) {
      ggplot(df, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = FALSE) +
        scale_color_manual(values = ch3_species_cols, name = "Gatunek") +
        labs(x = "Długość dzioba (mm)", y = "Wysokość dzioba (mm)") +
        theme_upwr() +
        theme(legend.position = "top")
    } else {
      model <- lm(bill_depth_mm ~ bill_length_mm + species, data = df)
      coefs <- broom::tidy(model)
      coefs <- coefs[coefs$term != "(Intercept)", ]
      labels <- c(
        bill_length_mm   = "Długość dzioba",
        speciesChinstrap = "Gatunek: Chinstrap",
        speciesGentoo    = "Gatunek: Gentoo"
      )
      coefs$term <- ifelse(coefs$term %in% names(labels), labels[coefs$term], coefs$term)
      ggplot(coefs, aes(x = estimate, y = term)) +
        geom_vline(xintercept = 0, linetype = "dashed", color = upwr_secondary) +
        geom_point(color = unname(upwr_cat["wrzos"]), size = 3) +
        geom_errorbarh(aes(xmin = estimate - 1.96 * std.error,
                           xmax = estimate + 1.96 * std.error),
                       height = 0.2, color = unname(upwr_cat["wrzos"])) +
        labs(x = "β w modelu z kontrolą gatunku", y = NULL) +
        theme_upwr()
    }
  })

  output$ch3_control_info <- renderUI({
    df <- .cas_data
    step <- ch3_control_step()
    m_simple <- lm(bill_depth_mm ~ bill_length_mm, data = df)
    m_ctrl <- lm(bill_depth_mm ~ bill_length_mm + species, data = df)
    ts <- broom::tidy(m_simple)
    tc <- broom::tidy(m_ctrl)
    term_labels <- c(
      bill_length_mm   = "Długość dzioba",
      speciesChinstrap = "Gatunek: Chinstrap",
      speciesGentoo    = "Gatunek: Gentoo"
    )
    if (step == 1) {
      tagList(
        lc_stat_box("β długość dzioba", round(ts$estimate[2], 3), color = unname(upwr_cat["niebo"])),
        lc_stat_box("p", format_p_value(ts$p.value[2]), color = upwr_secondary),
        lc_feedback(type = "warning",
          p("W modelu prostym współczynnik jest ujemny: pingwiny o dłuższym
            dziobie mają pozornie niższy dziób. Ale to złudzenie — w danych
            siedzą trzy gatunki o różnej budowie."))
      )
    } else if (step == 2) {
      tagList(
        lc_feedback(type = "info",
          p("Po rozbiciu na gatunki obraz się odwraca: wewnątrz każdego gatunku
            dłuższy dziób idzie w parze z wyższym dziobem. Pomieszanie trzech
            chmur dawało fałszywy ujemny trend."))
      )
    } else {
      rows <- lapply(2:nrow(tc), function(i) {
        term <- tc$term[i]
        term_pl <- if (term %in% names(term_labels)) term_labels[[term]] else term
        tags$tr(tags$td(term_pl), tags$td(round(tc$estimate[i], 4)),
                tags$td(round(tc$std.error[i], 4)), tags$td(format_p_value(tc$p.value[i])))
      })
      tagList(
        lc_stat_box("β długość dzioba (z kontrolą)", round(tc$estimate[2], 3),
                    color = unname(upwr_cat["szalwia"])),
        tags$table(class = "lc-table lc-table-bordered lc-table-striped",
          tags$thead(tags$tr(tags$th("Zmienna"), tags$th("β"), tags$th("SE"), tags$th("p"))),
          tags$tbody(rows)
        ),
        lc_feedback(type = "ok",
          p("Po dołączeniu gatunku współczynnik długości dzioba zmienia znak na
            dodatni — to jest paradoks Simpsona. Kontrola gatunku ujawnia
            prawdziwą zależność wewnątrz grup."))
      )
    }
  })

  # Widget "Efekt dodawania zmiennych" został przeniesiony do ch4
  # (Jak porównywać modele) — tam pasuje merytorycznie.

  # --- Widget: multikolinearnosc ---
  ch3_collin_data <- reactiveVal(NULL)

  observeEvent(input$ch3_collin_new, {
    ch3_collin_data(generate_collinearity_data(140, input$ch3_collin_rho))
  })

  output$ch3_collin_plot <- renderPlot({
    df <- ch3_collin_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Generuj i dopasuj'",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      ggplot(df, aes(x = x1, y = x2)) +
        geom_point(color = upwr_secondary, alpha = 0.5) +
        geom_smooth(method = "lm", se = FALSE, color = unname(upwr_cat["niebo"])) +
        labs(x = "X₁", y = "X₂") +
        theme_upwr()
    }
  })

  output$ch3_collin_info <- renderUI({
    df <- ch3_collin_data()
    if (is.null(df)) return(NULL)
    model <- lm(y ~ x1 + x2, data = df)
    coefs <- broom::tidy(model)
    vifs <- compute_vif_simple(df, c("x1", "x2"))
    rows <- lapply(2:nrow(coefs), function(i) {
      term <- coefs$term[i]
      tags$tr(
        tags$td(term),
        tags$td(round(coefs$estimate[i], 3)),
        tags$td(round(coefs$std.error[i], 3)),
        tags$td(format_p_value(coefs$p.value[i])),
        tags$td(round(vifs[[term]], 2))
      )
    })
    tagList(
      lc_stat_box("corr(X₁,X₂)", round(cor(df$x1, df$x2), 2), color = unname(upwr_cat["niebo"])),
      lc_stat_box("R² modelu", round(summary(model)$r.squared, 3), color = unname(upwr_cat["szalwia"])),
      tags$table(class = "lc-table lc-table-bordered lc-table-striped",
        style = "font-size: 13px;",
        tags$thead(tags$tr(tags$th("Zmienna"), tags$th("β"), tags$th("SE"), tags$th("p"), tags$th("VIF"))),
        tags$tbody(rows)
      ),
      lc_feedback(type = "warning",
        p("Im bardziej X₁ i X₂ są podobne, tym trudniej modelowi stabilnie przypisać osobny efekt każdej zmiennej."))
    )
  })
}
