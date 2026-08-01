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
      p("Klasyczny przykład: w danych CASchools wyniki uczniów rosną wraz
        z wydatkami na ucznia. Brzmi prosto — ale wydatki są skorelowane
        z dochodem okręgu, a okręgi bogate mają też mniejsze klasy. Co
        ", tags$em("naprawdę"), " wpływa na wyniki? Tego nie powie nam żadne
        z pojedynczych równań. Musimy zbudować model z wieloma X-ami naraz.")
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
      p("Zobaczmy to na realnych danych CASchools: wyniki testów szkolnych
        w zależności od cech okręgu. Wybierz, które predyktory dodać — i zwróć
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
      label = "Ryc. 3.1", title = "CASchools: model z wieloma predyktorami",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Dane: 420 okręgów szkolnych w Kalifornii. Przełączaj
                    predyktory — model przelicza się na tych samych realnych
                    danych."),
          selectInput("ch3_outcome", "Zmienna zależna Y:",
            choices = c(
              "Wynik: czytanie" = "read",
              "Wynik: matematyka" = "math"
            ),
            selected = "read"
          ),
          tags$div(class = "form-group shiny-input-container",
            tags$label(class = "control-label", "Predyktor bazowy:"),
            tags$div(tags$strong("Dochód okręgu (tys. USD)"))
          ),
          checkboxGroupInput("ch3_predictors", "Dodaj predyktory:",
            choices = c(
              "Dotacje do obiadów (%)" = "lunch",
              "Angielski jako drugi język (%)" = "english",
              "Uczniowie / nauczyciel" = "student_teacher_ratio",
              "Wydatki na ucznia" = "expenditure",
              "Komputery" = "computer",
              "CalWORKs (%)" = "calworks"
            ),
            selected = "lunch"
          ),
          helpText(style = "margin-top: 8px; font-size: 12px;",
            "Model zawsze zaczyna od dochodu okręgu. Kolejne checkboxy
             dodają następne predyktory do tego samego równania.")
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
      p("Widget pokazuje to krok po kroku na CASchools: najpierw policzymy
        prosty związek wyniku czytania z dochodem okręgu, potem z odsetkiem
        uczniów z dotacją do obiadu, a na końcu zobaczymy, co zostaje, gdy
        obie informacje kontrolujemy naraz razem z odsetkiem uczniów uczących
        się angielskiego jako drugiego języka.")
    ),

    figure_panel(
      label = "Ryc. 3.2", title = "Efekt pozorny i kontrola zmiennych",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Porównujemy modele proste i model wieloraki na tych samych
                    420 okręgach szkolnych."),
          h5("Kroki:"),
          actionButton("ch3_control_step1", "1. Czytanie ~ dochód",
                       class = "lc-btn-outline", width = "100%"),
          actionButton("ch3_control_step2", "2. Czytanie ~ lunch",
                       class = "lc-btn-outline", width = "100%"),
          actionButton("ch3_control_step3", "3. Model z kontrolą",
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
        "? Powiedzmy: dochód okręgu i wydatki na ucznia są silnie ze sobą
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
    "read" = "Wynik: czytanie",
    "math" = "Wynik: matematyka",
    "lunch" = "Dotacje do obiadów (%)",
    "income" = "Dochód okręgu (tys. USD)",
    "english" = "Angielski jako drugi język (%)",
    "student_teacher_ratio" = "Uczniowie / nauczyciel",
    "expenditure" = "Wydatki na ucznia",
    "computer" = "Komputery",
    "calworks" = "CalWORKs (%)"
  )

  ch3_base_x <- "income"

  ch3_selected_predictors <- reactive({
    unique(c(ch3_base_x, input$ch3_predictors))
  })

  # Model jako reactive: zależy od danych i wyboru predyktorów.
  # Dzięki temu przełączanie checkboxów porównuje modele na TYCH SAMYCH danych.
  ch3_model <- reactive({
    df <- .cas_data
    outcome <- input$ch3_outcome
    if (is.null(outcome)) outcome <- "read"
    preds <- ch3_selected_predictors()
    formula <- as.formula(paste(outcome, "~", paste(preds, collapse = " + ")))
    lm(formula, data = df)
  })

  output$ch3_model_coefs <- renderUI({
    model <- ch3_model()
    if (is.null(model)) return(NULL)

    coefs <- broom::tidy(model)

    labels_pl <- c("(Intercept)" = "Wyraz wolny", ch3_labels_pl)

    coefs$term_pl <- ifelse(coefs$term %in% names(labels_pl),
                             labels_pl[coefs$term], coefs$term)

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

  ch3_group_reps <- function(df, var, labels) {
    group <- ch3_make_bins(df[[var]], labels)
    reps <- tapply(df[[var]], group, median, na.rm = TRUE)
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
      grid[[extra[1]]] <- as.numeric(color_info$reps[grid$color_group])
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
      grid[[facet_vars[1]]] <- as.numeric(facet_info_1$reps[grid$facet_1])
    }

    if (length(facet_vars) >= 2) {
      facet_info_2 <- ch3_group_reps(df, facet_vars[2], c("niższe", "wyższe"))
      grid <- merge(
        grid,
        data.frame(facet_2 = names(facet_info_2$reps), stringsAsFactors = FALSE),
        all = TRUE
      )
      grid[[facet_vars[2]]] <- as.numeric(facet_info_2$reps[grid$facet_2])
    }

    other_vars <- setdiff(predictors, names(grid))
    for (var in other_vars) {
      grid[[var]] <- mean(df[[var]], na.rm = TRUE)
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
    if (is.null(outcome)) outcome <- "read"
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
      p <- p +
        geom_point(aes(color = color_group), alpha = 0.68, size = 1.9) +
        geom_line(data = grid, aes(x = .data[[x_var]], y = pred, color = color_group),
                  linewidth = 1.05) +
        scale_color_manual(
          values = c(
            "niskie" = unname(upwr_cat["szalwia"]),
            "średnie" = unname(upwr_cat["bursztyn"]),
            "wysokie" = unname(upwr_cat["terakota"])
          ),
          name = ch3_labels_pl[[color_var]]
        )
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
    if (is.null(outcome)) outcome <- "read"
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
      model_grid[[var]] <- mean(df[[var]], na.rm = TRUE)
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
  }, alt = "Wykres współczynników modelu regresji z 95% przedziałami ufności.")

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

  # --- Widget: kontrola zmiennych na CASchools ---
  ch3_control_step <- reactiveVal(1)

  observeEvent(input$ch3_control_step1, ch3_control_step(1))
  observeEvent(input$ch3_control_step2, ch3_control_step(2))
  observeEvent(input$ch3_control_step3, ch3_control_step(3))

  output$ch3_control_plot <- renderPlot({
    df <- .cas_data
    step <- ch3_control_step()
    if (step == 1) {
      ggplot(df, aes(x = income, y = read)) +
        geom_point(color = upwr_secondary, alpha = 0.5) +
        geom_smooth(method = "lm", se = FALSE, color = unname(upwr_cat["niebo"])) +
        labs(x = "Dochód okręgu (tys. USD)", y = "Wynik: czytanie") +
        theme_upwr()
    } else if (step == 2) {
      ggplot(df, aes(x = lunch, y = read)) +
        geom_point(color = upwr_secondary, alpha = 0.5) +
        geom_smooth(method = "lm", se = FALSE, color = unname(upwr_cat["szalwia"])) +
        labs(x = "Dotacje do obiadów (%)", y = "Wynik: czytanie") +
        theme_upwr()
    } else {
      model <- lm(read ~ income + lunch + english, data = df)
      coefs <- broom::tidy(model)
      coefs <- coefs[coefs$term != "(Intercept)", ]
      labels <- c(
        income = "Dochód okręgu",
        lunch = "Dotacje do obiadów",
        english = "Angielski jako drugi język"
      )
      coefs$term <- labels[coefs$term]
      ggplot(coefs, aes(x = estimate, y = term)) +
        geom_vline(xintercept = 0, linetype = "dashed", color = upwr_secondary) +
        geom_point(color = unname(upwr_cat["wrzos"]), size = 3) +
        geom_errorbarh(aes(xmin = estimate - 1.96 * std.error,
                           xmax = estimate + 1.96 * std.error),
                       height = 0.2, color = unname(upwr_cat["wrzos"])) +
        labs(x = "β w modelu wielorakim", y = NULL) +
        theme_upwr()
    }
  }, alt = "Wykres pokazujący zależności proste i współczynniki po kontroli pozostałych zmiennych.")

  output$ch3_control_info <- renderUI({
    df <- .cas_data
    step <- ch3_control_step()
    m_income <- lm(read ~ income, data = df)
    m_lunch <- lm(read ~ lunch, data = df)
    m_both <- lm(read ~ income + lunch + english, data = df)
    ti <- broom::tidy(m_income)
    tl <- broom::tidy(m_lunch)
    tb <- broom::tidy(m_both)
    if (step == 1) {
      tagList(
        lc_stat_box("β dochód", round(ti$estimate[2], 3), color = unname(upwr_cat["niebo"])),
        lc_stat_box("p", format_p_value(ti$p.value[2]), color = upwr_secondary),
        lc_feedback(type = "info",
          p("W modelu prostym bogatsze okręgi mają wyższe wyniki czytania.
            Ale dochód niesie też informację o składzie społecznym okręgu,
            więc nie traktujemy tego jeszcze jako czystego efektu dochodu."))
      )
    } else if (step == 2) {
      tagList(
        lc_stat_box("β lunch", round(tl$estimate[2], 3), color = unname(upwr_cat["szalwia"])),
        lc_stat_box("p", format_p_value(tl$p.value[2]), color = upwr_secondary),
        lc_feedback(type = "info",
          p("Odsetek uczniów z dotacją do obiadu jest silnie ujemnie
            powiązany z wynikiem czytania. W kolejnym kroku sprawdzimy,
            co zostaje po kontroli dochodu i odsetka uczniów uczących się
            angielskiego."))
      )
    } else {
      rows <- lapply(2:nrow(tb), function(i) {
        tags$tr(tags$td(tb$term[i]), tags$td(round(tb$estimate[i], 4)),
                tags$td(round(tb$std.error[i], 4)), tags$td(format_p_value(tb$p.value[i])))
      })
      tagList(
        tags$table(class = "lc-table lc-table-bordered lc-table-striped",
          tags$thead(tags$tr(tags$th("Zmienna"), tags$th("β"), tags$th("SE"), tags$th("p"))),
          tags$tbody(rows)
        ),
        lc_feedback(type = "warning",
          p("Współczynnik oznacza efekt danej zmiennej po odjęciu informacji
            wspólnej z pozostałymi predyktorami. To nadal nie jest dowód
            przyczynowy, tylko lepszy opis zależności w danych obserwacyjnych."))
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
  }, alt = "Wykres punktowy dwóch coraz silniej współliniowych predyktorów.")

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
