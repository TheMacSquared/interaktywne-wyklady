# ============================================================================
# CHAPTER 4: Jak porównywać modele
# ============================================================================

ch4_ui <- list(
  id    = "ch-porownanie",
  num   = "04",
  title = "Jak porównywać modele?",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 04 · Regresja",
      num    = "04",
      title  = "Jak porównywać modele?",
      lead   = "W ch2 oceniliśmy pojedynczy model. Teraz mamy kilku kandydatów —
                jak wybrać najlepszego?"
    ),

    tagList(
      p("W rozdziale 2 nauczyliśmy się oceniać ", tags$em("jeden"),
        " model: wzorzec reszt, R², RMSE. W rozdziale 3 zbudowaliśmy modele
        z różną liczbą predyktorów. Teraz pojawia się naturalne pytanie:
        który z tych modeli wybrać?"),
      p("Można by chcieć po prostu wziąć model o najwyższym R². Okazuje się
        jednak, że to klasyczna pułapka — zobaczymy zaraz dlaczego.")
    ),

    lc_h2("ch4-problem", "Dlaczego sam R² nie wystarczy?"),

    tagList(
      p("R² ma jedną zdradliwą właściwość przy porównaniach: zawsze rośnie,
        kiedy dodajemy do modelu kolejny predyktor — nawet zupełnie
        bezsensowny. Matematycznie nie jest to przypadek: każdy nowy X
        może tylko zmniejszyć (albo pozostawić bez zmian) sumę kwadratów
        reszt, więc R² nigdy nie spada."),
      p("Konsekwencja: jeśli porównujemy modele o ", tags$em("różnej"),
        " liczbie zmiennych, R² nie jest sprawiedliwym sędzią — premiuje
        ten bardziej rozdęty, nawet gdy dodatkowe X-y są przypadkiem.
        Potrzebujemy metryk, które ", tags$em("karzą za złożoność"), ".")
    ),

    inline_callout(label = "Uwaga", color = "uwaga",
      "Wybór modelu o największym R² to klasyczna droga do przeuczenia.
       Następny widget pokaże, jak R² rośnie nawet wtedy, gdy nowe
       predyktory niewiele wnoszą."
    ),

    lc_h2("ch4-efekt-dodawania", "Efekt dodawania zmiennych"),

    tagList(
      p("Zobaczmy to empirycznie. Widget buduje cztery modele kolejno z
        1, 2, 3 i 4 predyktorami i pokazuje, jak zmieniają się metryki.")
    ),

    figure_panel(
      label = "Ryc. 4.1", title = "Krok po kroku",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Modele z 1, 2, 3 i 4 predyktorami — porównanie metryk."),
          actionButton("ch4_stepwise", "Buduj modele krok po kroku",
                       class = "lc-btn-warning", width = "100%")
        ),
        column(8,
          zoom_plot_ui("ch4_step_plot", height = "300px"),
          uiOutput("ch4_step_table")
        )
      )
    ),

    inline_callout(label = "Co się dzieje?", color = "wskazowka",
      "R² stale rośnie. Adjusted R² i AIC zaczynają w pewnym momencie
       się stabilizować albo wręcz pogarszać — to sygnał, że dodawanie
       kolejnego X przestaje się opłacać."
    ),

    lc_h2("ch4-metryki", "Metryki porównawcze"),

    tagList(
      p("W rozdziale 2 mieliśmy ", withMathJax("\\(R^2\\)"), " i ", tags$em("RMSE"),
        " — miary jakości pojedynczego modelu. Teraz dochodzą trzy metryki
        porównawcze, które albo karzą za złożoność, albo dzielą dane na trening i test:"),
      tags$table(class = "lc-table lc-table-bordered", style = "font-size: 14px;",
        tags$thead(
          tags$tr(tags$th("Metryka"), tags$th("Co mierzy"), tags$th("Lepiej gdy"))
        ),
        tags$tbody(
          tags$tr(tags$td(withMathJax("\\(R^2_{adj}\\)")),
                  tags$td("R² skorygowane za liczbę predyktorów"),
                  tags$td("wyższe")),
          tags$tr(tags$td("AIC"),
                  tags$td("Jakość + złożoność (kara za parametry)"),
                  tags$td("niższe")),
          tags$tr(tags$td("BIC"),
                  tags$td("Jak AIC, ale silniejsza kara za parametry"),
                  tags$td("niższe"))
        )
      )
    ),

    lc_formula_box(
      p(tags$strong("AIC:"), withMathJax("\\(AIC = -2 \\ln(L) + 2k\\)")),
      p(tags$strong("BIC:"), withMathJax("\\(BIC = -2 \\ln(L) + k \\ln(n)\\)")),
      p("gdzie L = wiarygodność modelu, k = liczba parametrów, n = liczba obserwacji")
    ),

    tagList(
      p("AIC i BIC są bezsensowne w izolacji — usłyszeć „AIC = 2384\" nic
        nie mówi. Ich sens jest ", tags$em("różnicowy"),
        ": porównujemy kilka modeli i wybieramy ten o ", tags$em("niższej"),
        " wartości. Im większa różnica, tym pewniejszy wybór.")
    ),

    lc_h2("ch4-arena", "Arena modeli liniowych"),

    tagList(
      p("Zobaczmy te metryki w akcji. Widget generuje dane i buduje cztery
        modele o rosnącej złożoności — porównaj, który wygrywa w każdej
        kategorii.")
    ),

    figure_panel(
      label = "Ryc. 4.2", title = "Porównanie modeli regresji",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Generujemy dane i budujemy 4 modele z różną
                    liczbą predyktorów."),
          sliderInput("ch4_n", "n:", min = 50, max = 300, value = 150, step = 25),
          actionButton("ch4_compare", "Buduj i porównaj modele",
                       class = "lc-btn-primary", width = "100%")
        ),
        column(8,
          zoom_plot_ui("ch4_metrics_plot", height = "350px"),
          uiOutput("ch4_metrics_table")
        )
      )
    ),

    inline_callout(label = "AIC vs BIC", color = "wskazowka",
      "AIC faworyzuje modele z lepszą predykcją (nawet złożone).
       BIC mocniej karze za złożoność — preferuje prostsze modele.
       Gdy się nie zgadzają, AIC jest lepszy do predykcji, BIC do
       wyjaśnienia."
    ),

    lc_h2("ch4-overfitting", "Przeuczenie (overfitting)"),

    tagList(
      p("AIC i BIC działają, gdy modele są ", tags$em("zagnieżdżone"),
        " (jeden zawiera predyktory drugiego). Co, jeśli porównujemy modele
        zasadniczo różne — np. wielomian różnego stopnia? Najlepszą miarą
        staje się wtedy generalizacja na nowe dane."),
      p("Najpierw zobaczmy sam efekt przeuczenia: model z dużą liczbą
        parametrów może idealnie dopasować się do danych treningowych,
        ale działać fatalnie na nowych obserwacjach.")
    ),

    figure_panel(
      label = "Ryc. 4.3", title = "Wielomian: dopasowanie vs generalizacja",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch4_poly_degree", "Stopień wielomianu:",
                      min = 1, max = 15, value = 1, step = 1),
          sliderInput("ch4_poly_n", "n (punktów):",
                      min = 15, max = 100, value = 30, step = 5),
          actionButton("ch4_poly_gen", "Generuj",
                       class = "lc-btn-primary", width = "100%")
        ),
        column(8,
          zoom_plot_ui("ch4_poly_plot", height = "300px"),
          uiOutput("ch4_poly_stats")
        )
      )
    ),

    lc_h2("ch4-train-test", "Train/test: ostateczny sędzia"),

    tagList(
      p("Podział danych na zbiór treningowy i testowy: model uczy się na
        jednej części, a my oceniamy go na drugiej. Jeśli model dobrze
        działa tylko na treningowej, a źle na testowej — to przeuczenie."),
      p("To najuczciwszy test, bo dane testowe ", tags$em("naprawdę"),
        " są dla modelu nowe.")
    ),

    figure_panel(
      label = "Ryc. 4.4", title = "Train/test: kiedy model przestaje generalizować",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch4_tt_degree", "Stopień wielomianu:",
                      min = 1, max = 15, value = 1, step = 1),
          actionButton("ch4_tt_new", "Nowy podział train/test",
                       class = "lc-btn-warning", width = "100%")
        ),
        column(8,
          zoom_plot_ui("ch4_tt_plot", height = "330px"),
          uiOutput("ch4_tt_info")
        )
      )
    ),

    inline_callout(label = "Złota reguła", color = "uwaga",
      "Najlepszy model to nie ten z najwyższym R², ale ten, który
       najlepiej generalizuje na nowe dane. Używaj AIC/BIC do wyboru
       złożoności, train/test do ostatecznej weryfikacji."
    ),

    lc_h2("ch4-co-dalej", "Co dalej"),

    tagList(
      p("Mamy komplet narzędzi do porównywania: ",
        withMathJax("\\(R^2_{adj}\\)"),
        ", AIC, BIC dla modeli o różnej liczbie predyktorów; train/test
        dla zasadniczo różnych modeli. Wszystkie zakładały jednak, że Y
        jest ", tags$em("ciągłe"), "."),
      p("A co, gdy Y to zdał/nie zdał, kliknął/nie kliknął, kupił/nie
        kupił? Wtedy regresja liniowa zawodzi — daje predykcje poza
        zakresem [0, 1] i nie jest sensownym modelem. Następny rozdział
        wprowadza regresję logistyczną, która jest stworzona dokładnie dla takich sytuacji.")
    ),

    lc_chapter_next(
      num       = "05",
      title     = "Regresja logistyczna",
      lead      = "gdy zmienna zależna jest binarna",
      target_id = "ch-logistyczna"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch4_server <- function(input, output, session) {

  # --- Widget: Efekt dodawania zmiennych (przeniesiony z ch3 wielorakiej) ---
  ch4_step_data <- reactiveVal(NULL)

  observeEvent(input$ch4_stepwise, {
    df <- generate_multi_data(150)

    pred_sets <- list(
      c("godziny_nauki"),
      c("godziny_nauki", "frekwencja"),
      c("godziny_nauki", "frekwencja", "stres"),
      c("godziny_nauki", "frekwencja", "stres", "sen_h")
    )

    results <- lapply(seq_along(pred_sets), function(i) {
      formula <- as.formula(paste("ocena ~", paste(pred_sets[[i]], collapse = " + ")))
      model <- lm(formula, data = df)
      metrics <- compute_model_metrics(model)
      data.frame(
        k = i,
        predictors = paste(pred_sets[[i]], collapse = " + "),
        r_squared = metrics$r_squared,
        adj_r_squared = metrics$adj_r_squared,
        aic = metrics$aic,
        bic = metrics$bic,
        rmse = metrics$rmse
      )
    })

    ch4_step_data(do.call(rbind, results))
  })

  zoom_plot_server("ch4_step_plot", reactive({
    df <- ch4_step_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Buduj modele'",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      long <- df %>%
        select(k, r_squared, adj_r_squared) %>%
        tidyr::pivot_longer(cols = c(r_squared, adj_r_squared),
                            names_to = "metric", values_to = "value") %>%
        mutate(metric = ifelse(metric == "r_squared", "R²", "adj. R²"))

      ggplot(long, aes(x = k, y = value, color = metric)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 3) +
        scale_x_continuous(breaks = 1:4,
                           labels = paste0(1:4, " pred.")) +
        scale_color_manual(values = c(unname(upwr_cat["niebo"]), unname(upwr_cat["szalwia"])), name = NULL) +
        labs(
             x = "Liczba predyktorów", y = "Wartość") +
        theme_upwr() +
        theme(legend.position = "top")
    }
  }))

  output$ch4_step_table <- renderUI({
    df <- ch4_step_data()
    if (is.null(df)) return(NULL)

    rows <- lapply(1:nrow(df), function(i) {
      tags$tr(
        tags$td(df$predictors[i]),
        tags$td(round(df$r_squared[i], 3)),
        tags$td(round(df$adj_r_squared[i], 3)),
        tags$td(round(df$aic[i], 1)),
        tags$td(round(df$bic[i], 1)),
        tags$td(round(df$rmse[i], 3))
      )
    })

    tags$table(class = "lc-table lc-table-bordered lc-table-striped",
      style = "font-size: 13px;",
      tags$thead(
        tags$tr(tags$th("Predyktory"), tags$th("R²"), tags$th("adj.R²"),
                tags$th("AIC"), tags$th("BIC"), tags$th("RMSE"))
      ),
      tags$tbody(rows)
    )
  })

  # --- Widget: Arena modeli liniowych ---
  ch4_models <- reactiveVal(NULL)

  observeEvent(input$ch4_compare, {
    df <- generate_multi_data(input$ch4_n)

    models <- list(
      "1: nauka" = lm(ocena ~ godziny_nauki, data = df),
      "2: nauka + frekw." = lm(ocena ~ godziny_nauki + frekwencja, data = df),
      "3: nauka + frekw. + stres" = lm(ocena ~ godziny_nauki + frekwencja + stres, data = df),
      "4: wszystkie" = lm(ocena ~ godziny_nauki + frekwencja + stres + sen_h, data = df)
    )

    results <- lapply(names(models), function(name) {
      m <- compute_model_metrics(models[[name]])
      data.frame(
        model = name,
        r_squared = m$r_squared,
        adj_r_squared = m$adj_r_squared,
        aic = m$aic,
        bic = m$bic,
        rmse = m$rmse,
        n_params = m$n_params
      )
    })

    ch4_models(do.call(rbind, results))
  })

  zoom_plot_server("ch4_metrics_plot", reactive({
    df <- ch4_models()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Buduj i porównaj'",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      long <- df %>%
        select(model, adj_r_squared, aic, bic, rmse) %>%
        tidyr::pivot_longer(-model, names_to = "metric", values_to = "value") %>%
        mutate(metric = factor(metric,
          levels = c("adj_r_squared", "aic", "bic", "rmse"),
          labels = c("adj. R²", "AIC", "BIC", "RMSE")))

      ggplot(long, aes(x = model, y = value, fill = model)) +
        geom_col(alpha = 0.8) +
        facet_wrap(~metric, scales = "free_y", ncol = 4) +
        scale_fill_manual(values = c(unname(upwr_cat["niebo"]), unname(upwr_cat["szalwia"]), unname(upwr_cat["bursztyn"]), unname(upwr_cat["wrzos"]))) +
        labs(x = NULL, y = "Wartość") +
        theme_upwr() +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
    }
  }))

  output$ch4_metrics_table <- renderUI({
    df <- ch4_models()
    if (is.null(df)) return(NULL)

    best_adj_r2 <- which.max(df$adj_r_squared)
    best_aic <- which.min(df$aic)
    best_bic <- which.min(df$bic)
    best_rmse <- which.min(df$rmse)

    rows <- lapply(1:nrow(df), function(i) {
      tags$tr(
        tags$td(df$model[i]),
        tags$td(round(df$r_squared[i], 3)),
        tags$td(style = if (i == best_adj_r2) "font-weight:bold; color:var(--upwr-sage);" else "",
                round(df$adj_r_squared[i], 3)),
        tags$td(style = if (i == best_aic) "font-weight:bold; color:var(--upwr-sage);" else "",
                round(df$aic[i], 1)),
        tags$td(style = if (i == best_bic) "font-weight:bold; color:var(--upwr-sage);" else "",
                round(df$bic[i], 1)),
        tags$td(style = if (i == best_rmse) "font-weight:bold; color:var(--upwr-sage);" else "",
                round(df$rmse[i], 3))
      )
    })

    tags$table(class = "lc-table lc-table-bordered lc-table-striped",
      style = "font-size: 13px;",
      tags$thead(
        tags$tr(tags$th("Model"), tags$th("R²"), tags$th("adj.R²"),
                tags$th("AIC"), tags$th("BIC"), tags$th("RMSE"))
      ),
      tags$tbody(rows)
    )
  })

  # --- Widget: Overfitting ---
  ch4_poly_data <- reactiveVal(NULL)

  observeEvent(input$ch4_poly_gen, {
    n <- input$ch4_poly_n
    x <- sort(runif(n, 0, 10))
    y <- sin(x) * 3 + rnorm(n, 0, 1)
    ch4_poly_data(data.frame(x = x, y = y))
  })

  zoom_plot_server("ch4_poly_plot", reactive({
    df <- ch4_poly_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Generuj'",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      degree <- input$ch4_poly_degree
      model <- lm(y ~ poly(x, degree), data = df)

      x_pred <- seq(min(df$x), max(df$x), length.out = 200)
      y_pred <- predict(model, newdata = data.frame(x = x_pred))

      pred_df <- data.frame(x = x_pred, y = y_pred)

      ggplot() +
        geom_point(data = df, aes(x = x, y = y), color = upwr_secondary, alpha = 0.5) +
        geom_line(data = pred_df, aes(x = x, y = y), color = unname(upwr_cat["niebo"]), linewidth = 1.2) +
        labs(
             x = "X", y = "Y") +
        theme_upwr()
    }
  }))

  output$ch4_poly_stats <- renderUI({
    df <- ch4_poly_data()
    if (is.null(df)) return(NULL)
    degree <- input$ch4_poly_degree
    model <- lm(y ~ poly(x, degree), data = df)
    metrics <- compute_model_metrics(model)

    tagList(
      lc_stat_box("R²", round(metrics$r_squared, 3), color = unname(upwr_cat["niebo"])),
      lc_stat_box("adj.R²", round(metrics$adj_r_squared, 3), color = unname(upwr_cat["szalwia"])),
      lc_stat_box("AIC", round(metrics$aic, 1), color = unname(upwr_cat["bursztyn"])),
      lc_stat_box("BIC", round(metrics$bic, 1), color = upwr_secondary)
    )
  })

  # --- Widget: train/test overfitting ---
  ch4_tt_data <- reactiveVal(generate_train_test_poly())

  observeEvent(input$ch4_tt_new, {
    ch4_tt_data(generate_train_test_poly())
  })

  zoom_plot_server("ch4_tt_plot", reactive({
    sets <- ch4_tt_data()
    train <- sets$train
    test <- sets$test
    degree <- input$ch4_tt_degree
    model <- lm(y ~ poly(x, degree), data = train)
    grid <- data.frame(x = seq(0, 10, length.out = 300))
    grid$y <- predict(model, newdata = grid)

    ggplot() +
      geom_point(data = test, aes(x = x, y = y), color = unname(upwr_cat["bursztyn"]),
                 alpha = 0.25, size = 1.8) +
      geom_point(data = train, aes(x = x, y = y), color = upwr_secondary,
                 alpha = 0.75, size = 2.2) +
      geom_line(data = grid, aes(x = x, y = y), color = unname(upwr_cat["niebo"]),
                linewidth = 1.2) +
      labs(x = "X", y = "Y") +
      theme_upwr()
  }))

  output$ch4_tt_info <- renderUI({
    sets <- ch4_tt_data()
    train <- sets$train
    test <- sets$test
    degree <- input$ch4_tt_degree

    degrees <- 1:15
    metrics <- lapply(degrees, function(d) {
      model <- lm(y ~ poly(x, d), data = train)
      train_rmse <- sqrt(mean((train$y - predict(model, train))^2))
      test_rmse <- sqrt(mean((test$y - predict(model, test))^2))
      data.frame(degree = d, train_rmse = train_rmse, test_rmse = test_rmse)
    })
    metrics <- do.call(rbind, metrics)
    current <- metrics[metrics$degree == degree, ]
    best <- metrics[which.min(metrics$test_rmse), ]

    tagList(
      lc_stat_box("RMSE train", round(current$train_rmse, 2), color = unname(upwr_cat["niebo"])),
      lc_stat_box("RMSE test", round(current$test_rmse, 2), color = unname(upwr_cat["bursztyn"])),
      lc_stat_box("Najlepszy test", paste0("stopień ", best$degree), caption = paste("RMSE", round(best$test_rmse, 2)), color = unname(upwr_cat["szalwia"])),
      lc_feedback(type = if (current$test_rmse > best$test_rmse * 1.25) "warning" else "info",
        p("Model może coraz lepiej zapamiętywać trening, ale oceniamy go po błędzie na nowych danych."))
    )
  })
}
