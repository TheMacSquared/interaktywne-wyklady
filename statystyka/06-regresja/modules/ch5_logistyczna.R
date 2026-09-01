# ============================================================================
# CHAPTER 5: Regresja logistyczna
# ============================================================================

ch5_ui <- list(
  id    = "ch-logistyczna",
  num   = "05",
  title = "Regresja logistyczna",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 05 · Regresja",
      num   = "05",
      title  = "Regresja logistyczna.",
      lead   = "Regresja liniowa wymaga ciągłej zmiennej zależnej.
                A co, gdy Y to 0 lub 1 (sukces/porażka)?"
    ),

    tagList(
      p("W rozdziale 4 spotkaliśmy się z porównywaniem modeli liniowych —
        ale wszystkie zakładały, że Y jest ciągłe. Co, gdy Y to ",
        tags$em("zdał albo nie zdał"), "? Albo ",
        tags$em("kliknął albo nie kliknął"), "? Albo ",
        tags$em("przeżył albo nie przeżył"), "? Wtedy regresja liniowa nie
        odmawia odpowiedzi — ale ta odpowiedź jest bezsensowna."),
      p("Zaraz zobaczymy, dlaczego — a potem poznamy alternatywę.")
    ),

    lc_h2("ch5-od-ciaglej-do-binarnej", "Od wyniku ciągłego do zmiennej 0/1"),

    tagList(
      p("Najprościej zobaczyć regresję logistyczną jako odpowiedź na sytuację,
        w której zwykły wynik liczbowy zamieniamy na zdarzenie: zdał albo
        nie zdał. Próg nie jest drobiazgiem technicznym — to definicja
        zmiennej zależnej."),
      p("Poniżej używamy danych CASchools. Najpierw patrzymy na oryginalny
        wynik czytania, potem ustawiamy próg zaliczenia i dopiero z tak
        utworzonego Y = 0/1 budujemy model logistyczny.")
    ),

    lc_feedback(
      type = "warning",
      tags$strong("To demonstracja, nie zalecenie:"),
      " sztuczne progowanie wyniku ciągłego traci informację. Jeśli wynik",
      " punktowy jest dostępny i odpowiada na pytanie badawcze, zwykle lepiej",
      " modelować go bez zamiany na 0/1. Logistyczna jest naturalna wtedy,",
      " gdy samo zdarzenie jest binarne."
    ),

    figure_panel(
      label = "Ryc. 5.0", title = "Od wyniku punktowego do prawdopodobieństwa zdania",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch5_cas_y_cut", "Próg zaliczenia: zdał od:",
                      min = 630, max = 680, value = 656, step = 1),
          uiOutput("ch5_cas_threshold_note"),
          lc_feedback(type = "info",
            tags$strong("Co pokazuje widget?"),
            tags$ul(
              tags$li("Najpierw mamy zwykły wynik punktowy z czytania."),
              tags$li("Próg zamienia wynik na zmienną: zdał / nie zdał."),
              tags$li("Regresja logistyczna modeluje prawdopodobieństwo klasy 'zdał'.")
            )
          )
        ),
        column(8,
          tags$h4("Krok 1: wynik ciągły i próg zaliczenia"),
          lc_plot_fullscreen("ch5_cas_continuous_plot", height = "280px"),
          tags$h4("Krok 2: model logistyczny daje prawdopodobieństwo klasy 1"),
          lc_plot_fullscreen("ch5_cas_logit_plot", height = "310px"),
          uiOutput("ch5_cas_model_table"),
          uiOutput("ch5_cas_model_metrics")
        )
      )
    ),

    lc_feedback(
      type = "info",
      tags$strong("Dwa różne progi:"),
      " próg tworzący Y=0/1 definiuje zdarzenie przed dopasowaniem modelu.",
      " Próg klasyfikacji, np. p ≥ 0,5, zamienia przewidywane prawdopodobieństwo",
      " na decyzję dopiero po dopasowaniu modelu."
    ),

    lc_h2("ch5-dlaczego", "Dlaczego nie regresja liniowa?"),

    tagList(
      p("Zacznijmy od eksperymentu: nałóżmy zwykłą regresję liniową na
        dane binarne (zdał = 1, nie zdał = 0) i zobaczmy, co się stanie.")
    ),

    figure_panel(
      label = "Ryc. 5.1", title = "Liniowy vs logistyczny na danych binarnych",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Stały przykład: czy student zdaje egzamin w zależności od godzin nauki?
                    Dane mają wyraźne przejście od małych do dużych prawdopodobieństw.")
        ),
        column(8,
          zoom_plot_ui("ch5_lin_log_plot", height = "300px"),
          uiOutput("ch5_lin_log_stats")
        )
      )
    ),

    inline_callout(label = "Wniosek", color = "uwaga",
      "Linia liniowa wychodzi poza [0, 1] — przy małej liczbie godzin daje
       prawdopodobieństwa ujemne, przy dużej powyżej 100%. To nie są
       prawdopodobieństwa, to absurd. Potrzebujemy modelu, który z definicji
       zwraca wartości w przedziale [0, 1]."
    ),

    tagList(
      p("Rozwiązaniem jest regresja logistyczna.
        Zamiast modelować Y bezpośrednio, modelujemy prawdopodobieństwo sukcesu:"),
      lc_formula_box(
        withMathJax(helpText(
          "$$P(Y=1) = \\frac{1}{1 + e^{-(\\beta_0 + \\beta_1 X_1 + \\ldots + \\beta_k X_k)}}$$"
        )),
        p("Funkcja logistyczna (sigmoida) zamyka wynik w [0, 1] —
          niezależnie od tego, jak duże albo małe są X-y.")
      )
    ),

    lc_h2("ch5-krzywa", "Krzywa logistyczna"),

    figure_panel(
      label = "Ryc. 5.2", title = "Sigmoida w akcji",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch5_b0", "β₀ (intercept):",
                      min = -10, max = 10, value = -4, step = 0.5),
          sliderInput("ch5_b1", "β₁ (slope):",
                      min = -3, max = 3, value = 0.2, step = 0.05),
          hr(),
          div(class = "preset-buttons",
            actionButton("ch5_preset_steep", "Stromy",
                         class = "lc-btn-outline"),
            actionButton("ch5_preset_flat", "Płaski",
                         class = "lc-btn-secondary-outline"),
            actionButton("ch5_preset_neg", "Odwrotny",
                         class = "lc-btn-danger-outline")
          )
        ),
        column(8,
          zoom_plot_ui("ch5_sigmoid_plot", height = "350px")
        )
      )
    ),

    lc_h2("ch5-model-dane", "Model logistyczny na danych"),

    tagList(
      p("Scenariusz: czy student zda egzamin? Predyktory: godziny nauki
        i średnia ocen.")
    ),

    figure_panel(
      label = "Ryc. 5.4", title = "Predykcja zdania egzaminu",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch5_n", "n:", min = 50, max = 300, value = 150, step = 25),
          selectInput("ch5_predictor", "Prezentowany predyktor:",
            choices = c(
              "Godziny nauki" = "godziny_nauki",
              "Średnia ocen"  = "srednia_ocen"
            ),
            selected = "godziny_nauki"
          ),
          actionButton("ch5_fit", "Dopasuj model",
                       class = "lc-btn-primary", width = "100%"),
          hr(),
          h5("Predykcja dla nowego studenta:"),
          numericInput("ch5_pred_hours", "Godziny nauki:", value = 20, min = 0, max = 40),
          numericInput("ch5_pred_gpa", "Średnia ocen:", value = 3.5, min = 2, max = 5, step = 0.1),
          uiOutput("ch5_prediction")
        ),
        column(8,
          zoom_plot_ui("ch5_logit_plot", height = "350px"),
          uiOutput("ch5_model_summary")
        )
      )
    ),

    lc_h2("ch5-iloraz-szans", "Interpretacja: iloraz szans"),

    tagList(
      p("W regresji logistycznej współczynniki interpretujemy przez iloraz szans (odds ratio):"),
      lc_formula_box(
        withMathJax(helpText("$$OR = e^{\\beta_j}$$")),
        p("OR = 1.5 oznacza: wzrost X o 1 zwiększa szanse sukcesu
          1.5-krotnie.")
      )
    ),

    figure_panel(
      label = "Ryc. 5.5", title = "Odds ratio",
      full_width = TRUE,
      helpText("Używa modelu dopasowanego powyżej."),
      uiOutput("ch5_odds_ratios")
    ),

    figure_panel(
      label = "Ryc. 5.6", title = "Próg klasyfikacji i macierz pomyłek",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Używa modelu dopasowanego w Ryc. 5.4."),
          sliderInput("ch5_threshold", "Próg decyzji:",
                      min = 0.1, max = 0.9, value = 0.5, step = 0.05)
        ),
        column(8,
          zoom_plot_ui("ch5_threshold_plot", height = "280px"),
          uiOutput("ch5_threshold_info")
        )
      )
    ),

    inline_callout(label = "Ocena modelu", color = "wskazowka",
      "Nie używamy R² w sensie liniowym. Zamiast tego: AIC, BIC, oraz
       macierz pomyłek (confusion matrix) z dokładnością, czułością
       i swoistością."
    ),

    lc_h2("ch5-zalozenia", "Kiedy logistyczna może zawieść?"),

    tagList(
      p("Regresja logistyczna nie wymaga normalności Y ani reszt w sensie
        regresji liniowej, ale też nie jest magicznym guzikiem. Najważniejsze
        warunki dotyczą konstrukcji danych i stabilności modelu:"),
      tags$table(class = "lc-table lc-table-bordered lc-table-striped",
        style = "font-size: 14px;",
        tags$thead(
          tags$tr(tags$th("Warunek"), tags$th("Co oznacza w praktyce"))
        ),
        tags$tbody(
          tags$tr(
            tags$td(tags$strong("Y jest binarne")),
            tags$td("modelujemy zdarzenie 0/1: zdał/nie zdał, kupił/nie kupił")
          ),
          tags$tr(
            tags$td(tags$strong("Niezależne obserwacje")),
            tags$td("ten sam student, klient lub zakład nie powinien pojawiać się wiele razy bez modelu z powtórzeniami")
          ),
          tags$tr(
            tags$td(tags$strong("Liniowość logitu")),
            tags$td("dla predyktorów ilościowych zależność ma być mniej więcej liniowa na skali log-odds")
          ),
          tags$tr(
            tags$td(tags$strong("Brak silnej współliniowości")),
            tags$td("tak jak w regresji wielorakiej: predyktory nie powinny powtarzać tej samej informacji")
          ),
          tags$tr(
            tags$td(tags$strong("Dość zdarzeń")),
            tags$td("przy bardzo małej liczbie sukcesów lub porażek współczynniki są niestabilne")
          ),
          tags$tr(
            tags$td(tags$strong("Brak separacji")),
            tags$td("jeśli jeden predyktor idealnie oddziela 0 od 1, klasyczne estymaty mogą uciekać do nieskończoności")
          )
        )
      ),
      p("Jeśli któryś z tych punktów jest problemem, zwykle lepsza jest
        prostsza specyfikacja, więcej danych, regularyzacja albo specjalne
        warianty regresji logistycznej, np. regresja Firtha przy separacji.")
    ),

    lc_chapter_next(
      num       = "06",
      title     = "Ściąga",
      lead      = "podsumowanie wzorów i zasad",
      target_id = "ch-sciaga"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch5_server <- function(input, output, session) {

  ch5_fmt <- function(x, digits = 3) {
    ifelse(is.na(x), "", formatC(x, digits = digits, format = "f"))
  }

  ch5_p <- function(x) {
    ifelse(is.na(x), "", ifelse(x < 0.001, "< 0.001", ch5_fmt(x, 3)))
  }

  ch5_cas_data <- reactive({
    df <- .cas_data
    y_cut <- input$ch5_cas_y_cut
    if (is.null(y_cut)) y_cut <- median(df$read, na.rm = TRUE)
    df$zdal_read <- as.integer(df$read >= y_cut)
    df
  })

  ch5_cas_model <- reactive({
    glm(zdal_read ~ income + lunch + english,
        data = ch5_cas_data(), family = binomial)
  })

  output$ch5_cas_threshold_note <- renderUI({
    lc_feedback(type = "ok",
      p("Y = 1, czyli 'zdał', oznacza wynik czytania od ",
        tags$strong(ch5_fmt(input$ch5_cas_y_cut, 0)),
        " pkt. Zmiana tego progu zmienia definicję zmiennej zależnej
        i przelicza współczynniki.")
    )
  })

  output$ch5_cas_continuous_plot <- renderPlot({
    df <- ch5_cas_data()

    ggplot(df, aes(income, read, color = factor(zdal_read))) +
      geom_point(alpha = 0.62, size = 2) +
      geom_hline(yintercept = input$ch5_cas_y_cut,
                 color = upwr_accent, linewidth = 1.05, linetype = "dashed") +
      annotate(
        "label",
        x = min(df$income, na.rm = TRUE),
        y = input$ch5_cas_y_cut,
        hjust = 0,
        vjust = -0.45,
        label = paste0("próg zaliczenia: ", input$ch5_cas_y_cut, " pkt"),
        color = upwr_accent,
        fill = "white",
        linewidth = 0
      ) +
      scale_color_manual(
        values = c("0" = unname(upwr_cat["grafit"]), "1" = unname(upwr_cat["szalwia"])),
        labels = c("0" = "nie zdał", "1" = "zdał"),
        name = "Klasa"
      ) +
      labs(
        x = "Dochód okręgu (tys. USD)",
        y = "Wynik z czytania",
        caption = "To jeszcze nie jest regresja logistyczna. To oryginalny wynik i próg, który tworzy później zmienną 0/1."
      ) +
      theme_upwr()
  })

  output$ch5_cas_logit_plot <- renderPlot({
    df <- ch5_cas_data()
    mod <- ch5_cas_model()
    df$prob <- fitted(mod)

    ggplot(df, aes(income, prob, color = factor(zdal_read))) +
      geom_point(alpha = 0.7) +
      scale_color_manual(
        values = c("0" = unname(upwr_cat["grafit"]), "1" = unname(upwr_cat["szalwia"])),
        labels = c("0" = "nie zdał", "1" = "zdał"),
        name = "Klasa"
      ) +
      labs(x = "Dochód okręgu (tys. USD)", y = "Prawdopodobieństwo zdania") +
      theme_upwr()
  })

  output$ch5_cas_model_table <- renderUI({
    tb <- broom::tidy(ch5_cas_model(), exponentiate = TRUE, conf.int = TRUE)
    labels <- c(
      "(Intercept)" = "Stała (szanse wyjściowe, nie OR)",
      "income" = "Dochód okręgu (tys. USD)",
      "lunch" = "Lunch subsydiowany (%)",
      "english" = "Angielski jako drugi język (%)"
    )
    rows <- lapply(seq_len(nrow(tb)), function(i) {
      term <- tb$term[i]
      tags$tr(
        tags$td(if (!is.na(labels[term])) unname(labels[term]) else term),
        tags$td(ch5_fmt(tb$estimate[i], 3)),
        tags$td(ch5_fmt(tb$std.error[i], 3)),
        tags$td(ch5_p(tb$p.value[i])),
        tags$td(ifelse(tb$p.value[i] < 0.05, "tak", "nie"))
      )
    })

    tags$table(class = "lc-table lc-table-bordered lc-table-striped",
      tags$thead(tags$tr(
        tags$th("Zmienna"), tags$th("Iloraz szans (OR)"),
        tags$th("Błąd stand."), tags$th("p-value"), tags$th("p < 0.05?")
      )),
      tags$tbody(rows)
    )
  })

  output$ch5_cas_model_metrics <- renderUI({
    model <- ch5_cas_model()
    p_hat <- fitted(model)
    y <- model$y
    rmse <- sqrt(mean((y - p_hat)^2))

    lc_stat_grid(
      lc_stat_box("AIC", ch5_fmt(AIC(model), 1), color = unname(upwr_cat["wrzos"])),
      lc_stat_box("BIC", ch5_fmt(BIC(model), 1), color = upwr_secondary),
      lc_stat_box("RMSE prawdop.", ch5_fmt(rmse, 3),
                  caption = "dla przewidywanych prawdopodobieństw",
                  color = unname(upwr_cat["bursztyn"])),
      columns = 3
    )
  })

  # --- Widget: Liniowy vs logistyczny (przeniesiony z ch4) ---
  ch5_lin_log_data <- reactive({
    x <- seq(0, 40, length.out = 180)
    p <- 1 / (1 + exp(-(-7 + 0.35 * x)))
    u <- ((seq_along(x) * 37) %% 100) / 100
    y <- as.integer(u < p)
    y_jitter <- ifelse(y == 1, 1, 0) + sin(seq_along(x) * 1.7) * 0.025

    data.frame(
      godziny_nauki = x,
      zdal_num = y,
      zdal_plot = y_jitter,
      zdal = factor(y, levels = c(0, 1), labels = c("Nie", "Tak"))
    )
  })

  zoom_plot_server("ch5_lin_log_plot", reactive({
    df <- ch5_lin_log_data()

    ggplot(df, aes(x = godziny_nauki, y = zdal_num)) +
      geom_point(aes(y = zdal_plot), alpha = 0.34, color = upwr_secondary, size = 1.6) +
      geom_smooth(method = "lm", se = FALSE, color = unname(upwr_cat["niebo"]),
                  linewidth = 1, linetype = "dashed") +
      geom_smooth(method = "glm", method.args = list(family = "binomial"),
                  se = FALSE, color = unname(upwr_cat["wrzos"]), linewidth = 1.2) +
      geom_hline(yintercept = c(0, 1), linetype = "dotted", color = upwr_rule) +
      annotate("text", x = 13, y = 0.28, label = "Logistyczny", color = unname(upwr_cat["wrzos"]),
               fontface = "bold") +
      annotate("text", x = 34, y = 1.12, label = "Liniowy", color = unname(upwr_cat["niebo"]),
               fontface = "bold") +
      labs(
           x = "Godziny nauki", y = "P(zdanie)") +
      coord_cartesian(ylim = c(-0.2, 1.2)) +
      theme_upwr()
  }))

  output$ch5_lin_log_stats <- renderUI({
    df <- ch5_lin_log_data()

    lin <- lm(zdal_num ~ godziny_nauki, data = df)
    log <- glm(zdal_num ~ godziny_nauki, data = df, family = binomial)

    lin_pred <- ifelse(fitted(lin) >= 0.5, 1, 0)
    log_pred <- ifelse(fitted(log) >= 0.5, 1, 0)
    acc_lin <- mean(lin_pred == df$zdal_num) * 100
    acc_log <- mean(log_pred == df$zdal_num) * 100

    outside <- mean(fitted(lin) < 0 | fitted(lin) > 1) * 100

    lc_stat_grid(columns = 3,
      lc_stat_box("Liniowy", round(acc_lin, 1), "%", color = unname(upwr_cat["niebo"])),
      lc_stat_box("Logistyczny", round(acc_log, 1), "%", color = unname(upwr_cat["wrzos"])),
      lc_stat_box("Liniowy poza [0,1]", round(outside, 1), "%", color = unname(upwr_cat["terakota"]))
    )
  })

  # --- Widget 1: Sigmoida ---
  observeEvent(input$ch5_preset_steep, {
    updateSliderInput(session, "ch5_b0", value = -5)
    updateSliderInput(session, "ch5_b1", value = 0.5)
  })
  observeEvent(input$ch5_preset_flat, {
    updateSliderInput(session, "ch5_b0", value = -1)
    updateSliderInput(session, "ch5_b1", value = 0.05)
  })
  observeEvent(input$ch5_preset_neg, {
    updateSliderInput(session, "ch5_b0", value = 5)
    updateSliderInput(session, "ch5_b1", value = -0.3)
  })

  zoom_plot_server("ch5_sigmoid_plot", reactive({
    b0 <- input$ch5_b0
    b1 <- input$ch5_b1
    x <- seq(-5, 45, length.out = 500)
    p <- 1 / (1 + exp(-(b0 + b1 * x)))

    ggplot(data.frame(x = x, p = p), aes(x = x, y = p)) +
      geom_line(color = unname(upwr_cat["wrzos"]), linewidth = 1.5) +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = upwr_secondary, alpha = 0.5) +
      labs(
           x = "X", y = "P(Y = 1)") +
      ylim(0, 1) +
      theme_upwr()
  }))

  # --- Widget 2: Model logistyczny ---
  ch5_data <- reactiveVal(NULL)
  ch5_model <- reactiveVal(NULL)

  observeEvent(input$ch5_fit, {
    df <- generate_logistic_data(input$ch5_n)
    ch5_data(df)
    model <- glm(zdal_num ~ godziny_nauki + srednia_ocen,
                 data = df, family = binomial)
    ch5_model(model)
  })

  zoom_plot_server("ch5_logit_plot", reactive({
    df <- ch5_data()
    model <- ch5_model()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Dopasuj model'",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      pred_var <- input$ch5_predictor
      pred_label <- if (pred_var == "godziny_nauki") "Godziny nauki" else "Średnia ocen"

      # Predykcja dla wykresu (trzymajac drugi predyktor na sredniej)
      other_var <- setdiff(c("godziny_nauki", "srednia_ocen"), pred_var)
      other_mean <- mean(df[[other_var]])

      x_seq <- seq(min(df[[pred_var]]), max(df[[pred_var]]), length.out = 200)
      newdata <- data.frame(x_seq, other_mean)
      names(newdata) <- c(pred_var, other_var)
      newdata$pred_prob <- predict(model, newdata, type = "response")

      ggplot() +
        geom_jitter(data = df, aes(x = .data[[pred_var]], y = .data[["zdal_num"]]),
                    height = 0.03, alpha = 0.3, color = upwr_secondary) +
        geom_line(data = newdata, aes(x = .data[[pred_var]], y = .data[["pred_prob"]]),
                  color = unname(upwr_cat["wrzos"]), linewidth = 1.5) +
        geom_hline(yintercept = 0.5, linetype = "dashed", color = unname(upwr_cat["bursztyn"])) +
        labs(
             x = pred_label, y = "P(zdanie egzaminu)") +
        ylim(-0.05, 1.05) +
        theme_upwr()
    }
  }))

  output$ch5_model_summary <- renderUI({
    model <- ch5_model()
    if (is.null(model)) return(NULL)

    g <- broom::glance(model)
    coefs <- broom::tidy(model)

    # Confusion matrix
    df <- ch5_data()
    pred_class <- ifelse(predict(model, type = "response") >= 0.5, 1, 0)
    accuracy <- mean(pred_class == df$zdal_num) * 100

    tagList(
      lc_stat_box("AIC", round(g$AIC, 1), color = unname(upwr_cat["wrzos"])),
      lc_stat_box("BIC", round(g$BIC, 1), color = upwr_secondary),
      lc_stat_box("Dokładność", round(accuracy, 1), "%", color = unname(upwr_cat["szalwia"]))
    )
  })

  output$ch5_prediction <- renderUI({
    model <- ch5_model()
    if (is.null(model)) return(NULL)

    newdata <- data.frame(
      godziny_nauki = input$ch5_pred_hours,
      srednia_ocen = input$ch5_pred_gpa
    )
    prob <- predict(model, newdata, type = "response")

    color <- if (prob >= 0.5) unname(upwr_cat["szalwia"]) else unname(upwr_cat["terakota"])
    decision <- if (prob >= 0.5) "Prawdopodobnie zda" else "Raczej nie zda"

    lc_stat_box("P(zdanie)", round(prob, 3), caption = decision, color = color)
  })

  # --- Widget 3: Odds ratios ---
  output$ch5_odds_ratios <- renderUI({
    model <- ch5_model()
    if (is.null(model)) {
      return(lc_feedback(type = "warning", "Najpierw dopasuj model."))
    }

    coefs <- broom::tidy(model, conf.int = TRUE)
    coefs$or <- exp(coefs$estimate)
    coefs$or_low <- exp(coefs$conf.low)
    coefs$or_high <- exp(coefs$conf.high)

    labels_pl <- c(
      "(Intercept)" = "Wyraz wolny",
      "godziny_nauki" = "Godziny nauki (+1h)",
      "srednia_ocen" = "Średnia ocen (+1 pkt)"
    )

    coefs$term_pl <- ifelse(coefs$term %in% names(labels_pl),
                             labels_pl[coefs$term], coefs$term)

    rows <- lapply(2:nrow(coefs), function(i) {  # pomijamy intercept
      tags$tr(
        tags$td(coefs$term_pl[i]),
        tags$td(round(coefs$estimate[i], 3)),
        tags$td(tags$strong(round(coefs$or[i], 3))),
        tags$td(paste0("[", round(coefs$or_low[i], 3), " ; ",
                        round(coefs$or_high[i], 3), "]")),
        tags$td(format_p_value(coefs$p.value[i]))
      )
    })

    tagList(
      tags$table(class = "lc-table lc-table-bordered",
        style = "font-size: 14px;",
        tags$thead(
          tags$tr(tags$th("Zmienna"), tags$th("β"), tags$th("OR"),
                  tags$th("95% CI (OR)"), tags$th("p"))
        ),
        tags$tbody(rows)
      ),
      lc_feedback(type = "info",
        p(tags$strong("Interpretacja OR:"),
          " OR > 1 oznacza, że wzrost predyktora o 1 zwiększa szanse sukcesu.
            OR < 1 — zmniejsza. OR = 1 — brak wpływu.")
      )
    )
  })

  # --- Widget: prog klasyfikacji ---
  zoom_plot_server("ch5_threshold_plot", reactive({
    model <- ch5_model()
    df <- ch5_data()
    if (is.null(model) || is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Najpierw dopasuj model w Ryc. 5.4",
                 size = 5.5, color = upwr_reference) +
        theme_void()
    } else {
      probs <- predict(model, type = "response")
      pred <- ifelse(probs >= input$ch5_threshold, 1, 0)
      cm <- as.data.frame(table(
        Rzeczywiste = factor(df$zdal_num, levels = c(0, 1), labels = c("Nie", "Tak")),
        Predykcja = factor(pred, levels = c(0, 1), labels = c("Nie", "Tak"))
      ))
      ggplot(cm, aes(x = Predykcja, y = Rzeczywiste, fill = Freq)) +
        geom_tile(color = "white", linewidth = 1) +
        geom_text(aes(label = Freq), size = 7, fontface = "bold", color = "white") +
        scale_fill_gradient(low = unname(upwr_cat["niebo"]), high = upwr_secondary) +
        labs(x = "Predykcja modelu", y = "Rzeczywistość") +
        theme_upwr() +
        theme(legend.position = "none")
    }
  }))

  output$ch5_threshold_info <- renderUI({
    model <- ch5_model()
    df <- ch5_data()
    if (is.null(model) || is.null(df)) return(NULL)
    probs <- predict(model, type = "response")
    pred <- ifelse(probs >= input$ch5_threshold, 1, 0)
    tp <- sum(pred == 1 & df$zdal_num == 1)
    tn <- sum(pred == 0 & df$zdal_num == 0)
    fp <- sum(pred == 1 & df$zdal_num == 0)
    fn <- sum(pred == 0 & df$zdal_num == 1)
    accuracy <- (tp + tn) / length(pred)
    sensitivity <- ifelse(tp + fn == 0, NA, tp / (tp + fn))
    specificity <- ifelse(tn + fp == 0, NA, tn / (tn + fp))
    tagList(
      lc_stat_box("Accuracy", paste0(round(accuracy * 100, 1), "%"), color = unname(upwr_cat["szalwia"])),
      lc_stat_box("Czułość", paste0(round(sensitivity * 100, 1), "%"), caption = "wykrywa Tak", color = unname(upwr_cat["niebo"])),
      lc_stat_box("Swoistość", paste0(round(specificity * 100, 1), "%"), caption = "wykrywa Nie", color = unname(upwr_cat["bursztyn"])),
      lc_feedback(type = "warning", p("Obniżenie progu zwykle zwiększa czułość, ale może obniżyć swoistość."))
    )
  })
}
