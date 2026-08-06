# ============================================================================
# CHAPTER 0: Regresja light - crash course 45 minut
# ============================================================================

.light_fmt <- function(x, digits = 3) {
  ifelse(is.na(x), "", formatC(x, digits = digits, format = "f"))
}

.light_p <- function(x) {
  ifelse(is.na(x), "", ifelse(x < 0.001, "< 0.001", .light_fmt(x, 3)))
}

.light_coef_table <- function(model, exponentiate = FALSE) {
  tb <- broom::tidy(model, conf.int = exponentiate, exponentiate = exponentiate)
  label <- if (exponentiate) "OR / efekt" else "Współczynnik"
  term_labels <- c("(Intercept)" = "Stała", .cas_labels)
  rows <- lapply(seq_len(nrow(tb)), function(i) {
    term <- tb$term[i]
    term <- if (!is.na(term_labels[term])) unname(term_labels[term]) else term
    tags$tr(
      tags$td(term),
      tags$td(.light_fmt(tb$estimate[i], 3)),
      tags$td(.light_fmt(tb$std.error[i], 3)),
      tags$td(.light_p(tb$p.value[i])),
      tags$td(ifelse(tb$p.value[i] < 0.05, "tak", "nie"))
    )
  })
  tags$table(
    class = "lc-table lc-table-bordered lc-table-striped",
    tags$thead(tags$tr(
      tags$th("Zmienna"), tags$th(label), tags$th("Błąd stand."),
      tags$th("p-value"), tags$th("p < 0.05?")
    )),
    tags$tbody(rows)
  )
}

.light_linear_metrics <- function(models) {
  rows <- lapply(names(models), function(nm) {
    mod <- models[[nm]]
    g <- broom::glance(mod)
    rmse <- sqrt(mean(residuals(mod)^2))
    tags$tr(
      tags$td(nm),
      tags$td(length(coef(mod)) - 1),
      tags$td(.light_fmt(g$r.squared, 3)),
      tags$td(.light_fmt(g$adj.r.squared, 3)),
      tags$td(.light_fmt(AIC(mod), 1)),
      tags$td(.light_fmt(BIC(mod), 1)),
      tags$td(.light_fmt(rmse, 2))
    )
  })
  tags$table(
    class = "lc-table lc-table-bordered lc-table-striped",
    tags$thead(tags$tr(
      tags$th("Model"), tags$th("Predyktory"), tags$th("R2"),
      tags$th("R2 adj."), tags$th("AIC"), tags$th("BIC"), tags$th("RMSE")
    )),
    tags$tbody(rows)
  )
}

.light_logistic_metrics <- function(model) {
  p_hat <- fitted(model)
  y <- model$y
  rmse <- sqrt(mean((y - p_hat)^2))
  data.frame(
    AIC = AIC(model),
    BIC = BIC(model),
    RMSE_prawdopodobienstw = rmse,
    stringsAsFactors = FALSE
  )
}

ch0_light_ui <- list(
  id    = "ch-light",
  num   = "45",
  title = "Regresja light",
  duration = "45 min",
  content = tagList(

    lc_chapter_hero(
      kicker = "Regresja · wersja light",
      num    = "45",
      title  = "Regresja w 45 minut.",
      lead   = "Crash course narzędziowy: dopasować model, odczytać wynik,
                porównać kilka modeli i rozpoznać, kiedy Y jest binarne."
    ),

    lc_h2("light-plan", "Plan na zajęcia"),

    tags$table(class = "lc-table lc-table-bordered",
      tags$thead(tags$tr(tags$th("Czas"), tags$th("Blok"), tags$th("Efekt po bloku"))),
      tags$tbody(
        tags$tr(tags$td("0-6 min"), tags$td("Model liniowy prosty"),
                tags$td("student wie, gdzie wskazać Y i X w jamovi")),
        tags$tr(tags$td("6-16 min"), tags$td("Czytanie outputu"),
                tags$td("umie powiedzieć: kierunek, wielkość, p-value, R2, RMSE")),
        tags$tr(tags$td("16-27 min"), tags$td("Regresja wieloraka"),
                tags$td("interpretuje efekt ceteris paribus")),
        tags$tr(tags$td("27-35 min"), tags$td("Porównywanie modeli"),
                tags$td("porównuje R2, R2 adj., AIC, BIC i RMSE")),
        tags$tr(tags$td("35-45 min"), tags$td("Logistyczna dla 0/1"),
                tags$td("rozumie praktyczną różnicę: prawdopodobieństwo zamiast punktów Y"))
      )
    ),

    inline_callout(label = "Cel light", color = "ok", open = TRUE,
      "Po tych zajęciach student nie musi znać wyprowadzenia OLS ani logitu.
       Ma umieć dobrać typ regresji, uruchomić model, przeczytać tabelę
       i uczciwie porównać kilka kandydatów."
    ),

    lc_h2("light-lm", "1. Model liniowy prosty"),

    p("Zaczynamy od pytania: czy dochód okręgu szkolnego pomaga przewidywać
      wynik z czytania? To jest klasyczny przypadek: Y jest ilościowe,
      X jest ilościowe, więc używamy regresji liniowej."),

    figure_panel(
      label = "Demo 1", title = "Pierwszy model: wynik czytania i dochód",
      full_width = TRUE,
      fluidRow(
        column(4,
          lc_feedback(type = "info",
            tags$strong("W jamovi pokazujesz live:"),
            tags$ul(
              tags$li("Regression → Linear Regression."),
              tags$li("Dependent Variable: wynik z czytania."),
              tags$li("Covariates: dochód okręgu."),
              tags$li("Włącz: estimates, model fit, residual plots / diagnostics.")
            )
          ),
          lc_feedback(type = "info",
            p("Najważniejsze zdanie: współczynnik przy dochodzie mówi,
              o ile średnio zmienia się wynik czytania, gdy dochód okręgu
              rośnie o 1 tys. USD.")
          )
        ),
        column(8,
          lc_plot_fullscreen("ch0_lm_plot", height = "330px"),
          uiOutput("ch0_lm_table"),
          uiOutput("ch0_lm_sentence")
        )
      )
    ),

    lc_h2("light-output", "2. Jak czytać output"),

    tags$table(class = "lc-table lc-table-bordered",
      tags$thead(tags$tr(tags$th("Element"), tags$th("Pytanie"), tags$th("Jak mówić po ludzku?"))),
      tags$tbody(
        tags$tr(tags$td("estimate"), tags$td("jak duży jest efekt?"),
                tags$td("wzrost X o 1 jednostkę wiąże się ze zmianą Y o ...")),
        tags$tr(tags$td("p-value"), tags$td("czy efekt jest wyraźny statystycznie?"),
                tags$td("małe p-value to sygnał, że efekt nie wygląda na przypadek")),
        tags$tr(tags$td("R2"), tags$td("ile zmienności Y łapie model?"),
                tags$td("większe lepiej, ale R2 zawsze rośnie po dodaniu zmiennych")),
        tags$tr(tags$td("RMSE"), tags$td("jak duże są typowe błędy?"),
                tags$td("w jednostkach Y; dla czytania to punkty testu"))
      )
    ),

    lc_h2("light-metrics-intuition", "2a. Metryki intuicyjnie"),

    p("Zanim zaczniemy porównywać modele, zatrzymujemy się na trzech
      liczbach z outputu. Tu używamy gotowych mini-widgetów z pełnej wersji:
      R2 jako część zmienności złapana przez model, RMSE jako typowy błąd
      w punktach i AIC jako porównanie modeli z karą za złożoność."),

    figure_panel(
      label = "Demo 1b", title = "R2: ile zmienności łapie model",
      full_width = TRUE,
      helpText("Im ciaśniej punkty leżą przy linii, tym większa część zmienności Y jest wyjaśniona przez model."),
      zoom_plot_ui("ch2_r2_compare_plot", height = "340px")
    ),

    figure_panel(
      label = "Demo 1c", title = "RMSE: typowa pomyłka modelu",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("RMSE ma jednostki zmiennej Y. Dlatego zawsze pytamy:
                    czy taka pomyłka jest duża względem skali wyniku?"),
          selectInput("ch2_rmse_case", "Model:",
            choices = .ch2_rmse_choices,
            selected = "read_lunch"
          ),
          uiOutput("ch2_rmse_interpretation")
        ),
        column(8,
          zoom_plot_ui("ch2_rmse_plot", height = "320px"),
          uiOutput("ch2_rmse_stats")
        )
      )
    ),

    figure_panel(
      label = "Demo 1d", title = "AIC: dopasowanie kontra złożoność",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("AIC i BIC nie są oceną jednego modelu w izolacji. Porównujemy kilka kandydatów: niżej znaczy lepiej."),
          lc_feedback(type = "warning",
            p("Zwróć uwagę: po dodaniu zmiennej 'komputery' R2 minimalnie rośnie,
              ale AIC i BIC idą w górę. To znak, że dodatkowa złożoność nie
              opłaca się w tym porównaniu.")
          )
        ),
        column(8,
          uiOutput("ch0_aic_table")
        )
      )
    ),

    lc_h2("light-multi", "3. Model wieloraki"),

    p("Teraz dokładamy kilka X-ów. To już nie jest tylko 'dochód a wynik',
      ale pytanie: co zostaje z efektu dochodu, kiedy kontrolujemy lunch,
      odsetek uczniów uczących się angielskiego i liczebność klas?"),

    figure_panel(
      label = "Demo 2", title = "Regresja wieloraka: dobierz predyktory",
      full_width = TRUE,
      fluidRow(
        column(4,
          checkboxGroupInput("ch0_multi_x", "Predyktory w modelu:",
            choices = c(
              "Dochód okręgu" = "income",
              "Lunch subsydiowany (%)" = "lunch",
              "Angielski jako drugi język (%)" = "english",
              "Uczniowie / nauczyciel" = "student_teacher_ratio",
              "Wydatki na ucznia" = "expenditure"
            ),
            selected = c("income", "lunch", "english")
          ),
          lc_feedback(type = "info",
            tags$strong("W jamovi pokazujesz live:"),
            tags$ul(
              tags$li("Dependent Variable: wynik z czytania."),
              tags$li("Covariates: zaznaczone predyktory."),
              tags$li("Porównaj, co dzieje się ze współczynnikiem dochodu po dodaniu kontroli.")
            )
          )
        ),
        column(8,
          uiOutput("ch0_multi_spec"),
          uiOutput("ch0_multi_table"),
          uiOutput("ch0_multi_sentence")
        )
      )
    ),

    inline_callout(label = "Zdanie klucz", color = "wskazowka", open = TRUE,
      "W regresji wielorakiej współczynnik znaczy: zmiana Y przy wzroście
       tej jednej zmiennej o 1 jednostkę, przy stałych pozostałych zmiennych."
    ),

    lc_h2("light-compare", "4. Porównywanie modeli"),

    p("W praktyce rzadko mamy jeden oczywisty model. Porównujemy kilka
      kandydatów: prosty, średni i bogatszy. Tu używamy R2, adjusted R2,
      AIC, BIC i RMSE."),

    figure_panel(
      label = "Demo 3", title = "R2, AIC, BIC i RMSE w jednej tabeli",
      full_width = TRUE,
      fluidRow(
        column(4,
          tags$ul(
            tags$li("R2: wyżej lepiej, ale zawsze rośnie przy dodawaniu X."),
            tags$li("R2 adj.: wyżej lepiej, karze za dodatkowe X."),
            tags$li("AIC/BIC: niżej lepiej."),
            tags$li("RMSE: niżej lepiej, w jednostkach Y.")
          ),
          lc_feedback(type = "warning",
            p("Do porównań modeli o różnej liczbie predyktorów nie wybieraj
              automatycznie najwyższego R2. Popatrz na R2 adj., AIC/BIC i RMSE.")
          )
        ),
        column(8,
          uiOutput("ch0_compare_table"),
          lc_plot_fullscreen("ch0_compare_plot", height = "300px")
        )
      )
    ),

    lc_h2("light-logistic", "5. Logistyczna: gdy Y jest 0/1"),

    p("Jeśli wynik jest dychotomiczny, np. szkoła jest powyżej mediany
      wyniku czytania albo nie, zwykła regresja liniowa przestaje być
      naturalna. Regresja logistyczna zwraca praktycznie coś innego:
      prawdopodobieństwo klasy 1."),

    figure_panel(
      label = "Demo 4", title = "Analog dla zmiennej dychotomicznej",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch0_log_y_cut", "Próg zaliczenia: zdał od:",
                      min = 630, max = 680, value = 656, step = 1),
          uiOutput("ch0_log_threshold_note"),
          lc_feedback(type = "info",
            tags$strong("W jamovi pokazujesz live:"),
            tags$ul(
              tags$li("Regression → Logistic Regression."),
              tags$li("Dependent Variable: zdał/nie zdał, utworzone z wyniku czytania."),
              tags$li("Covariates: dochód, lunch, angielski jako drugi język."),
              tags$li("Włącz: model fit, współczynniki i predicted probabilities, jeśli dostępne.")
            )
          )
        ),
        column(8,
          tags$h4("Krok 1: wynik ciągły i próg zaliczenia"),
          lc_plot_fullscreen("ch0_log_continuous_plot", height = "280px"),
          tags$h4("Krok 2: model logistyczny daje prawdopodobieństwo klasy 1"),
          lc_plot_fullscreen("ch0_log_plot", height = "310px"),
          lc_feedback(type = "info",
            p(tags$strong("Tu próg oznacza definicję zmiennej 0/1."),
              "Zmieniając próg zaliczenia, zmieniamy pytanie badawcze:
              model nie przewiduje już liczby punktów, tylko prawdopodobieństwo
              uzyskania wyniku co najmniej na tym poziomie.")
          ),
          uiOutput("ch0_log_table"),
          tags$h4("Metryki modelu logistycznego"),
          uiOutput("ch0_log_metrics")
        )
      )
    ),

    inline_callout(label = "Praktyczna różnica", color = "ok", open = TRUE,
      tags$ul(
        tags$li("Liniowa: przewiduje wartość Y, np. 665 punktów."),
        tags$li("Logistyczna: przewiduje prawdopodobieństwo klasy 1, np. 0.72."),
        tags$li("Klasę 1 definiujemy progiem: np. wynik od 656 punktów oznacza 'zdał'.")
      )
    ),

    lc_h2("light-sciaga", "Ściąga na koniec"),

    tags$table(class = "lc-table lc-table-bordered lc-table-striped",
      tags$thead(tags$tr(tags$th("Sytuacja"), tags$th("Co wybieram w jamovi"), tags$th("Czytam przede wszystkim"))),
      tags$tbody(
        tags$tr(tags$td("Y ilościowa, jeden X"),
                tags$td("Linear Regression: Y jako dependent, X jako covariate/factor"),
                tags$td("estimate, p-value, R2, RMSE")),
        tags$tr(tags$td("Y ilościowa, wiele X"),
                tags$td("Linear Regression z kilkoma covariates/factors"),
                tags$td("efekty ceteris paribus, R2 adj., AIC/BIC, RMSE")),
        tags$tr(tags$td("Y binarna 0/1"),
                tags$td("Logistic Regression: Y binarna jako dependent"),
                tags$td("współczynniki, prawdopodobieństwa, AIC/BIC, RMSE"))
      )
    )
  )
)

ch0_light_server <- function(input, output, session) {

  cas <- .cas_data
  lm_simple <- lm(read ~ income, data = cas)

  output$ch0_lm_plot <- renderPlot({
    ggplot(cas, aes(income, read)) +
      geom_point(alpha = 0.65, color = upwr_cat["grafit"]) +
      geom_smooth(method = "lm", se = TRUE, color = upwr_accent, linewidth = 1) +
      labs(x = "Dochód okręgu (tys. USD)", y = "Wynik z czytania")
  })

  output$ch0_lm_table <- renderUI({
    .light_coef_table(lm_simple)
  })

  output$ch0_lm_sentence <- renderUI({
    tb <- broom::tidy(lm_simple)
    a <- tb$estimate[tb$term == "(Intercept)"]
    b <- tb$estimate[tb$term == "income"]
    pval <- tb$p.value[tb$term == "income"]
    g <- broom::glance(lm_simple)
    x_example <- 20
    y_hat <- as.numeric(predict(lm_simple, newdata = data.frame(income = x_example)))

    tagList(
      lc_feedback(type = "ok",
        p("Interpretacja: wzrost dochodu okręgu o 1 tys. USD wiąże się średnio
          ze zmianą wyniku czytania o ", tags$strong(.light_fmt(b, 2)),
          " pkt. R2 = ", tags$strong(.light_fmt(g$r.squared, 3)),
          ", p-value = ", tags$strong(.light_p(pval)), ".")
      ),
      lc_formula_box(
        tags$div(
          style = paste(
            "font-size: clamp(18px, 2vw, 26px);",
            "line-height: 1.35;",
            "font-weight: 700;",
            "white-space: nowrap;"
          ),
          HTML(paste0(
            "wynik = ",
            .light_fmt(a, 2), " + ",
            .light_fmt(b, 2), " &times; dochód"
          ))
        ),
        tags$div(
          style = "font-size: 15px; margin-top: 10px;",
          "Dochód wpisujemy w tysiącach USD, więc 20 oznacza 20 tys. USD."
        )
      ),
      lc_feedback(type = "info",
        p(tags$strong("Mini-zadanie: "),
          "jaki będzie średni przewidywany wynik, jeśli okręg ma dochód ",
          tags$strong("20 tys. USD"), "?"),
        p(.light_fmt(a, 2), " + ", .light_fmt(b, 2), " × ", x_example,
          " = ", tags$strong(.light_fmt(y_hat, 1)), " pkt.")
      )
    )
  })

  ch0_aic_data <- reactive({
    specs <- list(
      list(
        model = "M1: dochód",
        predictors = "dochód",
        formula = read ~ income
      ),
      list(
        model = "M2: + lunch",
        predictors = "dochód + lunch",
        formula = read ~ income + lunch
      ),
      list(
        model = "M3: + angielski",
        predictors = "dochód + lunch + angielski",
        formula = read ~ income + lunch + english
      ),
      list(
        model = "M4: + komputery",
        predictors = "dochód + lunch + angielski + komputery",
        formula = read ~ income + lunch + english + computer
      )
    )

    do.call(rbind, lapply(specs, function(spec) {
      model <- lm(spec$formula, data = cas)
      g <- broom::glance(model)
      data.frame(
        model = spec$model,
        predictors = spec$predictors,
        r_squared = g$r.squared,
        adj_r_squared = g$adj.r.squared,
        aic = AIC(model),
        bic = BIC(model),
        rmse = sqrt(mean(residuals(model)^2)),
        stringsAsFactors = FALSE
      )
    }))
  })

  output$ch0_aic_table <- renderUI({
    df <- ch0_aic_data()
    best_aic <- which.min(df$aic)
    rows <- lapply(seq_len(nrow(df)), function(i) {
      tags$tr(
        class = if (i == best_aic) "table-success" else NULL,
        tags$td(df$predictors[i]),
        tags$td(.light_fmt(df$r_squared[i], 3)),
        tags$td(.light_fmt(df$adj_r_squared[i], 3)),
        tags$td(.light_fmt(df$aic[i], 1)),
        tags$td(.light_fmt(df$bic[i], 1)),
        tags$td(.light_fmt(df$rmse[i], 2))
      )
    })

    tagList(
      tags$table(class = "lc-table lc-table-bordered lc-table-striped",
        tags$thead(
          tags$tr(tags$th("Predyktory"), tags$th("R2"), tags$th("R2 adj."),
                  tags$th("AIC"), tags$th("BIC"), tags$th("RMSE"))
        ),
        tags$tbody(rows)
      ),
      lc_feedback(type = "info",
        p("Najniższe AIC ma model M3. Model M4 ma odrobinę wyższe R2,
          ale AIC i BIC są gorsze, więc dodatkowy predyktor nie wnosi
          wystarczająco dużo.")
      )
    )
  })

  multi_model <- reactive({
    xs <- input$ch0_multi_x
    if (is.null(xs) || length(xs) == 0) xs <- "income"
    lm(as.formula(paste("read ~", paste(xs, collapse = " + "))), data = cas)
  })

  output$ch0_multi_spec <- renderUI({
    vars <- all.vars(formula(multi_model()))[-1]
    vars <- unname(.cas_labels[vars])
    tags$div(class = "lc-formula-box",
      p(tags$strong("Y: "), .cas_labels["read"]),
      p(tags$strong("Predyktory: "), paste(vars, collapse = ", "))
    )
  })

  output$ch0_multi_table <- renderUI({
    .light_coef_table(multi_model())
  })

  output$ch0_multi_sentence <- renderUI({
    mod <- multi_model()
    g <- broom::glance(mod)
    lc_feedback(type = "info",
      p("Ten model ma R2 adj. = ", tags$strong(.light_fmt(g$adj.r.squared, 3)),
        " i AIC = ", tags$strong(.light_fmt(AIC(mod), 1)),
        ". Każdy współczynnik czytamy przy stałych pozostałych predyktorach.")
    )
  })

  compare_models <- reactive({
    list(
      "M1: dochód" = lm(read ~ income, data = cas),
      "M2: dochód + lunch + angielski" = lm(read ~ income + lunch + english, data = cas),
      "M3: M2 + klasy + wydatki" =
        lm(read ~ income + lunch + english + student_teacher_ratio + expenditure, data = cas)
    )
  })

  output$ch0_compare_table <- renderUI({
    .light_linear_metrics(compare_models())
  })

  output$ch0_compare_plot <- renderPlot({
    mods <- compare_models()
    df <- do.call(rbind, lapply(names(mods), function(nm) {
      mod <- mods[[nm]]
      g <- broom::glance(mod)
      data.frame(
        model = nm,
        R2_adj = g$adj.r.squared,
        RMSE = sqrt(mean(residuals(mod)^2)),
        AIC = AIC(mod),
        BIC = BIC(mod),
        stringsAsFactors = FALSE
      )
    }))
    df$model <- factor(df$model, levels = df$model)
    ggplot(df, aes(model, R2_adj, fill = model)) +
      geom_col(width = 0.62, show.legend = FALSE) +
      coord_cartesian(ylim = c(max(0, min(df$R2_adj) - 0.05), max(df$R2_adj) + 0.03)) +
      scale_fill_manual(values = unname(upwr_cat[c("grafit", "bursztyn", "szalwia")])) +
      labs(x = NULL, y = "Adjusted R2") +
      theme(axis.text.x = element_text(angle = 15, hjust = 1))
  })

  logistic_data <- reactive({
    df <- cas
    y_cut <- input$ch0_log_y_cut
    if (is.null(y_cut)) y_cut <- median(df$read, na.rm = TRUE)
    df$wysoki_wynik <- as.integer(df$read >= y_cut)
    df
  })

  logistic_model <- reactive({
    glm(wysoki_wynik ~ income + lunch + english,
        data = logistic_data(), family = binomial)
  })

  output$ch0_log_plot <- renderPlot({
    df <- logistic_data()
    mod <- logistic_model()
    df$prob <- fitted(mod)
    ggplot(df, aes(income, prob, color = factor(wysoki_wynik))) +
      geom_point(alpha = 0.7) +
      scale_color_manual(
        values = c("0" = unname(upwr_cat["grafit"]), "1" = unname(upwr_cat["szalwia"])),
        labels = c("0" = "nie zdał", "1" = "zdał"),
        name = "Klasa"
      ) +
      labs(x = "Dochód okręgu (tys. USD)", y = "Prawdopodobieństwo zdania")
  })

  output$ch0_log_continuous_plot <- renderPlot({
    df <- logistic_data()

    ggplot(df, aes(income, read, color = factor(wysoki_wynik))) +
      geom_point(alpha = 0.62, size = 2) +
      geom_hline(yintercept = input$ch0_log_y_cut,
                 color = upwr_accent, linewidth = 1.05, linetype = "dashed") +
      annotate(
        "label",
        x = min(df$income, na.rm = TRUE),
        y = input$ch0_log_y_cut,
        hjust = 0,
        vjust = -0.45,
        label = paste0("próg zaliczenia: ", input$ch0_log_y_cut, " pkt"),
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
        caption = "To jeszcze nie jest regresja logistyczna. To oryginalny wynik i umowny próg, który tworzy później zmienną zdał/nie zdał."
      )
  })

  output$ch0_log_table <- renderUI({
    .light_coef_table(logistic_model(), exponentiate = TRUE)
  })

  output$ch0_log_threshold_note <- renderUI({
    lc_feedback(type = "ok",
      p("Y = 1, czyli 'zdał', oznacza wynik czytania od ",
        tags$strong(.light_fmt(input$ch0_log_y_cut, 0)),
        " pkt. Zmiana tego progu zmienia definicję zmiennej zależnej i przelicza współczynniki.")
    )
  })

  output$ch0_log_metrics <- renderUI({
    m <- .light_logistic_metrics(logistic_model())
    tags$table(class = "lc-table lc-table-bordered",
      tags$thead(tags$tr(tags$th("AIC"), tags$th("BIC"),
                         tags$th("RMSE prawdopodobieństw"))),
      tags$tbody(tags$tr(
        tags$td(.light_fmt(m$AIC, 1)),
        tags$td(.light_fmt(m$BIC, 1)),
        tags$td(.light_fmt(m$RMSE_prawdopodobienstw, 3))
      ))
    )
  })
}
