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
          checkboxGroupInput("ch3_predictors", "Predyktory:",
            choices = c(
              "Dotacje do obiadów (%)" = "lunch",
              "Dochód okręgu (tys. USD)" = "income",
              "Angielski jako drugi język (%)" = "english",
              "Uczniowie / nauczyciel" = "student_teacher_ratio",
              "Wydatki na ucznia" = "expenditure",
              "Komputery" = "computer",
              "CalWORKs (%)" = "calworks"
            ),
            selected = c("lunch", "income", "english",
                         "student_teacher_ratio", "expenditure",
                         "computer", "calworks")
          ),
          helpText(style = "margin-top: 8px; font-size: 12px;",
            "Domyślny model celowo zawiera też predyktory słabe lub redundantne.
             Dzięki temu tabela nie udaje, że w realnych danych wszystko jest
             istotne.")
        ),
        column(8,
          lc_feedback(type = "info",
            p("Tabela pokazuje współczynniki pełnego modelu addytywnego,
              bez interakcji. Gwiazdka przy p-value oznacza p < 0.05.")
          ),
          uiOutput("ch3_model_coefs"),
          plotOutput("ch3_coef_plot", height = "250px"),
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
          plotOutput("ch3_control_plot", height = "320px"),
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
          plotOutput("ch3_collin_plot", height = "300px"),
          uiOutput("ch3_collin_info")
        )
      )
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

  # Model jako reactive: zależy od danych i wyboru predyktorów.
  # Dzięki temu przełączanie checkboxów porównuje modele na TYCH SAMYCH danych.
  ch3_model <- reactive({
    df <- .cas_data
    outcome <- input$ch3_outcome
    if (is.null(outcome)) outcome <- "read"
    preds <- input$ch3_predictors
    if (length(preds) == 0) preds <- "lunch"
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

  output$ch3_coef_plot <- renderPlot({
    model <- ch3_model()
    if (is.null(model)) return(NULL)

    coefs <- broom::tidy(model, conf.int = TRUE)
    coefs <- coefs[coefs$term != "(Intercept)", ]

    if (nrow(coefs) == 0) return(NULL)

    labels_pl <- ch3_labels_pl
    coefs$term_pl <- ifelse(coefs$term %in% names(labels_pl),
                             labels_pl[coefs$term], coefs$term)
    coefs$significant <- coefs$p.value < 0.05

    ggplot(coefs, aes(x = estimate, y = term_pl, color = significant)) +
      geom_point(size = 3) +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
      geom_vline(xintercept = 0, linetype = "dashed", color = upwr_secondary) +
      scale_color_manual(values = c("TRUE" = unname(upwr_cat["niebo"]), "FALSE" = unname(upwr_cat["terakota"])),
                         labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
                         name = NULL) +
      labs(
           x = "Estymata β", y = NULL) +
      theme_upwr() +
      theme(legend.position = "top")
  })

  output$ch3_model_stats <- renderUI({
    model <- ch3_model()
    if (is.null(model)) return(NULL)
    metrics <- compute_model_metrics(model)
    tagList(
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
  })

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
