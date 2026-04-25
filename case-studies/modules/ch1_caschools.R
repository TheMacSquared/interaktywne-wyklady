# ============================================================================
# CASE STUDY 1: CASchools
# Pytanie: Czy zmniejszenie klas poprawi wyniki uczniow w Kalifornii?
# ============================================================================

# Ladowanie i przygotowanie danych
data("CASchools", package = "AER")
ca <- CASchools
ca$score <- (ca$read + ca$math) / 2
ca$str <- ca$students / ca$teachers
ca$comp_per_student <- ca$computer / ca$students
ca$poverty <- ifelse(ca$lunch >= 50, "Wysoki", ifelse(ca$lunch >= 25, "Średni", "Niski"))
ca$poverty <- factor(ca$poverty, levels = c("Niski", "Średni", "Wysoki"))

ch1_ui <- lecture_chapter(id = "ch1", num = "1", title = "CASchools", content = tagList(
  fluidRow(column(8, offset = 2,

    # ========================================================================
    # KONTEKST: Sytuacja decyzyjna
    # ========================================================================
    lc_h2("sec-01", "Sytuacja wyjściowa"),

    div(class = "lc-prose",
      p("Jesteśmy analitykami w kalifornijskim departamencie edukacji.
        Polityk proponuje zmniejszenie liczebności klas jako sposób na poprawę
        wyników egzaminacyjnych. Program będzie kosztować miliardy dolarów."),
      p("Naszym zadaniem jest zbadać na dostępnych danych:"),
      div(class = "lc-feedback lc-feedback-info",
        tags$strong("Główne pytanie badawcze:"),
        p(tags$em("\"Czy zmniejszenie liczby uczniów na nauczyciela (STR)
          faktycznie prowadzi do lepszych wyników, czy też obserwowany
          związek wynika z innych czynników?\"")),
        p("Innymi słowy: czy warto wydać te pieniądze na mniejsze klasy,
          czy może są skuteczniejsze interwencje?")
      )
    ),

    div(class = "lc-prose",
      p(tags$b("Dane:"), " CASchools — 420 dystryktów szkolnych w Kalifornii
        (pakiet AER). Każda obserwacja to jeden dystrykt."),
      p(tags$b("Zmienna zależna:"), " średni wynik egzaminu = (reading + math) / 2"),
      p(tags$b("Kluczowa zmienna niezależna:"), " STR = uczniowie / nauczyciele")
    ),

    div(class = "lc-feedback lc-feedback-warning",
      tags$strong("Plan analizy:"),
      tags$ol(
        tags$li("Poznać dane — czym dysponujemy i jakie są potencjalne zmienne zakłócające"),
        tags$li("Sprawdzić prosty związek STR → wyniki (korelacja, regresja prosta)"),
        tags$li("Zidentyfikować zmienne zakłócające — co jeszcze wpływa na wyniki i jest skorelowane z STR?"),
        tags$li("Zbudować model wieloraki — czy efekt STR przetrwa kontrolowanie zakłóceń?"),
        tags$li("Odpowiedzieć na pytanie decyzyjne")
      )
    ),

    # ========================================================================
    # KROK 1: Poznanie danych
    # ========================================================================
    lc_h2("sec-02", "Krok 1: Poznanie danych"),

    div(class = "analysis-step",
      span(class = "step-number", "1"),
      "Zanim zaczniemy analizować, musimy rozumieć,
                   czym dysponujemy. Jakie zmienne mogą być istotne?"
    ),

    div(class = "lc-figure-panel",
      h4("Przegląd zmiennych"),
      fluidRow(
        column(4,
          selectInput("ch1_eda_var", "Zmienna:",
            choices = c(
              "Średni wynik (score)" = "score",
              "Uczniowie/nauczyciel (STR)" = "str",
              "Wydatki/uczeń" = "expenditure",
              "Dochód dystryktu" = "income",
              "% English learners" = "english",
              "% darmowy lunch (proxy biedy)" = "lunch",
              "% CalWorks (zasiłki)" = "calworks"
            ),
            selected = "score"
          )
        ),
        column(8,
          plotOutput("ch1_eda_plot", height = "280px"),
          uiOutput("ch1_eda_stats")
        )
      )
    ),

    div(class = "lc-prose",
      p(tags$b("Kluczowa obserwacja:"), " mamy kilka zmiennych opisujących
        status socjoekonomiczny dystryktu (dochód, % darmowy lunch, % CalWorks).
        Mogą być zmiennymi zakłócającymi — jeśli biedniejsze dystrykty
        mają jednocześnie większe klasy I gorsze wyniki, to obserwowany
        związek STR→wyniki może być pozorny.")
    ),

    div(class = "lc-figure-panel",
      h4("Macierz korelacji — szukamy potencjalnych zakłóceń"),
      plotOutput("ch1_corr_plot", height = "400px")
    ),

    div(class = "lc-feedback lc-feedback-danger",
      tags$strong("Czerwona flaga!"),
      p("Zmienne społeczno-ekonomiczne (lunch, calworks, income) są:"),
      tags$ul(
        tags$li("silnie skorelowane z wynikami (r ≈ −0.87 dla lunch)"),
        tags$li("skorelowane z STR (biedniejsze dystrykty mają większe klasy)")
      ),
      p("To oznacza, że prosty związek STR→wyniki może być artefaktem biedy.
        Musimy to rozsuplać.")
    ),

    # ========================================================================
    # KROK 2: Prosty zwiazek STR -> wyniki
    # ========================================================================
    lc_h2("sec-03", "Krok 2: Prosty związek STR → wyniki"),

    div(class = "analysis-step",
      span(class = "step-number", "2"),
      "Najpierw sprawdźmy naiwny związek — bez kontroli czegokolwiek.
                   To będzie nasz punkt wyjścia."
    ),

    div(class = "lc-prose",
      div(class = "lc-formula-box",
        p(withMathJax("\\(H_0: \\rho_{\\text{STR, score}} = 0\\)"),
          " — brak związku liniowego"),
        p(withMathJax("\\(H_1: \\rho_{\\text{STR, score}} \\neq 0\\)"),
          " — jest związek")
      )
    ),

    div(class = "lc-figure-panel",
      h4("Korelacja i regresja prosta: score ~ STR"),
      fluidRow(
        column(4,
          checkboxInput("ch1_str_color", "Koloruj wg poziomu biedy", value = FALSE),
          hr(),
          uiOutput("ch1_str_test")
        ),
        column(8,
          plotOutput("ch1_str_plot", height = "380px")
        )
      )
    ),

    div(class = "lc-feedback lc-feedback-warning",
      tags$strong("Na tym etapie polityk powiedziałby:"),
      p(tags$em("\"Widzicie? Mniejsze klasy = lepsze wyniki! Dajcie mi budżet.\"")),
      p("Ale my wiemy, że to może być pozorna korelacja.
        Włączmy kolorowanie wg biedy — widać, że biedne dystrykty (czerwone)
        skupiają się w prawym dolnym rogu (duże klasy, niskie wyniki).
        Bieda może tłumaczyć oba zjawiska.")
    ),

    # ========================================================================
    # KROK 3: Czy bieda to zmienna zaklócajaca?
    # ========================================================================
    lc_h2("sec-04", "Krok 3: Czy bieda jest zmienną zakłócającą?"),

    div(class = "analysis-step",
      span(class = "step-number", "3"),
      "Aby zmienna Z była zakłócającą, musi spełniać dwa warunki:
                   (a) wpływać na Y i (b) być skorelowana z X.
                   Sprawdzamy oba."
    ),

    div(class = "lc-prose",
      p("Zmienną zakłócającą (confoundem) nazwiemy ",
        tags$b("% darmowy lunch"), " — to standardowe proxy biedy
        w badaniach edukacyjnych."),
      p("Warunek (a): lunch wpływa na wyniki?"),
      p("Warunek (b): lunch jest skorelowany z STR?")
    ),

    div(class = "lc-figure-panel",
      h4("Sprawdzenie dwóch warunków"),
      fluidRow(
        column(6,
          plotOutput("ch1_conf_a", height = "280px")
        ),
        column(6,
          plotOutput("ch1_conf_b", height = "280px")
        )
      ),
      uiOutput("ch1_conf_stats")
    ),

    div(class = "lc-prose",
      p("Oba warunki spełnione. Lunch (bieda) jest confoundem.
        Teraz kluczowe pytanie: czy efekt STR przetrwa, gdy skontrolujemy biedę?")
    ),

    div(class = "lc-figure-panel",
      h4("Dodatkowe sprawdzenie: wyniki w grupach biedy"),
      fluidRow(
        column(4,
          uiOutput("ch1_anova_result")
        ),
        column(8,
          plotOutput("ch1_anova_plot", height = "320px")
        )
      )
    ),

    div(class = "lc-feedback lc-feedback-info",
      tags$strong("Wynik ANOVA potwierdza:"),
      " bieda ma ogromny wpływ na wyniki — różnica między grupami
        to ~30 punktów. To wielokrotnie więcej niż cały zakres STR."
    ),

    # ========================================================================
    # KROK 4: Kontrolowanie zaklócen (regresja wieloraka)
    # ========================================================================
    lc_h2("sec-05", "Krok 4: Czy efekt STR przetrwa kontrolę?"),

    div(class = "analysis-step",
      span(class = "step-number", "4"),
      "Budujemy modele regresji, stopniowo dodając zmienne kontrolne.
                   Obserwujemy, co dzieje się z współczynnikiem STR."
    ),

    div(class = "lc-prose",
      p("Strategia: zaczynamy od prostego modelu (sam STR) i dodajemy
        zmienne, które podejrzewamy o zakłócanie. Jeśli β przy STR
        znacznie zmaleje lub straci istotność, to prosty związek był pozorny.")
    ),

    div(class = "lc-figure-panel",
      h4("Seria modeli — co się dzieje z efektem STR?"),
      actionButton("ch1_compare_models", "Buduj 4 modele",
                   class = "lc-btn-primary", width = "250px"),
      br(), br(),
      uiOutput("ch1_model_comparison"),
      plotOutput("ch1_beta_str_plot", height = "250px")
    ),

    div(class = "lc-feedback lc-feedback-ok",
      tags$strong("Kluczowe odkrycie:"),
      p("Współczynnik β przy STR maleje po dodaniu zmiennych kontrolnych,
        ale nie zanika całkowicie."),
      p("To sugeruje, że część oryginalnego efektu była pozorna
        (napedzana biedą), ale mniejsze klasy mogą mieć niewielki realny efekt
        — rzędu ~1 punkt na każdego dodatkowego ucznia na nauczyciela.")
    ),

    # ========================================================================
    # KROK 5: Wybrany model — szczegoly
    # ========================================================================
    lc_h2("sec-06", "Krok 5: Analiza wybranego modelu"),

    div(class = "analysis-step",
      span(class = "step-number", "5"),
      "Zbadajmy szczegółowo najlepszy model.
                   Możesz sam wybrać predyktory."
    ),

    div(class = "lc-figure-panel",
      h4("Model wieloraki — wybór predyktorów"),
      fluidRow(
        column(4,
          checkboxGroupInput("ch1_reg_vars", "Predyktory:",
            choices = c(
              "STR" = "str",
              "Dochód" = "income",
              "% English learners" = "english",
              "% darmowy lunch" = "lunch",
              "Wydatki/uczeń" = "expenditure"
            ),
            selected = c("str", "income", "english")
          ),
          actionButton("ch1_fit_model", "Dopasuj",
                       class = "lc-btn-primary", width = "100%"),
          br(), br(),
          uiOutput("ch1_reg_metrics")
        ),
        column(8,
          uiOutput("ch1_reg_coefs"),
          plotOutput("ch1_reg_coef_plot", height = "230px")
        )
      )
    ),

    div(class = "lc-feedback lc-feedback-warning",
      tags$strong("Eksperymentuj:"),
      tags$ul(
        tags$li("Dodaj ", tags$b("lunch"), " do modelu z income — co się stanie?
                 (Współliniowość! Oba mierzą biedę.)"),
        tags$li("Usuń income — jak zmieni się β przy STR?"),
        tags$li("Dodaj expenditure — czy wydatki mają efekt po kontroli biedy?")
      )
    ),

    # ========================================================================
    # KROK 6: Odpowiedz na pytanie decyzyjne
    # ========================================================================
    lc_h2("sec-07", "Krok 6: Odpowiedź na pytanie decyzyjne"),

    div(class = "analysis-step",
      span(class = "step-number", "6"),
      "Wracamy do oryginalnego pytania: czy zmniejszenie klas to dobra inwestycja?"
    ),

    div(class = "lc-feedback lc-feedback-ok",
      tags$strong("Co mówią dane:"),
      tags$ol(
        tags$li(tags$b("Prosty związek STR→wyniki istnieje"),
                " (r ≈ −0.23), ale jest w dużej mierze napędzany biedą
                — biedniejsze dystrykty mają większe klasy I gorsze wyniki."),
        tags$li(tags$b("Po kontroli biedy i ELL efekt STR maleje"),
                ", ale nie zanika — około −1 punkt na każdego dodatkowego
                ucznia na nauczyciela."),
        tags$li(tags$b("Bieda jest wielokrotnie silniejszym predyktorem"),
                " niż wielkość klas. Różnica między biednym a zamożnym dystryktem
                to ~30 punktów; cały zakres STR to ~5 punktów."),
        tags$li(tags$b("Wydatki na ucznia mają zaskakująco słaby efekt"),
                " po kontroli biedy — więcej pieniędzy samo w sobie nie pomaga.")
      )
    ),

    div(class = "lc-feedback lc-feedback-warning",
      tags$strong("Rekomendacja (gdyby to był raport):"),
      p("Zmniejszenie klas może mieć niewielki pozytywny efekt, ale nie jest
        \"silver bullet\". Za miliardy dolarów uzyska się poprawkę rzędu kilku punktów."),
      p("Bardziej efektywne mogą być interwencje celowane w przyczyny biedy edukacyjnej:
        wsparcie językowe dla ELL, programy żywieniowe, wsparcie rodzin.")
    ),

    div(class = "lc-feedback lc-feedback-danger",
      tags$strong("Ograniczenia naszej analizy:"),
      tags$ul(
        tags$li(tags$b("Dane obserwacyjne, nie eksperymentalne"),
                " — nie możemy orzekać o przyczynowości.
                  Może istnieją pominięte zmienne (np. jakość nauczycieli)."),
        tags$li(tags$b("Dane zagregowane na poziomie dystryktu"),
                " — tracimy zmienność między szkołami wewnątrz dystryktu.
                  Błąd ekologiczny."),
        tags$li(tags$b("Współliniowość"),
                " — lunch, income, calworks mierzą to samo (biedę).
                  Nie powinny być w modelu jednocześnie."),
        tags$li(tags$b("Przekrojowe, nie podłużne"),
                " — widzimy jeden moment, nie zmiany w czasie.
                  Nie wiemy, czy dystrykty które zmniejszyły klasy, poprawiły wyniki.")
      ),
      p(tags$em("Aby naprawdę odpowiedzieć na pytanie decyzyjne,
        potrzebowalibyśmy eksperymentu (np. projekt STAR z Tennessee)
        lub danych panelowych z instrumentami."))
    )

  )))
)

# ============================================================================
# SERVER
# ============================================================================

ch1_server <- function(input, output, session) {

  # --- Krok 1: EDA ---
  output$ch1_eda_plot <- renderPlot({
    var <- input$ch1_eda_var
    var_label <- switch(var,
      "score" = "Średni wynik", "str" = "Uczniowie/nauczyciel",
      "expenditure" = "Wydatki/uczeń ($)", "income" = "Dochód ($tys.)",
      "english" = "% English learners", "lunch" = "% darmowy lunch",
      "calworks" = "% CalWorks")

    p1 <- ggplot(ca, aes(x = .data[[var]])) +
      geom_histogram(bins = 30, fill = case_explore, alpha = 0.6, color = "white") +
      labs(title = paste0("Rozkład: ", var_label), x = var_label, y = "Liczba") +
      theme_upwr()

    p2 <- ggplot(ca, aes(y = .data[[var]])) +
      geom_boxplot(fill = case_explore, alpha = 0.4) +
      labs(title = "Boxplot", y = var_label) +
      theme_upwr()

    gridExtra::grid.arrange(p1, p2, ncol = 2, widths = c(2, 1))
  })

  output$ch1_eda_stats <- renderUI({
    var <- input$ch1_eda_var
    x <- ca[[var]]
    lc_stat_grid(
      lc_stat_box("n", length(x), color = case_explore),
      lc_stat_box("Śr.", round(mean(x), 1), color = case_reference),
      lc_stat_box("SD", round(sd(x), 1), color = case_reference),
      lc_stat_box("Zakres", paste0(round(min(x), 1), "–", round(max(x), 1)),
                  color = case_reference),
      columns = 4
    )
  })

  output$ch1_corr_plot <- renderPlot({
    vars <- c("score", "str", "expenditure", "income", "english", "lunch", "calworks")
    cor_mat <- cor(ca[, vars], use = "complete.obs")

    cor_df <- as.data.frame(as.table(cor_mat))
    names(cor_df) <- c("Var1", "Var2", "value")

    labels_pl <- c(
      "score" = "Wynik", "str" = "STR", "expenditure" = "Wydatki",
      "income" = "Dochód", "english" = "% ELL", "lunch" = "% lunch",
      "calworks" = "% CalWorks")
    cor_df$Var1 <- labels_pl[as.character(cor_df$Var1)]
    cor_df$Var2 <- labels_pl[as.character(cor_df$Var2)]

    ggplot(cor_df, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(value, 2)), size = 3.5) +
      scale_fill_gradient2(low = case_highlight, mid = "white", high = case_explore,
                           midpoint = 0, limits = c(-1, 1), name = "r") +
      labs(title = "Macierz korelacji — szukamy powiązań i potencjalnych zakłóceń",
           x = NULL, y = NULL) +
      theme_upwr() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  # --- Krok 2: STR vs wyniki ---
  output$ch1_str_plot <- renderPlot({
    p <- ggplot(ca, aes(x = str, y = score))

    if (input$ch1_str_color) {
      p <- p + geom_point(aes(color = poverty), alpha = 0.6, size = 2) +
        scale_color_manual(values = c(case_explore, case_conclude, case_highlight),
                           name = "Bieda")
    } else {
      p <- p + geom_point(color = case_reference, alpha = 0.4, size = 2)
    }

    p + geom_smooth(method = "lm", se = TRUE,
                    color = case_model, fill = case_model, alpha = 0.1) +
      labs(title = "STR vs wyniki egzaminu",
           subtitle = "Każdy punkt = jeden dystrykt szkolny",
           x = "Uczniowie na nauczyciela (STR)",
           y = "Średni wynik egzaminu") +
      theme_upwr()
  })

  output$ch1_str_test <- renderUI({
    cor_res <- rstatix::cor_test(ca, str, score, method = "pearson")
    tidy_cor <- as.data.frame(cor_res)

    model <- lm(score ~ str, data = ca)
    coefs <- broom::tidy(model)
    g <- broom::glance(model)

    tagList(
      div(class = "lc-feedback lc-feedback-info",
        p(tags$strong("Korelacja Pearsona:")),
        p(paste0("r = ", round(tidy_cor$cor, 3),
                 ", p ", if (tidy_cor$p < 0.001) "< 0.001" else paste0("= ", round(tidy_cor$p, 4)))),
        p(style = "color: var(--upwr-accent); font-weight: bold;",
          "Istotna ujemna korelacja")
      ),
      div(class = "lc-feedback lc-feedback-info",
        p(tags$strong("Regresja prosta:")),
        p(paste0("score = ", round(coefs$estimate[1], 1), " ",
                 round(coefs$estimate[2], 2), " × STR")),
        p(paste0("R² = ", round(g$r.squared, 3),
                 " (STR wyjaśnia tylko ", round(g$r.squared * 100, 1), "% zmienności)")),
        p(tags$em("Interpretacja: każdy dodatkowy uczeń/nauczyciela → wynik niższy o ~",
                  abs(round(coefs$estimate[2], 1)), " pkt"))
      )
    )
  })

  # --- Krok 3: Zmienne zaklocajace ---
  output$ch1_conf_a <- renderPlot({
    ggplot(ca, aes(x = lunch, y = score)) +
      geom_point(color = case_reference, alpha = 0.3, size = 1.5) +
      geom_smooth(method = "lm", se = FALSE, color = case_highlight, linewidth = 1.2) +
      labs(title = "Warunek (a): lunch → wyniki?",
           subtitle = paste0("r = ", round(cor(ca$lunch, ca$score), 3)),
           x = "% darmowy lunch", y = "Wynik") +
      theme_upwr()
  })

  output$ch1_conf_b <- renderPlot({
    ggplot(ca, aes(x = lunch, y = str)) +
      geom_point(color = case_reference, alpha = 0.3, size = 1.5) +
      geom_smooth(method = "lm", se = FALSE, color = case_test, linewidth = 1.2) +
      labs(title = "Warunek (b): lunch → STR?",
           subtitle = paste0("r = ", round(cor(ca$lunch, ca$str), 3)),
           x = "% darmowy lunch", y = "STR") +
      theme_upwr()
  })

  output$ch1_conf_stats <- renderUI({
    r_lunch_score <- cor(ca$lunch, ca$score)
    r_lunch_str <- cor(ca$lunch, ca$str)
    lc_stat_grid(
      lc_stat_box("lunch→wyniki", paste0("r = ", round(r_lunch_score, 3)),
                  color = case_highlight),
      lc_stat_box("lunch→STR", paste0("r = ", round(r_lunch_str, 3)),
                  color = case_test),
      lc_stat_box("Wniosek", "Oba istotne → confound!", color = case_conclude),
      columns = 3
    )
  })

  # ANOVA
  output$ch1_anova_plot <- renderPlot({
    means <- ca %>% group_by(poverty) %>%
      summarise(m = mean(score), .groups = "drop")

    ggplot(ca, aes(x = poverty, y = score, fill = poverty)) +
      geom_boxplot(alpha = 0.6, outlier.alpha = 0.2) +
      geom_jitter(width = 0.15, alpha = 0.1, size = 1) +
      scale_fill_manual(values = c(case_explore, case_conclude, case_highlight)) +
      labs(title = "Wyniki wg poziomu biedy",
           x = "Poziom biedy (na podst. % darmowy lunch)",
           y = "Średni wynik") +
      theme_upwr() +
      theme(legend.position = "none")
  })

  output$ch1_anova_result <- renderUI({
    result <- rstatix::anova_test(ca, score ~ poverty)
    tidy_res <- as.data.frame(result)

    tukey <- rstatix::tukey_hsd(ca, score ~ poverty)
    tukey_df <- as.data.frame(tukey)

    means <- ca %>% group_by(poverty) %>%
      summarise(m = round(mean(score), 1), n = n(), .groups = "drop")

    tagList(
      div(class = "lc-feedback lc-feedback-info",
        p(tags$strong("Średnie w grupach:")),
        lapply(1:nrow(means), function(i) {
          p(paste0(means$poverty[i], ": ", means$m[i], " (n=", means$n[i], ")"))
        }),
        hr(),
        p(tags$strong("ANOVA:")),
        p(paste0("F(", tidy_res$DFn, ",", tidy_res$DFd, ") = ",
                 round(tidy_res$F, 1),
                 ", p < 0.001, η² = ", round(tidy_res$ges, 3))),
        hr(),
        p(tags$strong("Tukey HSD:")),
        tags$ul(lapply(1:nrow(tukey_df), function(i) {
          tags$li(paste0(tukey_df$group1[i], " vs ", tukey_df$group2[i],
                         ": Δ = ", round(tukey_df$estimate[i], 1),
                         " pkt, p.adj ", if (tukey_df$p.adj[i] < 0.001) "< 0.001"
                         else paste0("= ", round(tukey_df$p.adj[i], 3))))
        }))
      )
    )
  })

  # --- Krok 4: Seria modeli ---
  ch1_models_data <- reactiveVal(NULL)

  observeEvent(input$ch1_compare_models, {
    m1 <- lm(score ~ str, data = ca)
    m2 <- lm(score ~ str + income, data = ca)
    m3 <- lm(score ~ str + income + english, data = ca)
    m4 <- lm(score ~ str + income + english + lunch, data = ca)

    models <- list(m1, m2, m3, m4)
    labels <- c("1: sam STR", "2: + dochód", "3: + dochód + ELL", "4: + dochód + ELL + lunch")

    results <- lapply(seq_along(models), function(i) {
      m <- models[[i]]
      g <- broom::glance(m)
      coefs <- broom::tidy(m)
      beta_str <- coefs$estimate[coefs$term == "str"]
      p_str <- coefs$p.value[coefs$term == "str"]
      data.frame(
        model = labels[i], r2 = g$r.squared, adj_r2 = g$adj.r.squared,
        aic = g$AIC, rmse = sqrt(mean(residuals(m)^2)),
        beta_str = beta_str, p_str = p_str
      )
    })

    ch1_models_data(do.call(rbind, results))
  })

  output$ch1_model_comparison <- renderUI({
    df <- ch1_models_data()
    if (is.null(df)) return(NULL)

    rows <- lapply(1:nrow(df), function(i) {
      p_str_fmt <- if (df$p_str[i] < 0.001) "< 0.001" else round(df$p_str[i], 3)
      sig_style <- if (df$p_str[i] < 0.05) "font-weight:bold;" else "color: var(--upwr-reference);"
      tags$tr(
        tags$td(df$model[i]),
        tags$td(style = sig_style, round(df$beta_str[i], 2)),
        tags$td(p_str_fmt),
        tags$td(round(df$adj_r2[i], 3)),
        tags$td(round(df$aic[i], 0))
      )
    })

    tagList(
      tags$table(class = "lc-table lc-table-bordered lc-table-striped",
        style = "font-size: 13px;",
        tags$thead(
          tags$tr(tags$th("Model"), tags$th("β STR"), tags$th("p (STR)"),
                  tags$th("adj.R²"), tags$th("AIC"))
        ),
        tags$tbody(rows)
      ),
      div(class = "lc-feedback lc-feedback-info",
        p(tags$strong("Obserwacja:"), " β przy STR spada z ~",
          round(df$beta_str[1], 1), " do ~", round(df$beta_str[3], 1),
          " po kontroli biedy i ELL. Efekt zmaleje o ~",
          round((1 - abs(df$beta_str[3]) / abs(df$beta_str[1])) * 100), "%.")
      )
    )
  })

  output$ch1_beta_str_plot <- renderPlot({
    df <- ch1_models_data()
    if (is.null(df)) return(NULL)

    df$model <- factor(df$model, levels = df$model)
    df$sig <- df$p_str < 0.05

    ggplot(df, aes(x = model, y = beta_str, fill = sig)) +
      geom_col(alpha = 0.8, width = 0.6) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      scale_fill_manual(values = c("TRUE" = case_model, "FALSE" = case_muted),
                        labels = c("TRUE" = "p < 0.05", "FALSE" = "nieistotny"),
                        name = NULL) +
      labs(title = "Jak zmienia się efekt STR po dodaniu zmiennych kontrolnych?",
           x = NULL, y = "β przy STR") +
      theme_upwr() +
      theme(legend.position = "top",
            axis.text.x = element_text(angle = 20, hjust = 1))
  })

  # --- Krok 5: Model interaktywny ---
  ch1_model <- reactiveVal(NULL)

  observeEvent(input$ch1_fit_model, {
    preds <- input$ch1_reg_vars
    if (length(preds) == 0) preds <- "str"
    formula <- as.formula(paste("score ~", paste(preds, collapse = " + ")))
    ch1_model(lm(formula, data = ca))
  })

  output$ch1_reg_coefs <- renderUI({
    model <- ch1_model()
    if (is.null(model)) return(NULL)

    coefs <- broom::tidy(model)
    labels_pl <- c(
      "(Intercept)" = "Wyraz wolny", "str" = "STR",
      "income" = "Dochód", "english" = "% ELL",
      "expenditure" = "Wydatki/ucz.", "lunch" = "% lunch")
    coefs$term_pl <- ifelse(coefs$term %in% names(labels_pl),
                             labels_pl[coefs$term], coefs$term)

    rows <- lapply(1:nrow(coefs), function(i) {
      sig <- coefs$p.value[i] < 0.05
      tags$tr(style = if (!sig && coefs$term[i] != "(Intercept)") "color: var(--upwr-reference);" else "",
        tags$td(coefs$term_pl[i]),
        tags$td(round(coefs$estimate[i], 3)),
        tags$td(round(coefs$std.error[i], 3)),
        tags$td(paste0(format.pval(coefs$p.value[i], digits = 3),
                        if (sig) " *" else ""))
      )
    })

    tags$table(class = "lc-table lc-table-bordered",
      style = "font-size: 13px;",
      tags$thead(tags$tr(tags$th("Zmienna"), tags$th("β"), tags$th("SE"), tags$th("p"))),
      tags$tbody(rows)
    )
  })

  output$ch1_reg_coef_plot <- renderPlot({
    model <- ch1_model()
    if (is.null(model)) return(NULL)

    coefs <- broom::tidy(model, conf.int = TRUE)
    coefs <- coefs[coefs$term != "(Intercept)", ]
    if (nrow(coefs) == 0) return(NULL)

    labels_pl <- c("str" = "STR", "income" = "Dochód", "english" = "% ELL",
                    "expenditure" = "Wydatki", "lunch" = "% lunch")
    coefs$term_pl <- ifelse(coefs$term %in% names(labels_pl),
                             labels_pl[coefs$term], coefs$term)
    coefs$sig <- coefs$p.value < 0.05

    ggplot(coefs, aes(x = estimate, y = term_pl, color = sig)) +
      geom_point(size = 3) +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
      geom_vline(xintercept = 0, linetype = "dashed", color = case_reference) +
      scale_color_manual(values = c("TRUE" = case_model, "FALSE" = case_highlight),
                         labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
                         name = NULL) +
      labs(title = "Współczynniki z 95% CI", x = "β", y = NULL) +
      theme_upwr() + theme(legend.position = "top")
  })

  output$ch1_reg_metrics <- renderUI({
    model <- ch1_model()
    if (is.null(model)) return(NULL)
    g <- broom::glance(model)
    rmse <- sqrt(mean(residuals(model)^2))
    lc_stat_grid(
      lc_stat_box("adj.R²", round(g$adj.r.squared, 3), color = case_model),
      lc_stat_box("AIC", round(g$AIC, 0), color = case_conclude),
      lc_stat_box("RMSE", round(rmse, 1), color = case_highlight),
      columns = 3
    )
  })
}
