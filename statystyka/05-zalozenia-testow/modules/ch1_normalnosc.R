# ============================================================================
# CHAPTER 1: Normalnosc rozkladu
# ============================================================================

ch1_ui <- lecture_chapter(
  id = "ch-normalnosc",
  num = "01",
  title = "Normalność rozkładu",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 01 · Założenia testów",
      num    = "01",
      title  = "Normalność rozkładu.",
      lead   = "Nie pytamy, czy dane są idealnie normalne, lecz czy ich kształt zagraża wnioskom. Zaczynamy od wykresu, a test traktujemy pomocniczo."
    ),

    lc_h2("ch1-metody", "Które metody wymagają normalności?"),

    tagList(
      p("To, co oceniamy, zależy od metody:"),
      tags$ul(
        tags$li(tags$b("Test t jednej próby"), " — rozkład badanej zmiennej wokół średniej"),
        tags$li(tags$b("Test t dla grup"), " — rozkład wyników (reszt) w porównywanych grupach"),
        tags$li(tags$b("Test t sparowany"), " — rozkład różnic między pomiarami, nie obu pomiarów osobno"),
        tags$li(tags$b("ANOVA"), " — normalność reszt w każdej grupie"),
        tags$li(tags$b("Korelacja Pearsona"), " — rozkład dwuwymiarowy normalny"),
        tags$li(tags$b("Regresja liniowa"), " — normalność reszt (nie danych!)")
      ),
      p(tags$b("Ważne:"), " Testy t i ANOVA zwykle tolerują łagodne odchylenia,
        zwłaszcza przy podobnych liczebnościach grup. Nie istnieje jednak jeden
        próg n, po którym można automatycznie zignorować silną skośność lub obserwacje odstające.")
    ),

    # ========================================================================
    # WIDGET 1: Wizualne sprawdzanie normalnosci
    # ========================================================================
    lc_h2("ch1-wizualnie", "Wizualne sprawdzanie normalności"),

    figure_panel(
      label = "Ryc. 1.1",
      title = "Histogram + Q-Q plot",
      fluidRow(
        column(4,
          selectInput("ch1_dist", "Rozkład danych:",
            choices = c(
              "Normalny" = "normal",
              "Prawoskośny" = "skewed",
              "Ciężkie ogony" = "heavy_tail",
              "Dwumodalny" = "bimodal",
              "Jednostajny" = "uniform"
            ),
            selected = "normal"
          ),
          sliderInput("ch1_n", "Wielkość próby (n):",
                      min = 10, max = 200, value = 50, step = 10),
          actionButton("ch1_gen", "Generuj dane",
                       class = "lc-btn-primary", width = "100%")
        ),
        column(8,
          zoom_plot_ui("ch1_normality_plots", height = "350px")
        )
      )
    ),

    lc_feedback(type = "info",
      tags$strong("Jak czytać Q-Q plot:"),
      " Punkty blisko linii oznaczają, że rozkład jest wystarczająco podobny do normalnego.
        Systematyczne odchylenia na końcach wskazują ciężkie lub lekkie ogony,
        wygięcie — skośność, a pojedyncze dalekie punkty — możliwe obserwacje odstające."
    ),

    # ========================================================================
    # WIDGET 2: Testy normalnosci
    # ========================================================================
    lc_h2("ch1-testy-formalne", "Test formalny jako pomoc"),

    tagList(
      p(tags$b("Shapiro–Wilk"), " sprawdza zgodność danych z rozkładem normalnym."),
      p(withMathJax("\\(H_0\\)"), ": dane pochodzą z rozkładu normalnego. ",
        "Małe p jest sygnałem odchylenia, ale nie mówi, czy odchylenie jest ważne dla naszej analizy.")
    ),

    figure_panel(
      label = "Ryc. 1.2",
      title = "Shapiro–Wilk — wynik obok Q-Q plotu",
      fluidRow(
        column(4,
          helpText("Używa danych z widgetu powyżej."),
          actionButton("ch1_test_norm", "Policz test Shapiro–Wilka",
                       class = "lc-btn-primary", width = "100%")
        ),
        column(8,
          uiOutput("ch1_norm_results")
        )
      )
    ),

    lc_feedback(type = "warning",
      tags$strong("Problem z testami formalnymi:"),
      " Przy dużym n test może wykryć drobne, praktycznie niegroźne odchylenie.
        Przy małym n może nie zauważyć poważnego problemu. ",
      tags$strong("Decyzję opieraj przede wszystkim na Q-Q plocie, outlierach i rodzaju analizy.")
    ),

    # ========================================================================
    # WIDGET 3: Co robic gdy naruszone?
    # ========================================================================
    lc_h2("ch1-naruszenia", "Gdy normalność jest naruszona"),

    tagList(
      p("Praktyczna kolejność postępowania:"),
      tags$ol(
        tags$li(tags$b("Sprawdź wykres i dane"), " — czy problemem jest łagodna skośność, czy pojedynczy błąd/outlier?"),
        tags$li(tags$b("Oceń odporność metody"), " — łagodne odchylenie często nie wymaga zmiany analizy."),
        tags$li(tags$b("Użyj alternatywy rangowej"), " — przy silnym naruszeniu albo zmiennej quasi-ilościowej; patrz tabela."),
        tags$li(tags$b("Transformuj tylko z uzasadnieniem"), " — np. log dla dodatnich danych o różnicach względnych. Transformacja zmienia skalę interpretacji.")
      )
    ),

    figure_panel(
      label = "Ryc. 1.3",
      title = "Efekt transformacji logarytmicznej",
      fluidRow(
        column(4,
          helpText("Generujemy dane prawoskośne i stosujemy log()."),
          sliderInput("ch1_trans_n", "n:", min = 30, max = 200, value = 80, step = 10),
          actionButton("ch1_transform", "Generuj i transformuj",
                       class = "lc-btn-warning", width = "100%")
        ),
        column(8,
          zoom_plot_ui("ch1_transform_plots", height = "300px"),
          uiOutput("ch1_transform_results")
        )
      )
    ),

    lc_feedback(type = "ok",
      tags$strong("Praktyczne alternatywy rangowe:"),
      p("To użyteczne zamienniki narzędziowe, ale ich wynik nie zawsze opisuje dokładnie tę samą wielkość co średnia."),
      tags$table(class = "lc-table lc-table-bordered", style = "font-size: 14px;",
        tags$tbody(
          tags$tr(tags$td("Test t jednej próby"), tags$td("→ Wilcoxon jednej próby")),
          tags$tr(tags$td("Test t niezależny"), tags$td("→ Mann-Whitney U")),
          tags$tr(tags$td("Test t sparowany"), tags$td("→ Wilcoxon par znakowych")),
          tags$tr(tags$td("ANOVA"), tags$td("→ Kruskal-Wallis")),
          tags$tr(tags$td("Pearson"), tags$td("→ Spearman"))
        )
      )
    ),

    lc_chapter_next(
      num = "02",
      title = "Jednorodne wariancje",
      lead = "założenie równego rozrzutu między porównywanymi grupami.",
      target_id = "ch-wariancje"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch1_server <- function(input, output, session) {

  ch1_data <- reactiveVal(NULL)

  observeEvent(input$ch1_gen, {
    ch1_data(generate_test_data(input$ch1_n, input$ch1_dist))
  })

  zoom_plot_server("ch1_normality_plots", reactive({
    x <- ch1_data()
    if (is.null(x)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Generuj dane'",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      df <- data.frame(x = x)

      p1 <- ggplot(df, aes(x = x)) +
        geom_histogram(aes(y = after_stat(density)), bins = 20,
                       fill = col_test, alpha = 0.6, color = "white") +
        stat_function(fun = dnorm, args = list(mean = mean(x), sd = sd(x)),
                      color = col_ok, linewidth = 1.2, linetype = "dashed") +
        labs(
             x = "Wartość", y = "Gęstość") +
        theme_upwr()

      p2 <- ggplot(df, aes(sample = x)) +
        stat_qq(color = col_test, alpha = 0.6) +
        stat_qq_line(color = col_ok, linewidth = 1) +
        labs(
             x = "Kwantyle teoretyczne", y = "Kwantyle próbkowe") +
        theme_upwr()

      gridExtra::grid.arrange(p1, p2, ncol = 2)
    }
  }))

  # --- Widget 2: Testy normalnosci ---
  output$ch1_norm_results <- renderUI({
    req(input$ch1_test_norm)
    x <- isolate(ch1_data())
    if (is.null(x)) return(lc_feedback(type = "warning", "Najpierw wygeneruj dane."))

    sw <- shapiro_test(data.frame(value = x), value)
    sw_color <- if (sw$p >= 0.05) col_ok else col_fail

    lc_feedback(type = "info",
      p(tags$strong("Shapiro–Wilk:"), " W = ", round(sw$statistic, 4),
        ", p = ", format_p_value(sw$p)),
      p(style = paste0("color:", sw_color, ";"),
        if (sw$p >= 0.05) {
          "Test nie wykrył wyraźnego odstępstwa. Nadal spójrz na Q-Q plot."
        } else {
          "Test wykrył odstępstwo. Oceń na Q-Q plocie jego rodzaj i znaczenie dla wybranej metody."
        })
    )
  })

  # --- Widget 3: Transformacja ---
  ch1_trans_data <- reactiveVal(NULL)

  observeEvent(input$ch1_transform, {
    x <- rgamma(input$ch1_trans_n, shape = 2, scale = 5) + 1
    ch1_trans_data(x)
  })

  zoom_plot_server("ch1_transform_plots", reactive({
    x <- ch1_trans_data()
    if (is.null(x)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Generuj i transformuj'",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      log_x <- log(x)

      p1 <- ggplot(data.frame(x = x), aes(sample = x)) +
        stat_qq(color = col_fail, alpha = 0.5) +
        stat_qq_line(color = col_fail) +
        labs(title = "Oryginalna skala",
             x = "Kwantyle teoretyczne", y = "Kwantyle próbkowe") +
        theme_upwr()

      p2 <- ggplot(data.frame(x = log_x), aes(sample = x)) +
        stat_qq(color = col_ok, alpha = 0.5) +
        stat_qq_line(color = col_ok) +
        labs(title = "Po transformacji log()",
             x = "Kwantyle teoretyczne", y = "Kwantyle próbkowe") +
        theme_upwr()

      gridExtra::grid.arrange(p1, p2, ncol = 2)
    }
  }))

  output$ch1_transform_results <- renderUI({
    x <- ch1_trans_data()
    if (is.null(x)) return(NULL)

    lc_feedback(type = "info",
      tags$strong("Porównaj kształt, nie tylko liczbę:"),
      " po transformacji punkty zwykle leżą bliżej prostej. Pamiętaj, że wynik
        interpretujemy teraz na skali logarytmicznej, czyli przez różnice względne/ilorazy."
    )
  })
}
