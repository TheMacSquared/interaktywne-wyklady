# ============================================================================
# CHAPTER 6: Ilosciowa i jakosciowa (2 grupy)
# ============================================================================

ch6_ui <- list(
  id = "ch-dwie-grupy", num = "08", title = "Test t dwóch grup",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 08 · Testowanie hipotez",
      num    = "08",
      title  = "Test t dwóch grup.",
      lead   = "„Czy mężczyźni jeżdżą szybciej niż kobiety?” Porównanie średnich między
                dwiema grupami — czy obserwowana różnica jest realna, czy wynika
                z losowości próby."
    ),

    lc_h2("ch6-intro", "Test t dla dwóch prób niezależnych"),

    tagList(
      p("Pytanie: czy średnie w dwóch grupach różnią się istotnie?"),
      p("Przykład: czy mężczyźni i kobiety różnią się wzrostem?"),
      p("Trzy warianty par hipotez:"),
      lc_formula_box(
        p(tags$b("Dwustronna"), " (grupy różnią się):"),
        p(withMathJax("\\(H_0: \\mu_1 = \\mu_2 \\quad\\)"),
          withMathJax("\\(H_a: \\mu_1 \\neq \\mu_2\\)"))
      ),
      lc_formula_box(
        p(tags$b("Prawostronna"), " (grupa 1 ma ",
          tags$em("wyższą"), " średnią):"),
        p(withMathJax("\\(H_0: \\mu_1 \\leq \\mu_2 \\quad\\)"),
          withMathJax("\\(H_a: \\mu_1 > \\mu_2\\)"))
      ),
      lc_formula_box(
        p(tags$b("Lewostronna"), " (grupa 1 ma ",
          tags$em("niższą"), " średnią):"),
        p(withMathJax("\\(H_0: \\mu_1 \\geq \\mu_2 \\quad\\)"),
          withMathJax("\\(H_a: \\mu_1 < \\mu_2\\)"))
      ),
      p("Statystyka testowa mierzy różnicę średnich wyrażoną w jednostkach błędu
        standardowego. Im większa wartość bezwzględna ", withMathJax("\\(|t|\\)"),
        ", tym bardziej nieprawdopodobne jest zobaczenie takiej różnicy gdy
        H₀ jest prawdziwa."),
      lc_formula_box(
        p("Statystyka testowa: ",
          withMathJax("\\(t = \\frac{\\bar{x}_1 - \\bar{x}_2}{SE}\\)"))
      )
    ),

    # ========================================================================
    # Cwiczenie: sformuluj hipotezy
    # ========================================================================
    lc_h2("ch6-cwiczenie", "Ćwiczenie: sformułuj hipotezy"),

    tagList(
      p("Porównanie dwóch grup — jak zapisać H₀ i Hₐ?")
    ),

    hypothesis_practice("ch6", list(
      list(
        question = "Czy mężczyźni jeżdżą szybciej niż kobiety? (średnia prędkość
                    przekroczenia limitu z fotoradarów, próba 200 kierowców)",
        h0 = "\\(H_0: \\mu_M \\leq \\mu_K\\)",
        ha = "\\(H_a: \\mu_M > \\mu_K\\) (mężczyźni szybciej)",
        note = "Jednostronny — pytanie jest kierunkowe."
      ),
      list(
        question = "Czy średni plon pszenicy odmiany X różni się od odmiany Y?",
        h0 = "\\(H_0: \\mu_X = \\mu_Y\\)",
        ha = "\\(H_a: \\mu_X \\neq \\mu_Y\\)",
        note = "Dwustronny — pytanie neutralne, nie wskazuje kierunku."
      ),
      list(
        question = "20 uczniów zmierzono przed i po kursie szybkiego czytania
                    (słowa na minutę). Czy kurs poprawił wyniki?",
        h0 = "\\(H_0: \\mu_d \\leq 0\\) (d = po − przed)",
        ha = "\\(H_a: \\mu_d > 0\\) (poprawa)",
        note = "Dane sparowane (nie niezależne!) — te same osoby mierzone dwa razy. Analizujemy różnice."
      )
    )),

    # ========================================================================
    # WIDGET 1: Test t niezalezny
    # ========================================================================
    lc_h2("ch6-niezalezny", "Test t niezależny"),

    figure_panel(
      label = "Ryc. 8.1",
      title = "Porównanie dwóch grup",
      fluidRow(
        column(4,
          selectInput("ch6_ind_var", "Zmienna ilościowa:",
            choices = c(
              "Wzrost" = "wzrost",
              "Waga" = "waga",
              "Średnia ocen" = "srednia_ocen",
              "Czas dojazdu" = "czas_dojazdu"
            ),
            selected = "wzrost"
          ),
          sliderInput("ch6_ind_n", "n (na grupę):",
                      min = 15, max = 100, value = 40, step = 5),
          actionButton("ch6_run_ind_t", "Losuj próbę",
                       class = "lc-btn-primary", width = "100%"),
          hr(),
          h5("Kroki testu:"),
          lc_stack(gap = "sm",
            actionButton("ch6_ind_step1", "1. Dane",
                         class = "lc-btn-outline", width = "100%"),
            actionButton("ch6_ind_step2", "2. Średnie w grupach",
                         class = "lc-btn-outline", width = "100%"),
            actionButton("ch6_ind_step3", "3. Statystyka t",
                         class = "lc-btn-outline", width = "100%"),
            actionButton("ch6_ind_step4", "4. p-wartość i decyzja",
                         class = "lc-btn-outline", width = "100%")
          )
        ),
        column(8,
          uiOutput("ch6_ind_hypothesis"),
          plotOutput("ch6_ind_boxplot", height = "300px"),
          uiOutput("ch6_ind_result")
        )
      )
    ),

    # ========================================================================
    # WIDGET 2: Test t parowy
    # ========================================================================
    lc_h2("ch6-parowy", "Test t dla prób zależnych (sparowany)"),

    tagList(
      p("Gdy mierzymy tych samych osobników dwa razy
        (przed i po interwencji), używamy testu t dla danych sparowanych."),
      p("Przykład: wyniki studentów przed i po korepetycjach."),
      p("Testujemy różnice: ", withMathJax("\\(d_i = x_{\\text{po},i} - x_{\\text{przed},i}\\)"),
        ". Pytamy, czy średnia różnic ≠ 0.")
    ),

    figure_panel(
      label = "Ryc. 8.2",
      title = "Test t dla danych sparowanych: przed i po",
      fluidRow(
        column(4,
          sliderInput("ch6_paired_n", "Liczba studentów:",
                      min = 10, max = 50, value = 25, step = 5),
          sliderInput("ch6_paired_effect", "Efekt interwencji (pkt):",
                      min = 0, max = 15, value = 5, step = 1),
          actionButton("ch6_run_paired", "Generuj i testuj",
                       class = "lc-btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch6_paired_plot", height = "300px"),
          uiOutput("ch6_paired_result")
        )
      )
    ),

    # ========================================================================
    # WIDGET 3: Sparowane vs. niesparowane — te same dane, inny wynik
    # ========================================================================
    lc_h2("ch6-compare", "Dlaczego sparowanie ma znaczenie?"),

    tagList(
      p("Wyobraź sobie badanie: 20 pacjentów zmierzono ciśnienie ",
        "przed", " nową dietą. Po 3 miesiącach na kontrolę wróciło",
        " tylko 15 — 5 pacjentów z najwyższym ciśnieniem wyjściowym nie zgłosiło się."),
      p("Te same dane, dwa podejścia:"),
      tags$ul(
        tags$li(tags$b("Niesparowane:"),
          " porównujemy 20 pomiarów 'przed' z 15 pomiarami 'po' — jak dwie niezależne grupy."),
        tags$li(tags$b("Sparowane:"),
          " bierzemy tylko 15 pacjentów z obydwoma pomiarami i liczymy różnice.")
      )
    ),

    figure_panel(
      label = "Ryc. 8.3",
      title = "Błąd atrycji: sparowane vs. niesparowane na tych samych danych",
      fluidRow(
        column(6,
          p(tags$strong("Analiza niesparowana"), " (n₁=20, n₂=15)"),
          plotOutput("ch6_compare_ind_plot", height = "260px"),
          uiOutput("ch6_compare_ind_result")
        ),
        column(6,
          p(tags$strong("Analiza sparowana"), " (15 par)"),
          plotOutput("ch6_compare_paired_plot", height = "260px"),
          uiOutput("ch6_compare_paired_result")
        )
      )
    ),

    inline_callout(
      label = "Błąd doboru",
      "5 pacjentów nie wróciło na kontrolę — nie było to przypadkowe, lecz związane
       z wyższym ciśnieniem wyjściowym (MNAR). Analiza niesparowana 'widzi' tę różnicę
       jako efekt diety; analiza sparowana eliminuje błąd, porównując każdego pacjenta
       ze sobą.",
      color = "info"
    ),

    inline_callout(
      label = "Uwaga",
      "gdy założenia testu t nie są spełnione (skrajne odstające, mocno skośny
       rozkład, małe n), stosuje się testy nieparametryczne — omówimy je w osobnym
       wykładzie.",
      color = "uwaga"
    ),

    lc_h2("ch6-cas", "Ćwiczenia", "CASchools — test t dwóch grup"),

    lc_feedback(type = "info",
      p(tags$b("Dane: "), "420 okręgów szkolnych Kalifornii (1998–1999). Plik: ",
        tags$code("dane/caschools.csv"), "."),
      p("Zmienne w zadaniach: ", tags$code("read"),
        " (wyniki z czytania), ", tags$code("grades"),
        " (typ szkoły: KK-06/KK-08), ",
        tags$code("student_teacher_ratio"), " (STR).")
    ),

    figure_panel(label = "Ćwiczenie",
      h4("Zadanie 6 — Czy typ szkoły różnicuje wyniki z czytania?"),
      p("Okręgi dzielą się na szkoły ", tags$code("KK-06"), " i ", tags$code("KK-08"),
        ". Przetestuj, czy średnie wyniki ", tags$code("read"),
        " różnią się między grupami. Wykonaj test t dla prób niezależnych.
        Zapisz: t, df, p. Czy różnica jest istotna?"),
      actionButton("cas_ch6_ans6", "Pokaż rozwiązanie",
                   class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("cas_ch6_sol6")
    ),

    figure_panel(label = "Ćwiczenie",
      h4("Zadanie 7 — Duże klasy vs małe — czy STR ma znaczenie?"),
      p("Utwórz zmienną binarną: ",
        tags$code("high_str = (student_teacher_ratio > 20)"),
        ". Porównaj wyniki ", tags$code("read"),
        " między okręgami z dużym (STR > 20) i małym (STR ≤ 20) stosunkiem.
        Czy różnica jest istotna? Jak duże jest przesunięcie w punktach?
        Skąd może wynikać ta różnica?"),
      actionButton("cas_ch6_ans7", "Pokaż rozwiązanie",
                   class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("cas_ch6_sol7")
    ),

    lc_chapter_next(
      num       = "09",
      title     = "ANOVA",
      lead      = "a gdy grup jest więcej niż dwie?",
      target_id = "ch-anova"
    )
  )
)

# ============================================================================
# DANE — CASchools (wczytane raz przy ladowaniu modulu)
# ============================================================================

.ch6_cas <- read.csv(file.path(app_dir, "dane", "caschools.csv"),
                     stringsAsFactors = FALSE)

# Statyczne dane do widgetu porownania sparowany vs. niesparowany (Ryc. 8.3)
.ch6_compare <- local({
  przed_paired  <- c(132, 138, 145, 141, 136, 152, 143, 139,
                     147, 134, 148, 141, 137, 144, 150)
  po_paired     <- c(130, 140, 142, 143, 127, 154, 142, 144,
                     140, 133, 145, 142, 135, 149, 148)
  przed_dropout <- c(164, 170, 175, 161, 178)

  pairs <- data.frame(id = 1:15, przed = przed_paired, po = po_paired)

  ind_data <- data.frame(
    wartosc = c(przed_paired, przed_dropout, po_paired),
    grupa   = factor(
      c(rep("Przed", 20), rep("Po", 15)),
      levels = c("Przed", "Po")
    ),
    typ = c(rep("para", 15), rep("dropout", 5), rep("para", 15))
  )

  long_pairs <- pivot_longer(pairs, cols = c(przed, po),
                             names_to = "moment", values_to = "cisnienie") %>%
    mutate(moment = factor(moment,
                           levels = c("przed", "po"),
                           labels = c("Przed", "Po")))

  list(pairs = pairs, long_pairs = long_pairs, ind_data = ind_data)
})

# ============================================================================
# SERVER
# ============================================================================

ch6_server <- function(input, output, session) {

  # Shared independent data
  ch6_ind_data_state <- reactiveVal(NULL)
  ch6_ind_data <- reactive({
    state <- ch6_ind_data_state()
    if (is.null(state)) return(NULL)
    req(input$ch6_ind_var, input$ch6_ind_n)

    if (!identical(state$var, input$ch6_ind_var) ||
        !isTRUE(state$n_per_group == input$ch6_ind_n)) {
      return(NULL)
    }

    state$data
  })
  ch6_ind_step <- reactiveVal(0)

  observeEvent(input$ch6_run_ind_t, {
    req(input$ch6_ind_var, input$ch6_ind_n)
    n <- input$ch6_ind_n
    data <- generate_student_data(n * 2)
    ch6_ind_data_state(list(
      var = input$ch6_ind_var,
      n_per_group = n,
      data = data
    ))
    ch6_ind_step(0)
  }, ignoreInit = TRUE)

  observeEvent(list(input$ch6_ind_var, input$ch6_ind_n), {
    ch6_ind_step(0)
  }, ignoreInit = TRUE)

  observeEvent(input$ch6_ind_step1, ch6_ind_step(1))
  observeEvent(input$ch6_ind_step2, ch6_ind_step(2))
  observeEvent(input$ch6_ind_step3, ch6_ind_step(3))
  observeEvent(input$ch6_ind_step4, ch6_ind_step(4))

  # Shared paired data
  ch6_paired_data_state <- reactiveVal(NULL)
  ch6_paired_data <- reactive({
    state <- ch6_paired_data_state()
    if (is.null(state)) return(NULL)
    req(input$ch6_paired_n, input$ch6_paired_effect)

    if (!isTRUE(state$n == input$ch6_paired_n) ||
        !isTRUE(state$effect == input$ch6_paired_effect)) {
      return(NULL)
    }

    state$data
  })

  observeEvent(input$ch6_run_paired, {
    req(input$ch6_paired_n, input$ch6_paired_effect)
    ch6_paired_data_state(list(
      n = input$ch6_paired_n,
      effect = input$ch6_paired_effect,
      data = generate_paired_data(input$ch6_paired_n, input$ch6_paired_effect)
    ))
  }, ignoreInit = TRUE)

  # --- Widget 1: Test t niezalezny ---
  ch6_ind_var_label <- function(var) {
    switch(var,
      "wzrost" = "Wzrost (cm)",
      "waga" = "Waga (kg)",
      "srednia_ocen" = "Średnia ocen",
      "czas_dojazdu" = "Czas dojazdu (min)",
      var
    )
  }

  ch6_ind_stats <- reactive({
    data <- ch6_ind_data()
    req(data)
    var <- input$ch6_ind_var
    formula <- as.formula(paste(var, "~ plec"))
    tidy_res <- as.data.frame(rstatix::t_test(data, formula))
    means <- data %>%
      dplyr::group_by(plec) %>%
      dplyr::summarise(
        n = dplyr::n(),
        m = mean(.data[[var]], na.rm = TRUE),
        s = sd(.data[[var]], na.rm = TRUE),
        .groups = "drop"
      )
    list(test = tidy_res, means = means)
  })

  output$ch6_ind_hypothesis <- renderUI({
    var_label <- tolower(ch6_ind_var_label(input$ch6_ind_var))
    lc_formula_box(
      p(tags$b("Hipoteza formalna (dwustronna):")),
      p(withMathJax("\\(H_0: \\mu_{K} = \\mu_{M}\\)"),
        " — średni ", var_label, " jest taki sam w obu grupach."),
      p(withMathJax("\\(H_a: \\mu_{K} \\neq \\mu_{M}\\)"),
        " — średni ", var_label, " różni się między grupami.")
    )
  })

  output$ch6_ind_boxplot <- renderPlot({
    data <- ch6_ind_data()
    if (is.null(data)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij „Losuj próbę”",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      var <- input$ch6_ind_var
      var_label <- ch6_ind_var_label(var)
      step <- ch6_ind_step()

      if (step == 0) {
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "Próba gotowa! Klikaj kroki po kolei.",
                   size = 5, color = upwr_reference) +
          theme_void()
      } else if (step <= 2) {
        p <- ggplot(data, aes(x = plec, y = .data[[var]], fill = plec)) +
          geom_boxplot(alpha = 0.45, outlier.alpha = 0.3, width = 0.5) +
          geom_jitter(width = 0.15, alpha = 0.35, size = 1.5) +
          scale_fill_manual(values = c(col_h0, col_reject)) +
          labs(
               x = "Płeć", y = var_label) +
          theme(legend.position = "none")

        if (step >= 2) {
          means <- ch6_ind_stats()$means
          means$hj <- ifelse(seq_len(nrow(means)) == 1, 1.15, -0.15)
          p <- p +
            stat_summary(fun = mean, geom = "point", shape = 23,
                         size = 4, fill = "white", color = upwr_secondary) +
            geom_text(
              data = means,
              aes(x = plec, y = m, label = paste0("x = ", round(m, 1)), hjust = hj),
              inherit.aes = FALSE,
              nudge_y = diff(range(data[[var]], na.rm = TRUE)) * 0.04,
              color = upwr_secondary,
              fontface = "bold"
            )
        }
        p
      } else if (step == 3) {
        st <- ch6_ind_stats()
        t_stat <- st$test$statistic
        df <- st$test$df
        x_seq <- seq(-4, 4, length.out = 500)
        y_seq <- dt(x_seq, df = df)
        plot_df <- data.frame(x = x_seq, y = y_seq)
        ggplot(plot_df, aes(x = x, y = y)) +
          geom_line(color = col_h0, linewidth = 1.2) +
          geom_vline(xintercept = t_stat, color = col_reject,
                     linewidth = 1.2, linetype = "dashed") +
          annotate("text", x = t_stat, y = max(y_seq) * 0.9,
                   label = paste0("t = ", round(t_stat, 3)),
                   hjust = if (t_stat > 0) -0.1 else 1.1,
                   color = col_reject, fontface = "bold") +
          labs(
               x = "Statystyka testowa", y = "Gęstość") +
          theme()
      } else {
        st <- ch6_ind_stats()
        plot_test_distribution(st$test$statistic, df = st$test$df, test_type = "t")
      }
    }
  })

  output$ch6_ind_result <- renderUI({
    data <- ch6_ind_data()
    step <- ch6_ind_step()
    if (is.null(data) || step == 0) return(NULL)

    var <- input$ch6_ind_var
    var_label <- tolower(ch6_ind_var_label(var))
    st <- ch6_ind_stats()
    tidy_res <- st$test
    means <- st$means
    higher <- means$plec[which.max(means$m)]
    lower <- means$plec[which.min(means$m)]
    diff_val <- round(max(means$m) - min(means$m), 2)
    res <- format_test_result(tidy_res$p)

    info <- switch(as.character(step),
      "1" = tagList(
        lc_stat_box("n", nrow(data), caption = "osób łącznie", color = col_h0),
        p("Każdy punkt to jedna osoba. Najpierw patrzymy, czy grupy wizualnie
          wyglądają na przesunięte względem siebie.")
      ),
      "2" = tagList(
        tags$table(class = "lc-table lc-table-bordered lc-table-sm",
          tags$thead(tags$tr(tags$th("Grupa"), tags$th("n"), tags$th(HTML("<span style='text-decoration:overline'>x</span>")), tags$th("s"))),
          tags$tbody(lapply(seq_len(nrow(means)), function(i) {
            tags$tr(
              tags$td(as.character(means$plec[i])),
              tags$td(means$n[i]),
              tags$td(round(means$m[i], 2)),
              tags$td(round(means$s[i], 2))
            )
          }))
        ),
        p("Różnica średnich w próbie wynosi ", tags$b(diff_val),
          ". Test pyta, czy taka różnica jest duża względem zmienności w grupach.")
      ),
      "3" = tagList(
        lc_stat_box("t", round(tidy_res$statistic, 3),
                    caption = paste0("df = ", round(tidy_res$df, 1)),
                    color = col_effect),
        p("Statystyka t to różnica średnich przeliczona na jednostki błędu
          standardowego. Im dalej od zera, tym bardziej skrajny wynik pod H₀.")
      ),
      "4" = tagList(
        p(tags$strong("Wynik testu t niezależnego:")),
        p(paste0("t(", round(tidy_res$df, 1), ") = ",
                 round(tidy_res$statistic, 3))),
        ui_p_value(tidy_res$p),
        p(style = paste0("color:", res$color, "; font-weight: bold;"),
          res$decision),
        if (tidy_res$p < 0.05) {
          p(tags$strong("Werdykt: "),
            "średnia ", var_label, " różni się istotnie między grupami — ",
            "w próbie była wyższa w grupie ", tags$b(as.character(higher)),
            " niż ", tags$b(as.character(lower)),
            " o ", tags$b(diff_val), ".")
        } else {
          p(tags$strong("Werdykt: "),
            "nie ma podstaw, by twierdzić, że średnia ", var_label,
            " różni się między grupami. Obserwowana w próbie różnica ",
            tags$b(diff_val),
            " (na korzyść grupy ", tags$b(as.character(higher)),
            ") mieści się w zakresie wahań losowych.")
        }
      )
    )
    lc_feedback(type = "info", info)
  })

  # --- Widget 2: Test t parowy ---
  output$ch6_paired_plot <- renderPlot({
    data <- ch6_paired_data()
    if (is.null(data)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Generuj i testuj'",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      # Connected dot plot
      long <- data %>%
        pivot_longer(cols = c(wynik_przed, wynik_po),
                     names_to = "moment", values_to = "wynik") %>%
        mutate(moment = factor(moment,
                               levels = c("wynik_przed", "wynik_po"),
                               labels = c("Przed", "Po")))

      ggplot(long, aes(x = moment, y = wynik)) +
        geom_line(aes(group = student), alpha = 0.3, color = col_paired) +
        geom_point(aes(color = moment), size = 2.5, alpha = 0.7) +
        scale_color_manual(values = c(col_h0, col_reject)) +
        labs(
             x = "Moment", y = "Wynik") +
                theme(legend.position = "none")
    }
  })

  output$ch6_paired_result <- renderUI({
    data <- ch6_paired_data()
    if (is.null(data)) return(NULL)

    long <- data %>%
      pivot_longer(cols = c(wynik_przed, wynik_po),
                   names_to = "moment", values_to = "wynik")
    long$moment <- factor(long$moment,
                          levels = c("wynik_przed", "wynik_po"))

    result <- rstatix::t_test(long, wynik ~ moment, paired = TRUE)
    tidy_res <- as.data.frame(result)

    mean_diff <- mean(data$wynik_po - data$wynik_przed)
    res <- format_test_result(tidy_res$p)
    direction <- if (mean_diff > 0) "wzrosły" else if (mean_diff < 0) "spadły" else "nie zmieniły się"

    lc_feedback(type = "info",
      p(tags$strong("Wynik testu t dla danych sparowanych:")),
      p(paste0("Średnia różnica: ", round(mean_diff, 2), " pkt")),
      p(paste0("t(", tidy_res$df, ") = ", round(tidy_res$statistic, 3))),
      ui_p_value(tidy_res$p),
      p(style = paste0("color:", res$color, "; font-weight: bold;"),
        res$decision),
      if (tidy_res$p < 0.05) {
        p(tags$strong("Werdykt: "),
          "wyniki istotnie się zmieniły — średnio ", tags$b(direction),
          " o ", tags$b(round(abs(mean_diff), 2)), " pkt.")
      } else {
        p(tags$strong("Werdykt: "),
          "nie ma podstaw, by twierdzić, że wyniki się zmieniły. ",
          "Obserwowana w próbie zmiana (", tags$b(round(mean_diff, 2)),
          " pkt) mieści się w zakresie wahań losowych.")
      }
    )
  })

  # --- Widget: porownanie sparowany vs. niesparowany (Ryc. 8.3) ---

  output$ch6_compare_ind_plot <- renderPlot({
    d <- .ch6_compare$ind_data
    d$kolor <- factor(
      ifelse(d$typ == "dropout", "Brak kontroli (n=5)", as.character(d$grupa)),
      levels = c("Przed", "Po", "Brak kontroli (n=5)")
    )
    means <- d %>% group_by(grupa) %>% summarise(m = mean(wartosc), .groups = "drop")

    ggplot(d, aes(x = grupa, y = wartosc)) +
      geom_boxplot(aes(fill = grupa), alpha = 0.35, outlier.alpha = 0,
                   width = 0.45, color = "grey40") +
      geom_jitter(aes(color = kolor, shape = kolor),
                  width = 0.12, alpha = 0.8, size = 2.2) +
      geom_text(data = means,
                aes(y = m, label = paste0("x = ", round(m, 1)),
                    hjust = ifelse(as.integer(grupa) == 1, 1.15, -0.15)),
                nudge_y = 2, color = upwr_secondary, fontface = "bold", size = 3.5) +
      scale_fill_manual(values = c("Przed" = col_h0, "Po" = col_reject)) +
      scale_color_manual(values = c("Przed"             = col_h0,
                                    "Po"                = col_reject,
                                    "Brak kontroli (n=5)" = "#8B1A1A")) +
      scale_shape_manual(values = c("Przed" = 16, "Po" = 16,
                                    "Brak kontroli (n=5)" = 17)) +
      labs(x = NULL, y = "Ciśnienie skurczowe (mmHg)", color = NULL, shape = NULL) +
      guides(fill = "none") +
      theme(legend.position = "bottom", legend.text = element_text(size = 9))
  })

  output$ch6_compare_ind_result <- renderUI({
    d <- .ch6_compare$ind_data
    result <- rstatix::t_test(d, wartosc ~ grupa)
    res <- format_test_result(result$p)
    smry <- d %>% group_by(grupa) %>%
      summarise(n = n(), m = round(mean(wartosc), 1), s = round(sd(wartosc), 1),
                .groups = "drop")
    tagList(
      tags$table(class = "lc-table lc-table-bordered lc-table-sm",
        tags$thead(tags$tr(
          tags$th("Grupa"), tags$th("n"),
          tags$th(HTML("<span style='text-decoration:overline'>x</span>")),
          tags$th("s")
        )),
        tags$tbody(lapply(seq_len(nrow(smry)), function(i) {
          tags$tr(
            tags$td(as.character(smry$grupa[i])),
            tags$td(smry$n[i]),
            tags$td(smry$m[i]),
            tags$td(smry$s[i])
          )
        }))
      ),
      p(paste0("t(", round(result$df, 0), ") = ", round(result$statistic, 3))),
      ui_p_value(result$p),
      p(style = paste0("color:", res$color, "; font-weight: bold;"), res$decision)
    )
  })

  output$ch6_compare_paired_plot <- renderPlot({
    long <- .ch6_compare$long_pairs
    ggplot(long, aes(x = moment, y = cisnienie)) +
      geom_line(aes(group = id), alpha = 0.3, color = col_paired) +
      geom_point(aes(color = moment), size = 2.5, alpha = 0.8) +
      scale_color_manual(values = c("Przed" = col_h0, "Po" = col_reject)) +
      labs(x = NULL, y = "Ciśnienie skurczowe (mmHg)") +
      theme(legend.position = "none")
  })

  output$ch6_compare_paired_result <- renderUI({
    long <- .ch6_compare$long_pairs
    result <- rstatix::t_test(long, cisnienie ~ moment, paired = TRUE)
    res <- format_test_result(result$p)
    diffs <- .ch6_compare$pairs$po - .ch6_compare$pairs$przed
    tagList(
      tags$table(class = "lc-table lc-table-bordered lc-table-sm",
        tags$thead(tags$tr(
          tags$th("Miara"), tags$th("n"),
          tags$th(HTML("<span style='text-decoration:overline'>d</span>")),
          tags$th("s_d")
        )),
        tags$tbody(tags$tr(
          tags$td("po − przed"),
          tags$td(15),
          tags$td(round(mean(diffs), 2)),
          tags$td(round(sd(diffs), 2))
        ))
      ),
      p(paste0("t(14) = ", round(result$statistic, 3))),
      ui_p_value(result$p),
      p(style = paste0("color:", res$color, "; font-weight: bold;"), res$decision)
    )
  })

  # --- Cwiczenia CASchools ---

  .cas_t2samp <- function(x, grp) {
    grp <- as.factor(grp); lvls <- levels(grp)
    x1 <- x[grp == lvls[1]]; x2 <- x[grp == lvls[2]]
    n1 <- length(x1); n2 <- length(x2)
    m1 <- mean(x1); m2 <- mean(x2); s1 <- sd(x1); s2 <- sd(x2)
    se <- sqrt(s1^2/n1 + s2^2/n2)
    t_val <- (m1 - m2) / se
    df <- (s1^2/n1 + s2^2/n2)^2 /
          ((s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1))
    p_val <- 2 * pt(-abs(t_val), df)
    sp <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2) / (n1+n2-2))
    d <- (m1 - m2) / sp
    list(lvls=lvls, n1=n1, n2=n2, m1=m1, m2=m2, s1=s1, s2=s2,
         t=t_val, df=df, p=p_val, d=d)
  }

  cas_vis6 <- reactiveVal(FALSE)
  cas_vis7 <- reactiveVal(FALSE)

  observeEvent(input$cas_ch6_ans6, {
    nowy <- !cas_vis6()
    cas_vis6(nowy)
    updateActionButton(session, "cas_ch6_ans6",
      label = if (nowy) "Ukryj rozwiązanie" else "Pokaż rozwiązanie")
  }, ignoreInit = TRUE)

  output$cas_ch6_sol6 <- renderUI({
    if (!cas_vis6()) return(NULL)
    df2 <- .ch6_cas[!is.na(.ch6_cas$read) & !is.na(.ch6_cas$grades), ]
    r <- .cas_t2samp(df2$read, df2$grades)
    lc_feedback(type = "ok", style = "margin-top: 10px;",
      p(tags$b("H₀: "), "μ(KK-06) = μ(KK-08) · ",
        tags$b("Hₐ: "), "μ(KK-06) ≠ μ(KK-08)"),
      tags$ul(
        tags$li(sprintf("%s: n = %d, x̄ = %.2f, s = %.2f",
                        r$lvls[1], r$n1, r$m1, r$s1)),
        tags$li(sprintf("%s: n = %d, x̄ = %.2f, s = %.2f",
                        r$lvls[2], r$n2, r$m2, r$s2)),
        tags$li(sprintf("t(%s) = %.3f, p %s %s",
          round(r$df, 1), r$t,
          if (r$p < 0.001) "<" else "=",
          if (r$p < 0.001) "0.001" else format(round(r$p, 4), nsmall = 4))),
      ),
      if (r$p < 0.05) tags$b(style = paste0("color:", upwr_accent), "Odrzucamy H₀")
      else tags$b("Brak podstaw do odrzucenia H₀"),
      p(tags$b("Interpretacja: "),
        sprintf(
          "Różnica %.2f pkt jest %s (p %s 0.05).",
          abs(r$m1 - r$m2),
          if (r$p < 0.05) "istotna" else "nieistotna",
          if (r$p < 0.05) "<" else ">"
        ))
    )
  })

  observeEvent(input$cas_ch6_ans7, {
    nowy <- !cas_vis7()
    cas_vis7(nowy)
    updateActionButton(session, "cas_ch6_ans7",
      label = if (nowy) "Ukryj rozwiązanie" else "Pokaż rozwiązanie")
  }, ignoreInit = TRUE)

  output$cas_ch6_sol7 <- renderUI({
    if (!cas_vis7()) return(NULL)
    high_str <- .ch6_cas$student_teacher_ratio > 20
    r <- .cas_t2samp(.ch6_cas$read, high_str)
    m_lo <- .ch6_cas$read[!high_str]; m_hi <- .ch6_cas$read[high_str]
    lc_feedback(type = "ok", style = "margin-top: 10px;",
      p(tags$b("H₀: "), "μ(STR ≤ 20) = μ(STR > 20) · ",
        tags$b("Hₐ: "), "μ(STR ≤ 20) ≠ μ(STR > 20)"),
      tags$ul(
        tags$li(sprintf("STR ≤ 20: n = %d, x̄ = %.2f",
                        sum(!high_str), mean(m_lo))),
        tags$li(sprintf("STR > 20: n = %d, x̄ = %.2f",
                        sum(high_str), mean(m_hi))),
        tags$li(sprintf("Różnica: %.2f pkt",
                        mean(m_lo) - mean(m_hi))),
        tags$li(sprintf("t(%s) = %.3f, p %s %s",
          round(r$df, 1), r$t,
          if (r$p < 0.001) "<" else "=",
          if (r$p < 0.001) "0.001" else format(round(r$p, 4), nsmall = 4))),
      ),
      if (r$p < 0.05) tags$b(style = paste0("color:", upwr_accent), "Odrzucamy H₀")
      else tags$b("Brak podstaw do odrzucenia H₀"),
      p(tags$b("Uwaga: "),
        "STR > 20 to często okręgi biedniejsze. Różnica może być częściowo
        konfundowana dochodem — by to zbadać, potrzeba analizy regresji
        z kontrolą zmiennych towarzyszących.")
    )
  })

}
