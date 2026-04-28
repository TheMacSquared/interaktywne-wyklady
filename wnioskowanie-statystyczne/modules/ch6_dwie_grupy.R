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
        note = "Test parowy (nie niezależny!) — te same osoby mierzone dwa razy. Analizujemy różnice."
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
    lc_h2("ch6-parowy", "Test t dla prób zależnych (parowy)"),

    tagList(
      p("Gdy mierzymy tych samych osobników dwa razy
        (przed i po interwencji), używamy testu parowego."),
      p("Przykład: wyniki studentów przed i po korepetycjach."),
      p("Testujemy różnice: ", withMathJax("\\(d_i = x_{\\text{po},i} - x_{\\text{przed},i}\\)"),
        ". Pytamy, czy średnia różnic ≠ 0.")
    ),

    figure_panel(
      label = "Ryc. 8.2",
      title = "Test parowy: przed i po",
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

    margin_callout(
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
        " różnią się między grupami. Wykonaj test t dla prób niezależnych i oblicz
        Cohen's d. Jak duży jest efekt? Czy różnica jest edukacyjnie znacząca?"),
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

# ============================================================================
# SERVER
# ============================================================================

ch6_server <- function(input, output, session) {

  # Shared independent data
  ch6_ind_data <- reactiveVal(NULL)
  ch6_ind_step <- reactiveVal(0)

  observeEvent(input$ch6_run_ind_t, {
    n <- input$ch6_ind_n
    data <- generate_student_data(n * 2)
    ch6_ind_data(data)
    ch6_ind_step(0)
  })

  observeEvent(input$ch6_ind_var, {
    ch6_ind_data(NULL)
    ch6_ind_step(0)
  })

  observeEvent(input$ch6_ind_step1, ch6_ind_step(1))
  observeEvent(input$ch6_ind_step2, ch6_ind_step(2))
  observeEvent(input$ch6_ind_step3, ch6_ind_step(3))
  observeEvent(input$ch6_ind_step4, ch6_ind_step(4))

  # Shared paired data
  ch6_paired_data <- reactiveVal(NULL)

  observeEvent(input$ch6_run_paired, {
    ch6_paired_data(generate_paired_data(input$ch6_paired_n, input$ch6_paired_effect))
  })

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
    d_val <- as.data.frame(rstatix::cohens_d(data, formula))$effsize
    means <- data %>%
      dplyr::group_by(plec) %>%
      dplyr::summarise(
        n = dplyr::n(),
        m = mean(.data[[var]], na.rm = TRUE),
        s = sd(.data[[var]], na.rm = TRUE),
        .groups = "drop"
      )
    list(test = tidy_res, d = d_val, means = means)
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
          labs(title = paste0(var_label, " według płci"),
               x = "Płeć", y = var_label) +
          theme(legend.position = "none")

        if (step >= 2) {
          means <- ch6_ind_stats()$means
          p <- p +
            stat_summary(fun = mean, geom = "point", shape = 23,
                         size = 4, fill = "white", color = upwr_secondary) +
            geom_text(
              data = means,
              aes(x = plec, y = m, label = paste0("x̄ = ", round(m, 1))),
              inherit.aes = FALSE,
              nudge_y = diff(range(data[[var]], na.rm = TRUE)) * 0.08,
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
          labs(title = paste0("Rozkład pod H0: t(", round(df, 1), ")"),
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
    d_val <- st$d
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
          tags$thead(tags$tr(tags$th("Grupa"), tags$th("n"), tags$th("x̄"), tags$th("s"))),
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
        p(paste0("p = ", format.pval(tidy_res$p, digits = 4))),
        p(paste0("Cohen's d = ", round(d_val, 3),
                 " (efekt ", effect_size_label(d_val), ")")),
        p(tags$em(interpret_cohens_d(d_val))),
        p(style = paste0("color:", res$color, "; font-weight: bold;"),
          res$decision),
        p(tags$strong("Werdykt: "),
          "średnia ", var_label, " w grupie ", tags$b(as.character(higher)),
          " była wyższa od grupy ", tags$b(as.character(lower)),
          " o ", tags$b(diff_val), ".")
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
        labs(title = "Wyniki przed i po interwencji",
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

    d_val <- mean(data$wynik_po - data$wynik_przed) / sd(data$wynik_po - data$wynik_przed)
    mean_diff <- mean(data$wynik_po - data$wynik_przed)
    res <- format_test_result(tidy_res$p)
    direction <- if (mean_diff > 0) "wzrosły" else if (mean_diff < 0) "spadły" else "nie zmieniły się"

    lc_feedback(type = "info",
      p(tags$strong("Wynik testu t parowego:")),
      p(paste0("Średnia różnica: ", round(mean_diff, 2), " pkt")),
      p(paste0("t(", tidy_res$df, ") = ", round(tidy_res$statistic, 3))),
      p(paste0("p = ", format.pval(tidy_res$p, digits = 4))),
      p(paste0("Cohen's d = ", round(d_val, 3),
               " (efekt ", effect_size_label(d_val), ")")),
      p(tags$em(interpret_cohens_d(d_val))),
      p(style = paste0("color:", res$color, "; font-weight: bold;"),
        res$decision),
      p(tags$strong("Werdykt: "),
        "wyniki średnio ", tags$b(direction), " o ", tags$b(round(abs(mean_diff), 2)), " pkt.")
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
        tags$li(sprintf("Cohen's d = %.3f (%s efekt)", r$d, effect_size_label(r$d)))
      ),
      if (r$p < 0.05) tags$b(style = paste0("color:", upwr_accent), "Odrzucamy H₀")
      else tags$b("Brak podstaw do odrzucenia H₀"),
      p(tags$b("Interpretacja: "),
        sprintf(
          "Różnica %.2f pkt jest %s (p %s 0.05). Efekt %s (d = %.3f).
           Pamiętaj: istotność statystyczna ≠ istotność praktyczna.",
          abs(r$m1 - r$m2),
          if (r$p < 0.05) "istotna" else "nieistotna",
          if (r$p < 0.05) "<" else ">",
          effect_size_label(r$d), r$d
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
        tags$li(sprintf("Cohen's d = %.3f (%s efekt)", abs(r$d), effect_size_label(r$d)))
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
