# ============================================================================
# CHAPTER 7: ANOVA
# ============================================================================

ch7_ui <- list(
  id = "ch-anova", num = "09", title = "ANOVA",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 09 · Testowanie hipotez",
      num    = "09",
      title  = "ANOVA.",
      lead   = "„Czy średnia cena posiłku różni się między Spiżem, budką z knyszą a Pasibusem?”
                Gdy grup jest trzy lub więcej, analiza wariancji porównuje wszystkie jednym
                testem — bez inflacji błędu I rodzaju."
    ),

    # ========================================================================
    # SEKCJA 1: Motywacja — dlaczego nie kilka testów t?
    # ========================================================================
    lc_h2("ch7-motywacja", "Dlaczego nie kilka testów t?"),

    tagList(
      p("Mamy trzy grupy i chcemy porównać ich średnie. Naturalny odruch:
        przeprowadzić trzy testy t — A vs B, A vs C, B vs C — i zobaczyć,
        które pary się różnią. Dlaczego to zły pomysł?"),
      p("Każdy test t przeprowadzany przy ", withMathJax("\\(\\alpha = 0{,}05\\)"),
        " dopuszcza 5% ryzyka ", tags$em("fałszywego alarmu"),
        " — odrzucenia H₀ gdy w rzeczywistości różnicy nie ma.
        Gdy testów jest więcej, to ryzyko ", tags$b("składa się"),
        " — pojawia się tzw. inflacja błędu I rodzaju:"),
      tags$table(class = "lc-table lc-table-bordered", style = "font-size: 15px; margin: 10px 0;",
        tags$thead(tags$tr(tags$th("Liczba grup"), tags$th("Liczba par"),
                           tags$th("Ryzyko ≥ 1 fałszywego alarmu"))),
        tags$tbody(
          tags$tr(tags$td("3"), tags$td("3"), tags$td("~14%")),
          tags$tr(tags$td("4"), tags$td("6"), tags$td("~26%")),
          tags$tr(tags$td("5"), tags$td("10"), tags$td("~40%"))
        )
      ),
      p("Przy 5 grupach w dwóch na pięciu analizach wyjdzie „istotna różnica”,
        której tam ", tags$em("nie ma"), ". Potrzebujemy jednego testu,
        który odpowiada na pytanie „czy w ogóle coś się różni między grupami?”
        z kontrolowanym ryzykiem — to właśnie ", tags$b("ANOVA"), ".")
    ),

    # ========================================================================
    # SEKCJA 2: Wprowadzenie ANOVA
    # ========================================================================
    lc_h2("ch7-intro", "ANOVA jednoczynnikowa"),

    tagList(
      p("ANOVA (Analysis of Variance) to uogólnienie testu t na 3 lub więcej grup.
        Zadaje ", tags$b("jedno"), " pytanie: czy średnie w k grupach różnią się między sobą?"),
      p("Przykład: czy pH jogurtu różni się między trzema temperaturami fermentacji (20°C, 25°C, 30°C)?"),
      lc_formula_box(
        p(withMathJax("\\(H_0: \\mu_1 = \\mu_2 = \\ldots = \\mu_k\\) — wszystkie średnie są równe")),
        p(withMathJax("\\(H_a:\\) co najmniej jedna średnia różni się od pozostałych"))
      ),
      p("ANOVA działa dzięki ", tags$b("dekompozycji wariancji"), ":
        całkowitą zmienność danych dzieli na dwie części — zmienność ",
        tags$em("między"), " grupami (różnice średnich) i zmienność ",
        tags$em("wewnątrz"), " grup (naturalne rozrzuty). Statystyka F porównuje te dwie części:"),
      p("Duże F = różnice między grupami są większe niż moglibyśmy oczekiwać z samego
        wewnątrzgrupowego szumu. Jeśli F jest dostatecznie duże, p-wartość spada poniżej α
        i odrzucamy H₀. Odrzucenie mówi, że ", tags$em("co najmniej jedna"),
        " średnia odstaje — ale nie mówi, ", tags$b("która"), ". Do tego służy test post-hoc (niżej).")
    ),

    figure_panel(
      label = "Ryc. 9.1",
      title = "Intuicja ANOVA: sygnał kontra szum",
      fluidRow(
        column(4,
          sliderInput("ch7_int_between", "Różnice między grupami:",
                      min = 0, max = 8, value = 3, step = 0.5),
          sliderInput("ch7_int_within", "Rozrzut wewnątrz grup:",
                      min = 0.5, max = 5, value = 1.6, step = 0.1),
          uiOutput("ch7_int_stats")
        ),
        column(8,
          div(class = "ws-chart-wrap",
            tags$canvas(id = "ch7_anova_signal_chart")
          )
        )
      )
    ),

    # ========================================================================
    # Cwiczenie: sformuluj hipotezy
    # ========================================================================
    lc_h2("ch7-cwiczenie", "Ćwiczenie: sformułuj hipotezy"),

    tagList(
      p("ANOVA ma zawsze jedną H₀ i jedną Hₐ, niezależnie od liczby grup.
        Sprawdź, czy potrafisz je zapisać dla poniższych sytuacji.")
    ),

    hypothesis_practice("ch7", list(
      list(
        question = "Czy średnia cena posiłku różni się między Spiżem,
                    budką z knyszą a Pasibusem?",
        h0 = "\\(H_0: \\mu_1 = \\mu_2 = \\mu_3\\) (Spiż, knysza, Pasibus mają tę samą średnią cenę)",
        ha = "\\(H_a:\\) co najmniej jedna średnia różni się od pozostałych",
        note = "ANOVA jest zawsze „dwustronna” w tym sensie, że wykrywa dowolne odchylenie jednej grupy. Nie mówi która — do tego post-hoc."
      ),
      list(
        question = "Technolog testuje cztery metody pasteryzacji mleka. Zmienna:
                    liczba kolonii bakterii po 7 dniach.",
        h0 = "\\(H_0: \\mu_1 = \\mu_2 = \\mu_3 = \\mu_4\\)",
        ha = "\\(H_a:\\) co najmniej jedna średnia jest różna",
        note = "4 grupy — 6 par porównań. Bez ANOVA musielibyśmy zrobić 6 testów t i ryzyko fałszywego alarmu wzrosłoby do ~26%."
      ),
      list(
        question = "Porównujemy średni czas dojazdu do pracy w trzech miastach:
                    Warszawa, Kraków, Wrocław.",
        h0 = "\\(H_0: \\mu_1 = \\mu_2 = \\mu_3\\) (Warszawa, Kraków, Wrocław — ten sam średni czas)",
        ha = "\\(H_a:\\) co najmniej jedna średnia jest różna",
        note = "Jeśli ANOVA odrzuci H₀, trzeba sprawdzić post-hoc (Games-Howell) które miasta konkretnie się różnią."
      )
    )),

    # ========================================================================
    # WIDGET 1: ANOVA jednoczynnikowa
    # ========================================================================
    lc_h2("ch7-akcja", "ANOVA w akcji"),

    figure_panel(
      label = "Ryc. 9.2",
      title = "ANOVA jednoczynnikowa",
      fluidRow(
        column(4,
          selectInput("ch7_scenario", "Scenariusz:",
            choices = c(
              "Fermentacja jogurtu (TŻ)" = "fermentation",
              "Kierunki studiów" = "students",
              "Stanowisko pracy a wypadki/stres (IB)" = "workplace"
            ),
            selected = "fermentation"
          ),
          uiOutput("ch7_var_ui"),
          sliderInput("ch7_n", "n (ogółem):",
                      min = 80, max = 300, value = 160, step = 20),
          actionButton("ch7_run_anova", "Generuj i testuj",
                       class = "lc-btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch7_boxplot", height = "350px"),
          uiOutput("ch7_anova_result")
        )
      )
    ),

    # ========================================================================
    # Intuicja eta^2
    # ========================================================================
    lc_h2("ch7-eta", "Jak czytać siłę efektu (η²)?"),

    tagList(
      p("P-wartość mówi ", tags$em("czy"), " różnice istnieją, ale nie mówi, ",
        tags$em("jak duże"), " są. ", tags$b("η² (eta kwadrat)"),
        " odpowiada na to drugie pytanie — mówi, jaki ułamek całej zmienności
        wyjaśnia czynnik grupujący. Innymi słowy: ile procent różnic między
        próbkami tłumaczy ten czynnik, a ile zostaje na „inne rzeczy”?"),
      tags$table(class = "lc-table lc-table-bordered", style = "font-size: 15px; margin: 10px 0;",
        tags$thead(
          tags$tr(tags$th("η²"), tags$th("Jak to czytać"), tags$th("Efekt"))
        ),
        tags$tbody(
          tags$tr(tags$td("0,01"), tags$td("~1% zmienności wyjaśnione — wpływ symboliczny"), tags$td("mały")),
          tags$tr(tags$td("0,06"), tags$td("~6% zmienności wyjaśnione — czynnik liczy się"), tags$td("średni")),
          tags$tr(tags$td("0,14"), tags$td("~14% zmienności wyjaśnione — czynnik dominujący"), tags$td("duży"))
        )
      ),
      p(tags$b("Przykład (fermentacja):"),
        " η² = 0,23 oznacza, że około 23% różnic w pH jogurtu tłumaczy temperatura fermentacji.
        Pozostałe 77% to inne czynniki: drobnoustroje startowe, czystość surowca, czas, partia mleka.")
    ),

    inline_callout(
      label = "Uwaga praktyczna",
      tagList(
        "małe p przy małym η² oznacza często tylko ", tags$em("dużą próbę"),
        ", nie duży efekt. Zawsze raportuj ", tags$b("oba"), "."
      )
    ),

    # ========================================================================
    # WIDGET 2: Post-hoc Games-Howell
    # ========================================================================
    lc_h2("ch7-posthoc", "Testy post-hoc (Games-Howell)"),

    tagList(
      p("ANOVA mówi „grupy różnią się”, ale nie mówi „które”. Wracamy więc
        do porównań parami — ale tym razem z kontrolowanym ryzykiem fałszywego alarmu
        na poziomie całej rodziny testów, a nie pojedynczego testu."),
      p(tags$b("Games-Howell"), " porównuje każdą parę grup z korektą na wielokrotne
        porównania — trzyma ryzyko błędu I rodzaju na 5% dla ", tags$b("całej rodziny"),
        " porównań, niezależnie od liczby par. Dodatkowo nie wymaga równych wariancji
        w grupach, więc jest bezpiecznym wyborem domyślnym.")
    ),

    inline_callout(
      label = "Ważne",
      tagList(
        "Testy post-hoc wykonujemy ", tags$b("tylko"), " gdy ANOVA jest istotna.
         Bez tego korekcja na wielokrotne porównania jest niepotrzebna."
      ),
      color = "uwaga"
    ),

    margin_code_note(
      code = "jamovi: One-Way ANOVA\n→ Post-Hoc Tests\n→ ✓ Games-Howell",
      description = "Ścieżka w jamovi dla testu post-hoc po ANOVA."
    ),

    figure_panel(
      label = "Ryc. 9.3",
      title = "Games-Howell",
      helpText("Używa danych z ANOVA powyżej. Najpierw uruchom ANOVA!"),
      actionButton("ch7_run_tukey", "Testuj Games-Howellem",
                   class = "lc-btn-warning"),
      br(), br(),

      h5("Macierz p-wartości"),
      p(class = "text-muted",
        style = "font-size: 13px; margin-top: -4px;",
        "Tak wygląda tabela post-hoc w jamovi — odczytaj p-wartość dla każdej pary grup."),
      uiOutput("ch7_tukey_matrix"),

      br(),
      h5("Różnice parowe z 95% CI"),
      plotOutput("ch7_tukey_plot", height = "260px"),

      uiOutput("ch7_tukey_result")
    ),

    lc_h2("ch7-cas", "Ćwiczenia", "CASchools — ANOVA"),

    lc_feedback(type = "info",
      p(tags$b("Dane: "), "420 okręgów szkolnych Kalifornii (1998–1999). Plik: ",
        tags$code("dane/caschools.csv"), "."),
      p("Zmienne w zadaniu: ", tags$code("read"),
        " (wyniki z czytania), ", tags$code("income"),
        " (dochód okręgu, tys. USD).")
    ),

    figure_panel(label = "Ćwiczenie",
      h4("Zadanie 10 — Czy wyniki czytania różnią się między tercylami dochodu?"),
      p("Podziel okręgi na trzy równe grupy dochodowe (tercyle): ",
        tags$b("niski / średni / wysoki"),
        " (użyj ", tags$em("split into groups"), " w Jamovi lub utwórz zmienną
        ręcznie na podstawie kwantyli 0, 1/3, 2/3, 1).
        Wykonaj jednoczynnikową ANOVA dla zmiennej ", tags$code("read"),
        " między grupami. Zapisz: F, df, p, η².
        Wykonaj post-hoc Games-Howell i wskaż, które pary różnią się istotnie."),
      actionButton("cas_ch7_ans10", "Pokaż rozwiązanie",
                   class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("cas_ch7_sol10")
    ),

    lc_chapter_next(
      num       = "10",
      title     = "Drzewo decyzyjne",
      lead      = "mapa wyboru testu — od typu zmiennych do konkretnego testu.",
      target_id = "ch-drzewo"
    )
  )
)

# ============================================================================
# DANE — CASchools (wczytane raz przy ladowaniu modulu)
# ============================================================================

.ch7_cas <- read.csv(file.path(app_dir, "dane", "caschools.csv"),
                     stringsAsFactors = FALSE)

# ============================================================================
# SERVER
# ============================================================================

ch7_server <- function(input, output, session) {

  ch7_int_data <- reactive({
    set.seed(404)
    n_per_group <- 35
    between <- input$ch7_int_between
    within <- input$ch7_int_within
    groups <- factor(rep(c("A", "B", "C"), each = n_per_group),
                     levels = c("A", "B", "C"))
    means <- c(0, between / 2, between)
    y <- rnorm(length(groups), mean = rep(means, each = n_per_group), sd = within)
    data.frame(grupa = groups, wynik = y)
  })

  observe({
    req(input$ch7_int_between, input$ch7_int_within)
    session$sendCustomMessage("ws_anova_signal_chart", list(
      id = "ch7_anova_signal_chart",
      between = input$ch7_int_between,
      within = input$ch7_int_within
    ))
  })

  output$ch7_int_plot <- renderPlot({
    d <- ch7_int_data()
    group_means <- d %>%
      dplyr::group_by(grupa) %>%
      dplyr::summarise(m = mean(wynik), .groups = "drop")

    ggplot(d, aes(x = grupa, y = wynik, fill = grupa)) +
      geom_boxplot(alpha = 0.35, outlier.shape = NA, width = 0.55) +
      geom_jitter(width = 0.14, alpha = 0.35, size = 1.8) +
      geom_point(data = group_means, aes(y = m),
                 shape = 23, size = 4, fill = "white", color = upwr_secondary) +
      geom_hline(yintercept = mean(d$wynik), linetype = "dashed",
                 color = upwr_reference) +
      scale_fill_upwr() +
      labs(
           
           x = "Grupa", y = "Wynik") +
      theme(legend.position = "none")
  })

  output$ch7_int_stats <- renderUI({
    d <- ch7_int_data()
    fit <- aov(wynik ~ grupa, data = d)
    s <- summary(fit)[[1]]
    f_val <- s[["F value"]][1]
    p_val <- s[["Pr(>F)"]][1]
    eta2 <- s[["Sum Sq"]][1] / sum(s[["Sum Sq"]])

    tagList(
      lc_stat_box("F", round(f_val, 2), color = col_effect),
      lc_stat_box("p", format.pval(p_val, digits = 3), color = col_pvalue),
      lc_stat_box("η²", round(eta2, 2),
                  caption = paste0(round(100 * eta2), "% zmienności wyjaśnione"),
                  color = col_accept),
      lc_feedback(type = "info",
        p("ANOVA robi dokładnie to porównanie: ",
          tags$b("zmienność między grupami"),
          " dzieli przez ",
          tags$b("zmienność wewnątrz grup"),
          ". Gdy sygnał rośnie albo szum maleje, F zwykle rośnie.")
      )
    )
  })

  # Shared ANOVA data
  ch7_data_state <- reactiveVal(NULL)
  ch7_data <- reactive({
    state <- ch7_data_state()
    if (is.null(state)) return(NULL)
    req(input$ch7_scenario, input$ch7_n)

    if (!identical(state$scenario, input$ch7_scenario) ||
        !isTRUE(state$n == input$ch7_n)) {
      return(NULL)
    }

    state$data
  })

  # Konfiguracja scenariuszy: jakie zmienne zależne, jaka kolumna grupująca, etykiety
  ch7_scenario_cfg <- function(scenario) {
    if (identical(scenario, "fermentation")) {
      list(
        group_col = "temperatura",
        group_label = "Temperatura fermentacji",
        vars = c("pH jogurtu" = "pH", "Kwasowość miareczkowa (°SH)" = "kwasowosc_SH"),
        var_labels = c(pH = "pH jogurtu", kwasowosc_SH = "Kwasowość (°SH)")
      )
    } else if (identical(scenario, "workplace")) {
      list(
        group_col = "stanowisko",
        group_label = "Stanowisko pracy",
        vars = c("Dni nieobecności (urazy) / rok" = "nieobecnosci",
                 "Poziom stresu (0–100)" = "stres"),
        var_labels = c(nieobecnosci = "Dni nieobecności / rok",
                       stres = "Poziom stresu (0–100)")
      )
    } else {
      list(
        group_col = "kierunek",
        group_label = "Kierunek",
        vars = c("Średnia ocen" = "srednia_ocen", "Wzrost" = "wzrost", "Czas dojazdu" = "czas_dojazdu"),
        var_labels = c(srednia_ocen = "Średnia ocen", wzrost = "Wzrost (cm)", czas_dojazdu = "Czas dojazdu (min)")
      )
    }
  }

  output$ch7_var_ui <- renderUI({
    cfg <- ch7_scenario_cfg(input$ch7_scenario)
    selectInput("ch7_var", "Zmienna zależna:",
                choices = cfg$vars,
                selected = unname(cfg$vars[1]))
  })

  observeEvent(input$ch7_run_anova, {
    req(input$ch7_scenario, input$ch7_n)
    data <- if (identical(input$ch7_scenario, "fermentation")) {
      generate_fermentation_data(input$ch7_n)
    } else if (identical(input$ch7_scenario, "workplace")) {
      generate_workplace_data(input$ch7_n)
    } else {
      generate_student_data(input$ch7_n)
    }

    ch7_data_state(list(
      scenario = input$ch7_scenario,
      n = input$ch7_n,
      data = data
    ))
  }, ignoreInit = TRUE)

  # --- Widget 1: ANOVA ---
  output$ch7_boxplot <- renderPlot({
    data <- ch7_data()
    if (is.null(data)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Generuj i testuj'",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      cfg <- ch7_scenario_cfg(input$ch7_scenario)
      var <- input$ch7_var
      req(var %in% names(data))
      var_label <- cfg$var_labels[[var]]
      group_col <- cfg$group_col

      ggplot(data, aes(x = .data[[group_col]], y = .data[[var]], fill = .data[[group_col]])) +
        geom_boxplot(alpha = 0.6, outlier.alpha = 0.3) +
        geom_jitter(width = 0.15, alpha = 0.2, size = 1) +
        scale_fill_upwr() +
        labs(
             x = cfg$group_label, y = var_label) +
                theme(legend.position = "none")
    }
  })

  output$ch7_anova_result <- renderUI({
    data <- ch7_data()
    if (is.null(data)) return(NULL)

    cfg <- ch7_scenario_cfg(input$ch7_scenario)
    var <- input$ch7_var
    req(var %in% names(data))
    formula <- as.formula(paste(var, "~", cfg$group_col))

    result <- rstatix::anova_test(data, formula)
    tidy_res <- as.data.frame(result)

    # Eta-squared jest w wyniku rstatix (generalized eta squared)
    eta_sq <- tidy_res$ges

    # Etykieta efektu dla η² (progi Cohena: 0.01 mały, 0.06 średni, 0.14 duży)
    eta_label <- if (eta_sq < 0.01) "pomijalny"
      else if (eta_sq < 0.06) "mały"
      else if (eta_sq < 0.14) "średni"
      else "duży"

    p_val <- tidy_res$p
    res <- format_test_result(p_val)

    lc_feedback(type = "info",
      p(tags$strong("Wynik ANOVA jednoczynnikowej:")),
      p(paste0("F(", tidy_res$DFn, ", ", tidy_res$DFd, ") = ",
               round(tidy_res$F, 3))),
      p(paste0("p = ", format.pval(p_val, digits = 4))),
      p(paste0("η² = ", round(eta_sq, 3),
               " (efekt ", eta_label, ", ok. ",
               round(eta_sq * 100), "% zmienności wyjaśnione)")),
      p(style = paste0("color:", res$color, "; font-weight: bold;"),
        res$decision)
    )
  })

  # --- Widget 2: Games-Howell post-hoc ---

  # Pomocnicza: licz raz, podawaj do plot/matrix/result
  ch7_gh_data <- reactive({
    req(input$ch7_run_tukey)
    data <- isolate(ch7_data())
    if (is.null(data)) return(NULL)
    cfg <- ch7_scenario_cfg(isolate(input$ch7_scenario))
    var <- isolate(input$ch7_var)
    req(var %in% names(data))
    formula <- as.formula(paste(var, "~", cfg$group_col))
    list(
      gh     = as.data.frame(rstatix::games_howell_test(data, formula)),
      groups = levels(factor(data[[cfg$group_col]]))
    )
  })

  # Macierz p-wartości (format jamovi: dolno-trójkątna, z gwiazdkami istotności)
  output$ch7_tukey_matrix <- renderUI({
    gd <- ch7_gh_data()
    if (is.null(gd)) return(NULL)
    groups <- gd$groups
    gh <- gd$gh
    k <- length(groups)

    # Pretty-print p (z gwiazdką przy istotnym)
    fmt_p <- function(p) {
      stars <- if (p < 0.001) " ***" else if (p < 0.01) " **" else if (p < 0.05) " *" else ""
      txt <- if (p < 0.001) "< 0,001" else sprintf("%.3f", p)
      list(txt = txt, stars = stars, sig = p < 0.05)
    }

    # Budujemy wiersze: pierwsza kolumna to nazwa grupy (wiersz),
    # potem po jednej komórce na każdą grupę wcześniejszą (dolno-trójkątna)
    rows <- lapply(seq_len(k), function(i) {
      cells <- lapply(seq_len(k), function(j) {
        if (j > i) {
          tags$td(style = "background: transparent; border: none;", "")
        } else if (j == i) {
          tags$td(style = "background: var(--upwr-surface-sunken); color: var(--upwr-reference); text-align: center;", "—")
        } else {
          # szukamy pary (groups[j], groups[i]) w gh_df
          idx <- which((gh$group1 == groups[j] & gh$group2 == groups[i]) |
                       (gh$group1 == groups[i] & gh$group2 == groups[j]))
          if (length(idx) == 0) return(tags$td(""))
          p  <- gh$p.adj[idx[1]]
          fp <- fmt_p(p)
          bg <- if (fp$sig) "var(--upwr-accent-tint)" else "var(--upwr-surface)"
          color <- if (fp$sig) "var(--upwr-accent)" else "var(--upwr-ink-soft)"
          weight <- if (fp$sig) "600" else "400"
          tags$td(
            style = paste0("background:", bg, "; color: ", color,
                           "; font-weight: ", weight,
                           "; text-align: right; font-variant-numeric: tabular-nums;"),
            fp$txt, tags$span(style = "color: var(--upwr-accent);", fp$stars)
          )
        }
      })
      tags$tr(
        tags$th(style = "text-align: left;", groups[i]),
        cells
      )
    })

    tags$table(
      class = "lc-table lc-table-bordered",
      style = "font-size: 14px; max-width: 560px;",
      tags$thead(
        tags$tr(
          tags$th(""),
          lapply(groups, function(g) tags$th(style = "text-align: center;", g))
        )
      ),
      tags$tbody(rows),
      tags$caption(style = "caption-side: bottom; font-size: 12px; color: var(--upwr-reference);",
        "p-wartości skorygowane metodą Games-Howella. ",
        tags$b("*"), " p < 0,05    ", tags$b("**"), " p < 0,01    ",
        tags$b("***"), " p < 0,001"
      )
    )
  })

  output$ch7_tukey_plot <- renderPlot({
    gd <- ch7_gh_data()
    if (is.null(gd)) return(NULL)

    gh_df <- gd$gh
    gh_df$comparison <- paste0(gh_df$group1, " — ", gh_df$group2)
    gh_df$significant <- gh_df$p.adj < 0.05

    ggplot(gh_df, aes(x = estimate, y = comparison, color = significant)) +
      geom_point(size = 3) +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
      geom_vline(xintercept = 0, linetype = "dashed", color = upwr_secondary) +
      scale_color_manual(values = c("TRUE" = col_reject, "FALSE" = col_accept),
                         labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
                         name = NULL) +
      labs(x = "Różnica średnich", y = NULL) +
      theme(legend.position = "top")
  })

  output$ch7_tukey_result <- renderUI({
    gd <- ch7_gh_data()
    if (is.null(gd)) {
      return(lc_feedback(type = "warning", "Najpierw uruchom ANOVA."))
    }
    gh_df <- gd$gh
    sig_pairs <- gh_df[gh_df$p.adj < 0.05, ]
    n_sig <- nrow(sig_pairs)

    if (n_sig == 0) {
      lc_feedback(type = "info",
        p(tags$strong("Żadna para nie różni się istotnie"),
          " (po korekcji Games-Howell)."))
    } else {
      lc_feedback(type = "ok",
        p(tags$strong(paste0(n_sig, " istotna(e) różnica(e):"))),
        tags$ul(
          lapply(1:n_sig, function(i) {
            tags$li(paste0(sig_pairs$group1[i], " — ", sig_pairs$group2[i],
                           ": Δ = ", round(sig_pairs$estimate[i], 2),
                           ", p.adj = ", format.pval(sig_pairs$p.adj[i], digits = 3)))
          })
        )
      )
    }
  })

  # --- Cwiczenia CASchools ---

  cas_vis10 <- reactiveVal(FALSE)

  observeEvent(input$cas_ch7_ans10, {
    nowy <- !cas_vis10()
    cas_vis10(nowy)
    updateActionButton(session, "cas_ch7_ans10",
      label = if (nowy) "Ukryj rozwiązanie" else "Pokaż rozwiązanie")
  }, ignoreInit = TRUE)

  output$cas_ch7_sol10 <- renderUI({
    if (!cas_vis10()) return(NULL)
    r <- local({
      edu <- .ch7_cas
      q   <- quantile(edu$income, probs = c(0, 1/3, 2/3, 1))
      edu$income_group <- cut(edu$income, breaks = q,
        labels = c("Niski", "Średni", "Wysoki"), include.lowest = TRUE)
      grp_stats <- tapply(edu$read, edu$income_group, function(x)
        c(n = length(x), m = mean(x), s = sd(x)))
      fit   <- aov(read ~ income_group, data = edu)
      s     <- summary(fit)[[1]]
      F_val <- s[["F value"]][1]
      df1   <- s[["Df"]][1]; df2 <- s[["Df"]][2]
      p_val <- s[["Pr(>F)"]][1]
      eta2  <- s[["Sum Sq"]][1] / sum(s[["Sum Sq"]])
      ph    <- TukeyHSD(fit)$income_group
      list(grp_stats = grp_stats, F = F_val, df1 = df1, df2 = df2,
           p = p_val, eta2 = eta2, ph = ph)
    })
    lc_feedback(type = "ok", style = "margin-top: 10px;",
      p(tags$b("H₀: "), "μ_niski = μ_średni = μ_wysoki · ",
        tags$b("Hₐ: "), "co najmniej jedna para się różni"),
      tags$table(class = "lc-table lc-table-bordered lc-table-sm",
        tags$thead(tags$tr(
          tags$th("Tercyl"), tags$th("n"), tags$th("x̄ read"), tags$th("s")
        )),
        tags$tbody(lapply(c("Niski","Średni","Wysoki"), function(g) {
          v <- r$grp_stats[[g]]
          tags$tr(tags$td(g), tags$td(v["n"]),
                  tags$td(round(v["m"], 2)), tags$td(round(v["s"], 2)))
        }))
      ),
      tags$ul(
        tags$li(sprintf("F(%d, %d) = %.3f, p %s %s",
          r$df1, r$df2, r$F,
          if (r$p < 0.001) "<" else "=",
          if (r$p < 0.001) "0.001" else format(round(r$p, 4), nsmall = 4))),
        tags$li(sprintf("η² = %.3f (%s efekt — %.1f%% wariancji wyjaśnione przez dochód)",
          r$eta2,
          if (r$eta2 < 0.01) "pomijalny"
          else if (r$eta2 < 0.06) "mały"
          else if (r$eta2 < 0.14) "średni" else "duży",
          100 * r$eta2))
      ),
      if (r$p < 0.05) tags$b(style = paste0("color:", upwr_accent), "Odrzucamy H₀")
      else tags$b("Brak podstaw do odrzucenia H₀"),
      p(tags$b("Post-hoc Tukey HSD (przybliżenie — w Jamovi użyj Games-Howell):")),
      tags$ul(lapply(rownames(r$ph), function(nm) {
        pp <- r$ph[nm, "p adj"]
        tags$li(sprintf("%s: Δ = %.2f pkt, p.adj %s %s",
          nm, r$ph[nm, "diff"],
          if (pp < 0.001) "<" else "=",
          if (pp < 0.001) "0.001" else format(round(pp, 3), nsmall = 3)))
      }))
    )
  })
}
