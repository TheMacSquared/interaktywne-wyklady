# ============================================================================
# CHAPTER 6: Dwie zmienne jakosciowe (chi-kwadrat, Fisher)
# ============================================================================

ch5_ui <- list(
  id = "ch-dwie-jakosciowe", num = "07", title = "Test χ² niezależności",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 07 · Testowanie hipotez",
      num    = "07",
      title  = "Test χ² niezależności.",
      lead   = "„Czy wybór kierunku studiów zależy od płci?” Dwie zmienne jakościowe —
                tabela kontyngencji, liczebności oczekiwane i statystyka χ² rozstrzygają,
                czy to niezależność czy zależność."
    ),

    # ========================================================================
    # Wprowadzenie
    # ========================================================================
    lc_h2("ch5-intro", "Tabela kontyngencji i test χ²"),

    tagList(
      p("Gdy mamy dwie ", gloss("zmienna jakościowa", "zmienne jakościowe"), ", pytamy: czy są ze sobą powiązane?",
        " Narzędzie: ", gloss("tabela kontyngencji"), " (krzyżowa) + ",
        gloss("test chi-kwadrat", "test χ²"), " niezależności."),
      p("Idea: porównujemy to, co zaobserwowaliśmy",
        " z tym, czego oczekiwalibyśmy, gdyby zmienne były niezależne."),
      lc_formula_box(
        p(withMathJax("\\(H_0:\\)"), " zmienne są niezależne"),
        p(withMathJax("\\(H_a:\\)"), " zmienne są powiązane")
      ),
      lc_formula_box(
        p("Liczności oczekiwane: ", withMathJax("\\(E_{ij} = \\frac{n_{i\\cdot} \\cdot n_{\\cdot j}}{n}\\)")),
        p("Statystyka testowa: ", withMathJax("\\(\\chi^2 = \\sum \\frac{(O_{ij} - E_{ij})^2}{E_{ij}}\\)"))
      )
    ),

    # ========================================================================
    # WIDGET 0: Budowanie intuicji — co to znaczy niezaleznosc?
    # ========================================================================
    lc_h2("ch5-intuicja", "Budowanie intuicji: co to znaczy „niezależność”?"),

    tagList(
      p("Zanim przejdziemy do wzorów, zbudujmy intuicję na przykładzie:")
    ),

    figure_panel(
      label = "Ryc. 7.1",
      title = "Przykład: czy płeć wpływa na dostawanie mandatów?",

      tagList(
        p("Mamy dane z 200 kontroli drogowych. Pytanie: ",
          tags$em("„Czy szansa dostania mandatu jest niezależna od płci?”"))
      ),

      lc_toolbar(
        lc_step_nav("ch5_narr_step",
          c("Pokaż dane", "Załóżmy niezależność — co by było?",
            "Porównaj: obserwowane i oczekiwane"),
          start = 1)
      ),
      uiOutput("ch5_narr")
    ),

    # ========================================================================
    # Cwiczenie: sformuluj hipotezy
    # ========================================================================
    lc_h2("ch5-cwiczenie", "Ćwiczenie: sformułuj hipotezy"),

    tagList(
      p("Jak wyglądają H₀ i Hₐ dla pytań o związek dwóch zmiennych jakościowych?")
    ),

    hypothesis_practice("ch5", list(
      list(
        question = "Czy wybór kierunku studiów zależy od płci?",
        h0 = "\\(H_0:\\) kierunek i płeć są niezależne",
        ha = "\\(H_a:\\) kierunek i płeć są powiązane",
        note = "Test χ² niezależności zawsze testuje niezależność vs. związek — nie mówi nic o kierunku zależności."
      ),
      list(
        question = "Czy typ opakowania (szkło / plastik / karton) ma związek
                    z występowaniem pleśni w sokach?",
        h0 = "\\(H_0:\\) rodzaj opakowania nie wpływa na ryzyko pojawienia się pleśni",
        ha = "\\(H_a:\\) przynajmniej jedno opakowanie wiąże się z innym ryzykiem pleśni",
        note = "Choć merytorycznie spodziewamy się kierunku (niektóre opakowania pleśnieją częściej), test χ² jest zawsze dwustronny."
      ),
      list(
        question = "Czy preferencje konsumentów (lubi / nie lubi) zależą od regionu
                    Polski (płd. / pn. / centr. / wsch. / zach.)?",
        h0 = "\\(H_0:\\) rozkład preferencji jest taki sam we wszystkich regionach",
        ha = "\\(H_a:\\) rozkład preferencji różni się między przynajmniej dwoma regionami",
        note = "Tabela 2 × 5. Test χ² działa na dowolne wymiary tabeli kontyngencji."
      )
    )),

    # ========================================================================
    # WIDGET 1: Chi-kwadrat krokowy
    # ========================================================================
    lc_h2("ch5-krok", "Test χ² niezależności — krok po kroku"),

    figure_panel(
      label = "Ryc. 7.2",
      title = "Test χ² niezależności — krok po kroku",
      uiOutput("ch5_hypothesis_panel"),
      lc_step_widget("ch5_test",
        steps = c("Tabela obserwowana", "Procenty — co widzimy?",
                  "Tabela oczekiwana + χ²", "p-wartość i decyzja"),
        toolbar = lc_toolbar(
          selectInput("ch5_scenario", "Scenariusz (2×2)",
            choices = c(
              "Opakowanie a pleśń (TŻ)" = "packaging",
              "Typ gleby a kategoria plonu (R)" = "soil",
              "Środki ochrony a uraz (IB)" = "ppe_accident"
            ),
            selected = "packaging"
          ),
          lc_slider("ch5_n", "Wielkość próby (n)", 50, 300, 120, 10),
          lc_action("ch5_new_sample", "Losuj próbę", icon = "shuffle", variant = "solid"),
          # Kolory kategorii zmiennej w kolumnach zamiast legendy wykresu.
          lc_readouts(uiOutput("ch5_test_legend"))
        ),
        plot_id = "ch5_step_plot",
        extra = uiOutput("ch5_test_table")
      )
    ),

    # ========================================================================
    # WIDGET 2: Chi-kwadrat vs Fisher (porownanie)
    # ========================================================================
    lc_h2("ch5-fisher", "Test χ² a test Fishera"),

    tagList(
      p("Test χ² opiera się na przybliżeniu. Gdy próba jest mała,
        niektóre ", gloss("liczność oczekiwana", "oczekiwane liczności"), " mogą być < 5 — wtedy przybliżenie zawodzi."),
      p("Alternatywa: ", gloss("test dokładny Fishera"),
        " — liczy p-wartość dokładnie, jak test dwumianowy dla proporcji.")
    ),

    figure_panel(
      label = "Ryc. 7.3",
      title = "Porównanie: χ² vs Fisher",
      lc_action("ch5_compare", "Porównaj χ² i Fishera (na tych samych danych)", variant = "solid"),
      br(), br(),
      uiOutput("ch5_compare_result")
    ),

    tagList(
      p("Kiedy który?"),
      tags$table(class = "lc-table lc-table-bordered", style = "font-size: 15px;",
        tags$thead(
          tags$tr(tags$th(""), tags$th("Test χ²"), tags$th("Test Fishera"))
        ),
        tags$tbody(
          tags$tr(
            tags$td(tags$b("Metoda")),
            tags$td("Przybliżony (rozkład χ²)"),
            tags$td("Dokładny (kombinatoryka)")
          ),
          tags$tr(
            tags$td(tags$b("Warunek")),
            tags$td("Wszystkie E₀ ≥ 5"),
            tags$td("Działa zawsze")
          ),
          tags$tr(
            tags$td(tags$b("Duże n")),
            tags$td(style = "background: var(--upwr-sage-tint);", "Szybki, praktycznie identyczny wynik"),
            tags$td("Działa, ale wolniejszy")
          ),
          tags$tr(
            tags$td(tags$b("Małe n")),
            tags$td(style = "background: var(--upwr-accent-tint);", "Może być niedokładny"),
            tags$td(style = "background: var(--upwr-sage-tint);", "Bezpieczny wybór")
          ),
          tags$tr(
            tags$td(tags$b("W Jamovi")),
            tags$td("χ² (domyślnie)"),
            tags$td("Zaznacz: Fisher's exact test")
          )
        )
      )
    ),

    lc_h2("ch5-cas", "Ćwiczenia", "CASchools — test χ² niezależności"),

    lc_feedback(type = "info",
      p(tags$b("Dane: "), "420 okręgów szkolnych Kalifornii (1998–1999). Plik: ",
        tags$code("dane/caschools.csv"), "."),
      p("Zmienne w zadaniach: ", tags$code("grades"),
        " (typ szkoły: KK-06/KK-08), ",
        tags$code("english"), " (% uczniów ELL), ",
        tags$code("student_teacher_ratio"), " (STR), ",
        tags$code("lunch"), " (% uczniów z dotacją — wskaźnik ubóstwa).")
    ),

    figure_panel(label = "Ćwiczenie",
      h4("Zadanie 8 — Czy typ szkoły wiąże się z wysokim odsetkiem uczniów ELL?"),
      p("Utwórz zmienną binarną: ",
        tags$code("high_english = (english > 20)"),
        ". Zbuduj tabelę krzyżową ", tags$code("grades"), " × ",
        tags$code("high_english"),
        " i wykonaj test χ² niezależności.
        Zapisz: χ², df, p. Co wynika? Czy typ szkoły jest niezależny
        od odsetka uczniów uczących się angielskiego?"),
      lc_action("cas_ch5_ans8", "Pokaż rozwiązanie", variant = "solid"),
      uiOutput("cas_ch5_sol8")
    ),

    figure_panel(label = "Ćwiczenie",
      h4("Zadanie 9 — Czy przeładowane klasy idą w parze z ubóstwem?"),
      p("Utwórz dwie zmienne binarne: ",
        tags$code("high_str = (student_teacher_ratio > 20)"),
        " i ", tags$code("high_lunch = (lunch > 50)"),
        ". Wykonaj test χ² niezależności. Czy STR i ubóstwo są ze sobą powiązane?
        Co sugeruje wynik dla interpretacji zadania 5 z korelacji?"),
      lc_action("cas_ch5_ans9", "Pokaż rozwiązanie", variant = "solid"),
      uiOutput("cas_ch5_sol9")
    ),

    lc_chapter_next(
      num       = "08",
      title     = "Test t dwóch grup",
      lead      = "porównanie średnich między dwiema grupami — czy różnica jest realna?",
      target_id = "ch-dwie-grupy"
    )
  )
)

# ============================================================================
# DANE — CASchools (wczytane raz przy ladowaniu modulu)
# ============================================================================

.ch5_cas <- read.csv(file.path(app_dir, "dane", "caschools.csv"),
                     stringsAsFactors = FALSE)

# ============================================================================
# SERVER
# ============================================================================

ch5_server <- function(input, output, session) {

  # --- Parametry scenariuszy ---
  scenario_params <- list(
    packaging = list(
      lab1 = "Opakowanie", lab2 = "Pleśń",
      cats1 = c("Szkło", "Plastik", "Karton"),
      cats2 = c("Tak", "Nie"),
      probs = matrix(c(0.05, 0.95, 0.12, 0.88, 0.20, 0.80), nrow = 3, byrow = TRUE),
      question = "Czy typ opakowania wpływa na występowanie pleśni?",
      h0_text = "\\(H_0:\\) typ opakowania i występowanie pleśni są niezależne",
      h1_text = "\\(H_a:\\) typ opakowania i występowanie pleśni są powiązane"),
    atmosphere = list(
      lab1 = "Atmosfera pakowania", lab2 = "Ocena świeżości po 7 dniach",
      cats1 = c("Powietrze", "MAP (modyfikowana)", "Próżnia"),
      cats2 = c("Świeże", "Średniej jakości", "Zepsute"),
      probs = matrix(c(0.15, 0.40, 0.45,
                        0.55, 0.35, 0.10,
                        0.70, 0.25, 0.05), nrow = 3, byrow = TRUE),
      question = "Czy atmosfera pakowania wpływa na świeżość mięsa po 7 dniach?",
      h0_text = "\\(H_0:\\) atmosfera pakowania i ocena świeżości są niezależne",
      h1_text = "\\(H_a:\\) atmosfera pakowania i ocena świeżości są powiązane"),
    soil = list(
      lab1 = "Typ gleby", lab2 = "Plon",
      cats1 = c("Piaszczysta", "Gliniasta", "Czarnoziemna"),
      cats2 = c("Niski", "Wysoki"),
      probs = matrix(c(0.65, 0.35, 0.45, 0.55, 0.25, 0.75), nrow = 3, byrow = TRUE),
      question = "Czy typ gleby wpływa na kategorię plonu?",
      h0_text = "\\(H_0:\\) typ gleby i kategoria plonu są niezależne",
      h1_text = "\\(H_a:\\) typ gleby i kategoria plonu są powiązane"),
    pasteurization = list(
      lab1 = "Metoda pasteryzacji", lab2 = "Liczba bakterii po 7 dniach",
      cats1 = c("Niska (63°C, 30 min)", "Wysoka (72°C, 15 s)", "UHT (135°C, 2 s)"),
      cats2 = c("Niska (< norma)", "Średnia", "Wysoka (> norma)"),
      probs = matrix(c(0.35, 0.40, 0.25,
                        0.60, 0.30, 0.10,
                        0.90, 0.08, 0.02), nrow = 3, byrow = TRUE),
      question = "Czy metoda pasteryzacji mleka wpływa na liczebność bakterii po 7 dniach?",
      h0_text = "\\(H_0:\\) metoda pasteryzacji i liczebność bakterii są niezależne",
      h1_text = "\\(H_a:\\) metoda pasteryzacji i liczebność bakterii są powiązane"),
    ppe_accident = list(
      lab1 = "Środki ochrony (SOI)", lab2 = "Ciężkość wypadku",
      cats1 = c("Niepełne", "Pełne"),
      cats2 = c("Brak urazu", "Uraz lekki", "Uraz ciężki"),
      probs = matrix(c(0.45, 0.35, 0.20,
                        0.80, 0.17, 0.03), nrow = 2, byrow = TRUE),
      question = "Czy stosowanie pełnych środków ochrony indywidualnej wpływa na ciężkość wypadku przy pracy?",
      h0_text = "\\(H_0:\\) stosowanie SOI i ciężkość wypadku są niezależne",
      h1_text = "\\(H_a:\\) stosowanie SOI i ciężkość wypadku są powiązane")
  )

  # --- Wspoldzielone dane ---
  # Tabela jest losowana dla konkretnego scenariusza i n. Po zmianie tych
  # inputow wymaga ponownego losowania, zamiast udawac aktualne dane.
  ch5_tab_state <- reactiveVal(NULL)
  ch5_tab <- reactive({
    state <- ch5_tab_state()
    if (is.null(state)) return(NULL)
    req(input$ch5_scenario, input$ch5_n)

    if (!identical(state$scenario, input$ch5_scenario) ||
        !isTRUE(state$n == input$ch5_n)) {
      return(NULL)
    }

    state$tab
  })

  # Krok widgetu (1..4) żyje w przeglądarce; nowa próba ani zmiana
  # scenariusza nie cofa kroku.
  ch5_step <- lc_step_server("ch5_test", input)$step

  observeEvent(input$ch5_new_sample, {
    req(input$ch5_scenario, input$ch5_n)
    par <- scenario_params[[input$ch5_scenario]]
    req(!is.null(par))
    n <- input$ch5_n
    n_per_cat1 <- rmultinom(1, n, rep(1, length(par$cats1)))

    rows <- list()
    for (i in seq_along(par$cats1)) {
      cats2_draws <- sample(par$cats2, n_per_cat1[i], replace = TRUE, prob = par$probs[i, ])
      rows[[i]] <- data.frame(var1 = par$cats1[i], var2 = cats2_draws)
    }
    df <- do.call(rbind, rows)
    df$var1 <- factor(df$var1, levels = par$cats1)
    df$var2 <- factor(df$var2, levels = par$cats2)

    ch5_tab_state(list(
      scenario = input$ch5_scenario,
      n = n,
      tab = table(df$var1, df$var2)
    ))
  }, ignoreInit = TRUE)

  # --- Widget 0: Narracja niezaleznosci (mandaty) ---
  # Stale dane do narracji (nie losowane)
  narr_tab <- matrix(c(30, 70, 50, 50), nrow = 2, byrow = TRUE,
    dimnames = list(c("Kobiety", "Mężczyźni"),
                    c("Mandat", "Brak mandatu")))
  narr_exp <- matrix(c(40, 60, 40, 60), nrow = 2, byrow = TRUE,
    dimnames = dimnames(narr_tab))
  narr_steps <- c("Pokaż dane", "Załóżmy niezależność — co by było?",
                  "Porównaj: obserwowane i oczekiwane")

  # Kroki 1..3 (kropki); wartość 0 po „Wstecz” z kroku 1 pokazuje krok 1.
  ch5_narr_step <- reactive(max(1L, as.integer(input$ch5_narr_step %||% 1L)))

  output$ch5_narr <- renderUI({
    step <- ch5_narr_step()
    kicker <- paste0("Krok ", step, " z 3 · ", narr_steps[step])

    switch(as.character(step),
      "1" = lc_step_text(kicker, title = "Dane z 200 kontroli:",
        lc_crosstab(narr_tab, measure = "n", label = "Dane z 200 kontroli"),
        p("Kobiety: 30% dostało mandat. Mężczyźni: 50%. Wygląda na różnicę.
          Ale czy to może być przypadek?")
      ),
      "2" = lc_step_text(kicker, title = "Załóżmy, że płeć NIE ma znaczenia (H₀).",
        p("Skoro płeć nie wpływa na mandaty, to nie musimy dzielić danych na kobiety i mężczyzn.
          Patrzymy na ", tags$b("całość"), ": 80 mandatów na 200 kontroli = ",
          tags$b("40%"), "."),
        p("Jeśli płeć jest niezależna, to te 40% powinno być ",
          tags$b("takie samo"), " dla kobiet i mężczyzn:"),
        lc_crosstab(narr_exp, measure = "n", lead = FALSE, label = "Tabela oczekiwana"),
        p("To jest ", tags$b("tabela oczekiwana"), " — ile by było, gdyby płeć nie miała wpływu.")
      ),
      "3" = lc_step_text(kicker, title = "Porównanie: obserwowane i oczekiwane",
        lc_table(
          data.frame(group = rownames(narr_tab), obs = narr_tab[, "Mandat"],
                     exp = narr_exp[, "Mandat"],
                     diff = narr_tab[, "Mandat"] - narr_exp[, "Mandat"]),
          cols = list(
            lc_col("group", "", "row"),
            lc_col("obs", "Mandat (obs.)"),
            lc_col("exp", "Mandat (oczek.)"),
            lc_col("diff", "Różnica")
          )
        ),
        p("Kobiety dostały ", tags$b("10 mandatów mniej"), " niż oczekiwano,
          mężczyźni ", tags$b("10 więcej"), "."),
        p("Test χ² bierze te różnice, podnosi do kwadratu, dzieli przez oczekiwane
          i sumuje po wszystkich komórkach. Im większa ta suma, tym trudniej
          wytłumaczyć różnice przypadkiem."),
        p(tags$em("To właśnie robi wzór: "),
          withMathJax("\\(\\chi^2 = \\sum \\frac{(O_{ij} - E_{ij})^2}{E_{ij}}\\)"))
      )
    )
  })

  # --- Panel hipotezy ---
  output$ch5_hypothesis_panel <- renderUI({
    par <- scenario_params[[input$ch5_scenario]]
    tab <- ch5_tab()
    tagList(
      lc_feedback(type = "info", style = "font-size: 16px;",
        p(tags$b("Pytanie potoczne:")),
        p(tags$em(paste0("„", par$question, "”")))
      ),
      lc_formula_box(
        p(tags$b("Hipoteza formalna:")),
        p(withMathJax(par$h0_text)),
        p(withMathJax(par$h1_text))
      ),
      if (is.null(tab)) {
        div(style = "text-align: center; margin: 10px 0; color: var(--upwr-reference);",
          p(tags$em("Kliknij „Losuj próbę”"))
        )
      }
    )
  })

  # Kolory kategorii zmiennej w kolumnach (kroki 1–2): dane, grupa, trzecia.
  ch5_cat_colours <- function(n_cat) {
    c(STEP_ROLES$data$colour, STEP_ROLES$group$colour,
      unname(upwr_cat["szalwia"]))[seq_len(n_cat)]
  }

  # Odczyty z kolorem kategorii zastępują legendę wykresu słupkowego.
  output$ch5_test_legend <- renderUI({
    tab <- ch5_tab()
    if (is.null(tab) || ch5_step() > 2) return(NULL)
    par <- scenario_params[[input$ch5_scenario]]
    cols <- ch5_cat_colours(ncol(tab))
    lapply(seq_len(ncol(tab)), function(j) {
      lc_readout(paste0(par$lab2, ": ", colnames(tab)[j]), lc_fmt(sum(tab[, j])),
                 color = cols[j], swatch = TRUE)
    })
  })

  # --- Krokowy wykres ---
  zoom_plot_server("ch5_step_plot", reactive({
    tab <- ch5_tab()
    step <- ch5_step()
    par <- scenario_params[[input$ch5_scenario]]

    if (is.null(tab)) return(NULL)

    if (step <= 2) {
      df <- as.data.frame(tab)
      names(df) <- c("Var1", "Var2", "Freq")
      df <- df %>%
        group_by(Var1) %>%
        mutate(pct = round(Freq / sum(Freq) * 100, 1)) %>%
        ungroup()
      # Krok 1: liczności; krok 2: procenty w obrębie wiersza.
      df$value <- if (step == 1) df$Freq else df$pct
      df$label <- if (step == 1) df$Freq else paste0(df$pct, "%")
      y_top <- if (step == 1) max(df$Freq) * 1.15 else 110

      # Wypełnienie z kategorii (aes), więc krawędź wyniku podana wprost:
      # step_result() ustawia stałe wypełnienie.
      ggplot(df, aes(x = Var1, y = value, fill = Var2)) +
        geom_col(position = position_dodge(width = 0.9), width = 0.85,
                 alpha = STEP_ROLES$data$alpha, colour = STEP_EDGE$colour,
                 linewidth = STEP_EDGE$linewidth) +
        geom_text(aes(label = label), position = position_dodge(width = 0.9),
                  vjust = -0.3, size = 4, family = "mono",
                  colour = STEP_ROLES$known$colour) +
        scale_fill_manual(values = ch5_cat_colours(ncol(tab))) +
        labs(x = par$lab1, y = if (step == 1) "Liczność" else "Procent") +
        step_frame(xlim = c(0.4, nrow(tab) + 0.6), ylim = c(0, y_top))
    } else {
      # Krok 3: statystyka χ²; krok 4: obszar odrzucenia i decyzja
      test <- chisq.test(tab)
      step_null_plot(as.numeric(test$statistic), df = as.numeric(test$parameter),
                     type = "chisq", phase = if (step == 3) "stat" else "decision")
    }
  }))

  # --- Opis kroku ---
  output$ch5_test_text <- renderUI({
    tab <- ch5_tab()
    step <- ch5_step()

    if (is.null(tab)) return(NULL)

    test <- chisq.test(tab)
    chi_stat <- as.numeric(test$statistic)
    df_val <- as.numeric(test$parameter)

    switch(as.character(step),
      "1" = tagList(
        "n = ", step_num(sum(tab)), ". To są obserwowane liczności. Ale same liczby
        trudno porównać, bo grupy mogą mieć różne rozmiary. Kliknij krok 2."
      ),
      "2" = tagList(
        "Gdyby zmienne były niezależne, procenty byłyby ",
        tags$strong("takie same", .noWS = "outside"), " w każdym wierszu.
        Czy widzisz różnice?"
      ),
      "3" = tagList(
        "χ² = ", step_num(lc_fmt(chi_stat, 3)), paste0(" (df = ", df_val, ")."),
        "Statystyka χ² mierzy łączną rozbieżność między tabelą obserwowaną a tabelą oczekiwaną.",
        if (any(test$expected < 5)) tagList(" ",
          lc_verdict("Uwaga: niektóre oczekiwane liczności < 5!", type = "danger"))
      ),
      "4" = tagList(
        paste0("Wynik testu χ² niezależności: χ²(", df_val, ") = "),
        step_num(lc_fmt(chi_stat, 3)), ". ", step_verdict(test$p.value)
      )
    )
  })

  # --- Tabele kroków pod wykresem ---
  output$ch5_test_table <- renderUI({
    tab <- ch5_tab()
    step <- ch5_step()
    par <- scenario_params[[input$ch5_scenario]]

    if (is.null(tab) || step == 4) return(NULL)

    tab <- as.matrix(unclass(tab))
    switch(as.character(step),
      "1" = lc_crosstab(tab, measure = "n", row_name = par$lab1,
                        col_name = par$lab2,
                        label = paste0("Tabela krzyżowa: ", par$lab1, " × ", par$lab2)),
      "2" = lc_crosstab(tab, measure = "row", row_name = par$lab1,
                        col_name = par$lab2, label = "Procenty w każdej grupie (wierszu)"),
      "3" = {
        expected <- chisq.test(tab)$expected
        keys <- paste0("c", seq_len(ncol(expected)))
        df <- data.frame(group = rownames(expected), check.names = FALSE)
        for (j in seq_along(keys)) df[[keys[j]]] <- expected[, j]
        lc_table(df,
          cols = c(list(lc_col("group", par$lab1, "row")),
                   Map(function(k, lab) lc_col(k, lab, digits = 1),
                       keys, colnames(expected))),
          lead = "Liczności oczekiwane (gdyby H₀ prawdziwa):")
      }
    )
  })

  # --- Widget 2: Porownanie chi-kwadrat vs Fisher ---
  output$ch5_compare_result <- renderUI({
    req(input$ch5_compare)
    tab <- isolate(ch5_tab())

    if (is.null(tab)) {
      return(lc_feedback(type = "warning",
        "Najpierw wylosuj próbę w widgecie powyżej."))
    }

    test_chi <- chisq.test(tab)
    test_fisher <- tryCatch(
      fisher.test(tab),
      error = function(e) fisher.test(tab, simulate.p.value = TRUE, B = 2000)
    )

    low_exp <- any(test_chi$expected < 5)
    n_low <- sum(test_chi$expected < 5)

    div(
      tags$table(class = "lc-table lc-table-bordered", style = "font-size: 15px;",
        tags$thead(
          tags$tr(tags$th(""), tags$th("Test χ²"), tags$th("Test Fishera"))
        ),
        tags$tbody(
          tags$tr(
            tags$td(tags$b("p-wartość")),
            tags$td(tags$b(format_p_value(test_chi$p.value))),
            tags$td(tags$b(format_p_value(test_fisher$p.value)))
          ),
          tags$tr(
            tags$td(tags$b("Decyzja")),
            tags$td(style = paste0("color:", format_test_result(test_chi$p.value)$color),
                    format_test_result(test_chi$p.value)$decision),
            tags$td(style = paste0("color:", format_test_result(test_fisher$p.value)$color),
                    format_test_result(test_fisher$p.value)$decision)
          )
        )
      ),
      lc_feedback(type = if (low_exp) "danger" else "ok",
        p(tags$b("Oczekiwane liczności < 5: "),
          if (low_exp) paste0("TAK (", n_low, " komórek) — χ² może być niedokładny, preferuj Fishera!")
          else "NIE — oba testy dają wiarygodne wyniki.")
      )
    )
  })

  # --- Cwiczenia CASchools ---

  .cas_chisq <- function(tab) {
    ct <- chisq.test(tab, correct = FALSE)
    n  <- sum(tab)
    k  <- min(nrow(tab), ncol(tab))
    v  <- sqrt(ct$statistic / (n * (k - 1)))
    list(chi2 = unname(ct$statistic), df = unname(ct$parameter),
         p = ct$p.value, tab = tab, v = v, n = n)
  }

  cas_vis8 <- reactiveVal(FALSE)
  cas_vis9 <- reactiveVal(FALSE)

  observeEvent(input$cas_ch5_ans8, {
    nowy <- !cas_vis8()
    cas_vis8(nowy)
    updateActionButton(session, "cas_ch5_ans8",
      label = if (nowy) "Ukryj rozwiązanie" else "Pokaż rozwiązanie")
  }, ignoreInit = TRUE)

  output$cas_ch5_sol8 <- renderUI({
    if (!cas_vis8()) return(NULL)
    r <- local({
      high_eng <- .ch5_cas$english > 20
      .cas_chisq(table(grades = .ch5_cas$grades, high_english = high_eng))
    })
    tab <- r$tab
    lc_feedback(type = "ok", style = "margin-top: 10px;",
      p(tags$b("H₀: "), "typ szkoły i high_english są niezależne · ",
        tags$b("Hₐ: "), "zmienne są zależne"),
      tags$table(class = "lc-table lc-table-bordered lc-table-sm",
        tags$thead(tags$tr(
          tags$th("grades"), tags$th("high_english = FALSE"),
          tags$th("high_english = TRUE"), tags$th("suma")
        )),
        tags$tbody(lapply(rownames(tab), function(g) {
          tags$tr(tags$td(g),
            tags$td(tab[g, "FALSE"]), tags$td(tab[g, "TRUE"]),
            tags$td(sum(tab[g, ])))
        }))
      ),
      tags$ul(
        tags$li(sprintf("χ²(%d) = %.3f, p %s %s",
          r$df, r$chi2,
          if (r$p < 0.001) "<" else "=",
          if (r$p < 0.001) "0.001" else format(round(r$p, 4), nsmall = 4))),
        tags$li(sprintf("Cramér's V = %.3f", r$v))
      ),
      if (r$p < 0.05) tags$b(style = paste0("color:", upwr_accent), "Odrzucamy H₀")
      else tags$b("Brak podstaw do odrzucenia H₀"),
      p(tags$b("Interpretacja: "),
        "Test χ² stwierdza, czy zmienne są zależne — nie jak duże jest przesunięcie ani
        w jakim kierunku. Siłę związku wyraża Cramér's V. By zobaczyć kierunek —
        porównaj proporcje high_english w każdej grupie grades.")
    )
  })

  observeEvent(input$cas_ch5_ans9, {
    nowy <- !cas_vis9()
    cas_vis9(nowy)
    updateActionButton(session, "cas_ch5_ans9",
      label = if (nowy) "Ukryj rozwiązanie" else "Pokaż rozwiązanie")
  }, ignoreInit = TRUE)

  output$cas_ch5_sol9 <- renderUI({
    if (!cas_vis9()) return(NULL)
    r <- local({
      high_str   <- .ch5_cas$student_teacher_ratio > 20
      high_lunch <- .ch5_cas$lunch > 50
      .cas_chisq(table(high_str = high_str, high_lunch = high_lunch))
    })
    tab <- r$tab
    p_hi_str_poor <- tab["TRUE",  "TRUE"] / sum(tab["TRUE", ])
    p_lo_str_poor <- tab["FALSE", "TRUE"] / sum(tab["FALSE", ])
    lc_feedback(type = "ok", style = "margin-top: 10px;",
      p(tags$b("H₀: "), "high_str i high_lunch są niezależne · ",
        tags$b("Hₐ: "), "zmienne są zależne"),
      tags$table(class = "lc-table lc-table-bordered lc-table-sm",
        tags$thead(tags$tr(
          tags$th("high_str"), tags$th("high_lunch = FALSE"),
          tags$th("high_lunch = TRUE"), tags$th("suma")
        )),
        tags$tbody(lapply(rownames(tab), function(g) {
          tags$tr(tags$td(g),
            tags$td(tab[g, "FALSE"]), tags$td(tab[g, "TRUE"]),
            tags$td(sum(tab[g, ])))
        }))
      ),
      tags$ul(
        tags$li(sprintf("χ²(%d) = %.3f, p %s %s",
          r$df, r$chi2,
          if (r$p < 0.001) "<" else "=",
          if (r$p < 0.001) "0.001" else format(round(r$p, 4), nsmall = 4))),
        tags$li(sprintf("Cramér's V = %.3f", r$v)),
        tags$li(sprintf("Odsetek high_lunch wśród STR > 20: %.1f%%", 100 * p_hi_str_poor)),
        tags$li(sprintf("Odsetek high_lunch wśród STR ≤ 20: %.1f%%", 100 * p_lo_str_poor))
      ),
      if (r$p < 0.05) tags$b(style = paste0("color:", upwr_accent), "Odrzucamy H₀")
      else tags$b("Brak podstaw do odrzucenia H₀"),
      p(tags$b("Wniosek: "),
        "Okręgi z przeładowanymi klasami mają wyraźnie wyższy odsetek ubogich uczniów.
        STR może być proxy dla zasobności — dlatego korelacja STR–read z zadania 5
        jest częściowo konfundowana dochodem.")
    )
  })
}
