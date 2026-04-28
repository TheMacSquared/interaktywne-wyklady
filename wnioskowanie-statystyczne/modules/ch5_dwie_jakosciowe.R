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
      p("Gdy mamy dwie zmienne jakościowe, pytamy: ",
        tags$b("czy są ze sobą powiązane?"),
        " Narzędzie: tabela kontyngencji (krzyżowa) + test χ² niezależności."),
      p("Idea: porównujemy to, co ", tags$b("zaobserwowaliśmy"),
        " z tym, czego ", tags$b("oczekiwalibyśmy, gdyby zmienne były niezależne"), "."),
      lc_formula_box(
        p(withMathJax("\\(H_0:\\) zmienne są niezależne — ",
                      "\\(H_a:\\) zmienne są powiązane")),
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

      actionButton("ch5_narr_step1", "1. Pokaż dane",
                   class = "lc-btn-outline", width = "100%"),
      uiOutput("ch5_narr1"),
      br(),

      conditionalPanel(
        condition = "input.ch5_narr_step1 % 2 == 1",
        actionButton("ch5_narr_step2", "2. Załóżmy niezależność — co by było?",
                     class = "lc-btn-outline", width = "100%"),
        uiOutput("ch5_narr2"),
        br(),

        conditionalPanel(
          condition = "input.ch5_narr_step2 % 2 == 1",
          actionButton("ch5_narr_step3", "3. Porównaj: obserwowane i oczekiwane",
                       class = "lc-btn-outline", width = "100%"),
          uiOutput("ch5_narr3")
        )
      )
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
        h0 = "\\(H_0:\\) typ opakowania i występowanie pleśni są niezależne",
        ha = "\\(H_a:\\) są powiązane",
        note = "Choć merytorycznie spodziewamy się kierunku (niektóre opakowania pleśnieją częściej), test χ² jest zawsze dwustronny."
      ),
      list(
        question = "Czy preferencje konsumentów (lubi / nie lubi) zależą od regionu
                    Polski (płd. / pn. / centr. / wsch. / zach.)?",
        h0 = "\\(H_0:\\) preferencja i region są niezależne",
        ha = "\\(H_a:\\) są powiązane",
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
      fluidRow(
        column(4,
          selectInput("ch5_scenario", "Scenariusz (2×2):",
            choices = c(
              "Opakowanie a pleśń (TŻ)" = "packaging",
              "Typ gleby a kategoria plonu (R)" = "soil",
              "Środki ochrony a uraz (IB)" = "ppe_accident"
            ),
            selected = "packaging"
          ),
          sliderInput("ch5_n", "Wielkość próby (n):",
                      min = 50, max = 300, value = 120, step = 10),
          actionButton("ch5_new_sample", "Losuj próbę",
                       class = "lc-btn-primary", width = "100%"),
          hr(),
          h5("Kroki testu:"),
          lc_stack(gap = "sm",
            actionButton("ch5_step1", "1. Tabela obserwowana",
                         class = "lc-btn-outline", width = "100%"),
            actionButton("ch5_step2", "2. Procenty — co widzimy?",
                         class = "lc-btn-outline", width = "100%"),
            actionButton("ch5_step3", "3. Tabela oczekiwana + χ²",
                         class = "lc-btn-outline", width = "100%"),
            actionButton("ch5_step4", "4. p-wartość i decyzja",
                         class = "lc-btn-outline", width = "100%")
          )
        ),
        column(8,
          uiOutput("ch5_hypothesis_panel"),
          plotOutput("ch5_step_plot", height = "350px"),
          uiOutput("ch5_step_info")
        )
      )
    ),

    # ========================================================================
    # WIDGET 2: Chi-kwadrat vs Fisher (porownanie)
    # ========================================================================
    lc_h2("ch5-fisher", "Test χ² a test Fishera"),

    tagList(
      p("Test χ² opiera się na przybliżeniu. Gdy próba jest mała,
        niektóre oczekiwane liczności mogą być < 5 — wtedy przybliżenie zawodzi."),
      p("Alternatywa: ", tags$b("test dokładny Fishera"),
        " — liczy p-wartość dokładnie, jak test dwumianowy dla proporcji.")
    ),

    figure_panel(
      label = "Ryc. 7.3",
      title = "Porównanie: χ² vs Fisher",
      actionButton("ch5_compare", "Porównaj χ² i Fishera (na tych samych danych)",
                   class = "lc-btn-primary", width = "100%"),
      br(), br(),
      uiOutput("ch5_compare_result")
    ),

    tagList(
      p(tags$b("Kiedy który?")),
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

    # ========================================================================
    # Jak interpretowac sile zwiazku
    # ========================================================================
    lc_h2("ch5-sila", "Jak duża jest różnica? Siła związku"),

    tagList(
      p("P-wartość mówi ", tags$em("czy"), " związek istnieje, ale nie ",
        tags$em("jak duży"), " jest. Zobaczmy to na naszych danych:")
    ),

    figure_panel(
      label = "Ryc. 7.4",
      title = "Siła związku — na naszych danych",
      actionButton("ch5_effect", "Pokaż siłę związku",
                   class = "lc-btn-primary", width = "100%"),
      br(), br(),
      uiOutput("ch5_effect_result")
    ),

    tagList(
      p(tags$b("Jak czytać siłę związku:")),
      p(tags$b("1. Procenty w grupach"), " — najlepsza intuicja.
        Jeśli odsetek to 45% wobec 47% — nawet przy p < 0,05 różnica jest
        praktycznie żadna. Jeśli 30% wobec 70% — efekt jest ogromny."),
      p(tags$b("2. Cramér's V"), " — współczynnik siły związku [0–1]:"),
      lc_formula_box(
        p(withMathJax("\\(V = \\sqrt{\\frac{\\chi^2}{n \\cdot (k - 1)}}\\)"),
          " gdzie k = min(wiersze, kolumny)")
      ),
      p("Interpretacja: < 0,1 pomijalny, 0,1–0,3 mały, 0,3–0,5 średni,
        > 0,5 duży."),
      p("Zawsze ", tags$b("zacznij od procentów"),
        " — to język zrozumiały dla każdego odbiorcy.")
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
      actionButton("cas_ch5_ans8", "Pokaż rozwiązanie",
                   class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("cas_ch5_sol8")
    ),

    figure_panel(label = "Ćwiczenie",
      h4("Zadanie 9 — Czy przeładowane klasy idą w parze z ubóstwem?"),
      p("Utwórz dwie zmienne binarne: ",
        tags$code("high_str = (student_teacher_ratio > 20)"),
        " i ", tags$code("high_lunch = (lunch > 50)"),
        ". Wykonaj test χ² niezależności. Czy STR i ubóstwo są ze sobą powiązane?
        Co sugeruje wynik dla interpretacji zadania 5 z korelacji?"),
      actionButton("cas_ch5_ans9", "Pokaż rozwiązanie",
                   class = "lc-btn-ok-outline lc-btn-sm"),
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
  ch5_tab <- reactiveVal(NULL)
  ch5_step <- reactiveVal(0)

  observeEvent(input$ch5_new_sample, {
    par <- scenario_params[[input$ch5_scenario]]
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

    ch5_tab(table(df$var1, df$var2))
    ch5_step(0)
  })

  # --- Widget 0: Narracja niezaleznosci (mandaty) ---
  # Stale dane do narracji (nie losowane)
  narr_tab <- matrix(c(30, 70, 50, 50), nrow = 2, byrow = TRUE,
    dimnames = list(c("Kobiety", "Mężczyźni"),
                    c("Mandat", "Brak mandatu")))

  output$ch5_narr1 <- renderUI({
    req(input$ch5_narr_step1 %% 2 == 1)

    lc_feedback(type = "info", style = "margin-top: 10px;",
      p(tags$b("Dane z 200 kontroli:")),
      tags$table(class = "lc-table lc-table-bordered", style = "font-size: 15px;",
        tags$thead(tags$tr(tags$th(""), tags$th("Mandat"), tags$th("Brak mandatu"), tags$th("Razem"))),
        tags$tbody(
          tags$tr(tags$td(tags$b("Kobiety")), tags$td("30"), tags$td("70"), tags$td("100")),
          tags$tr(tags$td(tags$b("Mężczyźni")), tags$td("50"), tags$td("50"), tags$td("100")),
          tags$tr(tags$td(tags$b("Razem")), tags$td("80"), tags$td("120"), tags$td("200"))
        )
      ),
      p("Kobiety: 30% dostało mandat. Mężczyźni: 50%. Wygląda na różnicę.
        Ale czy to może być przypadek?")
    )
  })

  output$ch5_narr2 <- renderUI({
    req(input$ch5_narr_step2 %% 2 == 1)

    lc_feedback(type = "warning", style = "margin-top: 10px;",
      p(tags$b("Załóżmy, że płeć NIE ma znaczenia (H₀).")),
      p("Skoro płeć nie wpływa na mandaty, to nie musimy dzielić danych na kobiety i mężczyzn.
        Patrzymy na ", tags$b("całość"), ": 80 mandatów na 200 kontroli = ",
        tags$b("40%"), "."),
      p("Jeśli płeć jest niezależna, to te 40% powinno być ",
        tags$b("takie samo"), " dla kobiet i mężczyzn:"),
      tags$table(class = "lc-table lc-table-bordered", style = "font-size: 15px;",
        tags$thead(tags$tr(tags$th(""), tags$th("Mandat"), tags$th("Brak mandatu"), tags$th("Razem"))),
        tags$tbody(
          tags$tr(tags$td(tags$b("Kobiety")), tags$td(tags$em("40")), tags$td(tags$em("60")), tags$td("100")),
          tags$tr(tags$td(tags$b("Mężczyźni")), tags$td(tags$em("40")), tags$td(tags$em("60")), tags$td("100")),
          tags$tr(tags$td(tags$b("Razem")), tags$td("80"), tags$td("120"), tags$td("200"))
        )
      ),
      p("To jest ", tags$b("tabela oczekiwana"), " — ile by było, gdyby płeć nie miała wpływu.")
    )
  })

  output$ch5_narr3 <- renderUI({
    req(input$ch5_narr_step3 %% 2 == 1)

    lc_feedback(type = "ok", style = "margin-top: 10px;",
      p(tags$b("Porównanie: obserwowane i oczekiwane")),
      tags$table(class = "lc-table lc-table-bordered", style = "font-size: 15px;",
        tags$thead(tags$tr(tags$th(""), tags$th("Mandat (obs.)"), tags$th("Mandat (oczek.)"), tags$th("Różnica"))),
        tags$tbody(
          tags$tr(tags$td(tags$b("Kobiety")), tags$td("30"), tags$td("40"), tags$td(tags$b("−10"))),
          tags$tr(tags$td(tags$b("Mężczyźni")), tags$td("50"), tags$td("40"), tags$td(tags$b("+10")))
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
  })

  observeEvent(input$ch5_scenario, {
    ch5_tab(NULL)
    ch5_step(0)
  })

  observeEvent(input$ch5_step1, ch5_step(1))
  observeEvent(input$ch5_step2, ch5_step(2))
  observeEvent(input$ch5_step3, ch5_step(3))
  observeEvent(input$ch5_step4, ch5_step(4))

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

  # --- Krokowy wykres ---
  output$ch5_step_plot <- renderPlot({
    tab <- ch5_tab()
    step <- ch5_step()
    par <- scenario_params[[input$ch5_scenario]]

    if (is.null(tab) || step == 0) return(NULL)

    if (step <= 2) {
      df <- as.data.frame(tab)
      names(df) <- c("Var1", "Var2", "Freq")

      if (step == 1) {
        # Slupki z liczebnosciami
        ggplot(df, aes(x = Var1, y = Freq, fill = Var2)) +
          geom_col(position = "dodge", alpha = 0.8) +
          geom_text(aes(label = Freq), position = position_dodge(width = 0.9),
                    vjust = -0.3, size = 4) +
          labs(title = paste0(par$lab1, " a ", par$lab2, " (liczności)"),
               x = par$lab1, y = "Liczność", fill = par$lab2) +
          scale_fill_upwr() +
                    theme(legend.position = "top")
      } else {
        # Slupki z procentami (w obrębie wiersza)
        df_pct <- df %>%
          group_by(Var1) %>%
          mutate(pct = round(Freq / sum(Freq) * 100, 1)) %>%
          ungroup()

        ggplot(df_pct, aes(x = Var1, y = pct, fill = Var2)) +
          geom_col(position = "dodge", alpha = 0.8) +
          geom_text(aes(label = paste0(pct, "%")),
                    position = position_dodge(width = 0.9),
                    vjust = -0.3, size = 4) +
          labs(title = paste0(par$lab1, " a ", par$lab2, " (% w grupie)"),
               x = par$lab1, y = "Procent", fill = par$lab2) +
          scale_fill_upwr() +
                    theme(legend.position = "top")
      }
    } else {
      # Krok 3-4: rozklad chi-kwadrat
      test <- chisq.test(tab)
      chi_stat <- as.numeric(test$statistic)
      df_val <- as.numeric(test$parameter)
      plot_test_distribution(chi_stat, df = df_val, test_type = "chisq")
    }
  })

  # --- Krokowe info ---
  output$ch5_step_info <- renderUI({
    tab <- ch5_tab()
    step <- ch5_step()
    par <- scenario_params[[input$ch5_scenario]]

    if (is.null(tab) || step == 0) return(NULL)

    test <- chisq.test(tab)
    n_total <- sum(tab)

    # Buduj HTML tabele krzyzowa
    .html_table <- function(mat, caption = "") {
      header <- tags$tr(tags$th(""),
        lapply(colnames(mat), function(cn) tags$th(cn)))
      rows <- lapply(seq_len(nrow(mat)), function(i) {
        tags$tr(tags$td(tags$b(rownames(mat)[i])),
          lapply(seq_len(ncol(mat)), function(j) tags$td(mat[i, j])))
      })
      div(
        if (nchar(caption) > 0) p(tags$b(caption)),
        tags$table(class = "lc-table lc-table-bordered lc-table-striped",
                   style = "font-size: 14px;",
          tags$thead(header),
          tags$tbody(rows))
      )
    }

    info <- switch(as.character(step),
      "1" = tagList(
        lc_stat_box("n", n_total, color = col_h0),
        .html_table(tab, paste0("Tabela krzyżowa: ", par$lab1, " × ", par$lab2)),
        p("To są obserwowane liczności. Ale same liczby trudno porównać,
          bo grupy mogą mieć różne rozmiary. Kliknij krok 2.")
      ),
      "2" = {
        pct_tab <- round(prop.table(tab, margin = 1) * 100, 1)
        pct_mat <- matrix(paste0(pct_tab, "%"), nrow = nrow(pct_tab),
                          dimnames = dimnames(pct_tab))
        tagList(
          .html_table(pct_mat, "Procenty w każdej grupie (wierszu):"),
          p("Gdyby zmienne były niezależne, procenty byłyby ",
            tags$b("takie same"), " w każdym wierszu.
            Czy widzisz różnice?")
        )
      },
      "3" = {
        chi_stat <- as.numeric(test$statistic)
        df_val <- as.numeric(test$parameter)
        exp_mat <- round(test$expected, 1)
        low_exp <- any(test$expected < 5)
        tagList(
          .html_table(exp_mat, "Liczności oczekiwane (gdyby H₀ prawdziwa):"),
          lc_stat_box(
            "p",
            format.pval(p_val, digits = 4),
            caption = paste0("χ²(", df_val, ") = ", round(chi_stat, 3)),
            color = col_effect
          ),
          p("Statystyka χ² mierzy łączną rozbieżność między tabelą obserwowancą
            a tabelą oczekiwaną."),
          if (low_exp) p(style = "color: var(--upwr-accent); font-weight: bold;",
            "⚠ Uwaga: niektóre oczekiwane liczności < 5!")
        )
      },
      "4" = {
        p_val <- test$p.value
        res <- format_test_result(p_val)

        # Cramers V
        k <- min(nrow(tab), ncol(tab))
        v <- sqrt(as.numeric(test$statistic) / (n_total * (k - 1)))

        # Zakres procentow
        pct_tab <- prop.table(tab, margin = 1) * 100
        pct_cols <- apply(pct_tab, 2, function(col) round(range(col), 1))

        tagList(
          lc_stat_box(
            "Cramér's V",
            round(v, 3),
            caption = effect_size_label(v),
            color = col_pvalue
          ),
          p(style = paste0("color: ", res$color, "; font-weight: bold; font-size: 16px;"),
            res$decision),
          p(res$explanation),
          hr(),
          p("Rozrzut procentów między grupami: ",
            paste(colnames(pct_tab), "od", pct_cols[1, ], "do", pct_cols[2, ], "%",
                  collapse = "; "))
        )
      }
    )
    lc_feedback(type = "info", info)
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
            tags$td(tags$b(format.pval(test_chi$p.value, digits = 4))),
            tags$td(tags$b(format.pval(test_fisher$p.value, digits = 4)))
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

  # --- Widget 3: Sila zwiazku na danych z Widget 1 ---
  output$ch5_effect_result <- renderUI({
    req(input$ch5_effect)
    tab <- isolate(ch5_tab())
    par <- isolate(scenario_params[[input$ch5_scenario]])

    if (is.null(tab)) {
      return(lc_feedback(type = "warning",
        "Najpierw wylosuj próbę w widgecie powyżej."))
    }

    test <- chisq.test(tab)
    n_total <- sum(tab)
    k <- min(nrow(tab), ncol(tab))
    v <- sqrt(as.numeric(test$statistic) / (n_total * (k - 1)))

    # Tabela procentow per wiersz
    pct_tab <- prop.table(tab, margin = 1) * 100

    # Buduj czytelna tabelke procentow
    pct_rows <- lapply(seq_len(nrow(pct_tab)), function(i) {
      tags$tr(
        tags$td(tags$b(rownames(pct_tab)[i])),
        lapply(seq_len(ncol(pct_tab)), function(j) {
          tags$td(paste0(round(pct_tab[i, j], 1), "%"))
        })
      )
    })

    # Zakres procentow per kolumna
    range_info <- sapply(seq_len(ncol(pct_tab)), function(j) {
      vals <- pct_tab[, j]
      paste0(colnames(pct_tab)[j], ": od ", round(min(vals), 1),
             "% do ", round(max(vals), 1), "%",
             " (rozrzut ", round(max(vals) - min(vals), 1), " pp)")
    })

    div(
      p(tags$b("Procenty w każdej grupie ", par$lab1, ":")),
      tags$table(class = "lc-table lc-table-bordered lc-table-striped", style = "font-size: 15px;",
        tags$thead(tags$tr(
          tags$th(par$lab1),
          lapply(colnames(pct_tab), function(cn) tags$th(paste0(par$lab2, ": ", cn)))
        )),
        tags$tbody(pct_rows)
      ),
      lc_feedback(type = "info",
        p(tags$b("Rozrzut procentów między grupami:")),
        tags$ul(lapply(range_info, function(ri) tags$li(ri))),
        p("Im większy rozrzut, tym silniejszy związek praktyczny."),
        hr(),
        p(tags$b("Cramér's V = ", round(v, 3)),
          " (", effect_size_label(v), ")")
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
        tags$li(sprintf("Cramér's V = %.3f (%s efekt)", r$v, effect_size_label(r$v)))
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
        tags$li(sprintf("Cramér's V = %.3f (%s efekt)", r$v, effect_size_label(r$v))),
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
