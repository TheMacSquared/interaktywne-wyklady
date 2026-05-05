# ============================================================================
# CHAPTER 4: Jedna zmienna jakosciowa — test dwumianowy
# ============================================================================

ch3_ui <- list(
  id = "ch-jedna-jakosciowa", num = "05", title = "Test proporcji",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 05 · Testowanie hipotez",
      num    = "05",
      title  = "Test proporcji.",
      lead   = "„Czy w naszej populacji faktycznie 30% osób to leworęczni?” Gdy pytanie
                dotyczy odsetka, nie średniej — test dwumianowy i jego z-przybliżenie."
    ),

    # ========================================================================
    # Wprowadzenie
    # ========================================================================
    lc_h2("ch3-pytanie", "Od pytania do testu dwumianowego"),

    tagList(
      p("Gdy zmienna ma dwie kategorie (sukces/porażka, tak/nie, spełnia/nie spełnia),
        pytamy o proporcję w populacji."),
      p("Narzędzie: ", tags$b("test dwumianowy"),
        " — porównuje obserwowany odsetek z wartością referencyjną p₀."),
      p("Test dwumianowy jest dokładny — nie opiera się na przybliżeniu normalnym,
        działa nawet przy małych próbach."),
      p("Trzy warianty par hipotez — zależnie od brzmienia pytania:"),
      lc_formula_box(
        p(tags$b("Dwustronna"), " (proporcja różni się od ",
          withMathJax("\\(p_0\\)"), "):"),
        p(withMathJax("\\(H_0: p = p_0 \\quad\\)"),
          withMathJax("\\(H_a: p \\neq p_0\\)"))
      ),
      lc_formula_box(
        p(tags$b("Prawostronna"), " (proporcja ",
          tags$em("wyższa"), " niż ", withMathJax("\\(p_0\\)"), "):"),
        p(withMathJax("\\(H_0: p \\leq p_0 \\quad\\)"),
          withMathJax("\\(H_a: p > p_0\\)"))
      ),
      lc_formula_box(
        p(tags$b("Lewostronna"), " (proporcja ",
          tags$em("niższa"), " niż ", withMathJax("\\(p_0\\)"), "):"),
        p(withMathJax("\\(H_0: p \\geq p_0 \\quad\\)"),
          withMathJax("\\(H_a: p < p_0\\)"))
      ),
      p("W teście dwumianowym statystyką testową jest sama liczba sukcesów ",
        withMathJax("\\(k\\)"),
        " — nie trzeba jej standaryzować, bo pod H₀ zna jej rozkład dokładnie
        (to rozkład dwumianowy ", withMathJax("\\(B(n, p_0)\\)"),
        "). p-wartość liczymy bezpośrednio jako prawdopodobieństwo wyniku co najmniej
        tak skrajnego jak obserwowany:"),
      lc_formula_box(
        p("Statystyka: ", withMathJax("\\(k\\)"),
          " (liczba sukcesów w ", withMathJax("\\(n\\)"), " próbach)"),
        p("p-wartość (dwustronna): ",
          withMathJax("\\(P(K \\leq k\\ \\text{lub}\\ K \\geq k)\\)"),
          " przy ", withMathJax("\\(K \\sim B(n, p_0)\\)"))
      )
    ),

    # ========================================================================
    # Cwiczenie: sformuluj hipotezy
    # ========================================================================
    lc_h2("ch3-cwiczenie", "Ćwiczenie: sformułuj hipotezy"),

    tagList(
      p("Spróbuj sam przełożyć pytanie potoczne na H₀ i Hₐ. Przedyskutuj
        w grupie, a potem sprawdź.")
    ),

    hypothesis_practice("ch3", list(
      list(
        question = "Producent deklaruje, że 80% słoików jego dżemu spełnia
                    wymóg minimalnej zawartości owoców. Kontrola sprawdza,
                    czy ten odsetek się zgadza.",
        h0 = "\\(H_0: p = 0{,}80\\)",
        ha = "\\(H_a: p \\neq 0{,}80\\)",
        note = "Dwustronny — interesuje nas każde odchylenie od deklaracji."
      ),
      list(
        question = "W standardowej produkcji 3% opakowań jest wadliwych.
                    Sprawdzamy, czy nowa linia produkcyjna generuje więcej braków.",
        h0 = "\\(H_0: p \\leq 0{,}03\\) (nie gorzej niż standard)",
        ha = "\\(H_a: p > 0{,}03\\) (więcej wadliwych)",
        note = "Jednostronny (prawostronny) — pytamy tylko o pogorszenie."
      ),
      list(
        question = "Rolnik twierdzi, że kiełkuje mu co najmniej 90% nasion.
                    Chcemy sprawdzić, czy ta deklaracja jest prawdziwa
                    (z perspektywy klienta — ryzykujemy kupując słabsze nasiona).",
        h0 = "\\(H_0: p \\geq 0{,}90\\)",
        ha = "\\(H_a: p < 0{,}90\\)",
        note = "Jednostronny (lewostronny) — klienta martwi tylko, że jest gorzej."
      )
    )),

    # ========================================================================
    # WIDGET 1: Test dwumianowy dwustronny (krokowy)
    # ========================================================================
    lc_h2("ch3-krok", "Test dwumianowy — krok po kroku"),

    figure_panel(
      label = "Ryc. 5.1",
      title = "Test dwumianowy — krok po kroku",
      fluidRow(
        column(4,
          selectInput("ch3_scenario", "Scenariusz:",
            choices = c(
              "Jakość wody (p₀ = 80%)" = "water_quality",
              "Zdawalność egzaminu (p₀ = 60%)" = "exam_pass",
              "Kiełkowalność nasion (p₀ = 90%)" = "germination",
              "Produkty poza normą (p₀ = 3%)" = "defects",
              "Używanie kasków na budowie (p₀ = 95%, IB)" = "helmets"
            ),
            selected = "water_quality"
          ),
          sliderInput("ch3_n", "Wielkość próby (n):",
                      min = 20, max = 200, value = 50, step = 10),
          actionButton("ch3_new_sample", "Losuj próbę",
                       class = "lc-btn-primary", width = "100%"),
          hr(),
          h5("Kroki testu:"),
          lc_stack(gap = "sm",
            actionButton("ch3_step1", "1. Dane",
                         class = "lc-btn-outline", width = "100%"),
            actionButton("ch3_step3", "2. Rozkład pod H₀",
                         class = "lc-btn-outline", width = "100%"),
            actionButton("ch3_step4", "3. p-wartość i decyzja",
                         class = "lc-btn-outline", width = "100%")
          )
        ),
        column(8,
          uiOutput("ch3_hypothesis_panel"),
          plotOutput("ch3_step_plot", height = "350px"),
          uiOutput("ch3_step_info")
        )
      )
    ),

    inline_callout(
      label = "Co zrobiliśmy?",
      tagList(
        tags$ol(
          tags$li("Zebraliśmy dane i obliczyliśmy proporcję z próby: ",
                  withMathJax("\\(\\hat{p} = k/n\\)")),
          tags$li("Sprawdziliśmy jak wygląda rozkład dwumianowy pod H₀"),
          tags$li("Policzyliśmy p-wartość — jak prawdopodobny jest nasz wynik jeśli H₀ prawdziwa")
        )
      )
    ),

    # ========================================================================
    # WIDGET 2: Test dwumianowy jednostronny (te same dane)
    # ========================================================================
    lc_h2("ch3-jednostronny", "A jeśli znamy kierunek?"),

    tagList(
      p("Tak jak przy teście t — czasem nie pytamy „czy różni się?”,
        ale „czy jest większa / mniejsza niż p₀?”"),
      p("Użyjemy tych samych danych co powyżej, ale zmienimy pytanie na kierunkowe.")
    ),

    figure_panel(
      label = "Ryc. 5.2",
      title = "Test dwumianowy jednostronny",
      fluidRow(
        column(4,
          helpText("Dane: te same co w teście dwustronnym powyżej."),
          hr(),
          h5("Kroki testu:"),
          lc_stack(gap = "sm",
            actionButton("ch3b_step1", "1. Dane",
                         class = "lc-btn-outline", width = "100%"),
            actionButton("ch3b_step3", "2. Rozkład pod H₀",
                         class = "lc-btn-outline", width = "100%"),
            actionButton("ch3b_step4", "3. p-wartość i decyzja",
                         class = "lc-btn-outline", width = "100%")
          )
        ),
        column(8,
          uiOutput("ch3b_hypothesis_panel"),
          plotOutput("ch3b_step_plot", height = "350px"),
          uiOutput("ch3b_step_info")
        )
      )
    ),

    inline_callout(
      label = "Dwu- a jednostronny",
      tagList(
        tags$ul(
          tags$li(tags$b("Dwustronny (≠):"), " p-wartość liczymy po obu stronach. Bezpieczniejszy."),
          tags$li(tags$b("Jednostronny (> lub <):"), " p-wartość tylko po jednej stronie. Mocniejszy, ale ślepy na efekt w drugą stronę.")
        ),
        tags$p("Te same dane, ten sam wynik k/n, ale ", tags$b("inna p-wartość"),
               " — bo inaczej zadane pytanie!")
      ),
      color = "uwaga"
    ),

    # ========================================================================
    # WIDGET 3: Porownanie — test dwumianowy vs test proporcji
    # ========================================================================
    lc_h2("ch3-porownanie", "Test dwumianowy a test proporcji"),

    tagList(
      p("W Jamovi i wielu podręcznikach spotkasz też ",
        tags$b("test proporcji (z-test)"),
        ". Działa na przybliżeniu normalnym:"),
      lc_formula_box(
        p(withMathJax("\\(z = \\frac{\\hat{p} - p_0}{\\sqrt{p_0(1-p_0)/n}}\\)"))
      ),
      p("Porównajmy oba testy na tych samych danych:")
    ),

    figure_panel(
      label = "Ryc. 5.3",
      title = "Porównanie wyników: dwumianowy vs z-test",
      actionButton("ch3_compare", "Porównaj testy", class = "lc-btn-primary", width = "100%"),
      br(), br(),
      uiOutput("ch3_compare_result")
    ),

    tagList(
      p(tags$b("Kiedy który?")),
      tags$table(class = "lc-table lc-table-bordered", style = "font-size: 15px;",
        tags$thead(
          tags$tr(tags$th(""), tags$th("Test dwumianowy"), tags$th("Test proporcji (z-test)"))
        ),
        tags$tbody(
          tags$tr(
            tags$td(tags$b("Metoda")),
            tags$td("Dokładny — liczy z rozkładu B(n, p₀)"),
            tags$td("Przybliżony — używa rozkładu normalnego")
          ),
          tags$tr(
            tags$td(tags$b("Małe n")),
            tags$td(style = "background: var(--upwr-sage-tint);", "Działa zawsze"),
            tags$td(style = "background: var(--upwr-accent-tint);", "Może być niedokładny")
          ),
          tags$tr(
            tags$td(tags$b("Duże n")),
            tags$td("Działa, ale wolniejszy"),
            tags$td(style = "background: var(--upwr-sage-tint);", "Daje praktycznie ten sam wynik")
          ),
          tags$tr(
            tags$td(tags$b("W Jamovi")),
            tags$td("Binomial test"),
            tags$td("Proportion test (N Outcomes)")
          )
        )
      ),
      p(tags$b("Reguła kciuka:"),
        " jeśli ", withMathJax("\\(np_0 \\geq 10\\)"), " i ",
        withMathJax("\\(n(1-p_0) \\geq 10\\)"),
        " — oba testy dadzą praktycznie ten sam wynik.")
    ),

    lc_h2("ch3-cas", "Ćwiczenia", "CASchools — test proporcji"),

    lc_feedback(type = "info",
      p(tags$b("Dane: "), "420 okręgów szkolnych Kalifornii (1998–1999). Plik: ",
        tags$code("dane/caschools.csv"), "."),
      p("Zmienne w zadaniach: ", tags$code("grades"),
        " (typ szkoły: KK-06 lub KK-08), ",
        tags$code("lunch"), " (% uczniów z dotacją do obiadów — wskaźnik ubóstwa).")
    ),

    figure_panel(label = "Ćwiczenie",
      h4("Zadanie A — Czy większość okręgów obejmuje klasy tylko do 6.?"),
      p("Okręgi dzielą się na szkoły klas KK-06 i KK-08. Przetestuj ",
        tags$b("dwustronnie"), ", czy odsetek okręgów KK-06 różni się od 50%.
        Sformułuj H₀ i Hₐ, oblicz p-wartość testem dwumianowym (α = 0.05).
        Jak interpretujesz wynik?"),
      actionButton("cas_ch3_ans_a", "Pokaż rozwiązanie",
                   class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("cas_ch3_sol_a")
    ),

    figure_panel(label = "Ćwiczenie",
      h4("Zadanie B — Czy więcej niż 30% okręgów ma wysoki poziom ubóstwa?"),
      p("Przyjmij, że okrąg ma wysoki poziom ubóstwa, gdy ", tags$code("lunch > 50%"),
        ". Przetestuj ", tags$b("jednostronnie (prawostronnie)"), ",
        czy odsetek takich okręgów przekracza normę 30%.
        Sformułuj H₀ i Hₐ, wykonaj test dwumianowy. Jaki wniosek?"),
      actionButton("cas_ch3_ans_b", "Pokaż rozwiązanie",
                   class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("cas_ch3_sol_b")
    ),

    lc_chapter_next(
      num       = "06",
      title     = "Korelacja",
      lead      = "związek między dwiema zmiennymi ilościowymi.",
      target_id = "ch-korelacja"
    )
  )
)

# ============================================================================
# DANE — CASchools (wczytane raz przy ladowaniu modulu)
# ============================================================================

.ch3_cas <- read.csv(file.path(app_dir, "dane", "caschools.csv"),
                     stringsAsFactors = FALSE)

# ============================================================================
# SERVER
# ============================================================================

ch3_server <- function(input, output, session) {

  # --- Parametry scenariuszy ---
  scenario_params <- list(
    water_quality = list(
      p0 = 0.80, p_true = 0.85, n_default = 50,
      success_label = "spełnia normę", failure_label = "nie spełnia",
      title = "Jakość próbek wody",
      question = "Czy odsetek próbek spełniających normy różni się od deklarowanych 80%?",
      h0_text = "\\(H_0: p = 0.80\\) (odsetek zgodny z deklaracją)",
      h1_text = "\\(H_a: p \\neq 0.80\\) (odsetek odbiega od deklaracji)",
      question_1s = "Czy odsetek próbek spełniających normy jest wyższy niż 80%?",
      h0_text_1s = "\\(H_0: p \\leq 0.80\\)",
      h1_text_1s = "\\(H_a: p > 0.80\\)",
      alt_1s = "greater"),
    exam_pass = list(
      p0 = 0.60, p_true = 0.68, n_default = 50,
      success_label = "zdał", failure_label = "nie zdał",
      title = "Zdawalność egzaminu",
      question = "Czy zdawalność różni się od 60% (wartość historyczna)?",
      h0_text = "\\(H_0: p = 0.60\\) (zdawalność typowa)",
      h1_text = "\\(H_a: p \\neq 0.60\\) (zdawalność odbiega od normy)",
      question_1s = "Czy zdawalność jest wyższa niż historyczne 60%?",
      h0_text_1s = "\\(H_0: p \\leq 0.60\\)",
      h1_text_1s = "\\(H_a: p > 0.60\\)",
      alt_1s = "greater"),
    germination = list(
      p0 = 0.90, p_true = 0.86, n_default = 50,
      success_label = "wykiełkowało", failure_label = "nie wykiełkowało",
      title = "Kiełkowalność nasion",
      question = "Czy kiełkowalność partii nasion różni się od deklarowanych 90%?",
      h0_text = "\\(H_0: p = 0.90\\) (kiełkowalność zgodna z deklaracją)",
      h1_text = "\\(H_a: p \\neq 0.90\\) (kiełkowalność odbiega)",
      question_1s = "Czy kiełkowalność jest niższa niż deklarowane 90%?",
      h0_text_1s = "\\(H_0: p \\geq 0.90\\)",
      h1_text_1s = "\\(H_a: p < 0.90\\)",
      alt_1s = "less"),
    defects = list(
      p0 = 0.03, p_true = 0.06, n_default = 50,
      success_label = "poza normą", failure_label = "w normie",
      title = "Kontrola jakości produktów",
      question = "Czy odsetek produktów nie spełniających normy różni się od dopuszczalnych 3%?",
      h0_text = "\\(H_0: p = 0.03\\) (odsetek wadliwych zgodny z normą)",
      h1_text = "\\(H_a: p \\neq 0.03\\) (odsetek odbiega od normy)",
      question_1s = "Czy odsetek produktów poza normą przekracza dopuszczalne 3%?",
      h0_text_1s = "\\(H_0: p \\leq 0.03\\)",
      h1_text_1s = "\\(H_a: p > 0.03\\)",
      alt_1s = "greater"),
    helmets = list(
      p0 = 0.95, p_true = 0.88, n_default = 80,
      success_label = "nosi kask", failure_label = "bez kasku",
      title = "Używanie kasków na budowie",
      question = "Czy odsetek pracowników używających kasków odbiega od zakładanych 95%?",
      h0_text = "\\(H_0: p = 0.95\\) (odsetek zgodny z wymaganiem)",
      h1_text = "\\(H_a: p \\neq 0.95\\) (odsetek odbiega od wymagania)",
      question_1s = "Czy odsetek pracowników używających kasków jest niższy niż wymagane 95%?",
      h0_text_1s = "\\(H_0: p \\geq 0.95\\)",
      h1_text_1s = "\\(H_a: p < 0.95\\)",
      alt_1s = "less")
  )

  # --- Wspoldzielone dane ---
  # Jedna probka dla testu dwustronnego i jednostronnego; po zmianie
  # scenariusza albo n stara probka nie jest juz zgodna z pytaniem.
  ch3_data_state <- reactiveVal(NULL)
  ch3_data <- reactive({
    state <- ch3_data_state()
    if (is.null(state)) return(NULL)
    req(input$ch3_scenario, input$ch3_n)

    if (!identical(state$scenario, input$ch3_scenario) ||
        !isTRUE(state$n == input$ch3_n)) {
      return(NULL)
    }

    list(k = state$k, n = state$n)
  })
  ch3_step <- reactiveVal(0)
  ch3b_step <- reactiveVal(0)

  observeEvent(input$ch3_new_sample, {
    req(input$ch3_scenario, input$ch3_n)
    par <- scenario_params[[input$ch3_scenario]]
    req(!is.null(par))
    n <- input$ch3_n
    k <- rbinom(1, n, par$p_true)
    ch3_data_state(list(
      scenario = input$ch3_scenario,
      n = n,
      k = k
    ))
    ch3_step(0)
    ch3b_step(0)
  }, ignoreInit = TRUE)

  observeEvent(list(input$ch3_scenario, input$ch3_n), {
    ch3_step(0)
    ch3b_step(0)
  }, ignoreInit = TRUE)

  observeEvent(input$ch3_step1, ch3_step(1))
  observeEvent(input$ch3_step3, ch3_step(3))
  observeEvent(input$ch3_step4, ch3_step(4))

  observeEvent(input$ch3b_step1, ch3b_step(1))
  observeEvent(input$ch3b_step3, ch3b_step(3))
  observeEvent(input$ch3b_step4, ch3b_step(4))

  # =============================================
  # WIDGET 1: Dwustronny
  # =============================================

  output$ch3_hypothesis_panel <- renderUI({
    par <- scenario_params[[input$ch3_scenario]]
    d <- ch3_data()

    tagList(
      lc_feedback(type = "info", style = "font-size: 16px;",
        p(tags$b("Pytanie potoczne:")),
        p(tags$em(paste0("„", par$question, "”")))
      ),
      lc_formula_box(
        p(tags$b("Hipoteza formalna (dwustronna):")),
        p(withMathJax(par$h0_text)),
        p(withMathJax(par$h1_text))
      ),
      if (is.null(d)) {
        div(style = "text-align: center; margin: 10px 0; color: var(--upwr-reference);",
          p(tags$em("Kliknij „Losuj próbę”"))
        )
      }
    )
  })

  output$ch3_step_plot <- renderPlot({
    d <- ch3_data()
    step <- ch3_step()
    par <- scenario_params[[input$ch3_scenario]]

    if (is.null(d)) return(NULL)

    k <- d$k; n <- d$n; p0 <- par$p0

    if (step == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Próba gotowa! Klikaj kroki po kolei.",
                 size = 5, color = upwr_reference) +
        theme_void()
    } else if (step == 1) {
      # Krok 1: slupki sukces/porazka z proporcją
      phat <- k / n
      df <- data.frame(
        kat = c(par$success_label, par$failure_label),
        count = c(k, n - k)
      )
      df$kat <- factor(df$kat, levels = c(par$success_label, par$failure_label))

      ggplot(df, aes(x = kat, y = count, fill = kat)) +
        geom_col(alpha = 0.8, width = 0.6) +
        geom_text(aes(label = count), vjust = -0.5, size = 5, fontface = "bold") +
        scale_fill_manual(values = c(col_accept, col_reject)) +
        annotate("text", x = 1.5, y = max(k, n - k) * 0.7,
                 label = paste0("p̂ = ", k, "/", n, " = ", round(phat, 3)),
                 size = 5, color = col_pvalue, fontface = "bold") +
        labs(x = NULL, y = "Liczba") +
        theme(legend.position = "none")
    } else {
      # Krok 3-4: rozklad dwumianowy pod H0
      x_vals <- 0:n
      probs <- dbinom(x_vals, n, p0)
      df <- data.frame(x = x_vals, prob = probs)

      # Wyznacz skrajne wartosci (dwustronnie)
      if (step == 4) {
        p_lower <- pbinom(k, n, p0)
        p_upper <- 1 - pbinom(k - 1, n, p0)
        # Dwustronna p-wartosc
        p_val <- binom.test(k, n, p0, alternative = "two.sided")$p.value
        df$extreme <- dbinom(x_vals, n, p0) <= dbinom(k, n, p0)
      } else {
        df$extreme <- FALSE
      }

      ggplot(df, aes(x = x, y = prob, fill = extreme)) +
        geom_col(width = 0.8, alpha = 0.7) +
        geom_vline(xintercept = k, color = col_reject, linewidth = 1.2) +
        scale_fill_manual(values = c("TRUE" = col_pvalue, "FALSE" = col_h0),
                          guide = "none") +
        annotate("text", x = k, y = max(probs) * 0.9,
                 label = paste0("k = ", k),
                 hjust = if (k > n * p0) -0.2 else 1.2,
                 color = col_reject, fontface = "bold") +
        labs(
             x = "Liczba sukcesów", y = "Prawdopodobieństwo") +
        theme()
    }
  })

  output$ch3_step_info <- renderUI({
    d <- ch3_data()
    step <- ch3_step()
    par <- scenario_params[[input$ch3_scenario]]

    if (is.null(d) || step == 0) return(NULL)

    k <- d$k; n <- d$n; p0 <- par$p0; phat <- k / n

    info <- switch(as.character(step),
      "1" = tagList(
        lc_stat_box("n", n, color = col_h0),
        lc_stat_box("p̂", k, "/", n, " = ", round(phat, 3),
                    caption = paste0(par$success_label, ": ", k),
                    color = col_accept),
        lc_stat_box("p₀", p0, color = upwr_secondary),
        p("Proporcja z próby: ", tags$b(round(phat, 3)),
          ". Wartość referencyjna: ", tags$b(p0),
          ". Różnica: ", tags$b(round(phat - p0, 3)),
          ". Ale czy to dużo?")
      ),
      "3" = tagList(
        p("Rozkład dwumianowy B(", n, ", ", p0,
          ") pokazuje ile sukcesów ",
          tags$em("spodziewalibyśmy się"), " gdyby H₀ była prawdziwa."),
        p("Czerwona linia = nasz wynik k = ", tags$b(k),
          ". Czy wypada w centrum czy na obrzeżach?")
      ),
      "4" = {
        test <- binom.test(k, n, p0, alternative = "two.sided")
        res <- format_test_result(test$p.value)
        tagList(
          lc_stat_box("p", format_p_value(test$p.value), color = col_pvalue),
          p(style = paste0("color: ", res$color, "; font-weight: bold; font-size: 16px;"),
            res$decision),
          p(res$explanation)
        )
      }
    )
    lc_feedback(type = "info", info)
  })

  # =============================================
  # WIDGET 2: Jednostronny (te same dane)
  # =============================================

  output$ch3b_hypothesis_panel <- renderUI({
    par <- scenario_params[[input$ch3_scenario]]
    d <- ch3_data()

    tagList(
      lc_feedback(type = "info", style = "font-size: 16px;",
        p(tags$b("Pytanie potoczne (kierunkowe):")),
        p(tags$em(paste0("„", par$question_1s, "”")))
      ),
      lc_formula_box(
        p(tags$b("Hipoteza formalna (jednostronna!):")),
        p(withMathJax(par$h0_text_1s)),
        p(withMathJax(par$h1_text_1s))
      ),
      if (is.null(d)) {
        div(style = "text-align: center; margin: 10px 0; color: var(--upwr-reference);",
          p(tags$em("Najpierw wylosuj próbę w teście dwustronnym powyżej"))
        )
      }
    )
  })

  output$ch3b_step_plot <- renderPlot({
    d <- ch3_data()
    step <- ch3b_step()
    par <- scenario_params[[input$ch3_scenario]]

    if (is.null(d) || step == 0) return(NULL)

    k <- d$k; n <- d$n; p0 <- par$p0

    if (step == 1) {
      phat <- k / n
      df <- data.frame(
        kat = c(par$success_label, par$failure_label),
        count = c(k, n - k)
      )
      df$kat <- factor(df$kat, levels = c(par$success_label, par$failure_label))

      ggplot(df, aes(x = kat, y = count, fill = kat)) +
        geom_col(alpha = 0.8, width = 0.6) +
        geom_text(aes(label = count), vjust = -0.5, size = 5, fontface = "bold") +
        scale_fill_manual(values = c(col_accept, col_reject)) +
        annotate("text", x = 1.5, y = max(k, n - k) * 0.7,
                 label = paste0("p̂ = ", round(phat, 3), " (te same dane)"),
                 size = 5, color = col_pvalue, fontface = "bold") +
        labs(x = NULL, y = "Liczba") +
        theme(legend.position = "none")
    } else {
      # Krok 3-4: rozklad z zaznaczonym jednym ogonem
      x_vals <- 0:n
      probs <- dbinom(x_vals, n, p0)
      df <- data.frame(x = x_vals, prob = probs)

      if (step == 4) {
        if (par$alt_1s == "greater") {
          df$extreme <- x_vals >= k
        } else {
          df$extreme <- x_vals <= k
        }
      } else {
        df$extreme <- FALSE
      }

      ggplot(df, aes(x = x, y = prob, fill = extreme)) +
        geom_col(width = 0.8, alpha = 0.7) +
        geom_vline(xintercept = k, color = col_reject, linewidth = 1.2) +
        scale_fill_manual(values = c("TRUE" = col_pvalue, "FALSE" = col_h0),
                          guide = "none") +
        annotate("text", x = k, y = max(probs) * 0.9,
                 label = paste0("k = ", k),
                 hjust = if (k > n * p0) -0.2 else 1.2,
                 color = col_reject, fontface = "bold") +
        labs(
             
             x = "Liczba sukcesów", y = "Prawdopodobieństwo") +
        theme()
    }
  })

  output$ch3b_step_info <- renderUI({
    d <- ch3_data()
    step <- ch3b_step()
    par <- scenario_params[[input$ch3_scenario]]

    if (is.null(d) || step == 0) return(NULL)

    k <- d$k; n <- d$n; p0 <- par$p0; phat <- k / n
    dir_label <- if (par$alt_1s == "greater") "większa" else "mniejsza"

    info <- switch(as.character(step),
      "1" = tagList(
        lc_stat_box("n", n, " (te same dane co wyżej)", color = col_h0),
        lc_stat_box("p̂", round(phat, 3), " (ta sama wartość!)", color = col_pvalue),
        p("Statystyki takie same — dane się nie zmieniły. Zmieniło się tylko pytanie (kierunek).")
      ),
      "3" = tagList(
        p("Ten sam rozkład B(", n, ", ", p0,
          "), ale teraz patrzymy tylko na ",
          tags$b(if (par$alt_1s == "greater") "prawy" else "lewy"), " ogon.")
      ),
      "4" = {
        test <- binom.test(k, n, p0, alternative = par$alt_1s)
        res <- format_test_result(test$p.value)
        tagList(
          lc_stat_box("p", format_p_value(test$p.value),
                     " (jednostronnie!)", color = col_pvalue),
          p(style = paste0("color: ", res$color, "; font-weight: bold; font-size: 16px;"),
            res$decision),
          p(res$explanation),
          p(tags$em("Porównaj z testem dwustronnym wyżej — te same dane,
            ale inna p-wartość!"))
        )
      }
    )
    lc_feedback(type = "info", info)
  })

  # =============================================
  # WIDGET 3: Porownanie dwumianowy vs proporcji
  # =============================================

  output$ch3_compare_result <- renderUI({
    req(input$ch3_compare)
    d <- isolate(ch3_data())
    par <- isolate(scenario_params[[input$ch3_scenario]])

    if (is.null(d)) {
      return(lc_feedback(type = "warning",
        "Najpierw wylosuj próbę w widgecie powyżej."))
    }

    k <- d$k; n <- d$n; p0 <- par$p0; phat <- k / n

    # Test dwumianowy
    binom_res <- binom.test(k, n, p0, alternative = "two.sided")

    # Test proporcji (z-test z poprawką ciągłości)
    prop_res <- prop.test(k, n, p = p0, alternative = "two.sided", correct = TRUE)

    # Statystyka z ręcznie
    z_stat <- (phat - p0) / sqrt(p0 * (1 - p0) / n)

    # Warunki przybliżenia normalnego
    np0 <- n * p0
    nq0 <- n * (1 - p0)
    ok <- np0 >= 10 && nq0 >= 10

    div(
      tags$table(class = "lc-table lc-table-bordered", style = "font-size: 15px;",
        tags$thead(
          tags$tr(tags$th(""), tags$th("Test dwumianowy"), tags$th("Test proporcji (z)"))
        ),
        tags$tbody(
          tags$tr(
            tags$td(tags$b("Dane")),
            tags$td(paste0("k = ", k, ", n = ", n)),
            tags$td(paste0("k = ", k, ", n = ", n))
          ),
          tags$tr(
            tags$td(tags$b("Statystyka")),
            tags$td(paste0("k = ", k, " (dokładna)")),
            tags$td(paste0("z = ", round(z_stat, 3)))
          ),
          tags$tr(
            tags$td(tags$b("p-wartość")),
            tags$td(tags$b(format_p_value(binom_res$p.value))),
            tags$td(tags$b(format_p_value(prop_res$p.value)))
          ),
          tags$tr(
            tags$td(tags$b("Decyzja")),
            tags$td(style = paste0("color:", format_test_result(binom_res$p.value)$color),
                    format_test_result(binom_res$p.value)$decision),
            tags$td(style = paste0("color:", format_test_result(prop_res$p.value)$color),
                    format_test_result(prop_res$p.value)$decision)
          )
        )
      ),
      lc_feedback(type = if (ok) "ok" else "danger",
        p(tags$b("Warunki przybliżenia normalnego: "),
          withMathJax(paste0("\\(np_0 = ", round(np0, 1), "\\)")),
          " i ",
          withMathJax(paste0("\\(n(1-p_0) = ", round(nq0, 1), "\\)")),
          if (ok) " — oba ≥ 10, przybliżenie działa dobrze."
          else " — warunek niespiełniony! Test proporcji może być niedokładny.")
      )
    )
  })

  # --- Cwiczenia CASchools ---

  cas_vis_a <- reactiveVal(FALSE)
  cas_vis_b <- reactiveVal(FALSE)

  observeEvent(input$cas_ch3_ans_a, {
    nowy <- !cas_vis_a()
    cas_vis_a(nowy)
    updateActionButton(session, "cas_ch3_ans_a",
      label = if (nowy) "Ukryj rozwiązanie" else "Pokaż rozwiązanie")
  }, ignoreInit = TRUE)

  output$cas_ch3_sol_a <- renderUI({
    if (!cas_vis_a()) return(NULL)
    r <- local({
      k <- sum(.ch3_cas$grades == "KK-06")
      n <- nrow(.ch3_cas)
      p_obs <- k / n
      bt <- binom.test(k, n, p = 0.5, alternative = "two.sided")
      list(k = k, n = n, p_obs = p_obs, p_val = bt$p.value,
           ci_lo = bt$conf.int[1], ci_hi = bt$conf.int[2])
    })
    lc_feedback(type = "ok", style = "margin-top: 10px;",
      p(tags$b("H₀: "), "p_KK06 = 0.5 · ", tags$b("Hₐ: "), "p_KK06 ≠ 0.5"),
      tags$ul(
        tags$li(sprintf("k = %d, n = %d, p̂ = %.3f (%.1f%%)",
                        r$k, r$n, r$p_obs, 100 * r$p_obs)),
        tags$li(sprintf("p %s %s (test dwumianowy, dwustronny)",
          if (r$p_val < 0.001) "<" else "=",
          if (r$p_val < 0.001) "0.001" else format(round(r$p_val, 4), nsmall = 4))),
        tags$li(sprintf("95%% CI: [%.3f, %.3f]", r$ci_lo, r$ci_hi))
      ),
      if (r$p_val < 0.05) tags$b(style = paste0("color:", upwr_accent), "Odrzucamy H₀")
      else tags$b("Brak podstaw do odrzucenia H₀"),
      p(tags$b("Interpretacja: "),
        sprintf(
          "%.1f%% okręgów to szkoły KK-06. Odsetek istotnie %s się od 50%%
           (p %s 0.05) — szkoły KK-06 %s dominują.",
          100 * r$p_obs,
          if (r$p_val < 0.05) "różni" else "nie różni",
          if (r$p_val < 0.05) "<" else ">",
          if (r$p_obs > 0.5 && r$p_val < 0.05) "istotnie" else "nieistotnie"
        ))
    )
  })

  observeEvent(input$cas_ch3_ans_b, {
    nowy <- !cas_vis_b()
    cas_vis_b(nowy)
    updateActionButton(session, "cas_ch3_ans_b",
      label = if (nowy) "Ukryj rozwiązanie" else "Pokaż rozwiązanie")
  }, ignoreInit = TRUE)

  output$cas_ch3_sol_b <- renderUI({
    if (!cas_vis_b()) return(NULL)
    r <- local({
      high_lunch <- .ch3_cas$lunch > 50
      k <- sum(high_lunch)
      n <- length(high_lunch)
      p_obs <- k / n
      bt <- binom.test(k, n, p = 0.30, alternative = "greater")
      list(k = k, n = n, p_obs = p_obs, p_val = bt$p.value,
           ci_lo = bt$conf.int[1], ci_hi = bt$conf.int[2])
    })
    lc_feedback(type = "ok", style = "margin-top: 10px;",
      p(tags$b("H₀: "), "p_ubóstwo ≤ 0.30 · ",
        tags$b("Hₐ: "), "p_ubóstwo > 0.30"),
      tags$ul(
        tags$li(sprintf("k = %d okręgów z lunch > 50%%, n = %d, p̂ = %.3f (%.1f%%)",
                        r$k, r$n, r$p_obs, 100 * r$p_obs)),
        tags$li(sprintf("p %s %s (test dwumianowy, jednostronny prawy)",
          if (r$p_val < 0.001) "<" else "=",
          if (r$p_val < 0.001) "0.001" else format(round(r$p_val, 4), nsmall = 4))),
        tags$li(sprintf("95%% CI dolne: %.3f", r$ci_lo))
      ),
      if (r$p_val < 0.05) tags$b(style = paste0("color:", upwr_accent), "Odrzucamy H₀")
      else tags$b("Brak podstaw do odrzucenia H₀"),
      p(tags$b("Interpretacja: "),
        sprintf(
          "%.1f%% okręgów ma wysoki poziom ubóstwa (lunch > 50%%).
           Odsetek ten istotnie %s normę 30%% (p %s 0.05).",
          100 * r$p_obs,
          if (r$p_val < 0.05) "przekracza" else "nie przekracza",
          if (r$p_val < 0.05) "<" else ">"
        ))
    )
  })
}
