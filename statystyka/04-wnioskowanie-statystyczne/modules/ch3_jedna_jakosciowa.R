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
      p("Narzędzie: ", gloss("test dwumianowy"),
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
      p("W teście dwumianowym ", gloss("statystyka testowa", "statystyką testową"), " jest sama liczba sukcesów ",
        withMathJax("\\(k\\)"),
        " — nie trzeba jej standaryzować, bo pod H₀ zna jej rozkład dokładnie
        (to ", gloss("rozkład dwumianowy"), " ", withMathJax("\\(B(n, p_0)\\)"),
        "). ", gloss("p-wartość"), " liczymy bezpośrednio jako prawdopodobieństwo wyniku co najmniej
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
      uiOutput("ch3_hypothesis_panel"),
      lc_step_widget("ch3_test",
        steps = c("Dane", "Rozkład pod H₀", "p-wartość i decyzja"),
        toolbar = lc_toolbar(
          selectInput("ch3_scenario", "Scenariusz",
            choices = c(
              "Jakość wody (p₀ = 80%)" = "water_quality",
              "Zdawalność egzaminu (p₀ = 60%)" = "exam_pass",
              "Kiełkowalność nasion (p₀ = 90%)" = "germination",
              "Produkty poza normą (p₀ = 3%)" = "defects",
              "Używanie kasków na budowie (p₀ = 95%, IB)" = "helmets"
            ),
            selected = "water_quality"
          ),
          lc_slider("ch3_n", "Wielkość próby (n)", 20, 200, 50, 10),
          lc_action("ch3_new_sample", "Losuj próbę", icon = "shuffle", variant = "solid")
        ),
        plot_id = "ch3_step_plot"
      )
    ),

    inline_callout(
      label = "Co zrobiliśmy?",
      tagList(
        tags$ol(
          tags$li("Zebraliśmy dane i obliczyliśmy ", gloss("proporcja z próby", "proporcję z próby"), ": ",
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
      uiOutput("ch3b_hypothesis_panel"),
      lc_step_widget("ch3b_test",
        steps = c("Dane", "Rozkład pod H₀", "p-wartość i decyzja"),
        toolbar = lc_toolbar(
          helpText("Dane: te same co w teście dwustronnym powyżej.")
        ),
        plot_id = "ch3b_step_plot"
      )
    ),

    inline_callout(
      label = "Dwu- a jednostronny",
      tagList(
        tags$ul(
          tags$li(tags$b("Dwustronny (≠):"), " p-wartość liczymy po obu stronach. Bezpieczniejszy."),
          tags$li(tags$b("Jednostronny (> lub <):"), " p-wartość tylko po jednej stronie. Mocniejszy, ale ślepy na efekt w drugą stronę.")
        ),
        tags$p("Te same dane, ten sam wynik k/n, ale inna p-wartość",
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
        "test proporcji (z-test)",
        ". Działa na przybliżeniu normalnym:"),
      lc_formula_box(
        p(withMathJax("\\(z = \\frac{\\hat{p} - p_0}{\\sqrt{p_0(1-p_0)/n}}\\)"))
      ),
      p("Porównajmy oba testy na tych samych danych:")
    ),

    figure_panel(
      label = "Ryc. 5.3",
      title = "Porównanie wyników: dwumianowy vs z-test",
      lc_action("ch3_compare", "Porównaj testy", variant = "solid"),
      br(), br(),
      uiOutput("ch3_compare_result")
    ),

    tagList(
      p("Kiedy który?"),
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
      p("Reguła kciuka:",
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
      p("Okręgi dzielą się na szkoły klas KK-06 i KK-08. Przetestuj
        dwustronnie, czy odsetek okręgów KK-06 różni się od 50%.
        Sformułuj H₀ i Hₐ, oblicz p-wartość testem dwumianowym (α = 0.05).
        Jak interpretujesz wynik?"),
      lc_action("cas_ch3_ans_a", "Pokaż rozwiązanie", variant = "solid"),
      uiOutput("cas_ch3_sol_a")
    ),

    figure_panel(label = "Ćwiczenie",
      h4("Zadanie B — Czy więcej niż 30% okręgów ma wysoki poziom ubóstwa?"),
      p("Przyjmij, że okrąg ma wysoki poziom ubóstwa, gdy ", tags$code("lunch > 50%"),
        ". Przetestuj jednostronnie (prawostronnie),
        czy odsetek takich okręgów przekracza normę 30%.
        Sformułuj H₀ i Hₐ, wykonaj test dwumianowy. Jaki wniosek?"),
      lc_action("cas_ch3_ans_b", "Pokaż rozwiązanie", variant = "solid"),
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

  # Kroki widgetów (1..3) żyją w przeglądarce; nowa próba ani zmiana
  # scenariusza nie cofa kroku.
  ch3_step <- lc_step_server("ch3_test", input)$step
  ch3b_step <- lc_step_server("ch3b_test", input)$step

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
  }, ignoreInit = TRUE)

  # Krok 1: słupki sukces / porażka (dane i druga kategoria) z proporcją.
  ch3_counts_plot <- function(k, n, par, phat_label) {
    df <- data.frame(
      kat = factor(c(par$success_label, par$failure_label),
                   levels = c(par$success_label, par$failure_label)),
      count = c(k, n - k)
    )
    y_top <- max(k, n - k) * 1.2

    ggplot(df, aes(x = kat, y = count)) +
      step_result(geom_col, data = df[1, ], width = 0.6) +
      step_result(geom_col, data = df[2, ], width = 0.6,
                  fill = STEP_ROLES$group$colour) +
      geom_text(aes(label = count), vjust = -0.5, size = 5, fontface = "bold",
                family = "mono", colour = STEP_ROLES$known$colour) +
      step_symbol_label(1.5, max(k, n - k) * 0.7, phat_label, role = "new",
                        hjust = 0.5, size = 5) +
      labs(x = NULL, y = "Liczba") +
      step_frame(xlim = c(0.4, 2.6), ylim = c(0, y_top))
  }

  # Kroki 2–3: rozkład dwumianowy pod H₀; extreme = słupki p-wartości.
  # Rama: zakres z niepomijalnym prawdopodobieństwem plus wynik k.
  ch3_binom_plot <- function(k, n, p0, extreme, step) {
    df <- data.frame(x = 0:n, prob = dbinom(0:n, n, p0), extreme = extreme)
    shown <- df$x[df$prob >= max(df$prob) * 1e-3]
    xlim <- range(c(shown, k)) + c(-1.5, 1.5)
    y_top <- max(df$prob) * 1.15
    k_role <- step_role(step, 2)

    ggplot(df, aes(x = x, y = prob)) +
      step_result(geom_col, data = df[!(df$extreme & step >= 3), ], width = 0.8) +
      step_show(step, 3, step_result(geom_col, data = df[df$extreme, ], width = 0.8,
                                     fill = STEP_ROLES$new$colour)) +
      step_line(k_role, xintercept = k, helper = FALSE) +
      step_label(k, y_top * 0.9, paste0("k = ", k), role = k_role,
                 hjust = if (k > n * p0) -0.2 else 1.2) +
      labs(x = "Liczba sukcesów", y = "Prawdopodobieństwo") +
      step_frame(xlim = xlim, ylim = c(0, y_top))
  }


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

  zoom_plot_server("ch3_step_plot", reactive({
    d <- ch3_data()
    step <- ch3_step()
    par <- scenario_params[[input$ch3_scenario]]

    if (is.null(d)) return(NULL)

    k <- d$k; n <- d$n; p0 <- par$p0

    if (step == 1) {
      ch3_counts_plot(k, n, par,
                      paste0("hat(p) == '", k, "/", n, " = ", lc_fmt(k / n, 3), "'"))
    } else {
      # Wartości co najmniej tak mało prawdopodobne jak k (dwustronnie)
      extreme <- dbinom(0:n, n, p0) <= dbinom(k, n, p0)
      ch3_binom_plot(k, n, p0, extreme, step)
    }
  }))

  output$ch3_test_text <- renderUI({
    d <- ch3_data()
    step <- ch3_step()
    par <- scenario_params[[input$ch3_scenario]]

    if (is.null(d)) return(NULL)

    k <- d$k; n <- d$n; p0 <- par$p0; phat <- k / n

    switch(as.character(step),
      "1" = tagList(
        "n = ", step_num(n), paste0(", ", par$success_label, ": "), step_num(k),
        paste0(". Proporcja z próby: p̂ = ", k, "/", n, " = "), step_num(lc_fmt(phat, 3)),
        ". Wartość referencyjna: p₀ = ", step_num(p0),
        ". Różnica: ", step_num(lc_fmt(phat - p0, 3)), ". Ale czy to dużo?"
      ),
      "2" = tagList(
        paste0("Rozkład dwumianowy B(", n, ", ", p0, ") pokazuje ile sukcesów "),
        tags$em("spodziewalibyśmy się", .noWS = "outside"),
        " gdyby H₀ była prawdziwa. Pionowa linia = nasz wynik k = ", step_num(k),
        ". Czy wypada w centrum czy na obrzeżach?"
      ),
      "3" = step_verdict(binom.test(k, n, p0, alternative = "two.sided")$p.value)
    )
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

  zoom_plot_server("ch3b_step_plot", reactive({
    d <- ch3_data()
    step <- ch3b_step()
    par <- scenario_params[[input$ch3_scenario]]

    if (is.null(d)) return(NULL)

    k <- d$k; n <- d$n; p0 <- par$p0

    if (step == 1) {
      ch3_counts_plot(k, n, par,
                      paste0("hat(p) == ", lc_fmt(k / n, 3), " ~ '(te same dane)'"))
    } else {
      # Jeden ogon: wartości co najmniej tak skrajne jak k w kierunku Hₐ
      extreme <- if (par$alt_1s == "greater") 0:n >= k else 0:n <= k
      ch3_binom_plot(k, n, p0, extreme, step)
    }
  }))

  output$ch3b_test_text <- renderUI({
    d <- ch3_data()
    step <- ch3b_step()
    par <- scenario_params[[input$ch3_scenario]]

    if (is.null(d)) return(NULL)

    k <- d$k; n <- d$n; p0 <- par$p0; phat <- k / n

    switch(as.character(step),
      "1" = tagList(
        "n = ", step_num(n), " (te same dane co wyżej), p̂ = ",
        step_num(lc_fmt(phat, 3)), " (ta sama wartość!). Statystyki takie same —
        dane się nie zmieniły. Zmieniło się tylko pytanie (kierunek)."
      ),
      "2" = tagList(
        paste0("Ten sam rozkład B(", n, ", ", p0, "), ale teraz patrzymy tylko na ",
               if (par$alt_1s == "greater") "prawy" else "lewy", " ogon.")
      ),
      "3" = tagList(
        "Jednostronnie: ",
        step_verdict(binom.test(k, n, p0, alternative = par$alt_1s)$p.value), " ",
        tags$em("Porównaj z testem dwustronnym wyżej — te same dane,
          ale inna p-wartość!")
      )
    )
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
