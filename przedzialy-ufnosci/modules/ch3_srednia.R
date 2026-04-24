# ============================================================================
# CHAPTER 3: Przedzial dla sredniej
# ============================================================================

ch3_ui <- list(
  id    = "ch-srednia",
  num   = "03",
  title = "Przedział dla średniej",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 03 · Przedziały ufności",
      num    = "03",
      title  = "Przedział dla średniej.",
      lead   = "Wiemy już, czym jest przedział ufności i jak go
                interpretować. Czas na konkrety: wzór i obliczenia."
    ),

    lc_h2("ch3-wzor", "Wzór"),

    tagList(
      p("Przedział ufności dla średniej populacji wygląda tak:"),
      lc_formula_box(
        withMathJax("$$CI = \\bar{x} \\pm t^*_{\\alpha/2,\\, n-1} \\cdot \\frac{s}{\\sqrt{n}}$$")
      ),
      p("Trzy składniki:"),
      tags$ul(
        tags$li(withMathJax("\\(\\bar{x}\\)"),
                " — średnia z próby (środek przedziału)"),
        tags$li(withMathJax("\\(s/\\sqrt{n}\\)"),
                " — błąd standardowy średniej (jak bardzo średnia z próby waha się z próby na próbę)"),
        tags$li(withMathJax("\\(t^*\\)"),
                " — wartość krytyczna z rozkładu t-Studenta zależna od poziomu ufności i ", withMathJax("\\(n-1\\)"), " stopni swobody")
      ),
      p(tags$b("Dlaczego rozkład t, a nie normalny (z)?"),
        " Bo ", withMathJax("\\(\\sigma\\)"), " populacji nie znamy — szacujemy je z próby jako ",
        withMathJax("\\(s\\)"),
        ". To dodaje niepewności, dlatego używamy szerszego rozkładu (t ma \"grubsze ogony\" niż normalny).
        Im większe ", withMathJax("\\(n\\)"),
        ", tym lepsze oszacowanie ", withMathJax("\\(\\sigma\\)"),
        " i tym bardziej rozkład t przypomina normalny."),
      p("W praktyce nie musisz się tym przejmować. Programy statystyczne (jamovi, SPSS, R) ",
        tags$em("zawsze"),
        " liczą CI dla średniej używając rozkładu t. Nie ma osobnego \"z-przedziału\"
        do wyboru. Ten rozdział nauczy Cię interpretować gotowe przedziały — a nie liczyć je ręcznie.")
    ),

    margin_callout(label = "W jamovi", color = "wskazowka",
      tagList(
        "Analyses → T-Tests → One Sample T-Test → przeciągnij zmienną
         ilościową do Dependent Variables → w panelu Additional
         Statistics zaznacz Confidence interval (domyślnie 95%).
         W tabeli wyników odczytasz kolumny ",
        tags$code("Mean"), ", ", tags$code("Lower"), ", ",
        tags$code("Upper"), "."
      )
    ),

    lc_h2("ch3-budowa", "Budowa przedziału — krok po kroku"),

    tagList(
      p("Zobaczmy, jak z konkretnej próby (25 pomiarów wzrostu) powstaje
        przedział ufności. Przejdź przez 4 kroki, obserwując co pojawia
        się na wykresie.")
    ),

    figure_panel(
      label = "Ryc. 3.1", title = "Konstruowanie przedziału",
      full_width = TRUE,
      div(class = "step-buttons",
        actionButton("ch3_step1", "1. Próba",    class = "lc-btn-outline"),
        actionButton("ch3_step2", "2. Średnia",  class = "lc-btn-outline"),
        actionButton("ch3_step3", "3. ± SE",     class = "lc-btn-outline"),
        actionButton("ch3_step4", "4. Przedział", class = "lc-btn-outline")
      ),
      lc_inline_row(gap = "md",
        actionButton("ch3_step_new_sample", "↻ Nowa próba",
                     class = "lc-btn-secondary-outline lc-btn-sm")
      ),
      plotOutput("ch3_step_plot", height = "340px"),
      uiOutput("ch3_step_explanation")
    ),

    lc_h2("ch3-roznica", "Budowa przedziału dla różnicy średnich"),

    tagList(
      p("CI dla różnicy dwóch średnich buduje się analogicznie, ale błąd
        standardowy jest inny — trzeba połączyć niepewność z obu prób:"),
      lc_formula_box(
        withMathJax("$$CI = (\\bar{x}_1 - \\bar{x}_2) \\pm t^* \\cdot \\sqrt{\\frac{s_1^2}{n_1} + \\frac{s_2^2}{n_2}}$$")
      ),
      p("Porównamy wzrost mężczyzn i kobiet — po 25 osób w każdej grupie.")
    ),

    figure_panel(
      label = "Ryc. 3.2", title = "Konstruowanie CI dla różnicy",
      full_width = TRUE,
      div(class = "step-buttons",
        actionButton("ch3_dstep1", "1. Dwie próby",   class = "lc-btn-outline"),
        actionButton("ch3_dstep2", "2. Dwie średnie", class = "lc-btn-outline"),
        actionButton("ch3_dstep3", "3. Różnica",      class = "lc-btn-outline"),
        actionButton("ch3_dstep4", "4. ± SE",         class = "lc-btn-outline"),
        actionButton("ch3_dstep5", "5. Przedział",    class = "lc-btn-outline")
      ),
      lc_inline_row(gap = "md",
        actionButton("ch3_dstep_new_sample", "↻ Nowe próby",
                     class = "lc-btn-secondary-outline lc-btn-sm")
      ),
      plotOutput("ch3_dstep_plot", height = "420px"),
      uiOutput("ch3_dstep_explanation")
    ),

    lc_h2("ch3-scenariusze", "Dwa CI grup czy CI różnicy? — trzy scenariusze"),

    tagList(
      p("Gdy porównujesz dwie grupy, masz dwa sposoby, żeby spojrzeć na wynik:"),
      tags$ol(
        tags$li(tags$b("Dwa osobne CI"),
                " — rysujemy CI dla każdej grupy i patrzymy, czy się nakrywają."),
        tags$li(tags$b("CI różnicy"),
                " — liczymy bezpośrednio CI dla ", withMathJax("\\(\\mu_1 - \\mu_2\\)"),
                " i patrzymy, czy zawiera 0.")
      ),
      p("Zwykle dają tę samą odpowiedź. Ale nie zawsze — zobacz trzy przykłady
        z technologii żywności.")
    ),

    # --- Scenariusz A ---
    tags$details(class = "case-study", open = NA,
      tags$summary(
        span(class = "case-icon", "\U0001f33e"),
        "A. Dwaj dostawcy mąki — zgodne sygnały, różnica istotna"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Zakład piekarniczy porównuje dwóch dostawców mąki pszennej typu 550
            pod względem zawartości białka (%). Pobrano po 40 partii od każdego dostawcy.")
        ),
        plotOutput("ch3_comp_A_plot", height = "340px"),
        uiOutput("ch3_comp_A_verdict")
      )
    ),

    # --- Scenariusz B ---
    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f964"),
        "B. Jogurt w szkle vs w plastiku — zgodne sygnały, brak różnicy"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Technolog sprawdza, czy materiał opakowania wpływa na zawartość tłuszczu (%)
            w jogurcie naturalnym po 7 dniach przechowywania. Po 30 próbek z każdego typu.")
        ),
        plotOutput("ch3_comp_B_plot", height = "340px"),
        uiOutput("ch3_comp_B_verdict")
      )
    ),

    # --- Scenariusz C (PULAPKA) ---
    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U000026a0️"),
        "C. Dwie linie płatków — UWAGA, pułapka wzrokowa"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Zakład sprawdza, czy dwie linie produkcyjne płatków śniadaniowych
            dają produkt o tej samej zawartości błonnika (g / 100 g).
            Po 120 partii z każdej linii.")
        ),
        plotOutput("ch3_comp_C_plot", height = "340px"),
        uiOutput("ch3_comp_C_verdict")
      )
    ),

    margin_callout(label = "Zasada praktyczna", color = "uwaga",
      "Kiedy porównujesz grupy, patrz przede wszystkim na CI różnicy —
       to liczba, która uwzględnia niepewność obu pomiarów naraz.
       Porównywanie dwóch osobnych CI na oko to szybki skrót — często
       działa, ale przy granicznych różnicach potrafi wprowadzić w błąd
       (tak jak w scenariuszu C)."
    ),

    lc_h2("ch3-case-studies", "Case studies — jak interpretować CI w praktyce"),

    tagList(
      p("Poniżej kilka realistycznych sytuacji. W każdej budujesz CI krok po kroku
        (jak w poprzednich sekcjach), a na końcu weryfikujesz dwie hipotezy:
        jedną, która jest prawdziwa, i jedną, która nie jest. Klikaj nagłówki,
        żeby rozwijać case'y.")
    ),

    lc_h3("A. Przedział dla jednej średniej"),

    tags$details(class = "case-study", open = NA,
      tags$summary(
        span(class = "case-icon", "\U0001f4cf"),
        "A1. Wzrost studentów — czytanie pojedynczego CI"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Zmierzyłeś wzrost 30 studentów. Średnia z próby ",
            withMathJax("\\(\\bar{x} = 173.4\\)"), " cm,
            odchylenie standardowe ", withMathJax("\\(s = 8.2\\)"), " cm.
            Zbudujmy CI dla średniego wzrostu i sprawdźmy dwie hipotezy.")
        ),
        uiOutput("ch3_caseA1_buttons"),
        plotOutput("ch3_caseA1_plot", height = "260px"),
        uiOutput("ch3_caseA1_explain")
      )
    ),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f50d"),
        "A2. Ten sam pomiar, trzy różne wielkości próby"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Porównaj trzy badania mierzące stężenie zanieczyszczenia
            (µg/m³). Średnia = 32.0, s = 8.0, ale ",
            tags$b("n różne"), " (10, 50, 200). Dodawaj CI jeden po drugim
            i patrz, jak się zwężają.")
        ),
        uiOutput("ch3_caseA2_buttons"),
        plotOutput("ch3_caseA2_plot", height = "260px"),
        uiOutput("ch3_caseA2_explain")
      )
    ),

    lc_h3("B. Przedział dla różnicy średnich"),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f48a"),
        "B1. Test leku na ciśnienie — CI dla różnicy nie obejmuje 0"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Badamy nowy lek na obniżenie ciśnienia krwi.
            ", tags$b("Lek:"), " n=40, średnie obniżenie 12.3 mmHg, s=4.5.
            ", tags$b("Placebo:"), " n=40, średnie obniżenie 4.1 mmHg, s=4.2.")
        ),
        uiOutput("ch3_caseB1_buttons"),
        plotOutput("ch3_caseB1_plot", height = "380px"),
        uiOutput("ch3_caseB1_explain")
      )
    ),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f33f"),
        "B2. Dwa nawozy — CI dla różnicy obejmuje 0"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Porównujesz plon kukurydzy dla dwóch nawozów.
            ", tags$b("Nawoz X:"), " n=25, średnia 8.4 t/ha, s=1.2.
            ", tags$b("Nawoz Y:"), " n=25, średnia 8.1 t/ha, s=1.3.")
        ),
        uiOutput("ch3_caseB2_buttons"),
        plotOutput("ch3_caseB2_plot", height = "380px"),
        uiOutput("ch3_caseB2_explain")
      )
    ),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "⚠️"),
        "B3. Pułapka nakładających się CI"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Mierzysz czas reakcji w dwóch grupach (n=150 każda).
            ", tags$b("Grupa A:"), " średnia 350 ms, s=45.
            ", tags$b("Grupa B:"), " średnia 362 ms, s=45.
            CI każdej grupy osobno się nakładają — czy różnica jest istotna?")
        ),
        uiOutput("ch3_caseB3_buttons"),
        plotOutput("ch3_caseB3_plot", height = "380px"),
        uiOutput("ch3_caseB3_explain")
      )
    ),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f4ca"),
        "B4. Istotne statystycznie ≠ ważne praktycznie"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Bardzo duże badanie porównuje IQ w dwóch województwach.
            ", tags$b("Wojew. A:"), " n=20 000, średnia 100.4, s=15.
            ", tags$b("Wojew. B:"), " n=20 000, średnia 100.0, s=15.
            Różnica 0.4 pkt IQ — dużo czy mało?")
        ),
        uiOutput("ch3_caseB4_buttons"),
        plotOutput("ch3_caseB4_plot", height = "380px"),
        uiOutput("ch3_caseB4_explain")
      )
    ),

    lc_h3("C. Wiele grup — forest plot"),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f3eb"),
        "C1. Cztery metody nauczania — czy któraś wystaje?"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Porównujesz średni wynik egzaminu (0–40 pkt) dla studentów
            uczących się czterema metodami (po 25 studentów w każdej).
            Dodawaj CI jeden po drugim i obserwuj.")
        ),
        uiOutput("ch3_caseC1_buttons"),
        plotOutput("ch3_caseC1_plot", height = "300px"),
        uiOutput("ch3_caseC1_explain")
      )
    ),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f3e5"),
        "C2. Pięć oddziałów szpitalnych — czas oczekiwania"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Mierzysz średni czas oczekiwania na konsultację (minuty) w pięciu
            oddziałach szpitala. Który wymaga interwencji?")
        ),
        uiOutput("ch3_caseC2_buttons"),
        plotOutput("ch3_caseC2_plot", height = "340px"),
        uiOutput("ch3_caseC2_explain")
      )
    ),

    tagList(
      lc_feedback(type = "info",
        tags$strong("Najważniejsze do zapamiętania:"),
        tags$ol(
          tags$li("CI dla różnicy mówi czy różnica jest istotna — sprawdź
                   czy zawiera 0."),
          tags$li("Nie porównuj nakładania się CI poszczególnych grup —
                   to MOŻE dać mylny obraz. Zawsze patrz na CI
                   dla różnicy."),
          tags$li("„Istotne statystycznie” ≠ „ważne praktycznie”.
                   Przy bardzo dużym n nawet trywialne różnice
                   będą istotne."),
          tags$li("Forest plot to standardowy sposób porównania wielu
                   grup. Patrz nie tylko na średnie, ale przede
                   wszystkim na długość każdego CI.")
        )
      )
    ),

    lc_chapter_next(
      num       = "04",
      title     = "Przedział dla proporcji",
      lead      = "CI dla odsetków: Wald, Wilson, Clopper-Pearson",
      target_id = "ch-proporcja"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch3_server <- function(input, output, session) {

  # --- Widget 1: Budowa przedzialu krok po kroku ---
  ch3_step <- reactiveVal(0)
  ch3_step_sample <- reactiveVal(NULL)

  # Generuj probke na starcie (po pierwszym kliknieciu dowolnego kroku)
  generate_step_sample <- function() {
    set.seed(sample.int(.Machine$integer.max, 1))
    generate_population_sample("normal", 25)
  }

  observeEvent(input$ch3_step1, {
    if (is.null(ch3_step_sample())) {
      ch3_step_sample(generate_step_sample())
    }
    ch3_step(1)
  })
  observeEvent(input$ch3_step2, {
    if (is.null(ch3_step_sample())) {
      ch3_step_sample(generate_step_sample())
    }
    ch3_step(2)
  })
  observeEvent(input$ch3_step3, {
    if (is.null(ch3_step_sample())) {
      ch3_step_sample(generate_step_sample())
    }
    ch3_step(3)
  })
  observeEvent(input$ch3_step4, {
    if (is.null(ch3_step_sample())) {
      ch3_step_sample(generate_step_sample())
    }
    ch3_step(4)
  })
  observeEvent(input$ch3_step_new_sample, {
    ch3_step_sample(generate_step_sample())
    # zostawiamy biezacy step, zeby user zobaczyl od razu jak zmienia sie wykres
    if (ch3_step() == 0) ch3_step(1)
  })

  output$ch3_step_plot <- renderPlot({
    step <- ch3_step()
    samp <- ch3_step_sample()

    if (step == 0 || is.null(samp)) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "Kliknij '1. Próba' aby zacząć",
                   size = 6, color = upwr_reference) +
          theme_void()
      )
    }

    xbar <- mean(samp)
    s <- sd(samp)
    n <- length(samp)
    se <- s / sqrt(n)
    t_star <- qt(0.975, df = n - 1)
    me <- t_star * se

    # Stala os X dla wszystkich krokow (oparta na surowych danych + CI)
    xlims <- range(c(samp, xbar - 1.2 * me, xbar + 1.2 * me))
    pad <- diff(xlims) * 0.05
    xlims <- c(xlims[1] - pad, xlims[2] + pad)

    # Jitter punktow na Y (deterministyczny na podstawie wartosci)
    set.seed(42)
    jitter_y <- runif(n, min = 0.55, max = 0.90)
    samp_df <- data.frame(x = samp, y = jitter_y)

    # Oddzielne poziomy Y - kazdy element na swojej linii
    Y_MEAN <- 0.38
    Y_SE   <- 0.12
    Y_CI   <- -0.18

    # Wyszarzanie poprzednich elementow
    c_faded <- "#adb5bd"
    c_mean <- if (step >= 3) c_faded else col_estimate
    c_se   <- if (step >= 4) c_faded else col_hit

    p <- ggplot() +
      xlim(xlims) +
      ylim(-0.45, 0.98) +
      labs(x = "Wzrost (cm)", y = NULL) +
      theme_upwr() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    # Krok 1+: surowe punkty z proby
    if (step >= 1) {
      p <- p + geom_point(data = samp_df, aes(x = x, y = y),
                          color = col_ci, size = 3, alpha = 0.7)
    }

    # Krok 2+: pionowa linia prowadzaca + diament sredniej
    if (step >= 2) {
      p <- p +
        geom_vline(xintercept = xbar, color = "#adb5bd",
                   linewidth = 0.8, linetype = "dotted") +
        geom_point(aes(x = xbar, y = Y_MEAN), color = c_mean,
                   size = 7, shape = 18) +
        annotate("text", x = xbar, y = Y_MEAN - 0.13,
                 label = paste0("x̄ = ", round(xbar, 2)),
                 color = c_mean, fontface = "bold", size = 5)
    }

    # Krok 3+: przedzial +/- SE (zielony, wezszy)
    if (step >= 3) {
      p <- p +
        geom_errorbarh(aes(xmin = xbar - se, xmax = xbar + se, y = Y_SE),
                       height = 0.07, color = c_se, linewidth = 1.8) +
        annotate("text", x = xbar, y = Y_SE - 0.10,
                 label = paste0("± SE = ±", round(se, 2)),
                 color = c_se, fontface = "bold", size = 4.5)
    }

    # Krok 4: pelny CI (t* * SE, szerszy, niebieski)
    if (step >= 4) {
      p <- p +
        geom_errorbarh(aes(xmin = xbar - me, xmax = xbar + me, y = Y_CI),
                       height = 0.10, color = col_ci, linewidth = 2.2) +
        annotate("text", x = xbar, y = Y_CI - 0.13,
                 label = paste0("95% CI: [", round(xbar - me, 2),
                                " ; ", round(xbar + me, 2), "]"),
                 color = col_ci, fontface = "bold", size = 5)
    }

    p + ggtitle(paste0("Krok ", step, " z 4"))
  })

  output$ch3_step_explanation <- renderUI({
    step <- ch3_step()
    samp <- ch3_step_sample()
    if (step == 0 || is.null(samp)) return(NULL)

    xbar <- mean(samp)
    s <- sd(samp)
    n <- length(samp)
    se <- s / sqrt(n)
    t_star <- qt(0.975, df = n - 1)
    me <- t_star * se

    switch(as.character(step),
      "1" = lc_feedback(type = "info",
        p(tags$strong("Krok 1:"), " Próba.",
          " Pobraliśmy ", tags$b(n), " pomiarów wzrostu. Każda kropka to jedna osoba.
          Zauważ, jak bardzo surowe obserwacje są ", tags$b("rozrzucone"),
          " — rozrzut indywidualny w populacji jest duży."),
        p("Statystyki z próby: ",
          withMathJax(paste0("\\(\\bar{x} = ", round(xbar, 2), "\\)")),
          ", ",
          withMathJax(paste0("\\(s = ", round(s, 2), "\\)")),
          ", ",
          withMathJax(paste0("\\(n = ", n, "\\)")), ".")
      ),
      "2" = lc_feedback(type = "info",
        p(tags$strong("Krok 2:"), " Średnia z próby.",
          " Obliczamy ",
          withMathJax(paste0("\\(\\bar{x} = ", round(xbar, 2), "\\)")), " cm.
          To nasz ", tags$b("estymator punktowy"),
          " — najlepsze pojedyncze oszacowanie prawdziwej średniej populacji."),
        p("Ale pojedyncza liczba nie wystarczy. Inna próba dałaby inną średnią.
          Musimy wyrazić ", tags$b("niepewność"), " tego oszacowania.")
      ),
      "3" = lc_feedback(type = "info",
        p(tags$strong("Krok 3:"), " Błąd standardowy (± SE).",
          " Błąd standardowy średniej to:"),
        p(withMathJax(paste0("\\(SE = \\frac{s}{\\sqrt{n}} = \\frac{", round(s, 2),
                             "}{\\sqrt{", n, "}} = ", round(se, 2), "\\)"))),
        p("SE mówi, jak bardzo ", withMathJax("\\(\\bar{x}\\)"),
          " waha się z próby na próbę. Zauważ — jest ",
          tags$b("znacznie mniejszy"),
          " niż rozrzut surowych danych! To dlatego, że średnia z próby \"uśrednia\"
          losowe odchylenia poszczególnych obserwacji."),
        p("Ale ", tags$b("± 1 SE"), " to tylko około 68% ufności.
          Żeby dostać 95%, trzeba tę szerokość ", tags$em("powiększyć"),
          " przez wartość krytyczną.")
      ),
      "4" = {
        covers <- (xbar - me <= 170) & (170 <= xbar + me)
        lc_feedback(type = if (covers) "ok" else "danger",
          p(tags$strong("Krok 4:"), " Przedział ufności (± t* · SE)."),
          p("Mnożymy SE przez wartość krytyczną ",
            withMathJax(paste0("\\(t^*_{0.975, ", n - 1, "} = ",
                               round(t_star, 3), "\\)")), ":"),
          p(withMathJax(paste0("\\(ME = t^* \\cdot SE = ", round(t_star, 3),
                               " \\cdot ", round(se, 2), " = ", round(me, 2), "\\)"))),
          p(tags$b("95% CI: ["),
            round(xbar - me, 2), " ; ", round(xbar + me, 2), tags$b("]")),
          p("Zauważ, jak niebieski (pełny) przedział jest ",
            tags$b("szerszy"), " niż zielony (± SE) — dokładnie ",
            round(t_star, 2), "× szerszy. To dodatkowa niepewność z tego,
            że szacujemy σ z próby (a nie znamy go)."),
          p(tags$em(if (covers) "Ten przedział zawiera prawdziwą średnią populacji (μ = 170 cm)."
                    else "Ten przedział NIE zawiera prawdziwej średniej populacji (μ = 170 cm) — klikaj 'Nowa próba', żeby zobaczyć jak rzadko to się zdarza."))
        )
      }
    )
  })

  # --- Widget 2: Budowa przedzialu dla roznicy srednich ---
  ch3_dstep <- reactiveVal(0)
  ch3_dstep_samples <- reactiveVal(NULL)

  generate_diff_samples <- function() {
    list(
      men   = rnorm(25, mean = 178, sd = 7),
      women = rnorm(25, mean = 165, sd = 6)
    )
  }

  observeEvent(input$ch3_dstep1, {
    if (is.null(ch3_dstep_samples())) ch3_dstep_samples(generate_diff_samples())
    ch3_dstep(1)
  })
  observeEvent(input$ch3_dstep2, {
    if (is.null(ch3_dstep_samples())) ch3_dstep_samples(generate_diff_samples())
    ch3_dstep(2)
  })
  observeEvent(input$ch3_dstep3, {
    if (is.null(ch3_dstep_samples())) ch3_dstep_samples(generate_diff_samples())
    ch3_dstep(3)
  })
  observeEvent(input$ch3_dstep4, {
    if (is.null(ch3_dstep_samples())) ch3_dstep_samples(generate_diff_samples())
    ch3_dstep(4)
  })
  observeEvent(input$ch3_dstep5, {
    if (is.null(ch3_dstep_samples())) ch3_dstep_samples(generate_diff_samples())
    ch3_dstep(5)
  })
  observeEvent(input$ch3_dstep_new_sample, {
    ch3_dstep_samples(generate_diff_samples())
    if (ch3_dstep() == 0) ch3_dstep(1)
  })

  output$ch3_dstep_plot <- renderPlot({
    step <- ch3_dstep()
    samples <- ch3_dstep_samples()

    if (step == 0 || is.null(samples)) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "Kliknij '1. Dwie próby' aby zacząć",
                   size = 6, color = upwr_reference) +
          theme_void()
      )
    }

    men <- samples$men
    women <- samples$women
    n1 <- length(men); n2 <- length(women)
    x1 <- mean(men); x2 <- mean(women)
    s1 <- sd(men);   s2 <- sd(women)
    diff_val <- x1 - x2
    se <- sqrt(s1^2 / n1 + s2^2 / n2)
    df_w <- (s1^2 / n1 + s2^2 / n2)^2 /
            ((s1^2 / n1)^2 / (n1 - 1) + (s2^2 / n2)^2 / (n2 - 1))
    t_star <- qt(0.975, df = df_w)
    me <- t_star * se

    col_men <- col_ci      # niebieski
    col_women <- col_miss  # czerwony

    # ---- GORNY PANEL: dwie grupy na skali wzrostu ----
    xlims_top <- range(c(men, women))
    pad_top <- diff(xlims_top) * 0.06
    xlims_top <- c(xlims_top[1] - pad_top, xlims_top[2] + pad_top)

    set.seed(42)
    jitter_men <- runif(n1, min = 1.55, max = 2.05)
    jitter_women <- runif(n2, min = 0.75, max = 1.25)
    men_df <- data.frame(x = men, y = jitter_men)
    women_df <- data.frame(x = women, y = jitter_women)

    p_top <- ggplot() +
      xlim(xlims_top) +
      ylim(0.35, 2.25) +
      labs(x = "Wzrost (cm)", y = NULL) +
      theme_upwr() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    # Etykiety grup po lewej
    p_top <- p_top +
      annotate("text", x = xlims_top[1], y = 1.8, label = "Mężczyźni",
               hjust = 0, fontface = "bold", size = 4.5, color = col_men) +
      annotate("text", x = xlims_top[1], y = 1.0, label = "Kobiety",
               hjust = 0, fontface = "bold", size = 4.5, color = col_women)

    # Krok 1+: punkty
    if (step >= 1) {
      p_top <- p_top +
        geom_point(data = men_df, aes(x = x, y = y),
                   color = col_men, size = 3, alpha = 0.7) +
        geom_point(data = women_df, aes(x = x, y = y),
                   color = col_women, size = 3, alpha = 0.7)
    }

    # Krok 2+: srednie (diamenty + linie)
    if (step >= 2) {
      p_top <- p_top +
        geom_segment(aes(x = x1, xend = x1, y = 0.4, yend = 2.1),
                     color = col_men, linetype = "dotted", linewidth = 0.8) +
        geom_segment(aes(x = x2, xend = x2, y = 0.4, yend = 2.1),
                     color = col_women, linetype = "dotted", linewidth = 0.8) +
        geom_point(aes(x = x1, y = 1.8), color = col_men,
                   size = 7, shape = 18) +
        geom_point(aes(x = x2, y = 1.0), color = col_women,
                   size = 7, shape = 18) +
        annotate("text", x = x1, y = 2.15,
                 label = paste0("x̄₁ = ", round(x1, 2)),
                 color = col_men, fontface = "bold", size = 4.5) +
        annotate("text", x = x2, y = 0.55,
                 label = paste0("x̄₂ = ", round(x2, 2)),
                 color = col_women, fontface = "bold", size = 4.5)
    }

    # Dla krokow 1-2 zwracamy tylko gorny panel
    if (step < 3) {
      return(p_top + ggtitle(paste0("Krok ", step, " z 5")))
    }

    # ---- DOLNY PANEL: roznica w skali wycentrowanej na 0 ----
    # Limity: obejmij 0 i CI z marginesem
    xlims_bot <- range(c(0, diff_val - 1.3 * me, diff_val + 1.3 * me))
    pad_bot <- diff(xlims_bot) * 0.08
    xlims_bot <- c(xlims_bot[1] - pad_bot, xlims_bot[2] + pad_bot)

    p_bot <- ggplot() +
      xlim(xlims_bot) +
      ylim(-0.55, 0.55) +
      labs(x = "Różnica średnich (cm)  —  Mężczyźni − Kobiety",
           y = NULL) +
      theme_upwr() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      geom_vline(xintercept = 0, color = col_true,
                 linewidth = 1, linetype = "dashed") +
      annotate("text", x = 0, y = 0.45, label = "0 = brak różnicy",
               color = col_true, fontface = "bold", size = 4, hjust = -0.1)

    # Krok 3+: punkt roznicy
    p_bot <- p_bot +
      geom_point(aes(x = diff_val, y = 0), color = col_estimate,
                 size = 7, shape = 18) +
      annotate("text", x = diff_val, y = -0.22,
               label = paste0("x̄₁ − x̄₂ = ", round(diff_val, 2)),
               color = col_estimate, fontface = "bold", size = 4.5)

    # Krok 4+: waski przedzial SE
    if (step >= 4) {
      p_bot <- p_bot +
        geom_errorbarh(aes(xmin = diff_val - se, xmax = diff_val + se, y = 0),
                       height = 0.08, color = col_hit, linewidth = 1.8) +
        annotate("text", x = diff_val, y = 0.17,
                 label = paste0("± SE = ±", round(se, 2)),
                 color = col_hit, fontface = "bold", size = 4)
    }

    # Krok 5: pelen CI
    if (step >= 5) {
      p_bot <- p_bot +
        geom_errorbarh(aes(xmin = diff_val - me, xmax = diff_val + me, y = 0),
                       height = 0.14, color = col_ci, linewidth = 2.2, alpha = 0.6) +
        annotate("text", x = diff_val, y = -0.42,
                 label = paste0("95% CI: [", round(diff_val - me, 2),
                                " ; ", round(diff_val + me, 2), "]"),
                 color = col_ci, fontface = "bold", size = 4.8)
    }

    # Polacz patchworkiem
    library(patchwork)
    (p_top / p_bot) +
      plot_layout(heights = c(2, 1)) +
      plot_annotation(title = paste0("Krok ", step, " z 5"))
  })

  output$ch3_dstep_explanation <- renderUI({
    step <- ch3_dstep()
    samples <- ch3_dstep_samples()
    if (step == 0 || is.null(samples)) return(NULL)

    men <- samples$men
    women <- samples$women
    n1 <- length(men); n2 <- length(women)
    x1 <- mean(men); x2 <- mean(women)
    s1 <- sd(men);   s2 <- sd(women)
    diff_val <- x1 - x2
    se <- sqrt(s1^2 / n1 + s2^2 / n2)
    df_w <- (s1^2 / n1 + s2^2 / n2)^2 /
            ((s1^2 / n1)^2 / (n1 - 1) + (s2^2 / n2)^2 / (n2 - 1))
    t_star <- qt(0.975, df = df_w)
    me <- t_star * se

    switch(as.character(step),
      "1" = lc_feedback(type = "info",
        p(tags$strong("Krok 1:"), " Dwie próby.",
          " Mierzymy wzrost w obu grupach: ", tags$b(n1), " mężczyzn i ",
          tags$b(n2), " kobiet. Każdy punkt to jedna osoba.
          Zauważ — rozrzut surowych danych jest duży, ale wyraźnie widać,
          że średnia \"niebieska\" leży na prawo od średniej \"czerwonej\".")
      ),
      "2" = lc_feedback(type = "info",
        p(tags$strong("Krok 2:"), " Dwie średnie.",
          " Obliczamy średnią w każdej grupie:"),
        p(withMathJax(paste0("\\(\\bar{x}_1 = ", round(x1, 2), "\\)"))),
        p(withMathJax(paste0("\\(\\bar{x}_2 = ", round(x2, 2), "\\)"))),
        p("Każda średnia ma własną niepewność — ale interesuje nas
          nie każda z osobna, tylko ", tags$b("różnica między nimi"), ".")
      ),
      "3" = lc_feedback(type = "info",
        p(tags$strong("Krok 3:"), " Różnica.",
          " Estymator punktowy różnicy: ",
          withMathJax(paste0("\\(\\bar{x}_1 - \\bar{x}_2 = ", round(x1, 2),
                             " - ", round(x2, 2), " = ",
                             round(diff_val, 2), "\\)")), " cm."),
        p("W dolnym panelu przenosimy się do nowej skali — ", tags$b("skali różnicy"),
          ". Punkt = nasze oszacowanie różnicy. Pionowa linia na 0 oznacza ",
          tags$em("\"gdyby różnicy nie było\""),
          ". Teraz musimy otoczyć naszą różnicę przedziałem niepewności.")
      ),
      "4" = lc_feedback(type = "info",
        p(tags$strong("Krok 4:"), " Błąd standardowy różnicy (± SE).",
          " SE różnicy łączy niepewności z obu prób:"),
        p(withMathJax(paste0(
          "\\(SE_{różnicy} = \\sqrt{\\frac{s_1^2}{n_1} + \\frac{s_2^2}{n_2}} = ",
          "\\sqrt{\\frac{", round(s1, 2), "^2}{", n1, "} + \\frac{",
          round(s2, 2), "^2}{", n2, "}} = ", round(se, 2), "\\)"))),
        p(tags$b("Ważne:"), " wariancje się ", tags$em("dodają"),
          ", nie SE. Dlatego SE różnicy jest ", tags$em("mniejszy"),
          " niż suma SE poszczególnych średnich — to właśnie źródło ",
          tags$b("pułapki nakładających się CI"),
          " (patrz case B3 poniżej).")
      ),
      "5" = {
        covers_zero <- (diff_val - me <= 0) & (0 <= diff_val + me)
        lc_feedback(type = if (covers_zero) "warning" else "ok",
          p(tags$strong("Krok 5:"), " Przedział ufności dla różnicy."),
          p("Wartość krytyczna z rozkładu t (df Welcha ≈ ",
            round(df_w, 1), "): ",
            withMathJax(paste0("\\(t^* = ", round(t_star, 3), "\\)"))),
          p(withMathJax(paste0("\\(ME = t^* \\cdot SE = ", round(t_star, 3),
                               " \\cdot ", round(se, 2), " = ",
                               round(me, 2), "\\)"))),
          p(tags$b("95% CI: ["),
            round(diff_val - me, 2), " ; ", round(diff_val + me, 2),
            tags$b("] cm")),
          p(tags$em(if (covers_zero)
              "CI obejmuje 0 — nie możemy stwierdzić, że różnica jest istotna."
            else
              paste0("CI nie obejmuje 0 — różnica jest istotna. ",
                     "Możemy stwierdzić z 95% ufnością, że mężczyźni są średnio ",
                     "o co najmniej ", round(diff_val - me, 1),
                     " cm wyżsi od kobiet.")))
        )
      }
    )
  })

  # ============================================================================
  # WIDGET 3: CASE STUDIES (konstruktory krok po kroku + hipotezy)
  # ============================================================================

  # ---- Helpery statystyczne ----
  ci_mean <- function(xbar, s, n, conf = 0.95) {
    t_star <- qt(1 - (1 - conf) / 2, df = n - 1)
    me <- t_star * s / sqrt(n)
    list(lower = xbar - me, upper = xbar + me, me = me,
         t_star = t_star, se = s / sqrt(n))
  }
  ci_diff_means <- function(x1, s1, n1, x2, s2, n2, conf = 0.95) {
    se <- sqrt(s1^2 / n1 + s2^2 / n2)
    df_w <- (s1^2 / n1 + s2^2 / n2)^2 /
            ((s1^2 / n1)^2 / (n1 - 1) + (s2^2 / n2)^2 / (n2 - 1))
    t_star <- qt(1 - (1 - conf) / 2, df = df_w)
    diff <- x1 - x2
    me <- t_star * se
    list(diff = diff, lower = diff - me, upper = diff + me,
         me = me, df = df_w, se = se, t_star = t_star)
  }

  # ---- Werdykt hipotezy ----
  # dir = "gt" (CI > bound), "lt" (CI < bound)
  # Zwraca: "yes" / "no" / "maybe"
  hypothesis_verdict <- function(lower, upper, bound, dir) {
    if (dir == "gt") {
      if (lower > bound)      "yes"
      else if (upper < bound) "no"
      else                    "maybe"
    } else {  # lt
      if (upper < bound)      "yes"
      else if (lower > bound) "no"
      else                    "maybe"
    }
  }

  verdict_class <- function(v) {
    switch(v, "yes" = "ok", "no" = "danger",
           "maybe" = "warning")
  }
  verdict_label <- function(v) {
    switch(v, "yes" = "TAK", "no" = "NIE", "maybe" = "NIEPEWNE")
  }

  # Kolor obszaru hipotezy (fioletowawy)
  col_hyp <- "#8e44ad"

  # ---- CONFIG case'ow ----
  # Kazdy case: type, data, xlab, steps (labele przyciskow), hypotheses (lista 2)
  # hypotheses: list of list(text, bound, dir, interval_fn)
  # Dla "single_mean" / "diff_means" / "compare_n" / "forest" interval_fn
  # wyciaga (lower, upper) z konfiguracji.

  cases_config <- list(
    A1 = list(
      type = "single_mean",
      data = list(xbar = 173.4, s = 8.2, n = 30),
      xlab = "Wzrost (cm)",
      steps = c("1. Próba", "2. Średnia", "3. ± SE", "4. Przedział"),
      hypotheses = list(
        list(text = "Średni wzrost przekracza 168 cm",
             bound = 168, dir = "gt",
             explain_yes = "Dolna granica CI (≈ 170.3) leży powyżej 168. Cały CI jest w obszarze hipotezy — z 95% ufnością średni wzrost w populacji przekracza 168 cm."),
        list(text = "Średni wzrost przekracza 180 cm",
             bound = 180, dir = "gt",
             explain_no = "Górna granica CI (≈ 176.5) leży poniżej 180. Cały CI jest poza obszarem hipotezy — nie możemy stwierdzić, że średnia wzrostu przekracza 180 cm.")
      )
    ),
    A2 = list(
      type = "compare_n",
      data = list(xbar = 32.0, s = 8.0, ns = c(10, 50, 200)),
      xlab = "Stężenie (µg/m³)",
      steps = c("1. n = 10", "2. n = 50", "3. n = 200"),
      hypotheses = list(
        list(text = "Stężenie przekracza 25 µg/m³",
             bound = 25, dir = "gt",
             explain_yes = "Wszystkie trzy CI leżą powyżej 25 — nawet najszerszy (n=10) ma dolną granicę ≈ 26.3. Każde z badan potwierdza hipotezę. Większe n daje tylko bardziej precyzyjne oszacowanie, ale wniosek jest ten sam."),
        list(text = "Stężenie przekracza 35 µg/m³",
             bound = 35, dir = "gt",
             explain_no = "Górna granica CI nawet dla n=200 (≈ 33.1) leży poniżej 35 — nawet najdokładniejsze badanie nie pozwala stwierdzić, że stężenie przekracza 35. Zauważ: dla n=10 CI sięga aż do 37.7 i przecina 35, więc tam sytuacja byłaby niepewna — to pokazuje, dlaczego duży n jest cenny: daje bardziej definitywną odpowiedź.")
      )
    ),
    B1 = list(
      type = "diff_means",
      data = list(x1 = 12.3, s1 = 4.5, n1 = 40, x2 = 4.1, s2 = 4.2, n2 = 40,
                  label1 = "Lek", label2 = "Placebo",
                  unit = "mmHg", diff_label = "Lek − placebo"),
      xlab = "Obniżenie ciśnienia (mmHg)",
      steps = c("1. Próby", "2. Średnie", "3. Różnica", "4. ± SE", "5. Przedział"),
      hypotheses = list(
        list(text = "Lek skuteczniej obniża ciśnienie niż placebo (różnica > 0)",
             bound = 0, dir = "gt",
             explain_yes = "Cały CI dla różnicy leży powyżej 0 — lek rzeczywiście obniża ciśnienie skuteczniej niż placebo. To ten sam wniosek co \"różnica istotna statystycznie\"."),
        list(text = "Lek działa o więcej niż 12 mmHg lepiej niż placebo",
             bound = 12, dir = "gt",
             explain_no = "Górna granica CI (≈ 10.1) leży poniżej 12. Cały CI jest w przedziale 6–10 mmHg — efekt leku jest wyraźny, ale nie tak duży jak głosi hipoteza.")
      )
    ),
    B2 = list(
      type = "diff_means",
      data = list(x1 = 8.4, s1 = 1.2, n1 = 25, x2 = 8.1, s2 = 1.3, n2 = 25,
                  label1 = "Nawoz X", label2 = "Nawoz Y",
                  unit = "t/ha", diff_label = "X − Y"),
      xlab = "Plon (t/ha)",
      steps = c("1. Próby", "2. Średnie", "3. Różnica", "4. ± SE", "5. Przedział"),
      hypotheses = list(
        list(text = "Różnica plonów jest mniejsza niż 2 t/ha",
             bound = 2, dir = "lt",
             explain_yes = "Cały CI dla różnicy leży poniżej 2 t/ha. Możemy być pewni, że nawet jeśli któryś nawoz jest lepszy, to różnica nie jest duża (mniej niż 2 t/ha)."),
        list(text = "Nawoz X daje więcej niż 2 t/ha większy plon niż Y",
             bound = 2, dir = "gt",
             explain_no = "Górna granica CI (≈ 1.0) leży poniżej 2 — nawet najbardziej optymistyczny scenariusz nie przewiduje tak dużej przewagi X nad Y. Uwaga: to nie znaczy, że X jest lepszy od Y — CI obejmuje też wartości ujemne, więc nie wiemy nawet, który nawoz jest lepszy.")
      )
    ),
    B3 = list(
      type = "diff_means",
      data = list(x1 = 350, s1 = 45, n1 = 150, x2 = 362, s2 = 45, n2 = 150,
                  label1 = "Grupa A", label2 = "Grupa B",
                  unit = "ms", diff_label = "A − B"),
      xlab = "Czas reakcji (ms)",
      steps = c("1. Próby", "2. Średnie", "3. Różnica", "4. ± SE", "5. Przedział"),
      hypotheses = list(
        list(text = "Grupa A reaguje szybciej niż B (różnica < 0)",
             bound = 0, dir = "lt",
             explain_yes = "Mimo że CI każdej grupy osobno się nakładają (zobacz górny panel!), CI dla różnicy cały leży poniżej 0. To jest właśnie pułapka nakładających się CI: SE różnicy jest mniejszy niż suma SE pojedynczych średnich, dlatego CI dla różnicy bywa węższy niż by sugerowały nakładające się CI grup."),
        list(text = "Grupa A jest szybsza o co najmniej 25 ms",
             bound = -25, dir = "lt",
             explain_no = "Dolna granica CI (≈ -22) nie dosięga -25 — cały CI jest powyżej tej wartości. Nie możemy stwierdzić, że różnica wynosi co najmniej 25 ms. Wiemy tylko, że różnica jest istotna (A szybsza) i mieści się między 2 a 22 ms.")
      )
    ),
    B4 = list(
      type = "diff_means",
      data = list(x1 = 100.4, s1 = 15, n1 = 20000, x2 = 100.0, s2 = 15, n2 = 20000,
                  label1 = "Wojew. A", label2 = "Wojew. B",
                  unit = "pkt IQ", diff_label = "A − B"),
      xlab = "IQ (punkty)",
      steps = c("1. Próby", "2. Średnie", "3. Różnica", "4. ± SE", "5. Przedział"),
      hypotheses = list(
        list(text = "Województwo A ma wyższe średnie IQ niż B (różnica > 0)",
             bound = 0, dir = "gt",
             explain_yes = "Dzięki ogromnej próbie (n=20000 w każdej grupie) CI jest bardzo wąski i nie obejmuje 0. Formalnie: różnica jest istotna statystycznie."),
        list(text = "Różnica wynosi co najmniej 1 punkt IQ",
             bound = 1, dir = "gt",
             explain_no = "Cały CI leży poniżej 1 (górna granica ≈ 0.7). Różnica jest statystycznie istotna, ale rozmiarowo trywialna — 0.4 punktu IQ to ~0.03 SD, nic zauważalnego w życiu. To klasyczna ilustracja, że istotność statystyczna ≠ ważność praktyczna.")
      )
    ),
    C1 = list(
      type = "forest",
      data = list(
        groups = c("Tradycyjna", "E-learning", "Flipped class", "Tutoring"),
        means  = c(28.5, 30.2, 31.8, 33.4),
        sds    = c(5.2, 5.8, 5.5, 4.9),
        ns     = c(25, 25, 25, 25)
      ),
      xlab = "Średni wynik egzaminu (0–40 pkt)",
      steps = c("1. Punkty", "2. Średnie", "3. CI"),
      hypotheses = list(
        list(kind = "pairwise",
             text = "Które metody nauczania różnią się istotnie?",
             unit = "pkt")
      )
    ),
    C2 = list(
      type = "forest",
      data = list(
        groups = c("Kardiologia", "Neurologia", "Ortopedia", "Pulmonologia", "SOR"),
        means  = c(22, 28, 25, 31, 75),
        sds    = c(8, 10, 9, 11, 25),
        ns     = c(60, 55, 70, 50, 80)
      ),
      xlab = "Średni czas oczekiwania (min)",
      steps = c("1. Punkty", "2. Średnie", "3. CI"),
      hypotheses = list(
        list(kind = "pairwise",
             text = "Które oddziały różnią się istotnie czasem oczekiwania?",
             unit = "min")
      )
    )
  )

  # ---- Reactive state per case ----
  # Dla kazdego case'a: aktualny step (0 = nic, 1..n = budowa CI,
  #                     n+1 = hipoteza 1, n+2 = hipoteza 2)
  ch3_case_state <- reactiveValues()
  for (cid in names(cases_config)) {
    ch3_case_state[[cid]] <- 0
  }

  # ---- Helper: narysuj pasek CI dla pojedynczej sredniej ----
  # step: 0 = nic, 1 = punkty, 2 = +srednia, 3 = +SE, 4 = +CI
  # hypothesis: NULL lub list(bound, dir)
  plot_single_mean_step <- function(data, step, xlab,
                                     hypothesis = NULL, title = NULL) {
    xbar <- data$xbar; s <- data$s; n <- data$n
    ci <- ci_mean(xbar, s, n)
    se <- ci$se; me <- ci$me
    t_star <- ci$t_star

    # Generujemy "fake" punkty z parametrow (reproducowalnie)
    set.seed(42)
    samp <- rnorm(n, mean = xbar, sd = s)
    samp <- (samp - mean(samp)) / sd(samp) * s + xbar  # wymus dokladnie xbar, s

    # Limity
    xlims <- range(c(samp, xbar - 1.2 * me, xbar + 1.2 * me))
    if (!is.null(hypothesis)) {
      xlims <- range(c(xlims, hypothesis$bound))
    }
    pad <- diff(xlims) * 0.05
    xlims <- c(xlims[1] - pad, xlims[2] + pad)

    set.seed(7)
    jitter_y <- runif(n, min = 0.15, max = 0.55)
    samp_df <- data.frame(x = samp, y = jitter_y)

    p <- ggplot() +
      xlim(xlims) +
      ylim(-0.55, 0.75) +
      labs(x = xlab, y = NULL, title = title) +
      theme_upwr() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    # Obszar hipotezy (pod spodem wszystkiego)
    if (!is.null(hypothesis)) {
      if (hypothesis$dir == "gt") {
        p <- p + annotate("rect",
                          xmin = hypothesis$bound, xmax = Inf,
                          ymin = -Inf, ymax = Inf,
                          fill = col_hyp, alpha = 0.15)
      } else {
        p <- p + annotate("rect",
                          xmin = -Inf, xmax = hypothesis$bound,
                          ymin = -Inf, ymax = Inf,
                          fill = col_hyp, alpha = 0.15)
      }
      p <- p +
        geom_vline(xintercept = hypothesis$bound, color = col_hyp,
                   linewidth = 1, linetype = "solid") +
        annotate("text", x = hypothesis$bound, y = 0.68,
                 label = paste0(if (hypothesis$dir == "gt") "≥ " else "≤ ",
                                hypothesis$bound),
                 color = col_hyp, fontface = "bold", size = 4.5, hjust = -0.1)
    }

    if (step >= 1) {
      p <- p + geom_point(data = samp_df, aes(x = x, y = y),
                          color = col_ci, size = 3, alpha = 0.7)
    }
    if (step >= 2) {
      p <- p +
        geom_vline(xintercept = xbar, color = col_estimate,
                   linewidth = 1, linetype = "dotted") +
        geom_point(aes(x = xbar, y = 0), color = col_estimate,
                   size = 7, shape = 18) +
        annotate("text", x = xbar, y = -0.18,
                 label = paste0("x̄ = ", round(xbar, 2)),
                 color = col_estimate, fontface = "bold", size = 5)
    }
    if (step >= 3) {
      p <- p +
        geom_errorbarh(aes(xmin = xbar - se, xmax = xbar + se, y = 0),
                       height = 0.06, color = col_hit, linewidth = 1.8) +
        annotate("text", x = xbar, y = 0.14,
                 label = paste0("± SE = ±", round(se, 2)),
                 color = col_hit, fontface = "bold", size = 4)
    }
    if (step >= 4) {
      p <- p +
        geom_errorbarh(aes(xmin = xbar - me, xmax = xbar + me, y = 0),
                       height = 0.12, color = col_ci, linewidth = 2.2,
                       alpha = 0.6) +
        annotate("text", x = xbar, y = -0.38,
                 label = paste0("95% CI: [", round(xbar - me, 2),
                                " ; ", round(xbar + me, 2), "]"),
                 color = col_ci, fontface = "bold", size = 4.8)
    }

    p
  }

  # ---- Plot dla compare_n ----
  plot_compare_n_step <- function(data, step, xlab, hypothesis = NULL) {
    xbar <- data$xbar; s <- data$s; ns <- data$ns

    # Kazdy step = jeden dodatkowy CI
    ci_list <- lapply(ns, function(n) {
      ci <- ci_mean(xbar, s, n)
      list(n = n, lower = ci$lower, upper = ci$upper, me = ci$me)
    })

    visible_k <- step  # ile CI pokazujemy
    if (visible_k < 1) visible_k <- 0
    if (visible_k > length(ns)) visible_k <- length(ns)

    all_lowers <- sapply(ci_list, function(c) c$lower)
    all_uppers <- sapply(ci_list, function(c) c$upper)
    xlims <- c(min(all_lowers), max(all_uppers))
    if (!is.null(hypothesis)) {
      xlims <- range(c(xlims, hypothesis$bound))
    }
    pad <- diff(xlims) * 0.1
    xlims <- c(xlims[1] - pad, xlims[2] + pad)

    y_positions <- seq_along(ns)

    p <- ggplot() +
      xlim(xlims) +
      ylim(0.3, length(ns) + 0.7) +
      labs(x = xlab, y = NULL) +
      theme_upwr() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    if (!is.null(hypothesis)) {
      if (hypothesis$dir == "gt") {
        p <- p + annotate("rect",
                          xmin = hypothesis$bound, xmax = Inf,
                          ymin = -Inf, ymax = Inf,
                          fill = col_hyp, alpha = 0.15)
      } else {
        p <- p + annotate("rect",
                          xmin = -Inf, xmax = hypothesis$bound,
                          ymin = -Inf, ymax = Inf,
                          fill = col_hyp, alpha = 0.15)
      }
      p <- p +
        geom_vline(xintercept = hypothesis$bound, color = col_hyp,
                   linewidth = 1, linetype = "solid") +
        annotate("text", x = hypothesis$bound, y = length(ns) + 0.5,
                 label = paste0(if (hypothesis$dir == "gt") "≥ " else "≤ ",
                                hypothesis$bound),
                 color = col_hyp, fontface = "bold", size = 4.5, hjust = -0.1)
    }

    if (visible_k >= 1) {
      rows_df <- data.frame(
        y = sapply(seq_len(visible_k), function(i) y_positions[i]),
        lower = sapply(seq_len(visible_k), function(i) ci_list[[i]]$lower),
        upper = sapply(seq_len(visible_k), function(i) ci_list[[i]]$upper),
        n = sapply(seq_len(visible_k), function(i) ci_list[[i]]$n),
        xbar_val = xbar
      )
      p <- p +
        geom_errorbarh(data = rows_df,
                       aes(xmin = lower, xmax = upper, y = y),
                       height = 0.12, color = col_ci, linewidth = 1.8) +
        geom_point(data = rows_df,
                   aes(x = xbar_val, y = y),
                   color = col_estimate, size = 5, shape = 18)
      # Labelki n i granic CI dodajemy przez annotate (jeden po drugim)
      for (i in seq_len(visible_k)) {
        ci <- ci_list[[i]]
        y <- y_positions[i]
        p <- p +
          annotate("text", x = xlims[1], y = y,
                   label = paste0("n = ", ci$n),
                   hjust = 0, fontface = "bold", size = 4.5,
                   color = upwr_secondary) +
          annotate("text", x = ci$upper, y = y + 0.22,
                   label = paste0("[", round(ci$lower, 2), " ; ",
                                  round(ci$upper, 2), "]"),
                   hjust = 1, size = 3.8, color = col_ci,
                   fontface = "bold")
      }
    }

    p
  }

  # ---- Plot dla diff_means ----
  # step 1=proby, 2=srednie, 3=roznica, 4=+SE, 5=+CI
  plot_diff_means_step <- function(data, step, xlab, hypothesis = NULL) {
    x1 <- data$x1; s1 <- data$s1; n1 <- data$n1
    x2 <- data$x2; s2 <- data$s2; n2 <- data$n2

    cid <- ci_diff_means(x1, s1, n1, x2, s2, n2)
    diff_val <- cid$diff
    se <- cid$se; me <- cid$me

    col_g1 <- col_ci
    col_g2 <- col_miss

    # Generuj reprezentatywne probki z parametrow
    set.seed(11)
    samp1 <- rnorm(n1, mean = x1, sd = s1)
    samp1 <- (samp1 - mean(samp1)) / sd(samp1) * s1 + x1
    set.seed(17)
    samp2 <- rnorm(n2, mean = x2, sd = s2)
    samp2 <- (samp2 - mean(samp2)) / sd(samp2) * s2 + x2

    # Gdy n > 80, pokazujemy losowa podproke (dla czytelnosci)
    max_show <- 80
    if (n1 > max_show) samp1 <- sample(samp1, max_show)
    if (n2 > max_show) samp2 <- sample(samp2, max_show)

    # ---- GORNY PANEL ----
    xlims_top <- range(c(samp1, samp2))
    pad_t <- diff(xlims_top) * 0.06
    xlims_top <- c(xlims_top[1] - pad_t, xlims_top[2] + pad_t)

    set.seed(42)
    jit1 <- runif(length(samp1), 1.55, 2.05)
    set.seed(43)
    jit2 <- runif(length(samp2), 0.75, 1.25)

    p_top <- ggplot() +
      xlim(xlims_top) +
      ylim(0.35, 2.25) +
      labs(x = xlab, y = NULL) +
      theme_upwr() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      annotate("text", x = xlims_top[1], y = 1.8, label = data$label1,
               hjust = 0, fontface = "bold", size = 4.5, color = col_g1) +
      annotate("text", x = xlims_top[1], y = 1.0, label = data$label2,
               hjust = 0, fontface = "bold", size = 4.5, color = col_g2)

    if (step >= 1) {
      p_top <- p_top +
        geom_point(data = data.frame(x = samp1, y = jit1),
                   aes(x = x, y = y), color = col_g1, size = 3, alpha = 0.7) +
        geom_point(data = data.frame(x = samp2, y = jit2),
                   aes(x = x, y = y), color = col_g2, size = 3, alpha = 0.7)
    }
    if (step >= 2) {
      p_top <- p_top +
        geom_segment(aes(x = x1, xend = x1, y = 0.4, yend = 2.1),
                     color = col_g1, linetype = "dotted", linewidth = 0.8) +
        geom_segment(aes(x = x2, xend = x2, y = 0.4, yend = 2.1),
                     color = col_g2, linetype = "dotted", linewidth = 0.8) +
        geom_point(aes(x = x1, y = 1.8), color = col_g1,
                   size = 7, shape = 18) +
        geom_point(aes(x = x2, y = 1.0), color = col_g2,
                   size = 7, shape = 18) +
        annotate("text", x = x1, y = 2.15,
                 label = paste0("x̄₁ = ", round(x1, 2)),
                 color = col_g1, fontface = "bold", size = 4.5) +
        annotate("text", x = x2, y = 0.55,
                 label = paste0("x̄₂ = ", round(x2, 2)),
                 color = col_g2, fontface = "bold", size = 4.5)
    }

    if (step < 3) {
      return(p_top)
    }

    # ---- DOLNY PANEL ----
    xlims_bot <- range(c(0, diff_val - 1.3 * me, diff_val + 1.3 * me))
    if (!is.null(hypothesis)) {
      xlims_bot <- range(c(xlims_bot, hypothesis$bound))
    }
    pad_b <- diff(xlims_bot) * 0.1
    xlims_bot <- c(xlims_bot[1] - pad_b, xlims_bot[2] + pad_b)

    p_bot <- ggplot() +
      xlim(xlims_bot) +
      ylim(-0.55, 0.65) +
      labs(x = paste0("Różnica (", data$unit, ")  —  ", data$diff_label),
           y = NULL) +
      theme_upwr() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    # Obszar hipotezy
    if (!is.null(hypothesis)) {
      if (hypothesis$dir == "gt") {
        p_bot <- p_bot + annotate("rect",
                                   xmin = hypothesis$bound, xmax = Inf,
                                   ymin = -Inf, ymax = Inf,
                                   fill = col_hyp, alpha = 0.15)
      } else {
        p_bot <- p_bot + annotate("rect",
                                   xmin = -Inf, xmax = hypothesis$bound,
                                   ymin = -Inf, ymax = Inf,
                                   fill = col_hyp, alpha = 0.15)
      }
      p_bot <- p_bot +
        geom_vline(xintercept = hypothesis$bound, color = col_hyp,
                   linewidth = 1, linetype = "solid") +
        annotate("text", x = hypothesis$bound, y = 0.55,
                 label = paste0(if (hypothesis$dir == "gt") "≥ " else "≤ ",
                                hypothesis$bound),
                 color = col_hyp, fontface = "bold", size = 4.5, hjust = -0.1)
    } else {
      # linia zero gdy brak hipotezy
      p_bot <- p_bot +
        geom_vline(xintercept = 0, color = col_true,
                   linewidth = 1, linetype = "dashed") +
        annotate("text", x = 0, y = 0.55, label = "0 = brak różnicy",
                 color = col_true, fontface = "bold", size = 4, hjust = -0.1)
    }

    p_bot <- p_bot +
      geom_point(aes(x = diff_val, y = 0), color = col_estimate,
                 size = 7, shape = 18) +
      annotate("text", x = diff_val, y = -0.22,
               label = paste0("x̄₁ − x̄₂ = ", round(diff_val, 2)),
               color = col_estimate, fontface = "bold", size = 4.5)

    if (step >= 4) {
      p_bot <- p_bot +
        geom_errorbarh(aes(xmin = diff_val - se, xmax = diff_val + se, y = 0),
                       height = 0.08, color = col_hit, linewidth = 1.8) +
        annotate("text", x = diff_val, y = 0.17,
                 label = paste0("± SE = ±", round(se, 2)),
                 color = col_hit, fontface = "bold", size = 4)
    }
    if (step >= 5) {
      p_bot <- p_bot +
        geom_errorbarh(aes(xmin = diff_val - me, xmax = diff_val + me, y = 0),
                       height = 0.14, color = col_ci, linewidth = 2.2,
                       alpha = 0.6) +
        annotate("text", x = diff_val, y = -0.42,
                 label = paste0("95% CI: [", round(diff_val - me, 2),
                                " ; ", round(diff_val + me, 2), "]"),
                 color = col_ci, fontface = "bold", size = 4.8)
    }

    library(patchwork)
    (p_top / p_bot) + plot_layout(heights = c(2, 1))
  }

  # ---- Plot dla forest (wiele grup) ----
  plot_forest_step <- function(data, step, xlab, hypothesis = NULL) {
    groups <- data$groups
    means <- data$means
    sds <- data$sds
    ns <- data$ns
    k <- length(groups)

    ci_list <- lapply(seq_len(k), function(i) {
      ci <- ci_mean(means[i], sds[i], ns[i])
      list(lower = ci$lower, upper = ci$upper)
    })
    all_lowers <- sapply(ci_list, function(c) c$lower)
    all_uppers <- sapply(ci_list, function(c) c$upper)

    # Limity
    xlims <- range(c(all_lowers, all_uppers))
    if (!is.null(hypothesis)) {
      xlims <- range(c(xlims, hypothesis$bound))
    }
    pad <- diff(xlims) * 0.12
    xlims <- c(xlims[1] - pad, xlims[2] + pad)

    # Wygeneruj fake punkty dla kazdej grupy
    points_df <- do.call(rbind, lapply(seq_len(k), function(i) {
      set.seed(50 + i)
      samp <- rnorm(ns[i], mean = means[i], sd = sds[i])
      samp <- (samp - mean(samp)) / sd(samp) * sds[i] + means[i]
      if (ns[i] > 60) samp <- sample(samp, 60)
      set.seed(100 + i)
      jit <- runif(length(samp), min = i - 0.25, max = i + 0.25)
      data.frame(x = samp, y = jit, group = groups[i])
    }))

    y_positions <- seq_len(k)
    group_df <- data.frame(group = groups, y = y_positions,
                            mean = means, lower = all_lowers, upper = all_uppers)

    p <- ggplot() +
      xlim(xlims) +
      ylim(0.3, k + 0.7) +
      labs(x = xlab, y = NULL) +
      theme_upwr() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    # Etykiety grup
    p <- p +
      annotate("text", x = xlims[1], y = y_positions,
               label = groups, hjust = 0, fontface = "bold", size = 4.5,
               color = upwr_secondary)

    # Obszar hipotezy
    if (!is.null(hypothesis)) {
      if (hypothesis$dir == "gt") {
        p <- p + annotate("rect",
                          xmin = hypothesis$bound, xmax = Inf,
                          ymin = -Inf, ymax = Inf,
                          fill = col_hyp, alpha = 0.15)
      } else {
        p <- p + annotate("rect",
                          xmin = -Inf, xmax = hypothesis$bound,
                          ymin = -Inf, ymax = Inf,
                          fill = col_hyp, alpha = 0.15)
      }
      p <- p +
        geom_vline(xintercept = hypothesis$bound, color = col_hyp,
                   linewidth = 1, linetype = "solid") +
        annotate("text", x = hypothesis$bound, y = k + 0.45,
                 label = paste0(if (hypothesis$dir == "gt") "≥ " else "≤ ",
                                hypothesis$bound),
                 color = col_hyp, fontface = "bold", size = 4.5, hjust = -0.1)
    }

    # Krok 1+: punkty
    if (step >= 1) {
      p <- p + geom_point(data = points_df, aes(x = x, y = y),
                          color = col_ci, size = 2.3, alpha = 0.55)
    }
    # Krok 2+: srednie
    if (step >= 2) {
      p <- p + geom_point(data = group_df, aes(x = mean, y = y),
                          color = col_estimate, size = 6, shape = 18)
    }
    # Krok 3+: CI
    if (step >= 3) {
      p <- p + geom_errorbarh(data = group_df,
                               aes(xmin = lower, xmax = upper, y = y),
                               height = 0.18, color = col_ci, linewidth = 1.8)
    }

    p
  }

  # ---- Liczba "core" krokow budowy CI (bez hipotez) ----
  n_core_steps <- function(cfg) length(cfg$steps)

  # ---- Dekoder fazy hipotezy ze stanu ----
  # State po n_core: kazda hipoteza ma 2 stany (treść, werdykt).
  #   n_core + 1 = hipoteza 1 (tylko treść)
  #   n_core + 2 = hipoteza 1 (z werdyktem)
  #   n_core + 3 = hipoteza 2 (tylko treść)
  #   n_core + 4 = hipoteza 2 (z werdyktem)
  # Zwraca NULL jezeli step jest w fazie budowy CI lub poza zakresem.
  hyp_phase <- function(step, n_core, n_hyp) {
    if (step <= n_core) return(NULL)
    offset <- step - n_core
    j <- (offset - 1) %/% 2 + 1
    reveal <- (offset - 1) %% 2 == 1
    if (j > n_hyp) return(NULL)
    list(idx = j, reveal = reveal)
  }
  # Konwersja: idx hipotezy + reveal -> wartosc state
  hyp_state <- function(n_core, j, reveal) {
    n_core + (j - 1) * 2 + (if (reveal) 2 else 1)
  }

  # ---- Generator przyciskow dla case'a ----
  case_buttons_ui <- function(case_id) {
    cfg <- cases_config[[case_id]]
    current <- ch3_case_state[[case_id]]
    n_core <- n_core_steps(cfg)
    n_hyp <- length(cfg$hypotheses)
    phase <- hyp_phase(current, n_core, n_hyp)

    # Przyciski budowy CI
    core_btns <- lapply(seq_along(cfg$steps), function(i) {
      btn_class <- if (current == i) "lc-btn-primary" else "lc-btn-outline"
      actionButton(paste0("ch3_case", case_id, "_step", i),
                   cfg$steps[i], class = btn_class)
    })

    # Przyciski hipotez (pojawiaja sie dopiero po wybudowaniu CI).
    # Hipoteza jest "aktywna" gdy state odpowiada jej idx (tresc lub werdykt).
    hyp_btns <- if (current >= n_core) {
      lapply(seq_along(cfg$hypotheses), function(j) {
        is_active <- !is.null(phase) && phase$idx == j
        btn_class <- if (is_active) "lc-btn-warning" else "lc-btn-warning-outline"
        actionButton(paste0("ch3_case", case_id, "_hyp", j),
                     paste0("Hipoteza ", j), class = btn_class)
      })
    } else {
      list(helpText("Wybuduj pełny przedział, żeby sprawdzić hipotezy."))
    }

    # Drugi rzad: przycisk "Pokaż werdykt" - tylko gdy hipoteza wybrana i jeszcze nie odkryta
    reveal_row <- if (!is.null(phase) && !phase$reveal) {
      div(class = "step-buttons lc-mt-xs",
        actionButton(paste0("ch3_case", case_id, "_reveal"),
                     "\U0001f50d Pokaż werdykt", class = "lc-btn-ok"))
    } else {
      NULL
    }

    tagList(
      div(class = "step-buttons", core_btns),
      div(class = "step-buttons lc-mt-xs", hyp_btns),
      reveal_row
    )
  }

  # ---- Glowny render: plot + explanation dla case'a ----
  render_case_plot <- function(case_id) {
    cfg <- cases_config[[case_id]]
    step <- ch3_case_state[[case_id]]
    n_core <- n_core_steps(cfg)

    if (step == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "Kliknij pierwszy krok, żeby zacząć",
                   size = 5, color = upwr_reference) +
          theme_void()
      )
    }

    # Czy jesteśmy w fazie hipotezy?
    n_hyp <- length(cfg$hypotheses)
    phase <- hyp_phase(step, n_core, n_hyp)
    hypothesis <- NULL
    plot_step <- step
    if (!is.null(phase)) {
      # W obu sub-fazach (tresc i werdykt) plot wyglada tak samo:
      # pelny CI + obszar hipotezy. Werdykt jest tylko w wyjasnieniu.
      hyp_obj <- cfg$hypotheses[[phase$idx]]
      # Hipoteza pairwise (dla forest plot) nie ma bound/dir - nie rysujemy obszaru,
      # studenci patrza na nakladanie sie CI poszczegolnych grup.
      if (is.null(hyp_obj$kind) || hyp_obj$kind != "pairwise") {
        hypothesis <- hyp_obj
      }
      plot_step <- n_core
    }

    switch(cfg$type,
      "single_mean" = plot_single_mean_step(cfg$data, plot_step, cfg$xlab,
                                             hypothesis = hypothesis),
      "compare_n"   = plot_compare_n_step(cfg$data, plot_step, cfg$xlab,
                                           hypothesis = hypothesis),
      "diff_means"  = plot_diff_means_step(cfg$data, plot_step, cfg$xlab,
                                            hypothesis = hypothesis),
      "forest"      = plot_forest_step(cfg$data, plot_step, cfg$xlab,
                                        hypothesis = hypothesis)
    )
  }

  # ---- Pairwise: macierz nakladania CI dla forest plot ----
  # Zwraca macierz logiczna NxN: TRUE = grupy roznia sie istotnie (CI nie nakladaja).
  forest_pairwise_matrix <- function(data) {
    k <- length(data$groups)
    cis <- lapply(seq_len(k), function(i) {
      ci_mean(data$means[i], data$sds[i], data$ns[i])
    })
    m <- matrix(FALSE, nrow = k, ncol = k,
                dimnames = list(data$groups, data$groups))
    for (i in seq_len(k)) for (j in seq_len(k)) {
      if (i == j) next
      # Roznia sie gdy CI[i] i CI[j] sie nie nakladaja
      m[i, j] <- (cis[[i]]$upper < cis[[j]]$lower) ||
                 (cis[[j]]$upper < cis[[i]]$lower)
    }
    m
  }

  # Tabelka HTML dla macierzy pairwise
  render_pairwise_table <- function(mat) {
    groups <- rownames(mat)
    k <- length(groups)
    header <- tags$tr(
      tags$th(""),
      lapply(groups, function(g) tags$th(g, style = "padding: 4px 8px; text-align: center; font-size: 12px;"))
    )
    rows <- lapply(seq_len(k), function(i) {
      tags$tr(
        tags$th(groups[i], style = "padding: 4px 8px; text-align: right; font-size: 12px;"),
        lapply(seq_len(k), function(j) {
          if (i == j) {
            tags$td("—", style = "padding: 4px 8px; text-align: center; color: var(--upwr-reference);")
          } else if (mat[i, j]) {
            tags$td("✓", style = "padding: 4px 8px; text-align: center; color: var(--upwr-sage); font-weight: bold; font-size: 16px;")
          } else {
            tags$td("×", style = "padding: 4px 8px; text-align: center; color: var(--upwr-accent); font-size: 16px;")
          }
        })
      )
    })
    tags$table(
      style = "border-collapse: collapse; margin: 8px auto; border: 1px solid var(--upwr-rule);",
      tags$thead(header),
      tags$tbody(rows)
    )
  }

  # Narracja "jak w raporcie" dla pairwise
  pairwise_narrative <- function(data, mat, unit = "") {
    groups <- data$groups
    means <- data$means
    k <- length(groups)
    unit_str <- if (nzchar(unit)) paste0(" ", unit) else ""

    # Wyciagnij istotne pary z gornego trojkata, z kierunkiem (wieksza > mniejsza)
    diff_pairs <- list()
    for (i in seq_len(k - 1)) for (j in seq(i + 1, k)) {
      if (mat[i, j]) {
        if (means[i] > means[j]) {
          diff_pairs[[length(diff_pairs) + 1]] <- list(hi = groups[i], lo = groups[j])
        } else {
          diff_pairs[[length(diff_pairs) + 1]] <- list(hi = groups[j], lo = groups[i])
        }
      }
    }
    n_diff <- length(diff_pairs)

    if (n_diff == 0) {
      return(paste0(
        "Żadna para grup nie wykazała istotnej różnicy — wszystkie 95% CI ",
        "nakładają się wzajemnie. Na podstawie tych danych nie możemy ",
        "stwierdzić różnic między badanymi grupami."
      ))
    }

    # Sprawdz czy jedna grupa odstaje od WSZYSTKICH innych (np. SOR vs reszta)
    standout_idx <- which(sapply(seq_len(k), function(i) all(mat[i, -i])))
    if (length(standout_idx) == 1) {
      i <- standout_idx
      others <- means[-i]
      direction <- if (means[i] > max(others)) "wyższą" else "niższą"
      return(paste0(
        "Spośród wszystkich badanych grup wyraźnie odstaje ",
        tags$b(groups[i]), " (średnia ", round(means[i], 1), unit_str,
        ") — ma istotnie ", direction, " wartość niż każda z pozostałych grup ",
        "(jej 95% CI nie nakłada się z żadnym innym). ",
        "Pozostałe grupy mają średnie w przedziale ",
        round(min(others), 1), "–", round(max(others), 1), unit_str,
        ", a ich CI nakładają się — nie możemy stwierdzić między nimi istotnych różnic."
      ))
    }

    # Wymien konkretne istotne pary
    pair_strs <- sapply(diff_pairs, function(pp) {
      paste0(tags$b(pp$hi), " > ", tags$b(pp$lo))
    })
    pairs_inline <- if (length(pair_strs) == 1) {
      pair_strs[1]
    } else if (length(pair_strs) == 2) {
      paste(pair_strs, collapse = " oraz ")
    } else {
      paste0(paste(pair_strs[-length(pair_strs)], collapse = ", "),
             " oraz ", pair_strs[length(pair_strs)])
    }

    intro <- if (n_diff == 1) {
      "Spośród wszystkich porównań jedynie jedna para wykazała istotną różnicę: "
    } else {
      paste0("Istotne różnice (CI nie nakładają się) wykazały ",
             n_diff, " pary: ")
    }

    paste0(
      intro, pairs_inline, ". ",
      "Pozostałe pary nie różnią się istotnie — ich 95% CI nakładają się, ",
      "więc na podstawie tych danych nie możemy między nimi rozróżnić."
    )
  }

  # ---- Render: explanation ----
  render_case_explain <- function(case_id) {
    cfg <- cases_config[[case_id]]
    step <- ch3_case_state[[case_id]]
    n_core <- n_core_steps(cfg)

    if (step == 0) return(NULL)

    # Faza hipotezy
    n_hyp <- length(cfg$hypotheses)
    phase <- hyp_phase(step, n_core, n_hyp)
    if (!is.null(phase)) {
      hyp <- cfg$hypotheses[[phase$idx]]

      # Sub-faza 1: tylko tresc hipotezy (czas na dyskusje ze studentami)
      if (!phase$reveal) {
        return(lc_feedback(type = "info",
          p(tags$strong("Hipoteza ", phase$idx, ": "), hyp$text),
          p(tags$em("Spojrz na wykres: gdzie lezy CI wzgledem obszaru hipotezy?
                    Co o tym sadzicie? Kliknięcie ", tags$b("Pokaż werdykt"),
                    " odsloni odpowiedź."))
        ))
      }

      # Sub-faza 2: werdykt + wyjasnienie
      # Specjalny przypadek: hipoteza pairwise dla forest plot
      if (!is.null(hyp$kind) && hyp$kind == "pairwise") {
        mat <- forest_pairwise_matrix(cfg$data)
        narrative <- pairwise_narrative(cfg$data, mat,
                                         unit = if (!is.null(hyp$unit)) hyp$unit else "")
        return(lc_feedback(type = "ok",
          p(tags$strong("Hipoteza: "), hyp$text),
          p(tags$strong("Werdykt — macierz par:")),
          p(tags$em("✓ = grupy różnią się istotnie (CI nie nakładają się);  ",
                    "× = nie można stwierdzić różnicy (CI nakładają się)"),
            style = "font-size: 12px; color: var(--upwr-reference);"),
          render_pairwise_table(mat),
          p(tags$strong("Jak to opisać w raporcie:"),
            style = "margin-top: 12px;"),
          p(HTML(narrative), style = "font-style: italic;")
        ))
      }

      verdict <- compute_verdict_for_case(cfg, hyp)
      cls <- verdict_class(verdict)
      label <- verdict_label(verdict)

      body <- if (verdict == "yes" && !is.null(hyp$explain_yes)) {
        p(hyp$explain_yes)
      } else if (verdict == "no" && !is.null(hyp$explain_no)) {
        p(hyp$explain_no)
      } else {
        p("CI przecina granicę hipotezy — nie możemy jednoznacznie
          stwierdzić, czy jest prawdziwa.")
      }

      return(lc_feedback(type = cls,
        p(tags$strong("Hipoteza ", phase$idx, ": "), hyp$text),
        p(tags$strong("Werdykt: ", label)),
        body
      ))
    }

    # Faza budowy CI — wyjaśnienie ostatniego kroku
    # Dla uproszczenia: krótki neutralny feedback.
    lc_feedback(type = "info",
      p(tags$strong(cfg$steps[step])),
      p("Krok ", step, " z ", n_core, ".")
    )
  }

  # ---- Werdykt dla case'a ----
  compute_verdict_for_case <- function(cfg, hyp) {
    switch(cfg$type,
      "single_mean" = {
        ci <- ci_mean(cfg$data$xbar, cfg$data$s, cfg$data$n)
        hypothesis_verdict(ci$lower, ci$upper, hyp$bound, hyp$dir)
      },
      "compare_n" = {
        # Werdykt bazujemy na najwazszym (najwiekszym n) CI
        # (= najbardziej precyzyjnym oszacowaniu)
        largest_n <- max(cfg$data$ns)
        ci <- ci_mean(cfg$data$xbar, cfg$data$s, largest_n)
        hypothesis_verdict(ci$lower, ci$upper, hyp$bound, hyp$dir)
      },
      "diff_means" = {
        cid <- ci_diff_means(cfg$data$x1, cfg$data$s1, cfg$data$n1,
                              cfg$data$x2, cfg$data$s2, cfg$data$n2)
        hypothesis_verdict(cid$lower, cid$upper, hyp$bound, hyp$dir)
      },
      "forest" = {
        # Znajdz odpowiednia grupe
        idx <- which(cfg$data$groups == hyp$which)
        ci <- ci_mean(cfg$data$means[idx], cfg$data$sds[idx], cfg$data$ns[idx])
        hypothesis_verdict(ci$lower, ci$upper, hyp$bound, hyp$dir)
      }
    )
  }

  # ---- Podlaczenie observerow + outputow dla kazdego case'a ----
  # case_id przekazywany jako argument funkcji = wlasciwe closure dla kazdego case'a
  # (zastepuje wczesniejszy wzorzec for + local({}), ktory cicho gubil rejestracje).
  register_case <- function(case_id) {
    cfg <- cases_config[[case_id]]
    n_core <- length(cfg$steps)

    # Observery dla przyciskow core stepow
    lapply(seq_along(cfg$steps), function(i) {
      force(i)
      observeEvent(input[[paste0("ch3_case", case_id, "_step", i)]], {
        ch3_case_state[[case_id]] <- i
      }, ignoreInit = TRUE)
    })

    # Observery dla przyciskow hipotez (kazda hipoteza -> faza "tylko tresc")
    lapply(seq_along(cfg$hypotheses), function(j) {
      force(j)
      observeEvent(input[[paste0("ch3_case", case_id, "_hyp", j)]], {
        ch3_case_state[[case_id]] <- hyp_state(n_core, j, reveal = FALSE)
      }, ignoreInit = TRUE)
    })

    # Observer dla przycisku "Pokaz werdykt" - przelacza obecna hipoteze w faze "werdykt"
    observeEvent(input[[paste0("ch3_case", case_id, "_reveal")]], {
      current <- ch3_case_state[[case_id]]
      n_hyp <- length(cfg$hypotheses)
      phase <- hyp_phase(current, n_core, n_hyp)
      if (!is.null(phase) && !phase$reveal) {
        ch3_case_state[[case_id]] <- hyp_state(n_core, phase$idx, reveal = TRUE)
      }
    }, ignoreInit = TRUE)

    # Rendery z jawna reaktywna zaleznoscia na ch3_case_state[[case_id]]
    output[[paste0("ch3_case", case_id, "_buttons")]] <- renderUI({
      ch3_case_state[[case_id]]
      case_buttons_ui(case_id)
    })
    output[[paste0("ch3_case", case_id, "_plot")]] <- renderPlot({
      ch3_case_state[[case_id]]
      render_case_plot(case_id)
    })
    output[[paste0("ch3_case", case_id, "_explain")]] <- renderUI({
      ch3_case_state[[case_id]]
      render_case_explain(case_id)
    })
  }

  for (cid in names(cases_config)) {
    register_case(cid)
  }

  # ==========================================================================
  # WIDGET 2B: NAKLADAJACE SIE CI GRUP vs CI ROZNICY
  # Trzy statyczne scenariusze z dziedziny technologii zywnosci
  # ==========================================================================

  # --- Dane (statyczne, przygotowane z ustalonymi seedami) ---
  ch3_comp_data <- list(
    A = list(
      g1_name = "Dostawca A",
      g2_name = "Dostawca B",
      unit    = "zawartość białka (%)",
      g1 = c(13.17,11.08,11.38,11.55,11.22,11.23,12.25,11.73,11.89,13.11,
             12.01,13.43,13.17,11.99,12.94,12.08,11.26,11.62,11.80,12.39,
             12.30,12.22,12.58,10.97,12.56,11.91,12.25,12.16,11.21,11.63,
             11.28,12.23,11.87,11.75,11.55,11.46,12.40,11.14,11.71,11.99),
      g2 = c(11.63,10.48,10.73,10.11,10.67,10.66,11.71,11.25,10.96,11.46,
             10.74,10.90,11.12,11.92,11.33,11.19, 9.96,11.09,11.00,10.36,
             10.95,11.00,11.23,11.32,11.09,11.57,11.36,11.59,11.66,11.32,
             11.16,10.35,10.53,10.38, 9.92,10.10,10.37,10.57,10.86,12.35)
    ),
    B = list(
      g1_name = "Szkło",
      g2_name = "Plastik",
      unit    = "zawartość tłuszczu (%)",
      g1 = c(3.03,3.19,2.80,2.84,3.47,2.95,3.51,3.34,3.17,2.93,
             2.97,3.09,2.80,3.12,2.89,3.18,3.12,3.40,3.03,3.02,
             3.01,3.18,3.07,3.27,3.20,3.18,3.13,2.99,3.12,2.93),
      g2 = c(2.93,2.98,3.38,2.82,2.99,3.33,3.16,3.60,3.06,3.12,
             2.80,3.22,3.43,2.99,3.43,3.12,2.66,3.43,3.39,3.26,
             3.41,3.15,3.01,3.33,3.25,3.35,3.17,3.32,3.58,3.23)
    ),
    C = list(
      g1_name = "Linia 1",
      g2_name = "Linia 2",
      unit    = "zawartość błonnika (g / 100 g)",
      # Wygenerowane: set.seed(3); round(rnorm(120, 9.3, 1.0), 2), round(rnorm(120, 9.0, 1.0), 2)
      g1 = c(8.34,8.99,10.62,7.52,9.70,10.56,10.25, 9.04, 8.49,10.13,
             7.94,10.82,10.78, 7.98,11.47, 9.16,10.82, 8.87, 7.83, 8.53,
             8.35,12.44, 9.78, 9.70,11.17, 8.54, 9.25, 8.43,10.52, 8.06,
             8.64, 8.22,10.67, 9.71, 9.80,10.42,10.19,11.20,10.62, 9.83,
             8.61, 9.36, 9.62, 9.18, 9.76, 9.53,10.91,10.57, 9.08, 9.46,
             10.25, 7.21, 9.78, 9.64, 8.19,10.27, 9.46, 9.29,10.25, 7.76,
             11.24, 9.04, 9.85,10.18,11.01, 8.86, 9.24, 9.28, 9.09, 8.29,
             9.87,10.10, 9.17, 9.41,11.07, 9.18, 9.19,10.50, 9.79, 8.65,
             8.85, 9.22,10.81,10.12, 8.36, 8.92, 8.70,10.63, 9.81, 9.07,
             9.44, 9.43, 8.67,11.13, 7.88,10.15, 9.56, 9.78, 8.75, 9.82,
             8.51, 8.41, 9.77, 9.08, 7.55, 9.04,10.43, 7.98, 7.29, 8.58,
             8.12, 8.16, 9.74, 9.25, 8.69,10.36, 7.22,10.15, 9.52, 8.95),
      g2 = c(8.30,10.15, 9.77, 7.74, 9.45, 7.68, 7.64, 9.92, 9.36, 8.28,
             9.14,10.19, 9.85, 7.83, 9.09, 9.45, 7.63, 9.09, 8.52, 9.26,
             8.20,10.05,11.76, 8.75,10.03, 9.95, 9.54, 8.60, 8.70, 7.74,
             8.94, 9.74, 9.41,10.51, 7.75, 7.99, 9.08, 9.20, 8.13, 7.76,
             8.31, 9.10, 9.40,10.24, 9.16,11.00, 9.25,10.82, 8.96, 9.42,
             8.10,10.32, 9.01, 8.35, 9.36,10.20, 9.71, 7.18, 8.33, 8.24,
             9.56, 8.10, 8.83, 9.32, 8.00, 8.33, 8.20,10.19,10.94, 8.35,
             8.87, 7.86, 9.56, 9.09,10.51, 9.30, 7.26, 8.50, 8.16, 8.30,
             8.45, 8.04,10.66, 7.19,10.16, 8.73, 8.61, 9.17, 8.85, 9.57,
             9.28, 9.64,10.54, 8.79,11.00,10.24, 9.75,10.32, 8.96, 7.31,
             9.35, 8.64, 9.17,10.40, 7.64, 9.23, 9.86, 9.59, 9.48, 9.27,
             8.32, 8.92, 9.76, 9.57, 8.72, 8.07, 7.55, 9.74, 9.43, 9.32)
    )
  )

  # Helper: liczy CI dla dwoch grup + CI roznicy (Welch)
  ch3_comp_cis <- function(g1, g2, conf = 0.95) {
    alpha <- 1 - conf
    n1 <- length(g1); n2 <- length(g2)
    m1 <- mean(g1);   m2 <- mean(g2)
    s1 <- sd(g1);     s2 <- sd(g2)
    se1 <- s1 / sqrt(n1); se2 <- s2 / sqrt(n2)
    ci1 <- m1 + c(-1, 1) * qt(1 - alpha / 2, n1 - 1) * se1
    ci2 <- m2 + c(-1, 1) * qt(1 - alpha / 2, n2 - 1) * se2
    se_d <- sqrt(se1^2 + se2^2)
    df_w <- (se1^2 + se2^2)^2 / (se1^4 / (n1 - 1) + se2^4 / (n2 - 1))
    ci_d <- (m1 - m2) + c(-1, 1) * qt(1 - alpha / 2, df_w) * se_d
    list(m1 = m1, m2 = m2, ci1 = ci1, ci2 = ci2,
         md = m1 - m2, ci_d = ci_d,
         overlap_lo = max(ci1[1], ci2[1]),
         overlap_hi = min(ci1[2], ci2[2]))
  }

  # Helper: plot trzech CI (grupa 1, grupa 2, roznica) z paskiem nakladania
  ch3_comp_plot <- function(scenario_key) {
    dat <- ch3_comp_data[[scenario_key]]
    cis <- ch3_comp_cis(dat$g1, dat$g2)

    # Rama wykresu: lewy panel (CI grup), prawy panel (CI roznicy)
    # Zrobimy w jednym plocie z facet_grid
    df_groups <- data.frame(
      row   = c(2, 1),
      label = c(dat$g1_name, dat$g2_name),
      mean  = c(cis$m1, cis$m2),
      lo    = c(cis$ci1[1], cis$ci2[1]),
      hi    = c(cis$ci1[2], cis$ci2[2]),
      panel = "CI grup (osobno)"
    )
    df_diff <- data.frame(
      row   = 1.5,
      label = paste0(dat$g1_name, " − ", dat$g2_name),
      mean  = cis$md,
      lo    = cis$ci_d[1],
      hi    = cis$ci_d[2],
      panel = "CI różnicy"
    )

    overlap_present <- cis$overlap_lo <= cis$overlap_hi
    df_overlap <- if (overlap_present) {
      data.frame(xmin = cis$overlap_lo, xmax = cis$overlap_hi, panel = "CI grup (osobno)")
    } else {
      NULL
    }

    p_groups <- ggplot(df_groups, aes(y = row)) +
      { if (!is.null(df_overlap))
          geom_rect(data = df_overlap,
                    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                    inherit.aes = FALSE,
                    fill = "#f1c40f", alpha = 0.25)
      } +
      geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.18,
                     color = col_ci, linewidth = 1.4) +
      geom_point(aes(x = mean), color = col_estimate, size = 4) +
      geom_text(aes(x = mean, label = sprintf("%.2f", mean)),
                vjust = -1.2, color = col_estimate, fontface = "bold", size = 4.2) +
      scale_y_continuous(breaks = df_groups$row, labels = df_groups$label,
                         limits = c(0.5, 2.5)) +
      labs(title = "CI grup (osobno)", x = dat$unit, y = NULL) +
      theme_upwr() +
      theme(plot.title = element_text(size = 13, face = "bold"))

    p_diff <- ggplot(df_diff, aes(y = row)) +
      geom_vline(xintercept = 0, color = col_true,
                 linewidth = 1.0, linetype = "dashed") +
      annotate("text", x = 0, y = 2.3, label = "0",
               color = col_true, fontface = "bold", size = 4.5) +
      geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.18,
                     color = col_ci, linewidth = 1.4) +
      geom_point(aes(x = mean), color = col_estimate, size = 4) +
      geom_text(aes(x = mean, label = sprintf("%.2f", mean)),
                vjust = -1.2, color = col_estimate, fontface = "bold", size = 4.2) +
      scale_y_continuous(breaks = df_diff$row, labels = df_diff$label,
                         limits = c(0.5, 2.5)) +
      labs(title = paste0("CI różnicy (", dat$g1_name, " − ", dat$g2_name, ")"),
           x = paste("różnica —", dat$unit), y = NULL) +
      theme_upwr() +
      theme(plot.title = element_text(size = 13, face = "bold"))

    # Uklad jeden pod drugim
    gridExtra::grid.arrange(p_groups, p_diff, ncol = 1, heights = c(1, 1))
  }

  # Helper: werdykt tekstowy
  ch3_comp_verdict <- function(scenario_key) {
    dat <- ch3_comp_data[[scenario_key]]
    cis <- ch3_comp_cis(dat$g1, dat$g2)
    overlap_present <- cis$overlap_lo <= cis$overlap_hi
    diff_excludes_0 <- !(cis$ci_d[1] <= 0 & 0 <= cis$ci_d[2])
    overlap_w <- if (overlap_present) cis$overlap_hi - cis$overlap_lo else 0

    fmt <- function(x) sprintf("%.2f", x)
    ci_txt <- function(ci) paste0("[", fmt(ci[1]), "; ", fmt(ci[2]), "]")

    # Wspolna czesc opisu
    facts <- tagList(
      p(tags$b("Co widzimy:")),
      tags$ul(
        tags$li(dat$g1_name, ": średnia ", fmt(cis$m1),
                ", 95% CI ", ci_txt(cis$ci1)),
        tags$li(dat$g2_name, ": średnia ", fmt(cis$m2),
                ", 95% CI ", ci_txt(cis$ci2)),
        tags$li(tags$b("CI różnicy"), " (",
                dat$g1_name, " − ", dat$g2_name, "): ",
                ci_txt(cis$ci_d))
      ),
      tags$ul(
        tags$li("Czy CI grup się nakrywają? ",
                tags$b(if (overlap_present)
                  paste0("TAK (na odcinku szerokości ", fmt(overlap_w), ")")
                else "NIE")),
        tags$li("Czy CI różnicy zawiera 0? ",
                tags$b(if (diff_excludes_0) "NIE" else "TAK"))
      )
    )

    # Werdykt w zaleznosci od scenariusza
    if (scenario_key == "A") {
      lc_feedback(type = "ok",
        facts,
        p(tags$b("Werdykt:"),
          " Oba spojrzenia zgodne. CI grup się nie nakrywają, a CI różnicy
          nie zawiera 0 — średnia zawartość białka różni się istotnie.
          Najlepsze oszacowanie: mąka Dostawcy A ma o ",
          fmt(cis$ci_d[1]), "–", fmt(cis$ci_d[2]),
          " punktu procentowego więcej białka.")
      )
    } else if (scenario_key == "B") {
      lc_feedback(type = "ok",
        facts,
        p(tags$b("Werdykt:"),
          " Oba spojrzenia zgodne. CI grup mocno się nakrywają, a CI różnicy
          zawiera 0 — nie mamy podstaw mówić, że materiał opakowania
          wpływa na zawartość tłuszczu w jogurcie.")
      )
    } else {
      lc_feedback(type = "warning",
        facts,
        p(tags$b("Spojrzenia się rozjeżdżają:")),
        tags$ul(
          tags$li("Wzrokiem: CI grup ledwo się stykają (nakrywają się na odcinku ",
                  fmt(overlap_w),
                  " g) — naiwnie powiedzielibyśmy \"linie produkują podobne płatki\"."),
          tags$li("Liczbowo: CI różnicy to ", ci_txt(cis$ci_d),
                  " — nie zawiera 0, więc różnica jest istotna.
                   Linia 1 produkuje płatki o ",
                   fmt(cis$ci_d[1]), "–", fmt(cis$ci_d[2]),
                   " g / 100 g bogatsze w błonnik.")
        ),
        p(tags$b("Dlaczego CI różnicy jest węższe niż suma CI grup?"),
          " Bo błąd standardowy różnicy to ",
          withMathJax("\\(\\sqrt{SE_1^2 + SE_2^2}\\)"),
          ", a nie ", withMathJax("\\(SE_1 + SE_2\\)"),
          ". Matematyka łączy niepewności \"po pitagorasie\", nie przez sumowanie —
          dlatego CI różnicy jest ostrzejszym narzędziem niż porównywanie CI grup na oko.")
      )
    }
  }

  output$ch3_comp_A_plot <- renderPlot({ ch3_comp_plot("A") })
  output$ch3_comp_B_plot <- renderPlot({ ch3_comp_plot("B") })
  output$ch3_comp_C_plot <- renderPlot({ ch3_comp_plot("C") })
  output$ch3_comp_A_verdict <- renderUI({ ch3_comp_verdict("A") })
  output$ch3_comp_B_verdict <- renderUI({ ch3_comp_verdict("B") })
  output$ch3_comp_C_verdict <- renderUI({ ch3_comp_verdict("C") })

}
