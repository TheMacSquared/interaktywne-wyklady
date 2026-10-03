# ============================================================================
# CHAPTER 4: Jedna zmienna ilosciowa — test t jednej proby
# ============================================================================

ch2_ui <- list(
  id = "ch-jedna-ilosciowa", num = "04", title = "Test t jednej próby",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 04 · Testowanie hipotez",
      num    = "04",
      title  = "Test t jednej próby.",
      lead   = "„Czy nasi studenci mają typowy poziom koncentracji?” Jeden pomiar na
                osobie, porównanie średniej z wartością referencyjną — od danych, przez
                statystykę testową, do p-wartości."
    ),

    # ========================================================================
    # Case study otwierajacy
    # ========================================================================
    lc_h2("ch2-pytanie", "Od pytania do testu"),

    tagList(
      p("Statystyk nie zaczyna od wzorów — zaczyna od pytania. Ktoś przychodzi i pyta w języku potocznym:"),
      lc_feedback(type = "info", style = "font-size: 18px; text-align: center;",
        tags$em("„Czy nasi studenci mają typowy poziom koncentracji?
        Bo wydaje mi się, że coś z nimi jest nie tak.”")
      ),
      p("Zadanie statystyka: przełożyć to na formalną hipotezę i dodać kontekst —
        typowy to ile? Mamy wartość referencyjną
        z pilotażu: średni wynik testu koncentracji w populacji = 70 pkt."),
      p("Pytanie potoczne zamienia się w jedną z trzech par hipotez
        — zależnie od tego, w którą stronę pytamy:"),
      lc_formula_box(
        p(tags$b("Dwustronna"), " (sprawdzamy, czy średnia w ogóle się różni):"),
        p(withMathJax("\\(H_0: \\mu = 70 \\quad\\)"),
          withMathJax("\\(H_a: \\mu \\neq 70\\)"))
      ),
      lc_formula_box(
        p(tags$b("Prawostronna"), " (pytamy, czy średnia jest ",
          tags$em("wyższa"), " niż norma):"),
        p(withMathJax("\\(H_0: \\mu \\leq 70 \\quad\\)"),
          withMathJax("\\(H_a: \\mu > 70\\)"))
      ),
      lc_formula_box(
        p(tags$b("Lewostronna"), " (pytamy, czy średnia jest ",
          tags$em("niższa"), " niż norma):"),
        p(withMathJax("\\(H_0: \\mu \\geq 70 \\quad\\)"),
          withMathJax("\\(H_a: \\mu < 70\\)"))
      ),
      p("Wybór wariantu wynika z brzmienia ", gloss("pytanie badawcze", "pytania badawczego"), " i musi być
        zdecydowany przed zbieraniem danych."),
      p("Niezależnie od wybranego wariantu, liczymy tę samą ",
        gloss("statystyka testowa", "statystykę testową"), " — mierzy ona, ile ",
        gloss("błąd standardowy", "błędów standardowych"), "
        dzieli średnią z próby od wartości referencyjnej ",
        withMathJax("\\(\\mu_0\\)"),
        ". Różni się tylko sposób liczenia ", gloss("p-wartość", "p-wartości"), " (po jednej albo po obu stronach rozkładu)."),
      p("Wzór na ", gloss("test t"), " jednej próby:"),
      lc_formula_box(
        p(withMathJax("\\(t = \\frac{\\bar{x} - \\mu_0}{s / \\sqrt{n}}, \\quad df = n - 1\\)"))
      )
    ),

    # ========================================================================
    # Cwiczenie: sformuluj hipotezy
    # ========================================================================
    lc_h2("ch2-cwiczenie", "Ćwiczenie: sformułuj hipotezy"),

    tagList(
      p("Zanim zobaczysz test w działaniu — spróbuj sam. Dla każdego pytania
        badawczego zastanów się, jak wyglądałyby H₀ i Hₐ",
        ". Przedyskutuj w grupie, a potem kliknij „Pokaż odpowiedź”.")
    ),

    hypothesis_practice("ch2", list(
      list(
        question = "Producent deklaruje, że średnia zawartość soli w chlebie
                    wynosi 1,2 g / 100 g. Chcemy sprawdzić, czy jego deklaracja
                    pasuje do rzeczywistości.",
        h0 = "\\(H_0: \\mu = 1{,}2\\) (zgodnie z deklaracją)",
        ha = "\\(H_a: \\mu \\neq 1{,}2\\) (odbiega od deklaracji)",
        note = "Dwustronny — nie wiemy, w którą stronę może odbiegać."
      ),
      list(
        question = "Norma technologiczna przewiduje, że dojrzewanie sera trwa
                    średnio 45 dni. Producent twierdzi, że jego nowa metoda
                    skraca ten czas.",
        h0 = "\\(H_0: \\mu \\geq 45\\) (nie krócej niż norma)",
        ha = "\\(H_a: \\mu < 45\\) (krócej)",
        note = "Jednostronny (lewostronny) — hipoteza kierunkowa wynika z treści pytania."
      ),
      list(
        question = "Sprawdzamy, czy średnia waga paczki kawy (deklarowana 250 g)
                    jest zgodna z normą. Dla konsumenta ważne jest wykrycie
                    odchyłek w obie strony.",
        h0 = "\\(H_0: \\mu = 250\\)",
        ha = "\\(H_a: \\mu \\neq 250\\)",
        note = "Dwustronny — interesuje nas każde odchylenie, nie tylko niższa waga."
      )
    )),

    # ========================================================================
    # WIDGET 1: Krokowy test t jednej proby
    # ========================================================================
    lc_h2("ch2-krok", "Test t jednej próby — krok po kroku"),

    figure_panel(
      label = "Ryc. 4.1",
      title = "Test t jednej próby — krok po kroku",
      uiOutput("ch2_hypothesis_panel"),
      lc_step_widget("ch2_test",
        steps = c("Dane", "Statystyki opisowe", "Statystyka testowa",
                  "p-wartość i decyzja"),
        toolbar = lc_toolbar(
          selectInput("ch2_scenario", "Scenariusz",
            choices = c(
              "Koncentracja (μ₀ = 70 pkt)" = "concentration",
              "Zużycie wody (μ₀ = 150 l)" = "water",
              "Plon pszenicy (μ₀ = 5 t/ha)" = "yield",
              "Trwałość jogurtu (μ₀ = 14 dni)" = "yogurt",
              "Hałas w hali (μ₀ = 85 dB, IB)" = "noise"
            ),
            selected = "concentration"
          ),
          lc_slider("ch2_n", "Wielkość próby (n)", 10, 100, 40, 5),
          lc_action("ch2_new_sample", "Losuj próbę", icon = "shuffle", variant = "solid")
        ),
        plot_id = "ch2_step_plot"
      )
    ),

    # ========================================================================
    # Interpretacja
    # ========================================================================
    inline_callout(
      label = "Co zrobiliśmy?",
      tagList(
        tags$ol(
          tags$li("Zebraliśmy dane (", gloss("próba", "próbę"), ")"),
          tags$li("Obliczyliśmy średnią i ", gloss("odchylenie standardowe")),
          tags$li("Policzyliśmy, jak daleko średnia z próby jest od μ₀ — to statystyka t"),
          tags$li("Sprawdziliśmy, czy taka wartość t jest zaskakująca (p-wartość)")
        ),
        tags$p("Jeśli p < 0.05, różnica między naszą próbą a wartością referencyjną
               jest zbyt duża, by ją wytłumaczyć przypadkiem.")
      )
    ),

    # ========================================================================
    # WIDGET 2: Test jednostronny — to samo pytanie, ale z kierunkiem
    # ========================================================================
    lc_h2("ch2-jednostronny", "A jeśli znamy kierunek? Test jednostronny"),

    tagList(
      p("W pierwszym teście pytaliśmy: „czy średnia różni się od μ₀?” (dwustronny, ≠).
        Ale czasem mamy silniejsze podejrzenie — nie tylko „czy różni się”,
        ale „czy jest większa / mniejsza”."),
      p("Użyjemy tych samych danych co powyżej, ale zmienimy pytanie na kierunkowe.
        Zobaczcie, jak zmienia się hipoteza i wykres.")
    ),

    figure_panel(
      label = "Ryc. 4.2",
      title = "Test t jednostronny — krok po kroku",
      uiOutput("ch2b_hypothesis_panel"),
      lc_step_widget("ch2b_test",
        steps = c("Dane", "Statystyki opisowe", "Statystyka testowa",
                  "p-wartość i decyzja"),
        toolbar = lc_toolbar(
          helpText("Dane: te same co w teście dwustronnym powyżej.")
        ),
        plot_id = "ch2b_step_plot"
      )
    ),

    inline_callout(
      label = "Dwustronny a jednostronny",
      tagList(
        tags$ul(
          tags$li(tags$b("Dwustronny (≠):"), " bezpieczniejszy, wykrywa efekt w obie strony.
            Punkt krytyczny dalej od zera — trudniej odrzucić H₀."),
          tags$li(tags$b("Jednostronny (> lub <):"), " mocniejszy w jednym kierunku,
            ale ", tags$em("ślepy"), " na efekt w drugim."),
          tags$li("Regułą: jednostronny decydujemy przed zbieraniem danych!")
        )
      ),
      color = "uwaga"
    ),

    lc_h2("ch2-cas", "Ćwiczenia", "CASchools — test t jednej próby"),

    lc_feedback(type = "info",
      p(tags$b("Dane: "), "420 okręgów szkolnych Kalifornii (1998–1999). Plik: ",
        tags$code("dane/caschools.csv"), "."),
      p("Zmienne w zadaniach: ", tags$code("read"),
        " (średni wynik z czytania, ok. 655 pkt), ",
        tags$code("income"), " (dochód okręgu, tys. USD).")
    ),

    figure_panel(label = "Ćwiczenie",
      h4("Zadanie 1 — Czy wyniki z czytania różnią się od normy 650 pkt?"),
      p("Departament edukacji podaje normę 650 pkt. Przetestuj, czy średni wynik ",
        tags$code("read"), " w okręgach Kalifornii istotnie różni się",
        " od 650. Sformułuj H₀ i Hₐ, wykonaj test t jednej próby (α = 0.05).
        Co raportowałbyś departamentowi?"),
      lc_action("cas_ch2_ans1", "Pokaż rozwiązanie", variant = "solid"),
      uiOutput("cas_ch2_sol1")
    ),

    figure_panel(label = "Ćwiczenie",
      h4("Zadanie 2 — Czy typowy dochód okręgu przekracza 15 tys. USD?"),
      p("Hipoteza dyrekcji: „Nasz stan to stan zamożnych\" — typowy okrąg ma dochód
        powyżej 15 tys. USD. Przetestuj jednostronnie (prawostronnie)",
        " zmienną ", tags$code("income"),
        ". Sformułuj H₀ i Hₐ dla hipotezy kierunkowej.
        Czy wynik jest istotny statystycznie? A praktycznie?"),
      lc_action("cas_ch2_ans2", "Pokaż rozwiązanie", variant = "solid"),
      uiOutput("cas_ch2_sol2")
    ),

    lc_chapter_next(
      num       = "05",
      title     = "Test proporcji",
      lead      = "a co gdy pytamy nie o średnią, lecz o procent?",
      target_id = "ch-jedna-jakosciowa"
    )
  )
)

# ============================================================================
# DANE — CASchools (wczytane raz przy ladowaniu modulu)
# ============================================================================

.ch2_cas <- read.csv(file.path(app_dir, "dane", "caschools.csv"),
                     stringsAsFactors = FALSE)

# ============================================================================
# WIDGETY KROKOWE TESTÓW — wspólne dla rozdziałów 04–08
# ============================================================================
# app.R ładuje ten plik przed ch3–ch6; funkcje są wołane dopiero w serwerze.

# Liczba w opisie kroku: <b> (mono w .lc-stepper-text), bez spacji wokół.
step_num <- function(x) tags$b(x, .noWS = "outside")

# step_label() z wyrażeniem plotmath (np. "mu[0] == 70", "bar(x) == 73.9"):
# indeksy i znaki łączące (μ₀, x̄, p̂) nie mają glifów w czcionkach showtext.
step_symbol_label <- function(x, y, label, role = "known", hjust = 0, vjust = 0,
                              size = 3.6) {
  annotate("text", x = x, y = y, label = label, parse = TRUE, hjust = hjust,
           vjust = vjust, colour = STEP_ROLES[[role]]$colour, fontface = "bold",
           size = size)
}

# Rozkład statystyki pod H₀ w rolach widgetu krokowego.
# phase = "stat": krzywa (znana) i statystyka (nowa);
# phase = "decision": statystyka znana, obszar odrzucenia i wartości krytyczne nowe.
# Rama osi zależy tylko od statystyki i df, więc oba kroki mają tę samą.
step_null_plot <- function(stat, df, type = c("t", "chisq"),
                           alternative = "two.sided",
                           phase = c("stat", "decision"), alpha = 0.05) {
  type <- match.arg(type)
  phase <- match.arg(phase)
  stat <- as.numeric(stat)

  if (type == "t") {
    half <- max(4, abs(stat) * 1.1 + 0.5)
    xlim <- c(-half, half)
    dens <- function(x) dt(x, df)
    crit <- switch(alternative,
      two.sided = qt(1 - alpha / 2, df) * c(-1, 1),
      greater   = qt(1 - alpha, df),
      less      = qt(alpha, df)
    )
    reject <- function(x) switch(alternative,
      two.sided = abs(x) >= crit[2],
      greater   = x >= crit,
      less      = x <= crit
    )
  } else {
    xlim <- c(0, max(stat * 2, 15))
    dens <- function(x) dchisq(x, df)
    crit <- qchisq(1 - alpha, df)
    reject <- function(x) x >= crit
  }

  curve <- data.frame(x = seq(xlim[1], xlim[2], length.out = 600))
  curve$y <- dens(curve$x)
  visible <- is.finite(curve$y) & curve$x >= xlim[1] + diff(xlim) * 0.02
  y_top <- max(curve$y[visible]) * 1.25
  curve$y <- pmin(curve$y, y_top)

  stat_role <- if (phase == "stat") "new" else "known"
  label_hjust <- if (stat > xlim[1] + 0.7 * diff(xlim)) 1.1 else -0.1
  stat_text <- step_label(
    stat, y_top * 0.97,
    paste0(if (type == "t") "t" else "χ²", " = ", lc_fmt(stat, 3)),
    role = stat_role, hjust = label_hjust, vjust = 1
  )

  p <- ggplot(curve, aes(x = x, y = y))
  if (phase == "decision") {
    in_reject <- reject(curve$x)
    # Obszar odrzucenia: osobne fragmenty, żeby geom_area nie łączył ogonów.
    for (part in split(curve[in_reject, ], cumsum(!in_reject)[in_reject])) {
      p <- p + step_layer(geom_area, "new", data = part, fill_role = TRUE,
                          colour = NA, alpha = 0.3)
    }
    p <- p +
      step_layer(geom_area, "background", data = curve[!in_reject, ],
                 fill_role = TRUE, colour = NA) +
      step_line("new", xintercept = crit)
    if (type == "t" && alternative == "two.sided") {
      tail_mid <- (crit[2] + xlim[2]) / 2
      p <- p +
        step_label(0, y_top * 0.45, "nie odrzucamy H0", role = "known", hjust = 0.5) +
        step_label(-tail_mid, y_top * 0.25, "Ha", role = "new", hjust = 0.5) +
        step_label(tail_mid, y_top * 0.25, "Ha", role = "new", hjust = 0.5)
    }
  }
  p +
    step_layer(geom_line, "known") +
    step_line(stat_role, xintercept = stat, helper = FALSE) +
    stat_text +
    labs(x = "Statystyka testowa", y = "Gęstość") +
    step_frame(xlim = xlim, ylim = c(0, y_top))
}

# Decyzja w opisie kroku: werdykt, p z kropką dziesiętną i porównanie z α.
step_verdict <- function(p_value, alpha = 0.05) {
  res <- format_test_result(p_value, alpha)
  sig <- p_value < alpha
  verdict <- lc_verdict(res$decision, type = if (sig) "danger" else "ok")
  verdict$.noWS <- "outside"
  tagList(
    verdict, ". ",
    if (p_value < 0.001) "p < " else "p = ",
    step_num(if (p_value < 0.001) "0.001" else lc_fmt(p_value, 3)),
    if (sig) " < α = " else " ≥ α = ", alpha,
    if (sig) " — wynik istotny statystycznie." else " — wynik nieistotny statystycznie."
  )
}

# ============================================================================
# SERVER
# ============================================================================

ch2_server <- function(input, output, session) {

  # --- Dane scenariuszy ---
  scenario_params <- list(
    concentration = list(mu0 = 70, mu_true = 72, sd = 13,
                         xlab = "Wynik testu koncentracji (pkt)",
                         title = "Koncentracja studentów",
                         question = "Czy nasi studenci mają typowy poziom koncentracji?",
                         h0_text = "\\(H_0: \\mu = 70\\) (koncentracja jest typowa)",
                         h1_text = "\\(H_a: \\mu \\neq 70\\) (koncentracja odbiega od normy)"),
    water  = list(mu0 = 150, mu_true = 158, sd = 25,
                  xlab = "Zużycie wody (l/osobę/dobę)",
                  title = "Zużycie wody w gminie",
                  question = "Czy zużycie wody w naszej gminie spełnia normę projektową 150 l/osobę?",
                  h0_text = "\\(H_0: \\mu = 150\\) (zużycie zgodne z normą)",
                  h1_text = "\\(H_a: \\mu \\neq 150\\) (zużycie odbiega od normy)"),
    yield  = list(mu0 = 5, mu_true = 5.4, sd = 0.8,
                  xlab = "Plon pszenicy (t/ha)",
                  title = "Plony na poletku doświadczalnym",
                  question = "Czy średni plon pszenicy na naszych poletkach odpowiada średniej krajowej 5 t/ha?",
                  h0_text = "\\(H_0: \\mu = 5\\) (plon typowy dla kraju)",
                  h1_text = "\\(H_a: \\mu \\neq 5\\) (plon odbiega od średniej krajowej)"),
    yogurt = list(mu0 = 14, mu_true = 15.2, sd = 2.5,
                  xlab = "Trwałość (dni do przeterminowania)",
                  title = "Trwałość jogurtu naturalnego",
                  question = "Czy trwałość naszego jogurtu spełnia deklarowane 14 dni?",
                  h0_text = "\\(H_0: \\mu = 14\\) (trwałość zgodna z deklaracją)",
                  h1_text = "\\(H_a: \\mu \\neq 14\\) (trwałość odbiega od deklaracji)"),
    noise  = list(mu0 = 85, mu_true = 87.5, sd = 4,
                  xlab = "Poziom hałasu (dB)",
                  title = "Hałas w hali produkcyjnej",
                  question = "Czy średni poziom hałasu w hali mieści się w normie NDS 85 dB?",
                  h0_text = "\\(H_0: \\mu = 85\\) (hałas zgodny z normą)",
                  h1_text = "\\(H_a: \\mu \\neq 85\\) (hałas odbiega od normy)")
  )

  # Jedna wspólna próba dla widgetu dwustronnego i jednostronnego.
  # Trzymamy metadane próbki, żeby po zmianie scenariusza albo n stara
  # próbka była traktowana jako nieaktualna, a nie jako dane do nowego pytania.
  ch2_sample_state <- reactiveVal(NULL)

  observeEvent(input$ch2_new_sample, {
    req(input$ch2_scenario, input$ch2_n)
    par <- scenario_params[[input$ch2_scenario]]
    req(!is.null(par))
    n <- input$ch2_n

    ch2_sample_state(list(
      scenario = input$ch2_scenario,
      n = n,
      values = rnorm(n, mean = par$mu_true, sd = par$sd)
    ))
  }, ignoreInit = TRUE)

  ch2_sample <- reactive({
    state <- ch2_sample_state()
    if (is.null(state)) return(NULL)
    req(input$ch2_scenario, input$ch2_n)

    if (!identical(state$scenario, input$ch2_scenario) ||
        !isTRUE(state$n == input$ch2_n)) {
      return(NULL)
    }

    state$values
  })

  # Krok widgetu (1..4) żyje w przeglądarce; nowa próba ani zmiana
  # scenariusza nie cofa kroku.
  ch2_step <- lc_step_server("ch2_test", input)$step

  # Histogram danych (kroki 1–2): rama z danych i μ₀, wspólna dla obu kroków.
  ch2_hist_plot <- function(samp, mu0, xlab, step) {
    rng <- range(c(samp, mu0))
    pad <- diff(rng) * 0.06
    breaks <- seq(min(samp), max(samp), length.out = 16)
    y_top <- max(hist(samp, breaks = breaks, plot = FALSE)$counts) * 1.3
    x_bar <- mean(samp)

    p <- ggplot(data.frame(x = samp), aes(x = x)) +
      step_result(geom_histogram, breaks = breaks) +
      labs(x = xlab, y = "Liczba")

    if (step >= 2) {
      # μ₀ znamy z hipotezy; średnia z próby jest nowa w kroku 2.
      mean_right <- x_bar >= mu0
      p <- p +
        step_line("known", xintercept = mu0) +
        step_line(step_role(step, 2), xintercept = x_bar, helper = FALSE) +
        step_symbol_label(mu0, y_top * 0.95, paste0("mu[0] == ", mu0), role = "known",
                          hjust = if (mean_right) 1.1 else -0.1, vjust = 1) +
        step_symbol_label(x_bar, y_top * 0.95, paste0("bar(x) == ", lc_fmt(x_bar, 2)),
                          role = step_role(step, 2),
                          hjust = if (mean_right) -0.1 else 1.1, vjust = 1)
    }
    p + step_frame(xlim = rng + c(-pad, pad), ylim = c(0, y_top))
  }

  # --- Panel hipotezy (zawsze widoczny) ---
  output$ch2_hypothesis_panel <- renderUI({
    par <- scenario_params[[input$ch2_scenario]]
    samp <- ch2_sample()

    div(class = "ch2-step-panel",
      lc_feedback(type = "info", style = "font-size: 16px;",
        p(tags$b("Pytanie potoczne:")),
        p(tags$em(paste0("„", par$question, "”")))
      ),
      lc_formula_box(
        p(tags$b("Hipoteza formalna (dwustronna):")),
        p(withMathJax(par$h0_text)),
        p(withMathJax(par$h1_text))
      ),
      if (is.null(samp)) {
        div(style = "text-align: center; margin: 10px 0; color: var(--upwr-reference);",
          p(tags$em("Kliknij „Losuj próbę”, żeby zebrać dane"))
        )
      }
    )
  })

  # --- Krokowy wykres ---
  zoom_plot_server("ch2_step_plot", reactive({
    samp <- ch2_sample()
    step <- ch2_step()
    par <- scenario_params[[input$ch2_scenario]]
    mu0 <- par$mu0

    if (is.null(samp)) return(NULL)

    if (step <= 2) {
      ch2_hist_plot(samp, mu0, par$xlab, step)
    } else {
      n <- length(samp)
      t_stat <- (mean(samp) - mu0) / (sd(samp) / sqrt(n))
      step_null_plot(t_stat, df = n - 1, type = "t",
                     phase = if (step == 3) "stat" else "decision")
    }
  }))

  # --- Opis kroku ---
  output$ch2_test_text <- renderUI({
    samp <- ch2_sample()
    step <- ch2_step()
    par <- scenario_params[[input$ch2_scenario]]
    mu0 <- par$mu0

    if (is.null(samp)) return(NULL)

    n <- length(samp)
    x_bar <- mean(samp)
    s <- sd(samp)
    se <- s / sqrt(n)
    t_stat <- (x_bar - mu0) / se
    p_val <- 2 * pt(-abs(t_stat), df = n - 1)

    switch(as.character(step),
      "1" = tagList(
        "Mamy próbę ", step_num(n), " obserwacji. Chcemy sprawdzić, czy średnia
        różni się od μ₀ = ", step_num(mu0), "."
      ),
      "2" = tagList(
        "x̄ = ", step_num(lc_fmt(x_bar, 2)), ", s = ", step_num(lc_fmt(s, 2)),
        ", SE = s/√n = ", step_num(lc_fmt(se, 2)), ". Różnica między x̄ a μ₀: ",
        step_num(lc_fmt(x_bar - mu0, 2)),
        ". Ale czy to dużo? Musimy to odnieść do zmienności (SE)."
      ),
      "3" = tagList(
        paste0("t = (", lc_fmt(x_bar, 2), " − ", mu0, ") / ", lc_fmt(se, 2), " = "),
        step_num(lc_fmt(t_stat, 3)), ". Statystyka t mówi: średnia z próby jest ",
        step_num(lc_fmt(abs(t_stat), 1)), " błędów standardowych od μ₀.",
        if (abs(t_stat) > 2) " To sporo!" else " To niewiele."
      ),
      "4" = step_verdict(p_val)
    )
  })

  # --- Widget 2: Test jednostronny (te same dane co Widget 1) ---
  scenario_params_1s <- list(
    concentration = list(alt = "less",
                         question = "Czy studenci mają niższą koncentrację niż norma 70 pkt?",
                         h0_text = "\\(H_0: \\mu \\geq 70\\) (koncentracja nie jest niższa)",
                         h1_text = "\\(H_a: \\mu < 70\\) (koncentracja jest niższa niż norma)"),
    water  = list(alt = "greater",
                  question = "Czy zużycie wody w gminie przekracza normę projektową 150 l/osobę?",
                  h0_text = "\\(H_0: \\mu \\leq 150\\) (zużycie nie przekracza normy)",
                  h1_text = "\\(H_a: \\mu > 150\\) (zużycie przekracza normę)"),
    yield  = list(alt = "greater",
                  question = "Czy nowa odmiana daje wyższy plon niż średnia krajowa 5 t/ha?",
                  h0_text = "\\(H_0: \\mu \\leq 5\\) (plon nie jest wyższy)",
                  h1_text = "\\(H_a: \\mu > 5\\) (plon jest wyższy niż średnia krajowa)"),
    yogurt = list(alt = "greater",
                  question = "Czy trwałość jogurtu jest dłuższa niż deklarowane 14 dni?",
                  h0_text = "\\(H_0: \\mu \\leq 14\\) (trwałość nie przekracza deklaracji)",
                  h1_text = "\\(H_a: \\mu > 14\\) (trwałość jest dłuższa niż deklarowana)"),
    noise  = list(alt = "greater",
                  question = "Czy średni poziom hałasu w hali przekracza normę NDS 85 dB?",
                  h0_text = "\\(H_0: \\mu \\leq 85\\) (hałas w normie)",
                  h1_text = "\\(H_a: \\mu > 85\\) (hałas przekracza normę)")
  )

  # Krok widgetu 2 (1..4); wspólna próba nie cofa kroku.
  ch2b_step <- lc_step_server("ch2b_test", input)$step

  # Panel hipotezy (jednostronny) — zawsze widoczny jako naglowek
  output$ch2b_hypothesis_panel <- renderUI({
    par1s <- scenario_params_1s[[input$ch2_scenario]]
    samp <- ch2_sample()

    div(class = "ch2-step-panel",
      lc_feedback(type = "info", style = "font-size: 16px;",
        p(tags$b("Pytanie potoczne (kierunkowe):")),
        p(tags$em(paste0("„", par1s$question, "”")))
      ),
      lc_formula_box(
        p(tags$b("Hipoteza formalna (jednostronna!):")),
        p(withMathJax(par1s$h0_text)),
        p(withMathJax(par1s$h1_text))
      ),
      if (is.null(samp)) {
        div(style = "text-align: center; margin: 10px 0; color: var(--upwr-reference);",
          p(tags$em("Najpierw wylosuj próbę w teście dwustronnym powyżej"))
        )
      }
    )
  })

  # Krokowy wykres (jednostronny)
  zoom_plot_server("ch2b_step_plot", reactive({
    samp <- ch2_sample()
    step <- ch2b_step()
    par <- scenario_params[[input$ch2_scenario]]
    par1s <- scenario_params_1s[[input$ch2_scenario]]
    mu0 <- par$mu0

    if (is.null(samp)) return(NULL)

    if (step <= 2) {
      ch2_hist_plot(samp, mu0, par$xlab, step)
    } else {
      n <- length(samp)
      t_stat <- (mean(samp) - mu0) / (sd(samp) / sqrt(n))
      step_null_plot(t_stat, df = n - 1, type = "t", alternative = par1s$alt,
                     phase = if (step == 3) "stat" else "decision")
    }
  }))

  # Opis kroku (jednostronny)
  output$ch2b_test_text <- renderUI({
    samp <- ch2_sample()
    step <- ch2b_step()
    par <- scenario_params[[input$ch2_scenario]]
    par1s <- scenario_params_1s[[input$ch2_scenario]]
    mu0 <- par$mu0

    if (is.null(samp)) return(NULL)

    n <- length(samp)
    x_bar <- mean(samp)
    s <- sd(samp)
    se <- s / sqrt(n)
    t_stat <- (x_bar - mu0) / se

    # p-wartosc jednostronna
    p_val <- if (par1s$alt == "less") {
      pt(t_stat, df = n - 1)
    } else {
      pt(t_stat, df = n - 1, lower.tail = FALSE)
    }

    dir_label <- if (par1s$alt == "less") "mniejsza" else "większa"

    switch(as.character(step),
      "1" = tagList(
        "n = ", step_num(n), " (te same dane co wyżej). Pytamy, czy średnia jest ",
        dir_label, " niż μ₀ = ", step_num(mu0), "."
      ),
      "2" = tagList(
        "x̄ = ", step_num(lc_fmt(x_bar, 2)), ", s = ", step_num(lc_fmt(s, 2)),
        ", SE = s/√n = ", step_num(lc_fmt(se, 2)), ", t = ",
        step_num(lc_fmt(t_stat, 3)), " (taka sama wartość). Statystyki takie same
        jak wyżej — dane się nie zmieniły. Zmieniło się tylko pytanie (kierunek)."
      ),
      "3" = tagList(
        "t = ", step_num(lc_fmt(t_stat, 3)), ". Statystyka t jest identyczna.
        Ale w teście jednostronnym patrzymy tylko na ",
        if (par1s$alt == "less") "lewy" else "prawy", " ogon rozkładu."
      ),
      "4" = tagList(
        "Jednostronnie: ", step_verdict(p_val), " ",
        tags$em("Porównaj z testem dwustronnym wyżej — te same dane, ten sam t,
          ale inna p-wartość!")
      )
    )
  })

  # --- Cwiczenia CASchools ---

  cas_vis1 <- reactiveVal(FALSE)
  cas_vis2 <- reactiveVal(FALSE)

  observeEvent(input$cas_ch2_ans1, {
    nowy <- !cas_vis1()
    cas_vis1(nowy)
    updateActionButton(session, "cas_ch2_ans1",
      label = if (nowy) "Ukryj rozwiązanie" else "Pokaż rozwiązanie")
  }, ignoreInit = TRUE)

  output$cas_ch2_sol1 <- renderUI({
    if (!cas_vis1()) return(NULL)
    r <- local({
      x <- .ch2_cas$read; mu <- 650
      n <- length(x); m <- mean(x); s <- sd(x); se <- s / sqrt(n)
      t_val <- (m - mu) / se; df <- n - 1
      p_val <- 2 * pt(-abs(t_val), df)
      d <- (m - mu) / s
      list(n = n, m = m, s = s, t = t_val, df = df, p = p_val, d = d)
    })
    div(class = "ch2-step-panel",
      lc_feedback(type = "ok", style = "margin-top: 10px;",
        p(tags$b("H₀: "), "μ_read = 650 · ", tags$b("Hₐ: "), "μ_read ≠ 650"),
        tags$ul(
          tags$li(sprintf("n = %d, x̄ = %.2f, s = %.2f", r$n, r$m, r$s)),
          tags$li(sprintf("t(%s) = %.3f, p %s %s",
            round(r$df, 1), r$t,
            if (r$p < 0.001) "<" else "=",
            if (r$p < 0.001) "0.001" else format(round(r$p, 4), nsmall = 4))),
        ),
        if (r$p < 0.05) tags$b(style = paste0("color:", upwr_accent), "Odrzucamy H₀")
        else tags$b("Brak podstaw do odrzucenia H₀"),
        p(tags$b("Interpretacja: "),
          sprintf(
            "Średni wynik (%.2f pkt) różni się istotnie od normy 650 pkt (p < 0.05).
             Różnica wynosi %.2f pkt.",
            r$m, r$m - 650
          ))
      )
    )
  })

  observeEvent(input$cas_ch2_ans2, {
    nowy <- !cas_vis2()
    cas_vis2(nowy)
    updateActionButton(session, "cas_ch2_ans2",
      label = if (nowy) "Ukryj rozwiązanie" else "Pokaż rozwiązanie")
  }, ignoreInit = TRUE)

  output$cas_ch2_sol2 <- renderUI({
    if (!cas_vis2()) return(NULL)
    r <- local({
      x <- .ch2_cas$income; mu <- 15
      n <- length(x); m <- mean(x); s <- sd(x); se <- s / sqrt(n)
      t_val <- (m - mu) / se; df <- n - 1
      p_val <- pt(t_val, df, lower.tail = FALSE)
      d <- (m - mu) / s
      list(n = n, m = m, s = s, t = t_val, df = df, p = p_val, d = d)
    })
    div(class = "ch2-step-panel",
      lc_feedback(type = "ok", style = "margin-top: 10px;",
        p(tags$b("H₀: "), "μ_income ≤ 15 · ", tags$b("Hₐ: "), "μ_income > 15"),
        tags$ul(
          tags$li(sprintf("n = %d, x̄ = %.2f, s = %.2f (tys. USD)", r$n, r$m, r$s)),
          tags$li(sprintf("t(%s) = %.3f, p %s %s (jednostronnie)",
            round(r$df, 1), r$t,
            if (r$p < 0.001) "<" else "=",
            if (r$p < 0.001) "0.001" else format(round(r$p, 4), nsmall = 4))),
        ),
        if (r$p < 0.05) tags$b(style = paste0("color:", upwr_accent), "Odrzucamy H₀")
        else tags$b("Brak podstaw do odrzucenia H₀"),
        p(tags$b("Interpretacja: "),
          sprintf(
            "Średni dochód (%.2f tys. USD) jest istotnie wyższy od 15 tys. (p < 0.05).
             Uwaga: hipotezę kierunkową formułujemy PRZED zebraniem danych — inaczej
             influjemy błąd I rodzaju.",
            r$m
          ))
      )
    )
  })
}
