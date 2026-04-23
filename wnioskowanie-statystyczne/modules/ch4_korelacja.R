# ============================================================================
# CHAPTER 5: Dwie zmienne ilosciowe (korelacja Pearsona)
# ============================================================================

ch4_ui <- list(
  id = "ch-korelacja", num = "05", title = "Korelacja",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 05 · Testowanie hipotez",
      num    = "05",
      title  = "Korelacja.",
      lead   = "„Czy wraz ze wzrostem temperatury rośnie sprzedaż lodów?” Współczynnik
                Pearsona pokazuje kierunek i siłę zależności między dwiema zmiennymi
                ilościowymi — a test istotności mówi, czy to nie przypadek."
    ),

    # ========================================================================
    # Wprowadzenie: wspolczynnik korelacji
    # ========================================================================
    h2(id = "ch4-pearson", class = "section-title", "Współczynnik korelacji Pearsona"),

    div(class = "narrative",
      p("Współczynnik korelacji Pearsona ", withMathJax("\\(r\\)"),
        " mierzy siłę i kierunek liniowego związku między dwiema zmiennymi ilościowymi."),
      p("Przyjmuje wartości od ", tags$b("−1"), " do ", tags$b("+1"), ":"),
      tags$ul(
        tags$li(tags$b("r = +1"), " — doskonała korelacja dodatnia (wzrost jednej = wzrost drugiej)"),
        tags$li(tags$b("r = 0"), " — brak korelacji liniowej"),
        tags$li(tags$b("r = −1"), " — doskonała korelacja ujemna (wzrost jednej = spadek drugiej)")
      ),
      div(class = "formula-box",
        p(withMathJax("\\(r = \\frac{\\sum (x_i - \\bar{x})(y_i - \\bar{y})}{\\sqrt{\\sum(x_i-\\bar{x})^2 \\cdot \\sum(y_i-\\bar{y})^2}}\\)"))
      )
    ),

    # --- Wykres 1: sila korelacji (statyczny obrazek) ---
    div(class = "narrative",
      p("Im większe ", withMathJax("\\(|r|\\)"),
        ", tym ciaśniej punkty grupują się wokół prostej — poniżej trzy
        zbiory o różnej sile korelacji.")
    ),
    figure_panel(
      label = "Ryc. 5.1",
      title = "Siła korelacji",
      tags$img(src = "assets/correlation-strength.png",
               style = "width: 100%; border-radius: 4px;")
    ),

    # --- Wykres 2: kierunek korelacji (statyczny obrazek) ---
    div(class = "narrative",
      p("Znak ", withMathJax("\\(r\\)"),
        " mówi o kierunku: dodatni oznacza, że obie zmienne rosną razem;
        ujemny — jedna rośnie, druga maleje; zero — brak trendu liniowego.")
    ),
    figure_panel(
      label = "Ryc. 5.2",
      title = "Kierunek korelacji",
      tags$img(src = "assets/correlation-direction.png",
               style = "width: 100%; border-radius: 4px;")
    ),

    # --- Wykres 3: rozrzut vs r (statyczny obrazek) ---
    div(class = "narrative",
      p("Warto odróżnić korelację od nachylenia prostej. Wszystkie trzy
        panele poniżej mają ten sam trend wzrostowy (podobne nachylenie),
        ale im większy rozrzut punktów wokół prostej, tym niższe ",
        withMathJax("\\(r\\)"),
        ". Korelacja łączy obie cechy: kierunek trendu ", tags$em("i"),
        " to, jak ciaśno punkty go trzymają.")
    ),
    figure_panel(
      label = "Ryc. 5.3",
      title = "Uwaga: r to nie nachylenie!",
      tags$img(src = "assets/correlation-scatter.png",
               style = "width: 100%; border-radius: 4px;")
    ),

    # ========================================================================
    # WIDGET 1: Test korelacji dwustronny (krokowy)
    # ========================================================================
    h2(id = "ch4-krok", class = "section-title", "Test korelacji — krok po kroku"),

    div(class = "narrative",
      p("Korelacja z próby (", withMathJax("\\(r\\)"),
        ") prawie nigdy nie wynosi dokładnie zero, nawet gdy w populacji związku nie ma.
        Pytanie: czy obserwowane ", withMathJax("\\(r\\)"),
        " jest wystarczająco dalekie od zera, by odrzucić brak związku?"),
      p("Trzy warianty par hipotez:"),
      div(class = "formula-box",
        p(tags$b("Dwustronna"), " (jakikolwiek związek liniowy):"),
        p(withMathJax("\\(H_0: \\rho = 0 \\quad\\)"),
          withMathJax("\\(H_a: \\rho \\neq 0\\)"))
      ),
      div(class = "formula-box",
        p(tags$b("Prawostronna"), " (dodatni związek — obie rosną razem):"),
        p(withMathJax("\\(H_0: \\rho \\leq 0 \\quad\\)"),
          withMathJax("\\(H_a: \\rho > 0\\)"))
      ),
      div(class = "formula-box",
        p(tags$b("Lewostronna"), " (ujemny związek — jedna rośnie, druga maleje):"),
        p(withMathJax("\\(H_0: \\rho \\geq 0 \\quad\\)"),
          withMathJax("\\(H_a: \\rho < 0\\)"))
      ),
      p("Dla wszystkich trzech wariantów używamy tej samej statystyki testowej —
        transformacja ", withMathJax("\\(r\\)"),
        " na skalę rozkładu t o ", withMathJax("\\(n-2\\)"),
        " stopniach swobody. Im dalej od zera, tym bardziej nieprawdopodobny jest
        taki wynik gdy w populacji korelacji nie ma."),
      div(class = "formula-box",
        p("Statystyka testowa: ",
          withMathJax("\\(t = \\frac{r\\sqrt{n-2}}{\\sqrt{1-r^2}}, \\quad df = n - 2\\)"))
      )
    ),

    # ========================================================================
    # Cwiczenie: sformuluj hipotezy
    # ========================================================================
    h2(id = "ch4-cwiczenie", class = "section-title", "Ćwiczenie: sformułuj hipotezy"),

    div(class = "narrative",
      p("Jak wyglądają H₀ i Hₐ w poniższych sytuacjach? Zastanów się i sprawdź.")
    ),

    hypothesis_practice("ch4", list(
      list(
        question = "Producent lodów podejrzewa, że sprzedaż rośnie wraz
                    ze średnią temperaturą dnia. Zbiera dane z 60 dni.",
        h0 = "\\(H_0: \\rho \\leq 0\\) (brak dodatniego związku)",
        ha = "\\(H_a: \\rho > 0\\) (wyższa temperatura → wyższa sprzedaż)",
        note = "Jednostronny — pytanie jest kierunkowe („rośnie wraz z”)."
      ),
      list(
        question = "Czy istnieje jakikolwiek związek między liczbą godzin snu
                    a oceną z egzaminu?",
        h0 = "\\(H_0: \\rho = 0\\) (brak związku liniowego)",
        ha = "\\(H_a: \\rho \\neq 0\\) (jest związek)",
        note = "Dwustronny — pytamy neutralnie, bez zakładania kierunku."
      ),
      list(
        question = "Inżynier bada, czy większe stężenie dodatku X skraca
                    trwałość produktu na półce.",
        h0 = "\\(H_0: \\rho \\geq 0\\)",
        ha = "\\(H_a: \\rho < 0\\) (więcej dodatku → krótsza trwałość)",
        note = "Jednostronny (lewostronny) — hipoteza kierunkowa ujemna."
      )
    )),

    figure_panel(
      label = "Ryc. 5.4",
      title = "Test korelacji — krok po kroku",
      fluidRow(
        column(4,
          selectInput("ch4_scenario", "Scenariusz:",
            choices = c(
              "Sen a ocena z egzaminu" = "sleep_grade",
              "Azotany a odległość od źródła" = "nitrate_dist",
              "Nawadnianie a plon" = "irrigation_yield",
              "Stężenie konserwantu a trwałość" = "preserv_shelf",
              "Szkolenie BHP a wypadki (IB)" = "training_accidents"
            ),
            selected = "sleep_grade"
          ),
          sliderInput("ch4_n", "Wielkość próby (n):",
                      min = 15, max = 100, value = 40, step = 5),
          actionButton("ch4_new_sample", "Losuj próbę",
                       class = "btn-primary", width = "100%"),
          hr(),
          h5("Kroki testu:"),
          div(style = "display: flex; flex-direction: column; gap: 6px;",
            actionButton("ch4_step1", "1. Dane (wykres rozrzutu)",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch4_step2", "2. Korelacja z próby",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch4_step3", "3. Statystyka testowa",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch4_step4", "4. p-wartość i decyzja",
                         class = "btn-outline-primary", width = "100%")
          )
        ),
        column(8,
          uiOutput("ch4_hypothesis_panel"),
          plotOutput("ch4_step_plot", height = "350px"),
          uiOutput("ch4_step_info")
        )
      )
    ),

    # ========================================================================
    # WIDGET 2: Test jednostronny (te same dane)
    # ========================================================================
    h2(id = "ch4-jednostronny", class = "section-title", "A jeśli znamy kierunek?"),

    div(class = "narrative",
      p("Tak jak wcześniej — czasem nie pytamy „czy jest związek?”,
        ale „czy więcej X = więcej Y?” Te same dane, zmienione pytanie.")
    ),

    figure_panel(
      label = "Ryc. 5.5",
      title = "Test korelacji jednostronny",
      fluidRow(
        column(4,
          helpText("Dane: te same co w teście dwustronnym powyżej."),
          hr(),
          h5("Kroki testu:"),
          div(style = "display: flex; flex-direction: column; gap: 6px;",
            actionButton("ch4b_step1", "1. Dane",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch4b_step2", "2. Korelacja z próby",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch4b_step3", "3. Statystyka testowa",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch4b_step4", "4. p-wartość i decyzja",
                         class = "btn-outline-primary", width = "100%")
          )
        ),
        column(8,
          uiOutput("ch4b_hypothesis_panel"),
          plotOutput("ch4b_step_plot", height = "350px"),
          uiOutput("ch4b_step_info")
        )
      )
    ),

    # ========================================================================
    # Pulapki korelacji
    # ========================================================================
    h2(id = "ch4-pulapki", class = "section-title", "Pułapki korelacji"),

    div(class = "narrative",
      p("Współczynnik korelacji to potężne narzędzie, ale łatwo go
        źle zinterpretować. Oto cztery klasyczne pułapki:")
    ),

    # --- 1. Kwartet Anscombe'a ---
    div(class = "narrative",
      p(tags$b("1. Kwartet Anscombe’a — te same statystyki, różne dane.")),
      p("Poniższe cztery zbiory danych mają identyczną korelację (~0,82),
        tę samą średnią i wariancję — a zupełnie inną strukturę. Tylko wykres
        pozwala odkleić statystykę od rzeczywistości. To najmocniejszy argument
        za tym, żeby zawsze rysować wykres przed interpretacją r.")
    ),
    figure_panel(
      label = "Ryc. 5.6",
      title = "Kwartet Anscombe’a",
      tags$img(src = "assets/anscombe-quartet.png",
               style = "width: 100%; border-radius: 4px;")
    ),

    # --- 2. Korelacja pozorna (spurious) ---
    div(class = "narrative",
      p(tags$b("2. Korelacja pozorna (spurious correlation).")),
      p("Spożycie lodów i liczba utonięć korelują dodatnio. Czy lody zabijają?
        Oczywiście nie — obie zmienne zależą od temperatury (zmienna ukryta /
        konfounder). Korelacja między X i Y może wynikać z tego, że obie
        zależą od Z. Bez kontroli zmiennych zakłócających nie można wnioskować
        o przyczynowości."),
      p("Więcej absurdalnych przykładów: ",
        tags$a(href = "https://www.tylervigen.com/spurious-correlations",
               target = "_blank",
               "Spurious Correlations (Tyler Vigen) →"))
    ),

    # --- 3. Paradoks Simpsona ---
    div(class = "narrative",
      p(tags$b("3. Paradoks Simpsona.")),
      p("Globalnie: więcej nauki wydaje się obniżać wyniki (r ujemne, czarna
        linia). Ale w każdej szkole z osobna więcej nauki daje ",
        tags$b("wyższy"), " wynik (r dodatnie, kolorowe linie). Jak to
        możliwe? Uczniowie słabej szkoły uczą się dużo (materiał jest dla nich
        trudniejszy), ale mimo to mają niskie wyniki. Uczniowie silnej szkoły
        uczą się mniej (materiał przychodzi łatwiej) i mają wysokie wyniki.
        Po połączeniu danych „wychodzi”, że nauka obniża wyniki."),
      p(tags$em("Zmienna ukryta:"), " poziom szkoły (konfounder). Agregacja
        danych bez uwzględnienia grup może ", tags$b("odwrócić"),
        " rzeczywisty kierunek zależności."),
      p("Więcej: ",
        tags$a(href = "https://en.wikipedia.org/wiki/Simpson%27s_paradox",
               target = "_blank", "Wikipedia →"), " | ",
        tags$a(href = "https://www.youtube.com/watch?v=ebEkn-BiW5k",
               target = "_blank", "Film TED-Ed →"))
    ),
    figure_panel(
      label = "Ryc. 5.8",
      title = "Paradoks Simpsona",
      tags$img(src = "assets/simpson-paradox.png",
               style = "width: 100%; max-width: 650px; border-radius: 4px;")
    ),

    # --- 4. Nieliniowość przy r ~ 0 ---
    div(class = "narrative",
      p(tags$b("4. Nieliniowość przy r ≈ 0.")),
      p("Zależność kwadratowa (U-kształtna) daje r bliskie zeru, choć związek
        jest silny i deterministyczny. Pearson mierzy wyłącznie zależność
        liniową — nie każdą.")
    ),
    figure_panel(
      label = "Ryc. 5.9",
      title = "Nieliniowość przy r ≈ 0",
      tags$img(src = "assets/correlation-nonlinear.png",
               style = "max-width: 500px; width: 100%; border-radius: 4px;")
    ),

    # --- 5. Outlier (widget interaktywny) ---
    div(class = "narrative",
      p(tags$b("5. Wpływ outliera na r.")),
      p("Jeden punkt odległy od reszty może sztucznie wytworzyć korelację tam,
        gdzie jej nie ma — albo drastycznie ją zmienić. Pobaw się poniższym
        widgetem: wygeneruj dane bez korelacji, potem dodaj outliera i zobacz,
        jak r skacze.")
    ),
    figure_panel(
      label = "Ryc. 5.10",
      title = "Wpływ outliera na r",
      fluidRow(
        column(4,
          actionButton("ch4_gen_outlier", "Nowe dane (brak korelacji)",
                       class = "btn-primary", width = "100%"),
          actionButton("ch4_add_outlier", "Dodaj outliera!",
                       class = "btn-danger", width = "100%"),
          br(), br(),
          uiOutput("ch4_outlier_r")
        ),
        column(8,
          plotOutput("ch4_outlier_plot", height = "300px")
        )
      )
    ),

    div(class = "narrative",
      p(tags$b("Podsumowanie pułapek:")),
      tags$ol(
        tags$li("Zawsze rysuj wykres przed interpretacją r (Anscombe)."),
        tags$li("Korelacja nie oznacza przyczynowości — szukaj konfounderów."),
        tags$li("Agregacja danych może odwrócić kierunek zależności (Simpson)."),
        tags$li("r mierzy tylko zależność liniową (nieliniowość)."),
        tags$li("Jeden outlier może drastycznie zmienić r.")
      )
    ),

    lc_chapter_next(
      num       = "06",
      title     = "Test χ² niezależności",
      lead      = "związek między dwiema zmiennymi jakościowymi.",
      target_id = "ch-dwie-jakosciowe"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch4_server <- function(input, output, session) {

  # --- Parametry scenariuszy ---
  scenario_params <- list(
    sleep_grade = list(
      r_true = 0.45, xlab = "Godziny snu", ylab = "Ocena z egzaminu",
      x_mean = 7,   x_sd = 1.5,
      y_mean = 70,  y_sd = 12,
      title = "Sen a ocena",
      question = "Czy istnieje związek między ilością snu a oceną z egzaminu?",
      h0_text = "\\(H_0: \\rho = 0\\) (brak związku liniowego)",
      h1_text = "\\(H_a: \\rho \\neq 0\\) (jest związek)",
      question_1s = "Czy więcej snu wiąże się z wyższą oceną?",
      h0_text_1s = "\\(H_0: \\rho \\leq 0\\)",
      h1_text_1s = "\\(H_a: \\rho > 0\\)",
      alt_1s = "greater"),
    nitrate_dist = list(
      r_true = 0.55, xlab = "Odległość od źródła (km)", ylab = "Stężenie azotanów (mg/l)",
      x_mean = 15,  x_sd = 8,
      y_mean = 30,  y_sd = 12,
      title = "Azotany wzdłuż rzeki",
      question = "Czy stężenie azotanow jest powiązane z odległością od źródła?",
      h0_text = "\\(H_0: \\rho = 0\\) (brak związku)",
      h1_text = "\\(H_a: \\rho \\neq 0\\) (jest związek)",
      question_1s = "Czy stężenie azotanow rośnie z odległością od źródła?",
      h0_text_1s = "\\(H_0: \\rho \\leq 0\\)",
      h1_text_1s = "\\(H_a: \\rho > 0\\)",
      alt_1s = "greater"),
    irrigation_yield = list(
      r_true = 0.60, xlab = "Nawadnianie (mm/tydzień)", ylab = "Plon (t/ha)",
      x_mean = 25,  x_sd = 10,
      y_mean = 6,   y_sd = 1.5,
      title = "Nawadnianie a plon",
      question = "Czy ilość nawadniania jest powiązana z plonem?",
      h0_text = "\\(H_0: \\rho = 0\\) (brak związku)",
      h1_text = "\\(H_a: \\rho \\neq 0\\) (jest związek)",
      question_1s = "Czy większe nawadnianie daje wyższe plony?",
      h0_text_1s = "\\(H_0: \\rho \\leq 0\\)",
      h1_text_1s = "\\(H_a: \\rho > 0\\)",
      alt_1s = "greater"),
    preserv_shelf = list(
      r_true = 0.50, xlab = "Stężenie konserwantu (mg/kg)", ylab = "Trwałość (dni)",
      x_mean = 200, x_sd = 60,
      y_mean = 30,  y_sd = 8,
      title = "Konserwant a trwałość",
      question = "Czy stężenie konserwantu wpływa na trwałość produktu?",
      h0_text = "\\(H_0: \\rho = 0\\) (brak związku)",
      h1_text = "\\(H_a: \\rho \\neq 0\\) (jest związek)",
      question_1s = "Czy większe stężenie konserwantu wydłuża trwałość?",
      h0_text_1s = "\\(H_0: \\rho \\leq 0\\)",
      h1_text_1s = "\\(H_a: \\rho > 0\\)",
      alt_1s = "greater"),
    training_accidents = list(
      r_true = -0.55,
      xlab = "Godziny szkolenia BHP / rok",
      ylab = "Liczba wypadków / 100 prac. / rok",
      x_mean = 20,  x_sd = 7,
      y_mean = 8,   y_sd = 3,
      title = "Szkolenie BHP a wypadki",
      question = "Czy liczba godzin szkolenia BHP wiąże się z liczbą wypadków?",
      h0_text = "\\(H_0: \\rho = 0\\) (brak związku)",
      h1_text = "\\(H_a: \\rho \\neq 0\\) (jest związek)",
      question_1s = "Czy więcej godzin szkolenia BHP wiąże się z mniejszą liczbą wypadków?",
      h0_text_1s = "\\(H_0: \\rho \\geq 0\\)",
      h1_text_1s = "\\(H_a: \\rho < 0\\)",
      alt_1s = "less")
  )

  # --- Wspoldzielone dane ---
  ch4_data <- reactiveVal(NULL)
  ch4_step <- reactiveVal(0)
  ch4b_step <- reactiveVal(0)

  observeEvent(input$ch4_new_sample, {
    par <- scenario_params[[input$ch4_scenario]]
    n <- input$ch4_n
    ch4_data(generate_correlation_data(n, par$r_true, "linear",
                                       x_mean = par$x_mean, x_sd = par$x_sd,
                                       y_mean = par$y_mean, y_sd = par$y_sd))
    ch4_step(0)
    ch4b_step(0)
  })

  observeEvent(input$ch4_scenario, {
    ch4_data(NULL)
    ch4_step(0)
    ch4b_step(0)
  })

  observeEvent(input$ch4_step1, ch4_step(1))
  observeEvent(input$ch4_step2, ch4_step(2))
  observeEvent(input$ch4_step3, ch4_step(3))
  observeEvent(input$ch4_step4, ch4_step(4))

  observeEvent(input$ch4b_step1, ch4b_step(1))
  observeEvent(input$ch4b_step2, ch4b_step(2))
  observeEvent(input$ch4b_step3, ch4b_step(3))
  observeEvent(input$ch4b_step4, ch4b_step(4))

  # =============================================
  # WIDGET 1: Test dwustronny
  # =============================================

  output$ch4_hypothesis_panel <- renderUI({
    par <- scenario_params[[input$ch4_scenario]]
    d <- ch4_data()
    tagList(
      div(class = "callout-info", style = "font-size: 16px;",
        p(tags$b("Pytanie potoczne:")),
        p(tags$em(paste0("„", par$question, "”")))
      ),
      div(class = "formula-box",
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

  output$ch4_step_plot <- renderPlot({
    d <- ch4_data()
    step <- ch4_step()
    par <- scenario_params[[input$ch4_scenario]]

    if (is.null(d)) return(NULL)

    if (step == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Próba gotowa! Klikaj kroki po kolei.",
                 size = 5, color = upwr_reference) +
        theme_void()
    } else if (step <= 2) {
      p <- ggplot(d, aes(x = x, y = y)) +
        geom_point(color = col_h0, alpha = 0.6, size = 2.5) +
        labs(title = par$title, x = par$xlab, y = par$ylab) +
        theme()

      if (step >= 2) {
        p <- p + geom_smooth(method = "lm", se = FALSE,
                             color = col_reject, linewidth = 1)
      }
      p
    } else if (step == 3) {
      # Rozklad t bez zacienionego pola
      n <- nrow(d)
      r_val <- cor(d$x, d$y)
      t_stat <- r_val * sqrt(n - 2) / sqrt(1 - r_val^2)

      x_seq <- seq(-4, 4, length.out = 500)
      y_seq <- dt(x_seq, df = n - 2)
      plot_df <- data.frame(x = x_seq, y = y_seq)

      ggplot(plot_df, aes(x = x, y = y)) +
        geom_line(color = col_h0, linewidth = 1.2) +
        geom_vline(xintercept = t_stat, color = col_reject,
                   linewidth = 1.2, linetype = "dashed") +
        annotate("text", x = t_stat, y = max(y_seq) * 0.9,
                 label = paste0("t = ", round(t_stat, 3)),
                 hjust = if (t_stat > 0) -0.1 else 1.1,
                 color = col_reject, fontface = "bold") +
        labs(title = paste0("Rozkład pod H0: t(", n - 2, ")"),
             x = "Statystyka testowa", y = "Gęstość") +
        theme()
    } else {
      n <- nrow(d)
      r_val <- cor(d$x, d$y)
      t_stat <- r_val * sqrt(n - 2) / sqrt(1 - r_val^2)
      plot_test_distribution(t_stat, df = n - 2, test_type = "t")
    }
  })

  output$ch4_step_info <- renderUI({
    d <- ch4_data()
    step <- ch4_step()
    par <- scenario_params[[input$ch4_scenario]]

    if (is.null(d) || step == 0) return(NULL)

    n <- nrow(d)
    r_val <- cor(d$x, d$y)
    t_stat <- r_val * sqrt(n - 2) / sqrt(1 - r_val^2)
    p_val <- 2 * pt(-abs(t_stat), df = n - 2)
    res <- format_test_result(p_val)

    info <- switch(as.character(step),
      "1" = tagList(
        div(class = "stat-box", style = paste0("border-left-color:", col_h0, ";"),
            paste0("n = ", n, " par obserwacji")),
        p("Każdy punkt to jedna obserwacja z dwiema wartościami: ",
          par$xlab, " i ", par$ylab, ". Czy widać trend?")
      ),
      "2" = tagList(
        div(class = "stat-box", style = paste0("border-left-color:", col_pvalue, ";"),
            paste0("r = ", round(r_val, 3))),
        p("Korelacja z próby: ", tags$b(round(r_val, 3)),
          ". Ale czy to wystarczająco daleko od zera, by odrzucić H₀?")
      ),
      "3" = tagList(
        div(class = "stat-box", style = paste0("border-left-color:", col_effect, ";"),
            paste0("t = ", round(r_val, 3), " · √", n - 2,
                   " / √(1 − ", round(r_val^2, 3),
                   ") = ", round(t_stat, 3))),
        p("Zamieniamy r na statystykę t, żeby móc porównać z rozkładem t(", n - 2, ").")
      ),
      "4" = tagList(
        div(class = "stat-box", style = paste0("border-left-color:", col_pvalue, ";"),
            paste0("p = ", format.pval(p_val, digits = 4))),
        p(style = paste0("color: ", res$color, "; font-weight: bold; font-size: 16px;"),
          res$decision),
        p(res$explanation)
      )
    )
    div(class = "callout-info", info)
  })

  # =============================================
  # WIDGET 2: Jednostronny (te same dane)
  # =============================================

  output$ch4b_hypothesis_panel <- renderUI({
    par <- scenario_params[[input$ch4_scenario]]
    d <- ch4_data()
    tagList(
      div(class = "callout-info", style = "font-size: 16px;",
        p(tags$b("Pytanie potoczne (kierunkowe):")),
        p(tags$em(paste0("„", par$question_1s, "”")))
      ),
      div(class = "formula-box",
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

  output$ch4b_step_plot <- renderPlot({
    d <- ch4_data()
    step <- ch4b_step()
    par <- scenario_params[[input$ch4_scenario]]

    if (is.null(d) || step == 0) return(NULL)

    n <- nrow(d)
    r_val <- cor(d$x, d$y)
    t_stat <- r_val * sqrt(n - 2) / sqrt(1 - r_val^2)

    if (step <= 2) {
      p <- ggplot(d, aes(x = x, y = y)) +
        geom_point(color = col_h0, alpha = 0.6, size = 2.5) +
        labs(title = par$title, x = par$xlab, y = par$ylab) +
        theme()
      if (step >= 2) {
        p <- p + geom_smooth(method = "lm", se = FALSE,
                             color = col_reject, linewidth = 1)
      }
      p
    } else if (step == 3) {
      x_seq <- seq(-4, 4, length.out = 500)
      y_seq <- dt(x_seq, df = n - 2)
      plot_df <- data.frame(x = x_seq, y = y_seq)

      ggplot(plot_df, aes(x = x, y = y)) +
        geom_line(color = col_h0, linewidth = 1.2) +
        geom_vline(xintercept = t_stat, color = col_reject,
                   linewidth = 1.2, linetype = "dashed") +
        annotate("text", x = t_stat, y = max(y_seq) * 0.9,
                 label = paste0("t = ", round(t_stat, 3)),
                 hjust = if (t_stat > 0) -0.1 else 1.1,
                 color = col_reject, fontface = "bold") +
        labs(title = paste0("Rozkład pod H0: t(", n - 2, ")"),
             subtitle = "Test jednostronny — tylko jeden ogon!",
             x = "Statystyka testowa", y = "Gęstość") +
        theme()
    } else {
      plot_test_distribution(t_stat, df = n - 2, test_type = "t",
                             alternative = par$alt_1s)
    }
  })

  output$ch4b_step_info <- renderUI({
    d <- ch4_data()
    step <- ch4b_step()
    par <- scenario_params[[input$ch4_scenario]]

    if (is.null(d) || step == 0) return(NULL)

    n <- nrow(d)
    r_val <- cor(d$x, d$y)
    t_stat <- r_val * sqrt(n - 2) / sqrt(1 - r_val^2)
    p_val <- pt(t_stat, df = n - 2, lower.tail = (par$alt_1s == "less"))
    res <- format_test_result(p_val)

    info <- switch(as.character(step),
      "1" = tagList(
        div(class = "stat-box", style = paste0("border-left-color:", col_h0, ";"),
            paste0("n = ", n, " (te same dane co wyżej)")),
        p("Te same obserwacje, ale pytamy o kierunek związku.")
      ),
      "2" = tagList(
        div(class = "stat-box", style = paste0("border-left-color:", col_pvalue, ";"),
            paste0("r = ", round(r_val, 3), " (ta sama wartość!)")),
        p("Korelacja się nie zmieniła. Zmieniło się pytanie.")
      ),
      "3" = tagList(
        div(class = "stat-box", style = paste0("border-left-color:", col_effect, ";"),
            paste0("t = ", round(t_stat, 3), " (ta sama wartość!)")),
        p("W teście jednostronnym patrzymy tylko na ",
          tags$b(if (par$alt_1s == "greater") "prawy" else "lewy"), " ogon.")
      ),
      "4" = tagList(
        div(class = "stat-box", style = paste0("border-left-color:", col_pvalue, ";"),
            paste0("p = ", format.pval(p_val, digits = 4), " (jednostronnie!)")),
        p(style = paste0("color: ", res$color, "; font-weight: bold; font-size: 16px;"),
          res$decision),
        p(res$explanation),
        p(tags$em("Porównaj z testem dwustronnym wyżej — te same dane, ten sam r i t, ale inna p-wartość!"))
      )
    )
    div(class = "callout-info", info)
  })

  # =============================================
  # Pulapka 5: Outlier
  # =============================================
  ch4_outlier_data <- reactiveVal(NULL)

  observeEvent(input$ch4_gen_outlier, {
    ch4_outlier_data(generate_correlation_data(50, 0, "none"))
  })

  observeEvent(input$ch4_add_outlier, {
    df <- ch4_outlier_data()
    if (is.null(df)) return()
    outlier <- data.frame(x = max(df$x) + 15, y = max(df$y) + 15)
    ch4_outlier_data(rbind(df, outlier))
  })

  output$ch4_outlier_plot <- renderPlot({
    df <- ch4_outlier_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij „Nowe dane”",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      n_base <- 50
      r_val <- cor(df$x, df$y)

      ggplot(df, aes(x = x, y = y)) +
        geom_point(color = ifelse(seq_len(nrow(df)) > n_base, col_reject, col_h0),
                   size = ifelse(seq_len(nrow(df)) > n_base, 4, 2.5),
                   alpha = 0.7) +
        geom_smooth(method = "lm", se = FALSE, color = col_reject, alpha = 0.5) +
        labs(title = paste0("r = ", round(r_val, 3)),
             x = "X", y = "Y") +
        theme()
    }
  })

  output$ch4_outlier_r <- renderUI({
    df <- ch4_outlier_data()
    if (is.null(df)) return(NULL)
    r_val <- cor(df$x, df$y)
    n_outliers <- max(0, nrow(df) - 50)
    tagList(
      div(class = "stat-box", style = paste0("border-left-color:", col_h0, ";"),
          paste0("r = ", round(r_val, 3))),
      div(class = "stat-box", style = paste0("border-left-color:", col_reject, ";"),
          paste0("Outlierów: ", n_outliers))
    )
  })
}
