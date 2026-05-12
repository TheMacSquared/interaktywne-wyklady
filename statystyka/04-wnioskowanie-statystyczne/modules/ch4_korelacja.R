# ============================================================================
# CHAPTER 5: Dwie zmienne ilosciowe (korelacja Pearsona)
# ============================================================================

ch4_ui <- list(
  id = "ch-korelacja", num = "06", title = "Korelacja",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 06 · Testowanie hipotez",
      num    = "06",
      title  = "Korelacja.",
      lead   = "„Czy wraz ze wzrostem temperatury rośnie sprzedaż lodów?” Współczynnik
                Pearsona pokazuje kierunek i siłę zależności między dwiema zmiennymi
                ilościowymi — a test istotności mówi, czy to nie przypadek."
    ),

    # ========================================================================
    # Wprowadzenie: wspolczynnik korelacji
    # ========================================================================
    lc_h2("ch4-pearson", "Współczynnik korelacji Pearsona"),

    tagList(
      p("Współczynnik korelacji Pearsona ", withMathJax("\\(r\\)"),
        " mierzy siłę i kierunek liniowego związku między dwiema zmiennymi ilościowymi."),
      p("Przyjmuje wartości od −1 do +1:"),
      tags$ul(
        tags$li(tags$b("r = +1"), " — doskonała korelacja dodatnia (wzrost jednej = wzrost drugiej)"),
        tags$li(tags$b("r = 0"), " — brak korelacji liniowej"),
        tags$li(tags$b("r = −1"), " — doskonała korelacja ujemna (wzrost jednej = spadek drugiej)")
      ),
      lc_formula_box(
        p(withMathJax("\\(r = \\frac{\\sum (x_i - \\bar{x})(y_i - \\bar{y})}{\\sqrt{\\sum(x_i-\\bar{x})^2 \\cdot \\sum(y_i-\\bar{y})^2}}\\)"))
      )
    ),

    # --- Wykres 1: kierunek korelacji ---
    tagList(
      p("Znak ", withMathJax("\\(r\\)"),
        " mówi o kierunku: dodatni oznacza, że obie zmienne rosną razem;
        ujemny — jedna rośnie, druga maleje; zero — brak trendu liniowego.")
    ),
    figure_panel(
      label = "Ryc. 6.1",
      title = "Kierunek korelacji",
      tags$img(src = "assets/correlation-direction.png",
               style = "width: 100%; border-radius: 4px;")
    ),

    # --- Wykres 2: sila korelacji (rozrzut wokol prostej) ---
    tagList(
      p("Wartość bezwzględna ", withMathJax("\\(|r|\\)"),
        " mówi o ", tags$em("sile"), " związku liniowego — czyli o tym,
        jak ciaśnie punkty grupują się wokół prostej. Trzy panele poniżej
        pokazują zbiory o coraz większej sile korelacji.")
    ),
    figure_panel(
      label = "Ryc. 6.2",
      title = "Siła korelacji",
      tags$img(src = "assets/correlation-scatter.png",
               style = "width: 100%; border-radius: 4px;")
    ),

    # --- Wykres 3: r vs nachylenie ---
    tagList(
      p("Uwaga na pułapkę: ", withMathJax("\\(r\\)"),
        " mierzy ciasność punktów wokół prostej, ale ", tags$em("nie"),
        " jej nachylenie. Trzy panele poniżej mają wyraźnie różne nachylenia,
        a mimo to ", withMathJax("\\(r\\)"),
        " jest w każdym z nich podobnie wysokie — bo punkty równie ciasno
        trzymają się prostej, niezależnie od tego, jak ostro idzie ona w górę.")
    ),
    figure_panel(
      label = "Ryc. 6.3",
      title = "r nie zależy od nachylenia",
      tags$img(src = "assets/correlation-strength.png",
               style = "width: 100%; border-radius: 4px;")
    ),

    # ========================================================================
    # WIDGET 1: Test korelacji dwustronny (krokowy)
    # ========================================================================
    lc_h2("ch4-krok", "Test korelacji — krok po kroku"),

    tagList(
      p("Korelacja z próby (", withMathJax("\\(r\\)"),
        ") prawie nigdy nie wynosi dokładnie zero, nawet gdy w populacji związku nie ma.
        Pytanie: czy obserwowane ", withMathJax("\\(r\\)"),
        " jest wystarczająco dalekie od zera, by odrzucić brak związku?"),
      p("Trzy warianty par hipotez:"),
      lc_formula_box(
        p(tags$b("Dwustronna"), " (jakikolwiek związek liniowy):"),
        p(withMathJax("\\(H_0: \\rho = 0 \\quad\\)"),
          withMathJax("\\(H_a: \\rho \\neq 0\\)"))
      ),
      lc_formula_box(
        p(tags$b("Prawostronna"), " (dodatni związek — obie rosną razem):"),
        p(withMathJax("\\(H_0: \\rho \\leq 0 \\quad\\)"),
          withMathJax("\\(H_a: \\rho > 0\\)"))
      ),
      lc_formula_box(
        p(tags$b("Lewostronna"), " (ujemny związek — jedna rośnie, druga maleje):"),
        p(withMathJax("\\(H_0: \\rho \\geq 0 \\quad\\)"),
          withMathJax("\\(H_a: \\rho < 0\\)"))
      ),
      p("Dla wszystkich trzech wariantów używamy tej samej statystyki testowej —
        transformacja ", withMathJax("\\(r\\)"),
        " na skalę rozkładu t o ", withMathJax("\\(n-2\\)"),
        " stopniach swobody. Im dalej od zera, tym bardziej nieprawdopodobny jest
        taki wynik gdy w populacji korelacji nie ma."),
      lc_formula_box(
        p("Statystyka testowa: ",
          withMathJax("\\(t = \\frac{r\\sqrt{n-2}}{\\sqrt{1-r^2}}, \\quad df = n - 2\\)"))
      )
    ),

    # ========================================================================
    # Cwiczenie: sformuluj hipotezy
    # ========================================================================
    lc_h2("ch4-cwiczenie", "Ćwiczenie: sformułuj hipotezy"),

    tagList(
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
      label = "Ryc. 6.4",
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
                       class = "lc-btn-primary", width = "100%"),
          hr(),
          h5("Kroki testu:"),
          lc_stack(gap = "sm",
            actionButton("ch4_step1", "1. Dane (wykres rozrzutu)",
                         class = "lc-btn-outline", width = "100%"),
            actionButton("ch4_step2", "2. Korelacja z próby",
                         class = "lc-btn-outline", width = "100%"),
            actionButton("ch4_step3", "3. Statystyka testowa",
                         class = "lc-btn-outline", width = "100%"),
            actionButton("ch4_step4", "4. p-wartość i decyzja",
                         class = "lc-btn-outline", width = "100%")
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
    lc_h2("ch4-jednostronny", "A jeśli znamy kierunek?"),

    tagList(
      p("Tak jak wcześniej — czasem nie pytamy „czy jest związek?”,
        ale „czy więcej X = więcej Y?” Te same dane, zmienione pytanie.")
    ),

    figure_panel(
      label = "Ryc. 6.5",
      title = "Test korelacji jednostronny",
      fluidRow(
        column(4,
          helpText("Dane: te same co w teście dwustronnym powyżej."),
          hr(),
          h5("Kroki testu:"),
          lc_stack(gap = "sm",
            actionButton("ch4b_step1", "1. Dane",
                         class = "lc-btn-outline", width = "100%"),
            actionButton("ch4b_step2", "2. Korelacja z próby",
                         class = "lc-btn-outline", width = "100%"),
            actionButton("ch4b_step3", "3. Statystyka testowa",
                         class = "lc-btn-outline", width = "100%"),
            actionButton("ch4b_step4", "4. p-wartość i decyzja",
                         class = "lc-btn-outline", width = "100%")
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
    lc_h2("ch4-pulapki", "Pułapki korelacji"),

    tagList(
      p("Współczynnik korelacji to potężne narzędzie, ale łatwo go
        źle zinterpretować. Oto cztery klasyczne pułapki:")
    ),

    # --- 1. Kwartet Anscombe'a ---
    tagList(
      p("1. Kwartet Anscombe’a — te same statystyki, różne dane."),
      p("Poniższe cztery zbiory danych mają identyczną korelację (~0,82),
        tę samą średnią i wariancję — a zupełnie inną strukturę. Tylko wykres
        pozwala odkleić statystykę od rzeczywistości. To najmocniejszy argument
        za tym, żeby zawsze rysować wykres przed interpretacją r.")
    ),
    figure_panel(
      label = "Ryc. 6.6",
      title = "Kwartet Anscombe’a",
      tags$img(src = "assets/anscombe-quartet.png",
               style = "width: 100%; border-radius: 4px;")
    ),

    # --- 2. Korelacja pozorna (spurious) ---
    tagList(
      p("2. Korelacja pozorna (spurious correlation)."),
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
    tagList(
      p("3. Paradoks Simpsona."),
      p("Globalnie: więcej nauki wydaje się obniżać wyniki (r ujemne, czarna
        linia). Ale w każdej szkole z osobna więcej nauki daje ",
        "wyższy", " wynik (r dodatnie, kolorowe linie). Jak to
        możliwe? Uczniowie słabej szkoły uczą się dużo (materiał jest dla nich
        trudniejszy), ale mimo to mają niskie wyniki. Uczniowie silnej szkoły
        uczą się mniej (materiał przychodzi łatwiej) i mają wysokie wyniki.
        Po połączeniu danych „wychodzi”, że nauka obniża wyniki."),
      p(tags$em("Zmienna ukryta:"), " poziom szkoły (konfounder). Agregacja
        danych bez uwzględnienia grup może odwrócić",
        " rzeczywisty kierunek zależności."),
      p("Więcej: ",
        tags$a(href = "https://en.wikipedia.org/wiki/Simpson%27s_paradox",
               target = "_blank", "Wikipedia →"), " | ",
        tags$a(href = "https://www.youtube.com/watch?v=ebEkn-BiW5k",
               target = "_blank", "Film TED-Ed →"))
    ),
    figure_panel(
      label = "Ryc. 6.8",
      title = "Paradoks Simpsona",
      div(class = "step-buttons",
        actionButton("ch4_simpson_global", "Spojrzenie globalne",
                     class = "lc-btn-outline"),
        actionButton("ch4_simpson_groups", "Paradoks",
                     class = "lc-btn-outline")
      ),
      plotOutput("ch4_simpson_plot", height = "420px"),
      uiOutput("ch4_simpson_caption")
    ),

    # --- 4. Nieliniowość przy r ~ 0 ---
    tagList(
      p("4. Nieliniowość przy r ≈ 0."),
      p("Zależność kwadratowa (U-kształtna) daje r bliskie zeru, choć związek
        jest silny i deterministyczny. Pearson mierzy wyłącznie zależność
        liniową — nie każdą.")
    ),
    figure_panel(
      label = "Ryc. 6.9",
      title = "Nieliniowość przy r ≈ 0",
      tags$img(src = "assets/correlation-nonlinear.png",
               style = "max-width: 500px; width: 100%; border-radius: 4px;")
    ),

    # --- 5. Outlier (widget interaktywny) ---
    tagList(
      p("5. Wpływ outliera na r."),
      p("Jeden punkt odległy od reszty może sztucznie wytworzyć korelację tam,
        gdzie jej nie ma — albo drastycznie ją zmienić. Pobaw się poniższym
        widgetem: wygeneruj dane bez korelacji, potem dodaj outliera i zobacz,
        jak r skacze.")
    ),
    figure_panel(
      label = "Ryc. 6.10",
      title = "Wpływ outliera na r",
      fluidRow(
        column(4,
          actionButton("ch4_gen_outlier", "Nowe dane (brak korelacji)",
                       class = "lc-btn-primary", width = "100%"),
          actionButton("ch4_add_outlier", "Dodaj outliera!",
                       class = "lc-btn-danger", width = "100%"),
          br(), br(),
          uiOutput("ch4_outlier_r")
        ),
        column(8,
          plotOutput("ch4_outlier_plot", height = "300px")
        )
      )
    ),

    tagList(
      p("Podsumowanie pułapek:"),
      tags$ol(
        tags$li("Zawsze rysuj wykres przed interpretacją r (Anscombe)."),
        tags$li("Korelacja nie oznacza przyczynowości — szukaj konfounderów."),
        tags$li("Agregacja danych może odwrócić kierunek zależności (Simpson)."),
        tags$li("r mierzy tylko zależność liniową (nieliniowość)."),
        tags$li("Jeden outlier może drastycznie zmienić r.")
      )
    ),

    lc_h2("ch4-cas", "Ćwiczenia", "CASchools — korelacja Pearsona"),

    lc_feedback(type = "info",
      p(tags$b("Dane: "), "420 okręgów szkolnych Kalifornii (1998–1999). Plik: ",
        tags$code("dane/caschools.csv"), "."),
      p("Zmienne w zadaniach: ", tags$code("read"), " i ", tags$code("math"),
        " (wyniki testów), ", tags$code("income"),
        " (dochód okręgu, tys. USD), ", tags$code("student_teacher_ratio"),
        " (liczba uczniów na nauczyciela).")
    ),

    figure_panel(label = "Ćwiczenie",
      h4("Zadanie 3 — Jak silnie czytanie i matematyka idą w parze?"),
      p("Oblicz korelację Pearsona między ", tags$code("read"), " i ", tags$code("math"),
        ". Zanim klikniesz: czy spodziewasz się korelacji dodatniej czy ujemnej?
        Silnej czy słabej? Zanotuj przewidywanie i sprawdź wynik."),
      actionButton("cas_ch4_ans3", "Pokaż rozwiązanie",
                   class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("cas_ch4_sol3")
    ),

    figure_panel(label = "Ćwiczenie",
      h4("Zadanie 4 — Czy zamożniejsze okręgi uczą się lepiej?"),
      p("Oblicz korelację Pearsona między ", tags$code("income"), " a ", tags$code("read"),
        ". Jaki znak ma r? Czy korelacja jest istotna? Czy możesz wyciągnąć wniosek
        przyczynowy — że wyższy dochód ", tags$em("powoduje"), " lepsze wyniki?"),
      actionButton("cas_ch4_ans4", "Pokaż rozwiązanie",
                   class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("cas_ch4_sol4")
    ),

    figure_panel(label = "Ćwiczenie",
      h4("Zadanie 5 — Czy przeładowane klasy szkodzą wynikom?"),
      p("Oblicz korelację Pearsona między ", tags$code("student_teacher_ratio"),
        " (STR) a ", tags$code("read"),
        ". Dlaczego korelacja jest ujemna? Czy jest istotna statystycznie?
        Czy silna praktycznie? Pomyśl, co może być konfunderem."),
      actionButton("cas_ch4_ans5", "Pokaż rozwiązanie",
                   class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("cas_ch4_sol5")
    ),

    lc_chapter_next(
      num       = "07",
      title     = "Test χ² niezależności",
      lead      = "związek między dwiema zmiennymi jakościowymi.",
      target_id = "ch-dwie-jakosciowe"
    )
  )
)

# ============================================================================
# DANE — CASchools (wczytane raz przy ladowaniu modulu)
# ============================================================================

.ch4_cas <- read.csv(file.path(app_dir, "dane", "caschools.csv"),
                     stringsAsFactors = FALSE)

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
      question = "Czy stężenie azotanów jest powiązane z odległością od źródła?",
      h0_text = "\\(H_0: \\rho = 0\\) (brak związku)",
      h1_text = "\\(H_a: \\rho \\neq 0\\) (jest związek)",
      question_1s = "Czy stężenie azotanów rośnie z odległością od źródła?",
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
  # Jedna probka dla testu dwustronnego i jednostronnego; po zmianie
  # scenariusza albo n stara probka nie pasuje juz do opisu pytania.
  ch4_data_state <- reactiveVal(NULL)
  ch4_data <- reactive({
    state <- ch4_data_state()
    if (is.null(state)) return(NULL)
    req(input$ch4_scenario, input$ch4_n)

    if (!identical(state$scenario, input$ch4_scenario) ||
        !isTRUE(state$n == input$ch4_n)) {
      return(NULL)
    }

    state$data
  })
  ch4_step <- reactiveVal(0)
  ch4b_step <- reactiveVal(0)

  observeEvent(input$ch4_new_sample, {
    req(input$ch4_scenario, input$ch4_n)
    par <- scenario_params[[input$ch4_scenario]]
    req(!is.null(par))
    n <- input$ch4_n
    ch4_data_state(list(
      scenario = input$ch4_scenario,
      n = n,
      data = generate_correlation_data(n, par$r_true, "linear",
                                       x_mean = par$x_mean, x_sd = par$x_sd,
                                       y_mean = par$y_mean, y_sd = par$y_sd)
    ))
    ch4_step(0)
    ch4b_step(0)
  }, ignoreInit = TRUE)

  observeEvent(list(input$ch4_scenario, input$ch4_n), {
    ch4_step(0)
    ch4b_step(0)
  }, ignoreInit = TRUE)

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
        labs(x = par$xlab, y = par$ylab) +
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
        labs(
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
        lc_stat_box("n", n,
                    caption = "par obserwacji",
                    color = col_h0),
        p("Każdy punkt to jedna obserwacja z dwiema wartościami: ",
          par$xlab, " i ", par$ylab, ". Czy widać trend?")
      ),
      "2" = tagList(
        lc_stat_box("r", round(r_val, 3), color = col_pvalue),
        p("Korelacja z próby: ", tags$b(round(r_val, 3)),
          ". Ale czy to wystarczająco daleko od zera, by odrzucić H₀?")
      ),
      "3" = tagList(
        lc_stat_box(
          "p",
          format_p_value(p_val),
          caption = paste0("t = ", round(r_val, 3), " · √", n - 2,
                           " / √(1 − ", round(r_val^2, 3),
                           ") = ", round(t_stat, 3)),
          color = col_effect
        ),
        p("Zamieniamy r na statystykę t, żeby móc porównać z rozkładem t(", n - 2, ").")
      ),
      "4" = tagList(
        lc_stat_box("p", format_p_value(p_val), color = col_pvalue),
        p(style = paste0("color: ", res$color, "; font-weight: bold; font-size: 16px;"),
          res$decision),
        p(res$explanation)
      )
    )
    lc_feedback(type = "info", info)
  })

  # =============================================
  # WIDGET 2: Jednostronny (te same dane)
  # =============================================

  output$ch4b_hypothesis_panel <- renderUI({
    par <- scenario_params[[input$ch4_scenario]]
    d <- ch4_data()
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
        labs(x = par$xlab, y = par$ylab) +
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
        labs(
             
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
        lc_stat_box("n", n, " (te same dane co wyżej)", color = col_h0),
        p("Te same obserwacje, ale pytamy o kierunek związku.")
      ),
      "2" = tagList(
        lc_stat_box("r", round(r_val, 3), " (ta sama wartość!)", color = col_pvalue),
        p("Korelacja się nie zmieniła. Zmieniło się pytanie.")
      ),
      "3" = tagList(
        lc_stat_box("t", round(t_stat, 3), " (ta sama wartość!)", color = col_effect),
        p("W teście jednostronnym patrzymy tylko na ",
          tags$b(if (par$alt_1s == "greater") "prawy" else "lewy"), " ogon.")
      ),
      "4" = tagList(
        lc_stat_box("p", format_p_value(p_val), " (jednostronnie!)", color = col_pvalue),
        p(style = paste0("color: ", res$color, "; font-weight: bold; font-size: 16px;"),
          res$decision),
        p(res$explanation),
        p(tags$em("Porównaj z testem dwustronnym wyżej — te same dane, ten sam r i t, ale inna p-wartość!"))
      )
    )
    lc_feedback(type = "info", info)
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
        labs(
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
      lc_stat_box("r", round(r_val, 3), color = col_h0),
      lc_stat_box("Outlierów", n_outliers, color = col_reject)
    )
  })

  # --- Cwiczenia CASchools ---

  .cas_cor <- function(x, y) {
    ok <- complete.cases(x, y); x <- x[ok]; y <- y[ok]
    n <- length(x); r <- cor(x, y)
    t_val <- r * sqrt((n - 2) / (1 - r^2)); df <- n - 2
    p_val <- 2 * pt(-abs(t_val), df)
    list(r = r, t = t_val, df = df, p = p_val, n = n, r2 = r^2)
  }

  cas_vis3 <- reactiveVal(FALSE)
  cas_vis4 <- reactiveVal(FALSE)
  cas_vis5 <- reactiveVal(FALSE)

  observeEvent(input$cas_ch4_ans3, {
    nowy <- !cas_vis3()
    cas_vis3(nowy)
    updateActionButton(session, "cas_ch4_ans3",
      label = if (nowy) "Ukryj rozwiązanie" else "Pokaż rozwiązanie")
  }, ignoreInit = TRUE)

  output$cas_ch4_sol3 <- renderUI({
    if (!cas_vis3()) return(NULL)
    r <- .cas_cor(.ch4_cas$read, .ch4_cas$math)
    lc_feedback(type = "ok", style = "margin-top: 10px;",
      tags$ul(
        tags$li(sprintf("r = %.3f, t(%d) = %.3f, p %s %s",
          r$r, r$df, r$t,
          if (r$p < 0.001) "<" else "=",
          if (r$p < 0.001) "0.001" else format(round(r$p, 4), nsmall = 4))),
        tags$li(sprintf("R² = %.3f → czytanie wyjaśnia %.1f%% wariancji wyników z matematyki",
                        r$r2, 100 * r$r2))
      ),
      tags$b(style = paste0("color:", upwr_accent), "Odrzucamy H₀"),
      p(tags$b("Interpretacja: "),
        sprintf("r = %.3f — korelacja silnie dodatnia.
          Okręgi z lepszymi wynikami z czytania osiągają też wyższe wyniki z matematyki
          (%.1f%% wspólnej wariancji). Obie zmienne mierzą ogólny poziom edukacji.",
          r$r, 100 * r$r2))
    )
  })

  observeEvent(input$cas_ch4_ans4, {
    nowy <- !cas_vis4()
    cas_vis4(nowy)
    updateActionButton(session, "cas_ch4_ans4",
      label = if (nowy) "Ukryj rozwiązanie" else "Pokaż rozwiązanie")
  }, ignoreInit = TRUE)

  output$cas_ch4_sol4 <- renderUI({
    if (!cas_vis4()) return(NULL)
    r <- .cas_cor(.ch4_cas$income, .ch4_cas$read)
    lc_feedback(type = "ok", style = "margin-top: 10px;",
      tags$ul(
        tags$li(sprintf("r = %.3f, t(%d) = %.3f, p %s %s",
          r$r, r$df, r$t,
          if (r$p < 0.001) "<" else "=",
          if (r$p < 0.001) "0.001" else format(round(r$p, 4), nsmall = 4))),
        tags$li(sprintf("R² = %.3f — dochód wyjaśnia %.1f%% wariancji wyników",
                        r$r2, 100 * r$r2))
      ),
      tags$b(style = paste0("color:", upwr_accent), "Odrzucamy H₀"),
      p(tags$b("Korelacja ≠ przyczynowość: "),
        "Korelacja jest istotna i dodatnia — bogatsze okręgi mają wyższe wyniki.
        Jednak nie możemy stwierdzić, że dochód ", tags$em("powoduje"),
        " lepsze wyniki. Trzecia zmienna (jakość nauczycieli, kapitał kulturowy rodziny)
        może tłumaczyć obie. Potrzeba badania eksperymentalnego lub quasi-eksperymentalnego.")
    )
  })

  observeEvent(input$cas_ch4_ans5, {
    nowy <- !cas_vis5()
    cas_vis5(nowy)
    updateActionButton(session, "cas_ch4_ans5",
      label = if (nowy) "Ukryj rozwiązanie" else "Pokaż rozwiązanie")
  }, ignoreInit = TRUE)

  output$cas_ch4_sol5 <- renderUI({
    if (!cas_vis5()) return(NULL)
    r <- .cas_cor(.ch4_cas$student_teacher_ratio, .ch4_cas$read)
    lc_feedback(type = "ok", style = "margin-top: 10px;",
      tags$ul(
        tags$li(sprintf("r = %.3f, t(%d) = %.3f, p %s %s",
          r$r, r$df, r$t,
          if (r$p < 0.001) "<" else "=",
          if (r$p < 0.001) "0.001" else format(round(r$p, 4), nsmall = 4))),
        tags$li(sprintf("R² = %.3f — STR wyjaśnia %.1f%% wariancji wyników",
                        r$r2, 100 * r$r2))
      ),
      tags$b(style = paste0("color:", upwr_accent), "Odrzucamy H₀"),
      p(tags$b("Interpretacja: "),
        sprintf("r = %.3f — korelacja ujemna: wyższy STR (więcej uczniów na nauczyciela)
          wiąże się z niższymi wynikami z czytania. STR wyjaśnia tylko %.1f%%
          wariancji. Uwaga: STR jest często proxy dla zasobności okręgu — dochód może
          być konfunderem tej zależności.",
          r$r, 100 * r$r2))
    )
  })

  # ---- Widget Paradoks Simpsona ----
  ch4_simpson_data <- local({
    set.seed(42)
    schools <- c("Szkoła słaba", "Szkoła średnia", "Szkoła silna")
    school_levels <- c(slaba = 48, srednia = 65, silna = 82)
    study_means   <- c(slaba = 24, srednia = 17, silna =  9)
    study_sd <- 4.5
    within_slope <- 1.4
    within_noise <- 7
    n_per_group <- 70

    rows <- lapply(seq_along(school_levels), function(i) {
      key <- names(school_levels)[i]
      study <- pmax(0.5, rnorm(n_per_group, mean = study_means[key], sd = study_sd))
      score <- school_levels[key] +
               within_slope * (study - mean(study)) +
               rnorm(n_per_group, 0, within_noise)
      data.frame(
        szkola  = factor(schools[i], levels = schools),
        godziny = study,
        wynik   = score
      )
    })
    do.call(rbind, rows)
  })

  ch4_simpson_view <- reactiveVal("global")
  observeEvent(input$ch4_simpson_global, ch4_simpson_view("global"))
  observeEvent(input$ch4_simpson_groups, ch4_simpson_view("groups"))

  output$ch4_simpson_plot <- renderPlot({
    df <- ch4_simpson_data
    view <- ch4_simpson_view()

    school_colors <- c(
      "Szkoła słaba"   = unname(upwr_cat["terakota"]),
      "Szkoła średnia" = unname(upwr_cat["bursztyn"]),
      "Szkoła silna"   = unname(upwr_cat["niebo"])
    )

    if (view == "global") {
      ggplot(df, aes(x = godziny, y = wynik)) +
        geom_point(color = "grey75", size = 2.4, alpha = 0.85) +
        geom_smooth(method = "lm", se = FALSE,
                    color = upwr_secondary, linewidth = 1.4) +
        labs(x = "Godziny nauki / tydzień", y = "Wynik z egzaminu") +
        theme_upwr()
    } else {
      ggplot(df, aes(x = godziny, y = wynik, color = szkola)) +
        geom_smooth(method = "lm", se = FALSE,
                    color = upwr_secondary, linewidth = 1.0,
                    linetype = "dashed", alpha = 0.5,
                    aes(group = 1)) +
        geom_point(size = 2.4, alpha = 0.85) +
        geom_smooth(method = "lm", se = FALSE, linewidth = 1.2,
                    aes(group = szkola)) +
        scale_color_manual(values = school_colors, name = NULL) +
        labs(x = "Godziny nauki / tydzień", y = "Wynik z egzaminu") +
        theme_upwr() +
        theme(legend.position = "top")
    }
  })

  output$ch4_simpson_caption <- renderUI({
    df <- ch4_simpson_data
    view <- ch4_simpson_view()
    r_global <- round(cor(df$godziny, df$wynik), 2)

    if (view == "global") {
      lc_feedback(type = "warning",
        tags$strong("Spojrzenie globalne: "),
        sprintf("r = %s. Więcej godzin nauki → niższy wynik z egzaminu? ",
                format(r_global, nsmall = 2)),
        "To wygląda na absurd — przecież nauka powinna pomagać. ",
        tags$em("Kliknij „Paradoks”, żeby zobaczyć, co tu się naprawdę dzieje.")
      )
    } else {
      r_per_school <- df %>%
        group_by(szkola) %>%
        summarise(r = cor(godziny, wynik), .groups = "drop")
      r_text <- paste0(r_per_school$szkola, ": r = ", round(r_per_school$r, 2),
                       collapse = "; ")

      lc_feedback(type = "ok",
        tags$strong("Podział na szkoły: "),
        "w każdej szkole z osobna więcej nauki → ",
        tags$b("wyższy"), " wynik (",
        r_text, "). ",
        tags$br(), tags$br(),
        "Globalnie wychodziło odwrotnie, bo ",
        tags$b("poziom szkoły"),
        " jest ukrytym konfunderem: uczniowie słabej szkoły uczą się
         więcej (materiał trudniejszy), ale i tak mają niższe wyniki niż
         uczniowie szkoły silnej. Po połączeniu grup ten efekt szkoły
         maskuje rzeczywisty pozytywny wpływ nauki w obrębie szkoły."
      )
    }
  })
}
