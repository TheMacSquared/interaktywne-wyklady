# ============================================================================
# CHAPTER 5: Dwie zmienne ilosciowe (korelacja Pearsona)
# ============================================================================

ch4_ui <- tabPanel("5. Dwie zmienne ilościowe",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Dotychczas badaliśmy jedną zmienną.
       Teraz pytamy: czy dwie zmienne ilościowe są ze sobą powiązane?"
    ),

    # ========================================================================
    # Wprowadzenie: wspolczynnik korelacji
    # ========================================================================
    div(class = "section-title", "Współczynnik korelacji Pearsona"),

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
    div(class = "widget-block",
      h4("Siła korelacji"),
      tags$img(src = "assets/correlation-strength.png",
               style = "width: 100%; border-radius: 4px;"),
      p(class = "narrative", style = "margin-top: 8px;",
        "Im większe ", withMathJax("\\(|r|\\)"),
        ", tym ciaśniej punkty grupują się wokół prostej.")
    ),

    # --- Wykres 2: kierunek korelacji (statyczny obrazek) ---
    div(class = "widget-block",
      h4("Kierunek korelacji"),
      tags$img(src = "assets/correlation-direction.png",
               style = "width: 100%; border-radius: 4px;"),
      p(class = "narrative", style = "margin-top: 8px;",
        "Znak ", withMathJax("\\(r\\)"),
        " mówi o kierunku: dodatni = obie rosną razem, ujemny = jedna rośnie, druga maleje,
        zero = brak trendu liniowego.")
    ),

    # --- Wykres 3: rozrzut vs r (statyczny obrazek) ---
    div(class = "widget-block",
      h4("Uwaga: r to nie nachylenie!"),
      tags$img(src = "assets/correlation-scatter.png",
               style = "width: 100%; border-radius: 4px;"),
      p(class = "narrative", style = "margin-top: 8px;",
        "Wszystkie trzy panele mają ten sam trend wzrostowy (podobne nachylenie prostej).
        Ale im większy rozrzut punktów wokół prostej, tym niższe r.
        Korelacja łączy obie cechy: kierunek trendu ", tags$em("i"), " to, jak ciaśno punkty go trzymają.")
    ),

    # ========================================================================
    # WIDGET 1: Test korelacji dwustronny (krokowy)
    # ========================================================================
    div(class = "section-title", "Test korelacji — krok po kroku"),

    div(class = "narrative",
      p("Korelacja z próby (", withMathJax("\\(r\\)"),
        ") prawie nigdy nie wynosi dokładnie zero, nawet gdy w populacji związku nie ma.
        Pytanie: czy obserwowane ", withMathJax("\\(r\\)"),
        " jest wystarczająco dalekie od zera, by odrzucić brak związku?"),
      div(class = "formula-box",
        p(withMathJax("\\(H_0: \\rho = 0\\)"), " — ",
          withMathJax("\\(H_a: \\rho \\neq 0\\)")),
        p(withMathJax("\\(t = \\frac{r\\sqrt{n-2}}{\\sqrt{1-r^2}}, \\quad df = n - 2\\)"))
      )
    ),

    div(class = "widget-block",
      fluidRow(
        column(4,
          selectInput("ch4_scenario", "Scenariusz:",
            choices = c(
              "Sen a ocena z egzaminu" = "sleep_grade",
              "Azotany a odległość od źródła" = "nitrate_dist",
              "Nawadnianie a plon" = "irrigation_yield",
              "Stężenie konserwantu a trwałość" = "preserv_shelf"
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
    div(class = "section-title", "A jeśli znamy kierunek?"),

    div(class = "narrative",
      p("Tak jak wcześniej — czasem nie pytamy „czy jest związek?”,
        ale „czy więcej X = więcej Y?” Te same dane, zmienione pytanie.")
    ),

    div(class = "widget-block",
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
    div(class = "section-title", "Pułapki korelacji"),

    div(class = "narrative",
      p("Współczynnik korelacji to potężne narzędzie, ale łatwo go
        źle zinterpretować. Oto cztery klasyczne pułapki:")
    ),

    # --- 1. Kwartet Anscombe'a ---
    div(class = "widget-block",
      h4("1. Kwartet Anscombe’a — te same statystyki, różne dane"),
      tags$img(src = "assets/anscombe-quartet.png",
               style = "width: 100%; border-radius: 4px;"),
      p(class = "narrative", style = "margin-top: 8px;",
        "Cztery zbiory danych z identyczną korelacją (~0.82),
        tą samą średnią i wariancją — ale zupełnie inną strukturą.
        Tylko wykres pozwala odkleić statystykę od rzeczywistości.
        To argument za tym, żeby zawsze rysować wykres przed interpretacją r.")
    ),

    # --- 2. Korelacja pozorna (spurious) ---
    div(class = "widget-block",
      h4("2. Korelacja pozorna (spurious correlation)"),
      div(class = "narrative",
        p("Spożycie lodów i liczba utonięć korelują dodatnio.
          Czy lody zabijają? Oczywiście nie — obie zmienne zależą od ",
          "temperatury (zmienna ukryta / konfounder)."),
        p("Korelacja między X i Y może wynikać z tego,
          że obie zależą od Z. Bez kontroli zmiennych zakłócających
          nie można wnioskować o przyczynowości."),
        p("Więcej absurdalnych przykładów:"),
        p(tags$a(href = "https://www.tylervigen.com/spurious-correlations",
                 target = "_blank", style = "font-size: 16px;",
                 "Spurious Correlations (Tyler Vigen) →"))
      )
    ),

    # --- 3. Paradoks Simpsona ---
    div(class = "widget-block",
      h4("3. Paradoks Simpsona"),
      tags$img(src = "assets/simpson-paradox.png",
               style = "width: 100%; max-width: 650px; border-radius: 4px;"),
      div(class = "narrative", style = "margin-top: 8px;",
        p(tags$b("Czarna linia"), " (globalna): więcej nauki → ",
          tags$b("niższy"), " wynik (r = −0.75). Nauka szkodzi?!"),
        p(tags$b("Kolorowe linie"), " (per szkoła): w każdej szkole z osobna więcej nauki → ",
          tags$b("wyższy"), " wynik (r dodatnie). Nauka pomaga!"),
        p("Jak to możliwe? Uczniowie słabej szkoły uczą się dużo
          (materiał jest dla nich trudniejszy), ale mimo to mają niskie wyniki.
          Uczniowie silnej szkoły uczą się mniej (materiał przychodzi łatwiej)
          i mają wysokie wyniki. Po połączeniu danych „wychodzi”, że nauka obniża wyniki."),
        p(tags$em("Zmienna ukryta:"), " poziom szkoły (konfounder).",
          " Agregacja danych maskuje rzeczywisty kierunek zależności."),
        p(tags$a(href = "https://en.wikipedia.org/wiki/Simpson%27s_paradox",
                 target = "_blank",
                 "Więcej: Wikipedia →"), " | ",
          tags$a(href = "https://www.youtube.com/watch?v=ebEkn-BiW5k",
                 target = "_blank",
                 "Film TED-Ed →"))
      )
    ),

    # --- 4. Nieliniowość przy r ~ 0 ---
    div(class = "widget-block",
      h4("4. Nieliniowość przy r ≈ 0"),
      tags$img(src = "assets/correlation-nonlinear.png",
               style = "max-width: 500px; width: 100%; border-radius: 4px;"),
      p(class = "narrative", style = "margin-top: 8px;",
        "Zależność kwadratowa (U-kształtna) daje r bliskie zeru,
        choć związek jest silny i deterministyczny.
        Pearson mierzy wyłącznie zależność liniową — nie każdą.")
    ),

    # --- 5. Outlier (widget interaktywny) ---
    div(class = "widget-block",
      h4("5. Wpływ outliery na r"),
      div(class = "narrative",
        p("Jeden punkt odległy od reszty może ",
          "sztucznie wytworzyć korelację tam, gdzie jej nie ma.")
      ),
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

    div(class = "callout-danger",
      tags$strong("Podsumowanie pułapek:"),
      tags$ol(
        tags$li("Zawsze rysuj wykres przed interpretacją r (Anscombe)"),
        tags$li("Korelacja nie oznacza przyczynowości — szukaj konfounderów"),
        tags$li("Agregacja danych może odwrócić kierunek zależności (Simpson)"),
        tags$li("r mierzy tylko zależność liniową (nieliniowość)"),
        tags$li("Jeden outlier może drastycznie zmienić r")
      )
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: związek między dwiema zmiennymi jakościowymi"),
      actionButton("ch4_next", "Dalej → 6. Dwie zmienne jakościowe",
                   class = "btn-primary btn-lg")
    )
  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch4_server <- function(input, output, session) {

  # --- Parametry scenariuszy ---
  scenario_params <- list(
    sleep_grade = list(
      r_true = 0.45, xlab = "Godziny snu", ylab = "Ocena z egzaminu",
      title = "Sen a ocena",
      question = "Czy istnieje związek między ilością snu a oceną z egzaminu?",
      h0_text = "\\(H_0: \\rho = 0\\) (brak związku liniowego)",
      h1_text = "\\(H_a: \\rho \\neq 0\\) (jest związek)",
      question_1s = "Czy więcej snu wiąże się z wyższą oceną?",
      h0_text_1s = "\\(H_0: \\rho \\leq 0\\)",
      h1_text_1s = "\\(H_a: \\rho > 0\\)",
      alt_1s = "greater"),
    nitrate_dist = list(
      r_true = 0.55, xlab = "Odległość od źródła (km)", ylab = "NO₃ (mg/l)",
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
      title = "Konserwant a trwałość",
      question = "Czy stężenie konserwantu wpływa na trwałość produktu?",
      h0_text = "\\(H_0: \\rho = 0\\) (brak związku)",
      h1_text = "\\(H_a: \\rho \\neq 0\\) (jest związek)",
      question_1s = "Czy większe stężenie konserwantu wydłuża trwałość?",
      h0_text_1s = "\\(H_0: \\rho \\leq 0\\)",
      h1_text_1s = "\\(H_a: \\rho > 0\\)",
      alt_1s = "greater")
  )

  # --- Wspoldzielone dane ---
  ch4_data <- reactiveVal(NULL)
  ch4_step <- reactiveVal(0)
  ch4b_step <- reactiveVal(0)

  observeEvent(input$ch4_new_sample, {
    par <- scenario_params[[input$ch4_scenario]]
    n <- input$ch4_n
    ch4_data(generate_correlation_data(n, par$r_true, "linear"))
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
        theme_educational()

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
        labs(title = paste0("Rozkład pod H₀: t(", n - 2, ")"),
             x = "Statystyka testowa", y = "Gęstość") +
        theme_educational()
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
        div(class = "stat-box", style = paste0("background:", col_h0, ";"),
            paste0("n = ", n, " par obserwacji")),
        p("Każdy punkt to jedna obserwacja z dwiema wartościami: ",
          par$xlab, " i ", par$ylab, ". Czy widać trend?")
      ),
      "2" = tagList(
        div(class = "stat-box", style = paste0("background:", col_pvalue, ";"),
            paste0("r = ", round(r_val, 3))),
        p("Korelacja z próby: ", tags$b(round(r_val, 3)),
          ". Ale czy to wystarczająco daleko od zera, by odrzucić H₀?")
      ),
      "3" = tagList(
        div(class = "stat-box", style = paste0("background:", col_effect, ";"),
            paste0("t = ", round(r_val, 3), " · √", n - 2,
                   " / √(1 − ", round(r_val^2, 3),
                   ") = ", round(t_stat, 3))),
        p("Zamieniamy r na statystykę t, żeby móc porównać z rozkładem t(", n - 2, ").")
      ),
      "4" = tagList(
        div(class = "stat-box", style = paste0("background:", col_pvalue, ";"),
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
        theme_educational()
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
        labs(title = paste0("Rozkład pod H₀: t(", n - 2, ")"),
             subtitle = "Test jednostronny — tylko jeden ogon!",
             x = "Statystyka testowa", y = "Gęstość") +
        theme_educational()
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
        div(class = "stat-box", style = paste0("background:", col_h0, ";"),
            paste0("n = ", n, " (te same dane co wyżej)")),
        p("Te same obserwacje, ale pytamy o kierunek związku.")
      ),
      "2" = tagList(
        div(class = "stat-box", style = paste0("background:", col_pvalue, ";"),
            paste0("r = ", round(r_val, 3), " (ta sama wartość!)")),
        p("Korelacja się nie zmieniła. Zmieniło się pytanie.")
      ),
      "3" = tagList(
        div(class = "stat-box", style = paste0("background:", col_effect, ";"),
            paste0("t = ", round(t_stat, 3), " (ta sama wartość!)")),
        p("W teście jednostronnym patrzymy tylko na ",
          tags$b(if (par$alt_1s == "greater") "prawy" else "lewy"), " ogon.")
      ),
      "4" = tagList(
        div(class = "stat-box", style = paste0("background:", col_pvalue, ";"),
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
        theme_educational()
    }
  })

  output$ch4_outlier_r <- renderUI({
    df <- ch4_outlier_data()
    if (is.null(df)) return(NULL)
    r_val <- cor(df$x, df$y)
    n_outliers <- max(0, nrow(df) - 50)
    tagList(
      div(class = "stat-box", style = paste0("background:", col_h0, ";"),
          paste0("r = ", round(r_val, 3))),
      div(class = "stat-box", style = paste0("background:", col_reject, ";"),
          paste0("Outlierów: ", n_outliers))
    )
  })
}
