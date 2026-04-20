# ============================================================================
# CHAPTER 5: Dwie zmienne ilosciowe (korelacja Pearsona)
# ============================================================================

ch4_ui <- tabPanel("5. Dwie zmienne ilo\u015bciowe",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Dotychczas badali\u015bmy jedn\u0105 zmienn\u0105.
       Teraz pytamy: czy dwie zmienne ilo\u015bciowe s\u0105 ze sob\u0105 powi\u0105zane?"
    ),

    # ========================================================================
    # Wprowadzenie: wspolczynnik korelacji
    # ========================================================================
    div(class = "section-title", "Wsp\u00f3\u0142czynnik korelacji Pearsona"),

    div(class = "narrative",
      p("Wsp\u00f3\u0142czynnik korelacji Pearsona ", withMathJax("\\(r\\)"),
        " mierzy si\u0142\u0119 i kierunek liniowego zwi\u0105zku mi\u0119dzy dwiema zmiennymi ilo\u015bciowymi."),
      p("Przyjmuje warto\u015bci od ", tags$b("\u22121"), " do ", tags$b("+1"), ":"),
      tags$ul(
        tags$li(tags$b("r = +1"), " \u2014 doskona\u0142a korelacja dodatnia (wzrost jednej = wzrost drugiej)"),
        tags$li(tags$b("r = 0"), " \u2014 brak korelacji liniowej"),
        tags$li(tags$b("r = \u22121"), " \u2014 doskona\u0142a korelacja ujemna (wzrost jednej = spadek drugiej)")
      ),
      div(class = "formula-box",
        p(withMathJax("\\(r = \\frac{\\sum (x_i - \\bar{x})(y_i - \\bar{y})}{\\sqrt{\\sum(x_i-\\bar{x})^2 \\cdot \\sum(y_i-\\bar{y})^2}}\\)"))
      )
    ),

    # --- Wykres 1: sila korelacji (statyczny obrazek) ---
    div(class = "widget-block",
      h4("Si\u0142a korelacji"),
      tags$img(src = "assets/correlation-strength.png",
               style = "width: 100%; border-radius: 4px;"),
      p(class = "narrative", style = "margin-top: 8px;",
        "Im wi\u0119ksze ", withMathJax("\\(|r|\\)"),
        ", tym cia\u015bniej punkty grupuj\u0105 si\u0119 wok\u00f3\u0142 prostej.")
    ),

    # --- Wykres 2: kierunek korelacji (statyczny obrazek) ---
    div(class = "widget-block",
      h4("Kierunek korelacji"),
      tags$img(src = "assets/correlation-direction.png",
               style = "width: 100%; border-radius: 4px;"),
      p(class = "narrative", style = "margin-top: 8px;",
        "Znak ", withMathJax("\\(r\\)"),
        " m\u00f3wi o kierunku: dodatni = obie rosn\u0105 razem, ujemny = jedna ro\u015bnie, druga maleje,
        zero = brak trendu liniowego.")
    ),

    # --- Wykres 3: rozrzut vs r (statyczny obrazek) ---
    div(class = "widget-block",
      h4("Uwaga: r to nie nachylenie!"),
      tags$img(src = "assets/correlation-scatter.png",
               style = "width: 100%; border-radius: 4px;"),
      p(class = "narrative", style = "margin-top: 8px;",
        "Wszystkie trzy panele maj\u0105 ten sam trend wzrostowy (podobne nachylenie prostej).
        Ale im wi\u0119kszy rozrzut punkt\u00f3w wok\u00f3\u0142 prostej, tym ni\u017csze r.
        Korelacja \u0142\u0105czy obie cechy: kierunek trendu ", tags$em("i"), " to, jak cia\u015bno punkty go trzymaj\u0105.")
    ),

    # ========================================================================
    # WIDGET 1: Test korelacji dwustronny (krokowy)
    # ========================================================================
    div(class = "section-title", "Test korelacji \u2014 krok po kroku"),

    div(class = "narrative",
      p("Korelacja z pr\u00f3by (", withMathJax("\\(r\\)"),
        ") prawie nigdy nie wynosi dok\u0142adnie zero, nawet gdy w populacji zwi\u0105zku nie ma.
        Pytanie: czy obserwowane ", withMathJax("\\(r\\)"),
        " jest wystarczaj\u0105co dalekie od zera, by odrzuci\u0107 brak zwi\u0105zku?"),
      div(class = "formula-box",
        p(withMathJax("\\(H_0: \\rho = 0\\)"), " \u2014 ",
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
              "Azotany a odleg\u0142o\u015b\u0107 od \u017ar\u00f3d\u0142a" = "nitrate_dist",
              "Nawadnianie a plon" = "irrigation_yield",
              "St\u0119\u017cenie konserwantu a trwa\u0142o\u015b\u0107" = "preserv_shelf"
            ),
            selected = "sleep_grade"
          ),
          sliderInput("ch4_n", "Wielko\u015b\u0107 pr\u00f3by (n):",
                      min = 15, max = 100, value = 40, step = 5),
          actionButton("ch4_new_sample", "Losuj pr\u00f3b\u0119",
                       class = "btn-primary", width = "100%"),
          hr(),
          h5("Kroki testu:"),
          div(style = "display: flex; flex-direction: column; gap: 6px;",
            actionButton("ch4_step1", "1. Dane (wykres rozrzutu)",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch4_step2", "2. Korelacja z pr\u00f3by",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch4_step3", "3. Statystyka testowa",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch4_step4", "4. p-warto\u015b\u0107 i decyzja",
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
    div(class = "section-title", "A je\u015bli znamy kierunek?"),

    div(class = "narrative",
      p("Tak jak wcze\u015bniej \u2014 czasem nie pytamy \u201eczy jest zwi\u0105zek?\u201d,
        ale \u201eczy wi\u0119cej X = wi\u0119cej Y?\u201d Te same dane, zmienione pytanie.")
    ),

    div(class = "widget-block",
      fluidRow(
        column(4,
          helpText("Dane: te same co w te\u015bcie dwustronnym powy\u017cej."),
          hr(),
          h5("Kroki testu:"),
          div(style = "display: flex; flex-direction: column; gap: 6px;",
            actionButton("ch4b_step1", "1. Dane",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch4b_step2", "2. Korelacja z pr\u00f3by",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch4b_step3", "3. Statystyka testowa",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch4b_step4", "4. p-warto\u015b\u0107 i decyzja",
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
    div(class = "section-title", "Pu\u0142apki korelacji"),

    div(class = "narrative",
      p("Wsp\u00f3\u0142czynnik korelacji to pot\u0119\u017cne narz\u0119dzie, ale \u0142atwo go
        \u017ale zinterpretowa\u0107. Oto cztery klasyczne pu\u0142apki:")
    ),

    # --- 1. Kwartet Anscombe'a ---
    div(class = "widget-block",
      h4("1. Kwartet Anscombe\u2019a \u2014 te same statystyki, r\u00f3\u017cne dane"),
      tags$img(src = "assets/anscombe-quartet.png",
               style = "width: 100%; border-radius: 4px;"),
      p(class = "narrative", style = "margin-top: 8px;",
        "Cztery zbiory danych z identyczn\u0105 korelacj\u0105 (~0.82),
        t\u0105 sam\u0105 \u015bredni\u0105 i wariancj\u0105 \u2014 ale zupe\u0142nie inn\u0105 struktur\u0105.
        Tylko wykres pozwala odklei\u0107 statystyk\u0119 od rzeczywisto\u015bci.
        To argument za tym, \u017ceby zawsze rysowa\u0107 wykres przed interpretacj\u0105 r.")
    ),

    # --- 2. Korelacja pozorna (spurious) ---
    div(class = "widget-block",
      h4("2. Korelacja pozorna (spurious correlation)"),
      div(class = "narrative",
        p("Spo\u017cycie lod\u00f3w i liczba utoni\u0119\u0107 koreluj\u0105 dodatnio.
          Czy lody zabijaj\u0105? Oczywi\u015bcie nie \u2014 obie zmienne zale\u017c\u0105 od ",
          "temperatury (zmienna ukryta / konfounder)."),
        p("Korelacja mi\u0119dzy X i Y mo\u017ce wynika\u0107 z tego,
          \u017ce obie zale\u017c\u0105 od Z. Bez kontroli zmiennych zak\u0142\u00f3caj\u0105cych
          nie mo\u017cna wnioskowa\u0107 o przyczynowo\u015bci."),
        p("Wi\u0119cej absurdalnych przyk\u0142ad\u00f3w:"),
        p(tags$a(href = "https://www.tylervigen.com/spurious-correlations",
                 target = "_blank", style = "font-size: 16px;",
                 "Spurious Correlations (Tyler Vigen) \u2192"))
      )
    ),

    # --- 3. Paradoks Simpsona ---
    div(class = "widget-block",
      h4("3. Paradoks Simpsona"),
      tags$img(src = "assets/simpson-paradox.png",
               style = "width: 100%; max-width: 650px; border-radius: 4px;"),
      div(class = "narrative", style = "margin-top: 8px;",
        p(tags$b("Czarna linia"), " (globalna): wi\u0119cej nauki \u2192 ",
          tags$b("ni\u017cszy"), " wynik (r = \u22120.75). Nauka szkodzi?!"),
        p(tags$b("Kolorowe linie"), " (per szko\u0142a): w ka\u017cdej szkole z osobna wi\u0119cej nauki \u2192 ",
          tags$b("wy\u017cszy"), " wynik (r dodatnie). Nauka pomaga!"),
        p("Jak to mo\u017cliwe? Uczniowie s\u0142abej szko\u0142y ucz\u0105 si\u0119 du\u017co
          (materia\u0142 jest dla nich trudniejszy), ale mimo to maj\u0105 niskie wyniki.
          Uczniowie silnej szko\u0142y ucz\u0105 si\u0119 mniej (materia\u0142 przychodzi \u0142atwiej)
          i maj\u0105 wysokie wyniki. Po po\u0142\u0105czeniu danych \u201ewychodzi\u201d, \u017ce nauka obni\u017ca wyniki."),
        p(tags$em("Zmienna ukryta:"), " poziom szko\u0142y (konfounder).",
          " Agregacja danych maskuje rzeczywisty kierunek zale\u017cno\u015bci."),
        p(tags$a(href = "https://en.wikipedia.org/wiki/Simpson%27s_paradox",
                 target = "_blank",
                 "Wi\u0119cej: Wikipedia \u2192"), " | ",
          tags$a(href = "https://www.youtube.com/watch?v=ebEkn-BiW5k",
                 target = "_blank",
                 "Film TED-Ed \u2192"))
      )
    ),

    # --- 4. Nieliniowość przy r ~ 0 ---
    div(class = "widget-block",
      h4("4. Nieliniowo\u015b\u0107 przy r \u2248 0"),
      tags$img(src = "assets/correlation-nonlinear.png",
               style = "max-width: 500px; width: 100%; border-radius: 4px;"),
      p(class = "narrative", style = "margin-top: 8px;",
        "Zale\u017cno\u015b\u0107 kwadratowa (U-kszta\u0142tna) daje r bliskie zeru,
        cho\u0107 zwi\u0105zek jest silny i deterministyczny.
        Pearson mierzy wy\u0142\u0105cznie zale\u017cno\u015b\u0107 liniow\u0105 \u2014 nie ka\u017cd\u0105.")
    ),

    # --- 5. Outlier (widget interaktywny) ---
    div(class = "widget-block",
      h4("5. Wp\u0142yw outliery na r"),
      div(class = "narrative",
        p("Jeden punkt odleg\u0142y od reszty mo\u017ce ",
          "sztucznie wytworzy\u0107 korelacj\u0119 tam, gdzie jej nie ma.")
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
      tags$strong("Podsumowanie pu\u0142apek:"),
      tags$ol(
        tags$li("Zawsze rysuj wykres przed interpretacj\u0105 r (Anscombe)"),
        tags$li("Korelacja nie oznacza przyczynowo\u015bci \u2014 szukaj konfounder\u00f3w"),
        tags$li("Agregacja danych mo\u017ce odwr\u00f3ci\u0107 kierunek zale\u017cno\u015bci (Simpson)"),
        tags$li("r mierzy tylko zale\u017cno\u015b\u0107 liniow\u0105 (nieliniowo\u015b\u0107)"),
        tags$li("Jeden outlier mo\u017ce drastycznie zmieni\u0107 r")
      )
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: zwi\u0105zek mi\u0119dzy dwiema zmiennymi jako\u015bciowymi"),
      actionButton("ch4_next", "Dalej \u2192 6. Dwie zmienne jako\u015bciowe",
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
      question = "Czy istnieje zwi\u0105zek mi\u0119dzy ilo\u015bci\u0105 snu a ocen\u0105 z egzaminu?",
      h0_text = "\\(H_0: \\rho = 0\\) (brak zwi\u0105zku liniowego)",
      h1_text = "\\(H_a: \\rho \\neq 0\\) (jest zwi\u0105zek)",
      question_1s = "Czy wi\u0119cej snu wi\u0105\u017ce si\u0119 z wy\u017csz\u0105 ocen\u0105?",
      h0_text_1s = "\\(H_0: \\rho \\leq 0\\)",
      h1_text_1s = "\\(H_a: \\rho > 0\\)",
      alt_1s = "greater"),
    nitrate_dist = list(
      r_true = 0.55, xlab = "Odleg\u0142o\u015b\u0107 od \u017ar\u00f3d\u0142a (km)", ylab = "NO\u2083 (mg/l)",
      title = "Azotany wzd\u0142u\u017c rzeki",
      question = "Czy st\u0119\u017cenie azotanow jest powi\u0105zane z odleg\u0142o\u015bci\u0105 od \u017ar\u00f3d\u0142a?",
      h0_text = "\\(H_0: \\rho = 0\\) (brak zwi\u0105zku)",
      h1_text = "\\(H_a: \\rho \\neq 0\\) (jest zwi\u0105zek)",
      question_1s = "Czy st\u0119\u017cenie azotanow ro\u015bnie z odleg\u0142o\u015bci\u0105 od \u017ar\u00f3d\u0142a?",
      h0_text_1s = "\\(H_0: \\rho \\leq 0\\)",
      h1_text_1s = "\\(H_a: \\rho > 0\\)",
      alt_1s = "greater"),
    irrigation_yield = list(
      r_true = 0.60, xlab = "Nawadnianie (mm/tydzie\u0144)", ylab = "Plon (t/ha)",
      title = "Nawadnianie a plon",
      question = "Czy ilo\u015b\u0107 nawadniania jest powi\u0105zana z plonem?",
      h0_text = "\\(H_0: \\rho = 0\\) (brak zwi\u0105zku)",
      h1_text = "\\(H_a: \\rho \\neq 0\\) (jest zwi\u0105zek)",
      question_1s = "Czy wi\u0119ksze nawadnianie daje wy\u017csze plony?",
      h0_text_1s = "\\(H_0: \\rho \\leq 0\\)",
      h1_text_1s = "\\(H_a: \\rho > 0\\)",
      alt_1s = "greater"),
    preserv_shelf = list(
      r_true = 0.50, xlab = "St\u0119\u017cenie konserwantu (mg/kg)", ylab = "Trwa\u0142o\u015b\u0107 (dni)",
      title = "Konserwant a trwa\u0142o\u015b\u0107",
      question = "Czy st\u0119\u017cenie konserwantu wp\u0142ywa na trwa\u0142o\u015b\u0107 produktu?",
      h0_text = "\\(H_0: \\rho = 0\\) (brak zwi\u0105zku)",
      h1_text = "\\(H_a: \\rho \\neq 0\\) (jest zwi\u0105zek)",
      question_1s = "Czy wi\u0119ksze st\u0119\u017cenie konserwantu wyd\u0142u\u017ca trwa\u0142o\u015b\u0107?",
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
        p(tags$em(paste0("\u201e", par$question, "\u201d")))
      ),
      div(class = "formula-box",
        p(tags$b("Hipoteza formalna (dwustronna):")),
        p(withMathJax(par$h0_text)),
        p(withMathJax(par$h1_text))
      ),
      if (is.null(d)) {
        div(style = "text-align: center; margin: 10px 0; color: #7f8c8d;",
          p(tags$em("Kliknij \u201eLosuj pr\u00f3b\u0119\u201d"))
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
                 label = "Pr\u00f3ba gotowa! Klikaj kroki po kolei.",
                 size = 5, color = "#7f8c8d") +
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
        labs(title = paste0("Rozk\u0142ad pod H\u2080: t(", n - 2, ")"),
             x = "Statystyka testowa", y = "G\u0119sto\u015b\u0107") +
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
        p("Ka\u017cdy punkt to jedna obserwacja z dwiema warto\u015bciami: ",
          par$xlab, " i ", par$ylab, ". Czy wida\u0107 trend?")
      ),
      "2" = tagList(
        div(class = "stat-box", style = paste0("background:", col_pvalue, ";"),
            paste0("r = ", round(r_val, 3))),
        p("Korelacja z pr\u00f3by: ", tags$b(round(r_val, 3)),
          ". Ale czy to wystarczaj\u0105co daleko od zera, by odrzuci\u0107 H\u2080?")
      ),
      "3" = tagList(
        div(class = "stat-box", style = paste0("background:", col_effect, ";"),
            paste0("t = ", round(r_val, 3), " \u00b7 \u221a", n - 2,
                   " / \u221a(1 \u2212 ", round(r_val^2, 3),
                   ") = ", round(t_stat, 3))),
        p("Zamieniamy r na statystyk\u0119 t, \u017ceby m\u00f3c por\u00f3wna\u0107 z rozk\u0142adem t(", n - 2, ").")
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
        p(tags$em(paste0("\u201e", par$question_1s, "\u201d")))
      ),
      div(class = "formula-box",
        p(tags$b("Hipoteza formalna (jednostronna!):")),
        p(withMathJax(par$h0_text_1s)),
        p(withMathJax(par$h1_text_1s))
      ),
      if (is.null(d)) {
        div(style = "text-align: center; margin: 10px 0; color: #7f8c8d;",
          p(tags$em("Najpierw wylosuj pr\u00f3b\u0119 w te\u015bcie dwustronnym powy\u017cej"))
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
        labs(title = paste0("Rozk\u0142ad pod H\u2080: t(", n - 2, ")"),
             subtitle = "Test jednostronny \u2014 tylko jeden ogon!",
             x = "Statystyka testowa", y = "G\u0119sto\u015b\u0107") +
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
            paste0("n = ", n, " (te same dane co wy\u017cej)")),
        p("Te same obserwacje, ale pytamy o kierunek zwi\u0105zku.")
      ),
      "2" = tagList(
        div(class = "stat-box", style = paste0("background:", col_pvalue, ";"),
            paste0("r = ", round(r_val, 3), " (ta sama warto\u015b\u0107!)")),
        p("Korelacja si\u0119 nie zmieni\u0142a. Zmieni\u0142o si\u0119 pytanie.")
      ),
      "3" = tagList(
        div(class = "stat-box", style = paste0("background:", col_effect, ";"),
            paste0("t = ", round(t_stat, 3), " (ta sama warto\u015b\u0107!)")),
        p("W te\u015bcie jednostronnym patrzymy tylko na ",
          tags$b(if (par$alt_1s == "greater") "prawy" else "lewy"), " ogon.")
      ),
      "4" = tagList(
        div(class = "stat-box", style = paste0("background:", col_pvalue, ";"),
            paste0("p = ", format.pval(p_val, digits = 4), " (jednostronnie!)")),
        p(style = paste0("color: ", res$color, "; font-weight: bold; font-size: 16px;"),
          res$decision),
        p(res$explanation),
        p(tags$em("Por\u00f3wnaj z testem dwustronnym wy\u017cej \u2014 te same dane, ten sam r i t, ale inna p-warto\u015b\u0107!"))
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
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij \u201eNowe dane\u201d",
                 size = 6, color = "#7f8c8d") +
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
          paste0("Outlier\u00f3w: ", n_outliers))
    )
  })
}
