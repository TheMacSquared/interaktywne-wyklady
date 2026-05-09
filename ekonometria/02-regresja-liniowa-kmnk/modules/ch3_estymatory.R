# ============================================================================
# ROZDZIAŁ 3: Estymatory parametrów strukturalnych
# ============================================================================

ch3_ui <- lecture_chapter(
  id = "ch-estymatory",
  num = "03",
  title = "Estymatory parametrów",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 02 · KMNK",
      num = "03",
      title = "Skąd się biorą b₀ i b₁?",
      lead = "Wcześniej wiedzieliśmy, że KMNK wybiera prostą minimalizującą sumę kwadratów reszt. Teraz zaglądamy pod maskę: jak właściwie liczone są wyraz wolny i nachylenie?"
    ),

    lc_h2("ch3-rozroznienie", "Parametr β czy estymata b?"),
    lc_p("Statystyka rozróżnia dwa byty, które na pierwszy rzut oka wyglądają podobnie. Parametr populacyjny β opisuje cały świat, którego nie widzimy w całości. Estymata b to liczba, którą faktycznie obliczamy z konkretnej próby — i która zmieniłaby się, gdyby próba była inna."),
    figure_panel(
      label = "Tabela 3.1",
      title = "Parametr populacji vs. estymata z próby",
      tags$table(
        class = "table",
        tags$thead(tags$tr(
          tags$th("Cecha"),
          tags$th("Parametr β"),
          tags$th("Estymata b")
        )),
        tags$tbody(
          tags$tr(
            tags$td("Skąd pochodzi?"),
            tags$td("z całej populacji"),
            tags$td("z konkretnej próby")
          ),
          tags$tr(
            tags$td("Czy go znamy?"),
            tags$td("nie — to wartość, którą próbujemy odgadnąć"),
            tags$td("tak — to liczba, którą wyliczamy")
          ),
          tags$tr(
            tags$td("Czy jest losowy?"),
            tags$td("nie — to stała, choć nieznana"),
            tags$td("tak — zależy od tego, kogo trafiliśmy do próby")
          ),
          tags$tr(
            tags$td("Notacja"),
            tags$td("β₀, β₁ (litery greckie)"),
            tags$td("b₀, b₁ albo β̂₀, β̂₁ (z daszkiem)")
          )
        )
      )
    ),

    lc_h2("ch3-formula", "Wzory KMNK"),
    lc_p("Wzór na nachylenie b₁ wygląda groźnie, ale ma prostą interpretację: licznik to kowariancja X i Y, mianownik — wariancja X."),
    lc_formula_box(
      withMathJax(helpText("$$\\hat\\beta_1 = \\frac{\\sum_i (x_i - \\bar{x})(y_i - \\bar{y})}{\\sum_i (x_i - \\bar{x})^2}$$")),
      withMathJax(helpText("$$\\hat\\beta_0 = \\bar{y} - \\hat\\beta_1 \\bar{x}$$")),
      p("Licznik mówi, jak Y systematycznie zmienia się razem z X. Mianownik mówi, jak rozrzucone są same wartości X. Iloraz daje liczbę jednostek Y na jednostkę X — czyli nachylenie prostej."),
      p("Wzór na b₀ jest natomiast prostym następstwem: prosta KMNK zawsze przechodzi przez punkt (x̄, ȳ), więc gdy znamy nachylenie i ten punkt, wyraz wolny wyliczamy jednoznacznie.")
    ),

    lc_h2("ch3-historia", "Historia z dwóch firm konsultingowych"),
    lc_p("Pewna sieć handlowa zleciła dwóm firmom konsultingowym to samo pytanie: jak metraż sklepu wpływa na miesięczną sprzedaż? Każda firma dostała inną próbę 50 sklepów z tego samego rynku."),
    lc_p("Firma A oszacowała b₁ = 1,42 (tysiąca zł sprzedaży na metr kwadratowy). Firma B — b₁ = 1,61. Klient wpadł w popłoch: kto się pomylił?"),
    lc_p("Odpowiedź: nikt. Obie próby pochodzą z tej samej populacji sklepów, ale przez losowy dobór trafiły na nieco inny zestaw lokalizacji. Każda estymata jest poprawnym oszacowaniem prawdziwego, nieznanego β₁ — różnią się dlatego, że dane są zaszumione, a próby nie pokrywają się idealnie."),

    # ------------------------------------------------------------------------
    # Widget główny: kumulatywna symulacja rozkładu estymatora
    # ------------------------------------------------------------------------
    lc_h2("ch3-rozklad", "Skąd się bierze rozkład b₁? Buduj go iteracja po iteracji."),
    lc_p("Pojedyncza estymata to jedna kropka. Ciekawsze pytanie brzmi: gdyby ten sam projekt powtórzyć 100 razy z różnymi próbami, jaki rozkład ", em("liczb"), " by z tego wyszedł? Tak się rodzi pojęcie ‚rozkład estymatora‘ — i jest podstawą całego wnioskowania statystycznego."),
    lc_p("Symulator niżej startuje od pustego histogramu. Każde kliknięcie ‚+1 próba‘ losuje jedną próbę, dopasowuje KMNK i dorzuca b₁ do histogramu. ‚+50 prób‘ robi to samo 50 razy hurtem. Patrz, jak histogram zaczyna wyłaniać kształt rozkładu normalnego — i jak średnia z wielu prób zbiega do prawdziwej β₁ = 1,5."),
    figure_panel(
      label = "Ryc. 3.1",
      title = "Rozkład estymatora b₁ — symulacja krok po kroku",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          tags$strong("Parametry symulacji"),
          sliderInput("ch3_sim_n", "Liczność każdej próby (n)", min = 20, max = 200, value = 50, step = 10),
          sliderInput("ch3_sim_sigma", "Szum w populacji (σ)", min = 5, max = 40, value = 20, step = 5),
          tags$hr(),
          tags$strong("Losuj próby"),
          actionButton("ch3_los_1", "+1 próba", class = "btn-primary", width = "100%"),
          tags$br(), tags$br(),
          actionButton("ch3_los_50", "+50 prób", class = "btn-primary", width = "100%"),
          tags$br(), tags$br(),
          actionButton("ch3_reset", "Reset", class = "btn-outline-secondary", width = "100%")
        ),
        column(
          8,
          fluidRow(
            column(6, plotOutput("ch3_sim_proba", height = "260px")),
            column(6, plotOutput("ch3_sim_hist", height = "260px"))
          ),
          uiOutput("ch3_sim_status"),
          uiOutput("ch3_sim_verdict")
        )
      )
    ),
    inline_callout(
      label = "Wskazówka",
      color = "ok",
      "Po pierwszych 5–10 próbach histogram wygląda chaotycznie. Po 50 zaczyna nabierać kształtu dzwonu. Po 200 — to prawie idealny rozkład normalny wokół β₁ = 1,5. To centralne twierdzenie graniczne w działaniu: niezależnie od oryginalnego rozkładu szumu, średnia (i b₁) ma rozkład normalny dla dużych n."
    ),

    # ------------------------------------------------------------------------
    # Drugi widget: trzy próby naraz (szybka wersja porównawcza)
    # ------------------------------------------------------------------------
    lc_h2("ch3-trzy-proby", "Trzy próby na raz — podgląd zmienności"),
    lc_p("Kumulatywny histogram pokazuje rozkład w długim biegu. Czasem warto też zobaczyć trzy konkretne próby obok siebie — żeby wzrokowo ocenić, jak różne mogą być prostymi KMNK z tego samego mechanizmu."),
    figure_panel(
      label = "Ryc. 3.2",
      title = "Estymata b₁ na trzech kolejnych próbach",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          actionButton("ch3_resample", "Wylosuj nowe próby", class = "btn-primary", width = "100%"),
          tags$br(), tags$br(),
          sliderInput("ch3_sigma", "Szum w danych (σ)", min = 1, max = 20, value = 8, step = 1)
        ),
        column(8, plotOutput("ch3_plot", height = "320px"), uiOutput("ch3_summary"))
      )
    ),

    inline_callout(
      label = "Zapamiętaj",
      color = "wskazowka",
      open = TRUE,
      "Estymator to przepis (formuła). Estymata to konkretna liczba uzyskana po podstawieniu danych. b₁ jest losowy w sensie: zależy od próby, nie od populacji. Pojedyncza estymata nie wyrokuje o prawdzie — dopiero z miarą niepewności (do której dojdziemy w wykładzie 03) wiemy, czy 1,42 i 1,61 to praktycznie ta sama odpowiedź."
    ),

    lc_chapter_next(
      num = "04",
      title = "Założenia KMNK",
      lead = "warunki, których wymaga klasyczna metoda",
      target_id = "ch-zalozenia"
    )
  )
)

ch3_server <- function(input, output, session) {

  # --- Widget 1: kumulatywna symulacja -------------------------------------

  ch3_estymaty <- reactiveVal(numeric(0))
  ch3_ostatnia <- reactiveVal(NULL)
  ch3_seed_global <- reactiveVal(31L)

  ch3_los_jedna <- function(seed) {
    eco02_kmnk_data(n = input$ch3_sim_n, beta0 = 20, beta1 = 1.5,
                    sigma = input$ch3_sim_sigma, seed = seed)
  }

  observeEvent(input$ch3_los_1, {
    seed_now <- ch3_seed_global() + length(ch3_estymaty()) + 1L
    df <- ch3_los_jedna(seed_now)
    fit <- lm(y ~ x, data = df)
    ch3_estymaty(c(ch3_estymaty(), unname(coef(fit)[2])))
    ch3_ostatnia(df)
  })

  observeEvent(input$ch3_los_50, {
    base_seed <- ch3_seed_global() + length(ch3_estymaty()) + 1L
    nowe <- vapply(seq_len(50), function(k) {
      df <- ch3_los_jedna(base_seed + k - 1L)
      unname(coef(lm(y ~ x, data = df))[2])
    }, numeric(1))
    df_last <- ch3_los_jedna(base_seed + 49L)
    ch3_estymaty(c(ch3_estymaty(), nowe))
    ch3_ostatnia(df_last)
  })

  observeEvent(input$ch3_reset, {
    ch3_estymaty(numeric(0))
    ch3_ostatnia(NULL)
    ch3_seed_global(sample.int(1e6, 1))
  })

  # Reset symulacji jeśli zmieniono parametry populacji
  observeEvent(list(input$ch3_sim_n, input$ch3_sim_sigma), {
    ch3_estymaty(numeric(0))
    ch3_ostatnia(NULL)
  }, ignoreInit = TRUE)

  output$ch3_sim_proba <- renderPlot({
    df <- ch3_ostatnia()
    if (is.null(df)) {
      return(
        ggplot() +
          annotate("text", x = 0, y = 0,
                   label = "Kliknij ‚+1 próba‘, by zacząć.",
                   color = unname(upwr_cat["grafit"]), size = 4.2) +
          theme_void()
      )
    }
    fit <- lm(y ~ x, data = df)
    coefs <- coef(fit)
    ggplot(df, aes(x, y)) +
      geom_point(color = unname(upwr_cat["grafit"]), alpha = 0.7, size = 2) +
      geom_abline(intercept = 20, slope = 1.5,
                  color = unname(upwr_cat["niebo"]),
                  linetype = "dashed", linewidth = 0.8) +
      geom_abline(intercept = coefs[1], slope = coefs[2],
                  color = upwr_accent, linewidth = 1.2) +
      coord_cartesian(xlim = c(20, 210), ylim = c(0, 400)) +
      labs(x = "X — metraż (m²)", y = "Y — sprzedaż (tys. zł)",
           title = paste0("Ostatnia próba: b₁ = ", eco_fmt(coefs[2], 3))) +
      theme_upwr()
  })

  output$ch3_sim_hist <- renderPlot({
    estymaty <- ch3_estymaty()
    n_prob <- length(estymaty)

    if (n_prob == 0) {
      return(
        ggplot() +
          annotate("text", x = 0, y = 0,
                   label = "Histogram pojawi się\npo pierwszej próbie.",
                   color = unname(upwr_cat["grafit"]), size = 4.2) +
          theme_void()
      )
    }

    df <- data.frame(b1 = estymaty)
    binwidth <- if (n_prob < 20) 0.1 else if (n_prob < 100) 0.05 else 0.03

    ggplot(df, aes(b1)) +
      geom_histogram(binwidth = binwidth, fill = unname(upwr_cat["szalwia"]),
                     color = "white", alpha = 0.85) +
      geom_vline(xintercept = 1.5, color = unname(upwr_cat["niebo"]),
                 linetype = "dashed", linewidth = 1) +
      geom_vline(xintercept = mean(estymaty), color = upwr_accent, linewidth = 1) +
      labs(x = "Oszacowane b₁", y = "Liczba prób",
           title = paste0("Rozkład estymat (n_prób = ", n_prob, ")")) +
      coord_cartesian(xlim = c(0.5, 2.5)) +
      theme_upwr()
  })

  output$ch3_sim_status <- renderUI({
    estymaty <- ch3_estymaty()
    n_prob <- length(estymaty)
    if (n_prob == 0) return(NULL)

    teor_se <- input$ch3_sim_sigma / sqrt(input$ch3_sim_n) /
               sqrt(var(seq(30, 200, length.out = input$ch3_sim_n)))

    lc_stat_grid(
      lc_stat_box(label = "Liczba prób", value = n_prob,
                  caption = "ile estymat dotychczas",
                  color = upwr_secondary),
      lc_stat_box(label = "Średnia b₁", value = eco_fmt(mean(estymaty), 3),
                  caption = "powinna zbiegać do 1,5",
                  color = upwr_accent),
      lc_stat_box(label = "SD b₁ (empir.)", value = if (n_prob >= 2) eco_fmt(sd(estymaty), 3) else "—",
                  caption = "empiryczny SE estymatora",
                  color = unname(upwr_cat["szalwia"])),
      lc_stat_box(label = "SE teoretyczne", value = eco_fmt(teor_se, 3),
                  caption = "z formuły KMNK",
                  color = unname(upwr_cat["niebo"])),
      columns = 4
    )
  })

  output$ch3_sim_verdict <- renderUI({
    estymaty <- ch3_estymaty()
    n_prob <- length(estymaty)

    if (n_prob == 0) {
      return(lc_feedback(
        type = "info",
        "Symulacja czeka na start. Kliknij ‚+1 próba‘, żeby zacząć budować rozkład."
      ))
    }

    if (n_prob < 5) {
      return(lc_feedback(
        type = "info",
        strong("Pierwsze próby. "),
        "Histogram jest jeszcze chaotyczny — to normalne. Z 1, 2 czy 3 próbami nie widać żadnego rozkładu, tylko pojedyncze kreski. Kliknij ‚+50‘, żeby przeskoczyć dalej."
      ))
    }

    if (n_prob < 30) {
      return(lc_feedback(
        type = "info",
        strong("Już coś widać. "),
        "Po ", n_prob, " próbach średnia b₁ to ", eco_fmt(mean(estymaty), 3),
        " — z każdą kolejną próbą będzie się stabilizować wokół prawdziwej β₁ = 1,5. ",
        "Kontynuuj symulację, żeby zobaczyć kształt rozkładu."
      ))
    }

    odchylka <- mean(estymaty) - 1.5
    sd_emp <- sd(estymaty)
    teor_se <- input$ch3_sim_sigma / sqrt(input$ch3_sim_n) /
               sqrt(var(seq(30, 200, length.out = input$ch3_sim_n)))

    lc_feedback(
      type = "ok",
      strong("Rozkład się ujawnia. "),
      "Po ", strong(n_prob), " próbach średnia oszacowanie to ",
      strong(eco_fmt(mean(estymaty), 3)),
      " (różnica od prawdziwego β₁ = 1,5 to tylko ", eco_fmt(odchylka, 3), "). ",
      "Empiryczne odchylenie standardowe estymat = ", strong(eco_fmt(sd_emp, 3)),
      " — to jest dokładnie błąd standardowy SE(b₁) liczony empirycznie. ",
      "Teoretyczna formuła przewiduje ", eco_fmt(teor_se, 3),
      "; widać, że oba są bardzo blisko."
    )
  })

  # --- Widget 2: trzy próby (zachowane z poprzedniej wersji) ---------------

  ch3_seed <- reactiveVal(2024)

  observeEvent(input$ch3_resample, {
    ch3_seed(sample.int(1e6, 1))
  })

  ch3_samples <- reactive({
    base_seed <- ch3_seed()
    do.call(rbind, lapply(1:3, function(i) {
      d <- eco_regression_data(n = 50, beta0 = 10, beta1 = 1.5,
                               sigma = input$ch3_sigma, seed = base_seed + i)
      d$proba <- factor(paste("Próba", i), levels = paste("Próba", 1:3))
      d
    }))
  })

  ch3_fits <- reactive({
    samples <- ch3_samples()
    do.call(rbind, lapply(levels(samples$proba), function(lvl) {
      d <- samples[samples$proba == lvl, ]
      fit <- lm(y ~ x, data = d)
      data.frame(
        proba = factor(lvl, levels = levels(samples$proba)),
        b0 = coef(fit)[1],
        b1 = coef(fit)[2]
      )
    }))
  })

  output$ch3_plot <- renderPlot({
    ggplot(ch3_samples(), aes(x, y)) +
      geom_point(color = unname(upwr_cat["grafit"]), alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE, color = upwr_accent, linewidth = 1) +
      geom_abline(intercept = 10, slope = 1.5, color = unname(upwr_cat["niebo"]),
                  linetype = "dashed", linewidth = 0.8) +
      facet_wrap(~ proba, nrow = 1) +
      labs(x = "X", y = "Y", caption = "Linia czerwona — KMNK; przerywana niebieska — prawdziwa β₁ = 1,5") +
      theme_upwr()
  })

  output$ch3_summary <- renderUI({
    fits <- ch3_fits()
    boxes <- lapply(seq_len(nrow(fits)), function(i) {
      lc_stat_box(
        label = as.character(fits$proba[i]),
        value = paste0("b₁ = ", eco_fmt(fits$b1[i], 3)),
        caption = paste0("b₀ = ", eco_fmt(fits$b0[i], 2)),
        color = c(unname(upwr_cat["niebo"]), unname(upwr_cat["szalwia"]),
                  unname(upwr_cat["terakota"]))[i]
      )
    })
    do.call(lc_stat_grid, c(boxes, list(columns = 3)))
  })
}
