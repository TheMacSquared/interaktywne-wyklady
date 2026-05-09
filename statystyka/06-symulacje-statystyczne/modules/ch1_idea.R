# ============================================================================
# CHAPTER 1: Idea resamplingowa
# ============================================================================

ch1_ui <- lecture_chapter(
  id = "ch-idea",
  num = "01",
  title = "Idea resamplingowa",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 01 · Symulacje statystyczne",
      num    = "01",
      title  = "Idea resamplingowa",
      lead   = "Zaczynamy od intuicji: próbę traktujemy jak mini-populację i uczymy się losować z niej ponownie."
    ),

    lc_feedback(type = "info",
      "Znamy już klasyczne testy i przedziały ufności oparte na założeniach o rozkładach.
       Czas poznać podejście, które z tych założeń rezygnuje."
    ),

    lc_h2("ch1-sec-01", "Skąd biorą się klasyczne metody?"),

    tagList(
      p("Klasyczna statystyka opiera się na ", tags$b("założeniach o rozkładzie"),
        " populacji lub na Centralnym Twierdzeniu Granicznym (CTG). Na przykład:"),
      tags$ul(
        tags$li("Test t wymaga normalności lub dużej próby (CTG)"),
        tags$li("Przedział ufności dla średniej zakada symetrię błędu"),
        tags$li("Wzory na błąd standardowy dotyczą konkretnych statystyk (mean, proportion)")
      )
    ),

    lc_feedback(type = "warning",
      tags$strong("Problem:"),
      " Co jeśli dane są silnie skośne i próba mała? Co jeśli chcemy przedział ufności
       dla mediany albo percentyla? Klasyczne wzory ",
      tags$b("nie istnieją"), " lub są niedokładne."
    ),

    lc_h2("ch1-sec-02", "Idea resamplingowa"),

    tagList(
      p("Kluczowy pomysł: ", tags$b("traktuj próbę jako mini-populację"),
        " i losuj z niej wielokrotnie."),
      p("Jeśli próba jest reprezentatywna dla populacji, to losowanie z próby
        imituje losowanie z populacji. Możemy wtedy:", tags$b(" symulacyjnie "),
        "uzyskać to, co klasyczna statystyka oblicza analitycznie.")
    ),

    lc_feedback(type = "info",
      tags$strong("Słowniczek:"),
      tags$ul(
        tags$li(tags$b("Próba bootstrapowa:"),
                " losowanie ze zwracaniem z oryginalnej próby (n obserwacji → n obserwacji,
                  część się powtarza)"),
        tags$li(tags$b("Rozkład bootstrapowy:"),
                " empiryczny rozkład statystyki z wielu prób bootstrapowych"),
        tags$li(tags$b("Test permutacyjny:"),
                " przetasowanie etykiet grup, aby zasymulować świat H₀"),
        tags$li(tags$b("Jackknife:"),
                " leave-one-out — n prób, każda bez jednej obserwacji"),
        tags$li(tags$b("Cross-validation:"),
                " k-krotny podział na trening/test do oceny modelu"),
        tags$li(tags$b("Monte Carlo:"),
                " symulacja z rozkładu (np. mocy testu lub rozkładu pod H₀)")
      )
    ),

    lc_h2("ch1-sec-03", "Mapa metod resamplingowych"),

    tagList(
      p("Pięć metod, które poznamy w tym rozdziale, różnią się celem
        i sposobem resamplingu:")
    ),

    tags$table(class = "decision-table",
      tags$thead(tags$tr(
        tags$th("Metoda"),
        tags$th("Cel"),
        tags$th("Sposób resamplingu")
      )),
      tags$tbody(
        tags$tr(
          tags$td(tags$b("Bootstrap")),
          tags$td("Przedział ufności dla dowolnej statystyki"),
          tags$td("Losowanie ze zwracaniem z próby")
        ),
        tags$tr(
          tags$td(tags$b("Jackknife")),
          tags$td("Estymacja obciążenia i SE"),
          tags$td("Leave-one-out: n prób bez kolejnych obserwacji")
        ),
        tags$tr(
          tags$td(tags$b("Test permutacyjny")),
          tags$td("Test hipotezy bez założeń"),
          tags$td("Przetasowanie etykiet grup")
        ),
        tags$tr(
          tags$td(tags$b("Cross-validation")),
          tags$td("Ocena jakości modelu predykcyjnego"),
          tags$td("k-krotny podział trening/test")
        ),
        tags$tr(
          tags$td(tags$b("Monte Carlo")),
          tags$td("Moc testu lub rozkład pod H₀"),
          tags$td("Losowanie z parametrycznego rozkładu")
        )
      )
    ),

    # ========================================================================
    # WIDGET 1: Proba bootstrapowa w akcji
    # ========================================================================
    lc_h2("ch1-sec-04", "Próba bootstrapowa w akcji"),

    tagList(
      p("Zacznijmy od najmniejszego kroku: jednej próby bootstrapowej.
        Pobierz próbę z populacji, a następnie wylosuj z niej jedną próbę
        bootstrapową (losowanie ze zwracaniem)."),
      p("Kolor kropek pokazuje, ile razy dana obserwacja została wylosowana:",
        tags$span(style = "color:var(--upwr-rule); font-weight:bold;", " szara"),
        " = pominął (0 razy),",
        tags$span(style = "color:var(--upwr-cat-niebo); font-weight:bold;", " niebieska"),
        " = raz,",
        tags$span(style = "color:var(--upwr-cat-bursztyn); font-weight:bold;", " pomarańczowa"),
        " = dwa razy lub więcej.")
    ),

    figure_panel(label = "Ryc. 1.1", title = "Jedna próba bootstrapowa",
      fluidRow(
        column(4,
          selectInput("ch1_dist", "Kształt danych:",
            choices = c(
              "Prawoskosśny (Gamma)" = "skewed",
              "Normalny"               = "normal",
              "Dwumodalny"             = "bimodal"
            ),
            selected = "skewed"
          ),
          sliderInput("ch1_n_orig", "Wielkość próby (n):",
                      min = 8, max = 30, value = 15, step = 1),
          hr(),
          actionButton("ch1_draw_orig", "Pobierz próbę",
                       class = "lc-btn-primary", width = "100%"),
          br(), br(),
          actionButton("ch1_resample_one", "+ Nowa próba bootstrapowa",
                       class = "lc-btn-warning", width = "100%"),
          br(), br(),
          actionButton("ch1_resample_reset", "Wyczyść próby",
                       class = "lc-btn-secondary-outline", width = "100%"),
          br(), br(),
          uiOutput("ch1_demo_stats")
        ),
        column(8,
          plotOutput("ch1_bootstrap_demo")
        )
      )
    ),

    # ========================================================================
    # WIDGET 2: Budowanie rozkladu bootstrapowego
    # ========================================================================
    lc_h2("ch1-sec-05", "Rozkład bootstrapowy"),

    tagList(
      p("Jedna próba bootstrapowa daje jedną wartość średniej.
        Tysiąc prób daje rozkład średniej. To właśnie ",
        tags$b("rozkład bootstrapowy"), "."),
      p("Jego odchylenie standardowe jest ",
        tags$b("błędem standardowym"),
        " statystyki — dokładnie to, co wzory analityczne
        próbują obliczyć.")
    ),

    figure_panel(label = "Ryc. 1.2", title = "Budowanie rozkładu bootstrapowego",
      p(class = "text-muted",
        "Najpierw pobierz próbę (powyżej), następnie kliknij \"Uruchom\"."),
      fluidRow(
        column(4,
          sliderInput("ch1_B", "Liczba prób bootstrapowych (B):",
                      min = 50, max = 2000, value = 500, step = 50),
          actionButton("ch1_build_dist", "Uruchom bootstrap",
                       class = "lc-btn-primary", width = "100%"),
          br(), br(),
          actionButton("ch1_build_reset", "Reset",
                       class = "lc-btn-secondary-outline", width = "100%"),
          br(), br(),
          uiOutput("ch1_boot_stats")
        ),
        column(8,
          plotOutput("ch1_boot_dist_plot", height = "320px")
        )
      )
    ),

    lc_feedback(type = "ok",
      tags$strong("Aha-moment:"),
      " Odchylenie standardowe rozkładu bootstrapowego = ",
      tags$b("błąd standardowy statystyki"),
      ". Właśnie to klasyczne wzory próbują obliczyć analitycznie —
       bootstrap oblicza to symulacyjnie!"
    ),

    lc_chapter_next(
      num = "02",
      title = "Bootstrap — przedziały",
      lead = "jak z rozkładu bootstrapowego budować przedziały ufności.",
      target_id = "ch-bootstrap-ci"
    )

  )
)
# ============================================================================
# SERVER
# ============================================================================

ch1_server <- function(input, output, session) {

  ch1_orig_data  <- reactiveVal(NULL)
  ch1_resample   <- reactiveVal(NULL)
  ch1_boot_dist  <- reactiveVal(NULL)

  # --- Widget 1: pobierz probe ---
  observeEvent(input$ch1_draw_orig, {
    x <- generate_sample_data(input$ch1_n_orig, dist = input$ch1_dist)
    ch1_orig_data(x)
    ch1_resample(NULL)
    ch1_boot_dist(NULL)
  })

  # Reset przy zmianie parametrow
  observeEvent(input$ch1_dist,    { ch1_orig_data(NULL); ch1_resample(NULL); ch1_boot_dist(NULL) })
  observeEvent(input$ch1_n_orig,  { ch1_orig_data(NULL); ch1_resample(NULL); ch1_boot_dist(NULL) })
  observeEvent(input$ch1_resample_reset, {
    ch1_resample(NULL)
  })

  # --- Widget 1: kolejne proby bootstrapowe (lista) ---
  observeEvent(input$ch1_resample_one, {
    req(ch1_orig_data())
    x        <- ch1_orig_data()
    rs       <- sample(x, size = length(x), replace = TRUE)
    current  <- ch1_resample()
    if (is.null(current)) current <- list()
    # Ogranicz do 8 prob zeby wykres pozostal czytelny
    if (length(current) >= 8) current <- current[-1]
    ch1_resample(c(current, list(rs)))
  })

  output$ch1_bootstrap_demo <- renderPlot({
    orig <- ch1_orig_data()
    rs   <- ch1_resample()  # lista wektorow lub NULL

    if (is.null(orig)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij 'Pobierz próbę'",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else if (is.null(rs)) {
      # Pokaz tylko oryginalna probe
      df <- data.frame(x = orig, y = 0)
      ggplot(df, aes(x = x, y = y)) +
        geom_jitter(color = sim_bootstrap, height = 0.1, size = 3.5, alpha = 0.9) +
        geom_vline(xintercept = mean(orig), color = sim_secondary,
                   linewidth = 1.2, linetype = "dashed") +
        annotate("text", x = mean(orig), y = Inf,
                 label = paste0("śr. = ", round(mean(orig), 2)),
                 vjust = -0.3, hjust = -0.1, color = sim_secondary, size = 4) +
        scale_y_continuous(breaks = 0, labels = "Oryginalna próba") +
        labs(
             x = "Wartość", y = NULL) +
        theme_upwr() +
        theme(axis.text.y = element_text(size = 12))
    } else {
      plot_bootstrap_step(orig, rs, sim_bootstrap, sim_warning, sim_secondary)
    }
  }, height = function() {
    rs <- ch1_resample()
    if (is.null(rs)) 360 else max(360, 120 + length(rs) * 80)
  })

  output$ch1_demo_stats <- renderUI({
    orig <- ch1_orig_data()
    rs   <- ch1_resample()
    if (is.null(orig)) return(NULL)
    tag_list <- list(
      div(class = "lc-stat-box", style = paste0("background:", sim_secondary, ";"),
          paste0("n = ", length(orig))),
      div(class = "lc-stat-box", style = paste0("background:", sim_bootstrap, ";"),
          paste0("śr. oryg. = ", round(mean(orig), 2)))
    )
    if (!is.null(rs)) {
      last_rs <- rs[[length(rs)]]
      tag_list <- c(tag_list, list(
        div(class = "lc-stat-box", style = paste0("background:", sim_warning, ";"),
            paste0("śr. boot. #", length(rs), " = ", round(mean(last_rs), 2))),
        div(class = "lc-stat-box", style = paste0("background:", sim_secondary, "; opacity:0.7;"),
            paste0("Liczba prób: ", length(rs)))
      ))
    }
    do.call(tagList, tag_list)
  })

  # --- Widget 2: rozklad bootstrapowy ---
  observeEvent(input$ch1_build_reset, {
    ch1_boot_dist(NULL)
  })

  observeEvent(input$ch1_build_dist, {
    req(ch1_orig_data())
    x <- ch1_orig_data()
    result <- run_bootstrap(x, mean, B = input$ch1_B)
    ch1_boot_dist(result)
  })

  output$ch1_boot_dist_plot <- renderPlot({
    result <- ch1_boot_dist()
    if (is.null(result)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij 'Uruchom bootstrap'",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      ci <- bootstrap_ci_percentile(result, conf_level = 0.95)
      plot_bootstrap_distribution(result, ci,
                                   stat_label = "Średniane bootstrapowe",
                                   sim_bootstrap = sim_bootstrap,
                                   sim_observed = sim_observed,
                                   sim_success = sim_success)
    }
  })

  output$ch1_boot_stats <- renderUI({
    result <- ch1_boot_dist()
    if (is.null(result)) return(NULL)
    tagList(
      div(class = "lc-stat-box", style = paste0("background:", sim_secondary, ";"),
          paste0("B = ", result$B)),
      div(class = "lc-stat-box", style = paste0("background:", sim_observed, ";"),
          paste0("obs. = ", round(result$observed, 3))),
      div(class = "lc-stat-box", style = paste0("background:", sim_bootstrap, ";"),
          paste0("SE = ", round(result$se, 4)))
    )
  })

}
