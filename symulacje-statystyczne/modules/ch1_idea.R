# ============================================================================
# CHAPTER 1: Idea resamplingowa
# ============================================================================

ch1_ui <- tabPanel("1. Idea resamplingowa",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Znamy ju\u017c klasyczne testy i przedzia\u0142y ufno\u015bci oparte na za\u0142o\u017ceniach o rozk\u0142adach.
       Czas pozna\u0107 podej\u015bcie, kt\u00f3re z tych za\u0142o\u017ce\u0144 rezygnuje."
    ),

    div(class = "section-title",
        "Sk\u0105d bior\u0105 si\u0119 klasyczne metody?"),

    div(class = "narrative",
      p("Klasyczna statystyka opiera si\u0119 na ", tags$b("za\u0142o\u017ceniach o rozk\u0142adzie"),
        " populacji lub na Centralnym Twierdzeniu Granicznym (CTG). Na przyk\u0142ad:"),
      tags$ul(
        tags$li("Test t wymaga normalno\u015bci lub du\u017cej pr\u00f3by (CTG)"),
        tags$li("Przedzia\u0142 ufno\u015bci dla \u015bredniej zakada symetri\u0119 b\u0142\u0119du"),
        tags$li("Wzory na b\u0142\u0105d standardowy dotycz\u0105 konkretnych statystyk (mean, proportion)")
      )
    ),

    div(class = "callout-warning",
      tags$strong("Problem:"),
      " Co je\u015bli dane s\u0105 silnie sko\u015bne i pr\u00f3ba ma\u0142a? Co je\u015bli chcemy przedzia\u0142 ufno\u015bci
       dla mediany albo percentyla? Klasyczne wzory ",
      tags$b("nie istniej\u0105"), " lub s\u0105 niedok\u0142adne."
    ),

    div(class = "section-title", "Idea resamplingowa"),

    div(class = "narrative",
      p("Kluczowy pomys\u0142: ", tags$b("traktuj pr\u00f3b\u0119 jako mini-populacj\u0119"),
        " i losuj z niej wielokrotnie."),
      p("Je\u015bli pr\u00f3ba jest reprezentatywna dla populacji, to losowanie z pr\u00f3by
        imituje losowanie z populacji. Mo\u017cemy wtedy:", tags$b(" symulacyjnie "),
        "uzyska\u0107 to, co klasyczna statystyka oblicza analitycznie.")
    ),

    div(class = "callout-info",
      tags$strong("S\u0142owniczek:"),
      tags$ul(
        tags$li(tags$b("Pr\u00f3ba bootstrapowa:"),
                " losowanie ze zwracaniem z oryginalnej pr\u00f3by (n obserwacji \u2192 n obserwacji,
                  cz\u0119\u015b\u0107 si\u0119 powtarza)"),
        tags$li(tags$b("Rozk\u0142ad bootstrapowy:"),
                " empiryczny rozk\u0142ad statystyki z wielu pr\u00f3b bootstrapowych"),
        tags$li(tags$b("Test permutacyjny:"),
                " przetasowanie etykiet grup, aby zasymulowa\u0107 \u015bwiat H\u2080"),
        tags$li(tags$b("Jackknife:"),
                " leave-one-out \u2014 n pr\u00f3b, ka\u017cda bez jednej obserwacji"),
        tags$li(tags$b("Cross-validation:"),
                " k-krotny podzia\u0142 na trening/test do oceny modelu"),
        tags$li(tags$b("Monte Carlo:"),
                " symulacja z rozk\u0142adu (np. mocy testu lub rozk\u0142adu pod H\u2080)")
      )
    ),

    div(class = "section-title", "Mapa metod resamplingowych"),

    div(class = "narrative",
      p("Pi\u0119\u0107 metod, kt\u00f3re poznamy w tym rozdziale, r\u00f3\u017cni\u0105 si\u0119 celem
        i sposobem resamplingu:")
    ),

    tags$table(class = "decision-table",
      tags$thead(tags$tr(
        tags$th("Metoda"),
        tags$th("Cel"),
        tags$th("Spos\u00f3b resamplingu")
      )),
      tags$tbody(
        tags$tr(
          tags$td(tags$b("Bootstrap")),
          tags$td("Przedzia\u0142 ufno\u015bci dla dowolnej statystyki"),
          tags$td("Losowanie ze zwracaniem z pr\u00f3by")
        ),
        tags$tr(
          tags$td(tags$b("Jackknife")),
          tags$td("Estymacja obci\u0105\u017cenia i SE"),
          tags$td("Leave-one-out: n pr\u00f3b bez kolejnych obserwacji")
        ),
        tags$tr(
          tags$td(tags$b("Test permutacyjny")),
          tags$td("Test hipotezy bez za\u0142o\u017ce\u0144"),
          tags$td("Przetasowanie etykiet grup")
        ),
        tags$tr(
          tags$td(tags$b("Cross-validation")),
          tags$td("Ocena jako\u015bci modelu predykcyjnego"),
          tags$td("k-krotny podzia\u0142 trening/test")
        ),
        tags$tr(
          tags$td(tags$b("Monte Carlo")),
          tags$td("Moc testu lub rozk\u0142ad pod H\u2080"),
          tags$td("Losowanie z parametrycznego rozk\u0142adu")
        )
      )
    ),

    # ========================================================================
    # WIDGET 1: Proba bootstrapowa w akcji
    # ========================================================================
    div(class = "section-title", "Pr\u00f3ba bootstrapowa w akcji"),

    div(class = "narrative",
      p("Zacznijmy od najmniejszego kroku: jednej pr\u00f3by bootstrapowej.
        Pobierz pr\u00f3b\u0119 z populacji, a nast\u0119pnie wylosuj z niej jedn\u0105 pr\u00f3b\u0119
        bootstrapow\u0105 (losowanie ze zwracaniem)."),
      p("Kolor kropek pokazuje, ile razy dana obserwacja zosta\u0142a wylosowana:",
        tags$span(style = "color:#bdc3c7; font-weight:bold;", " szara"),
        " = pomin\u0105\u0142 (0 razy),",
        tags$span(style = "color:#3498db; font-weight:bold;", " niebieska"),
        " = raz,",
        tags$span(style = "color:#f39c12; font-weight:bold;", " pomara\u0144czowa"),
        " = dwa razy lub wi\u0119cej.")
    ),

    div(class = "widget-block",
      h4("Jedna pr\u00f3ba bootstrapowa"),
      fluidRow(
        column(4,
          selectInput("ch1_dist", "Kszta\u0142t danych:",
            choices = c(
              "Prawoskos\u015bny (Gamma)" = "skewed",
              "Normalny"               = "normal",
              "Dwumodalny"             = "bimodal"
            ),
            selected = "skewed"
          ),
          sliderInput("ch1_n_orig", "Wielko\u015b\u0107 pr\u00f3by (n):",
                      min = 8, max = 30, value = 15, step = 1),
          hr(),
          actionButton("ch1_draw_orig", "Pobierz pr\u00f3b\u0119",
                       class = "btn-primary", width = "100%"),
          br(), br(),
          actionButton("ch1_resample_one", "+ Nowa pr\u00f3ba bootstrapowa",
                       class = "btn-warning", width = "100%"),
          br(), br(),
          actionButton("ch1_resample_reset", "Wyczy\u015b\u0107 pr\u00f3by",
                       class = "btn-outline-secondary", width = "100%"),
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
    div(class = "section-title", "Rozk\u0142ad bootstrapowy"),

    div(class = "narrative",
      p("Jedna pr\u00f3ba bootstrapowa daje jedn\u0105 warto\u015b\u0107 \u015bredniej.
        Tysi\u0105c pr\u00f3b daje rozk\u0142ad \u015bredniej. To w\u0142a\u015bnie ",
        tags$b("rozk\u0142ad bootstrapowy"), "."),
      p("Jego odchylenie standardowe jest ",
        tags$b("b\u0142\u0119dem standardowym"),
        " statystyki \u2014 dok\u0142adnie to, co wzory analityczne
        pr\u00f3buj\u0105 obliczy\u0107.")
    ),

    div(class = "widget-block",
      h4("Budowanie rozk\u0142adu bootstrapowego"),
      p(class = "text-muted",
        "Najpierw pobierz pr\u00f3b\u0119 (powy\u017cej), nast\u0119pnie kliknij \"Uruchom\"."),
      fluidRow(
        column(4,
          sliderInput("ch1_B", "Liczba pr\u00f3b bootstrapowych (B):",
                      min = 50, max = 2000, value = 500, step = 50),
          actionButton("ch1_build_dist", "Uruchom bootstrap",
                       class = "btn-primary", width = "100%"),
          br(), br(),
          actionButton("ch1_build_reset", "Reset",
                       class = "btn-outline-secondary", width = "100%"),
          br(), br(),
          uiOutput("ch1_boot_stats")
        ),
        column(8,
          plotOutput("ch1_boot_dist_plot", height = "320px")
        )
      )
    ),

    div(class = "callout-success",
      tags$strong("Aha-moment:"),
      " Odchylenie standardowe rozk\u0142adu bootstrapowego = ",
      tags$b("b\u0142\u0105d standardowy statystyki"),
      ". W\u0142a\u015bnie to klasyczne wzory pr\u00f3buj\u0105 obliczy\u0107 analitycznie \u2014
       bootstrap oblicza to symulacyjnie!"
    ),

    div(class = "chapter-transition",
      p("Dalej: jak z rozk\u0142adu bootstrapowego budowa\u0107 przedzia\u0142y ufno\u015bci"),
      actionButton("ch1_next",
                   "Dalej \u2192 2. Bootstrap \u2014 przedzia\u0142y",
                   class = "btn-primary btn-lg")
    )

  ))
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
                 label = "Kliknij 'Pobierz pr\u00f3b\u0119'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else if (is.null(rs)) {
      # Pokaz tylko oryginalna probe
      df <- data.frame(x = orig, y = 0)
      ggplot(df, aes(x = x, y = y)) +
        geom_jitter(color = col_primary, height = 0.1, size = 3.5, alpha = 0.9) +
        geom_vline(xintercept = mean(orig), color = col_dark,
                   linewidth = 1.2, linetype = "dashed") +
        annotate("text", x = mean(orig), y = Inf,
                 label = paste0("\u015br. = ", round(mean(orig), 2)),
                 vjust = -0.3, hjust = -0.1, color = col_dark, size = 4) +
        scale_y_continuous(breaks = 0, labels = "Oryginalna pr\u00f3ba") +
        labs(title = paste0("Pr\u00f3ba (n = ", length(orig), ")"),
             x = "Warto\u015b\u0107", y = NULL) +
        theme_educational() +
        theme(axis.text.y = element_text(size = 12))
    } else {
      plot_bootstrap_step(orig, rs, col_primary, col_warning, col_dark)
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
      div(class = "stat-box", style = paste0("background:", col_dark, ";"),
          paste0("n = ", length(orig))),
      div(class = "stat-box", style = paste0("background:", col_primary, ";"),
          paste0("\u015br. oryg. = ", round(mean(orig), 2)))
    )
    if (!is.null(rs)) {
      last_rs <- rs[[length(rs)]]
      tag_list <- c(tag_list, list(
        div(class = "stat-box", style = paste0("background:", col_warning, ";"),
            paste0("\u015br. boot. #", length(rs), " = ", round(mean(last_rs), 2))),
        div(class = "stat-box", style = paste0("background:", col_dark, "; opacity:0.7;"),
            paste0("Liczba pr\u00f3b: ", length(rs)))
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
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      ci <- bootstrap_ci_percentile(result, conf_level = 0.95)
      plot_bootstrap_distribution(result, ci,
                                   stat_label = "\u015aredniane bootstrapowe",
                                   col_primary = col_primary,
                                   col_secondary = col_secondary,
                                   col_success = col_success)
    }
  })

  output$ch1_boot_stats <- renderUI({
    result <- ch1_boot_dist()
    if (is.null(result)) return(NULL)
    tagList(
      div(class = "stat-box", style = paste0("background:", col_dark, ";"),
          paste0("B = ", result$B)),
      div(class = "stat-box", style = paste0("background:", col_secondary, ";"),
          paste0("obs. = ", round(result$observed, 3))),
      div(class = "stat-box", style = paste0("background:", col_primary, ";"),
          paste0("SE = ", round(result$se, 4)))
    )
  })

}
