# ============================================================================
# CHAPTER 2: Bayes Factor vs p-value
# ============================================================================

ch2_ui <- lecture_chapter(
  id = "ch-bf-vs-p",
  num = "02",
  title = "BF vs p-wartość",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 02 · Metody bayesowskie",
      num    = "02",
      title  = "BF vs p-wartość",
      lead   = "Bayes Factor odpowiada na inne pytanie niż p-wartość i pozwala ważyć dowód za modelami."
    ),

    lc_feedback(type = "info",
      "Mamy prior i posterior. Teraz druga kluczowa idea bayesowska:
       miara „siły dowodu‟ — Bayes Factor — i jej porównanie z p-wartością."
    ),

    lc_h2("ch2-sec-01", "P-wartość odpowiada na inne pytanie niż myślisz"),

    tagList(
      p(tags$b("p-wartość"), " = P(dane ≥ obserwowane | H₀ prawdziwe).
         To warunkowe prawdopodobieństwo ", tags$em("danych"), " przy założeniu
         H₀ — nie mówi nam, jak prawdopodobna jest sama H₀."),
      p(tags$b("BF₁₀"), " = P(dane | H₁) / P(dane | H₀).
         To stosunek „jak dobrze dane pasują do H₁‟ do „jak dobrze pasują do H₀‟. ",
         tags$b("BF₁₀ = 10"), " znaczy, że dane są 10× bardziej prawdopodobne pod H₁ niż pod H₀.")
    ),

    lc_feedback(type = "info",
      tags$b("Skala Jeffreysa (konwencja dla BF₁₀):"),
      tags$ul(
        tags$li("1 – 3: dowód anekdotyczny (słaby)"),
        tags$li("3 – 10: dowód umiarkowany"),
        tags$li("10 – 30: dowód silny"),
        tags$li("30 – 100: dowód bardzo silny"),
        tags$li("> 100: dowód ekstremalny")
      ),
      "Symetrycznie: BF₁₀ < 1 czytamy jako 1/BF₁₀ dowód za H₀."
    ),

    lc_h2("ch2-sec-02", "Porównanie na jednej próbie"),

    tagList(
      p("Generujemy jedną próbę (z prawdziwej średniej μ), testujemy H₀: μ = 0.
         Lewa kolumna — klasyczny test t i p-wartość.
         Prawa kolumna — ttestBF i BF₁₀. Te same dane, dwie odpowiedzi.")
    ),

    figure_panel(label = "Ryc. 2.1", title = "Te same dane, dwa paradygmaty",
      fluidRow(column(12,
        fluidRow(
          column(4,
            sliderInput("ch2_n", "Wielkość próby n:",
                        min = 5, max = 200, value = 30, step = 5)
          ),
          column(4,
            sliderInput("ch2_true_mu", "Prawdziwe μ (ukryte):",
                        min = -1.5, max = 1.5, value = 0.3, step = 0.05)
          ),
          column(4,
            br(),
            actionButton("ch2_draw", "↻ Nowa próba",
                         class = "lc-btn-primary", width = "100%")
          )
        )
      )),

      fluidRow(
        column(6,
          div(class = "panel-frequentist",
            h5("Podejście częstościowe"),
            zoom_plot_ui("ch2_freq_plot", height = "260px"),
            uiOutput("ch2_freq_result")
          )
        ),
        column(6,
          div(class = "panel-bayesian",
            h5("Podejście bayesowskie"),
            zoom_plot_ui("ch2_bayes_plot", height = "260px"),
            uiOutput("ch2_bayes_result")
          )
        )
      ),

      lc_feedback(type = "info",
        uiOutput("ch2_comparison")
      )
    ),

    lc_h2("ch2-sec-03", "Paradoks Lindleya: duża próba"),

    tagList(
      p("Ustaw n = 200 i małą prawdziwą różnicę (np. μ = 0.1).
         Obserwuj: p-wartość szybko spada poniżej 0.05 („istotny wynik‟),
         ale BF może wciąż mówić „słaby dowód‟, a czasem nawet wskazywać na H₀!"),
      p("To nie jest błąd — to dwa różne pytania.
         Częstościowo „istotne‟ znaczy „trudno by to zobaczyć pod H₀‟;
         bayesowsko „silny dowód‟ znaczy „dane znacznie lepiej pasują do H₁ niż H₀‟.
         To dwa różne aspekty tego samego obserwowanego wyniku.")
    ),

    lc_feedback(type = "ok",
      tags$strong("Kiedy BF i p zgadzają się:"),
      " zwykle przy małych-średnich próbach i wyraźnych efektach.",
      tags$br(),
      tags$strong("Kiedy mogą się różnić:"),
      " małe efekty + duża próba (wtedy p jest niskie, BF umiarkowane),
         lub bardzo mała próba (BF realistycznie mówi „nie wiem‟, p potrafi być mylące)."
    ),

    lc_chapter_next(
      num = "03",
      title = "HDI vs CI",
      lead = "różnica między przedziałem wiarygodności i ufności.",
      target_id = "ch-hdi-vs-ci"
    )

  )
)

ch2_server <- function(input, output, session) {

  sample_data <- reactiveVal(NULL)

  # Wstepna próba
  observe({
    if (is.null(sample_data())) {
      x <- rnorm(bayes_input(input$ch2_n, 30),
                 mean = bayes_input(input$ch2_true_mu, 0.2), sd = 1)
      sample_data(x)
    }
  })

  # Nowa próba na przycisk lub zmiane parametrów
  observeEvent(list(input$ch2_draw, input$ch2_n, input$ch2_true_mu), {
    x <- rnorm(bayes_input(input$ch2_n, 30),
               mean = bayes_input(input$ch2_true_mu, 0.2), sd = 1)
    sample_data(x)
  }, ignoreInit = TRUE)

  bf_result <- reactive({
    x <- sample_data()
    req(x)
    compute_bf_one_sample(x, mu0 = 0)
  })

  # LEWA KOLUMNA - freq
  zoom_plot_server("ch2_freq_plot", reactive({
    x <- sample_data()
    req(x)
    plot_sample_data(x, mu0 = 0, title = "Próba + H0: mu = 0",
                     col_freq = bayes_freq)
  }))

  output$ch2_freq_result <- renderUI({
    r <- bf_result()
    p_info <- format_pval_pl(r$p_value)
    ci <- r$ci_freq
    tagList(
      lc_feedback(type = "info",
        tags$b("t-test Studenta (H₀: μ = 0)"), tags$br(),
        "t = ", round(r$t_statistic, 3),
        "  |  df = ", round(r$df, 1), tags$br(),
        HTML(p_info$decision), tags$br(),
        tags$b("95% CI: "), "[", round(ci[1], 3), ", ", round(ci[2], 3), "]"
      )
    )
  })

  # PRAWA KOLUMNA - bayes
  zoom_plot_server("ch2_bayes_plot", reactive({
    r <- bf_result()
    plot_bf_scale(r$bf10)
  }))

  output$ch2_bayes_result <- renderUI({
    r <- bf_result()
    interp <- interpret_bf(r$bf10)
    hdi <- r$hdi
    tagList(
      lc_feedback(type = "info",
        tags$b("Bayes Factor (H₁: μ ≠ 0 vs H₀: μ = 0)"), tags$br(),
        "BF₁₀ = ", format_bf(r$bf10), tags$br(),
        tags$b("Interpretacja: "), interp$short_summary, tags$br(),
        tags$b("Mediana posterior μ: "), round(r$posterior_median, 3), tags$br(),
        tags$b("95% HDI: "), "[", round(hdi["lower"], 3), ", ",
        round(hdi["upper"], 3), "]"
      )
    )
  })

  # Narracja porownawcza
  output$ch2_comparison <- renderUI({
    r <- bf_result()
    agreement <- (r$p_value < 0.05 && r$bf10 > 3) ||
                  (r$p_value >= 0.05 && r$bf10 < 3)
    base_text <- paste0(
      "Na tej próbie (n = ", r$n, "): p = ",
      round(r$p_value, 4), " vs BF₁₀ = ", format_bf(r$bf10), ". "
    )
    verdict <- if (agreement) {
      "Oba podejścia prowadzą do zgodnej decyzji."
    } else if (r$p_value < 0.05 && r$bf10 < 3) {
      "Częstościowo: istotny; Bayesowsko: dowód słaby —
       klasyczny znak paradoksu Lindleya (n duże, efekt mały)."
    } else if (r$p_value >= 0.05 && r$bf10 > 3) {
      "Częstościowo: nieistotny; Bayesowsko: umiarkowany dowód za H₁ —
       p-wartość „pochowała‟ efekt, który BF wyłapuje."
    } else {
      "Podejścia dostarczają różnych odcieni odpowiedzi."
    }
    tagList(tags$b("Co to znaczy: "), base_text, verdict)
  })
}
