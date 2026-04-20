# ============================================================================
# CHAPTER 2: Bayes Factor vs p-value
# ============================================================================

ch2_ui <- tabPanel("2. BF vs p-warto\u015b\u0107",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Mamy prior i posterior. Teraz druga kluczowa idea bayesowska:
       miara \u201esi\u0142y dowodu\u201f \u2014 Bayes Factor \u2014 i jej porównanie z p-warto\u015bci\u0105."
    ),

    div(class = "section-title", "P-warto\u015b\u0107 odpowiada na inne pytanie ni\u017c my\u015blisz"),

    div(class = "narrative",
      p(tags$b("p-warto\u015b\u0107"), " = P(dane \u2265 obserwowane | H\u2080 prawdziwe).
         To warunkowe prawdopodobie\u0144stwo ", tags$em("danych"), " przy za\u0142o\u017ceniu
         H\u2080 \u2014 nie m\u00f3wi nam, jak prawdopodobna jest sama H\u2080."),
      p(tags$b("BF\u2081\u2080"), " = P(dane | H\u2081) / P(dane | H\u2080).
         To stosunek \u201ejak dobrze dane pasuj\u0105 do H\u2081\u201f do \u201ejak dobrze pasuj\u0105 do H\u2080\u201f. ",
         tags$b("BF\u2081\u2080 = 10"), " znaczy, \u017ce dane s\u0105 10\u00d7 bardziej prawdopodobne pod H\u2081 ni\u017c pod H\u2080.")
    ),

    div(class = "callout-info",
      tags$b("Skala Jeffreysa (konwencja dla BF\u2081\u2080):"),
      tags$ul(
        tags$li("1 \u2013 3: dow\u00f3d anekdotyczny (s\u0142aby)"),
        tags$li("3 \u2013 10: dow\u00f3d umiarkowany"),
        tags$li("10 \u2013 30: dow\u00f3d silny"),
        tags$li("30 \u2013 100: dow\u00f3d bardzo silny"),
        tags$li("> 100: dow\u00f3d ekstremalny")
      ),
      "Symetrycznie: BF\u2081\u2080 < 1 czytamy jako 1/BF\u2081\u2080 dow\u00f3d za H\u2080."
    ),

    div(class = "section-title", "Porównanie na jednej próbie"),

    div(class = "narrative",
      p("Generujemy jedn\u0105 pr\u00f3b\u0119 (z prawdziwej \u015bredniej \u03bc), testujemy H\u2080: \u03bc = 0.
         Lewa kolumna \u2014 klasyczny test t i p-warto\u015b\u0107.
         Prawa kolumna \u2014 ttestBF i BF\u2081\u2080. Te same dane, dwie odpowiedzi.")
    ),

    div(class = "widget-block",
      h4("Te same dane, dwa paradygmaty"),
      fluidRow(column(12,
        fluidRow(
          column(4,
            sliderInput("ch2_n", "Wielko\u015b\u0107 pr\u00f3by n:",
                        min = 5, max = 200, value = 30, step = 5)
          ),
          column(4,
            sliderInput("ch2_true_mu", "Prawdziwe \u03bc (ukryte):",
                        min = -1.5, max = 1.5, value = 0.3, step = 0.05)
          ),
          column(4,
            br(),
            actionButton("ch2_draw", "\u21bb Nowa pr\u00f3ba",
                         class = "btn-primary", width = "100%")
          )
        )
      )),

      fluidRow(
        column(6,
          div(class = "panel-frequentist",
            h5("Podej\u015bcie cz\u0119sto\u015bciowe"),
            plotOutput("ch2_freq_plot", height = "260px"),
            uiOutput("ch2_freq_result")
          )
        ),
        column(6,
          div(class = "panel-bayesian",
            h5("Podej\u015bcie bayesowskie"),
            plotOutput("ch2_bayes_plot", height = "260px"),
            uiOutput("ch2_bayes_result")
          )
        )
      ),

      div(class = "callout-info",
        uiOutput("ch2_comparison")
      )
    ),

    div(class = "section-title", "Paradoks Lindleya: du\u017ca pr\u00f3ba"),

    div(class = "narrative",
      p("Ustaw n = 200 i ma\u0142\u0105 prawdziw\u0105 r\u00f3\u017cnic\u0119 (np. \u03bc = 0.1).
         Obserwuj: p-warto\u015b\u0107 szybko spada poni\u017cej 0.05 (\u201eistotny wynik\u201f),
         ale BF mo\u017ce wci\u0105\u017c m\u00f3wi\u0107 \u201es\u0142aby dow\u00f3d\u201f, a czasem nawet wskazywa\u0107 na H\u2080!"),
      p("To nie jest b\u0142\u0105d \u2014 to dwa r\u00f3\u017cne pytania.
         Cz\u0119sto\u015bciowo \u201eistotne\u201f znaczy \u201etrudno by to zobaczy\u0107 pod H\u2080\u201f;
         bayesowsko \u201esilny dow\u00f3d\u201f znaczy \u201edane znacznie lepiej pasuj\u0105 do H\u2081 ni\u017c H\u2080\u201f.
         To dwa r\u00f3\u017cne aspekty tego samego obserwowanego wyniku.")
    ),

    div(class = "callout-success",
      tags$strong("Kiedy BF i p zgadzaj\u0105 si\u0119:"),
      " zwykle przy ma\u0142ych-\u015brednich pr\u00f3bach i wyra\u017anych efektach.",
      tags$br(),
      tags$strong("Kiedy mog\u0105 si\u0119 r\u00f3\u017cni\u0107:"),
      " ma\u0142e efekty + du\u017ca pr\u00f3ba (wtedy p jest niskie, BF umiarkowane),
         lub bardzo ma\u0142a pr\u00f3ba (BF realistycznie m\u00f3wi \u201enie wiem\u201f, p potrafi by\u0107 myl\u0105ce)."
    ),

    div(class = "chapter-transition",
      p("Wiemy, \u017ce p i BF to r\u00f3\u017cne odpowiedzi.
         A co z przedzia\u0142ami? Czy 95% CI i 95% HDI to to samo?"),
      actionButton("ch2_next",
                   "Dalej: HDI vs CI \u2192",
                   class = "btn-primary btn-lg")
    )

  )) # column, fluidRow
)

ch2_server <- function(input, output, session) {

  sample_data <- reactiveVal(NULL)

  # Wstepna próba
  observe({
    if (is.null(sample_data())) {
      x <- rnorm(input$ch2_n, mean = input$ch2_true_mu, sd = 1)
      sample_data(x)
    }
  })

  # Nowa próba na przycisk lub zmiane parametrów
  observeEvent(list(input$ch2_draw, input$ch2_n, input$ch2_true_mu), {
    x <- rnorm(input$ch2_n, mean = input$ch2_true_mu, sd = 1)
    sample_data(x)
  }, ignoreInit = TRUE)

  bf_result <- reactive({
    x <- sample_data()
    req(x)
    compute_bf_one_sample(x, mu0 = 0)
  })

  # LEWA KOLUMNA - freq
  output$ch2_freq_plot <- renderPlot({
    x <- sample_data()
    req(x)
    plot_sample_data(x, mu0 = 0, title = "Pr\u00f3ba + H\u2080: \u03bc = 0",
                     col_freq = col_frequentist)
  })

  output$ch2_freq_result <- renderUI({
    r <- bf_result()
    p_info <- format_pval_pl(r$p_value)
    ci <- r$ci_freq
    tagList(
      div(class = "callout-info",
        tags$b("t-test Studenta (H\u2080: \u03bc = 0)"), tags$br(),
        "t = ", round(r$t_statistic, 3),
        "  |  df = ", round(r$df, 1), tags$br(),
        HTML(p_info$decision), tags$br(),
        tags$b("95% CI: "), "[", round(ci[1], 3), ", ", round(ci[2], 3), "]"
      )
    )
  })

  # PRAWA KOLUMNA - bayes
  output$ch2_bayes_plot <- renderPlot({
    r <- bf_result()
    plot_bf_scale(r$bf10)
  })

  output$ch2_bayes_result <- renderUI({
    r <- bf_result()
    interp <- interpret_bf(r$bf10)
    hdi <- r$hdi
    tagList(
      div(class = "callout-info",
        tags$b("Bayes Factor (H\u2081: \u03bc \u2260 0 vs H\u2080: \u03bc = 0)"), tags$br(),
        "BF\u2081\u2080 = ", format_bf(r$bf10), tags$br(),
        tags$b("Interpretacja: "), interp$short_summary, tags$br(),
        tags$b("Mediana posterior \u03bc: "), round(r$posterior_median, 3), tags$br(),
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
      "Na tej pr\u00f3bie (n = ", r$n, "): p = ",
      round(r$p_value, 4), " vs BF\u2081\u2080 = ", format_bf(r$bf10), ". "
    )
    verdict <- if (agreement) {
      "Oba podej\u015bcia prowadz\u0105 do zgodnej decyzji."
    } else if (r$p_value < 0.05 && r$bf10 < 3) {
      "Cz\u0119sto\u015bciowo: istotny; Bayesowsko: dow\u00f3d s\u0142aby \u2014
       klasyczny znak paradoksu Lindleya (n du\u017ce, efekt ma\u0142y)."
    } else if (r$p_value >= 0.05 && r$bf10 > 3) {
      "Cz\u0119sto\u015bciowo: nieistotny; Bayesowsko: umiarkowany dow\u00f3d za H\u2081 \u2014
       p-warto\u015b\u0107 \u201epochowa\u0142a\u201f efekt, kt\u00f3ry BF wy\u0142apuje."
    } else {
      "Podej\u015bcia dostarczaj\u0105 r\u00f3\u017cnych odcieni odpowiedzi."
    }
    tagList(tags$b("Co to znaczy: "), base_text, verdict)
  })
}
