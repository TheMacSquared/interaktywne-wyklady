# ============================================================================
# CHAPTER 1: Intuicja Bayesa - prior, likelihood, posterior
# ============================================================================

ch1_ui <- tabPanel("1. Intuicja",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Zanim por\u00f3wnamy \u015bwiat bayesowski z cz\u0119sto\u015bciowym:
       potrzebujemy trzech poj\u0119\u0107 \u2014 prior, likelihood, posterior."
    ),

    div(class = "section-title", "Po co w og\u00f3le Bayes?"),

    div(class = "narrative",
      p("W statystyce cz\u0119sto\u015bciowej odpowiadamy na pytanie:
         \u201ejak prawdopodobne s\u0105 dane przy za\u0142o\u017cyciu H\u2080?\u201f \u2014 to jest p-warto\u015b\u0107.
         W statystyce bayesowskiej odpowiadamy na pytanie, kt\u00f3re intuicyjnie cz\u0119\u015bciej
         interesuje badacza: ", tags$b("\u201ejak prawdopodobna jest hipoteza przy danych, kt\u00f3re zebrali\u015bmy?\u201f")),
      p("Cena tej wygody: musimy zdeklarowa\u0107, co s\u0105dzili\u015bmy ", tags$em("przed"),
         " zebraniem danych (prior). Dane aktualizuj\u0105 to przekonanie do posterior.")
    ),

    div(class = "callout-info",
      tags$strong("Tw\u00f3r, kt\u00f3ry to opisuje (twierdzenie Bayesa):"),
      withMathJax("$$P(\\theta \\mid \\text{dane}) \\;\\propto\\; \\underbrace{P(\\text{dane} \\mid \\theta)}_{\\text{likelihood}} \\;\\cdot\\; \\underbrace{P(\\theta)}_{\\text{prior}}$$")
    ),

    div(class = "section-title", "Na konkrecie: rzut monet\u0105"),

    div(class = "narrative",
      p("Rzucamy monet\u0105 ", tags$em("n"), " razy i zliczamy or\u0142y. Szukana jest
         ", tags$b("\u03b8"), " \u2014 rzeczywiste prawdopodobie\u0144stwo or\u0142a."),
      p("Widget ni\u017cej pozwala wybra\u0107 prior (przekonanie ", tags$em("zanim"),
         " rzucamy) oraz same dane (ile or\u0142\u00f3w w ilu rzutach).
         Zobaczysz, jak dane \u201eobracaj\u0105\u201f priora w posterior.")
    ),

    div(class = "widget-block",
      h4("Prior \u2192 Likelihood \u2192 Posterior (beta-binomial)"),
      fluidRow(
        column(4,
          h5("Prior: Beta(\u03b1, \u03b2)"),
          sliderInput("ch1_alpha", "\u03b1 (pseudo-sukcesy):",
                      min = 0.5, max = 20, value = 2, step = 0.5),
          sliderInput("ch1_beta", "\u03b2 (pseudo-pora\u017cki):",
                      min = 0.5, max = 20, value = 2, step = 0.5),
          div(class = "preset-buttons",
            actionButton("ch1_prior_flat",     "Neutralny",
                         class = "btn-outline-secondary btn-sm"),
            actionButton("ch1_prior_fair",     "Uczciwa moneta",
                         class = "btn-outline-secondary btn-sm"),
            actionButton("ch1_prior_biased",   "Podejrzana",
                         class = "btn-outline-secondary btn-sm")
          ),
          hr(),
          h5("Dane (rzuty)"),
          sliderInput("ch1_trials", "Liczba rzut\u00f3w n:",
                      min = 0, max = 200, value = 20, step = 1),
          sliderInput("ch1_successes", "Liczba or\u0142\u00f3w:",
                      min = 0, max = 200, value = 14, step = 1),
          hr(),
          checkboxInput("ch1_show_prior", "Poka\u017c prior", value = TRUE),
          checkboxInput("ch1_show_lik",   "Poka\u017c likelihood", value = TRUE),
          checkboxInput("ch1_show_post",  "Poka\u017c posterior", value = TRUE)
        ),
        column(8,
          plotOutput("ch1_plot", height = "420px"),
          uiOutput("ch1_summary")
        )
      )
    ),

    div(class = "callout-success",
      tags$strong("Aha-moment:"),
      " Posterior to kompromis mi\u0119dzy priorem a danymi.
       Im wi\u0119cej danych, tym bardziej posterior przesuwa si\u0119 w stron\u0119 likelihood \u2014
       prior traci znaczenie (chyba \u017ce by\u0142 bardzo silny)."
    ),

    div(class = "section-title", "Kiedy prior ma du\u017ce znaczenie?"),

    div(class = "narrative",
      p("Z widgetu widzisz dwie ko\u0144c\u00f3wki:"),
      tags$ul(
        tags$li(tags$b("Ma\u0142o danych (n = 5, 10):"),
                 " prior mocno wp\u0142ywa na posterior."),
        tags$li(tags$b("Du\u017co danych (n = 100+):"),
                 " prior ledwo wida\u0107 \u2014 dane dominuj\u0105."),
        tags$li(tags$b("Silny prior (np. \u03b1=20, \u03b2=20):"),
                 " \u201eprzytrzymuje\u201f posterior blisko 0.5 nawet przy du\u017cej pr\u00f3bie.")
      ),
      p("To jest w\u0142a\u015bnie ukryta przewaga Bayesa: ", tags$em("je\u015bli masz wiedz\u0119 sprzed badania
         (poprzednie publikacje, mechanizm biologiczny), mo\u017cesz j\u0105 wprowadzi\u0107"),
         " \u2014 i jednocze\u015bnie to ryzyko: ", tags$em("je\u015bli prior jest b\u0142\u0119dny, psuje posterior przy ma\u0142ej pr\u00f3bie"), ".")
    ),

    div(class = "chapter-transition",
      p("Wiemy ju\u017c, co to prior i posterior.
         Czas por\u00f3wna\u0107 bayesowski dow\u00f3d (BF) z cz\u0119sto\u015bciowym (p-warto\u015b\u0107)."),
      actionButton("ch1_next",
                   "Dalej: Bayes Factor vs p-warto\u015b\u0107 \u2192",
                   class = "btn-primary btn-lg")
    )

  ))  # close column, fluidRow
)

ch1_server <- function(input, output, session) {

  # Presety priora
  observeEvent(input$ch1_prior_flat, {
    updateSliderInput(session, "ch1_alpha", value = 1)
    updateSliderInput(session, "ch1_beta",  value = 1)
  })
  observeEvent(input$ch1_prior_fair, {
    updateSliderInput(session, "ch1_alpha", value = 10)
    updateSliderInput(session, "ch1_beta",  value = 10)
  })
  observeEvent(input$ch1_prior_biased, {
    updateSliderInput(session, "ch1_alpha", value = 15)
    updateSliderInput(session, "ch1_beta",  value = 5)
  })

  # Walidacja: sukcesy <= rzuty
  observeEvent(input$ch1_trials, {
    if (input$ch1_successes > input$ch1_trials) {
      updateSliderInput(session, "ch1_successes", value = input$ch1_trials)
    }
    updateSliderInput(session, "ch1_successes",
                      max = input$ch1_trials)
  })

  bb_df <- reactive({
    beta_binomial_posterior(
      successes   = input$ch1_successes,
      trials      = input$ch1_trials,
      alpha_prior = input$ch1_alpha,
      beta_prior  = input$ch1_beta
    )
  })

  output$ch1_plot <- renderPlot({
    df <- bb_df()
    plot_prior_likelihood_posterior(
      df,
      theta_label     = "\u03b8 (prawdopodobie\u0144stwo or\u0142a)",
      show_prior      = input$ch1_show_prior,
      show_likelihood = input$ch1_show_lik,
      show_posterior  = input$ch1_show_post
    )
  })

  output$ch1_summary <- renderUI({
    df <- bb_df()
    # Statystyki posterior (analitycznie z Beta(alpha_post, beta_post))
    a_post <- unique(df$alpha_post)
    b_post <- unique(df$beta_post)
    post_mean <- a_post / (a_post + b_post)
    # HDI z probek
    samples <- rbeta(4000, a_post, b_post)
    hdi <- hdi_from_samples(samples, prob = 0.95)

    prior_mean <- input$ch1_alpha / (input$ch1_alpha + input$ch1_beta)
    mle <- if (input$ch1_trials > 0)
             input$ch1_successes / input$ch1_trials else NA

    div(class = "callout-info",
      tags$b("Prior:"),
      " \u015brednia = ", round(prior_mean, 3),
      "  |  Beta(", input$ch1_alpha, ", ", input$ch1_beta, ")",
      tags$br(),
      tags$b("Likelihood (MLE):"),
      if (is.na(mle)) " brak danych" else paste0(" \u03b8\u0302 = ", round(mle, 3),
                                                  " (", input$ch1_successes, "/",
                                                  input$ch1_trials, ")"),
      tags$br(),
      tags$b("Posterior:"),
      " \u015brednia = ", round(post_mean, 3),
      "  |  95% HDI = [", round(hdi["lower"], 3), ", ",
      round(hdi["upper"], 3), "]",
      "  |  Beta(", round(a_post, 1), ", ", round(b_post, 1), ")"
    )
  })
}
