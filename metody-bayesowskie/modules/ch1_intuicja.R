# ============================================================================
# CHAPTER 1: Intuicja Bayesa - prior, likelihood, posterior
# ============================================================================

ch1_ui <- tabPanel("1. Intuicja",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Zanim porównamy świat bayesowski z częstościowym:
       potrzebujemy trzech pojęć — prior, likelihood, posterior."
    ),

    div(class = "section-title", "Po co w ogóle Bayes?"),

    div(class = "narrative",
      p("W statystyce częstościowej odpowiadamy na pytanie:
         „jak prawdopodobne są dane przy założyciu H₀?‟ — to jest p-wartość.
         W statystyce bayesowskiej odpowiadamy na pytanie, które intuicyjnie częściej
         interesuje badacza: ", tags$b("„jak prawdopodobna jest hipoteza przy danych, które zebraliśmy?‟")),
      p("Cena tej wygody: musimy zdeklarować, co sądziliśmy ", tags$em("przed"),
         " zebraniem danych (prior). Dane aktualizują to przekonanie do posterior.")
    ),

    div(class = "callout-info",
      tags$strong("Twór, który to opisuje (twierdzenie Bayesa):"),
      withMathJax("$$P(\\theta \\mid \\text{dane}) \\;\\propto\\; \\underbrace{P(\\text{dane} \\mid \\theta)}_{\\text{likelihood}} \\;\\cdot\\; \\underbrace{P(\\theta)}_{\\text{prior}}$$")
    ),

    div(class = "section-title", "Na konkrecie: rzut monetą"),

    div(class = "narrative",
      p("Rzucamy monetą ", tags$em("n"), " razy i zliczamy orły. Szukana jest
         ", tags$b("θ"), " — rzeczywiste prawdopodobieństwo orła."),
      p("Widget niżej pozwala wybrać prior (przekonanie ", tags$em("zanim"),
         " rzucamy) oraz same dane (ile orłów w ilu rzutach).
         Zobaczysz, jak dane „obracają‟ priora w posterior.")
    ),

    div(class = "widget-block",
      h4("Prior → Likelihood → Posterior (beta-binomial)"),
      fluidRow(
        column(4,
          h5("Prior: Beta(α, β)"),
          sliderInput("ch1_alpha", "α (pseudo-sukcesy):",
                      min = 0.5, max = 20, value = 2, step = 0.5),
          sliderInput("ch1_beta", "β (pseudo-porażki):",
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
          sliderInput("ch1_trials", "Liczba rzutów n:",
                      min = 0, max = 200, value = 20, step = 1),
          sliderInput("ch1_successes", "Liczba orłów:",
                      min = 0, max = 200, value = 14, step = 1),
          hr(),
          checkboxInput("ch1_show_prior", "Pokaż prior", value = TRUE),
          checkboxInput("ch1_show_lik",   "Pokaż likelihood", value = TRUE),
          checkboxInput("ch1_show_post",  "Pokaż posterior", value = TRUE)
        ),
        column(8,
          plotOutput("ch1_plot", height = "420px"),
          uiOutput("ch1_summary")
        )
      )
    ),

    div(class = "callout-success",
      tags$strong("Aha-moment:"),
      " Posterior to kompromis między priorem a danymi.
       Im więcej danych, tym bardziej posterior przesuwa się w stronę likelihood —
       prior traci znaczenie (chyba że był bardzo silny)."
    ),

    div(class = "section-title", "Kiedy prior ma duże znaczenie?"),

    div(class = "narrative",
      p("Z widgetu widzisz dwie końcówki:"),
      tags$ul(
        tags$li(tags$b("Mało danych (n = 5, 10):"),
                 " prior mocno wpływa na posterior."),
        tags$li(tags$b("Dużo danych (n = 100+):"),
                 " prior ledwo widać — dane dominują."),
        tags$li(tags$b("Silny prior (np. α=20, β=20):"),
                 " „przytrzymuje‟ posterior blisko 0.5 nawet przy dużej próbie.")
      ),
      p("To jest właśnie ukryta przewaga Bayesa: ", tags$em("jeśli masz wiedzę sprzed badania
         (poprzednie publikacje, mechanizm biologiczny), możesz ją wprowadzić"),
         " — i jednocześnie to ryzyko: ", tags$em("jeśli prior jest błędny, psuje posterior przy małej próbie"), ".")
    ),

    div(class = "chapter-transition",
      p("Wiemy już, co to prior i posterior.
         Czas porównać bayesowski dowód (BF) z częstościowym (p-wartość)."),
      actionButton("ch1_next",
                   "Dalej: Bayes Factor vs p-wartość →",
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
      theta_label     = "θ (prawdopodobieństwo orła)",
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
      " średnia = ", round(prior_mean, 3),
      "  |  Beta(", input$ch1_alpha, ", ", input$ch1_beta, ")",
      tags$br(),
      tags$b("Likelihood (MLE):"),
      if (is.na(mle)) " brak danych" else paste0(" θ̂ = ", round(mle, 3),
                                                  " (", input$ch1_successes, "/",
                                                  input$ch1_trials, ")"),
      tags$br(),
      tags$b("Posterior:"),
      " średnia = ", round(post_mean, 3),
      "  |  95% HDI = [", round(hdi["lower"], 3), ", ",
      round(hdi["upper"], 3), "]",
      "  |  Beta(", round(a_post, 1), ", ", round(b_post, 1), ")"
    )
  })
}
