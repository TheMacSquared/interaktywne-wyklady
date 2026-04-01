# ============================================================================
# CHAPTER 6: Centralne Twierdzenie Graniczne
# ============================================================================

ch6_ui <- tabPanel("6. Centralne Tw. Graniczne",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Rozk\u0142ad normalny pojawia si\u0119 wsz\u0119dzie. Ale dlaczego?
       Odpowied\u017a to jedno z najwa\u017cniejszych twierdze\u0144 w statystyce."
    ),

    div(class = "section-title", "Centralne Twierdzenie Graniczne (CTG)"),

    div(class = "narrative",
      p("Centralne Twierdzenie Graniczne m\u00f3wi, \u017ce:"),
      div(class = "callout-success",
        tags$strong("CTG:"),
        " Je\u015bli wezmiesz pr\u00f3b\u0119 n obserwacji z ", tags$b("dowolnego"),
        " rozk\u0142adu (o sko\u0144czonej wariancji) i obliczysz \u015bredni\u0105,
        to rozk\u0142ad tej \u015bredniej b\u0119dzie zbiega\u0142 do ", tags$b("normalnego"),
        " wraz ze wzrostem n."
      ),
      p("To wyja\u015bnia, dlaczego rozk\u0142ad normalny jest wsz\u0119dzie \u2014 wiele
        zjawisk naturalnych to suma wielu drobnych, niezale\u017cnych czynnik\u00f3w.
        Zobaczmy to na w\u0142asne oczy!")
    ),

    # ========================================================================
    # WIDGET 1: Eksperyment CLT (kluczowy!)
    # ========================================================================
    div(class = "section-title", "Eksperyment CLT"),

    div(class = "widget-block",
      h4("Symulacja: \u015brednie z dowolnego rozk\u0142adu \u2192 normalny"),
      fluidRow(
        column(4,
          selectInput("ch6_pop_dist", "Rozk\u0142ad populacji:",
            choices = c(
              "Jednostajny"         = "uniform",
              "Wyk\u0142adniczy (sko\u015bny)" = "exponential",
              "Dwumodalny"          = "bimodal",
              "U-kszta\u0142tny"          = "u_shape",
              "Kostka (dyskretny)"  = "die"
            ),
            selected = "exponential"
          ),
          sliderInput("ch6_sample_size", "Wielko\u015b\u0107 pr\u00f3by (n):",
                      min = 1, max = 100, value = 5, step = 1),
          hr(),
          div(style = "display: flex; flex-direction: column; gap: 8px;",
            actionButton("ch6_take_1", "Pobierz 1 pr\u00f3b\u0119",
                         class = "btn-primary", width = "100%"),
            actionButton("ch6_take_100", "Pobierz 100 pr\u00f3b",
                         class = "btn-primary", width = "100%"),
            actionButton("ch6_take_1000", "Pobierz 1000 pr\u00f3b",
                         class = "btn-warning", width = "100%"),
            hr(),
            actionButton("ch6_reset", "Reset",
                         class = "btn-outline-secondary", width = "100%")
          ),
          br(),
          uiOutput("ch6_sample_count")
        ),
        column(8,
          plotOutput("ch6_pop_plot", height = "180px"),
          plotOutput("ch6_means_plot", height = "300px"),
          uiOutput("ch6_means_stats")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Aha-moment:"),
      " Zmie\u0144 rozk\u0142ad populacji na najbardziej 'dziki' jaki znajdziesz
        (U-kszta\u0142tny, dwumodalny). Potem zbi\u00f3r 1000 pr\u00f3b.
        Histogram \u015brednich i tak stanie si\u0119 dzwonem!"
    ),

    # ========================================================================
    # WIDGET 2: Wplyw wielkosci proby
    # ========================================================================
    div(class = "section-title", "Wp\u0142yw wielko\u015bci pr\u00f3by"),

    div(class = "narrative",
      p("Im wi\u0119ksza pr\u00f3ba n, tym szybciej rozk\u0142ad \u015brednich staje si\u0119
        normalny. Zobaczmy to por\u00f3wnuj\u0105c r\u00f3\u017cne n obok siebie.")
    ),

    div(class = "widget-block",
      h4("Rozk\u0142ad \u015brednich z Exp(\u03bb=0.5) dla r\u00f3\u017cnych n"),
      selectInput("ch6_effect_dist", "Rozk\u0142ad populacji:",
        choices = c(
          "Wyk\u0142adniczy" = "exponential",
          "Jednostajny"  = "uniform",
          "U-kszta\u0142tny"  = "u_shape"
        ),
        selected = "exponential"
      ),
      plotOutput("ch6_effect_plot", height = "350px")
    ),

    # ========================================================================
    # WIDGET 3: Dlaczego to dziala?
    # ========================================================================
    div(class = "section-title", "Dlaczego to dzia\u0142a? \u2014 intuicja"),

    div(class = "widget-block",
      h4("Od jednej obserwacji do \u015bredniej z 30"),
      fluidRow(
        column(4,
          actionButton("ch6_why_step1", "1. Jedna obserwacja",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("ch6_why_step2", "2. \u015arednia z 2",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("ch6_why_step3", "3. \u015arednia z 5",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("ch6_why_step4", "4. \u015arednia z 30",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("ch6_why_reset", "Reset",
                       class = "btn-outline-secondary", width = "100%")
        ),
        column(8,
          plotOutput("ch6_why_plot", height = "350px"),
          uiOutput("ch6_why_text")
        )
      )
    ),

    div(class = "callout-success",
      tags$strong("Kluczowa intuicja:"),
      " U\u015brednianie 'wyg\u0142adza' indywidualne dziwactwa. Ekstrema w jednym
        kierunku s\u0105 niwelowane przez ekstrema w drugim. Im wi\u0119cej
        u\u015bredniamy, tym bli\u017cej \u015brodka l\u0105dujemy."
    ),

    div(class = "formula-box",
      withMathJax(helpText(
        "$$\\bar{X}_n \\xrightarrow{d} N\\left(\\mu, \\frac{\\sigma}{\\sqrt{n}}\\right)$$"
      )),
      p(style = "font-size: 13px; color: #7f8c8d;",
        "Rozk\u0142ad \u015bredniej ma t\u0119 sam\u0105 \u015bredni\u0105 \u03bc co populacja, ale
         odchylenie standardowe maleje jak 1/\u221an.")
    ),

    # --- Transition ---
    div(class = "chapter-transition",
      p("CTG wyja\u015bnia, dlaczego rozk\u0142ad normalny jest tak wa\u017cny
        dla wnioskowania statystycznego. Na koniec \u2014 kompaktowa \u015bci\u0105ga
        ze wszystkimi wzorami i regu\u0142ami decyzyjnymi."),
      actionButton("ch6_next", "Dalej: 7. \u015aci\u0105ga \u2192",
                   class = "btn-primary btn-lg")
    ),

    br(), br()
  ))
)

# --------------------------------------------------------------------------
# Chapter 6 Server
# --------------------------------------------------------------------------

ch6_server <- function(input, output, session) {

  # --- Widget 1: Eksperyment CLT ---
  collected_means <- reactiveVal(numeric(0))

  take_samples <- function(k) {
    n <- input$ch6_sample_size
    dist <- input$ch6_pop_dist
    new_means <- replicate(k, {
      samp <- generate_population_sample(dist, n)
      mean(samp)
    })
    collected_means(c(collected_means(), new_means))
  }

  observeEvent(input$ch6_take_1, take_samples(1))
  observeEvent(input$ch6_take_100, take_samples(100))
  observeEvent(input$ch6_take_1000, take_samples(1000))
  observeEvent(input$ch6_reset, collected_means(numeric(0)))

  # Reset przy zmianie rozkladu lub n
  observeEvent(c(input$ch6_pop_dist, input$ch6_sample_size), {
    collected_means(numeric(0))
  })

  output$ch6_sample_count <- renderUI({
    n <- length(collected_means())
    div(class = "stat-box", style = paste0("background: ", col_primary, ";"),
        paste0("Pr\u00f3b: ", n))
  })

  output$ch6_pop_plot <- renderPlot({
    dist <- input$ch6_pop_dist
    dist_label <- dist_names_pl[dist]

    if (dist == "die") {
      df <- data.frame(x = 1:6, prob = rep(1/6, 6))
      ggplot(df, aes(x = factor(x), y = prob)) +
        geom_col(fill = col_warning, color = "white", alpha = 0.85, width = 0.6) +
        scale_y_continuous(limits = c(0, 0.3), expand = expansion(mult = c(0, 0))) +
        labs(title = paste0("Populacja: ", dist_label),
             subtitle = "To NIE jest normalny!",
             x = "", y = "P(X=k)") +
        theme_prob(base_size = 11)
    } else {
      data <- generate_population_sample(dist, 10000)
      df <- data.frame(x = data)
      ggplot(df, aes(x = x)) +
        geom_density(fill = col_warning, color = col_dark, alpha = 0.5, linewidth = 0.8) +
        labs(title = paste0("Populacja: ", dist_label),
             subtitle = "To NIE jest normalny!",
             x = "", y = "G\u0119sto\u015b\u0107") +
        theme_prob(base_size = 11)
    }
  })

  output$ch6_means_plot <- renderPlot({
    means <- collected_means()

    if (length(means) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij 'Pobierz pr\u00f3b\u0119', aby rozpocz\u0105\u0107",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      dist <- input$ch6_pop_dist
      n <- input$ch6_sample_size
      params <- get_population_params(dist)
      theo_mu <- params$mu
      theo_sd <- params$sigma / sqrt(n)

      df <- data.frame(x = means)

      p <- ggplot(df, aes(x = x))

      if (length(means) >= 5) {
        p <- p + geom_histogram(aes(y = after_stat(density)),
                                bins = min(50, max(10, length(means) / 5)),
                                fill = col_primary, color = "white", alpha = 0.7)
      } else {
        p <- p + geom_dotplot(fill = col_primary, alpha = 0.7, binwidth = theo_sd / 3)
      }

      if (length(means) >= 30 && theo_sd > 0) {
        x_range <- seq(min(means) - theo_sd, max(means) + theo_sd, length.out = 200)
        norm_df <- data.frame(x = x_range, y = dnorm(x_range, theo_mu, theo_sd))
        p <- p + geom_line(data = norm_df, aes(x = x, y = y),
                           color = col_secondary, linewidth = 1.5, linetype = "solid")
      }

      p + geom_vline(xintercept = theo_mu, color = col_secondary, linetype = "dashed") +
        labs(title = paste0("Rozk\u0142ad \u015brednich (n=", n, ", ", length(means), " pr\u00f3b)"),
             subtitle = paste0("Krzywa: N(", round(theo_mu, 2), ", ",
                               round(theo_sd, 2), ")"),
             x = "\u015arednia z pr\u00f3by", y = "G\u0119sto\u015b\u0107") +
        theme_prob()
    }
  })

  output$ch6_means_stats <- renderUI({
    means <- collected_means()
    req(length(means) >= 2)

    dist <- input$ch6_pop_dist
    n <- input$ch6_sample_size
    params <- get_population_params(dist)
    theo_sd <- params$sigma / sqrt(n)

    div(style = "text-align: center; margin-top: 10px;",
      div(class = "stat-box", style = paste0("background: ", col_primary, ";"),
          paste0("\u015ar. \u015brednich = ", round(mean(means), 3))),
      div(class = "stat-box", style = paste0("background: ", col_dark, ";"),
          paste0("SD \u015brednich = ", round(sd(means), 3))),
      div(class = "stat-box", style = paste0("background: ", col_warning, ";"),
          paste0("Teor. SD = \u03c3/\u221an = ", round(theo_sd, 3)))
    )
  })

  # --- Widget 2: Wplyw wielkosci proby ---
  output$ch6_effect_plot <- renderPlot({
    dist <- input$ch6_effect_dist
    params <- get_population_params(dist)

    ns <- c(1, 5, 30, 100)
    plot_data <- do.call(rbind, lapply(ns, function(n) {
      means <- replicate(2000, mean(generate_population_sample(dist, n)))
      data.frame(
        mean_val = means,
        n_label = paste0("n = ", n)
      )
    }))
    plot_data$n_label <- factor(plot_data$n_label,
                                levels = paste0("n = ", ns))

    # Krzywe normalne
    norm_data <- do.call(rbind, lapply(ns, function(n) {
      theo_sd <- params$sigma / sqrt(n)
      x_seq <- seq(params$mu - 4*theo_sd, params$mu + 4*theo_sd, length.out = 200)
      data.frame(
        x = x_seq,
        y = dnorm(x_seq, params$mu, theo_sd),
        n_label = paste0("n = ", n)
      )
    }))
    norm_data$n_label <- factor(norm_data$n_label, levels = paste0("n = ", ns))

    ggplot(plot_data, aes(x = mean_val)) +
      geom_histogram(aes(y = after_stat(density)),
                     bins = 40, fill = col_primary, color = "white", alpha = 0.6) +
      geom_line(data = norm_data, aes(x = x, y = y),
                color = col_secondary, linewidth = 1.2) +
      facet_wrap(~n_label, scales = "free") +
      labs(title = paste0("2000 \u015brednich z rozk\u0142adu: ",
                          dist_names_pl[dist]),
           x = "\u015arednia z pr\u00f3by", y = "G\u0119sto\u015b\u0107") +
      theme_prob(base_size = 12)
  })

  # --- Widget 3: Dlaczego to dziala? ---
  ch6_why_step <- reactiveVal(0)

  observeEvent(input$ch6_why_step1, ch6_why_step(1))
  observeEvent(input$ch6_why_step2, ch6_why_step(2))
  observeEvent(input$ch6_why_step3, ch6_why_step(3))
  observeEvent(input$ch6_why_step4, ch6_why_step(4))
  observeEvent(input$ch6_why_reset, ch6_why_step(0))

  output$ch6_why_plot <- renderPlot({
    step <- ch6_why_step()

    if (step == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij krok 1, aby zacz\u0105\u0107",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      n_val <- c(1, 2, 5, 30)[step]
      means <- replicate(5000, mean(rexp(n_val, 0.5)))
      df <- data.frame(x = means)
      params <- get_population_params("exponential")
      theo_sd <- params$sigma / sqrt(n_val)

      p <- ggplot(df, aes(x = x)) +
        geom_histogram(aes(y = after_stat(density)),
                       bins = 50, fill = col_primary, color = "white", alpha = 0.7)

      if (n_val >= 2) {
        x_range <- seq(min(means), max(means), length.out = 200)
        norm_df <- data.frame(x = x_range,
                              y = dnorm(x_range, params$mu, theo_sd))
        p <- p + geom_line(data = norm_df, aes(x = x, y = y),
                           color = col_secondary, linewidth = 1.5)
      }

      p + labs(title = paste0("Rozk\u0142ad \u015bredniej z ", n_val,
                              if (n_val == 1) " obserwacji" else " obserwacji",
                              " (wyk\u0142adniczy)"),
               subtitle = if (n_val == 1) "Identyczny z rozk\u0142adem populacji"
                           else paste0("Krzywa: N(", round(params$mu, 2), ", ",
                                       round(theo_sd, 2), ")"),
               x = "\u015arednia", y = "G\u0119sto\u015b\u0107") +
        theme_prob()
    }
  })

  output$ch6_why_text <- renderUI({
    step <- ch6_why_step()
    texts <- list(
      NULL,
      "Pojedyncza obserwacja z rozk\u0142adu wyk\u0142adniczego \u2014 wyra\u017anie prawosko\u015bny!",
      "\u015arednia z 2: ju\u017c mniej skrajnych warto\u015bci, lewa strona zaczyna si\u0119 wype\u0142nia\u0107.",
      "\u015arednia z 5: kszta\u0142t staje si\u0119 bardziej symetryczny. Ekstrema si\u0119 niweluj\u0105.",
      "\u015arednia z 30: praktycznie normalny! Krzywa gaussowska pasuje niemal idealnie."
    )
    if (step > 0) div(class = "callout-info", texts[[step + 1]])
  })

}
