# ============================================================================
# CHAPTER 6: Centralne Twierdzenie Graniczne
# ============================================================================

ch6_ui <- list(
  id = "ch-ctg", num = "06", title = "Centralne Twierdzenie Graniczne",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 06 · Rozkłady prawdopodobieństwa",
      num    = "06",
      title  = "Centralne Twierdzenie Graniczne.",
      lead   = "Rozkład normalny pojawia się wszędzie. Ale dlaczego?
                Odpowiedź to jedno z najważniejszych twierdzeń w statystyce."
    ),

    h2(id = "ch6-ctg", class = "section-title", "Centralne Twierdzenie Graniczne (CTG)"),

    div(class = "narrative",
      p("Centralne Twierdzenie Graniczne mówi, że:"),
      div(class = "callout-success",
        tags$strong("CTG:"),
        " Jeśli wezmiesz próbę n obserwacji z ", tags$b("dowolnego"),
        " rozkładu (o skończonej wariancji) i obliczysz średnią,
        to rozkład tej średniej będzie zbiegał do ", tags$b("normalnego"),
        " wraz ze wzrostem n."
      ),
      p("To wyjaśnia, dlaczego rozkład normalny jest wszędzie — wiele
        zjawisk naturalnych to suma wielu drobnych, niezależnych czynników.
        Zobaczmy to na własne oczy!")
    ),

    figure_panel(
      label = "Film",
      title = "Wideo wprowadzające",
      full_width = TRUE,
      div(style = "position: relative; padding-bottom: 56.25%; height: 0; overflow: hidden;",
        tags$iframe(
          src = "https://www.youtube.com/embed/jvoxEYmQHNM",
          style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; border: 0;",
          allowfullscreen = NA,
          allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
        )
      )
    ),

    # ========================================================================
    # WIDGET 1: Eksperyment CLT (kluczowy!)
    # ========================================================================
    h2(id = "ch6-eksperyment", class = "section-title", "Eksperyment CLT"),

    figure_panel(
      label = "Ryc. 6.1",
      title = "Symulacja: średnie z dowolnego rozkładu → normalny",
      full_width = TRUE,
      fluidRow(
        column(4,
          selectInput("ch6_pop_dist", "Rozkład populacji:",
            choices = c(
              "Jednostajny"         = "uniform",
              "Wykładniczy (skośny)" = "exponential",
              "Dwumodalny"          = "bimodal",
              "U-kształtny"          = "u_shape",
              "Kostka (dyskretny)"  = "die"
            ),
            selected = "exponential"
          ),
          sliderInput("ch6_sample_size", "Wielkość próby (n):",
                      min = 1, max = 100, value = 5, step = 1),
          hr(),
          div(style = "display: flex; flex-direction: column; gap: 8px;",
            actionButton("ch6_take_1", "Pobierz 1 próbę",
                         class = "btn-primary", width = "100%"),
            actionButton("ch6_take_100", "Pobierz 100 prób",
                         class = "btn-primary", width = "100%"),
            actionButton("ch6_take_1000", "Pobierz 1000 prób",
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

    margin_callout(
      label = "Aha-moment",
      "Zmień rozkład na najbardziej 'dziki' (U-kształtny, dwumodalny),
       pobierz 1000 prób — histogram średnich i tak stanie się dzwonem!"
    ),

    # ========================================================================
    # WIDGET 2: Wplyw wielkosci proby
    # ========================================================================
    h2(id = "ch6-wielkosc-proby", class = "section-title", "Wpływ wielkości próby"),

    div(class = "narrative",
      p("Im większa próba n, tym szybciej rozkład średnich staje się
        normalny. Zobaczmy to porównując różne n obok siebie.")
    ),

    figure_panel(
      label = "Ryc. 6.2",
      title = "Rozkład średnich dla różnych n",
      full_width = TRUE,
      selectInput("ch6_effect_dist", "Rozkład populacji:",
        choices = c(
          "Wykładniczy" = "exponential",
          "Jednostajny"  = "uniform",
          "U-kształtny"  = "u_shape"
        ),
        selected = "exponential"
      ),
      plotOutput("ch6_effect_plot", height = "350px")
    ),

    # ========================================================================
    # WIDGET 3: Dlaczego to dziala?
    # ========================================================================
    h2(id = "ch6-dlaczego", class = "section-title", "Dlaczego to działa? — intuicja"),

    figure_panel(
      label = "Ryc. 6.3",
      title = "Od jednej obserwacji do średniej z 30",
      full_width = TRUE,
      fluidRow(
        column(4,
          actionButton("ch6_why_step1", "1. Jedna obserwacja",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("ch6_why_step2", "2. Średnia z 2",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("ch6_why_step3", "3. Średnia z 5",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("ch6_why_step4", "4. Średnia z 30",
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

    margin_callout(
      label = "Kluczowa intuicja",
      "Uśrednianie 'wygładza' indywidualne dziwactwa. Ekstrema w jednym
       kierunku są niwelowane przez ekstrema w drugim. Im więcej uśredniamy,
       tym bliżej środka lądujemy.",
      color = "ok"
    ),

    div(class = "formula-box",
      withMathJax(helpText(
        "$$\\bar{X}_n \\xrightarrow{d} N\\left(\\mu, \\frac{\\sigma}{\\sqrt{n}}\\right)$$"
      )),
      p(style = "font-size: 13px; color: #7f8c8d;",
        "Rozkład średniej ma tę samą średnią μ co populacja, ale
         odchylenie standardowe maleje jak 1/√n.")
    ),

    lc_chapter_next(
      num       = "07",
      title     = "Ściąga",
      lead      = "kompaktowe podsumowanie wszystkich wzorów i rozkładów.",
      target_id = "ch-sciaga"
    )
  )
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
    div(class = "stat-box", style = paste0("background: ", unname(upwr_cat["niebo"]), ";"),
        paste0("Prób: ", n))
  })

  output$ch6_pop_plot <- renderPlot({
    dist <- input$ch6_pop_dist
    dist_label <- dist_names_pl[dist]

    if (dist == "die") {
      df <- data.frame(x = 1:6, prob = rep(1/6, 6))
      ggplot(df, aes(x = factor(x), y = prob)) +
        geom_col(fill = unname(upwr_cat["bursztyn"]), color = "white", alpha = 0.85, width = 0.6) +
        scale_y_continuous(limits = c(0, 0.3), expand = expansion(mult = c(0, 0))) +
        labs(title = paste0("Populacja: ", dist_label),
             subtitle = "To NIE jest normalny!",
             x = "", y = "P(X=k)") +
        theme_upwr(base_size = 11)
    } else {
      data <- generate_population_sample(dist, 10000)
      df <- data.frame(x = data)
      ggplot(df, aes(x = x)) +
        geom_density(fill = unname(upwr_cat["bursztyn"]), color = upwr_secondary, alpha = 0.5, linewidth = 0.8) +
        labs(title = paste0("Populacja: ", dist_label),
             subtitle = "To NIE jest normalny!",
             x = "", y = "Gęstość") +
        theme_upwr(base_size = 11)
    }
  })

  output$ch6_means_plot <- renderPlot({
    means <- collected_means()

    if (length(means) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij 'Pobierz próbę', aby rozpocząć",
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
                                fill = unname(upwr_cat["niebo"]), color = "white", alpha = 0.7)
      } else {
        p <- p + geom_dotplot(fill = unname(upwr_cat["niebo"]), alpha = 0.7, binwidth = theo_sd / 3)
      }

      if (length(means) >= 30 && theo_sd > 0) {
        x_range <- seq(min(means) - theo_sd, max(means) + theo_sd, length.out = 200)
        norm_df <- data.frame(x = x_range, y = dnorm(x_range, theo_mu, theo_sd))
        p <- p + geom_line(data = norm_df, aes(x = x, y = y),
                           color = unname(upwr_cat["terakota"]), linewidth = 1.5, linetype = "solid")
      }

      p + geom_vline(xintercept = theo_mu, color = unname(upwr_cat["terakota"]), linetype = "dashed") +
        labs(title = paste0("Rozkład średnich (n=", n, ", ", length(means), " prób)"),
             subtitle = paste0("Krzywa: N(", round(theo_mu, 2), ", ",
                               round(theo_sd, 2), ")"),
             x = "Średnia z próby", y = "Gęstość") +
        theme_upwr()
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
      div(class = "stat-box", style = paste0("background: ", unname(upwr_cat["niebo"]), ";"),
          paste0("Śr. średnich = ", round(mean(means), 3))),
      div(class = "stat-box", style = paste0("background: ", upwr_secondary, ";"),
          paste0("SD średnich = ", round(sd(means), 3))),
      div(class = "stat-box", style = paste0("background: ", unname(upwr_cat["bursztyn"]), ";"),
          paste0("Teor. SD = σ/√n = ", round(theo_sd, 3)))
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
                     bins = 40, fill = unname(upwr_cat["niebo"]), color = "white", alpha = 0.6) +
      geom_line(data = norm_data, aes(x = x, y = y),
                color = unname(upwr_cat["terakota"]), linewidth = 1.2) +
      facet_wrap(~n_label, scales = "free") +
      labs(title = paste0("2000 średnich z rozkładu: ",
                          dist_names_pl[dist]),
           x = "Średnia z próby", y = "Gęstość") +
      theme_upwr(base_size = 12)
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
                 label = "Kliknij krok 1, aby zacząć",
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
                       bins = 50, fill = unname(upwr_cat["niebo"]), color = "white", alpha = 0.7)

      if (n_val >= 2) {
        x_range <- seq(min(means), max(means), length.out = 200)
        norm_df <- data.frame(x = x_range,
                              y = dnorm(x_range, params$mu, theo_sd))
        p <- p + geom_line(data = norm_df, aes(x = x, y = y),
                           color = unname(upwr_cat["terakota"]), linewidth = 1.5)
      }

      p + labs(title = paste0("Rozkład średniej z ", n_val,
                              if (n_val == 1) " obserwacji" else " obserwacji",
                              " (wykładniczy)"),
               subtitle = if (n_val == 1) "Identyczny z rozkładem populacji"
                           else paste0("Krzywa: N(", round(params$mu, 2), ", ",
                                       round(theo_sd, 2), ")"),
               x = "Średnia", y = "Gęstość") +
        theme_upwr()
    }
  })

  output$ch6_why_text <- renderUI({
    step <- ch6_why_step()
    texts <- list(
      NULL,
      "Pojedyncza obserwacja z rozkładu wykładniczego — wyraźnie prawoskośny!",
      "Średnia z 2: już mniej skrajnych wartości, lewa strona zaczyna się wypełniać.",
      "Średnia z 5: kształt staje się bardziej symetryczny. Ekstrema się niwelują.",
      "Średnia z 30: praktycznie normalny! Krzywa gaussowska pasuje niemal idealnie."
    )
    if (step > 0) div(class = "callout-info", texts[[step + 1]])
  })

}
