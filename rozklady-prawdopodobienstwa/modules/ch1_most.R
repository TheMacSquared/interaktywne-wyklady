# ============================================================================
# CHAPTER 1: Od danych do prawdopodobienstwa
# ============================================================================

ch1_ui <- tabPanel("1. Od danych do prawdopodobie\u0144stwa",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Od danych do prawdopodobie\u0144stwa"),

    div(class = "narrative",
      p("W statystyce opisowej nauczyli\u015bmy si\u0119 liczy\u0107 cz\u0119sto\u015bci, oblicza\u0107
        \u015brednie i rysowa\u0107 histogramy. Ale co, je\u015bli chcemy ",
        tags$b("przewidywa\u0107"), " przysz\u0142e obserwacje?"),
      p("Klucz le\u017cy w poj\u0119ciu ", tags$b("prawdopodobie\u0144stwa"),
        ". W tym rozdziale zobaczymy, jak cz\u0119sto\u015bci wzgl\u0119dne
        zbiegaj\u0105 do prawdopodobie\u0144stw i czym jest rozk\u0142ad prawdopodobie\u0144stwa.")
    ),

    # ========================================================================
    # WIDGET 0: Rozklad empiryczny vs teoretyczny
    # ========================================================================
    div(class = "section-title", "Rozk\u0142ad empiryczny vs teoretyczny"),

    div(class = "narrative",
      p("Znasz ju\u017c histogram \u2014 pokazuje, jak cz\u0119sto dane przyjmuj\u0105
        r\u00f3\u017cne warto\u015bci. To jest ", tags$b("rozk\u0142ad empiryczny"),
        " \u2014 oparty na obserwacjach."),
      p("A gdyby\u015bmy znali ", tags$b("regu\u0142\u0119 generuj\u0105c\u0105 dane"),
        "? Wtedy zamiast histogramu mieliby\u015bmy g\u0142adk\u0105 krzyw\u0105 \u2014 ",
        tags$b("rozk\u0142ad teoretyczny"),
        ". Zobaczmy, jak jedno przechodzi w drugie.")
    ),

    div(class = "widget-block",
      h4("Histogram (dane) vs krzywa g\u0119sto\u015bci (model)"),
      fluidRow(
        column(4,
          selectInput("ch1_emp_dist", "Rozk\u0142ad \u017ar\u00f3d\u0142owy:",
            choices = c(
              "Wzrost student\u00f3w (normalny)" = "normal",
              "Czas dojazdu (sko\u015bny)"       = "skewed",
              "Ocena losowa (jednostajny)"   = "uniform"
            ),
            selected = "normal"
          ),
          sliderInput("ch1_emp_n", "Wielko\u015b\u0107 pr\u00f3by:",
                      min = 20, max = 5000, value = 200, step = 20),
          actionButton("ch1_emp_resample", "Losuj now\u0105 pr\u00f3b\u0119",
                       class = "btn-primary", width = "100%"),
          hr(),
          checkboxInput("ch1_show_hist", "Histogram (dane empiryczne)", value = TRUE),
          checkboxInput("ch1_show_density", "Krzywa g\u0119sto\u015bci (model teoretyczny)", value = FALSE)
        ),
        column(8,
          plotOutput("ch1_emp_vs_theo", height = "380px"),
          uiOutput("ch1_emp_text")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Kluczowa idea:"),
      " W\u0142\u0105cz obie warstwy i zwi\u0119kszaj pr\u00f3b\u0119. Im wi\u0119cej danych,
        tym lepiej histogram przybli\u017ca krzyw\u0105.
        Rozk\u0142ad teoretyczny to ", tags$b("idea\u0142"), " \u2014 dane to jego ",
        tags$b("niedoskona\u0142e odbicie"), "."
    ),

    # ========================================================================
    # WIDGET 1: Stabilizacja czestosci (rzut kostka)
    # ========================================================================
    div(class = "section-title", "Prawo wielkich liczb w akcji"),

    div(class = "narrative",
      p("Wyobra\u017a sobie, \u017ce rzucasz kostk\u0105. Jak cz\u0119sto wypada ka\u017cda \u015bcianka?
        Przy kilku rzutach wyniki s\u0105 chaotyczne, ale im wi\u0119cej rzut\u00f3w,
        tym cz\u0119sto\u015bci wzgl\u0119dne staj\u0105 si\u0119 bardziej stabilne.")
    ),

    div(class = "widget-block",
      h4("Rzuty kostk\u0105 \u2014 stabilizacja cz\u0119sto\u015bci"),
      fluidRow(
        column(4,
          div(style = "display: flex; flex-direction: column; gap: 8px;",
            actionButton("ch1_roll_1", "Rzu\u0107 1x",
                         class = "btn-primary", width = "100%"),
            actionButton("ch1_roll_10", "Rzu\u0107 10x",
                         class = "btn-primary", width = "100%"),
            actionButton("ch1_roll_100", "Rzu\u0107 100x",
                         class = "btn-primary", width = "100%"),
            actionButton("ch1_roll_1000", "Rzu\u0107 1000x",
                         class = "btn-primary", width = "100%"),
            hr(),
            actionButton("ch1_roll_reset", "Reset",
                         class = "btn-outline-secondary", width = "100%")
          ),
          br(),
          uiOutput("ch1_roll_count")
        ),
        column(8,
          plotOutput("ch1_freq_bar", height = "250px"),
          plotOutput("ch1_conv_plot", height = "250px")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Prawo wielkich liczb:"),
      " Wraz ze wzrostem liczby obserwacji, cz\u0119sto\u015b\u0107 wzgl\u0119dna ka\u017cdego
        wyniku zbiega do jego prawdopodobie\u0144stwa teoretycznego.
        Dla uczciwej kostki ka\u017cda \u015bcianka ma P = 1/6 \u2248 0.167."
    ),

    # ========================================================================
    # WIDGET 2: Czestosci vs prawdopodobienstwo
    # ========================================================================
    div(class = "section-title", "Cz\u0119sto\u015bci vs prawdopodobie\u0144stwo"),

    div(class = "narrative",
      p("Prawdopodobie\u0144stwo to ", tags$b("teoretyczny model"),
        " opisuj\u0105cy, jak cz\u0119sto powinny wyst\u0119powa\u0107 r\u00f3\u017cne wyniki.
        Cz\u0119sto\u015bci wzgl\u0119dne z danych to ", tags$b("empiryczne przybli\u017cenie"),
        " tego modelu.")
    ),

    div(class = "widget-block",
      h4("Por\u00f3wnanie: teoria vs obserwacja"),
      fluidRow(
        column(4,
          radioButtons("ch1_scenario", "Scenariusz:",
            choices = c(
              "Uczciwa kostka"   = "fair",
              "Obci\u0105\u017cona kostka" = "loaded",
              "Moneta"           = "coin"
            ),
            selected = "fair"
          ),
          sliderInput("ch1_n_obs", "Liczba obserwacji:",
                      min = 10, max = 5000, value = 100, step = 10),
          actionButton("ch1_resample", "Losuj ponownie",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch1_freq_vs_prob", height = "350px"),
          uiOutput("ch1_freq_vs_prob_text")
        )
      )
    ),

    # ========================================================================
    # WIDGET 3: Czym jest rozklad?
    # ========================================================================
    div(class = "section-title", "Czym jest rozk\u0142ad prawdopodobie\u0144stwa?"),

    div(class = "narrative",
      p("Rozk\u0142ad prawdopodobie\u0144stwa to ", tags$b("kompletny opis"),
        " wszystkich mo\u017cliwych wynik\u00f3w i ich prawdopodobie\u0144stw.
        Musi spe\u0142nia\u0107 dwa warunki:"),
      tags$ol(
        tags$li("Ka\u017cde prawdopodobie\u0144stwo jest nieujemne: P(x) \u2265 0"),
        tags$li("Suma wszystkich prawdopodobie\u0144stw wynosi 1")
      ),
      p("Spr\u00f3buj sam(a) zbudowa\u0107 rozk\u0142ad \u2014 ustaw prawdopodobie\u0144stwa
        czterech wynik\u00f3w tak, \u017ceby sumowa\u0142y si\u0119 do 1.")
    ),

    div(class = "widget-block",
      h4("Zbuduj w\u0142asny rozk\u0142ad"),
      fluidRow(
        column(6,
          sliderInput("ch1_p1", "P(Wynik A):", min = 0, max = 1, value = 0.25, step = 0.01),
          sliderInput("ch1_p2", "P(Wynik B):", min = 0, max = 1, value = 0.25, step = 0.01),
          sliderInput("ch1_p3", "P(Wynik C):", min = 0, max = 1, value = 0.25, step = 0.01),
          sliderInput("ch1_p4", "P(Wynik D):", min = 0, max = 1, value = 0.25, step = 0.01),
          uiOutput("ch1_sum_check")
        ),
        column(6,
          plotOutput("ch1_custom_dist", height = "300px")
        )
      )
    ),

    div(class = "callout-warning",
      tags$strong("Zapami\u0119taj:"),
      " Rozk\u0142ad prawdopodobie\u0144stwa to nie dane \u2014 to ", tags$b("model matematyczny"),
      ". Dane to pr\u00f3bka z tego modelu. Im wi\u0119ksza pr\u00f3bka,
        tym lepiej przybli\u017ca model."
    ),

    # --- Transition ---
    div(class = "chapter-transition",
      p("Wiemy ju\u017c, czym jest rozk\u0142ad prawdopodobie\u0144stwa.
        Zanim poznamy konkretne rozk\u0142ady, odpowiedzmy na dwa kluczowe
        pytania: czego si\u0119 spodziewa\u0107 i jak bardzo wyniki mog\u0105 si\u0119 r\u00f3\u017cni\u0107?"),
      actionButton("ch1_next", "Dalej: 2. Warto\u015b\u0107 oczekiwana i wariancja \u2192",
                   class = "btn-primary btn-lg")
    ),

    br(), br()
  ))
)

# --------------------------------------------------------------------------
# Chapter 1 Server
# --------------------------------------------------------------------------

ch1_server <- function(input, output, session) {

  # --- Widget 0: Rozklad empiryczny vs teoretyczny ---
  ch1_emp_data <- reactiveVal(NULL)

  generate_emp_data <- function() {
    n <- input$ch1_emp_n
    dist <- input$ch1_emp_dist
    data <- switch(dist,
      "normal"  = rnorm(n, mean = 170, sd = 8),
      "skewed"  = rgamma(n, shape = 3, scale = 10) + 5,
      "uniform" = runif(n, min = 1, max = 6)
    )
    ch1_emp_data(list(data = data, dist = dist, n = n))
  }

  observe({
    input$ch1_emp_dist
    input$ch1_emp_n
    generate_emp_data()
  })
  observeEvent(input$ch1_emp_resample, generate_emp_data())

  output$ch1_emp_vs_theo <- renderPlot({
    d <- ch1_emp_data()
    req(d)

    show_hist <- input$ch1_show_hist
    show_dens <- input$ch1_show_density

    if (!show_hist && !show_dens) {
      return(ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "W\u0142\u0105cz przynajmniej jedn\u0105 warstw\u0119",
                 size = 6, color = "#7f8c8d") +
        theme_void())
    }

    df <- data.frame(x = d$data)

    # Zakres teoretyczny osi X
    theo_xlim <- switch(d$dist,
      "normal"  = c(170 - 4*8, 170 + 4*8),   # mu +/- 4*sigma
      "skewed"  = c(0, 5 + qgamma(0.999, shape = 3, scale = 10)),
      "uniform" = c(0.5, 6.5)
    )
    # Zakres empiryczny
    emp_xlim <- range(d$data)
    # Wez szerszy z dwoch
    x_lo <- min(theo_xlim[1], emp_xlim[1])
    x_hi <- max(theo_xlim[2], emp_xlim[2])
    x_margin <- (x_hi - x_lo) * 0.05
    fixed_xlim <- c(x_lo - x_margin, x_hi + x_margin)

    x_seq <- seq(fixed_xlim[1], fixed_xlim[2], length.out = 500)

    theo_y <- switch(d$dist,
      "normal"  = dnorm(x_seq, mean = 170, sd = 8),
      "skewed"  = dgamma(x_seq - 5, shape = 3, scale = 10),
      "uniform" = dunif(x_seq, min = 1, max = 6)
    )
    theo_y[is.na(theo_y) | theo_y < 0] <- 0
    df_theo <- data.frame(x = x_seq, y = theo_y)

    # Stale breaks oparte na danych (niezalezne od osi)
    n_bins <- min(50, max(10, d$n / 10))
    bin_breaks <- seq(min(d$data), max(d$data), length.out = n_bins + 1)

    # Zakres Y: max z gestosci teoretycznej i histogramu
    theo_ymax <- max(theo_y)
    hist_obj <- hist(d$data, breaks = bin_breaks, plot = FALSE)
    hist_ymax <- max(hist_obj$density)
    fixed_ymax <- max(theo_ymax, hist_ymax) * 1.08

    dist_label <- switch(d$dist,
      "normal"  = "Rozk\u0142ad normalny N(170, 8)",
      "skewed"  = "Rozk\u0142ad gamma (sko\u015bny)",
      "uniform" = "Rozk\u0142ad jednostajny U(1, 6)"
    )

    p <- ggplot()

    if (show_hist) {
      p <- p + geom_histogram(data = df, aes(x = x, y = after_stat(density)),
                               breaks = bin_breaks,
                               fill = col_primary, color = "white", alpha = 0.6)
    }

    if (show_dens) {
      p <- p + geom_line(data = df_theo, aes(x = x, y = y),
                          color = col_secondary, linewidth = 1.8) +
        geom_area(data = df_theo, aes(x = x, y = y),
                  fill = col_secondary, alpha = 0.1)
    }

    p + coord_cartesian(xlim = fixed_xlim, ylim = c(0, fixed_ymax)) +
    labs(
      title = paste0(if (show_hist && show_dens) "Dane + model"
                     else if (show_hist) "Dane empiryczne (histogram)"
                     else "Model teoretyczny (krzywa)"),
      subtitle = paste0("n = ", d$n, " | ", dist_label),
      x = "Warto\u015b\u0107", y = "G\u0119sto\u015b\u0107"
    ) +
    theme_prob()
  })

  output$ch1_emp_text <- renderUI({
    show_hist <- input$ch1_show_hist
    show_dens <- input$ch1_show_density

    if (show_hist && show_dens) {
      div(class = "callout-success",
        "Widzisz obie warstwy. Histogram to dane, krzywa to model.
         Zwi\u0119kszaj pr\u00f3b\u0119 \u2014 histogram coraz lepiej przybli\u017ca krzyw\u0105!")
    } else if (show_hist) {
      div(class = "callout-info",
        "To histogram \u2014 rozk\u0142ad empiryczny oparty na danych.
         W\u0142\u0105cz krzyw\u0105 g\u0119sto\u015bci, \u017ceby zobaczy\u0107 model teoretyczny.")
    } else if (show_dens) {
      div(class = "callout-info",
        "To krzywa g\u0119sto\u015bci \u2014 model teoretyczny.
         W\u0142\u0105cz histogram, \u017ceby por\u00f3wna\u0107 z danymi.")
    }
  })

  # --- Widget 1: Rzuty kostka ---
  dice_rolls <- reactiveVal(integer(0))

  observeEvent(input$ch1_roll_1, {
    dice_rolls(c(dice_rolls(), sample(1:6, 1)))
  })
  observeEvent(input$ch1_roll_10, {
    dice_rolls(c(dice_rolls(), sample(1:6, 10, replace = TRUE)))
  })
  observeEvent(input$ch1_roll_100, {
    dice_rolls(c(dice_rolls(), sample(1:6, 100, replace = TRUE)))
  })
  observeEvent(input$ch1_roll_1000, {
    dice_rolls(c(dice_rolls(), sample(1:6, 1000, replace = TRUE)))
  })
  observeEvent(input$ch1_roll_reset, {
    dice_rolls(integer(0))
  })

  output$ch1_roll_count <- renderUI({
    n <- length(dice_rolls())
    div(class = "stat-box", style = paste0("background: ", col_primary, ";"),
        paste0("Rzut\u00f3w: ", n))
  })

  output$ch1_freq_bar <- renderPlot({
    rolls <- dice_rolls()
    if (length(rolls) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij przycisk, aby rzuci\u0107 kostk\u0105",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      df <- data.frame(face = factor(rolls, levels = 1:6))
      freq_df <- df %>% count(face, .drop = FALSE) %>%
        mutate(rel_freq = n / sum(n))
      ggplot(freq_df, aes(x = face, y = rel_freq)) +
        geom_col(fill = col_primary, color = "white", alpha = 0.85) +
        geom_hline(yintercept = 1/6, color = col_secondary, linewidth = 1, linetype = "dashed") +
        geom_text(aes(label = sprintf("%.3f", rel_freq)), vjust = -0.5, size = 4) +
        scale_y_continuous(limits = c(0, max(0.35, max(freq_df$rel_freq) * 1.15)),
                           expand = expansion(mult = c(0, 0.05))) +
        labs(title = "Cz\u0119sto\u015bci wzgl\u0119dne", x = "\u015acianka", y = "Cz\u0119sto\u015b\u0107 wzgl\u0119dna") +
        annotate("text", x = 6.3, y = 1/6, label = "1/6", color = col_secondary,
                 fontface = "bold", size = 4, hjust = 0) +
        theme_prob()
    }
  })

  output$ch1_conv_plot <- renderPlot({
    rolls <- dice_rolls()
    if (length(rolls) < 2) return(NULL)

    # Linia zbieznosci dla kazdej scianki
    n_total <- length(rolls)
    # Wybierz punkty do wykreslenia (max 200 punktow dla wydajnosci)
    if (n_total <= 200) {
      indices <- seq_len(n_total)
    } else {
      indices <- unique(c(
        seq(1, min(50, n_total)),
        round(seq(51, n_total, length.out = 150))
      ))
    }

    conv_data <- do.call(rbind, lapply(indices, function(i) {
      tab <- table(factor(rolls[1:i], levels = 1:6)) / i
      data.frame(n = i, face = factor(1:6), rel_freq = as.numeric(tab))
    }))

    ggplot(conv_data, aes(x = n, y = rel_freq, color = face)) +
      geom_line(linewidth = 0.8, alpha = 0.7) +
      geom_hline(yintercept = 1/6, color = "gray40", linewidth = 0.8, linetype = "dashed") +
      scale_color_brewer(palette = "Set2", name = "\u015acianka") +
      labs(title = "Zbie\u017cno\u015b\u0107 cz\u0119sto\u015bci do 1/6",
           x = "Liczba rzut\u00f3w", y = "Cz\u0119sto\u015b\u0107 wzgl\u0119dna") +
      theme_prob() +
      theme(legend.position = "right")
  })

  # --- Widget 2: Czestosci vs prawdopodobienstwo ---
  freq_data <- reactiveVal(NULL)

  observe({
    input$ch1_resample
    input$ch1_scenario
    n <- input$ch1_n_obs

    scenario <- input$ch1_scenario
    if (scenario == "fair") {
      obs <- sample(1:6, n, replace = TRUE)
      theo <- rep(1/6, 6)
      labels <- as.character(1:6)
    } else if (scenario == "loaded") {
      probs <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5)
      obs <- sample(1:6, n, replace = TRUE, prob = probs)
      theo <- probs
      labels <- as.character(1:6)
    } else {
      obs <- sample(c(1, 2), n, replace = TRUE)
      theo <- c(0.5, 0.5)
      labels <- c("Orze\u0142", "Reszka")
    }

    freq_data(list(obs = obs, theo = theo, labels = labels))
  })

  output$ch1_freq_vs_prob <- renderPlot({
    fd <- freq_data()
    req(fd)

    n_levels <- length(fd$labels)
    tab <- table(factor(fd$obs, levels = 1:n_levels)) / length(fd$obs)

    df <- data.frame(
      outcome = rep(fd$labels, 2),
      value = c(as.numeric(tab), fd$theo),
      type = rep(c("Obserwowane", "Teoretyczne"), each = n_levels)
    )
    df$outcome <- factor(df$outcome, levels = fd$labels)

    ggplot(df, aes(x = outcome, y = value, fill = type)) +
      geom_col(position = "dodge", alpha = 0.85, color = "white") +
      scale_fill_manual(values = c("Obserwowane" = col_primary, "Teoretyczne" = col_secondary),
                        name = "") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(title = paste0("n = ", length(fd$obs), " obserwacji"),
           x = "Wynik", y = "Proporcja / Prawdopodobie\u0144stwo") +
      theme_prob() +
      theme(legend.position = "top")
  })

  output$ch1_freq_vs_prob_text <- renderUI({
    fd <- freq_data()
    req(fd)
    n <- length(fd$obs)
    max_diff <- max(abs(table(factor(fd$obs, levels = 1:length(fd$labels))) / n - fd$theo))

    div(class = if (max_diff < 0.05) "callout-success" else "callout-info",
      paste0("Maksymalna r\u00f3\u017cnica mi\u0119dzy cz\u0119sto\u015bci\u0105 a prawdopodobie\u0144stwem: ",
             sprintf("%.3f", max_diff),
             if (max_diff < 0.05) " \u2014 dobra zgodno\u015b\u0107!" else " \u2014 spr\u00f3buj zwi\u0119kszy\u0107 n")
    )
  })

  # --- Widget 3: Zbuduj wlasny rozklad ---
  output$ch1_sum_check <- renderUI({
    s <- input$ch1_p1 + input$ch1_p2 + input$ch1_p3 + input$ch1_p4
    if (abs(s - 1) < 0.005) {
      div(class = "stat-box", style = paste0("background: ", col_success, ";"),
          paste0("\u2211 = ", sprintf("%.2f", s), " \u2714"))
    } else {
      div(class = "stat-box", style = paste0("background: ", col_secondary, ";"),
          paste0("\u2211 = ", sprintf("%.2f", s), " \u2260 1 \u2718"))
    }
  })

  output$ch1_custom_dist <- renderPlot({
    probs <- c(input$ch1_p1, input$ch1_p2, input$ch1_p3, input$ch1_p4)
    s <- sum(probs)
    valid <- abs(s - 1) < 0.005

    df <- data.frame(
      outcome = c("A", "B", "C", "D"),
      prob = probs
    )

    ggplot(df, aes(x = outcome, y = prob)) +
      geom_col(fill = if (valid) col_success else "#95a5a6",
               color = "white", alpha = 0.85, width = 0.6) +
      geom_text(aes(label = sprintf("%.2f", prob)), vjust = -0.5, size = 5) +
      scale_y_continuous(limits = c(0, 1.1), expand = expansion(mult = c(0, 0))) +
      labs(title = if (valid) "Poprawny rozk\u0142ad!" else "Suma musi wynosi\u0107 1",
           x = "Wynik", y = "Prawdopodobie\u0144stwo") +
      theme_prob()
  })

}
