# ============================================================================
# CHAPTER 5: Kształt rozkładu
# ============================================================================

ch5_ui <- tabPanel("5. Kształt rozkładu",
  fluidRow(column(8, offset = 2,

    # --- Introduction ---
    div(class = "chapter-recap",
      "Znamy juz położenie i rozrzut. Ostatni element układanki: jaki kształt ma rozkład?
       Czy jest symetryczny, czy moze ma 'długi ogon' w jedna strone?"
    ),
    uiOutput("tracker_ch5"),
    div(class = "section-title", h2("5. Kształt rozkładu")),

    div(class = "narrative",
      p("Dwa rozkłady mogą mieć ta sama średnia i odchylenie standardowe,
        a wyglądać zupełnie inaczej. Kształt rozkładu mówi nam o asymetrii
        i 'ciężkości' ogonów."),
      p("W tym rozdziale poznasz dwie miary kształtu:",
        tags$strong("skośność"), "(asymetria) i",
        tags$strong("kurtoze"), "(ciężkość ogonów).")
    ),

    # --- Widget 1: Skewness ---
    div(class = "section-title", h3("5.1 Skośność (asymetria)")),

    div(class = "narrative",
      p("Skośność mierzy asymetrię rozkładu. Wartość skośności = 0 oznacza
        idealną symetrię, wartości dodatnie wskazują na dłuższy ogon w prawo,
        a ujemne — w lewo.")
    ),

    div(class = "widget-block",
      h4("Porównanie trzech typów skośności"),
      plotOutput("ch5_skew_comparison", height = "300px"),
      div(class = "callout-info",
        tags$strong("Trzy typy rozkładów: "),
        "lewostronnie skośny (ogon w lewo), symetryczny (brak ogona), prawostronnie skośny (ogon w prawo)."
      )
    ),

    div(class = "widget-block",
      h4("Sprawdź skośność w naszych danych"),
      selectInput("ch5_skew_var", "Wybierz zmienną:",
        choices = c(
          "Wzrost" = "wzrost",
          "Czas dojazdu" = "czas_dojazdu",
          "Średnia ocen" = "srednia_ocen",
          "Liczba nieobecności" = "liczba_nieobecnosci"
        ),
        selected = "czas_dojazdu"
      ),
      plotOutput("ch5_skew_plot", height = "350px"),
      uiOutput("ch5_skew_info")
    ),

    # --- Widget 2: Kurtosis ---
    div(class = "section-title", h3("5.2 Kurtoza (ci\u0119\u017cko\u015b\u0107 ogon\u00f3w)")),

    div(class = "narrative",
      p("Kurtoza mierzy, jak 'ci\u0119\u017ckie' s\u0105 ogony rozk\u0142adu \u2014 czyli
        jak cz\u0119sto pojawiaj\u0105 si\u0119 warto\u015bci ekstremalne. Nie chodzi
        o 'sp\u0142aszczenie' szczytu, lecz o to, ile obserwacji le\u017cy daleko
        od \u015bredniej.")
    ),

    div(class = "widget-block",
      h4("Por\u00f3wnaj rozk\u0142ady o r\u00f3\u017cnej kurtozie"),
      fluidRow(
        column(8,
          sliderInput("ch5_kurt_val", "Nadwy\u017ckowa kurtoza:",
            min = -1.2, max = 6, value = 0, step = 0.2
          )
        ),
        column(4,
          div(style = "margin-top: 25px; display: flex; gap: 4px; flex-wrap: wrap;",
            actionButton("ch5_kurt_platy", "Platykurtyczny",
                         class = "btn-outline-primary btn-sm"),
            actionButton("ch5_kurt_mezo", "Mezokurtyczny",
                         class = "btn-outline-primary btn-sm"),
            actionButton("ch5_kurt_lepto", "Leptokurtyczny",
                         class = "btn-outline-primary btn-sm")
          )
        )
      ),
      plotOutput("ch5_kurt_plot", height = "350px"),
      plotOutput("ch5_kurt_tails", height = "220px"),
      uiOutput("ch5_kurt_text")
    ),

    # --- Widget 3: Full picture ---
    div(class = "section-title", h3("5.3 Pelny obraz")),

    div(class = "narrative",
      p("Na koniec - pelny obraz. Dla każdej zmiennej ilościowej mozemy
        opisać jej położenie, rozrzut i kształt.")
    ),

    div(class = "widget-block",
      selectInput("ch5_full_var", "Wybierz zmienna:",
        choices = c(
          "Wzrost" = "wzrost",
          "Średnia ocen" = "srednia_ocen",
          "Czas dojazdu" = "czas_dojazdu",
          "Waga" = "waga"
        ),
        selected = "wzrost"
      ),
      plotOutput("ch5_full_hist", height = "350px"),
      plotOutput("ch5_full_box", height = "120px"),
      tableOutput("ch5_full_table"),
      uiOutput("ch5_full_interpretation")
    ),

    # --- Transition to ch6 ---
    div(class = "chapter-transition",
      p("Teraz potrafisz opisac rozk\u0142ad zmiennej ilo\u015bciowej w trzech wymiarach:
        po\u0142o\u017cenie, rozrzut i kszta\u0142t. Czas na podsumowanie -- \u015bci\u0105ga
        ze wszystkimi narz\u0119dziami w jednym miejscu."),
      actionButton("ch5_next", "Dalej: 6. \u015aci\u0105ga \u2192",
                   class = "btn-primary btn-lg")
    ),

    br(), br()
  ))
) # end ch5 tabPanel

# --------------------------------------------------------------------------
# Chapter 5 Server
# --------------------------------------------------------------------------

ch5_server <- function(input, output, session) {

  # --------------------------------------------------------------------------
  # Widget 1: Skewness
  # --------------------------------------------------------------------------

  ch5_skew_data <- reactive({
    req(input$ch5_skew_var)
    vals <- student_data[[input$ch5_skew_var]]
    vals <- vals[!is.na(vals)]
    list(
      values = vals,
      label = variable_meta[[input$ch5_skew_var]]$label,
      var_name = input$ch5_skew_var
    )
  })

  output$ch5_skew_comparison <- renderPlot({
    set.seed(42)
    n_pts <- 5000
    left_skew  <- -rgamma(n_pts, shape = 4, scale = 1)
    symmetric  <- rnorm(n_pts, mean = 0, sd = 2)
    right_skew <- rgamma(n_pts, shape = 4, scale = 1)

    df_cmp <- rbind(
      data.frame(x = left_skew,  typ = "Lewostronnie skośny"),
      data.frame(x = symmetric,  typ = "Symetryczny"),
      data.frame(x = right_skew, typ = "Prawostronnie skośny")
    )
    df_cmp$typ <- factor(df_cmp$typ,
      levels = c("Lewostronnie skośny", "Symetryczny", "Prawostronnie skośny"))

    sk_vals <- c(
      round(e1071::skewness(left_skew), 2),
      round(e1071::skewness(symmetric), 2),
      round(e1071::skewness(right_skew), 2)
    )
    label_df <- data.frame(
      typ = factor(
        c("Lewostronnie skośny", "Symetryczny", "Prawostronnie skośny"),
        levels = c("Lewostronnie skośny", "Symetryczny", "Prawostronnie skośny")),
      label = paste0("skośność = ", sk_vals)
    )

    ggplot(df_cmp, aes(x = x)) +
      geom_density(aes(fill = typ), alpha = 0.5, color = "#2c3e50", linewidth = 0.8) +
      geom_text(data = label_df,
        aes(label = label), x = 0, y = Inf, vjust = 1.5,
        size = 4, fontface = "italic", color = "#2c3e50",
        inherit.aes = FALSE) +
      facet_wrap(~typ, scales = "free_x") +
      scale_fill_manual(values = c(
        "Lewostronnie skośny" = "#e74c3c",
        "Symetryczny" = "#27ae60",
        "Prawostronnie skośny" = "#3498db"
      )) +
      labs(x = "Wartość", y = "Gęstość",
        title = "Trzy typy skośności rozkładu") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none",
            strip.text = element_text(face = "bold", size = 12))
  })

  output$ch5_skew_plot <- renderPlot({
    d <- ch5_skew_data()
    vals <- d$values
    m <- mean(vals)
    med <- median(vals)
    sk <- e1071::skewness(vals)

    df <- data.frame(x = vals)

    ggplot(df, aes(x = x)) +
      geom_histogram(aes(y = after_stat(density)),
        bins = 20, fill = "#3498db", color = "white", alpha = 0.6) +
      geom_density(color = "#2c3e50", linewidth = 1) +
      geom_vline(xintercept = m, color = "#e74c3c", linewidth = 1.2, linetype = "solid") +
      geom_vline(xintercept = med, color = "#3498db", linewidth = 1.2, linetype = "dashed") +
      annotate("text",
        x = m, y = -Inf, vjust = -0.5,
        label = paste0("Średnia = ", round(m, 2)),
        color = "#e74c3c", size = 3.5, fontface = "bold") +
      annotate("text",
        x = med, y = -Inf, vjust = -2,
        label = paste0("Mediana = ", round(med, 2)),
        color = "#3498db", size = 3.5, fontface = "bold") +
      labs(
        title = paste0(d$label, " — skośność = ", round(sk, 3)),
        x = d$label,
        y = "Gęstość"
      ) +
      theme_minimal(base_size = 14)
  })

  output$ch5_skew_info <- renderUI({
    d <- ch5_skew_data()
    vals <- d$values
    sk <- e1071::skewness(vals)
    m <- mean(vals)
    med <- median(vals)

    p(tags$strong("Skośność = ", round(sk, 3)),
      " | Średnia = ", round(m, 2),
      ", Mediana = ", round(med, 2))
  })

  # --------------------------------------------------------------------------
  # Widget 2: Kurtosis (suwak kurtozy, bez t-rozk\u0142adu)
  # --------------------------------------------------------------------------

  observeEvent(input$ch5_kurt_platy, { updateSliderInput(session, "ch5_kurt_val", value = -1.0) })
  observeEvent(input$ch5_kurt_mezo,  { updateSliderInput(session, "ch5_kurt_val", value = 0) })
  observeEvent(input$ch5_kurt_lepto, { updateSliderInput(session, "ch5_kurt_val", value = 4) })

  # Generate density with target excess kurtosis
  # Leptokurtic: t-distribution scaled to sd=1 (higher peak, heavier tails)
  # Platykurtic: beta(a,a) scaled to sd=1 (flatter peak, no tails)
  ch5_kurt_density <- reactive({
    ek <- input$ch5_kurt_val
    req(!is.null(ek))
    x_seq <- seq(-5, 5, length.out = 500)

    if (ek < -0.01) {
      # Platykurtyczny: beta(a,a) scaled to sd=1
      # excess_kurt of beta(a,a) = -6/(2a+3)
      # So a = -(6/ek + 3) / 2, but ek is negative
      # ek = -6/(2a+3) -> 2a+3 = -6/ek -> a = (-6/ek - 3)/2
      a <- max(1.01, (-6 / ek - 3) / 2)
      scale_b <- sqrt(2 * a + 1)
      # beta(a,a) on [0,1] mapped to [-scale_b, scale_b] for sd=1
      dens <- dbeta((x_seq / scale_b + 1) / 2, a, a) / (2 * scale_b)
      # Zero out beyond support
      dens[abs(x_seq) > scale_b] <- 0
    } else if (ek <= 0.01) {
      dens <- dnorm(x_seq)
    } else {
      # Leptokurtyczny: t-distribution scaled to sd=1
      # excess_kurt = 6 / (df - 4) -> df = 6/ek + 4
      df_mapped <- max(4.5, 6 / ek + 4)
      sd_t <- sqrt(df_mapped / (df_mapped - 2))
      dens <- dt(x_seq * sd_t, df = df_mapped) * sd_t
    }
    data.frame(x = x_seq, dens = dens, norm = dnorm(x_seq))
  })

  output$ch5_kurt_plot <- renderPlot({
    df <- ch5_kurt_density()
    ek <- input$ch5_kurt_val

    type_name <- if (ek < -0.1) "Platykurtyczny" else if (ek > 0.1) "Leptokurtyczny" else "Mezokurtyczny"
    type_color <- if (ek < -0.1) "#f39c12" else if (ek > 0.1) "#e74c3c" else "#27ae60"

    ggplot(df, aes(x = x)) +
      geom_line(aes(y = norm), color = "#95a5a6", linewidth = 1, linetype = "dashed") +
      geom_area(aes(y = dens), fill = type_color, alpha = 0.35) +
      geom_line(aes(y = dens), color = type_color, linewidth = 1.2) +
      annotate("text", x = -4.5, y = max(df$dens) * 0.95,
        label = paste0(type_name, " (kurtoza = ", round(ek, 1), ")"),
        color = type_color, hjust = 0, size = 5, fontface = "bold") +
      annotate("text", x = -4.5, y = max(df$dens) * 0.85,
        label = "Rozk\u0142ad normalny (kurtoza = 0)",
        color = "#95a5a6", hjust = 0, size = 4) +
      labs(x = "x", y = "G\u0119sto\u015b\u0107",
        title = "Jak kurtoza wp\u0142ywa na kszta\u0142t rozk\u0142adu?") +
      theme_minimal(base_size = 14)
  })

  output$ch5_kurt_tails <- renderPlot({
    df <- ch5_kurt_density()
    ek <- input$ch5_kurt_val
    type_color <- if (ek < -0.1) "#f39c12" else if (ek > 0.1) "#e74c3c" else "#27ae60"

    tail_df <- df[df$x >= 2.5, ]

    ggplot(tail_df, aes(x = x)) +
      geom_area(aes(y = norm), fill = "#95a5a6", alpha = 0.15) +
      geom_line(aes(y = norm), color = "#95a5a6", linewidth = 1, linetype = "dashed") +
      geom_area(aes(y = dens), fill = type_color, alpha = 0.3) +
      geom_line(aes(y = dens), color = type_color, linewidth = 1.2) +
      labs(x = "x", y = "G\u0119sto\u015b\u0107",
        title = "Powi\u0119kszenie prawego ogona (x > 2.5)") +
      theme_minimal(base_size = 14)
  })

  output$ch5_kurt_text <- renderUI({
    ek <- input$ch5_kurt_val
    req(!is.null(ek))

    if (ek < -0.5) {
      type_class <- "callout-warning"
      type_name <- "Platykurtyczny"
      desc <- "Rozk\u0142ad ma l\u017cejsze ogony ni\u017c normalny \u2014 warto\u015bci ekstremalne
               s\u0105 rzadsze. Dane s\u0105 bardziej 'skoncentrowane' w okolicy \u015bredniej,
               bez dalekich obserwacji."
    } else if (ek < 0.5) {
      type_class <- "callout-info"
      type_name <- "Mezokurtyczny"
      desc <- "Rozk\u0142ad jest zbli\u017cony do normalnego \u2014 ogony maj\u0105 'typow\u0105'
               ci\u0119\u017cko\u015b\u0107. To punkt odniesienia, wzgl\u0119dem kt\u00f3rego por\u00f3wnujemy
               inne rozk\u0142ady."
    } else {
      type_class <- "callout-danger"
      type_name <- "Leptokurtyczny"
      desc <- paste0("Rozk\u0142ad ma ci\u0119\u017csze ogony ni\u017c normalny \u2014 warto\u015bci
               ekstremalne pojawiaj\u0105 si\u0119 cz\u0119\u015bciej ni\u017c by\u015bmy oczekiwali.
               W finansach to oznacza wi\u0119ksze ryzyko ekstremalnych strat
               ('czarne \u0142ab\u0119dzie').")
    }

    div(class = type_class,
      tags$strong(paste0(type_name, " (nadwy\u017ckowa kurtoza = ", round(ek, 1), ")")),
      p(desc),
      tags$ul(
        tags$li(tags$b("Platykurtyczny"), " (kurtoza < 0): lekkie ogony, mniej ekstrema\u0142\u00f3w"),
        tags$li(tags$b("Mezokurtyczny"), " (kurtoza \u2248 0): rozk\u0142ad normalny \u2014 punkt odniesienia"),
        tags$li(tags$b("Leptokurtyczny"), " (kurtoza > 0): ci\u0119\u017ckie ogony, wi\u0119cej ekstrema\u0142\u00f3w")
      )
    )
  })

  # --------------------------------------------------------------------------
  # Widget 3: Full picture (Capstone)
  # --------------------------------------------------------------------------

  ch5_full_data <- reactive({
    req(input$ch5_full_var)
    vals <- student_data[[input$ch5_full_var]]
    vals <- vals[!is.na(vals)]
    list(
      values = vals,
      label = variable_meta[[input$ch5_full_var]]$label,
      var_name = input$ch5_full_var
    )
  })

  output$ch5_full_hist <- renderPlot({
    d <- ch5_full_data()
    vals <- d$values
    m <- mean(vals)
    med <- median(vals)
    df <- data.frame(x = vals)

    ggplot(df, aes(x = x)) +
      geom_histogram(aes(y = after_stat(density)),
        bins = 20, fill = "#3498db", color = "white", alpha = 0.5) +
      geom_density(color = "#2c3e50", linewidth = 1) +
      geom_rug(color = "#2c3e50", alpha = 0.5) +
      geom_vline(xintercept = m, color = "#e74c3c", linewidth = 1.1) +
      geom_vline(xintercept = med, color = "#3498db", linewidth = 1.1, linetype = "dashed") +
      annotate("text", x = m, y = Inf, vjust = 2, hjust = -0.1,
        label = paste0("Średnia = ", round(m, 2)),
        color = "#e74c3c", size = 3.8, fontface = "bold") +
      annotate("text", x = med, y = Inf, vjust = 3.5, hjust = -0.1,
        label = paste0("Mediana = ", round(med, 2)),
        color = "#3498db", size = 3.8, fontface = "bold") +
      labs(
        title = paste0("Rozkład zmiennej: ", d$label),
        x = d$label,
        y = "Gęstość"
      ) +
      theme_minimal(base_size = 14)
  })

  output$ch5_full_box <- renderPlot({
    d <- ch5_full_data()
    df <- data.frame(x = d$values)

    ggplot(df, aes(x = x)) +
      geom_boxplot(fill = "#3498db", alpha = 0.4, color = "#2c3e50",
        outlier.color = "#e74c3c", outlier.size = 3) +
      labs(x = d$label) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      )
  })

  output$ch5_full_table <- renderTable({
    d <- ch5_full_data()
    vals <- d$values

    n <- length(vals)
    m <- mean(vals)
    med <- median(vals)
    s <- sd(vals)
    v <- var(vals)
    rng <- diff(range(vals))
    q1 <- quantile(vals, 0.25, names = FALSE)
    q3 <- quantile(vals, 0.75, names = FALSE)
    iqr_val <- IQR(vals)
    cv <- (s / m) * 100
    sk <- e1071::skewness(vals)
    ku <- e1071::kurtosis(vals)
    trimmed <- mean(vals, trim = 0.1)

    # Mode: bin with highest frequency
    h <- hist(vals, breaks = 20, plot = FALSE)
    mode_bin_idx <- which.max(h$counts)
    mode_val <- (h$breaks[mode_bin_idx] + h$breaks[mode_bin_idx + 1]) / 2

    stats_df <- data.frame(
      Statystyka = c(
        "n", "Srednia", "Mediana", "Dominanta (środek przedziałowy)",
        "Sr. ucinana 10%",
        "Odch. std.", "Wariancja", "Rozstep", "IQR", "CV (%)",
        "Minimum", "Q1", "Q3", "Maksimum",
        "Skośność", "Kurtoza"
      ),
      Wartość = c(
        as.character(n),
        formatC(m, format = "f", digits = 2),
        formatC(med, format = "f", digits = 2),
        formatC(mode_val, format = "f", digits = 2),
        formatC(trimmed, format = "f", digits = 2),
        formatC(s, format = "f", digits = 2),
        formatC(v, format = "f", digits = 2),
        formatC(rng, format = "f", digits = 2),
        formatC(iqr_val, format = "f", digits = 2),
        formatC(cv, format = "f", digits = 1),
        formatC(min(vals), format = "f", digits = 2),
        formatC(q1, format = "f", digits = 2),
        formatC(q3, format = "f", digits = 2),
        formatC(max(vals), format = "f", digits = 2),
        formatC(sk, format = "f", digits = 3),
        formatC(ku, format = "f", digits = 3)
      ),
      stringsAsFactors = FALSE
    )
    stats_df
  }, striped = TRUE, hover = TRUE, width = "100%", align = "lr")

  output$ch5_full_interpretation <- renderUI({
    d <- ch5_full_data()
    vals <- d$values
    label <- d$label

    m <- mean(vals)
    med <- median(vals)
    s <- sd(vals)
    q1 <- quantile(vals, 0.25, names = FALSE)
    q3 <- quantile(vals, 0.75, names = FALSE)
    iqr_val <- IQR(vals)
    sk <- e1071::skewness(vals)

    # Outliers
    lower_fence <- q1 - 1.5 * iqr_val
    upper_fence <- q3 + 1.5 * iqr_val
    outliers_low <- vals[vals < lower_fence]
    outliers_high <- vals[vals > upper_fence]
    n_outliers <- length(outliers_low) + length(outliers_high)

    if (n_outliers > 0) {
      outlier_text <- paste0(
        "Wykryto ", n_outliers, " wartości odstających ",
        "(poza przedziałem [", round(lower_fence, 2), ", ",
        round(upper_fence, 2), "])."
      )
    } else {
      outlier_text <- "Brak wartości odstających (wg kryterium 1.5 * IQR)."
    }

    div(class = "callout-info",
      p(tags$strong("Podsumowanie:")),
      tags$ul(
        tags$li(paste0("Średnia = ", round(m, 2), ", Mediana = ", round(med, 2))),
        tags$li(paste0("Typowy student ma ", tolower(label),
          " między ", round(q1, 2), " a ", round(q3, 2), ".")),
        tags$li(paste0("Skośność = ", round(sk, 3), ", Kurtoza = ", round(e1071::kurtosis(vals), 3))),
        tags$li(outlier_text)
      )
    )
  })
}
