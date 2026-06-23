# ============================================================================
# CHAPTER 2: Anatomia szeregu — dekompozycja
# ============================================================================

ch2_ui <- list(
  id    = "ch-dekompozycja",
  num   = "02",
  title = "Anatomia szeregu: dekompozycja",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 02 · Szeregi czasowe",
      num    = "02",
      title  = "Anatomia szeregu czasowego.",
      lead   = "Każdy szereg da się rozłożyć na składowe: trend, sezonowość i reszta losowa.
                To rozkład STL — mapa, która porządkuje cały wykład."
    ),

    lc_h2("ch2-skladowe", "Trzy składowe"),

    tagList(
      lc_p("Większość szeregów, z którymi zetkniesz się w praktyce, nie jest czystym
        chaosem ani prostą linią. Składają się na nie trzy warstwy:"),
      tags$ul(
        tags$li(tags$strong("Trend (T):"),
          " długoterminowa tendencja wzrostu lub spadku.
          Temperatura rośnie o ≈0,025°C rocznie; bezrobocie maleje przez lata hossy."),
        tags$li(tags$strong("Sezonowość (S):"),
          " regularny, powtarzający się wzorzec o stałym okresie.
          Temperatura jest najniższa w styczniu, najwyższa w lipcu — co roku."),
        tags$li(tags$strong("Reszta (R):"),
          " to, czego trend i sezonowość nie wyjaśniły.
          Może być losowa (biały szum) lub zawierać niewyjaśnione struktury.")
      ),
      lc_p("Formalnie szereg ", tags$em("x_t"), " zapisujemy jako:"),
      lc_formula_box(
        withMathJax(helpText("$$x_t = T_t + S_t + R_t \\quad \\text{(model addytywny)}$$")),
        withMathJax(helpText("$$x_t = T_t \\times S_t \\times R_t \\quad \\text{(model multiplikatywny)}$$")),
        p(tags$strong("Addytywny"), " — gdy amplituda wahań sezonowych jest stała (np. temperatura)."),
        p(tags$strong("Multiplikatywny"), " — gdy wahania rosną proporcjonalnie do poziomu (np. sprzedaż).")
      )
    ),

    lc_h2("ch2-syntetyczny", "Widget: zbuduj szereg z części"),

    tagList(
      lc_p("Zanim zobaczymy dekompozycję na danych rzeczywistych, zbudujmy szereg
        od podstaw — sumując składowe. Suwaki kontrolują amplitude każdej z warstw.")
    ),

    figure_panel(
      label = "Ryc. 2.1", title = "Syntetyczny szereg — suma składowych",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch2_syn_trend", "Nachylenie trendu:",
                      min = -0.5, max = 0.5, value = 0.1, step = 0.05),
          sliderInput("ch2_syn_seas", "Amplituda sezonowości:",
                      min = 0, max = 15, value = 5, step = 0.5),
          sliderInput("ch2_syn_noise", "Szum losowy (σ):",
                      min = 0, max = 5, value = 1.5, step = 0.25),
          radioButtons("ch2_syn_type", "Model:",
                       choices = c("Addytywny" = "add", "Multiplikatywny" = "mult"),
                       selected = "add", inline = TRUE)
        ),
        column(8,
          zoom_plot_ui("ch2_syn_plot", height = "340px")
        )
      )
    ),

    lc_h2("ch2-stl", "Dekompozycja STL na prawdziwych danych"),

    tagList(
      lc_p("STL (", tags$em("Seasonal-Trend decomposition using LOESS"), ")
        to robustna metoda rozkładu używana zarówno przez analityków, jak i w pakiecie R ",
        tags$code("forecast"), ". Wydziela trend metodą LOESS, a sezonowość przez cykliczne
        uśrednianie — i jest odporna na obserwacje odstające.")
    ),

    figure_panel(
      label = "Ryc. 2.2", title = "STL — dekompozycja wybranego szeregu",
      full_width = TRUE,
      fluidRow(
        column(3,
          selectInput("ch2_stl_data", "Szereg:",
                      choices = .ts_choices_for("warszawa", "bezrobocie", "noclegi", "sprzedaz"),
                      selected = "warszawa"),
          radioButtons("ch2_stl_type", "Model:",
                       choices = c("Addytywny" = "additive", "Multiplikatywny" = "multiplicative"),
                       selected = "additive"),
          uiOutput("ch2_stl_info")
        ),
        column(9,
          zoom_plot_ui("ch2_stl_plot", height = "420px")
        )
      )
    ),

    lc_h2("ch2-interpretacja", "Jak czytać dekompozycję?"),

    tagList(
      lc_p("Dekompozycja daje cztery wykresy: oryginał, trend, sezonowość i reszta.
        Przy interpretacji patrz na:"),
      tags$ul(
        tags$li(tags$strong("Skalę osi Y."),
          " Jeśli reszta ma skalę porównywalną do sezonowości — model źle opisuje wzorzec."),
        tags$li(tags$strong("Wzorzec w reszcie."),
          " Dobra dekompozycja zostawia w reszcie los, nie strukturę."),
        tags$li(tags$strong("Zmienność sezonowości."),
          " Rosnąca amplituda sugeruje model multiplikatywny.")
      ),
      margin_callout(label = "Zapamiętaj", color = "wskazowka",
        "Dekompozycja to opis, nie model. Możemy ją czytać i interpretować,
         ale nie prognozować z niej bezpośrednio — do tego służą ARIMA i ETS."
      )
    ),

    lc_chapter_next(
      num       = "03",
      title     = "Trend: jak go mierzyć i usuwać",
      lead      = "regresja liniowa, LOESS, średnia krocząca — trzy podejścia",
      target_id = "ch-trend"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch2_server <- function(input, output, session) {

  zoom_plot_server("ch2_syn_plot", reactive({
    slope   <- input$ch2_syn_trend
    seas_a  <- input$ch2_syn_seas
    noise_s <- input$ch2_syn_noise
    model   <- input$ch2_syn_type

    set.seed(42)
    n   <- 96
    t   <- seq_len(n)
    trend_comp <- slope * t
    seas_comp  <- seas_a * sin(2 * pi * t / 12)
    noise_comp <- rnorm(n, 0, noise_s)

    y <- if (model == "add") {
      10 + trend_comp + seas_comp + noise_comp
    } else {
      pmax(0.5, (10 + trend_comp) * (1 + seas_comp / 20) + noise_comp)
    }

    df_long <- data.frame(
      t     = rep(t, 4),
      value = c(y, 10 + trend_comp, seas_comp, noise_comp),
      comp  = rep(c("Oryginał", "Trend", "Sezonowość", "Reszta"), each = n)
    )
    df_long$comp <- factor(df_long$comp,
                           levels = c("Oryginał", "Trend", "Sezonowość", "Reszta"))

    ggplot(df_long, aes(x = t, y = value)) +
      geom_line(color = upwr_accent, linewidth = 0.8) +
      facet_wrap(~ comp, ncol = 1, scales = "free_y") +
      labs(x = "Czas (miesiące)", y = NULL) +
      theme_upwr() +
      theme(strip.text = element_text(face = "bold"))
  }))

  ch2_stl_result <- reactive({
    key  <- input$ch2_stl_data
    type <- input$ch2_stl_type
    ts_obj <- .ts_datasets[[key]]$get_ts()
    stl(ts_obj, s.window = "periodic", robust = TRUE)
  })

  zoom_plot_server("ch2_stl_plot", reactive({
    key    <- input$ch2_stl_data
    info   <- .ts_datasets[[key]]
    stl_r  <- ch2_stl_result()
    comp   <- stl_r$time.series
    ts_obj <- .ts_datasets[[key]]$get_ts()
    n      <- length(ts_obj)

    t_vals <- time(ts_obj)
    years  <- floor(t_vals)
    months <- round((t_vals - years) * 12) + 1
    date   <- as.Date(paste(years, months, "01", sep = "-"))

    df <- data.frame(
      date      = rep(date, 4),
      value     = c(as.numeric(ts_obj),
                    as.numeric(comp[, "trend"]),
                    as.numeric(comp[, "seasonal"]),
                    as.numeric(comp[, "remainder"])),
      component = rep(c("Oryginał", "Trend", "Sezonowość", "Reszta"), each = n)
    )
    df$component <- factor(df$component,
                           levels = c("Oryginał", "Trend", "Sezonowość", "Reszta"))

    colors <- c(
      "Oryginał"    = upwr_secondary,
      "Trend"       = unname(upwr_cat["niebo"]),
      "Sezonowość"  = unname(upwr_cat["szalwia"]),
      "Reszta"      = unname(upwr_cat["terakota"])
    )

    ggplot(df, aes(x = date, y = value, color = component)) +
      geom_line(linewidth = 0.85) +
      facet_wrap(~ component, ncol = 1, scales = "free_y") +
      scale_color_manual(values = colors, guide = "none") +
      labs(x = NULL, y = info$unit) +
      theme_upwr() +
      theme(strip.text = element_text(face = "bold"))
  }))

  output$ch2_stl_info <- renderUI({
    key  <- input$ch2_stl_data
    info <- .ts_datasets[[key]]
    lc_feedback(type = "info",
      p(tags$strong(info$label)),
      p(style = "margin-top:4px; font-size: 0.85em;", info$desc)
    )
  })
}
