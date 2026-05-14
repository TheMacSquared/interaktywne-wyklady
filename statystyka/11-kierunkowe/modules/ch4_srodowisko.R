# ============================================================================
# CHAPTER 4: Inzynieria Srodowiska
# ============================================================================

ch4_ui <- lecture_chapter(
  id = "ch-srodowisko",
  num = "04",
  title = "Inżynieria Środowiska",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 04 · Materiał kierunkowy",
      num    = "04",
      title  = "Regresja, logarytmy i LOD.",
      lead   = "Dane środowiskowe bywają skośne, potęgowe i ucięte od dołu przez granicę wykrywalności."
    ),

    lc_h2("ch4-transformacje", "Kiedy prosta potrzebuje logarytmu"),
    p("Stężenia zanieczyszczeń często maleją nie liniowo, lecz procentowo: każdy kolejny kilometr od źródła obniża poziom o podobny procent. Wtedy model log-log bywa bardziej naturalny niż zwykła prosta na oryginalnej skali."),

    figure_panel(
      label = "Ryc. 4.1", title = "Stężenie metalu ciężkiego a odległość od źródła",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch4_lod", "Granica wykrywalności LOD:", min = 0.2, max = 2.0, value = 0.8, step = 0.1),
          radioButtons("ch4_scale", "Widok modelu:", choices = c("skala oryginalna" = "raw", "log-log" = "log"), selected = "raw"),
          uiOutput("ch4_lod_info")
        ),
        column(8, zoom_plot_ui("ch4_env_plot", height = "350px"))
      )
    ),

    lc_h2("ch4-lod", "Poniżej LOD to nie zero"),
    p("Wpisanie zera udaje, że wiemy więcej niż wiemy. Usunięcie obserwacji też zmienia pytanie. Proste podstawienia, jak LOD/2 albo LOD/√2, są szybkie i czasem wystarczą, ale przy dużym odsetku cenzorowania mogą zaniżyć średnią i zaburzyć korelację."),

    figure_panel(
      label = "Ryc. 4.2", title = "Jak wybór strategii LOD zmienia wynik regresji?",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Ta sama próbka, cztery szybkie decyzje analityczne. Przy małym udziale LOD różnice bywają kosmetyczne; przy dużym mogą zmienić interpretację nachylenia."),
          uiOutput("ch4_strategy_info")
        ),
        column(8, zoom_plot_ui("ch4_strategy_plot", height = "320px"))
      )
    ),

    lc_feedback(type = "info",
      tags$strong("Intuicja cenzorowania lewostronnego:"),
      " wartość istnieje, ale wiemy tylko, że jest mniejsza od progu oznaczalności."
    )
  )
)

ch4_server <- function(input, output, session) {
  env_data <- reactive(generate_env_lod(input$ch4_lod))

  zoom_plot_server("ch4_env_plot", reactive({
    d <- env_data()
    d$plot_value <- ifelse(is.na(d$observed), input$ch4_lod / 2, d$observed)
    if (input$ch4_scale == "log") {
      ggplot(d, aes(distance, plot_value)) +
        geom_point(aes(shape = below_lod, color = below_lod), size = 2.5, alpha = 0.75) +
        geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
        scale_x_log10() +
        scale_y_log10() +
        scale_color_manual(values = c("FALSE" = upwr_single, "TRUE" = upwr_accent), guide = "none") +
        labs(x = "Odległość od źródła (km, log)", y = "Stężenie (mg/kg, log)", shape = "Poniżej LOD")
    } else {
      ggplot(d, aes(distance, plot_value)) +
        geom_hline(yintercept = input$ch4_lod, linetype = "dashed", color = upwr_accent) +
        geom_point(aes(shape = below_lod, color = below_lod), size = 2.5, alpha = 0.75) +
        geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
        scale_color_manual(values = c("FALSE" = upwr_single, "TRUE" = upwr_accent), guide = "none") +
        labs(x = "Odległość od źródła (km)", y = "Stężenie lub LOD/2 (mg/kg)", shape = "Poniżej LOD")
    }
  }))

  output$ch4_lod_info <- renderUI({
    d <- env_data()
    share <- mean(d$below_lod)
    substituted <- ifelse(is.na(d$observed), input$ch4_lod / 2, d$observed)
    lc_stat_grid(
      lc_stat_box("Poniżej LOD", fmt_pct(share), color = upwr_accent),
      lc_stat_box("Średnia po LOD/2", fmt(mean(substituted), 2), caption = "proste podstawienie", color = upwr_secondary),
      columns = 1
    )
  })

  lod_strategies <- reactive({
    d <- env_data()
    lod <- input$ch4_lod
    strategies <- list(
      "Usuń <LOD" = d$observed,
      "0" = ifelse(is.na(d$observed), 0.001, d$observed),
      "LOD/2" = ifelse(is.na(d$observed), lod / 2, d$observed),
      "LOD/√2" = ifelse(is.na(d$observed), lod / sqrt(2), d$observed)
    )
    bind_rows(lapply(names(strategies), function(name) {
      y <- strategies[[name]]
      keep <- !is.na(y) & y > 0
      fit <- lm(log(y[keep]) ~ log(d$distance[keep]))
      data.frame(
        strategia = name,
        srednia = mean(y[keep]),
        nachylenie = coef(fit)[2],
        n = sum(keep)
      )
    }))
  })

  zoom_plot_server("ch4_strategy_plot", reactive({
    tab <- lod_strategies() |>
      pivot_longer(c(srednia, nachylenie), names_to = "metryka", values_to = "wartosc") |>
      mutate(
        metryka = recode(metryka, srednia = "Średnia stężenia", nachylenie = "Nachylenie log-log")
      )
    ggplot(tab, aes(strategia, wartosc, fill = strategia)) +
      geom_col(width = 0.6) +
      facet_wrap(~ metryka, scales = "free_y") +
      scale_fill_manual(values = upwr_cat_n(4), guide = "none") +
      labs(x = NULL, y = NULL)
  }))

  output$ch4_strategy_info <- renderUI({
    tab <- lod_strategies()
    spread <- max(tab$nachylenie) - min(tab$nachylenie)
    lc_stat_box("Rozpiętość nachyleń", fmt(spread, 2),
                caption = "różnica między strategiami obsługi <LOD", color = upwr_accent)
  })
}
