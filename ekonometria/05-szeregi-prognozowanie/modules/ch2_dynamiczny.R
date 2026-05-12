# ============================================================================
# ROZDZIAŁ 2: Modele dynamiczne i prognoza
# ============================================================================

ch2_ui <- lecture_chapter(
  id = "ch-prognoza",
  num = "02",
  title = "Modele dynamiczne i prognoza",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 05 · Prognozy",
      num = "02",
      title = "Trend, sezonowość, prognoza.",
      lead = "Mając trend i sezonowość, dopasowujemy prosty model regresji z czasem t i kwartałem jako zmiennymi. To daje nam prognozę punktową i — co ważniejsze — przedziałową."
    ),

    lc_h2("ch2-model", "Model regresji z trendem i sezonowością"),
    lc_p("Najprostszy „dynamiczny” model traktuje numer okresu i kwartał jak zwykłe zmienne objaśniające. Trend wchodzi liniowo (z każdym okresem o β1 więcej), a sezonowość — przez zmienne 0/1 dla kwartałów."),
    lc_formula_box(
      withMathJax(helpText("$$Y_t = \\beta_0 + \\beta_1 t + \\sum_{q=2}^{4} \\gamma_q D_{qt} + \\varepsilon_t$$")),
      tags$ul(
        tags$li(strong("t"), " — numer okresu (1, 2, 3, …)."),
        tags$li(strong("D_qt"), " — zmienne 0/1 dla kwartałów (kwartał 1 jest referencyjny)."),
        tags$li(strong("γ_q"), " — przeciętna różnica między kwartałem q a referencyjnym.")
      )
    ),

    lc_h2("ch2-prognoza-vs-przedzial", "Prognoza punktowa i przedziałowa"),
    lc_p("Prognoza punktowa to jedna liczba — najlepsze nasze przypuszczenie. Prognoza przedziałowa to zakres, w którym z określonym prawdopodobieństwem (typowo 95%) znajdzie się rzeczywista wartość. Dobra prognoza to prawie zawsze przedział, nie liczba."),

    lc_h2("ch2-widget", "Eksperyment z trendem i sezonem"),
    figure_panel(
      label = "Ryc. 2.1",
      title = "Dopasowanie modelu i przedział prognozy",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          sliderInput("ch2_trend",  "Siła trendu",                 min = -1, max = 2,  value = 0.7, step = 0.1),
          sliderInput("ch2_season", "Amplituda sezonowości",       min = 0,  max = 20, value = 8,   step = 1),
          sliderInput("ch2_noise",  "Szum",                         min = 1,  max = 15, value = 4,   step = 1),
          sliderInput("ch2_h",      "Horyzont prognozy (kwartałów)", min = 4, max = 16, value = 8,   step = 4)
        ),
        column(
          8,
          plotOutput("ch2_plot", height = "380px"),
          uiOutput("ch2_stats"),
          uiOutput("ch2_feedback")
        )
      )
    ),

    lc_h2("ch2-reguly", "Trzy reguły rozsądnego prognozowania"),
    tags$ol(
      tags$li(strong("Założenie stabilności:"), " prognozujemy zakładając, że mechanizm nie zmieni się w przyszłości. To często nieprawda — kryzys, zmiana polityki, pandemia rozbijają najlepiej dopasowany model."),
      tags$li(strong("Krótki horyzont:"), " im dalej w przyszłość, tym szerszy przedział i mniejsza ufność w prognozę. Cztery kwartały do przodu to zwykle granica sensownej precyzji."),
      tags$li(strong("Sygnalizuj niepewność:"), " zawsze podawaj przedział, nie tylko punkt. Klient łatwiej zrozumie „od X do Y” niż samo „Z”, a Ty nie zostajesz złapany na pojedynczej liczbie.")
    ),

    inline_callout(
      label = "Pułapka",
      color = "uwaga",
      "Wąski przedział ufności na podstawie krótkiego szeregu = fałszywe poczucie pewności. Im mniej historii, tym ostrożniej."
    ),

    lc_chapter_next(
      num = "03",
      title = "Dokładność prognozy",
      lead = "miary ex ante i ex post",
      target_id = "ch-dokladnosc"
    )
  )
)

ch2_server <- function(input, output, session) {
  ch2_df <- reactive({
    eco_ts_data(n = 48,
                trend  = input$ch2_trend,
                season = input$ch2_season,
                noise  = input$ch2_noise,
                seed   = 55)
  })

  ch2_fit <- reactive({
    lm(y ~ t + quarter, data = ch2_df())
  })

  ch2_forecast <- reactive({
    df <- ch2_df()
    h  <- input$ch2_h
    future <- data.frame(
      t = (max(df$t) + 1):(max(df$t) + h),
      quarter = factor(rep(1:4, length.out = h), levels = levels(df$quarter))
    )
    pred <- as.data.frame(predict(ch2_fit(), newdata = future, interval = "prediction"))
    cbind(future, pred)
  })

  output$ch2_plot <- renderPlot({
    df <- ch2_df()
    f  <- ch2_forecast()
    ggplot(df, aes(t, y)) +
      geom_line(color = upwr_secondary, linewidth = 0.6) +
      geom_point(color = upwr_secondary, size = 1.8) +
      geom_ribbon(data = f, aes(x = t, ymin = lwr, ymax = upr),
                  fill = upwr_seq_burgundy[3], alpha = 0.25, inherit.aes = FALSE) +
      geom_line(data = f, aes(t, fit), color = upwr_accent, linewidth = 1) +
      geom_point(data = f, aes(t, fit), color = upwr_accent, size = 2) +
      geom_vline(xintercept = max(df$t) + 0.5, linetype = "dashed",
                 color = unname(upwr_cat["grafit"]), alpha = 0.5) +
      labs(x = "Okres (kwartał)", y = "Wartość Y") +
      theme_upwr()
  })

  output$ch2_stats <- renderUI({
    fit <- ch2_fit()
    g <- broom::glance(fit)
    rmse <- sqrt(mean(residuals(fit)^2))
    lc_stat_grid(
      lc_stat_box("R²", eco_fmt(g$r.squared, 3),
                  caption = "część zmienności wyjaśniona przez model",
                  color = unname(upwr_cat["niebo"])),
      lc_stat_box("RMSE", eco_fmt(rmse, 2),
                  caption = "przeciętna pomyłka na danych historycznych",
                  color = unname(upwr_cat["terakota"])),
      lc_stat_box("Horyzont", input$ch2_h,
                  caption = "kwartałów w przód",
                  color = upwr_accent),
      columns = 3
    )
  })

  output$ch2_feedback <- renderUI({
    fit <- ch2_fit()
    g <- broom::glance(fit)
    rmse <- sqrt(mean(residuals(fit)^2))
    f <- ch2_forecast()
    band_first <- (f$upr[1] - f$lwr[1]) / 2
    band_last  <- (f$upr[nrow(f)] - f$lwr[nrow(f)]) / 2
    growth <- band_last / band_first

    msg <- paste0(
      "Model wyjaśnia ", eco_fmt(100 * g$r.squared, 1),
      "% zmienności szeregu. Przeciętna pomyłka na danych historycznych to ±",
      eco_fmt(rmse, 2), " jednostek. ",
      "Prognoza na ", input$ch2_h,
      " kwartałów rozszerza się od ±", eco_fmt(band_first, 2),
      " w pierwszym kwartale do ±", eco_fmt(band_last, 2),
      " w ostatnim (", eco_fmt(growth, 2),
      "× szerzej) — niepewność rośnie z horyzontem."
    )
    lc_feedback(type = "info", lc_p(msg))
  })
}
