ch1_ui <- lecture_chapter(
  id = "ch-szereg",
  num = "01",
  title = "Szereg czasowy",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdzial 05 · Prognozy",
      num = "01",
      title = "Dane uporzadkowane w czasie.",
      lead = "W szeregu czasowym kolejnosc obserwacji jest czescia informacji."
    ),
    lc_h2("skladniki", "Skladniki szeregu"),
    tags$ul(
      tags$li("trend - dlugookresowy kierunek zmian,"),
      tags$li("sezonowosc - regularny wzorzec w roku, kwartale lub tygodniu,"),
      tags$li("wahania losowe - czesc nieprzewidywalna.")
    ),
    lc_chapter_next("02", "Model dynamiczny", "trend, sezonowosc i prognoza", "ch-prognoza")
  )
)

ch2_ui <- lecture_chapter(
  id = "ch-prognoza",
  num = "02",
  title = "Prognozowanie",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdzial 05 · Prognozy",
      num = "02",
      title = "Prognoza punktowa i przedzialowa.",
      lead = "Prosty model trendu z sezonowoscia daje start do rozmowy o regułach prognozowania."
    ),
    figure_panel(
      label = "Ryc. 5.1",
      title = "Trend, sezonowosc i horyzont prognozy",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("trend", "Trend", -1, 2, 0.7, step = 0.1),
          sliderInput("season", "Sezonowosc", 0, 20, 8, step = 1),
          sliderInput("noise", "Szum", 1, 15, 4, step = 1),
          sliderInput("h", "Horyzont prognozy", 4, 16, 8, step = 4)
        ),
        column(8, plotOutput("ts_plot", height = "380px"), uiOutput("ts_stats"))
      )
    ),
    lc_chapter_next("03", "Dokladnosc prognozy", "ex ante i ex post jako kolejne rozwiniecie", "ch-dokladnosc")
  )
)

ch3_ui <- lecture_chapter(
  id = "ch-dokladnosc",
  num = "03",
  title = "Dokladnosc prognozy",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdzial 05 · Prognozy",
      num = "03",
      title = "Miary ex ante i ex post.",
      lead = "Po prognozie potrzebujemy oceny: jak szeroki jest przedzial i jak model mylil sie na danych historycznych."
    ),
    tags$ul(
      tags$li("ex ante - ocena niepewnosci zanim poznamy realizacje zjawiska,"),
      tags$li("ex post - porownanie prognozy z pozniejsza obserwacja,"),
      tags$li("MAE, RMSE, MAPE - typowe miary bledu prognozy.")
    ),
    inline_callout("Do rozbudowy", color = "wskazowka", open = TRUE,
      "Mozna tu dodac zadanie: ukryj ostatnie 8 obserwacji, dopasuj model na reszcie i policz bledy ex post.")
  )
)

chapter_server <- function(input, output, session) {
  df <- reactive(eco_ts_data(trend = input$trend, season = input$season,
                             noise = input$noise, seed = 55))
  forecast <- reactive({
    fit <- lm(y ~ t + quarter, data = df())
    future <- data.frame(
      t = (max(df()$t) + 1):(max(df()$t) + input$h),
      quarter = factor(rep(1:4, length.out = input$h), levels = levels(df()$quarter))
    )
    cbind(future, as.data.frame(predict(fit, newdata = future, interval = "prediction")))
  })

  output$ts_plot <- renderPlot({
    f <- forecast()
    ggplot(df(), aes(t, y)) +
      geom_line(color = upwr_secondary) +
      geom_point(color = upwr_secondary) +
      geom_ribbon(data = f, aes(y = fit, ymin = lwr, ymax = upr),
                  fill = upwr_seq_burgundy[3], alpha = 0.2, inherit.aes = FALSE) +
      geom_line(data = f, aes(t, fit), color = upwr_accent, linewidth = 1) +
      geom_point(data = f, aes(t, fit), color = upwr_accent) +
      labs(x = "Okres", y = "Wartosc") +
      theme_upwr()
  })

  output$ts_stats <- renderUI({
    fit <- lm(y ~ t + quarter, data = df())
    lc_stat_grid(
      lc_stat_box("R2", eco_fmt(summary(fit)$r.squared, 3), color = unname(upwr_cat["niebo"])),
      lc_stat_box("RMSE", eco_fmt(sqrt(mean(residuals(fit)^2)), 2), color = unname(upwr_cat["terakota"])),
      lc_stat_box("Horyzont", input$h, caption = "okresow", color = upwr_accent),
      columns = 3
    )
  })
}
