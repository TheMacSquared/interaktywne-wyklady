# ============================================================================
# ROZDZIAŁ 3: Dokładność prognozy
# ============================================================================

ch3_ui <- lecture_chapter(
  id = "ch-dokladnosc",
  num = "03",
  title = "Dokładność prognozy",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 05 · Prognozy",
      num = "03",
      title = "Skąd wiemy, czy prognoza jest dobra?",
      lead = "Skąd wiemy, czy prognoza jest dobra, zanim zobaczymy przyszłość? Symulujemy: ukrywamy ostatnie h obserwacji, prognozujemy je, porównujemy z rzeczywistością."
    ),

    lc_h2("ch3-ex-ante-post", "Ex ante a ex post"),
    lc_p("Dokładność prognozy ocenia się na dwa sposoby — z modelu (jeszcze przed faktem) i przez porównanie z realizacją (po fakcie). Każdy odpowiada na inne pytanie."),
    figure_panel(
      label = "Tabela 3.1",
      title = "Dwa rodzaje oceny prognozy",
      tags$table(
        class = "table",
        tags$thead(tags$tr(
          tags$th("Rodzaj"),
          tags$th("Co mierzy?"),
          tags$th("Z czego liczona?")
        )),
        tags$tbody(
          tags$tr(
            tags$td(strong("Ex ante")),
            tags$td("niepewność wynikającą z modelu i danych"),
            tags$td("błąd standardowy prognozy, przedziały ufności")
          ),
          tags$tr(
            tags$td(strong("Ex post")),
            tags$td("rzeczywistą trafność prognozy"),
            tags$td("porównanie prognozy z obserwacją (wymaga ukrycia danych albo czekania)")
          )
        )
      )
    ),

    lc_h2("ch3-miary", "Trzy miary błędu prognozy"),
    lc_p("Kiedy mamy już realizację, możemy policzyć, jak bardzo prognoza się myliła. Każda miara robi to trochę inaczej:"),
    lc_formula_box(
      withMathJax(helpText("$$MAE = \\frac{1}{h}\\sum |y_t - \\hat y_t|, \\quad RMSE = \\sqrt{\\frac{1}{h}\\sum (y_t - \\hat y_t)^2}, \\quad MAPE = \\frac{100}{h}\\sum \\left|\\frac{y_t - \\hat y_t}{y_t}\\right|\\%$$")),
      tags$ul(
        tags$li(strong("MAE"), " — średni błąd absolutny, w jednostkach Y."),
        tags$li(strong("RMSE"), " — pierwiastek ze średniego kwadratu błędu, w jednostkach Y; mocniej karze duże pomyłki."),
        tags$li(strong("MAPE"), " — średni procentowy błąd, pozwala porównywać modele dla zmiennych w różnych skalach.")
      )
    ),

    lc_h2("ch3-widget", "Test prognozy ex post — ukryjmy ostatnie h kwartałów"),
    lc_p("Standardowa metoda: dzielimy szereg na część treningową i testową. Trenujemy model na początku, prognozujemy końcówkę i porównujemy z rzeczywistością."),
    figure_panel(
      label = "Ryc. 3.1",
      title = "Train / test split na szeregu czasowym",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          sliderInput("ch3_h",     "Ile ostatnich kwartałów ukryć?", min = 4, max = 16, value = 8, step = 4),
          sliderInput("ch3_noise", "Poziom szumu w danych",          min = 1, max = 15, value = 4, step = 1)
        ),
        column(
          8,
          plotOutput("ch3_plot", height = "380px"),
          uiOutput("ch3_stats"),
          uiOutput("ch3_feedback")
        )
      )
    ),

    inline_callout(
      label = "Wskazówka",
      color = "wskazowka",
      "Dobry workflow prognostyczny: wytrenuj model na historii bez ostatnich h obserwacji, oceń ex post, dopiero wtedy wytrenuj model na całych danych i prognozuj przyszłość."
    ),

    lc_chapter_next(
      num = "04",
      title = "Twoja prognoza",
      lead = "ćwiczenie z wyboru modelu",
      target_id = "ch-cwiczenie"
    )
  )
)

ch3_server <- function(input, output, session) {
  ch3_df <- reactive({
    eco_ts_data(n = 48, trend = 0.8, season = 8,
                noise = input$ch3_noise, seed = 77)
  })

  ch3_split <- reactive({
    df <- ch3_df()
    h  <- input$ch3_h
    n  <- nrow(df)
    list(
      train = df[seq_len(n - h), , drop = FALSE],
      test  = df[(n - h + 1):n, , drop = FALSE]
    )
  })

  ch3_pred <- reactive({
    s <- ch3_split()
    fit <- lm(y ~ t + quarter, data = s$train)
    pred <- as.data.frame(predict(fit, newdata = s$test, interval = "prediction"))
    cbind(s$test, pred)
  })

  ch3_metrics <- reactive({
    p <- ch3_pred()
    err <- p$y - p$fit
    list(
      MAE  = mean(abs(err)),
      RMSE = sqrt(mean(err^2)),
      MAPE = 100 * mean(abs(err / p$y))
    )
  })

  output$ch3_plot <- renderPlot({
    s <- ch3_split()
    p <- ch3_pred()
    cutoff <- max(s$train$t) + 0.5

    ggplot() +
      geom_line(data = s$train, aes(t, y), color = upwr_secondary, linewidth = 0.6) +
      geom_point(data = s$train, aes(t, y), color = upwr_secondary, size = 1.8) +
      geom_ribbon(data = p, aes(x = t, ymin = lwr, ymax = upr),
                  fill = upwr_seq_burgundy[3], alpha = 0.25) +
      geom_line(data = p, aes(t, fit), color = upwr_accent, linewidth = 1) +
      geom_point(data = p, aes(t, fit), color = upwr_accent, size = 2) +
      geom_point(data = p, aes(t, y), color = unname(upwr_cat["szalwia"]),
                 size = 2.5, shape = 17) +
      geom_vline(xintercept = cutoff, linetype = "dashed",
                 color = unname(upwr_cat["grafit"]), alpha = 0.6) +
      annotate("text", x = cutoff, y = max(c(s$train$y, p$y, p$upr)),
               label = " ← trening | test → ", hjust = 0.5, vjust = 1,
               color = unname(upwr_cat["grafit"]), size = 3.5) +
      labs(x = "Okres (kwartał)", y = "Wartość Y",
           caption = "Trójkąty: prawdziwe wartości testowe; linia: prognoza modelu") +
      theme_upwr()
  })

  output$ch3_stats <- renderUI({
    m <- ch3_metrics()
    lc_stat_grid(
      lc_stat_box("MAE", eco_fmt(m$MAE, 2),
                  caption = "średni błąd absolutny",
                  color = unname(upwr_cat["niebo"])),
      lc_stat_box("RMSE", eco_fmt(m$RMSE, 2),
                  caption = "pierwiastek ze średniego kwadratu",
                  color = unname(upwr_cat["terakota"])),
      lc_stat_box("MAPE", paste0(eco_fmt(m$MAPE, 1), "%"),
                  caption = "średni błąd procentowy",
                  color = unname(upwr_cat["szalwia"])),
      columns = 3
    )
  })

  output$ch3_feedback <- renderUI({
    m <- ch3_metrics()
    h <- input$ch3_h
    rmse_to_mae <- m$RMSE / m$MAE
    diag <- if (rmse_to_mae > 1.3) {
      "RMSE wyraźnie większe od MAE — model miał pojedyncze duże pomyłki, które „ciągną” średni kwadrat w górę."
    } else {
      "RMSE niewiele większe od MAE — błędy są raczej wyrównane, bez pojedynczych dużych wpadek."
    }
    msg <- paste0(
      "Na ukrytych ", h, " kwartałach prognoza myliła się średnio o ±",
      eco_fmt(m$MAE, 2), " jednostek (MAPE = ", eco_fmt(m$MAPE, 1),
      "%). RMSE = ", eco_fmt(m$RMSE, 2), ". ", diag
    )
    lc_feedback(type = "info", lc_p(msg))
  })
}
