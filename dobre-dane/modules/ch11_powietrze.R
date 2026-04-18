# Tab 11: Powietrze — airquality, braki danych + szereg czasowy

ch11_ui <- tabPanel("11. Powietrze",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Jakość powietrza w Nowym Jorku"),

    div(class = "narrative",
      p("Dane o jakości powietrza w Nowym Jorku. 153 pomiary z lata 1973.
        Zmienne: Ozone (ppb), Solar.R (promieniowanie), Wind (mph), Temp (F)."),
      p("Źródło: wbudowany zbiór 'airquality' w R.")
    ),

    div(class = "section-title", "Podgląd danych"),

    div(class = "widget-block",
      DT::dataTableOutput("tab10_table")
    ),

    div(class = "section-title", "Czy są braki danych?"),

    div(class = "widget-block",
      plotOutput("tab10_missing", height = "300px"),
      uiOutput("tab10_missing_info")
    ),

    div(class = "section-title", "Odkryj ukryty problem"),

    div(class = "widget-block",
      actionButton("tab10_reveal", "Pokaż dane w kolejności", class = "btn-warning btn-lg", width = "100%"),
      conditionalPanel("input.tab10_reveal > 0",
        plotOutput("tab10_lineplot", height = "350px"),
        div(class = "callout-danger",
          "To nie są niezależne obserwacje!",
          " To pomiary dzienne - widać wyraźną sezonowość.",
          tags$br(),
          "Temperatura i ozon zmieniają się sezonowo - każdy dzień zależy od poprzedniego."
        )
      )
    ),

    conditionalPanel("input.tab10_reveal > 0",
      div(class = "widget-block",
        h4("Autokorelacja - dowód braku niezależności"),
        plotOutput("tab10_lag", height = "300px"),
        uiOutput("tab10_autocorr_info")
      )
    ),

    div(class = "section-title", "Werdykt"),

    div(class = "callout-danger",
      tags$strong("DWA poważne problemy:"),
      tags$br(),
      tags$strong("1. Braki danych:"), " Ozone ma 24% braków (37 z 153). ",
      "Po usunięciu braków zostaje 111 obserwacji.",
      tags$br(),
      tags$strong("2. Brak niezależności:"), " To szereg czasowy! ",
      "Obserwacje dzienne są silnie autokorelowane.",
      tags$br(),
      "Klasyczne testy (t-test, korelacja Pearsona) zakładają niezależność obserwacji - ",
      "tutaj to założenie jest złamane."
    ),

    div(class = "callout-info",
      tags$strong("Czy można to uratować?"),
      tags$br(),
      "Agregacja miesięczna rozwiązałaby problem niezniezależności \u2014 średnia z całego
      miesiąca to jedna obserwacja, a kolejne miesiące są od siebie mniej zależne.",
      tags$br(),
      "Problem: ten zbiór obejmuje tylko 5 miesięcy (maj\u2013wrzesień 1973).
      Po agregacji zostaje n\u00a0=\u00a05 \u2014 za mało na jakąkolwiek analizę.",
      tags$br(),
      tags$em("Gdyby dane obejmowały wiele lat, agregacja miesięczna (np. średni ozon
      w każdym miesiącu roku przez 20 lat = 240 obserwacji) byłaby sensownym wyjściem.")
    ),

    uiOutput("tab10_verdict"),

    div(class = "chapter-transition",
      p("To był ostatni zbiór danych. Zobaczmy podsumowanie."),
      actionButton("ch10_next", "Dalej: 12. Ściąga \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  )))

ch11_server <- function(input, output, session) {

  aq <- airquality

  output$tab10_table <- DT::renderDataTable({
    datatable(round_df(aq), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$tab10_missing <- renderPlot({
    miss_pct <- sapply(aq, function(x) mean(is.na(x)) * 100)
    df_miss <- data.frame(variable = names(miss_pct), pct = miss_pct)
    df_miss <- df_miss[df_miss$pct > 0, ]   # ukryj zmienne bez braków
    df_miss$color <- ifelse(df_miss$pct > 20, col_bad,
                     ifelse(df_miss$pct > 5, col_mixed, col_good))

    ggplot(df_miss, aes(x = reorder(variable, -pct), y = pct, fill = color)) +
      geom_col() +
      scale_fill_identity() +
      geom_text(aes(label = paste0(round(pct, 1), "%")),
                vjust = -0.4, size = 5, fontface = "bold") +
      geom_hline(yintercept = 5,  linetype = "dashed", color = col_mixed, linewidth = 0.8) +
      geom_hline(yintercept = 20, linetype = "dashed", color = col_bad,   linewidth = 0.8) +
      annotate("text", x = Inf, y = 6.5,  label = "5% \u2014 akceptowalne",
               hjust = 1.05, color = col_mixed, size = 3.8) +
      annotate("text", x = Inf, y = 21.5, label = "20% \u2014 powa\u017cny problem",
               hjust = 1.05, color = col_bad,   size = 3.8) +
      scale_y_continuous(limits = c(0, 30)) +
      labs(title = "Procent braków danych (tylko zmienne z brakami)",
           x = NULL, y = "% braków") +
      theme_minimal(base_size = 14)
  })

  output$tab10_missing_info <- renderUI({
    ozone_na <- sum(is.na(aq$Ozone))
    solar_na <- sum(is.na(aq$Solar.R))
    n_complete <- sum(complete.cases(aq))
    div(class = "callout-warning",
      paste0("Ozone: ", ozone_na, " braków (", round(ozone_na / nrow(aq) * 100, 1), "%), ",
             "Solar.R: ", solar_na, " braków (", round(solar_na / nrow(aq) * 100, 1), "%). ",
             "Kompletne obserwacje: ", n_complete, " z ", nrow(aq), ".")
    )
  })

  output$tab10_lineplot <- renderPlot({
    aq$row <- 1:nrow(aq)
    ggplot(aq, aes(x = row, y = Ozone)) +
      geom_line(color = col_primary, alpha = 0.7) +
      geom_point(color = col_primary, size = 1.5, alpha = 0.5) +
      labs(title = "Ozone w kolejności obserwacji",
           subtitle = "Widać wyraźną sezonowość - to nie są niezależne pomiary!",
           x = "Numer obserwacji (= dzien)", y = "Ozone (ppb)") +
      theme_minimal(base_size = 14)
  })

  output$tab10_lag <- renderPlot({
    oz <- aq$Ozone
    oz_clean <- oz[!is.na(oz)]
    n <- length(oz_clean)
    lag_df <- data.frame(x = oz_clean[-n], y = oz_clean[-1])
    r <- round(cor(lag_df$x, lag_df$y), 3)

    ggplot(lag_df, aes(x = x, y = y)) +
      geom_point(alpha = 0.4, color = col_dark) +
      geom_smooth(method = "lm", color = col_bad, se = TRUE) +
      labs(title = paste0("Autokorelacja lag-1 (r = ", r, ")"),
           subtitle = "Jeśli obserwacje są niezależne, nie powinno być korelacji",
           x = "Ozone(t)", y = "Ozone(t+1)") +
      theme_minimal(base_size = 14)
  })

  output$tab10_autocorr_info <- renderUI({
    oz <- aq$Ozone[!is.na(aq$Ozone)]
    n <- length(oz)
    r <- cor(oz[-n], oz[-1])
    div(class = "callout-danger",
      paste0("Autokorelacja lag-1: r = ", round(r, 3), ". ",
             "Gdyby obserwacje były niezależne, oczekiwaliśmy r bliskiego 0. ",
             "Wartość ", round(r, 2), " oznacza silną zależność między kolejnymi dniami.")
    )
  })

  output$tab10_verdict <- renderUI({
    # hipoteza, n, mix, zmiennosc, struktura, niezaleznosc | braki, definicje, bledy
    # Powietrze: hipoteza ok, n ok, mix ok(ish), zmiennosc ok, struktura warn, niezaleznosc NO | braki NO, definicje ok, bledy ok
    render_verdict(c("yes", "yes", "yes", "yes", "warn", "no", "no", "yes", "yes"), "bad")
  })
}
