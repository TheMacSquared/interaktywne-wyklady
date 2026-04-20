# Tab 11: Kawiarnia — sprzedaż dzienna, braki danych + szereg czasowy

ch11_ui <- tabPanel("11. Kawiarnia",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Sprzedaż w kawiarni studenckiej"),

    div(class = "narrative",
      p("Dane ze sprzedaży kawiarni działającej w kampusie uczelni. 245 dni obserwacji
        (rok akademicki: październik\u2013czerwiec). Zmienne: data, dzień tygodnia,
        liczba sprzedanych kaw i temperatura zewnętrzna."),
      p("Chcemy zbadać, czy temperatura wpływa na sprzedaż.")
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
          " Widać wyraźną periodyczność tygodniową \u2014 każdy poniedziałek wysoki,
          każdy weekend niski. Każdy dzień jest podobny do poprzedniego.",
          tags$br(),
          "Testy statystyczne zakładają niezależność obserwacji \u2014 to założenie jest tutaj złamane."
        )
      )
    ),

    conditionalPanel("input.tab10_reveal > 0",
      div(class = "widget-block",
        h4("Autokorelacja \u2014 dowód braku niezależności"),
        plotOutput("tab10_lag", height = "300px"),
        uiOutput("tab10_autocorr_info")
      )
    ),

    div(class = "section-title", "Werdykt"),

    div(class = "callout-danger",
      tags$strong("DWA poważne problemy:"),
      tags$br(),
      tags$strong("1. Braki danych:"), " kawy ma ", uiOutput("tab10_missing_pct", inline = TRUE), " braków
      (kawiarnia zamknięta w święta, awarie systemu).",
      tags$br(),
      tags$strong("2. Brak niezależności:"), " To szereg czasowy! ",
      "Obserwacje dzienne są silnie autokorelowane \u2014 głównie przez cotygodniową periodyczność.",
      tags$br(),
      "Korelacja Pearsona między temperaturą a sprzedażą byłaby błędem metodologicznym."
    ),

    div(class = "callout-info",
      tags$strong("Czy można to uratować?"),
      tags$br(),
      "Agregacja tygodniowa rozwiązałaby problem zależności \u2014 średnia sprzedaż z całego tygodnia
      to jedna obserwacja, a kolejne tygodnie są od siebie znacznie mniej zależne.",
      tags$br(),
      "Po agregacji: n = ~35 tygodni \u2014 skromnie, ale możliwe do analizy.",
      tags$br(),
      tags$em("Gdyby dane obejmowały kilka lat akademickich, mielibyśmy 35 tyg. × 3 lata = ok. 105
      obserwacji \u2014 wtedy analiza sezonowości i trendów byłaby pełnoprawna.")
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

  output$tab10_table <- DT::renderDataTable({
    datatable(round_df(cafe_data), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$tab10_missing <- renderPlot({
    miss_pct <- sapply(cafe_data[, c("kawy", "temperatura")], function(x) mean(is.na(x)) * 100)
    df_miss  <- data.frame(variable = names(miss_pct), pct = miss_pct)
    df_miss  <- df_miss[df_miss$pct > 0, ]
    df_miss$color <- ifelse(df_miss$pct > 20, col_bad,
                     ifelse(df_miss$pct > 5,  col_mixed, col_good))

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
    kawy_na <- sum(is.na(cafe_data$kawy))
    temp_na <- sum(is.na(cafe_data$temperatura))
    n_comp  <- sum(complete.cases(cafe_data[, c("kawy", "temperatura")]))
    div(class = "callout-warning",
      paste0("kawy: ", kawy_na, " braków (", round(kawy_na / nrow(cafe_data) * 100, 1), "%) \u2014 ",
             "zamknięcia i awarie. ",
             "temperatura: ", temp_na, " braków (", round(temp_na / nrow(cafe_data) * 100, 1), "%) \u2014 ",
             "akceptowalne. ",
             "Kompletne obserwacje: ", n_comp, " z ", nrow(cafe_data), ".")
    )
  })

  output$tab10_missing_pct <- renderUI({
    kawy_na  <- sum(is.na(cafe_data$kawy))
    pct      <- round(kawy_na / nrow(cafe_data) * 100, 1)
    tags$strong(paste0(pct, "%"))
  })

  output$tab10_lineplot <- renderPlot({
    df <- cafe_data[!is.na(cafe_data$kawy), ]
    ggplot(df, aes(x = dzien, y = kawy)) +
      geom_line(color = col_primary, alpha = 0.6) +
      geom_point(color = col_primary, size = 1.2, alpha = 0.4) +
      labs(title = "Sprzedaż kaw w kolejności dni",
           subtitle = "Widoczna periodyczność tygodniowa \u2014 obserwacje nie są niezależne!",
           x = "Numer dnia (= kolejność w roku akademickim)", y = "Liczba sprzedanych kaw") +
      theme_minimal(base_size = 14)
  })

  output$tab10_lag <- renderPlot({
    kw        <- cafe_data$kawy[!is.na(cafe_data$kawy)]
    n         <- length(kw)
    lag_df    <- data.frame(x = kw[-n], y = kw[-1])
    r         <- round(cor(lag_df$x, lag_df$y), 3)

    ggplot(lag_df, aes(x = x, y = y)) +
      geom_point(alpha = 0.4, color = col_dark) +
      geom_smooth(method = "lm", color = col_bad, se = TRUE) +
      labs(title = paste0("Autokorelacja lag-1 (r = ", r, ")"),
           subtitle = "Je\u015bli obserwacje s\u0105 niezale\u017cne, nie powinno by\u0107 korelacji",
           x = "Kawy(t)", y = "Kawy(t+1)") +
      theme_minimal(base_size = 14)
  })

  output$tab10_autocorr_info <- renderUI({
    kw <- cafe_data$kawy[!is.na(cafe_data$kawy)]
    n  <- length(kw)
    r  <- cor(kw[-n], kw[-1])
    div(class = "callout-danger",
      paste0("Autokorelacja lag-1: r = ", round(r, 3), ". ",
             "Gdyby obserwacje były niezależne, oczekiwaliśmy r bliskiego 0. ",
             "Wartość ", round(r, 2), " oznacza silną zależność między kolejnymi dniami \u2014 ",
             "głównie przez powtarzający się rytm tygodnia.")
    )
  })

  output$tab10_verdict <- renderUI({
    render_verdict(c("yes", "yes", "yes", "yes", "warn", "no", "no", "yes", "yes"), "bad")
  })
}
