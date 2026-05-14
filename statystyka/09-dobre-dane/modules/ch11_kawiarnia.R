# Tab 11: Kawiarnia — sprzedaż dzienna, braki danych + szereg czasowy

ch11_ui <- lecture_chapter(id = "ch11", num = "11", title = "Kawiarnia", content = tagList(
  fluidRow(column(8, offset = 2,

    lc_chapter_hero(
      kicker = "Rozdział 11 · Co czyni dobry zbiór danych?",
      num    = "11",
      title  = "Kawiarnia studencka.",
      lead   = "Dane dzienne mogą wyglądać jak zwykła tabela, ale ukrywać
                strukturę szeregu czasowego i naruszenie niezależności obserwacji."
    ),

    lc_h2("sec-01", "Sprzedaż w kawiarni studenckiej"),

    div(class = "lc-prose",
      p("Dane ze sprzedaży kawiarni działającej w kampusie uczelni. 245 dni obserwacji
        (rok akademicki: październik–czerwiec). Zmienne: data, dzień tygodnia,
        liczba sprzedanych kaw i temperatura zewnętrzna."),
      p("Chcemy zbadać, czy temperatura wpływa na sprzedaż.")
    ),

    lc_h2("sec-02", "Podgląd danych"),

    div(class = "lc-figure-panel",
      DT::dataTableOutput("tab10_table")
    ),

    lc_h2("sec-03", "Czy są braki danych?"),

    div(class = "lc-figure-panel",
      zoom_plot_ui("tab10_missing", height = "300px"),
      uiOutput("tab10_missing_info")
    ),

    lc_h2("sec-04", "Odkryj ukryty problem"),

    div(class = "lc-figure-panel",
      actionButton("tab10_reveal", "Pokaż dane w kolejności", class = "lc-btn-warning lc-btn-lg", width = "100%"),
      conditionalPanel("input.tab10_reveal > 0",
        zoom_plot_ui("tab10_lineplot", height = "350px"),
        div(class = "lc-feedback lc-feedback-danger",
          "To nie są niezależne obserwacje!",
          " Widać wyraźną periodyczność tygodniową — każdy poniedziałek wysoki,
          każdy weekend niski. Każdy dzień jest podobny do poprzedniego.",
          tags$br(),
          "Testy statystyczne zakładają niezależność obserwacji — to założenie jest tutaj złamane."
        )
      )
    ),

    conditionalPanel("input.tab10_reveal > 0",
      div(class = "lc-figure-panel",
        h4("Autokorelacja — dowód braku niezależności"),
        zoom_plot_ui("tab10_lag", height = "300px"),
        uiOutput("tab10_autocorr_info")
      )
    ),

    lc_h2("sec-05", "Werdykt"),

    div(class = "lc-feedback lc-feedback-danger",
      tags$strong("DWA poważne problemy:"),
      tags$br(),
      tags$strong("1. Braki danych:"), " kawy ma ", uiOutput("tab10_missing_pct", inline = TRUE), " braków
      (kawiarnia zamknięta w święta, awarie systemu).",
      tags$br(),
      tags$strong("2. Brak niezależności:"), " To szereg czasowy! ",
      "Obserwacje dzienne są silnie autokorelowane — głównie przez cotygodniową periodyczność.",
      tags$br(),
      "Korelacja Pearsona między temperaturą a sprzedażą byłaby błędem metodologicznym."
    ),

    div(class = "lc-feedback lc-feedback-info",
      tags$strong("Czy można to uratować?"),
      tags$br(),
      "Agregacja tygodniowa rozwiązałaby problem zależności — średnia sprzedaż z całego tygodnia
      to jedna obserwacja, a kolejne tygodnie są od siebie znacznie mniej zależne.",
      tags$br(),
      "Po agregacji: n = ~35 tygodni — skromnie, ale możliwe do analizy.",
      tags$br(),
      tags$em("Gdyby dane obejmowały kilka lat akademickich, mielibyśmy 35 tyg. × 3 lata = ok. 105
      obserwacji — wtedy analiza sezonowości i trendów byłaby pełnoprawna.")
    ),

    lc_chapter_next(
      num = "12",
      title = "Ściąga",
      lead = "To był ostatni zbiór danych. Zobaczmy podsumowanie.",
      target_id = "ch12"
    ),

    div(style = "height: 40px;")
  ))))

ch11_server <- function(input, output, session) {

  output$tab10_table <- DT::renderDataTable({
    datatable(round_df(cafe_data), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  zoom_plot_server("tab10_missing", reactive({
    miss_pct <- sapply(cafe_data[, c("kawy", "temperatura")], function(x) mean(is.na(x)) * 100)
    df_miss  <- data.frame(variable = names(miss_pct), pct = miss_pct)
    df_miss  <- df_miss[df_miss$pct > 0, ]
    df_miss$color <- ifelse(df_miss$pct > 20, data_bad,
                     ifelse(df_miss$pct > 5,  data_mixed, data_good))

    ggplot(df_miss, aes(x = reorder(variable, -pct), y = pct, fill = color)) +
      geom_col() +
      scale_fill_identity() +
      geom_text(aes(label = paste0(round(pct, 1), "%")),
                vjust = -0.4, size = 5, fontface = "bold") +
      geom_hline(yintercept = 5,  linetype = "dashed", color = data_mixed, linewidth = 0.8) +
      geom_hline(yintercept = 20, linetype = "dashed", color = data_bad,   linewidth = 0.8) +
      annotate("text", x = Inf, y = 6.5,  label = "5% — akceptowalne",
               hjust = 1.05, color = data_mixed, size = 3.8) +
      annotate("text", x = Inf, y = 21.5, label = "20% — poważny problem",
               hjust = 1.05, color = data_bad,   size = 3.8) +
      scale_y_continuous(limits = c(0, 30)) +
      labs(
           x = NULL, y = "% braków") +
      theme_upwr(base_size = 14)
  }))

  output$tab10_missing_info <- renderUI({
    kawy_na <- sum(is.na(cafe_data$kawy))
    temp_na <- sum(is.na(cafe_data$temperatura))
    n_comp  <- sum(complete.cases(cafe_data[, c("kawy", "temperatura")]))
    div(class = "lc-feedback lc-feedback-warning",
      paste0("kawy: ", kawy_na, " braków (", round(kawy_na / nrow(cafe_data) * 100, 1), "%) — ",
             "zamknięcia i awarie. ",
             "temperatura: ", temp_na, " braków (", round(temp_na / nrow(cafe_data) * 100, 1), "%) — ",
             "akceptowalne. ",
             "Kompletne obserwacje: ", n_comp, " z ", nrow(cafe_data), ".")
    )
  })

  output$tab10_missing_pct <- renderUI({
    kawy_na  <- sum(is.na(cafe_data$kawy))
    pct      <- round(kawy_na / nrow(cafe_data) * 100, 1)
    tags$strong(paste0(pct, "%"))
  })

  zoom_plot_server("tab10_lineplot", reactive({
    df <- cafe_data[!is.na(cafe_data$kawy), ]
    ggplot(df, aes(x = dzien, y = kawy)) +
      geom_line(color = data_primary, alpha = 0.6) +
      geom_point(color = data_primary, size = 1.2, alpha = 0.4) +
      labs(
           
           x = "Numer dnia (= kolejność w roku akademickim)", y = "Liczba sprzedanych kaw") +
      theme_upwr(base_size = 14)
  }))

  zoom_plot_server("tab10_lag", reactive({
    kw        <- cafe_data$kawy[!is.na(cafe_data$kawy)]
    n         <- length(kw)
    lag_df    <- data.frame(x = kw[-n], y = kw[-1])
    r         <- round(cor(lag_df$x, lag_df$y), 3)

    ggplot(lag_df, aes(x = x, y = y)) +
      geom_point(alpha = 0.4, color = data_reference) +
      geom_smooth(method = "lm", color = data_bad, se = TRUE) +
      labs(
           
           x = "Kawy(t)", y = "Kawy(t+1)") +
      theme_upwr(base_size = 14)
  }))

  output$tab10_autocorr_info <- renderUI({
    kw <- cafe_data$kawy[!is.na(cafe_data$kawy)]
    n  <- length(kw)
    r  <- cor(kw[-n], kw[-1])
    div(class = "lc-feedback lc-feedback-danger",
      paste0("Autokorelacja lag-1: r = ", round(r, 3), ". ",
             "Gdyby obserwacje były niezależne, oczekiwaliśmy r bliskiego 0. ",
             "Wartość ", round(r, 2), " oznacza silną zależność między kolejnymi dniami — ",
             "głównie przez powtarzający się rytm tygodnia.")
    )
  })

}
