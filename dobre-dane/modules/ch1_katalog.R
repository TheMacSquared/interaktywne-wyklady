# Tab 1: Katalog — 7 typowych problemów w danych z wizualizacjami

ch1_ui <- tabPanel("1. Katalog",
  fluidRow(column(10, offset = 1,

    div(class = "section-title", "Katalog problemów w danych"),

    div(class = "narrative",
      p("Poniżej zobaczysz 7 typowych problemów, które mogą dyskwalifikować zbiór danych.
        Każdy problem pokazujemy tak, jak wyglądałoby to w jamovi lub Excelu (tabela)
        oraz na wykresie. Gdzie to możliwe - pokaz surowe vs oczyszczone dane.")
    ),

    # --- Problem 1: Za mało danych ---
    div(class = "problem-card",
      div(class = "problem-header",
        span(class = "problem-number", "1"),
        h3(class = "problem-name", "Za mało danych")
      ),
      div(class = "problem-desc",
        "Kolega przepytał 6 znajomych i chce robić test t. Czy to wystarczy?"
      ),
      div(class = "dual-view",
        div(class = "view-panel",
          div(class = "view-label", "Widok danych"),
          div(class = "jamovi-table", DT::dataTableOutput("cat1_table"))
        ),
        div(class = "view-panel",
          div(class = "view-label", "Co widać na wykresie"),
          plotOutput("cat1_plot", height = "280px")
        )
      ),
      div(class = "callout-danger", style = "margin-top: 10px;",
        tags$strong("Problem:"), " Przy n = 6 histogram ma ogromne dziury, ",
        "przedział ufności jest bardzo szeroki, a moc testu < 10%. ",
        "Nawet duży efekt będzie nieistotny statystycznie.",
        tags$br(),
        tags$strong("Zasada:"), " Minimum 20-30 obserwacji na grupę."
      )
    ),

    # --- Problem 2: Brak zmienności ---
    div(class = "problem-card",
      div(class = "problem-header",
        span(class = "problem-number", "2"),
        h3(class = "problem-name", "Brak zmienności")
      ),
      div(class = "problem-desc",
        "Firma przeprowadziła ankietę zadowolenia. Ale wszyscy wiedzą, że szef ją czyta..."
      ),
      div(class = "dual-view",
        div(class = "view-panel",
          div(class = "view-label", "Widok danych"),
          div(class = "jamovi-table", DT::dataTableOutput("cat2_table"))
        ),
        div(class = "view-panel",
          div(class = "view-label", "Co widać na wykresach"),
          plotOutput("cat2_plot_zadowolenie", height = "200px"),
          plotOutput("cat2_plot", height = "200px")
        )
      ),
      div(class = "callout-danger", style = "margin-top: 10px;",
        tags$strong("Problem:"), " staż pracy jest prawie stały (zakres 2.8\u20133.2 lata). ",
        "Wynagrodzenia się różnią, ale nie widać żadnego wzorca \u2014 punkty tworzą pionową chmurę.",
        tags$br(),
        "Gdy jedna zmienna nie ma żadnego rozrzutu, nie da się ocenić czy i jak wpływa na drugą."
      )
    ),

    # --- Problem 3: Błędy i literówki ---
    div(class = "problem-card",
      div(class = "problem-header",
        span(class = "problem-number", "3"),
        h3(class = "problem-name", "Błędy i literówki w danych")
      ),
      div(class = "problem-desc",
        "Dane z portalu nieruchomości skopiowane do Excela. Wszystko wygląda OK... na pierwszy rzut oka."
      ),
      div(class = "toggle-pills",
        actionButton("cat3_raw", "Surowe", class = "pill-btn active"),
        actionButton("cat3_clean", "Oczyszczone", class = "pill-btn")
      ),
      div(class = "dual-view",
        div(class = "view-panel",
          div(class = "view-label", "Widok danych"),
          div(class = "jamovi-table", DT::dataTableOutput("cat3_table"))
        ),
        div(class = "view-panel",
          div(class = "view-label", "Cena vs powierzchnia"),
          plotOutput("cat3_plot", height = "280px")
        )
      ),
      div(class = "callout-warning", style = "margin-top: 10px;",
        tags$strong("Typowe błędy:"),
        " brak zer (45 zamiast 450 000), dodatkowe zero (5 500 000 zamiast 550 000), ",
        "ujemna cena (-300 000), literówka w pokojach (42 zamiast 4).",
        tags$br(),
        tags$strong("Zasada:"), " Zawsze sprawdź zakresy zmiennych (min, max) zanim zaczniesz analizę."
      )
    ),

    # --- Problem 4: Źle zdefiniowane zmienne ---
    div(class = "problem-card",
      div(class = "problem-header",
        span(class = "problem-number", "4"),
        h3(class = "problem-name", "Źle zdefiniowane zmienne")
      ),
      div(class = "problem-desc",
        "Student zrobił ankietę z pytaniami otwartymi. Każdy odpowiedział po swojemu."
      ),
      div(class = "toggle-pills",
        actionButton("cat4_raw", "Surowe", class = "pill-btn active"),
        actionButton("cat4_clean", "Oczyszczone", class = "pill-btn")
      ),
      div(class = "dual-view",
        div(class = "view-panel",
          div(class = "view-label", "Widok danych"),
          div(class = "jamovi-table", DT::dataTableOutput("cat4_table"))
        ),
        div(class = "view-panel",
          div(class = "view-label", "Próba zrobienia histogramu"),
          plotOutput("cat4_plot", height = "280px")
        )
      ),
      div(class = "callout-warning", style = "margin-top: 10px;",
        tags$strong("Problem:"), " R/jamovi nie wie, co zrobić z '3-4h' albo 'dobrze'. ",
        "Czyszczenie jest możliwe, ale tracimy dużo danych (NA).",
        tags$br(),
        tags$strong("Zasada:"), " Zamknięte pytania + spójne skale + pilotaż ankiety."
      )
    ),

    # --- Problem 5: Braki danych ---
    div(class = "problem-card",
      div(class = "problem-header",
        span(class = "problem-number", "5"),
        h3(class = "problem-name", "Braki danych (NA)")
      ),
      div(class = "problem-desc",
        "Ankieta ze 12 odpowiedziami. Nie każdy odpowiedział na wszystkie pytania."
      ),
      div(class = "dual-view",
        div(class = "view-panel",
          div(class = "view-label", "Widok danych"),
          div(class = "jamovi-table", DT::dataTableOutput("cat5_table"))
        ),
        div(class = "view-panel",
          div(class = "view-label", "Procent brakow na zmienna"),
          plotOutput("cat5_plot", height = "280px")
        )
      ),
      div(class = "callout-info", style = "margin-top: 10px;",
        tags$strong("Progi:"),
        " < 5% braków = OK (usuń wiersze). 5-20% = ostrożnie (rozważ imputację). ",
        "> 20% = zmienna może odpaść z analizy.",
        tags$br(),
        tags$strong("Uwaga:"), " Braki rzadko są losowe! Może ludzie pomijali trudne pytania?"
      )
    ),

    # --- Problem 6: Brak niezależności ---
    div(class = "problem-card",
      div(class = "problem-header",
        span(class = "problem-number", "6"),
        h3(class = "problem-name", "Brak niezależności obserwacji")
      ),
      div(class = "problem-desc",
        "Dane o temperaturze i ozonie - 20 dni pomiarów. W tabeli wygląda normalnie..."
      ),
      div(class = "dual-view",
        div(class = "view-panel",
          div(class = "view-label", "Widok danych"),
          div(class = "jamovi-table", DT::dataTableOutput("cat6_table"))
        ),
        div(class = "view-panel",
          div(class = "view-label", "Dane w kolejności (liniowy)"),
          plotOutput("cat6_plot", height = "280px")
        )
      ),
      div(class = "callout-danger", style = "margin-top: 10px;",
        tags$strong("Problem:"), " W tabeli te dane wyglądają jak 20 niezależnych pomiarów. ",
        "Ale wykres liniowy zdradza sezonowość - każdy dzień zależy od poprzedniego.",
        tags$br(),
        tags$strong("Konsekwencja:"), " Test t i korelacja Pearsona zakładają niezależność. ",
        "Złam to założenie = fałszywie istotne wyniki."
      )
    ),

    # --- Problem 7: Zła struktura ---
    div(class = "problem-card",
      div(class = "problem-header",
        span(class = "problem-number", "7"),
        h3(class = "problem-name", "Zła struktura danych")
      ),
      div(class = "problem-desc",
        "Dziennik szkolny: każdy wiersz to jedna ocena ucznia, nie jeden uczeń."
      ),
      div(class = "toggle-pills",
        actionButton("cat7_events", "Oceny (surowe)", class = "pill-btn active"),
        actionButton("cat7_agg", "Zagregowane", class = "pill-btn")
      ),
      div(class = "dual-view",
        div(class = "view-panel",
          div(class = "view-label", "Widok danych"),
          div(class = "jamovi-table", DT::dataTableOutput("cat7_table"))
        ),
        div(class = "view-panel",
          div(class = "view-label", "Ile masz obserwacji?"),
          plotOutput("cat7_plot", height = "280px")
        )
      ),
      div(class = "callout-danger", style = "margin-top: 10px;",
        tags$strong("Problem:"), " 8 wierszy wygląda jak n = 8, ale to oceny, nie uczniowie. ",
        "Po agregacji do poziomu uczniów masz n = 3. Test t na n = 3?",
        tags$br(),
        tags$strong("Zasada:"), " Zawsze pytaj: co jest jednostką obserwacji? ",
        "Osoba? Firma? Dzień? Wiersz w tabeli \u2260 obserwacja."
      )
    ),

    # --- Podsumowanie: Checklist ---
    div(class = "section-title", "Podsumowanie: Checklist jakości danych"),

    div(class = "narrative",
      p("Teraz już znasz typowe problemy. Użyj poniższego checklistu,
        żeby systematycznie oceniać każdy zbiór danych.")
    ),

    div(class = "widget-block",
      h4("Checklist jakości danych"),
      tags$p(tags$strong(style = "color: #e74c3c;", "KRYTYCZNE"),
        " - jeśli nie spełniasz, szukaj innego zbioru:"),
      checkboxGroupInput("intro_critical", NULL,
        choices = c(
          "Dane odpowiadają hipotezie badawczej (mierzą to, co chcesz badać)" = "hyp",
          "Wystarczająca liczba obserwacji (n \u2265 20-30 na grupę/podgrupę)" = "n",
          "Mix typów zmiennych (ilościowe + jakościowe)" = "mix",
          "Zmienność w danych (nie wszystko takie samo)" = "var",
          "Struktura danych pasuje do planowanych analiz" = "fit",
          "Niezależność obserwacji (lub możliwość agregacji)" = "indep"
        )
      ),
      tags$p(tags$strong(style = "color: #f39c12;", "NAPRAWIALNE"),
        " - wymagają pracy, ale się da:"),
      checkboxGroupInput("intro_fixable", NULL,
        choices = c(
          "Mało braków danych (< 5%)" = "missing",
          "Jednoznaczne definicje zmiennych" = "def",
          "Brak błędów i podejrzanych wartości" = "errors"
        )
      ),
      uiOutput("intro_thermometer")
    ),

    div(class = "chapter-transition",
      p("Pora przetestować tę wiedzę na prawdziwych zbiorach danych."),
      actionButton("cat_next", "Dalej: 2. Szkoły w Kalifornii \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  )))

ch1_server <- function(input, output, session) {

  # --- Problem 1: Za malo danych ---
  output$cat1_table <- DT::renderDataTable({
    sketch <- htmltools::withTags(table(
      class = "display",
      thead(tr(
        th("id", br(span(class = "var-type", "id"))),
        th("plec", br(span(class = "var-type", "nominalna"))),
        th("wiek", br(span(class = "var-type", "ciagla"))),
        th("stres", br(span(class = "var-type", "porzadkowa"))),
        th("oceny", br(span(class = "var-type", "ciagla")))
      ))
    ))
    datatable(cat_small, container = sketch, rownames = FALSE,
              options = list(dom = 't', ordering = FALSE, pageLength = 10))
  })

  output$cat1_plot <- renderPlot({
    ggplot(cat_small, aes(x = oceny)) +
      geom_histogram(bins = 4, fill = col_bad, color = "white", alpha = 0.8) +
      geom_vline(xintercept = mean(cat_small$oceny), linetype = "dashed", color = col_dark, linewidth = 1) +
      annotate("text", x = mean(cat_small$oceny) + 0.15, y = 2.2,
               label = paste0("M = ", round(mean(cat_small$oceny), 2)), hjust = 0, size = 4.5) +
      scale_y_continuous(breaks = 0:3) +
      labs(title = "Histogram ocen (n = 6)", x = "Średnia ocen", y = "Liczebność") +
      theme_minimal(base_size = 14)
  })

  # --- Problem 2: Brak zmiennosci ---
  output$cat2_table <- DT::renderDataTable({
    sketch <- htmltools::withTags(table(
      class = "display",
      thead(tr(
        th("id", br(span(class = "var-type", "id"))),
        th("zadowolenie", br(span(class = "var-type", "porzadkowa"))),
        th("wynagrodzenie", br(span(class = "var-type", "ciagla"))),
        th("staz", br(span(class = "var-type", "ciagla"))),
        th("dzial", br(span(class = "var-type", "nominalna")))
      ))
    ))
    datatable(cat_novar, container = sketch, rownames = FALSE,
              options = list(dom = 't', ordering = FALSE, pageLength = 12))
  })

  output$cat2_plot_zadowolenie <- renderPlot({
    pct_45 <- round(100 * mean(cat_novar$zadowolenie >= 4))
    ggplot(cat_novar, aes(x = factor(zadowolenie))) +
      geom_bar(fill = col_bad, alpha = 0.85) +
      scale_x_discrete(limits = c("1","2","3","4","5")) +
      labs(title = paste0("Zadowolenie: ", pct_45, "% odpowiedzi to 4 lub 5"),
           x = "Ocena (1\u20135)", y = "Liczba") +
      theme_minimal(base_size = 13)
  })

  output$cat2_plot <- renderPlot({
    ggplot(cat_novar, aes(x = staz, y = wynagrodzenie)) +
      geom_point(size = 3, alpha = 0.6, color = col_bad) +
      scale_x_continuous(limits = c(1, 10)) +
      labs(title = paste0("Sta\u017c vs wynagrodzenie (r = ",
                          round(cor(cat_novar$staz, cat_novar$wynagrodzenie), 3), ")"),
           subtitle = "Sta\u017c skupiony w w\u0105skim fragmencie osi",
           x = "Sta\u017c pracy (lata)", y = "Wynagrodzenie (PLN)") +
      theme_minimal(base_size = 13)
  })

  # --- Problem 3: Bledy i literowki (toggle) ---
  cat3_view <- reactiveVal("raw")
  observeEvent(input$cat3_raw, {
    cat3_view("raw")
    shinyjs_js <- paste0(
      "$('#cat3_raw').addClass('active'); $('#cat3_clean').removeClass('active');"
    )
    session$sendCustomMessage(type = "shinyjs-runjs", message = list(code = shinyjs_js))
  })
  observeEvent(input$cat3_clean, {
    cat3_view("clean")
    shinyjs_js <- paste0(
      "$('#cat3_clean').addClass('active'); $('#cat3_raw').removeClass('active');"
    )
    session$sendCustomMessage(type = "shinyjs-runjs", message = list(code = shinyjs_js))
  })

  output$cat3_table <- DT::renderDataTable({
    if (cat3_view() == "raw") {
      d <- cat_errors
    } else {
      d <- cat_errors_clean
    }
    sketch <- htmltools::withTags(table(
      class = "display",
      thead(tr(
        th("id", br(span(class = "var-type", "id"))),
        th("cena", br(span(class = "var-type", "ciagla"))),
        th("powierzchnia", br(span(class = "var-type", "ciagla"))),
        th("pokoje", br(span(class = "var-type", "dyskretna"))),
        th("dzielnica", br(span(class = "var-type", "nominalna")))
      ))
    ))
    dt <- datatable(d, container = sketch, rownames = FALSE,
                    options = list(dom = 't', ordering = FALSE, pageLength = 12))
    if (cat3_view() == "raw") {
      dt <- dt %>%
        formatStyle("cena", backgroundColor = styleInterval(
          c(0, 1000000), c("#fdedec", "white", "#fdedec"))) %>%
        formatStyle("powierzchnia", backgroundColor = styleInterval(
          c(500), c("white", "#fdedec"))) %>%
        formatStyle("pokoje", backgroundColor = styleInterval(
          c(10), c("white", "#fdedec")))
    }
    dt
  })

  output$cat3_plot <- renderPlot({
    if (cat3_view() == "raw") {
      d <- cat_errors
      title_txt <- "Z błędami"
      col <- col_bad
    } else {
      d <- cat_errors_clean
      title_txt <- "Po oczyszczeniu"
      col <- col_good
    }
    model <- lm(cena ~ powierzchnia, data = d)
    r2 <- round(summary(model)$r.squared, 3)
    ggplot(d, aes(x = powierzchnia, y = cena)) +
      geom_point(size = 3, alpha = 0.7, color = col_dark) +
      geom_smooth(method = "lm", color = col, se = TRUE) +
      labs(title = paste0(title_txt, " (R\u00b2 = ", r2, ")"),
           x = "Powierzchnia (m\u00b2)", y = "Cena (PLN)") +
      theme_minimal(base_size = 14)
  })

  # --- Problem 4: Zle zdefiniowane zmienne (toggle) ---
  cat4_view <- reactiveVal("raw")
  observeEvent(input$cat4_raw, {
    cat4_view("raw")
    session$sendCustomMessage(type = "shinyjs-runjs",
      message = list(code = "$('#cat4_raw').addClass('active'); $('#cat4_clean').removeClass('active');"))
  })
  observeEvent(input$cat4_clean, {
    cat4_view("clean")
    session$sendCustomMessage(type = "shinyjs-runjs",
      message = list(code = "$('#cat4_clean').addClass('active'); $('#cat4_raw').removeClass('active');"))
  })

  output$cat4_table <- DT::renderDataTable({
    if (cat4_view() == "raw") {
      d <- cat_messy
      sketch <- htmltools::withTags(table(
        class = "display",
        thead(tr(
          th("id", br(span(class = "var-type", "id"))),
          th("czas_nauki", br(span(class = "var-type", "tekst?!"))),
          th("ocena_kursu", br(span(class = "var-type", "tekst?!"))),
          th("aktywnosc", br(span(class = "var-type", "tekst?!")))
        ))
      ))
      datatable(d, container = sketch, rownames = FALSE,
                options = list(dom = 't', ordering = FALSE, pageLength = 10)) %>%
        formatStyle(c("czas_nauki", "ocena_kursu", "aktywnosc"),
                    backgroundColor = "#fef9e7")
    } else {
      d <- cat_messy_clean
      sketch <- htmltools::withTags(table(
        class = "display",
        thead(tr(
          th("id", br(span(class = "var-type", "id"))),
          th("czas_nauki_h", br(span(class = "var-type", "ciagla"))),
          th("ocena_kursu_1_10", br(span(class = "var-type", "ciagla"))),
          th("aktywnosc_razy_tyg", br(span(class = "var-type", "ciagla")))
        ))
      ))
      datatable(d, container = sketch, rownames = FALSE,
                options = list(dom = 't', ordering = FALSE, pageLength = 10))
    }
  })

  output$cat4_plot <- renderPlot({
    if (cat4_view() == "raw") {
      nums <- suppressWarnings(as.numeric(cat_messy$czas_nauki))
      n_ok <- sum(!is.na(nums))
      n_fail <- sum(is.na(nums))
      df <- data.frame(
        status = c("Rozpoznane\njako liczba", "Nie da się\nprzeczytać"),
        n = c(n_ok, n_fail)
      )
      ggplot(df, aes(x = status, y = n, fill = status)) +
        geom_col(width = 0.6) +
        scale_fill_manual(values = c(col_good, col_bad)) +
        geom_text(aes(label = n), vjust = -0.5, size = 6, fontface = "bold") +
        labs(title = "czas_nauki: próba konwersji na liczby",
             subtitle = paste0(n_fail, " z ", nrow(cat_messy), " odpowiedzi nie da się użyć"),
             x = NULL, y = "Liczba odpowiedzi") +
        theme_minimal(base_size = 14) +
        theme(legend.position = "none") +
        ylim(0, max(df$n) + 1)
    } else {
      d <- cat_messy_clean[!is.na(cat_messy_clean$czas_nauki_h), ]
      ggplot(d, aes(x = czas_nauki_h)) +
        geom_histogram(bins = 5, fill = col_good, color = "white", alpha = 0.8) +
        labs(title = paste0("Histogram (n = ", nrow(d), " z ", nrow(cat_messy_clean), ")"),
             subtitle = "Po oczyszczeniu - ale straciliśmy połowę danych",
             x = "Godziny nauki/tydzień", y = "Liczebność") +
        theme_minimal(base_size = 14)
    }
  })

  # --- Problem 5: Braki danych ---
  output$cat5_table <- DT::renderDataTable({
    sketch <- htmltools::withTags(table(
      class = "display",
      thead(tr(
        th("id", br(span(class = "var-type", "id"))),
        th("wiek", br(span(class = "var-type", "ciagla"))),
        th("stres", br(span(class = "var-type", "porzadkowa"))),
        th("oceny", br(span(class = "var-type", "ciagla"))),
        th("kierunek", br(span(class = "var-type", "nominalna")))
      ))
    ))
    # Replace NA with styled text for visibility
    d <- cat_missing
    datatable(d, container = sketch, rownames = FALSE,
              options = list(dom = 't', ordering = FALSE, pageLength = 12)) %>%
      formatStyle(names(d)[-1],
        backgroundColor = styleEqual(NA, "#f5f5f5"),
        color = styleEqual(NA, "#bbb"))
  })

  output$cat5_plot <- renderPlot({
    miss_pct <- sapply(cat_missing[, -1], function(x) mean(is.na(x)) * 100)
    df_miss <- data.frame(variable = names(miss_pct), pct = miss_pct)
    df_miss$color <- ifelse(df_miss$pct > 20, col_bad, ifelse(df_miss$pct > 5, col_mixed, col_good))

    ggplot(df_miss, aes(x = reorder(variable, -pct), y = pct, fill = color)) +
      geom_col(width = 0.6) +
      scale_fill_identity() +
      geom_text(aes(label = paste0(round(pct), "%")), vjust = -0.5, size = 5, fontface = "bold") +
      geom_hline(yintercept = 5, linetype = "dashed", color = col_mixed) +
      geom_hline(yintercept = 20, linetype = "dashed", color = col_bad) +
      annotate("text", x = 3.5, y = 7, label = "5% = OK", color = col_mixed, size = 3.5) +
      annotate("text", x = 3.5, y = 22, label = "20% = problem", color = col_bad, size = 3.5) +
      labs(title = "Procent braków na zmienną", x = NULL, y = "% braków (NA)") +
      theme_minimal(base_size = 14) +
      ylim(0, 35)
  })

  # --- Problem 6: Brak niezaleznosci ---
  output$cat6_table <- DT::renderDataTable({
    sketch <- htmltools::withTags(table(
      class = "display",
      thead(tr(
        th("dzien", br(span(class = "var-type", "id"))),
        th("data", br(span(class = "var-type", "data"))),
        th("temperatura", br(span(class = "var-type", "ciagla"))),
        th("ozon_ppb", br(span(class = "var-type", "ciagla")))
      ))
    ))
    datatable(cat_timeseries, container = sketch, rownames = FALSE,
              options = list(dom = 't', ordering = FALSE, pageLength = 20, scrollY = "260px"))
  })

  output$cat6_plot <- renderPlot({
    ggplot(cat_timeseries, aes(x = dzien, y = temperatura)) +
      geom_line(color = col_bad, linewidth = 1.2) +
      geom_point(color = col_bad, size = 2.5) +
      labs(title = "Temperatura w kolejności pomiarów",
           subtitle = "Wyraźna fala - każdy dzień zależy od poprzedniego!",
           x = "Dzień pomiaru", y = "Temperatura (\u00b0C)") +
      theme_minimal(base_size = 14)
  })

  # --- Problem 7: Zla struktura (toggle) ---
  cat7_view <- reactiveVal("events")
  observeEvent(input$cat7_events, {
    cat7_view("events")
    session$sendCustomMessage(type = "shinyjs-runjs",
      message = list(code = "$('#cat7_events').addClass('active'); $('#cat7_agg').removeClass('active');"))
  })
  observeEvent(input$cat7_agg, {
    cat7_view("agg")
    session$sendCustomMessage(type = "shinyjs-runjs",
      message = list(code = "$('#cat7_agg').addClass('active'); $('#cat7_events').removeClass('active');"))
  })

  output$cat7_table <- DT::renderDataTable({
    if (cat7_view() == "events") {
      sketch <- htmltools::withTags(table(
        class = "display",
        thead(tr(
          th("uczen", br(span(class = "var-type", "nominalna"))),
          th("przedmiot", br(span(class = "var-type", "nominalna"))),
          th("ocena", br(span(class = "var-type", "dyskretna")))
        ))
      ))
      datatable(cat_events, container = sketch, rownames = FALSE,
                options = list(dom = 't', ordering = FALSE, pageLength = 10))
    } else {
      sketch <- htmltools::withTags(table(
        class = "display",
        thead(tr(
          th("uczen", br(span(class = "var-type", "nominalna"))),
          th("srednia", br(span(class = "var-type", "ciagla"))),
          th("n_ocen", br(span(class = "var-type", "dyskretna")))
        ))
      ))
      datatable(cat_events_agg, container = sketch, rownames = FALSE,
                options = list(dom = 't', ordering = FALSE, pageLength = 10))
    }
  })

  output$cat7_plot <- renderPlot({
    if (cat7_view() == "events") {
      df <- data.frame(label = "Wiersze\nw tabeli", n = nrow(cat_events))
      ggplot(df, aes(x = label, y = n)) +
        geom_col(fill = col_mixed, width = 0.4) +
        geom_text(aes(label = paste0("n = ", n)), vjust = -0.5, size = 7, fontface = "bold") +
        labs(title = "Ile masz 'obserwacji'?",
             subtitle = "8 wierszy, ale to oceny, nie uczniowie",
             x = NULL, y = NULL) +
        ylim(0, 10) +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    } else {
      df <- data.frame(label = "Uczniowie\n(obserwacje)", n = nrow(cat_events_agg))
      ggplot(df, aes(x = label, y = n)) +
        geom_col(fill = col_bad, width = 0.4) +
        geom_text(aes(label = paste0("n = ", n)), vjust = -0.5, size = 7, fontface = "bold",
                  color = col_bad) +
        labs(title = "Po agregacji",
             subtitle = "n = 3 uczni\u00f3w. Test t? Zdecydowanie za ma\u0142o.",
             x = NULL, y = NULL) +
        ylim(0, 5) +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    }
  })
}
