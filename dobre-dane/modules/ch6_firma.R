# Tab 6: Firma — ankieta firmowa, brak zmienności

ch6_ui <- tabPanel("6. Firma",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Ankieta firmowa"),

    div(class = "narrative",
      p("Firma przeprowadza anonimową ankietę zadowolenia pracowników.
        Problem w tym, że wszyscy wiedzą, że szef ją czyta...
        Zebrano dane od 80 pracowników.")
    ),

    div(class = "section-title", "Podgląd danych"),

    div(class = "widget-block",
      DT::dataTableOutput("tab5_table")
    ),

    div(class = "section-title", "Zmienna 1: Zadowolenie z pracy"),

    div(class = "widget-block",
      plotOutput("tab5_plot_zadowolenie", height = "300px")
    ),
    div(class = "callout-danger",
      tags$strong("Problem:"), " brak zróżnicowania odpowiedzi.",
      " 95% pracowników zaznaczyło 4 lub 5. Skala 1\u20135 w praktyce działa tu jak skala 1\u20132 \u2014
      kiedy wszyscy odpowiadają tak samo, zmienna nic nie mówi."
    ),

    div(class = "section-title", "Zmienna 2: Dział"),

    div(class = "widget-block",
      plotOutput("tab5_plot_departament", height = "300px")
    ),
    div(class = "callout-danger",
      tags$strong("Problem:"), " niezbalansowane grupy.",
      " 94% respondentów to dział IT. Pozostałe działy mają po 1\u20132 osoby \u2014
      jakiekolwiek porównanie między działami będzie niemożliwe."
    ),

    div(class = "section-title", "Zmienna 3: Staż pracy"),

    div(class = "toggle-pills",
      actionButton("tab5_staz_normal", "Dane", class = "pill-btn active"),
      actionButton("tab5_staz_wide", "Pełna skala (1\u201310 lat)", class = "pill-btn")
    ),
    div(class = "widget-block",
      plotOutput("tab5_plot_staz", height = "300px")
    ),
    div(class = "callout-warning",
      tags$strong("Uwaga:"), " wąska rozpiętość wartości.",
      " Wszyscy pracownicy mają staż w przedziale 2.8\u20133.5 roku. Sama w sobie mała zmienność
      nie jest błędem \u2014 zdarzają się takie dane. Ale gdy ",
      tags$em("cały zbiór"), " wygląda podobnie, wykrycie jakichkolwiek zależności staje się
      bardzo trudne."
    ),

    div(class = "section-title", "Zmienna 4: Wynagrodzenie"),

    div(class = "widget-block",
      plotOutput("tab5_plot_wynagrodzenie", height = "300px")
    ),
    div(class = "callout-success",
      "Wynagrodzenia mają normalny rozrzut.",
      " To dobra wiadomość \u2014 ta zmienna wydaje się użyteczna.
      Zobaczmy więc, czy możemy ją powiązać z czymś innym w tym zbiorze."
    ),

    div(class = "section-title", "Zmienna 5: Płeć"),

    div(class = "widget-block",
      plotOutput("tab5_plot_plec", height = "300px")
    ),
    div(class = "callout-danger",
      tags$strong("Problem:"), " niezbalansowane grupy.",
      " 90% respondentów to mężczyźni (ok. 72 os.), kobiet jest ok. 8. Porównanie
      według płci nie ma sensu przy takiej dysproporcji."
    ),

    div(class = "section-title", "Co się dzieje gdy próbujemy szukać zależności?"),

    div(class = "callout-info",
      "Wynagrodzenie ma dobry rozrzut. Czy możemy powiązać je ze stażem pracy? ",
      "Sprawdźmy \u2014 pamiętaj, że staż mieści się w bardzo wąskim przedziale."
    ),

    div(class = "widget-block",
      plotOutput("tab5_scatter", height = "300px")
    ),

    div(class = "section-title", "Co by było, gdyby dane miały normalną zmienność?"),

    div(class = "callout-info",
      "Co by było, gdyby pracownicy różnili się stażem bardziej \u2014 np. od 1 do 15 lat?
      Przesuń suwak i obserwuj jak pojawia się związek między stażem a wynagrodzeniem."
    ),

    div(class = "widget-block",
      sliderInput("tab5_sd_mult", "Mnożnik rozrzutu danych:", min = 1, max = 5, value = 1, step = 0.5),
      plotOutput("tab5_scatter_sim", height = "300px")
    ),

    div(class = "section-title", "Werdykt"),

    div(class = "callout-danger",
      "Ten zbiór danych nie nadaje się do analizy.",
      tags$br(),
      "Wynagrodzenia mają dobry rozrzut, ale trudno to wykorzystać: odpowiedzi o zadowoleniu
      są skupione przy maksimum, działy i płeć skrajnie niezbalansowane, a staż pracy
      jest prawie stały. Nie ma zmiennej, którą można sensownie powiązać z wynagrodzeniem."
    ),

    uiOutput("tab5_verdict"),

    div(class = "chapter-transition",
      p("Pora na duży, dobry zbiór danych."),
      actionButton("ch5_next", "Dalej: 7. Wynagrodzenia \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  )))

ch6_server <- function(input, output, session) {

  output$tab5_table <- DT::renderDataTable({
    datatable(round_df(corp_data), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$tab5_plot_zadowolenie <- renderPlot({
    pct_45 <- round(100 * mean(corp_data$zadowolenie >= 4))
    ggplot(corp_data, aes(x = factor(zadowolenie))) +
      geom_bar(fill = col_bad, alpha = 0.85) +
      scale_x_discrete(limits = c("1","2","3","4","5")) +
      labs(
        title = paste0("Zadowolenie z pracy (skala 1\u20135): ", pct_45, "% odpowiedzi to 4 lub 5"),
        x = "Ocena zadowolenia", y = "Liczba pracowników"
      ) +
      theme_minimal(base_size = 14)
  })

  output$tab5_plot_departament <- renderPlot({
    dept_counts <- corp_data %>%
      count(departament) %>%
      mutate(pct = round(100 * n / sum(n)),
             departament = reorder(departament, -n))
    ggplot(dept_counts, aes(x = departament, y = n)) +
      geom_col(fill = col_bad, alpha = 0.85) +
      geom_text(aes(label = paste0(pct, "%")), vjust = -0.4, size = 4.5) +
      labs(title = "Rozkład pracowników według działu",
           x = "Dział", y = "Liczba pracowników") +
      theme_minimal(base_size = 14)
  })

  tab5_staz_view <- reactiveVal("normal")
  observeEvent(input$tab5_staz_normal, {
    tab5_staz_view("normal")
    session$sendCustomMessage(type = "shinyjs-runjs", message = list(code =
      "$('#tab5_staz_normal').addClass('active'); $('#tab5_staz_wide').removeClass('active');"))
  })
  observeEvent(input$tab5_staz_wide, {
    tab5_staz_view("wide")
    session$sendCustomMessage(type = "shinyjs-runjs", message = list(code =
      "$('#tab5_staz_wide').addClass('active'); $('#tab5_staz_normal').removeClass('active');"))
  })

  output$tab5_plot_staz <- renderPlot({
    med_staz <- median(corp_data$staz_pracy)
    sd_staz  <- round(sd(corp_data$staz_pracy), 2)
    p <- ggplot(corp_data, aes(x = staz_pracy)) +
      geom_histogram(bins = 15, fill = col_mixed, color = "white", alpha = 0.85) +
      geom_vline(xintercept = med_staz, color = col_dark, linetype = "dashed", linewidth = 1) +
      annotate("text", x = med_staz, y = Inf, label = paste0("mediana = ", med_staz),
               vjust = 2, hjust = -0.1, size = 4, color = col_dark) +
      labs(
        title = paste0("Staż pracy  |  zakres: ", min(corp_data$staz_pracy),
                       "\u2013", max(corp_data$staz_pracy), " lat  |  SD = ", sd_staz),
        x = "Staż pracy (lata)", y = "Liczba pracowników"
      ) +
      theme_minimal(base_size = 14)
    if (tab5_staz_view() == "wide") p <- p + scale_x_continuous(limits = c(1, 10))
    p
  })

  output$tab5_plot_wynagrodzenie <- renderPlot({
    med_wyn <- median(corp_data$wynagrodzenie)
    sd_wyn  <- round(sd(corp_data$wynagrodzenie))
    ggplot(corp_data, aes(x = wynagrodzenie)) +
      geom_histogram(bins = 15, fill = col_primary, color = "white", alpha = 0.85) +
      geom_vline(xintercept = med_wyn, color = col_dark, linetype = "dashed", linewidth = 1) +
      annotate("text", x = med_wyn, y = Inf, label = paste0("mediana = ", med_wyn, " PLN"),
               vjust = 2, hjust = -0.1, size = 4, color = col_dark) +
      labs(
        title = paste0("Wynagrodzenie  |  zakres: ", min(corp_data$wynagrodzenie),
                       "\u2013", max(corp_data$wynagrodzenie), " PLN  |  SD = ", sd_wyn, " PLN"),
        x = "Wynagrodzenie (PLN)", y = "Liczba pracowników"
      ) +
      theme_minimal(base_size = 14)
  })

  output$tab5_plot_plec <- renderPlot({
    plec_counts <- corp_data %>%
      count(plec) %>%
      mutate(pct = round(100 * n / sum(n)))
    ggplot(plec_counts, aes(x = plec, y = n)) +
      geom_col(fill = col_bad, alpha = 0.85) +
      geom_text(aes(label = paste0(pct, "%  (n=", n, ")")), vjust = -0.4, size = 4.5) +
      labs(title = "Rozkład pracowników według płci",
           x = "Płeć", y = "Liczba pracowników") +
      theme_minimal(base_size = 14)
  })

  output$tab5_scatter <- renderPlot({
    ggplot(corp_data, aes(x = staz_pracy, y = wynagrodzenie)) +
      geom_point(alpha = 0.5, size = 3, color = col_dark) +
      geom_smooth(method = "lm", color = col_bad, se = TRUE) +
      scale_x_continuous(limits = c(1, 10)) +
      labs(title = "Staż pracy vs wynagrodzenie",
           subtitle = paste0("r = ", round(cor(corp_data$staz_pracy, corp_data$wynagrodzenie), 3),
                             "  \u2014  staż w wąskim przedziale, wynagrodzenia zróżnicowane"),
           x = "Staż pracy (lata)", y = "Wynagrodzenie (PLN)") +
      theme_minimal(base_size = 14)
  })

  output$tab5_scatter_sim <- renderPlot({
    mult <- input$tab5_sd_mult
    sim_staz <- mean(corp_data$staz_pracy) + (corp_data$staz_pracy - mean(corp_data$staz_pracy)) * mult
    sim_wyn <- mean(corp_data$wynagrodzenie) + (corp_data$wynagrodzenie - mean(corp_data$wynagrodzenie)) * mult
    # Add true correlation
    set.seed(42)
    sim_wyn <- sim_wyn + (sim_staz - mean(sim_staz)) * 200 + rnorm(corp_n, 0, 100 * mult)
    r <- round(cor(sim_staz, sim_wyn), 3)

    ggplot(data.frame(x = sim_staz, y = sim_wyn), aes(x, y)) +
      geom_point(alpha = 0.5, size = 3, color = col_dark) +
      geom_smooth(method = "lm", color = col_primary, se = TRUE) +
      labs(title = paste0("Symulacja z SD \u00d7 ", mult),
           subtitle = paste0("r = ", r),
           x = "Staż pracy (lata)", y = "Wynagrodzenie (PLN)") +
      theme_minimal(base_size = 14)
  })

  output$tab5_verdict <- renderUI({
    # hipoteza, n, mix, zmiennosc, struktura, niezaleznosc | braki, definicje, bledy
    # Firma: jedyny problem to brak zmiennosci (zadowolenie skupione, staz wąski, grupy niezbalansowane)
    render_verdict(c("yes", "yes", "yes", "no", "yes", "yes", "yes", "yes", "yes"), "bad")
  })
}
