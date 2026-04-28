# Tab 6: Hotel — oceny hotelu boutique, brak zmienności

ch6_ui <- lecture_chapter(id = "ch6", num = "6", title = "Hotel", content = tagList(
  fluidRow(column(8, offset = 2,

    lc_h2("sec-01", "Oceny hotelu boutique"),

    div(class = "lc-prose",
      p("Portal rezerwacyjny zebrał opinie gości ekskluzywnego hotelu boutique.
        80 recenzji po ostatnim sezonie. Chcemy zbadać, co wpływa na ocenę hotelu.")
    ),

    lc_h2("sec-02", "Podgląd danych"),

    div(class = "lc-figure-panel",
      DT::dataTableOutput("tab5_table")
    ),

    lc_h2("sec-03", "Zmienna 1: Ocena ogólna"),

    div(class = "lc-figure-panel",
      plotOutput("tab5_plot_zadowolenie", height = "300px")
    ),
    div(class = "lc-feedback lc-feedback-danger",
      tags$strong("Problem:"), " brak zróżnicowania odpowiedzi.",
      " 92% gości wystawiło ocenę 4 lub 5. Skala 1–5 w praktyce działa tu jak skala 1–2 —
      kiedy wszyscy odpowiadają tak samo, zmienna nic nie mówi o tym, co różnicuje pobyt."
    ),

    lc_h2("sec-04", "Zmienna 2: Typ pokoju"),

    div(class = "lc-figure-panel",
      plotOutput("tab5_plot_departament", height = "300px")
    ),
    div(class = "lc-feedback lc-feedback-danger",
      tags$strong("Problem:"), " niezbalansowane grupy.",
      " 84% gości nocowało w Apartamencie Premium. Pozostałe typy pokojów mają po kilka obserwacji —
      porównanie satysfakcji między typami pokojów będzie niemożliwe."
    ),

    lc_h2("sec-05", "Zmienna 3: Długość pobytu"),

    div(class = "toggle-pills",
      actionButton("tab5_staz_normal", "Dane", class = "pill-btn active"),
      actionButton("tab5_staz_wide", "Pełna skala (1–14 nocy)", class = "pill-btn")
    ),
    div(class = "lc-figure-panel",
      plotOutput("tab5_plot_staz", height = "300px")
    ),
    div(class = "lc-feedback lc-feedback-warning",
      tags$strong("Uwaga:"), " wąska rozpiętość wartości.",
      " Wszyscy goście zatrzymali się na 1–3 noce. Sama w sobie ograniczona rozpiętość nie jest błędem
      — może tak wyglądał ten segment hotelu. Ale gdy ", tags$em("cały zbiór"),
      " jest skupiony w tak wąskim przedziale, wykrycie zależności między długością pobytu
      a innymi zmiennymi staje się bardzo trudne."
    ),

    lc_h2("sec-06", "Zmienna 4: Cena za noc"),

    div(class = "lc-figure-panel",
      plotOutput("tab5_plot_wynagrodzenie", height = "300px")
    ),
    div(class = "lc-feedback lc-feedback-ok",
      "Ceny za noc mają dobry rozrzut.",
      " To dobra wiadomość — ta zmienna wydaje się użyteczna.
      Zobaczmy więc, czy możemy ją powiązać z czymś innym w tym zbiorze."
    ),

    lc_h2("sec-07", "Zmienna 5: Kraj gościa"),

    div(class = "lc-figure-panel",
      plotOutput("tab5_plot_plec", height = "300px")
    ),
    div(class = "lc-feedback lc-feedback-danger",
      tags$strong("Problem:"), " niezbalansowane grupy.",
      " 87% gości to turyści z Polski. Pozostałe kraje mają po 1–4 osoby —
      jakiekolwiek porównanie między krajami nie ma sensu przy takiej dysproporcji."
    ),

    lc_h2("sec-08", "Co się dzieje gdy próbujemy szukać zależności?"),

    div(class = "lc-feedback lc-feedback-info",
      "Cena za noc ma dobry rozrzut. Czy możemy powiązać ją z długością pobytu? ",
      "Sprawdźmy — pamiętaj, że długość pobytu mieści się w bardzo wąskim przedziale."
    ),

    div(class = "lc-figure-panel",
      plotOutput("tab5_scatter", height = "300px")
    ),

    lc_h2("sec-09", "Co by było, gdyby dane miały normalną zmienność?"),

    div(class = "lc-feedback lc-feedback-info",
      "Co by było, gdyby goście różnili się długością pobytu bardziej — np. od 1 do 14 nocy?
      Przesuń suwak i obserwuj jak pojawia się zależność: dłuższy pobyt — niższa cena za noc
      (zniżki za dłuższe rezerwacje)."
    ),

    div(class = "lc-figure-panel",
      sliderInput("tab5_sd_mult", "Mnożnik rozrzutu danych:", min = 1, max = 5, value = 1, step = 0.5),
      plotOutput("tab5_scatter_sim", height = "300px")
    ),

    lc_h2("sec-10", "Werdykt"),

    div(class = "lc-feedback lc-feedback-danger",
      "Ten zbiór danych nie nadaje się do analizy.",
      tags$br(),
      "Ceny za noc mają dobry rozrzut, ale trudno to wykorzystać: oceny skupione przy maksimum,
      typy pokojów i kraje skrajnie niezbalansowane, a długość pobytu prawie stała.
      Nie ma zmiennej, z którą można sensownie powiązać cenę za noc."
    ),

    lc_chapter_next(
      num = "07",
      title = "Wynagrodzenia",
      lead = "Pora na duży, dobry zbiór danych.",
      target_id = "ch7"
    ),

    div(style = "height: 40px;")
  ))))

ch6_server <- function(input, output, session) {

  output$tab5_table <- DT::renderDataTable({
    datatable(round_df(hotel_data), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$tab5_plot_zadowolenie <- renderPlot({
    pct_45 <- round(100 * mean(hotel_data$ocena_ogolna >= 4))
    ggplot(hotel_data, aes(x = factor(ocena_ogolna))) +
      geom_bar(fill = data_bad, alpha = 0.85) +
      scale_x_discrete(limits = c("1","2","3","4","5")) +
      labs(
        title = paste0("Ocena ogólna hotelu (skala 1–5): ", pct_45, "% odpowiedzi to 4 lub 5"),
        x = "Ocena ogólna", y = "Liczba gości"
      ) +
      theme_upwr(base_size = 14)
  })

  output$tab5_plot_departament <- renderPlot({
    typ_counts <- hotel_data %>%
      count(typ_pokoju) %>%
      mutate(pct = round(100 * n / sum(n)),
             typ_pokoju = reorder(typ_pokoju, -n))
    ggplot(typ_counts, aes(x = typ_pokoju, y = n)) +
      geom_col(fill = data_bad, alpha = 0.85) +
      geom_text(aes(label = paste0(pct, "%")), vjust = -0.4, size = 4.5) +
      labs(title = "Rozkład gości według typu pokoju",
           x = "Typ pokoju", y = "Liczba gości") +
      theme_upwr(base_size = 14)
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
    med_pobytu <- median(hotel_data$dlugosc_pobytu)
    p <- ggplot(hotel_data, aes(x = dlugosc_pobytu)) +
      geom_bar(fill = data_mixed, alpha = 0.85, width = 0.6) +
      geom_vline(xintercept = med_pobytu, color = data_reference, linetype = "dashed", linewidth = 1) +
      annotate("text", x = med_pobytu, y = Inf, label = paste0("mediana = ", med_pobytu, " noc"),
               vjust = 2, hjust = -0.1, size = 4, color = data_reference) +
      scale_x_continuous(breaks = 1:14) +
      labs(
        title = paste0("Długość pobytu  |  zakres: ", min(hotel_data$dlugosc_pobytu),
                       "–", max(hotel_data$dlugosc_pobytu), " noce"),
        x = "Długość pobytu (noce)", y = "Liczba gości"
      ) +
      theme_upwr(base_size = 14)
    if (tab5_staz_view() == "wide") p <- p + scale_x_continuous(limits = c(1, 14), breaks = seq(1, 14, 2))
    p
  })

  output$tab5_plot_wynagrodzenie <- renderPlot({
    med_cena <- median(hotel_data$cena_za_noc)
    sd_cena  <- round(sd(hotel_data$cena_za_noc))
    ggplot(hotel_data, aes(x = cena_za_noc)) +
      geom_histogram(bins = 15, fill = data_primary, color = "white", alpha = 0.85) +
      geom_vline(xintercept = med_cena, color = data_reference, linetype = "dashed", linewidth = 1) +
      annotate("text", x = med_cena, y = Inf, label = paste0("mediana = ", med_cena, " PLN"),
               vjust = 2, hjust = -0.1, size = 4, color = data_reference) +
      labs(
        title = paste0("Cena za noc  |  zakres: ", min(hotel_data$cena_za_noc),
                       "–", max(hotel_data$cena_za_noc), " PLN  |  SD = ", sd_cena, " PLN"),
        x = "Cena za noc (PLN)", y = "Liczba gości"
      ) +
      theme_upwr(base_size = 14)
  })

  output$tab5_plot_plec <- renderPlot({
    kraj_counts <- hotel_data %>%
      count(kraj_goscia) %>%
      mutate(pct = round(100 * n / sum(n)))
    ggplot(kraj_counts, aes(x = reorder(kraj_goscia, -n), y = n)) +
      geom_col(fill = data_bad, alpha = 0.85) +
      geom_text(aes(label = paste0(pct, "%  (n=", n, ")")), vjust = -0.4, size = 4.5) +
      labs(title = "Rozkład gości według kraju",
           x = "Kraj gościa", y = "Liczba gości") +
      theme_upwr(base_size = 14)
  })

  output$tab5_scatter <- renderPlot({
    ggplot(hotel_data, aes(x = dlugosc_pobytu, y = cena_za_noc)) +
      geom_point(alpha = 0.5, size = 3, color = data_reference) +
      geom_smooth(method = "lm", color = data_bad, se = TRUE) +
      scale_x_continuous(limits = c(1, 14), breaks = seq(1, 14, 2)) +
      labs(title = "Długość pobytu vs cena za noc",
           subtitle = paste0("r = ", round(cor(hotel_data$dlugosc_pobytu, hotel_data$cena_za_noc), 3),
                             "  —  pobyt w wąskim przedziale 1–3 noce"),
           x = "Długość pobytu (noce)", y = "Cena za noc (PLN)") +
      theme_upwr(base_size = 14)
  })

  output$tab5_scatter_sim <- renderPlot({
    mult <- input$tab5_sd_mult
    set.seed(42)
    spread <- (mult - 1) * 3
    sim_pobytu <- pmax(1, hotel_data$dlugosc_pobytu + runif(hotel_n, -spread, spread))
    sim_cena   <- hotel_data$cena_za_noc - (sim_pobytu - mean(sim_pobytu)) * 25 +
                    rnorm(hotel_n, 0, 40)
    r <- round(cor(sim_pobytu, sim_cena), 3)

    ggplot(data.frame(x = sim_pobytu, y = sim_cena), aes(x, y)) +
      geom_point(alpha = 0.5, size = 3, color = data_reference) +
      geom_smooth(method = "lm", color = data_primary, se = TRUE) +
      labs(title = paste0("Symulacja z rozrzutem ×", mult),
           subtitle = paste0("r = ", r, "  —  dłuższy pobyt = niższa cena za noc (znizka wolumenowa)"),
           x = "Długość pobytu (symulowane noce)", y = "Cena za noc (PLN)") +
      theme_upwr(base_size = 14)
  })

}
