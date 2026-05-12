# ============================================================================
# ROZDZIAŁ 4: Twoja prognoza (ćwiczenie)
# ============================================================================

ch4_ui <- lecture_chapter(
  id = "ch-cwiczenie",
  num = "04",
  title = "Twoja prognoza",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 05 · Prognozy",
      num = "04",
      title = "Twoja prognoza.",
      lead = "Sprawdź się: wybierz najlepszy model dla danego szeregu i oceń, jak realistyczny jest przedział ufności."
    ),

    lc_h2("ch4-pomysl", "Sytuacja"),
    lc_p("Sklep z odzieżą zebrał kwartalną sprzedaż za 5 lat (20 kwartałów). Sprzedaż rośnie z roku na rok, a IV kwartał — z powodu Bożego Narodzenia — jest zawsze wyraźnie wyższy. Trzeba zaprognozować następne 4 kwartały, żeby zaplanować zatowarowanie i zatrudnienie sezonowe."),

    figure_panel(
      label = "Ryc. 4.1",
      title = "Sprzedaż kwartalna 2020Q1–2024Q4 (tys. zł)",
      full_width = TRUE,
      plotOutput("ch4_plot", height = "340px")
    ),

    lc_h2("ch4-pytanie", "Który model wybrać?"),
    radioButtons("ch4_model", NULL,
      choices = c(
        "Sam trend liniowy: y ~ t" = "a",
        "Trend + zmienna 0/1 dla kwartałów: y ~ t + quarter" = "b",
        "Średnia ruchoma z 4 ostatnich kwartałów" = "c",
        "Zostawić ostatnią obserwację jako prognozę" = "d"
      ),
      selected = character(0)
    ),
    uiOutput("ch4_feedback_model"),

    lc_h2("ch4-pytanie2", "Jaki przedział ufności jest realistyczny?"),
    radioButtons("ch4_band", NULL,
      choices = c(
        "Tym węższy, im więcej historii" = "a",
        "Tym węższy, im wyższe R²" = "b",
        "Rośnie z horyzontem prognozy" = "c",
        "Wszystkie powyższe" = "d"
      ),
      selected = character(0)
    ),
    uiOutput("ch4_feedback_band"),

    inline_callout(
      label = "Zapamiętaj",
      color = "wskazowka",
      open = TRUE,
      "Prognozowanie to nie magia — to dopasowanie sensownego modelu i uczciwa ocena niepewności. Zawsze podawaj przedział, zawsze testuj ex post."
    )
  )
)

ch4_server <- function(input, output, session) {
  ch4_df <- reactive({
    raw <- eco_ts_data(n = 20, trend = 0.6, season = 10, noise = 2, seed = 999)
    # Przeskalowanie na "tys. zł" - sprzedaż 50–100
    raw$y <- 50 + (raw$y - min(raw$y)) / (max(raw$y) - min(raw$y)) * 50
    years <- 2020 + (raw$t - 1) %/% 4
    qs <- ((raw$t - 1) %% 4) + 1
    raw$label <- paste0(years, "Q", qs)
    raw
  })

  output$ch4_plot <- renderPlot({
    df <- ch4_df()
    ggplot(df, aes(t, y)) +
      geom_line(color = upwr_secondary, linewidth = 0.7) +
      geom_point(aes(color = quarter), size = 2.5) +
      scale_color_manual(values = upwr_cat_n(4),
                         name = "Kwartał") +
      scale_x_continuous(breaks = seq(1, 20, by = 4),
                         labels = paste0(2020:2024, "Q1")) +
      labs(x = NULL, y = "Sprzedaż (tys. zł)") +
      theme_upwr()
  })

  output$ch4_feedback_model <- renderUI({
    sel <- input$ch4_model
    if (is.null(sel) || length(sel) == 0) return(NULL)
    switch(sel,
      a = lc_feedback(type = "warning",
        lc_p("Łapie trend, ale ignoruje sezonowość — IV kwartał będzie systematycznie zaniżony, a I kwartał zawyżony. MAPE byłby wysoki właśnie w punktach świątecznych.")),
      b = lc_feedback(type = "ok",
        lc_p(strong("Dokładnie. "), "Łączy trend i sezonowość — to podstawowy model dynamiczny dla szeregów z obu efektami. Dla tego sklepu właśnie tego potrzebujemy.")),
      c = lc_feedback(type = "warning",
        lc_p("Wygładza, ale gubi i trend (prognoza staje się płaska), i sezonowość (uśredniona do zera). Słabe dla rosnącego szeregu z wyraźnymi wzorcami kwartalnymi.")),
      d = lc_feedback(type = "warning",
        lc_p("Naiwna prognoza — ignoruje wszystko, co model widział w historii. Może mieć sens jako benchmark („czy mój model jest lepszy niż nic?”), ale nie jako rekomendacja dla zarządu.")),
      NULL
    )
  })

  output$ch4_feedback_band <- renderUI({
    sel <- input$ch4_band
    if (is.null(sel) || length(sel) == 0) return(NULL)
    switch(sel,
      a = lc_feedback(type = "warning",
        lc_p("To prawda, ale niepełna — to tylko jeden z czynników. Spróbuj jeszcze raz.")),
      b = lc_feedback(type = "warning",
        lc_p("To prawda, ale niepełna — wyższe R² to mniejsza zmienność reszt, więc węższy przedział. Ale nie tylko o to chodzi.")),
      c = lc_feedback(type = "warning",
        lc_p("To prawda, ale niepełna — błąd standardowy prognozy rośnie z odległością od ostatniej obserwacji. Ale to nie jedyny czynnik.")),
      d = lc_feedback(type = "ok",
        lc_p(strong("Dokładnie. "), "Wszystkie trzy czynniki łącznie kształtują szerokość przedziału: dłuższa historia i lepiej dopasowany model go zawężają, a dalszy horyzont — rozszerza.")),
      NULL
    )
  })
}
