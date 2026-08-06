# ============================================================================
# CHAPTER 3B: KONTEKST, ZMIENNE JAKOŚCIOWE I INTERAKCJE
# ============================================================================

.ch3b_species_colors <- c(
  Adelie = unname(upwr_cat["szalwia"]),
  Chinstrap = unname(upwr_cat["bursztyn"]),
  Gentoo = unname(upwr_cat["terakota"])
)

.ch3b_term_labels <- c(
  "(Intercept)" = "Stała: Adelie",
  flipper_length_mm = "Długość płetwy",
  speciesChinstrap = "Gatunek: Chinstrap",
  speciesGentoo = "Gatunek: Gentoo",
  `flipper_length_mm:speciesChinstrap` = "Płetwa × Chinstrap",
  `flipper_length_mm:speciesGentoo` = "Płetwa × Gentoo"
)

ch3b_ui <- list(
  id = "ch-3b",
  num = "03B",
  title = "Kontekst i interakcje",
  duration = "30–45 min",
  content = tagList(
    lc_chapter_hero(
      kicker = "Regresja · pogłębienie",
      num = "03B",
      title = "Jedna linia może ukrywać trzy różne historie.",
      lead = paste(
        "Pingwiny wprowadzają naturalne grupy. Dzięki nim widać, dlaczego",
        "predyktor jakościowy może odwrócić wniosek i dlaczego czasem potrzebujemy interakcji."
      )
    ),

    lc_h2("ch3b-simpson", "Pominięta zmienna i paradoks Simpsona"),

    p(
      "Po połączeniu trzech gatunków długość i wysokość dzioba wydają się",
      "związane ujemnie. Po rozdzieleniu gatunków relacja wewnątrz grup zmienia",
      "kierunek. Nie jest to sztuczka wykresu — gatunek jest pominiętą zmienną",
      "opisującą różne populacje."
    ),

    figure_panel(
      label = "Demo 4.1",
      title = "Paradoks Simpsona krok po kroku",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          actionButton(
            "ch3b_simpson_all", "1. Jedna linia dla wszystkich",
            class = "lc-btn-outline", width = "100%"
          ),
          actionButton(
            "ch3b_simpson_groups", "2. Pokaż gatunki",
            class = "lc-btn-outline", width = "100%"
          ),
          actionButton(
            "ch3b_simpson_control", "3. Kontroluj gatunek",
            class = "lc-btn-primary", width = "100%"
          ),
          uiOutput("ch3b_simpson_stats")
        ),
        column(
          8,
          zoom_plot_ui("ch3b_simpson_plot", height = "430px"),
          uiOutput("ch3b_simpson_explanation")
        )
      )
    ),

    lc_feedback(
      type = "warning",
      tags$strong("Pułapka:"),
      " współczynnik modelu prostego miesza różnice między grupami z relacją",
      " obserwowaną wewnątrz każdej grupy. Więcej danych nie naprawia pominiętej zmiennej."
    ),

    lc_h2("ch3b-kategoria", "Predyktor jakościowy w równaniu"),

    p(
      "Gatunek nie ma sensownej jednostki liczbowej. Model tworzy więc zmienne",
      "wskaźnikowe i porównuje każdy gatunek z poziomem odniesienia. Tutaj",
      "poziomem odniesienia jest Adelie."
    ),

    lc_formula_box(withMathJax(
      "$$\\widehat{masa}=\\beta_0+\\beta_1\\,płetwa+\\beta_2 I(Chinstrap)+\\beta_3 I(Gentoo)$$"
    )),

    tags$ul(
      tags$li("β₁ opisuje wspólne nachylenie linii."),
      tags$li("β₂ i β₃ przesuwają poziom przewidywanej masy względem Adelie."),
      tags$li("Model addytywny zakłada, że linie dla gatunków są równoległe.")
    ),

    lc_h2("ch3b-interakcja", "Czy nachylenie zależy od gatunku?"),

    p(
      "Interakcja odpowiada na pytanie, czy dodatkowy milimetr płetwy ma taki",
      "sam związek z masą ciała u każdego gatunku. To pytanie o mechanizm,",
      "a nie obowiązkowy sposób poprawiania dopasowania."
    ),

    figure_panel(
      label = "Demo 4.2",
      title = "Model addytywny kontra model z interakcją",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          radioButtons(
            "ch3b_interaction_model",
            "Model:",
            choices = c(
              "Addytywny: płetwa + gatunek" = "add",
              "Z interakcją: płetwa × gatunek" = "interaction"
            ),
            selected = "add"
          ),
          uiOutput("ch3b_interaction_metrics"),
          lc_feedback(
            type = "info",
            tags$strong("Czytaj wykres przed tabelą:"),
            " równoległe linie oznaczają wspólne nachylenie; różne nachylenia",
            " oznaczają, że efekt płetwy zależy od gatunku."
          )
        ),
        column(
          8,
          zoom_plot_ui("ch3b_interaction_plot", height = "400px"),
          uiOutput("ch3b_interaction_table")
        )
      )
    ),

    lc_feedback(
      type = "info",
      tags$strong("Transfer do CASchools:"),
      " analogiczne pytanie brzmi: czy związek dochodu z wynikiem jest taki sam",
      " w okręgach o różnym kontekście społecznym? Pingwiny pokazują mechanizm",
      " czyściej, a CASchools przypomina o ostrożności interpretacji."
    ),

    lc_h2("ch3b-decyzja", "Jak zdecydować, czy dodać interakcję?"),

    tags$ol(
      tags$li("Najpierw sformułuj pytanie: czy efekt X powinien zależeć od grupy?"),
      tags$li("Narysuj przewidywane linie i sprawdź, czy różnica ma sens praktyczny."),
      tags$li("Porównaj dopasowanie, ale uwzględnij dodatkową złożoność modelu."),
      tags$li("Zachowaj efekty główne, jeżeli w modelu występuje ich interakcja.")
    ),

    lc_chapter_next(
      num = "04",
      title = "Porównywanie modeli",
      lead = "Różnica w dopasowaniu musi uzasadnić dodatkową złożoność.",
      target_id = "ch-4"
    )
  )
)

ch3b_server <- function(input, output, session) {
  penguins <- .penguins_data
  simpson_step <- reactiveVal("all")

  observeEvent(input$ch3b_simpson_all, simpson_step("all"))
  observeEvent(input$ch3b_simpson_groups, simpson_step("groups"))
  observeEvent(input$ch3b_simpson_control, simpson_step("control"))

  simple_model <- lm(bill_depth_mm ~ bill_length_mm, data = penguins)
  controlled_model <- lm(bill_depth_mm ~ bill_length_mm + species, data = penguins)

  zoom_plot_server("ch3b_simpson_plot", reactive({
    step <- simpson_step()
    plot <- ggplot(penguins, aes(bill_length_mm, bill_depth_mm))

    if (identical(step, "all")) {
      plot <- plot +
        geom_point(color = upwr_secondary, alpha = 0.55) +
        geom_smooth(method = "lm", se = FALSE, color = upwr_accent, linewidth = 1.2)
    } else {
      plot <- plot +
        geom_point(aes(color = species, shape = species), alpha = 0.62) +
        geom_smooth(aes(color = species), method = "lm", se = FALSE, linewidth = 1.05) +
        scale_color_manual(values = .ch3b_species_colors, name = "Gatunek") +
        labs(shape = "Gatunek")
    }

    plot +
      labs(
        title = if (identical(step, "all")) {
          "Połączone dane sugerują związek ujemny"
        } else {
          "Wewnątrz gatunków linie mają inny kierunek"
        },
        x = "Długość dzioba (mm)",
        y = "Wysokość dzioba (mm)"
      ) +
      theme_upwr()
  }), alt = paste(
    "Punkty długości i wysokości dzioba pingwinów. Po pokazaniu gatunków",
    "widoczne są trzy grupy i odmienne linie regresji."
  ))

  output$ch3b_simpson_stats <- renderUI({
    b_simple <- coef(simple_model)[["bill_length_mm"]]
    b_control <- coef(controlled_model)[["bill_length_mm"]]
    lc_stat_grid(
      lc_stat_box("Bez gatunku", round(b_simple, 3), caption = "nachylenie"),
      lc_stat_box(
        "Po kontroli gatunku", round(b_control, 3),
        caption = "nachylenie", color = upwr_accent
      ),
      columns = 1
    )
  })

  output$ch3b_simpson_explanation <- renderUI({
    step <- simpson_step()
    if (identical(step, "all")) {
      return(lc_feedback(
        type = "warning",
        "Model nie wie jeszcze, że punkty pochodzą z trzech różnych gatunków."
      ))
    }
    if (identical(step, "groups")) {
      return(lc_feedback(
        type = "info",
        "Kolor ujawnia ukrytą strukturę: gatunki różnią się położeniem obu zmiennych."
      ))
    }
    lc_feedback(
      type = "ok",
      tags$strong("Po kontroli gatunku znak współczynnika się zmienia."),
      " To przykład, w którym model wieloraki odpowiada na inne, lepiej określone pytanie."
    )
  })

  interaction_model <- reactive({
    if (identical(input$ch3b_interaction_model, "interaction")) {
      lm(body_mass_g ~ flipper_length_mm * species, data = penguins)
    } else {
      lm(body_mass_g ~ flipper_length_mm + species, data = penguins)
    }
  })

  prediction_grid <- reactive({
    grid <- expand.grid(
      flipper_length_mm = seq(
        min(penguins$flipper_length_mm),
        max(penguins$flipper_length_mm),
        length.out = 160
      ),
      species = levels(penguins$species)
    )
    grid$species <- factor(grid$species, levels = levels(penguins$species))
    grid$prediction <- predict(interaction_model(), newdata = grid)
    grid
  })

  zoom_plot_server("ch3b_interaction_plot", reactive({
    ggplot(penguins, aes(flipper_length_mm, body_mass_g, color = species, shape = species)) +
      geom_point(alpha = 0.42) +
      geom_line(
        data = prediction_grid(),
        aes(
          x = flipper_length_mm, y = prediction,
          color = species, group = species
        ),
        linewidth = 1.2,
        inherit.aes = FALSE
      ) +
      scale_color_manual(values = .ch3b_species_colors, name = "Gatunek") +
      labs(
        title = if (identical(input$ch3b_interaction_model, "interaction")) {
          "Interakcja pozwala gatunkom mieć różne nachylenia"
        } else {
          "Model addytywny wymusza równoległe linie"
        },
        x = "Długość płetwy (mm)",
        y = "Masa ciała (g)",
        shape = "Gatunek"
      ) +
      theme_upwr()
  }), alt = paste(
    "Masa ciała względem długości płetwy dla trzech gatunków pingwinów.",
    "Model addytywny pokazuje równoległe linie, a model interakcyjny różne nachylenia."
  ))

  output$ch3b_interaction_metrics <- renderUI({
    model <- interaction_model()
    rmse <- sqrt(mean(residuals(model)^2))
    lc_stat_grid(
      lc_stat_box("AIC", round(AIC(model), 1), color = unname(upwr_cat["wrzos"])),
      lc_stat_box("RMSE", round(rmse, 1), color = upwr_secondary),
      lc_stat_box("Parametry", length(coef(model)), color = unname(upwr_cat["bursztyn"])),
      columns = 1
    )
  })

  output$ch3b_interaction_table <- renderUI({
    table <- broom::tidy(interaction_model())
    table$label <- vapply(table$term, function(term) {
      if (term %in% names(.ch3b_term_labels)) {
        unname(.ch3b_term_labels[[term]])
      } else {
        term
      }
    }, character(1))

    tags$table(
      class = "lc-table lc-table-bordered lc-table-striped",
      tags$thead(tags$tr(
        tags$th("Składnik"),
        tags$th("Współczynnik"),
        tags$th("Błąd stand."),
        tags$th("p-value")
      )),
      tags$tbody(lapply(seq_len(nrow(table)), function(index) {
        tags$tr(
          tags$td(table$label[[index]]),
          tags$td(round(table$estimate[[index]], 3)),
          tags$td(round(table$std.error[[index]], 3)),
          tags$td(if (table$p.value[[index]] < 0.001) {
            "< 0,001"
          } else {
            format(round(table$p.value[[index]], 3), decimal.mark = ",")
          })
        )
      }))
    )
  })
}
