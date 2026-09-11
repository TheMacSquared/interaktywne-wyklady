# Tab 4: Pingwiny — palmerpenguins, dobry zbiór

ch4_ui <- lecture_chapter(id = "ch4", num = "4", title = "Pingwiny", content = tagList(
  fluidRow(column(8, offset = 2,

    lc_chapter_hero(
      kicker = "Rozdział 04 · Co czyni dobry zbiór danych?",
      num    = "04",
      title  = "Pingwiny z Antarktydy.",
      lead   = "Dobry zbiór nie musi być idealny. Ważne, żeby braki i ograniczenia
                były jawne, małe i możliwe do uzasadnienia."
    ),

    lc_h2("sec-01", "Pingwiny z Antarktydy"),

    div(class = "lc-prose",
      p("Dane z badania 344 pingwinów trzech gatunków (Adelie, Chinstrap, Gentoo)
        na trzech wyspach archipelagu Palmera na Antarktydzie.
        Pomiary ciała: dziób, płetwy, masa."),
      p("Źródło: pakiet palmerpenguins w R (Horst, Hill & Gorman, 2020).")
    ),

    lc_h2("sec-02", "Podgląd danych"),

    div(class = "lc-figure-panel",
      DT::dataTableOutput("tab3_table")
    ),

    lc_h2("sec-03", "Czy są braki danych?"),

    div(class = "lc-figure-panel",
      zoom_plot_ui("tab3_missing", height = "250px"),
      uiOutput("tab3_missing_info")
    ),

    lc_h2("sec-04", "Eksploracja"),

    div(class = "lc-figure-panel",
      fluidRow(
        column(4, selectInput("tab3_var", "Zmienna:",
          choices = c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g"))),
        column(8, zoom_plot_ui("tab3_boxplot", height = "300px"))
      )
    ),

    lc_h2("sec-05", "Werdykt"),

    div(class = "lc-feedback lc-feedback-ok",
      tags$strong("Dobry zbiór!"),
      " n = 344, trzy zbalansowane grupy gatunków, jasno zdefiniowane zmienne pomiarowe.",
      tags$br(),
      "Niewielkie braki danych (< 3%) - można je bezpiecznie usunąć (listwise deletion).",
      tags$br(),
      "Możliwe analizy: test t, ANOVA, korelacja, regresja, chi-kwadrat."
    ),

    lc_chapter_next(
      num = "05",
      title = "Filmy Tarantino",
      lead = "Następny zbiór wygląda ciekawie... ale czy nadaje się do analizy?",
      target_id = "ch5"
    ),

    div(style = "height: 40px;")
  ))))

ch4_server <- function(input, output, session) {

  output$tab3_table <- DT::renderDataTable({
    datatable(round_df(penguins), options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  zoom_plot_server("tab3_missing", reactive({
    miss_pct <- sapply(penguins, function(x) mean(is.na(x)) * 100)
    df_miss <- data.frame(variable = names(miss_pct), pct = miss_pct)
    df_miss$color <- ifelse(df_miss$pct > 5, data_bad, ifelse(df_miss$pct > 0, data_mixed, data_good))

    ggplot(df_miss, aes(x = reorder(variable, -pct), y = pct, fill = color)) +
      geom_col() +
      scale_fill_identity() +
      geom_hline(yintercept = 5, linetype = "dashed", color = data_bad) +
      annotate("text", x = 2, y = 6, label = "Próg 5%", color = data_bad, size = 4) +
      labs( x = NULL, y = "% braków") +
      theme_upwr(base_size = 14) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  }))

  output$tab3_missing_info <- renderUI({
    n_complete <- sum(complete.cases(penguins))
    n_total <- nrow(penguins)
    div(class = "lc-feedback lc-feedback-info",
      paste0("Kompletne obserwacje: ", n_complete, " z ", n_total,
             " (", round(n_complete / n_total * 100, 1), "%). ",
             "Braki dotyczą głównie zmiennej sex (", sum(is.na(penguins$sex)), " NA).")
    )
  })

  zoom_plot_server("tab3_boxplot", reactive({
    req(input$tab3_var)
    ggplot(penguins %>% filter(!is.na(.data[[input$tab3_var]])),
           aes(x = species, y = .data[[input$tab3_var]], fill = species)) +
      geom_boxplot(alpha = 0.7) +
      scale_fill_manual(values = c(data_primary, data_mixed, data_good)) +
      labs(x = "Gatunek", y = input$tab3_var) +
      theme_upwr(base_size = 14) +
      theme(legend.position = "none")
  }))

}
