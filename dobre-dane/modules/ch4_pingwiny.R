# Tab 4: Pingwiny — palmerpenguins, dobry zbiór

ch4_ui <- tabPanel("4. Pingwiny",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Pingwiny z Antarktydy"),

    div(class = "narrative",
      p("Dane z badania 344 pingwinów trzech gatunków (Adelie, Chinstrap, Gentoo)
        na trzech wyspach archipelagu Palmera na Antarktydzie.
        Pomiary ciała: dziób, płetwy, masa."),
      p("Źródło: pakiet palmerpenguins w R (Horst, Hill & Gorman, 2020).")
    ),

    div(class = "section-title", "Podgląd danych"),

    div(class = "widget-block",
      DT::dataTableOutput("tab3_table")
    ),

    div(class = "section-title", "Czy są braki danych?"),

    div(class = "widget-block",
      plotOutput("tab3_missing", height = "250px"),
      uiOutput("tab3_missing_info")
    ),

    div(class = "section-title", "Eksploracja"),

    div(class = "widget-block",
      fluidRow(
        column(4, selectInput("tab3_var", "Zmienna:",
          choices = c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g"))),
        column(8, plotOutput("tab3_boxplot", height = "300px"))
      )
    ),

    div(class = "section-title", "Werdykt"),

    div(class = "callout-success",
      tags$strong("Dobry zbiór!"),
      " n = 344, trzy zbalansowane grupy gatunków, jasno zdefiniowane zmienne pomiarowe.",
      tags$br(),
      "Niewielkie braki danych (< 3%) - można je bezpiecznie usunąć (listwise deletion).",
      tags$br(),
      "Możliwe analizy: test t, ANOVA, korelacja, regresja, chi-kwadrat."
    ),

    div(class = "chapter-transition",
      p("Następny zbiór wygląda ciekawie... ale czy nadaje się do analizy?"),
      actionButton("ch3_next", "Dalej: 5. Filmy Tarantino \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  )))

ch4_server <- function(input, output, session) {

  output$tab3_table <- DT::renderDataTable({
    datatable(round_df(penguins), options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  output$tab3_missing <- renderPlot({
    miss_pct <- sapply(penguins, function(x) mean(is.na(x)) * 100)
    df_miss <- data.frame(variable = names(miss_pct), pct = miss_pct)
    df_miss$color <- ifelse(df_miss$pct > 5, col_bad, ifelse(df_miss$pct > 0, col_mixed, col_good))

    ggplot(df_miss, aes(x = reorder(variable, -pct), y = pct, fill = color)) +
      geom_col() +
      scale_fill_identity() +
      geom_hline(yintercept = 5, linetype = "dashed", color = col_bad) +
      annotate("text", x = 2, y = 6, label = "Próg 5%", color = col_bad, size = 4) +
      labs(title = "Procent braków danych", x = NULL, y = "% braków") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })

  output$tab3_missing_info <- renderUI({
    n_complete <- sum(complete.cases(penguins))
    n_total <- nrow(penguins)
    div(class = "callout-info",
      paste0("Kompletne obserwacje: ", n_complete, " z ", n_total,
             " (", round(n_complete / n_total * 100, 1), "%). ",
             "Braki dotyczą głównie zmiennej sex (", sum(is.na(penguins$sex)), " NA).")
    )
  })

  output$tab3_boxplot <- renderPlot({
    req(input$tab3_var)
    ggplot(penguins %>% filter(!is.na(.data[[input$tab3_var]])),
           aes(x = species, y = .data[[input$tab3_var]], fill = species)) +
      geom_boxplot(alpha = 0.7) +
      scale_fill_manual(values = c(col_primary, col_mixed, col_good)) +
      labs(title = paste(input$tab3_var, "wg gatunku"), x = "Gatunek", y = input$tab3_var) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })

}
