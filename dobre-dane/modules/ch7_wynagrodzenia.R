# Tab 7: Wynagrodzenia — Wage (ISLR), dobry zbiór

ch7_ui <- tabPanel("7. Wynagrodzenia",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Wynagrodzenia w USA"),

    div(class = "narrative",
      p("Dane z Current Population Survey: 3000 mężczyzn z regionu Mid-Atlantic.
        Informacje o zarobkach, wykształceniu, zawodzie, wieku i zdrowiu."),
      p("Źródło: pakiet ISLR w R (Introduction to Statistical Learning).")
    ),

    div(class = "section-title", "Podgląd danych"),

    div(class = "widget-block",
      DT::dataTableOutput("tab6_table")
    ),

    div(class = "section-title", "Eksploracja"),

    div(class = "widget-block",
      fluidRow(
        column(4, selectInput("tab6_var", "Wybierz zmienną:",
          choices = c("wage", "age", "education", "jobclass", "health", "maritl", "race"))),
        column(8, plotOutput("tab6_hist", height = "300px"))
      )
    ),

    div(class = "section-title", "Werdykt"),

    div(class = "callout-success",
      tags$strong("Bardzo dobry zbiór!"),
      " n = 3000, kompletne dane, bogaty mix zmiennych ilościowych i jakościowych.",
      tags$br(),
      "Można zapytać: czy wykształcenie przekłada się na zarobki? Czy zawód różnicuje wynagrodzenie?",
      " Czy starsi mężczyźni zarabiają więcej niż młodsi?",
      tags$br(),
      tags$em("Uwaga: dane tylko dla mężczyzn z jednego regionu USA — wyniki nie generalizują się na inne grupy.")
    ),

    div(class = "chapter-transition",
      p("Następny zbiór to przykład złej ankiety."),
      actionButton("ch6_next", "Dalej: 8. Trudna ankieta →",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  )))

ch7_server <- function(input, output, session) {

  output$tab6_table <- DT::renderDataTable({
    datatable(round_df(Wage[, c("year", "age", "maritl", "race", "education", "jobclass", "health", "wage")]),
              options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  output$tab6_hist <- renderPlot({
    req(input$tab6_var)
    var <- input$tab6_var
    if (var %in% c("wage", "age")) {
      ggplot(Wage, aes(x = .data[[var]])) +
        geom_histogram(bins = 30, fill = col_primary, color = "white", alpha = 0.8) +
        labs(title = paste("Rozkład:", var), x = var, y = "Liczebność") +
        theme_minimal(base_size = 14)
    } else {
      ggplot(Wage, aes(x = .data[[var]])) +
        geom_bar(fill = col_primary, alpha = 0.8) +
        labs(title = paste("Rozkład:", var), x = var, y = "Liczebność") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
    }
  })

}
