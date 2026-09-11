# Tab 7: Wynagrodzenia — Wage (ISLR), dobry zbiór

ch7_ui <- lecture_chapter(id = "ch7", num = "7", title = "Wynagrodzenia", content = tagList(
  fluidRow(column(8, offset = 2,

    lc_chapter_hero(
      kicker = "Rozdział 07 · Co czyni dobry zbiór danych?",
      num    = "07",
      title  = "Wynagrodzenia w USA.",
      lead   = "Duży, kompletny zbiór z mieszanką zmiennych pokazuje,
                jak wygląda materiał gotowy do wielu typów analiz."
    ),

    lc_h2("sec-01", "Wynagrodzenia w USA"),

    div(class = "lc-prose",
      p("Dane z Current Population Survey: 3000 mężczyzn z regionu Mid-Atlantic.
        Informacje o zarobkach, wykształceniu, zawodzie, wieku i zdrowiu."),
      p("Źródło: pakiet ISLR w R (Introduction to Statistical Learning).")
    ),

    lc_h2("sec-02", "Podgląd danych"),

    div(class = "lc-figure-panel",
      DT::dataTableOutput("tab6_table")
    ),

    lc_h2("sec-03", "Eksploracja"),

    div(class = "lc-figure-panel",
      fluidRow(
        column(4, selectInput("tab6_var", "Wybierz zmienną:",
          choices = c("wage", "age", "education", "jobclass", "health", "maritl", "race"))),
        column(8, zoom_plot_ui("tab6_hist", height = "300px"))
      )
    ),

    lc_h2("sec-04", "Werdykt"),

    div(class = "lc-feedback lc-feedback-ok",
      tags$strong("Bardzo dobry zbiór!"),
      " n = 3000, kompletne dane, bogaty mix zmiennych ilościowych i jakościowych.",
      tags$br(),
      "Można zapytać: czy wykształcenie przekłada się na zarobki? Czy zawód różnicuje wynagrodzenie?",
      " Czy starsi mężczyźni zarabiają więcej niż młodsi?",
      tags$br(),
      tags$em("Uwaga: dane tylko dla mężczyzn z jednego regionu USA — wyniki nie generalizują się na inne grupy.")
    ),

    lc_chapter_next(
      num = "08",
      title = "Trudna ankieta",
      lead = "Następny zbiór to przykład złej ankiety.",
      target_id = "ch8"
    ),

    div(style = "height: 40px;")
  ))))

ch7_server <- function(input, output, session) {

  output$tab6_table <- DT::renderDataTable({
    datatable(round_df(Wage[, c("year", "age", "maritl", "race", "education", "jobclass", "health", "wage")]),
              options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  zoom_plot_server("tab6_hist", reactive({
    req(input$tab6_var)
    var <- input$tab6_var
    if (var %in% c("wage", "age")) {
      ggplot(Wage, aes(x = .data[[var]])) +
        geom_histogram(bins = 30, fill = data_primary, color = "white", alpha = 0.8) +
        labs( x = var, y = "Liczebność") +
        theme_upwr(base_size = 14)
    } else {
      ggplot(Wage, aes(x = .data[[var]])) +
        geom_bar(fill = data_primary, alpha = 0.8) +
        labs( x = var, y = "Liczebność") +
        theme_upwr(base_size = 14) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
    }
  }))

}
