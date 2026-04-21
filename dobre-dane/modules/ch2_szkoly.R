# Tab 2: Szkoły — CASchools (AER), dobry zbiór wzorcowy

ch2_ui <- tabPanel("2. Szkoły",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Szkoły w Kalifornii"),

    div(class = "narrative",
      p("Zbiór danych z 420 okręgów szkolnych w Kalifornii. Zawiera wyniki testów
        standaryzowanych, wydatki na ucznia, dochody w okręgu i dane demograficzne."),
      p("Źródło: pakiet AER w R (Academic Economic Research).")
    ),

    div(class = "section-title", "Podgląd danych"),

    div(class = "widget-block",
      DT::dataTableOutput("tab1_table")
    ),

    div(class = "callout-info",
      tags$strong("Zmienne:"), " district, school (identyfikatory), ",
      "students, teachers (liczebności), expenditure (wydatki/ucznia $), ",
      "income (średni dochód w okręgu $tys.), english (% uczniów uczących się angielskiego), ",
      "lunch (% uczniów z darmowym lunchem), calworks (% rodzin na zasiłku), ",
      "read, math (wyniki testów Stanford 9)."
    ),

    div(class = "section-title", "Eksploracja zmiennych"),

    div(class = "widget-block",
      fluidRow(
        column(4, selectInput("tab1_var", "Wybierz zmienną:",
          choices = c("read", "math", "expenditure", "income", "english", "lunch",
                      "students", "teachers", "calworks"))),
        column(8, plotOutput("tab1_hist", height = "300px"))
      ),
      verbatimTextOutput("tab1_summary")
    ),

    div(class = "section-title", "Zależności między zmiennymi"),

    div(class = "widget-block",
      fluidRow(
        column(4, selectInput("tab1_x", "Zmienna X:", choices = c("expenditure", "income", "english", "lunch", "calworks", "students"), selected = "income")),
        column(4, selectInput("tab1_y", "Zmienna Y:", choices = c("read", "math"), selected = "read"))
      ),
      plotOutput("tab1_scatter_plot", height = "350px")
    ),

    div(class = "section-title", "Werdykt"),

    div(class = "callout-success",
      tags$strong("Bardzo dobry zbiór!"),
      " 420 okręgów szkolnych — wystarczy do każdej analizy.",
      tags$br(),
      "Można zapytać: czy wyższe wydatki na ucznia przekładają się na lepsze wyniki testów?",
      " Czy ubóstwo w okręgu koreluje z wynikami? Czy są różnice między okręgami?",
      tags$br(),
      tags$em("Uwaga: to dane dla okręgów szkolnych USA — wyniki nie dotyczą Polski.")
    ),

    div(class = "chapter-transition",
      p("To był wzorcowy zbiór. Następny będzie... inny."),
      actionButton("ch1_next", "Dalej: 3. Ankieta na grupie →",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  )))

ch2_server <- function(input, output, session) {

  output$tab1_table <- DT::renderDataTable({
    datatable(round_df(CASchools[, c("district", "school", "students", "teachers", "expenditure",
                            "income", "english", "lunch", "calworks", "read", "math")]),
              options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  output$tab1_hist <- renderPlot({
    req(input$tab1_var)
    ggplot(CASchools, aes(x = .data[[input$tab1_var]])) +
      geom_histogram(bins = 25, fill = col_primary, color = "white", alpha = 0.8) +
      labs(title = paste("Rozkład:", input$tab1_var), x = input$tab1_var, y = "Liczebność") +
      theme_minimal(base_size = 14)
  })

  output$tab1_summary <- renderPrint({
    req(input$tab1_var)
    summary(CASchools[[input$tab1_var]])
  })

  output$tab1_scatter_plot <- renderPlot({
    ggplot(CASchools, aes(x = .data[[input$tab1_x]], y = .data[[input$tab1_y]])) +
      geom_point(alpha = 0.5, color = col_dark) +
      geom_smooth(method = "lm", color = col_primary, se = TRUE) +
      labs(title = paste(input$tab1_y, "~", input$tab1_x),
           x = input$tab1_x, y = input$tab1_y) +
      theme_minimal(base_size = 14)
  })

}
