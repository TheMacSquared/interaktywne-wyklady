# Tab 9: Mieszkania — ceny mieszkań, outliery i błędy, zbiór mieszany

ch9_ui <- tabPanel("9. Mieszkania",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Ceny mieszkań"),

    div(class = "narrative",
      p("Dane z portalu z ogłoszeniami nieruchomości - 150 ofert skopiowanych do Excela.
        Chcemy zbadać zależność ceny od powierzchni.")
    ),

    div(class = "section-title", "Podgląd danych"),

    div(class = "widget-block",
      DT::dataTableOutput("tab8_table")
    ),

    div(class = "section-title", "Cena vs powierzchnia"),

    div(class = "widget-block",
      plotOutput("tab8_scatter_raw", height = "350px")
    ),

    div(class = "section-title", "Szukanie outlierów"),

    div(class = "widget-block",
      fluidRow(
        column(6, plotOutput("tab8_box_cena",        height = "260px")),
        column(6, plotOutput("tab8_box_powierzchnia", height = "260px"))
      ),
      fluidRow(
        column(6, plotOutput("tab8_box_pokoje",      height = "260px")),
        column(6, plotOutput("tab8_box_rok_budowy",  height = "260px"))
      )
    ),

    div(class = "widget-block",
      checkboxInput("tab8_clean", "Usuń podejrzane obserwacje", value = FALSE),
      conditionalPanel("input.tab8_clean",
        plotOutput("tab8_scatter_clean", height = "350px")
      )
    ),

    div(class = "section-title", "Quiz: błąd czy prawdziwy outlier?"),

    div(class = "widget-block",
      uiOutput("tab8_quiz"),
      actionButton("tab8_check_quiz", "Sprawdź odpowiedzi", class = "btn-primary"),
      uiOutput("tab8_quiz_result")
    ),

    div(class = "section-title", "Werdykt"),

    div(class = "callout-warning",
      tags$strong("Dane dobre po czyszczeniu!"),
      tags$br(),
      "Podstawowa struktura zbioru jest dobra (n=150, zróżnicowane zmienne, jasne definicje).",
      tags$br(),
      "Ale błędy wprowadzania danych drastycznie zaburzają wyniki (R\u00b2 skacze po ich usunięciu).",
      tags$br(),
      tags$strong("Klucz:"), " Rozróżnij błąd danych (usuń) od prawdziwego outliera (przemyśl zachowanie)."
    ),

    uiOutput("tab8_verdict"),

    div(class = "chapter-transition",
      p("Następny zbiór to przykład dobrze zaprojektowanej ankiety."),
      actionButton("ch8_next", "Dalej: 10. Ankieta studencka \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  )))

ch9_server <- function(input, output, session) {

  output$tab8_table <- DT::renderDataTable({
    datatable(round_df(apt_data), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  # Identify error rows
  error_rows <- c(3, 17, 42, 28, 55, 71)

  apt_clean <- reactive({
    if (input$tab8_clean) {
      apt_data[-error_rows, ]
    } else {
      apt_data
    }
  })

  output$tab8_scatter_raw <- renderPlot({
    model <- lm(cena ~ powierzchnia, data = apt_data)
    r2 <- round(summary(model)$r.squared, 3)
    ggplot(apt_data, aes(x = powierzchnia, y = cena)) +
      geom_point(alpha = 0.5, color = col_dark) +
      geom_smooth(method = "lm", color = col_bad, se = TRUE) +
      labs(title = paste0("Cena vs powierzchnia (R\u00b2 = ", r2, ")"),
           x = "Powierzchnia (m\u00b2)", y = "Cena (PLN)") +
      theme_minimal(base_size = 14)
  })

  make_boxplot <- function(var, label, unit = "") {
    ggplot(apt_data, aes(y = .data[[var]])) +
      geom_boxplot(fill = col_mixed, alpha = 0.7, width = 0.4) +
      labs(title = label,
           y = if (nchar(unit) > 0) paste0(label, " (", unit, ")") else label) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }

  output$tab8_box_cena        <- renderPlot({ make_boxplot("cena",        "Cena",        "PLN") })
  output$tab8_box_powierzchnia <- renderPlot({ make_boxplot("powierzchnia","Powierzchnia","m\u00b2") })
  output$tab8_box_pokoje       <- renderPlot({ make_boxplot("pokoje",      "Pokoje",      "") })
  output$tab8_box_rok_budowy   <- renderPlot({ make_boxplot("rok_budowy",  "Rok budowy",  "") })

  output$tab8_scatter_clean <- renderPlot({
    d <- apt_clean()
    model <- lm(cena ~ powierzchnia, data = d)
    r2 <- round(summary(model)$r.squared, 3)
    ggplot(d, aes(x = powierzchnia, y = cena)) +
      geom_point(alpha = 0.5, color = col_dark) +
      geom_smooth(method = "lm", color = col_good, se = TRUE) +
      labs(title = paste0("Po czyszczeniu (R\u00b2 = ", r2, ")"),
           x = "Powierzchnia (m\u00b2)", y = "Cena (PLN)") +
      theme_minimal(base_size = 14)
  })

  # Quiz
  output$tab8_quiz <- renderUI({
    tagList(
      h4("Sklasyfikuj każdą podejrzaną obserwację — błąd danych czy prawdziwy outlier?"),
      tags$p(style = "color: #7f8c8d; font-size: 13px;",
        "Uwaga: ta sama liczba może być błędem lub outlirem zależnie od kontekstu.
         Czytaj cały rekord."),

      div(style = "margin: 14px 0; padding: 10px; background: #f8f9fa; border-radius: 6px;",
        tags$strong("1."),
        " Cena: 45 PLN | Powierzchnia: 52 m\u00b2 | Pokoje: 2 | Dzielnica: Mokot\u00f3w",
        tags$br(),
        radioButtons("tab8_q1", NULL, choices = c("B\u0142\u0105d danych", "Prawdziwy outlier"), inline = TRUE)
      ),

      div(style = "margin: 14px 0; padding: 10px; background: #f8f9fa; border-radius: 6px;",
        tags$strong("2."),
        " Cena: 5 500 000 PLN | Powierzchnia: 35 m\u00b2 | Pokoje: 1 | Dzielnica: Praga-P\u00f3\u0142noc",
        tags$br(),
        radioButtons("tab8_q2", NULL, choices = c("B\u0142\u0105d danych", "Prawdziwy outlier"), inline = TRUE)
      ),

      div(style = "margin: 14px 0; padding: 10px; background: #f8f9fa; border-radius: 6px;",
        tags$strong("3."),
        " Cena: -300 000 PLN | Powierzchnia: 48 m\u00b2 | Pokoje: 2 | Dzielnica: Wola",
        tags$br(),
        radioButtons("tab8_q3", NULL, choices = c("B\u0142\u0105d danych", "Prawdziwy outlier"), inline = TRUE)
      ),

      div(style = "margin: 14px 0; padding: 10px; background: #f8f9fa; border-radius: 6px;",
        tags$strong("4."),
        " Cena: 850 000 PLN | Powierzchnia: 1 200 m\u00b2 | Pokoje: 3 | Dzielnica: Ursynów",
        tags$br(),
        radioButtons("tab8_q4", NULL, choices = c("B\u0142\u0105d danych", "Prawdziwy outlier"), inline = TRUE)
      ),

      div(style = "margin: 14px 0; padding: 10px; background: #f8f9fa; border-radius: 6px;",
        tags$strong("5."),
        " Cena: 1 150 000 PLN | Powierzchnia: 120 m\u00b2 | Pokoje: 5 | Dzielnica: \u015ar\u00f3dmie\u015bcie | Rok budowy: 2023",
        tags$br(),
        radioButtons("tab8_q5", NULL, choices = c("B\u0142\u0105d danych", "Prawdziwy outlier"), inline = TRUE)
      )
    )
  })

  output$tab8_quiz_result <- renderUI({
    req(input$tab8_check_quiz > 0)
    isolate({
      answers <- c(input$tab8_q1, input$tab8_q2, input$tab8_q3, input$tab8_q4, input$tab8_q5)
      correct <- c("B\u0142\u0105d danych", "B\u0142\u0105d danych", "B\u0142\u0105d danych", "B\u0142\u0105d danych", "Prawdziwy outlier")
      explanations <- c(
        "Cena 45 PLN za 52 m\u00b2 na Mokotowie \u2014 brakuje czterech zer. Prawdopodobnie wpisano '45' zamiast '450 000 PLN'. B\u0142\u0105d danych.",
        "5,5 mln PLN za 35 m\u00b2 na Pradze P\u00f3\u0142noc = 157 000 PLN/m\u00b2. Nierealne. Gdyby to by\u0142o 280 m\u00b2 w \u015ar\u00f3dmie\u015bciu, mog\u0142oby by\u0107 outlirem \u2014 ale ta kombinacja nie ma sensu. B\u0142\u0105d danych (prawdopodobnie jedno zero za du\u017co).",
        "Ujemna cena jest matematycznie niemo\u017cliwa. To b\u0142\u0105d znaku przy imporcie danych lub b\u0142\u0105d oper. B\u0142\u0105d danych.",
        "1200 m\u00b2 to wielko\u015b\u0107 biurowca, nie mieszkania. Przy 3 pokojach i cenie 850 000 PLN prawie na pewno wpisano '1200' zamiast '120 m\u00b2'. B\u0142\u0105d danych.",
        "9 600 PLN/m\u00b2 za nowe, du\u017ce mieszkanie w \u015ar\u00f3dmie\u015bciu (rok budowy 2023) \u2014 drogo, ale taki rynek istnieje. To prawdziwy outlier: warto odnotowa\u0107, ale nie usuwa\u0107."
      )

      items <- sapply(1:5, function(i) {
        ok <- answers[i] == correct[i]
        icon <- if (ok) "\u2705" else "\u274c"
        paste0("<div style='padding: 6px 0; border-bottom: 1px solid #eee;'>",
               icon, " <b>Pyt. ", i, ":</b> ", explanations[i], "</div>")
      })

      score <- sum(answers == correct)
      div(class = if (score >= 4) "callout-success" else "callout-warning", style = "margin-top: 15px;",
        tags$strong(paste0("Wynik: ", score, "/5")),
        HTML(paste(items, collapse = ""))
      )
    })
  })

  output$tab8_verdict <- renderUI({
    # hipoteza, n, mix, zmiennosc, struktura, niezaleznosc | braki, definicje, bledy
    # Mieszkania: wszystko krytyczne ok, ale bledy w danych (naprawialny problem)
    render_verdict(c("yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "no"), "mixed")
  })
}
