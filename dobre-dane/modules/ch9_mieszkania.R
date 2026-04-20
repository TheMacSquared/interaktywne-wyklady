# Tab 9: Badania laboratoryjne — błędy danych, zbiór mieszany

ch9_ui <- tabPanel("9. Laboratorium",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Badania laboratoryjne"),

    div(class = "narrative",
      p("Wyniki badań laboratoryjnych 150 pacjentów \u2014 dane przepisane ręcznie z kart do Excela.
        Chcemy zbadać zależność hemoglobiny od wieku.")
    ),

    div(class = "section-title", "Podgląd danych"),

    div(class = "widget-block",
      DT::dataTableOutput("tab8_table")
    ),

    div(class = "section-title", "Wiek a hemoglobina"),

    div(class = "widget-block",
      plotOutput("tab8_scatter_raw", height = "350px")
    ),

    div(class = "section-title", "Szukanie outlierów"),

    div(class = "widget-block",
      fluidRow(
        column(6, plotOutput("tab8_box_hemoglobina", height = "260px")),
        column(6, plotOutput("tab8_box_glukoza",     height = "260px"))
      ),
      fluidRow(
        column(6, plotOutput("tab8_box_wiek",        height = "260px")),
        column(6, plotOutput("tab8_box_cisnienie",   height = "260px"))
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
      "Ale błędy ręcznego przepisywania drastycznie zaburzają wyniki (R\u00b2 skacze po ich usunięciu).",
      tags$br(),
      tags$strong("Klucz:"), " Rozróżnij błąd danych (usuń) od prawdziwego outliera (przemyśl zachowanie)."
    ),

    div(class = "chapter-transition",
      p("Następny zbiór to przykład dobrze zaprojektowanej ankiety."),
      actionButton("ch8_next", "Dalej: 10. Ankieta studencka \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  )))

ch9_server <- function(input, output, session) {

  output$tab8_table <- DT::renderDataTable({
    datatable(round_df(lab_data), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  error_rows <- c(3, 17, 28, 42, 55, 71)

  lab_clean <- reactive({
    if (input$tab8_clean) lab_data[-error_rows, ] else lab_data
  })

  output$tab8_scatter_raw <- renderPlot({
    model <- lm(hemoglobina ~ wiek, data = lab_data)
    r2    <- round(summary(model)$r.squared, 3)
    ggplot(lab_data, aes(x = wiek, y = hemoglobina)) +
      geom_point(alpha = 0.5, color = col_dark) +
      geom_smooth(method = "lm", color = col_bad, se = TRUE) +
      labs(title = paste0("Wiek vs hemoglobina (R\u00b2 = ", r2, ")"),
           x = "Wiek (lata)", y = "Hemoglobina (g/dL)") +
      theme_minimal(base_size = 14)
  })

  make_boxplot <- function(var, label, unit = "") {
    ggplot(lab_data, aes(y = .data[[var]])) +
      geom_boxplot(fill = col_mixed, alpha = 0.7, width = 0.4) +
      labs(title = label,
           y = if (nchar(unit) > 0) paste0(label, " (", unit, ")") else label) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }

  output$tab8_box_hemoglobina <- renderPlot({ make_boxplot("hemoglobina", "Hemoglobina", "g/dL") })
  output$tab8_box_glukoza     <- renderPlot({ make_boxplot("glukoza",     "Glukoza",     "mg/dL") })
  output$tab8_box_wiek        <- renderPlot({ make_boxplot("wiek",        "Wiek",        "lata") })
  output$tab8_box_cisnienie   <- renderPlot({ make_boxplot("cisnienie",   "Ci\u015bnienie skurczowe", "mmHg") })

  output$tab8_scatter_clean <- renderPlot({
    d     <- lab_clean()
    model <- lm(hemoglobina ~ wiek, data = d)
    r2    <- round(summary(model)$r.squared, 3)
    ggplot(d, aes(x = wiek, y = hemoglobina)) +
      geom_point(alpha = 0.5, color = col_dark) +
      geom_smooth(method = "lm", color = col_good, se = TRUE) +
      labs(title = paste0("Po czyszczeniu (R\u00b2 = ", r2, ")"),
           x = "Wiek (lata)", y = "Hemoglobina (g/dL)") +
      theme_minimal(base_size = 14)
  })

  output$tab8_quiz <- renderUI({
    tagList(
      h4("Sklasyfikuj każdą podejrzaną obserwację \u2014 błąd danych czy prawdziwy outlier?"),
      tags$p(style = "color: #7f8c8d; font-size: 13px;",
        "Ta sama wartość może być błędem lub outlirem zależnie od kontekstu. Czytaj cały rekord."),

      div(style = "margin: 14px 0; padding: 10px; background: #f8f9fa; border-radius: 6px;",
        tags$strong("1."),
        paste0(" Hemoglobina: -14.2 g/dL | Wiek: ", lab_data$wiek[3], " lat | P\u0142e\u0107: ", lab_data$plec[3]),
        tags$br(),
        radioButtons("tab8_q1", NULL, choices = c("B\u0142\u0105d danych", "Prawdziwy outlier"), inline = TRUE)
      ),

      div(style = "margin: 14px 0; padding: 10px; background: #f8f9fa; border-radius: 6px;",
        tags$strong("2."),
        paste0(" Hemoglobina: 1420 g/dL | Wiek: ", lab_data$wiek[17], " lat | P\u0142e\u0107: ", lab_data$plec[17]),
        tags$br(),
        radioButtons("tab8_q2", NULL, choices = c("B\u0142\u0105d danych", "Prawdziwy outlier"), inline = TRUE)
      ),

      div(style = "margin: 14px 0; padding: 10px; background: #f8f9fa; border-radius: 6px;",
        tags$strong("3."),
        paste0(" Ci\u015bnienie skurczowe: -70 mmHg | Wiek: ", lab_data$wiek[42], " lat | P\u0142e\u0107: ", lab_data$plec[42]),
        tags$br(),
        radioButtons("tab8_q3", NULL, choices = c("B\u0142\u0105d danych", "Prawdziwy outlier"), inline = TRUE)
      ),

      div(style = "margin: 14px 0; padding: 10px; background: #f8f9fa; border-radius: 6px;",
        tags$strong("4."),
        paste0(" Glukoza: 11 000 mg/dL | Wiek: ", lab_data$wiek[28], " lat | P\u0142e\u0107: ", lab_data$plec[28]),
        tags$br(),
        radioButtons("tab8_q4", NULL, choices = c("B\u0142\u0105d danych", "Prawdziwy outlier"), inline = TRUE)
      ),

      div(style = "margin: 14px 0; padding: 10px; background: #f8f9fa; border-radius: 6px;",
        tags$strong("5."),
        paste0(" Glukoza: 310 mg/dL | Wiek: ", lab_data$wiek[100], " lat | P\u0142e\u0107: ", lab_data$plec[100],
               " | Hemoglobina: ", lab_data$hemoglobina[100], " g/dL"),
        tags$br(),
        radioButtons("tab8_q5", NULL, choices = c("B\u0142\u0105d danych", "Prawdziwy outlier"), inline = TRUE)
      )
    )
  })

  output$tab8_quiz_result <- renderUI({
    req(input$tab8_check_quiz > 0)
    isolate({
      answers <- c(input$tab8_q1, input$tab8_q2, input$tab8_q3, input$tab8_q4, input$tab8_q5)
      correct <- c("B\u0142\u0105d danych", "B\u0142\u0105d danych", "B\u0142\u0105d danych",
                   "B\u0142\u0105d danych", "Prawdziwy outlier")
      explanations <- c(
        "Ujemna hemoglobina jest fizycznie niemożliwa. Prawdopodobnie minus pojawił się przez błąd klawiatury lub importu. Błąd danych.",
        "Hemoglobina 1420 g/dL \u2014 norma to 12\u201317 g/dL. Brakuje przecinka dziesiętnego: powinno być 14,20. Błąd danych (błąd zapisu).",
        "Ujemne ciśnienie tętnicze jest niemożliwe fizjologicznie. Znak minus musiał pojawić się przez błąd wprowadzania. Błąd danych.",
        "Glukoza 11 000 mg/dL \u2014 norma to 70\u2013110 mg/dL, a nawet w śpiączce cukrzycowej nie przekracza ~1000. Powinno być 110 mg/dL (3 zera za dużo). Błąd danych.",
        "Glukoza 310 mg/dL jest wysoka, ale medycznie możliwa \u2014 taki poziom zdarza się u pacjentów z niekontrolowaną cukrzycą. Reszta parametrów wygląda spójnie. Prawdziwy outlier: warto odnotować, ale nie usuwać."
      )

      items <- sapply(1:5, function(i) {
        ok   <- answers[i] == correct[i]
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

}
