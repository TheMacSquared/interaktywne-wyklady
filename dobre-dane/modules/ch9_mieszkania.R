# Tab 9: Badania laboratoryjne — błędy danych, zbiór mieszany

ch9_ui <- lecture_chapter(id = "ch9", num = "9", title = "Laboratorium", content = tagList(
  fluidRow(column(8, offset = 2,

    lc_h2("sec-01", "Badania laboratoryjne"),

    div(class = "lc-prose",
      p("Wyniki badań laboratoryjnych 150 pacjentów — dane przepisane ręcznie z kart do Excela.
        Chcemy zbadać zależność hemoglobiny od wieku.")
    ),

    lc_h2("sec-02", "Podgląd danych"),

    div(class = "lc-figure-panel",
      DT::dataTableOutput("tab8_table")
    ),

    lc_h2("sec-03", "Wiek a hemoglobina"),

    div(class = "lc-figure-panel",
      plotOutput("tab8_scatter_raw", height = "350px")
    ),

    lc_h2("sec-04", "Szukanie outlierów"),

    div(class = "lc-figure-panel",
      fluidRow(
        column(6, plotOutput("tab8_box_hemoglobina", height = "260px")),
        column(6, plotOutput("tab8_box_glukoza",     height = "260px"))
      ),
      fluidRow(
        column(6, plotOutput("tab8_box_wiek",        height = "260px")),
        column(6, plotOutput("tab8_box_cisnienie",   height = "260px"))
      )
    ),

    div(class = "lc-figure-panel",
      checkboxInput("tab8_clean", "Usuń podejrzane obserwacje", value = FALSE),
      conditionalPanel("input.tab8_clean",
        plotOutput("tab8_scatter_clean", height = "350px")
      )
    ),

    lc_h2("sec-05", "Quiz: błąd czy prawdziwy outlier?"),

    div(class = "lc-figure-panel",
      uiOutput("tab8_quiz"),
      actionButton("tab8_check_quiz", "Sprawdź odpowiedzi", class = "lc-btn-primary"),
      uiOutput("tab8_quiz_result")
    ),

    lc_h2("sec-06", "Werdykt"),

    div(class = "lc-feedback lc-feedback-warning",
      tags$strong("Dane dobre po czyszczeniu!"),
      tags$br(),
      "Podstawowa struktura zbioru jest dobra (n=150, zróżnicowane zmienne, jasne definicje).",
      tags$br(),
      "Ale błędy ręcznego przepisywania drastycznie zaburzają wyniki (R² skacze po ich usunięciu).",
      tags$br(),
      tags$strong("Klucz:"), " Rozróżnij błąd danych (usuń) od prawdziwego outliera (przemyśl zachowanie)."
    ),

    div(class = "chapter-transition",
      p("Następny zbiór to przykład dobrze zaprojektowanej ankiety."),
      actionButton("ch8_next", "Dalej: 10. Ankieta studencka →",
                   class = "lc-btn-primary lc-btn-lg")
    ),

    div(style = "height: 40px;")
  ))))

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
      geom_point(alpha = 0.5, color = data_reference) +
      geom_smooth(method = "lm", color = data_bad, se = TRUE) +
      labs(title = paste0("Wiek vs hemoglobina (R² = ", r2, ")"),
           x = "Wiek (lata)", y = "Hemoglobina (g/dL)") +
      theme_upwr(base_size = 14)
  })

  make_boxplot <- function(var, label, unit = "") {
    ggplot(lab_data, aes(y = .data[[var]])) +
      geom_boxplot(fill = data_mixed, alpha = 0.7, width = 0.4) +
      labs(title = label,
           y = if (nchar(unit) > 0) paste0(label, " (", unit, ")") else label) +
      theme_upwr(base_size = 13) +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }

  output$tab8_box_hemoglobina <- renderPlot({ make_boxplot("hemoglobina", "Hemoglobina", "g/dL") })
  output$tab8_box_glukoza     <- renderPlot({ make_boxplot("glukoza",     "Glukoza",     "mg/dL") })
  output$tab8_box_wiek        <- renderPlot({ make_boxplot("wiek",        "Wiek",        "lata") })
  output$tab8_box_cisnienie   <- renderPlot({ make_boxplot("cisnienie",   "Ciśnienie skurczowe", "mmHg") })

  output$tab8_scatter_clean <- renderPlot({
    d     <- lab_clean()
    model <- lm(hemoglobina ~ wiek, data = d)
    r2    <- round(summary(model)$r.squared, 3)
    ggplot(d, aes(x = wiek, y = hemoglobina)) +
      geom_point(alpha = 0.5, color = data_reference) +
      geom_smooth(method = "lm", color = data_good, se = TRUE) +
      labs(title = paste0("Po czyszczeniu (R² = ", r2, ")"),
           x = "Wiek (lata)", y = "Hemoglobina (g/dL)") +
      theme_upwr(base_size = 14)
  })

  output$tab8_quiz <- renderUI({
    tagList(
      h4("Sklasyfikuj każdą podejrzaną obserwację — błąd danych czy prawdziwy outlier?"),
      tags$p(style = "color: var(--upwr-reference); font-size: 13px;",
        "Ta sama wartość może być błędem lub outlirem zależnie od kontekstu. Czytaj cały rekord."),

      div(style = "margin: 14px 0; padding: 10px; background: var(--upwr-panel); border-radius: 6px;",
        tags$strong("1."),
        paste0(" Hemoglobina: -14.2 g/dL | Wiek: ", lab_data$wiek[3], " lat | Płeć: ", lab_data$plec[3]),
        tags$br(),
        radioButtons("tab8_q1", NULL, choices = c("Błąd danych", "Prawdziwy outlier"), inline = TRUE)
      ),

      div(style = "margin: 14px 0; padding: 10px; background: var(--upwr-panel); border-radius: 6px;",
        tags$strong("2."),
        paste0(" Hemoglobina: 1420 g/dL | Wiek: ", lab_data$wiek[17], " lat | Płeć: ", lab_data$plec[17]),
        tags$br(),
        radioButtons("tab8_q2", NULL, choices = c("Błąd danych", "Prawdziwy outlier"), inline = TRUE)
      ),

      div(style = "margin: 14px 0; padding: 10px; background: var(--upwr-panel); border-radius: 6px;",
        tags$strong("3."),
        paste0(" Ciśnienie skurczowe: -70 mmHg | Wiek: ", lab_data$wiek[42], " lat | Płeć: ", lab_data$plec[42]),
        tags$br(),
        radioButtons("tab8_q3", NULL, choices = c("Błąd danych", "Prawdziwy outlier"), inline = TRUE)
      ),

      div(style = "margin: 14px 0; padding: 10px; background: var(--upwr-panel); border-radius: 6px;",
        tags$strong("4."),
        paste0(" Glukoza: 11 000 mg/dL | Wiek: ", lab_data$wiek[28], " lat | Płeć: ", lab_data$plec[28]),
        tags$br(),
        radioButtons("tab8_q4", NULL, choices = c("Błąd danych", "Prawdziwy outlier"), inline = TRUE)
      ),

      div(style = "margin: 14px 0; padding: 10px; background: var(--upwr-panel); border-radius: 6px;",
        tags$strong("5."),
        paste0(" Glukoza: 310 mg/dL | Wiek: ", lab_data$wiek[100], " lat | Płeć: ", lab_data$plec[100],
               " | Hemoglobina: ", lab_data$hemoglobina[100], " g/dL"),
        tags$br(),
        radioButtons("tab8_q5", NULL, choices = c("Błąd danych", "Prawdziwy outlier"), inline = TRUE)
      )
    )
  })

  output$tab8_quiz_result <- renderUI({
    req(input$tab8_check_quiz > 0)
    isolate({
      answers <- c(input$tab8_q1, input$tab8_q2, input$tab8_q3, input$tab8_q4, input$tab8_q5)
      correct <- c("Błąd danych", "Błąd danych", "Błąd danych",
                   "Błąd danych", "Prawdziwy outlier")
      explanations <- c(
        "Ujemna hemoglobina jest fizycznie niemożliwa. Prawdopodobnie minus pojawił się przez błąd klawiatury lub importu. Błąd danych.",
        "Hemoglobina 1420 g/dL — norma to 12–17 g/dL. Brakuje przecinka dziesiętnego: powinno być 14,20. Błąd danych (błąd zapisu).",
        "Ujemne ciśnienie tętnicze jest niemożliwe fizjologicznie. Znak minus musiał pojawić się przez błąd wprowadzania. Błąd danych.",
        "Glukoza 11 000 mg/dL — norma to 70–110 mg/dL, a nawet w śpiączce cukrzycowej nie przekracza ~1000. Powinno być 110 mg/dL (3 zera za dużo). Błąd danych.",
        "Glukoza 310 mg/dL jest wysoka, ale medycznie możliwa — taki poziom zdarza się u pacjentów z niekontrolowaną cukrzycą. Reszta parametrów wygląda spójnie. Prawdziwy outlier: warto odnotować, ale nie usuwać."
      )

      items <- sapply(1:5, function(i) {
        ok   <- answers[i] == correct[i]
        icon <- if (ok) "✅" else "❌"
        paste0("<div style='padding: 6px 0; border-bottom: 1px solid var(--upwr-rule);'>",
               icon, " <b>Pyt. ", i, ":</b> ", explanations[i], "</div>")
      })

      score <- sum(answers == correct)
      div(class = if (score >= 4) "lc-feedback lc-feedback-ok" else "lc-feedback lc-feedback-warning", style = "margin-top: 15px;",
        tags$strong(paste0("Wynik: ", score, "/5")),
        HTML(paste(items, collapse = ""))
      )
    })
  })

}
