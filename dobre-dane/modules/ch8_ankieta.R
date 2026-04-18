# Tab 8: Ankieta — trudna ankieta, źle zdefiniowane zmienne

ch8_ui <- tabPanel("8. Ankieta",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Trudna ankieta"),

    div(class = "narrative",
      p("Student zaprojektował ankietę bez konsultacji z prowadzącym i bez pilotażu.
        Rozesłał ją na grupie i zebrała 90 odpowiedzi. Oto wynik:")
    ),

    div(class = "section-title", "Podgląd danych"),

    div(class = "widget-block",
      DT::dataTableOutput("tab7_table")
    ),

    div(class = "section-title", "Spróbuj policzyć średnią"),

    div(class = "widget-block",
      selectInput("tab7_var", "Wybierz zmienną:",
        choices = c("czas_na_studia", "ocena_kursu", "aktywnosc", "samopoczucie", "ulubiony_kolor")),
      actionButton("tab7_mean", "Policz średnią", class = "btn-primary"),
      uiOutput("tab7_mean_result")
    ),

    div(class = "section-title", "Jak to naprawić?"),

    div(class = "widget-block",
      radioButtons("tab7_toggle", "Widok danych:", choices = c("Surowe", "Oczyszczone"), inline = TRUE),
      DT::dataTableOutput("tab7_clean_table"),
      uiOutput("tab7_clean_info")
    ),

    div(class = "callout-info",
      tags$strong("Jak tego uniknąć:"),
      tags$br(),
      "1. Zamknięte pytania (gotowe opcje do wyboru)",
      tags$br(),
      "2. Spójne skale (np. zawsze 1-10 albo zawsze 1-5)",
      tags$br(),
      "3. Pilotaż ankiety (przetestuj na 5 osobach przed rozesłaniem)",
      tags$br(),
      "4. Jasna instrukcja (np. 'podaj liczbę godzin tygodniowo')"
    ),

    div(class = "section-title", "Werdykt"),

    div(class = "callout-danger",
      "Dane wymagają gruntownego czyszczenia!",
      tags$br(),
      "Zmienne tekstowe zamiast liczbowych, niespójne skale, brak kodowania.",
      tags$br(),
      "Zmienna 'ulubiony_kolor' jest nieistotna - nie wiąże się z żadnym pytaniem badawczym.",
      tags$br(),
      "R nie wie, co zrobić z '3-4h' albo 'dobrze' jako wartością liczbową."
    ),

    uiOutput("tab7_verdict"),

    div(class = "section-title", "Drugi przykład: dane do uratowania"),

    div(class = "narrative",
      p("Inna ankieta o nawykach studenckich, podobny problem \u2014 respondenci odpowiadali
        różnie na te same pytania. Ale tym razem prawie każdą odpowiedź można przypisać
        do kategorii. Porównaj surowe dane z wersją po kategoryzacji.")
    ),

    div(class = "toggle-pills",
      actionButton("tab7b_raw", "Surowe", class = "pill-btn active"),
      actionButton("tab7b_cat", "Po kategoryzacji", class = "pill-btn")
    ),

    div(class = "widget-block",
      DT::dataTableOutput("tab7b_table")
    ),

    div(class = "callout-success",
      "10 z 12 wierszy można uratować (83%).",
      tags$br(),
      tags$b("rok_studiow:"), " \"pierwszy\", \"I rok\", \"1\" \u2192 wszystkie to rok 1.",
      tags$br(),
      tags$b("tryb:"), " \"s\", \"S\", \"zaoczny\" \u2192 \"stacjonarny\" lub \"niestacjonarny\".",
      tags$br(),
      tags$b("godziny_nauki:"), " \"ok. 5\", \"4-6h\", \"5h\" \u2192 kategoria \"srednie (4-6h)\".
        Straty: \"duzo\" i \"malo\" \u2014 za mało informacji żeby przypisać do kategorii."
    ),

    div(class = "chapter-transition",
      p("Następny zbiór ma inny rodzaj problemów - błędy w danych."),
      actionButton("ch7_next", "Dalej: 9. Ceny mieszkań \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  )))

ch8_server <- function(input, output, session) {

  output$tab7_table <- DT::renderDataTable({
    datatable(round_df(messy_data), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$tab7_mean_result <- renderUI({
    req(input$tab7_mean > 0)
    isolate({
      var <- input$tab7_var
      vals <- messy_data[[var]]

      if (var == "samopoczucie") {
        # This one is numeric already
        div(class = "callout-info", style = "margin-top: 10px;",
          paste0("Średnia samopoczucia: ", round(mean(vals), 1),
                 " (ale uwaga: wszyscy zaokrąglają do 10 - to nie jest prawdziwa skala ciągła)")
        )
      } else if (var == "ulubiony_kolor") {
        div(class = "callout-warning", style = "margin-top: 10px;",
          "Ulubiony kolor to zmienna nominalna - średnia nie ma sensu. ",
          "A poza tym: jak ta zmienna wiąże się z Twoim pytaniem badawczym?"
        )
      } else {
        nums <- safe_numeric(vals)
        n_na <- sum(is.na(nums))
        pct_na <- round(n_na / length(nums) * 100, 1)

        if (n_na == 0) {
          div(class = "callout-info", style = "margin-top: 10px;",
            paste0("Średnia: ", round(mean(nums, na.rm = TRUE), 2))
          )
        } else {
          div(class = "callout-danger", style = "margin-top: 10px;",
            paste0(n_na, " z ", length(nums), " wartości (", pct_na, "%) nie dało się przekonwertować na liczby!"),
            tags$br(),
            "Przykłady problematycznych wartości: ",
            paste(head(vals[is.na(nums)], 5), collapse = ", "),
            tags$br(), tags$br(),
            "R nie wie, co zrobić z tekstem jak '3-4h' albo 'dobrze'."
          )
        }
      }
    })
  })

  output$tab7_clean_table <- DT::renderDataTable({
    if (input$tab7_toggle == "Surowe") {
      datatable(round_df(messy_data), options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
    } else {
      # Cleaned version
      clean <- data.frame(
        czas_na_studia_h = c(NA, 3.5, 5, NA, NA, 2, NA, 3, 6, NA)[
          match(messy_data$czas_na_studia,
                c("duzo", "3-4h", "5", "caly dzien", "malo", "ok. 2 godziny",
                  "nie wiem", "3", "6h dziennie", "weekendy"))],
        ocena_kursu_1_10 = c(8, NA, 4, NA, 7.5, NA, 9, NA, 6, NA, 10, NA)[
          match(messy_data$ocena_kursu,
                c("8/10", "dobrze", "4", "B+", "7.5", "srednia", "9", "bardzo dobrze",
                  "6/10", "slabo", "10", "ok"))],
        aktywnosc_razy_tyg = c(NA, 0, NA, 3, NA, 7, 2, NA)[
          match(messy_data$aktywnosc,
                c("tak", "nie", "czasami", "3 razy w tygodniu", "rzadko",
                  "codziennie", "2x", "nie wiem"))],
        samopoczucie_1_10 = round(messy_data$samopoczucie / 10),
        stringsAsFactors = FALSE
      )
      datatable(round_df(clean), options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
    }
  })

  output$tab7_clean_info <- renderUI({
    if (input$tab7_toggle == "Oczyszczone") {
      div(class = "callout-info", style = "margin-top: 10px;",
        tags$strong("Zmiany:"),
        tags$br(), "- czas_na_studia: zamieniono na godziny (ale dużo wartości to NA - niejednoznaczne odpowiedzi)",
        tags$br(), "- ocena_kursu: ujednolicono do skali 1-10 (tekst -> NA)",
        tags$br(), "- aktywnosc: zamieniono na razy/tydzień (dużo NA)",
        tags$br(), "- samopoczucie: przeskalowano 1-100 -> 1-10",
        tags$br(), "- ulubiony_kolor: USUNIĘTO (nieistotna zmienna)",
        tags$br(), tags$br(),
        tags$em("Wniosek: czyszczenie jest możliwe, ale tracimy dużo danych. Lepiej zaprojektować ankietę poprawnie od początku.")
      )
    }
  })

  output$tab7_verdict <- renderUI({
    # hipoteza, n, mix, zmiennosc, struktura, niezaleznosc | braki, definicje, bledy
    # Trudna ankieta: hipoteza ok, n ok, brak mix (bo nic nie jest liczbowe), zmiennosc ok, struktura nie, niezaleznosc ok | braki ok, definicje NO, bledy ok
    render_verdict(c("yes", "yes", "no", "yes", "no", "yes", "yes", "no", "yes"), "bad")
  })

  tab7b_view <- reactiveVal("raw")
  observeEvent(input$tab7b_raw, {
    tab7b_view("raw")
    session$sendCustomMessage(type = "shinyjs-runjs", message = list(code =
      "$('#tab7b_raw').addClass('active'); $('#tab7b_cat').removeClass('active');"))
  })
  observeEvent(input$tab7b_cat, {
    tab7b_view("cat")
    session$sendCustomMessage(type = "shinyjs-runjs", message = list(code =
      "$('#tab7b_cat').addClass('active'); $('#tab7b_raw').removeClass('active');"))
  })

  output$tab7b_table <- DT::renderDataTable({
    if (tab7b_view() == "raw") {
      datatable(fixable_data,
                options = list(dom = 't', ordering = FALSE, pageLength = 12),
                rownames = FALSE)
    } else {
      datatable(fixable_data_cat,
                options = list(dom = 't', ordering = FALSE, pageLength = 12),
                rownames = FALSE) %>%
        DT::formatStyle("nauka_kat",
          backgroundColor = DT::styleEqual(NA, "#fdedec"),
          target = "cell")
    }
  })
}
