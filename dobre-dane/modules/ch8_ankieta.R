# Tab 8: Formularz rejestracyjny — mix dobrych i złych zmiennych

ch8_ui <- tabPanel("8. Formularz",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Formularz rejestracyjny kursu"),

    div(class = "narrative",
      p("Organizatorzy kursu wakacyjnego zebrali zapisy przez formularz online.
        Nie wszystkie pola były dobrze przemyślane. Zebrano 90 zgłoszeń.")
    ),

    div(class = "section-title", "Podgląd danych"),

    div(class = "widget-block",
      DT::dataTableOutput("tab7_table")
    ),

    div(class = "section-title", "Spróbuj policzyć średnią"),

    div(class = "widget-block",
      selectInput("tab7_var", "Wybierz zmienną:",
        choices = c("wiek", "wyksztalcenie", "doswiadczenie", "dostepnosc", "ocena_umiejetnosci")),
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
      "1. Zamknięte pytania dla zmiennych kluczowych (gotowe opcje do wyboru)",
      tags$br(),
      "2. Rozróżnij zmienne analizowane od informacyjnych (wiek → liczba, nie tekst)",
      tags$br(),
      "3. Pilotaż formularza (przetestuj na 5 osobach przed uruchomieniem)",
      tags$br(),
      "4. Jasna instrukcja (np. 'podaj lata doświadczenia jako liczbę')"
    ),

    div(class = "section-title", "Werdykt"),

    div(class = "callout-warning",
      "Dane częściowo nadają się do analizy.",
      tags$br(),
      tags$strong("Dobre zmienne:"), " wiek (liczbowy, czysty) i wykształcenie (kategoryczne, spójne).",
      tags$br(),
      tags$strong("Złe zmienne:"), " doświadczenie i dostępność — tekst bez struktury, nieprzeliczalny.",
      tags$br(),
      "ocena_umiejętności \u2014 mieszanka liczb i tekstu, ~50% można uratować.",
      tags$br(),
      "R nie wie, co zrobić z 'trochę' albo 'elastycznie' jako wartością liczbową."
    ),

    div(class = "section-title", "Drugi przykład: dane do uratowania"),

    div(class = "narrative",
      p("Inny formularz kursu, podobny problem \u2014 respondenci odpowiadali różnie na te same pola.
        Ale tym razem prawie każdą odpowiedź można przypisać do kategorii.
        Porównaj surowe dane z wersją po standaryzacji.")
    ),

    div(class = "toggle-pills",
      actionButton("tab7b_raw", "Surowe", class = "pill-btn active"),
      actionButton("tab7b_cat", "Po standaryzacji", class = "pill-btn")
    ),

    div(class = "widget-block",
      DT::dataTableOutput("tab7b_table")
    ),

    div(class = "callout-success",
      "10 z 12 wierszy można uratować (83%).",
      tags$br(),
      tags$b("poziom:"), " \"podst.\", \"PODSTAWOWY\" \u2192 wszystkie to \"podstawowy\".",
      tags$br(),
      tags$b("platnosc:"), " \"przel.\", \"przelew bankowy\" \u2192 \"przelew\"; \"paypal\" \u2192 \"karta\".",
      tags$br(),
      tags$b("godziny_tyg:"), " \"ok. 5\", \"4-6h\", \"5h\" \u2192 kategoria \"srednie (4-6h)\".
        Straty: \"duzo\" i \"malo\" \u2014 za mało informacji żeby przypisać do kategorii."
    ),

    div(class = "chapter-transition",
      p("Następny zbiór ma inny rodzaj problemów \u2014 błędy w danych."),
      actionButton("ch7_next", "Dalej: 9. Badania laboratoryjne \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  )))

ch8_server <- function(input, output, session) {

  output$tab7_table <- DT::renderDataTable({
    datatable(round_df(reg_data), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$tab7_mean_result <- renderUI({
    req(input$tab7_mean > 0)
    isolate({
      var  <- input$tab7_var
      vals <- reg_data[[var]]

      if (var == "wiek") {
        div(class = "callout-success", style = "margin-top: 10px;",
          paste0("Średnia wieku: ", round(mean(vals, na.rm = TRUE), 1), " lat. "),
          "Ta zmienna jest czysta i liczbowa \u2014 nie ma problemów z kodowaniem."
        )
      } else if (var == "wyksztalcenie") {
        div(class = "callout-info", style = "margin-top: 10px;",
          "Wykształcenie to zmienna kategoryczna \u2014 średnia nie ma matematycznego sensu. ",
          "Ale przynajmniej jest dobrze zakodowana: gotowe, spójne kategorie."
        )
      } else {
        nums  <- safe_numeric(vals)
        n_na  <- sum(is.na(nums))
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
            "R nie wie, co zrobić z tekstem jak 'dobry' albo 'elastycznie'."
          )
        }
      }
    })
  })

  output$tab7_clean_table <- DT::renderDataTable({
    if (input$tab7_toggle == "Surowe") {
      datatable(round_df(reg_data), options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
    } else {
      dosw_map  <- c("3" = 3, "5 lat" = 5, "ponad rok" = 1, "nie mam" = 0, "brak" = 0)
      ocena_map <- c("7" = 7, "6" = 6, "4" = 4, "9" = 9, "7.5" = 7.5,
                     "dobry" = 7, "bardzo dobry" = 9, "sredni" = 5, "B+" = 7, "8/10" = 8)
      clean <- data.frame(
        wiek             = reg_data$wiek,
        wyksztalcenie    = reg_data$wyksztalcenie,
        doswiadczenie_lat = as.numeric(dosw_map[reg_data$doswiadczenie]),
        ocena_1_10       = as.numeric(ocena_map[reg_data$ocena_umiejetnosci]),
        stringsAsFactors = FALSE
      )
      datatable(round_df(clean), options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
    }
  })

  output$tab7_clean_info <- renderUI({
    if (input$tab7_toggle == "Oczyszczone") {
      div(class = "callout-info", style = "margin-top: 10px;",
        tags$strong("Zmiany:"),
        tags$br(), "- wiek: bez zmian \u2014 już był czysty",
        tags$br(), "- wyksztalcenie: bez zmian \u2014 już były spójne kategorie",
        tags$br(), "- doswiadczenie_lat: tylko wpisy numeryczne i kilka słów kluczowych \u2192 reszta NA",
        tags$br(), "- ocena_1_10: słowne oceny zamienione na liczby ('dobry' \u2192 7 itd.) \u2014 ~50% odzyskane",
        tags$br(), "- dostepnosc: USUNIĘTO \u2014 nie da się zakodować",
        tags$br(), tags$br(),
        tags$em("Wniosek: dwie zmienne są od razu użyteczne, dwie wymagają pracy i tracą dane,
                 jedna jest nie do odratowania. Lepiej zaprojektować formularz poprawnie od początku.")
      )
    }
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
