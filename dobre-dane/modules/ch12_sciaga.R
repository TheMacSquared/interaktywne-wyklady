# Tab 12: Ściąga — podsumowanie i checklist jakości danych

ch12_ui <- tabPanel("12. Ściąga",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Ściąga - jak ocenić zbiór danych"),

    div(class = "section-title", "Podsumowanie 10 zbiorów"),

    div(class = "widget-block",
      tableOutput("tab11_summary")
    ),

    div(class = "section-title", "Checklist jakości danych"),

    div(class = "callout-danger",
      HTML("
        <strong style='font-size: 15px;'>KRYTYCZNE - jeśli nie spełniasz, szukaj innego zbioru:</strong>
        <ol>
          <li><strong>Czy dane odpowiadają hipotezie badawczej?</strong> Najpierw sformułuj co chcesz badać, potem sprawdź czy dane to mierzą.</li>
          <li><strong>Czy masz n &ge; 20-30 na grupę?</strong> Liczy się n w każdej podgrupie. Porównujesz 3 grupy? Potrzebujesz 3 &times; 30 = 90.</li>
          <li><strong>Czy masz mix typów zmiennych?</strong> Ilościowe do korelacji/regresji, jakościowe do t-testów i chi-kwadrat.</li>
          <li><strong>Czy jest zmienność?</strong> SD &asymp; 0 oznacza brak możliwości analizy.</li>
          <li><strong>Czy struktura danych pasuje do analiz?</strong> Sprawdź czy masz odpowiednie zmienne do każdej planowanej analizy.</li>
          <li><strong>Czy obserwacje są niezależne?</strong> Dane czasowe lub z klastrów wymagają specjalnych metod (lub agregacji).</li>
        </ol>
      ")
    ),

    div(class = "callout-warning",
      HTML("
        <strong style='font-size: 15px;'>NAPRAWIALNE - wymagają pracy, ale się da:</strong>
        <ol start='7'>
          <li><strong>Czy braki &lt; 5%?</strong> Można usunąć obserwacje z brakami lub imputować. Powyżej 20-30% w zmiennej - ta zmienna może odpaść.</li>
          <li><strong>Czy zmienne są jednoznacznie zdefiniowane?</strong> Można rekodować, przejść na rangi - ale każda decyzja ma konsekwencje.</li>
          <li><strong>Czy nie ma błędów i outlierów?</strong> Sprawdź zakresy, literówki. Odróżniaj błędy (usuń) od prawdziwych outlierów (przemyśl).</li>
        </ol>
      ")
    ),

    div(class = "section-title", "Dopasowanie analizy do danych"),

    div(class = "widget-block",
      tableOutput("tab11_analysis_table")
    ),

    div(class = "callout-info",
      tags$strong("Wskazówka:"),
      " Użyj tego checklistu oceniając dane do swojego projektu końcowego.",
      tags$br(),
      "Jeśli nie spełniasz kryteriów krytycznych - szukaj innego zbioru.",
      tags$br(),
      "Jeśli masz problemy naprawialne - możesz pracować z tymi danymi, ale zaplanuj czas na czyszczenie."
    ),

    div(style = "height: 60px;")
  )))

ch12_server <- function(input, output, session) {

  output$tab11_summary <- renderTable({
    data.frame(
      Nr = 2:11,
      Zbior = c("Szkoły w Kalifornii", "Ankieta na grupie", "Pingwiny",
                "Filmy Tarantino", "Ankieta firmowa", "Wynagrodzenia USA",
                "Trudna ankieta", "Ceny mieszkań", "Ankieta studencka", "Jakość powietrza"),
      n = c(420, 8, 344, "~1800 zdarzeń", 80, 3000, 90, 150, 150, 153),
      Werdykt = c("DOBRY", "ZŁY", "DOBRY", "ZŁY", "ZŁY", "DOBRY", "ZŁY", "MIESZANY", "DOBRY", "ZŁY"),
      Problem = c("Brak", "Za mała próba", "Niewielkie braki", "Zła struktura, n=7 po agregacji",
                  "Brak zmienności", "Brak", "Źle zdefiniowane zmienne",
                  "Outliery i błędy", "Brak", "Braki danych + szereg czasowy"),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$tab11_analysis_table <- renderTable({
    data.frame(
      Analiza = c("Test t", "Korelacja Pearsona", "Regresja liniowa", "Test chi-kwadrat"),
      Min_n = c("20-30 na grupę", "30 ogólnie", "10k + 50 (k = predyktory)", "5 w każdej komórce tabeli"),
      Zmienne = c("1 ilościowa + 1 jakościowa (2 grupy)", "2 ilościowe (ciągłe)",
                  "1 ilościowa (Y) + k ilościowych/jakościowych (X)", "2 jakościowe"),
      Dodatkowe = c("Normalność, równość wariancji", "Liniowość, normalność",
                    "Liniowość, normalność reszt, homoskedastyczność", "Niezależność obserwacji"),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}
