# Tab 12: Ściąga — podsumowanie i lista kontrolna jakości danych

ch12_ui <- lecture_chapter(id = "ch12", num = "12", title = "Ściąga", content = tagList(
  fluidRow(column(8, offset = 2,

    lc_chapter_hero(
      kicker = "Rozdział 12 · Co czyni dobry zbiór danych?",
      num    = "12",
      title  = "Ściąga jakości danych.",
      lead   = "Krótka lista kontrolna: co dyskwalifikuje zbiór,
                co da się naprawić i jak dopasować analizę do struktury danych."
    ),

    lc_h2("sec-01", "Podsumowanie 10 zbiorów"),

    div(class = "lc-figure-panel",
      uiOutput("tab11_summary")
    ),

    lc_h2("sec-02", "Lista kontrolna jakości danych"),

    lc_note("Krytyczne", title = "Jeśli zbiór ich nie spełnia, poszukaj innego",
      tags$ol(
        tags$li(tags$strong("Czy dane odpowiadają hipotezie badawczej?"),
          " Najpierw sformułuj, co chcesz badać, potem sprawdź, czy dane to mierzą."),
        tags$li(tags$strong("Czy liczebność wystarcza w każdej grupie?"),
          " Liczy się n w każdej porównywanej podgrupie, nie w całym zbiorze.
          Ile obserwacji potrzeba, zależy od spodziewanej wielkości efektu i planowanej analizy."),
        tags$li(tags$strong("Czy zbiór zawiera różne typy zmiennych?"),
          " Zmienne ilościowe do korelacji i regresji, jakościowe do porównań grup
          (test t) i testu chi-kwadrat."),
        tags$li(tags$strong("Czy jest zmienność?"),
          " Zmienna o SD bliskim zera nie nadaje się do analizy."),
        tags$li(tags$strong("Czy struktura danych pasuje do analiz?"),
          " Sprawdź, czy masz odpowiednie zmienne do każdej planowanej analizy
          i czy wiersz tabeli to jednostka obserwacji."),
        tags$li(tags$strong("Czy obserwacje są niezależne?"),
          " Dane czasowe lub pogrupowane wymagają specjalnych metod albo agregacji.")
      )
    ),

    lc_note("Naprawialne", title = "Wymagają pracy, ale się da",
      tags$ol(start = 7,
        tags$li(tags$strong("Czy braków danych jest niewiele?"),
          " Przy niewielkim odsetku braków można usunąć obserwacje z brakami albo
          zastosować imputację. Gdy w zmiennej brakuje dużej części wartości
          (orientacyjnie powyżej 20–30%), ta zmienna może odpaść."),
        tags$li(tags$strong("Czy zmienne są jednoznacznie zdefiniowane?"),
          " Można rekodować albo przejść na kategorie lub rangi, ale każda decyzja
          ma konsekwencje."),
        tags$li(tags$strong("Czy nie ma błędów i wartości odstających?"),
          " Sprawdź zakresy i literówki. Odróżniaj błędy (popraw albo usuń) od
          prawdziwych wartości odstających (przemyśl, czy je zostawić).")
      )
    ),

    lc_h2("sec-03", "Dopasowanie analizy do danych"),

    div(class = "lc-figure-panel",
      uiOutput("tab11_analysis_table")
    ),

    lc_note("Wskazówka",
      "Użyj tej listy kontrolnej, oceniając dane do swojego projektu końcowego.
      Jeśli zbiór nie spełnia kryteriów krytycznych, poszukaj innego. Jeśli ma
      problemy naprawialne, można z nim pracować, ale zaplanuj czas na czyszczenie."
    ),

    div(style = "height: 60px;")
  ))))

ch12_server <- function(input, output, session) {

  output$tab11_summary <- renderUI({
    df <- data.frame(
      Nr = 2:11,
      Zbior = c("Szkoły w Kalifornii", "Ankieta na grupie", "Pingwiny",
                "Filmy Tarantino", "Hotel boutique", "Wynagrodzenia USA",
                "Formularz rejestracyjny kursu", "Badania laboratoryjne", "Ankieta studencka", "Kawiarnia"),
      n = c("420", "8", "344", "1894 zdarzenia (7 filmów)", "80", "3000", "90", "150", "150", "245"),
      Werdykt = c("DOBRY", "ZŁY", "DOBRY", "ZŁY", "ZŁY", "DOBRY", "ZŁY", "MIESZANY", "DOBRY", "ZŁY"),
      Problem = c("Brak poważnych problemów", "Za mało danych", "Niewielkie braki danych",
                  "Zła struktura danych (po agregacji n = 7)",
                  "Brak zmienności", "Brak poważnych problemów", "Źle zdefiniowane zmienne",
                  "Błędy i literówki w danych, wartości odstające", "Brak poważnych problemów",
                  "Braki danych, brak niezależności obserwacji"),
      stringsAsFactors = FALSE
    )
    lc_table(df,
      cols = list(
        lc_col("Nr", "Nr", "row"),
        lc_col("Zbior", "Zbiór", "text"),
        lc_col("n", "n", "text"),
        lc_col("Werdykt", "Werdykt", "text"),
        lc_col("Problem", "Problem", "text")
      ),
      narrow = "cards"
    )
  })

  output$tab11_analysis_table <- renderUI({
    df <- data.frame(
      Analiza = c("Test t", "Korelacja Pearsona", "Regresja liniowa", "Test chi-kwadrat"),
      Min_n = c("zależy od wielkości efektu; zwykle kilkadziesiąt na grupę",
                "zwykle co najmniej kilkadziesiąt par obserwacji",
                "więcej predyktorów wymaga więcej obserwacji",
                "liczebności oczekiwane w komórkach nie za małe (wykład 05)"),
      Zmienne = c("1 ilościowa + 1 jakościowa (2 grupy)", "2 ilościowe (ciągłe)",
                  "1 ilościowa (Y) + k ilościowych/jakościowych (X)", "2 jakościowe"),
      Dodatkowe = c("Normalność, równość wariancji", "Liniowość, normalność",
                    "Liniowość, normalność reszt, homoskedastyczność", "Niezależność obserwacji"),
      stringsAsFactors = FALSE
    )
    lc_table(df,
      cols = list(
        lc_col("Analiza", "Analiza", "row"),
        lc_col("Min_n", "Liczebność (orientacyjnie)", "text"),
        lc_col("Zmienne", "Zmienne", "text"),
        lc_col("Dodatkowe", "Dodatkowe założenia", "text")
      ),
      narrow = "cards"
    )
  })
}
