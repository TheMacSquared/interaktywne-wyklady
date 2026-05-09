# ============================================================================
# ROZDZIAŁ 4: Postępowanie ekonometryczne
# ============================================================================

ch4_ui <- lecture_chapter(
  id = "ch-postepowanie",
  num = "04",
  title = "Postępowanie ekonometryczne",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 01 · Model ekonometryczny",
      num = "04",
      title = "Od problemu do modelu.",
      lead = "Na koniec pierwszego rozdziału porządkujemy schemat pracy, który będzie wracał w regresji, prognozowaniu i optymalizacji."
    ),

    lc_h2("ch4-procedura", "Procedura"),
    lc_p("Ekonometryczna analiza zjawiska gospodarczego ma swój utrwalony tryb postępowania. Każdy z kolejnych rozdziałów kursu odpowiada jednemu lub dwóm krokom z tej listy."),
    figure_panel(
      label = "Schemat",
      title = "Siedem kroków pracy z modelem",
      tags$ol(
        tags$li(tags$strong("Problem:"), " zapisujemy pytanie ekonomiczne."),
        tags$li(tags$strong("Hipoteza:"), " określamy oczekiwany kierunek zależności."),
        tags$li(tags$strong("Dane:"), " wskazujemy źródło, jednostkę obserwacji i zakres."),
        tags$li(tags$strong("Specyfikacja:"), " wybieramy zmienne i postać równania."),
        tags$li(tags$strong("Estymacja:"), " liczymy parametry modelu."),
        tags$li(tags$strong("Weryfikacja:"), " sprawdzamy sens ekonomiczny, dopasowanie i istotność."),
        tags$li(tags$strong("Użycie:"), " interpretujemy, prognozujemy albo wspieramy decyzje.")
      )
    ),

    lc_h2("ch4-przyklad", "Przykład: jak wzrasta sprzedaż piekarni?"),
    lc_p("Spróbujmy przejść te kroki dla małej historii, która wróci w kolejnych rozdziałach:"),
    tags$ol(
      tags$li(tags$strong("Problem:"), " właściciel piekarni chce wiedzieć, czy reklama w lokalnej gazecie zwiększa sprzedaż chleba."),
      tags$li(tags$strong("Hipoteza:"), " większe nakłady na reklamę → większa sprzedaż (kierunek dodatni)."),
      tags$li(tags$strong("Dane:"), " 24 miesiące rachunków za reklamę i miesięcznych przychodów."),
      tags$li(tags$strong("Specyfikacja:"), " sprzedaż = β₀ + β₁ · reklama + ε, model liniowy."),
      tags$li(tags$strong("Estymacja:"), " liczymy b₀ i b₁ metodą najmniejszych kwadratów."),
      tags$li(tags$strong("Weryfikacja:"), " czy znak b₁ jest dodatni? Czy efekt jest istotny? Czy model dobrze opisuje dane?"),
      tags$li(tags$strong("Użycie:"), " jeśli tak — szacujemy, ile dodatkowej sprzedaży przyniesie wzrost reklamy o 1000 zł.")
    ),

    lc_h2("ch4-syllabus", "Miejsce w całym kursie"),
    lc_p("Ten rozdział jest fundamentem dla dalszych tematów z programu: KMNK, błędów standardowych, weryfikacji modelu, modeli dynamicznych, prognozowania oraz modeli optymalizacyjnych."),
    figure_panel(
      label = "Plan",
      title = "Kolejne rozdziały kursu",
      tags$ul(
        tags$li("02 — Regresja liniowa z jedną zmienną i założenia KMNK"),
        tags$li("03 — Estymatory parametrów i błędy standardowe"),
        tags$li("04 — Weryfikacja merytoryczna i statystyczna modelu"),
        tags$li("05 — Szeregi czasowe i prognozowanie"),
        tags$li("06 — Optymalizacja liniowa"),
        tags$li("07 — Simpleks i dualizm")
      )
    ),

    inline_callout(
      label = "Wskazówka",
      color = "wskazowka",
      open = TRUE,
      "Wracaj do tych siedmiu kroków za każdym razem, gdy zaczynasz nowe zadanie. Ekonometria nie jest „policzeniem czegoś w Excelu” — jest procesem, w którym statystyka jest tylko jednym z etapów."
    )
  )
)

ch4_server <- function(input, output, session) {}
