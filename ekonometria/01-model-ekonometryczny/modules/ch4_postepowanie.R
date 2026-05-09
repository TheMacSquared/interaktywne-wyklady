# ============================================================================
# CHAPTER 4: Postepowanie ekonometryczne
# ============================================================================

ch4_ui <- lecture_chapter(
  id = "ch-postepowanie",
  num = "04",
  title = "Postepowanie ekonometryczne",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdzial 01 · Model ekonometryczny",
      num = "04",
      title = "Od problemu do modelu.",
      lead = "Na koniec pierwszego rozdzialu porzadkujemy workflow, ktory bedzie wracal w regresji, prognozowaniu i optymalizacji."
    ),

    lc_h2("ch4-procedura", "Procedura"),
    figure_panel(
      label = "Schemat",
      title = "Siedem krokow pracy z modelem",
      tags$ol(
        tags$li(tags$strong("Problem:"), " zapisujemy pytanie ekonomiczne."),
        tags$li(tags$strong("Hipoteza:"), " okreslamy oczekiwany kierunek zaleznosci."),
        tags$li(tags$strong("Dane:"), " wskazujemy zrodlo, jednostke obserwacji i zakres."),
        tags$li(tags$strong("Specyfikacja:"), " wybieramy zmienne i postac rownania."),
        tags$li(tags$strong("Estymacja:"), " liczymy parametry modelu."),
        tags$li(tags$strong("Weryfikacja:"), " sprawdzamy sens ekonomiczny, dopasowanie i istotnosc."),
        tags$li(tags$strong("Uzycie:"), " interpretujemy, prognozujemy albo wspieramy decyzje.")
      )
    ),

    lc_h2("ch4-syllabus", "Miejsce w calym kursie"),
    lc_p("Ten rozdzial jest fundamentem dla dalszych tematow z syllabusowego programu: KMNK, bledow standardowych, weryfikacji modelu, szeregowych modeli dynamicznych, prognozowania oraz modeli optymalizacyjnych."),
    figure_panel(
      label = "Plan",
      title = "Proponowane kolejne rozdzialy",
      tags$ul(
        tags$li("02 - Regresja liniowa z jedna zmienna i zalozenia KMNK"),
        tags$li("03 - Estymatory parametrow i bledy standardowe"),
        tags$li("04 - Weryfikacja merytoryczna i statystyczna modelu"),
        tags$li("05 - Szeregi czasowe i prognozowanie"),
        tags$li("06 - Optymalizacja liniowa"),
        tags$li("07 - Simpleks i dualizm")
      )
    ),

    inline_callout(
      label = "Do decyzji",
      color = "wskazowka",
      open = TRUE,
      "Numeracja kolejnych rozdzialow jest robocza. Mozemy ja latwo zmienic po rozmowie z prowadzaca i po ustaleniu, czy material optymalizacyjny ma byc osobnym blokiem czy czescia jednego rozdzialu."
    )
  )
)

ch4_server <- function(input, output, session) {}
