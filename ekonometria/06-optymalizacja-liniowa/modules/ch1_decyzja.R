# ============================================================================
# ROZDZIAŁ 1: Problem decyzyjny
# ============================================================================

ch1_ui <- lecture_chapter(
  id = "ch-decyzja",
  num = "01",
  title = "Problem decyzyjny",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 06 · Optymalizacja",
      num = "01",
      title = "Od decyzji do modelu.",
      lead = "Piekarnia produkuje chleb i bułki. Mąka i czas pracy pieca są ograniczone. Ile czego upiec, żeby maksymalnie zarobić? To jest typowy problem decyzyjny — i ekonomia ma na niego matematyczną odpowiedź."
    ),

    lc_h2("ch1-historia", "Historia z piekarni"),
    lc_p("Pan Jan piecze chleb i bułki. Każdy chleb wymaga 0.5 kg mąki i 0.3 godziny pracy pieca. Każda bułka — 0.2 kg mąki i 0.1 godziny pieca. Dziennie ma 30 kg mąki i 12 godzin pracy pieca. Chleb sprzedaje za 15 zł zysku, bułkę za 6 zł zysku. Pytanie brzmi prosto: co i ile robić, żeby na koniec dnia zarobić jak najwięcej?"),
    lc_p("Intuicja podpowiada różne odpowiedzi: „same chleby, bo droższe” albo „same bułki, bo szybciej”. Programowanie liniowe zamienia tę dyskusję na rachunek."),

    lc_h2("ch1-elementy", "Cztery elementy zadania optymalizacyjnego"),
    figure_panel(
      label = "Schemat",
      title = "Z czego składa się każde zadanie PL",
      tags$ol(
        tags$li(tags$strong("Zmienne decyzyjne:"), " to, co WYBIERAMY. Tu: x₁ = liczba chlebów, x₂ = liczba bułek."),
        tags$li(tags$strong("Funkcja celu:"), " to, co MAKSYMALIZUJEMY (lub minimalizujemy). Tu: zysk Z = 15·x₁ + 6·x₂."),
        tags$li(tags$strong("Ograniczenia:"), " zasoby, których nie można przekroczyć. Tu: mąka (0.5·x₁ + 0.2·x₂ ≤ 30) i piec (0.3·x₁ + 0.1·x₂ ≤ 12)."),
        tags$li(tags$strong("Warunki nieujemności:"), " zwykle nie produkujemy ujemnej liczby produktów: x₁, x₂ ≥ 0.")
      )
    ),

    lc_h2("ch1-zapis", "Sformalizowany zapis"),
    lc_p("Te cztery elementy zapisujemy razem jako jedno zadanie:"),
    lc_formula_box(
      withMathJax(helpText("$$\\max\\, Z = 15x_1 + 6x_2$$")),
      withMathJax(helpText("$$0.5x_1 + 0.2x_2 \\le 30 \\quad \\text{(mąka)}$$")),
      withMathJax(helpText("$$0.3x_1 + 0.1x_2 \\le 12 \\quad \\text{(piec)}$$")),
      withMathJax(helpText("$$x_1, x_2 \\ge 0$$"))
    ),

    lc_h2("ch1-typy-zadan", "Inne typy zadań — ten sam wzorzec"),
    lc_p("Schemat jest uniwersalny. Trzy przykłady, które na pierwszy rzut oka nie mają nic wspólnego z piekarnią, w rzeczywistości są tym samym zadaniem matematycznym:"),
    figure_panel(
      label = "Tabela 1.1",
      title = "Wspólny wzorzec różnych decyzji",
      tags$table(
        class = "table",
        tags$thead(tags$tr(
          tags$th("Sytuacja"),
          tags$th("Zmienne decyzyjne"),
          tags$th("Cel"),
          tags$th("Ograniczenia")
        )),
        tags$tbody(
          tags$tr(tags$td("Gospodarstwo rolne"), tags$td("hektary pszenicy, hektary kukurydzy"), tags$td("max przychód"), tags$td("ziemia, nawóz")),
          tags$tr(tags$td("Warsztat stolarski"), tags$td("liczba krzeseł, liczba stołów"), tags$td("max zysk"), tags$td("drewno, czas pracy")),
          tags$tr(tags$td("Inwestycja"), tags$td("kwota w akcje, kwota w obligacje"), tags$td("max oczekiwany zwrot"), tags$td("kapitał, tolerancja ryzyka"))
        )
      )
    ),

    inline_callout(
      label = "Zapamiętaj",
      color = "wskazowka",
      open = TRUE,
      "Każde zadanie optymalizacji liniowej da się zapisać w tym samym schemacie: zmienne decyzyjne, funkcja celu, ograniczenia, warunki nieujemności. Pierwszy krok analitysza — przetłumaczyć opis biznesowy na ten formalizm."
    ),

    lc_chapter_next(
      num = "02",
      title = "Postać klasyczna i standardowa",
      lead = "jak rozwiązuje to maszyna",
      target_id = "ch-postac"
    )
  )
)

ch1_server <- function(input, output, session) {}
