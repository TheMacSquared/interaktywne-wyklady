# ==========================================================================
# ROZDZIAŁ 6: ŚCIĄGA
# ==========================================================================

ch6_ui <- lecture_chapter(
  id = "ch-sciaga",
  num = "06",
  title = "Ściąga",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 06 · Język ryzyka",
      num = "06",
      title = "Najpierw nazwij, potem licz.",
      lead = "Dobra analiza zaczyna się od krótkiej specyfikacji zdarzenia,
              ekspozycji, okresu i danych. Wzór jest dopiero kolejnym krokiem."
    ),

    lc_h2("ch6-mapa", "Mapa pojęć"),
    figure_panel(
      label = "Ściąga 1.1",
      title = "Pięć ról w opisie sytuacji",
      full_width = TRUE,
      tags$table(
        class = "lc-table lc-table-striped lc-table-bordered",
        tags$thead(tags$tr(
          tags$th("Pojęcie"),
          tags$th("Pytanie"),
          tags$th("Przykład z Bananpolu")
        )),
        tags$tbody(
          tags$tr(tags$td("Zagrożenie"), tags$td("Co może spowodować szkodę?"), tags$td("Skórka na przejściu")),
          tags$tr(tags$td("Ekspozycja"), tags$td("Kto lub co ma kontakt z zagrożeniem?"), tags$td("Pracownik przechodzący korytarzem")),
          tags$tr(tags$td("Zdarzenie"), tags$td("Co dokładnie ma zajść?"), tags$td("Poślizgnięcie: utrata przyczepności i upadek (w rejestrze zmian: co najmniej jedno podczas zmiany)")),
          tags$tr(tags$td("Skutek"), tags$td("Jakie może być następstwo?"), tags$td("Uraz nadgarstka")),
          tags$tr(tags$td("Zabezpieczenie"), tags$td("Co przerywa drogę do szkody?"), tags$td("Kontrola i sprzątanie przejścia"))
        )
      )
    ),

    lc_h2("ch6-checklista", "Sześć pytań przed obliczeniem"),
    lc_stat_grid(
      lc_stat_box("1", "Jak brzmi zdarzenie?", caption = "Jednoznacznie i obserwowalnie"),
      lc_stat_box("2", "Spośród czego liczę?", caption = "Mianownik albo przestrzeń wyników"),
      lc_stat_box("3", "Jaka jest jednostka?", caption = "Np. zmiana, przejście, paleta"),
      lc_stat_box("4", "Jaki jest okres?", caption = "Wspólny dla porównań"),
      lc_stat_box("5", "Jakie są założenia?", caption = "Zwłaszcza porównywalność i symetria"),
      lc_stat_box("6", "Jakie są skutki?", caption = "Prawdopodobieństwo nie kończy analizy"),
      columns = 3
    ),

    lc_h2("ch6-wzory", "Najważniejsze zapisy i ich znaczenie"),
    lc_formula_box(
      withMathJax("$$\\widehat p=\\frac{\\text{zaobserwowane zdarzenia}}
                   {\\text{porównywalne obserwacje}}$$"),
      tags$p("Częstość empiryczna opisuje konkretny zbiór obserwacji.")
    ),
    lc_formula_box(
      withMathJax("$$P(A)=\\frac{|A|}{|\\Omega|}$$"),
      tags$p("Definicja klasyczna wymaga skończonej przestrzeni jednakowo możliwych wyników.")
    ),
    lc_formula_box(
      withMathJax("$$P(A\\cup B)=P(A)+P(B)-P(A\\cap B)$$"),
      tags$p("Część wspólną odejmujemy, aby nie policzyć tych samych wyników dwa razy.")
    ),
    lc_formula_box(
      withMathJax("$$P(A^c)=1-P(A)$$"),
      tags$p("Dopełnienie obejmuje wszystkie wyniki, w których zdarzenie A nie zaszło.")
    ),
    lc_formula_box(
      withMathJax("$$P(\\Omega)=1,\\qquad P(\\emptyset)=0,\\qquad 0\\le P(A)\\le 1$$"),
      tags$p("Zdarzenie pewne Ω ma prawdopodobieństwo 1, zdarzenie niemożliwe ∅
             ma 0, a każde zdarzenie mieści się między tymi granicami.")
    ),

    lc_h2("ch6-model", "Jak rozpoznać właściwy punkt startu"),
    figure_panel(
      label = "Ściąga 1.2",
      full_width = TRUE,
      tags$table(
        class = "lc-table lc-table-striped lc-table-bordered",
        tags$thead(tags$tr(
          tags$th("Sytuacja"), tags$th("Punkt startu"), tags$th("Najważniejsze pytanie")
        )),
        tags$tbody(
          tags$tr(tags$td("Losowanie z jawnej, symetrycznej listy"), tags$td("Definicja klasyczna"), tags$td("Czy wyniki są jednakowo możliwe?")),
          tags$tr(tags$td("Rejestr porównywalnych obserwacji"), tags$td("Częstość empiryczna"), tags$td("Czy mianownik i zasady rejestracji są wspólne?")),
          tags$tr(tags$td("Zdarzenia zależne od warunków"), tags$td("Dalszy model probabilistyczny"), tags$td("Co zmienia informacja o warunku?")),
          tags$tr(tags$td("Priorytet działania"), tags$td("Profil ryzyka"), tags$td("Jakie są skutki, bariery i kryteria decyzji?"))
        )
      )
    ),

    lc_feedback(
      type = "ok",
      tags$strong("Minimalny komunikat:"),
      " „W 100 porównywalnych zmianach zarejestrowano 8 zmian ze zdarzeniem,
        czyli częstość 0,08. Dane nie opisują jeszcze dotkliwości skutków ani
        przyczyn różnic między zmianami.”"
    ),

    lc_chapter_next(
      num = "07",
      title = "Quiz",
      lead = "Sprawdź, czy rozpoznajesz mianownik i granice modelu.",
      target_id = "ch-quiz"
    )
  )
)

ch6_server <- function(input, output, session) {
  invisible(NULL)
}
