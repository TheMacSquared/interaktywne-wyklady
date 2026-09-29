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

    lc_h2("ch6-podsumowanie", "Podsumowanie"),
    lc_p(
      "Wykład zaczął się od skórki na korytarzu i od rozdzielenia jednej historii
       na pięć ról: zagrożenie, ekspozycję, zdarzenie, skutek i zabezpieczenie
       (definicja 1.1). Rachunek prawdopodobieństwa dotyczy tylko jednej z nich —
       zdarzenia — i działa tylko wtedy, gdy zdarzenie jest obserwowalne i ma
       jednostkę. Pierwszym źródłem liczb jest rejestr: częstość empiryczna
       n_A / n (wzór 1.1) opisuje konkretną serię obserwacji, zmienia się od
       serii do serii i stabilizuje dopiero przy dużej liczbie porównywalnych
       zmian."
    ),
    lc_p(
      "Drugim źródłem jest symetria. Gdy doświadczenie ma skończoną przestrzeń
       zdarzeń elementarnych Ω (definicja 1.3), a wyniki są jednakowo możliwe z
       konstrukcji — jak przy losowaniu palety generatorem — prawdopodobieństwo
       zdarzenia A ⊆ Ω to |A| / |Ω| (wzór 1.2). Z tej definicji wynikają granice
       (1.3): P(Ω) = 1, P(∅) = 0, 0 ≤ P(A) ≤ 1. Zdarzenia łączymy jak zbiory
       (definicja 1.6): dopełnienie daje wzór (1.4), suma — wzór (1.5) z
       odjęciem części wspólnej, a „ani A, ani B” — prawa de Morgana (1.6)."
    ),
    lc_p(
      "Oba źródła liczb spełniają te same trzy aksjomaty Kołmogorowa (1.7), z
       których wynikają wszystkie wzory tego wykładu; w kolejnych wykładach
       aksjomaty pozwolą pracować także z modelami, w których wyników jest
       nieskończenie wiele. Wreszcie rozdział o decyzji przypomniał granicę
       rachunku: prawdopodobieństwo musi mieć horyzont, nie wolno go dodawać
       dla zdarzeń, które nie są rozłączne (przykład 1.6), i samo nie wyznacza
       priorytetu — ten wymaga profilu skutków, ekspozycji i barier."
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
      tags$p("Wzór (1.1). Częstość empiryczna opisuje konkretny zbiór obserwacji.")
    ),
    lc_formula_box(
      withMathJax("$$P(A)=\\frac{|A|}{|\\Omega|}$$"),
      tags$p("Wzór (1.2). Definicja klasyczna wymaga skończonej przestrzeni jednakowo możliwych wyników.")
    ),
    lc_formula_box(
      withMathJax("$$P(A\\cup B)=P(A)+P(B)-P(A\\cap B)$$"),
      tags$p("Wzór (1.5). Część wspólną odejmujemy, aby nie policzyć tych samych wyników dwa razy.")
    ),
    lc_formula_box(
      withMathJax("$$P(A^c)=1-P(A)$$"),
      tags$p("Wzór (1.4). Dopełnienie obejmuje wszystkie wyniki, w których zdarzenie A nie zaszło.")
    ),
    lc_formula_box(
      withMathJax("$$P(\\Omega)=1,\\qquad P(\\emptyset)=0,\\qquad 0\\le P(A)\\le 1$$"),
      tags$p("Wzór (1.3). Zdarzenie pewne Ω ma prawdopodobieństwo 1, zdarzenie niemożliwe ∅
             ma 0, a każde zdarzenie mieści się między tymi granicami.")
    ),
    lc_formula_box(
      withMathJax("$$P(A)\\ge 0,\\qquad P(\\Omega)=1,\\qquad A\\cap B=\\emptyset\\ \\Rightarrow\\ P(A\\cup B)=P(A)+P(B)$$"),
      tags$p("Aksjomaty (1.7). Każde poprawne przypisanie prawdopodobieństw — z symetrii
             czy z danych — musi je spełniać; pozostałe wzory z nich wynikają.")
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
      lead = "Dziesięć pytań o to, co naprawdę liczysz.",
      target_id = "ch-quiz"
    )
  )
)

ch6_server <- function(input, output, session) {
  invisible(NULL)
}
