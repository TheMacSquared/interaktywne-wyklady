# ============================================================================
# CHAPTER 1: Model ekonometryczny
# ============================================================================

ch1_ui <- lecture_chapter(
  id = "ch-model",
  num = "01",
  title = "Model ekonometryczny",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdzial 01 · Ekonometria",
      num = "01",
      title = "Model ekonometryczny.",
      lead = "Zaczynamy od jezyka modelu: co jest zmienna objasniana, co objasniajaca, a co zostaje w skladniku losowym."
    ),

    lc_h2("ch1-po-co", "Po co budujemy model?"),
    lc_p("Model ekonometryczny jest uproszczonym opisem zaleznosci gospodarczej zapisanym w postaci rownania. Jego celem nie jest skopiowanie rzeczywistosci, tylko uporzadkowanie pytania: co chcemy wyjasnic, czym to wyjasniamy i jak duza jest niepewnosc tego opisu."),
    lc_formula_box(
      withMathJax(helpText("$$Y_i = f(X_{1i}, X_{2i}, \\ldots, X_{ki}) + \\varepsilon_i$$")),
      p(withMathJax("\\(Y\\)"), " - zmienna objasniana, np. sprzedaz, popyt, koszt, plon."),
      p(withMathJax("\\(X\\)"), " - zmienne objasniajace, np. cena, dochod, naklady, czas."),
      p(withMathJax("\\(\\varepsilon\\)"), " - skladnik losowy: czynniki pominiete, bledy pomiaru i przypadkowosc.")
    ),

    lc_h2("ch1-zmienne", "Klasyfikacja zmiennych"),
    figure_panel(
      label = "Mapa",
      title = "Elementy modelu w praktycznym pytaniu",
      tagList(
        p(tags$strong("Pytanie:"), " Jak naklady reklamowe wplywaja na miesieczna sprzedaz produktu?"),
        lc_stat_grid(
          eco_metric("Y", "sprzedaz", "wynik, ktory probujemy wyjasnic", unname(upwr_cat["niebo"])),
          eco_metric("X", "naklady", "czynnik kontrolowany lub obserwowany", unname(upwr_cat["szalwia"])),
          eco_metric("epsilon", "reszta", "to, czego model nie opisal", unname(upwr_cat["terakota"])),
          columns = 3
        )
      )
    ),

    lc_h2("ch1-postepowanie", "Postepowanie ekonometryczne"),
    tagList(
      tags$ol(
        tags$li("Sformulowanie problemu ekonomicznego i hipotezy."),
        tags$li("Dobor zmiennych oraz zebranie danych."),
        tags$li("Wybor postaci modelu."),
        tags$li("Estymacja parametrow."),
        tags$li("Weryfikacja merytoryczna i statystyczna."),
        tags$li("Interpretacja, prognoza albo decyzja.")
      ),
      inline_callout(
        label = "Do ustalenia z prowadzaca",
        color = "wskazowka",
        open = TRUE,
        "W tej wersji zostawiamy miejsce na docelowe przyklady branzowe: firma, rynek pracy, rolnictwo, finanse publiczne albo dane regionalne."
      )
    ),

    lc_chapter_next(
      num = "02",
      title = "Zmienne w modelu",
      lead = "co wyjasniamy, czym wyjasniamy i czego nie widzimy",
      target_id = "ch-zmienne"
    )
  )
)

ch1_server <- function(input, output, session) {}
