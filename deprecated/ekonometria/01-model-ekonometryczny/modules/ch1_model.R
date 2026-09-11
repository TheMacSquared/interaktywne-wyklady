# ============================================================================
# ROZDZIAŁ 1: Model ekonometryczny
# ============================================================================

ch1_ui <- lecture_chapter(
  id = "ch-model",
  num = "01",
  title = "Model ekonometryczny",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 01 · Ekonometria",
      num = "01",
      title = "Model ekonometryczny.",
      lead = "Zaczynamy od języka modelu: co jest zmienną objaśnianą, co objaśniającą, a co zostaje w składniku losowym."
    ),

    lc_h2("ch1-po-co", "Po co budujemy model?"),
    lc_p("Wyobraź sobie, że właściciel piekarni patrzy na rachunki za reklamę i zastanawia się, czy wydane pieniądze faktycznie przekładają się na większą sprzedaż chleba. Może tak, może nie — danych jest dużo, a zależność niejasna. Ekonometria daje mu narzędzie: zapisuje to pytanie w postaci równania i mierzy, czy dane się z nim zgadzają."),
    lc_p("Model ekonometryczny jest uproszczonym opisem zależności gospodarczej zapisanym w postaci równania. Jego celem nie jest skopiowanie rzeczywistości, tylko uporządkowanie pytania: co chcemy wyjaśnić, czym to wyjaśniamy i jak duża jest niepewność tego opisu."),
    lc_formula_box(
      withMathJax(helpText("$$Y_i = f(X_{1i}, X_{2i}, \\ldots, X_{ki}) + \\varepsilon_i$$")),
      p(withMathJax("\\(Y\\)"), " — zmienna objaśniana, np. sprzedaż, popyt, koszt, plon."),
      p(withMathJax("\\(X\\)"), " — zmienne objaśniające, np. cena, dochód, nakłady, czas."),
      p(withMathJax("\\(\\varepsilon\\)"), " — składnik losowy: czynniki pominięte, błędy pomiaru i przypadkowość.")
    ),

    lc_h2("ch1-zmienne", "Klasyfikacja zmiennych"),
    figure_panel(
      label = "Mapa",
      title = "Elementy modelu w praktycznym pytaniu",
      tagList(
        p(tags$strong("Pytanie:"), " Jak nakłady reklamowe wpływają na miesięczną sprzedaż produktu?"),
        lc_stat_grid(
          eco_metric("Y", "sprzedaż", "wynik, który próbujemy wyjaśnić", unname(upwr_cat["niebo"])),
          eco_metric("X", "nakłady", "czynnik kontrolowany lub obserwowany", unname(upwr_cat["szalwia"])),
          eco_metric("ε", "reszta", "to, czego model nie opisał", unname(upwr_cat["terakota"])),
          columns = 3
        )
      )
    ),

    lc_h2("ch1-postepowanie", "Postępowanie ekonometryczne"),
    tagList(
      tags$ol(
        tags$li("Sformułowanie problemu ekonomicznego i hipotezy."),
        tags$li("Dobór zmiennych oraz zebranie danych."),
        tags$li("Wybór postaci modelu."),
        tags$li("Estymacja parametrów."),
        tags$li("Weryfikacja merytoryczna i statystyczna."),
        tags$li("Interpretacja, prognoza albo decyzja.")
      ),
      inline_callout(
        label = "Zapamiętaj",
        color = "wskazowka",
        open = TRUE,
        "Model nigdy nie jest dokładnym odwzorowaniem rzeczywistości. Jest narzędziem do podejmowania decyzji, kiedy dane są zaszumione, a zależności trudno zauważyć gołym okiem."
      )
    ),

    lc_chapter_next(
      num = "02",
      title = "Zmienne w modelu",
      lead = "co wyjaśniamy, czym wyjaśniamy i czego nie widzimy",
      target_id = "ch-zmienne"
    )
  )
)

ch1_server <- function(input, output, session) {}
