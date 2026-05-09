# ============================================================================
# ROZDZIAŁ 1: Idea simpleksu
# ============================================================================

ch1_ui <- lecture_chapter(
  id = "ch-simpleks",
  num = "01",
  title = "Idea simpleksu",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 07 · Simpleks",
      num = "01",
      title = "Wierzchołki, ale nie wszystkie.",
      lead = "Twierdzenie podstawowe mówi: optimum zawsze jest w wierzchołku. Ale wierzchołków może być wiele — w realnych zadaniach setki. Czy musimy sprawdzać wszystkie? Nie — simpleks robi to sprytnie."
    ),

    lc_h2("ch1-pomysl", "Pomysł"),
    lc_p("Wyobraź sobie, że stoisz na jednym z wierzchołków obszaru dopuszczalnego. Patrzysz na sąsiednie wierzchołki: czy któryś z nich daje większy zysk? Jeśli tak — przechodzisz tam. Jeśli nie — jesteś w optimum. Tak działa simpleks — nie skanuje wszystkich punktów, tylko idzie po krawędziach wielokąta, zawsze w stronę poprawy."),

    lc_h2("ch1-kroki", "Cztery kroki algorytmu"),
    figure_panel(
      label = "Schemat",
      title = "Simpleks w czterech krokach",
      tags$ol(
        tags$li(tags$strong("Start:"), " wybierz dowolny wierzchołek dopuszczalny (zwykle początek układu, x = 0)."),
        tags$li(tags$strong("Test:"), " sprawdź, czy któryś sąsiedni wierzchołek poprawi funkcję celu."),
        tags$li(tags$strong("Krok:"), " jeśli tak — przejdź do najlepszego sąsiada. Jeśli nie — koniec."),
        tags$li(tags$strong("Stop:"), " aktualny wierzchołek to optimum.")
      )
    ),

    lc_h2("ch1-piekarnia", "Wracamy do piekarni"),
    lc_p("W zadaniu z rozdziału 06 (chleb i bułki, mąka i piec) wierzchołki obszaru dopuszczalnego to: V₀ = (0, 0), V₁ = (60, 0), V₂ = optimum (przecięcie ograniczeń mąki i pieca), V₃ = (0, 90). Simpleks zacznie od V₀ (zysk = 0), zobaczy, że V₁ daje zysk większy, przejdzie tam. Potem sprawdzi V₂ — większy zysk, przejdzie. Z V₂ wszystkie sąsiednie wierzchołki dają mniej — koniec."),
    figure_panel(
      label = "Tabela 1.1",
      title = "Trasa simpleksu po wierzchołkach",
      tags$table(
        class = "table",
        tags$thead(tags$tr(
          tags$th("Krok"),
          tags$th("Wierzchołek"),
          tags$th("(x₁, x₂)"),
          tags$th("Zysk Z"),
          tags$th("Decyzja")
        )),
        tags$tbody(
          tags$tr(tags$td("0"), tags$td("V₀"), tags$td("(0, 0)"),   tags$td("0 zł"),    tags$td("start; sąsiedzi V₁ i V₃ poprawiają — idziemy do V₁")),
          tags$tr(tags$td("1"), tags$td("V₁"), tags$td("(60, 0)"),  tags$td("1800 zł"), tags$td("sąsiad V₂ poprawia — idziemy do V₂")),
          tags$tr(tags$td("2"), tags$td("V₂"), tags$td("optimum"),  tags$td("max"),     tags$td("żaden sąsiad nie poprawia — STOP"))
        )
      )
    ),

    lc_h2("ch1-dlaczego-dziala", "Dlaczego simpleks na pewno znajdzie optimum?"),
    lc_p("Funkcja celu jest liniowa, a obszar dopuszczalny wypukły. Jeśli przejście do sąsiada poprawia cel, to nie ma w okolicy lepszego wyboru — wystarczy iść dalej w tę samą stronę. A że wierzchołków jest skończenie wiele, algorytm musi się zatrzymać. To nie jest gradient descent z lokalnym minimum — w problemach LP nie ma lokalnych pułapek."),

    inline_callout(
      label = "Zapamiętaj",
      color = "wskazowka",
      "Simpleks NIE sprawdza wszystkich wierzchołków. Idzie tylko po sąsiadach, w kierunku poprawy. Dla 100 zmiennych liczba wierzchołków rośnie wykładniczo, ale liczba kroków simpleksu zwykle pozostaje umiarkowana — to dlatego algorytm jest praktyczny dla problemów z setkami i tysiącami zmiennych."
    ),

    lc_chapter_next("02", "Tablica simpleksowa", "narzędzie, które realizuje algorytm", "ch-tablica")
  )
)

ch1_server <- function(input, output, session) {}
