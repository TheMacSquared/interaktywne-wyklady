# ============================================================================
# CHAPTER 2: Populacja i próba
# ============================================================================

ch2_ui <- list(
  id    = "ch-populacja",
  num   = "02",
  title = "Populacja i próba",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 02 · Dane i populacja",
      num    = "02",
      title  = "Zbadaliśmy dwieście osób, a mówimy o milionach.",
      lead   = "Sondaż przed wyborami pyta około tysiąca osób, a wynik podaje
                się dla całego kraju. Grupę, o której chcemy coś powiedzieć,
                nazywamy populacją, a tę, którą faktycznie zbadaliśmy, próbą.
                Prawie cała statystyka dotyczy przejścia od jednej do drugiej."
    ),

    lc_p("W poprzednim rozdziale ustaliliśmy, że wiersz tabeli opisuje jedną
      obserwację. Teraz pytanie brzmi: czy tabela zawiera wszystkie
      obserwacje, które nas interesują, czy tylko ich część. Od odpowiedzi
      zależy, co wolno powiedzieć na podstawie danych."),

    lc_h2("ch2-definicje", "Populacja, próba, badanie pełne"),

    lc_p(gloss("populacja", "Populacja"), " to cały zbiór jednostek, o których
      chcemy wyciągnąć wniosek: wszyscy dorośli mieszkańcy Polski, wszystkie
      krowy w stadzie, wszystkie partie tabletek wyprodukowane w tym roku.
      ", gloss("próba", "Próba"), " to ta część populacji, którą faktycznie
      zbadaliśmy. Liczebność populacji oznaczamy wielką literą N,
      liczebność próby małą literą n."),

    lc_p("Gdy badamy wszystkie jednostki populacji, mówimy o badaniu pełnym.
      Tak działa narodowy spis powszechny albo ewidencja wszystkich studentów
      w systemie uczelni. Badanie pełne zdarza się jednak rzadko. Bywa zbyt
      drogie, jak ankieta wśród wszystkich Polaków, zbyt wolne, jak pomiar
      każdego drzewa w lesie, albo niszczące, jak test wytrzymałości każdej
      śruby z partii, po którym nie zostałaby żadna do sprzedania. Dlatego
      zwykle badamy próbę i na jej podstawie mówimy coś o populacji."),

    lc_p("Populacja musi być określona dokładnie, zanim zaczniemy zbierać dane.
      „Studenci wydziału” to za mało: czy liczymy osoby na urlopie
      dziekańskim, studentów wymiany, studia zaoczne? Każda z tych decyzji
      zmienia N i może zmienić wynik. W praktyce populację wyznacza ",
      gloss("operat losowania"), ", czyli lista jednostek, z której
      losujemy próbę, na przykład wykaz studentów z dziekanatu albo rejestr
      gospodarstw rolnych. Kogo nie ma w operacie, ten nie może trafić
      do próby."),

    lc_h2("ch2-losowanie", "Próba z populacji"),

    lc_p("Wyobraźmy sobie, że wszyscy ", lc_fmt(pop_N), " studentów wydziału
      pisało dziś egzamin ze statystyki. Wyniki będą dopiero za tydzień,
      a Ty chcesz wiedzieć już teraz, ilu z nas zdało. Nie zadzwonisz
      do wszystkich, więc zadzwonisz do kilkudziesięciu. Na wykresie
      każda kropka to jedna osoba. Przejdź przez cztery kroki, a na końcu
      sprawdzimy, jak daleko od prawdy jest to, co usłyszałeś."),

    figure_panel(
      label = "Ryc. 2.1",
      width_mode = "text",
      scene_widget("ch2_populacja", "Kto zdał egzamin ze statystyki?",
        steps = c("Populacja", "Lista", "Próba", "Rzeczywistość"),
        labels = c("Zadzwoń do kolegów", "Zadzwoń do kolegów", "Zadzwoń do kolegów", "Zadzwoń do kolegów"),
        options = list(list(name = "n", label = "Do ilu dzwonisz (n)", from = 3,
                            values = c(20, 50, 200), selected = 50)),
        more = NULL,
        config = list(kind = "pop", n = 50, z = as.integer(faculty$zdal),
                      aria = "Dwa tysiące czterysta kropek reprezentujących studentów po egzaminie, wylosowana próba i odsetek, który zdał"))
    ),

    lc_p("Nawet przy n = 200 próba to tylko ",
      paste0(lc_fmt(100 * 200 / pop_N, 1), "% wydziału. Kolejne losowania wybierają
      inne osoby, a kropki próby rozrzucone są po całym wykresie, bez
      skupisk w jednym miejscu. To jest właśnie cecha losowania: o tym,
      kto trafi do próby, decyduje przypadek, a nie badacz ani sami
      badani. Dzięki temu próba nie faworyzuje żadnej grupy, na przykład
      osób mieszkających blisko uczelni albo studentów pierwszego roku.")),

    lc_p("Co ciekawe, dokładność wniosków z dobrze wylosowanej próby zależy
      przede wszystkim od n, a prawie nie zależy od N. Tysiąc losowo
      wybranych osób mówi o kraju liczącym 38 milionów mieszkańców niemal
      tyle samo, co tysiąc osób o mieście liczącym 200 tysięcy. Dlaczego tak
      jest, zobaczymy w wykładach 02 i 03. Najpierw musimy rozróżnić liczbę,
      której szukamy w populacji, od liczby, którą liczymy z próby."),

    lc_chapter_next(
      num       = "03",
      title     = "Parametr i statystyka",
      lead      = "liczba, której nie znamy, i liczba, którą mamy",
      target_id = "ch-parametr"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch2_server <- function(input, output, session) {
  scene_texts(input, output, "ch2_populacja", list(
    tagList("Każda kropka to student, który właśnie wyszedł z egzaminu. Wyników jeszcze nie ma, więc nikt nie wie,
      ilu zdało. Razem jest ", tags$code("N", .noWS = "outside"), " = ", lc_fmt(pop_N), " osób. Zadzwoń do kilku losowych kolegów
      i zapytaj, jak im poszło."),
    tagList("Zanim zadzwonisz, potrzebujesz listy numerów. Najlepsza jest lista obecności, ale ktoś był chory,
      ktoś na wymianie. Puste kółka to osoby poza listą: nie ma ich w operacie, więc nigdy do nich nie zadzwonisz.
      Losujemy tylko z tego, co mamy."),
    tagList("Dzwonisz do losowych osób i każda mówi, czy zdała. Zielone to zdali, czerwone to nie. Zmień ",
      tags$code("n", .noWS = "outside"), " i dzwoń kilka razy. Kto odbierze, zmienia się za każdym razem."),
    tagList("Wyniki wchodzą do USOS i wiemy, jak było naprawdę. Dolny pasek porównuje rzeczywistość z tym,
      co usłyszałeś. Z kilkudziesięciu telefonów wyszło zwykle blisko prawdy. Przy ", tags$code("n", .noWS = "outside"),
      " = 200 jeszcze bliżej, a przy 20 zdarza się spora pomyłka.")
  ))
}
