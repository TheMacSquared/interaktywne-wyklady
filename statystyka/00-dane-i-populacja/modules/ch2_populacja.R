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

    lc_p("Nasz wydział to populacja licząca N = ", lc_fmt(pop_N), " studentów.
      Na wykresie każda kropka to jedna osoba z operatu, czyli z listy
      z dziekanatu. Panel losuje z tej listy próbę o wybranej liczebności:
      każda osoba ma tę samą szansę, że do niej trafi, niezależnie od tego,
      gdzie stoi na liście."),

    figure_panel(
      label = "Ryc. 2.1",
      title = "Wydział jako populacja i wylosowana z niego próba",
      width_mode = "wide",
      lc_toolbar(
        lc_slider("ch2_n", "Liczebność próby (n)", 10, 400, 50, 10),
        lc_action("ch2_draw", "Losuj próbę", icon = "shuffle", variant = "solid"),
        lc_readouts(uiOutput("ch2_reads"))
      ),
      lc_plot("ch2_grid_plot", ratio = "3/2")
    ),

    lc_p("Nawet przy n = 400 próba to tylko ",
      paste0(lc_fmt(100 * 400 / pop_N, 1), "% wydziału. Kolejne losowania wybierają
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

  ch2_trigger <- reactiveVal(0)
  observeEvent(input$ch2_draw, ch2_trigger(ch2_trigger() + 1))

  ch2_ids <- reactive({
    ch2_trigger()
    req(input$ch2_n)
    sample(pop_N, input$ch2_n)
  })

  output$ch2_reads <- renderUI({
    n <- length(ch2_ids())
    tagList(
      lc_readout("populacja (N)", lc_fmt(pop_N), color = col_pop, swatch = TRUE),
      lc_readout("próba (n)", n, color = col_sample, swatch = TRUE),
      lc_readout("n / N", paste0(lc_fmt(100 * n / pop_N, 1), "%"))
    )
  })

  zoom_plot_server("ch2_grid_plot", reactive({
    ids <- ch2_ids()
    df <- faculty
    df$in_sample <- df$id %in% ids
    ggplot(df, aes(gx, gy)) +
      geom_point(data = df[!df$in_sample, ], color = col_pop, alpha = 0.35,
                 size = 1.3) +
      geom_point(data = df[df$in_sample, ], color = col_sample, size = 2.2) +
      scale_y_reverse() +
      coord_equal() +
      theme_void()
  }), alt = "Siatka 2400 kropek; wylosowane osoby są wyróżnione kolorem")
}
