# Tab 7: Wynagrodzenia — Wage (ISLR), dobry zbiór

ch7_ui <- lecture_chapter(id = "ch7", num = "7", title = "Wynagrodzenia", content = tagList(

  lc_chapter_hero(
    kicker = "Rozdział 07 · Co czyni dobry zbiór danych?",
    num    = "07",
    title  = "Wynagrodzenia w USA.",
    lead   = "Duży, kompletny zbiór z mieszanką zmiennych ilościowych
              i jakościowych to materiał, na którym można przeprowadzić
              niemal każdą analizę z tego kursu."
  ),

  lc_h2("sec-01", "Opis"),

  lc_p("Dane pochodzą z amerykańskiego badania Current Population Survey
    i są dołączone do podręcznika „An Introduction to Statistical Learning”.
    Zbiór obejmuje 3000 mężczyzn z regionu Mid-Atlantic, ankietowanych
    w latach 2003–2009, i 11 zmiennych: zarobki, wiek, wykształcenie,
    rodzaj pracy, stan zdrowia i kilka innych cech. Pytań, które można
    tu zadać, jest wiele, a najbardziej oczywiste brzmi: czy wykształcenie
    przekłada się na zarobki?"),

  lc_p("W podglądzie pokazujemy osiem zmiennych:"),

  tags$ul(
    tags$li(tags$code("year"), " — rok badania,"),
    tags$li(tags$code("age"), " — wiek w latach,"),
    tags$li(tags$code("maritl"), " — stan cywilny (pięć kategorii),"),
    tags$li(tags$code("race"), " — rasa (cztery kategorie),"),
    tags$li(tags$code("education"), " — wykształcenie (pięć uporządkowanych poziomów),"),
    tags$li(tags$code("jobclass"), " — rodzaj pracy: przemysł albo usługi informacyjne,"),
    tags$li(tags$code("health"), " — samoocena zdrowia (dwie kategorie),"),
    tags$li(tags$code("wage"), " — roczne wynagrodzenie w tysiącach dolarów.")
  ),

  lc_h2("sec-02", "Podgląd danych"),

  lc_p("Przeglądając tabelę, zwróć uwagę na typy zmiennych i na to, czy
    w kolumnach pojawiają się braki."),

  figure_panel(
    label = "Tab. 7.1",
    title = "Wage: 3000 mężczyzn",
    uiOutput("tab6_table")
  ),

  lc_p("Jeden wiersz to jeden mężczyzna, więc ", gloss("jednostka obserwacji", "jednostka obserwacji"), " jest
    oczywista. W całym zbiorze nie ma ani jednego brakującego pola. Obok
    dwóch ", gloss("zmienna ilościowa", "zmiennych ilościowych"), " (wiek
    i zarobki) mamy kilka ",
    gloss("zmienna jakościowa", "jakościowych"), ", w tym wykształcenie,
    które jest ", gloss("zmienna porządkowa", "zmienną porządkową"),
    ". Kategorie są zapisane po angielsku i ponumerowane, ale ich
    znaczenie jest jednoznaczne."),

  lc_h2("sec-03", "Eksploracja"),

  lc_p("Przy eksploracji zwróć uwagę, czy zmienne mają rozrzut i czy
    kategorie mają dość obserwacji do porównań."),

  figure_panel(
    label = "Ryc. 7.1",
    title = "Rozkład wybranej zmiennej",
    lc_toolbar(
      selectInput("tab6_var", "Zmienna",
        choices = c("wage — wynagrodzenie" = "wage", "age — wiek" = "age",
                    "education — wykształcenie" = "education",
                    "jobclass — rodzaj pracy" = "jobclass",
                    "health — zdrowie" = "health",
                    "maritl — stan cywilny" = "maritl",
                    "race — rasa" = "race"))
                    ),
                    lc_plot("tab6_hist", max_height = "300px")
  ),

  lc_p("Wynagrodzenia mają duży rozrzut: od około 20 do 318 tys. dolarów,
    z ", gloss("mediana", "medianą"), " 104.9 tys. i średnią 111.7 tys. Rozkład ma wyraźny prawy
    ogon, a powyżej 250 tys. widać osobne skupisko 79 osób. To nie błędy
    wpisywania, tylko prawdopodobnie najlepiej zarabiający, ale przed
    modelowaniem warto je obejrzeć osobno. Wiek obejmuje zakres od 18 do
    80 lat. Każdy poziom wykształcenia ma co najmniej 268 osób, a obie
    klasy pracy są prawie równe (1544 i 1456)."),

  lc_p("Żaden z problemów z katalogu nie dyskwalifikuje tego zbioru. Ślady
    są tylko dwa. Pierwszy to ", gloss("niezbalansowane grupy", "niezbalansowane grupy"), " w kilku zmiennych:
    wdowców jest 19, a w kategorii „inna” rasa 37 osób, więc takie
    kategorie lepiej połączyć z innymi albo pominąć w porównaniach.
    Drugi to brak zmienności w zmiennej regionu, której nie ma
    w podglądzie: wszyscy badani pochodzą z tego samego regionu. Nie
    szkodzi to analizie, ale wyznacza granicę wniosków."),

  lc_h2("sec-04", "Werdykt"),

  lc_p("Zbiór ma wszystko, czego szukaliśmy w poprzednich przykładach:
    dużo obserwacji, kompletne dane, rozrzut w zmiennych ilościowych
    i liczne grupy w jakościowych. Sens mają tu niemal wszystkie analizy
    z wykładów 04–06. Mediana zarobków rośnie z wykształceniem od 81.3 tys.
    dolarów bez ukończonej szkoły średniej do 141.8 tys. ze stopniem
    naukowym, co można porównać testem dla kilku grup. Zarobki w dwóch
    klasach pracy porównamy testem dla dwóch grup, a związek wieku
    z zarobkami opiszemy korelacją albo regresją."),

  lc_p("Dwa zastrzeżenia dotyczą interpretacji, nie jakości danych. Po
    pierwsze, są to ", gloss("dane obserwacyjne"), ", więc różnica
    zarobków między poziomami wykształcenia nie jest jeszcze efektem
    przyczynowym. Może ją częściowo tłumaczyć ",
    gloss("zmienna zakłócająca"), ", na przykład zamożność rodziny,
    z której ktoś pochodzi. Zmienne obecne w zbiorze, takie jak wiek czy
    rodzaj pracy, możemy uwzględnić w ", gloss("regresja wieloraka", "regresji wielorakiej"), " z wykładu 06,
    ale niezmierzonych już nie. Po drugie, badani to wyłącznie mężczyźni z jednego regionu USA, więc
    wyników nie można przenosić na kobiety ani na inne regiony."),

  lc_note("Werdykt",
    "Bardzo dobry zbiór: 3000 kompletnych obserwacji i bogata mieszanka
    zmiennych, z zastrzeżeniem, że wnioski dotyczą tylko mężczyzn z jednego
    regionu USA."),

  lc_chapter_next(
    num = "08",
    title = "Formularz rejestracyjny",
    lead = "Zbiór może mieć sensowną liczbę osób i ciekawe pytania, a mimo
            to nie nadawać się do analizy, bo pytania zadano tak, że
            odpowiedzi nie da się policzyć.",
    target_id = "ch8"
  )
))

ch7_server <- function(input, output, session) {

  wage_labels <- c(
    wage = "Wynagrodzenie (tys. USD rocznie)", age = "Wiek (lata)",
    education = "Wykształcenie", jobclass = "Rodzaj pracy",
    health = "Samoocena zdrowia", maritl = "Stan cywilny", race = "Rasa"
  )

  output$tab6_table <- renderUI({
    dd_data_table(round_df(Wage[, c("year", "age", "maritl", "race", "education", "jobclass", "health", "wage")]),
                  page_size = 8, page = input$tab6_table_page, page_input = "tab6_table_page")
  })

  zoom_plot_server("tab6_hist", reactive({
    req(input$tab6_var)
    var <- input$tab6_var
    if (var %in% c("wage", "age")) {
      ggplot(Wage, aes(x = .data[[var]])) +
        geom_histogram(bins = 30, fill = data_primary, color = "white", alpha = 0.8) +
        labs( x = wage_labels[[var]], y = "Liczebność") +
        theme_upwr(base_size = 14)
    } else {
      ggplot(Wage, aes(x = .data[[var]])) +
        geom_bar(fill = data_primary, alpha = 0.8) +
        labs( x = wage_labels[[var]], y = "Liczebność") +
        theme_upwr(base_size = 14) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
    }
  }))

}
