# Tab 2: Szkoły — CASchools (AER), dobry zbiór wzorcowy

# Polskie etykiety zmiennych CASchools (wartości = nazwy kolumn w danych)
.ch2_var_labels <- c(
  read        = "Wynik z czytania, pkt (read)",
  math        = "Wynik z matematyki, pkt (math)",
  expenditure = "Wydatki na ucznia, USD (expenditure)",
  income      = "Średni dochód w okręgu, tys. USD (income)",
  english     = "Uczniowie uczący się angielskiego, % (english)",
  lunch       = "Uczniowie z dotacją do obiadu, % (lunch)",
  students    = "Liczba uczniów (students)",
  teachers    = "Liczba nauczycieli (teachers)",
  calworks    = "Uczniowie z rodzin na zasiłku, % (calworks)"
)
.ch2_choices <- function(vars) stats::setNames(vars, .ch2_var_labels[vars])

ch2_ui <- lecture_chapter(id = "ch2", num = "2", title = "Szkoły", content = tagList(

  lc_chapter_hero(
    kicker = "Rozdział 02 · Co czyni dobry zbiór danych?",
    num    = "02",
    title  = "Szkoły w Kalifornii.",
    lead   = "Zaczynamy od wzorcowego zbioru: dużo obserwacji, jasne zmienne
              i realne pytania badawcze."
  ),

  lc_h2("sec-01", "Opis"),

  lc_p("Pierwsze studium przypadku to zbiór, który znamy już z wykładów 04
    i 06. CASchools opisuje 420 okręgów szkolnych w Kalifornii w roku
    szkolnym 1998–1999. Każdy wiersz to jeden okręg, a 14 kolumn opisuje
    jego wielkość (liczba uczniów i nauczycieli), zasoby (wydatki na ucznia),
    sytuację społeczną (średni dochód, odsetek uczniów uczących się
    angielskiego, odsetek uczniów z dotacją do obiadu i z rodzin na
    zasiłku) oraz średnie wyniki uczniów klas piątych w standaryzowanym
    teście z czytania i matematyki. Dane zebrał kalifornijski departament
    edukacji, a spopularyzował je podręcznik ekonometrii Stocka i Watsona."),

  lc_p("Na takich danych można zadać kilka sensownych pytań. Czy okręgi, które
    wydają więcej na ucznia, mają lepsze wyniki testów? Jak silnie wyniki
    wiążą się z zamożnością okręgu? Czy czytanie i matematyka idą w parze?
    Zanim zaczniemy szukać odpowiedzi, sprawdzimy zbiór według katalogu
    z rozdziału 1. Warto ocenić go samodzielnie przed przeczytaniem werdyktu."),

  lc_p("Podgląd danych niżej pokazuje 11 z 14 kolumn (pomija hrabstwo,
    zakres klas i liczbę komputerów):"),

  tags$ul(
    tags$li(tags$code("district"), ", ", tags$code("school"),
      " — kod okręgu i nazwa szkoły,"),
    tags$li(tags$code("students"), ", ", tags$code("teachers"),
      " — liczba uczniów i nauczycieli,"),
    tags$li(tags$code("expenditure"), " — wydatki na ucznia (USD),"),
    tags$li(tags$code("income"), " — średni dochód w okręgu (tys. USD),"),
    tags$li(tags$code("english"), " — odsetek uczniów uczących się angielskiego (%),"),
    tags$li(tags$code("lunch"), " — odsetek uczniów z dotacją do obiadu (%),"),
    tags$li(tags$code("calworks"), " — odsetek uczniów z rodzin na zasiłku CalWorks (%),"),
    tags$li(tags$code("read"), ", ", tags$code("math"),
      " — średni wynik testu Stanford 9 z czytania i matematyki (pkt).")
  ),

  lc_h2("sec-02", "Podgląd danych"),

  lc_p("W podglądzie zwróć uwagę, co jest ",
    gloss("jednostka obserwacji", "jednostką obserwacji"), " i czy wartości
    w kolumnach mają sensowne zakresy."),

  figure_panel(
    label = "Tab. 2.1",
    title = "CASchools: 420 okręgów szkolnych",
    uiOutput("tab1_table")
  ),

  lc_p("Każda kolumna ma jasną definicję i jednostkę, a w całym zbiorze nie
    ma ani jednej brakującej wartości. Zmienne ilościowe przeważają, ale
    z odsetków łatwo zbudować grupy (np. okręgi biedniejsze i zamożniejsze),
    więc dane nadają się zarówno do korelacji i regresji, jak i do porównań
    grup."),

  lc_h2("sec-03", "Eksploracja zmiennych"),

  lc_p("Przy ocenie pojedynczej zmiennej patrzymy na kształt rozkładu,
    zakres i wartości, które nie mieszczą się w sensownych granicach."),

  figure_panel(
    label = "Ryc. 2.1",
    title = "Rozkład wybranej zmiennej",
    lc_toolbar(
      selectInput("tab1_var", "Zmienna",
        choices = .ch2_choices(c("read", "math", "expenditure", "income", "english",
                                 "lunch", "students", "teachers", "calworks")))
    ),
    lc_plot("tab1_hist", max_height = "300px"),
    verbatimTextOutput("tab1_summary")
  ),

  lc_p("Wyniki testów mają rozkład zbliżony do symetrycznego: wynik
    z czytania waha się od 604.5 do 704 pkt, ze średnią 655.0 i medianą
    655.8. Inaczej wyglądają zmienne opisujące wielkość okręgu. Liczba
    uczniów ma silną prawostronną ", gloss("skośność"), ": mediana wynosi
    950.5, ale największy okręg liczy 27 176 uczniów, a 24 okręgi mają ich
    ponad 10 000. To nie błąd, tylko rzeczywiste różnice między małymi
    okręgami wiejskimi a wielkimi miastami. Żadna wartość nie wygląda na
    literówkę: odsetki mieszczą się w przedziale od 0 do 100%, a wydatki
    na ucznia (od 3926 do 7712 USD) są realistyczne. Problemy z katalogu,
    takie jak błędy i literówki w danych albo braki danych, tu nie
    występują."),

  lc_h2("sec-04", "Zależności między zmiennymi"),

  lc_p("Przy ocenie związków patrzymy, czy chmura punktów ma wyraźny kształt
    i czy prosta dobrze go opisuje."),

  figure_panel(
    label = "Ryc. 2.2",
    title = "Wynik testu a cechy okręgu",
    lc_toolbar(
      selectInput("tab1_x", "Zmienna X",
        choices = .ch2_choices(c("expenditure", "income", "english", "lunch",
                                 "calworks", "students")),
        selected = "income"),
      selectInput("tab1_y", "Zmienna Y",
        choices = .ch2_choices(c("read", "math")), selected = "read")
    ),
    lc_plot("tab1_scatter_plot", max_height = "350px")
  ),

  lc_p("Dochód okręgu i wynik z czytania są wyraźnie powiązane:
    współczynnik ", gloss("korelacja", "korelacji"), " wynosi 0.70,
    a prosta rośnie o około 1.9 pkt na każdy tysiąc dolarów dochodu.
    Chmura punktów lekko się jednak wygina: przy najwyższych dochodach
    wyniki rosną wolniej, niż przewiduje prosta. Najsilniejszy związek
    ma odsetek uczniów z dotacją do obiadu (r = -0.88), a najsłabszy
    wydatki na ucznia (r = 0.22). Zmienność jest duża we wszystkich
    kolumnach, więc problem braku zmienności z katalogu też nie
    występuje."),

  lc_p("Te zależności trzeba czytać ostrożnie. Wydatki na ucznia rosną
    razem z dochodem okręgu (r = 0.31), więc dochód działa jak ",
    gloss("zmienna zakłócająca"), " w pytaniu o wpływ wydatków. To ",
    gloss("dane obserwacyjne"), ": nikt nie przydzielał okręgom budżetów
    losowo. Z wykresu rozrzutu nie wynika, że dodatkowy dolar podniósłby
    wyniki, a jedynie, że okręgi wydające więcej mają je przeciętnie
    trochę wyższe."),

  lc_h2("sec-05", "Werdykt"),

  lc_p("Zbiór przechodzi przez katalog bez zastrzeżeń. 420 obserwacji
    wystarcza nawet wtedy, gdy podzielimy okręgi na kilka grup. Zmienne
    są jasno zdefiniowane, mają duży rozrzut i nie zawierają braków ani
    błędów. Obserwacje to osobne okręgi, a każdy z nich występuje w tabeli
    raz. Na tych danych mają sens wszystkie analizy z wykładów 03–06:
    przedziały ufności dla średnich, testy porównujące grupy okręgów,
    korelacja oraz regresja prosta i wieloraka, w której dochód występuje
    jako zmienna kontrolna."),

  lc_p("Dwa ograniczenia wynikają nie z jakości danych, tylko z ich natury.
    Jednostką obserwacji jest okręg, nie uczeń, więc wnioski dotyczą
    okręgów i nie przenoszą się automatycznie na pojedyncze dzieci. Dane
    opisują też Kalifornię sprzed ćwierć wieku, więc nie mówią nic
    bezpośrednio o polskich szkołach."),

  lc_note("Werdykt",
    "Bardzo dobry zbiór: duże n, jasne zmienne bez braków i błędów,
    ograniczony tylko obserwacyjnym charakterem danych i jednostką
    obserwacji na poziomie okręgu."),

  lc_chapter_next(
    num = "03",
    title = "Ankieta na grupie",
    lead = "Następny zbiór ma poprawne zmienne, ale za mało obserwacji,
            żeby cokolwiek z nich wynikało.",
    target_id = "ch3"
  )
))

ch2_server <- function(input, output, session) {

  output$tab1_table <- renderUI({
    dd_data_table(round_df(CASchools[, c("district", "school", "students", "teachers", "expenditure",
                                "income", "english", "lunch", "calworks", "read", "math")]),
                  page_size = 8, page = input$tab1_table_page, page_input = "tab1_table_page")
  })

  zoom_plot_server("tab1_hist", reactive({
    req(input$tab1_var)
    ggplot(CASchools, aes(x = .data[[input$tab1_var]])) +
      geom_histogram(bins = 25, fill = data_primary, color = "white", alpha = 0.8) +
      labs(x = .ch2_var_labels[[input$tab1_var]], y = "Liczebność") +
      theme_upwr(base_size = 14)
  }))

  output$tab1_summary <- renderPrint({
    req(input$tab1_var)
    summary(CASchools[[input$tab1_var]])
  })

  zoom_plot_server("tab1_scatter_plot", reactive({
    req(input$tab1_x, input$tab1_y)
    ggplot(CASchools, aes(x = .data[[input$tab1_x]], y = .data[[input$tab1_y]])) +
      geom_point(alpha = 0.5, color = data_reference) +
      geom_smooth(method = "lm", color = data_primary, se = TRUE) +
      labs(
           x = .ch2_var_labels[[input$tab1_x]], y = .ch2_var_labels[[input$tab1_y]]) +
      theme_upwr(base_size = 14)
  }))

}
