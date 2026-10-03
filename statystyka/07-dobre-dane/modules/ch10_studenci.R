# Tab 10: Studenci — wzorcowa ankieta studencka, dobry zbiór

ch10_ui <- lecture_chapter(id = "ch10", num = "10", title = "Studenci", content = tagList(

  lc_chapter_hero(
    kicker = "Rozdział 10 · Co czyni dobry zbiór danych?",
    num    = "10",
    title  = "Ankieta studencka.",
    lead   = "Zamknięte pytania, spójne kodowanie i wystarczająca liczba
              odpowiedzi sprawiają, że dane nadają się do analizy bez czyszczenia."
  ),

  lc_h2("sec-01", "Opis"),

  lc_p("Ankietę wypełniło 150 studentów. Każdy podał płeć, kierunek i rok
    studiów, liczbę godzin nauki, poziom stresu na skali od 1 do 10, średnią
    ocen i liczbę kursów, na które jest zapisany. Pytania przypominają ankietę
    200 studentów, z której korzystaliśmy w wykładzie 01, a niemal te same
    pytania zadał kolega z rozdziału 3, tyle że ośmiu osobom. Taki zbiór
    pozwala zapytać na przykład, czy studenci, którzy uczą się więcej, mają
    wyższą średnią ocen, albo czy poziom stresu różni się między kierunkami."),

  lc_h2("sec-02", "Podgląd danych"),

  lc_p("Oceniając tabelę, sprawdź, czy każda kolumna ma jeden typ i spójny
    zapis wartości."),

  figure_panel(
    label = "Ryc. 10.1",
    title = "Odpowiedzi 150 studentów",
    uiOutput("tab9_table")
  ),

  lc_p("Siedem zmiennych reprezentuje wszystkie cztery typy z wykładu 01:"),

  tags$ul(
    tags$li(tags$code("plec"), " — płeć, ", gloss("zmienna nominalna", "zmienna nominalna")),
    tags$li(tags$code("kierunek"), " — kierunek studiów, zmienna nominalna"),
    tags$li(tags$code("rok_studiow"), " — rok studiów od 1 do 5, ",
      gloss("zmienna porządkowa", "zmienna porządkowa")),
    tags$li(tags$code("godziny_nauki"), " — liczba godzin nauki, ",
      gloss("zmienna ciągła", "zmienna ciągła")),
    tags$li(tags$code("stres"), " — samoocena stresu od 1 do 10, zmienna porządkowa
      (", gloss("skala Likerta", "skala typu Likerta"), ")"),
    tags$li(tags$code("srednia_ocen"), " — średnia ocen od 2.0 do 5.0, zmienna ciągła"),
    tags$li(tags$code("liczba_kursow"), " — liczba kursów, ",
      gloss("zmienna dyskretna", "zmienna dyskretna"))
  ),

  lc_p("W całym zbiorze nie ma ani jednego braku danych, a wartości mieszczą
    się w sensownych zakresach: godziny nauki od 2.3 do 30, średnia ocen od
    2.59 do 5.00, a stres wykorzystuje całą skalę od 1 do 10. W grupach jest
    wystarczająco dużo osób do porównań: 97 kobiet i 53 mężczyzn, a najmniej
    liczny kierunek, biologia, ma 19 osób. Słabszym punktem jest rok studiów.
    Na pierwszym roku jest 56 osób, a na piątym tylko 6, więc porównanie
    wszystkich pięciu roczników oparłoby się na bardzo nierównych grupach.
    To jedyny ślad problemu, który w katalogu z rozdziału 1 nazwaliśmy „za mało
    danych”, i dotyczy tylko jednej podgrupy."),

  lc_h2("sec-03", "Werdykt"),

  lc_p("Ta ankieta pokazuje, jak wyglądają dane zaprojektowane z myślą
    o analizie: zamknięte pytania, spójne skale, jedna osoba w jednym wierszu.
    Mają tu sens narzędzia z wykładów 01–06: statystyki opisowe, przedziały
    ufności dla średniej, test t dla porównania płci, ANOVA dla kierunków, ",
    gloss("test chi-kwadrat"), " dla płci i kierunku oraz korelacja i regresja
    dla godzin nauki i średniej ocen. Jedyną decyzją do podjęcia jest
    sposób traktowania stresu: skalę od 1 do 10 można liczyć jak liczby, ale
    trzeba to zrobić świadomie, tak jak przy zmiennych porządkowych
    w wykładzie 01."),

  lc_p("Dobry zbiór nie gwarantuje ciekawych wyników. W tej ankiecie
    korelacje między zmiennymi ilościowymi nie przekraczają 0.14. Słaba
    zależność jest jednak informacją o studentach, a nie o usterkach danych.
    Warto zestawić ten zbiór z formularzem z rozdziału 8 i z ankietą na
    grupie z rozdziału 3: tematy są podobne, a o jakości danych zdecydowały
    liczba odpowiedzi i sposób zadania pytań."),

  lc_note("Werdykt",
    "Zbiór dobry: dane nadają się do analizy bez czyszczenia, a jedynym
     ograniczeniem jest mała liczebność najstarszych roczników."
  ),

  lc_chapter_next(
    num = "11",
    title = "Kawiarnia",
    lead = "Ostatni zbiór wygląda równie porządnie, ale jego wiersze to kolejne
            dni, a kolejne dni nie są od siebie niezależne.",
    target_id = "ch11"
  ),

  div(style = "height: 40px;")
))

ch10_server <- function(input, output, session) {

  output$tab9_table <- renderUI({
    dd_data_table(round_df(survey_data), page_size = 8, page = input$tab9_table_page, page_input = "tab9_table_page")
  })

}
