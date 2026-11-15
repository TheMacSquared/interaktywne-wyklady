# Tab 3: Grupa — za mało danych (n=8), zły zbiór

# Tabela dawnego serwera, składana przy budowaniu strony.
tab2_table_html <- function(view = NULL) {
  dd_data_table(round_df(small_data), n = 10)
}

ch3_ui <- lecture_chapter(id = "ch3", num = "3", title = "Grupa", content = tagList(

  lc_chapter_hero(
    kicker = "Rozdział 03 · Co czyni dobry zbiór danych?",
    num    = "03",
    title  = "Ankieta na grupie.",
    lead   = "Ten zbiór wygląda jak typowy projekt studencki, ale ma problem,
              którego nie da się naprawić kosmetyką: zbyt małe n."
  ),

  lc_h2("ch3-sec-01", "Opis"),

  lc_p("Drugie studium przypadku to typowy projekt zaliczeniowy. Dzień przed
    terminem oddania student pyta 8 znajomych o płeć, kierunek studiów,
    liczbę godzin nauki w tygodniu, poziom stresu w skali od 1 do 10
    i średnią ocen. Powstaje zbiór z 8 wierszami i 5 zmiennymi. Pytania,
    które chciałby na nim zadać, brzmią rozsądnie: czy osoby uczące się
    dłużej mają wyższą średnią, czy kobiety i mężczyźni różnią się poziomem
    stresu, czy kierunki różnią się ocenami. Przed dalszą lekturą warto
    samodzielnie ocenić, czy te dane pozwolą na nie odpowiedzieć."),

  # Struktura zbioru (jednostka obserwacji, n, typy kolumn)
  figure_panel(
    label = "Ryc. 3.1",
    title = "Struktura zbioru ankiety",
    width_mode = "text",
    dd_schema("znajomy studenta", 8, c(
      plec = "nominalna",
      kierunek = "nominalna",
      godziny_nauki = "ciagla",
      stres = "porzadkowa",
      srednia_ocen = "ciagla"
    ))
  ),

  lc_h2("ch3-sec-02", "Podgląd danych"),

  lc_p("W podglądzie policz, ile osób trafia do każdej grupy, którą
    student chciałby porównać."),

  figure_panel(
    label = "Tab. 3.1",
    title = "Ankieta: 8 odpowiedzi",
    tab2_table_html()
  ),

  lc_p("Same zmienne nie budzą zastrzeżeń: dwie jakościowe (płeć, kierunek)
    i trzy ilościowe, bez braków i bez wartości spoza skali. Kłopot widać
    dopiero po policzeniu osób w grupach. Kobiet i mężczyzn jest po 4,
    a na kierunkach od 1 osoby (biologia) do 3 osób (psychologia).
    Porównanie kierunków opierałoby się więc na pojedynczych odpowiedziach,
    a porównanie płci na czterech osobach z każdej strony."),

  lc_h2("ch3-sec-03", "Ile obserwacji naprawdę potrzeba?"),

  lc_p("Z wykładu 04 wiemy, że precyzję oszacowania średniej opisuje
    szerokość ", gloss("przedział ufności", "przedziału ufności"), ".
    Zależy ona od rozrzutu danych i od liczby obserwacji:"),

  # Szerokość przedziału ufności ze znacznikami
  lc_formula_annotated(
    "\\text{szerokość} = 2 \\cdot ⟦1|t^*_{\\alpha/2,\\, n-1}⟧ \\cdot \\frac{⟦2|s⟧}{\\sqrt{⟦3|n⟧}}",
    notes  = list("wartość krytyczna", "odchylenie standardowe", "liczba obserwacji"),
    colors = c("indygo", "wrzos", "szalwia"),
    above  = 2
  ),

  lc_p("Z wykładu 05 wiemy z kolei, że ", gloss("moc testu"), " to
    prawdopodobieństwo, że test wykryje różnicę, która naprawdę istnieje.
    Panel pokazuje, jak od liczby obserwacji zależą trzy rzeczy: kształt
    ", gloss("histogram", "histogramu"), ", szerokość 95% przedziału ufności dla średniej ocen (przy
    ", gloss("odchylenie standardowe", "odchyleniu standardowym"), " 0.6) i moc ", gloss("test t", "testu t"), " porównującego dwie
    równoliczne grupy, gdy różnica średnich wynosi pół odchylenia
    standardowego (d = 0.5)."),

  figure_panel(
    label = "Ryc. 3.2",
    title = "Liczba obserwacji a precyzja i moc",
    s08_n_figure()
  ),

  lc_p("Wszystkie trzy wykresy prowadzą do tego samego wniosku. Przy n = 8
    histogram ma tylko kilka słupków i nie da się z niego odczytać kształtu
    rozkładu. W naszej ankiecie średnia ocen wynosi 3.71, odchylenie
    standardowe 0.62, a 95% przedział ufności rozciąga się od 3.20 do 4.23.
    Ma ponad 1 punkt szerokości, czyli nie odróżnia grupy ze średnią
    na poziomie 3.2 od grupy ze średnią 4.2. Szerokość
    maleje proporcjonalnie do 1/√n, więc każde kolejne zawężenie kosztuje
    coraz więcej obserwacji: przy 30 osobach przedział ma około 0.45 pkt
    szerokości, a przy 100 osobach około 0.24 pkt."),

  lc_p("Jeszcze gorzej wygląda moc. Przy 8 osobach, po 4 w każdej grupie,
    test t wykrywa różnicę d = 0.5 tylko w około 9% przypadków. W dziewięciu
    badaniach na dziesięć wynik byłby nieistotny, choć różnica istnieje.
    Moc 80% wymaga około 64 osób w każdej grupie, czyli ponad 120 łącznie.
    To problem „Za mało danych” z katalogu z rozdziału 1, i w tym zbiorze
    dotyka on każdej analizy naraz."),

  lc_h2("ch3-sec-04", "Werdykt"),

  lc_p("Tego zbioru nie da się uratować czyszczeniem ani przekodowaniem, bo
    brakuje w nim nie jakości, lecz informacji. Jedyną naprawą jest zebranie
    większej liczby odpowiedzi, najlepiej po wcześniejszym oszacowaniu, ile
    osób potrzeba do wykrycia różnicy, która nas interesuje. Przy tym
    szacowaniu liczy się liczebność w każdej porównywanej grupie, a nie
    łączna liczba wierszy: 30 osób podzielonych na trzy kierunki to tylko
    po 10 osób na kierunek."),

  lc_p("Z 8 odpowiedzi można co najwyżej opisać te konkretne osoby, na
    przykład tabelą lub wykresem punktowym. Testy, przedziały ufności
    i regresja wymagają uogólnienia na populację studentów, a ta
    próba jest za mała i w dodatku dobrana spośród znajomych."),

  lc_note("Werdykt",
    "Zbiór do odrzucenia: przy 8 osobach żadna analiza wnioskująca nie da
    wiarygodnego wyniku, a jedyną naprawą jest zebranie nowych danych."),

  lc_chapter_next(
    num = "04",
    title = "Pingwiny",
    lead = "Kolejny zbiór ma kilkaset obserwacji i drobne braki, które
            da się uczciwie obsłużyć.",
    target_id = "ch4"
  )
))
