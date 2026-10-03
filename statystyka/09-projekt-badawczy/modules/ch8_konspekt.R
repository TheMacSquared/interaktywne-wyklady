ch8_ui <- lecture_chapter(id = "ch8", num = "8", title = "Od konspektu do wniosku", content = tagList(
  lc_chapter_hero(
    kicker = "Rozdział 08 · Domknięcie projektu",
    num = "08",
    title = "Od konspektu do wniosku.",
    lead = "Konspekt napisany przed analizą staje się szkieletem raportu.
            Przy każdym tropie dopisujemy wynik, jego skalę, ograniczenie
            i pytanie, które z niego wynika."
  ),

  lc_p("Cel, który prowadził przez cały wykład, brzmiał: ",
    tags$em(tr_goal), " Rozdziały 5–7 dostarczyły wyników: pięć prostych
    testów, przegląd zmiennych zakłócających i serię modeli kontrolnych.
    Ten rozdział składa je we wniosek i pokazuje, co z tego trafia do
    raportu."),

  lc_h2("sec-01", "Konspekt po analizie"),

  lc_p("Po analizie konspekt nie znika. Jego cztery części z rozdziału 4
    (cel, zmienne i pomiar, tropy, plan interpretacji) zostają, a obok
    nich pojawiają się cztery nowe: wyniki przy tropach, interpretacja
    celu, ograniczenia i następny krok. Każda ma przykład dla naszych
    danych."),

  lc_h3("Wyniki przy tropach", num = "1"),
  lc_p("Przy każdym tropie zapisujemy, czy dane go wzmocniły, czy
    osłabiły, i jak duży jest efekt. Cel się nie zmienia tylko
    dlatego, że jeden wynik okazał się ciekawy."),
  lc_note("U nas", p("Atrakcyjność, płeć, status native speaker i odsetek
    odpowiedzi wiążą się z ", tags$code("eval"), " także w modelu kontrolnym.
    Status mniejszościowy w prostym porównaniu był nieistotny, a w pełnym
    modelu istotny.")),

  lc_h3("Interpretacja celu", num = "2"),
  lc_p("Zbieramy tropy razem i piszemy, co cała wiązka mówi o głównym ",
    gloss("pytanie badawcze", "pytaniu badawczym"), "."),
  lc_note("U nas", p("Ocena z ankiety wygląda raczej na wskaźnik mieszany niż
    na czystą miarę jakości nauczania.")),

  lc_h3("Ograniczenia", num = "3"),
  lc_p("Nazywamy, czego dane nie pozwalają stwierdzić. To część jakości
    projektu, a nie porażka analizy."),
  lc_note("U nas", p(gloss("dane obserwacyjne", "Dane obserwacyjne"), " pokazują współwystępowanie,
    ale nie pozwalają rozstrzygnąć ", gloss("przyczynowość", "przyczynowości"),
    "; kursy tego samego prowadzącego nie są niezależnymi obserwacjami.")),

  lc_h3("Następny krok", num = "4"),
  lc_p("Zapisujemy, jak rozwinąć projekt: jakie dane, pomiary albo
    porównania byłyby potrzebne po pierwszej analizie."),
  lc_note("U nas", p("Pomiar efektów uczenia się, dane o trudności kursu
    i oczekiwanej ocenie, komentarze z ankiet.")),

  lc_h2("sec-02", "Wniosek dla naszych danych"),

  lc_p("Wniosek odpowiada na cel, podaje skalę efektów i od razu mówi,
    czego nie wiadomo. Dla naszej wiązki wygląda tak."),

  lc_p("Ocena kursu z ankiety wiąże się z cechami, które nie są jakością
    nauczania. Kursy prowadzone przez osoby oceniane jako atrakcyjniejsze
    dostają wyższe oceny, także po uwzględnieniu płci, wieku, statusu
    native speaker, typu kursu i odsetka odpowiedzi. Różnica między
    prowadzącymi z dolnych i górnych 10% oceny atrakcyjności to średnio
    około 0.29 punktu (95% przedział ufności od 0.15 do 0.42), czyli
    mniej więcej połowa odchylenia standardowego ocen. Kursy prowadzone
    przez kobiety mają w tym samym modelu oceny niższe o około 0.20
    punktu. Ocena wiąże się też z tym, jaka część grupy wypełniła
    ankietę: przy wyższym odsetku odpowiedzi oceny są wyższe. Wszystkie te zmienne razem
    wyjaśniają jednak mniej niż jedną piątą zmienności ocen (skorygowany
    R² = 0.178), więc ocena z ankiety nie jest też głównie odbiciem
    wyglądu czy płci prowadzącego."),

  lc_p("Ocena z ankiety wygląda więc na wskaźnik mieszany. Może zawierać
    informację o jakości nauczania, ale zawiera też składniki, które
    z jakością nie mają oczywistego związku. Tego, ile w niej samej
    jakości, te dane nie rozstrzygną, bo nie ma w nich żadnej niezależnej
    miary tego, czego studenci się nauczyli."),

  lc_p("Wniosek ma kilka ograniczeń, które trzeba wymienić razem z nim."),

  tags$ul(
    tags$li("Dane obserwacyjne. Pokazują, że atrakcyjność i ocena
      współwystępują, ale nie, że jedna wpływa na drugą. Prowadzący
      oceniani jako atrakcyjniejsi mogą różnić się czymś, czego nie
      zmierzono, na przykład stylem prowadzenia."),
    tags$li("Obserwacje nie są niezależne. 463 kursy prowadziły 94 osoby,
      a cechy prowadzącego powtarzają się we wszystkich jego kursach.
      Przedziały ufności i p-wartości są przez to zbyt optymistyczne."),
    tags$li("Małe grupy. Wyniki dla statusu native speaker i mniejszości
      opierają się na 7 i 12 prowadzących. Wynik dla mniejszości zmienia
      się w zależności od zestawu zmiennych kontrolnych."),
    tags$li("Pomiar. Ocena atrakcyjności to ocena wystawiona przez
      studentów, a nie cecha osoby, i częściowo odzwierciedla wiek.
      Ocena kursu nie jest bezpośrednim pomiarem jakości nauczania."),
    tags$li("Zakres danych. Zbiór opisuje kursy jednego uniwersytetu
      w jednym okresie. Nie wiadomo, czy podobny wzór wystąpi gdzie
      indziej.")
  ),

  lc_note("Zasada", rule = TRUE,
    "Wniosek z danych obserwacyjnych mówi, co współwystępuje, w jakiej
     skali i z jaką niepewnością, a także czego dane nie pozwalają
     rozstrzygnąć i dlaczego."),

  lc_p("Takie ograniczenia nie osłabiają pracy. Pokazują, że autorzy
    rozumieją granice własnego badania, i mówią czytelnikowi, jak daleko
    może się na wniosku oprzeć. Całą drogę widać w tablicy tropów: cel,
    pięć tropów, narzędzia, miary efektów i ostrożne werdykty. Raport
    wraca do konspektu, który powstał przed analizą, i pokazuje, co dane
    w nim zmieniły."),

  figure_panel(label = "Ryc. 8.1", title = "Tak wygląda domknięty projekt: cel + wiązka + werdykty",
    tr_board_ui(reveal = tr_trop_order, show_verdict = TRUE)
  ),

  lc_p("Tablica pokazuje werdykty z rozdziału 5. Wniosek z tego rozdziału
    jest od niej bogatszy o model kontrolny i ograniczenia. Tak jest
    w każdym projekcie: pierwsze testy porządkują tropy, a wniosek powstaje
    dopiero wtedy, gdy wyniki zestawi się razem i z tym, czego w danych
    nie ma."),

  lc_h2("sec-03", "Domknięcie kursu"),

  lc_p("Ten wykład kończy kurs statystyki. Jego projekt badawczy korzystał
    po trochu z każdego wcześniejszego wykładu. Typy zmiennych z wykładu
    01 zdecydowały, które narzędzie pasuje do którego tropu, a statystyki
    opisowe z tego samego wykładu stały w każdym panelu rozdziału 5.
    Rozkłady z wykładu 02 i przedziały ufności z wykładu 03 stoją za
    każdym przedziałem i każdą p-wartością. Testy z wykładu 04 sprawdziły
    pojedyncze tropy, a wykład 05 podpowiedział, kiedy sięgnąć po test
    nieparametryczny. Regresja wieloraka i porównanie modeli z wykładu 06
    pozwoliły sprawdzić tropy razem. Pytania o jakość danych z wykładu 07
    wróciły przy pomiarze i przy niezależności obserwacji, a wykład 08
    pokazał tę samą ścieżkę na innym zbiorze."),

  lc_p("Metody z kursu są narzędziami do odpowiadania na pytania.
    Pojedynczy test, wykres czy model rzadko odpowiada na pytanie
    badawcze sam. Odpowiedź powstaje z ich zestawienia: z celu
    sformułowanego przed analizą, z wyników sprawdzonych na kilka
    sposobów i z uczciwie zapisanych ograniczeń. Tę samą drogę można
    przejść na własnych danych.")
  )
)

ch8_server <- function(input, output, session) {
}
