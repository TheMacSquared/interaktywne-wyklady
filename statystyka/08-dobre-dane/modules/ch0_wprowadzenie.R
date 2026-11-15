# Tab 0: Wprowadzenie — wstęp do wykładu o jakości danych

ch0_ui <- lecture_chapter(id = "ch0", num = "0", title = "Wprowadzenie", content = tagList(

  lc_chapter_hero(
    kicker = "Rozdział 00 · Co czyni dobry zbiór danych?",
    num    = "00",
    title  = "Od pomysłu do danych.",
    lead   = "Dobra analiza zaczyna się przed pierwszym testem: od sprawdzenia,
              czy dane naprawdę odpowiadają na pytanie badawcze i czy da się
              je sensownie analizować."
  ),

  lc_h2("ch0-sec-01", "Od pomysłu do danych"),

  lc_p("W poprzednich wykładach dane były gotowe: ankieta studentów, okręgi
    szkolne z Kalifornii, pingwiny z Antarktydy. Wiedzieliśmy, które zmienne
    są ilościowe, a które jakościowe, i mogliśmy od razu liczyć średnie,
    ", gloss("przedział ufności", "przedziały ufności"), ", testy i regresję. We własnym projekcie kolejność jest
    odwrotna. Najpierw trzeba wiedzieć, co chcemy zbadać, a dopiero potem
    szukać danych, które na to pytanie odpowiedzą."),

  lc_p("Każda analiza zaczyna się więc od pomysłu, jeszcze zanim otworzymy
    jakikolwiek plik. Szukamy związku między dwiema zmiennymi? Porównujemy
    grupy? Sprawdzamy, czy coś zmienia się w czasie? Na tym etapie nie
    potrzebujemy formalnej hipotezy statystycznej, wystarczy jasno opisany
    pomysł w zwykłym języku. Z niego wyprowadzimy później ",
    gloss("hipoteza badawcza", "hipotezy"), " i hipotezy statystyczne,
    tak jak w wykładzie 05."),

  lc_p("Warto wybierać tematy z własnej dziedziny. Kto rozumie
    kontekst, zadaje lepsze pytania, szybciej zauważa absurdalny wynik
    i łatwiej formułuje sensowne hipotezy. Znajomość dziedziny daje analizie
    niuans, którego nie zastąpi żaden podręcznik statystyki."),

  lc_h2("ch0-sec-02", "Drugi krok: dane"),

  lc_p("Gdy pomysł jest gotowy, trzeba znaleźć albo zebrać dane. Tu pojawia
    się pierwsza pułapka: nie każdy zbiór nadaje się do planowanej analizy.
    Jedne problemy dyskwalifikują dane od razu i żadna metoda ich nie
    naprawi. Inne wymagają pracy, ale po oczyszczeniu dane nadal są
    użyteczne. Ten wykład uczy odróżniać jedne od drugich."),

  lc_p("Przed przejściem dalej zastanów się przez chwilę, co warto sprawdzić
    najpierw po otwarciu nieznanego zbioru danych i co może w nim pójść
    nie tak. Porównaj potem swoją listę z katalogiem z następnego rozdziału."),

  lc_h2("ch0-sec-03", "Plan wykładu"),

  lc_p("Rozdział 1 to katalog siedmiu typowych problemów w danych. Każdy
    pokazujemy na małym przykładzie: jak wygląda w tabeli, jak na wykresie,
    czym grozi w analizie i co można z nim zrobić. Katalog kończy lista
    kontrolna, która zbiera wszystkie kryteria w jednym miejscu."),

  lc_p("Rozdziały 2–11 to dziesięć zbiorów danych do samodzielnej oceny.
    Każdy ma ten sam układ: opis zbioru, podgląd danych, eksploracja
    i werdykt. Część zbiorów jest wzorcowa, część ma usterki do naprawienia,
    a część nie nadaje się do klasycznej analizy statystycznej. Najwięcej
    daje samodzielna ocena zbioru według listy kontrolnej przed
    przeczytaniem werdyktu."),

  lc_chapter_next(
    num = "01",
    title = "Katalog problemów",
    lead = "Siedem typowych problemów w danych i to, jak rozpoznać je w tabeli i na wykresie.",
    target_id = "ch1"
  )
))

# Pasek oceny listy kontrolnej liczy przeglądarka (modules/figures.js, rycina s08_checklist).
ch0_server <- function(input, output, session) {}
