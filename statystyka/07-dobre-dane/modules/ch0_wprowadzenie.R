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

  lc_h2("sec-01", "Od pomysłu do danych"),

  lc_p("W poprzednich wykładach dane były gotowe: ankieta studentów, okręgi
    szkolne z Kalifornii, pingwiny z Antarktydy. Wiedzieliśmy, które zmienne
    są ilościowe, a które jakościowe, i mogliśmy od razu liczyć średnie,
    przedziały ufności, testy i regresję. We własnym projekcie kolejność jest
    odwrotna. Najpierw trzeba wiedzieć, co chcemy zbadać, a dopiero potem
    szukać danych, które na to pytanie odpowiedzą."),

  lc_p("Każda analiza zaczyna się więc od pomysłu, jeszcze zanim otworzymy
    jakikolwiek plik. Szukamy związku między dwiema zmiennymi? Porównujemy
    grupy? Sprawdzamy, czy coś zmienia się w czasie? Na tym etapie nie
    potrzebujemy formalnej hipotezy statystycznej, wystarczy jasno opisany
    pomysł w zwykłym języku. Z niego wyprowadzimy później ",
    gloss("hipoteza badawcza", "hipotezy"), " i hipotezy statystyczne,
    tak jak w wykładzie 04."),

  lc_p("Warto wybierać tematy, które naprawdę Cię interesują. Kto rozumie
    kontekst, zadaje lepsze pytania, szybciej zauważa absurdalny wynik
    i łatwiej formułuje sensowne hipotezy. Znajomość dziedziny daje analizie
    niuans, którego nie zastąpi żaden podręcznik statystyki."),

  lc_h2("sec-02", "Drugi krok: dane"),

  lc_p("Gdy pomysł jest gotowy, trzeba znaleźć albo zebrać dane. Tu pojawia
    się pierwsza pułapka: nie każdy zbiór nadaje się do planowanej analizy.
    Jedne problemy dyskwalifikują dane od razu i żadna metoda ich nie
    naprawi. Inne wymagają pracy, ale po oczyszczeniu dane nadal są
    użyteczne. Ten wykład uczy odróżniać jedne od drugich."),

  lc_p("Zanim przejdziesz dalej, zastanów się przez chwilę, co warto sprawdzić
    najpierw po otwarciu nieznanego zbioru danych i co może w nim pójść
    nie tak. Porównaj potem swoją listę z katalogiem z następnego rozdziału."),

  lc_h2("sec-03", "Plan wykładu"),

  lc_p("Rozdział 1 to katalog siedmiu typowych problemów w danych. Każdy
    pokazujemy na małym przykładzie: jak wygląda w tabeli, jak na wykresie,
    czym grozi w analizie i co można z nim zrobić. Katalog kończy lista
    kontrolna, która zbiera wszystkie kryteria w jednym miejscu."),

  lc_p("Rozdziały 2–11 to dziesięć zbiorów danych do samodzielnej oceny.
    Każdy ma ten sam układ: opis zbioru, podgląd danych, eksploracja
    i werdykt. Część zbiorów jest wzorcowa, część ma usterki do naprawienia,
    a część nie nadaje się do klasycznej analizy statystycznej. Najwięcej
    skorzystasz, jeśli przed przeczytaniem werdyktu ocenisz zbiór
    samodzielnie według listy kontrolnej."),

  lc_p("Rozdział 12 to ściąga: lista kontrolna, podsumowanie dziesięciu zbiorów
    i zestawienie, jakich danych wymagają metody poznane w wykładach 01–06."),

  lc_chapter_next(
    num = "01",
    title = "Katalog problemów",
    lead = "Siedem typowych problemów w danych i to, jak rozpoznać je w tabeli i na wykresie.",
    target_id = "ch1"
  )
))

ch0_server <- function(input, output, session) {

  # Pasek oceny dla listy kontrolnej z rozdziału 1 (inputy intro_critical, intro_fixable).
  output$intro_thermometer <- renderUI({
    n_critical <- length(input$intro_critical)
    n_fixable <- length(input$intro_fixable)
    n_total <- n_critical + n_fixable
    pct <- n_total / 9 * 100

    # Zasada z rozdziału 1: każde niespełnione kryterium krytyczne dyskwalifikuje
    # zbiór; naprawialne decydują tylko o tym, czy potrzebne jest czyszczenie.
    if (n_total == 0) {
      color <- "var(--upwr-reference)"
      verdict <- "info"
      label <- "Zaznacz kryteria, które spełnia oceniany zbiór."
    } else if (n_critical < 6) {
      color <- data_bad
      verdict <- "danger"
      label <- "Zbiór nie nadaje się do zaplanowanej analizy: poszukaj innego zbioru albo zbierz nowe dane."
    } else if (n_fixable < 3) {
      color <- data_mixed
      verdict <- "warning"
      label <- "Zbiór wymaga czyszczenia i przygotowania przed analizą."
    } else {
      color <- data_good
      verdict <- "ok"
      label <- "Dane gotowe do analizy."
    }

    tagList(
      div(style = "background: var(--upwr-rule); border-radius: 10px; height: 30px; margin-top: 15px;",
        div(style = paste0("background: ", color, "; height: 30px; border-radius: 10px; width: ", pct, "%;
                            transition: width 0.3s; text-align: center; line-height: 30px; color: white; font-weight: bold;"),
          paste0(n_total, "/9")
        )
      ),
      lc_status(lc_verdict(tags$strong(label), type = verdict)),
      if (n_critical < 6 && n_fixable > 0)
        lc_caption("Naprawialne kryteria nie ratują krytycznych problemów.")
    )
  })
}
