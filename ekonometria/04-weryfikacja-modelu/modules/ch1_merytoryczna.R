# ============================================================================
# ROZDZIAŁ 1: Weryfikacja merytoryczna
# ============================================================================

ch1_ui <- lecture_chapter(
  id = "ch-merytoryczna",
  num = "01",
  title = "Weryfikacja merytoryczna",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 04 · Weryfikacja",
      num = "01",
      title = "Czy ten model w ogóle ma sens?",
      lead = "Zanim spojrzymy na p-wartość, musimy zadać prostsze pytanie: czy ten model w ogóle ma sens ekonomiczny? Czasem statystyka mówi 'wszystko OK', a model po prostu opisuje bzdurę."
    ),

    lc_h2("ch1-checklist", "Cztery pytania kontrolne"),
    lc_p("Weryfikacja merytoryczna to czytanie modelu oczami ekonomisty, nie statystyka. Cztery pytania, które warto zadać zawsze, zanim ktoś pochwali się R²."),

    tagList(
      p(strong("1. Znak parametru:"), " Czy znak jest zgodny z teorią? Jeśli model ", tags$em("popyt ~ cena"), " daje β₁ > 0, to mamy sygnał alarmowy — popyt zwykle maleje z ceną. Może model jest źle wyspecyfikowany, może w danych jest coś dziwnego (np. dobra luksusowe), a może po prostu pomyliliśmy zmienne. Tak czy inaczej — trzeba zatrzymać się i pomyśleć."),
      p(strong("2. Skala efektu:"), " Czy wartość β₁ ma sens przy jednostkach zmiennych? Jeśli X to udział w przedziale 0–1, a β₁ = 50, to znaczy: wzrost o 1 punkt procentowy daje +0.5 jednostki Y, ale wzrost o 1 ", tags$em("całą jednostkę"), " (czyli ze 0% do 100%) daje +50. Może być sensowne, może nie — zawsze sprawdzaj jednostki."),
      p(strong("3. Pominięte zmienne:"), " Czy nie brakuje oczywistego czynnika? Klasyczny przykład: ", tags$em("pensja ~ wzrost"), " — wyjdzie istotny związek, ale prawdziwy mechanizm to płeć, wykształcenie i branża. Wzrost jest tu tylko proxy dla czegoś innego."),
      p(strong("4. Interpretowalność:"), " Czy potrafisz wyjaśnić wynik osobie z branży? Jeśli model ma β dla 'iloraz wieku do kwadratu pierwiastka z dochodu' — coś poszło nie tak. Model, którego nie da się obronić przy stole, nie zostanie też obroniony w raporcie.")
    ),

    lc_h2("ch1-mini-przyklad", "Mini-przykład: kiedy model statystycznie OK, ale merytorycznie zły"),
    lc_p("Pewien analityk dopasował model 'sprzedaż lodów ~ liczba utonięć' i dostał wysokie R² oraz p < 0.001. Statystycznie super. Merytorycznie? Obie zmienne rosną z temperaturą — to klasyczna pomylona zależność (spurious correlation). Trzeba dodać zmienną kontrolną: temperaturę. Po jej uwzględnieniu efekt utonięć na sprzedaż lodów znika."),

    inline_callout(
      label = "Zapamiętaj",
      color = "wskazowka",
      open = TRUE,
      "Weryfikacja merytoryczna idzie PRZED statystyczną. Jeśli model nie ma sensu ekonomicznego, żadna p-wartość tego nie naprawi."
    ),

    lc_chapter_next(
      num = "02",
      title = "Miary dopasowania",
      lead = "R², SE reszt i diagnoza wzrokowa",
      target_id = "ch-dopasowanie"
    )
  )
)

ch1_server <- function(input, output, session) {
  # Rozdział 1 nie ma elementów dynamicznych.
}
