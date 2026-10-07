# Blok 01: Język ryzyka ---------------------------------------------------

jezyk_quiz <- list(
  intro = "Pytania układają się w tej samej kolejności co wykład. Pierwsze dotyczy
    ról z definicji 1.1, kolejne — różnicy między częstością z krótkiej serii
    a prawdopodobieństwem modelowym, warunków definicji klasycznej (wzór 1.2) oraz
    reguły sumy (wzór 1.5). Ostatnie sprawdza, czy prawdopodobieństwo nie
    zostaje pomylone z pełnym opisem ryzyka. Odpowiadaj najpierw bez zaglądania
    do wcześniejszych rozdziałów; omówienie pojawi się po sprawdzeniu.",
  outro = "Jeśli błędy skupiły się na jednym typie pytań, wróć do odpowiedniego
    miejsca skryptu. Pomyłki w rolach i opisie zdarzenia wskazują na
    rozdział 01 i przykład 1.1. Pomyłki przy krótkiej serii bez zdarzeń — na
    rozdział 02 i uwagę o prawie wielkich liczb. Pomyłki w rachunku na
    zbiorach najlepiej rozwiązać, licząc kwadraty na siatce ze stu kontroli w
    rozdziale 04: każdy wzór tego wykładu da się tam sprawdzić ręcznie.",
  questions = list(
    list(
      question = "Skórka leżąca na przejściu jest przede wszystkim:",
      choices = c("zagrożeniem" = "a", "skutkiem" = "b", "prawdopodobieństwem" = "c", "zdarzeniem szczytowym" = "d"),
      correct = "a",
      explanation = "Skórka jest źródłem możliwości szkody. Upadek byłby zdarzeniem, a uraz jego skutkiem."
    ),
    list(
      question = "W 20 zmianach nie było poślizgnięcia. Co można z tego bezpiecznie wywnioskować?",
      choices = c(
        "prawdopodobieństwo zdarzenia wynosi dokładnie zero" = "a",
        "w tej serii częstość wyniosła zero, ale modelowe prawdopodobieństwo nie musi być zerowe" = "b",
        "zabezpieczenia są doskonałe" = "c",
        "zdarzenie jest niemożliwe w przyszłości" = "d"
      ),
      correct = "b",
      explanation = "Częstość z małej serii jest zmienna. Brak zdarzeń w obserwacji nie dowodzi niemożliwości zdarzenia."
    ),
    list(
      question = "Kiedy klasyczna definicja P(A) = |A| / |Ω| jest uzasadniona?",
      choices = c(
        "zawsze, gdy znamy liczbę awarii" = "a",
        "gdy wyniki są skończone i jednakowo możliwe" = "b",
        "tylko dla zdarzeń niepożądanych" = "c",
        "gdy próba ma co najmniej 100 obserwacji" = "d"
      ),
      correct = "b",
      explanation = "Liczenie przypadków sprzyjających wymaga dobrze określonej, skończonej przestrzeni jednakowo możliwych wyników."
    ),
    list(
      question = "W 100 kontrolach zdarzenie A wystąpiło 30 razy, B — 20 razy, a oba naraz — 8 razy. Ile razy wystąpiło A lub B?",
      choices = c("42" = "a", "50" = "b", "58" = "c", "8" = "d"),
      correct = "a",
      explanation = "30 + 20 - 8 = 42. Część wspólna została uwzględniona w obu licznikach, więc odejmujemy ją raz."
    ),
    list(
      question = "Zdarzenia A i B mają takie samo prawdopodobieństwo. Czy wymagają automatycznie tego samego priorytetu?",
      choices = c(
        "tak, identyczne prawdopodobieństwo oznacza identyczne ryzyko" = "a",
        "tak, jeśli wyrażono je procentowo" = "b",
        "nie, mogą różnić się skutkami, ekspozycją, barierami i niepewnością" = "c",
        "nie, ponieważ prawdopodobieństwo nigdy nie jest użyteczne" = "d"
      ),
      correct = "c",
      explanation = "Priorytet działania wymaga profilu szerszego niż sama wartość prawdopodobieństwa."
    )
  )
)

.required_report_fields <- c("definition", "exposure", "period", "consequence")

# Ćwiczenia 1–4 są interaktywne (ocena w jezyk_cwiczenia_server), 5–7 mają
# zwinięte odpowiedzi, a 8–12 to pytania przeniesione z dawnego, dłuższego quizu.
jezyk_exercises <- list(
  list(task = tagList(
    tags$p(tags$strong("Uzupełnij informację przed decyzją:"), " notatka dla dyrektora — czego brakuje?"),
    tags$p(
      "Zaznacz dane, których potrzebujesz, aby porównać częstość, a następnie
       pełne ryzyko obu magazynów. Wszystkie wybrane informacje powinny mieć tę
       samą definicję po obu stronach porównania."
    ),
    tags$div(class = "lc-choices",
      checkboxGroupInput(
        "ch8_fields",
        "Czego brakuje?",
        choices = c(
          "Jednoznacznej definicji zdarzenia i zasad rejestracji" = "definition",
          "Liczby porównywalnych ekspozycji, np. przejść lub pracownikogodzin" = "exposure",
          "Wspólnego okresu i informacji o warunkach pracy" = "period",
          "Informacji o rodzaju oraz dotkliwości skutków" = "consequence"
        )
      )
    ),
    tags$div(class = "lc-choices", `data-correct` = "insufficient", `data-reveal` = "ch8_check",
      radioButtons(
        "ch8_conclusion",
        "Który wniosek jest teraz uzasadniony?",
        choices = c(
          "Bananpol jest bezpieczniejszy, bo 3 < 5" = "safer",
          "Magazyny są równie bezpieczne" = "equal",
          "Na podstawie samych liczników nie da się ich porównać" = "insufficient"
        ),
        selected = character(0)
      )
    ),
    lc_action("ch8_check", "Sprawdź rekomendację", variant = "solid"),
    uiOutput("ch8_feedback")
  )),
  list(task = tagList(
    tags$p(tags$strong("Policz działania na zdarzeniach:"), " sto kontroli rampy."),
    tags$p(
      "W 100 kontrolach rampy zdarzenie A — zastawione przejście — wystąpiło
       28 razy. Zdarzenie B — brak oznakowania — wystąpiło 17 razy. Oba
       zdarzenia wystąpiły jednocześnie 6 razy."
    ),
    tags$ol(
      tags$li("Ile kontroli należało do A ∩ B?"),
      tags$li("Ile kontroli należało do A ∪ B?"),
      tags$li("Ile kontroli nie należało ani do A, ani do B?"),
      tags$li("Czy A i B są rozłączne? Uzasadnij jednym zdaniem.")
    ),
    lc_action("ch8_sets_solution", "Pokaż tok rozwiązania", variant = "solid"),
    uiOutput("ch8_sets_feedback")
  )),
  list(task = tagList(
    tags$p(tags$strong("Rozpoznaj punkt startu:"), " nie każdy ułamek znaczy to samo."),
    tags$p(
      "Dla każdej sytuacji wybierz: definicja klasyczna, częstość empiryczna
       albo potrzeba dalszego modelu i danych."
    ),
    selectInput(
      "ch8_model_1",
      "1. Spośród 30 ponumerowanych palet losujemy jedną; 4 mają uszkodzone zabezpieczenie.",
      choices = c(
        "— wybierz —" = "",
        "Definicja klasyczna" = "classical",
        "Częstość empiryczna" = "empirical",
        "Dalszy model i dane" = "model"
      )
    ),
    selectInput(
      "ch8_model_2",
      "2. W rejestrze 8 ze 100 porównywalnych zmian zawierało zdarzenie.",
      choices = c(
        "— wybierz —" = "",
        "Definicja klasyczna" = "classical",
        "Częstość empiryczna" = "empirical",
        "Dalszy model i dane" = "model"
      )
    ),
    selectInput(
      "ch8_model_3",
      "3. Chcemy przewidzieć jutrzejsze ryzyko przy deszczu, większym ruchu i nowej procedurze sprzątania.",
      choices = c(
        "— wybierz —" = "",
        "Definicja klasyczna" = "classical",
        "Częstość empiryczna" = "empirical",
        "Dalszy model i dane" = "model"
      )
    ),
    lc_action("ch8_models_check", "Sprawdź dobór", variant = "solid"),
    uiOutput("ch8_models_feedback")
  )),
  list(task = tagList(
    tags$p(tags$strong("Przenieś język poza Bananpol:"), " alarm gazowy w laboratorium."),
    tags$p(
      "W dwóch zdaniach zdefiniuj zagrożenie, ekspozycję, zdarzenie, skutek i
       zabezpieczenie dla sytuacji: czujnik sygnalizuje wzrost stężenia gazu
       w laboratorium, w którym pracują trzy osoby. Dodaj jednostkę i okres,
       względem których można byłoby obserwować częstość zdarzenia."
    ),
    textAreaInput(
      "ch8_transfer_text",
      "Twoja odpowiedź",
      rows = 6,
      placeholder = "Zagrożenie: ... Ekspozycja: ... Zdarzenie: ..."
    ),
    lc_action("ch8_transfer_rubric", "Pokaż kryteria samooceny", variant = "solid"),
    uiOutput("ch8_transfer_feedback")
  )),
  list(
    task = "Audytor losuje jedną zmianę z 15 par dzień–zmiana (przykład 1.3).
      C — wylosowano zmianę ranną, D — wylosowano poniedziałek lub wtorek.
      Oblicz P(C ∪ D) oraz prawdopodobieństwo, że nie zaszło ani C, ani D.",
    answer = c(
      "|C| = 5, |D| = 2 · 3 = 6, C ∩ D = {(pon, ranna), (wt, ranna)}, więc |C ∩ D| = 2.",
      "Ze wzoru (1.5): P(C ∪ D) = 5/15 + 6/15 - 2/15 = 9/15 = 0.6.",
      "Z praw de Morgana (1.6) i wzoru (1.4): P(Cᶜ ∩ Dᶜ) = 1 - 0.6 = 0.4. Sprawdzenie: 3 dni (śr–pt) × 2 zmiany nieranne = 6 wyników, 6/15 = 0.4."
    )
  ),
  list(
    task = "W arkuszu oceny czujnika gazu w chłodni wpisano prawdopodobieństwa
      czterech wyników, które wykluczają się i wyczerpują wszystkie możliwości
      w ciągu jednej zmiany: brak alarmu 0.70; alarm fałszywy 0.20; alarm
      prawdziwy 0.15; awaria czujnika 0.02. Czy takie przypisanie jest
      dopuszczalne?",
    answer = c(
      "Nie. Wyniki są rozłączne i razem tworzą Ω, więc z aksjomatów (1.7) ich prawdopodobieństwa muszą sumować się do P(Ω) = 1. Tymczasem 0.70 + 0.20 + 0.15 + 0.02 = 1.07.",
      "Arkusz jest wewnętrznie sprzeczny niezależnie od danych: co najmniej jedna wartość jest błędna. Trzeba wrócić do źródła każdej liczby, a nie „przeskalować” wszystkie tak, żeby suma wyszła 1."
    )
  ),
  list(
    task = "W rejestrze korytarza przy pakowni 12 ze 150 zmian zawierało
      poślizgnięcie. Oblicz częstość empiryczną. Przyjmując p = 0.08, oszacuj
      typowe odchylenie częstości w serii 150 zmian i oceń, czy wynik różny o
      0.01 od poprzedniego roku jest mocnym sygnałem zmiany.",
    answer = c(
      "Ze wzoru (1.1): p̂ = 12/150 = 0.08.",
      "Typowe odchylenie: √(0.08 · 0.92 / 150) ≈ 0.022. Różnica 0.01 jest ponad dwa razy mniejsza niż typowe wahanie częstości przy tej liczbie zmian, więc nie jest mocnym sygnałem zmiany. Potrzeba dłuższej serii albo informacji o zmianie warunków."
    )
  ),
  list(
    task = list(
      "Oddział A zgłosił 3 poślizgnięcia, a oddział B — 5. Czego przede wszystkim brakuje do porównania częstości?",
      risk_parts("nazwiska kierownika", "liczby porównywalnych ekspozycji i okresu obserwacji", "koloru posadzki", "średniej wieku pracowników")
    ),
    answer = c(
      "b) liczby porównywalnych ekspozycji i okresu obserwacji.",
      "Same liczniki nie wystarczają. Potrzebujemy mianownika, jednostki ekspozycji i wspólnego okresu."
    )
  ),
  list(
    task = list(
      "Który opis zdarzenia jest najbardziej użyteczny do obliczeń?",
      risk_parts("w magazynie jest niebezpiecznie", "banany bywają śliskie", "co najmniej jeden upadek w korytarzu podczas jednej 8-godzinnej zmiany", "pracownicy powinni uważać")
    ),
    answer = c(
      "c) co najmniej jeden upadek w korytarzu podczas jednej 8-godzinnej zmiany.",
      "Zdarzenie jest obserwowalne, ma miejsce, jednostkę i horyzont czasu."
    )
  ),
  list(
    task = list(
      "Prawdopodobieństwo upadku opisuje pełne ryzyko dla pracownika:",
      risk_parts("tak, zawsze", "nie, trzeba jeszcze uwzględnić możliwe skutki i kontekst decyzji", "tak, jeśli wynik podamy w procentach", "nie, ponieważ prawdopodobieństwo nie ma znaczenia")
    ),
    answer = c(
      "b) nie, trzeba jeszcze uwzględnić możliwe skutki i kontekst decyzji.",
      "Prawdopodobieństwo jest ważną składową analizy, ale nie opisuje samo dotkliwości ani rodzaju skutków."
    )
  ),
  list(
    task = list(
      "Dwa niezerowe zdarzenia rozłączne są automatycznie niezależne:",
      risk_parts("tak, ponieważ nie zachodzą razem", "nie, zajście jednego wyklucza drugie, więc zmienia o nim informację", "tak, jeśli mają takie samo prawdopodobieństwo", "nie da się tego rozstrzygnąć")
    ),
    answer = c(
      "b) nie, zajście jednego wyklucza drugie, więc zmienia o nim informację.",
      "Rozłączność oznacza pustą część wspólną. Gdy jedno zdarzenie zaszło, drugie na pewno nie zaszło — to silna zależność."
    )
  ),
  list(
    task = list(
      "Jeśli P(A) = 0.18, to P(Aᶜ) wynosi:",
      risk_parts("0.18", "0.82", "1.18", "nie da się obliczyć")
    ),
    answer = c(
      "b) 0.82.",
      "A i jego dopełnienie wyczerpują całą przestrzeń, dlatego P(Aᶜ) = 1 - 0.18 = 0.82."
    )
  )
)

jezyk_sciaga_widget <- tagList(
  risk_assessment_ui("j1", jezyk_quiz, jezyk_exercises, exercises_intro = c(
    "Dyrektor Bananpolu dostał komunikat: „W czerwcu mieliśmy trzy
     wypadki, a drugi magazyn pięć, więc jesteśmy bezpieczniejsi”.
     Twoim zadaniem jest zatrzymać zbyt szybki wniosek.",
    "Ćwiczenia sprawdzają trzy umiejętności z tego wykładu. Pierwsza to
     zatrzymanie wniosku, który opiera się na samym liczniku — to problem
     mianownika z rozdziału 02. Druga to rachunek na zdarzeniach według wzorów
     (1.2)–(1.6). Trzecia to rozpoznanie, skąd w danej sytuacji bierze się
     prawdopodobieństwo: z symetrii, z rejestru czy dopiero z modelu. Zadania 5–12
     mają odpowiedzi zwinięte pod treścią."
  )),
  lc_h2("jezyk-sprawdzenie-wzorzec", "Wzorzec poprawionego komunikatu"),
  lc_note("Przykład",
    "„W czerwcu magazyn A zgłosił 3 zdarzenia, a magazyn B — 5. Przed
      porównaniem potrzebujemy wspólnej definicji zdarzenia, porównywalnej
      ekspozycji i danych o skutkach. Same liczniki nie uzasadniają rankingu
      bezpieczeństwa.”"
  ),
  lc_h2("jezyk-sprawdzenie-most", "Co zmieni dodatkowa informacja?"),
  lc_p(
    "W tym wykładzie ustaliliśmy mianownik i język zdarzeń. W następnym
     sprawdzimy, jak informacja o warunkach — mokrej posadzce, natężeniu ruchu
     albo niesprawnym sprzątaniu — zmienia ocenę prawdopodobieństwa."
  ),
  lc_note("Pytanie",
    "Jakiego jednego zdania zabrakło w ostatnim raporcie o bezpieczeństwie,
      który czytałeś lub przygotowywałeś?"
  )
)

# Schemat łańcucha z definicji 1.1: cztery ogniwa w rzędzie, a pod nimi
# zabezpieczenie, które przecina drogę przed zdarzeniem albo po nim. Kliknięcie
# pojęcia pokazuje pod schematem jego definicję i pytanie kontrolne ze ściągi.
# Celowo bez przykładów z korytarza — to odpowiedzi do ćwiczenia z kartami.
jezyk_chain_terms <- list(
  hazard = list(
    color = upwr_cat[["terakota"]], caption = c("Źródło możliwej", "szkody"),
    definition = "Zagrożenie to źródło lub stan, który może spowodować szkodę.",
    question = "Co może spowodować szkodę?"
  ),
  exposure = list(
    color = upwr_cat[["bursztyn"]], caption = c("Kontakt w danych", "warunkach"),
    definition = "Ekspozycja to kontakt osób albo mienia z zagrożeniem w określonych warunkach i przez określony czas.",
    question = "Kto lub co ma kontakt z zagrożeniem?"
  ),
  event = list(
    color = upwr_accent, caption = c("To, co faktycznie", "zaszło"),
    definition = "Zdarzenie to obserwowalny wynik, który w danym okresie zachodzi albo nie zachodzi.",
    question = "Co dokładnie ma zajść?"
  ),
  consequence = list(
    color = upwr_cat[["wrzos"]], caption = c("Następstwo", "zdarzenia"),
    definition = "Skutek to następstwo zdarzenia, opisane rodzajem i dotkliwością.",
    question = "Jakie może być następstwo?"
  ),
  safeguard = list(
    color = upwr_cat[["szalwia"]], caption = c("Element przerywający", "łańcuch"),
    definition = "Zabezpieczenie (bariera) to element, który przerywa drogę od zagrożenia do zdarzenia albo od zdarzenia do skutku.",
    question = "Co przerywa drogę do szkody?"
  )
)

jezyk_chain_svg <- function() {
  node <- function(key, x, y, w, h) {
    term <- jezyk_chain_terms[[key]]
    cx <- x + w / 2
    sprintf(
      '<g class="lc-chain-node" data-key="%s" tabindex="0" role="button" aria-label="%s"
          style="--node-color:%s"
          onclick="lcChainSelect(this)" onkeydown="if(event.key===\'Enter\'||event.key===\' \'){event.preventDefault();lcChainSelect(this);}">
         <rect x="%d" y="%d" width="%d" height="%d" rx="6"/>
         <text class="lc-chain-name" x="%g" y="%d">%s</text>
         <text class="lc-chain-caption" x="%g" y="%d">%s</text>
         <text class="lc-chain-caption" x="%g" y="%d">%s</text>
       </g>',
      key, risk_term_labels[[key]], term$color, x, y, w, h,
      cx, y + 44, risk_term_labels[[key]],
      cx, y + 72, term$caption[[1]], cx, y + 92, term$caption[[2]]
    )
  }
  arrow <- function(x1, x2, y) {
    sprintf('<line class="lc-chain-arrow" x1="%d" y1="%d" x2="%d" y2="%d" marker-end="url(#lc-chain-head)"/>', x1, y, x2, y)
  }
  # Linia od zabezpieczenia do strzałki, zakończona poprzeczką „przerwania”.
  barrier <- function(x_from, x_to, label, anchor) {
    label_x <- if (anchor == "end") x_to - 12 else x_to + 12
    sprintf(
      '<path class="lc-chain-link" d="M%d 238 C %d 200, %d 190, %d 92"/>
       <line class="lc-chain-cut" x1="%d" y1="50" x2="%d" y2="82"/>
       <text class="lc-chain-where" x="%d" y="150" style="text-anchor:%s">%s</text>',
      x_from, x_from, x_to, x_to, x_to, x_to, label_x, anchor, label
    )
  }
  HTML(paste0(
    '<svg class="lc-chain" viewBox="0 0 960 340" role="group" aria-label="Łańcuch od zagrożenia do skutku">
       <defs><marker id="lc-chain-head" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto">
         <path d="M0 0 L10 5 L0 10 z" class="lc-chain-headfill"/></marker></defs>',
    arrow(212, 246, 66), arrow(457, 491, 66), arrow(702, 736, 66),
    barrier(400, 474, "przed zdarzeniem", "end"), barrier(560, 719, "po zdarzeniu", "start"),
    node("hazard", 10, 10, 200, 112), node("exposure", 255, 10, 200, 112),
    node("event", 500, 10, 200, 112), node("consequence", 745, 10, 200, 112),
    node("safeguard", 360, 228, 240, 104),
    '</svg>'
  ))
}

jezyk_chain_widget <- figure_panel(
  label = "Schemat 1.1",
  title = "Łańcuch od zagrożenia do skutku",
  full_width = TRUE,
  jezyk_chain_svg(),
  tags$p(class = "lc-chain-hint", "Kliknij pojęcie, aby zobaczyć jego definicję i pytanie kontrolne."),
  uiOutput("ch1_chain_detail"),
  tags$script(HTML(
    "function lcChainSelect(el) {
       el.closest('svg').querySelectorAll('.lc-chain-node').forEach(function(n) {
         n.classList.toggle('is-active', n === el);
       });
       Shiny.setInputValue('ch1_chain_click', el.dataset.key, {priority: 'event'});
     }"
  ))
)

jezyk_block <- list(
  id = "jezyk", title = "Język ryzyka",
  chapters = list(
    list(
      id = "sytuacja", title = "Łańcuch ryzyka", hook = "Skórka to jeszcze nie wypadek",
      lead = "W korytarzu Bananpolu znaleziono skórkę od banana. Brzmi jak
        dowcip, ale pozwala precyzyjnie oddzielić zagrożenie, ekspozycję,
        zdarzenie, skutek i zabezpieczenie.",
      sections = list(
        list(
          id = "bananpol", title = "Witamy w Bananpolu",
          body = list(
            "Bananpol jest fikcyjnym importerem bananów. Firma ma rampę rozładunkową,
               dojrzewalnię z chłodnią, magazyn wysokiego składowania, linię sortowania
               i pakowania, wózki widłowe oraz instalację chłodniczą z alarmami.
               Właśnie zaczynasz tu pracę jako inspektor bezpieczeństwa.",
            "Przez całą serię wykładów będziesz uzupełniać mapę ryzyka tej firmy: od
               dzisiejszej skórki na korytarzu, przez alarmy i awarie urządzeń, aż po
               drzewo błędów całej instalacji w finale. Każde nowe pojęcie dostanie
               swoje miejsce w tym samym zakładzie, więc wyniki z kolejnych wykładów
               będą do siebie pasować.",
            lc_note("Dane fikcyjne",
              "Wszystkie liczby w kursie są wymyślone na potrzeby dydaktyki i nie
               opisują żadnej prawdziwej firmy. Prawdziwe są tylko metody."
            ),
            lc_note("Pytanie na start",
              "Czy obecność skórki oznacza, że doszło do wypadku? Najpierw odpowiedz
               intuicyjnie, dopiero potem uporządkuj historię."
            ),
            "Pierwszy dzień pracy zaczyna się od notatki z porannego obchodu:
               „skórka od banana na korytarzu przy dojrzewalni, ryzyko wypadku”.
               Notatka brzmi rozsądnie, ale nie da się z nią nic policzyć. Nie wiadomo,
               czy ktoś już się poślizgnął, ile osób tamtędy chodzi, jak długo skórka
               leżała ani czym mogłoby się skończyć ewentualne potknięcie. Słowo
               „ryzyko” skleja tu kilka różnych rzeczy, a każda z nich wymaga innych
               danych i innego działania.",
            "Dlatego zanim pojawi się pierwszy wzór, rozkładamy historię na części.
               Rachunek prawdopodobieństwa dotyczy zdarzeń, czyli precyzyjnie
               opisanych wyników obserwacji. Jeżeli zdarzenie jest opisane mgliście,
               żaden wzór tego nie naprawi: dwie osoby policzą dwie różne liczby i obie
               będą miały rację względem swojej definicji."
          )
        ),
        list(
          id = "slownik", title = "Pięć różnych elementów jednej historii",
          body = list(
            "W analizie ryzyka podobne słowa bywają używane zamiennie. Tutaj każde
               ma osobną rolę. Zagrożenie może spowodować szkodę, ekspozycja tworzy
               kontakt z zagrożeniem, zdarzenie opisuje to, co zaszło, a skutek mówi o
               następstwie. Zabezpieczenie ma przerwać ten łańcuch.",
            risk_definition("1.1", "Łańcuch od zagrożenia do skutku", c(
              "Zagrożenie to źródło lub stan, który może spowodować szkodę. Ekspozycja
               to kontakt osób albo mienia z zagrożeniem w określonych warunkach i przez
               określony czas. Zdarzenie to obserwowalny wynik, który w danym okresie
               zachodzi albo nie zachodzi. Skutek to następstwo zdarzenia, opisane
               rodzajem i dotkliwością. Zabezpieczenie (bariera) to element, który
               przerywa drogę od zagrożenia do zdarzenia albo od zdarzenia do skutku.",
              "Prawdopodobieństwo będziemy przypisywać wyłącznie zdarzeniom. Zagrożenie
               samo w sobie nie ma prawdopodobieństwa — ma je dopiero konkretne
               zdarzenie, np. poślizgnięcie się na tym przejściu podczas jednej zmiany."
            )),
            jezyk_chain_widget,
            "Kolejność w tym łańcuchu nie jest przypadkowa. Zagrożenie istnieje, zanim
               ktokolwiek się do niego zbliży; ekspozycja sprawia, że zdarzenie staje
               się w ogóle możliwe; skutek zależy od tego, jak przebiegło zdarzenie.
               Zabezpieczenia mogą działać w dwóch miejscach: przed zdarzeniem (sprzątanie
               usuwa skórkę, zanim ktoś na nią nadepnie) albo po nim (antypoślizgowe
               obuwie czy pierwsza pomoc ograniczają dotkliwość). To rozróżnienie wróci
               w wykładzie 09, gdy będziemy budować drzewo błędów."
          )
        ),
        list(
          id = "klasyfikacja", title = "Uporządkuj incydent Bananpolu",
          body = list(
            "Przypisz każdemu zdaniu jedną rolę. Karty w puli leżą w przypadkowej
               kolejności, więc sama pozycja niczego nie podpowiada.",
            risk_try("dla każdej karty zadaj sobie pytanie z definicji 1.1: czy to
              źródło szkody, kontakt z nim, to, co zaszło, następstwo, czy element
              przerywający łańcuch? Dopiero potem przeciągnij kartę i sprawdź wynik."),
            figure_panel(
              label = "Ćwiczenie 1.1",
              title = "Od zagrożenia do zabezpieczenia",
              full_width = TRUE,
              lc_drop_match(
                input_id = "ch1_assign",
                items = risk_scenario_items[
                  match(risk_scenario_pool_order, risk_scenario_items$id),
                  c("id", "text")
                ],
                zones = risk_term_labels,
                colors = c(
                  upwr_cat[["terakota"]],
                  upwr_cat[["bursztyn"]],
                  upwr_accent,
                  upwr_cat[["wrzos"]],
                  upwr_cat[["szalwia"]]
                ),
                actions = lc_action("ch1_check", "Sprawdź klasyfikację", variant = "solid")
              ),
              uiOutput("ch1_feedback")
            ),
            "Najczęstsze pomyłki dotyczą dwóch par. Skórka bywa brana za zdarzenie,
               bo „coś się stało” — ktoś ją upuścił. Ale z perspektywy bezpieczeństwa
               pracowników skórka jest stanem otoczenia, a zdarzeniem jest dopiero utrata
               przyczepności i upadek. Druga para to zdarzenie i skutek: upadek i
               złamanie nadgarstka to dwie różne rzeczy, bo ten sam upadek może skończyć
               się siniakiem albo niczym. Gdy te role się zlewają, prawdopodobieństwo
               upadku zaczyna udawać miarę dotkliwości — a nią nie jest.",
            figure_panel(
              label = "Ćwiczenie 1.2",
              title = "Ta sama analiza przy rampie",
              full_width = TRUE,
              tags$p("Druga historia z Bananpolu: wózek widłowy przy rampie. Przypisz
                każdemu zdaniu rolę z definicji 1.1."),
              lc_drop_match(
                input_id = "ch1_assign_rampa",
                items = data.frame(id = names(risk_term_labels),
                                   text = unname(risk_term_labels),
                                   stringsAsFactors = FALSE),
                zones = stats::setNames(
                  risk_rampa_items$text[match(risk_rampa_pool_order, risk_rampa_items$id)],
                  risk_rampa_pool_order
                ),
                colors = rep(upwr_reference, length(risk_rampa_pool_order)),
                hint = paste(
                  "Przeciągnij nazwę roli do zdania, które ją opisuje.",
                  "Bez myszy: Enter podnosi kartę, strzałki wybierają zdanie,",
                  "Enter upuszcza, Escape anuluje, Delete odsyła kartę do puli."
                ),
                class = "is-rows",
                actions = lc_action("ch1_check_rampa", "Sprawdź klasyfikację", variant = "solid")
              ),
              uiOutput("ch1_feedback_rampa")
            ),
            risk_check("j1_chk_role",
              "Posadzka przy myjni skrzynek jest mokra przez całą zmianę. Jaką rolę pełni ten fakt w łańcuchu z definicji 1.1?",
              c("Zdarzenie" = "event", "Zagrożenie" = "hazard", "Skutek" = "consequence"),
              correct = "hazard",
              explanation = "Mokra posadzka to stan otoczenia, który może spowodować szkodę. Zdarzeniem byłby dopiero upadek na niej, a skutkiem — uraz. Zagrożenie nie jest zdarzeniem: sama mokra posadzka nie mówi jeszcze, czy ktoś się poślizgnie ani jak poważny będzie uraz.",
              hints = c(
                event = "Czy mokra posadzka to wynik, który „zaszedł albo nie” w konkretnej chwili? Co musiałoby się stać, żeby ktoś ucierpiał?",
                consequence = "Skutek jest następstwem zdarzenia. Jakie zdarzenie musiałoby nastąpić wcześniej?"
              )
            )
          )
        ),
        list(
          id = "proces", title = "Gdzie w procesie mieści się rachunek?",
          body = list(
            "Ustalamy cel i zakres, identyfikujemy zagrożenia i scenariusze, analizujemy ich prawdopodobieństwo oraz skutki, oceniamy wynik według jawnych kryteriów, wdrażamy działanie i sprawdzamy pozostałe ryzyko. Komunikacja z osobami narażonymi i odpowiedzialnymi trwa na każdym etapie. Ten kurs rozwija przede wszystkim część probabilistyczną."
          )
        ),
        list(
          id = "scenariusz", title = "Zanim dostaniesz gotowe drzewo",
          body = list(
            "W parze wybierz zagrożenie przy rozładunku. Zapisz: źródło zagrożenia, zdarzenie inicjujące, osoby narażone, istniejące bariery i dwa możliwe skutki. Dodaj błąd człowieka lub procedury oraz jedną brakującą informację. Oddziel bariery zapobiegające zdarzeniu od tych, które ograniczają skutek po jego wystąpieniu.",
            "Ta część jest ćwiczeniem wstępnym, a nie oceną ryzyka. Chodzi o to,
              żeby przed jakimkolwiek rachunkiem wiedzieć, jakie zdarzenie będziemy
              liczyć i gdzie w łańcuchu działa każda bariera. Kiedy w kolejnych
              rozdziałach zaczniemy przypisywać zdarzeniom liczby, każde z nich musi
              dać się wskazać w takim opisie.",
            "Przykład: uszkodzenie opakowania → wyciek na przejście → poślizgnięcie → brak urazu albo uraz. Kontrola opakowania zapobiega wyciekowi, usunięcie rozlania i odgrodzenie ograniczają kontakt. Sama tabliczka ostrzegawcza zależy od zauważenia i reakcji człowieka. Ponowna kontrola przejścia sprawdza, czy działanie było skuteczne."
          )
        )
      )
    ),
    list(
      id = "czestosc", title = "Częstość empiryczna", hook = "Jeden miesiąc może kłamać",
      lead = "Częstość empiryczna, czyli udział zmian z poślizgnięciem w
        rejestrze, zmienia się od serii do serii. Stabilny wzorzec
        odsłania dopiero wiele porównywalnych okresów.",
      teaser = "Sprawdzimy, dlaczego jeden miesiąc obserwacji potrafi mylić.",
      body = list(
        lc_note("Jednostka obserwacji",
          "Jedna próba oznacza jedną 8-godzinną zmianę w konkretnym korytarzu.
           Zdarzenie rejestrowe: co najmniej jedno poślizgnięcie (utrata
           przyczepności i upadek) podczas tej zmiany. Rejestr zlicza zmiany ze
           zdarzeniem, nie pojedyncze poślizgnięcia."
        )
      ),
      sections = list(
        list(
          id = "mianownik", title = "Najpierw ustal mianownik",
          body = list(
            "Zdanie „były trzy poślizgnięcia” nie pozwala porównać dwóch magazynów.
               Potrzebujemy wiedzieć, w ilu porównywalnych zmianach mogły wystąpić, jak
               zdefiniowano zdarzenie i czy obserwacje dotyczą tych samych warunków.",
            "Mianownik jest tak samo ważny jak licznik, bo zmienia pytanie. Trzy
               poślizgnięcia na 40 zmian to inna sytuacja niż trzy na 400 zmian, choć
               licznik jest identyczny. Równie ważne jest, co liczymy w liczniku: w
               rejestrze Bananpolu zliczamy zmiany, w których doszło do co najmniej
               jednego poślizgnięcia. Zmiana z dwoma upadkami liczy się raz. Taki wybór
               sprawia, że każda obserwacja kończy się jednym z dwóch wyników — zdarzenie
               zaszło albo nie — i że częstość zawsze leży między 0 a 1.",
            risk_definition("1.2", "Częstość empiryczna", c(
              "Niech w n porównywalnych, niezależnie przeprowadzonych obserwacjach
               zdarzenie A zaszło n_A razy. Częstością empiryczną (względną) zdarzenia A
               nazywamy iloraz n_A / n, oznaczany p̂ₙ (czytaj: p z daszkiem).",
              "Częstość jest wynikiem konkretnej serii obserwacji. Inna seria tej samej
               długości da zwykle inną wartość, dlatego piszemy p̂ — to
               oszacowanie, a nie sam parametr modelu."
            )),
            risk_formula(
              "\\widehat{p}_n=\\frac{n_A}{n}=\\frac{\\text{liczba zmian ze zdarzeniem}}{\\text{liczba obserwowanych zmian}}",
              num = "1.1",
              legend = c(
                "n" = "liczba porównywalnych obserwacji (tutaj: zmian)",
                "n_A" = "liczba obserwacji, w których zaszło zdarzenie A",
                "\\widehat{p}_n" = "częstość empiryczna po n obserwacjach"
              )
            ),
            risk_example("1.2", "Który korytarz jest bardziej śliski?",
              problem = c(
                "W korytarzu przy dojrzewalni obserwowano 40 zmian; w 3 z nich doszło
                 do poślizgnięcia. W korytarzu przy pakowni obserwowano 120 zmian; zdarzenie
                 wystąpiło w 5 z nich. Kierownik pakowni twierdzi, że u niego jest gorzej,
                 bo „było więcej wypadków”. Oblicz częstości i oceń ten argument."
              ),
              steps = c(
                "Dojrzewalnia: ze wzoru (1.1) p̂ = 3/40 = 0.075.",
                "Pakownia: p̂ = 5/120 ≈ 0.042.",
                "Licznik jest większy w pakowni, ale mianownik jest trzykrotnie większy.
                 Na zmianę przypada tam mniej zdarzeń.",
                "Porównanie ma sens tylko wtedy, gdy obie serie używają tej samej definicji
                 zdarzenia i tej samej jednostki obserwacji (zmiana w jednym korytarzu)."
              ),
              answer = "0.075 wobec około 0.042 — to dojrzewalnia ma wyższą częstość. Argument
                „więcej wypadków” pomija mianownik. Seria 40 zmian jest jednak krótka, więc
                różnica może częściowo wynikać z przypadku; to sprawdzi symulacja poniżej."
            )
          )
        ),
        list(
          id = "symulacja", title = "Zobacz stabilizację częstości",
          body = list(
            "Aplikacja wylosowała i ukryła modelowe prawdopodobieństwo. Dodawaj kolejne
               fikcyjne zmiany i spróbuj oszacować je na podstawie częstości empirycznej.
               Małe serie mogą wyglądać dramatycznie albo podejrzanie dobrze. Gdy uznasz,
               że danych jest dość, odsłoń wartość przyjętą w modelu.",
            risk_try("ustaw suwak na swoje oszacowanie, zanim zobaczysz dane. Kliknij
              kilka razy „+1” i zapisz częstość po 10 zmianach. Potem dodawaj po
              100 i po 1000 i obserwuj, jak zmienia się zakres wahań linii. Gdy
              uznasz, że danych jest dość, popraw oszacowanie i kliknij „Odsłoń
              i porównaj”. Na koniec kliknij „Nowa seria” i porównaj początek nowej
              linii z poprzednią."),
            figure_panel(
              label = "Ćwiczenie 2",
              title = "Teoria kontra kolejne zmiany w Bananpolu",
              full_width = TRUE,
              lc_toolbar(
                lc_action_group(ch2_add_1 = "+1", ch2_add_10 = "+10",
                                ch2_add_100 = "+100", ch2_add_1000 = "+1 000",
                                label = "Dodaj zmiany"),
                lc_slider("ch2_guess", "Twoje oszacowanie P", 0, 0.30, 0.15, 0.01),
                lc_action("ch2_reveal", "Odsłoń i porównaj", variant = "outline"),
                lc_action("ch2_reset", "Nowa seria", variant = "outline"),
                lc_readouts(uiOutput("ch2_stats"))
              ),
              lc_plots(
                tags$div(
                  tags$h4("Ostatnie 100 zmian"),
                  lc_plot("ch2_grid", ratio = "1/1", max_height = "280px")
                ),
                tags$div(
                  tags$h4("Skumulowana częstość"),
                  lc_plot("ch2_line", ratio = "1.7/1", max_height = "340px")
                )
              ),
              uiOutput("ch2_note"),
              uiOutput("ch2_feedback")
            ),
            "Na początku serii linia skacze gwałtownie: po jednej zmianie częstość
               wynosi 0 albo 1, a po kilku zmianach jedno zdarzenie przesuwa ją o
               kilkanaście punktów procentowych. Z każdą kolejną setką zmian pojedyncza
               obserwacja waży coraz mniej, więc linia uspokaja się i zbliża do poziomu,
               który po odsłonięciu okazuje się modelowym prawdopodobieństwem. Dwie
               różne serie mogą na początku wyglądać zupełnie inaczej, a po tysiącu zmian
               leżą blisko siebie.",
            "To zachowanie ma nazwę: prawo wielkich liczb. Mówi ono, że przy
               niezależnych i porównywalnych obserwacjach częstość empiryczna p̂ₙ z
               coraz większym prawdopodobieństwem leży blisko prawdopodobieństwa p,
               gdy n rośnie. Nie mówi natomiast, że w krótkiej serii częstość będzie
               bliska p, ani że po serii „pechowych” zmian nastąpi seria „szczęśliwych”,
               która wyrówna wynik. Stabilizacja bierze się z rozcieńczania, a nie z
               kompensowania.",
            risk_derivation("jak szybko częstość się stabilizuje", c(
              "Typowe odchylenie częstości p̂ₙ od prawdopodobieństwa p wynosi około
               √(p(1 - p)/n). Wzór wyprowadzimy w wykładzie 04 przy rozkładzie
               dwumianowym; tutaj wystarczy jego skutek.",
              "Przy p = 0.10 typowe odchylenie wynosi około 0.095 po 10 zmianach, 0.030
               po 100 zmianach i 0.0095 po 1000 zmianach. Aby zmniejszyć rozrzut
               dziesięciokrotnie, potrzeba stukrotnie więcej obserwacji. Dlatego seria
               40 zmian z przykładu 1.2 nie wystarcza, by rozstrzygnąć, który korytarz
               jest naprawdę bardziej śliski."
            ), lines = c(
              "n = 10:    √(0.1 · 0.9 / 10)   ≈ 0.095",
              "n = 100:   √(0.1 · 0.9 / 100)  = 0.030",
              "n = 1000:  √(0.1 · 0.9 / 1000) ≈ 0.0095"
            )),
            risk_check("j1_chk_seria",
              "Model przyjmuje P = 0.08 poślizgnięcia na zmianę. W ostatnich 20 zmianach nie było ani jednego zdarzenia. Co z tego wynika?",
              c(
                "Model jest błędny, bo częstość wyniosła 0" = "wrong",
                "Taka seria jest przy P = 0.08 całkiem możliwa; 20 zmian to za mało, by odrzucić model" = "possible",
                "Następne zmiany muszą przynieść więcej zdarzeń, żeby wyrównać średnią" = "compensate"
              ),
              correct = "possible",
              explanation = "Przy niezależnych zmianach seria 20 zmian bez zdarzenia ma prawdopodobieństwo 0.92²⁰ ≈ 0.19 — zdarza się mniej więcej w co piątej takiej serii (rachunek pokażemy w wykładzie 04). Częstość 0 z krótkiej serii nie przeczy modelowi.",
              hints = c(
                wrong = "Częstość z krótkiej serii mocno się waha. Przypomnij sobie początek linii w symulacji.",
                compensate = "Prawo wielkich liczb działa przez rozcieńczanie, nie przez wyrównywanie. Zmiany nie „pamiętają” poprzednich wyników."
              )
            ),
            lc_note("Wniosek",
              "Prawdopodobieństwo jest własnością modelu, a częstość jest wynikiem
                konkretnej serii obserwacji. Nie oczekujemy, że w każdej małej serii
                będą identyczne."
            ),
            lc_warn("Pułapka",
              "Stabilizacja częstości nie naprawia złej definicji zdarzenia, zmiany
                warunków ani błędów rejestracji. Więcej danych nie zastępuje dobrego
                modelu obserwacji."
            ),
            "Częstość empiryczna odpowiada więc na pytanie „jak często to się
               zdarzało w tych obserwacjach?”. Prawdopodobieństwo odpowiada na pytanie
               „jak często spodziewamy się tego w porównywalnych warunkach?”. Przejście od
               pierwszego do drugiego wymaga założenia, że przyszłe zmiany będą podobne do
               obserwowanych. W następnym rozdziale zobaczymy sytuację, w której
               prawdopodobieństwo można przypisać bez żadnych obserwacji — samym
               rozumowaniem o symetrii."
          )
        )
      )
    ),
    list(
      id = "przestrzen", title = "Przestrzeń zdarzeń", hook = "Szansę można znać, zanim coś się stanie",
      lead = "Nie zawsze potrzebujemy rejestru wypadków. Gdy losujemy paletę do
        kontroli, szansę wyznacza sama konstrukcja losowania: wystarczy
        wypisać przestrzeń zdarzeń, czyli wszystkie palety, które mogą
        zostać wybrane, i uzasadnić, że są jednakowo możliwe.",
      teaser = "Zobaczymy, kiedy wolno liczyć przypadki sprzyjające.",
      body = list(
        lc_note("Eksperyment",
          "Inspektor losuje dokładnie jedną z 24 palet. Każda paleta ma własny
           numer w generatorze losowym i tę samą szansę wyboru."
        ),
        "Do Bananpolu przyjechała dostawa 24 palet. Inspektor nie ma czasu
           skontrolować wszystkich, więc losuje jedną i sprawdza zabezpieczenie
           ładunku. Przed losowaniem chce wiedzieć, jak duża jest szansa, że trafi na
           paletę z uszkodzonym zabezpieczeniem, jeśli w dostawie jest ich sześć.
           Tym razem nie potrzebujemy rejestru z poprzednich miesięcy: odpowiedź
           wynika z samej konstrukcji losowania. Żeby ją zapisać porządnie,
           potrzebujemy trzech pojęć — doświadczenia, przestrzeni i zdarzenia."
      ),
      sections = list(
        list(
          id = "przestrzen", title = "Wyniki i zdarzenia",
          body = list(
            risk_definition("1.3", "Doświadczenie losowe i przestrzeń zdarzeń elementarnych", c(
              "Doświadczenie losowe to procedura, którą można (przynajmniej w myśli)
               powtarzać w tych samych warunkach i której wyniku nie znamy z góry.
               Każdy możliwy, niepodzielny wynik doświadczenia nazywamy zdarzeniem
               elementarnym i oznaczamy ω.",
              "Zbiór wszystkich zdarzeń elementarnych to przestrzeń zdarzeń
               elementarnych Ω. Musi ona spełniać dwa warunki: każdy przebieg
               doświadczenia kończy się jakimś elementem Ω (wyczerpywalność) i nigdy
               dwoma naraz (wzajemne wykluczanie)."
            )),
            "Przestrzeń wyników zawiera wszystkie palety, które mogą zostać wybrane.
               Zdarzenie A jest podzbiorem: paletami z uszkodzonym zabezpieczeniem
               ładunku. Losujemy paletę, a nie uszkodzenie.",
            risk_definition("1.4", "Zdarzenie losowe", c(
              "Zdarzeniem losowym nazywamy podzbiór A przestrzeni Ω. Mówimy, że
               zdarzenie A zaszło, jeśli wynik doświadczenia ω należy do A (ω ∈ A).",
              "Szczególne przypadki: zdarzenie elementarne {ω} zawiera jeden wynik;
               zdarzenie pewne to cała przestrzeń Ω — zachodzi zawsze; zdarzenie
               niemożliwe to zbiór pusty ∅ — nie zachodzi nigdy. Liczbę elementów
               zdarzenia A oznaczamy |A|."
            )),
            "W losowaniu palety Ω = {1, 2, …, 24}, czyli |Ω| = 24. Jeśli uszkodzone
               zabezpieczenie mają palety 1–6, to A = {1, 2, 3, 4, 5, 6} i |A| = 6.
               Zauważ, że zdarzenie nie jest „rzeczą, która się dzieje”, tylko zbiorem
               wyników, przy których uznajemy, że coś zaszło. Ta zmiana perspektywy
               pozwala potem na zdarzeniach wykonywać działania jak na zbiorach.",
            "Pozostaje pytanie, ile wynosi P(A). Jeśli procedura losowania sprawia, że
               żadna paleta nie jest wyróżniona, to każdej z 24 palet przypisujemy tę samą
               szansę 1/24. Zdarzenie A obejmuje sześć takich jednakowo możliwych wyników,
               więc jego prawdopodobieństwo to 6 · 1/24. Uogólnienie tego rozumowania to
               klasyczna definicja prawdopodobieństwa, pochodząca od Laplace’a.",
            risk_definition("1.5", "Klasyczna definicja prawdopodobieństwa", c(
              "Jeżeli przestrzeń Ω jest skończona, a wszystkie zdarzenia elementarne są
               jednakowo możliwe, to prawdopodobieństwem zdarzenia A ⊆ Ω nazywamy
               iloraz liczby zdarzeń elementarnych sprzyjających A do liczby wszystkich
               zdarzeń elementarnych (wzór 1.2)."
            )),
            risk_formula(
              "P(A)=\\frac{|A|}{|\\Omega|}=\\frac{\\text{liczba wyników sprzyjających}}{\\text{liczba jednakowo możliwych wyników}}",
              num = "1.2",
              legend = c(
                "|A|" = "liczba zdarzeń elementarnych należących do A",
                "|\\Omega|" = "liczba wszystkich zdarzeń elementarnych"
              )
            ),
            "Dla dostawy Bananpolu P(A) = 6/24 = 0.25. Oba założenia definicji są
               tu spełnione z konstrukcji: palet jest skończenie wiele, a równe szanse
               gwarantuje generator losowy, który przypisuje każdej palecie jeden numer.
               Gdyby inspektor wybierał „na oko” paletę stojącą najbliżej drzwi, drugie
               założenie przestałoby obowiązywać, choć liczby 6 i 24 by się nie zmieniły.",
            risk_example("1.3", "Losowanie zmiany do audytu",
              problem = c(
                "Audytor losuje jedną zmianę z tygodnia roboczego: jeden z pięciu dni
                 (poniedziałek–piątek) i jedną z trzech zmian (ranna, popołudniowa,
                 nocna); każda para dzień–zmiana ma tę samą szansę. Wypisz przestrzeń Ω
                 i oblicz prawdopodobieństwa zdarzeń A — wylosowano zmianę nocną oraz
                 B — wylosowano piątek."
              ),
              steps = c(
                "Zdarzeniem elementarnym jest para (dzień, zmiana), np. (pon, ranna).
                 Samo „poniedziałek” nie jest wynikiem elementarnym, bo nie mówi, którą
                 zmianę wylosowano.",
                "Ω zawiera 5 · 3 = 15 par, wszystkie jednakowo możliwe — definicja 1.5 ma
                 zastosowanie.",
                "A = {(pon, nocna), (wt, nocna), (śr, nocna), (czw, nocna), (pt, nocna)},
                 |A| = 5, więc ze wzoru (1.2) P(A) = 5/15 = 1/3 ≈ 0.333.",
                "B = {(pt, ranna), (pt, popołudniowa), (pt, nocna)}, |B| = 3, więc
                 P(B) = 3/15 = 0.2."
              ),
              answer = "|Ω| = 15, P(A) = 1/3, P(B) = 0.2. Do tych samych zdarzeń wrócimy
                w przykładzie 1.4, łącząc je spójnikami „i” oraz „lub”."
            )
          )
        ),
        list(
          id = "slownik", title = "Te same pojęcia w języku formalnym",
          body = list(
            "Podręczniki rachunku prawdopodobieństwa używają kilku stałych nazw.
               Wszystkie już znasz z przykładu palety — tutaj tylko je porządkujemy.",
            figure_panel(
              label = "Słownik",
              title = "Losowanie palety w terminologii formalnej",
              full_width = TRUE,
              lc_table(
                data.frame(
                  term = c("Doświadczenie losowe", "Wynik elementarny", "Przestrzeń wyników Ω",
                           "Zdarzenie A", "Zdarzenie pewne", "Zdarzenie niemożliwe"),
                  meaning = c("Powtarzalna procedura o niepewnym wyniku",
                              "Pojedynczy, niepodzielny wynik doświadczenia",
                              "Zbiór wszystkich wyników elementarnych",
                              "Dowolny podzbiór przestrzeni Ω",
                              "Cała przestrzeń Ω — zachodzi zawsze",
                              "Zbiór pusty ∅ — nie zachodzi nigdy"),
                  example = c("Losowanie jednej palety do kontroli", "Numer wylosowanej palety",
                              "Wszystkie 24 palety", "Palety z uszkodzonym zabezpieczeniem",
                              "Wylosowano którąś z 24 palet", "Wylosowano paletę numer 25")
                ),
                cols = list(
                  lc_col("term", "Termin", "row"),
                  lc_col("meaning", "Znaczenie", "text"),
                  lc_col("example", "W Bananpolu", "text")
                ),
                narrow = "cards"
              )
            ),
            "Z definicji klasycznej wynikają trzy podstawowe własności. Możesz je
               sprawdzić suwakiem poniżej: ustaw 0 palet sprzyjających (zdarzenie
               niemożliwe), potem 24 (zdarzenie pewne).",
            risk_formula(
              "P(\\Omega)=1,\\qquad P(\\emptyset)=0,\\qquad 0\\le P(A)\\le 1",
              num = "1.3"
            ),
            "Prawdopodobieństwo zdarzenia pewnego wynosi 1, niemożliwego 0,
               a każdego innego zdarzenia — wartość pomiędzy.",
            risk_derivation("własności (1.3) z definicji klasycznej", c(
              "Wystarczy policzyć elementy. Zdarzenie pewne to cała przestrzeń, więc
               jego licznik jest równy mianownikowi. Zbiór pusty nie ma elementów.
               Każde zdarzenie A jest podzbiorem Ω, więc ma od 0 do |Ω| elementów."
            ), lines = c(
              "P(Ω) = |Ω| / |Ω| = 1",
              "P(∅) = |∅| / |Ω| = 0 / |Ω| = 0",
              "∅ ⊆ A ⊆ Ω   ⇒   0 ≤ |A| ≤ |Ω|   ⇒   0 ≤ P(A) ≤ 1"
            )),
            "Własności (1.3) są prostym, ale skutecznym testem poprawności każdego
               rachunku w tym kursie. Jeśli wynik wychodzi ujemny albo większy od
               jedności, błąd jest gdzieś wcześniej: w liczniku, w mianowniku albo w
               tym, że dodano coś dwa razy. Ten ostatni przypadek zobaczymy w następnym
               rozdziale na diagramie Venna."
          )
        ),
        list(
          id = "paletki", title = "Zbuduj zdarzenie na siatce palet",
          body = list(
            "Zmieniaj liczbę palet z uszkodzonym zabezpieczeniem. Siatka pokazuje
               pełny mianownik, zdarzenie A oraz jego dopełnienie.",
            risk_try("ustaw 6 palet i odczytaj P(A) oraz P(Aᶜ). Potem przesuń suwak na
              0 i na 24 i sprawdź, które kafelki zmieniają kolor. Na koniec dodaj w
              pamięci obie wartości P(A) i P(Aᶜ) dla kilku ustawień suwaka."),
            figure_panel(
              label = "Ćwiczenie 3",
              title = "Losowa kontrola jednej palety",
              full_width = TRUE,
              lc_toolbar(
                lc_slider("ch3_favourable", "Palety z uszkodzonym zabezpieczeniem", 0, 24, 6, 1),
                lc_readouts(uiOutput("ch3_stats"))
              ),
              lc_plot("ch3_grid", ratio = "1.4/1", max_height = "430px"),
              lc_caption(
                "Zdarzenie A: wylosowana paleta ma uszkodzone zabezpieczenie.",
                tone = "info"
              )
            ),
            "Siatka pokazuje całe Ω naraz: 24 kafelki to mianownik, kafelki w kolorze
               zdarzenia A to licznik. Przy sześciu uszkodzonych paletach P(A) = 0.25, a
               pozostałe 18 kafelków tworzy dopełnienie Aᶜ o prawdopodobieństwie 0.75.
               Przy ustawieniu 0 zdarzenie A staje się zbiorem pustym, a przy 24 —
               całą przestrzenią; obie skrajności to własności (1.3). Niezależnie od
               położenia suwaka P(A) i P(Aᶜ) sumują się do 1, bo każdy kafelek ma
               dokładnie jeden z dwóch kolorów. Tę obserwację zapiszemy jako wzór (1.4)
               w następnym rozdziale."
          )
        ),
        list(
          id = "granica", title = "Kiedy ten iloraz nie wystarcza",
          body = list(
            "Palety są jednakowo możliwe, bo wymusza to procedura losowania. Realne
               awarie maszyn, pożary i upadki nie tworzą zwykle listy symetrycznych
               przypadków. Ich prawdopodobieństwa zależą od warunków, ekspozycji,
               historii i zabezpieczeń. Wtedy potrzebujemy danych albo innego modelu.",
            "Najczęstszy błąd polega na tym, że wypisujemy możliwe wyniki, liczymy je
               i dzielimy, nie sprawdzając, czy są jednakowo możliwe. Każdą zmianę można
               opisać jako „było poślizgnięcie” albo „nie było”, ale to nie znaczy, że
               każdy z tych wyników ma szansę 1/2. Założenie równych szans nie wynika z
               liczby wyników, tylko z mechanizmu doświadczenia: z generatora losowego,
               symetrii urządzenia albo procedury wyboru. Tam, gdzie takiego mechanizmu
               nie ma, wracamy do częstości z rozdziału 02 albo budujemy model.",
            risk_check("j1_chk_symetria",
              "Kolega proponuje: „Zmiana może skończyć się wypadkiem albo nie, więc P(wypadek) = 1/2 z definicji klasycznej”. Co jest nie tak?",
              c(
                "Nic — dwa wyniki, jeden sprzyjający, więc 1/2" = "ok",
                "Przestrzeń jest źle wypisana, bo ma tylko dwa elementy" = "size",
                "Nic nie uzasadnia, że oba wyniki są jednakowo możliwe" = "symmetry"
              ),
              correct = "symmetry",
              explanation = "Definicja 1.5 wymaga jednakowo możliwych zdarzeń elementarnych. Dwuelementowa przestrzeń jest w porządku, ale żaden mechanizm nie nadaje wypadkowi i jego brakowi równych szans — dlatego to prawdopodobieństwo trzeba oszacować z danych. Podobnie „jedna z 24 palet” opisuje losowanie do kontroli; nie oznacza, że ryzyko uszkodzenia każdej palety powstało z klasycznej symetrii.",
              hints = c(
                ok = "Sprawdź oba warunki definicji 1.5. Który z nich nie jest tu uzasadniony?",
                size = "Przestrzeń dwuelementowa jest dopuszczalna — rzut monetą też ma dwa wyniki. Czym moneta różni się od zmiany w magazynie?"
              )
            )
          )
        )
      )
    ),
    list(
      id = "zbiory", title = "Działania na zdarzeniach", hook = "Skórka lub mokro to nie skórka i mokro",
      lead = "W raportach bezpieczeństwa słowa „lub”, „i” oraz „nie” zmieniają
        to, co liczymy. Na stu kontrolach korytarza Bananpolu zobaczymy
        działania na zdarzeniach: sumę, część wspólną i dopełnienie.",
      teaser = "Przetłumaczymy słowa „lub”, „i” oraz „nie” na działania na zbiorach.",
      body = list(
        lc_note("Dwa zdarzenia",
          tags$div("A — podczas kontroli znaleziono skórkę na przejściu."),
          tags$div("B — podczas kontroli posadzka była mokra.")
        ),
        "Kierownik zmiany pyta inspektora: „Jak często przejście jest
           niebezpieczne?”. Inspektor ma w notesie dwie osobne kolumny — skórka na
           przejściu i mokra posadzka — z wynikami stu kontroli. Żadna z nich nie
           odpowiada na pytanie wprost. „Niebezpieczne” może znaczyć „skórka lub
           mokro”, a może „skórka i mokro jednocześnie”; każde z tych odczytań daje
           inną liczbę. Zanim cokolwiek policzymy, musimy przetłumaczyć zdanie na
           działanie na zdarzeniach."
      ),
      sections = list(
        list(
          id = "jezyk", title = "Przetłumacz zdanie na zbiór",
          body = list(
            "Zdarzenie A ∪ B zachodzi, gdy wystąpiło A lub B, włącznie z sytuacją,
               gdy wystąpiły oba. Zdarzenie A ∩ B wymaga obu warunków naraz. Dopełnienie
               Aᶜ obejmuje wszystkie wyniki, w których A nie zaszło.",
            risk_definition("1.6", "Działania na zdarzeniach", list(
              "Niech A i B będą zdarzeniami w tej samej przestrzeni Ω.",
              tags$ul(
                class = "lc-def-list",
                tags$li(tags$strong("Suma"), " A ∪ B — wyniki należące do A lub do B (lub do obu)."),
                tags$li(tags$strong("Iloczyn"), " A ∩ B — wyniki należące jednocześnie do A i do B."),
                tags$li(tags$strong("Dopełnienie"), " (zdarzenie przeciwne) Aᶜ — wyniki Ω, które nie należą do A."),
                tags$li(tags$strong("Różnica"), " A \\ B — wyniki należące do A, ale nie do B; zachodzi A \\ B = A ∩ Bᶜ.")
              )
            )),
            figure_panel(
              label = "Rycina 1.1",
              title = "Cztery działania na zdarzeniach",
              full_width = TRUE,
              lc_plot("ch4_ops_plot", ratio = "1.7/1", max_height = "480px")
            ),
            lc_caption("Zaznaczony obszar to zdarzenie otrzymane w wyniku działania; prostokąt oznacza całą przestrzeń Ω."),
            risk_definition("1.7", "Zdarzenia rozłączne", c(
              "Zdarzenia A i B są rozłączne (wykluczające się), jeśli A ∩ B = ∅, czyli
               nie mogą zajść jednocześnie. Przykład: zdarzenie A i jego dopełnienie Aᶜ
               są zawsze rozłączne, a ich suma to cała przestrzeń: A ∪ Aᶜ = Ω."
            )),
            "Z ostatniej uwagi wynika najczęściej używany wzór tego kursu. Każdy wynik
               należy albo do A, albo do Aᶜ — nigdy do obu i zawsze do któregoś. W
               definicji klasycznej oznacza to |A| + |Aᶜ| = |Ω|; po podzieleniu przez |Ω|
               dostajemy wzór (1.4). Obserwowaliśmy to już na siatce palet w rozdziale 03:
               P(A) i P(Aᶜ) zawsze sumowały się do 1.",
            risk_formula("P(A^{c})=1-P(A)", num = "1.4",
              legend = c("A^{c}" = "zdarzenie przeciwne do A: A nie zaszło")),
            "Wzór (1.4) jest szczególnie wygodny, gdy zdarzenie ma postać „co
               najmniej jeden”. Bezpośrednie liczenie takich zdarzeń wymaga zebrania
               wielu przypadków, a jego dopełnienie — „ani jeden” — jest zwykle jednym,
               prostym przypadkiem. Ten trik wróci wielokrotnie, zwłaszcza w wykładach 04
               i 08.",
            "Suma zdarzeń jest trudniejsza. Kusi, żeby dodać P(A) i P(B), ale wyniki
               należące do obu zdarzeń zostałyby wtedy policzone dwa razy — raz w A i raz
               w B. Prześledź to na diagramie, zanim zapiszemy jakikolwiek wzór.",
            risk_try("klikaj „Dalej” i obserwuj diagram krok po kroku. Na
              kroku 2 zwróć uwagę, który obszar dostał dwa kolory; na kroku 4 sprawdź,
              dlaczego samo dodawanie przestaje być błędem."),
            figure_panel(
              label = "Demonstracja 1.1",
              full_width = TRUE,
              lc_step_widget("ch4_venn",
                title = "Dlaczego nie wystarczy dodać P(A) i P(B)?",
                steps = c("Dane", "Naiwna suma", "Korekta", "Zdarzenia rozłączne"),
                plot_id = "ch4_venn_plot",
                ratio = "1.8/1"
              )
            ),
            "Naiwna suma 0.70 + 0.60 = 1.30 łamie własność 0 ≤ P ≤ 1 ze wzoru (1.3) —
               to sygnał, że coś policzono podwójnie. Wystarczy odjąć część wspólną
               raz: wynik 0.90 jest poprawny i oznacza prawdopodobieństwo, że zaszło
               przynajmniej jedno z dwóch zdarzeń.",
            "Ta sama zasada działa przy liczeniu elementów. W definicji klasycznej
               |A ∪ B| = |A| + |B| - |A ∩ B|, bo część wspólna wchodzi do obu
               składników, a w sumie ma się znaleźć jeden raz. Po podzieleniu przez |Ω|
               dostajemy wzór (1.5).",
            risk_formula("P(A\\cup B)=P(A)+P(B)-P(A\\cap B)", num = "1.5",
              legend = c(
                "A\\cup B" = "zaszło A lub B (lub oba)",
                "A\\cap B" = "zaszły jednocześnie A i B"
              )),
            "W ostatnim kroku demonstracji koła się nie stykają, P(A ∩ B) = 0 i
               wzór (1.5) upraszcza się do zwykłego dodawania. Dodawanie
               prawdopodobieństw bez poprawki jest więc poprawne tylko dla zdarzeń
               rozłącznych (definicja 1.7).",
            risk_example("1.4", "Nocna zmiana lub piątek",
              problem = c(
                "Wróć do losowania zmiany z przykładu 1.3 (|Ω| = 15, A — zmiana nocna,
                 B — piątek). Oblicz P(A ∩ B), P(A ∪ B), P(Aᶜ) oraz prawdopodobieństwo,
                 że wylosowana zmiana nie jest ani nocna, ani piątkowa."
              ),
              steps = c(
                "A ∩ B = {(pt, nocna)} — jeden wynik, więc P(A ∩ B) = 1/15 ≈ 0.067.",
                "Ze wzoru (1.5): P(A ∪ B) = 5/15 + 3/15 - 1/15 = 7/15 ≈ 0.467. Bez
                 odjęcia części wspólnej wyszłoby 8/15 — piątkowa nocka zostałaby
                 policzona dwa razy.",
                "Ze wzoru (1.4): P(Aᶜ) = 1 - 5/15 = 10/15 ≈ 0.667.",
                "„Ani A, ani B” to dopełnienie sumy: P((A ∪ B)ᶜ) = 1 - 7/15 = 8/15 ≈ 0.533.
                 Sprawdzenie przez wyliczenie: 4 dni pon–czw × 2 zmiany dzienne = 8 wyników."
              ),
              answer = "P(A ∩ B) = 1/15, P(A ∪ B) = 7/15, P(Aᶜ) = 2/3, P(ani A, ani B) = 8/15."
            )
          )
        ),
        list(
          id = "siatka", title = "Zbuduj dwa zdarzenia na 100 kontrolach",
          body = list(
            "Zmieniaj liczebności zdarzeń Me i Be oraz ich część wspólną. Aplikacja pilnuje, by
               wybrane zbiory mogły zmieścić się w przestrzeni 100 wyników.",
            "Tym razem przestrzeń to sto kontroli korytarza, a prawdopodobieństwa są
               częstościami z definicji 1.2: P(Me) = |Me|/100. Każdy kwadrat to jedna
               kontrola i należy do dokładnie jednej z czterech grup — tylko Me, tylko Be,
               Me i Be, ani Me, ani Be. Cztery grupy są parami rozłączne i razem wypełniają
               Ω, dlatego wszystkie wzory tego rozdziału można sprawdzić zwykłym
               liczeniem kwadratów.",
            risk_try("zostaw ustawienia startowe (Me = 30, Be = 20, część wspólna 8) i
              policz kwadraty w każdym kolorze. Potem zwiększ część wspólną do 20 i
              zmniejsz ją do 0. Na koniec ustaw Me = 80 i Be = 40 i sprawdź, dlaczego
              suwak części wspólnej nie pozwala zejść poniżej 20."),
            figure_panel(
              label = "Ćwiczenie 4",
              title = "Suma, iloczyn i dopełnienie zdarzeń",
              full_width = TRUE,
              lc_toolbar(
                lc_slider("ch4_n_a", "Liczba kontroli ze zdarzeniem Me", 0, 80, 30, 1),
                lc_slider("ch4_n_b", "Liczba kontroli ze zdarzeniem Be", 0, 80, 20, 1),
                lc_slider("ch4_overlap", "Liczba kontroli z Me i Be", 0, 20, 8, 1),
                lc_readouts(uiOutput("ch4_stats"))
              ),
              lc_plot("ch4_event_grid", ratio = "1.3/1", max_height = "480px")
            ),
            "Przy ustawieniach startowych panel pokazuje P(Me ∩ Be) = 0.08, P(Me ∪ Be) =
               0.42, P(Meᶜ) = 0.70 i „ani Me, ani Be” = 0.58. Sprawdzenie wzorem (1.5):
               0.30 + 0.20 - 0.08 = 0.42. Ostatnia wartość to 1 - 0.42, bo kontrola,
               w której nie było ani skórki, ani mokrej posadzki, jest dokładnie
               dopełnieniem sumy. Tę równoważność zapisują prawa de Morgana.",
            risk_formula("(A\\cup B)^{c}=A^{c}\\cap B^{c},\\qquad (A\\cap B)^{c}=A^{c}\\cup B^{c}", num = "1.6"),
            "Pierwsze prawo czytamy: „nie zaszło ani A, ani B” to to samo co „nie
               zaszło A i nie zaszło B”. Drugie: „nie zaszły oba naraz” to to samo co
               „nie zaszło A lub nie zaszło B”. W raportach bezpieczeństwa przydaje się
               szczególnie pierwsze — kontrola „bez żadnych uwag” jest dopełnieniem sumy
               wszystkich rodzajów uwag. Suwak części wspólnej ma też ograniczenia: przy
               Me = 80 i Be = 40 część wspólna musi mieć co najmniej 20 kontroli, bo
               inaczej suma przekroczyłaby 100.",
            risk_check("j1_chk_suma",
              "W 100 kontrolach P(A) = 0.30, P(B) = 0.20, a P(A ∪ B) = 0.50. Co można powiedzieć o zdarzeniach A i B?",
              c(
                "Są rozłączne — w żadnej kontroli nie wystąpiły razem" = "disjoint",
                "Wystąpiły razem w 50 kontrolach" = "fifty",
                "Nie da się nic powiedzieć bez P(A ∩ B)" = "unknown"
              ),
              correct = "disjoint",
              explanation = "Ze wzoru (1.5): P(A ∩ B) = P(A) + P(B) - P(A ∪ B) = 0.30 + 0.20 - 0.50 = 0. Część wspólna jest pusta, więc zdarzenia są rozłączne (definicja 1.7).",
              hints = c(
                fifty = "0.50 to prawdopodobieństwo sumy, nie części wspólnej. Przekształć wzór (1.5).",
                unknown = "Wzór (1.5) łączy cztery wielkości. Znasz trzy z nich — wylicz czwartą."
              )
            )
          )
        ),
        list(
          id = "aksjomaty", title = "Jedna definicja dla symetrii i dla danych",
          body = list(
            "Mamy już dwa sposoby przypisywania liczb zdarzeniom: definicję klasyczną
               dla symetrycznych losowań i częstość dla rejestrów. Wzory (1.3)–(1.5)
               wyprowadziliśmy z liczenia elementów, ale działają one w obu przypadkach —
               a także w modelach z kolejnych wykładów, gdzie wyników jest nieskończenie
               wiele (czas do awarii, stężenie gazu). Współczesny rachunek
               prawdopodobieństwa odwraca więc kierunek: nie mówi, skąd bierze się
               liczba, tylko jakie reguły musi spełniać każde sensowne przypisanie.",
            risk_definition("1.8", "Aksjomatyczna definicja prawdopodobieństwa (Kołmogorow)", c(
              "Prawdopodobieństwem nazywamy funkcję P, która każdemu zdarzeniu A ⊆ Ω
               przypisuje liczbę P(A) i spełnia trzy aksjomaty (1.7): nieujemność,
               unormowanie oraz addytywność dla zdarzeń parami rozłącznych.",
              "Addytywność zapisuje się w wersji dla przeliczalnie wielu zdarzeń; w tym
               wykładzie wystarczy wersja dla dwóch: jeśli A ∩ B = ∅, to P(A ∪ B) =
               P(A) + P(B)."
            )),
            risk_formula(
              "P(A)\\ge 0,\\qquad P(\\Omega)=1,\\qquad P\\Big(\\bigcup_{i} A_i\\Big)=\\sum_{i} P(A_i)",
              num = "1.7",
              legend = c(
                "A_i" = "kolejne zdarzenia parami rozłączne: żadne dwa nie mogą zajść razem (trzeci aksjomat dotyczy tylko takich zdarzeń)",
                "\\bigcup_{i} A_i" = "zaszło któreś z nich"
              )
            ),
            risk_derivation("wzory (1.3)–(1.5) z aksjomatów", c(
              "Definicja klasyczna i częstość spełniają aksjomaty (1.7) — łatwo to
               sprawdzić, licząc elementy. Ważniejsze jest odwrócenie: każda własność,
               którą wyprowadzimy z samych aksjomatów, obowiązuje w każdym modelu, a nie
               tylko przy symetrii.",
              "Dopełnienie: A i Aᶜ są rozłączne, a ich suma to Ω. Zdarzenie niemożliwe:
               ∅ = Ωᶜ. Suma dowolnych zdarzeń: A ∪ B rozkładamy na rozłączne kawałki A
               oraz B \\ A, a B na rozłączne kawałki A ∩ B oraz B \\ A. Ograniczenie z
               góry: skoro P(Aᶜ) ≥ 0, to P(A) = 1 - P(Aᶜ) ≤ 1."
            ), lines = c(
              "1 = P(Ω) = P(A ∪ Aᶜ) = P(A) + P(Aᶜ)       ⇒  P(Aᶜ) = 1 - P(A)          (1.4)",
              "P(∅) = P(Ωᶜ) = 1 - P(Ω) = 0                                           (1.3)",
              "P(A ∪ B) = P(A) + P(B \\ A)",
              "P(B)     = P(A ∩ B) + P(B \\ A)            ⇒  P(A ∪ B) = P(A) + P(B) - P(A ∩ B)   (1.5)"
            )),
            "Aksjomaty mają też praktyczną funkcję kontrolną. Jeśli w arkuszu oceny
               ryzyka trzy wykluczające się scenariusze awarii mają prawdopodobieństwa
               0.5, 0.4 i 0.3, to arkusz jest wewnętrznie sprzeczny: ich suma 1.2
               przekracza P(Ω) = 1. Nie trzeba znać żadnych danych, żeby wykryć taki błąd.",
            risk_example("1.5", "Co najmniej jedna uszkodzona paleta",
              problem = c(
                "Z dostawy 24 palet, w której 6 ma uszkodzone zabezpieczenie, inspektor
                 losuje jednocześnie dwie różne palety; każda para ma tę samą szansę.
                 Oblicz prawdopodobieństwo, że co najmniej jedna z wylosowanych palet
                 ma uszkodzone zabezpieczenie."
              ),
              steps = c(
                "Zdarzeniem elementarnym jest nieuporządkowana para palet. Liczba par:
                 |Ω| = C(24, 2) = 24 · 23 / 2 = 276.",
                "Zdarzenie „co najmniej jedna uszkodzona” obejmuje pary z jedną albo dwiema
                 uszkodzonymi paletami. Łatwiej policzyć dopełnienie: „obie nieuszkodzone”.
                 Takich par jest C(18, 2) = 18 · 17 / 2 = 153.",
                "Ze wzoru (1.2): P(obie nieuszkodzone) = 153/276 ≈ 0.554.",
                "Ze wzoru (1.4): P(co najmniej jedna uszkodzona) = 1 - 153/276 = 123/276 ≈ 0.446.",
                "Sprawdzenie wprost: dokładnie jedna uszkodzona to 6 · 18 = 108 par, obie
                 uszkodzone to C(6, 2) = 15 par; razem 123 pary, jak wyżej."
              ),
              answer = "Około 0.446. Dopełnienie sprowadziło rachunek do jednego przypadku
                zamiast dwóch."
            )
          )
        ),
        list(
          id = "pulapka", title = "Rozłączne nie znaczy niezależne",
          body = list(
            "Zdarzenia rozłączne nie mogą zajść razem, więc ich część wspólna jest
               pusta. Zdarzenia niezależne mogą zajść razem, ale informacja o jednym nie
               zmienia prawdopodobieństwa drugiego. Dwa niezerowe zdarzenia rozłączne
               nie są niezależne: gdy A zaszło, wiemy na pewno, że B nie zaszło.",
            lc_warn("Pułapka",
              "W rachunku prawdopodobieństwa „A lub B” obejmuje także przypadek
                „A i B”, chyba że wyraźnie mówimy o alternatywie wykluczającej."
            )
          )
        )
      )
    ),
    list(
      id = "decyzja", title = "Macierz ryzyka", hook = "Rzadszy wypadek może być groźniejszy",
      lead = "Dyrektor Bananpolu ma dwa problemy: częste poślizgnięcia i rzadkie
        kolizje z wózkiem. Samo prawdopodobieństwo nie ustali, który jest
        pierwszy, bo liczy się też skutek i horyzont czasowy. Macierz
        ryzyka zestawia te informacje obok siebie.",
      teaser = "Dwa zdarzenia o podobnej częstości mogą mieć zupełnie inne skutki.",
      sections = list(
        list(
          id = "porownanie", title = "Dwa problemy dyrektora Bananpolu",
          body = list(
            "Poniższe liczby są fikcyjne i służą wyłącznie temu ćwiczeniu. Porównaj
               oba przypadki, zwracając uwagę na dokładne brzmienie każdej odpowiedzi.",
            lc_readouts(
              lc_readout("A · Poślizgnięcie, P na zmianę", "0.08", color = upwr_cat[["bursztyn"]]),
              lc_readout("B · Kolizja z wózkiem, P na zmianę", "0.002", color = upwr_cat[["terakota"]])
            ),
            lc_caption("Możliwe skutki: poślizgnięcie od braku urazu do złamania,
              kolizja z wózkiem: ciężki lub śmiertelny uraz."),
            "Obie liczby łatwiej porównać jako częstości naturalne. P = 0.08 na zmianę
               oznacza w modelu około 80 zmian z poślizgnięciem na 1000 zmian w tym
               korytarzu. P = 0.002 oznacza około 2 zmiany z kolizją na 1000 zmian w
               strefie transportu. Poślizgnięcie jest więc czterdzieści razy częstsze.
               Pytanie brzmi, czy ta różnica sama rozstrzyga, czym dyrektor powinien
               zająć się najpierw.",
            risk_try("zanim klikniesz „Sprawdź rozumowanie”, zapisz jednym zdaniem,
              czego brakuje w każdej z trzech pierwszych odpowiedzi. Potem wybierz
              odpowiedź i porównaj swoje uzasadnienie z komentarzem."),
            figure_panel(
              label = "Decyzja",
              title = "Jaki priorytet można teraz uzasadnić?",
              tags$div(class = "lc-choices", `data-correct` = "insufficient", `data-reveal` = "ch5_check",
                radioButtons(
                  "ch5_priority",
                  "Wybierz najlepiej uzasadnione stwierdzenie",
                  choices = c(
                    "Najpierw A, bo ma większe prawdopodobieństwo" = "a",
                    "Najpierw B, bo może mieć cięższy skutek" = "b",
                    "Oba problemy mają takie samo ryzyko" = "equal",
                    "Same prawdopodobieństwa nie wystarczają do ustalenia priorytetu" = "insufficient"
                  ),
                  selected = character(0)
                )
              ),
              lc_action("ch5_check", "Sprawdź rozumowanie", variant = "solid"),
              uiOutput("ch5_feedback")
            ),
            "Każda z pierwszych trzech odpowiedzi opiera się na jednym wymiarze
               problemu. Większe prawdopodobieństwo poślizgnięcia jest faktem, ale nie
               mówi nic o skutkach. Cięższy skutek kolizji też jest faktem, ale pomija
               to, jak rzadko do niej dochodzi. Stwierdzenie o „takim samym ryzyku”
               wymagałoby reguły, która zamienia prawdopodobieństwo i skutek na jedną
               liczbę — a takiej reguły nikt jeszcze nie ustalił. Poprawna odpowiedź nie
               jest uchylaniem się od decyzji, tylko wskazaniem, jakich informacji
               brakuje, żeby ją podjąć.",
            lc_note("Granica wykładu",
              "Ten kurs buduje przede wszystkim składową probabilistyczną analizy.
               Skutków nie zamieniamy automatycznie w pieniądze ani punkty."
            )
          )
        ),
        list(
          id = "horyzont", title = "Prawdopodobieństwo zawsze ma horyzont",
          body = list(
            "Obie liczby dotyczą jednej zmiany. Dyrektor myśli jednak w horyzoncie
               roku: ile razy w ciągu 250 zmian może dojść do kolizji? Porównywanie
               prawdopodobieństw liczonych dla różnych horyzontów — na zmianę, na
               miesiąc, na rok — jest jednym z najczęstszych błędów w raportach
               bezpieczeństwa. Każde prawdopodobieństwo musi mieć podaną jednostkę tak
               samo jak częstość z definicji 1.2 ma swój mianownik.",
            "Przejście od jednej zmiany do roku wymaga założeń o tym, jak zmiany są
               ze sobą powiązane; zrobimy to porządnie w wykładzie 04. Już teraz wzory
               z tego wykładu pozwalają jednak wykryć błąd, który pojawia się bardzo
               często: mnożenie prawdopodobieństwa na zmianę przez liczbę zmian.",
            risk_example("1.6", "Czy 250 · 0.002 to prawdopodobieństwo w roku?",
              problem = c(
                "Analityk pisze: „P(kolizji na zmianę) = 0.002, w roku jest 250 zmian,
                 więc P(co najmniej jednej kolizji w roku) = 250 · 0.002 = 0.5”. Tą samą
                 metodą dla poślizgnięcia dostałby 250 · 0.08. Oceń ten rachunek."
              ),
              steps = c(
                "Zdarzenie „co najmniej jedna kolizja w roku” to suma zdarzeń K₁ ∪ K₂ ∪ … ∪ K₂₅₀,
                 gdzie Kᵢ oznacza kolizję na i-tej zmianie.",
                "Dodawanie prawdopodobieństw jest poprawne tylko dla zdarzeń rozłącznych
                 (aksjomat (1.7)). Kolizje na różnych zmianach nie są rozłączne — w roku
                 mogą zdarzyć się dwie.",
                "Ze wzoru (1.5) dla dwóch zdarzeń: P(K₁ ∪ K₂) = P(K₁) + P(K₂) - P(K₁ ∩ K₂)
                 ≤ P(K₁) + P(K₂). Suma prawdopodobieństw jest więc tylko górnym
                 ograniczeniem, bo pomija odjęcie części wspólnych.",
                "Dla poślizgnięcia 250 · 0.08 = 20 — liczba większa od 1, co łamie
                 własność (1.3). To ostateczny dowód, że metoda jest błędna.",
                "Przy dodatkowym założeniu niezależności zmian (wykład 04) i wzorze (1.4):
                 P(co najmniej jednej kolizji) = 1 - 0.998²⁵⁰ ≈ 0.394, a dla poślizgnięcia
                 1 - 0.92²⁵⁰ — praktycznie 1."
              ),
              answer = "0.5 to górne ograniczenie, nie prawdopodobieństwo; przy niezależnych
                zmianach właściwa wartość to około 0.39. Iloczyn 250 · 0.002 = 0.5 ma
                jednak inną, poprawną interpretację: średnio pół kolizji na rok, czyli
                około jedna kolizja na dwa lata."
            ),
            "Przykład pokazuje, że nawet bez danych o skutkach porównanie wymaga
               uzgodnienia horyzontu. W skali roku poślizgnięcie w tym korytarzu jest w
               modelu praktycznie pewne, a kolizja ma szansę mniej więcej 2 do 5. Obie
               liczby są wysokie, ale opisują zupełnie różne zdarzenia — i to prowadzi
               nas do profilu ryzyka."
          )
        ),
        list(
          id = "profil", title = "Profil dwóch problemów",
          body = list(
            "Dla obu problemów warto zapisać profil: definicję zdarzenia,
               prawdopodobieństwo wraz z horyzontem, możliwe skutki, liczbę osób
               eksponowanych, niepewność danych, działające bariery oraz dostępne
               działania. Dopiero wtedy można zastosować jawne kryteria priorytetu.",
            figure_panel(
              label = "Profil ryzyka",
              full_width = TRUE,
              lc_table(
                data.frame(
                  pytanie = c("Co może się zdarzyć?", "Kto jest eksponowany?", "Jakie są skutki?", "Jakie bariery działają?", "Czego nie wiemy?"),
                  poslizgniecie = c("Upadek w korytarzu", "Osoby korzystające z przejścia", "Różna dotkliwość urazu", "Sprzątanie i oznakowanie", "Kompletność rejestru"),
                  kolizja_z_wozkiem = c("Potrącenie pieszego", "Piesi w strefie transportu", "Możliwy uraz ciężki", "Separacja ruchu i ograniczenie prędkości", "Ruch pieszych i zdarzenia bliskie wypadku")
                ),
                cols = list(
                  lc_col("pytanie", "Pytanie", "row"),
                  lc_col("poslizgniecie", "Poślizgnięcie", "text"),
                  lc_col("kolizja_z_wozkiem", "Kolizja z wózkiem", "text")
                ),
                narrow = "cards", prose = TRUE
              )
            )
          )
        ),
        list(
          id = "macierz", title = "Jak czytać macierz ryzyka",
          body = list(
            "Kategorie „rzadkie”, „możliwe”, „poważne” i „katastrofalne” pomagają
               porządkować dyskusję, ale są skalami porządkowymi. Iloczyn numerów pól
               1–5 nie staje się automatycznie ilościową miarą ryzyka. Granice kategorii
               i reguły decyzji muszą być jawne.",
            "Problem z iloczynem numerów pól łatwo zobaczyć na przykładzie. Zdarzenie
               o częstości „5 — prawie pewne” i skutku „2 — drobny” dostaje 10 punktów,
               tak samo jak zdarzenie o częstości „2 — rzadkie” i skutku „5 —
               katastrofalny”. Równa liczba punktów nie oznacza równego ryzyka — oznacza
               tylko, że tak wypadła arytmetyka na numerach kategorii. Odstępy między
               kategoriami też nie są równe: przejście od „rzadkiego” do „możliwego” może
               oznaczać dziesięciokrotny wzrost prawdopodobieństwa, a od „drobnego” do
               „poważnego” — zupełnie inny rodzaj szkody.",
            risk_check("j1_chk_macierz",
              "W macierzy 5 × 5 zdarzenie X ma pole (prawdopodobieństwo 5, skutek 2), a zdarzenie Y — pole (2, 5). Oba dostają iloczyn 10. Co z tego wynika?",
              c(
                "Oba zdarzenia mają to samo ryzyko" = "same",
                "Iloczyn numerów kategorii porządkowych nie jest miarą ryzyka; potrzebne są jawne reguły priorytetu" = "ordinal",
                "Y jest ważniejsze, bo skutek zawsze przeważa" = "severity"
              ),
              correct = "ordinal",
              explanation = "Numery kategorii są etykietami porządku, a nie wielkościami, które wolno mnożyć. Równy iloczyn nie oznacza równego ryzyka — decyzja wymaga jawnych kryteriów, np. progu dla ciężkich skutków niezależnie od częstości.",
              hints = c(
                same = "Czy odstęp między kategoriami 1 i 2 musi być taki sam jak między 4 i 5? Co wtedy znaczy iloczyn?",
                severity = "To może być rozsądna reguła, ale trzeba ją jawnie przyjąć. Sama macierz jej nie zawiera."
              )
            ),
            lc_note("Dobra praktyka",
              "Wynik probabilistyczny kończ zdaniem: co ten wynik zmienia, jakiego
                skutku dotyczy i które założenie jest najważniejsze."
            )
          )
        )
      )
    ),
    list(
      id = "sprawdzenie", title = "Ściąga i sprawdzenie", hook = "Najpierw nazwij, potem licz",
      lead = "Dobra analiza zaczyna się od krótkiej specyfikacji zdarzenia,
        ekspozycji, okresu i danych. Wzór jest dopiero kolejnym krokiem.",
      teaser = "Zbierzemy cały wykład w jedną mapę pojęć i pytań.",
      sections = list(
        list(
          id = "podsumowanie", title = "Podsumowanie",
          body = list(
            "Wykład zaczął się od skórki na korytarzu i od rozdzielenia jednej historii
       na pięć ról: zagrożenie, ekspozycję, zdarzenie, skutek i zabezpieczenie
       (definicja 1.1). Rachunek prawdopodobieństwa dotyczy tylko jednej z nich —
       zdarzenia — i działa tylko wtedy, gdy zdarzenie jest obserwowalne i ma
       jednostkę. Pierwszym źródłem liczb jest rejestr: częstość empiryczna
       n_A / n (wzór 1.1) opisuje konkretną serię obserwacji, zmienia się od
       serii do serii i stabilizuje dopiero przy dużej liczbie porównywalnych
       zmian.",
            "Drugim źródłem jest symetria. Gdy doświadczenie ma skończoną przestrzeń
       zdarzeń elementarnych Ω (definicja 1.3), a wyniki są jednakowo możliwe z
       konstrukcji — jak przy losowaniu palety generatorem — prawdopodobieństwo
       zdarzenia A ⊆ Ω to |A| / |Ω| (wzór 1.2). Z tej definicji wynikają granice
       (1.3): P(Ω) = 1, P(∅) = 0, 0 ≤ P(A) ≤ 1. Zdarzenia łączymy jak zbiory
       (definicja 1.6): dopełnienie daje wzór (1.4), suma — wzór (1.5) z
       odjęciem części wspólnej, a „ani A, ani B” — prawa de Morgana (1.6).",
            "Oba źródła liczb spełniają te same trzy aksjomaty Kołmogorowa (1.7), z
       których wynikają wszystkie wzory tego wykładu; w kolejnych wykładach
       aksjomaty pozwolą pracować także z modelami, w których wyników jest
       nieskończenie wiele. Wreszcie rozdział o decyzji przypomniał granicę
       rachunku: prawdopodobieństwo musi mieć horyzont, nie wolno go dodawać
       dla zdarzeń, które nie są rozłączne (przykład 1.6), i samo nie wyznacza
       priorytetu — ten wymaga profilu skutków, ekspozycji i barier."
          )
        ),
        list(
          id = "mapa", title = "Mapa pojęć",
          body = list(
            figure_panel(
              label = "Ściąga 1.1",
              title = "Pięć ról w opisie sytuacji",
              full_width = TRUE,
              lc_table(
                data.frame(
                  concept = c("Zagrożenie", "Ekspozycja", "Zdarzenie", "Skutek", "Zabezpieczenie"),
                  question = c("Co może spowodować szkodę?",
                               "Kto lub co ma kontakt z zagrożeniem?",
                               "Co dokładnie ma zajść?",
                               "Jakie może być następstwo?",
                               "Co przerywa drogę do szkody?"),
                  example = c("Skórka na przejściu", "Pracownik przechodzący korytarzem",
                              "Poślizgnięcie: utrata przyczepności i upadek (w rejestrze zmian: co najmniej jedno podczas zmiany)",
                              "Uraz nadgarstka", "Kontrola i sprzątanie przejścia")
                ),
                cols = list(
                  lc_col("concept", "Pojęcie", "row"),
                  lc_col("question", "Pytanie", "text"),
                  lc_col("example", "Przykład z Bananpolu", "text")
                ),
                narrow = "cards"
              )
            )
          )
        ),
        list(
          id = "checklista", title = "Sześć pytań przed obliczeniem",
          body = list(
            tags$ol(
              tags$li(b_("Jak brzmi zdarzenie?"), " Jednoznacznie i obserwowalnie."),
              tags$li(b_("Spośród czego liczę?"), " Mianownik albo przestrzeń wyników."),
              tags$li(b_("Jaka jest jednostka?"), " Np. zmiana, przejście, paleta."),
              tags$li(b_("Jaki jest okres?"), " Wspólny dla porównań."),
              tags$li(b_("Jakie są założenia?"), " Zwłaszcza porównywalność i symetria."),
              tags$li(b_("Jakie są skutki?"), " Prawdopodobieństwo nie kończy analizy.")
            )
          )
        ),
        list(
          id = "wzory", title = "Najważniejsze zapisy i ich znaczenie",
          body = list(
            lc_formula_box(
              withMathJax("$$\\widehat p=\\frac{\\text{zaobserwowane zdarzenia}}
                           {\\text{porównywalne obserwacje}}$$"),
              tags$p("Wzór (1.1). Częstość empiryczna opisuje konkretny zbiór obserwacji.")
            ),
            lc_formula_box(
              withMathJax("$$P(A)=\\frac{|A|}{|\\Omega|}$$"),
              tags$p("Wzór (1.2). Definicja klasyczna wymaga skończonej przestrzeni jednakowo możliwych wyników.")
            ),
            lc_formula_box(
              withMathJax("$$P(A\\cup B)=P(A)+P(B)-P(A\\cap B)$$"),
              tags$p("Wzór (1.5). Część wspólną odejmujemy, aby nie policzyć tych samych wyników dwa razy.")
            ),
            lc_formula_box(
              withMathJax("$$P(A^c)=1-P(A)$$"),
              tags$p("Wzór (1.4). Dopełnienie obejmuje wszystkie wyniki, w których zdarzenie A nie zaszło.")
            ),
            lc_formula_box(
              withMathJax("$$P(\\Omega)=1,\\qquad P(\\emptyset)=0,\\qquad 0\\le P(A)\\le 1$$"),
              tags$p("Wzór (1.3). Zdarzenie pewne Ω ma prawdopodobieństwo 1, zdarzenie niemożliwe ∅
                     ma 0, a każde zdarzenie mieści się między tymi granicami.")
            ),
            lc_formula_box(
              withMathJax("$$P(A)\\ge 0,\\qquad P(\\Omega)=1,\\qquad A\\cap B=\\emptyset\\ \\Rightarrow\\ P(A\\cup B)=P(A)+P(B)$$"),
              tags$p("Aksjomaty (1.7). Każde poprawne przypisanie prawdopodobieństw — z symetrii
                     czy z danych — musi je spełniać; pozostałe wzory z nich wynikają.")
            )
          )
        ),
        list(
          id = "model", title = "Jak rozpoznać właściwy punkt startu",
          body = list(
            figure_panel(
              label = "Ściąga 1.2",
              full_width = TRUE,
              lc_table(
                data.frame(
                  situation = c("Losowanie z jawnej, symetrycznej listy",
                                "Rejestr porównywalnych obserwacji",
                                "Zdarzenia zależne od warunków", "Priorytet działania"),
                  start = c("Definicja klasyczna", "Częstość empiryczna",
                            "Dalszy model probabilistyczny", "Profil ryzyka"),
                  question = c("Czy wyniki są jednakowo możliwe?",
                               "Czy mianownik i zasady rejestracji są wspólne?",
                               "Co zmienia informacja o warunku?",
                               "Jakie są skutki, bariery i kryteria decyzji?")
                ),
                cols = list(
                  lc_col("situation", "Sytuacja", "row"),
                  lc_col("start", "Punkt startu", "text"),
                  lc_col("question", "Najważniejsze pytanie", "text")
                ),
                narrow = "cards"
              )
            ),

            lc_note("Przykład",
              "„W 100 porównywalnych zmianach zarejestrowano 8 zmian ze zdarzeniem,
                czyli częstość 0.08. Dane nie opisują jeszcze dotkliwości skutków ani
                przyczyn różnic między zmianami.”"
            )
          )
        )
      ),
      widget = jezyk_sciaga_widget
    )
  )
)

jezyk_chapters <- risk_block_chapters(jezyk_block)

jezyk_sytuacja_server <- function(input, output, session) {
  output$ch1_chain_detail <- renderUI({
    key <- input$ch1_chain_click
    req(key %in% names(jezyk_chain_terms))
    term <- jezyk_chain_terms[[key]]
    tags$div(
      class = "lc-chain-detail", style = paste0("--node-color:", term$color),
      tags$div(class = "lc-chain-detail-name", risk_term_labels[[key]]),
      tags$p(term$definition),
      tags$p(tags$strong("Pytanie kontrolne:"), paste0(" ", term$question))
    )
  })

  # Klasyfikacja do ról z definicji 1.1. Ćwiczenie 1.1: karty-zdania do pól ról;
  # ćwiczenie 1.2 (rows = TRUE): karty-role do pól zdań.
  classification_feedback <- function(assign_id, check_id, items, ok_text, rows = FALSE) {
    checked <- reactiveVal(FALSE)
    observeEvent(input[[check_id]], checked(TRUE))

    renderUI({
      req(checked())

      answers <- if (rows) {
        rows_assignment_to_answers(input[[assign_id]], items)
      } else {
        assignment_to_answers(input[[assign_id]], items)
      }
      result <- score_risk_classification(answers, items)

      details <- lapply(seq_len(nrow(items)), function(i) {
        selected <- answers[[items$id[[i]]]]
        correct_code <- items$correct[[i]]
        is_correct <- result$correct[[i]]
        verdict <- if (is_correct) {
          "Dobrze rozpoznane. "
        } else if (nzchar(selected)) {
          paste0(if (rows) "Przypisana rola: " else "Trafiło do pola ",
                 risk_term_labels[[selected]], ". ")
        } else {
          if (rows) "Bez przypisanej roli. " else "Nie trafiło do żadnego pola. "
        }

        tags$li(
          tags$strong(paste0(risk_term_labels[[correct_code]], ": ")),
          items$text[[i]], " ", verdict,
          items$explanation[[i]]
        )
      })

      lc_status(
        lc_verdict(tags$strong(sprintf("Wynik: %d/%d.", result$score, result$total)), type = if (result$score == result$total) "ok" else "warning"),
        if (result$score == result$total) {
          ok_text
        } else {
          " Sprawdź różnicę między źródłem szkody, kontaktem, zdarzeniem i następstwem."
        },
        tags$ul(details)
      )
    })
  }

  output$ch1_feedback <- classification_feedback("ch1_assign", "ch1_check", risk_scenario_items,
    " Historia jest uporządkowana — można teraz zdefiniować zdarzenie do obliczeń.")
  output$ch1_feedback_rampa <- classification_feedback("ch1_assign_rampa", "ch1_check_rampa",
    risk_rampa_items, " Ten sam łańcuch opisuje zupełnie inną historię.", rows = TRUE)
}

jezyk_czestosc_server <- function(input, output, session) {
  probability_candidates <- seq(0.01, 0.30, by = 0.01)
  history <- reactiveVal(integer())
  model_probability <- reactiveVal(sample(probability_candidates, 1L))
  probability_revealed <- reactiveVal(FALSE)

  add_days <- function(n) {
    history(append_bernoulli_history(history(), n, model_probability()))
  }

  observeEvent(input$ch2_add_1, add_days(1L))
  observeEvent(input$ch2_add_10, add_days(10L))
  observeEvent(input$ch2_add_100, add_days(100L))
  observeEvent(input$ch2_add_1000, add_days(1000L))
  observeEvent(input$ch2_reveal, probability_revealed(TRUE))
  observeEvent(input$ch2_reset, {
    history(integer())
    model_probability(sample(setdiff(probability_candidates, model_probability()), 1L))
    probability_revealed(FALSE)
  })

  output$ch2_stats <- renderUI({
    observed <- history()
    n <- length(observed)
    tagList(
      lc_readout("Zmiany", format(n, big.mark = " ")),
      lc_readout("Ze zdarzeniem", format(sum(observed), big.mark = " "), color = upwr_accent),
      lc_readout("Częstość", if (n) risk_fmt_p(mean(observed)) else "—",
                 color = upwr_cat[["niebo"]]),
      lc_readout("Modelowe P",
                 if (probability_revealed()) risk_fmt_p(model_probability()) else "ukryte")
    )
  })

  # Siatka ostatnich 100 zmian: burgund = poślizgnięcie, niebieski = bez zdarzenia.
  zoom_plot_server("ch2_grid", reactive({
    last <- utils::tail(history(), 100L)
    cells <- data.frame(id = seq_len(100L))
    cells$column <- (cells$id - 1L) %% 10L + 1L
    cells$row <- (cells$id - 1L) %/% 10L + 1L
    cells$state <- factor(
      c(ifelse(last == 1L, "event", "none"), rep("empty", 100L - length(last))),
      levels = c("event", "none", "empty")
    )
    ggplot(cells, aes(column, -row, fill = state)) +
      geom_tile(colour = upwr_panel, linewidth = 1.2) +
      scale_fill_manual(values = c(event = upwr_accent, none = upwr_cat[["niebo"]],
                                   empty = upwr_rule), drop = FALSE, guide = "none") +
      coord_equal(expand = FALSE) +
      labs(x = NULL, y = NULL) +
      theme_void()
  }), alt = "Siatka ostatnich stu zmian; burgundowe pola to zmiany z poślizgnięciem.")

  zoom_plot_server("ch2_line", reactive({
    data <- cumulative_frequency(history())
    plot <- ggplot(data, aes(trial, frequency)) +
      geom_hline(yintercept = input$ch2_guess, colour = upwr_cat[["bursztyn"]],
                 linewidth = 0.9, linetype = "dotted") +
      coord_cartesian(ylim = c(0, 1)) +
      labs(x = "Liczba obserwowanych zmian", y = "Skumulowana częstość")
    if (probability_revealed()) {
      plot <- plot + geom_hline(yintercept = model_probability(), colour = upwr_accent,
                                linewidth = 0.9, linetype = "dashed")
    }
    if (nrow(data) == 0) {
      plot + scale_x_continuous(limits = c(0, 2))
    } else {
      plot + geom_line(linewidth = 0.8, colour = upwr_cat[["niebo"]])
    }
  }), alt = "Skumulowana częstość poślizgnięć z linią oszacowania; po odsłonięciu także linia modelowego P.")

  output$ch2_note <- renderUI({
    lc_caption(paste0(
      "Siatka: burgundowe pola to zmiany z poślizgnięciem, niebieskie bez zdarzenia, ",
      "szare jeszcze nieobserwowane. Wykres: kropkowana linia to Twoje oszacowanie",
      if (probability_revealed()) ", przerywana modelowe P." else "."
    ))
  })

  output$ch2_feedback <- renderUI({
    req(probability_revealed())
    observed <- history()
    n <- length(observed)
    p <- model_probability()
    lc_status(
      tags$strong("Porównanie:"),
      paste0(
        sprintf(" Twoje oszacowanie %s, modelowe P %s", risk_fmt_p(input$ch2_guess), risk_fmt_p(p)),
        if (n) sprintf(paste0(", częstość po %s zmianach %s. Przy tej liczbie zmian ",
                              "częstość waha się wokół P typowo o √(p(1 - p)/n) ≈ %s."),
                       format(n, big.mark = " "), risk_fmt_p(mean(observed)),
                       risk_fmt_p(sqrt(p * (1 - p) / n))) else
          ". Dodaj zmiany, żeby porównać z częstością."
      )
    )
  })
}

jezyk_przestrzen_server <- function(input, output, session) {
  pallet_data <- reactive({
    req(input$ch3_favourable)
    build_pallet_grid(input$ch3_favourable, total = 24L, columns = 6L)
  })

  output$ch3_stats <- renderUI({
    req(input$ch3_favourable)
    favourable <- as.integer(input$ch3_favourable)
    probability <- classical_probability(favourable, 24L)

    tagList(
      lc_readout("Licznik |A|", favourable, color = upwr_cat[["terakota"]]),
      lc_readout("Mianownik |Ω|", 24, color = upwr_secondary),
      lc_readout("P(A)", risk_fmt_p(probability), color = upwr_accent),
      lc_readout("P(Aᶜ)", risk_fmt_p(1 - probability), color = upwr_cat[["szalwia"]])
    )
  })

  pallet_plot <- reactive({
    data <- pallet_data()
    data$status <- ifelse(data$favourable, "event", "complement")

    ggplot(data, aes(x = column, y = -row, fill = status)) +
      geom_tile(colour = "white", linewidth = 2, width = 0.92, height = 0.92) +
      geom_text(aes(label = id), colour = "white", fontface = "bold", size = 4) +
      scale_fill_manual(
        values = c(
          "complement" = upwr_reference,
          "event" = upwr_cat[["terakota"]]
        ),
        breaks = c("complement", "event"),
        labels = expression("Dopełnienie " * A^c, "Zdarzenie A")
      ) +
      coord_equal() +
      scale_x_continuous(breaks = NULL) +
      scale_y_continuous(breaks = NULL) +
      labs(x = NULL, y = NULL, fill = NULL) +
      theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom"
      )
  })

  zoom_plot_server(
    "ch3_grid",
    pallet_plot,
    alt = paste(
      "Siatka 24 palet. Część palet należy do zdarzenia",
      "wylosowania palety z uszkodzonym zabezpieczeniem."
    )
  )
}

jezyk_zbiory_server <- function(input, output, session) {
  # Krok widgetu (1..4) żyje w przeglądarce; opis kroku jako tekst inline.
  venn_step <- lc_step_server("ch4_venn", input)$step

  output$ch4_venn_text <- renderUI({
    txt <- switch(
      as.character(venn_step()),
      "1" = paste(
        "Dwa zachodzące na siebie zdarzenia. P(A) = 0.70, P(B) = 0.60, a P(A ∩ B) = 0.40.",
        "Najpierw zaznaczamy oba zbiory bez wykonywania działania."
      ),
      "2" = paste(
        "Dodajemy całe A i całe B: \\(0.70+0.60=1.30\\).",
        "Wynik 1.30 nie może być prawdopodobieństwem. Ciemna część wspólna dostała dwa kolory — została policzona dwa razy."
      ),
      "3" = paste(
        "Usuwamy jedną kopię części wspólnej: \\(0.70+0.60-0.40=0.90\\).",
        "Obszar A ∩ B nadal należy do sumy, ale jest w niej liczony tylko raz."
      ),
      "4" = paste(
        "Kiedy samo dodawanie działa? \\(0.40+0.35=0.75\\).",
        "Koła nie zachodzą na siebie, więc P(A ∩ B) = 0. Niczego nie policzyliśmy dwa razy."
      )
    )
    withMathJax(txt)
  })

  venn_plot <- reactive({
    circle_points <- function(center_x, center_y, radius, n = 240L) {
      angle <- seq(0, 2 * pi, length.out = n)
      data.frame(
        x = center_x + radius * cos(angle),
        y = center_y + radius * sin(angle)
      )
    }

    step <- venn_step()
    if (step == 4L) {
      circle_a <- circle_points(3.15, 3.25, 1.55)
      circle_b <- circle_points(6.85, 3.25, 1.55)
    } else {
      center_a <- 4.05
      center_b <- 5.95
      radius <- 2.15
      circle_a <- circle_points(center_a, 3.25, radius)
      circle_b <- circle_points(center_b, 3.25, radius)
      half_angle <- acos((center_b - center_a) / (2 * radius))
      overlap <- rbind(
        data.frame(
          x = center_a + radius * cos(seq(-half_angle, half_angle, length.out = 120L)),
          y = 3.25 + radius * sin(seq(-half_angle, half_angle, length.out = 120L))
        ),
        data.frame(
          x = center_b + radius * cos(seq(pi - half_angle, pi + half_angle, length.out = 120L)),
          y = 3.25 + radius * sin(seq(pi - half_angle, pi + half_angle, length.out = 120L))
        )
      )
    }

    plot <- ggplot() +
      annotate(
        "rect",
        xmin = 0.8, xmax = 9.2, ymin = 0.45, ymax = 6.05,
        fill = upwr_panel, colour = upwr_rule, linewidth = 0.7
      )

    blend_with_panel <- function(colour, fraction = STEP_ROLES$data$alpha) {
      grDevices::colorRampPalette(c(upwr_panel, colour))(101L)[round(fraction * 100) + 1L]
    }
    # Role: A = dane (niebo), B = druga grupa (bursztyn); kontury wypełnionych kół czarne.
    a_colour <- STEP_ROLES$data$colour
    b_colour <- STEP_ROLES$group$colour

    if (step == 1L) {
      plot <- plot +
        step_layer(geom_polygon, "data", data = circle_a, mapping = aes(x = x, y = y),
                   fill = NA, linewidth = 1.3) +
        step_layer(geom_polygon, "group", data = circle_b, mapping = aes(x = x, y = y),
                   fill = NA, linewidth = 1.3)
    } else if (step == 3L) {
      a_fill <- blend_with_panel(a_colour)
      b_fill <- blend_with_panel(b_colour)
      plot <- plot +
        step_result(geom_polygon, data = circle_a, mapping = aes(x = x, y = y),
                    fill = a_fill, alpha = 1) +
        step_result(geom_polygon, data = circle_b, mapping = aes(x = x, y = y),
                    fill = b_fill, alpha = 1) +
        step_layer(geom_polygon, "new", data = overlap, mapping = aes(x = x, y = y),
                   fill = a_fill)
    } else {
      plot <- plot +
        step_result(geom_polygon, data = circle_a, mapping = aes(x = x, y = y),
                    fill = a_colour) +
        step_result(geom_polygon, data = circle_b, mapping = aes(x = x, y = y),
                    fill = b_colour)
    }

    label_x <- if (step == 4L) c(3.15, 6.85) else c(2.7, 7.3)
    label_text <- if (step == 4L) c("A\nP(A) = 0.40", "B\nP(B) = 0.35") else
      c("A\nP(A) = 0.70", "B\nP(B) = 0.60")

    plot <- plot +
      annotate("text", x = label_x, y = 3.35, label = label_text,
               fontface = "bold", size = 5, lineheight = 1.15)

    if (step == 2L) {
      plot <- plot + annotate(
        "label", x = 5, y = 3.25,
        label = "A ∩ B = 0.40\nPOLICZONE 2 RAZY",
        fill = "#ffffff", colour = STEP_ROLES$new$colour,
        linewidth = 0.4, fontface = "bold", size = 4.3
      )
    } else if (step == 3L) {
      plot <- plot + annotate(
        "label", x = 5, y = 3.25,
        label = "A ∩ B = 0.40\nJEDNO NALICZENIE",
        fill = "#ffffff", colour = STEP_ROLES$new$colour,
        linewidth = 0.4, fontface = "bold", size = 4,
        lineheight = 0.9
      )
    }

    plot +
      # Stała rama we wszystkich krokach.
      coord_equal(xlim = c(0.5, 9.5), ylim = c(0.6, 6.25), expand = FALSE) +
      labs(x = NULL, y = NULL) +
      theme_void() +
      theme(legend.position = "none")
  })

  zoom_plot_server(
    "ch4_venn_plot",
    venn_plot,
    alt = paste(
      "Czterostopniowy diagram Venna pokazujący podwójne policzenie części",
      "wspólnej oraz szczególny przypadek zdarzeń rozłącznych."
    )
  )

  ops_plot <- reactive({
    # Siatka punktów w prostokącie Ω; zdarzenie to zaznaczony podzbiór punktów.
    grid <- expand.grid(x = seq(0.05, 9.95, length.out = 300L), y = seq(0.05, 5.95, length.out = 180L))
    in_a <- (grid$x - 3.9)^2 + (grid$y - 3)^2 <= 2^2
    in_b <- (grid$x - 6.1)^2 + (grid$y - 3)^2 <= 2^2
    ops <- list(
      "A ~ '∪' ~ B" = in_a | in_b,
      "A ~ '∩' ~ B" = in_a & in_b,
      "A^c" = !in_a,
      "A ~ '\\\\' ~ B" = in_a & !in_b
    )
    data <- do.call(rbind, lapply(names(ops), function(op) {
      cbind(grid, op = factor(op, levels = names(ops)), selected = ops[[op]])
    }))

    circles <- do.call(rbind, lapply(names(ops), function(op) {
      angle <- seq(0, 2 * pi, length.out = 200L)
      rbind(
        data.frame(op = op, set = "A", x = 3.9 + 2 * cos(angle), y = 3 + 2 * sin(angle)),
        data.frame(op = op, set = "B", x = 6.1 + 2 * cos(angle), y = 3 + 2 * sin(angle))
      )
    }))
    circles$op <- factor(circles$op, levels = names(ops))
    labels <- data.frame(
      op = factor(rep(names(ops), each = 2L), levels = names(ops)),
      set = c("A", "B"), x = c(3.2, 6.8), y = 3
    )

    ggplot() +
      geom_raster(data = data, aes(x = x, y = y, fill = selected)) +
      geom_path(data = circles, aes(x = x, y = y, group = interaction(op, set)),
                colour = upwr_ink, linewidth = 0.6) +
      geom_text(data = labels, aes(x = x, y = y, label = set), fontface = "bold", size = 5) +
      annotate("rect", xmin = 0, xmax = 10, ymin = 0, ymax = 6,
               fill = NA, colour = upwr_ink, linewidth = 0.5) +
      facet_wrap(~op, nrow = 2L, labeller = label_parsed) +
      scale_fill_manual(values = c(
        "TRUE" = grDevices::colorRampPalette(c(upwr_panel, STEP_ROLES$new$colour))(101L)[46L],
        "FALSE" = upwr_panel
      )) +
      coord_equal(xlim = c(-0.1, 10.1), ylim = c(-0.1, 6.1), expand = FALSE) +
      labs(x = NULL, y = NULL) +
      theme_void() +
      theme(legend.position = "none",
            strip.text = element_text(face = "bold", size = rel(1.3), margin = margin(b = 4)),
            panel.spacing = grid::unit(1, "lines"))
  })

  zoom_plot_server(
    "ch4_ops_plot",
    ops_plot,
    alt = paste(
      "Cztery diagramy Venna: suma, iloczyn, dopełnienie A oraz różnica A bez B,",
      "z zaznaczonym obszarem odpowiadającym każdemu działaniu."
    )
  )

  observeEvent(list(input$ch4_n_a, input$ch4_n_b), {
    req(input$ch4_n_a, input$ch4_n_b)
    lower <- max(0L, input$ch4_n_a + input$ch4_n_b - 100L)
    upper <- min(input$ch4_n_a, input$ch4_n_b)
    current <- input$ch4_overlap
    if (is.null(current)) current <- lower
    value <- min(max(current, lower), upper)

    updateSliderInput(
      session,
      "ch4_overlap",
      min = lower,
      max = upper,
      value = value
    )
  })

  event_values <- reactive({
    req(input$ch4_n_a, input$ch4_n_b, input$ch4_overlap)
    lower <- max(0L, input$ch4_n_a + input$ch4_n_b - 100L)
    upper <- min(input$ch4_n_a, input$ch4_n_b)
    overlap <- min(max(input$ch4_overlap, lower), upper)
    list(
      n_a = as.integer(input$ch4_n_a),
      n_b = as.integer(input$ch4_n_b),
      overlap = as.integer(overlap)
    )
  })

  output$ch4_stats <- renderUI({
    values <- event_values()
    union <- values$n_a + values$n_b - values$overlap

    tagList(
      lc_readout("P(Me ∩ Be)", risk_fmt_p(values$overlap / 100), color = upwr_cat[["wrzos"]]),
      lc_readout("P(Me ∪ Be)", risk_fmt_p(union / 100), color = upwr_accent),
      lc_readout("P(Meᶜ)", risk_fmt_p(1 - values$n_a / 100), color = upwr_cat[["szalwia"]]),
      lc_readout("Ani Me, ani Be", risk_fmt_p((100 - union) / 100), color = upwr_reference)
    )
  })

  event_grid_plot <- reactive({
    values <- event_values()
    data <- build_event_grid(
      total = 100L,
      n_a = values$n_a,
      n_b = values$n_b,
      overlap = values$overlap,
      columns = 10L
    )

    ggplot(data, aes(x = column, y = -row, fill = status)) +
      geom_tile(colour = "white", linewidth = 0.7, width = 0.95, height = 0.95) +
      scale_fill_manual(values = c(
        "A i B" = upwr_cat[["wrzos"]],
        "Tylko A" = upwr_cat[["terakota"]],
        "Tylko B" = upwr_cat[["niebo"]],
        "Ani A, ani B" = upwr_reference
      ), labels = c(
        "A i B" = "Me i Be", "Tylko A" = "Tylko Me",
        "Tylko B" = "Tylko Be", "Ani A, ani B" = "Ani Me, ani Be"
      )) +
      coord_equal() +
      scale_x_continuous(breaks = NULL) +
      scale_y_continuous(breaks = NULL) +
      labs(x = NULL, y = NULL, fill = NULL) +
      theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom"
      )
  })

  zoom_plot_server(
    "ch4_event_grid",
    event_grid_plot,
    alt = paste(
      "Siatka stu kontroli podzielonych na zdarzenie A, zdarzenie B,",
      "ich część wspólną oraz wyniki nienależące do żadnego zdarzenia."
    )
  )
}

jezyk_decyzja_server <- function(input, output, session) {
  check_count <- reactiveVal(0L)

  observeEvent(input$ch5_check, {
    check_count(check_count() + 1L)
  })

  output$ch5_feedback <- renderUI({
    req(check_count() > 0)
    answer <- input$ch5_priority
    if (is.null(answer)) answer <- ""
    is_correct <- identical(answer, "insufficient")

    lc_status(
      lc_verdict(tags$strong(if (is_correct) "Dobrze." else "To zbyt szybki ranking."), type = if (is_correct) "ok" else "warning"),
      if (is_correct) {
        paste(
          "Prawdopodobieństwa odpowiadają tylko na część pytania.",
          "Priorytet wymaga jawnych kryteriów skutku, ekspozycji, barier i wykonalności działań."
        )
      } else {
        paste(
          "Wybrany argument może być ważny, ale sam nie wystarcza.",
          "Najpierw uzupełnij profil obu problemów i kryteria decyzji."
        )
      }
    )
  })
}

jezyk_cwiczenia_server <- function(input, output, session) {
  check_count <- reactiveVal(0L)
  sets_revealed <- reactiveVal(FALSE)
  models_check_count <- reactiveVal(0L)
  rubric_revealed <- reactiveVal(FALSE)

  observeEvent(input$ch8_check, {
    check_count(check_count() + 1L)
  })

  observeEvent(input$ch8_sets_solution, sets_revealed(TRUE))
  observeEvent(input$ch8_models_check, models_check_count(models_check_count() + 1L))
  observeEvent(input$ch8_transfer_rubric, rubric_revealed(TRUE))

  output$ch8_feedback <- renderUI({
    req(check_count() > 0)
    selected_fields <- input$ch8_fields
    if (is.null(selected_fields)) selected_fields <- character()
    selected_conclusion <- input$ch8_conclusion
    if (is.null(selected_conclusion)) selected_conclusion <- ""

    missing_fields <- setdiff(.required_report_fields, selected_fields)
    extra_fields <- setdiff(selected_fields, .required_report_fields)
    fields_ok <- length(missing_fields) == 0 && length(extra_fields) == 0
    conclusion_ok <- identical(selected_conclusion, "insufficient")
    all_ok <- fields_ok && conclusion_ok

    missing_labels <- c(
      definition = "definicja zdarzenia",
      exposure = "mianownik ekspozycji",
      period = "wspólny okres i warunki",
      consequence = "rodzaj skutków"
    )

    lc_status(
      lc_verdict(tags$strong(if (all_ok) "Rekomendacja jest kompletna." else "Wstrzymaj decyzję."), type = if (all_ok) "ok" else "warning"),
      if (!fields_ok) {
        paste0(
          " Brakuje: ",
          paste(unname(missing_labels[missing_fields]), collapse = ", "),
          "."
        )
      },
      if (!conclusion_ok) {
        " Same liczniki 3 i 5 nie pozwalają jeszcze ustalić, który magazyn jest bezpieczniejszy."
      },
      if (all_ok) {
        " Najpierw ujednolicamy definicje i ekspozycję, potem porównujemy częstości i skutki."
      }
    )
  })

  output$ch8_sets_feedback <- renderUI({
    req(sets_revealed())
    union_count <- 28 + 17 - 6
    neither_count <- 100 - union_count

    lc_status(
      tags$ol(
        tags$li("A ∩ B zawiera 6 kontroli — tę liczbę podano w treści."),
        tags$li(sprintf("A ∪ B zawiera 28 + 17 - 6 = %d kontroli.", union_count)),
        tags$li(sprintf("Ani A, ani B: 100 - %d = %d kontroli.", union_count, neither_count)),
        tags$li("Zdarzenia nie są rozłączne, ponieważ ich część wspólna zawiera 6 wyników.")
      )
    )
  })

  output$ch8_models_feedback <- renderUI({
    req(models_check_count() > 0)
    answers <- c(input$ch8_model_1, input$ch8_model_2, input$ch8_model_3)
    answers[is.na(answers)] <- ""
    correct <- c("classical", "empirical", "model")
    score <- sum(answers == correct)

    lc_status(
      lc_verdict(tags$strong(sprintf("Wynik: %d/3.", score)), type = if (score == 3) "ok" else "warning"),
      tags$ol(
        tags$li("Losowanie palety: definicja klasyczna, jeśli procedura zapewnia równe szanse."),
        tags$li("Rejestr zmian: częstość empiryczna z konkretnych obserwacji."),
        tags$li("Prognoza przy nowych warunkach: potrzebny dalszy model i dane o warunkach.")
      )
    )
  })

  output$ch8_transfer_feedback <- renderUI({
    req(rubric_revealed())
    lc_status(
      tags$strong("Sprawdź, czy odpowiedź zawiera:"),
      tags$ul(
        tags$li("źródło możliwej szkody, a nie tylko nazwę wypadku;"),
        tags$li("osoby i warunki ekspozycji;"),
        tags$li("jedno obserwowalne zdarzenie;"),
        tags$li("możliwy skutek oraz barierę;"),
        tags$li("mianownik, np. zmianę laboratoryjną, i jednoznaczny okres.")
      )
    )
  })
}

jezyk_server <- function(input, output, session) {
  jezyk_sytuacja_server(input, output, session)
  jezyk_czestosc_server(input, output, session)
  jezyk_przestrzen_server(input, output, session)
  jezyk_zbiory_server(input, output, session)
  jezyk_decyzja_server(input, output, session)
  jezyk_cwiczenia_server(input, output, session)
  risk_assessment_server("j1", jezyk_quiz, input, output)
}
