# Blok 05: Ile prób do zdarzenia -----------------------------------------

dozd_quiz <- list(questions = list(
  list(
  question = "Co jest ustalone w modelu ujemnym dwumianowym?",
  choices = c("Liczba oczekiwanych zdarzeń r" = "r", "Łączna liczba prób n" = "n", "Dokładny moment ostatniego zdarzenia" = "time"),
  correct = "r", explanation = "Eksperyment trwa do r-tego zdarzenia, więc liczba prób jest losowa."
),
  list(question = "Przy p=0,1 ile wynosi średnia liczba wszystkich prób do trzeciej wady?",
    choices = c("3" = "a", "27" = "b", "30" = "c"), correct = "c",
    explanation = "E(X)=r/p=30; 27 to średnia liczba niepowodzeń przed trzecim sukcesem."),
  list(question = "R zwrócił 27 niepowodzeń przed trzecim sukcesem. Ile było wszystkich prób?",
    choices = c("24" = "a", "30" = "b", "27" = "c"), correct = "b",
    explanation = "Dodajemy trzy próby zakończone sukcesem: X=Y+r."),
  list(question = "Po 20 próbach bez wady, przy stałym p i niezależności, szansa wady w następnej próbie…",
    choices = c("nadal wynosi p" = "a", "rośnie do 1" = "b", "spada do zera" = "c"), correct = "a",
    explanation = "Brak pamięci nie oznacza, że zdarzenie musi wkrótce nastąpić."),
  list(question = "Między seriami p jest losowe. Jak wyznaczyć średnią liczbę prób do r zdarzeń?",
    choices = c("r/E(p) zawsze" = "a", "E(p)/r" = "b", "r·E(1/p)" = "c"), correct = "c",
    explanation = "Warunkowo średnia wynosi r/p; następnie uśredniamy po rozkładzie p. Zmienność zmienia także średnią oczekiwania.")
))
dozd_exercises <- list(
  list(
    task = "Bananpol: przy p=0,10 wyznacz średnią liczbę kontroli do znalezienia trzech wadliwych zabezpieczeń i P(ukończenia do 40. kontroli).",
    answer = c(
      "E(X) = r/p = 3/0,10 = 30 kontroli.",
      "Audyt kończy się do 40. kontroli wtedy i tylko wtedy, gdy w 40 kontrolach znajdziemy co najmniej 3 wady: P(X ≤ 40) = P(B ≥ 3) dla B ~ Bin(40; 0,10) = 1 − [P(B=0) + P(B=1) + P(B=2)] ≈ 0,777. Mimo limitu o jedną trzecią wyższego od średniej mniej więcej co piąta seria go przekroczy."
    )
  ),
  list(
    task = "Diagnostyka: jakość zmienia się między partiami. Wyjaśnij, dlaczego model ze stałym p może zaniżyć niepewność planu.",
    answer = c(
      "Przy stałym p cała zmienność liczby kontroli wynika z losowości pojedynczych prób. Gdy p różni się między partiami, dochodzi drugie źródło zmienności — to, na jaką partię trafimy. Rozkład robi się szerszy, a jego prawy ogon grubszy.",
      "Dodatkowo średnia rośnie: przy losowym p wynosi r·E(1/p), a funkcja 1/p jest wypukła, więc r·E(1/p) ≥ r/E(p). Plan oparty na stałym p zaniża więc i średnią, i kwantyle."
    )
  ),
  list(
    task = "Transfer: audyt procedur BHP wykrywa naruszenie w pojedynczym audycie z prawdopodobieństwem p=0,05. Ile audytów zaplanować, żeby z prawdopodobieństwem co najmniej 90% zaobserwować dwa naruszenia?",
    answer = c(
      "Reguła zatrzymania: r = 2 naruszenia, więc X ~ ujemny dwumianowy z r = 2 i p = 0,05. Średnia: E(X) = 2/0,05 = 40 audytów.",
      "Szukamy najmniejszego n, dla którego P(X ≤ n) ≥ 0,90. W R: qnbinom(0.9, size = 2, prob = 0.05) + 2 = 77. Plan na 90% wymaga niemal dwukrotności średniej."
    )
  ),
  list(
    task = "Geometryczny: przy p=0,10 oblicz prawdopodobieństwo, że pierwsza wada nie pojawi się w pierwszych 30 kontrolach. Czy taki wynik audytu byłby mocnym dowodem, że p < 0,10?",
    answer = c(
      "P(X > 30) = (1 − p)^30 = 0,9^30 ≈ 0,042.",
      "To mniej niż 5%, więc wynik jest mało prawdopodobny przy p = 0,10 i przemawia za niższym p. Nie jest jednak dowodem: zdarza się w mniej więcej jednej serii na 24. Decyzja wymaga jawnego kryterium, a nie samego wrażenia „długo nic”."
    )
  ),
  list(
    task = "Brak pamięci: po 15 kontrolach bez wykrycia kierownik mówi: „wada musi się zaraz pojawić”. Oblicz P(wykrycia w kolejnych 5 kontrolach) przy p=0,10 i porównaj z prawdopodobieństwem wykrycia w pierwszych 5 kontrolach audytu.",
    answer = "Z własności braku pamięci oba prawdopodobieństwa są równe: 1 − 0,9^5 ≈ 0,410. Piętnaście kontroli bez wykrycia nie przybliża wykrycia — przy stałym p i niezależności proces „zaczyna się od nowa” po każdej kontroli."
  )
)

dozd_sciaga_widget <- tagList(
  figure_panel(
    label = "Ściąga 5.1",
    title = "Trzy rozkłady schematu Bernoulliego",
    full_width = TRUE,
    lc_table(
      data.frame(
        distribution = c("Dwumianowy", "Geometryczny", "Ujemny dwumianowy"),
        fixed = c("liczba prób n", "cel: 1 zdarzenie", "cel: r zdarzeń"),
        random = c("liczba zdarzeń", "liczba prób", "liczba prób"),
        question = c("Ile wad w partii 100 zaworów?", "Ile kontroli do pierwszej wady?",
                     "Ile kontroli do trzeciej wady?")
      ),
      cols = list(
        lc_col("distribution", "Rozkład", "row"),
        lc_col("fixed", "Co jest stałe", "text"),
        lc_col("random", "Co jest losowe", "text"),
        lc_col("question", "Pytanie inspektora", "text")
      ),
      narrow = "cards"
    )
  ),
  risk_assessment_ui("d5", dozd_quiz, dozd_exercises)
)

dozd_block <- list(id = "dozd", title = "Ile prób do zdarzenia", chapters = list(
  list(
    id = "regula", title = "Reguła zatrzymania", hook = "Moment zatrzymania zmienia rachunek", lead = "Dwumianowy zatrzymuje się po n próbach; ujemny dwumianowy po r zdarzeniach.",
    intro = c(
      "Audytor w Bananpolu nie pyta, ile wadliwych zabezpieczeń znajdzie w pięćdziesięciu kontrolach. Pyta, ile kontroli potrwa, zanim znajdzie trzy — bo na tyle musi zabudżetować czas i ludzi. To odwrócenie zmienia rozkład: losowa przestaje być liczba zdarzeń, a staje się liczba prób.",
      "Pojedyncze próby są dokładnie te same, co w poprzednim wykładzie — schemat Bernoulliego ze stałym p i niezależnością. Zmienia się wyłącznie reguła zatrzymania eksperymentu. Rozpoznanie, co jest stałe, a co losowe, jest pierwszym i najważniejszym krokiem doboru rozkładu."
    ),
    callout = list(
      label = "Dane Bananpolu",
      text = "Audyt zabezpieczeń ładunku: prawdopodobieństwo, że losowo wybrana paleta ma wadliwe zabezpieczenie, wynosi 0,10; cel audytu to r = 3 wykryte wady. Jednostka: kontrola jednej palety; horyzont: jedna seria audytowa. Liczby są fikcyjne.",
      color = "uwaga"
    ),
    sections = list(
      list(
        id = "zatrzymanie", title = "Stała liczba prób czy stała liczba wykryć",
        body = list(
          c(
            "Przypomnijmy, co daje schemat Bernoulliego. Mamy ciąg prób; każda kończy się jednym z dwóch wyników, które umownie nazywamy sukcesem i porażką. Prawdopodobieństwo sukcesu p jest w każdej próbie takie samo, a wyniki prób nie wpływają na siebie nawzajem. W audycie Bananpolu próbą jest kontrola jednej palety, a sukcesem — wykrycie wadliwego zabezpieczenia.",
            "Słowo „sukces” bywa w analizie ryzyka mylące, bo często oznacza zdarzenie niepożądane: wadę, awarię, wypadek. To tylko etykieta techniczna. Sukcesem nazywamy zdarzenie, które liczymy albo na które czekamy — niezależnie od tego, czy jest dla kogoś dobrą wiadomością.",
            "Sam schemat prób nie wystarcza jednak, żeby wskazać rozkład. Potrzebna jest jeszcze informacja, kiedy przestajemy wykonywać próby."
          ),
          risk_definition("5.1", "Reguła zatrzymania", c(
            "Reguła zatrzymania to zasada, która określa, po której próbie kończy się seria obserwacji.",
            "Przy regule „stała liczba prób” zatrzymujemy się po n-tej próbie, niezależnie od wyników; losowa jest wtedy liczba sukcesów. Przy regule „stała liczba sukcesów” zatrzymujemy się w chwili r-tego sukcesu, niezależnie od tego, ile prób to zajęło; losowa jest wtedy liczba prób."
          )),
          "Te same próby, z tym samym p, mogą więc prowadzić do dwóch różnych zmiennych losowych. Rozkład dwumianowy z poprzedniego wykładu opisuje pierwszą regułę. Ten wykład dotyczy drugiej. Zanim przejdziemy do wzorów, sprawdź, czy potrafisz wskazać regułę w konkretnym planie audytu.",
          risk_vote_panel("d5_vote", "d5_vote_feedback", "Chcemy znaleźć trzy wadliwe zabezpieczenia. Który element eksperymentu jest stały?", c("r=3 znalezione wady" = "r", "n — liczba kontroli" = "n", "odsetek wad w zebranej próbie" = "share")),
          "Kuszącą odpowiedzią jest odsetek wad: przecież p = 0,10 jest stałe. Ale p to parametr modelu, a nie element planu eksperymentu. Odsetek wad w zebranej próbie jest wynikiem i zmienia się od serii do serii. Plan audytu ustala tylko jedno: kończymy po trzeciej wykrytej wadzie."
        )
      ),
      list(
        id = "rozpoznanie", title = "Trzy pytania, trzy rozkłady",
        body = list(
          c(
            "W praktyce reguła zatrzymania rzadko jest nazwana wprost. Trzeba ją odczytać z pytania, które zadaje decydent. „Ile wad znajdziemy w stu kontrolach?” ustala liczbę prób i prowadzi do rozkładu dwumianowego. „Ile kontroli do pierwszej wady?” ustala liczbę sukcesów na jeden — to rozkład geometryczny. „Ile kontroli do trzeciej wady?” ustala liczbę sukcesów na r = 3 — to rozkład ujemny dwumianowy.",
            "Pomocny test: wyobraź sobie, że audyt właśnie się skończył. Co jest pewne, zanim spojrzysz w protokół? Jeśli wiesz, ile było kontroli, ale nie wiesz, ile wad — liczba prób była ustalona. Jeśli wiesz, że ostatnia kontrola wykryła wadę, a liczba wad jest z góry znana — ustalony był cel."
          ),
          risk_example("5.1", "Rozpoznaj regułę zatrzymania",
            problem = list(
              "Wskaż, co jest stałe, co losowe i jaki rozkład opisuje zmienną w każdej sytuacji.",
              risk_parts(
                "Magazynier kontroluje 40 palet z dostawy i zapisuje, ile ma wadliwe zabezpieczenie.",
                "Dział jakości kontroluje palety, dopóki nie znajdzie pierwszej wadliwej — wtedy wstrzymuje przyjęcie dostawy.",
                "Audytor potrzebuje pięciu wadliwych palet jako materiału do analizy przyczyn i kontroluje palety, dopóki ich nie zbierze."
              )
            ),
            steps = c(
              "Stała jest liczba prób n = 40; losowa liczba wad. Rozkład dwumianowy Bin(40; p).",
              "Stały jest cel: jedna wada; losowa liczba kontroli. Rozkład geometryczny z parametrem p.",
              "Stały jest cel: r = 5 wad; losowa liczba kontroli. Rozkład ujemny dwumianowy z parametrami r = 5 i p."
            ),
            steps_type = "a",
            answer = "(a) dwumianowy, (b) geometryczny, (c) ujemny dwumianowy. We wszystkich trzech sytuacjach pojedyncza kontrola wygląda tak samo; różni się tylko to, kiedy kończymy."
          ),
          risk_check("d5_chk_regula",
            "Służba BHP obserwuje zmiany robocze, dopóki nie zarejestruje drugiego poślizgnięcia. Która zmienna jest losowa?",
            c("Liczba poślizgnięć" = "events", "Liczba obserwowanych zmian" = "trials", "Żadna — obie są ustalone" = "none"),
            correct = "trials",
            explanation = "Cel r = 2 jest ustalony, więc losowa jest liczba zmian potrzebnych do jego osiągnięcia. To model ujemny dwumianowy.",
            hints = c(events = "Liczba poślizgnięć jest z góry znana: obserwacja kończy się po drugim.", none = "Gdyby obie były ustalone, nie byłoby czego modelować. Która liczba wynika z przebiegu obserwacji?")
          )
        )
      )
    )
  ),
  list(
    id = "geometryczny", title = "Rozkład geometryczny", hook = "Pierwsze wykrycie bywa szybkie albo bardzo późne", lead = "Rozkład geometryczny ma długi ogon: sukces może nadejść szybko albo bardzo późno.",
    intro = c(
      "Najprostsza wersja pytania: ile kontroli do pierwszej wady? Zanim padnie jakikolwiek wzór, zbuduj wyczucie — uruchom symulację kilka razy i obserwuj kształt histogramu: gdzie jest szczyt, jak długo ciągnie się ogon, jak często seria kończy się już przy pierwszych kontrolach.",
      "Dwie rzeczy powinny zwrócić uwagę. Najbardziej prawdopodobna jest zawsze pierwsza kontrola, a każda kolejna coraz mniej — mimo to średnia bywa myląca: przy p = 0,10 średnio czekamy 10 kontroli, ale co dziesiąta seria przekroczy 22 kontrole."
    ),
    sections = list(
      list(
        id = "symulacja", title = "Symulacja: jak długo czekamy?",
        body = list(
          risk_try("zostaw p = 0,10 i kliknij „Losuj ponownie” kilka razy. Zapisz, gdzie leży najwyższy słupek i jak daleko sięga najdłuższa seria. Potem zmień p na 0,30 i na 0,03."),
          risk_widget_panel("Symulacja", "Ile kontroli do pierwszej wady?", tagList(lc_slider("d5_geo_p", "Prawdopodobieństwo wady p", .01, .5, .1, .01), lc_action("d5_geo_run", "Losuj ponownie", icon = "shuffle")), "d5_geo", "d5_geo_stats"),
          c(
            "Niezależnie od p najwyższy słupek stoi przy pierwszej kontroli. Kolejne słupki systematycznie maleją, ale bardzo powoli, gdy p jest małe. Pojedyncze serie ciągną się kilka razy dłużej niż średnia. Przy p = 0,03 średni czas oczekiwania to około 33 kontroli, a najdłuższe serie wychodzą poza prawą krawędź wykresu, czyli ponad 80 kontroli.",
            "Te obserwacje mają proste wyjaśnienie rachunkowe, które wyprowadzimy w następnej sekcji. Warto je jednak najpierw zobaczyć: w planowaniu zasobów to właśnie długi ogon, a nie średnia, sprawia kłopot."
          )
        )
      ),
      list(
        id = "wzor", title = "Wzór na pierwsze wykrycie",
        body = list(
          c(
            "Oznaczmy przez X numer kontroli, w której pojawia się pierwsza wada. Zdarzenie {X = x} zachodzi dokładnie wtedy, gdy pierwsze x − 1 kontroli kończy się bez wykrycia, a x-ta kontrola wykrywa wadę. Z niezależności prób prawdopodobieństwo takiej drogi to iloczyn prawdopodobieństw wzdłuż niej — ten sam mechanizm, który w wykładzie o warunkach stosowaliśmy do drzewa zdarzeń."
          ),
          risk_definition("5.2", "Rozkład geometryczny", c(
            "Zmienna X ma rozkład geometryczny z parametrem p (0 < p ≤ 1), jeśli przyjmuje wartości 1, 2, 3, … z prawdopodobieństwami danymi wzorem (5.1). X to numer próby, w której w schemacie Bernoulliego pojawia się pierwszy sukces."
          )),
          risk_formula("P(X=x)=(1-p)^{x-1}\\,p,\\qquad x=1,2,3,\\ldots", num = "5.1",
            legend = c("x" = "numer kontroli z pierwszym wykryciem", "p" = "prawdopodobieństwo wykrycia w jednej kontroli", "(1-p)^{x-1}" = "prawdopodobieństwo x − 1 kontroli bez wykrycia z rzędu")),
          c(
            "Wzór od razu tłumaczy kształt histogramu z symulacji. Iloraz sąsiednich prawdopodobieństw jest stały: P(X = x + 1) / P(X = x) = 1 − p. Każdy słupek jest więc poprzednim pomnożonym przez 1 − p. Przy p = 0,10 słupki maleją zaledwie o 10% na krok — stąd powolne opadanie i długi ogon.",
            "Do planowania częściej niż pojedyncze słupki potrzebne jest prawdopodobieństwo, że czekamy dłużej niż x kontroli. Tu przydaje się trik z dopełnieniem: X > x oznacza dokładnie tyle, że pierwsze x kontroli nie wykryło niczego."
          ),
          risk_formula("P(X>x)=(1-p)^{x},\\qquad P(X\\le x)=1-(1-p)^{x}", num = "5.2"),
          "Średnia i wariancja rozkładu geometrycznego mają proste postaci:",
          risk_formula("E(X)=\\frac{1}{p},\\qquad \\operatorname{Var}(X)=\\frac{1-p}{p^{2}}", num = "5.3"),
          risk_derivation("E(X) = 1/p", c(
            "Najkrótsze uzasadnienie wykorzystuje analizę pierwszego kroku. Pierwsza kontrola zawsze się odbywa. Z prawdopodobieństwem p kończy serię; z prawdopodobieństwem 1 − p nic nie wykrywa i — dzięki niezależności — sytuacja zaczyna się od nowa, z tą samą oczekiwaną liczbą dalszych kontroli.",
            "Oznaczając m = E(X), dostajemy równanie, które ma jedno rozwiązanie:"
          ), lines = c("m = 1 + (1 − p) · m", "m − (1 − p) · m = 1", "p · m = 1", "m = 1/p")),
          "Wzór (5.3) mówi coś intuicyjnego: przy p = 0,10 wada trafia się średnio raz na dziesięć kontroli, więc średnio czekamy dziesięć kontroli. Wariancja jest jednak duża — odchylenie standardowe przy p = 0,10 to około 9,5 kontroli, prawie tyle co średnia. Średnia sama nie opisuje ryzyka długiego czekania.",
          risk_example("5.2", "Pierwsza wada w audycie Bananpolu",
            problem = list(
              "Przy p = 0,10 oblicz:",
              risk_parts(
                "P(X = 1) i P(X = 3).",
                "Prawdopodobieństwo znalezienia wady najpóźniej w 10. kontroli; porównaj wynik z tym, co sugeruje średnia E(X) = 10.",
                "Prawdopodobieństwo, że trzeba będzie więcej niż 22 kontroli."
              )
            ),
            steps = c(
              "Ze wzoru (5.1): P(X = 1) = 0,10; P(X = 3) = 0,9² · 0,1 = 0,081.",
              "Ze wzoru (5.2): P(X ≤ 10) = 1 − 0,9¹⁰ ≈ 1 − 0,349 = 0,651. Mediana to najmniejsze x, dla którego P(X ≤ x) ≥ 0,5: 0,9⁶ ≈ 0,531, a 0,9⁷ ≈ 0,478, więc mediana wynosi 7 — wyraźnie mniej niż średnia 10.",
              "P(X > 22) = 0,9²² ≈ 0,098 — mniej więcej co dziesiąta seria."
            ),
            steps_type = "a",
            answer = "W 65% serii wada pojawia się do 10. kontroli, a połowa serii kończy się do 7. kontroli. Średnią 10 podnoszą rzadkie, bardzo długie serie z prawego ogona."
          ),
          risk_check("d5_chk_geo",
            "Przy p = 0,10 która wartość jest bardziej prawdopodobna: X = 1 czy X = 10?",
            c("X = 1" = "one", "X = 10, bo to średnia" = "ten", "Są jednakowo prawdopodobne" = "equal"),
            correct = "one",
            explanation = "P(X = 1) = 0,1, a P(X = 10) = 0,9⁹ · 0,1 ≈ 0,039. W rozkładzie geometrycznym najbardziej prawdopodobna jest zawsze pierwsza próba; średnia nie jest wartością najczęstszą.",
            hints = c(ten = "Średnia nie musi być wartością najbardziej prawdopodobną. Porównaj słupki ze wzoru (5.1).", equal = "Każdy kolejny słupek to poprzedni pomnożony przez 1 − p.")
          )
        )
      ),
      list(
        id = "pamiec", title = "Brak pamięci",
        body = list(
          c(
            "Rozkład geometryczny nie pamięta porażek: po dwudziestu kontrolach bez wykrycia rozkład dalszego oczekiwania wygląda dokładnie tak samo jak na początku. Seria „bez wady” nie zwiastuje bliskiego wykrycia — to dyskretna wersja własności, którą w wykładzie o czasie życia spotkamy pod nazwą stałego hazardu.",
            "Formalnie: jeśli wiemy, że pierwsze s kontroli nic nie wykryło, prawdopodobieństwo czekania jeszcze ponad t kontroli jest takie samo, jak prawdopodobieństwo czekania ponad t kontroli od początku."
          ),
          risk_formula("P(X>s+t\\mid X>s)=P(X>t)", num = "5.4"),
          "Dowód wymaga tylko definicji prawdopodobieństwa warunkowego z wykładu 02 i wzoru (5.2). Zdarzenie {X > s + t} zawiera się w {X > s}, więc ich iloczyn to po prostu {X > s + t}. Stąd P(X > s + t | X > s) = P(X > s + t) / P(X > s) = (1 − p)^(s+t) / (1 − p)^s = (1 − p)^t = P(X > t).",
          risk_example("5.3", "Dwadzieścia kontroli bez wykrycia",
            problem = "Audyt przy p = 0,10 trwa już 20 kontroli i nie wykrył żadnej wady. Jakie jest prawdopodobieństwo, że wada pojawi się w ciągu kolejnych 10 kontroli? Jak częsta jest w ogóle seria 20 kontroli bez wykrycia?",
            steps = c(
              "Z braku pamięci (5.4): P(X ≤ 30 | X > 20) = 1 − P(X > 30 | X > 20) = 1 − P(X > 10) = 1 − 0,9¹⁰ ≈ 0,651.",
              "To dokładnie ta sama liczba co w przykładzie 5.2(b): szansa wykrycia w najbliższych 10 kontrolach nie zależy od tego, ile już czekaliśmy.",
              "Sama seria 20 kontroli bez wykrycia: P(X > 20) = 0,9²⁰ ≈ 0,122 — zdarza się w mniej więcej jednym audycie na osiem."
            ),
            answer = "Około 0,65, tak samo jak na początku audytu. Seria 20 kontroli bez wykrycia nie jest ani rzadka, ani nie „zbliża” wykrycia."
          ),
          "Brak pamięci jest własnością modelu, a nie świata. Jeśli kontroler się męczy, jeśli palety są ustawione w kolejności dostaw albo jeśli wady występują skupiskami, historia serii niesie informację i model geometryczny przestaje obowiązywać. Do tych założeń wrócimy w rozdziale o tym, kiedy model zawodzi."
        )
      )
    ),
    takeaway = "Brak wykrycia po trzydziestu kontrolach nie dowodzi, że wad nie ma. Model geometryczny pozwala policzyć, jak prawdopodobne jest tak długie oczekiwanie przy przyjętym p; dopiero jawne kryterium decyzyjne mówi, kiedy przerwać kontrolę."
  ),
  list(
    id = "rte", title = "Rozkład ujemny dwumianowy", hook = "Trzy wykrycia to trzy kolejki czekania", lead = "Łączna liczba prób jest sumą czasów oczekiwania na kolejne wykrycia, a oprogramowanie może liczyć ją na dwa sposoby.",
    intro = c(
      "Audytor potrzebuje trzech wykrytych wad, nie jednej. Oczekiwanie na trzecią wadę to trzy sklejone oczekiwania geometryczne: do pierwszej, potem do drugiej, potem do trzeciej. Suma tych trzech czasów ma rozkład ujemny dwumianowy.",
      "Współczynnik we wzorze zlicza układy: ostatnia, x-ta kontrola musi zakończyć się wykryciem, a wcześniejsze r−1 wykryć może rozmieścić się dowolnie wśród x−1 poprzednich kontroli. Porównaj kształt rozkładu z geometrycznym: im większe r, tym rozkład bardziej symetryczny i dalszy od zera."
    ),
    sections = list(
      list(
        id = "suma", title = "Suma czasów oczekiwania",
        body = list(
          c(
            "Zacznijmy od obserwacji, która daje średnią bez żadnego nowego wzoru. Po pierwszym wykryciu, dzięki niezależności i stałemu p, audyt „zaczyna się od nowa”: liczba kontroli do drugiego wykrycia ma znów rozkład geometryczny, niezależny od tego, co było wcześniej. To samo dotyczy trzeciego wykrycia.",
            "Łączna liczba kontroli X jest więc sumą r niezależnych zmiennych geometrycznych G₁ + G₂ + … + G_r. Średnia sumy to suma średnich, a — dla niezależnych składników — wariancja sumy to suma wariancji. Ze wzoru (5.3) dostajemy od razu:"
          ),
          risk_formula("E(X)=\\frac{r}{p},\\qquad \\operatorname{Var}(X)=\\frac{r(1-p)}{p^{2}}", num = "5.5",
            legend = c("r" = "liczba wykryć, na którą czekamy", "p" = "prawdopodobieństwo wykrycia w jednej kontroli")),
          "Dla audytu Bananpolu (r = 3, p = 0,10): E(X) = 30 kontroli, Var(X) = 270, odchylenie standardowe około 16,4 kontroli. Rozrzut względem średniej jest mniejszy niż w rozkładzie geometrycznym — dodawanie niezależnych oczekiwań częściowo uśrednia pecha i szczęście — ale wciąż duży."
        )
      ),
      list(
        id = "rozklad", title = "Wzór na r-te wykrycie",
        body = list(
          c(
            "Średnia to za mało do planowania; potrzebujemy całego rozkładu. Zdarzenie {X = x} — trzecia wada pojawia się dokładnie w x-tej kontroli — rozkłada się na dwa niezależne warunki. Po pierwsze, x-ta kontrola wykrywa wadę (prawdopodobieństwo p). Po drugie, wśród wcześniejszych x − 1 kontroli jest dokładnie r − 1 wykryć. Drugi warunek to rozkład dwumianowy z poprzedniego wykładu: C(x−1, r−1) · p^(r−1) · (1 − p)^(x−r). Mnożąc oba czynniki, dostajemy wzór (5.6)."
          ),
          risk_definition("5.3", "Rozkład ujemny dwumianowy", c(
            "Zmienna X ma rozkład ujemny dwumianowy z parametrami r (liczba całkowita, r ≥ 1) i p (0 < p ≤ 1), jeśli przyjmuje wartości r, r + 1, r + 2, … z prawdopodobieństwami danymi wzorem (5.6). X to numer próby, w której w schemacie Bernoulliego pojawia się r-ty sukces."
          )),
          risk_formula("P(X=x)=\\binom{x-1}{r-1}p^{r}(1-p)^{x-r},\\qquad x=r,r+1,\\ldots", num = "5.6",
            legend = c("\\binom{x-1}{r-1}" = "liczba sposobów rozmieszczenia r − 1 wcześniejszych wykryć wśród x − 1 kontroli", "p^{r}" = "r kontroli z wykryciem", "(1-p)^{x-r}" = "x − r kontroli bez wykrycia")),
          risk_example("5.4", "Trzecia wada w konkretnej kontroli",
            problem = list(
              "Przy p = 0,10 i r = 3 oblicz:",
              risk_parts(
                "Prawdopodobieństwo, że trzecia wada pojawi się dokładnie w 5. kontroli, wypisując wszystkie sprzyjające układy.",
                "Prawdopodobieństwo, że trzecia wada pojawi się dokładnie w 30. kontroli."
              )
            ),
            steps = c(
              "Piąta kontrola musi wykryć wadę; dwa wcześniejsze wykrycia mieszczą się wśród kontroli 1–4. Możliwe pary pozycji: {1,2}, {1,3}, {1,4}, {2,3}, {2,4}, {3,4} — to C(4, 2) = 6 układów. Każdy układ ma 3 wykrycia i 2 kontrole bez wykrycia, więc prawdopodobieństwo każdego to 0,1³ · 0,9² = 0,00081. Razem: 6 · 0,00081 = 0,00486.",
              "C(29, 2) = 406 układów; każdy ma prawdopodobieństwo 0,1³ · 0,9²⁷ ≈ 0,001 · 0,0581. Razem: 406 · 0,0000581 ≈ 0,0236."
            ),
            steps_type = "a",
            answer = "(a) około 0,005; (b) około 0,024. Nawet wartość równa średniej ma małe prawdopodobieństwo — rozkład rozciąga się na kilkadziesiąt możliwych wartości."
          ),
          risk_try("ustaw r = 1 i porównaj wykres z histogramem z rozdziału o pierwszym wykryciu. Potem zwiększaj r do 10 przy stałym p i obserwuj, jak przesuwa się szczyt i zmienia symetria."),
          risk_widget_panel("Rozkład", "Łączna liczba kontroli", tagList(lc_slider("d5_p", "p wykrycia", .01, .5, .1, .01), lc_slider("d5_r", "r", 1, 10, 3, 1)), "d5_nb", "d5_nb_stats"),
          c(
            "Dla r = 3 i p = 0,10 najwyższe słupki stoją przy 20 i 21 kontrolach, mediana wynosi 27, a średnia 30. Trzy różne „środki” rozkładu leżą w różnych miejscach, bo rozkład jest prawostronnie skośny. W miarę wzrostu r skośność maleje, a kształt coraz bardziej przypomina dzwon — to zapowiedź rozkładu normalnego z następnego wykładu."
          ),
          risk_check("d5_chk_nb",
            "Dlaczego we wzorze (5.6) jest C(x−1, r−1), a nie C(x, r)?",
            c("Bo ostatnia, x-ta kontrola musi być wykryciem" = "last", "Bo pierwsza kontrola nigdy nie wykrywa wady" = "first", "To tylko inna konwencja zapisu, wynik jest ten sam" = "same"),
            correct = "last",
            explanation = "Pozycja ostatniego wykrycia jest wymuszona przez regułę zatrzymania. Swobodnie rozmieszczamy tylko r − 1 wcześniejszych wykryć wśród x − 1 wcześniejszych kontroli.",
            hints = c(first = "Pierwsza kontrola może wykryć wadę. Który element serii jest wymuszony przez regułę zatrzymania?", same = "C(x, r) liczyłoby też układy, w których r-te wykrycie nastąpiło przed x-tą kontrolą — audyt skończyłby się wcześniej.")
          )
        ),
        takeaway = "Dla r = 1 ujemny dwumianowy pokrywa się z geometrycznym — warto to sprawdzić suwakiem. Uogólnienie nie dodaje nowych założeń: nadal stałe p i niezależne próby, zmienia się tylko cel."
      ),
      list(
        id = "parametryzacje", title = "Dwie parametryzacje",
        text = c(
          "Podręczniki i biblioteki liczą ten sam rozkład na dwa sposoby: jako łączną liczbę prób X albo jako liczbę niepowodzeń Y przed r-tym sukcesem. Funkcje R z rodziny nbinom używają drugiej konwencji — dlatego w kodzie tego kursu do wyniku dodaje się r. Obie wersje opisują tę samą serię kontroli; różnią się tylko tym, co liczą.",
          "Jeżeli znaleziono r zdarzeń po X wszystkich próbach, liczba wcześniejszych niepowodzeń wynosi X−r. Przeliczenie jest trywialne, ale tylko wtedy, gdy wiadomo, którą wielkość podaje źródło — w raporcie zawsze nazwij, co oznacza oś."
        ),
        formula = "X_{wszystkie}=Y_{niepowodzenia}+r",
        body = list(
          risk_example("5.5", "Co zwraca R?",
            problem = "Wywołanie dnbinom(27, size = 3, prob = 0.1) zwraca około 0,0236, a qnbinom(0.5, size = 3, prob = 0.1) zwraca 24. Zinterpretuj oba wyniki w języku audytu Bananpolu.",
            steps = c(
              "R liczy Y — liczbę kontroli bez wykrycia przed trzecim wykryciem. dnbinom(27, …) to P(Y = 27).",
              "Y = 27 oznacza X = 27 + 3 = 30 kontroli łącznie. Wynik 0,0236 zgadza się z przykładem 5.4(b).",
              "qnbinom(0.5, …) = 24 to mediana Y. Mediana łącznej liczby kontroli to 24 + 3 = 27."
            ),
            answer = "Prawdopodobieństwo, że audyt skończy się dokładnie na 30. kontroli, to około 0,024; mediana czasu audytu to 27 kontroli. Bez dodania r do kwantyla plan byłby zaniżony o trzy kontrole."
          )
        ),
        pitfall = "Bez nazwania parametryzacji wynik może różnić się dokładnie o r."
      )
    )
  ),
  list(
    id = "zasoby", title = "Limit planistyczny", hook = "Średnio wystarczy, a i tak zabraknie", lead = "Średnia r/p nie gwarantuje ukończenia przed limitem.",
    intro = c(
      "Przy p = 0,10 i celu r = 3 średnia liczba kontroli wynosi 30. Czy zaplanowanie dokładnie 30 kontroli wystarczy? Kalkulator poniżej pokazuje, że szansa ukończenia audytu w 30 kontrolach to niespełna 60% — rozkład jest skośny i długa seria pechowych kontroli wcale nie jest rzadka.",
      "Plan zasobów buduje się więc na kwantylu, nie na średniej: limit kontroli dobieramy tak, żeby prawdopodobieństwo ukończenia audytu przed limitem osiągnęło uzgodniony poziom, na przykład 95%. Różnica między średnią a kwantylem to właśnie zapas planistyczny."
    ),
    sections = list(
      list(
        id = "limit", title = "Średnia a kwantyl",
        formula = "E(X)=\\frac{r}{p}",
        body = list(
          c(
            "Pytanie planisty brzmi: jakie jest prawdopodobieństwo, że audyt skończy się najpóźniej w n-tej kontroli? Można by sumować wzór (5.6) od r do n, ale istnieje krótsza droga, która łączy ten wykład z poprzednim.",
            "Audyt kończy się najpóźniej w n-tej kontroli wtedy i tylko wtedy, gdy w pierwszych n kontrolach znaleziono co najmniej r wad. To dwa opisy tego samego zdarzenia: jeden w języku reguły „do r-tego sukcesu”, drugi w języku reguły „n prób”."
          ),
          risk_formula("P(X\\le n)=P(B_n\\ge r),\\qquad B_n\\sim \\mathrm{Bin}(n,p)", num = "5.7",
            legend = c("X" = "łączna liczba kontroli do r-tego wykrycia", "B_n" = "liczba wykryć w pierwszych n kontrolach")),
          risk_example("5.6", "Czy średnia wystarczy jako limit?",
            problem = "Przy p = 0,10 i r = 3 oblicz prawdopodobieństwo ukończenia audytu w limicie 30 kontroli, czyli limicie równym średniej.",
            steps = c(
              "Ze wzoru (5.7): P(X ≤ 30) = P(B ≥ 3) dla B ~ Bin(30; 0,1) = 1 − [P(B=0) + P(B=1) + P(B=2)].",
              "P(B=0) = 0,9³⁰ ≈ 0,0424.",
              "P(B=1) = 30 · 0,1 · 0,9²⁹ ≈ 0,1413.",
              "P(B=2) = C(30, 2) · 0,1² · 0,9²⁸ = 435 · 0,01 · 0,0523 ≈ 0,2277.",
              "P(X ≤ 30) ≈ 1 − 0,4114 = 0,589."
            ),
            answer = "Około 0,59. Limit równy średniej zawodzi w ponad czterech audytach na dziesięć."
          ),
          risk_definition("5.4", "Limit planistyczny", c(
            "Limit planistyczny na poziomie α to najmniejsza liczba prób n, dla której P(X ≤ n) ≥ α. Jest to kwantyl rzędu α rozkładu liczby prób. Różnicę między limitem planistycznym a średnią nazywamy zapasem planistycznym."
          )),
          risk_try("przesuwaj limit i znajdź najmniejszą wartość, przy której P(ukończenia do limitu) przekracza 0,95. Porównaj ją ze średnią i z 95. percentylem wyświetlanym w kalkulatorze."),
          figure_panel(label = "Kalkulator", title = "Limit liczby kontroli", lc_slider("d5_limit", "Limit", 3, 200, 40, 1), uiOutput("d5_plan"), full_width = TRUE),
          "Dla audytu Bananpolu limit na poziomie 95% wynosi 61 kontroli — dwa razy więcej niż średnia. Zapas planistyczny to 31 kontroli. Nie jest to zapas „na wszelki wypadek”; to bezpośrednia konsekwencja kształtu rozkładu. Im mniejsze p, tym dłuższy ogon i tym większy zapas przy tym samym poziomie pewności.",
          risk_check("d5_chk_plan",
            "Mediana liczby kontroli wynosi 27, a średnia 30. Co to mówi o limicie równym średniej?",
            c("Ukończy się w nim ponad połowa audytów" = "more", "Ukończy się w nim dokładnie połowa audytów" = "half", "Ukończy się w nim mniej niż połowa audytów" = "less"),
            correct = "more",
            explanation = "Skoro mediana (27) jest mniejsza od średniej, do 30. kontroli kończy się więcej niż połowa audytów — dokładnie około 59%. W rozkładach prawostronnie skośnych średnia leży powyżej mediany.",
            hints = c(half = "Połowa audytów kończy się do mediany, nie do średniej.", less = "Mediana leży przed średnią. Co to znaczy dla odsetka audytów zakończonych do 30. kontroli?")
          )
        )
      ),
      list(
        id = "plan", title = "Ile kontroli zaplanować?",
        text = "Wynik tego rachunku trafia do jednego dokumentu: planu audytu. Dobry plan nie obiecuje, że audyt się uda — podaje, z jakim prawdopodobieństwem uda się w ramach przyznanych zasobów, i co się stanie, jeśli limit zostanie osiągnięty bez ukończenia celu. Ta ostatnia pozycja jest najczęściej pomijana, a to ona decyduje, czy przekroczenie limitu będzie kontrolowaną decyzją, czy improwizacją.",
        bullets = c("cel r i definicja wykrycia", "p i jego źródło", "limit zasobów", "P(ukończenia przed limitem)", "reakcja, gdy limit zostanie przekroczony"),
        body = "Wybór poziomu α nie jest decyzją statystyczną. Wyższy poziom kosztuje: z 90% na 95% limit rośnie z 52 do 61 kontroli, a z 95% na 99% — do 81. Rachunek pokazuje koszt każdego punktu procentowego pewności; to, ile pewności się opłaca, zależy od konsekwencji niedokończonego audytu.",
        decision = "Planuj na podstawie prawdopodobieństwa ukończenia lub kwantyla, a nie tylko średniej; oddziel oczekiwaną liczbę kontroli od bezpiecznego zapasu planistycznego."
      )
    )
  ),
  list(
    id = "zawodzi", title = "Założenia modelu", hook = "Nie każda partia jest taka sama", lead = "Stałe p i niezależność są założeniami operacyjnymi.",
    intro = c(
      "Stałe p brzmi niewinnie, ale w praktyce oznacza: każda kontrolowana paleta pochodzi z tej samej populacji jakości. Gdy dostawy przychodzą od różnych dostawców albo jakość dryfuje w czasie, p zmienia się między partiami — a rozkład liczby kontroli robi się szerszy, niż obiecuje model.",
      "Symulacja porównuje świat stałego p ze światem, w którym p losuje się osobno dla każdej partii. Zmienia się także średnia: przy losowym p wynosi r·E(1/p), a nie r/E(p). Funkcja 1/p jest wypukła, więc przy tej samej średniej p zmienność wydłuża przeciętne oczekiwanie. Różni się również ogon — czyli dokładnie ta część rozkładu, na której opiera się plan zasobów. Niedoszacowany ogon to audyt, który „niespodziewanie” trwa dwa razy dłużej."
    ),
    sections = list(
      list(
        id = "mieszanka", title = "Partie o różnej jakości",
        body = list(
          "Najłatwiej zobaczyć mechanizm na najprostszej mieszance: dwóch rodzajach partii. Średnie p się nie zmienia, a mimo to średni czas audytu rośnie.",
          risk_example("5.7", "Dwaj dostawcy",
            problem = "Połowa partii pochodzi od dostawcy A (p = 0,05), połowa od dostawcy B (p = 0,15). Średnie p wynosi 0,10, tak jak dotąd. Audyt obejmuje jedną losowo wybraną partię i trwa do trzeciej wady. Oblicz średnią liczbę kontroli.",
            steps = c(
              "Warunkowo, przy znanej partii, liczba kontroli ma rozkład ujemny dwumianowy, więc ze wzoru (5.5): dla A średnia 3/0,05 = 60, dla B średnia 3/0,15 = 20.",
              "Uśredniamy po partiach (wzór na prawdopodobieństwo całkowite w wersji dla średnich): 0,5 · 60 + 0,5 · 20 = 40.",
              "Naiwny rachunek r/E(p) = 3/0,10 = 30 zaniża średnią o 10 kontroli."
            ),
            answer = "40 kontroli, a nie 30. Partie o niskim p wydłużają audyt bardziej, niż partie o wysokim p go skracają, bo czas oczekiwania zależy od 1/p, a nie od p."
          ),
          risk_try("zacznij od odchylenia p równego zero i sprawdź, że oba histogramy się pokrywają. Potem zwiększaj odchylenie i obserwuj prawy ogon oraz średnie w panelu."),
          risk_widget_panel("Porównanie", "Stałe p kontra partie o różnej jakości", lc_slider("d5_variation", "Odchylenie p przed ograniczeniem do [0,005; 0,95]", 0, .09, .04, .005), "d5_failure", "d5_failure_stats"),
          "Przy rosnącym odchyleniu histogram „zmiennego p” wyraźnie wyciąga się w prawo, a średnia symulowana rośnie. Szczególnie groźne są partie o bardzo małym p: przy p = 0,02 średni czas do trzech wad to 150 kontroli. Kilka takich partii wystarczy, by plan oparty na stałym p stał się fikcją.",
          risk_check("d5_chk_mix",
            "Średnie p w dwóch rodzajach partii wynosi 0,10. Średni czas do trzeciej wady w mieszance jest…",
            c("równy 30, bo średnie p się nie zmieniło" = "equal", "większy niż 30" = "greater", "mniejszy niż 30" = "smaller"),
            correct = "greater",
            explanation = "Czas oczekiwania rośnie jak 1/p, a ta funkcja jest wypukła. Uśrednianie po partiach daje r·E(1/p) ≥ r/E(p); równość zachodzi tylko wtedy, gdy p jest stałe.",
            hints = c(equal = "Średnia czasu zależy od średniej 1/p, nie od 1/(średnia p). Policz przykład 5.7.", smaller = "Porównaj, o ile wydłuża audyt partia z p = 0,05, a o ile skraca go partia z p = 0,15.")
          )
        )
      ),
      list(
        id = "diagnostyka", title = "Jak sprawdzić założenia?",
        text = c(
          "Model ma dwa założenia operacyjne i każde da się skonfrontować z danymi z wcześniejszych audytów. Stałość p sprawdzamy, porównując odsetki wad między partiami, dostawcami i okresami. Jeśli różnice są większe, niż wynikałoby z samej losowości rozkładu dwumianowego, p nie jest stałe.",
          "Niezależność sprawdzamy, patrząc na kolejność wyników w protokole. Wady zbite w serie — na przykład kilka wadliwych palet z rzędu z jednej dostawy — sugerują wspólną przyczynę. Wtedy wykrycie jednej wady zmienia szansę wykrycia następnej, a rozkład liczby kontroli może wyglądać zupełnie inaczej niż ujemny dwumianowy — zwykle jest bardziej rozproszony. Podobnie działa uczenie się kontrolera: p rośnie z każdą kolejną kontrolą, więc nie jest stałe."
        )
      ),
      list(
        id = "transfer", title = "Przykład transferowy: poszukiwania i rekrutacja",
        text = "Model „ile prób do r-tego sukcesu” pojawia się wszędzie tam, gdzie szuka się rzadkich obiektów: liczba odwiertów do drugiego złoża, liczba rozmów rekrutacyjnych do trzeciego zatrudnienia, liczba testów do wykrycia r-tej usterki oprogramowania. We wszystkich tych zastosowaniach ta sama pułapka: sukcesy zmieniają proces (uczenie, wyczerpanie puli), więc stałość p trzeba sprawdzić, zanim rozkład stanie się planem."
      )
    ),
    pitfall = "Uczenie kontrolera, grupowanie wad i zmiana dostawy mogą zmieniać p w czasie."
  ),
  list(
    id = "sprawdzenie", title = "Ściąga i sprawdzenie", hook = "Najpierw ustal, kiedy kończysz", lead = "Reguła zatrzymania → p i r → rozkład → limit → decyzja.",
    intro = "Masz teraz komplet trzech rozkładów zbudowanych na schemacie Bernoulliego. Ściąga zestawia je obok siebie — w quizie i ćwiczeniach najważniejsze będzie rozpoznanie, które pytanie prowadzi do którego rozkładu.",
    sections = list(
      list(
        id = "podsumowanie", title = "Podsumowanie",
        text = c(
          "Wykład zaczął się od jednej obserwacji: te same próby Bernoulliego prowadzą do różnych rozkładów zależnie od reguły zatrzymania. Gdy czekamy na pierwsze zdarzenie, liczba prób ma rozkład geometryczny (5.1) o średniej 1/p i długim prawym ogonie. Rozkład ten nie ma pamięci (5.4): seria bez zdarzenia nie przybliża zdarzenia.",
          "Gdy czekamy na r-te zdarzenie, liczba prób jest sumą r niezależnych czasów geometrycznych i ma rozkład ujemny dwumianowy (5.6) o średniej r/p. Tożsamość (5.7) łączy go z rozkładem dwumianowym i pozwala policzyć prawdopodobieństwo ukończenia w limicie. Plan zasobów opieramy na kwantylu, a nie na średniej, i zawsze nazywamy parametryzację używaną w oprogramowaniu.",
          "Wszystkie te wyniki zależą od stałego p i niezależności prób. Zmienność p między partiami wydłuża średni czas i poszerza ogon — dokładnie tam, gdzie leży limit planistyczny."
        )
      ),
      list(
        id = "sciaga", title = "Ściąga",
        bullets = c("Pytanie: ile prób do r-tego zdarzenia?", "Model: geometryczny dla r=1, ujemny dwumianowy dla r>1", "Założenia: stałe p i niezależność", "Wynik: rozkład liczby prób", "Interpretacja: zasoby potrzebne do osiągnięcia celu"),
        widget = dozd_sciaga_widget
      ),
      list(id = "most", title = "Co dalej", text = "Geometryczny i ujemny dwumianowy liczą dyskretne próby. W wykładzie o czasie życia to samo pytanie — jak długo czekamy na zdarzenie — zadamy w czasie ciągłym, a odpowiedzą rozkład wykładniczy i gamma.")
    )
  )
))

dozd_chapters <- risk_block_chapters(dozd_block)

dozd_server <- function(input, output, session) {
  vote <- reactiveVal(FALSE)
  observeEvent(input$d5_vote_check, vote(TRUE))
  output$d5_vote_feedback <- renderUI({
    req(vote())
    if (is.null(input$d5_vote)) {
      return(lc_feedback(type = "info", "Najpierw zaznacz jedną z odpowiedzi."))
    }
    lc_feedback(type = if (identical(input$d5_vote, "r")) "ok" else "warning", tags$strong("Stały jest cel:"), " r=3; liczba kontroli pozostaje losowa.")
  })
  geo_sample <- reactive({
    input$d5_geo_run
    rgeom(400, input$d5_geo_p) + 1
  })
  geo_plot <- reactive(ggplot(data.frame(x = geo_sample()), aes(x)) +
    geom_histogram(binwidth = 1, boundary = .5, fill = upwr_secondary, colour = "white") +
    coord_cartesian(xlim = c(1, min(80, max(geo_sample())))) +
    labs(x = "Liczba prób do pierwszego wykrycia", y = "Powtórzenia") +
    theme_upwr())
  zoom_plot_server("d5_geo", geo_plot, alt = "Histogram liczby prób potrzebnych do pierwszego wykrycia.")
  output$d5_geo_stats <- renderUI(tagList(lc_readout("Średnia teoretyczna", lc_fmt(1 / input$d5_geo_p, 1)), lc_readout("90. percentyl", qgeom(.9, input$d5_geo_p) + 1, color = upwr_accent)))
  nb_plot <- reactive({
    maxx <- max(input$d5_r + 10, qnbinom(.995, input$d5_r, input$d5_p) + input$d5_r)
    x <- input$d5_r:maxx
    ggplot(data.frame(x, p = vapply(x, risk_negative_binomial_total_pmf, numeric(1), r = input$d5_r, p = input$d5_p)), aes(x, p)) +
      geom_col(fill = upwr_accent) +
      labs(x = "Wszystkie próby", y = "Prawdopodobieństwo") +
      theme_upwr()
  })
  zoom_plot_server("d5_nb", nb_plot, alt = "Rozkład liczby wszystkich prób do osiągnięcia ustalonej liczby wykryć.")
  output$d5_nb_stats <- renderUI(lc_stat_grid(lc_stat_box("E(X)=r/p", round(input$d5_r / input$d5_p, 1)), lc_stat_box("Niepowodzenia średnio", round(input$d5_r * (1 - input$d5_p) / input$d5_p, 1)), columns = 1))
  output$d5_plan <- renderUI({
    pfinish <- risk_negative_binomial_finish(input$d5_limit, input$d5_r, input$d5_p)
    lc_stat_grid(lc_stat_box("Średnia", round(input$d5_r / input$d5_p, 1)), lc_stat_box("P(ukończenia do limitu)", risk_format_probability(pfinish), color = upwr_accent), lc_stat_box("95. percentyl", qnbinom(.95, input$d5_r, input$d5_p) + input$d5_r), columns = 1)
  })
  failure_data <- reactive({
    set.seed(505)
    stable <- rnbinom(1000, size = 3, prob = .1) + 3
    ps <- pmin(.95, pmax(.005, rnorm(1000, .1, input$d5_variation)))
    mixed <- vapply(ps, function(p) rnbinom(1, 3, p) + 3, numeric(1))
    data.frame(x = c(stable, mixed), model = rep(c("Stałe p", "Zmienne p między partiami"), each = 1000))
  })
  failure_plot <- reactive({
    ggplot(failure_data(), aes(x, fill = model)) +
      geom_histogram(binwidth = 3, position = "identity", alpha = .55) +
      coord_cartesian(xlim = c(3, 150)) +
      scale_fill_manual(values = upwr_cat_n(2)) +
      labs(x = "Liczba kontroli", y = "Powtórzenia", fill = NULL) +
      theme_upwr()
  })
  zoom_plot_server("d5_failure", failure_plot, alt = "Nakładające się histogramy stałego i zmiennego prawdopodobieństwa wykrycia.")
  output$d5_failure_stats <- renderUI({
    dat <- failure_data()
    means <- tapply(dat$x, dat$model, mean)
    lc_stat_grid(lc_stat_box("Średnia symulowana — stałe p", round(means[["Stałe p"]], 1)), lc_stat_box("Średnia symulowana — zmienne p", round(means[["Zmienne p między partiami"]], 1)), columns = 1)
  })
  risk_assessment_server("d5", dozd_quiz, input, output)
}
