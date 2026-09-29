# Blok 09: Analiza drzewa błędów ----------------------------------------

fta_quiz <- list(questions = list(
  list(question = "Czy dla bramki OR wolno zawsze dodać prawdopodobieństwa wejść?", choices = c("Nie — suma podwójnie liczy część wspólną" = "no", "Tak — OR z definicji jest sumą" = "yes", "Tylko gdy wartości są większe od 0,5" = "large"), correct = "no", explanation = "Dla niezależnych wejść używamy 1−∏(1−p_i); dla zdarzeń rozłącznych suma jest dokładna; dla niezależnych rzadkich zdarzeń stanowi przybliżenie."),
  list(question = "Dla I ∩ (D ∪ S), jakie są minimalne przekroje?",
    choices = c("{D,S}" = "a", "{I,D,S} jako jedyny" = "b", "{I,D} i {I,S}" = "c"), correct = "c",
    explanation = "Każdy z dwóch zestawów wystarcza; usunięcie dowolnego jego elementu odbiera wystarczalność."),
  list(question = "Ten sam liść C o q=0,05 pojawia się dwa razy pod AND. Ile wynosi P(C ∩ C)?",
    choices = c("0,0975" = "a", "0,05" = "b", "0,0025" = "c"), correct = "b",
    explanation = "C ∩ C=C; powtórzenie rysunku nie tworzy niezależnego zdarzenia."),
  list(question = "Dwie samodzielne bariery zastępują się w opanowaniu inicjacji. Kiedy zawodzi ochrona?",
    choices = c("Gdy zawiodą obie — AND" = "a", "Gdy zawiedzie jedna — OR" = "b", "Zawsze przy inicjacji" = "c"), correct = "a",
    explanation = "Logika jest inna niż w łańcuchu, w którym detekcja i wykonanie są wymagane razem."),
  list(question = "Co trzeba założyć w P(I)[1−(1−d)(1−s)], gdzie d=P(D | I), s=P(S | I)?",
    choices = c("Niezależność I od skutku TOP" = "a", "Rozłączność D i S" = "b", "Niezależność D i S warunkowo przy I" = "c"), correct = "c",
    explanation = "Reguła iloczynu z warunkowaniem jest ogólna; niezależność potrzebna jest do dopełnienia iloczynu wewnątrz OR.")
))
fta_exercises <- list(
  list(
    task = "Architektura: dla P(I)=0,005, d=0,05 i s=0,08 porównaj łańcuch wymagający obu funkcji z dwiema samodzielnymi barierami. Następnie dodaj wspólną przyczynę q=0,01 do łańcucha i wyznacz minimalne przekroje.",
    answer = c(
      "Łańcuch (obie funkcje wymagane, niepowodzenia łączy OR): P(TOP) = 0,005 · [1 − 0,95 · 0,92] = 0,005 · 0,126 = 0,00063. Dwie samodzielne bariery (niepowodzenia łączy AND): P(TOP) = 0,005 · 0,05 · 0,08 = 0,00002. Ta sama para urządzeń daje ryzyko różne o czynnik 31,5 — wyłącznie przez architekturę.",
      "Wspólna przyczyna w łańcuchu: TOP = I ∩ (C ∪ D₀ ∪ S₀), a d = 0,05 i s = 0,08 traktujemy jako lokalne niepowodzenia bez C. Ze wzoru (9.8): P(TOP) = 0,005 · [0,01 + 0,99 · 0,126] ≈ 0,000674, o około 7% więcej niż bez C. Minimalne przekroje: {I, C}, {I, D₀}, {I, S₀}."
    )
  ),
  list(
    task = "Bananpol: policz P(top) dla inicjacji 0,005 oraz OR warunkowych niepowodzeń detekcji 0,05 i modułu tłumienia 0,08 przy I.",
    answer = c(
      "Bramka OR przy I, wzór (9.2): P(D ∪ S | I) = 1 − (1 − 0,05)(1 − 0,08) = 1 − 0,874 = 0,126.",
      "Bramka AND z inicjacją, wzór (9.4): P(TOP) = 0,005 · 0,126 = 0,00063, czyli około 6,3 nieopanowanego pożaru na 10 000 magazyno-lat. Przybliżenie rzadkich zdarzeń dałoby 0,005 · 0,13 = 0,00065 — o około 3% za dużo."
    )
  ),
  list(
    task = "Diagnostyka: znajdź powtórzone zdarzenie bazowe i wyjaśnij ryzyko podwójnego liczenia.",
    answer = c(
      "Szukamy liści o tej samej nazwie albo tym samym fizycznym źródle w różnych gałęziach: wspólne zasilanie, ta sama ekipa serwisowa, ta sama partia czujników, ta sama procedura. W drzewie Bananpolu powtarza się inicjacja I — występuje w obu minimalnych przekrojach — a po dodaniu wspólnej przyczyny także C, które wyłącza detekcję i tłumienie.",
      "Rachunek bramka po bramce traktuje każde wystąpienie jak osobne, niezależne zdarzenie. Pod OR zawyża to wynik (to samo zdarzenie liczymy dwa razy), pod AND zaniża go, często o rząd wielkości (q² zamiast q). Poprawna droga: redukcja boolowska do minimalnych przekrojów, w których każde zdarzenie występuje raz, i rachunek na przekrojach (przykład 9.5)."
    )
  ),
  list(
    task = "Transfer: zbuduj małe drzewo utraty zasilania aparatury medycznej, oddzielając wspólną przyczynę.",
    answer = c(
      "Przykładowe zdarzenie szczytowe: przerwa w zasilaniu respiratora na sali intensywnej terapii dłuższa niż 10 sekund w ciągu roku. Zasilanie sieciowe S i zasilacz awaryjny U zastępują się nawzajem, więc ich niepowodzenia łączy AND. Oba źródła przechodzą jednak przez wspólną listwę rozdzielczą W przy łóżku.",
      "Poprawny zapis nie kopiuje W do obu gałęzi, tylko wyciąga je jako osobny liść pod bramką OR na szczycie: TOP = W ∪ (S ∩ U). Minimalne przekroje: {W} oraz {S, U}. Przekrój jednoelementowy {W} to pojedynczy punkt awarii — pierwszy kandydat do przeprojektowania, na przykład przez zasilanie UPS z osobnej listwy."
    )
  ),
  list(
    task = "Ważność: dla drzewa Bananpolu (P(I)=0,005, d=0,05, s=0,08) oblicz ważność Birnbauma każdego liścia i spadek P(TOP) po obniżeniu każdego parametru o 20%. Czy kolejność zależy od wielkości redukcji?",
    answer = c(
      "Ze wzoru (9.9): I_B(I) = 1 − 0,95 · 0,92 = 0,126; I_B(D) = 0,005 · 0,92 = 0,0046; I_B(S) = 0,005 · 0,95 = 0,00475.",
      "Ze wzoru (9.10) przy r = 0,2: inicjacja 0,2 · 0,005 · 0,126 = 0,000126; tłumienie 0,2 · 0,08 · 0,00475 = 0,000076; detekcja 0,2 · 0,05 · 0,0046 = 0,000046. Kolejność nie zależy od r, bo spadek jest proporcjonalny do r dla każdego liścia; od r zależy tylko skala słupków."
    )
  ),
  list(
    task = "Przybliżenie: przy P(I)=0,005 i gorszych barierach d=s=0,30 porównaj dokładne P(TOP) z przybliżeniem rzadkich zdarzeń. Czy przybliżenie wolno tu zastosować bez komentarza?",
    answer = c(
      "Dokładnie: P(D ∪ S | I) = 1 − 0,7 · 0,7 = 0,51, więc P(TOP) = 0,005 · 0,51 = 0,00255. Przybliżenie: 0,005 · (0,30 + 0,30) = 0,003.",
      "Przybliżenie zawyża wynik o około 18%. Samo P(TOP) jest małe, ale wejścia bramki OR już nie — a to od nich zależy błąd przybliżenia. Tu trzeba liczyć dokładnie albo przynajmniej podać, że wynik jest górnym oszacowaniem."
    )
  )
)

fta_block <- list(id = "fta", title = "Analiza drzewa błędów", chapters = list(
  list(
    id = "top", title = "Zdarzenie szczytowe", hook = "Najpierw nazwij awarię, której się boisz",
    lead = "Top event musi opisywać konkretny niepożądany stan, system i horyzont; poziomy drzewa rozdzielają skutek, logikę mechanizmu i zdarzenia bez dalszego rozwijania.",
    intro = c(
      "Analiza drzewa błędów (FTA) powstała w latach sześćdziesiątych przy programach rakietowych i lotniczych, a dziś jest standardem wszędzie tam, gdzie pojedyncza awaria ma zbyt poważne skutki, by czekać na dane z wypadków. W Bananpolu użyjemy jej do zdarzenia, które w rejestrach — na szczęście — nie występuje: nieopanowanego pożaru magazynu.",
      "Wszystko zaczyna się od definicji zdarzenia szczytowego. To zdanie, nad którym warto spędzić najwięcej czasu w całej analizie: musi wskazywać konkretny stan, konkretny system i horyzont odniesienia, tak żeby dwie osoby niezależnie potrafiły rozstrzygnąć, czy dane zdarzenie się w nim mieści."
    ),
    callout = list(
      label = "Dane Bananpolu",
      text = "Małe drzewo pożaru magazynu: P(inicjacji w roku) 0,005, P(braku detekcji | inicjacja) 0,05, P(niepowodzenia modułu tłumienia | inicjacja) 0,08. Analizujemy co najwyżej jedną inicjację w roku; parametry barier dotyczą tej inicjacji. Moduł tłumienia oznacza zdolność wykonawczą przy poprawnym sygnale, a detekcja ma osobne zasilanie. Liczby są fikcyjne.",
      color = "uwaga"
    ),
    sections = list(
      list(
        id = "most", title = "Od sukcesu do awarii",
        text = "W wykładzie o niezawodności opisywaliśmy logikę sukcesu systemu: kiedy całość działa. Drzewo błędów odwraca perspektywę — budujemy logikę awarii i pytamy, jakie kombinacje przyczyn prowadzą do zdarzenia szczytowego.",
        body = list(
          c(
            "Ta zmiana kierunku ma praktyczny powód. Schemat blokowy z wykładu 08 wymaga, żeby najpierw dało się wyliczyć wszystkie elementy, bez których system nie działa. Przy pożarze magazynu nie ma takiej listy: przyczyną może być zwarcie, niedopałek, przegrzany wózek, a opanowanie zależy od ludzi, czujników i instalacji. Łatwiej zacząć od jednego jasno opisanego skutku i schodzić w dół, pytając za każdym razem: co musiało się stać, żeby to nastąpiło?",
            "Zanim jednak zejdziemy w dół, trzeba ustalić, co dokładnie stoi na szczycie. Spróbuj ocenić trzy kandydatury."
          ),
          risk_vote_panel("f9_vote", "f9_vote_feedback", "Która definicja jest audytowalna?", c("Nieopanowany pożar magazynu w ciągu roku" = "good", "Problem z bezpieczeństwem" = "vague", "Awaria" = "failure")),
          "„Problem z bezpieczeństwem” i „awaria” nie mówią, o jaki stan chodzi, w jakim obiekcie ani w jakim czasie. Każdy uczestnik analizy dopisze do nich inne scenariusze, więc drzewo rozrośnie się bez końca, a wynik liczbowy nie będzie miał jednostki. Pierwsza definicja nazywa stan (pożar nieopanowany, czyli taki, którego nie ugasiła instalacja ani obsługa), obiekt (magazyn Bananpolu) i horyzont (jeden rok). Dopiero taka definicja pozwala zapytać o prawdopodobieństwo."
        )
      ),
      list(
        id = "definicja", title = "Co musi zawierać zdarzenie szczytowe",
        body = list(
          risk_definition("9.1", "Zdarzenie szczytowe", c(
            "Zdarzenie szczytowe (TOP) to jednoznacznie opisany niepożądany stan analizowanego systemu, dla którego budujemy drzewo błędów. Definicja wskazuje co najmniej: stan (co się stało), granice systemu (gdzie), horyzont lub warunki eksploatacji (kiedy i w jakim trybie pracy) oraz kryterium, po którym rozstrzygamy, że stan zaszedł."
          )),
          c(
            "Kryterium rozstrzygnięcia jest najczęściej pomijanym składnikiem. „Nieopanowany” może znaczyć: pożar, który przeniósł się poza strefę zapłonu; pożar, do którego wezwano straż; albo pożar, który zniszczył towar o wartości powyżej ustalonego progu. Każda z tych wersji prowadzi do nieco innego drzewa. Nie ma jednej właściwej — ważne, żeby wybór był jawny i żeby wszyscy uczestnicy analizy używali tego samego.",
            "Horyzont ma też znaczenie rachunkowe. Prawdopodobieństwo inicjacji 0,005 dotyczy jednego roku; gdybyśmy pytali o pięć lat, liczba ta byłaby inna, a pozostałe parametry — warunkowe względem jednej inicjacji — pozostałyby bez zmian. Mieszanie horyzontów w jednym drzewie jest tym samym błędem, co mieszanie czasów misji w układzie szeregowym z wykładu 08."
          ),
          risk_example("9.1", "Poprawianie definicji zdarzenia szczytowego",
            problem = "Kierownik logistyki proponuje zdarzenie szczytowe: „Awaria chłodni”. Popraw tę definicję tak, żeby spełniała definicję 9.1, i wskaż, które decyzje musiałeś podjąć.",
            steps = c(
              "Stan: „awaria” jest niejednoznaczna — czy chodzi o zatrzymanie sprężarki, czy o skutek dla towaru? Dla analizy ryzyka lepiej nazwać skutek: temperatura w komorze przekracza dopuszczalny próg.",
              "Granice systemu: która chłodnia i które komory? Na przykład komora dojrzewalni nr 2 wraz z zasilaniem i sterowaniem.",
              "Kryterium: jaki próg i jak długo? Na przykład temperatura powyżej 18 °C przez ponad 2 godziny, zarejestrowana przez rejestrator komory.",
              "Horyzont: jeden rok eksploatacji w trybie normalnej pracy."
            ),
            answer = "„Temperatura w komorze dojrzewalni nr 2 przekracza 18 °C przez ponad 2 godziny, co najmniej raz w roku eksploatacji.” Każdy składnik to decyzja analityka; wszystkie muszą być zapisane w nagłówku drzewa."
          )
        )
      ),
      list(
        id = "poziomy", title = "Zdarzenia szczytowe, pośrednie i bazowe",
        text = c(
          "Drzewo ma trzy rodzaje węzłów i każdy pełni inną rolę. Na szczycie stoi analizowany niepożądany stan. Pod nim zdarzenia pośrednie porządkują mechanizm — „zapłon nieopanowany” rozkłada się na „brak detekcji” i „brak tłumienia”. Na dole leżą zdarzenia bazowe: przyczyny, których świadomie nie rozwijamy dalej i którym przypisujemy prawdopodobieństwa.",
          "Granica „bazowości” jest decyzją analityka, nie właściwością świata. Brak detekcji można zostawić jako liść z parametrem z karty czujnika albo rozwinąć w osobne poddrzewo. Reguła praktyczna: rozwijaj dotąd, aż dojdziesz do zdarzeń, dla których masz dane albo które ktoś potrafi bezpośrednio poprawić."
        ),
        bullets = c("szczytowe: analizowany niepożądany stan", "pośrednie: wynik bramki lub podsystemu", "bazowe: przyczyna z przypisanym stanem albo prawdopodobieństwem"),
        body = list(
          risk_definition("9.2", "Zdarzenie bazowe i zdarzenie pośrednie", c(
            "Zdarzenie bazowe (liść drzewa) to zdarzenie, którego w danej analizie nie rozkładamy dalej; przypisujemy mu prawdopodobieństwo albo stan (zachodzi / nie zachodzi). Zdarzenie pośrednie to zdarzenie, które jest wynikiem bramki logicznej łączącej zdarzenia niższego poziomu."
          )),
          c(
            "W drzewie Bananpolu mamy trzy zdarzenia bazowe: inicjację I (zapłon w magazynie), brak detekcji D i niepowodzenie modułu tłumienia S. Zdarzeniem pośrednim jest „zabezpieczenia zawiodły”, czyli D lub S. Parametry D i S są warunkowe: d = P(D | I) i s = P(S | I) opisują zachowanie barier w chwili, gdy pożar już się zaczął. To ważne, bo czujnik, który jest sprawny w codziennych testach, może zawieść w dymie i temperaturze prawdziwego pożaru.",
            "Z drzewa wynika też, czego nie modelujemy. Nie ma w nim zachowania ludzi, dostępu straży ani rozmieszczenia towaru. Nie znaczy to, że te czynniki są nieistotne — tylko że w tej wersji analizy zostały poza granicą systemu z definicji 9.1. Do konsekwencji takich decyzji wrócimy w ostatnim rozdziale."
          ),
          risk_check("f9_chk_poziom",
            "Zespół rozwija „brak detekcji” na „uszkodzenie czujki” oraz „odcięcie zasilania centrali”. Jaką rolę pełni teraz „brak detekcji”?",
            c("Zdarzenie bazowe" = "base", "Zdarzenie pośrednie" = "inter", "Zdarzenie szczytowe" = "top"),
            correct = "inter",
            explanation = "Po rozwinięciu „brak detekcji” jest wynikiem bramki (tu OR) łączącej dwa liście, więc staje się zdarzeniem pośrednim. Parametr d nie jest już wpisywany ręcznie, lecz liczony z parametrów nowych liści.",
            hints = c(base = "Zdarzenie bazowe to takie, którego nie rozwijamy dalej. Czy tu je rozwinięto?", top = "Zdarzenie szczytowe jest jedno — nieopanowany pożar magazynu.")
          )
        )
      )
    )
  ),
  list(
    id = "konstruktor", title = "Bramki AND i OR", hook = "Czasem wystarczy jedna przyczyna, czasem trzeba dwóch",
    lead = "Pytanie operacyjne brzmi: czy wystarczy jedna przyczyna, czy potrzebna jest kombinacja? Zbudowaną logikę sprawdzamy, zanim pojawi się jakiekolwiek prawdopodobieństwo.",
    intro = c(
      "Protokół po pożarze magazynu nie pyta, dlaczego doszło do zapłonu — pyta, dlaczego nie udało się go opanować. Drzewo błędów buduje się w tym samym kierunku: od niepożądanego skutku w dół, do kombinacji przyczyn, które musiały wystąpić razem albo z których wystarczyła jedna.",
      "Przy każdym rozgałęzieniu zadajesz jedno pytanie: czy do zdarzenia nadrzędnego wystarczy dowolna z tych przyczyn (bramka OR), czy potrzebne są wszystkie naraz (bramka AND)? Dwie samodzielne bariery zastępujące się nawzajem zawodzą wspólnie przez AND. W naszym łańcuchu potrzebne są obie funkcje: wykrycie i wykonanie tłumienia, więc ich niepowodzenia łączymy przez OR. Logika wynika z instalacji, nie z samego słowa „bariera”."
    ),
    sections = list(
      list(
        id = "budowa", title = "Kierowany konstruktor",
        body = list(
          risk_definition("9.3", "Bramki AND i OR", c(
            "Bramka AND (koniunkcja) oznacza, że zdarzenie wyjściowe zachodzi wtedy i tylko wtedy, gdy zachodzą wszystkie zdarzenia wejściowe: wyjście = A₁ ∩ A₂ ∩ … ∩ Aₙ.",
            "Bramka OR (alternatywa) oznacza, że zdarzenie wyjściowe zachodzi wtedy i tylko wtedy, gdy zachodzi co najmniej jedno zdarzenie wejściowe: wyjście = A₁ ∪ A₂ ∪ … ∪ Aₙ."
          )),
          c(
            "Wybór bramki nie jest kwestią gustu, lecz opisem instalacji. Jeśli instalacja gaśnicza potrzebuje sygnału z centrali i sprawnego modułu wykonawczego, to brak któregokolwiek z nich wystarczy, żeby pożar nie został stłumiony — to OR niepowodzeń. Jeśli ten sam magazyn chronią dwa niezależne systemy, z których każdy sam gasi pożar, to ochrona zawodzi dopiero wtedy, gdy zawiodą oba — to AND niepowodzeń.",
            "Najczęstszy błąd początkujących polega na przenoszeniu intuicji z języka potocznego: „mamy detekcję i tłumienie, więc AND”. Spójnik „i” opisuje tu listę wymaganych funkcji, a nie warunek awarii. Awaria łańcucha wymaganych funkcji to OR ich niepowodzeń."
          ),
          risk_try("wybierz bramkę OR z przyczynami „brak detekcji” i „brak tłumienia”, a potem przełącz na AND. Dodaj „utratę zasilania” i zastanów się, która bramka opisuje każdą wersję instalacji."),
          figure_panel(label = "Budowa", title = "Utrata kontroli nad zapłonem", selectInput("f9_gate", "Logika", c("Wystarczy jedna przyczyna — OR" = "or", "Potrzebna kombinacja — AND" = "and")), checkboxGroupInput("f9_causes", "Przyczyny", c("Brak detekcji" = "detect", "Brak tłumienia" = "suppress", "Utrata zasilania" = "power"), selected = c("detect", "suppress")), uiOutput("f9_structure"), full_width = TRUE),
          "Konstruktor nie ocenia, która bramka jest właściwa — odpowiada tylko, co wybrana bramka znaczy. Dla bramki OR każde wybrane wejście samo wystarcza; dla AND wszystkie są potrzebne. Utrata zasilania, która wyłącza jednocześnie detekcję i tłumienie, nie pasuje dobrze do żadnej z prostych wersji: jest wspólną przyczyną i w rozdziale o przekrojach dostanie własną gałąź."
        )
      ),
      list(
        id = "bramki", title = "Bramki AND i OR bez liczb",
        text = c(
          "Drzewo błędów jest funkcją struktury z poprzedniego wykładu — tyle że zapisaną dla awarii zamiast sukcesu. Zanim wpiszesz do niego pierwszą liczbę, przetestuj samą logikę: aktywuj różne kombinacje zdarzeń bazowych i sprawdź, czy zdarzenie szczytowe reaguje tak, jak podpowiada wiedza o instalacji.",
          "Ten test wyłapuje najdroższe błędy analizy — złą bramkę albo brakującą przyczynę — wtedy, gdy poprawka kosztuje jeszcze tylko chwilę. Rachunek na błędnej strukturze jest bezbłędnie policzoną odpowiedzią na niewłaściwe pytanie."
        )
      ),
      list(
        id = "logika", title = "Nasze drzewo",
        text = "Logika drzewa Bananpolu brzmi: pożar wymyka się spod kontroli, gdy nastąpi inicjacja ORAZ zawiedzie co najmniej jedno z zabezpieczeń — detekcja LUB tłumienie. Sama inicjacja bez awarii barier nie wystarcza; awarie barier bez inicjacji też nie.",
        body = list(
          "W zapisie zbiorowym: TOP = I ∩ (D ∪ S). Z trzema liśćmi, z których każdy zachodzi albo nie, istnieje 2³ = 8 kombinacji stanów. Pełna tabela jest mała i warto ją przejść w całości — to najtańszy audyt logiki drzewa.",
          risk_try("zacznij od pustego zaznaczenia i włączaj liście pojedynczo, potem parami. Zapisz, które z ośmiu kombinacji aktywują zdarzenie szczytowe."),
          figure_panel(label = "Logika", title = "Aktywuj zdarzenia bazowe", checkboxGroupInput("f9_states", "Aktywne liście", c("Inicjacja" = "init", "Brak detekcji" = "detect", "Brak tłumienia" = "suppress"), selected = character(0)), uiOutput("f9_state_result"), full_width = TRUE),
          "Zdarzenie szczytowe aktywują dokładnie trzy kombinacje: {I, D}, {I, S} oraz {I, D, S}. Wszystkie zawierają inicjację, więc żadna kombinacja samych awarii barier nie wywołuje pożaru. Dwie pierwsze są najmniejsze — usunięcie z nich czegokolwiek wyłącza TOP. To pierwsze spotkanie z minimalnymi przekrojami, które w rozdziale czwartym posłużą do liczenia dużych drzew."
        )
      ),
      list(
        id = "dualnosc", title = "Dualność: drzewo błędów a schemat blokowy",
        body = list(
          c(
            "Bramki drzewa błędów mają bezpośrednie odpowiedniki w układach z wykładu 08 — szeregowym (8.2) i równoległym (8.3). Układ szeregowy zawodzi, gdy zawiedzie którykolwiek element — to bramka OR niepowodzeń. Układ równoległy zawodzi, gdy zawiodą wszystkie gałęzie — to bramka AND niepowodzeń. Drzewo błędów i schemat blokowy opisują ten sam system; różnią się tylko tym, czy mówimy językiem sukcesu, czy awarii.",
            "Przejście między nimi to prawo de Morgana: dopełnienie sumy jest iloczynem dopełnień. „Nie (A lub B)” znaczy „nie A i nie B”. Jeśli w drzewie błędów zamienimy każde zdarzenie na jego dopełnienie, a każdą bramkę OR na AND i odwrotnie, otrzymamy drzewo sukcesu — czyli schemat blokowy. Dla niezależnych zdarzeń wejściowych o prawdopodobieństwach p₁, …, pₙ dostajemy dwa wzory, które znasz już w wersji dla niezawodności."
          ),
          risk_formula("P(A_1\\cap\\cdots\\cap A_n)=\\prod_{i=1}^{n}p_i", num = "9.1",
            legend = c("A_i" = "i-te zdarzenie wejściowe bramki AND", "p_i" = "jego prawdopodobieństwo; wejścia niezależne")),
          risk_formula("P(A_1\\cup\\cdots\\cup A_n)=1-\\prod_{i=1}^{n}(1-p_i)", num = "9.2",
            legend = c("1-p_i" = "prawdopodobieństwo, że i-te wejście nie zachodzi", "\\prod(1-p_i)" = "prawdopodobieństwo, że nie zachodzi żadne wejście")),
          "Wzór (9.2) liczy OR przez dopełnienie: bramka OR nie zachodzi tylko wtedy, gdy nie zachodzi żadne wejście. Gdy p_i = 1 − R_i jest prawdopodobieństwem awarii elementu, iloczyn Π(1 − p_i) = Π R_i to niezawodność układu szeregowego. Stąd dualność:",
          risk_formula("P(\\text{OR awarii})=1-R_{\\text{szereg}},\\qquad P(\\text{AND awarii})=1-R_{\\text{równoległy}}", num = "9.3",
            legend = c("R_{\\text{szereg}}" = "niezawodność układu szeregowego tych samych elementów", "R_{\\text{równoległy}}" = "niezawodność układu równoległego")),
          risk_example("9.2", "Ta sama para barier w dwóch językach",
            problem = "Przy zaistniałej inicjacji detekcja działa z prawdopodobieństwem 0,95, a moduł tłumienia z prawdopodobieństwem 0,92 (niezależnie). Oblicz prawdopodobieństwo niepowodzenia ochrony (a) gdy obie funkcje są wymagane, (b) gdy są to dwie samodzielne bariery. Każdy wynik policz dwiema drogami: drzewem błędów i schematem blokowym.",
            steps = c(
              "(a) Drzewo: OR niepowodzeń, wzór (9.2): 1 − (1 − 0,05)(1 − 0,08) = 1 − 0,874 = 0,126. Schemat: układ szeregowy, R = 0,95 · 0,92 = 0,874, więc awaria 1 − 0,874 = 0,126.",
              "(b) Drzewo: AND niepowodzeń, wzór (9.1): 0,05 · 0,08 = 0,004. Schemat: układ równoległy, R = 1 − 0,05 · 0,08 = 0,996, więc awaria 0,004.",
              "Obie drogi dają te same liczby, bo to ten sam system; zmienia się tylko język opisu — zgodnie z (9.3)."
            ),
            answer = "(a) 0,126; (b) 0,004. Różnica między architekturami to czynnik 31,5 — przy tych samych urządzeniach."
          ),
          risk_check("f9_chk_brama",
            "Układ szeregowy trzech czujników zawodzi, gdy zawiedzie którykolwiek z nich. Jaką bramką zapiszesz jego awarię w drzewie błędów?",
            c("AND trzech awarii" = "and", "OR trzech awarii" = "or", "AND trzech sukcesów" = "and_ok"),
            correct = "or",
            explanation = "Awaria dowolnego elementu szeregu wystarcza, więc wejścia łączy OR. Równoważnie: sukces szeregu to AND sukcesów, a z prawa de Morgana jego dopełnienie to OR awarii.",
            hints = c(and = "AND awarii opisuje układ równoległy: system pada dopiero, gdy padną wszystkie.", and_ok = "AND sukcesów to opis działania szeregu, a drzewo błędów zapisuje awarię.")
          )
        )
      )
    )
  ),
  list(
    id = "rachunek", title = "Rachunek bramka po bramce", hook = "Liczymy od liści do korzenia",
    lead = "Najpierw liczymy niepowodzenie wymaganych funkcji przy inicjacji, potem ważymy je P(I).",
    intro = c(
      "Gdy struktura przeszła test logiczny, liczby wchodzą od dołu. Oznaczmy d=P(D | I), s=P(S | I). Zakładamy niezależność detekcji i modułu wykonawczego warunkowo przy inicjacji: P(D ∪ S | I)=1−(1−d)(1−s). Potem stosujemy ogólną regułę iloczynu P(TOP)=P(I)P(D ∪ S | I); ten krok nie wymaga niezależności od I.",
      "Zauważ, że dla rzadkich zdarzeń suma d + s jest dobrym przybliżeniem bramki OR — tutaj 0,13 wobec dokładnego 0,126 — ale to przybliżenie trzeba oznaczyć, a przy większych prawdopodobieństwach przestaje być dopuszczalne. Kurs liczy dokładnie; sumę zostawiamy do szybkich szacunków na marginesie."
    ),
    sections = list(
      list(
        id = "algorytm", title = "Rachunek bramka po bramce",
        body = list(
          c(
            "Rachunek od liści do korzenia jest prosty mechanicznie: każdą bramkę zastępujemy jedną liczbą, zaczynając od najniższego poziomu. Bramka AND niezależnych wejść daje iloczyn (9.1), bramka OR — dopełnienie iloczynu dopełnień (9.2). Wynik bramki staje się wejściem bramki poziom wyżej i tak aż do szczytu. Dla drzewa Bananpolu są tylko dwa kroki: najpierw OR barier przy inicjacji, potem AND z inicjacją.",
            "Drugi krok ma subtelność. Parametry d i s są warunkowe względem I, więc nie mnożymy „niezależnych” prawdopodobieństw, tylko stosujemy regułę iloczynu z wykładu 02: P(I ∩ B) = P(I) · P(B | I), gdzie B = D ∪ S. Ta reguła jest zawsze prawdziwa. Założenie niezależności pojawia się w jednym miejscu — wewnątrz bramki OR, gdy D i S łączymy wzorem (9.2) przy ustalonym I."
          ),
          risk_formula("P(\\mathrm{TOP})=P(I)\\,[1-(1-P(D\\mid I))(1-P(S\\mid I))]", num = "9.4",
            legend = c("P(I)" = "prawdopodobieństwo inicjacji w roku", "P(D\\mid I)=d" = "prawdopodobieństwo braku detekcji przy inicjacji", "P(S\\mid I)=s" = "prawdopodobieństwo niepowodzenia modułu tłumienia przy inicjacji")),
          risk_derivation("wzór (9.4)", c(
            "Zdarzenie szczytowe to TOP = I ∩ (D ∪ S). Reguła iloczynu daje P(TOP) = P(I) · P(D ∪ S | I).",
            "Wewnątrz bramki OR przechodzimy do dopełnienia: D ∪ S nie zachodzi dokładnie wtedy, gdy nie zachodzi ani D, ani S. Przy warunkowej niezależności D i S względem I prawdopodobieństwo tego dopełnienia to iloczyn."
          ), lines = c(
            "P(D ∪ S | I) = 1 − P(D' ∩ S' | I)",
            "             = 1 − P(D' | I) · P(S' | I)",
            "             = 1 − (1 − d)(1 − s)",
            "P(TOP)       = P(I) · [1 − (1 − d)(1 − s)]"
          )),
          risk_example("9.3", "Nieopanowany pożar magazynu",
            problem = "Dla danych Bananpolu (P(I) = 0,005, d = 0,05, s = 0,08) oblicz P(TOP) rachunkiem od liści do korzenia. Zinterpretuj wynik w skali 10 000 magazyno-lat.",
            steps = c(
              "Poziom 1, bramka OR przy I: P(D ∪ S | I) = 1 − 0,95 · 0,92 = 1 − 0,874 = 0,126.",
              "Poziom 2, bramka AND z inicjacją: P(TOP) = 0,005 · 0,126 = 0,00063.",
              "Skala: 0,00063 · 10 000 = 6,3. Spośród 10 000 magazyno-lat średnio około 6 kończy się nieopanowanym pożarem.",
              "Kontrola rzędu wielkości: inicjacja zdarza się w 50 na 10 000 magazyno-lat, a bariery zawodzą w 12,6% z nich — 50 · 0,126 = 6,3."
            ),
            answer = "P(TOP) = 0,00063, czyli około 6,3 nieopanowanego pożaru na 10 000 magazyno-lat."
          ),
          risk_try("zacznij od wartości domyślnych i sprawdź wynik przykładu 9.3. Potem podwój P(inicjacji), a osobno podwój P(braku detekcji) — porównaj, jak zmienia się P(top) w obu przypadkach."),
          risk_widget_panel("Obliczenia", "Parametry małego drzewa", tagList(sliderInput("f9_init", "P(inicjacji)", 0, .03, .005, .001), sliderInput("f9_detect", "P(braku detekcji | I)", 0, .3, .05, .01), sliderInput("f9_suppress", "P(niepowodzenia modułu tłumienia | I)", 0, .3, .08, .01)), "f9_tree_plot", "f9_tree_stats"),
          c(
            "Przy wartościach domyślnych panel pokazuje P(D ∪ S | I) = 0,126 i P(top) = 0,000630. Podwojenie inicjacji do 0,010 podwaja wynik do 0,00126 — inicjacja wchodzi przez AND, więc działa proporcjonalnie. Podwojenie braku detekcji do 0,10 zmienia wynik słabiej: P(D ∪ S | I) = 1 − 0,90 · 0,92 = 0,172, a P(top) = 0,00086, czyli wzrost o około 37%.",
            "Ta asymetria jest pierwszym sygnałem, że miejsce liścia w drzewie decyduje o jego wadze. Wrócimy do niej w rozdziale o rankingu."
          )
        )
      ),
      list(
        id = "przyblizenie", title = "Przybliżenie rzadkich zdarzeń",
        body = list(
          c(
            "Suma d + s wygląda jak naturalny wzór na OR, ale liczy dwukrotnie sytuację, w której zawodzą obie bariery naraz. Dla dwóch wejść poprawka jest dokładnie znana: P(D ∪ S) = d + s − P(D ∩ S). Przy niezależności część wspólna to d · s = 0,004, więc 0,13 − 0,004 = 0,126. Dla wielu wejść poprawek jest więcej, ale mają ten sam charakter: im mniejsze prawdopodobieństwa wejść, tym mniejsze iloczyny i tym mniejszy błąd sumy.",
            "Z tej obserwacji wynikają dwa oszacowania, które ograniczają wynik bramki OR z góry i z dołu. Pierwsze to sama suma. Drugie odejmuje od sumy wszystkie iloczyny par."
          ),
          risk_formula("\\sum_i p_i-\\sum_{i<j}p_ip_j\\;\\le\\;1-\\prod_i(1-p_i)\\;\\le\\;\\sum_i p_i", num = "9.5",
            legend = c("p_i" = "prawdopodobieństwa niezależnych wejść bramki OR", "\\sum_i p_i" = "przybliżenie rzadkich zdarzeń (górne oszacowanie)")),
          risk_example("9.4", "Kiedy suma wystarcza?",
            problem = "Porównaj dokładny wynik bramki OR z przybliżeniem rzadkich zdarzeń (a) dla trzech wejść 0,05; 0,08; 0,02, (b) dla dwóch wejść 0,30 i 0,30.",
            steps = c(
              "(a) Dokładnie: 1 − 0,95 · 0,92 · 0,98 = 1 − 0,85652 = 0,14348. Suma: 0,15. Dolne oszacowanie z (9.5): 0,15 − (0,004 + 0,001 + 0,0016) = 0,1434.",
              "Błąd sumy w (a): 0,15 / 0,14348 ≈ 1,045, czyli około 4,5% za dużo.",
              "(b) Dokładnie: 1 − 0,7 · 0,7 = 0,51. Suma: 0,60, o około 18% za dużo. Dla dwóch wejść dolne oszacowanie jest dokładne: 0,60 − 0,09 = 0,51."
            ),
            answer = "Przy wejściach rzędu kilku procent suma myli się o kilka procent; przy wejściach rzędu 0,3 błąd sięga kilkunastu procent. O jakości przybliżenia decydują prawdopodobieństwa wejść bramki, a nie wynik końcowy."
          ),
          risk_check("f9_chk_przybl",
            "Zdarzenie szczytowe ma prawdopodobieństwo 0,0003, ale jest wynikiem bramki OR dwóch wejść po 0,4 pomnożonej przez małe P(I). Czy przybliżenie rzadkich zdarzeń w tej bramce jest bezpieczne?",
            c("Tak, bo wynik jest bardzo mały" = "yes", "Nie, bo wejścia bramki nie są rzadkie" = "no", "Tak, bo przybliżenie zawsze zaniża wynik" = "under"),
            correct = "no",
            explanation = "Błąd sumy wynosi tu d · s = 0,16 wobec dokładnego 0,64 — suma 0,8 zawyża wynik bramki o 25%. Małe P(I) nie poprawia tego błędu, tylko go przenosi.",
            hints = c(yes = "Błąd przybliżenia zależy od iloczynów wejść bramki OR. Policz 0,4 · 0,4.", under = "Suma liczy część wspólną podwójnie, więc zawyża wynik, nie zaniża.")
          ),
          "Rachunek bramka po bramce ma jeszcze jedno ograniczenie, poważniejsze niż przybliżenie: działa poprawnie tylko wtedy, gdy każde zdarzenie bazowe występuje w drzewie jeden raz. Jeśli ten sam liść pojawia się w dwóch gałęziach, wyniki bramek przestają być niezależne i mnożenie ich daje błędny wynik. Tym zajmuje się następny rozdział."
        )
      )
    ),
    takeaway = "Liczby weszły do drzewa dopiero wtedy, gdy jego struktura była gotowa. Odwrotna kolejność — najpierw dostępne dane, potem logika — może ukryć wspólną przyczynę albo narzucić strukturę wygodną dla danych, a nie wierną mechanizmowi.",
    pitfall = "Iloczyn bez warunkowania wymaga niezależności. Ogólna reguła P(A ∩ B)=P(A)P(B | A) jej nie wymaga. W tym przykładzie niezależność przy I przyjęto wewnątrz bramki OR."
  ),
  list(
    id = "przekroje", title = "Minimalny przekrój", hook = "Najkrótsze drogi do awarii",
    lead = "Minimalny przekrój wystarcza do TOP, ale żaden jego właściwy podzbiór już nie wystarcza; powtórzone zdarzenie i wspólna przyczyna zmieniają listę przekrojów.",
    intro = c(
      "Duże drzewo trudno ogarnąć wzrokiem, ale można je streścić listą minimalnych przekrojów: zestawów zdarzeń wystarczających do TOP, z których nie można usunąć żadnego elementu. Minimalność dotyczy zawierania, a nie najmniejszej liczebności w całym drzewie. Nasze drzewo ma dwa, oba dwuelementowe — i to jest dobra wiadomość: żadna pojedyncza awaria nie wywołuje katastrofy.",
      "Przekroje czyta się jak diagnozę architektury. Przekrój jednoelementowy to pojedynczy punkt awarii — najpilniejszy sygnał do przeprojektowania. Wiele przekrojów współdzielących to samo zdarzenie (u nas: inicjację w obu) wskazuje, gdzie jedna interwencja osłabia kilka scenariuszy naraz."
    ),
    sections = list(
      list(
        id = "definicja", title = "Przekrój i minimalny przekrój",
        body = list(
          risk_definition("9.4", "Przekrój i minimalny przekrój", c(
            "Przekrój (cut set) to zbiór zdarzeń bazowych, których łączne zajście wystarcza do zajścia zdarzenia szczytowego, niezależnie od stanu pozostałych zdarzeń bazowych.",
            "Minimalny przekrój to przekrój, z którego nie można usunąć żadnego elementu bez utraty tej własności. Rząd przekroju to liczba jego elementów."
          )),
          c(
            "W drzewie Bananpolu {I, D, S} jest przekrojem, ale nie minimalnym: po usunięciu S zostaje {I, D}, które nadal wystarcza. Minimalne są {I, D} i {I, S} — te same zbiory, które w tabeli stanów z rozdziału drugiego okazały się najmniejszymi kombinacjami aktywującymi TOP.",
            "Minimalne przekroje wyznacza się algebrą zbiorów, tą samą, której używaliśmy do zdarzeń w wykładzie 01. Zaczynamy od szczytu, zastępujemy każdą bramkę jej wejściami (AND jako iloczyn, OR jako sumę), rozwijamy nawiasy do postaci sumy iloczynów i porządkujemy wynik dwiema regułami. Idempotentność: A ∩ A = A oraz A ∪ A = A — zdarzenie powtórzone w iloczynie liczymy raz. Pochłanianie: A ∪ (A ∩ B) = A — jeśli mniejszy zbiór już wystarcza, większy jest zbędny.",
            "Dla drzewa Bananpolu rozwinięcie jest jednokrokowe: TOP = I ∩ (D ∪ S) = (I ∩ D) ∪ (I ∩ S). Każdy iloczyn w tej sumie to jeden minimalny przekrój. Ogólnie zdarzenie szczytowe jest sumą swoich minimalnych przekrojów, a każdy przekrój — iloczynem swoich zdarzeń bazowych."
          ),
          risk_formula("\\mathrm{TOP}=K_1\\cup K_2\\cup\\cdots\\cup K_m,\\qquad P(K_j)=\\prod_{i\\in K_j}p_i", num = "9.6",
            legend = c("K_j" = "j-ty minimalny przekrój", "m" = "liczba minimalnych przekrojów", "p_i" = "prawdopodobieństwo zdarzenia bazowego i; zdarzenia niezależne"))
        )
      ),
      list(
        id = "sets", title = "Dwa przekroje drzewa Bananpolu",
        bullets = c("{inicjacja, brak detekcji}", "{inicjacja, brak tłumienia}"),
        body = list(
          risk_try("przełączaj między przekrojami i dla każdego zadaj pytanie: którego liścia usunięcie wyłącza ten scenariusz?"),
          figure_panel(label = "Podświetlenie", title = "Wybierz przekrój", radioButtons("f9_cut", NULL, c("I + D" = "id", "I + S" = "is"), selected = "id"), uiOutput("f9_cut_text"), full_width = TRUE),
          c(
            "Oba scenariusze dzielą inicjację: jej wyeliminowanie wyłącza oba naraz, a wyeliminowanie D albo S tylko jeden. To jakościowa zapowiedź rankingu z następnego rozdziału. Lista przekrojów pozwala też policzyć P(TOP) bez rysowania drzewa. Ze wzoru (9.6): P(K₁) = P(I) · d = 0,005 · 0,05 = 0,00025 oraz P(K₂) = P(I) · s = 0,005 · 0,08 = 0,0004.",
            "Przekroje nie są rozłączne — oba zachodzą, gdy zajdą I, D i S naraz — więc dokładna wartość wymaga odjęcia części wspólnej. Dla przekrojów obowiązuje ta sama zasada włączeń i wyłączeń, co dla bramki OR, a jej pierwszy wyraz daje przybliżenie rzadkich zdarzeń, powszechnie stosowane w dużych drzewach."
          ),
          risk_formula("P(\\mathrm{TOP})\\approx\\sum_{j=1}^{m}P(K_j),\\qquad P(K_1\\cup K_2)=P(K_1)+P(K_2)-P(K_1\\cap K_2)", num = "9.7",
            legend = c("\\sum_j P(K_j)" = "przybliżenie rzadkich zdarzeń; górne oszacowanie P(TOP)", "K_1\\cap K_2" = "jednoczesne zajście obu przekrojów")),
          risk_example("9.5", "P(TOP) z minimalnych przekrojów",
            problem = "Oblicz P(TOP) drzewa Bananpolu z listy minimalnych przekrojów: (a) przybliżeniem rzadkich zdarzeń, (b) dokładnie, z zasady włączeń i wyłączeń.",
            steps = c(
              "(a) Ze wzoru (9.7): P(TOP) ≈ 0,00025 + 0,0004 = 0,00065.",
              "(b) Część wspólna: K₁ ∩ K₂ = {I, D, S}; w iloczynie I występuje raz (idempotentność), więc P(K₁ ∩ K₂) = 0,005 · 0,05 · 0,08 = 0,00002.",
              "P(TOP) = 0,00025 + 0,0004 − 0,00002 = 0,00063 — dokładnie tyle, co w przykładzie 9.3."
            ),
            answer = "(a) 0,00065, około 3% powyżej wyniku dokładnego; (b) 0,00063. Obie drogi — bramka po bramce i przez przekroje — dają ten sam wynik dokładny."
          ),
          risk_check("f9_chk_mcs",
            "Drzewo ma minimalne przekroje {A}, {B, C} i {B, D}. Który z tych zbiorów jest przekrojem, ale nie minimalnym?",
            c("{A, B}" = "ab", "{B, C}" = "bc", "{C, D}" = "cd"),
            correct = "ab",
            explanation = "{A, B} wystarcza do TOP, bo zawiera {A}, ale po usunięciu B nadal wystarcza — więc nie jest minimalny. {B, C} jest minimalnym przekrojem, a {C, D} w ogóle nie jest przekrojem: nie zawiera żadnego z minimalnych.",
            hints = c(bc = "{B, C} jest na liście minimalnych przekrojów. Szukamy zbioru, który wystarcza, ale ma zbędny element.", cd = "Czy {C, D} zawiera w całości którykolwiek minimalny przekrój? Jeśli nie, nie wystarcza do TOP.")
          )
        )
      ),
      list(
        id = "powtorzenie", title = "Powtórzone zdarzenie bazowe",
        text = c(
          "W większych drzewach to samo zdarzenie bazowe — utrata zasilania, błąd tego samego zespołu, ta sama partia komponentów — pojawia się w kilku gałęziach. Rysunek może je pokazywać wielokrotnie, ale rachunek musi pamiętać, że to jedno zdarzenie: zachodzi albo nie zachodzi wszędzie naraz.",
          "Potraktowanie dwóch wystąpień jako niezależnych zdarzeń fałszuje wynik w sposób zależny od struktury: pod bramką OR zawyża (liczymy to samo dwa razy), pod bramką AND drastycznie zaniża — kwadrat małej liczby wygląda uspokajająco. Porównanie poniżej pokazuje oba błędy: q, błędne OR 1−(1−q)² oraz błędne AND q²."
        ),
        body = list(
          risk_try("ustaw P(utraty wspólnego zasilania) na 0,05, a potem przesuń suwak do 0,01 i do 0,20. Porównaj, jak zmienia się rozjazd każdego błędnego wyniku z poprawnym."),
          figure_panel(label = "Pułapka", title = "Dwa wystąpienia, jedno źródło", sliderInput("f9_repeat", "P(utraty wspólnego zasilania)", 0, .2, .05, .01), uiOutput("f9_repeat_result"), full_width = TRUE),
          c(
            "Przy q = 0,05 poprawna wartość to 0,050. Błędne OR daje 1 − 0,95² = 0,0975 (panel zaokrągla do 0,098) — prawie dwa razy za dużo. Błędne AND daje 0,05² = 0,0025, dwadzieścia razy za mało. Im mniejsze q, tym gorzej wypada AND: przy q = 0,01 kwadrat zaniża wynik stukrotnie. To dlatego pomyłka pod bramką AND jest groźniejsza: daje wrażenie redundancji tam, gdzie jej nie ma.",
            "W prawdziwych drzewach powtórzenie rzadko jest tak jawne jak C ∩ C. Zwykle ten sam liść wchodzi do dwóch różnych gałęzi, a jego wpływ ukrywa się w mieszance innych zdarzeń. Poniższy przykład pokazuje, jak redukcja do minimalnych przekrojów usuwa ten problem."
          ),
          risk_example("9.6", "Dwa kanały detekcji ze wspólnym zasilaniem",
            problem = c(
              "Detekcję w magazynie zapewniają dwa kanały czujek; każdy sam wystarcza, więc brak detekcji wymaga awarii obu (AND). Kanał k zawodzi, gdy uszkodzi się jego czujka Bₖ albo gdy zabraknie wspólnego zasilania Z (OR). Zatem: brak detekcji = (Z ∪ B₁) ∩ (Z ∪ B₂).",
              "Przyjmij P(Z) = 0,02 i P(B₁) = P(B₂) = 0,10, wszystkie niezależne. Oblicz P(brak detekcji) (a) naiwnie, bramka po bramce, (b) z minimalnych przekrojów."
            ),
            steps = c(
              "(a) Naiwnie: każdy kanał 1 − 0,98 · 0,90 = 0,118; AND dwóch kanałów 0,118² ≈ 0,0139. Rachunek traktuje dwa wystąpienia Z jak dwa niezależne zdarzenia.",
              "(b) Rozwijamy: (Z ∪ B₁) ∩ (Z ∪ B₂) = Z ∪ (Z ∩ B₂) ∪ (B₁ ∩ Z) ∪ (B₁ ∩ B₂). Pochłanianie usuwa oba iloczyny zawierające Z: zostaje Z ∪ (B₁ ∩ B₂).",
              "Minimalne przekroje: {Z} (rzędu 1) i {B₁, B₂} (rzędu 2). Ze wzoru (9.7): P = 0,02 + 0,01 − 0,02 · 0,01 = 0,0298. Przybliżenie rzadkich zdarzeń: 0,03.",
              "Porównanie: 0,0298 / 0,0139 ≈ 2,1."
            ),
            answer = "Poprawnie 0,0298; rachunek naiwny zaniża wynik ponad dwukrotnie. Redukcja ujawnia też pojedynczy punkt awarii {Z}, którego rysunek z dwoma kanałami nie pokazywał wprost."
          ),
          "Ten sam mechanizm działa w drzewie Bananpolu w drugą stronę. Inicjacja I występuje w obu przekrojach. Gdyby potraktować przekroje jak niezależne zdarzenia i połączyć je wzorem (9.2), wyszłoby 1 − (1 − 0,00025)(1 − 0,0004) ≈ 0,00064990 zamiast 0,00063 — około 3% za dużo. Pod OR błąd jest łagodny i idzie w bezpieczną stronę; pod AND bywa wielokrotny i idzie w stronę fałszywego spokoju.",
          risk_check("f9_chk_powt",
            "W drzewie liść Z występuje w dwóch gałęziach połączonych bramką AND. Analityk policzył je jak niezależne kopie. Jaki jest najbardziej prawdopodobny skutek?",
            c("Wynik zawyżony" = "over", "Wynik zaniżony" = "under", "Wynik poprawny, bo niezależność kopii nie ma znaczenia" = "ok"),
            correct = "under",
            explanation = "Pod AND iloczyn niezależnych kopii daje q² zamiast q (albo ogólniej zaniża wspólny składnik), co sugeruje redundancję, której nie ma. Pod OR ten sam błąd zawyżałby wynik.",
            hints = c(over = "Zawyżenie to skutek powtórzenia pod OR. Co robi iloczyn q · q z małą liczbą q?", ok = "Porównaj P(Z ∩ Z) = q z iloczynem q · q.")
          )
        ),
        pitfall = "Traktowanie powtórzeń jako niezależnych zaniża lub zawyża wynik zależnie od struktury."
      ),
      list(
        id = "wspolna", title = "Wspólna przyczyna zmienia strukturę",
        text = c(
          "Skoro utrata zasilania wyłącza jednocześnie detekcję i tłumienie, poprawka liczbowa nie wystarczy — trzeba przebudować drzewo. Wspólna przyczyna staje się osobnym zdarzeniem bazowym, które przez własną gałąź prowadzi do obu niesprawności, a minimalne przekroje trzeba wyznaczyć od nowa. W naszym drzewie pojawia się {I,C}. Ma dwa elementy, tak samo jak {I,D₀} i {I,S₀}; nowy przekrój nie musi być krótszy ani dominujący. D₀ i S₀ oznaczają lokalne niepowodzenia bez wspólnej przyczyny C.",
          "D=C ∪ D₀ i S=C ∪ S₀, więc TOP=I ∩ (C ∪ D₀ ∪ S₀). Przy niezależnych C, D₀, S₀ warunkowo przy I: P(TOP)=P(I)[q+(1−q)(1−(1−d₀)(1−s₀))]. Parametry d₀ i s₀ wykluczają wspólną przyczynę; nie dodajemy q do danych, które już ją zawierają."
        ),
        body = list(
          risk_formula("P(\\mathrm{TOP})=P(I)\\,\\bigl[q+(1-q)\\bigl(1-(1-d_0)(1-s_0)\\bigr)\\bigr]", num = "9.8",
            legend = c("q=P(C\\mid I)" = "prawdopodobieństwo wspólnego niepowodzenia obu funkcji przy inicjacji", "d_0, s_0" = "lokalne niepowodzenia detekcji i tłumienia bez przyczyny C")),
          "Wzór (9.8) czyta się jak drzewo: albo zachodzi wspólna przyczyna (q), albo nie zachodzi (1 − q) i wtedy działa zwykła bramka OR lokalnych niepowodzeń. Zapis przez przekroje daje to samo: TOP = (I ∩ C) ∪ (I ∩ D₀) ∪ (I ∩ S₀).",
          risk_example("9.7", "Ile kosztuje wspólne zasilanie?",
            problem = "Przyjmij q = 0,01 oraz lokalne d₀ = 0,05 i s₀ = 0,08. Oblicz P(TOP) (a) dla łańcucha z drzewa Bananpolu, (b) dla wariantu z dwiema samodzielnymi barierami (AND lokalnych niepowodzeń), i porównaj z wersjami bez wspólnej przyczyny.",
            steps = c(
              "(a) Łańcuch, wzór (9.8): 0,005 · [0,01 + 0,99 · 0,126] = 0,005 · 0,13474 ≈ 0,000674. Bez C: 0,00063. Wzrost o około 7%.",
              "(b) Bariery samodzielne: TOP = I ∩ (C ∪ (D₀ ∩ S₀)), więc P(TOP) = 0,005 · [0,01 + 0,99 · 0,004] = 0,005 · 0,01396 ≈ 0,0000698. Bez C: 0,005 · 0,004 = 0,00002. Wzrost 3,49-krotny.",
              "W wariancie (b) przekrój {I, C} jest rzędu 2, a przekrój {I, D₀, S₀} rzędu 3 — wspólna przyczyna skraca najkrótszą drogę do katastrofy."
            ),
            answer = "(a) ≈ 0,000674, (b) ≈ 0,0000698. Wspólna przyczyna najmocniej uderza w redundancję: w łańcuchu dodaje kilka procent, w układzie z dwiema barierami zjada większość zysku z redundancji."
          ),
          risk_try("ustaw P(C | I) na 0 i sprawdź, że oba wyniki się pokrywają. Potem zwiększaj q i obserwuj różnicę; na koniec wróć do rozdziału o rachunku i zmniejsz tam P(braku detekcji) do zera."),
          figure_panel(label = "Rachunek", title = "Trzy minimalne przekroje", sliderInput("f9_common", "P(C | I): wspólne niepowodzenie funkcji", 0, .2, .01, .005), uiOutput("f9_common_result"), full_width = TRUE),
          c(
            "Panel korzysta z suwaków detekcji i tłumienia z rozdziału o rachunku, interpretując je jako d₀ i s₀. Przy wartościach domyślnych i q = 0,01 pokazuje 0,000630 bez wspólnej przyczyny i 0,000674 z nią — zgodnie z przykładem 9.7(a). Nawet gdy lokalne niepowodzenia sprowadzimy do zera, wynik nie spadnie poniżej P(I) · q: przekroju {I, C} nie da się usunąć poprawianiem pojedynczych urządzeń.",
            "Dla q = 1 wynik osiąga P(I) = 0,005: każda inicjacja kończy się nieopanowanym pożarem, bo wspólna przyczyna wyłącza obie funkcje zawsze. To skrajny, ale pouczający przypadek — pokazuje, że wspólna przyczyna działa jak most łączący inicjację bezpośrednio ze zdarzeniem szczytowym."
          ),
          risk_check("f9_chk_wspolna",
            "Dane o niezawodności detekcji (d = 0,05) pochodzą z przeglądów, w których liczono także awarie spowodowane zanikiem zasilania. Co trzeba zrobić, dodając do drzewa osobny liść C?",
            c("Nic — dodać C z q i zostawić d = 0,05" = "keep", "Oczyścić d z przypadków zaniku zasilania, żeby otrzymać d₀" = "clean", "Usunąć D z drzewa, bo zawiera się w C" = "drop"),
            correct = "clean",
            explanation = "Jeśli d już zawiera wspólną przyczynę, dodanie C liczy ją podwójnie. Wzór (9.8) wymaga lokalnych parametrów d₀ i s₀, z których wyłączono C.",
            hints = c(keep = "Wtedy zanik zasilania byłby w drzewie dwa razy: raz w d, raz w q.", drop = "D ma też przyczyny lokalne, niezwiązane z zasilaniem.")
          )
        )
      )
    )
  ),
  list(
    id = "ranking", title = "Ważność Birnbauma", hook = "Nie każda poprawa ma tę samą wartość",
    lead = "Poprawiamy po kolei każdy liść i obserwujemy spadek P(top).",
    intro = c(
      "Drzewo z liczbami odpowiada wreszcie na pytanie zarządu: co poprawić najpierw? Eksperyment myślowy jest uczciwy — każdemu liściowi po kolei fundujemy tę samą względną redukcję i porównujemy spadek P(top). Struktura drzewa sprawia, że identyczna poprawa w różnych miejscach daje różne zyski.",
      "W naszym drzewie inicjacja wchodzi przez AND, więc jej redukcja przenosi się na wynik w pełnej proporcji; zabezpieczenia dzielą się zyskiem wewnątrz bramki OR. Ranking to jednak dopiero pierwsza kolumna tabeli decyzyjnej — obok muszą stanąć koszt i wykonalność, którymi zajmie się ostatni wykład."
    ),
    sections = list(
      list(
        id = "birnbaum", title = "Ważność Birnbauma",
        body = list(
          c(
            "Zacznijmy od pytania prostszego niż ranking: jak bardzo P(TOP) zależy od stanu jednego liścia? Najbardziej bezpośrednia odpowiedź porównuje dwa światy. W pierwszym liść i na pewno zachodzi, w drugim na pewno nie zachodzi; pozostałe liście zachowują swoje prawdopodobieństwa. Różnica P(TOP) między tymi światami mówi, jak często stan liścia i rozstrzyga o zdarzeniu szczytowym.",
            "W wykładzie 08 podobne pytanie zadawaliśmy dla niezawodności (istotność Birnbauma, wzór 8.15): który element poprawić, żeby system zyskał najwięcej. Tu patrzymy od strony awarii, ale rachunek jest analogiczny."
          ),
          risk_definition("9.5", "Ważność Birnbauma", c(
            "Ważność Birnbauma zdarzenia bazowego i to różnica prawdopodobieństw zdarzenia szczytowego, gdy i na pewno zachodzi, i gdy na pewno nie zachodzi. Jest to prawdopodobieństwo, że pozostałe zdarzenia bazowe ułożyły się tak, iż stan zdarzenia i przesądza o TOP (zdarzenie i jest wtedy krytyczne)."
          )),
          risk_formula("I_B(i)=P(\\mathrm{TOP}\\mid i\\text{ zachodzi})-P(\\mathrm{TOP}\\mid i\\text{ nie zachodzi})=\\frac{\\partial P(\\mathrm{TOP})}{\\partial p_i}", num = "9.9",
            legend = c("I_B(i)" = "ważność Birnbauma zdarzenia i", "p_i" = "prawdopodobieństwo zdarzenia i")),
          risk_derivation("dlaczego różnica równa się pochodnej", c(
            "Przy niezależnych zdarzeniach bazowych P(TOP) jest liniowe względem każdego p_i z osobna: rozkładając względem stanu zdarzenia i, dostajemy P(TOP) = p_i · P(TOP | i) + (1 − p_i) · P(TOP | nie i). Współczynnik przy p_i to właśnie różnica z (9.9), a więc i pochodna."
          ), lines = c(
            "P(TOP) = p_i · A + (1 − p_i) · B",
            "       = B + p_i · (A − B)",
            "∂P(TOP)/∂p_i = A − B = I_B(i)"
          )),
          risk_example("9.8", "Ważności w drzewie Bananpolu",
            problem = "Oblicz ważność Birnbauma inicjacji I, braku detekcji D i niepowodzenia tłumienia S dla P(I) = 0,005, d = 0,05, s = 0,08.",
            steps = c(
              "I: gdy I zachodzi, P(TOP) = 0,126; gdy nie — 0. I_B(I) = 0,126.",
              "D: gdy D zachodzi, TOP = I, więc P = 0,005; gdy nie — TOP = I ∩ S, więc P = 0,005 · 0,08 = 0,0004. I_B(D) = 0,0046 = 0,005 · (1 − 0,08).",
              "S: analogicznie 0,005 − 0,005 · 0,05 = 0,00475 = 0,005 · (1 − 0,05)."
            ),
            answer = "I_B(I) = 0,126, I_B(D) = 0,0046, I_B(S) = 0,00475. Ważność Birnbauma mierzy wrażliwość na bezwzględną zmianę p_i — ale zmiana inicjacji o 0,01 i zmiana braku detekcji o 0,01 to w praktyce zupełnie różne przedsięwzięcia."
          )
        )
      ),
      list(
        id = "krytycznosc", title = "Ta sama względna poprawa",
        body = list(
          c(
            "Ostatnie zdanie przykładu 9.8 wskazuje słabość ważności Birnbauma w zastosowaniach: nie uwzględnia ona, jak duże jest samo p_i. Obniżenie P(I) z 0,005 do 0,004 to redukcja o 20%, a obniżenie d z 0,05 do 0,04 — również o 20%, choć bezwzględnie dziesięć razy większa. Dlatego widget porównuje liście przy tej samej względnej redukcji r.",
            "Z liniowości P(TOP) względem p_i wynika, że obniżenie p_i do (1 − r) · p_i zmniejsza wynik dokładnie o r · p_i · I_B(i). Podzielone przez P(TOP) daje to miarę, która nie zależy od r."
          ),
          risk_formula("\\Delta P_i=r\\,p_i\\,I_B(i)=r\\,P(\\mathrm{TOP})\\,I_{CR}(i),\\qquad I_{CR}(i)=\\frac{p_i\\,I_B(i)}{P(\\mathrm{TOP})}", num = "9.10",
            legend = c("r" = "względna redukcja parametru, np. 0,5", "\\Delta P_i" = "spadek P(TOP) po redukcji liścia i", "I_{CR}(i)" = "ważność krytyczna liścia i")),
          risk_definition("9.6", "Ważność krytyczna", c(
            "Ważność krytyczna zdarzenia bazowego i to względny spadek P(TOP) przypadający na względny spadek p_i. Równoważnie: prawdopodobieństwo, że zdarzenie i zaszło i było krytyczne, pod warunkiem że zaszło zdarzenie szczytowe."
          )),
          risk_try("zostaw redukcję 0,5 i odczytaj kolejność słupków. Potem zmień redukcję na 0,2 i 0,9 — sprawdź, czy kolejność się zmienia."),
          risk_widget_panel("Wrażliwość", "Ta sama redukcja względna każdego liścia", sliderInput("f9_reduction", "Redukcja parametru", 0, .9, .5, .05), "f9_rank_plot", "f9_rank_stats"),
          c(
            "Widget liczy ranking dla wartości bazowych 0,005; 0,05; 0,08, niezależnie od suwaków z rozdziału o rachunku. Przy r = 0,5 słupki mają wysokości 0,000315 dla inicjacji, 0,000190 dla tłumienia i 0,000115 dla detekcji — to 50%, 30% i 18% wyjściowego P(TOP). Ważności krytyczne wynoszą więc 1, około 0,60 dla tłumienia i około 0,37 dla detekcji.",
            "Zmiana r skaluje wszystkie słupki w tej samej proporcji i nie zmienia kolejności — tak mówi wzór (9.10). Inicjacja wygrywa, bo każdy scenariusz przez nią przechodzi: należy do obu minimalnych przekrojów. Tłumienie wyprzedza detekcję, bo zawodzi częściej (0,08 wobec 0,05), więc jego przekrój {I, S} odpowiada za większą część ryzyka."
          ),
          "W literaturze spotkasz też miarę Fussella–Vesely’ego: udział w P(TOP) przekrojów zawierających dane zdarzenie. Dla inicjacji wynosi 1, dla detekcji 0,00025 / 0,00063 ≈ 0,40, dla tłumienia 0,0004 / 0,00063 ≈ 0,63 — kolejność jest ta sama co dla ważności krytycznej. Różne miary odpowiadają na nieco różne pytania, ale w małych drzewach zwykle prowadzą do tego samego rankingu.",
          risk_check("f9_chk_rank",
            "Dlaczego obniżenie P(I) o połowę obniża P(TOP) dokładnie o połowę?",
            c("Bo I należy do każdego minimalnego przekroju, a P(TOP) jest proporcjonalne do P(I)" = "all", "Bo I ma największe prawdopodobieństwo w drzewie" = "largest", "Bo redukcja o połowę zawsze działa proporcjonalnie" = "always"),
            correct = "all",
            explanation = "P(TOP) = P(I) · P(D ∪ S | I), więc wynik jest wprost proporcjonalny do P(I). Ważność krytyczna I równa się 1, bo I występuje w każdym scenariuszu.",
            hints = c(largest = "P(I) = 0,005 jest najmniejszym parametrem w drzewie.", always = "Połowa d obniża P(TOP) tylko o około 18%. Co odróżnia I od D?")
          )
        )
      )
    ),
    decision = "Ranking jest wskazówką do rozmowy o kosztach i wykonalności, nie automatycznym wyborem."
  ),
  list(
    id = "granice", title = "Granice drzewa błędów", hook = "Dokładny rachunek nie naprawi złego drzewa",
    lead = "Drzewo błędów zna tylko te przyczyny, które ktoś przewidział: niekompletnej struktury ani słabych danych nie naprawi żaden rachunek, więc audytujemy jedno i drugie.",
    intro = c(
      "Drzewo błędów modeluje tylko te scenariusze, które ktoś przewidział. Przyczyna nieobecna w drzewie ma w rachunku prawdopodobieństwo zero — nie dlatego, że jest niemożliwa, lecz dlatego, że nikt o niej nie pomyślał. Dlatego dojrzała analiza kończy się przeglądem eksperckim, a nie odczytem wyniku.",
      "Druga granica to statyczność: klasyczne FTA opisuje kombinacje stanów, słabiej radzi sobie z sekwencjami i czasem reakcji. Trzecia — jakość danych w liściach: wynik dziedziczy niepewność najsłabszego parametru, co w naszym kursie podkreślamy, oznaczając wszystkie liczby jako fikcyjne."
    ),
    sections = list(
      list(
        id = "kompletnosc", title = "Kompletność i dane",
        body = list(
          c(
            "Niekompletność drzewa zawsze działa w jedną stronę: brakujący liść pod bramką OR może tylko podnieść wynik, więc drzewo niekompletne zaniża ryzyko. Nie da się tego wykryć rachunkiem — rachunek jest bezbłędny dla drzewa, które dostał. Pomagają listy kontrolne typowych przyczyn, przegląd przez osoby spoza zespołu i porównanie z raportami z incydentów w podobnych obiektach.",
            "Statyczność oznacza, że drzewo zapisuje, które zdarzenia zaszły, ale nie w jakiej kolejności ani jak szybko. Detekcja, która zadziała po dwudziestu minutach, jest w drzewie „sprawna”, choć w praktyce przegapiła moment, w którym tłumienie mogło jeszcze pomóc. Takie zależności modeluje się drzewami zdarzeń albo analizą czasową; w FTA trzeba je przynajmniej wpisać do definicji zdarzeń, np. „brak detekcji w ciągu 2 minut od zapłonu”."
          ),
          risk_example("9.9", "Zapomniany zawór",
            problem = "Przegląd ekspercki wskazuje brakującą przyczynę: po konserwacji zawór zasilający instalację tłumienia bywa pozostawiany zamknięty. Przy inicjacji zdarza się to z prawdopodobieństwem 0,02, niezależnie od D i S. Jak zmienia się P(TOP) i lista minimalnych przekrojów?",
            steps = c(
              "Zamknięty zawór V wystarcza, żeby tłumienie zawiodło, więc dołącza do bramki OR: TOP = I ∩ (D ∪ S ∪ V).",
              "Ze wzoru (9.2): P(D ∪ S ∪ V | I) = 1 − 0,95 · 0,92 · 0,98 = 0,14348.",
              "P(TOP) = 0,005 · 0,14348 ≈ 0,000717, o około 14% więcej niż 0,00063.",
              "Nowy minimalny przekrój: {I, V}. Pozostałe: {I, D}, {I, S}."
            ),
            answer = "P(TOP) rośnie do około 0,000717 (+14%), a lista przekrojów zyskuje {I, V}. Przed przeglądem ten scenariusz miał w rachunku prawdopodobieństwo zero."
          ),
          c(
            "Niepewność danych można przynajmniej pokazać. Jeśli o d wiemy tylko tyle, że leży między 0,02 a 0,10, wzór (9.4) daje P(TOP) od 0,005 · (1 − 0,98 · 0,92) = 0,000492 do 0,005 · (1 − 0,90 · 0,92) = 0,00086. Uczciwy raport podaje taki przedział obok wartości punktowej — i wskazuje, który parametr go najbardziej rozszerza."
          ),
          risk_check("f9_chk_granice",
            "Zespół uzupełnia drzewo o przeoczoną przyczynę, dołączając ją do istniejącej bramki OR. Co może się stać z P(TOP)?",
            c("Może tylko wzrosnąć albo pozostać bez zmian" = "up", "Może tylko zmaleć" = "down", "Może zmienić się w dowolną stronę" = "any"),
            correct = "up",
            explanation = "Dodanie wejścia do bramki OR powiększa zbiór sytuacji prowadzących do jej wyjścia, a w drzewie złożonym wyłącznie z bramek AND i OR (drzewie koherentnym) większe wyjście bramki nie może obniżyć P(TOP). Dlatego niekompletne drzewo zaniża ryzyko.",
            hints = c(down = "Nowa przyczyna to nowy sposób, w jaki bramka OR może zajść.", any = "Czy w drzewie złożonym tylko z AND i OR większe prawdopodobieństwo liścia może obniżyć P(TOP)?")
          )
        )
      ),
      list(id = "audit", title = "Przegląd ekspercki", bullets = c("Czy top event jest jednoznaczny?", "Czy lista przyczyn jest wystarczająca?", "Gdzie założono niezależność?", "Czy jednostki i horyzonty są zgodne?", "Które dane są fikcyjne lub niepewne?"))
    )
  ),
  list(
    id = "sprawdzenie", title = "Ściąga i sprawdzenie", hook = "Najpierw logika, potem liczby",
    lead = "Zdarzenie szczytowe → bramki → rachunek → przekroje → istotność; quiz i ćwiczenia sprawdzają zarówno logikę drzewa, jak i rachunek.",
    sections = list(
      list(
        id = "podsumowanie", title = "Podsumowanie",
        text = c(
          "Analiza drzewa błędów zaczyna się od zdania, nie od liczby: audytowalnej definicji zdarzenia szczytowego (definicja 9.1). Drzewo rozkłada ten stan na zdarzenia pośrednie i bazowe, łącząc je bramkami AND i OR. Dla niezależnych wejść bramka AND daje iloczyn (9.1), a bramka OR — dopełnienie iloczynu dopełnień (9.2). Dzięki dualności (9.3) te same wzory znamy z układów szeregowych i równoległych: drzewo błędów jest schematem blokowym zapisanym w języku awarii.",
          "Rachunek od liści do korzenia (9.4) jest poprawny, gdy każdy liść występuje w drzewie raz. Ogólniejszą drogą są minimalne przekroje (9.6): zdarzenie szczytowe jest ich sumą, a P(TOP) liczymy z zasady włączeń i wyłączeń albo przybliżeniem rzadkich zdarzeń (9.5, 9.7), które zawyża wynik i wymaga małych wejść bramek OR. Powtórzone zdarzenia i wspólne przyczyny (9.8) trzeba zredukować algebraicznie, zanim cokolwiek pomnożymy — naiwne liczenie zawyża wynik pod OR i zaniża go pod AND.",
          "Miary ważności (9.9, 9.10) zamieniają drzewo w ranking: w drzewie Bananpolu najważniejsza jest inicjacja, bo należy do każdego przekroju. Ranking otwiera jednak dopiero rozmowę o kosztach, a wynik całej analizy jest tak dobry, jak kompletność drzewa i jakość danych w liściach."
        )
      ),
      list(
        id = "sciaga", title = "Ściąga",
        text = c(
          "FTA łączy wszystko, co kurs zbudował wcześniej: zdarzenia i dopełnienia z wykładu pierwszego, niezależność i wspólne przyczyny z drugiego, algebrę bramek z wykładu o systemach. Reguły poniżej wystarczają do audytu małego drzewa — własnego i cudzego.",
          "Quiz sprawdza regułę OR i rozumienie przekrojów; ćwiczenia prowadzą przez rachunek małego drzewa, polowanie na powtórzone zdarzenia bazowe i budowę własnego drzewa poza Bananpolem."
        ),
        bullets = c("Najpierw logika, potem liczby", "AND: potrzebne wszystkie wejścia", "OR: wystarczy co najmniej jedno wejście", "Powtórzony liść pozostaje tym samym zdarzeniem", "Wynik zależy od kompletności drzewa"),
        widget = risk_assessment_ui("f9", fta_quiz, fta_exercises)
      ),
      list(id = "most", title = "Co dalej", text = "Masz komplet narzędzi: od definicji zdarzenia po drzewo błędów. Ostatni wykład połączy je w jedno studium — z teczki danych Bananpolu, przez karty obliczeniowe, do czterozdaniowej rekomendacji dla zarządu.")
    )
  )
))
fta_chapters <- risk_block_chapters(fta_block)

fta_server <- function(input, output, session) {
  v <- reactiveVal(FALSE)
  observeEvent(input$f9_vote_check, v(TRUE))
  output$f9_vote_feedback <- renderUI({
    req(v())
    if (is.null(input$f9_vote)) {
      return(lc_feedback(type = "info", "Najpierw zaznacz jedną z odpowiedzi."))
    }
    lc_feedback(type = if (identical(input$f9_vote, "good")) "ok" else "warning", tags$strong("Dobra definicja:"), " nieopanowany pożar magazynu w ciągu jednego roku.")
  })
  output$f9_structure <- renderUI({
    n <- length(input$f9_causes)
    lc_feedback(type = if (n >= 2) "info" else "warning", paste0("Wybrano bramkę ", toupper(input$f9_gate), " i ", n, " wejść. "), if (input$f9_gate == "or") "Każde wejście może wystarczyć." else "Wszystkie wybrane wejścia są potrzebne.")
  })
  output$f9_state_result <- renderUI({
    s <- input$f9_states
    active <- "init" %in% s && ("detect" %in% s || "suppress" %in% s)
    lc_feedback(type = if (active) "warning" else "ok", tags$strong(if (active) "Top event aktywny." else "Top event nieaktywny."), " Logika: inicjacja AND (brak detekcji OR brak tłumienia).")
  })
  tree_value <- reactive(risk_fta_top(input$f9_init, input$f9_detect, input$f9_suppress))
  tree_plot <- reactive({
    nodes <- data.frame(x = c(2, 1, 3, .5, 1.5), y = c(3, 2, 2, 1, 1), label = c("TOP (AND)", "Inicjacja", "OR", "Brak detekcji", "Brak tłumienia"), type = c("Szczytowe", "Bazowe", "Bramka", "Bazowe", "Bazowe"))
    edges <- data.frame(x = c(2, 2, 3, 3), y = c(3, 3, 2, 2), xend = c(1, 3, .5, 1.5), yend = c(2, 2, 1, 1))
    ggplot() +
      geom_segment(data = edges, aes(x, y, xend = xend, yend = yend), colour = upwr_reference) +
      geom_point(data = nodes, aes(x, y, shape = type, colour = type), size = 7) +
      geom_text(data = nodes, aes(x, y - .25, label = label), size = 3) +
      scale_colour_manual(values = upwr_cat_n(3)) +
      coord_equal(xlim = c(0, 3.5), ylim = c(.5, 3.4)) +
      labs(title = "Małe drzewo Bananpolu", x = NULL, y = NULL, shape = NULL, colour = NULL) +
      theme_upwr() +
      theme(axis.text = element_blank(), axis.ticks = element_blank())
  })
  zoom_plot_server("f9_tree_plot", tree_plot, alt = "Drzewo błędów z inicjacją połączoną przez AND z bramką OR dwóch niesprawności zabezpieczeń.")
  output$f9_tree_stats <- renderUI(lc_stat_grid(lc_stat_box("P(D ∪ S | I)", risk_format_probability(risk_gate_or(c(input$f9_detect, input$f9_suppress)))), lc_stat_box("P(top)", risk_format_probability(tree_value()), color = upwr_accent), columns = 1))
  output$f9_cut_text <- renderUI(lc_feedback(type = "info", if (input$f9_cut == "id") "Inicjacja + brak detekcji wystarczają do TOP." else "Inicjacja + brak tłumienia wystarczają do TOP."))
  output$f9_repeat_result <- renderUI({
    q <- input$f9_repeat
    wrong <- risk_gate_or(c(q, q))
    lc_stat_grid(lc_stat_box("Jedno wspólne zdarzenie", risk_format_probability(q), color = upwr_accent), lc_stat_box("Błędne OR niezależnych kopii", risk_format_probability(wrong)), lc_stat_box("Błędne AND niezależnych kopii", risk_format_probability(q^2)), columns = 1)
  })
  output$f9_common_result <- renderUI({
    q <- input$f9_common
    local_failure <- risk_gate_or(c(input$f9_detect, input$f9_suppress))
    result <- input$f9_init * (q + (1 - q) * local_failure)
    tagList(lc_stat_grid(lc_stat_box("Bez wspólnej przyczyny", risk_format_probability(tree_value(), 6)), lc_stat_box("Ze wspólną przyczyną", risk_format_probability(result, 6)), columns = 1), lc_p("Przekroje: {I,C}, {I,D₀}, {I,S₀}. Suwaki detekcji i tłumienia interpretujemy tu jako lokalne niepowodzenia bez C."))
  })
  rank_plot <- reactive({
    base <- c(init = .005, detect = .05, suppress = .08)
    top0 <- do.call(risk_fta_top, unname(as.list(base)))
    gains <- vapply(names(base), function(n) {
      x <- base
      x[n] <- x[n] * (1 - input$f9_reduction)
      top0 - do.call(risk_fta_top, unname(as.list(x)))
    }, numeric(1))
    dat <- data.frame(element = factor(c("Inicjacja", "Detekcja", "Tłumienie"), levels = c("Inicjacja", "Detekcja", "Tłumienie")), gain = gains)
    ggplot(dat, aes(element, gain, fill = element)) +
      geom_col() +
      scale_fill_manual(values = upwr_cat_n(3), guide = "none") +
      labs(title = "Spadek P(top) po poprawie", x = NULL, y = "Redukcja P(top)") +
      theme_upwr()
  })
  zoom_plot_server("f9_rank_plot", rank_plot, alt = "Słupki redukcji prawdopodobieństwa zdarzenia szczytowego po poprawie każdego liścia.")
  output$f9_rank_stats <- renderUI(lc_feedback(type = "info", "Porównanie dotyczy modelu bazowego i jednakowej redukcji względnej."))
  risk_assessment_server("f9", fta_quiz, input, output)
}
