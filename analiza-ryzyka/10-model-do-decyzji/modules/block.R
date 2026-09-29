# Blok 10: Od modelu do decyzji -----------------------------------------

integracja_quiz <- list(questions = list(
  list(question = "Czy roczna awaria choć raz oznacza niedostępność podczas zapotrzebowania?", choices = c("Nie, urządzenie mogło zostać naprawione" = "no", "Tak, to ten sam horyzont" = "yes", "Tak, gdy p jest małe" = "rare"), correct = "no", explanation = "Stan podczas zapotrzebowania i awaria w dowolnym momencie roku to różne zdarzenia."),
  list(question = "Która liczba z karty detekcji trafia do FTA jako przeoczenie?",
    choices = c("P(I | alarm)" = "a", "FPR" = "b", "1−czułość" = "c"), correct = "c",
    explanation = "Przeoczenie to brak alarmu przy zapotrzebowaniu; posterior odpowiada na inne pytanie."),
  list(question = "Zmienia się czas misji. Co należy przeliczyć w karcie systemu?",
    choices = c("Tylko prawdopodobieństwo inicjacji" = "a", "R(t) wszystkich elementów" = "b", "Tylko etykietę czasu" = "c"), correct = "b",
    explanation = "Elementy muszą mieć wspólny czas; tu R zasilania i sterownika przeliczamy modelem wykładniczym."),
  list(question = "Co odróżnia R(3000) od R(1000)³ dla Weibulla?",
    choices = c("Ciągłe starzenie od trzech misji nowych urządzeń" = "a", "Tylko zaokrąglenie" = "b", "Nic, zawsze są równe" = "c"), correct = "a",
    explanation = "Potęgowanie identycznego R zakłada identyczny stan początkowy kolejnych misji, np. odnowę."),
  list(question = "Najlepsze działanie w budżecie przekracza limit w ostrożnym scenariuszu. Co raportujemy?",
    choices = c("Zatwierdzamy, bo jest najlepsze" = "a", "Ukrywamy ostrożny scenariusz" = "b", "Kryterium nie jest spełnione; potrzebny inny projekt lub zakres" = "c"), correct = "c",
    explanation = "Ranking i spełnienie kryterium to odrębne sprawdzenia; potrzebne jest jawne ryzyko po działaniu.")
))
integracja_exercises <- list(
  list(
    task = "Bananpol: dla misji 1000 h, p inicjacji 0,005, czułości 0,95, R zasilania 0,98 i sterownika 0,95 policz P(top) dla obu modeli wentylatora. Wyjaśnij, które parametry są warunkowe.",
    answer = c(
      "Model wykładniczy: R_A = R_B = e^(−1000/1500) ≈ 0,513; gałąź równoległa 1 − 0,487² ≈ 0,763; R_sys = 0,98 · 0,95 · 0,763 ≈ 0,711. Ze wzoru (10.8): P(TOP) = 0,005 · [1 − 0,95 · 0,711] ≈ 0,005 · 0,325 ≈ 0,00162, czyli około 16 na 10 000 misji.",
      "Weibull β = 2, η = 1700 h: R_A = e^(−(1000/1700)²) ≈ 0,707; gałąź równoległa ≈ 0,914; R_sys ≈ 0,851; P(TOP) = 0,005 · [1 − 0,95 · 0,851] ≈ 0,00096, czyli około 10 na 10 000 misji.",
      "Warunkowe są P(D | I) = 0,05 i P(S | I) = 1 − R_sys: opisują zachowanie barier tylko w misjach, w których zapotrzebowanie wystąpiło. Bezwarunkowe jest P(I) = 0,005 — udział misji z zapotrzebowaniem."
    )
  ),
  list(
    task = "Czas: porównaj R(3000), R(1000)³ i R(500)⁶ dla Weibulla β=2, η=1700 h. Nazwij różne polityki odnowy; sprawdź, czy podział czasu bez wymiany może zmienić wynik.",
    answer = c(
      "R(3000) ≈ 0,044 — jeden wentylator bez wymiany przez 3000 h. R(1000)³ ≈ 0,354 — wymiana na nowy co 1000 h. R(500)⁶ ≈ 0,595 — wymiana co 500 h. Każda liczba opisuje inną politykę odnowy (Definicja 10.3), a nie inny zapis tego samego zdarzenia.",
      "Podział czasu bez wymiany niczego nie zmienia: ze wzoru (10.5) R(500) · R(1000 | 500) · … · R(3000 | 2500) = R(3000), bo iloczyn prawdopodobieństw warunkowych się skraca. Zysk daje dopiero odnowa, a przy modelu wykładniczym nawet ona nic nie daje (0,135 we wszystkich trzech wariantach), bo brak pamięci oznacza, że wymiana nie odmładza."
    )
  ),
  list(
    task = "Decyzja: przy budżecie 2 i demonstracyjnym limicie P(top)=0,002 porównaj dopuszczalne działania w trzech scenariuszach. Sprawdź wynik po działaniu; jeśli żadne nie spełnia kryterium, nie deklaruj akceptowalności.",
    answer = c(
      "W budżecie 2 mieszczą się lepszy czujnik (koszt 2) i ograniczenie źródła ciepła (koszt 1). Dla modelu wykładniczego, misji 1000 h i u = 0,2: czujnik daje 0,00092 / 0,00154 / 0,00230 (optymistyczny / bazowy / ostrożny), ograniczenie źródła ciepła 0,00040 / 0,00081 / 0,00144.",
      "Czujnik przekracza limit 0,002 w scenariuszu ostrożnym, więc nie spełnia kryterium. Ograniczenie źródła ciepła mieści się w limicie we wszystkich trzech scenariuszach i jest najlepsze w każdym z nich — rekomendacja jest odporna w badanym zakresie. Dla Weibulla oba działania spełniają limit (najgorszy wynik czujnika ≈ 0,00130), ale ranking jest ten sam."
    )
  ),
  list(
    task = "Transfer: zbuduj scenariusz dla innego systemu, wskaż człowieka, procedurę, barierę techniczną i możliwy skutek. Oddaj rekomendację z ryzykiem po działaniu, właścicielem i terminem kontroli.",
    answer = c(
      "Odpowiedź jest otwarta; oceniamy kompletność łańcucha. Przykład: rozładunek chemikaliów. Zdarzenie inicjujące — rozszczelnienie złącza węża; bariera techniczna — zawór odcinający z czujnikiem ciśnienia; procedura — kontrola złącza przed rozładunkiem; człowiek — operator, który musi zareagować na alarm w ciągu minuty; skutek — wyciek do kanalizacji deszczowej.",
      "Notatka powinna mieć strukturę z Przykładu 10.12: kontrakt zdarzeń i jednostkę (jeden rozładunek), P(TOP) przed i po działaniu w kilku scenariuszach, koszt, jawne kryterium, założenia (np. niezależność reakcji operatora od awarii czujnika) oraz właściciela i termin kontroli skuteczności."
    )
  ),
  list(
    task = "Horyzont roczny: Bananpol wykonuje trzy misje po 1000 h rocznie, a przed każdą misją elementy układu są odnawiane. Dla modelu wykładniczego policz roczne prawdopodobieństwo co najmniej jednej utraty ochrony oraz wielkość 1 − R_sys³. Wyjaśnij, dlaczego różnią się ponad stukrotnie.",
    answer = c(
      "Ze wzoru (10.9): P_rok = 1 − (1 − 0,00162)³ ≈ 0,0049, prawie dokładnie 3 · P(TOP), bo P(TOP) jest małe.",
      "1 − R_sys³ = 1 − 0,711³ ≈ 0,641 to prawdopodobieństwo, że układ chłodzenia zawiódłby w co najmniej jednej z trzech misji, gdyby w każdej musiał pracować. To miara niezawodności układu, przydatna np. do planowania części zamiennych, ale nie ryzyko utraty ochrony: układ jest potrzebny tylko w misjach z zapotrzebowaniem, a te zdarzają się rzadko — co najmniej jedno zapotrzebowanie w roku ma prawdopodobieństwo 1 − 0,995³ ≈ 0,015."
    )
  ),
  list(
    task = "Odporność: ustaw model Weibulla, budżet 3 i zakres u = 0,5. Które działanie wygrywa w każdym scenariuszu? Wyjaśnij mechanizm ewentualnej zmiany rankingu.",
    answer = c(
      "W scenariuszach optymistycznym i bazowym wygrywa ograniczenie źródła ciepła, w ostrożnym — dodatkowy wentylator (≈ 0,00168 wobec ≈ 0,00172).",
      "Przy m = 1,5 skuteczność ograniczenia źródła ciepła spada do 0,5 · (2 − 1,5) = 0,25, więc redukuje ono ryzyko tylko o jedną czwartą. Redundancja nie ma w modelu parametru skuteczności — trzecia gałąź jest z założenia niezależna i w pełni sprawna. Zmiana rankingu wynika więc z tego, które założenie osłabiono, i powinna trafić do notatki jako warunek rekomendacji."
    )
  )
)

integracja_block <- list(id = "integracja", title = "Od modelu do decyzji", chapters = list(
  list(
    id = "teczka", title = "Kontrakt zdarzeń", hook = "Zanim wybierzesz wzór, spisz, co wiesz",
    lead = "Najpierw opisujemy misję i dostępne dane, potem nazywamy zdarzenia i dopiero wtedy wybieramy model.",
    intro = c(
      "Bananpol uruchamia partię w komorze. Na początku misji może wystąpić stan wymagający aktywnego chłodzenia przez cały zadany czas. Ochrona wymaga wykrycia tego stanu i ciągłej pracy układu chłodzenia. Analizujemy utratę wymaganej ochrony termicznej; nie utożsamiamy jej automatycznie z pożarem ani urazem.",
      "Wszystkie liczby są fikcyjne. W tym uproszczonym scenariuszu nie ma napraw podczas misji, a każdy element zaczyna sprawny i nowy. Jeden wentylator wystarcza do wymaganej wydajności; oba pracują, a awaria jednego nie zmienia charakterystyki drugiego.",
      "Ten wykład nie wprowadza nowego rachunku. Wszystkie narzędzia są już w kursie: prawdopodobieństwo warunkowe z wykładu 02, Bayes z wykładu 03, rozkład dwumianowy i granica przy zerze zdarzeń z wykładu 04, funkcje czasu życia z wykładu 07, struktura systemu z wykładu 08 i drzewo błędów z wykładu 09. Nowe jest zadanie: złożyć je w jedną, spójną analizę, która kończy się decyzją. Czytaj ten wykład jak rozwiązane studium przypadku — każdy krok ma uzasadnienie, odwołanie do źródła, przeliczenie i interpretację."
    ),
    callout = list(label = "Dane fikcyjne", text = "Misja bazowa: 1000 h. P zapotrzebowania na początku misji: 0,005; czułość: 0,95; FPR: 0,05. Parametry czasu życia wentylatora pochodzą z bloku 07. Zasilanie i sterownik mają wykładnicze czasy życia z R(1000 h)=0,98 i 0,95.", color = "uwaga"),
    sections = list(
      list(
        id = "audyt", title = "Od czego zacząć?",
        body = list(
          c(
            "Zarząd Bananpolu pyta krótko: którą barierę poprawić najpierw i czy po tej zmianie ryzyko będzie akceptowalne. Kusi, żeby od razu otworzyć arkusz i wstawić liczby do wzoru. Tymczasem największe błędy analiz integracyjnych nie powstają w rachunkach, lecz w szwach między nimi: w niezgodnych jednostkach, pomieszanych horyzontach czasu i cichych założeniach, których nikt nie zapisał.",
            "Dlatego studium przypadku zaczyna się od teczki. Zanim cokolwiek policzymy, zapisujemy, co analizujemy, w jakim horyzoncie, skąd pochodzą liczby i czego nie wiemy."
          ),
          risk_definition("10.1", "Teczka przypadku", c(
            "Teczka przypadku to zestawienie wszystkich wejść analizy przed rachunkiem: definicji zdarzeń, jednostki analizy i horyzontu czasu, stanu początkowego elementów, źródeł danych z warunkami pomiaru oraz jawnej listy braków i niepewności.",
            "Teczka nie zawiera jeszcze wyników. Jej zadaniem jest sprawić, żeby każda liczba użyta później miała nazwę, jednostkę i pochodzenie."
          )),
          risk_vote_panel("i10_vote", "i10_vote_feedback", "Od czego zacząć analizę przypadku?", c("Od definicji i audytu danych" = "audit", "Od wzoru" = "model", "Od najtańszego działania" = "cost")),
          "Wybór „od wzoru” wydaje się efektywny, ale wzór zakłada już odpowiedź na pytanie, co jest zdarzeniem i w jakim czasie je liczymy. Wybór „od najtańszego działania” odwraca kolejność całkiem: decyzję podejmuje się przed oceną, którą barierę warto wzmacniać. W obu przypadkach błąd definicji przechodzi niezauważony przez wszystkie kolejne kroki i wraca dopiero w pytaniu recenzenta.",
          risk_try("zaznacz w audycie te elementy teczki Bananpolu, które uważasz za kompletne na podstawie opisu powyżej i ramki z danymi. Dla każdego pola spróbuj powiedzieć jednym zdaniem, gdzie w tekście jest jego uzasadnienie."),
          figure_panel(label = "Audyt", title = "Co sprawdzono?", checkboxGroupInput("i10_fields", "Elementy teczki", c("Definicje zdarzeń" = "event", "Czas i stan początkowy" = "time", "Źródła i warunki pomiaru" = "source", "Niepewność i braki" = "missing")), uiOutput("i10_dossier"), full_width = TRUE),
          c(
            "Licznik pokazuje tylko, ile pól zaznaczono — i celowo nic więcej. W teczce Bananpolu czas i stan początkowy są opisane dobrze: misja 1000 h, elementy nowe, bez napraw. Definicje zdarzeń uporządkujemy w następnej sekcji. Najsłabiej wypadają źródła: dane są fikcyjne, a skuteczność działań (50%) jest w ramce opisana jako hipoteza wymagająca pilotażu. Uczciwa teczka mówi to wprost.",
            "Zaznaczenie pola nie jest dowodem. Dowodem jest zdanie, które da się wskazać recenzentowi: „czułość 0,95 zmierzono na 200 historycznych zapotrzebowaniach w tej samej komorze”. Tam, gdzie takiego zdania nie ma, pole powinno zostać puste, a brak — trafić na listę niepewności, którą wykorzystamy w rozdziale o odporności rekomendacji."
          )
        )
      ),
      list(
        id = "definicje", title = "Kontrakt zdarzeń",
        text = "I oznacza potrzebę chłodzenia na początku misji. D to niewykrycie tej potrzeby, a S — niezdolność układu do utrzymania chłodzenia przez misję. Definiujemy TOP jako I ∩ (D ∪ S). Obie funkcje są wymagane: skuteczna detekcja nie zastępuje chłodzenia, a układ nie zostanie uruchomiony bez sygnału. Roczne prawdopodobieństwo, wynik misji i odpowiedź na zapotrzebowanie nie są zamienne.",
        bullets = c("P(I): udział porównywalnych misji z zapotrzebowaniem na początku", "P(D | I)=1−czułość: przeoczenia wśród rzeczywistych zapotrzebowań", "P(S | I)=1−R_sys(t): awaria układu podczas wymaganej pracy", "D i S są niezależne warunkowo przy I; pomiar detekcji ma osobne zasilanie", "Parametry czasu życia dotyczą właśnie pracy pod wymaganym obciążeniem"),
        body = list(
          c(
            "Te trzy litery są umową między wszystkimi osobami, które później będą liczyć, recenzować i decydować. Dlatego nazywamy ją kontraktem. Jego rdzeniem jest zdarzenie szczytowe, które w wykładzie 09 nazywaliśmy TOP. Zapis I ∩ (D ∪ S) czyta się jak zdanie: jest zapotrzebowanie i (nie wykryto go lub chłodzenie nie wytrzymało). Iloczyn i suma zdarzeń to te same operacje, które w wykładzie 01 rysowaliśmy na diagramach Venna, a w wykładzie 09 — jako bramki AND i OR."
          ),
          risk_definition("10.2", "Kontrakt zdarzeń", c(
            "Kontrakt zdarzeń to zestaw nazwanych zdarzeń z jednoznacznym opisem, jednostką (tu: jedna misja) i warunkiem, w którym każde prawdopodobieństwo jest mierzone. Każde prawdopodobieństwo w analizie musi być albo bezwarunkowe względem jednostki (jak P(I)), albo jawnie warunkowe (jak P(D | I))."
          )),
          risk_formula("\\mathrm{TOP}=I\\cap(D\\cup S)", num = "10.1",
            legend = c("I" = "zapotrzebowanie na chłodzenie na początku misji", "D" = "niewykrycie zapotrzebowania przez detektor", "S" = "niezdolność układu chłodzenia do pracy przez cały czas misji", "\\mathrm{TOP}" = "utrata wymaganej ochrony termicznej w misji")),
          "Warunkowość w kontrakcie nie jest ozdobą. Czułość detektora zmierzono w sytuacjach, w których zapotrzebowanie rzeczywiście wystąpiło — to jest P(alarm | I), a nie odsetek wszystkich alarmów. Niezawodność układu liczymy dla pracy pod wymaganym obciążeniem, bo właśnie wtedy jego awaria ma znaczenie. Pomylenie warunku zmienia liczbę nawet wtedy, gdy nazwa wielkości brzmi tak samo.",
          risk_example("10.1", "Zapisz zdanie z raportu jako zdarzenie",
            problem = c(
              "Poniższe zdania pochodzą z protokołów czterech misji. Zapisz każde w języku kontraktu i rozstrzygnij, czy zaszło zdarzenie TOP.",
              "(a) Wystąpiło zapotrzebowanie, detektor dał alarm, ale po 400 h zatrzymał się sterownik. (b) Detektor dał alarm, choć zapotrzebowania nie było; wentylatory pracowały całą misję. (c) Wystąpiło zapotrzebowanie, wykryto je, wentylator A zatrzymał się po 200 h, a B pracował do końca. (d) Wystąpiło zapotrzebowanie, detektor milczał; układ chłodzenia był sprawny."
            ),
            steps = c(
              "(a) I ∩ Dᶜ ∩ S. Awaria sterownika wyłącza cały układ (sterownik jest połączony szeregowo), więc zaszło S, a zatem D ∪ S. TOP zaszło.",
              "(b) Iᶜ z fałszywym alarmem. Bez zapotrzebowania TOP nie może zajść — iloczyn z I jest pusty. Zbędne uruchomienie ma swój koszt, ale leży poza analizowanym TOP.",
              "(c) I ∩ Dᶜ ∩ Sᶜ. Jeden wentylator wystarcza, więc awaria A przy sprawnym B nie jest zdarzeniem S. TOP nie zaszło.",
              "(d) I ∩ D. Sprawny układ nie pomoże, jeśli nie dostanie sygnału. TOP zaszło, choć żaden element chłodzenia nie zawiódł."
            ),
            answer = "TOP zaszło w misjach (a) i (d). Przypadek (d) pokazuje, dlaczego w nawiasie jest suma D ∪ S: obie funkcje są wymagane i brak każdej z nich wystarcza do utraty ochrony."
          ),
          risk_check("i10_chk_kontrakt",
            "Inspektor zapisał w raporcie: „czułość 0,95, więc 5% misji kończy się przeoczeniem”. Co jest nie tak?",
            c("Nic, to poprawny wniosek" = "ok", "5% to P(D | I): dotyczy tylko misji z zapotrzebowaniem" = "cond", "Przeoczeń jest 95%" = "inverse"),
            correct = "cond",
            explanation = "1 − czułość = P(D | I) = 0,05 to odsetek przeoczeń wśród misji z zapotrzebowaniem. Wśród wszystkich misji przeoczenie zdarza się z prawdopodobieństwem P(I) · P(D | I) = 0,005 · 0,05 = 0,00025, czyli dwadzieścia razy rzadziej.",
            hints = c(ok = "Zapytaj, w których misjach w ogóle można coś przeoczyć.", inverse = "Czułość to prawdopodobieństwo alarmu przy zapotrzebowaniu, a nie przeoczenia.")
          )
        )
      ),
      list(
        id = "granica", title = "Granica modelu",
        text = "Awaria naprawiona przed zapotrzebowaniem nie jest niedostępnością podczas zapotrzebowania. Gdy potrzeba chłodzenia pojawia się w losowej chwili, trzeba modelować stan systemu w tej chwili oraz dalszą pracę. Nasza misja zaczyna się od ewentualnego zapotrzebowania i nie obejmuje napraw.",
        body = list(
          c(
            "Granica modelu to lista pytań, na które model celowo nie odpowiada. W Bananpolu są trzy takie granice. Po pierwsze, zapotrzebowanie pojawia się tylko na początku misji, więc wystarcza nam niezawodność R(t) z wykładów 07–08, a nie dostępność układu w losowej chwili. Po drugie, w misji nie ma napraw, więc awaria jest ostateczna aż do końca misji. Po trzecie, TOP kończy się na utracie ochrony — dalsze skutki, jak zniszczenie partii czy uraz, wymagają osobnego modelu.",
            "Każda z tych granic ma konsekwencję liczbową. Gdyby zapotrzebowanie mogło pojawić się w połowie misji, układ musiałby przetrwać pierwszą połowę bez obciążenia i drugą pod obciążeniem — rachunek wymagałby prawdopodobieństw warunkowych z wykładu 07. Gdyby dopuszczono naprawy, awaria wentylatora A w 200. godzinie mogłaby zostać usunięta przed awarią B. Model bez tych elementów nie jest błędny, ale jego wynik dotyczy tylko sytuacji, którą opisuje."
          ),
          risk_check("i10_chk_granica",
            "Serwisant twierdzi: „w zeszłym roku wentylator A padał dwa razy, więc ochrona była niedostępna”. Czego brakuje, żeby to rozstrzygnąć?",
            c("Informacji, czy w chwili awarii było zapotrzebowanie i czy przed nim usunięto usterkę" = "state", "Niczego — awaria oznacza niedostępność" = "none", "Liczby awarii wentylatora B" = "b_only"),
            correct = "state",
            explanation = "Niedostępność dotyczy stanu systemu w chwili, gdy ochrona jest potrzebna. Awaria naprawiona przed zapotrzebowaniem nie odbiera ochrony, a awaria A przy sprawnym B nie wyłącza chłodzenia.",
            hints = c(none = "Czy awaria usunięta przed zapotrzebowaniem odbiera ochronę?", b_only = "Stan B jest ważny, ale nie wystarcza: potrzebny jest też moment zapotrzebowania i historia napraw.")
          )
        ),
        pitfall = "Ujednolicenie etykiety czasu nie naprawia pomieszania zdarzeń. Najpierw nazwij warunki, potem łącz liczby."
      ),
      list(
        id = "mapa", title = "Mapa wyboru modelu",
        text = "Prześledź drogę od rejestru do decyzji. Losowość wyników przy znanym p różni się od niewiedzy o p, a obie różnią się od niepewności, czy wybrano właściwą logikę barier.",
        body = list(
          c(
            "Dobór modelu zaczyna się od pytania, a nie od danych. To samo zdanie z teczki — na przykład „w stu kontrolach zaworów nie było wady” — prowadzi do różnych modeli zależnie od tego, czy pytamy o liczbę wad w następnej partii, o czas do pierwszej wady, czy o to, jak duże może być nieznane p. Mapa poniżej zestawia pytania, które padały w kursie, z modelami, które na nie odpowiadają.",
            "Trzy rodzaje niepewności z akapitu wyżej odpowiadają trzem różnym narzędziom. Losowość przy znanym p opisuje rozkład, na przykład dwumianowy. Niewiedzę o p opisuje granica ufności, na przykład przy zerze zdarzeń. Niepewność co do logiki barier nie ma wzoru — ujawniają ją dopiero scenariusze i recenzja drzewa."
          ),
          risk_try("wybieraj kolejno pytania z listy i przy każdym przypomnij sobie wykład, w którym ten model się pojawił, oraz jedno założenie, bez którego przestaje działać."),
          figure_panel(label = "Nawigacja", title = "Wybierz pytanie", selectInput("i10_question", "Pytanie", c("Warunek zmienia ocenę" = "conditional", "Co oznacza alarm" = "bayes", "Ile zdarzeń w n próbach" = "binomial", "Ile prób do pierwszego zdarzenia" = "geometric", "Ile prób do r zdarzeń" = "negative", "Jak często przekraczamy próg" = "threshold", "Czy element dotrwa do czasu t" = "survival", "Czy system spełni funkcję" = "system")), uiOutput("i10_model"), full_width = TRUE),
          "Mapa podaje tylko nazwę modelu, bo reszta należy do analityka. Warunek zmieniający ocenę to wykład 02, alarm — 03, liczba zdarzeń w n próbach — 04, próby do pierwszego lub r-tego zdarzenia — 05, przekroczenie progu — 06, dotrwanie do czasu t — 07, a funkcja systemu — 08 i 09. W studium Bananpolu do końcowego drzewa trafią wyniki tylko trzech z nich: Bayesa w postaci 1 − czułość, czasu życia i struktury systemu.",
          risk_example("10.2", "Pytania z teczki a modele",
            problem = "Przypisz model i wykład źródłowy do każdego pytania zarządu: (a) Jak często alarm detektora oznacza rzeczywistą potrzebę chłodzenia? (b) Czy wentylator dotrwa do końca misji 1000 h? (c) Czy w partii 100 zaworów będzie co najmniej jedna wada? (d) Czy układ chłodzenia jako całość utrzyma pracę? (e) Czy w misji zabraknie wymaganej ochrony?",
            steps = c(
              "(a) Wzór Bayesa, wykład 03: P(I | alarm).",
              "(b) Funkcja niezawodności R(t) z modelu czasu życia, wykład 07.",
              "(c) Rozkład dwumianowy, wykład 04: P(X ≥ 1) = 1 − (1 − p)ⁿ.",
              "(d) Funkcja struktury systemu szeregowo-równoległego, wykład 08.",
              "(e) Drzewo błędów, wykład 09, zasilane wynikami (b) i (d) oraz czułością detektora."
            ),
            answer = "Pytania (a) i (c) mają odpowiedzi ważne dla Bananpolu, ale nie są liśćmi drzewa ochrony termicznej. Pytanie (e) jest właściwym pytaniem decyzyjnym i łączy pozostałe."
          ),
          risk_check("i10_chk_mapa",
            "W 100 kontrolach zaworów nie znaleziono wady. Które pytanie wymaga granicy ufności, a nie samego rozkładu dwumianowego?",
            c("Ile wad będzie w partii przy p = 0,02?" = "known", "Jak duże może być nieznane p, skoro nie było żadnej wady?" = "unknown", "Ile kontroli do pierwszej wady przy p = 0,02?" = "geo"),
            correct = "unknown",
            explanation = "Przy znanym p rozkład opisuje losowość wyników. Gdy p jest nieznane, wnioskujemy o nim z próby — to zadanie dla granicy ufności, np. wzoru (10.3).",
            hints = c(known = "Tu p jest dane, więc wystarcza rozkład dwumianowy.", geo = "To pytanie o czas oczekiwania przy znanym p — wykład 05.")
          )
        )
      )
    )
  ),
  list(
    id = "karty", title = "Karty obliczeniowe", hook = "Trzy pytania, trzy różne rachunki",
    lead = "Detekcja, kontrola partii i czas życia: trzy rachunki cząstkowe, każdy z własnym pytaniem i własną pułapką.",
    intro = c(
      "Zanim złożymy układ i drzewo, porządkujemy liczby na trzech kartach. Do końcowego FTA trafią tylko wyniki detekcji i czasu życia; karta kontroli partii pokazuje, jak wnioskować o p z próby, ale nie opisuje żadnego liścia drzewa ochrony termicznej.",
      "Karta obliczeniowa to jedna strona z jednym pytaniem, jednym modelem i jednym wynikiem, który przekazujemy dalej. Taki podział ma cel praktyczny: recenzent może sprawdzić każdą kartę osobno, a błąd w jednej nie ukrywa się w długim łańcuchu rachunków. Na każdej karcie zaznaczamy też, która liczba wychodzi z niej do drzewa — i która nie powinna."
    ),
    sections = list(
      list(
        id = "karta-alarm", title = "Karta 1: detekcja",
        text = "Detektor ocenia stan na początku misji. Bayes odpowiada, czy za alarmem stoi rzeczywista potrzeba chłodzenia. Do FTA trafi inna liczba z tej samej karty: 1−czułość. Zmiana FPR zmienia wiarygodność alarmu i koszt zbędnych reakcji, lecz w naszym modelu nie zmienia ryzyka przeoczenia. Skutki zbędnego uruchomienia są poza analizowanym TOP.",
        body = list(
          "Karta detekcji odpowiada na dwa różne pytania i dlatego jest najczęstszym źródłem pomyłek. Operator, który widzi alarm, pyta: jak bardzo mam mu wierzyć? To pytanie o prawdopodobieństwo a posteriori z wykładu 03. Analityk ryzyka pyta natomiast: jak często detektor milczy, gdy powinien alarmować? To pytanie o 1 − czułość. Obie liczby opisują ten sam detektor, ale warunkują na różnych zdarzeniach.",
          risk_formula("P(I\\mid A)=\\frac{P(A\\mid I)\\,P(I)}{P(A\\mid I)\\,P(I)+P(A\\mid I^{c})\\,(1-P(I))}", num = "10.2",
            legend = c("A" = "alarm detektora", "P(A\\mid I)" = "czułość (0,95)", "P(A\\mid I^{c})" = "odsetek fałszywych alarmów FPR (0,05)", "P(I)" = "udział misji z zapotrzebowaniem (0,005)")),
          risk_example("10.3", "Karta detekcji w liczbach naturalnych",
            problem = "Przedstaw kartę detekcji Bananpolu na 10 000 porównywalnych misjach. Ile jest alarmów prawdziwych i fałszywych? Jakie jest P(I | alarm), a jakie P(D | I)?",
            steps = c(
              "Misji z zapotrzebowaniem: 10 000 · 0,005 = 50; bez zapotrzebowania: 9950.",
              "Alarmy prawdziwe: 50 · 0,95 = 47,5; przeoczenia: 50 · 0,05 = 2,5.",
              "Alarmy fałszywe: 9950 · 0,05 = 497,5.",
              "P(I | alarm) = 47,5 / (47,5 + 497,5) = 47,5 / 545 ≈ 0,087 — tak samo ze wzoru (10.2).",
              "P(D | I) = 2,5 / 50 = 0,05 = 1 − czułość."
            ),
            answer = "Tylko około 9% alarmów oznacza rzeczywistą potrzebę chłodzenia, a mimo to detektor przeocza zaledwie 5% zapotrzebowań. Do drzewa trafia 0,05; liczba 0,087 opisuje wiarygodność alarmu dla operatora."
          ),
          risk_try("zmień FPR z 0,05 na 0,02, a potem na 0,20, obserwując obie liczby w panelu. Następnie przywróć FPR = 0,05 i zmień czułość na 0,90."),
          figure_panel(label = "Karta 1", title = "Detektor potrzeby chłodzenia", sliderInput("i10_init", "P(I): zapotrzebowanie na początku misji", .001, .03, bananpol$integration$initiation, .001), sliderInput("i10_sens", "Czułość P(alarm | I)", .5, 1, bananpol$integration$sensitivity, .01), sliderInput("i10_fpr", "FPR P(alarm | brak I)", 0, .2, bananpol$integration$false_positive_rate, .005), uiOutput("i10_alarm_result"), full_width = TRUE),
          c(
            "Przy FPR = 0,02 prawdopodobieństwo a posteriori rośnie do około 0,193, a przy FPR = 0,20 spada do około 0,023. Wejście do drzewa, P(D | I) = 0,050, nie drgnie ani razu. Dopiero zmiana czułości na 0,90 podwaja przeoczenia do 0,100. Suwaki P(I) i czułości są jednak wspólne dla całego wykładu: zmiana tutaj przenosi się do końcowego drzewa w następnym rozdziale.",
            "Wniosek dla notatki decyzyjnej jest praktyczny. Jeśli operatorzy skarżą się na fałszywe alarmy, poprawa FPR podniesie zaufanie do alarmu i zmniejszy koszt zbędnych reakcji — ale nie zmniejszy ryzyka utraty ochrony w naszym modelu. Te dwa cele trzeba nazywać osobno."
          ),
          risk_check("i10_chk_alarm",
            "Dostawca oferuje detektor o tej samej czułości 0,95, ale FPR 0,01. Jak zmieni się P(TOP) w naszym drzewie?",
            c("Spadnie, bo alarm będzie wiarygodniejszy" = "down", "Nie zmieni się" = "same", "Wzrośnie" = "up"),
            correct = "same",
            explanation = "Do drzewa trafia P(D | I) = 1 − czułość, niezależne od FPR. Nowy detektor zmienia P(I | alarm), czyli zaufanie operatora, a nie przeoczenia.",
            hints = c(down = "Która liczba z karty detekcji jest liściem drzewa?", up = "FPR nie wchodzi do wzoru na P(TOP).")
          )
        )
      ),
      list(
        id = "karta-kontrola", title = "Karta 2: kontrola partii",
        text = "Partia zaworów jest osobnym problemem odbiorczym. Dla zadanego p liczymy rozkład liczby wad. Gdy p jest nieznane, wnioskujemy z próby: zero wad w n niezależnych kontrolach daje oszacowanie punktowe zero, ale dodatnią górną granicę ufności. Nie wkładamy wyniku tej karty do drzewa ochrony termicznej, bo nie opisuje jego liścia.",
        body = list(
          "Karta kontroli partii ma dwie połowy. W pierwszej p jest znane i pytamy o losowy wynik kontroli — to rozkład dwumianowy z wykładu 04: średnia liczba wad n · p i prawdopodobieństwo co najmniej jednej wady 1 − (1 − p)ⁿ. W drugiej połowie p jest nieznane, a w próbie nie znaleziono żadnej wady. Oszacowanie punktowe wynosi wtedy zero, ale nikt rozsądny nie napisze w raporcie, że zawory są doskonałe.",
          risk_formula("p_{górne}=1-0{,}05^{1/n}\\quad (0\\text{ zdarzeń},\\ 95\\%\\text{ jednostronnie})", num = "10.3",
            legend = c("n" = "liczba niezależnych kontroli, z których żadna nie wykazała wady", "p_{górne}" = "jednostronna górna granica ufności 95% dla p")),
          risk_derivation("granica przy zerze zdarzeń", c(
            "Pytamy, dla jakich wartości p obserwacja „zero wad w n kontrolach” nie byłaby zbyt zaskakująca. Przy danym p jej prawdopodobieństwo wynosi (1 − p)ⁿ. Za zbyt zaskakujące uznajemy wyniki o prawdopodobieństwie poniżej 5%.",
            "Granica leży tam, gdzie (1 − p)ⁿ = 0,05. Logarytmując: n · ln(1 − p) = ln 0,05 ≈ −3,00. Dla małych p ln(1 − p) ≈ −p, stąd przybliżenie p_górne ≈ 3/n, zwane regułą trzech."
          ), lines = c("(1 − p)ⁿ = 0,05", "1 − p = 0,05^(1/n)", "p = 1 − 0,05^(1/n) ≈ 3/n")),
          risk_example("10.4", "Sto kontroli bez wady",
            problem = "Kontrola odbiorcza 100 zaworów nie wykazała żadnej wady. Podaj górną granicę 95% dla p i porównaj ją z regułą trzech. Ile kontroli bez wady potrzeba, żeby granica spadła poniżej 0,003?",
            steps = c(
              "Ze wzoru (10.3): p_górne = 1 − 0,05^(1/100) ≈ 0,0295. Reguła trzech daje 3/100 = 0,030 — różnica na trzecim miejscu po przecinku.",
              "Dla n = 1000: p_górne = 1 − 0,05^(1/1000) ≈ 0,00299, czyli już poniżej 0,003.",
              "Dla porównania n = 10: p_górne ≈ 0,259 — dziesięć kontroli bez wady prawie niczego nie wyklucza."
            ),
            answer = "Po 100 kontrolach bez wady możemy twierdzić tylko, że p nie przekracza około 0,03. Dziesięciokrotnie niższa granica wymaga dziesięciokrotnie większej próby."
          ),
          risk_try("ustaw n = 100 i p = 0,02, odczytaj trzy liczby, a potem przesuń n do 1000. Zwróć uwagę, które liczby zależą od suwaka p, a które nie."),
          figure_panel(label = "Karta 2", title = "Partia zaworów i niepewność p", sliderInput("i10_n", "Liczba niezależnych kontroli n", 10, 1000, 100, 10), sliderInput("i10_p", "Zadane p do prognozy liczby wad", .001, .1, .02, .001), uiOutput("i10_inspection_result"), full_width = TRUE),
          "Przy n = 100 i p = 0,02 panel pokazuje średnio 2 wady i prawdopodobieństwo co najmniej jednej wady około 0,867. Granica przy zerze zdarzeń wynosi 0,030 i nie reaguje na suwak p — bo odpowiada na inne pytanie: nie „co zobaczymy przy znanym p”, lecz „jakie p jest zgodne z tym, co zobaczyliśmy”. Po przesunięciu n do 1000 granica spada do około 0,003, co potwierdza Przykład 10.4."
        ),
        pitfall = "Granica ufności opisuje procedurę wnioskowania przy ustalonym n. Nie jest stwierdzeniem, że po obejrzeniu próby stały parametr ma 95% szans leżeć w przedziale."
      ),
      list(
        id = "karta-utrzymanie", title = "Karta 3: czas życia",
        text = "Wybierz hipotezę o czasie życia wentylatora i długość misji. Model wykładniczy ma MTTF=1500 h; Weibull β=2 i η=1700 h ma MTTF około 1507 h. Ich średnie są zbliżone, ale krzywe różne. Wynik R(t) tej karty trafia bezpośrednio do obu gałęzi systemu.",
        body = list(
          "Dwie hipotezy pochodzą z wykładu 07. Model wykładniczy zakłada stały hazard: wentylator nie starzeje się, a awarie są przypadkowe. Weibull z parametrem kształtu β = 2 opisuje zużycie: hazard rośnie liniowo z czasem pracy. Funkcje niezawodności obu modeli mają postać:",
          risk_formula("R_{\\exp}(t)=e^{-t/1500},\\qquad R_{W}(t)=e^{-(t/1700)^{2}}", num = "10.4",
            legend = c("t" = "czas misji w godzinach", "R(t)" = "prawdopodobieństwo, że wentylator pracuje nieprzerwanie do chwili t", "1500" = "MTTF modelu wykładniczego (h)", "1700" = "parametr skali Weibulla η (h)")),
          risk_example("10.5", "Ta sama średnia, inne ryzyko w misji",
            problem = "Oblicz MTTF Weibulla, a następnie R(1000) i R(3000) w obu modelach. Po ilu godzinach krzywe się przecinają?",
            steps = c(
              "MTTF Weibulla = η · Γ(1 + 1/β) = 1700 · Γ(1,5) ≈ 1700 · 0,886 ≈ 1507 h — prawie tyle samo co 1500 h w modelu wykładniczym.",
              "R_exp(1000) = e^(−0,667) ≈ 0,513; R_W(1000) = e^(−0,346) ≈ 0,707.",
              "R_exp(3000) = e^(−2) ≈ 0,135; R_W(3000) = e^(−3,114) ≈ 0,044.",
              "Krzywe przecinają się, gdy t/1500 = (t/1700)², czyli t = 1700² / 1500 ≈ 1927 h."
            ),
            answer = "Przed około 1927 h wentylator zużywający się jest bardziej niezawodny niż wykładniczy, a później mniej. Przy misji 1000 h model Weibulla daje R ≈ 0,707 zamiast 0,513, choć średnie czasy życia różnią się o 7 godzin."
          ),
          risk_try("porównaj oba modele przy czasie misji 1000 h, potem przesuń czas do 2000 h i 3000 h. Obserwuj liczbę przekazywaną do systemu."),
          risk_widget_panel("Karta 3", "Niezawodność wentylatora", tagList(selectInput("i10_life_model", "Model", c("Wykładniczy" = "exp", "Weibull — zużycie" = "weibull")), sliderInput("i10_time", "Czas misji (h)", 100, 3000, 1000, 50)), "i10_life_plot", "i10_life_stats"),
          c(
            "Przy 1000 h panel pokazuje 0,513 dla modelu wykładniczego i 0,707 dla Weibulla. Przy 3000 h kolejność się odwraca: 0,135 wobec 0,044. Krzywa Weibulla zaczyna płasko — hazard w chwili zero wynosi zero — i potem gwałtownie opada, a krzywa wykładnicza opada równomiernie od początku.",
            "Ta karta pokazuje, dlaczego sam MTTF nie wystarcza do analizy misji. Dwa modele o prawie tej samej średniej dają dla misji 1000 h niezawodności różniące się o 0,19, a wybór hipotezy o mechanizmie awarii przesunie końcowe P(TOP) prawie dwukrotnie. To jest niepewność modelu, a nie losowość — i musi trafić do notatki."
          ),
          risk_check("i10_chk_mttf",
            "Oba modele mają MTTF ≈ 1500 h. Dla której misji model Weibulla jest ostrożniejszy, czyli daje mniejsze R?",
            c("Dla misji 1000 h" = "short", "Dla misji 2500 h" = "long", "Dla żadnej, bo średnie są równe" = "none"),
            correct = "long",
            explanation = "Krzywe przecinają się około 1927 h. Po tym czasie zużycie sprawia, że Weibull daje mniejsze R — np. R_W(2500) ≈ 0,115 wobec R_exp(2500) ≈ 0,189.",
            hints = c(short = "Przy 1000 h Weibull daje 0,707, a wykładniczy 0,513.", none = "Równe średnie nie oznaczają równych krzywych — porównaj Przykład 10.5.")
          )
        )
      ),
      list(id = "odnowa", title = "Podział czasu nie odmładza", text = "Dla jednego urządzenia bez wymiany przetrwanie 3000 h to R(3000). Trzy misje nowych urządzeń po 1000 h dają R(1000)³; sześć po 500 h — R(500)⁶. To różne polityki odnowy. Przy kontynuacji pracy kolejne prawdopodobieństwa są warunkowe: R(2000)/R(1000), a nie ponownie R(1000).",
        body = list(
          risk_definition("10.3", "Polityka odnowy", c(
            "Polityka odnowy to zasada określająca stan elementu na początku każdej misji: nowy (wymiana lub naprawa do stanu „jak nowy”) albo używany, z historią dotychczasowej pracy. Rachunek dla kolejnych misji zależy od tej zasady, a nie tylko od łącznego czasu pracy."
          )),
          "Dla elementu używanego pytamy o przetrwanie kolejnych s godzin pod warunkiem, że przetrwał już t godzin. To zwykłe prawdopodobieństwo warunkowe z wykładu 02: zdarzenie „przetrwa t + s” zawiera się w „przetrwa t”, więc iloraz upraszcza się do ilorazu funkcji niezawodności.",
          risk_formula("R(t+s\\mid t)=\\frac{R(t+s)}{R(t)}", num = "10.5",
            legend = c("t" = "czas dotychczasowej pracy bez awarii", "s" = "długość kolejnej misji", "R(t+s\\mid t)" = "prawdopodobieństwo przetrwania kolejnych s godzin przez element używany")),
          risk_example("10.6", "Trzy polityki dla wentylatora Weibulla",
            problem = "Wentylator Weibulla (β = 2, η = 1700 h) ma pracować łącznie 3000 h. Porównaj: (a) jeden egzemplarz bez wymiany, (b) wymianę na nowy co 1000 h, (c) wymianę co 500 h. Ile wynosi szansa przetrwania drugiej misji 1000 h przez egzemplarz używany?",
            steps = c(
              "(a) R(3000) ≈ 0,044.",
              "(b) Każda misja zaczyna się od nowego egzemplarza: R(1000)³ ≈ 0,707³ ≈ 0,354.",
              "(c) R(500)⁶ ≈ 0,917⁶ ≈ 0,595.",
              "Egzemplarz używany w drugiej misji: ze wzoru (10.5) R(2000 | 1000) = R(2000)/R(1000) ≈ 0,251 / 0,707 ≈ 0,354. Równość z wynikiem (b) jest przypadkową cechą β = 2; ważne jest, że 0,354 to połowa wartości 0,707 dla nowego egzemplarza.",
              "Dla modelu wykładniczego wszystkie trzy polityki dają e^(−2) ≈ 0,135: brak pamięci sprawia, że wymiana nic nie zmienia."
            ),
            answer = "Przy zużyciu wymiana prewencyjna radykalnie poprawia przetrwanie łącznego czasu pracy (0,044 → 0,354 → 0,595). Przy stałym hazardzie ta sama wymiana jest wydatkiem bez efektu."
          ),
          "Studium Bananpolu przyjmuje w teczce, że każdy element zaczyna misję nowy. To właśnie polityka (b) i warunek, dzięki któremu możemy liczyć każdą misję osobno tym samym R(t). Jeśli w praktyce wentylatory pracują bez wymiany przez wiele misji, R dla kolejnej misji trzeba liczyć ze wzoru (10.5), a założenie z teczki staje się pozycją na liście niepewności.",
          risk_check("i10_chk_odnowa",
            "Wentylator wykładniczy przepracował już 2000 h bez awarii. Ile wynosi szansa, że przetrwa kolejne 1000 h?",
            c("R(1000) ≈ 0,513, tyle co nowy" = "same", "R(3000) ≈ 0,135" = "total", "Mniej niż dla nowego, bo jest zużyty" = "less"),
            correct = "same",
            explanation = "Ze wzoru (10.5): R(3000)/R(2000) = e^(−2)/e^(−4/3) = e^(−2/3) ≈ 0,513. Stały hazard oznacza brak pamięci — dotychczasowa praca nie zmienia przyszłości.",
            hints = c(total = "R(3000) to szansa przetrwania całych 3000 h od nowości; tu 2000 h już minęło bez awarii.", less = "To prawda dla Weibulla β > 1, ale nie dla modelu wykładniczego.")
          )
        )
      )
    )
  ),
  list(
    id = "system", title = "Model układu chłodzenia", hook = "Wszystko składa się w jedno drzewo",
    lead = "Zasilanie i sterownik są wymagane zawsze, wystarcza jeden z dwóch wentylatorów; inicjacja i niepowodzenie wymaganej ochrony tworzą wspólny scenariusz.",
    intro = "Wentylatory mają R(t) z karty czasu życia. Dla zasilania i sterownika zakładamy wykładniczy czas życia, więc R(t)=R(1000)^(t/1000). Wszystkie elementy liczymy dla wspólnego czasu misji. Zasilanie jest jawnym wspólnym zasobem obu wentylatorów; poza nim zakładamy niezależność elementów.",
    sections = list(
      list(
        id = "redukcja", title = "Redukcja układu",
        body = list(
          c(
            "Dane o zasilaniu i sterowniku podano dla 1000 h, a misja może trwać inaczej. Wykład 08 ostrzegał, że niezawodności dla różnych horyzontów nie wolno mnożyć. Przeliczenie wynika wprost z modelu wykładniczego: R(t) = e^(−λt) = (e^(−1000λ))^(t/1000), a e^(−1000λ) to właśnie podane R(1000)."
          ),
          risk_formula("R(t)=R(1000)^{t/1000}", num = "10.6",
            legend = c("R(1000)" = "niezawodność elementu na 1000 h (0,98 dla zasilania, 0,95 dla sterownika)", "t" = "czas misji w godzinach")),
          "Mając wszystkie R(t) dla tego samego czasu, redukujemy schemat blokowy tak jak w wykładzie 08. Zasilanie P i sterownik C są połączone szeregowo z resztą — każdy z nich jest konieczny. Wentylatory A i B tworzą układ równoległy — wystarcza jeden. Ponieważ zasilanie jest wspólne, jego R stoi przed nawiasem i mnoży całą gałąź chłodzenia.",
          risk_formula("R_{sys}(t)=R_P(t)R_C(t)[1-(1-R_A(t))(1-R_B(t))]", num = "10.7",
            legend = c("R_P, R_C" = "niezawodność zasilania i sterownika", "R_A, R_B" = "niezawodność wentylatorów z karty 3", "R_{sys}" = "niezawodność układu chłodzenia, czyli 1 − P(S | I)")),
          risk_example("10.7", "Redukcja układu dla misji 1000 h",
            problem = "Oblicz R_sys(1000) dla modelu wykładniczego wentylatorów. Porównaj z wariantem, w którym jest tylko jeden wentylator, i z modelem Weibulla.",
            steps = c(
              "Przy t = 1000 h wzór (10.6) daje R_P = 0,98 i R_C = 0,95 bez zmian.",
              "Gałąź równoległa: 1 − (1 − 0,513)² = 1 − 0,487² ≈ 0,763.",
              "Ze wzoru (10.7): R_sys = 0,98 · 0,95 · 0,763 ≈ 0,711, więc P(S | I) ≈ 0,289.",
              "Z jednym wentylatorem: 0,98 · 0,95 · 0,513 ≈ 0,478 — redundancja podnosi R_sys o ponad 0,23.",
              "Dla Weibulla: gałąź równoległa 1 − 0,293² ≈ 0,914, R_sys ≈ 0,851."
            ),
            answer = "R_sys ≈ 0,711 (wykładniczy) lub 0,851 (Weibull). Najsłabszym ogniwem jest gałąź wentylatorów, a nie sterownik, choć pojedynczy wentylator jest dużo mniej niezawodny niż sterownik."
          ),
          risk_try("przy czasie misji 1000 h zmniejsz R sterownika do 0,85 i obserwuj R systemu. Wróć do 0,95 i zmień czas misji na karcie 3 na 2000 h — patrz, jak zmieniają się wszystkie cztery składniki naraz."),
          figure_panel(label = "Redukcja", title = "Elementy w tej samej misji", sliderInput("i10_power", "R zasilania na 1000 h", .7, 1, bananpol$integration$power_r1000, .01), sliderInput("i10_controller", "R sterownika na 1000 h", .7, 1, bananpol$integration$controller_r1000, .01), uiOutput("i10_system_result"), full_width = TRUE),
          c(
            "Przy misji 2000 h panel pokazuje R zasilania 0,960 (= 0,98²), sterownika 0,902 (= 0,95²), gałęzi równoległej 0,458 i systemu 0,397. Wydłużenie misji uderza we wszystkie elementy naraz — dlatego etykieta czasu musi być wspólna. Gdyby ktoś przeliczył tylko wentylatory, a zasilanie i sterownik zostawił „na 1000 h”, zawyżyłby R_sys o około 7% (0,426 zamiast 0,397).",
            "Zmiana R sterownika przenosi się na R_sys proporcjonalnie, bo sterownik jest elementem szeregowym. Poprawa wentylatorów działa słabiej, niż sugerowałaby ich niska niezawodność, bo redundancja już część ryzyka wchłania. Tę asymetrię wykorzystamy przy wyborze interwencji."
          ),
          risk_check("i10_chk_system",
            "Gdzie we wzorze (10.7) widać, że zasilanie jest wspólną przyczyną dla obu wentylatorów?",
            c("R_P stoi przed nawiasem i mnoży całą gałąź równoległą" = "outside", "R_P występuje w każdym czynniku nawiasu" = "inside", "Nigdzie, zasilanie pominięto" = "missing"),
            correct = "outside",
            explanation = "Awaria zasilania wyłącza oba wentylatory naraz, więc zasilanie jest szeregowe względem całej gałęzi. Gdyby każdy wentylator miał własne zasilanie, R_P weszłoby do nawiasu — to właśnie zmienia interwencja „niezależne zasilanie”.",
            hints = c(inside = "Wtedy każdy wentylator miałby osobne zasilanie. Czy tak jest w Bananpolu?", missing = "Przeczytaj legendę wzoru (10.7).")
          )
        )
      ),
      list(
        id = "fta", title = "Końcowe FTA",
        text = "TOP wystąpi, gdy jest zapotrzebowanie i zabraknie detekcji lub ciągłego chłodzenia. Iloczyn P(I) i prawdopodobieństwa warunkowego nie wymaga niezależności od I. Dopełnienie iloczynu wewnątrz nawiasu wymaga natomiast niezależności D i S przy ustalonym I. Zależność detektora od wspólnego zasilania wymagałaby przebudowy drzewa.",
        body = list(
          "Mamy teraz wszystkie trzy liście drzewa: P(I) z teczki, P(D | I) z karty detekcji i P(S | I) = 1 − R_sys z redukcji układu. Drzewo z wykładu 09 ma bramkę OR nad D i S oraz bramkę AND łączącą ją z I. Wzór końcowy jest przekładem kontraktu (10.1) na prawdopodobieństwa.",
          risk_formula("P(TOP)=P(I)\\,[1-(1-P(D\\mid I))(1-P(S\\mid I))]", num = "10.8",
            legend = c("P(I)" = "prawdopodobieństwo zapotrzebowania w misji", "P(D\\mid I)" = "1 − czułość", "P(S\\mid I)" = "1 − R_sys(t) ze wzoru (10.7)")),
          risk_derivation("wzór (10.8)", c(
            "Z definicji prawdopodobieństwa warunkowego: P(I ∩ (D ∪ S)) = P(I) · P(D ∪ S | I). Ten krok nie wymaga żadnej niezależności.",
            "Zdarzenie przeciwne do D ∪ S to Dᶜ ∩ Sᶜ (prawo de Morgana). Jeśli D i S są niezależne warunkowo przy I, to P(Dᶜ ∩ Sᶜ | I) = (1 − P(D | I)) · (1 − P(S | I)). Stąd nawias we wzorze (10.8)."
          ), lines = c("P(TOP) = P(I) · P(D ∪ S | I)", "       = P(I) · [1 − P(Dᶜ ∩ Sᶜ | I)]", "       = P(I) · [1 − (1 − P(D | I)) · (1 − P(S | I))]")),
          risk_example("10.8", "P(TOP) dla misji bazowej",
            problem = "Oblicz P(TOP) dla misji 1000 h w obu modelach wentylatora i przedstaw wynik jako częstość naturalną. Sprawdź, ile wyniósłby rachunek przybliżony P(I) · [P(D | I) + P(S | I)].",
            steps = c(
              "Model wykładniczy: nawias = 1 − 0,95 · 0,711 ≈ 1 − 0,675 = 0,325.",
              "P(TOP) = 0,005 · 0,325 ≈ 0,00162, czyli około 16 na 10 000 misji.",
              "Weibull: nawias = 1 − 0,95 · 0,851 ≈ 0,191; P(TOP) ≈ 0,00096, około 10 na 10 000 misji.",
              "Przybliżenie sumą: 0,05 + 0,289 = 0,339 zamiast 0,325. Przybliżenie rzadkich zdarzeń zawodzi, bo P(S | I) nie jest małe."
            ),
            answer = "P(TOP) ≈ 0,0016 (wykładniczy) lub ≈ 0,0010 (Weibull). Wybór hipotezy o czasie życia zmienia wynik o około 40%."
          ),
          risk_try("najpierw odczytaj wysokości czterech słupków dla ustawień bazowych. Potem wróć do karty 3 i przełącz model na Weibulla, a na karcie 1 ustaw czułość 1,00. Sprawdź, który słupek reaguje."),
          risk_widget_panel("Integracja", "Utrata ochrony termicznej w misji", tags$p("Parametry zmieniasz na kartach detekcji, czasu życia i systemu."), "i10_fta_plot", "i10_fta_stats"),
          c(
            "W wersji bazowej słupek P(S | I) ≈ 0,289 dominuje nad P(D | I) = 0,050, a słupek P(TOP) jest ledwo widoczny, bo wszystko mnoży się przez P(I) = 0,005. Przy czułości 1,00 przeoczenia znikają, ale P(TOP) spada tylko do około 0,00145 — o jedną dziesiątą. Przełączenie na Weibulla obniża słupek S i P(TOP) bardziej niż idealny detektor.",
            "Ta proporcja jest najważniejszym wynikiem diagnostycznym studium: w Bananpolu ryzyko utraty ochrony tworzy przede wszystkim układ chłodzenia i częstość zapotrzebowania, a nie detekcja. Działania, które nie dotykają tych dwóch składników, mają małą dźwignię."
          ),
          risk_check("i10_chk_fta",
            "Detektor ma zasilanie wspólne z wentylatorami. Który krok wyprowadzenia (10.8) przestaje być prawdziwy?",
            c("P(I ∩ X) = P(I) · P(X | I)" = "cond", "Rozbicie P(Dᶜ ∩ Sᶜ | I) na iloczyn" = "indep", "Prawo de Morgana" = "morgan"),
            correct = "indep",
            explanation = "Awaria zasilania powoduje wtedy jednocześnie D i S, więc zdarzenia nie są warunkowo niezależne. Iloczyn (1 − P(D | I))(1 − P(S | I)) nie opisuje już łącznej sprawności obu funkcji — drzewo trzeba przebudować, wyodrębniając zasilanie jako wspólne zdarzenie podstawowe.",
            hints = c(cond = "To definicja prawdopodobieństwa warunkowego — nie wymaga żadnych założeń.", morgan = "Prawo de Morgana jest tożsamością zbiorów, zawsze prawdziwą.")
          )
        ),
        pitfall = "P(TOP) jest prawdopodobieństwem utraty wymaganej ochrony. Do prawdopodobieństwa szkody materialnej lub urazu potrzebny jest dalszy model skutków."
      ),
      list(
        id = "rok", title = "Horyzont roczny",
        body = list(
          c(
            "Zarząd myśli w latach, nie w misjach. Bananpol wykonuje trzy misje po 1000 h rocznie. Jeśli przed każdą misją elementy są odnawiane (Definicja 10.3), a misje nie wpływają na siebie nawzajem, roczny wynik to schemat Bernoulliego z wykładu 04: trzy niezależne próby, każda z prawdopodobieństwem P(TOP). Pytamy o co najmniej jedno zdarzenie w roku.",
            "Kusi też inny rachunek: skoro układ ma R_sys na misję, to w roku ma R_sys³, więc „roczne ryzyko” wynosi 1 − R_sys³. Ta liczba istnieje, ale odpowiada na inne pytanie — i jest pułapką, przed którą ostrzega pierwsze pytanie quizu."
          ),
          risk_formula("P_{rok}=1-\\bigl(1-P(TOP)\\bigr)^{3}\\approx 3\\,P(TOP)", num = "10.9",
            legend = c("P(TOP)" = "prawdopodobieństwo utraty ochrony w jednej misji ze wzoru (10.8)", "3" = "liczba misji w roku, przy odnowie przed każdą z nich", "P_{rok}" = "prawdopodobieństwo co najmniej jednej utraty ochrony w roku")),
          risk_example("10.9", "Rok z trzema misjami",
            problem = "Dla modelu wykładniczego i misji 1000 h oblicz: (a) roczne prawdopodobieństwo co najmniej jednej utraty ochrony; (b) wielkość 1 − R_sys³; (c) prawdopodobieństwo co najmniej jednego zapotrzebowania w roku. Zinterpretuj różnicę między (a) i (b).",
            steps = c(
              "(a) Ze wzoru (10.9): 1 − (1 − 0,00162)³ ≈ 0,0049; przybliżenie 3 · 0,00162 ≈ 0,0049 jest tu praktycznie dokładne. Dla Weibulla ≈ 0,0029.",
              "(b) 1 − 0,711³ ≈ 1 − 0,359 = 0,641; dla Weibulla ≈ 0,383.",
              "(c) 1 − 0,995³ ≈ 0,015.",
              "Liczba (b) to szansa, że układ zawiódłby w co najmniej jednej z trzech misji, gdyby w każdej musiał pracować pod wymaganym obciążeniem. Tymczasem jest potrzebny tylko w misjach z zapotrzebowaniem, a rok z co najmniej jednym zapotrzebowaniem zdarza się średnio raz na około 67 lat (c)."
            ),
            answer = "Roczne ryzyko utraty ochrony wynosi około 0,005, a nie 0,64. Wielkość 1 − R_sys³ opisuje niezawodność układu — przydaje się np. do planowania części zamiennych — ale nie jest ryzykiem zdarzenia TOP, bo pomija warunek I."
          ),
          "Wzór (10.9) opiera się na dwóch założeniach, które trzeba wpisać do notatki. Pierwsze to odnowa przed każdą misją; bez niej prawdopodobieństwa w kolejnych misjach byłyby warunkowe, jak we wzorze (10.5), i dla wentylatora zużywającego się rosłyby z misji na misję. Drugie to niezależność misji: jeśli jedna przyczyna, np. wadliwa partia wentylatorów, dotyka wszystkich trzech misji naraz, mnożenie prawdopodobieństw zaniża ryzyko roczne.",
          risk_check("i10_chk_rok",
            "Kierownik pisze w raporcie: „roczne ryzyko awarii chłodzenia wynosi 64%, więc ochrona termiczna jest nieakceptowalna”. Co jest błędne?",
            c("Liczba 0,64 pomija warunek I: chłodzenie jest potrzebne tylko w misjach z zapotrzebowaniem" = "missing_i", "Liczba jest poprawna, ale wniosek zbyt ostry" = "tone", "Należało policzyć R_sys³ zamiast 1 − R_sys³" = "complement"),
            correct = "missing_i",
            explanation = "1 − R_sys³ dotyczy pracy przez wszystkie trzy misje bez warunku zapotrzebowania. Ryzyko utraty ochrony w roku wynosi ze wzoru (10.9) około 0,005.",
            hints = c(tone = "Sprawdź, jakie zdarzenie opisuje 1 − R_sys³ — czy jest w nim I?", complement = "R_sys³ to szansa przetrwania wszystkich trzech misji; nie o to chodzi w pytaniu o ryzyko.")
          )
        ),
        pitfall = "Roczny horyzont wymaga jawnej polityki odnowy i niezależności misji. Potęgowanie R_sys bez warunku I zamienia pytanie o ryzyko w pytanie o niezawodność sprzętu."
      )
    )
  ),
  list(
    id = "interwencje", title = "Odporność rekomendacji", hook = "Dobra rekomendacja przetrwa zmianę założeń",
    lead = "Porównujemy efekt, budżet i wykonalność przy tej samej misji, a każdą interwencję przeliczamy w każdym scenariuszu.",
    intro = "Lepszy czujnik zmniejsza przeoczenia, ograniczenie źródła ciepła zmniejsza częstość zapotrzebowania, niezależne zasilanie dodaje drugą gałąź zasilania, a dodatkowy wentylator trzecią gałąź chłodzenia. Bazowa redukcja przeoczeń lub inicjacji o 50% jest fikcyjną hipotezą skuteczności działania, wymagającą danych z pilotażu. Nie wynika z samego częstszego przeglądu.",
    sections = list(
      list(
        id = "opcje", title = "Cztery interwencje",
        body = list(
          c(
            "Każda interwencja zmienia dokładnie jeden liść lub jeden element układu, więc jej efekt policzymy tym samym wzorem (10.8), podstawiając zmieniony składnik. Dwie pierwsze działają przez założoną skuteczność e — bazowo 0,5. Dwie pozostałe zmieniają strukturę układu: niezależne zasilanie zamienia element szeregowy P na dwa równoległe, a dodatkowy wentylator dokłada trzeci element do gałęzi równoległej."
          ),
          risk_formula("P(D\\mid I)'=(1-e)P(D\\mid I),\\quad P(I)'=(1-e)P(I),\\quad R_P'=1-(1-R_P)^{2},\\quad R_{AB}'=1-(1-R_A)^{3}", num = "10.10",
            legend = c("e" = "założona skuteczność działania (bazowo 0,5)", "R_P'" = "niezawodność zdublowanego, niezależnego zasilania", "R_{AB}'" = "niezawodność gałęzi z trzema wentylatorami", "'" = "wielkość po interwencji")),
          risk_definition("10.4", "Ryzyko po działaniu", c(
            "Ryzyko po działaniu (ryzyko rezydualne) to prawdopodobieństwo zdarzenia TOP policzone dla systemu po wdrożeniu interwencji, przy tych samych definicjach, jednostce i horyzoncie co ryzyko bazowe. Dopiero ono jest porównywane z kryterium akceptowalności; sama redukcja względem stanu bazowego nie wystarcza."
          )),
          risk_example("10.10", "Cztery interwencje dla misji bazowej",
            problem = "Dla modelu wykładniczego i misji 1000 h oblicz P(TOP) po każdej interwencji (bazowo 0,00162) oraz redukcję ryzyka na jednostkę kosztu. Koszty umowne: czujnik 2, ograniczenie źródła ciepła 1, zasilanie 4, wentylator 3.",
            steps = c(
              "Lepszy czujnik: P(D | I) = 0,025; nawias = 1 − 0,975 · 0,711 ≈ 0,307; P(TOP) ≈ 0,00154. Redukcja ≈ 0,00009, czyli około 5%.",
              "Ograniczenie źródła ciepła: P(I) = 0,0025; P(TOP) = 0,0025 · 0,325 ≈ 0,00081. Redukcja o połowę, bo P(I) mnoży cały wzór (10.8).",
              "Niezależne zasilanie: R_P = 1 − 0,02² = 0,9996; R_sys ≈ 0,9996 · 0,95 · 0,763 ≈ 0,725; P(TOP) ≈ 0,00156. Redukcja około 4%.",
              "Dodatkowy wentylator: gałąź = 1 − 0,487³ ≈ 0,885; R_sys ≈ 0,98 · 0,95 · 0,885 ≈ 0,824; P(TOP) ≈ 0,00109. Redukcja około 33%.",
              "Redukcja na jednostkę kosztu: ograniczenie źródła ciepła ≈ 0,00081, wentylator ≈ 0,00018, czujnik ≈ 0,00004, zasilanie ≈ 0,00002."
            ),
            answer = "Najwięcej daje ograniczenie źródła ciepła, które jest też najtańsze. Czujnik i zasilanie poprawiają elementy, które i tak są mocne (P(D | I) = 0,05, R_P = 0,98), więc mają małą dźwignię."
          ),
          risk_try("przejrzyj kolejno cztery interwencje przy budżecie 2, a potem zwiększ budżet do 4. Porównaj długości słupków z wynikami Przykładu 10.10 i zwróć uwagę, które opcje zmieniają kolor."),
          risk_widget_panel("Opcje", "Ryzyko po działaniu", tagList(selectInput("i10_intervention", "Interwencja", setNames(bananpol$interventions$id, bananpol$interventions$label)), sliderInput("i10_budget", "Budżet w jednostkach demonstracyjnych", 1, 4, 2, 1)), "i10_interventions_plot", "i10_intervention_stats"),
          c(
            "Wykres porządkuje interwencje od najlepszej: ograniczenie źródła ciepła (0,00081) i dodatkowy wentylator (0,00109) wyraźnie odstają, a lepszy czujnik (0,00154) i niezależne zasilanie (0,00156) mają słupki bliskie sobie i bliskie stanowi bazowemu 0,00162. Przy budżecie 2 w zasięgu są tylko czujnik i ograniczenie źródła ciepła; przy budżecie 4 wszystkie opcje zmieniają kolor, ale ranking pozostaje ten sam.",
            "Wynik ma mocne uzasadnienie strukturalne, a słabe empiryczne. Strukturalne, bo P(I) jest czynnikiem całego wzoru (10.8). Empiryczne — słabe, bo skuteczność 50% jest hipotezą z teczki. Właśnie dlatego kolejna sekcja sprawdza, czy ranking przetrwa, gdy tę skuteczność osłabimy."
          ),
          risk_check("i10_chk_czujnik",
            "Dlaczego czujnik, który zmniejsza przeoczenia o połowę, obniża P(TOP) tylko o około 5%?",
            c("Bo przeoczenia (0,05) to mały składnik sumy D ∪ S przy P(S | I) ≈ 0,29" = "small", "Bo czujnik działa tylko w połowie misji" = "half", "Bo FPR nadal wynosi 0,05" = "fpr"),
            correct = "small",
            explanation = "Nawias we wzorze (10.8) spada z 0,325 do 0,307, bo większość ryzyka niesie S. Połowienie małego składnika daje mały efekt.",
            hints = c(half = "Model nie ogranicza działania czujnika do części misji.", fpr = "FPR nie wchodzi do wzoru (10.8) — patrz karta 1.")
          )
        ),
        decision = "Budżet ogranicza zbiór opcji; pozostałe ryzyko porównaj z jawnym kryterium. Koszty są umownymi jednostkami, nie cenami rynkowymi."
      ),
      list(
        id = "scenariusze", title = "Odporność rekomendacji",
        text = "Mnożnik m=1±u skaluje P(I), prawdopodobieństwo przeoczenia oraz skumulowane hazardy elementów; prawdopodobieństwa ograniczamy do 1. Skuteczność czujnika i ograniczenia źródła ciepła wynosi 0,5(2−m): w ostrożnym scenariuszu jest niższa. Te same założenia stosujemy do wszystkich opcji przed ich porównaniem. Redundancja zakłada niezależność dodanej gałęzi także w scenariuszach.",
        body = list(
          "Skalowanie skumulowanego hazardu ma prostą postać. Dla każdego elementu R(t) = e^(−H(t)), gdzie H(t) to skumulowany hazard z wykładu 07. Zwiększenie H o czynnik m daje e^(−mH(t)) = R(t)^m. Dlatego scenariusz ostrożny nie „odejmuje” stałej od niezawodności, tylko podnosi ją do potęgi większej od 1 — tak samo dla każdego elementu i w każdej opcji.",
          risk_formula("m\\in\\{1-u,\\;1,\\;1+u\\},\\quad P(I)_m=\\min(1,\\,m\\,P(I)),\\quad R_m(t)=R(t)^{m},\\quad e_m=0{,}5\\,(2-m)", num = "10.11",
            legend = c("u" = "zakres niepewności ustawiany suwakiem", "m" = "mnożnik scenariusza: optymistyczny, bazowy, ostrożny", "e_m" = "skuteczność czujnika i ograniczenia źródła ciepła w scenariuszu")),
          risk_definition("10.5", "Odporność rekomendacji", c(
            "Rekomendacja jest odporna w badanym zakresie, jeśli to samo działanie jest najlepsze wśród dopuszczalnych opcji i spełnia kryterium w każdym rozpatrzonym scenariuszu. Odporność dotyczy zawsze konkretnego zestawu scenariuszy; nie jest gwarancją wobec założeń, których nie zmieniano."
          )),
          risk_example("10.11", "Czy ranking się odwraca?",
            problem = c(
              "(a) Dla modelu wykładniczego, u = 0,2 i budżetu 2 policz P(TOP) po obu dopuszczalnych działaniach w trzech scenariuszach.",
              "(b) Dla modelu Weibulla, u = 0,5 i budżetu 3 porównaj ograniczenie źródła ciepła z dodatkowym wentylatorem w scenariuszu ostrożnym."
            ),
            steps = c(
              "(a) Lepszy czujnik: 0,00092 / 0,00154 / 0,00230 (m = 0,8 / 1 / 1,2). Ograniczenie źródła ciepła: 0,00040 / 0,00081 / 0,00144. Ograniczenie źródła ciepła wygrywa we wszystkich trzech scenariuszach.",
              "(b) Przy m = 1,5 skuteczność e = 0,5 · (2 − 1,5) = 0,25, więc ograniczenie źródła ciepła obniża P(I) = 0,0075 tylko do 0,0056; wynik ≈ 0,00172.",
              "Dodatkowy wentylator nie ma parametru skuteczności: R wentylatora = 0,707^1,5 ≈ 0,595, gałąź z trzema ≈ 0,934; wynik ≈ 0,00168.",
              "W scenariuszu ostrożnym wygrywa więc wentylator, choć w pozostałych dwóch wygrywa ograniczenie źródła ciepła."
            ),
            answer = "W (a) rekomendacja jest odporna. W (b) ranking odwraca się przy największej niepewności, bo scenariusz osłabia tylko działania z założoną skutecznością. To nie jest błąd rachunku, lecz informacja: wynik zależy od hipotezy o skuteczności, więc pilotaż ograniczenia źródła ciepła jest warunkiem rekomendacji."
          ),
          risk_try("przy ustawieniach bazowych przesuń u od 0 do 0,5 i śledź tekst pod wykresem. Potem przełącz model na Weibulla na karcie 3, ustaw budżet 3 i powtórz."),
          risk_widget_panel("Niepewność", "Opcje w trzech scenariuszach", sliderInput("i10_uncertainty", "Zakres u", 0, .5, .2, .05), "i10_scenarios", "i10_scenarios_stats"),
          c(
            "Dla modelu wykładniczego i budżetu 2 tekst pod wykresem wskazuje ograniczenie źródła ciepła we wszystkich scenariuszach przy każdym u. Dla Weibulla przy budżecie 3 ta sama opcja wygrywa aż do u = 0,45; dopiero przy u = 0,5 w scenariuszu ostrożnym na pierwsze miejsce wychodzi dodatkowy wentylator. Słupki pokazują też, że rozrzut między scenariuszami jest dla każdej opcji większy niż różnice między opcjami w jednym scenariuszu.",
            "Ta ostatnia obserwacja jest kluczowa dla notatki: liczby „0,00081” nie wolno podawać jak pomiaru. Uczciwa rekomendacja mówi, w jakim zakresie niepewności jest najlepsza i przy jakim założeniu przestaje nią być."
          ),
          risk_check("i10_chk_scen",
            "Opcja X jest najlepsza w scenariuszu bazowym, ale nie w ostrożnym. Co należy napisać w notatce?",
            c("Rekomendujemy X bez zastrzeżeń, bo scenariusz bazowy jest najbardziej prawdopodobny" = "base", "Rekomendacja X zależy od założenia, które osłabia scenariusz ostrożny; wskazujemy, jak je sprawdzić" = "conditional", "Pomijamy scenariusz ostrożny jako skrajny" = "drop"),
            correct = "conditional",
            explanation = "Scenariusze nie mają przypisanych prawdopodobieństw, więc nie wolno uznać bazowego za rozstrzygający. Zmiana rankingu wskazuje założenie, które trzeba zweryfikować przed wdrożeniem.",
            hints = c(base = "Czy scenariuszom przypisano prawdopodobieństwa?", drop = "Pominięcie niewygodnego scenariusza to dokładnie to, przed czym ostrzega pytanie 5 quizu.")
          )
        ),
        takeaway = "Scenariusze są jawnym eksperymentem na założeniach, a nie przedziałem ufności ani dowodem odporności na wszystkie możliwe błędy modelu."
      )
    )
  ),
  list(
    id = "notatka", title = "Notatka decyzyjna", hook = "Mniej ryzyka to jeszcze nie dość",
    lead = "Mniejsze prawdopodobieństwo nie musi spełniać przyjętego kryterium; rachunek oddajemy wraz z założeniami, skutkami i planem sprawdzenia działania.",
    intro = "Wybierz działanie i demonstracyjny limit dla utraty ochrony w jednej misji. Notatka sprawdza budżet i najgorszy z rozpatrywanych scenariuszy. Limit służy wyłącznie ćwiczeniu, nie jest normą bezpieczeństwa. Uzgodnienie rzeczywistego kryterium wymaga także oceny skutków i narażenia.",
    sections = list(
      list(
        id = "memo", title = "Rekomendacja i ryzyko po działaniu",
        body = list(
          c(
            "Cały dotychczasowy rachunek istnieje po to, żeby ktoś mógł podjąć decyzję. Decydent nie przeczyta kart obliczeniowych; przeczyta jedną stronę. Ta strona musi jednak dać się obronić przed recenzentem, który zna wszystkie karty. Dlatego notatka ma stałą strukturę, w której każda liczba ma swoje miejsce i swoje źródło."
          ),
          risk_definition("10.6", "Notatka decyzyjna", c(
            "Notatka decyzyjna to krótki dokument zawierający: (1) wynik bazowy z jednostką i częstością naturalną, (2) rekomendowane działanie z kosztem i ryzykiem po działaniu w każdym scenariuszu, (3) jawne kryterium i rozstrzygnięcie, czy jest spełnione, (4) kluczowe założenia i granice modelu, (5) właściciela działania i termin kontroli jego skuteczności."
          )),
          "Najważniejsze rozróżnienie dotyczy punktów (2) i (3). Ranking mówi, które działanie jest najlepsze wśród dostępnych. Kryterium mówi, czy najlepsze jest wystarczająco dobre. Te dwa sprawdzenia są niezależne: można mieć jasnego zwycięzcę rankingu, który nie spełnia kryterium, i wtedy notatka musi to powiedzieć wprost.",
          risk_try("zostaw domyślne działanie „Lepszy czujnik” i limit 0,002, przeczytaj notatkę, a potem wybierz „Ograniczenie źródła ciepła”. Na koniec obniż limit do 0,001 i sprawdź, co zmieni się w punkcie trzecim."),
          figure_panel(label = "Notatka", title = "Wynik, działanie, kryterium i kontrola", selectInput("i10_recommend", "Rozważane działanie", setNames(bananpol$interventions$id, bananpol$interventions$label)), sliderInput("i10_target", "Demonstracyjny limit P(TOP) na misję", .0001, .01, .002, .0001), uiOutput("i10_memo"), full_width = TRUE),
          c(
            "Przy ustawieniach bazowych (model wykładniczy, 1000 h, budżet 2, u = 0,2) lepszy czujnik jest w budżecie, ale w najgorszym scenariuszu daje 0,00230 i przekracza limit 0,002. Ograniczenie źródła ciepła daje w najgorszym scenariuszu 0,00144 i limit spełnia. Niezależne zasilanie notatka oznacza jako będące poza budżetem.",
            "Po obniżeniu limitu do 0,001 ograniczenie źródła ciepła nadal jest najlepsze i bazowo spełnia limit (0,00081), ale w scenariuszu ostrożnym już nie (0,00144). Notatka pisze wtedy „nie jest spełniony” — i to jest poprawny wynik, a nie usterka. Zgodnie z decyzją poniżej wracamy do zakresu misji, budżetu lub projektu barier, zamiast łagodzić kryterium."
          ),
          risk_example("10.12", "Wzorcowa notatka decyzyjna",
            problem = c(
              "Napisz pełną notatkę decyzyjną dla zarządu Bananpolu przy ustawieniach bazowych: model wykładniczy wentylatorów, misja 1000 h, budżet 2 jednostki, zakres niepewności u = 0,2, demonstracyjny limit P(TOP) = 0,002 na misję.",
              "Rozwiązanie jest wzorem formy: tak powinna wyglądać notatka oddawana po każdym studium przypadku."
            ),
            steps = c(
              "Wynik. Prawdopodobieństwo utraty wymaganej ochrony termicznej w jednej misji 1000 h wynosi bazowo około 0,0016, czyli około 16 na 10 000 porównywalnych misji; przy trzech misjach rocznie i odnowie przed każdą z nich — około 0,005 rocznie. Ryzyko tworzy głównie układ chłodzenia (P(S | I) ≈ 0,29) i częstość zapotrzebowania (0,005); przeoczenia detektora (0,05) mają mały udział.",
              "Rekomendacja. Rekomendujemy ograniczenie źródła ciepła (koszt 1 jednostka, w budżecie). Ryzyko po działaniu wynosi około 0,0004 w scenariuszu optymistycznym, 0,0008 w bazowym i 0,0014 w ostrożnym. Działanie jest najlepsze wśród opcji dopuszczalnych w budżecie we wszystkich trzech scenariuszach. Lepszy czujnik (koszt 2) daje bazowo tylko około 5% redukcji.",
              "Kryterium. Demonstracyjny limit 0,002 na misję jest spełniony we wszystkich rozpatrzonych scenariuszach; najgorszy wynik (0,0014) leży o około 30% poniżej limitu. Limit ma charakter ćwiczeniowy i nie zastępuje uzgodnionego kryterium akceptowalności.",
              "Założenia i granice. Brak napraw w misji; elementy nowe na początku misji; D i S niezależne przy I (detektor ma osobne zasilanie); skuteczność działania 50% jest hipotezą bez danych z pilotażu. Przy bardzo dużej niepewności (u = 0,5) i modelu Weibulla ranking może się odwrócić na korzyść dodatkowego wentylatora. Model nie obejmuje skutków utraty ochrony ani zapotrzebowań pojawiających się w trakcie misji.",
              "Właściciel i kontrola. Właściciel: kierownik utrzymania. Kontrola: pomiar częstości zapotrzebowania po pierwszych misjach po wdrożeniu; jeżeli spadek jest mniejszy niż 25%, notatka wraca do przeglądu z wariantem dodatkowego wentylatora. Przegląd także po każdej zmianie instalacji."
            ),
            answer = "Notatka zawiera pięć elementów z Definicji 10.6, a każda liczba w niej pochodzi z jednego z wzorów (10.8)–(10.11). Czytelnik może się nie zgodzić z założeniami, ale wie dokładnie, z którymi — i to jest cel notatki."
          ),
          risk_check("i10_chk_memo",
            "Przy limicie 0,001 ograniczenie źródła ciepła daje bazowo 0,00081, a w scenariuszu ostrożnym 0,00144. Co wpisać w punkcie „kryterium”?",
            c("Kryterium spełnione, bo wynik bazowy jest poniżej limitu" = "base", "Kryterium niespełnione w scenariuszu ostrożnym; potrzebna inna opcja, zakres lub projekt" = "fail", "Podnieść limit do 0,0015, żeby rekomendacja przeszła" = "move"),
            correct = "fail",
            explanation = "Notatka sprawdza najgorszy z rozpatrzonych scenariuszy. Zmiana kryterium po to, by zatwierdzić wynik, odwraca logikę decyzji.",
            hints = c(base = "Który scenariusz sprawdza notatka?", move = "Przeczytaj decyzję pod panelem notatki.")
          )
        ),
        decision = "Jeżeli żadna dopuszczalna opcja nie spełnia kryterium, wróć do zakresu misji, budżetu lub projektu barier. Nie zmieniaj kryterium tylko po to, by zatwierdzić wynik."
      ),
      list(
        id = "podsumowanie", title = "Podsumowanie",
        text = c(
          "Studium Bananpolu przeszło całą drogę od teczki do decyzji. Zaczęło się od kontraktu zdarzeń (10.1), który ustalił jednostkę — jedną misję — i rozdzielił prawdopodobieństwa bezwarunkowe od warunkowych. Trzy karty obliczeniowe dostarczyły liczb cząstkowych: Bayes (10.2) wyjaśnił, dlaczego do drzewa trafia 1 − czułość, a nie wiarygodność alarmu; granica przy zerze zdarzeń (10.3) pokazała, jak wnioskować o nieznanym p; funkcje czasu życia (10.4) i warunkowe przetrwanie (10.5) ujawniły, że sama średnia i sam podział czasu nie wystarczą bez polityki odnowy.",
          "Redukcja układu (10.6)–(10.7) sprowadziła wszystkie elementy do wspólnego czasu misji, a końcowe drzewo (10.8) połączyło je w P(TOP) ≈ 0,0016 na misję. Horyzont roczny (10.9) wymagał jawnej odnowy i niezależności misji i dał około 0,005 rocznie — a nie 0,64, które powstaje po zgubieniu warunku I. Interwencje (10.10) i scenariusze (10.11) pokazały, że najtańsze działanie może być najskuteczniejsze, jeśli zmienia czynnik mnożący cały wzór, i że odporność rekomendacji trzeba sprawdzać wobec konkretnych, nazwanych założeń.",
          "Wspólna lekcja całego kursu jest prosta: liczba jest tak wiarygodna jak zdanie, które ją opisuje. Każde przejście w tym wykładzie — od alarmu do przeoczenia, od R(1000) do R(t), od misji do roku, od rankingu do kryterium — wymagało nazwania warunku. Notatka decyzyjna jest miejscem, w którym te warunki zostają zapisane dla kogoś, kto nie widział rachunku."
        )
      ),
      list(
        id = "obrona", title = "Ściąga i obrona rekomendacji",
        text = "Zespół objaśnia każdy liść i każdą bramkę. Recenzent pyta o brakujący scenariusz, błąd człowieka, zależność i źródło skuteczności działania. Na koniec trzeba nazwać ryzyko pozostałe po interwencji oraz to, czego model nie obejmuje.",
        bullets = c("Definicje i warunki poprzedzają rachunek", "Model czasu życia zasila system dla tego samego t", "Nie utożsamiamy posterioru z przeoczeniem ani awaryjności z niedostępnością", "Porównujemy opcje w każdym scenariuszu i w budżecie", "Notatka zawiera kryterium, ryzyko po działaniu, właściciela i termin przeglądu"),
        widget = risk_assessment_ui("i10", integracja_quiz, integracja_exercises)
      )
    )
  )
))
integracja_chapters <- risk_block_chapters(integracja_block)

integracja_server <- function(input, output, session) {
  vote <- reactiveVal(FALSE)
  observeEvent(input$i10_vote_check, vote(TRUE))
  output$i10_vote_feedback <- renderUI({
    req(vote())
    lc_feedback(type = if (identical(input$i10_vote, "audit")) "ok" else "warning", "Najpierw definicje, czas, warunki i źródła danych.")
  })
  output$i10_dossier <- renderUI(lc_feedback(type = "info", paste(length(input$i10_fields), "z 4 pól oznaczono jako sprawdzone. Samo zaznaczenie nie zastępuje uzasadnienia.")))
  output$i10_model <- renderUI({
    models <- c(conditional = "Warunkowe i całkowite", bayes = "Bayes", binomial = "Dwumianowy", geometric = "Geometryczny", negative = "Ujemny dwumianowy", threshold = "Rozkład ciągły i ogon", survival = "Funkcje czasu życia", system = "Funkcja struktury i FTA")
    lc_feedback(type = "info", models[[input$i10_question]])
  })
  output$i10_alarm_result <- renderUI({
    p <- risk_bayes(input$i10_init, input$i10_sens, input$i10_fpr)
    lc_stat_grid(lc_stat_box("P(I | alarm)", risk_format_probability(p)), lc_stat_box("P(D | I) — wejście do FTA", risk_format_probability(1 - input$i10_sens)), columns = 1)
  })
  output$i10_inspection_result <- renderUI(lc_stat_grid(
    lc_stat_box("E(X) przy zadanym p", round(input$i10_n * input$i10_p, 2)),
    lc_stat_box("P(co najmniej jednej wady)", risk_format_probability(risk_at_least_one(input$i10_n, input$i10_p))),
    lc_stat_box("Jeśli zaobserwowano zero: górna granica 95% dla p", risk_format_probability(risk_zero_failure_upper(input$i10_n))), columns = 1
  ))
  evaluate <- function(id = "none", stress = 1) risk_mission_analysis(
    input$i10_time, input$i10_life_model, input$i10_power,
    input$i10_controller, input$i10_init, input$i10_sens, id, stress
  )
  base <- reactive(evaluate())
  life_r <- reactive(base()$fan_r)
  sys_r <- reactive(base()$system_r)
  top_p <- reactive(base()$top)
  life_plot <- reactive({
    t <- seq(0, 3000, length.out = 400)
    r <- if (input$i10_life_model == "exp") exp(-t / 1500) else exp(-(t / 1700)^2)
    ggplot(data.frame(t, r), aes(t, r)) + geom_line(colour = upwr_accent, linewidth = 1) +
      geom_vline(xintercept = input$i10_time, linetype = 2) +
      labs(title = "Niezawodność pojedynczego wentylatora", x = "Czas (h)", y = "R(t)") + theme_upwr()
  })
  zoom_plot_server("i10_life_plot", life_plot, alt = "Krzywa wybranego modelu czasu życia wentylatora z zaznaczonym czasem misji.")
  output$i10_life_stats <- renderUI(lc_stat_grid(lc_stat_box("R(t) przekazane do systemu", risk_format_probability(life_r())), columns = 1))
  output$i10_system_result <- renderUI({
    b <- base()
    lc_stat_grid(lc_stat_box("Czas misji", paste(input$i10_time, "h")), lc_stat_box("R zasilania w misji", risk_format_probability(b$power_r)), lc_stat_box("R sterownika w misji", risk_format_probability(b$controller_r)), lc_stat_box("R gałęzi równoległych", risk_format_probability(b$parallel_r)), lc_stat_box("R systemu | I", risk_format_probability(sys_r())), columns = 1)
  })
  fta_plot <- reactive({
    b <- base()
    d <- data.frame(node = c("P(I)", "P(D | I)", "P(S | I)", "P(TOP)"), p = c(b$initiation, b$miss, b$cooling_failure, b$top))
    ggplot(d, aes(node, p, fill = node)) + geom_col() + scale_fill_manual(values = upwr_cat_n(4), guide = "none") + labs(title = "Zdarzenie inicjujące i warunkowe niepowodzenia", x = NULL, y = "Prawdopodobieństwo") + theme_upwr()
  })
  zoom_plot_server("i10_fta_plot", fta_plot, alt = "Prawdopodobieństwo inicjacji, warunkowe prawdopodobieństwa niepowodzeń oraz wynik na jedną misję.")
  output$i10_fta_stats <- renderUI(lc_stat_grid(lc_stat_box("P(TOP) na misję", risk_format_probability(top_p())), lc_stat_box("Na 10 000 porównywalnych misji", risk_natural_frequency(top_p(), 10000)), columns = 1))
  intervention_top <- function(id, stress = 1) evaluate(id, stress)$top
  interventions_plot <- reactive({
    d <- bananpol$interventions
    d$result <- vapply(d$id, intervention_top, numeric(1))
    d$budget <- ifelse(d$cost_index <= input$i10_budget, "W budżecie", "Poza budżetem")
    ggplot(d, aes(reorder(label, result), result, fill = budget)) + geom_col() + coord_flip() + scale_fill_manual(values = upwr_cat_n(length(unique(d$budget)))) + labs(title = "Wynik po interwencji", x = NULL, y = "P(TOP) na misję", fill = NULL) + theme_upwr()
  })
  zoom_plot_server("i10_interventions_plot", interventions_plot, alt = "Porównanie ryzyka po czterech działaniach z oznaczeniem dostępności w budżecie.")
  output$i10_intervention_stats <- renderUI({
    d <- bananpol$interventions[bananpol$interventions$id == input$i10_intervention, ]
    lc_stat_grid(lc_stat_box("P(TOP) po zmianie", risk_format_probability(intervention_top(d$id))), lc_stat_box("Koszt umowny", d$cost_index), lc_stat_box("Wykonalność", d$feasibility), columns = 1)
  })
  scenario_results <- reactive({
    scenarios <- c("Optymistyczny", "Bazowy", "Ostrożny")
    multipliers <- c(1 - input$i10_uncertainty, 1, 1 + input$i10_uncertainty)
    do.call(rbind, lapply(seq_along(scenarios), function(i) {
      d <- bananpol$interventions
      d$scenario <- scenarios[i]
      d$result <- vapply(d$id, intervention_top, numeric(1), stress = multipliers[i])
      d
    }))
  })
  scenarios_plot <- reactive({
    d <- scenario_results()
    d$scenario <- factor(d$scenario, levels = c("Optymistyczny", "Bazowy", "Ostrożny"))
    ggplot(d, aes(label, result, fill = scenario)) + geom_col(position = "dodge") + coord_flip() + scale_fill_manual(values = upwr_cat_n(3)) + labs(title = "Każda opcja w każdym scenariuszu", x = NULL, y = "P(TOP) po działaniu", fill = "Scenariusz") + theme_upwr()
  })
  zoom_plot_server("i10_scenarios", scenarios_plot, alt = "Trzy scenariusze ryzyka po każdej interwencji.")
  output$i10_scenarios_stats <- renderUI({
    d <- scenario_results()
    d <- d[d$cost_index <= input$i10_budget, ]
    winners <- lapply(split(d, d$scenario), function(x) {
      best <- x$label[abs(x$result - min(x$result)) < 1e-12]
      paste(best, collapse = " / ")
    })
    lc_feedback(type = "info", paste(paste(names(winners), unlist(winners), sep = ": "), collapse = "; "), ". Ranking minimalnego P(TOP) w budżecie; równe wyniki pokazano razem. To nie jest przedział ufności.")
  })
  output$i10_memo <- renderUI({
    d <- bananpol$interventions[bananpol$interventions$id == input$i10_recommend, ]
    results <- scenario_results()
    worst <- max(results$result[results$id == d$id])
    affordable <- d$cost_index <= input$i10_budget
    meets <- worst <= input$i10_target
    tags$ol(
      tags$li(paste0("Przy misji ", input$i10_time, " h bazowe P utraty ochrony wynosi ", risk_format_probability(top_p()), "; to ", risk_natural_frequency(top_p(), 10000), " porównywalnych misji.")),
      tags$li(paste0("Rozważamy: ", d$label, "; koszt ", d$cost_index, ", ", if (affordable) "w budżecie" else "poza budżetem", "; P(TOP) po działaniu bazowo ", risk_format_probability(intervention_top(d$id)), ", w najgorszym rozpatrzonym scenariuszu ", risk_format_probability(worst), ".")),
      tags$li(paste0("Demonstracyjny limit ", risk_format_probability(input$i10_target), if (meets) " jest spełniony w badanych scenariuszach" else " nie jest spełniony", "; ", if (affordable && meets) "wariant można przekazać do oceny skutków i wdrożenia" else "potrzebna jest inna opcja, projekt lub budżet", ". Założenia: brak napraw, niezależność detekcji od chłodzenia i skuteczność działań zgodna ze scenariuszem.")),
      tags$li("Właściciel proponowanej kontroli: kierownik utrzymania; weryfikacja detekcji, awarii i skuteczności działania po pierwszej misji oraz po każdej zmianie instalacji. Oddzielnie oceniamy skutki utraty ochrony i scenariusze pominięte w modelu.")
    )
  })
  risk_assessment_server("i10", integracja_quiz, input, output)
}
