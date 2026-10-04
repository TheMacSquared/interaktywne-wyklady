# Blok 04: Wiele prób -----------------------------------------------------

proby_quiz <- list(questions = list(
  list(
  question = "Który warunek jest konieczny dla prostego modelu dwumianowego?",
  choices = c(
    "Stała liczba prób i to samo p w każdej próbie" = "fixed",
    "Rosnące p po każdej awarii" = "growing",
    "Co najmniej trzy możliwe wyniki próby" = "three"
  ),
  correct = "fixed",
  explanation = "Model wymaga ustalonego n, dwóch wyników, stałego p i niezależności prób."
),
  list(question = "Dla n=100 i p=0.02 ile wynosi E(X)?",
    choices = c("0.02" = "a", "98" = "b", "2" = "c"), correct = "c",
    explanation = "E(X)=np=2; pojedyncza partia może dać inny wynik."),
  list(question = "Jak policzyć co najmniej jedną wadę?",
    choices = c("p^n" = "a", "1-(1-p)^n" = "b", "np w każdym przypadku" = "c"), correct = "b",
    explanation = "Przez dopełnienie zdarzenia, że wszystkie próby zakończą się bez wady."),
  list(question = "Co oznacza P(X≤2) w planie akceptacji c=2?",
    choices = c("Prawdopodobieństwo przyjęcia partii" = "a", "Prawdopodobieństwo dokładnie dwóch wad" = "b", "Prawdopodobieństwo odrzucenia partii" = "c"), correct = "a",
    explanation = "Przyjmujemy partię, gdy liczba wad nie przekracza limitu."),
  list(question = "Zero wad w 100 niezależnych próbach przy ustalonym n. Jaki wniosek jest poprawny?",
    choices = c("Udowodniono p=0" = "a", "Następna partia na pewno nie ma wad" = "b", "Górna jednostronna granica 95% dla p wynosi około 0.0295" = "c"), correct = "c",
    explanation = "Niezaobserwowanie wady pozostawia niepewność parametru; granica wynika z (1-p)^100=0.05.")
))
proby_exercises <- list(
  list(
    task = "Bananpol: dla n=100 i p=0.02 policz P(X=0), P(X=2) i P(X≥1).",
    answer = c(
      "Ze wzoru (4.3): P(X = 0) = 0.98¹⁰⁰ ≈ 0.133; P(X = 2) = C(100, 2) · 0.02² · 0.98⁹⁸ = 4950 · 0.0004 · 0.1381 ≈ 0.273.",
      "Ze wzoru (4.6): P(X ≥ 1) = 1 - P(X = 0) = 1 - 0.98¹⁰⁰ ≈ 0.867. Co najmniej jeden niesprawny zawór pojawi się w około 87% partii, choć pojedynczy zawór zawodzi tylko w 2% przypadków."
    )
  ),
  list(
    task = "Diagnostyka: partia pochodzi z dwóch dostaw o różnej jakości. Które założenie modelu dwumianowego jest zagrożone?",
    answer = c(
      "Zagrożona jest stałość p: zawory z dwóch dostaw mają różne prawdopodobieństwa niesprawności, więc próby nie są jednakowe.",
      "Jak w przykładzie 4.3, średnia liczba wad może pozostać taka sama, ale rozkład robi się szerszy: częściej zdarzają się partie bez wad i partie z wieloma wadami. Rozwiązaniem jest podział na warstwy — osobny model dla każdej dostawy."
    )
  ),
  list(
    task = "Transfer: zdefiniuj próbę, sukces i n dla kontroli 30 mocowań rusztowania.",
    answer = c(
      "Próba: kontrola jednego mocowania według tej samej procedury. Sukces (zdarzenie liczone): mocowanie niesprawne. n = 30, ustalone przed kontrolą.",
      "Jeśli p = 0.01, to E(X) = 30 · 0.01 = 0.3, a P(X ≥ 1) = 1 - 0.99³⁰ ≈ 0.260. Warto też zapytać, czy mocowania montowała ta sama ekipa — wspólna przyczyna błędów podważyłaby niezależność."
    )
  ),
  list(
    task = "Plan odbioru: losujemy n = 80 zaworów i przyjmujemy partię, gdy niesprawnych jest co najwyżej c = 2. Oblicz prawdopodobieństwo przyjęcia partii dobrej (p = 0.01) i słabej (p = 0.05).",
    answer = c(
      "Ze wzoru (4.7): P(przyjęcia) = P(X ≤ 2) dla X ~ Bin(80; p).",
      "Dla p = 0.01: P(X ≤ 2) ≈ 0.953, więc dobra partia jest odrzucana w około 5% przypadków. Dla p = 0.05: P(X ≤ 2) ≈ 0.231, więc słaba partia przechodzi mniej więcej raz na cztery–pięć odbiorów."
    )
  ),
  list(
    task = "Ekspozycja skumulowana: pracownik magazynu wykonuje codziennie czynność, przy której ryzyko urazu wynosi 0.001 na dzień. Rok ma 250 dni roboczych. Oblicz P(co najmniej jednego urazu) w ciągu roku i wyznacz, po ilu dniach ryzyko skumulowane przekroczy 50%.",
    answer = c(
      "Ze wzoru (4.6): po 250 dniach 1 - 0.999²⁵⁰ ≈ 0.221.",
      "Szukamy najmniejszego n, dla którego 1 - 0.999ⁿ ≥ 0.5, czyli n ≥ ln 0.5 / ln 0.999 ≈ 692.8. Odpowiedź: 693 dni, czyli niecałe trzy lata pracy (około 2.8 roku)."
    )
  )
)

proby_variable_widget <- figure_panel(
  label = "Słownik",
  title = "Jedna kontrola zaworu jako zmienna losowa",
  full_width = TRUE,
  lc_table(
    data.frame(
      outcome = c("Zawór niesprawny", "Zawór sprawny"),
      value = c("1", "0"),
      probability = c("p = 0.02", "1 - p = 0.98")
    ),
    cols = list(
      lc_col("outcome", "Wynik kontroli", "row"),
      lc_col("value", "Wartość Xᵢ", "text"),
      lc_col("probability", "Prawdopodobieństwo", "text")
    ),
    narrow = "cards"
  ),
  lc_caption("E(Xᵢ) = p = 0.02 to średni wynik jednej próby, a suma X₁ + … + X₁₀₀
    to licznik niesprawnych: zmienna, o którą naprawdę pytamy."),
  lc_status(
    tags$strong("Po co ta konstrukcja:"),
    " zdarzeń nie da się dodawać, ale liczby już tak. Zapisanie wyniku próby
      jako 0/1 pozwala sumować kontrole, liczyć średnie i budować rozkłady —
      cała dalsza część kursu stoi na tym pomoście."
  )
)

proby_sciaga_widget <- tagList(
  figure_panel(
    label = "Ściąga 4.1",
    title = "Pytania i odpowiedzi modelu dwumianowego",
    full_width = TRUE,
    lc_table(
      data.frame(
        pytanie = c("Dokładnie k niesprawnych", "Co najwyżej k", "Co najmniej k", "Co najmniej jedna", "Typowa liczba"),
        zapis = c("P(X = k)", "P(X ≤ k)", "P(X ≥ k) = 1 - P(X ≤ k-1)", "P(X ≥ 1) = 1 - (1-p)ⁿ", "E(X) = np"),
        narzedzie = c("wzór dwumianowy", "suma od 0 do k", "dopełnienie", "dopełnienie zera zdarzeń", "środek rozkładu, nie prognoza")
      ),
      cols = list(
        lc_col("pytanie", "Pytanie", "row"),
        lc_col("zapis", "Zapis", "text"),
        lc_col("narzedzie", "Narzędzie", "text")
      ),
      narrow = "cards", prose = TRUE
    )
  ),
  risk_assessment_ui("p4", proby_quiz, proby_exercises)
)

proby_block <- list(id = "proby", title = "Wiele prób", chapters = list(
  list(
    id = "jednostka", title = "Próba Bernoulliego", hook = "Jedna kontrola, dwa możliwe wyniki",
    lead = "Najpierw ustalamy jednostkę ekspozycji i wynik 0/1.",
    intro = c(
      "Nowy dostawca przysłał do Bananpolu partię stu zaworów do instalacji chłodniczej. Zanim policzysz cokolwiek, musisz zdecydować, co jest pojedynczą próbą i jaki wynik uznajesz za zdarzenie — od tej decyzji zależy każda dalsza liczba w analizie.",
      "Dotychczas pytaliśmy o pojedynczą zmianę albo pojedynczy alarm. Dziś zmieniamy skalę: interesuje nas cała seria porównywalnych prób i liczba zdarzeń, które się w niej pojawią. Ale seria jest dobra tylko wtedy, gdy jej elementy naprawdę są porównywalne — dlatego zaczynamy od definicji próby, nie od wzoru."
    ),
    callout = list(
      label = "Dane Bananpolu",
      text = "Kontrola przyjęcia dostawy: partia 100 zaworów, prawdopodobieństwo niesprawności pojedynczego zaworu 0.02. Jednostka: kontrola jednego elementu; horyzont: jedna partia kontrolna. Liczby są fikcyjne.",
      color = "uwaga"
    ),
    sections = list(
      list(
        id = "kryteria", title = "Dobra próba ma trzy cechy",
        text = "W wykładzie 01 przestrzeń wyników opisywała jedno doświadczenie. Teraz to samo doświadczenie powtarzamy wiele razy i chcemy mówić o całej serii naraz. Żeby liczba zdarzeń w serii cokolwiek znaczyła, każde powtórzenie musi być tym samym doświadczeniem — inaczej dodawalibyśmy do siebie jabłka i gruszki. Z tej potrzeby wynikają trzy praktyczne kryteria dobrej próby:",
        bullets = c(
          "jest jednoznacznie wyodrębniona — wiadomo, gdzie kończy się jedna, a zaczyna druga;",
          "ma dokładnie dwa rozłączne wyniki, nazwane przed obserwacją;",
          "jest porównywalna z pozostałymi — ten sam typ elementu, ta sama procedura kontroli."
        )
      ),
      list(
        id = "proba", title = "Sukces, porażka i zapis 0/1",
        body = list(
          c(
            "Kryteria z poprzedniej sekcji mają swoją nazwę. Doświadczenie, które kończy się jednym z dwóch wyników, nazywamy próbą Bernoulliego — od szwajcarskiego matematyka Jakuba Bernoulliego, który na przełomie XVII i XVIII wieku jako pierwszy ściśle zbadał długie serie takich prób.",
            "Dwa wyniki umownie nazywamy sukcesem i porażką. W analizie ryzyka ta etykieta bywa myląca, bo sukcesem jest zwykle zdarzenie niepożądane: niesprawny zawór, awaria, wypadek. To tylko nazwa techniczna — sukcesem nazywamy zdarzenie, które liczymy. W kontroli partii Bananpolu sukcesem jest więc znalezienie niesprawnego zaworu."
          ),
          risk_definition("4.1", "Próba Bernoulliego", c(
            "Próba Bernoulliego to doświadczenie losowe, które ma dokładnie dwa rozłączne wyniki: sukces (zdarzenie, które liczymy) i porażkę. Prawdopodobieństwo sukcesu oznaczamy literą p, a prawdopodobieństwo porażki wynosi wtedy 1 - p."
          )),
          "Dwa wyniki nie oznaczają, że świat jest dwuwartościowy. Zawór może być lekko nieszczelny, bardzo nieszczelny albo zablokowany; ciśnienie można zmierzyć z dokładnością do setnych części bara. Próbę Bernoulliego tworzymy decyzją analityka: ustalamy kryterium, które dzieli wszystkie możliwe wyniki na dwie grupy. Kryterium musi być zapisane przed kontrolą, bo inaczej granica między sukcesem a porażką przesuwa się w zależności od tego, co zobaczymy.",
          risk_example("4.1", "Czy to jest próba Bernoulliego?",
            problem = list(
              "Oceń, czy opisana obserwacja jest próbą Bernoulliego. Jeśli nie, zaproponuj, jak ją przekształcić.",
              risk_parts(
                "Kontroler sprawdza jeden zawór i zapisuje: sprawny albo niesprawny.",
                "Dyspozytor zapisuje liczbę usterek zgłoszonych w ciągu dnia.",
                "Technik mierzy ciśnienie otwarcia zaworu w barach."
              )
            ),
            steps = c(
              "Dwa rozłączne wyniki nazwane przed obserwacją — to próba Bernoulliego. Sukces: zawór niesprawny.",
              "Wynik może wynosić 0, 1, 2, 3, … — to nie jest próba Bernoulliego. Można ją nią uczynić, pytając: „czy tego dnia była co najmniej jedna usterka?”. Tracimy wtedy informację o liczbie usterek w ciągu dnia.",
              "Wynik jest liczbą rzeczywistą. Próbę Bernoulliego dostajemy, ustalając próg przed pomiarem, np. sukces: ciśnienie otwarcia poza tolerancją producenta."
            ),
            steps_type = "a",
            answer = "Tylko (a) jest próbą Bernoulliego wprost. (b) i (c) stają się nimi po jawnym zdefiniowaniu dwóch wyników, co zawsze oznacza świadomą utratę części informacji."
          ),
          "Teraz ta sama decyzja w kontekście całej dostawy. Wybierz definicję, która tworzy serię porównywalnych prób.",
          risk_vote_panel(
            "p4_vote", "p4_vote_feedback", "Która definicja tworzy porównywalne próby?",
            c(
              "Każdy skontrolowany zawór; wynik: sprawny/niesprawny" = "valve",
              "Każdy dzień; wynik: dowolna liczba usterek" = "day",
              "Cała fabryka; wynik: wszystkie obserwacje" = "factory"
            ),
            correct = "valve"
          ),
          "Definicja „dzień i dowolna liczba usterek” łamie kryterium dwóch wyników, a „cała fabryka” nie wyodrębnia żadnej powtarzalnej jednostki — mamy jedną obserwację, a nie serię. Tylko kontrola pojedynczego zaworu daje sto porównywalnych prób, a wtedy pytanie „ile niesprawnych w partii?” ma jasny sens.",
          risk_check("p4_chk_proba",
            "Magazyn rejestruje, czy paleta dotarła uszkodzona. Jedna paleta może mieć kilka uszkodzonych kartonów. Co jest próbą Bernoulliego?",
            c("Jedna paleta; sukces: co najmniej jeden uszkodzony karton" = "pallet", "Jedna paleta; wynik: liczba uszkodzonych kartonów" = "count", "Cały miesiąc dostaw; wynik: suma uszkodzeń" = "month"),
            correct = "pallet",
            explanation = "Paleta jest wyodrębnioną jednostką, a pytanie „czy jest co najmniej jeden uszkodzony karton?” daje dokładnie dwa rozłączne wyniki.",
            hints = c(count = "Liczba kartonów może wynosić 0, 1, 2, … — ile wyników ma taka obserwacja?", month = "Miesiąc to jedna obserwacja, a nie seria porównywalnych prób.")
          )
        )
      )
    )
  ),
  list(
    id = "bernoulli", title = "Schemat Bernoulliego", hook = "Pojedyncza kontrola zaskakuje, seria już nie",
    lead = "Pojedyncze wyniki są losowe, choć długookresowa częstość jest stabilna — o ile sytuacja spełnia założenia modelu.",
    intro = c(
      "Serię prób o dwóch wynikach, stałym p i wzajemnej niezależności nazywamy schematem Bernoulliego. To najprostszy generator losowości w tym kursie — i fundament trzech rozkładów, które poznasz w tym i następnym wykładzie.",
      "Uruchom serię kontroli kilka razy. Wzór kropek za każdym razem będzie inny: czasem niesprawne zawory pojawią się parami, czasem długo nie będzie żadnego. Losowość lokalna i stabilność globalna nie wykluczają się — to dwie strony tego samego schematu."
    ),
    sections = list(
      list(
        id = "seria", title = "Seria kontroli",
        body = list(
          risk_try("zostaw 100 kontroli i kliknij „Uruchom serię” pięć–sześć razy. Zapisz, ile niesprawnych zaworów (×) pojawiło się w każdej serii i czy zdarzyły się dwa krzyżyki obok siebie. Potem ustaw 200 kontroli."),
          figure_panel(
            label = "Symulacja", title = "Seria kontroli zaworów",
            lc_toolbar(
              lc_slider("p4_series_n", "Liczba kontroli", 10, 200, 100, 10),
              lc_action("p4_run", "Uruchom serię", variant = "solid"),
              lc_readouts(uiOutput("p4_series_stats"))
            ),
            verbatimTextOutput("p4_sequence"), full_width = TRUE
          ),
          c(
            "Symulacja używa p = 0.02, więc w serii stu kontroli średnio pojawiają się dwa niesprawne zawory. Poszczególne serie rzadko trafiają dokładnie w tę liczbę: mniej więcej jedna seria na siedem–osiem nie ma ani jednego krzyżyka (prawdopodobieństwo 0.98¹⁰⁰ ≈ 0.133), a mniej więcej co siódma ma cztery lub więcej. Krzyżyki obok siebie nie świadczą o żadnej zależności — w losowym ciągu skupiska pojawiają się same.",
            "Co więc jest stabilne? Nie wynik pojedynczej serii, lecz prawdopodobieństwa poszczególnych wyników. Żeby je policzyć, potrzebujemy precyzyjnej definicji schematu i jednego wzoru na prawdopodobieństwo konkretnego ciągu wyników."
          ),
          risk_definition("4.2", "Schemat Bernoulliego", c(
            "Schemat Bernoulliego to ciąg n prób Bernoulliego, w którym (1) liczba prób n jest ustalona przed obserwacją, (2) każda próba ma te same dwa rozłączne wyniki, (3) prawdopodobieństwo sukcesu p jest takie samo w każdej próbie, (4) wyniki prób są niezależne."
          )),
          "Z niezależności wynika reguła mnożenia z wykładu 02: prawdopodobieństwo, że kolejne próby dadzą określone wyniki, jest iloczynem prawdopodobieństw tych wyników — tak jak iloczyn wzdłuż jednej drogi w drzewie zdarzeń. Każdy sukces wnosi do iloczynu czynnik p, każda porażka czynnik 1 - p. Kolejność czynników nie ma znaczenia, więc liczy się tylko to, ile było sukcesów.",
          risk_formula("P(\\text{konkretny ciąg z } k \\text{ sukcesami})=p^{k}(1-p)^{n-k}", num = "4.1",
            legend = c("n" = "liczba prób", "k" = "liczba sukcesów w ciągu", "p" = "prawdopodobieństwo sukcesu w jednej próbie")),
          risk_example("4.2", "Konkretny ciąg kontroli",
            problem = list(
              "Przy p = 0.02 oblicz prawdopodobieństwo, że:",
              risk_parts(
                "Trzy pierwsze zawory są sprawne, a czwarty niesprawny.",
                "Pierwszy zawór jest niesprawny, a trzy kolejne sprawne.",
                "Wszystkie sto zaworów w partii jest sprawnych."
              )
            ),
            steps = c(
              "Ciąg: sprawny, sprawny, sprawny, niesprawny. Ze wzoru (4.1) z n = 4, k = 1: 0.98³ · 0.02 ≈ 0.0188.",
              "Ciąg: niesprawny, sprawny, sprawny, sprawny. Te same czynniki w innej kolejności: 0.02 · 0.98³ ≈ 0.0188.",
              "n = 100, k = 0: 0.98¹⁰⁰ ≈ 0.133."
            ),
            steps_type = "a",
            answer = "(a) i (b) mają to samo prawdopodobieństwo, około 0.019, bo zawierają tyle samo sukcesów. (c) wynosi około 0.133: partia bez żadnej wady wcale nie jest rzadkością, mimo że średnio spodziewamy się dwóch."
          )
        )
      ),
      list(
        id = "zalozenia", title = "Cztery założenia",
        body = list(
          "Dwumianowy jest modelem sytuacji, nie tylko wzorem. Zanim go użyjesz, sprawdź listę kontrolną:",
          tags$ul(
            tags$li("n jest ustalone przed obserwacją"),
            tags$li("każda próba ma dwa rozłączne wyniki"),
            tags$li("p jest stałe"),
            tags$li("wyniki prób są niezależne")
          ),
          "Każde z tych założeń psuje się w rozpoznawalny sposób. Dwie dostawy o różnej jakości wymieszane w jednej partii łamią stałość p. Wada, która uszkadza sąsiednie zawory w transporcie, łamie niezależność. Kontroler, który po znalezieniu wady zaczyna sprawdzać dokładniej, też łamie niezależność: sposób kolejnej kontroli zależy od wyniku poprzedniej. Reguła jest prosta — zmiana wywołana historią wyników łamie niezależność, a zmiana z przyczyn zewnętrznych, jak inna dostawa czy dryf maszyny, łamie stałość p. Wybierz scenariusz poniżej i sprawdź diagnozę.",
          "Osobny przypadek to losowanie bez zwracania dużej części małej partii: wtedy każda wyjęta sztuka zmienia skład reszty i właściwym modelem jest rozkład hipergeometryczny. Dwumianowy jest jego dobrym przybliżeniem, gdy próbka jest mała względem partii.",
          "Złamanie założenia nie jest sprawą estetyki. Zmienia liczby, na których opieramy decyzję — nawet wtedy, gdy średnia pozostaje taka sama. Najłatwiej pokazać to na przykładzie mieszanki dostaw.",
          risk_example("4.3", "Dwie dostawy o różnej jakości",
            problem = "Partia stu zaworów pochodzi w całości od dostawcy A (p = 0.01) albo w całości od dostawcy B (p = 0.03), każdy z prawdopodobieństwem 1/2. Średnie p wynosi 0.02, tak jak w danych Bananpolu. Oblicz prawdopodobieństwo partii bez żadnej wady i porównaj z modelem, który zakłada stałe p = 0.02.",
            steps = c(
              "Warunkowo, przy znanym dostawcy, schemat Bernoulliego obowiązuje. Dla A: P(X = 0 | A) = 0.99¹⁰⁰ ≈ 0.366. Dla B: P(X = 0 | B) = 0.97¹⁰⁰ ≈ 0.048.",
              "Prawdopodobieństwo całkowite z wykładu 02: P(X = 0) = 0.5 · 0.366 + 0.5 · 0.048 ≈ 0.207.",
              "Model ze stałym p = 0.02 daje 0.98¹⁰⁰ ≈ 0.133.",
              "Średnia liczba wad jest w obu modelach taka sama: 0.5 · 1 + 0.5 · 3 = 2 w mieszance, 100 · 0.02 = 2 w modelu stałego p."
            ),
            answer = "W mieszance partia bez wad zdarza się z prawdopodobieństwem około 0.207 zamiast 0.133. Ta sama średnia, ale szerszy rozkład: częściej trafiają się partie czyste i częściej partie z wieloma wadami."
          ),
          risk_try("wybierz kolejno trzy scenariusze i przy każdym zapytaj siebie, które z czterech założeń jest zagrożone, zanim przeczytasz diagnozę."),
          figure_panel(
            label = "Diagnoza", title = "Scenariusz partii",
            selectInput("p4_scenario", "Sytuacja", c("Jedna stabilna linia" = "stable", "Dwie dostawy o różnym p" = "mixture", "Uszkodzenie zwiększa ryzyko następnego" = "dependent")),
            uiOutput("p4_scenario_feedback"), full_width = TRUE
          ),
          "Diagnoza nie mówi „model jest zły”, lecz wskazuje, które założenie trzeba sprawdzić w danych. Mieszankę dostaw leczy się podziałem na warstwy: osobno partie od A, osobno od B, każda z własnym p. Zależność prób wymaga innego modelu albo zmiany jednostki — na przykład próbą staje się cała skrzynia zaworów, a nie pojedynczy zawór.",
          risk_check("p4_chk_zalozenia",
            "Kontroler po znalezieniu niesprawnego zaworu zaczyna dokładniej oglądać kolejne zawory i częściej wykrywa drobne wady. Które założenie schematu Bernoulliego jest złamane?",
            c("Niezależność prób" = "indep", "Stałość p" = "p", "Ustalone n" = "n", "Dwa wyniki próby" = "two"),
            correct = "indep",
            explanation = "Sposób kolejnej kontroli zależy od wyniku wcześniejszej, więc próby przestają być niezależne. To, że p przy okazji przestaje być stałe, jest skutkiem tej zależności: zmianę wywołuje historia wyników, a nie przyczyna zewnętrzna.",
            hints = c(p = "p rzeczywiście się zmienia, ale co wywołuje tę zmianę — przyczyna zewnętrzna czy wynik wcześniejszej kontroli?", n = "Liczba kontrolowanych zaworów nadal może być ustalona z góry. Co zmienia się w pojedynczej kontroli?", two = "Każda kontrola nadal kończy się wynikiem sprawny/niesprawny.")
          )
        )
      )
    ),
    pitfall = "Duża partia nie naprawia złej definicji próby ani zmiennego p."
  ),
  list(
    id = "zmienna", title = "Rozkład dwumianowy", hook = "Liczba awarii to suma zer i jedynek",
    lead = "Funkcja przypisująca wynikom liczby jest pomostem między „co może się zdarzyć” a „ile tego będzie”; suma takich zer i jedynek ma rozkład dwumianowy.",
    intro = c(
      "W pierwszym wykładzie zdarzenia były zbiorami: podzbiorami przestrzeni wyników. Zbiorów nie da się jednak dodawać ani uśredniać, a inspektor chce właśnie tego — policzyć niesprawne zawory w partii i porównać partie między sobą. Potrzebny jest pomost od zdarzeń do arytmetyki.",
      "Tym pomostem jest zmienna losowa: funkcja, która każdemu wynikowi doświadczenia przypisuje liczbę. Dla jednej kontroli zaworu przypisujemy 1, gdy zawór jest niesprawny, i 0, gdy sprawny. Rozkład zmiennej losowej mówi, które wartości i z jakim prawdopodobieństwem może ona przyjąć — dla pojedynczej próby to najprostszy rozkład tego kursu, rozkład Bernoulliego."
    ),
    body = list(
      risk_definition("4.3", "Zmienna losowa", c(
        "Zmienna losowa to funkcja X, która każdemu wynikowi ω z przestrzeni wyników Ω przypisuje liczbę rzeczywistą X(ω). Zmienną losową nazywamy dyskretną, jeśli przyjmuje skończenie wiele wartości albo wartości, które da się ponumerować (0, 1, 2, …)."
      )),
      "Definicja 4.3 brzmi abstrakcyjnie, ale opisuje rzecz codzienną. Wynikiem kontroli stu zaworów jest długi ciąg „sprawny, sprawny, niesprawny, …”. Zmienna X „liczba niesprawnych” zamienia każdy taki ciąg na jedną liczbę od 0 do 100. Różne ciągi mogą dać tę samą liczbę — i właśnie dlatego pytania o liczbę zdarzeń są prostsze niż pytania o konkretne ciągi.",
      risk_definition("4.4", "Rozkład zmiennej losowej dyskretnej", c(
        "Rozkład zmiennej losowej dyskretnej X to lista jej możliwych wartości x₁, x₂, … wraz z prawdopodobieństwami P(X = x₁), P(X = x₂), … . Prawdopodobieństwa są nieujemne i sumują się do 1."
      )),
      "Znajomość rozkładu wystarcza, żeby odpowiedzieć na każde pytanie o X: o konkretną wartość, o przedział wartości, o średnią. Cała praca modelowania polega więc na znalezieniu rozkładu — reszta to rachunki."
    ),
    sections = list(
      list(
        id = "suma", title = "Licznik jest sumą zer i jedynek",
        text = "Zapis 0/1 wygląda niepozornie, ale robi całą robotę: liczba niesprawnych zaworów w partii to po prostu suma X = X₁ + … + X₁₀₀. Pytanie „ile zdarzeń w n próbach?” stało się pytaniem o rozkład sumy zmiennych losowych — i na to pytanie odpowie dalsza część rozdziału.",
        body = list(
          "Pojedyncza zmienna Xᵢ przyjmuje tylko dwie wartości. Jej rozkład to rozkład Bernoulliego: wartość 1 z prawdopodobieństwem p i wartość 0 z prawdopodobieństwem 1 - p. Średnia takiej zmiennej jest równa p, bo 1 · p + 0 · (1 - p) = p.",
          risk_formula("P(X_i=1)=p,\\qquad P(X_i=0)=1-p,\\qquad E(X_i)=p", num = "4.2",
            legend = c("X_i" = "wynik i-tej kontroli zapisany jako 0 albo 1", "p" = "prawdopodobieństwo niesprawności jednego zaworu")),
          proby_variable_widget,
          "Tabela jest pełnym rozkładem jednej kontroli w sensie definicji 4.4: dwie wartości, dwa prawdopodobieństwa, suma 1. Suma stu takich zmiennych ma już 101 możliwych wartości. Zanim przejdziemy do stu, zobaczmy, jak powstaje rozkład sumy na najmniejszym przykładzie, który da się wypisać ręcznie.",
          risk_example("4.4", "Rozkład liczby wad w trzech kontrolach",
            problem = "Kontrolujemy n = 3 zawory przy p = 0.02. Wypisz wszystkie ciągi wyników, pogrupuj je według liczby niesprawnych i wyznacz rozkład zmiennej X = X₁ + X₂ + X₃.",
            steps = c(
              "Ciągów jest 2³ = 8. X = 0: jeden ciąg (SSS), prawdopodobieństwo 0.98³ = 0.941192.",
              "X = 1: trzy ciągi (NSS, SNS, SSN), każdy o prawdopodobieństwie 0.02 · 0.98² ze wzoru (4.1). Razem 3 · 0.019208 = 0.057624.",
              "X = 2: trzy ciągi (NNS, NSN, SNN), każdy 0.02² · 0.98. Razem 3 · 0.000392 = 0.001176.",
              "X = 3: jeden ciąg (NNN), 0.02³ = 0.000008.",
              "Kontrola: 0.941192 + 0.057624 + 0.001176 + 0.000008 = 1."
            ),
            answer = "P(X=0) ≈ 0.9412; P(X=1) ≈ 0.0576; P(X=2) ≈ 0.0012; P(X=3) = 0.000008. Prawdopodobieństwo każdej wartości to liczba ciągów razy prawdopodobieństwo jednego ciągu."
          ),
          risk_check("p4_chk_zmienna",
            "Która z wielkości jest zmienną losową w kontroli partii stu zaworów?",
            c("Liczba niesprawnych zaworów w partii" = "count", "Parametr p = 0.02" = "param", "Zdarzenie „partia zawiera wadę”" = "event"),
            correct = "count",
            explanation = "Liczba niesprawnych przypisuje każdemu wynikowi kontroli (ciągowi stu wyników) liczbę — to funkcja na przestrzeni wyników. p jest stałą modelu, a zdarzenie jest zbiorem wyników, nie liczbą.",
            hints = c(param = "p nie zmienia się od partii do partii — to parametr, który opisuje każdą próbę.", event = "Zdarzenie to zbiór wyników. Zmienna losowa przypisuje wynikom liczby.")
          )
        ),
        takeaway = "Zmienna losowa nie jest ani zmienną z algebry, ani niewiadomą — jest funkcją na przestrzeni wyników. Jej wartość poznajemy dopiero po doświadczeniu, ale jej rozkład znamy przed nim."
      ),
      list(
        id = "rozklad", title = "Rozkład liczby awarii",
        text = c(
          "Suma stu prób Bernoulliego o stałym p i wzajemnej niezależności ma rozkład dwumianowy. Współczynnik dwumianowy zlicza, na ile sposobów k niesprawnych zaworów może rozmieścić się wśród n kontroli — a reszta wzoru to znany z wykładu o warunkach iloczyn wzdłuż drogi.",
          "W praktyce inspektora rzadko potrzebne jest „dokładnie k”. Pytania decyzyjne mają formę ogonową: co najmniej jedna wada (czy partia jest podejrzana?), co najwyżej dwie (czy mieścimy się w limicie akceptacji?). Przełącznik pytania w widgecie zmienia zaznaczony obszar rozkładu — obserwuj, jak zmienia się wynik."
        ),
        body = list(
          "Przykład 4.4 pokazał przepis, który działa dla dowolnego n. Zdarzenie {X = k} składa się ze wszystkich ciągów, w których jest dokładnie k sukcesów. Każdy z nich ma, na mocy wzoru (4.1), to samo prawdopodobieństwo p^k · (1 - p)^(n-k). Wystarczy więc policzyć, ile jest takich ciągów, i pomnożyć. Liczbę sposobów wyboru k pozycji spośród n oznaczamy C(n, k) i nazywamy współczynnikiem dwumianowym (symbol Newtona).",
          risk_derivation("liczba układów C(n, k)", c(
            "Wybieramy pozycje k sukcesów w ciągu n prób. Pierwszą pozycję można wybrać na n sposobów, drugą na n - 1, …, k-tą na n - k + 1. Iloczyn n · (n - 1) · … · (n - k + 1) liczy jednak każdy zbiór pozycji k! razy — tyle jest kolejności, w jakich można wybrać te same k pozycji. Dzielimy więc przez k!.",
            "Dla n = 4 i k = 2 dostajemy (4 · 3) / 2 = 6 układów: {1,2}, {1,3}, {1,4}, {2,3}, {2,4}, {3,4}. Dla n = 3 i k = 1 — trzy układy, jak w przykładzie 4.4."
          ), lines = c("C(n, k) = n · (n - 1) · … · (n - k + 1) / k!", "        = n! / (k! · (n - k)!)", "C(100, 2) = 100 · 99 / 2 = 4950")),
          risk_definition("4.5", "Rozkład dwumianowy", c(
            "Zmienna X ma rozkład dwumianowy z parametrami n (liczba całkowita, n ≥ 1) i p (0 ≤ p ≤ 1), co zapisujemy X ~ Bin(n; p), jeśli przyjmuje wartości 0, 1, …, n z prawdopodobieństwami danymi wzorem (4.3). X to liczba sukcesów w schemacie Bernoulliego z n próbami."
          )),
          risk_formula("X\\sim \\mathrm{Bin}(n,p),\\quad P(X=k)={n\\choose k}p^k(1-p)^{n-k},\\qquad k=0.1,\\ldots,n", num = "4.3",
            legend = c("{n\\choose k}" = "liczba ciągów z dokładnie k sukcesami, C(n, k)", "p^k(1-p)^{n-k}" = "prawdopodobieństwo jednego takiego ciągu, wzór (4.1)")),
          "Pytania o „co najmniej” i „co najwyżej” dotyczą nie jednego słupka, lecz całego fragmentu rozkładu. Taki fragment ma w statystyce własną nazwę.",
          risk_definition("4.6", "Ogon rozkładu", c(
            "Prawy ogon rozkładu zmiennej X od wartości k to zdarzenie {X ≥ k} i jego prawdopodobieństwo; lewy ogon do wartości k to {X ≤ k}. Prawdopodobieństwo ogona jest sumą prawdopodobieństw wszystkich wartości, które do niego należą."
          )),
          risk_formula("P(X\\le k)=\\sum_{j=0}^{k}P(X=j),\\qquad P(X\\ge k)=1-P(X\\le k-1)", num = "4.4",
            legend = c("k" = "granica ogona", "j" = "wartości sumowane w lewym ogonie")),
          "Druga część wzoru (4.4) to trik z dopełnieniem: prawy ogon od k i lewy ogon do k - 1 są zdarzeniami przeciwnymi, więc zamiast sumować wiele wyrazów prawego ogona, odejmujemy od jedności kilka wyrazów lewego. Wrócimy do tego triku w następnym rozdziale i w wykładzie 05.",
          risk_example("4.5", "Partia stu zaworów",
            problem = list(
              "Dla X ~ Bin(100; 0.02) oblicz:",
              risk_parts("P(X = 0).", "P(X = 2).", "P(X ≤ 2).", "P(X ≥ 3).")
            ),
            steps = c(
              "Ze wzoru (4.3): C(100, 0) · 0.98¹⁰⁰ = 0.98¹⁰⁰ ≈ 0.1326.",
              "C(100, 2) · 0.02² · 0.98⁹⁸ = 4950 · 0.0004 · 0.1381 ≈ 0.2734.",
              "Potrzebne jest jeszcze P(X = 1) = 100 · 0.02 · 0.98⁹⁹ ≈ 0.2707. Ze wzoru (4.4): P(X ≤ 2) ≈ 0.1326 + 0.2707 + 0.2734 = 0.6767.",
              "Z dopełnienia: P(X ≥ 3) = 1 - P(X ≤ 2) ≈ 1 - 0.6767 = 0.3233."
            ),
            steps_type = "a",
            answer = "(a) ≈ 0.133; (b) ≈ 0.273; (c) ≈ 0.677; (d) ≈ 0.323. Mniej więcej jedna partia na trzy ma trzy lub więcej niesprawnych zaworów."
          ),
          risk_try("przy n = 100, p = 0.02 i k = 2 odczytaj wynik dla „Dokładnie k” i porównaj z przykładem 4.5(b). Potem przełącz na „Co najmniej k” i ustaw k = 3 — porównaj z 4.5(d). Na koniec zwiększ p do 0.05 i obserwuj, jak przesuwa się cały rozkład."),
          risk_widget_panel(
            "Rozkład", "Liczba niesprawnych zaworów",
            tagList(
              lc_slider("p4_n", "n", 10, 300, 100, 10), lc_slider("p4_p", "p", .001, .10, .02, .001),
              lc_slider("p4_k", "k", 0, 20, 2, 1),
              selectInput("p4_query", "Pytanie", c("Dokładnie k" = "exactly", "Co najmniej k" = "at_least", "Najwyżej k" = "at_most"))
            ),
            "p4_binom", "p4_binom_stats"
          ),
          "Przy domyślnych ustawieniach widget pokazuje 0.273 dla „dokładnie 2” i 0.323 dla „co najmniej 3” — te same liczby co w przykładzie 4.5. Najwyższe są słupki przy 1 i 2, a rozkład jest prawostronnie skośny: nie może zejść poniżej zera, ale ciągnie się w prawo. Po zwiększeniu p do 0.05 szczyt przesuwa się do 4–5, a rozkład staje się szerszy i bardziej symetryczny.",
          risk_check("p4_chk_ogon",
            "Jak obliczyć P(X ≥ 3) z tablicy wartości P(X ≤ k)?",
            c("1 - P(X ≤ 2)" = "right", "1 - P(X ≤ 3)" = "off", "P(X ≤ 3) - P(X ≤ 2)" = "exact"),
            correct = "right",
            explanation = "Zdarzeniem przeciwnym do {X ≥ 3} jest {X ≤ 2}, więc P(X ≥ 3) = 1 - P(X ≤ 2), zgodnie ze wzorem (4.4).",
            hints = c(off = "1 - P(X ≤ 3) to P(X ≥ 4): wartość 3 zniknęła z ogona.", exact = "Ta różnica to P(X = 3), czyli jeden słupek, a nie cały ogon.")
          )
        )
      )
    )
  ),
  list(
    id = "srednia", title = "Wartość oczekiwana", hook = "Średnia nie mówi, co spotka tę partię",
    lead = "np opisuje środek wielu partii, nie wynik jednej konkretnej partii; pytanie o co najmniej jedną wadę liczy się przez zdarzenie przeciwne.",
    intro = c(
      "Ile wad będzie w najbliższej partii stu zaworów przy p = 0.02? Kusząca odpowiedź — „dwie, przecież 100 razy 0.02 to 2” — jest błędna w sposób, który najlepiej zobaczyć na własne oczy: pojedyncza partia może mieć zero wad, a zdarza się też pięć.",
      "Symulacja poniżej losuje setki partii przy tych samych parametrach. Zanim spojrzysz na jakikolwiek wzór, obejrzyj histogram: gdzie leży jego środek, jak szeroko rozrzucają się wyniki i jak często zdarza się dokładnie ta „oczekiwana” liczba wad."
    ),
    sections = list(
      list(
        id = "powtorzenia", title = "Średnia nie jest prognozą",
        body = list(
          risk_try("zostaw 500 partii i odczytaj, jak wysoki jest słupek przy dwóch wadach w porównaniu z pozostałymi. Histogram używa n i p ustawionych w widgecie rozkładu z poprzedniego rozdziału — przy domyślnych n = 100 i p = 0.02 linia wartości oczekiwanej stoi przy 2. Potem zwiększ liczbę partii do 2000."),
          risk_widget_panel(
            "Powtórzenia", "Wiele partii przy tych samych parametrach",
            lc_slider("p4_batches", "Liczba partii", 50, 2000, 500, 50), "p4_batches_plot", "p4_batches_stats"
          ),
          "Przy n = 100 i p = 0.02 (domyślne ustawienia) 500 symulowanych partii ma od 0 do 7 niesprawnych zaworów. Dokładnie dwie wady ma 156 partii, czyli około 31% — najczęstszy wynik, ale wciąż mniejszość. Zero wad ma 67 partii (około 13%), a średnia z symulacji wynosi 2.03, bardzo blisko linii. Środek histogramu jest więc stabilny, a pojedyncze partie rozrzucają się wokół niego szeroko.",
          "Środek tego histogramu i jego szerokość mają zwięzły zapis. Najpierw potrzebujemy ogólnej definicji średniej zmiennej losowej, a potem zastosujemy ją do sumy zer i jedynek.",
          risk_definition("4.7", "Wartość oczekiwana i wariancja", c(
            "Wartość oczekiwana zmiennej dyskretnej X to średnia jej wartości ważona prawdopodobieństwami: E(X) = Σ x · P(X = x). Wariancja to wartość oczekiwana kwadratu odchylenia od średniej: Var(X) = E[(X - E(X))²]. Pierwiastek z wariancji to odchylenie standardowe; mierzy typowy rozrzut w jednostkach X."
          )),
          risk_formula("E(X)=np,\\qquad \\operatorname{Var}(X)=np(1-p)", num = "4.5",
            legend = c("n" = "liczba prób", "p" = "prawdopodobieństwo sukcesu w jednej próbie", "np" = "średnia liczba sukcesów w wielu porównywalnych seriach")),
          "Wartość oczekiwana opisuje środek ciężkości wielu porównywalnych partii; wariancja — rozrzut wyników wokół tego środka.",
          risk_derivation("E(X) = np i Var(X) = np(1 - p)", c(
            "Nie trzeba sumować wzoru (4.3). Wystarczy zapis X = X₁ + … + Xₙ z poprzedniego rozdziału. Średnia sumy to suma średnich, a ze wzoru (4.2) każda Xᵢ ma średnią p.",
            "Dla jednej próby: Xᵢ² = Xᵢ (bo 0² = 0 i 1² = 1), więc Var(Xᵢ) = E(Xᵢ²) - [E(Xᵢ)]² = p - p² = p(1 - p). Dla niezależnych prób wariancja sumy to suma wariancji."
          ), lines = c("E(X) = E(X₁) + … + E(Xₙ) = n · p", "Var(Xᵢ) = p - p² = p(1 - p)", "Var(X) = Var(X₁) + … + Var(Xₙ) = n · p(1 - p)")),
          risk_example("4.6", "Czego spodziewać się w jednej partii?",
            problem = "Dla X ~ Bin(100; 0.02) oblicz E(X), Var(X) i odchylenie standardowe. Następnie oblicz prawdopodobieństwo, że partia ma dokładnie E(X) wad, oraz prawdopodobieństwo, że liczba wad mieści się w przedziale od 1 do 3.",
            steps = c(
              "Ze wzoru (4.5): E(X) = 100 · 0.02 = 2; Var(X) = 100 · 0.02 · 0.98 = 1.96; odchylenie standardowe √1.96 = 1.4.",
              "Z przykładu 4.5: P(X = 2) ≈ 0.273.",
              "P(1 ≤ X ≤ 3) = P(X = 1) + P(X = 2) + P(X = 3) ≈ 0.2707 + 0.2734 + 0.1823 ≈ 0.726.",
              "Pozostałe ok. 27% to partie bez wad (0.133) oraz z czterema lub więcej wadami (1 - 0.859 ≈ 0.141)."
            ),
            answer = "E(X) = 2, odchylenie standardowe 1.4. Dokładnie dwie wady ma tylko około 27% partii; w przedziale E(X) ± 1 mieści się około 73%."
          ),
          risk_check("p4_chk_srednia",
            "Dla partii stu zaworów przy p = 0.02 mamy E(X) = 2. Które zdanie jest poprawne?",
            c("W wielu partiach średnia liczba wad będzie bliska 2" = "many", "Najbliższa partia będzie miała 2 wady" = "one_batch", "Większość partii ma dokładnie 2 wady" = "most"),
            correct = "many",
            explanation = "Wartość oczekiwana opisuje środek wielu porównywalnych partii. Pojedyncza partia ma dokładnie 2 wady tylko z prawdopodobieństwem około 0.273.",
            hints = c(one_batch = "Czy histogram z symulacji pokazuje jedną wartość, czy rozrzut?", most = "Sprawdź przykład 4.6: jaki odsetek partii ma dokładnie 2 wady?")
          )
        ),
        takeaway = "Wynik jednej partii może wyraźnie różnić się od wartości oczekiwanej: przy stałym p raz zobaczysz zero niesprawnych, innym razem kilka. E(X)=np opisuje środek tego histogramu, nie obietnicę dla konkretnej partii. Dopiero rozkład wyników wielu porównywalnych partii pokazuje, które liczebności są typowe, a które powinny skłonić do sprawdzenia modelu."
      ),
      list(
        id = "co-najmniej-jedna", title = "Co najmniej jedna",
        text = c(
          "Pytanie „czy w partii jest co najmniej jedna wada?” obejmuje mnóstwo scenariuszy: jedna wada, dwie, trzy… aż po sto. Zamiast sumować je wszystkie, liczymy jedno zdarzenie przeciwne — ani jednej wady — i odejmujemy od jedności. To najczęstszy trik rachunkowy analizy ryzyka.",
          "Krzywa poniżej pokazuje konsekwencję, którą łatwo przeoczyć: nawet bardzo małe p przy dużej liczbie prób daje niemal pewne zdarzenie. Rzadkość pojedynczej próby nie chroni długiej serii."
        ),
        body = list(
          "Zdarzenie {X = 0} to jeden konkretny ciąg: wszystkie n prób kończy się porażką. Ze wzoru (4.1) z k = 0 jego prawdopodobieństwo wynosi (1 - p)ⁿ. Trik z dopełnieniem daje wtedy wzór, który w tym kursie wróci jeszcze wiele razy — w wykładzie 05 przy czasie oczekiwania na pierwsze zdarzenie, a w wykładzie 08 przy niezawodności systemów.",
          risk_formula("P(X\\ge 1)=1-P(X=0)=1-(1-p)^n", num = "4.6",
            legend = c("p" = "prawdopodobieństwo zdarzenia w jednej próbie", "n" = "liczba niezależnych prób", "(1-p)^n" = "prawdopodobieństwo, że żadna próba nie da zdarzenia")),
          risk_example("4.7", "Ile kontroli, żeby zobaczyć wadę?",
            problem = list(
              "Przy p = 0.02 oblicz:",
              risk_parts(
                "P(co najmniej jednego niesprawnego zaworu) w partii 100 zaworów.",
                "Najmniejszą liczbę kontroli n, przy której to prawdopodobieństwo przekracza 0.5.",
                "Najmniejsze n, przy którym przekracza 0.9."
              )
            ),
            steps = c(
              "Ze wzoru (4.6): 1 - 0.98¹⁰⁰ ≈ 1 - 0.133 = 0.867.",
              "Warunek 1 - 0.98ⁿ ≥ 0.5 to 0.98ⁿ ≤ 0.5. Logarytmując: n ≥ ln 0.5 / ln 0.98 ≈ 34.3. Sprawdzenie: 1 - 0.98³⁴ ≈ 0.497, a 1 - 0.98³⁵ ≈ 0.507, więc n = 35.",
              "n ≥ ln 0.1 / ln 0.98 ≈ 114.0. Sprawdzenie: 1 - 0.98¹¹³ ≈ 0.898, a 1 - 0.98¹¹⁴ ≈ 0.900, więc n = 114."
            ),
            steps_type = "a",
            answer = "(a) ≈ 0.867; (b) 35 kontroli; (c) 114 kontroli. Przejście od „raczej zobaczymy wadę” do „prawie na pewno zobaczymy” wymaga ponad trzykrotnie więcej prób."
          ),
          risk_try("przy p = 0.02 odczytaj wartość dla n = 100 i porównaj z przykładem 4.7(a). Następnie zmniejsz p do 0.005 i sprawdź, jak zmienia się kształt krzywej i wartość dla n = 100."),
          risk_widget_panel(
            "Krzywa", "Ryzyko wraz z liczbą prób",
            lc_slider("p4_curve_p", "p niesprawności", .001, .10, .02, .001), "p4_one", "p4_one_stats"
          ),
          "Krzywa rośnie szybko na początku i coraz wolniej zbliża się do 1: każda kolejna próba dokłada coraz mniej, bo dopełnienie (1 - p)ⁿ maleje geometrycznie. Przy p = 0.005 wartość dla n = 100 spada do około 0.394, ale na prawym końcu wykresu, przy n = 300, sięga już około 0.778. Czterokrotnie mniejsze p nie daje czterokrotnie mniejszego ryzyka serii.",
          risk_check("p4_chk_jedna",
            "Przy p = 0.01 i n = 100 iloczyn np wynosi 1. Ile wynosi P(co najmniej jednego zdarzenia)?",
            c("Około 0.634" = "right", "1, bo np = 1" = "np", "0.01, jak w jednej próbie" = "single"),
            correct = "right",
            explanation = "Ze wzoru (4.6): 1 - 0.99¹⁰⁰ ≈ 0.634. Iloczyn np jest średnią liczbą zdarzeń, a nie prawdopodobieństwem; średnio jedno zdarzenie nie oznacza pewności, że wystąpi.",
            hints = c(np = "np to wartość oczekiwana liczby zdarzeń, wzór (4.5). Prawdopodobieństwo liczymy przez dopełnienie.", single = "Pytamy o całą serię stu prób, nie o jedną.")
          )
        )
      ),
      list(
        id = "transfer", title = "Przykład transferowy: ekspozycja skumulowana",
        text = "Ta sama krzywa opisuje każdą powtarzaną ekspozycję. Codzienny przejazd o ryzyku kolizji 0.0001 na przejazd daje po dziesięciu latach pracy (około 2500 przejazdów) ponad 20% szans co najmniej jednej kolizji. Wniosek dla profilaktyki: komunikaty „to zdarza się rzadko” trzeba zawsze uzupełniać horyzontem — rzadko na próbę nie znaczy rzadko w karierze.",
        body = list(
          risk_definition("4.8", "Ekspozycja skumulowana", c(
            "Ekspozycja skumulowana to liczba n powtórzeń narażenia na to samo ryzyko w przyjętym horyzoncie czasu (przejazdów, zmian roboczych, operacji). Ryzyko skumulowane to prawdopodobieństwo co najmniej jednego zdarzenia w tym horyzoncie, przy stałym p i niezależnych powtórzeniach równe 1 - (1 - p)ⁿ ze wzoru (4.6)."
          )),
          "W praktyce często słyszy się skrót: ryzyko skumulowane to po prostu n · p. Skrót działa tylko wtedy, gdy np jest małe, bo wtedy (1 - p)ⁿ ≈ 1 - np. Przy większych ekspozycjach n · p przeszacowuje ryzyko, a dla np > 1 daje absurdalne „prawdopodobieństwo” większe od jedności.",
          risk_example("4.8", "Dziesięć lat dojazdów",
            problem = "Kierowca wózka wykonuje dziennie jeden przejazd przez skrzyżowanie hali o ryzyku kolizji 0.0001 na przejazd, 250 dni w roku przez 10 lat. Oblicz ryzyko skumulowane i porównaj z przybliżeniem n · p.",
            steps = c(
              "Ekspozycja skumulowana: n = 250 · 10 = 2500 przejazdów.",
              "Ze wzoru (4.6): 1 - 0.9999²⁵⁰⁰ ≈ 0.221.",
              "Przybliżenie n · p = 2500 · 0.0001 = 0.25 — o około 3 punkty procentowe za dużo, bo np nie jest już bardzo małe."
            ),
            answer = "Około 0.221, czyli ponad 20%: mniej więcej jeden kierowca na pięciu w takiej karierze będzie miał co najmniej jedną kolizję. Skrót n · p daje tu wynik zawyżony, ale przy mniejszym np byłby wystarczający."
          )
        )
      )
    )
  ),
  list(
    id = "decyzja", title = "Plan odbioru", hook = "Zero wad w stu kontrolach to nie zero ryzyka",
    lead = "Plan kontroli łączy ryzyko partii z regułą akceptacji.",
    intro = c(
      "Rachunek dwumianowy staje się decyzją w planie odbioru partii: losujemy n elementów i akceptujemy dostawę, jeżeli liczba niesprawnych nie przekracza limitu c. Para (n, c) wyznacza dwie krzywe ryzyka — szansę odrzucenia dobrej partii i szansę przyjęcia złej.",
      "Nie ma planu doskonałego: zaostrzenie limitu chroni magazyn, ale częściej odrzuca przyzwoite dostawy; złagodzenie działa odwrotnie. Dlatego plan kontroli jest decyzją negocjowaną z dostawcą i zapisaną przed pierwszą kontrolą, a nie dobieraną po obejrzeniu wyników."
    ),
    sections = list(
      list(
        id = "plan", title = "Co należy zapisać?",
        bullets = c("wielkość losowanej próby", "dopuszczalna liczba niesprawnych", "p reprezentujące jakość partii", "konsekwencję odrzucenia i przeoczenia"),
        body = list(
          risk_definition("4.9", "Plan odbioru (n, c)", c(
            "Plan odbioru (n, c) to reguła: z partii losujemy n elementów, kontrolujemy każdy z nich i przyjmujemy partię wtedy i tylko wtedy, gdy liczba niesprawnych X nie przekracza liczby akceptacji c."
          )),
          "Jeśli losowane elementy tworzą schemat Bernoulliego z prawdopodobieństwem niesprawności p, to X ~ Bin(n; p), a prawdopodobieństwo przyjęcia partii jest lewym ogonem rozkładu dwumianowego — wzór (4.4) zastosowany do k = c. Traktowane jako funkcja p nazywa się krzywą operacyjną planu: pokazuje, jak plan reaguje na partie o różnej jakości.",
          risk_formula("P(\\text{przyjęcia}\\mid p)=P(X\\le c)=\\sum_{k=0}^{c}{n\\choose k}p^{k}(1-p)^{n-k}", num = "4.7",
            legend = c("n" = "liczba skontrolowanych elementów", "c" = "liczba akceptacji", "p" = "prawdopodobieństwo niesprawności w partii")),
          risk_example("4.9", "Plan (50, 1) dla dwóch jakości partii",
            problem = "Bananpol losuje 50 zaworów i przyjmuje partię, gdy znajdzie co najwyżej jeden niesprawny. Oblicz prawdopodobieństwo przyjęcia partii dobrej (p = 0.02) i słabej (p = 0.08). Porównaj z planem (100, 2).",
            steps = c(
              "Ze wzoru (4.7) dla p = 0.02: P(X ≤ 1) = 0.98⁵⁰ + 50 · 0.02 · 0.98⁴⁹ ≈ 0.736. Dobra partia jest odrzucana z prawdopodobieństwem około 0.264.",
              "Dla p = 0.08: P(X ≤ 1) ≈ 0.083. Słaba partia przechodzi mniej więcej raz na dwanaście odbiorów.",
              "Plan (100, 2): dla p = 0.02 P(X ≤ 2) ≈ 0.677 (z przykładu 4.5), dla p = 0.08 P(X ≤ 2) ≈ 0.011."
            ),
            answer = "Plan (50, 1): przyjęcie dobrej partii ≈ 0.736, słabej ≈ 0.083. Plan (100, 2) lepiej odsiewa słabe partie (≈ 0.011), ale płaci za to częstszym odrzucaniem dobrych — przy p = 0.02 odrzuca prawie co trzecią."
          ),
          "Przykład pokazuje, że liczba 0.02 w danych Bananpolu nie jest progiem, tylko przeciętną jakością dostaw. Jeśli taka jakość jest dla magazynu akceptowalna, plan, który odrzuca co trzecią przeciętną partię, generuje koszty sporów z dostawcą bez uzasadnienia. Wybór (n, c) to więc wybór między dwoma rodzajami błędu, a kontrola większej liczby elementów przy odpowiednio dobranym c pozwala zmniejszyć oba naraz — kosztem pracy kontrolerów.",
          risk_check("p4_chk_plan",
            "Przy stałym n = 50 zwiększamy liczbę akceptacji z c = 1 do c = 2. Co dzieje się z prawdopodobieństwem przyjęcia partii?",
            c("Rośnie dla każdej jakości partii" = "up", "Rośnie tylko dla dobrych partii" = "good", "Maleje, bo plan jest surowszy" = "down"),
            correct = "up",
            explanation = "P(X ≤ 2) = P(X ≤ 1) + P(X = 2), więc przy każdym p dochodzi nieujemny składnik. Łagodniejszy limit chroni dobre partie przed odrzuceniem, ale też częściej przepuszcza słabe.",
            hints = c(good = "We wzorze (4.7) dochodzi jeden wyraz sumy. Czy może być ujemny dla jakiegoś p?", down = "Większe c oznacza łagodniejszy, a nie surowszy plan.")
          )
        )
      ),
      list(
        id = "zero", title = "Zero wad w stu kontrolach",
        body = list(
          "Wszystkie dotychczasowe rachunki zakładały, że p jest znane. W praktyce p pochodzi z danych, a dane bywają skąpe. Najbardziej podchwytliwy przypadek to seria bez żadnego zdarzenia — kusi, żeby uznać, że ryzyka po prostu nie ma.",
          figure_panel(label = "Od danych do modelu", title = "Zero wad w stu kontrolach", full_width = TRUE,
            lc_p("Jeśli p nie podano, szacujemy je z próby. Zero wad w 100 niezależnych kontrolach daje oszacowanie punktowe 0, ale dokładna jednostronna górna granica ufności 95% wynosi 1-0.05^(1/100)≈0.0295. Przy tej wartości szansa zobaczenia zera wynosi jeszcze 5%. Założenia obejmują stałe p i ustaloną z góry liczebność próby."),
            lc_p("Dla następnej partii 100 elementów podstawienie oszacowania p=0 daje prognozę P(co najmniej jednej wady)=0, natomiast podstawienie górnej granicy daje 0.95. To wrażliwość prognozy na niepewność p, a nie 95-procentowe prawdopodobieństwo awarii partii. Losowość nowej partii i niepewność oszacowania to dwa różne źródła niepewności.")
          ),
          "Górna granica z panelu to największe p, przy którym zero wad w n próbach jest jeszcze „dość prawdopodobne”, czyli ma prawdopodobieństwo co najmniej 5%. Warunek (1 - p)ⁿ = 0.05 to warunek P(X ≥ 1) = 0.95 ze wzoru (4.6) zapisany od drugiej strony. Rozwiązując go względem p, dostajemy:",
          risk_formula("p_U=1-0.05^{1/n}\\approx \\frac{3}{n}", num = "4.8",
            legend = c("p_U" = "jednostronna górna granica ufności 95% dla p po zerze zdarzeń", "n" = "liczba niezależnych prób bez zdarzenia")),
          risk_derivation("reguła trzech", c(
            "Logarytmujemy warunek (1 - p)ⁿ = 0.05: n · ln(1 - p) = ln 0.05 ≈ -3.0. Dla małych p ln(1 - p) ≈ -p, więc n · p ≈ 3."
          ), lines = c("(1 - p)ⁿ = 0.05", "n · ln(1 - p) = ln 0.05 ≈ -2.996", "-n · p ≈ -3   ⇒   p_U ≈ 3/n")),
          risk_example("4.10", "Ile mówi seria bez wad?",
            problem = "Dostawca chwali się, że w 300 kolejnych kontrolach nie znaleziono ani jednej wady. Inny dostawca — że nie znalazł żadnej w 30 kontrolach. Oblicz górną granicę 95% dla p w obu przypadkach, dokładnie i regułą trzech.",
            steps = c(
              "n = 300: ze wzoru (4.8) p_U = 1 - 0.05^(1/300) ≈ 0.0099; reguła trzech: 3/300 = 0.010.",
              "n = 30: p_U = 1 - 0.05^(1/30) ≈ 0.095; reguła trzech: 3/30 = 0.10."
            ),
            answer = "Po 300 czystych kontrolach p może jeszcze wynosić około 1%, po 30 — nawet około 10%, czyli pięć razy więcej niż przeciętna jakość w Bananpolu. „Zero wad” bez podania n nie niesie prawie żadnej informacji."
          )
        )
      )
    ),
    decision = "Porównaj kilka jakości partii, zanim wybierzesz n i limit akceptacji."
  ),
  list(
    id = "sprawdzenie", title = "Ściąga i sprawdzenie", hook = "Najpierw jednostka, potem wzór",
    lead = "Jednostka → założenia → Bin(n,p) → pytanie ogonowe → decyzja.",
    intro = "Model dwumianowy jest pierwszym „gotowym” rozkładem w kursie i łatwo go nadużyć: wystarczy przeoczyć zmienne p albo zależność prób. Ściąga zbiera pytania i zapisy; quiz oraz ćwiczenia sprawdzają, czy potrafisz zarówno policzyć wynik, jak i zauważyć, kiedy liczyć nie wolno.",
    sections = list(
      list(
        id = "podsumowanie", title = "Podsumowanie",
        text = c(
          "Wykład zaczął się od decyzji, a nie od wzoru: co jest pojedynczą próbą i jaki wynik liczymy. Próba Bernoulliego (definicja 4.1) ma dwa rozłączne wyniki, a schemat Bernoulliego (definicja 4.2) powtarza ją n razy przy stałym p i niezależności. Z tych założeń wynika wzór (4.1): prawdopodobieństwo konkretnego ciągu zależy tylko od liczby sukcesów. Przykład 4.3 pokazał, że złamanie stałości p zmienia rozkład nawet przy tej samej średniej.",
          "Zmienna losowa (definicja 4.3) zamienia wyniki na liczby, dzięki czemu liczbę wad można zapisać jako sumę zer i jedynek (4.2). Zliczając ciągi o tej samej liczbie sukcesów, dostaliśmy rozkład dwumianowy (4.3), a pytania decyzyjne okazały się pytaniami o ogony (4.4). Wartość oczekiwana np i wariancja np(1 - p) ze wzoru (4.5) opisują środek i rozrzut wielu partii, a nie wynik jednej.",
          "Trik z dopełnieniem (4.6) odpowiada na najczęstsze pytanie analizy ryzyka — o co najmniej jedno zdarzenie — i pokazuje, że małe ryzyko pojedynczej próby kumuluje się w długiej ekspozycji. W decyzji odbioru partii lewy ogon (4.7) staje się prawdopodobieństwem przyjęcia, a seria bez zdarzeń daje tylko górną granicę dla p (4.8), nie dowód, że p = 0."
        )
      ),
      list(id = "sciaga", title = "Ściąga", bullets = c("Pytanie: ile zdarzeń w n próbach?", "Model: dwumianowy", "Założenia: stałe n i p, dwa wyniki, niezależność", "Wynik: prawdopodobieństwo liczby zdarzeń", "Interpretacja: dotyczy powtarzalnych partii"), widget = proby_sciaga_widget),
      list(
        id = "most", title = "Co dalej",
        text = "Dwumianowy zatrzymuje się po ustalonej liczbie prób i pyta o liczbę zdarzeń. W następnym wykładzie odwrócimy regułę zatrzymania: ustalimy liczbę zdarzeń, a losowa stanie się liczba prób potrzebnych, żeby je zaobserwować."
      )
    )
  )
))

proby_chapters <- risk_block_chapters(proby_block)

proby_server <- function(input, output, session) {
  vote <- reactiveVal(FALSE)
  observeEvent(input$p4_vote_check, vote(TRUE))
  output$p4_vote_feedback <- renderUI({
    req(vote())
    if (is.null(input$p4_vote)) {
      return(lc_caption(
               "Najpierw zaznacz jedną z odpowiedzi.",
               tone = "info"
             ))
    }
    lc_status(
      lc_verdict(tags$strong("Jednostka:"), type = if (identical(input$p4_vote, "valve")) "ok" else "warning"),
      " jeden zawór i dwa rozłączne wyniki."
    )
  })
  series <- reactive({
    input$p4_run
    rbinom(input$p4_series_n, 1, .02)
  })
  output$p4_sequence <- renderText(paste(ifelse(series() == 1, "×", "·"), collapse = " "))
  output$p4_series_stats <- renderUI(tagList(
    lc_readout("Niesprawne", sum(series()), color = upwr_accent),
    lc_readout("Oczekiwano średnio", round(length(series()) * .02, 1))
  ))
  output$p4_scenario_feedback <- renderUI({
    messages <- c(stable = "Założenia są wiarygodne, jeśli kontrole nie wpływają na siebie.", mixture = "Zmienia się p: rozważ warstwy dostaw.", dependent = "Zagrożona jest niezależność prób.")
    lc_caption(
      messages[[input$p4_scenario]]
    )
  })
  binom_plot <- reactive({
    kmax <- max(15, qbinom(.999, input$p4_n, input$p4_p))
    x <- 0:kmax
    dat <- data.frame(x, p = dbinom(x, input$p4_n, input$p4_p))
    selected <- switch(input$p4_query,
      exactly = x == input$p4_k,
      at_least = x >= input$p4_k,
      at_most = x <= input$p4_k
    )
    dat$part <- ifelse(selected, "Odpowiedź", "Pozostałe")
    ggplot(dat, aes(x, p, fill = part)) +
      geom_col() +
      scale_fill_manual(values = c(Odpowiedź = upwr_accent, Pozostałe = upwr_reference)) +
      labs(x = "Liczba niesprawnych", y = "Prawdopodobieństwo", fill = NULL) +
      theme_upwr()
  })
  zoom_plot_server("p4_binom", binom_plot, alt = "Słupkowy rozkład dwumianowy z wyróżnionym zakresem odpowiedzi.")
  output$p4_binom_stats <- renderUI({
    k <- min(input$p4_k, input$p4_n)
    tagList(
      lc_readout("Prawdopodobieństwo", risk_fmt_p(risk_binomial_probability(input$p4_n, input$p4_p, k, input$p4_query)), color = upwr_accent)
    )
  })
  batches <- reactive({
    set.seed(2404)
    rbinom(input$p4_batches, input$p4_n, input$p4_p)
  })
  batches_plot <- reactive(ggplot(data.frame(x = batches()), aes(x)) +
    geom_histogram(binwidth = 1, boundary = -.5, fill = upwr_secondary, colour = "white") +
    geom_vline(xintercept = input$p4_n * input$p4_p, colour = upwr_accent, linewidth = 1) +
    labs(x = "Liczba niesprawnych", y = "Liczba partii") +
    theme_upwr())
  zoom_plot_server("p4_batches_plot", batches_plot, alt = "Histogram liczby niesprawnych w wielu partiach z linią wartości oczekiwanej.")
  output$p4_batches_stats <- renderUI(tagList(
    lc_readout("E(X)", round(input$p4_n * input$p4_p, 2)),
    lc_readout("Zakres w symulacji", paste(range(batches()), collapse = "–"))
  ))
  one_plot <- reactive({
    n <- 1:300
    ggplot(data.frame(n, p = vapply(n, risk_at_least_one, numeric(1), p = input$p4_curve_p)), aes(n, p)) +
      geom_line(colour = upwr_accent, linewidth = 1.1) +
      labs(x = "Liczba prób", y = "P(X ≥ 1)") +
      theme_upwr()
  })
  zoom_plot_server("p4_one", one_plot, alt = "Rosnąca krzywa prawdopodobieństwa co najmniej jednej niesprawności.")
  output$p4_one_stats <- renderUI(tagList(
    lc_readout("Dla n=100", risk_fmt_p(risk_at_least_one(100, input$p4_curve_p)), color = upwr_accent)
  ))
  risk_assessment_server("p4", proby_quiz, input, output)
}
