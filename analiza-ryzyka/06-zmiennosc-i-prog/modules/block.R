# Blok 06: Zmienność i próg ---------------------------------------------

prog_quiz <- list(questions = list(
  list(question = "Która zmiana bezpośrednio zmniejsza P(T>85°C), gdy próg jest stały i leży powyżej średniej?", choices = c("Obniżenie średniej lub odchylenia standardowego" = "both", "Zwiększenie średniej" = "mean", "Ignorowanie ogona rozkładu" = "ignore"), correct = "both", explanation = "Położenie i rozrzut rozkładu wspólnie wyznaczają pole za progiem."),
  list(question = "T~N(82, 3), drugi parametr to σ. Jakie jest P(T>85)?",
    choices = c("Około 0.841" = "a", "Zero, bo średnia jest niższa" = "b", "Około 0.159" = "c"), correct = "c",
    explanation = "z=(85-82)/3=1 i liczymy prawy ogon 1-Φ(1)."),
  list(question = "Próg c leży poniżej μ. Co robi zmniejszenie σ przy stałych c i μ?",
    choices = c("Zawsze pozostawia 0.5" = "a", "Zwiększa P(T>c)" = "b", "Zmniejsza P(T>c)" = "c"), correct = "b",
    explanation = "Zwężenie rozkładu skupia więcej masy powyżej progu leżącego poniżej średniej."),
  list(question = "Jakie zdarzenie opisuje awarię obciążenie–wytrzymałość?",
    choices = c("S-L<0" = "a", "Nakładanie się gęstości" = "b", "S+L>0" = "c"), correct = "a",
    explanation = "Awaria zachodzi przy L>S. Pole wspólne gęstości nie mierzy częstości takich par."),
  list(question = "16% losowych pomiarów przekracza próg. Czy 16% zmian ma co najmniej jedno przekroczenie?",
    choices = c("Tak, procent nie ma jednostki" = "a", "Tak, jeśli użyto rozkładu normalnego" = "b", "Nie wynika to bez modelu pomiarów w zmianie" = "c"), correct = "c",
    explanation = "Pojedynczy pomiar i maksimum wielu pomiarów podczas zmiany to różne zmienne.")
))
prog_exercises <- list(
  list(
    task = "Bananpol: dla T~N(82, 3) policz P(T>85°C) i naturalną częstość na 1000 porównywalnych pomiarów.",
    answer = c(
      "Standaryzacja (6.4): z = (85 - 82)/3 = 1. Ze wzoru (6.5): P(T > 85) = 1 - Φ(1) = 1 - 0.841 = 0.159.",
      "Naturalna częstość: około 159 na 1000 porównywalnych pomiarów w ustalonym trybie pracy. To odsetek pojedynczych pomiarów, a nie odsetek zmian z co najmniej jednym przekroczeniem — do tego potrzebny jest model (6.7)."
    )
  ),
  list(
    task = "Diagnostyka (rozszerzenie): porównaj histogram i wykres kwantylowy; wskaż, co podważa model normalny.",
    answer = c(
      "Histogram pokazuje kształt całego rozkładu, ale przy kilkuset obserwacjach ogony są reprezentowane przez pojedyncze słupki i łatwo je przeoczyć. Wykres kwantylowy (6.11) porównuje każdy uporządkowany pomiar z jego normalnym odpowiednikiem, więc ogony są na nim wyraźnie widoczne.",
      "Model normalny podważają: systematyczne wygięcie punktów w jedną stronę (skośność — na przykład naturalna dolna granica temperatury), punkty odchodzące od prostej na obu końcach w przeciwnych kierunkach (ogony cięższe niż normalne) oraz pojedyncze skrajne pomiary daleko od prostej. Zgodność w środku wykresu niczego nie dowodzi o ogonie, a to ogon decyduje o ryzyku progowym."
    )
  ),
  list(
    task = "Transfer: dla obciążenia i wytrzymałości konstrukcji policz ryzyko jako P(L>S), nie jako pole nakładania krzywych.",
    answer = c(
      "Przyjmijmy dane liny z rozdziału 5: L ~ N(85; 8) kN, S ~ N(95; 7) kN, niezależne. Ze wzoru (6.9): μ_D = 10 kN, σ_D = √(8² + 7²) = √113 ≈ 10.63 kN. Indeks niezawodności β = 10/10.63 ≈ 0.94, więc ze wzoru (6.10) P(L > S) = Φ(-0.94) ≈ 0.173.",
      "Pole nakładania się obu gęstości wynosi w tym przykładzie około 0.50 — prawie trzy razy więcej. To liczba bez interpretacji probabilistycznej: nie mówi, jak często losowe obciążenie trafi na słabszą od niego wytrzymałość."
    )
  ),
  list(
    task = "Cel projektowy: przy μ = 82°C jakie odchylenie standardowe σ zapewni P(T > 85°C) ≤ 0.01? A jaką średnią trzeba osiągnąć, jeśli σ zostaje równe 3°C?",
    answer = c(
      "Warunek P(T > 85) ≤ 0.01 oznacza, że 85 ma być co najmniej kwantylem rzędu 0.99, czyli z ≥ z₀,₉₉ = 2.326 (qnorm(0.99)).",
      "Stabilizacja: (85 - 82)/σ ≥ 2.326, więc σ ≤ 3/2.326 ≈ 1.29°C — rozrzut trzeba zmniejszyć ponad dwukrotnie. Chłodzenie: (85 - μ)/3 ≥ 2.326, więc μ ≤ 85 - 6.98 ≈ 78.0°C — średnią trzeba obniżyć o około 4°C."
    )
  ),
  list(
    task = "Konwencja zapisu: dokumentacja dostawcy łożysk podaje „T ~ N(82, 9)” w konwencji wariancyjnej. Oblicz P(T > 85) poprawnie oraz wynik, który otrzyma ktoś, kto potraktuje 9 jako σ.",
    answer = c(
      "W konwencji N(μ, σ²) liczba 9 to wariancja, więc σ = 3°C i P(T > 85) = 1 - Φ(1) ≈ 0.159.",
      "Przy błędnym σ = 9: z = 3/9 ≈ 0.33 i P(T > 85) = 1 - Φ(0.33) ≈ 0.369 — ponad dwa razy więcej. Błąd konwencji nie daje drobnej różnicy zaokrągleń, tylko zupełnie inny obraz ryzyka."
    )
  )
)

prog_block <- list(id = "prog", title = "Zmienność i próg", chapters = list(
  list(
    id = "glosowanie", title = "Zmienność a próg", hook = "Średnia w normie, a próg przekroczony",
    # Poprawka raportu z rozdziału 1 (przyjęta 17.11.2026)
    widget = z6p_21_fix(1),
    lead = "Bez informacji o zmienności średnia nie odpowiada na pytanie o przekroczenie.",
    intro = c(
      "Na biurko kierownika dojrzewalni trafia raport z oceny ryzyka dla łożyska wentylatora 2. Stoi niżej, w siedmiu wierszach, i wygląda uspokajająco: średnia temperatura 82°C, próg ostrzegawczy 85°C, trzy stopnie zapasu, „w normie”. Każdy jego wiersz jest jednak błędny albo pusty. Przez cały wykład będziemy go poprawiać, wiersz po wierszu; poprawkę zobaczysz na końcu rozdziału, który ją uzasadnia.",
      "Ten wykład wprowadza zmienne ciągłe i rozkład normalny, ale jego prawdziwym tematem jest zmienność: dlaczego bez miary rozrzutu nie da się odpowiedzieć na żadne pytanie o przekroczenie progu. Zaczynamy od wiersza 2, czyli od tego, co mówi średnia, a czego nie mówi."
    ),
    callout = list(
      label = "Dane Bananpolu",
      text = "Temperatura łożyska wentylatora: średnia 82°C, odchylenie standardowe 3°C, wewnętrzny próg ostrzegawczy 85°C. Jednostka obserwacji: pojedynczy pomiar w ustalonym trybie pracy; temperatura w °C. Udział pomiarów ponad progiem nie jest prawdopodobieństwem co najmniej jednego przekroczenia w całej zmianie. Próg jest demonstracyjny, a liczby fikcyjne.",
      color = "uwaga"
    ),
    # Raport P21 (przyjęty 17.11.2026): zła wersja szablonu raportu, oś całego wykładu
    body = list(z6p_get("z6p_21_ui")),
    sections = list(
      list(
        id = "rozrzut", title = "Średnia to za mało",
        body = list(
          c(
            "Pytanie z raportu brzmi: jak często łożysko pracuje w temperaturze wyższej niż 85°C? Średnia mówi tylko, gdzie leży środek zbioru pomiarów. Nie mówi, jak daleko od tego środka odchodzą pojedyncze wyniki — a przekroczenie progu to właśnie sprawa pojedynczych wyników, nie średniej. Łożysko, którego temperatura trzyma się w przedziale 81–83°C, i łożysko, które skacze między 75 a 90°C, mogą mieć identyczną średnią.",
            "Zanim zaczniemy liczyć, zdecyduj, co można powiedzieć o ryzyku, znając tylko średnią i próg."
          ),
          risk_vote_panel("z6_vote", "z6_vote_feedback", "Średnia temperatura wynosi 82°C, próg 85°C. Czy ryzyko jest pomijalne?", c("Tak" = "yes", "Nie wiadomo" = "sd", "Zawsze wynosi 50%" = "half"), correct = "sd",
            feedback = list(
              "sd" = lc_status(lc_verdict(tags$strong("Potrzebujemy σ:"), type = "ok"),
                               " przy σ=3°C przekroczenie dotyczy około 16% porównywalnych pomiarów."),
              ".other" = lc_status(lc_verdict(tags$strong("Potrzebujemy σ:"), type = "warning"),
                                   " przy σ=3°C przekroczenie dotyczy około 16% porównywalnych pomiarów.")
            )),
          c(
            "Odpowiedź „tak” zakłada, że wszystkie pomiary leżą blisko średniej. Odpowiedź „50%” myli średnią z progiem: połowa wyników leży powyżej średniej, a nie powyżej dowolnego progu. Poprawna odpowiedź jest mniej efektowna, ale uczciwa: z samych dwóch liczb, 82 i 85, nie da się wyznaczyć ryzyka. Brakuje trzeciej — miary rozrzutu, czyli odchylenia standardowego σ.",
            "Ile zmienia ta trzecia liczba? Poniższy przykład wyprzedza rachunek, który wyprowadzimy w rozdziale 3; na razie wystarczy zauważyć skalę różnic."
          ),
          # Scena P1 (przyjęta 17.11.2026): po głosowaniu, przed przykładem 6.1
          z6p_get("z6p_1_ui"),
          risk_example("6.1", "Trzy łożyska o tej samej średniej",
            problem = "Trzy łożyska mają średnią temperaturę 82°C i pracują pod tym samym progiem 85°C. Różnią się odchyleniem standardowym: σ = 1°C, 3°C i 5°C. Przyjmując model normalny, porównaj odsetek pomiarów ponad progiem.",
            steps = c(
              "Dla każdego łożyska liczymy, ile odchyleń standardowych dzieli próg od średniej: z = (85 - 82)/σ. Wychodzi z = 3, z = 1 i z = 0.6.",
              "Odsetek pomiarów ponad progiem odczytujemy z rozkładu normalnego (wzór (6.5), w R: 1 - pnorm(z)): dla z = 3 około 0.0013, dla z = 1 około 0.159, dla z = 0.6 około 0.274.",
              "W naturalnych częstościach: około 1, 159 i 274 pomiary na 1000."
            ),
            answer = "Przy tej samej średniej ryzyko przekroczenia zmienia się ponad dwustukrotnie — od około 1 do około 274 pomiarów na tysiąc. O wyniku decyduje rozrzut."
          ),
          risk_check("z6_chk_srednia",
            "Dwa łożyska mają tę samą średnią 82°C. Łożysko A ma σ = 1°C, łożysko B σ = 3°C. Które częściej przekracza próg 85°C?",
            c("A, bo ma mniejsze σ" = "a", "B, bo jego wyniki sięgają dalej od średniej" = "b", "Oba jednakowo, bo średnie są równe" = "same"),
            correct = "b",
            explanation = "Próg leży powyżej średniej, więc większy rozrzut przesuwa więcej wyników poza próg: dla B około 0.159, dla A około 0.0013 (przykład 6.1).",
            hints = c(a = "Mniejsze σ oznacza wyniki skupione bliżej 82°C. Czy to ułatwia, czy utrudnia dotarcie do 85°C?", same = "Równe średnie nie oznaczają równego ryzyka — to główna teza tego rozdziału.")
          )
        )
      ),
      list(
        id = "histogram", title = "Od histogramu do pola",
        body = list(
          "Histogram z wielu zmian przybliża kształt rozkładu, a prawdopodobieństwo przekroczenia jest polem — zobacz, jak obraz stabilizuje się wraz z liczbą obserwacji. To ciągły odpowiednik stabilizacji częstości z pierwszego wykładu: tam stabilizowała się jedna liczba, tutaj stabilizuje się cały kształt.",
          c(
            "Histogram w skali gęstości ma ważną własność: wysokość słupka pomnożona przez jego szerokość to odsetek obserwacji w danym przedziale, a pola wszystkich słupków sumują się do jedności. Odsetek pomiarów ponad progiem 85°C to więc łączne pole słupków na prawo od progu. Gdy obserwacji przybywa, a słupki się zwężają, schodkowy kontur histogramu zbliża się do gładkiej krzywej — i to tę krzywą będziemy dalej nazywać gęstością."
          ),
          risk_try("dopisuj pomiary po 10, potem po 100 i po 1000. Obserwuj, kiedy przestaje się zmieniać środek histogramu, a kiedy czerwona ścieżka odsetka ponad 85°C. Kliknij „Od nowa” kilka razy i porównaj ścieżki."),
          # Symulacja P2B (przyjęta 17.11.2026): zastępuje histogram z suwakiem liczebności
          z6p_get("z6p_2b_ui"),
          c(
            "Środek histogramu ustawia się wokół 82°C już po kilkudziesięciu pomiarach. Odsetek ponad progiem stabilizuje się znacznie wolniej: przy kilkudziesięciu pomiarach potrafi wynieść 10% albo 25%, przy kilkuset waha się jeszcze o kilka punktów procentowych, a do wartości 15.9%, którą daje model, zbliża się dopiero przy tysiącach. Ponad 91°C model przewiduje około 1 pomiaru na 1000, więc w stu pomiarach zwykle nie ma ani jednego.",
            "Wniosek jest podwójny. Po pierwsze, model gęstości jest idealizacją histogramu z nieskończenie wielu pomiarów — dlatego prawdopodobieństwa w modelu to pola. Po drugie, ogon jest najsłabiej obsadzoną częścią danych: przy kilkudziesięciu pomiarach odsetek przekroczeń może się mylić o kilka punktów procentowych, a o przekroczeniach dalekich progów mała próba nie mówi prawie nic."
          )
        )
      )
    )
  ),
  list(
    id = "ciagla", title = "Rozkład normalny", hook = "Dokładnie tej temperatury nie będzie nigdy",
    lead = "Temperatura nie jest liczbą zdarzeń — wymaga gęstości, a rozkład normalny opisują dwa parametry: μ przesuwa środek, σ rozszerza lub zwęża krzywą.",
    intro = c(
      "Wiersz 2 raportu poprawiliśmy liczbą 159 na 1000 pomiarów. Ten rozdział buduje model, z którego taką liczbę się liczy: rozkład ciągły i krzywą normalną.",
      "W wykładach o próbach zmienne losowe zliczały zdarzenia: zero, jedna, dwie wady. Temperatura łożyska nie zlicza niczego — może wynieść 82.1°C, 82.14°C albo dowolną wartość pomiędzy. To wymusza zmianę narzędzi: zamiast prawdopodobieństw pojedynczych wartości pracujemy z gęstością, a prawdopodobieństwa czytamy z pól pod krzywą."),
    sections = list(
      list(
        id = "kontrast", title = "Dwa rodzaje zmiennych",
        text = "Zmienna dyskretna, jak liczba niesprawnych czujników z wykładów 04–05, przyjmuje policzalne wartości i każdej z nich można przypisać dodatnie prawdopodobieństwo. Zmienna ciągła, jak temperatura łożyska, może przyjąć dowolną wartość z przedziału — wyników jest nieprzeliczalnie wiele.",
        body = list(
          "W rozkładzie dwumianowym czy geometrycznym prawdopodobieństwa pojedynczych wartości sumowały się do jedności — mogliśmy narysować słupek nad każdą wartością. Przy temperaturze to niemożliwe: gdyby każda z nieprzeliczalnie wielu wartości miała dodatnie prawdopodobieństwo, suma byłaby nieskończona. Rolę słupków przejmuje więc krzywa, a rolę sumy — pole pod nią.",
          risk_definition("6.1", "Zmienna ciągła i gęstość", c(
            "Zmienna losowa X jest ciągła, jeśli istnieje funkcja f(x) ≥ 0, zwana gęstością, taka że prawdopodobieństwo trafienia X do dowolnego przedziału jest polem pod wykresem f nad tym przedziałem. Całe pole pod gęstością wynosi 1.",
            "Gęstość nie jest prawdopodobieństwem. Jej wartość mówi, jak gęsto wyniki skupiają się w okolicy danego punktu; prawdopodobieństwo powstaje dopiero po pomnożeniu przez szerokość przedziału."
          ))
        )
      ),
      list(
        id = "zero", title = "Dlaczego P(X=x)=0",
        text = "Dla zmiennej ciągłej prawdopodobieństwo trafienia dokładnie jednej wartości wynosi zero: pojedynczy punkt nie ma szerokości, więc pole nad nim znika. Sens mają dopiero prawdopodobieństwa przedziałów i przekroczeń, liczone jako pole pod krzywą gęstości.",
        body = list(
          risk_formula("P(a<X\\le b)=⟦1|\\int_a^b⟧ ⟦2|f(x)⟧\\,dx,\\qquad ⟦3|P(X=x)=0⟧", num = "6.1",
            notes = list("pole pod krzywą między granicami a i b", "gęstość: wysokość krzywej nad punktem x", "punkt nie ma szerokości, więc pole nad nim wynosi 0"),
            note_colors = c("accent", "niebo", "terakota")),
          # Pojęcie P3 (przyjęte 17.11.2026): pole między dwiema kreskami
          z6p_get("z6p_3_ui"),
          c(
            "Z P(X = x) = 0 wynika praktyczna wygoda: dla zmiennej ciągłej nie ma znaczenia, czy piszemy X > 85, czy X ≥ 85, ani czy przedział jest domknięty, czy otwarty. W rozkładach dyskretnych z poprzednich wykładów ta różnica była istotna — P(X ≤ 3) i P(X < 3) różniły się o słupek P(X = 3).",
            "Pola pod gęstością rzadko liczymy przez całkowanie. Wygodniej jest raz stablicować pole od lewego końca do każdego punktu — to dystrybuanta, znana z wykładu 04, tym razem dla zmiennej ciągłej. Każde pole przedziału to wtedy różnica dwóch jej wartości."
          ),
          risk_definition("6.2", "Dystrybuanta", c(
            "Dystrybuantą zmiennej X nazywamy funkcję F(x) = P(X ≤ x), czyli pole pod gęstością na lewo od punktu x. Dystrybuanta rośnie od 0 do 1; prawdopodobieństwo przekroczenia progu c to 1 - F(c)."
          )),
          risk_formula("\\begin{gathered}⟦1|F(x)=P(X\\le x)⟧\\\\ P(a<X\\le b)=⟦2|F(b)-F(a)⟧,\\qquad P(X>c)=⟦3|1-F(c)⟧\\end{gathered}", num = "6.2",
            notes = list("dystrybuanta: pole na lewo od x", "pole między a i b: różnica pól lewych", "przekroczenie progu c: dopełnienie do 1"),
            note_colors = c("niebo", "szalwia", "accent"), above = 1),
          risk_example("6.2", "Temperatura w przedziale roboczym",
            problem = "Dla T ~ N(82, 3) dystrybuanta w punktach 80°C i 85°C wynosi F(80) ≈ 0.252 i F(85) ≈ 0.841 (w R: pnorm(80, 82, 3) i pnorm(85, 82, 3)). Oblicz prawdopodobieństwo, że pomiar wypadnie między 80 a 85°C, oraz że przekroczy 85°C. Czy odpowiedź zmieni się, jeśli zapytamy o przedział domknięty 80–85°C?",
            steps = c(
              "Ze wzoru (6.2): P(80 < T ≤ 85) = F(85) - F(80) ≈ 0.841 - 0.252 = 0.589.",
              "P(T > 85) = 1 - F(85) ≈ 1 - 0.841 = 0.159.",
              "Z (6.1) P(T = 80) = P(T = 85) = 0, więc dołączenie końców przedziału nic nie zmienia: P(80 ≤ T ≤ 85) = 0.589."
            ),
            answer = "Około 59% pomiarów leży w przedziale 80–85°C, około 16% ponad 85°C; domknięcie przedziału nie zmienia wyników."
          ),
          "Warto jeszcze zobaczyć, dlaczego gęstość nie jest prawdopodobieństwem. Dla T ~ N(82, 3) gęstość w punkcie 82°C wynosi około 0.133 na stopień. Prawdopodobieństwo, że pomiar trafi w wąski przedział 81.95–82.05°C o szerokości 0.1°C, to w przybliżeniu 0.133 · 0.1 ≈ 0.0133. Gdy zwężamy przedział do zera, to prawdopodobieństwo znika — zgodnie z (6.1). Z kolei dla bardzo wąskiego rozkładu z σ = 0.25°C gęstość w środku przekracza 1.59 — liczbę, która jako prawdopodobieństwo byłaby niemożliwa.",
          risk_check("z6_chk_gestosc",
            "Dla T ~ N(82, 3) gęstość w punkcie 82°C wynosi około 0.133. Co to oznacza?",
            c("P(T = 82) ≈ 0.133" = "point", "Pomiar trafia w przedział 82 ± 0.05°C z prawdopodobieństwem około 0.133 · 0.1" = "interval", "13.3% pomiarów wynosi dokładnie 82°C" = "share"),
            correct = "interval",
            explanation = "Gęstość to prawdopodobieństwo na jednostkę szerokości. Pomnożona przez szerokość wąskiego przedziału (0.1°C) daje prawdopodobieństwo około 0.0133; dla pojedynczego punktu prawdopodobieństwo wynosi 0.",
            hints = c(point = "Dla zmiennej ciągłej P(T = x) = 0 — wzór (6.1). Gęstość wymaga pomnożenia przez szerokość.", share = "Żaden odsetek pomiarów nie wynosi „dokładnie” 82°C przy ciągłym pomiarze; to pytanie o punkt.")
          )
        ),
        pitfall = "Pytanie „jakie jest prawdopodobieństwo, że temperatura wyniesie dokładnie 85°C” nie ma użytecznej odpowiedzi; pytaj o przedział albo przekroczenie progu."
      ),
      list(
        id = "parametry", title = "Parametry μ i σ",
        body = list(
          "Wśród rozkładów ciągłych jeden odgrywa szczególną rolę. Gdy wynik powstaje jako suma wielu drobnych, niezależnych wpływów — wahań obciążenia, temperatury otoczenia, przepływu powietrza, tolerancji montażu — jego rozkład zbliża się do symetrycznej krzywej dzwonowej. W poprzednim wykładzie widzieliśmy to samo zjawisko przy rozkładzie ujemnym dwumianowym: suma wielu czasów oczekiwania z rosnącym r stawała się coraz bardziej symetryczna.",
          risk_definition("6.3", "Rozkład normalny", c(
            "Zmienna T ma rozkład normalny z parametrami μ i σ > 0, co w tym kursie zapisujemy T ~ N(μ, σ), jeśli jej gęstość dana jest wzorem (6.3). Średnia rozkładu wynosi μ, a odchylenie standardowe σ. Rozkład jest symetryczny względem μ i przyjmuje wartości z całej osi liczbowej."
          )),
          risk_formula("f(t)=⟦1|\\frac{1}{\\sigma\\sqrt{2\\pi}}⟧\\exp\\!\\left(-\\frac{⟦2|(t-\\mu)^2⟧}{⟦3|2\\sigma^2⟧}\\right)", num = "6.3",
            notes = list("stała: całe pole wynosi 1, więc większe σ to niższa krzywa", "kwadrat odległości od średniej μ: krzywa symetryczna, najwyższa w μ", "σ: szerokość krzywej, w jednostkach t"),
            note_colors = c("terakota", "niebo", "szalwia"), above = 1),
          c(
            "Rozkład normalny jest opisany dwiema liczbami o czytelnych rolach: μ mówi, gdzie leży środek, a σ — jak szeroko wyniki rozrzucają się wokół niego. Praktyczna linijka: około 68% wyników mieści się w przedziale μ±σ, około 95% w μ±2σ, a wyniki poza μ±3σ są rzadkością.",
            "Wzór (6.3) wygląda groźnie, ale jego treść jest prosta. Gęstość zależy od t tylko przez kwadrat odległości od średniej, mierzonej w jednostkach σ — dlatego krzywa jest symetryczna i najwyższa w μ. Czynnik przed wykładnikiem pilnuje, żeby całe pole wynosiło 1: im większe σ, tym krzywa szersza i jednocześnie niższa."
          ),
          risk_example("6.3", "Linijka 68–95–99.7 dla łożyska",
            problem = "Dla T ~ N(82, 3) wyznacz przedziały, w których leży około 68% i 95% pomiarów. Na tej podstawie oszacuj, bez tablic, odsetek pomiarów powyżej 85°C.",
            steps = c(
              "μ ± σ = 82 ± 3, czyli 79–85°C; dokładnie P(79 < T ≤ 85) ≈ 0.683.",
              "μ ± 2σ = 82 ± 6, czyli 76–88°C; dokładnie około 0.954. Poza μ ± 3σ = 73–91°C leży tylko około 0.27% pomiarów.",
              "Poza przedziałem 79–85°C leży około 1 - 0.683 = 0.317 pomiarów. Z symetrii połowa z nich jest powyżej 85°C: 0.317/2 ≈ 0.159."
            ),
            answer = "68% pomiarów w 79–85°C, 95% w 76–88°C; powyżej 85°C około 0.159 — ta sama liczba, którą dał przykład 6.1 dla σ = 3°C."
          ),
          # Pojęcie P5 (przyjęte 17.11.2026): linijka σ
          z6p_get("z6p_5_ui"),
          "Pobaw się suwakami i obserwuj wskaźnik z dla progu 85°C. Zauważ, że tę samą odległość od progu można osiągnąć chłodzeniem (mniejsze μ) albo stabilizacją pracy (mniejsze σ) — rozróżnienie, które wróci przy decyzjach.",
          risk_formula("T\\sim N(⟦1|\\mu⟧,⟦2|\\sigma⟧),\\qquad ⟦3|z=\\frac{t-\\mu}{\\sigma}⟧",
            notes = list("średnia: suwak μ przesuwa krzywą", "odchylenie standardowe: suwak σ zwęża lub poszerza krzywą", "wynik z: ile odchyleń σ dzieli próg t od średniej"),
            note_colors = c("niebo", "szalwia", "accent")),
          risk_try("zacznij od μ = 82°C i σ = 3°C (z = 1). Najpierw obniż μ do 80°C, potem wróć do 82°C i zmniejsz σ do 2°C. Zapisz z w obu przypadkach i zwróć uwagę, jak zmienia się wysokość krzywej przy zmianie σ."),
          # Piaskownica P4 (przyjęta 17.11.2026): zastępuje dawny panel „Model”
          z6p_get("z6p_4_ui"),
          c(
            "Obniżenie średniej do 80°C przesuwa całą krzywą w lewo bez zmiany kształtu i daje z = 5/3 ≈ 1.67. Zmniejszenie σ do 2°C zostawia środek w miejscu, ale krzywa staje się węższa i wyższa, a z rośnie do 1.5. W obu przypadkach próg „odsuwa się” od środka rozkładu, choć tylko w pierwszym zmieniła się temperatura, w której łożysko pracuje przeciętnie.",
            "Przy μ = 85°C wskaźnik z spada do zera: próg leży dokładnie w środku rozkładu i przekracza go połowa pomiarów, niezależnie od σ. Przy μ > 85°C wskaźnik staje się ujemny — przekroczenia są wtedy normą, a nie wyjątkiem."
          )
        )
      ),
      list(
        id = "konwencja", title = "Konwencja zapisu",
        text = "W tym kursie zapis T~N(μ, σ) oznacza, że drugim parametrem jest odchylenie standardowe σ. W wielu podręcznikach ten sam rozkład zapisuje się jako N(μ, σ²) z wariancją na drugim miejscu — przed podstawieniem liczb zawsze sprawdź, którą konwencję przyjmuje źródło.",
        body = list(
          "Konwencja z odchyleniem standardowym ma praktyczną przewagę: σ ma te same jednostki co pomiar (°C), więc można je bezpośrednio porównywać z odległością do progu. Funkcje R też jej używają: pnorm(85, mean = 82, sd = 3). Wariancja ma jednostki do kwadratu (°C²) i nie da się jej wprost odłożyć na osi temperatury.",
          risk_check("z6_chk_konwencja",
            "Podręcznik stosujący konwencję wariancyjną podaje T ~ N(82, 9). Jakie jest σ w zapisie tego kursu?",
            c("σ = 9°C" = "nine", "σ = 3°C" = "three", "σ = 81°C" = "eightyone"),
            correct = "three",
            explanation = "W konwencji N(μ, σ²) liczba 9 to wariancja, więc σ = √9 = 3°C i w zapisie kursu jest to T ~ N(82, 3). Potraktowanie 9 jako σ dałoby P(T > 85) ≈ 0.369 zamiast 0.159.",
            hints = c(nine = "W konwencji wariancyjnej drugi parametr to σ², nie σ.", eightyone = "Podnosisz do kwadratu zamiast wyciągać pierwiastek.")
          )
        )
      )
    )
  ),
  list(
    id = "standaryzacja", title = "Standaryzacja", hook = "Każdy pomiar da się przyłożyć do jednej linijki",
    # Poprawka raportu z rozdziału 1 (przyjęta 17.11.2026)
    widget = z6p_21_fix(3),
    lead = "Standaryzacja mówi, ile odchyleń standardowych dzieli wynik od średniej; próg dzieli rozkład na wyniki akceptowalne i przekroczenia.",
    intro = c(
      "Ten rozdział poprawia dwa wiersze raportu: okres odniesienia („16% pomiarów, więc 16% zmian”) i źródło ryzyka („pojedyncze, przypadkowe gorące odczyty”).",
      "Czy 85°C przy średniej 82°C i σ = 3°C to dużo? A 62 bary ciśnienia przy średniej 56 i σ = 2? Porównanie surowych liczb z różnych światów jest niemożliwe — dopóki obu nie przełożymy na wspólną jednostkę: liczbę odchyleń standardowych od średniej.",
      "Dla progu łożyska z = (85-82)/3 = 1: próg leży jedno odchylenie nad średnią, co w modelu normalnym oznacza około 16% przekroczeń. Dla ciśnienia z = 3 — przekroczenia są rzadkością. Standaryzacja porządkuje priorytety, zanim padnie jakakolwiek decyzja."
    ),
    sections = list(
      list(
        id = "jednostki", title = "Wspólna linijka z",
        text = "Po standaryzacji można porównywać temperaturę, ciśnienie i drgania, ale tylko w ramach sensownego modelu. Wynik z = 1 znaczy „jedno odchylenie nad średnią” zawsze; przełożenie tego na prawdopodobieństwo wymaga już założenia o kształcie rozkładu.",
        body = list(
          risk_definition("6.4", "Standaryzacja", c(
            "Standaryzacją wartości x zmiennej o średniej μ i odchyleniu standardowym σ nazywamy przekształcenie (6.4). Wynik z mówi, o ile odchyleń standardowych x leży powyżej (z > 0) lub poniżej (z < 0) średniej. Jeśli T ~ N(μ, σ), to Z = (T - μ)/σ ma standardowy rozkład normalny N(0, 1)."
          )),
          risk_formula("⟦1|z⟧=(⟦2|x-\\mu⟧)/⟦3|\\sigma⟧", num = "6.4",
            notes = list("ile odchyleń standardowych dzieli x od średniej; bez jednostki", "odległość od średniej w jednostkach pomiaru, np. w °C", "odchylenie standardowe: linijka do mierzenia odległości"),
            note_colors = c("accent", "niebo", "szalwia")),
          risk_derivation("dlaczego Z ma rozkład N(0, 1)", c(
            "Odjęcie stałej μ przesuwa cały rozkład, nie zmieniając jego kształtu ani rozrzutu: średnia spada do zera, odchylenie zostaje σ. Podzielenie przez σ zmienia skalę: odchylenie standardowe dzieli się przez σ i wynosi 1. Przekształcenie liniowe nie psuje kształtu krzywej dzwonowej, więc wynik nadal jest normalny.",
            "Dzięki temu jedna tablica, albo jedna funkcja Φ, wystarcza dla wszystkich rozkładów normalnych — wystarczy przeliczyć próg na z."
          ), lines = c("E(T - μ) = μ - μ = 0", "SD(T - μ) = σ", "SD((T - μ)/σ) = σ/σ = 1", "⇒ Z = (T - μ)/σ ~ N(0, 1)")),
          risk_example("6.4", "Temperatura czy ciśnienie?",
            problem = "W dojrzewalni monitoruje się temperaturę łożyska T ~ N(82, 3) z progiem 85°C oraz ciśnienie w instalacji chłodniczej P ~ N(56, 2) z progiem 62 bar. Które przekroczenie jest częstsze i ile razy?",
            steps = c(
              "Temperatura: z = (85 - 82)/3 = 1. Ciśnienie: z = (62 - 56)/2 = 3.",
              "Ze wzoru (6.5): P(T > 85) = 1 - Φ(1) ≈ 0.159; P(P > 62) = 1 - Φ(3) ≈ 0.00135.",
              "Stosunek: 0.159/0.00135 ≈ 118."
            ),
            answer = "Przekroczenie progu temperatury jest około 118 razy częstsze, choć w surowych liczbach zapas ciśnienia (6 bar) i temperatury (3°C) są nieporównywalne. O priorytecie decyduje z, nie surowa różnica."
          ),
          # Pojęcie P6 (przyjęte 17.11.2026): temperatura i ciśnienie na jednej linijce
          z6p_get("z6p_6_ui"),
          risk_check("z6_chk_z",
            "Czujnik drgań ma średnią 4.0 mm/s, σ = 0.5 mm/s i próg 5.0 mm/s. Gdzie leży ten próg w porównaniu z progiem temperatury łożyska (z = 1)?",
            c("Dalej od średniej: z = 2" = "further", "Bliżej średniej: z = 0.5" = "closer", "Nie da się porównać różnych jednostek" = "none"),
            correct = "further",
            explanation = "z = (5.0 - 4.0)/0.5 = 2. Próg drgań leży dwa odchylenia nad średnią, dalej niż próg temperatury; w modelu normalnym przekroczenie jest rzadsze (około 0.023 wobec 0.159).",
            hints = c(closer = "Odległość 1.0 mm/s trzeba podzielić przez σ = 0.5, a nie pomnożyć.", none = "Właśnie po to jest standaryzacja: z nie ma jednostek.")
          )
        )
      ),
      list(
        id = "ogon", title = "Ryzyko przekroczenia",
        text = c(
          "Wracamy do pytania z głosowania, tym razem z pełnym warsztatem. Prawdopodobieństwo przekroczenia progu to pole pod gęstością na prawo od progu — dla T~N(82, 3) i progu 85°C około 0.16. W naturalnych częstościach: mniej więcej 159 na 1000 porównywalnych pomiarów.",
          "Zanim zapiszemy to wzorem, pobaw się progiem i obserwuj, jak pole reaguje nieliniowo: w okolicy średniej każda zmiana progu o pół stopnia silnie zmienia wynik, a daleko w ogonie te same pół stopnia znaczy niewiele. Ta nieliniowość to znak rozpoznawczy ogonów rozkładu normalnego."
        ),
        body = list(
          risk_try("zacznij od progu 82°C (równego średniej) i przesuń go na 82.5°C. Zanotuj zmianę P(T > c). Potem zrób ten sam krok z 91°C na 91.5°C i porównaj obie zmiany."),
          # Piaskownica P7 (przyjęta 17.11.2026): zastępuje dawny panel „Ogon”
          z6p_get("z6p_7_ui"),
          c(
            "Przy progu 82°C panel pokazuje 0.500, a przy 82.5°C — 0.434: pół stopnia zabrało 6.6 punktu procentowego. Między 91 a 91.5°C prawdopodobieństwo spada z 0.00135 do 0.00077, czyli o niecałe 0.06 punktu procentowego. W liczbach bezwzględnych daleki ogon jest mało wrażliwy na próg. W liczbach względnych jest odwrotnie: te same pół stopnia zmniejsza ryzyko o ponad 40%. Która skala jest ważniejsza, zależy od tego, czy pytamy o liczbę przekroczeń, czy o rząd wielkości rzadkiego zdarzenia."
          ),
          "To, co robił suwak — odcinał pole na prawo od progu — zapisujemy jedną linijką, korzystając ze standaryzacji:",
          risk_formula("⟦1|P(T>c)⟧=1-⟦2|\\Phi⟧\\!\\left(⟦3|\\frac{c-\\mu}{\\sigma}⟧\\right)", num = "6.5",
            notes = list("prawy ogon: szansa przekroczenia progu c", "dystrybuanta N(0, 1): pole na lewo", "wynik z progu c, wzór (6.4)"),
            note_colors = c("accent", "wrzos", "indygo")),
          "Φ jest dystrybuantą standardowego rozkładu normalnego, a (c-μ)/σ to wynik z progu — odległość od średniej we wspólnej linijce odchyleń.",
          risk_definition("6.5", "Kwantyl rozkładu normalnego", c(
            "Kwantylem rzędu α zmiennej T nazywamy taką wartość t_α, że P(T ≤ t_α) = α. Dla standardowego rozkładu normalnego oznaczamy go z_α (w R: qnorm(α)); na przykład z₀,₉₅ ≈ 1.645, z₀,₉₉ ≈ 2.326, z₀,₉₉₉ ≈ 3.090. Z symetrii Φ(-z) = 1 - Φ(z)."
          )),
          "Wzór (6.5) odpowiada na pytanie „jakie ryzyko przy danym progu?”. Często pytanie jest odwrotne: przy jakim progu ryzyko spadnie do uzgodnionego poziomu? Odpowiedzią jest kwantyl, który z definicji 6.5 i wzoru (6.4) dostajemy, cofając standaryzację:",
          risk_formula("⟦1|t_{\\alpha}⟧=⟦2|\\mu⟧+⟦3|z_{\\alpha}⟧\\,⟦4|\\sigma⟧", num = "6.6",
            notes = list("próg, którego nie przekracza odsetek α pomiarów", "start: średnia", "ile odchyleń w górę: kwantyl N(0, 1)", "długość jednego odchylenia w jednostkach pomiaru"),
            note_colors = c("indygo", "niebo", "accent", "szalwia"), above = c(1, 3)),
          risk_example("6.5", "Ogon łożyska w liczbach",
            problem = "Dla T ~ N(82, 3) oblicz P(T > 85), P(T > 88) i P(T > 90) wraz z naturalnymi częstościami. Następnie wyznacz temperaturę, którą przekracza tylko 1% pomiarów.",
            steps = c(
              "z odpowiednio: 1, 2 i 8/3 ≈ 2.67.",
              "Ze wzoru (6.5): P(T > 85) ≈ 0.159 (159 na 1000), P(T > 88) ≈ 0.0228 (23 na 1000), P(T > 90) ≈ 0.0038 (4 na 1000).",
              "Temperatura przekraczana przez 1% pomiarów to kwantyl rzędu 0.99. Ze wzoru (6.6): t₀,₉₉ = 82 + 2.326 · 3 ≈ 88.98°C."
            ),
            answer = "Około 159, 23 i 4 pomiary na tysiąc; 1% pomiarów przekracza około 89.0°C. Przesunięcie progu z 85 do 88°C zmniejsza częstość przekroczeń prawie siedmiokrotnie."
          )
        ),
        takeaway = "Wynik progowy zawsze raportuj podwójnie: jako pole ogona i jako naturalną częstość w ustalonym okresie odniesienia. „P = 0.16” i „około 159 pomiarów na 1000” to ta sama liczba, ale tylko druga wersja uruchamia wyobraźnię decydenta."
      ),
      list(
        id = "horyzont", title = "Pomiar a zmiana robocza",
        body = list(
          "Liczba 0.159 dotyczy jednego losowego pomiaru. Kierownik zmiany pyta jednak o co innego — ostrzega o tym karta danych Bananpolu. Te dwa pytania brzmią podobnie, ale dotyczą różnych zmiennych:",
          lc_note("Dwa pytania",
            tags$p(tags$strong("O pomiar:"), " jak często pojedynczy pomiar przekracza 85°C? Odpowiedź już znamy: 0.159."),
            tags$p(tags$strong("O zmianę:"), " jak często w zmianie pojawia się co najmniej jeden alarm? To pytanie o maksimum ośmiu pomiarów, a nie o jeden pomiar.")),
          c(
            "Najprostszy model łączy ten wykład z wykładem 04. Jeśli w zmianie wykonuje się n pomiarów, każdy przekracza próg z prawdopodobieństwem p, a pomiary są niezależne, to liczba przekroczeń ma rozkład dwumianowy, a zmiana bez alarmu wymaga n „porażek” z rzędu."
          ),
          # Scena P8 (przyjęta 17.11.2026): dzienniki zmian, pytanie o pomiar / o zmianę / oba
          z6p_get("z6p_8_ui"),
          risk_formula("⟦1|P(\\text{co najmniej jedno przekroczenie w zmianie})⟧=1-⟦2|(1-p)^{n}⟧", num = "6.7",
            notes = list("zmiana z choć jednym pomiarem powyżej progu", "wszystkie n pomiarów zmiany poniżej progu; p z wzoru (6.5)"),
            note_colors = c("accent", "indygo")),
          risk_example("6.6", "Osiem pomiarów na zmianę",
            problem = "Temperatura łożyska jest zapisywana co godzinę, osiem razy na zmianę. Przyjmij p = 0.159 i niezależność pomiarów. Jaki odsetek zmian ma co najmniej jedno przekroczenie? Porównaj z czterema pomiarami na zmianę.",
            steps = c(
              "Ze wzoru (6.7) dla n = 8: 1 - 0.841⁸ ≈ 1 - 0.251 = 0.749.",
              "Dla n = 4: 1 - 0.841⁴ ≈ 0.499."
            ),
            answer = "Przy niezależnych pomiarach około trzech zmian na cztery ma alarm, choć pojedynczy pomiar przekracza próg tylko w 16% przypadków. Częstsze pomiary „podnoszą” ryzyko w zmianie, bo pytanie dotyczy innej zmiennej."
          ),
          "Założenie niezależności jest tu najsłabszym ogniwem. Temperatura łożyska zmienia się powoli: jeśli o 10:00 było gorąco, o 11:00 zapewne też będzie. Pomiary dodatnio skorelowane dają mniej zmian z alarmem, niż mówi (6.7), bo przekroczenia skupiają się w tych samych zmianach. Wzór (6.7) jest więc górnym oszacowaniem dla pomiarów dodatnio skorelowanych — prawdziwa wartość leży między p a 1 - (1 - p)ⁿ.",
          # Przypadek brzegowy P8b (przyjęty 17.11.2026): pomiary skorelowane; scena P8 ma tylko niezależne
          z6p_get("z6p_8b_ui"),
          risk_check("z6_chk_horyzont",
            "Pojedynczy pomiar przekracza próg z prawdopodobieństwem 0.159. W zmianie jest 8 niezależnych pomiarów. Jaki odsetek zmian ma co najmniej jedno przekroczenie?",
            c("0.159" = "single", "około 0.75" = "shift", "8 · 0.159 ≈ 1.27" = "sum"),
            correct = "shift",
            explanation = "1 - 0.841⁸ ≈ 0.749 — wzór (6.7). Pytanie o zmianę dotyczy innej zmiennej niż pytanie o pomiar.",
            hints = c(single = "To prawdopodobieństwo dla jednego pomiaru, a pytanie dotyczy ośmiu.", sum = "Prawdopodobieństwo nie może przekroczyć 1. Policz przez zdarzenie przeciwne: osiem pomiarów bez przekroczenia.")
          )
        )
      )
    )
  ),
  list(
    id = "dzialania", title = "Hierarchia interwencji", hook = "Schłodzić, uspokoić albo przesunąć próg",
    # Poprawka raportu z rozdziału 1 (przyjęta 17.11.2026)
    widget = z6p_21_fix(4),
    lead = "Chłodzenie przesuwa średnią, stabilizacja zwęża rozkład, zmiana progu przesuwa granicę.",
    intro = c(
      "Wiersz 6 raportu proponuje podnieść próg do 87°C, bo wtedy przekroczeń będzie mniej. Ten rozdział porównuje to z chłodzeniem i stabilizacją i pokazuje, czym te działania naprawdę się różnią.",
      "Gdy górny próg leży powyżej średniej, pole ogona można zmniejszyć przez obniżenie średniej lub ograniczenie zmienności. Zmniejszenie σ przy progu poniżej średniej zwiększa P(T>c), a przy progu równym średniej pozostawia 0.5. Zmiana progu zmienia samo zdarzenie. Fizycznie to trzy zupełnie różne interwencje — lepsze chłodzenie, wyrównanie obciążenia i warunków pracy albo decyzja konstrukcyjna o nowej granicy.",
      "Porównaj skuteczność konkretnych interwencji względem stanu bazowego μ=82°C, σ=3°C, c=85°C. Wynik zależy zarówno od punktu wyjścia, jak i wielkości zmiany parametrów; nie ma uniwersalnego rankingu chłodzenia i stabilizacji."
    ),
    sections = list(
      list(
        id = "mechanizm", title = "Wszystko przez z",
        body = list(
          c(
            "Wzór (6.5) pokazuje, że dla rozkładu normalnego ryzyko zależy od trzech parametrów tylko przez jedną liczbę: z = (c - μ)/σ. Im większe z, tym mniejsze P(T > c). Każde działanie na ogonie jest więc sposobem na zwiększenie z — i każde robi to inną drogą: chłodzenie zmniejsza μ w liczniku, stabilizacja zmniejsza σ w mianowniku, a podniesienie progu zwiększa c w liczniku.",
            "Ta obserwacja pozwala porównywać interwencje na papierze, zanim wyda się pieniądze. Ale z nie mówi, która interwencja jest tańsza ani która zmienia fizykę łożyska; to trzeba dołożyć z wiedzy inżynierskiej."
          ),
          risk_example("6.7", "Trzy interwencje dla łożyska",
            problem = list(
              "Stan bazowy: μ = 82°C, σ = 3°C, c = 85°C. Porównaj P(T > c) po:",
              risk_parts(
                "Chłodzeniu o 2°C.",
                "Stabilizacji zmniejszającej σ o 1°C.",
                "Podniesieniu progu o 2°C.",
                "Jednoczesnym chłodzeniu i stabilizacji."
              )
            ),
            steps = c(
              "Bazowo z = 1, P ≈ 0.159. Po chłodzeniu μ = 80: z = 5/3 ≈ 1.67, P ≈ 0.048.",
              "σ = 2: z = 3/2 = 1.5, P ≈ 0.067.",
              "c = 87: z = 5/3 ≈ 1.67, P ≈ 0.048 — dokładnie tyle co w podpunkcie a).",
              "μ = 80, σ = 2: z = 2.5, P ≈ 0.0062. Ile stabilizacji odpowiada chłodzeniu o 2°C? Trzeba z = 5/3, czyli σ = 3/(5/3) = 1.8°C — zmniejszenie o 1.2°C."
            ),
            steps_type = "a",
            answer = "Chłodzenie o 2°C daje ryzyko 0.048 (48 na 1000), stabilizacja o 1°C — 0.067 (67 na 1000), podniesienie progu o 2°C — również 0.048. Połączenie chłodzenia i stabilizacji zmniejsza ryzyko ponad 25-krotnie, do około 6 na 1000."
          ),
          risk_try("przełączaj kolejno „Stan bazowy”, „Chłodzenie”, „Stabilizacja”, „Wyższy próg” i „Chłodzenie i stabilizacja”. Porównaj P(T > c) z wynikami przykładu 6.7 i zwróć uwagę, które dwie opcje dają ten sam wynik — oraz co się wtedy rusza na rysunku: krzywa czy kreska progu."),
          # Galeria P9A (przyjęta 17.11.2026): zastępuje dawny panel z listą wyboru
          z6p_get("z6p_9a_ui"),
          c(
            "Galeria pokazuje 0.159 dla stanu bazowego, 0.048 dla chłodzenia, 0.067 dla stabilizacji, znowu 0.048 dla wyższego progu i około 0.006 dla chłodzenia ze stabilizacją. W tym punkcie wyjścia chłodzenie o 2°C wygrywa ze stabilizacją o 1°C — ale to wynik tej konkretnej pary liczb. Stabilizacja o 1.5°C (σ = 1.5) dałaby z = 2 i P ≈ 0.023, czyli lepiej niż chłodzenie.",
            "Najbardziej pouczająca jest zbieżność chłodzenia i podniesienia progu. Statystycznie są nieodróżnialne: obie zwiększają z do 5/3. Fizycznie są zupełnie różne — po chłodzeniu łożysko naprawdę pracuje w niższej temperaturze, a po zmianie progu pracuje dokładnie tak samo jak przedtem."
          ),
          risk_check("z6_chk_prog",
            "Chłodzenie o 2°C i podniesienie progu o 2°C dają to samo P(T > c) ≈ 0.048. Czy te działania są równoważne z punktu widzenia bezpieczeństwa łożyska?",
            c("Tak, bo mają to samo z" = "same", "Nie — tylko chłodzenie zmienia temperaturę pracy łożyska" = "mechanism", "Tak, jeśli raport podaje naturalną częstość" = "report"),
            correct = "mechanism",
            explanation = "Rachunek (6.5) widzi tylko z. Łożysko widzi temperaturę: po podniesieniu progu rozkład temperatur jest taki sam jak przedtem, zmieniła się jedynie definicja przekroczenia.",
            hints = c(same = "Równe z oznacza równe prawdopodobieństwo zdarzenia „T > c”. Ale czy w obu przypadkach to jest to samo zdarzenie?", report = "Sposób raportowania nie zmienia fizyki. Co dzieje się z temperaturą łożyska po każdym z działań?")
          )
        )
      ),
      list(
        id = "hierarchia", title = "Kolejność działań",
        text = "Dwie pierwsze interwencje zmieniają mechanizm — po ich wdrożeniu instalacja naprawdę pracuje chłodniej albo stabilniej. Trzecia zmienia tylko definicję problemu: przekroczeń „ubywa”, choć fizycznie nic się nie poprawiło. Podniesienie progu bywa zasadne, ale wymaga dowodu konstrukcyjnego, że wyższa temperatura jest bezpieczna — nigdy samej potrzeby poprawienia statystyk.",
        body = list(
          "Stąd kolejność pytań przy każdym ryzyku progowym. Najpierw: czy da się przesunąć średnią? Potem: czy da się ograniczyć zmienność — ujednolicić obciążenie, warunki, surowce? Dopiero na końcu: czy próg został ustalony poprawnie? Ta ostatnia rewizja jest uprawniona, gdy próg wziął się z ostrożnej reguły kciuka, a badania materiałowe pokazują zapas. Nie jest uprawniona, gdy jedynym argumentem jest liczba alarmów w raporcie.",
          # Przykłady P17 (przyjęte 17.11.2026): działania z zakładu dla każdej grupy
          z6p_get("z6p_17_ui")
        )
      ),
      # Sekcja przeniesiona 17.11.2026 za „Kolejność działań”: przypadek brzegowy
      # pokazuje, gdzie kończy się zasada „najpierw średnia, potem zmienność”.
      list(
        id = "odwrotnie", title = "Kiedy stabilizacja nie pomaga",
        text = "Kolejność działań zakłada, że próg leży nad średnią, tak jak 85°C nad 82°C. Gdy próg leży pod średnią, stabilizacja działa odwrotnie: zwężenie rozkładu podnosi ryzyko, zamiast je obniżać.",
        body = list(
          risk_derivation("kiedy zmniejszenie σ pomaga", c(
            "Φ jest funkcją rosnącą, więc P(T > c) = 1 - Φ(z) maleje, gdy z rośnie. Zmniejszenie σ mnoży z przez liczbę większą od 1 — ale z może być dodatnie, ujemne albo zerowe, zależnie od znaku c - μ.",
            "Dla progu 80°C poniżej średniej 82°C zmniejszenie σ z 3 do 2°C podnosi P(T > 80) z 0.748 do 0.841. Zwężenie rozkładu zbliża wszystkie wyniki do średniej, a średnia leży po „złej” stronie progu."
          ), lines = c("c > μ: z > 0, mniejsze σ ⇒ większe z ⇒ mniejsze P(T > c)", "c = μ: z = 0 dla każdego σ ⇒ P(T > c) = 0.5", "c < μ: z < 0, mniejsze σ ⇒ bardziej ujemne z ⇒ większe P(T > c)")),
          # Przypadek brzegowy P10A (przyjęty 17.11.2026): próg poniżej średniej
          z6p_get("z6p_10a_ui")
        )
      )
    ),
    decision = "Raportuj pole ogona oraz naturalną częstość w ustalonym okresie odniesienia; najpierw redukuj źródło ryzyka, podniesienie progu wymaga uzasadnienia konstrukcyjnego."
  ),
  list(
    id = "obciazenie", title = "Obciążenie–wytrzymałość", hook = "Nie tylko ciężar się zmienia",
    # Poprawka raportu z rozdziału 1 (przyjęta 17.11.2026)
    widget = z6p_21_lina,
    lead = "Awaria zachodzi wtedy, gdy obciążenie L przekracza wytrzymałość S.",
    intro = c(
      "Raport z rozdziału 1 dotyczy łożyska, ale ten sam dział napisał drugi, o linie dźwigu w magazynie: „ryzyko zerwania około 50%”. Na końcu rozdziału poprawimy go na tym samym szablonie.",
      "W konstrukcjach i instalacjach granica bezpieczeństwa rzadko jest stałą: wytrzymałość liny zmienia się z partią i zużyciem, a obciążenie z ładunkiem i pogodą. Pytanie o awarię staje się pytaniem o wyścig dwóch zmiennych losowych.",
      "Najpierw obejrzyj ten wyścig na przykładzie liny dźwigu: każde podniesienie to jedna para ładunek–lina, a przekątna dzieli pary na bezpieczne i awarie."
    ),
    sections = list(
      list(id = "most", title = "Próg też bywa zmienny", text = "Dotąd próg był stałą konstrukcyjną, a zmienna była tylko temperatura. Teraz sam próg — wytrzymałość S — również jest zmienną losową, więc porównujemy dwie zmienne naraz i pytamy o prawdopodobieństwo, że jedna przewyższy drugą.",
        # Scena P11 (przyjęta 17.11.2026): lina dźwigu, zastępuje dawną chmurę par z suwakami
        body = list(z6p_get("z6p_11_ui"),
          # Przykłady P18 (przyjęte 17.11.2026): inne sytuacje obciążenie–wytrzymałość
          z6p_get("z6p_18_ui"))),
      list(
        id = "symulacja", title = "Chmura par L i S",
        body = list(
          "W scenie obciążenie L ma rozkład normalny N(85, 8) kN, a wytrzymałość liny S — N(95, 7) kN. Obie zmienne są losowane niezależnie. Każda para (L, S) to jedno zdarzenie: konkretny ładunek trafił na konkretny egzemplarz liny.",
          c(
            "Przy tych parametrach model daje P(L>S) ≈ 0.173, czyli około 173 podniesień na 1000. Przy kilkudziesięciu podniesieniach odsetek par pod przekątną wyraźnie waha się wokół tej wartości, przy tysiącach zbliża się do niej. Gdyby średnie były równe, awarią kończyłaby się połowa par: P(L>S) = 0.5. Podniesienie średniej wytrzymałości do 105 kN zmniejszyłoby ryzyko do około 0.030.",
            "Zwróć uwagę, że chmura jest ukośną elipsą dopiero wtedy, gdy L i S są skorelowane; tutaj jest zbliżona do okręgu, bo losujemy je niezależnie. O ryzyku decyduje to, jaka część tej chmury leży po złej stronie przekątnej."
          ),
          "Chmura punktów podpowiada właściwą miarę ryzyka: to udział par poniżej przekątnej, czyli prawdopodobieństwo, że różnica D = S - L wypadnie ujemna:",
          risk_formula("P(\\text{awarii})=P(⟦1|L⟧>⟦2|S⟧)=P(⟦3|D⟧<0),\\qquad ⟦3|D⟧=S-L", num = "6.8",
            notes = list("obciążenie", "wytrzymałość", "zapas bezpieczeństwa (margines) w danej parze; awaria, gdy spada poniżej zera"),
            note_colors = c("bursztyn", "niebo", "terakota")),
          "Dla niezależnych rozkładów normalnych różnica D też jest normalna: o średniej μ_S - μ_L i wariancji σ_S² + σ_L². Rachunek progowy z poprzednich rozdziałów stosuje się wtedy do D i progu zero."
        )
      ),
      list(
        id = "roznica", title = "Różnica dwóch zmiennych normalnych",
        body = list(
          risk_definition("6.6", "Model obciążenie–wytrzymałość", c(
            "W modelu obciążenie–wytrzymałość awaria zachodzi, gdy losowe obciążenie L przekroczy losową wytrzymałość S. Prawdopodobieństwo awarii to P(L > S) = P(D < 0), gdzie D = S - L jest marginesem bezpieczeństwa.",
            "Iloraz średniej marginesu przez jego odchylenie standardowe, β = μ_D/σ_D, nazywamy indeksem niezawodności: mówi, o ile odchyleń standardowych zero leży poniżej średniego marginesu."
          )),
          risk_formula("\\mu_D=⟦1|\\mu_S-\\mu_L⟧,\\qquad \\sigma_D=\\sqrt{⟦2|\\sigma_S^{2}⟧+⟦3|\\sigma_L^{2}⟧}", num = "6.9",
            notes = list("średni margines: średnia wytrzymałość minus średnie obciążenie", "wariancja wytrzymałości", "wariancja obciążenia: dodaje się, choć odejmujemy (L i S niezależne)"),
            note_colors = c("terakota", "niebo", "bursztyn")),
          risk_derivation("dlaczego wariancje się dodają, choć odejmujemy", c(
            "Średnia różnicy to różnica średnich — to intuicyjne. Rozrzut różnicy nie może jednak maleć przez odejmowanie: niepewność co do L i niepewność co do S są dwoma niezależnymi źródłami wahań marginesu i obie go „rozmywają”.",
            "Formalnie wariancja sumy niezależnych zmiennych to suma wariancji, a Var(-L) = (-1)² · Var(L) = Var(L). Gdy L i S są skorelowane ze współczynnikiem ρ, dochodzi składnik -2ρσ_Sσ_L: dodatnia korelacja (na przykład oba zależą od temperatury otoczenia) zmniejsza rozrzut marginesu."
          ), lines = c("D = S + (-L)", "Var(D) = Var(S) + Var(-L) = σ_S² + σ_L²", "przy korelacji: Var(D) = σ_S² + σ_L² - 2ρ·σ_S·σ_L")),
          "Skoro D ~ N(μ_D, σ_D), prawdopodobieństwo awarii dostajemy ze wzoru (6.5) zastosowanego do progu zero — tym razem z lewego ogona:",
          risk_formula("P(L>S)=⟦1|\\Phi⟧\\!\\left(\\frac{0-\\mu_D}{\\sigma_D}\\right)=⟦1|\\Phi⟧(-⟦2|\\beta⟧),\\quad ⟦2|\\beta⟧=\\frac{⟦3|\\mu_S-\\mu_L⟧}{⟦4|\\sqrt{\\sigma_S^{2}+\\sigma_L^{2}}⟧}", num = "6.10",
            notes = list("dystrybuanta N(0, 1): lewy ogon, bo awaria to D < 0", "indeks niezawodności (definicja 6.6): ile σ_D dzieli zero od średniego marginesu", "średni margines μ_D, wzór (6.9)", "rozrzut marginesu σ_D, wzór (6.9)"),
            note_colors = c("wrzos", "accent", "terakota", "szalwia"), above = 1),
          # Pojęcie P12 (przyjęte 17.11.2026): od chmury par do marginesu D; stoi po (6.9)–(6.10),
          # bo kadry używają β i σ_D
          z6p_get("z6p_12_ui"),
          c(
            "Wzór (6.10) tłumaczy, dlaczego normy mówią o rozrzutach, a nie tylko o średnich. Tradycyjny współczynnik bezpieczeństwa μ_S/μ_L nie zawiera σ, więc dwie konstrukcje o tym samym współczynniku mogą mieć zupełnie różne ryzyko awarii.",
            "Tu też wraca pułapka z nakładaniem krzywych. Kusi, by za ryzyko uznać pole wspólne dwóch gęstości narysowanych na jednym wykresie. Dla L ~ N(85, 8) i S ~ N(95, 7) to pole wynosi około 0.50, podczas gdy P(L > S) ≈ 0.173. Pole nakładania mierzy podobieństwo kształtów, a nie częstość par, w których obciążenie wygrywa."
          ),
          # Przypadek brzegowy P13 (przyjęty 17.11.2026): pole nakładania to nie ryzyko
          z6p_get("z6p_13_ui"),
          risk_check("z6_chk_wariancja",
            "L ~ N(85, 8) i S ~ N(95, 7) są niezależne. Jakie jest odchylenie standardowe marginesu D = S - L?",
            c("√(8² - 7²) ≈ 3.9" = "minus", "√(8² + 7²) ≈ 10.6" = "plus", "8 - 7 = 1" = "diff"),
            correct = "plus",
            explanation = "Wariancje niezależnych składników się dodają, także przy odejmowaniu zmiennych: σ_D = √113 ≈ 10.63 — wzór (6.9). Użycie √15 dałoby P(awarii) ≈ 0.005 zamiast 0.173, czyli fałszywe poczucie bezpieczeństwa.",
            hints = c(minus = "Odejmowanie zmiennych nie odejmuje ich niepewności. Co to jest Var(-L)?", diff = "Odchyleń standardowych nie odejmuje się; działamy na wariancjach.")
          )
        )
      ),
      list(
        id = "transfer", title = "Przykład transferowy: lina dźwigu",
        body = list(
          "Lina o średniej wytrzymałości 95 kN pracuje z ładunkami o średnim obciążeniu 85 kN. Dziesięć kilonewtonów zapasu wygląda solidnie, ale o ryzyku decydują rozrzuty obu wielkości: wystarczy ciężka partia ładunków i osłabiona partia lin, żeby pary L > S przestały być teoretyczną ciekawostką. Normy konstrukcyjne mówią językiem kwantyli i współczynników bezpieczeństwa właśnie dlatego, że średnie nie wystarczają.",
          risk_example("6.8", "Ryzyko zerwania liny",
            problem = "Obciążenie L ~ N(85, 8) kN, wytrzymałość S ~ N(95, 7) kN, zmienne niezależne. Oblicz współczynnik bezpieczeństwa μ_S/μ_L, indeks niezawodności β i prawdopodobieństwo awarii pojedynczego podniesienia.",
            steps = c(
              "Współczynnik bezpieczeństwa: 95/85 ≈ 1.12.",
              "Ze wzoru (6.9): μ_D = 95 - 85 = 10 kN; σ_D = √(7² + 8²) = √113 ≈ 10.63 kN.",
              "β = 10/10.63 ≈ 0.94.",
              "Ze wzoru (6.10): P(L > S) = Φ(-0.94) ≈ 0.173."
            ),
            answer = "Około 0.173, czyli mniej więcej 173 awarie na 1000 podniesień — nie do przyjęcia, mimo że średnia wytrzymałość przewyższa średnie obciążenie o 12%. To te same liczby, co w scenie z liną na początku rozdziału."
          ),
          risk_example("6.9", "Ile wytrzymałości potrzeba?",
            problem = "Jaka średnia wytrzymałość liny zapewni P(L > S) ≤ 0.001 przy niezmienionych rozrzutach (σ_L = 8 kN, σ_S = 7 kN)? Porównaj z efektem ograniczenia rozrzutu obciążenia do σ_L = 4 kN przy μ_S = 95 kN.",
            steps = c(
              "P(L > S) = Φ(-β) ≤ 0.001 wymaga β ≥ z₀,₉₉₉ ≈ 3.090.",
              "μ_S - 85 ≥ 3.090 · 10.63 ≈ 32.85 kN, więc μ_S ≥ 117.85 kN.",
              "Ograniczenie rozrzutu obciążenia (na przykład ważenie ładunków): σ_D = √(7² + 4²) = √65 ≈ 8.06 kN, β = 10/8.06 ≈ 1.24, P(L > S) = Φ(-1.24) ≈ 0.107."
            ),
            answer = "Potrzebna średnia wytrzymałość to około 118 kN, czyli współczynnik bezpieczeństwa około 1.39. Samo ograniczenie rozrzutu obciążenia zmniejsza ryzyko z 0.173 do około 0.107 — wyraźnie, ale daleko od 0.001; tu trzeba działać na obu parametrach."
          )
        )
      )
    ),
    pitfall = "Pole nakładania dwóch gęstości nie jest prawdopodobieństwem L>S."
  ),
  list(
    id = "nienormalny", title = "Wykres kwantylowy", hook = "Ta sama średnia, zupełnie inny ogon",
    # Poprawka raportu z rozdziału 1 (przyjęta 17.11.2026)
    widget = z6p_21_fix(6),
    lead = "Skośność i ciężki ogon mogą silnie zmienić ryzyko progowe mimo podobnej średniej i odchylenia.",
    intro = c(
      "Wiersz 5 raportu brzmi po prostu: „Model standardowy”. Ten rozdział sprawdza, co kryje się za tym słowem.",
      "Model normalny jest wygodny, ale nie jest prawem przyrody. Procesy z naturalną dolną granicą bywają skośne, a procesy z rzadkimi zaburzeniami mają ogony cięższe, niż przewiduje krzywa dzwonowa. Trzy modele w galerii niżej mają tę samą średnią i to samo odchylenie, a wyraźnie różne ryzyko przekroczenia progu.",
      "Do diagnozy służy wykres kwantylowy: punkty na prostej oznaczają zgodność z modelem normalnym, a zagięcia na końcach — ogony inne niż normalne. To najtańsze narzędzie kontroli jakości modelu przed rachunkiem progowym."
    ),
    sections = list(
      list(
        id = "ogony", title = "Ten sam środek, inny ogon",
        body = list(
          # Scena P19B (przyjęta 17.11.2026): czas zatrzymania dwóch pras, przed pojęciem skośności
          z6p_get("z6p_19b_ui"),
          c(
            "Obie prasy mają tę samą średnią i to samo odchylenie standardowe, a prasa B przekracza dopuszczalny czas kilka razy częściej. Jej rozkład jest skośny: ma twardą dolną granicę, bo szybciej niż pozwala mechanika zatrzymać się nie da, i długi prawy ogon, w którym mieszczą się rzadkie zwłoki zaworu.",
            "Wszystkie rachunki z rozdziałów 2–5 korzystały z funkcji Φ, czyli zakładały, że kształt rozkładu jest dokładnie normalny. Średnia i odchylenie standardowe nie wyznaczają jednak kształtu. Dwa rozkłady o μ = 82°C i σ = 3°C mogą mieć zupełnie różne ogony, a ryzyko progowe mieszka właśnie w ogonie.",
            "Galeria poniżej porównuje trzy modele temperatury, wszystkie ze średnią 82°C i odchyleniem 3°C. Symetryczny to N(82, 3). Skośny to przesunięty rozkład gamma: ma twardą dolną granicę około 76.8°C (łożysko nie ostygnie poniżej temperatury otoczenia) i dłuższy prawy ogon. Model z ciężkim ogonem to przeskalowany rozkład t-Studenta z trzema stopniami swobody: większość pomiarów skupia się ciaśniej wokół 82°C, ale rzadkie skoki są znacznie dalsze niż w modelu normalnym."
          ),
          risk_example("6.10", "Trzy modele, trzy ryzyka",
            problem = "Dla trzech modeli temperatury o średniej 82°C i σ = 3°C (normalny, skośny gamma, ciężki ogon t₃) porównaj P(T > 85) oraz P(T > 91), czyli przekroczenie progu leżącego 1σ i 3σ nad średnią.",
            steps = c(
              "Model normalny, ze wzoru (6.5): P(T > 85) ≈ 0.159; P(T > 91) ≈ 0.00135.",
              "Model skośny (w R: 1 - pgamma(…)): P(T > 85) ≈ 0.149; P(T > 91) ≈ 0.0118.",
              "Model z ciężkim ogonem (w R: 1 - pt(…, df = 3)): P(T > 85) ≈ 0.091; P(T > 91) ≈ 0.0069.",
              "Stosunek do modelu normalnego przy progu 91°C: skośny około 8.7 razy więcej, ciężki ogon około 5.1 razy więcej."
            ),
            answer = "Przy progu 1σ nad średnią modele różnią się umiarkowanie, a model z ciężkim ogonem daje nawet mniej przekroczeń niż normalny. Przy progu 3σ oba modele nienormalne dają od 5 do 9 razy więcej przekroczeń. Założenie normalności najbardziej szkodzi tam, gdzie progi są najważniejsze — daleko w ogonie."
          ),
          risk_try("w widoku „Cały rozkład” porównaj środki trzech krzywych. Potem przełącz na „Ogon powyżej 88°C” i sprawdź, która krzywa leży najwyżej za progiem 91°C. Liczby z tabeli pod wykresem porównaj z przykładem 6.10."),
          # Galeria P14A (przyjęta 17.11.2026): zastępuje dawny histogram kształtów;
          # wykres kwantylowy tych samych kształtów stoi w sekcji „Jak czytać wykres kwantylowy”
          z6p_get("z6p_14a_ui"),
          c(
            "W całym rozkładzie różnice łatwo przeoczyć: wszystkie trzy krzywe mają garb w okolicy 82°C. Rozkład skośny ma wyraźnie ucięty lewy bok i nieco dłuższy prawy; rozkład z ciężkim ogonem ma wyższy i węższy garb, a jego ogony sięgają dalej niż u normalnego. Dopiero powiększenie ogona pokazuje, ile pola leży za wysokim progiem. Na danych, bez znanego modelu, te same różnice najlepiej widać na wykresie kwantylowym — i to jest główny powód, dla którego używa się go do diagnozy."
          )
        )
      ),
      list(
        id = "qq", title = "Jak czytać wykres kwantylowy",
        body = list(
          risk_definition("6.7", "Wykres kwantylowy (Q–Q)", c(
            "Wykres kwantylowy względem rozkładu normalnego to wykres punktowy, na którym każdy uporządkowany pomiar x₍ᵢ₎ zestawia się z kwantylem standardowego rozkładu normalnego tego samego rzędu. Jeśli dane pochodzą z rozkładu normalnego, punkty leżą w przybliżeniu na prostej o wyrazie wolnym μ i nachyleniu σ."
          )),
          risk_formula("\\left(⟦1|\\Phi^{-1}⟧\\!\\left(⟦2|\\frac{i-0.5}{n}⟧\\right),\\; ⟦3|x_{(i)}⟧\\right),\\qquad i=1,\\ldots,n", num = "6.11",
            notes = list("funkcja kwantylowa N(0, 1)", "rząd kwantyla i-tego pomiaru, z poprawką 0.5", "i-ty najmniejszy pomiar"),
            note_colors = c("wrzos", "terakota", "accent"), above = 1),
          c(
            "Logika jest ta sama co w standaryzacji (6.4), tylko odwrócona. Gdyby pomiary były dokładnie normalne, i-ty najmniejszy pomiar leżałby blisko μ + σ · Φ⁻¹((i - 0.5)/n) — to wzór (6.6) z α = (i - 0.5)/n. Odchylenie punktu od prostej mówi, o ile rzeczywisty kwantyl różni się od normalnego.",
            "Typowe wzory: punkty ułożone w łuk otwarty ku górze — oba końce powyżej prostej, środek nieco poniżej — oznaczają skośność prawostronną: prawy koniec ucieka w górę, bo prawy ogon jest dłuższy, a lewy koniec jest „podniesiony”, bo lewy ogon jest krótszy niż normalny. Punkty odchodzące od prostej na obu końcach w przeciwne strony, jak rozciągnięta litera S, oznaczają ogony cięższe niż normalne. Sam środek wykresu zwykle zgadza się dobrze i niewiele mówi."
          ),
          risk_try("przełączaj kształty i porównuj oba końce wykresu z opisem powyżej: dla rozkładu skośnego szukaj łuku, dla ciężkiego ogona rozciągniętej litery S. Sprawdź też, że w środku wszystkie trzy wykresy leżą blisko prostej."),
          # Dawny panel kształtów (17.11.2026): sam wykres kwantylowy; porównanie ogonów jest w P14A
          risk_sf_panel("Rozszerzenie", "Trzy kształty na wykresie kwantylowym", "z6_shapes",
            controls = lc_segmented("z6_shape", "Kształt", choices = c("Symetryczny" = "normal", "Skośny" = "skew", "Ciężki ogon" = "heavy")),
            config = z6_shapes_config),
          risk_example("6.11", "Pięć pomiarów na wykresie Q–Q",
            problem = "Pięć uporządkowanych pomiarów temperatury łożyska: 79.4; 80.9; 82.3; 83.1; 88.6°C. Wyznacz współrzędne punktów Q–Q według (6.11) i porównaj pomiary z kwantylami modelu N(82, 3).",
            steps = c(
              "Rzędy (i - 0.5)/5: 0.1; 0.3; 0.5; 0.7; 0.9. Kwantyle N(0, 1): -1.28; -0.52; 0; 0.52; 1.28.",
              "Kwantyle modelu N(82, 3) ze wzoru (6.6): 78.2; 80.4; 82.0; 83.6; 85.8°C.",
              "Różnice pomiar - model: +1.2; +0.5; +0.3; -0.5; +2.8. Cztery pierwsze punkty leżą blisko prostej, ostatni odstaje o prawie 1σ."
            ),
            answer = "Największy pomiar jest o 2.8°C wyższy, niż przewiduje model normalny — sygnał możliwego ciężkiego prawego ogona. Przy pięciu pomiarach to jednak tylko sygnał; do oceny ogona potrzeba znacznie więcej danych."
          ),
          # Pojęcie P15A (przyjęte 17.11.2026): jak powstaje punkt wykresu Q–Q
          z6p_get("z6p_15a_ui"),
          risk_check("z6_chk_qq", extension = TRUE,
            "Na wykresie Q–Q punkty w środku leżą na prostej, a na prawym końcu wyraźnie zaginają się w górę. Co to oznacza dla oceny P(T > c) przy wysokim progu c?",
            c("Model normalny zaniża ryzyko" = "under", "Model normalny zawyża ryzyko" = "over", "Nic, bo środek jest zgodny" = "nothing"),
            correct = "under",
            explanation = "Zagięcie w górę na prawym końcu oznacza, że najwyższe pomiary są wyższe, niż przewiduje model normalny: prawy ogon jest cięższy, a 1 - Φ(z) zaniża częstość przekroczeń wysokich progów.",
            hints = c(over = "Punkty powyżej prostej to pomiary wyższe niż normalne kwantyle tego samego rzędu. Czy to więcej, czy mniej wyników za progiem?", nothing = "Ryzyko progowe zależy od ogona, nie od środka rozkładu.")
          ),
          "Co robić, gdy wykres Q–Q odrzuca model normalny? Są trzy uczciwe drogi. Można dobrać inny rozkład — skośny, na przykład gamma albo logarytmiczno-normalny — i powtórzyć rachunek. Przy dużej liczbie pomiarów można oszacować ryzyko wprost jako odsetek przekroczeń w danych, pamiętając o słabym obsadzeniu ogona. Można wreszcie przyjąć ostrożniejszy próg decyzyjny. Nieuczciwa droga to raportowanie 1 - Φ(z) z adnotacją, że „model jest przybliżony”, bez sprawdzenia, w którą stronę przybliżenie się myli. W następnym wykładzie spotkamy rozkłady czasu życia, które z natury są skośne i nigdy nie są ujemne."
        )
      )
    ),
    extension = TRUE,
    pitfall = "Dopasowanie środka wykresu nie gwarantuje dobrego opisu ekstremów."
  ),
  list(
    id = "komunikat", title = "Komunikat o ryzyku", hook = "Wynik ma wskazać, co zrobić",
    lead = "Komunikat o ryzyku łączy poziom ryzyka w okresie odniesienia, źródło ryzyka i postępowanie z ryzykiem — dopiero wtedy kierownik wie, co zrobić.",
    intro = c(
      "Wszystkie poprawki z rozdziałów 1–6 składamy teraz w jeden raport. Dochodzą dwa wiersze, których wcześniej brakowało: zakres z kryterium decyzji i monitorowanie po wdrożeniu.",
      "Kompletny komunikat o ryzyku mieści się w trzech zdaniach: jaka część wyników przekracza próg i w jakim okresie odniesienia (na pomiar, na zmianę), co jest źródłem ryzyka w ogonie, które postępowanie z ryzykiem — chłodzenie, stabilizacja czy rewizja progu — rekomendujesz i dlaczego. Liczba bez źródła ryzyka nie wskazuje działania; działanie bez liczby nie ma uzasadnienia."
    ),
    sections = list(
      list(
        id = "przyklad", title = "Trzy zdania dla kierownika",
        body = list(
          risk_example("6.12", "Trzy zdania dla kierownika dojrzewalni",
            problem = "Na podstawie danych łożyska (μ = 82°C, σ = 3°C, próg 85°C, 8 pomiarów na zmianę) i przykładu 6.7 zredaguj komunikat o ryzyku w trzech zdaniach: poziom ryzyka w okresie odniesienia, źródło ryzyka, postępowanie z ryzykiem.",
            steps = c(
              "Poziom ryzyka i okres odniesienia: około 159 na 1000 pomiarów przekracza 85°C (6.5); przy ośmiu niezależnych pomiarach na zmianę alarm pojawiłby się w około 75% zmian (6.7) — przy skorelowanych pomiarach rzadziej.",
              "Źródło ryzyka: średnia leży tylko 1σ pod progiem, więc ryzyko tworzy zarówno położenie, jak i rozrzut temperatury.",
              "Postępowanie z ryzykiem: chłodzenie o 2°C obniża częstość do około 48 na 1000, a połączenie z ograniczeniem σ do 2°C — do około 6 na 1000 (przykład 6.7); podniesienie progu tylko po badaniu materiałowym łożyska."
            ),
            answer = "„Około 16% pomiarów, a w praktyce większość zmian, przekracza próg 85°C. Przyczyną jest średnia zbyt blisko progu przy rozrzucie 3°C. Rekomendujemy poprawę chłodzenia i wyrównanie obciążenia, co może obniżyć częstość przekroczeń do kilku na tysiąc; progu nie podnosimy bez badania materiałowego.”"
          )
          ,
          # Domknięcie P16A (przyjęte 17.11.2026): raport z rozdziału 1 poprawiony
          z6p_get("z6p_16a_ui"),
          # Widgety P20A–P20C (przyjęte 17.11.2026): samodzielna praca z komunikatem
          z6p_get("z6p_20a_ui"),
          z6p_get("z6p_20b_ui"),
          z6p_get("z6p_20c_ui")
        )
      )
    )
  ),
  list(
    id = "sprawdzenie", title = "Ściąga i sprawdzenie", hook = "Próg to decyzja, nie tylko liczba",
    lead = "Pytanie → model → założenia → P(X > c) → działanie; quiz i ćwiczenia łączą wykres, rachunek i sens inżynierski.",
    intro = c(
      "Quiz sprawdza rozumienie mechanizmu — co naprawdę zmniejsza pole ogona — a ćwiczenia prowadzą przez pełny rachunek: od parametrów, przez standaryzację, po naturalną częstość i diagnozę modelu."
    ),
    sections = list(
      list(
        id = "podsumowanie", title = "Podsumowanie",
        text = c(
          "Wykład zaczął się od raportu, w którym średnia 82°C leżała poniżej progu 85°C, i od obserwacji, że bez miary rozrzutu nie da się ocenić ryzyka. Temperatura jest zmienną ciągłą, więc prawdopodobieństwa są polami pod gęstością (6.1), a pojedyncza wartość ma prawdopodobieństwo zero. Pola odczytujemy z dystrybuanty (6.2). Rozkład normalny N(μ, σ) o gęstości (6.3) opisują dwa parametry: położenie μ i szerokość σ — w tym kursie zawsze z odchyleniem standardowym na drugim miejscu.",
          "Standaryzacja (6.4) przekłada każdy próg na wspólną linijkę z, a wzór (6.5) zamienia z na prawdopodobieństwo przekroczenia; kwantyl (6.6) odpowiada na pytanie odwrotne. Dla łożyska z = 1 i P(T > 85) ≈ 0.159, czyli około 159 pomiarów na tysiąc. Ryzyko w zmianie z wieloma pomiarami to inna zmienna — wzór (6.7). Chłodzenie, stabilizacja i zmiana progu działają przez to samo z, ale tylko dwa pierwsze zmieniają fizykę. Gdy próg sam jest losowy, jak wytrzymałość liny, ryzyko to P(D < 0) dla marginesu D = S - L (6.8), którego wariancja jest sumą wariancji (6.9), a prawdopodobieństwo awarii to Φ(-β) (6.10).",
          "Wszystkie te rachunki zależą od kształtu ogona. Wykres kwantylowy (6.11) pokazuje, czy model normalny opisuje ekstremalne pomiary; skośność i ciężkie ogony potrafią zwiększyć częstość przekroczeń odległych progów kilkakrotnie, przy tej samej średniej i tym samym σ."
        )
      ),
      list(
        id = "sciaga", title = "Ściąga",
        bullets = c("Pytanie: jaka część wyników przekracza próg?", "Model: rozkład zmiennej ciągłej", "Założenia: stabilność, kształt ogona, jednostki", "Wynik: P(X>c) i naturalna częstość", "Interpretacja: oczekiwane przekroczenia w porównywalnych ekspozycjach"),
        widget = risk_assessment_ui("z6", prog_quiz, prog_exercises),
        decision = "Najpierw redukuj źródło ryzyka; podniesienie progu wymaga uzasadnienia konstrukcyjnego."
      ),
      list(id = "most", title = "Co dalej", text = "Następny wykład zastosuje ten sam język gęstości, pola i ogona do szczególnej zmiennej ciągłej: czasu do awarii elementu.")
    )
  )
))
prog_chapters <- risk_block_chapters(prog_block)
