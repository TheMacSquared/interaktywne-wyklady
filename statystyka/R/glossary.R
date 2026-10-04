# ============================================================================
# SLOWNIK TERMINOW STATYSTYCZNYCH
# Uzywany przez gloss() — wstawia klikalny termin z popupem definicji.
# ============================================================================

.GLOSSARY <- list(

  # Podstawy -------------------------------------------------------------------
  "populacja"             = "Cały zbiór obiektów, o których chcemy wnioskować.",
  "próba"                 = "Wybrany podzbiór populacji, na którym przeprowadzamy pomiary.",
  "parametr"              = "Liczbowa cecha opisująca populację (np. μ, σ, p). Zazwyczaj nieznany.",
  "statystyka"            = "Funkcja danych z próby służąca do estymacji parametru (np. x̄, s, p̂).",
  "estymator"             = "Statystyka używana do szacowania nieznanego parametru populacji.",
  "estymacja"             = "Proces wnioskowania o parametrze populacji na podstawie próby.",
  "obserwacja"            = "Wszystko, co zapisano o jednej jednostce badania; jeden wiersz tabeli danych.",
  "zmienna"               = "Cecha zapisana dla każdej obserwacji, która może przyjmować różne wartości; jedna kolumna tabeli danych.",
  "operat losowania"      = "Lista jednostek populacji, z której losuje się próbę (np. wykaz studentów, rejestr gospodarstw).",
  "losowanie proste"      = "Dobór próby, w którym każda jednostka z operatu ma tę samą szansę trafienia do próby.",
  "losowanie warstwowe"   = "Dobór próby, w którym populację dzieli się na warstwy i losuje osobno w każdej z nich.",
  "próba wygodna"         = "Próba złożona z jednostek najłatwiej dostępnych lub zgłaszających się samodzielnie; zwykle obciążona.",
  "zmienność próbkowa"    = "Różnice wartości statystyki między kolejnymi próbami z tej samej populacji; maleje ze wzrostem n.",
  "statystyka opisowa"    = "Część statystyki, która streszcza zebrane dane liczbami i wykresami; jej wyniki dotyczą próby.",
  "wnioskowanie statystyczne" = "Przenoszenie wyników z próby na populację wraz z oceną niepewności (przedziały ufności, testy).",

  # Miary centralne i rozproszenia ---------------------------------------------
  "średnia"               = "Suma wartości w zbiorze podzielona przez ich liczbę. Oznaczana x̄ (próba) lub μ (populacja).",
  "mediana"               = "Wartość środkowa uporządkowanego zbioru danych. Odporna na wartości odstające.",
  "odchylenie standardowe"= "Miara przeciętnego odchylenia wartości od średniej. Pierwiastek z wariancji.",
  "wariancja"             = "Średnia kwadratów odchyleń od średniej. Mierzy rozproszenie danych.",
  "błąd standardowy"      = "Odchylenie standardowe rozkładu próbkowego statystyki (np. średniej).",

  # Rozkłady -------------------------------------------------------------------
  "rozkład normalny"      = "Symetryczny dzwonowy rozkład prawdopodobieństwa opisywany przez μ i σ.",
  "rozkład próbkowy"      = "Rozkład wartości statystyki obliczonej z wielu niezależnych prób.",
  "centralne twierdzenie graniczne" =
    "Rozkład próbkowy średniej dąży do normalnego, gdy n rośnie — niezależnie od rozkładu populacji.",

  # Przedziały ufności ---------------------------------------------------------
  "przedział ufności"     = "Zakres wartości wyznaczany metodą, która z zadanym prawdopodobieństwem (poziomem ufności) daje przedział pokrywający nieznany parametr.",
  "poziom ufności"        = "Prawdopodobieństwo, że przedział ufności pokryje prawdziwy parametr. Typowo 95%.",
  "margines błędu"        = "Połowa szerokości przedziału ufności: ±z·SE lub ±t·SE.",

  # Testy hipotez --------------------------------------------------------------
  "hipoteza zerowa"       = "H₀ — hipoteza o braku efektu lub braku różnicy. Odrzucamy ją lub nie.",
  "hipoteza alternatywna" = "Hₐ — hipoteza konkurencyjna wobec H₀; przyjmujemy ją, gdy dane przemawiają przeciw H₀ (p < α).",
  "p-wartość"             = "Prawdopodobieństwo uzyskania wyniku co najmniej tak ekstremalnego, zakładając że H₀ jest prawdziwa.",
  "poziom istotności"     = "Próg α (zazwyczaj 0.05), poniżej którego odrzucamy H₀.",
  "statystyka testowa"    = "Wartość obliczona z próby (np. t, z, χ²) służąca do podjęcia decyzji o H₀.",
  "test t"                = "Test sprawdzający hipotezę o średniej (lub różnicy średnich) gdy odchylenie populacji jest nieznane.",
  "test chi-kwadrat"      = "Test zgodności lub niezależności dla danych kategorycznych.",

  # Regresja -------------------------------------------------------------------
  "korelacja"             = "Miara liniowego związku między dwiema zmiennymi. Zakres: -1 do +1.",
  "regresja liniowa"      = "Model opisujący liniową zależność zmiennej odpowiedzi od predyktorów.",
  "współczynnik determinacji" = "R² — odsetek wariancji zmiennej zależnej wyjaśniony przez model.",

  # Inne -----------------------------------------------------------------------
  "rozkład dwumianowy"    = "Rozkład liczby sukcesów w n niezależnych próbach Bernoulliego z prawdopodobieństwem p.",
  "wartość odstająca"     = "Obserwacja znacznie odbiegająca od pozostałych wartości w zbiorze.",
  "ANOVA"                 = "Analiza wariancji — test porównujący średnie w więcej niż dwóch grupach.",
  "wielkość efektu"       = "Praktyczna wielkość różnicy lub związku, niezależna od istotności statystycznej.",

  # Statystyka opisowa (W01) --------------------------------------------------
  "zmienna nominalna" =
    "Zmienna jakościowa, której kategorie nie mają naturalnego porządku (np. kolor, gatunek).",
  "zmienna porządkowa" =
    "Zmienna jakościowa z naturalnym porządkiem kategorii, ale nieznanymi odległościami między nimi (np. wykształcenie).",
  "zmienna dyskretna" =
    "Zmienna ilościowa o wartościach policzalnych, zwykle całkowitych (np. liczba dzieci).",
  "zmienna ciągła" =
    "Zmienna ilościowa, która może przyjąć dowolną wartość z przedziału, także ułamkową (np. wzrost).",
  "częstość względna" =
    "Liczebność kategorii podzielona przez liczebność całej próby n; często podawana w procentach.",
  "częstość skumulowana" =
    "Suma częstości narastająco do danej kategorii; ma sens tylko dla danych uporządkowanych.",
  "tabela kontyngencji" =
    "Tabela krzyżowa liczebności dla wszystkich kombinacji kategorii dwóch zmiennych jakościowych.",
  "dominanta" =
    "Wartość lub kategoria występująca najczęściej; jedyna miara tendencji centralnej dla zmiennych nominalnych.",
  "histogram" =
    "Wykres liczebności obserwacji w kolejnych przedziałach (binach) wartości zmiennej ilościowej.",
  "kwartyl" =
    "Wartości Q1, Q2 i Q3 dzielące uporządkowane dane na cztery równe części; Q2 to mediana.",
  "percentyl" =
    "Wartość, poniżej której leży dany procent obserwacji (np. 90. percentyl).",
  "rozstęp" =
    "Różnica między wartością największą a najmniejszą; bardzo wrażliwy na wartości odstające.",
  "rozstęp międzykwartylowy" =
    "IQR = Q3 − Q1 — rozrzut środkowych 50% obserwacji, odporny na wartości odstające.",
  "wykres pudełkowy" =
    "Wykres oparty na kwartylach: pudełko to Q1–Q3, kreska w środku to mediana, wąsy sięgają do 1.5·IQR.",
  "współczynnik zmienności" =
    "CV = SD / średnia × 100% — względna miara rozrzutu, pozwala porównać zmienne mierzone w różnych skalach.",
  "reguła 68-95-99.7" =
    "W rozkładzie normalnym ok. 68% wartości leży w odległości ±1 SD od średniej, 95% w ±2 SD, 99.7% w ±3 SD.",
  "skośność" =
    "Miara asymetrii rozkładu: dodatnia oznacza dłuższy ogon w prawo, ujemna — w lewo, zero — symetrię.",
  "kurtoza" =
    "Miara „ciężkości” ogonów rozkładu, czyli skłonności do wartości skrajnych — nie spłaszczenia szczytu.",
  "średnia ucinana" =
    "Średnia obliczona po odrzuceniu ustalonego procentu najmniejszych i największych obserwacji.",
  "odporność" =
    "Cecha miary lub metody, której wynik niewiele się zmienia pod wpływem wartości odstających lub naruszeń założeń.",

  # Rozkłady prawdopodobieństwa (W02) -----------------------------------------
  "zmienna losowa" =
    "Zmienna, której wartość jest wynikiem zjawiska losowego; opisuje ją rozkład prawdopodobieństwa.",
  "rozkład prawdopodobieństwa" =
    "Kompletny opis możliwych wyników zmiennej losowej i ich prawdopodobieństw.",
  "rozkład empiryczny" =
    "Rozkład wartości zaobserwowanych w danych (np. histogram z próby).",
  "rozkład teoretyczny" =
    "Model rozkładu wynikający z reguły generującej dane, opisany wzorem i parametrami.",
  "prawo wielkich liczb" =
    "Wraz ze wzrostem liczby prób częstość względna wyniku zbliża się do jego prawdopodobieństwa, a średnia z próby — do wartości oczekiwanej.",
  "wartość oczekiwana" =
    "E(X) — średnia wartości zmiennej losowej ważona prawdopodobieństwami; „średnia w długim okresie”.",
  "funkcja prawdopodobieństwa" =
    "PMF — dla zmiennej dyskretnej przyporządkowuje każdej wartości jej prawdopodobieństwo P(X = x).",
  "funkcja gęstości" =
    "PDF — dla zmiennej ciągłej: pole pod krzywą nad przedziałem to prawdopodobieństwo wpadnięcia w ten przedział.",
  "próba Bernoulliego" =
    "Pojedyncze doświadczenie o dwóch wynikach (sukces/porażka) z prawdopodobieństwem sukcesu p.",
  "rozkład jednostajny" =
    "Rozkład, w którym każdy wynik (albo każdy odcinek przedziału o tej samej długości) jest jednakowo prawdopodobny.",
  "rozkład Poissona" =
    "Rozkład liczby zdarzeń w ustalonym przedziale czasu lub przestrzeni; wartość oczekiwana i wariancja są równe λ.",
  "rozkład geometryczny" =
    "Rozkład liczby prób Bernoulliego potrzebnych do uzyskania pierwszego sukcesu.",
  "bezpamięciowość" =
    "Dotychczasowy czas oczekiwania nie zmienia rozkładu dalszego oczekiwania; własność rozkładu geometrycznego i wykładniczego.",
  "rozkład wykładniczy" =
    "Ciągły rozkład czasu oczekiwania między zdarzeniami w procesie Poissona.",
  "rozkład t-Studenta" =
    "Symetryczny rozkład podobny do normalnego, ale z cięższymi ogonami; im więcej stopni swobody, tym bliżej N(0, 1).",
  "stopnie swobody" =
    "Parametr df rozkładów t, χ² i F; zwykle liczba obserwacji pomniejszona o liczbę oszacowanych parametrów (np. n - 1).",
  "rozkład chi-kwadrat" =
    "Rozkład sumy kwadratów k niezależnych zmiennych N(0, 1); nieujemny i prawoskośny, z k stopniami swobody.",
  "rozkład log-normalny" =
    "Rozkład zmiennej, której logarytm ma rozkład normalny; typowy dla wielkości rosnących multiplikatywnie (dochody, ceny).",
  "standaryzacja" =
    "Przekształcenie z = (x − μ)/σ; wynik z mówi, o ile odchyleń standardowych wartość leży od średniej.",
  "standardowy rozkład normalny" =
    "Rozkład normalny o średniej 0 i odchyleniu standardowym 1, oznaczany N(0, 1).",

  # Estymacja (W03) -----------------------------------------------------------
  "estymata" =
    "Konkretna wartość estymatora obliczona z danej próby (np. x̄ = 172.3 cm).",
  "nieobciążoność" =
    "Estymator jest nieobciążony, gdy jego wartość oczekiwana równa się parametrowi — nie myli się systematycznie w jedną stronę.",
  "efektywność estymatora" =
    "Spośród estymatorów nieobciążonych najefektywniejszy jest ten o najmniejszej wariancji.",
  "zgodność estymatora" =
    "Estymator jest zgodny, gdy wraz ze wzrostem próby zbiega do prawdziwej wartości parametru.",
  "wartość krytyczna" =
    "Wartość z rozkładu (np. z = 1.96 dla 95%) wyznaczająca szerokość przedziału ufności lub granicę obszaru odrzucenia H₀.",
  "pokrycie" =
    "Odsetek przedziałów ufności (w wielu powtórzeniach), które zawierają prawdziwy parametr; powinien odpowiadać poziomowi ufności.",
  "proporcja z próby" =
    "p̂ = liczba sukcesów / n — estymator odsetka p w populacji.",
  "przedział Walda" =
    "Najprostszy przedział ufności dla proporcji, p̂ ± z·√(p̂(1−p̂)/n); przy małym n lub p bliskim 0 albo 1 ma za niskie pokrycie.",
  "przedział Wilsona" =
    "Przedział ufności dla proporcji poprawiający wzór Walda; zachowuje dobre pokrycie także przy małych próbach.",
  "forest plot" =
    "Wykres zestawiający estymaty punktowe i przedziały ufności wielu grup lub badań na wspólnej osi.",
  "istotność praktyczna" =
    "Czy efekt jest na tyle duży, by miał znaczenie w praktyce; istotność statystyczna tego nie gwarantuje.",
  "wielkość próby" =
    "Liczba obserwacji n; planuje się ją tak, by uzyskać zakładany margines błędu lub moc testu.",

  # Testowanie hipotez (W04) --------------------------------------------------
  "błąd pierwszego rodzaju" =
    "Odrzucenie prawdziwej H₀ („fałszywy alarm”); jego prawdopodobieństwo to α.",
  "błąd drugiego rodzaju" =
    "Nieodrzucenie fałszywej H₀ („przegapiony efekt”); jego prawdopodobieństwo to β.",
  "moc testu" =
    "Prawdopodobieństwo wykrycia efektu, gdy ten naprawdę istnieje (1 − β); zwykle planuje się co najmniej 80%.",
  "obszar odrzucenia" =
    "Zakres wartości statystyki testowej, dla których odrzucamy H₀; wyznaczają go wartości krytyczne.",
  "test dwustronny" =
    "Test z Hₐ „różne od” (≠), wykrywający efekt w obu kierunkach; α dzieli się na oba ogony.",
  "test jednostronny" =
    "Test z Hₐ określającą kierunek (> lub <); mocniejszy w tym kierunku, ale ślepy na przeciwny. Kierunek ustala się przed zebraniem danych.",
  "test dwumianowy" =
    "Dokładny test porównujący odsetek sukcesów w próbie z wartością referencyjną p₀.",
  "liczebność oczekiwana" =
    "Liczebność komórki tabeli, jakiej oczekiwalibyśmy przy niezależności zmiennych: suma wiersza × suma kolumny / n.",
  "test dokładny Fishera" =
    "Test niezależności dla tabeli kontyngencji liczący p-wartość dokładnie; stosowany zwłaszcza wtedy, gdy liczebności oczekiwane są małe.",
  "próby zależne" =
    "Pomiary powiązane w pary (np. te same osoby przed i po); analizuje się różnice w parach.",
  "porównania wielokrotne" =
    "Wykonywanie wielu testów naraz; ryzyko co najmniej jednego fałszywego alarmu rośnie z liczbą testów.",
  "test post hoc" =
    "Porównania par grup po istotnym wyniku ANOVA, z kontrolą błędu dla całej rodziny porównań (np. Tukey, Games-Howell).",
  "d Cohena" =
    "Różnica średnich wyrażona w odchyleniach standardowych; miara siły efektu niezależna od n.",
  "eta kwadrat" =
    "η² — odsetek całkowitej wariancji wyjaśniony przynależnością do grupy (w ANOVA).",
  "V Cramera" =
    "Miara siły związku dwóch zmiennych jakościowych, od 0 do 1, oparta na χ²; dla tabeli 2×2 równa φ.",
  "paradoks Simpsona" =
    "Zależność w połączonych danych ma inny, nawet przeciwny kierunek niż w każdej z grup osobno.",
  "zmienna zakłócająca" =
    "Zmienna powiązana zarówno z predyktorem, jak i z wynikiem; może tworzyć albo maskować pozorny związek między nimi.",
  "korelacja pozorna" =
    "Związek dwóch zmiennych, który nie wynika z ich wzajemnej zależności, lecz np. ze wspólnej zmiennej zakłócającej.",

  # Założenia i testy nieparametryczne (W05) ----------------------------------
  "wykres kwantyl-kwantyl" =
    "Q-Q plot — zestawia kwantyle danych z kwantylami rozkładu normalnego; punkty blisko prostej oznaczają zgodność z normalnością.",
  "test Shapiro-Wilka" =
    "Test normalności; H₀: dane pochodzą z rozkładu normalnego.",
  "homoskedastyczność" =
    "Równość wariancji — w porównywanych grupach albo reszt modelu dla wszystkich wartości predyktora.",
  "test Levene'a" =
    "Test równości wariancji w grupach (H₀: wariancje są równe); odporny na brak normalności.",
  "test Bartletta" =
    "Test równości wariancji w grupach; mocniejszy od Levene'a, ale wrażliwy na brak normalności.",
  "test t Welcha" =
    "Wersja testu t dla dwóch grup, która nie zakłada równych wariancji; domyślna w R.",
  "test parametryczny" =
    "Test zakładający określony rozkład danych (zwykle normalny) i wnioskujący o jego parametrach, np. średniej.",
  "test nieparametryczny" =
    "Test o słabszych założeniach co do rozkładu, zwykle oparty na rangach; alternatywa przy silnych naruszeniach założeń.",
  "ranga" =
    "Pozycja obserwacji w danych uporządkowanych rosnąco; testy rangowe analizują rangi zamiast surowych wartości.",
  "test Manna-Whitneya" =
    "Nieparametryczny test porównujący dwie niezależne grupy na podstawie rang; alternatywa dla testu t.",
  "test Wilcoxona" =
    "Nieparametryczny test rangowy dla jednej próby lub prób zależnych; alternatywa dla testu t dla par.",
  "test Kruskala-Wallisa" =
    "Nieparametryczny test rangowy dla więcej niż dwóch grup; alternatywa dla jednoczynnikowej ANOVA.",
  "korelacja Spearmana" =
    "Korelacja rang; mierzy siłę związku monotonicznego, niekoniecznie liniowego.",
  "transformacja logarytmiczna" =
    "Zastąpienie wartości ich logarytmami; zmniejsza prawoskośność, a wyniki czyta się jako zmiany względne.",
  "niezależność obserwacji" =
    "Założenie, że wartość jednej obserwacji nie niesie informacji o innej; naruszają je np. powtarzane pomiary i szeregi czasowe.",

  # Regresja (W06) ------------------------------------------------------------
  "zmienna zależna" =
    "Zmienna objaśniana (Y) — wynik, który model przewiduje lub wyjaśnia.",
  "predyktor" =
    "Zmienna objaśniająca (X), której używamy do przewidywania lub wyjaśniania zmiennej zależnej.",
  "wyraz wolny" =
    "β₀ — przewidywana wartość Y, gdy wszystkie predyktory są równe 0.",
  "współczynnik regresji" =
    "β — o ile zmienia się przewidywane Y, gdy predyktor rośnie o jedną jednostkę (w regresji wielorakiej: przy stałych pozostałych).",
  "metoda najmniejszych kwadratów" =
    "Sposób dopasowania modelu: wybiera współczynniki minimalizujące sumę kwadratów reszt.",
  "reszta" =
    "e = y − ŷ — różnica między wartością zaobserwowaną a przewidywaną przez model.",
  "wartość przewidywana" =
    "ŷ — wartość Y wyznaczona przez model dla danych wartości predyktorów; średnia warunkowa Y.",
  "ekstrapolacja" =
    "Przewidywanie poza zakresem wartości predyktora obecnych w danych, gdzie model nigdy nie był sprawdzany.",
  "przeuczenie" =
    "Model dopasowany do danych treningowych tak mocno (także do szumu), że słabo przewiduje nowe obserwacje.",
  "RMSE" =
    "Pierwiastek ze średniego kwadratu reszt; typowy błąd predykcji w jednostkach Y.",
  "skorygowany R²" =
    "R² z karą za liczbę predyktorów; rośnie tylko wtedy, gdy nowy predyktor faktycznie poprawia model.",
  "regresja wieloraka" =
    "Regresja z więcej niż jednym predyktorem; każdy współczynnik opisuje związek przy stałych pozostałych zmiennych.",
  "współliniowość" =
    "Silna korelacja między predyktorami; utrudnia rozdzielenie ich wpływu i zawyża błędy standardowe (mierzy się ją VIF).",
  "zmienna wskaźnikowa" =
    "Zmienna 0/1 kodująca kategorię; jej współczynnik porównuje tę kategorię z poziomem odniesienia.",
  "interakcja" =
    "Związek predyktora z Y zależy od wartości innej zmiennej (np. różne nachylenia w różnych grupach).",
  "AIC" =
    "Kryterium informacyjne Akaikego — ocenia model łącznie za dopasowanie i złożoność; niższa wartość oznacza lepszy model.",
  "BIC" =
    "Bayesowskie kryterium informacyjne — jak AIC, ale mocniej karze za liczbę parametrów; niższa wartość oznacza lepszy model.",
  "zbiór testowy" =
    "Część danych odłożona przy dopasowaniu modelu, służąca do uczciwej oceny jego predykcji.",
  "obserwacja wpływowa" =
    "Obserwacja, której usunięcie wyraźnie zmienia oszacowania modelu (mierzy to m.in. odległość Cooka).",
  "regresja logistyczna" =
    "Model dla zmiennej zależnej 0/1, opisujący prawdopodobieństwo sukcesu jako funkcję predyktorów.",
  "iloraz szans" =
    "OR — ile razy zmieniają się szanse sukcesu, gdy predyktor rośnie o 1; w regresji logistycznej OR = e^β.",

  # Dane i metodologia badań (W07–W09) ----------------------------------------
  "jednostka obserwacji" =
    "To, czego dotyczy jeden wiersz danych — osoba, firma, dzień; od niej zależy, czym jest n.",
  "braki danych" =
    "Brakujące wartości (NA); rzadko są losowe, więc ich wzór może zniekształcić wyniki.",
  "imputacja" =
    "Uzupełnianie brakujących wartości oszacowaniami (np. średnią lub wartością przewidywaną z modelu).",
  "agregacja" =
    "Łączenie wielu obserwacji w jedną (np. średnia dzienna); może usunąć zależność, ale zmniejsza n i ukrywa zmienność.",
  "szereg czasowy" =
    "Pomiary tej samej wielkości w kolejnych chwilach; kolejne obserwacje zwykle nie są niezależne.",
  "autokorelacja" =
    "Korelacja wartości szeregu z jego wartościami przesuniętymi w czasie (np. dzień do dnia).",
  "sezonowość" =
    "Regularnie powtarzający się wzór w szeregu czasowym (np. tygodniowy lub roczny).",
  "skala Likerta" =
    "Skala ocen o kilku uporządkowanych poziomach (np. od „zdecydowanie się nie zgadzam” do „zdecydowanie się zgadzam”); daje dane porządkowe.",
  "zmienna kontrolna" =
    "Zmienna dodana do modelu, by oddzielić jej wpływ od związku, który nas interesuje.",
  "zmienna pominięta" =
    "Istotna zmienna nieuwzględniona w modelu; jeśli wiąże się z predyktorami, zniekształca ich oszacowania.",
  "dane obserwacyjne" =
    "Dane zebrane bez ingerencji badacza w przypisanie warunków; pokazują współwystępowanie, nie przyczynę.",
  "przyczynowość" =
    "Związek, w którym zmiana jednej zmiennej powoduje zmianę drugiej; wymaga eksperymentu albo silnych założeń.",
  "dane przekrojowe" =
    "Dane z jednego momentu dla wielu jednostek; w przeciwieństwie do danych podłużnych nie pokazują zmian w czasie.",
  "pytanie badawcze" =
    "Jedno główne pytanie, które da się rozważyć na danych i które porządkuje cały projekt.",
  "hipoteza badawcza" =
    "Robocze przypuszczenie o związku między zmiennymi; można je zawęzić lub odrzucić w toku analizy.",
  "obciążenie" =
    "Systematyczne zniekształcenie wyniku w jedną stronę, np. przez dobór próby lub sposób pomiaru.",

  # Statystyka opisowa (uzupełnienie) -----------------------------------------
  "zmienna jakościowa" =
    "Zmienna, której wartości to kategorie (np. płeć, gatunek); dzieli się na nominalne i porządkowe.",
  "zmienna ilościowa" =
    "Zmienna, której wartości są liczbami z sensem arytmetycznym (np. wzrost, liczba dzieci); dzieli się na dyskretne i ciągłe.",
  "liczebność" =
    "Liczba obserwacji w kategorii lub w całej próbie (częstość bezwzględna).",
  "tabela częstości" =
    "Tabela podająca dla każdej kategorii (lub przedziału) liczbę obserwacji i ich odsetek.",
  "wykres słupkowy" =
    "Wykres, w którym wysokość słupka pokazuje liczebność lub odsetek każdej kategorii zmiennej jakościowej.",
  "miara tendencji centralnej" =
    "Liczba opisująca „typową” wartość w danych, np. średnia, mediana lub dominanta.",
  "moda" =
    "Inna nazwa dominanty — wartość lub kategoria występująca najczęściej.",

  # Estymacja i testy (uzupełnienie) ------------------------------------------
  "przedział Cloppera-Pearsona" =
    "Dokładny przedział ufności dla proporcji oparty na rozkładzie dwumianowym; konserwatywny, zwykle nieco szerszy niż trzeba.",
  "korelacja Pearsona" =
    "Współczynnik r mierzący siłę i kierunek związku liniowego dwóch zmiennych ilościowych; od -1 do +1.",
  "tau Kendalla" =
    "Korelacja rangowa oparta na zgodności par obserwacji; alternatywa dla korelacji Spearmana przy małych próbach i wielu remisach.",
  "test t dla prób zależnych" =
    "Test t dla par pomiarów (np. przed i po); sprawdza, czy średnia różnic w parach różni się od zera.",
  "statystyka F" =
    "Statystyka testowa w ANOVA i regresji: stosunek zmienności wyjaśnionej (między grupami) do niewyjaśnionej (wewnątrz grup).",
  "test Games-Howella" =
    "Test post hoc po ANOVA, który nie zakłada równych wariancji ani równych liczebności grup.",
  "niezbalansowane grupy" =
    "Grupy o bardzo różnej liczebności; utrudniają porównania i obniżają moc testu.",

  # Regresja (uzupełnienie) ---------------------------------------------------
  "regresja prosta" =
    "Regresja liniowa z jednym predyktorem: ŷ = b₀ + b₁x.",
  "heteroskedastyczność" =
    "Nierówne wariancje — np. rozrzut reszt rośnie wraz z wartością predyktora; przeciwieństwo homoskedastyczności.",
  "VIF" =
    "Współczynnik inflacji wariancji — ile razy wariancja współczynnika rośnie przez współliniowość; im dalej od 1, tym mniej stabilne oszacowanie (bez jednej ostrej granicy).",
  "odległość Cooka" =
    "Miara wpływu pojedynczej obserwacji na model — jak bardzo zmieniłyby się przewidywania po jej usunięciu.",
  "zbiór treningowy" =
    "Część danych, na której dopasowujemy model; jego jakość ocenia się potem na zbiorze testowym.",
  "modele zagnieżdżone" =
    "Para modeli, z których prostszy powstaje z bardziej złożonego przez usunięcie predyktorów; można je porównać testem F.",
  "funkcja logistyczna" =
    "Krzywa w kształcie litery S (sigmoida), która zamienia dowolną liczbę na prawdopodobieństwo z przedziału od 0 do 1.",
  "próg klasyfikacji" =
    "Wartość prawdopodobieństwa (np. 0.5), powyżej której model logistyczny przypisuje obserwację do klasy „1”.",
  "macierz pomyłek" =
    "Tabela zestawiająca klasy przewidziane przez model z prawdziwymi: trafienia i oba rodzaje błędów.",

  # Dane i metodologia (uzupełnienie) -----------------------------------------
  "dane eksperymentalne" =
    "Dane z badania, w którym badacz losowo przydziela warunki; pozwalają wnioskować o przyczynowości.",
  "dane podłużne" =
    "Dane z wielokrotnych pomiarów tych samych jednostek w czasie; pokazują zmiany w obrębie jednostek."
)

# Wstawia klikalny termin ze słownika.
# term  — hasło w .GLOSSARY (mianownik), pokazywane w nagłówku popupu
# label — forma wyświetlana w tekście (np. odmieniona); domyślnie term
# Użycie: gloss("populacja", "populacji"), gloss("x", definition = "definicja inline")
gloss <- function(term, label = term, definition = NULL) {
  def <- if (!is.null(definition)) definition else .GLOSSARY[[term]]
  if (is.null(def)) stop(paste0("gloss(): brak definicji dla '", term, "'"))
  # .noWS: bez tego htmltools wstawia nową linię wokół spana i przed
  # następującą interpunkcją pojawia się spacja („pokrycie .”)
  tags$span(class = "lc-gloss", `data-term` = term, `data-def` = def, label,
            .noWS = "outside")
}
