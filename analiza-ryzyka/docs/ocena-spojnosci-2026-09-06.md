# Ocena spójności kursu analizy ryzyka

Data: 6 września 2026 r. Ocena materiałów w repozytorium, bez zmian aplikacji.

## Werdykt i zakres oceny

Kurs ma spójny szkielet i zawiera wszystkie osiem grup tematów wymienionych
w lokalnym sylabusie. Jego profil najlepiej opisuje określenie
„probabilistyczne podstawy analizy ryzyka i niezawodności”. Taki profil wynika
z sylabusa, więc przewaga rachunku prawdopodobieństwa nie jest sama w sobie
błędem ani skutkiem zbyt statystycznego podejścia prowadzącego.

Najważniejsze problemy dotyczą interpretacji modeli i połączeń między nimi,
zwłaszcza w blokach 08–10. Przed prowadzeniem tych bloków potrzebna jest
korekta. Ponadto warto dołożyć niewielką warstwę kontekstu: identyfikację
scenariuszy, źródła i niepewność parametrów, kryterium decyzji i ryzyko po
wdrożeniu zabezpieczeń. Nie wymaga to przebudowy kursu w przegląd wszystkich
metod zarządzania ryzykiem.

Przejrzano sylabus, plan, katalog, treści i ćwiczenia dziesięciu aplikacji,
wspólne mechanizmy quizów, rejestr danych oraz wybrane obliczenia i materiały
grafu pojęć. Kluczowe kontrprzykłady przeliczono w R. To audyt treści i logiki,
nie pełny test interfejsu ani próba prowadzenia zajęć w czasie rzeczywistym.
Lokalny `sylabus.md` zawiera treści programowe, lecz nie pełną kartę przedmiotu:
brakuje m.in. formalnych efektów uczenia się, wymagań wstępnych, wymiaru godzin
i zasad zaliczenia. Ocena zgodności formalnej ogranicza się do tego wyciągu.

## Pokrycie sylabusa

| Treść sylabusa | Materiały | Ocena |
|---|---|---|
| Pojęcia prawdopodobieństwa, definicja klasyczna | 01 | Obecne; dobrze podkreślono jednakowe szanse wyników i mianownik. |
| Warunkowe, iloczyn, całkowite, Bayes | 02–03 | Obecne; wartościowe naturalne częstości, rozróżnienie kierunków warunkowania i związku od przyczynowości. |
| Schemat Bernoulliego | 04 | Obecny wraz z definicją próby i założeniami. |
| Dwumianowy, geometryczny, ujemny dwumianowy | 04–05 | Obecne; dobry podział według tego, co jest ustalone i co losowe. |
| Rozkłady ciągłe i normalny | 06 | Obecne; dodatkowo sensowny model obciążenie–wytrzymałość. |
| Wykładniczy, gamma, Weibull | 07 | Wszystkie obecne, także gamma z niecałkowitym kształtem; potrzebne doprecyzowanie interpretacji czasu i hazardu. |
| Systemy, schematy, funkcja struktury, koherentność | 08 | Wszystkie obecne; koherentność ma definicję, ale wymaga samodzielnego zadania. |
| Awarie systemów złożonych | 08–09 | Obecne układy mieszane, FTA i przekroje; wspólna przyczyna w FTA jest słabiej przepracowana rachunkowo. |

Blok 10 jest uzasadnionym dodatkiem integracyjnym. Nie ma podstaw, by uznać
brak VaR, ryzyka portfelowego, rozbudowanego HAZOP czy modeli Markowa za
niewypełnienie tego sylabusa.

## Co warto zachować

- Kolejność: zdarzenie → warunek → wiele prób → czas → system → decyzja.
- Wspólny przypadek Bananpolu, przeplatany przykładami transferowymi.
- Regularne przypominanie o ekspozycji, definicji zdarzenia i niezależności.
- Rozróżnienie średniej od kwantyla i od prawdopodobieństwa przetrwania misji.
- Cenzorowanie, ograniczenia rozkładu normalnego, powtórzone liście FTA.
- Jawne stwierdzenie, że prawdopodobieństwo nie wystarcza do ustalenia priorytetu.

Skutki nie są całkiem pominięte: wracają w 01, a w 03 jest jakościowa macierz
konsekwencji reakcji na alarm. Problemem jest niedomknięcie tego w konkretnym
rachunku lub kryterium oceny końcowej rekomendacji.

## Poprawki najpilniejsze

### 1. FTA: roczna awaryjność nie oznacza niedostępności podczas zapłonu

Miejsce: [blok 10](../10-model-do-decyzji/modules/block.R), rozdział „Końcowe
FTA” i funkcje `annual_q()`, `top_p()`; także metadane FTA w
[bananpol.R](../R/bananpol.R).

Kod mnoży roczne prawdopodobieństwo inicjacji przez OR złożone z
`1−czułość` i `1−R_sys³`. Pierwszy parametr jest prawdopodobieństwem
przeoczenia przy wystąpieniu wykrywanego zdarzenia. Drugi oznacza co najmniej
jedną utratę funkcji w trzech misjach. Awaria systemu w styczniu, naprawiona
przed zapłonem w listopadzie, spełnia drugie zdarzenie, ale nie musi powodować
nieopanowanego pożaru. Samo ujednolicenie etykiety „rok” tego nie rozwiązuje.

Najprostsza spójna wersja dydaktyczna: zdefiniować I jako inicjację w roku,
D i S jako niepowodzenia funkcji podczas tej inicjacji, a następnie liczyć
`P(top)=P(I)P(D ∪ S | I)`. Przy warunkowej niezależności D i S otrzymujemy
iloczyn dopełnień z prawdopodobieństwami warunkowymi. Trzeba również uzasadnić,
dlaczego detektor przegrzania i układ chłodzenia reprezentują funkcje
niezbędne do opanowania wskazanego pożaru magazynu.

Rozróżnienie danych na żądanie, awarii w czasie i niedostępności omawia
[NASA, Fault Tree Handbook, rozdział 7](https://s3vi.ndc.nasa.gov/ssri-kb/static/resources/Fault%20Tree%20Handbook_NASA.pdf).
To podstawa interpretacji wejść, a nie wymóg dodania zaawansowanej metody.

### 2. FTA: narracja o barierach mówi co innego niż przyjęta bramka

Miejsce: [blok 09](../09-drzewo-bledow/modules/block.R), rozdziały
„Kierowany konstruktor” oraz „Bramki AND i OR bez liczb”.

Najpierw pada stwierdzenie, że pożar wymyka się spod kontroli, gdy zawiodą
wszystkie bariery. Następnie używany jest model `I AND (D OR S)`:
wystarczy awaria jednej z dwóch funkcji. Żadna bramka nie jest uniwersalnie
poprawna. OR pasuje do łańcucha wymagającego zarówno detekcji, jak i skutecznego
tłumienia; AND do dwóch samodzielnych, zastępujących się sposobów ochrony.
Opis konkretnej instalacji musi rozstrzygnąć wybór.

Przy liczbach kursu wariant OR daje `0,00063`, a wariant AND `0,00002` —
różnicę 31,5 raza. To dobry materiał na ćwiczenie: ten sam zestaw liczb,
dwie odmienne architektury i dwa różne wyniki.

W tym samym bloku narracja przy powtórzonym zdarzeniu zapowiada demonstrację
zaniżenia przez `q²`, podczas gdy widget pokazuje zawyżenie przez
`1−(1−q)²`. Ponadto objaśnienie quizowe błędnie nazywa sumę przybliżeniem
dla zdarzeń rozłącznych: dla rozłącznych jest ona dokładna, bez warunku rzadkości.

### 3. Rezerwa oczekująca została opisana wzorem zwykłego układu równoległego

Miejsce: [blok 08](../08-niezawodnosc-systemu/modules/block.R), „Układ równoległy”.

Zdanie „drugi wentylator […] czeka” sugeruje rezerwę uruchamianą dopiero po
awarii. Tymczasem `1−(1−R_A)(1−R_B)` używa niezawodności gałęzi dla tej samej
misji. Dla rezerwy trzeba określić starzenie w oczekiwaniu, uruchomienie
i przełącznik. Najprościej opisać dwa działające wentylatory, z których jeden
wystarcza do wymaganej funkcji, przy braku zmiany ich charakterystyk po awarii
drugiego. Rezerwę oczekującą wystarczy wymienić jako inny model.

Rozróżnienie rezerwy i przełączenia ilustruje
[NASA, Fault Tree Handbook, rozdział 8](https://s3vi.ndc.nasa.gov/ssri-kb/static/resources/Fault%20Tree%20Handbook_NASA.pdf).

### 4. Finał deklaruje połączenia, których część nie działa

Miejsce: [blok 10](../10-model-do-decyzji/modules/block.R).

- Wynik karty czasu życia `life_r()` nie zasila niezawodności wentylatora
  w karcie systemu. Pozostaje tam osobny suwak z wartością 0,92. Modele
  poprzedniej karty dają przy 1000 h około 0,513 i 0,707. Zmiana modelu
  życia nie zmienia wyniku końcowego. Należy połączyć karty albo jasno nazwać
  je odrębnymi przykładami.
- Zapowiedziana użyteczność „wiarygodności alarmu” nie jest realizowana:
  do FTA trafia czułość, nie posterior. Sam wybór czułości może być poprawny;
  trzeba poprawić obietnicę dydaktyczną i definicję zdarzenia.
- Scenariusze zmieniają wynik bazowy, lecz nie przeliczają rankingu wszystkich
  interwencji. Nie odpowiadają zatem na własne pytanie: czy rekomendacja
  pozostaje najlepsza przy niepewnych danych?
- „Częstszy przegląd” arbitralnie zmniejsza inicjację o połowę. To dodatkowe
  założenie skuteczności działania, którego nie wyprowadzono z modelu życia.

### 5. Powtarzanie misji wymaga modelu odnowy lub starzenia

Miejsce: [blok 10](../10-model-do-decyzji/modules/block.R), `1−R_sys³`
i ćwiczenie o sześciu krótszych misjach.

Niezależne, identyczne misje są jawnie zadeklarowane, więc sam iloczyn jest
poprawny w tym modelu. Brakuje jednak mechanizmu, który resetuje stan systemu,
oraz wskazania, jak przeliczać R po zmianie długości misji. Podział jednego
okresu eksploatacji na kartce nie odmładza urządzenia.

Dla Weibulla z kursu `R(3000)=0,0444`, podczas gdy `R(1000)³=0,3541`.
Druga liczba odpowiada trzem niezależnym misjom urządzeń zaczynających
w tym samym stanie początkowym. Dla jednego starzejącego się elementu
kolejne prawdopodobieństwa przeżycia są warunkowe, np. `R(2000)/R(1000)`.
W ćwiczeniu trzeba zachować łączną ekspozycję i określić politykę odnowy.

## Niewielkie uzupełnienia o dużej wartości

### Dane i niepewność parametrów

W 01 jest intuicyjne szacowanie p i rozróżnienie częstości od modelu.
Później dominują zadane parametry, a 07 wprost wyłącza ich estymację.
Nie potrzeba pełnego kursu wnioskowania, ale student powinien przejść raz
drogę: rejestr → oszacowanie → niepewność → wynik modelu → decyzja.

Proponowane 20–30 minut w 04 lub 10: zero awarii w 100 niezależnych próbach
nie oznacza p=0. Jednostronna dokładna górna granica ufności 95% wynosi
`1−0,05^(1/100) ≈ 2,95%`. Następnie sprawdzić, co ten zakres zmienia w wyniku
systemu. Warto rozdzielić losowość zdarzeń od niewiedzy o p oraz od niepewności
samego mechanizmu. Znaczenie małej liczby awarii dla precyzji oszacowań
podkreśla [NIST, Lack of failures](https://itl.nist.gov/div898/handbook/apr/section1/apr132.htm).

### Prawdopodobieństwo, intensywność, niezawodność i gotowość

W 07 dodać krótką tabelę: prawdopodobieństwo awarii do t, hazard z jednostką
1/h, liczba awarii na jednostkę czasu oraz dostępność funkcji w chwili
zapotrzebowania. Przy hazardzie przyda się
`P(t<T≤t+Δt | T>t) ≈ h(t)Δt` dla małego Δt i wyraźne stwierdzenie, że h(t)
nie jest prawdopodobieństwem. Ta uwaga istnieje w grafie pojęć, lecz warto
umieścić ją bezpośrednio w wykładzie.

Wystarczy też zaznaczyć granicę między czasem do pierwszej awarii a procesem
awarii i napraw. [NIST, Repair rate / ROCOF](https://www.itl.nist.gov/div898/handbook/apr/section1/apr125.htm)
omawia osobno zliczanie kolejnych awarii systemu naprawialnego. Nie trzeba
uczyć tu modeli Markowa.

### Krótki most przez proces Poissona

Rozkład Poissona nie jest wymieniony w sylabusie, więc jego brak nie stanowi
luki formalnej. Jest jednak naturalnym łącznikiem: liczba zdarzeń w czasie
ma rozkład Poissona, czas do pierwszego — wykładniczy, a do k-tego — Erlanga,
przy założeniach jednorodnego procesu Poissona. Wystarczy 10–15 minut,
stała intensywność i niezależne przyrosty. Samo „stałe tempo zgłoszeń” jest
zbyt mało precyzyjne. Związek z czasami między zdarzeniami opisuje
[NIST, Exponential distribution](https://www.itl.nist.gov/div898/handbook/apr/section1/apr161.htm).

### Identyfikacja scenariuszy, skutki i kryterium decyzji

Na początku kursu warto umieścić jedną mapę procesu: ustalenie zakresu,
identyfikacja zagrożeń i scenariuszy, analiza, ocena według kryteriów,
działanie i ponowna ocena. Taki szerszy kontekst odpowiada opisowi
[ISO 31000](https://www.iso.org/standard/65694.html), ale nie oznacza
konieczności wykładania całej normy.

Przed FTA przyda się jedno ćwiczenie tworzenia listy scenariuszy: zagrożenie,
zdarzenie inicjujące, narażeni, istniejące bariery, możliwe skutki, źródła
danych. Dodać pytanie o działanie człowieka, procedury i utrzymanie, żeby
ryzyko nie zostało utożsamione z zawodnością sprzętu. Krótki bow-tie lub
drzewo zdarzeń może pokazać różne skutki tej samej inicjacji.

W 03 albo 10 domknąć jeden przykład decyzji: np. koszt dodatkowej weryfikacji
alarmu wobec oczekiwanej straty materialnej i skuteczności reakcji. Nie
utożsamiać oczekiwanej straty z pełną oceną bezpieczeństwa. Końcowa notatka
powinna wskazywać także pozostałe ryzyko, kryterium jego oceny oraz osobę
i termin sprawdzenia skuteczności działania. „Mniejsze” nie znaczy jeszcze
„wystarczająco małe”.

## Dydaktyka i drobniejsze korekty

1. **Quizy:** [risk_block.R](../R/risk_block.R), `risk_quiz_questions()`,
   uzupełnia pojedyncze pytanie każdego bloku 02–10 czterema identycznymi
   pytaniami ogólnymi. Można zdobyć 4/5 bez znajomości nowego modelu.
   Potrzebne są zadania o konkretnej treści: dobór modelu, rachunek,
   interpretacja i wykrycie złego założenia. Serwer pokazuje wynik łączny,
   ale nie wyświetla zapisanych objaśnień do poszczególnych odpowiedzi.
2. **Materiały prowadzącego:** ćwiczenia 02–10 są zasadniczo listami poleceń.
   Przy przejmowaniu przedmiotu szczególnie przyda się klucz rozwiązań,
   dopuszczalne warianty odpowiedzi i typowe pomyłki, zwłaszcza dla FTA.
3. **Czas:** katalog wraz z 01 zakłada 15 spotkań po 90 minut, czyli 30 godzin
   dydaktycznych po 45 minut. Trzeba to porównać z pełną kartą przedmiotu.
   Blok 02 ma rozbudowany Monty Hall; w razie braku czasu ograniczyłbym tę
   dygresję, aby zachować czas na samodzielny model systemu i niepewność danych.
4. **Blok 05:** przy losowym p między partiami zmienia się nie tylko ogon,
   ale także średnia liczby prób: `E(X)=rE(1/p)`, nie `r/E(p)`. Zdanie
   „średnie są zbliżone” nie jest gwarantowane przez symulację; przy dużej
   zmienności może być bardzo nietrafne. Warto pokazać oba efekty.
5. **Blok 06:** zmniejszenie σ obniża prawdopodobieństwo przekroczenia
   górnego progu tylko gdy próg leży powyżej μ. Quiz nie podaje tego warunku,
   a suwaki pozwalają go naruszyć. Przy progu poniżej średniej efekt jest
   przeciwny. Należy też ujednolicić „pomiary” i „zmiany”: jedno przekroczenie
   w losowym pomiarze nie jest automatycznie przekroczeniem choć raz na zmianie.
6. **Blok 08:** kolejne niezależne jednakowe gałęzie zmniejszają zawodność
   o stały ułamek pozostałej wartości, lecz o malejącą wartość bezwzględną.
   Zdanie o „coraz mniejszej części pozostałego ryzyka” miesza te dwie miary.
7. **Blok 09:** minimalność przekroju oznacza, że nie da się usunąć żadnego
   elementu bez utraty wystarczalności, a nie globalnie najmniejszą liczebność.
   Przy wspólnej przyczynie należy rzeczywiście wyznaczyć nowe przekroje:
   w tym drzewie `{I,C}` nadal ma dwa elementy, więc nie musi być krótszy
   niż `{I,D}` i `{I,S}`. Dodatkowo na rysunku głównego drzewa brakuje
   jawnego oznaczenia bramki AND pod TOP.
8. **Blok 02:** zaokrąglenia naturalnych częstości powodują rozbieżność
   z modelem: 1000 zmian, udział 0,1 i ryzyka 0,12 / 0,005 dają 16,5
   oczekiwanych incydentów, a tabela pokazuje 16. Należy oznaczyć zaokrąglenie
   lub dobrać większą populację, zamiast nazywać obie liczby tym samym P(A).

## Zalecana kolejność prac

| Priorytet | Działanie | Powód |
|---|---|---|
| Przed blokami 08–10 | Doprecyzować fizyczny scenariusz, bramki i typy prawdopodobieństw | Błędy modelu mogą zmieniać wynik o rząd wielkości. |
| Przed finałem | Naprawić połączenia kart, czas misji i porównywanie interwencji w scenariuszach | Studium ma uczyć integracji bez ukrytych założeń. |
| Przed użyciem quizów | Dodać pytania tematyczne i objaśnienia; przygotować klucz ćwiczeń | Obecny wynik quizu słabo potwierdza opanowanie sylabusa. |
| W bieżącej edycji | Dodać dane → niepewność, słownik niezawodności i kryterium decyzji | To najkrótsze uzupełnienie kompetencji analityka ryzyka. |
| Jeśli starczy czasu | Poisson, krótki bow-tie/ETA, zadanie z koherentności i przekrojów | Wzmacniają połączenia między tematami. |

Największą korzyść da dopracowanie interpretacji już obecnych modeli oraz
jednego pełnego zadania końcowego. Poszerzanie listy metod jest mniej pilne.
Statystyczne przygotowanie prowadzącego dobrze pasuje do tego sylabusa;
obszarem wymagającym osobnego przygotowania jest przełożenie działania
instalacji i barier na zdarzenia, warunki oraz logikę systemu.
