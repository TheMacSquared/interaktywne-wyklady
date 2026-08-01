# Ocena planów wykładu z analizy ryzyka

## Werdykt

**Jako podstawę dalszej pracy wybieram plan A.** Jest pełniejszy dydaktycznie,
ostrożniejszy merytorycznie i lepiej rozpisuje drogę studenta od intuicji do
samodzielnej decyzji. Nie rekomenduję jednak realizowania go bez zmian.

Najlepsze rozwiązanie to **plan A jako szkielet programu, uzupełniony wybranymi
elementami planu B**:

- fabułą firmy „Bananpol” i rolą studenta-inspektora;
- konkretnymi konwencjami technicznymi repozytorium;
- wspólnym modelem danych przypadku;
- podejściem MVP, kryteriami odbioru i testami obliczeń;
- finałowym FTA, które jawnie wykorzystuje wyniki z wcześniejszych wykładów.

Nie jest to kompromis „pół na pół”. Plan A powinien decydować o kolejności,
zakresie i sposobie uczenia, a plan B — o oprawie fabularnej oraz części
organizacyjno-technicznej.

## Porównanie

| Kryterium | Plan A | Plan B | Ocena |
|---|---|---|---|
| Poprawność merytoryczna | Zasadniczo dobra; problemy dotyczą głównie przeciążenia, luk pojęciowych i notacji | Kilka istotnych błędów lub zbyt daleko idących uproszczeń | Wyraźnie A |
| Konstrukcja dydaktyczna | Cele, pytania, eksperyment, pułapka, decyzja i most do kolejnego tematu | Bardzo dobre hooki i narracja, ale mniej jawnych efektów uczenia się | A |
| Realność zakresu | Dziesięć spotkań daje więcej miejsca, choć wykłady 7 i 10 są przeładowane | Siedem bardzo gęstych spotkań; szczególnie przeciążone są 02, 03 i 05 | A |
| Spójność narracyjna | Powracająca instalacja gazowa plus przykłady transferowe | Importer bananów „Bananpol” i rola inspektora tworzą bardzo mocną klamrę semestru | B |
| Przenoszenie wiedzy między kontekstami | Zaplanowane wprost | Ryzyko zbyt silnego przywiązania całego kursu do jednego zakładu | A |
| Przygotowanie do implementacji | Dobre zasady ogólne, ale mało konkretów repozytoryjnych i kryteriów odbioru | Konkretna architektura, katalogi i komponenty, choć koszt prac jest niedoszacowany | B |
| Ocenianie i informacja zwrotna | Przemyślane zadania po wykładach i blokach, nacisk na dobór modelu i interpretację | Stały quiz i ćwiczenia, ale bez powiązania z mierzalnymi efektami | A |

### Dlaczego plan A wygrywa

Największą zaletą A nie jest liczba opisanych widgetów, lecz **architektura
uczenia się**. Student najpierw rozpoznaje problem, ujawnia własną intuicję,
ogląda mechanizm, poznaje zapis, a następnie podejmuje decyzję i mierzy się z
typową pułapką. Mosty między wykładami są naturalne, szczególnie przejście od
rozkładu dwumianowego do ujemnego dwumianowego przez zamianę tego, co jest
ustalone.

A lepiej chroni też przed mechanicznym stosowaniem wzorów. Regularnie pyta o
mianownik, jednostkę ekspozycji, niezależność, czas misji, wspólne przyczyny i
ograniczenia modelu. Ważne jest również to, że głównemu przypadkowi towarzyszą
inne konteksty. Dzięki temu student może sprawdzić, czy umie przenieść metodę
poza historię poznaną na zajęciach.

### Co plan B robi lepiej

„Bananpol” jest bardziej sugestywną osią fabularną niż sama
instalacja gazowa. Rola nowego inspektora, narastająca mapa ryzyka oraz powrót
wcześniejszych wyników w końcowym FTA dają wyraźne poczucie postępu. Warto ten
pomysł zachować, ale traktować firmę jako **przypadek główny, nie jedyny świat
przykładów**.

B lepiej rozpoznaje też istniejącą architekturę projektu: system wykładów,
komponenty interfejsu, quizy, ćwiczenia i docelową strukturę katalogów. Jest
bliżej specyfikacji produktu, lecz jeszcze nie jest planem produkcyjnym — brak
mu MVP, oszacowania drogich interakcji, kamieni milowych i kryteriów odbioru.

### Humor i motyw bananowy

Importer bananów daje miejsce na lekki, powracający żart bez niszczenia
inżynierskiego charakteru kursu. **Poślizgnięcie na skórce od banana** może być
pierwszym zdarzeniem analizowanym przez studentów, a potem wracać w kolejnych
modelach:

- w języku ryzyka student rozdziela skórkę jako zagrożenie, ruch pracowników
  jako ekspozycję, poślizgnięcie jako zdarzenie i uraz jako skutek;
- w prawdopodobieństwie warunkowym porównuje ryzyko przy mokrej posadzce,
  wzmożonym ruchu i sprawnej procedurze sprzątania;
- w schemacie Bernoulliego sprawdza, czy kolejne przejścia rzeczywiście mają
  stałe prawdopodobieństwo i są niezależne;
- w FTA rozkłada zdarzenie na obecność skórki, brak wykrycia, nieskuteczne
  sprzątanie i ekspozycję pracownika.

Najlepsza puenta dydaktyczna polega na odwróceniu dowcipu: sama skórka nie jest
jeszcze wypadkiem ani ryzykiem. Potrzebne są ekspozycja, określone warunki i
możliwy skutek. Humor ujawnia więc dokładnie to rozróżnienie, którego kurs ma
uczyć.

Żart powinien dotyczyć fikcyjnej sytuacji i konwencji kursu, nie osoby
poszkodowanej. Lekkie nazwy, komunikaty widgetów i okazjonalne „bananowe”
warianty odpowiedzi są pożądane, ale opisy obrażeń, poważnych awarii i decyzji
bezpieczeństwa powinny pozostać rzeczowe. Dzięki temu humor obniża napięcie, nie
bagatelizując konsekwencji.

## Znaczenie dopisanych krytyk

Obie krytyki są wartościowe i w większości trafne. Krytyka planu A wskazuje
głównie problemy zakresu i realizacji: niespójny limit widgetów, przeciążony
wykład o czasie życia, zbyt obszerny finał, brak jawnego wprowadzenia zmiennej
losowej, słabą obecność skutków oraz brak decyzji technicznych.

Krytyka planu B ujawnia problemy poważniejsze, bo część z nich mogłaby utrwalić
błędne rozumienie:

1. Iloczyn „szansa × skutek” nie jest uniwersalną definicją ryzyka, zwłaszcza
   dla porządkowych skal macierzy 1–5.
2. Pole nakładania się gęstości obciążenia i wytrzymałości nie jest
   prawdopodobieństwem awarii. Trzeba liczyć
   `P(obciążenie > wytrzymałość)`, np. przez rozkład różnicy albo symulację par.
3. Przykład dwumianowy miesza pracowników, dni i kwartał; bez jawnej jednostki
   ekspozycji nie wiadomo, czym jest pojedyncza próba.
4. Gamma jako czas do kolejnych etapów wymaga szczególnych założeń, a pojedynczy
   Weibull nie tworzy całej krzywej wannowej.
5. Niezawodność elementu musi odnosić się do określonego czasu misji i założeń
   o zależności; nie jest bezczasową własnością urządzenia.
6. `AND = iloczyn` wymaga niezależności, a dla OR suma jest co najwyżej
   przybliżeniem w szczególnych warunkach.

To przesądza o wyborze A. B można bezpiecznie wykorzystać dopiero po poprawieniu
tych punktów.

## Rekomendowany plan scalony

Zachowałbym dziesięcioelementowy układ A, ale wprowadził następujące zmiany:

1. **Język ryzyka i prawdopodobieństwa.** Rozdzielić zagrożenie, ekspozycję,
   zdarzenie, skutek, prawdopodobieństwo i ryzyko. Macierz ryzyka pokazać jako
   jakościowe narzędzie przesiewowe wraz z ograniczeniami, a nie jako definicję.
2. **Prawdopodobieństwo warunkowe, iloczyn i całkowite.** Pozostawić osobno od
   Bayesa, jak w A. To daje czas na mianownik, drzewa i niezależność.
3. **Bayes i alarmy.** Wykorzystać naturalne częstości oraz przypadek alarmu z
   magazynu „Bananpolu”. Oddzielić jakość detektora od kosztu decyzji po alarmie.
4. **Zmienna losowa, Bernoulli i rozkład dwumianowy.** Jawnie wprowadzić zmienną
   losową, wartość oczekiwaną i rozrzut. Każdy przykład opisać przez jednostkę
   próby, okres oraz ekspozycję.
5. **Ile prób do kolejnego zdarzenia.** Zmienić tytuł A tak, aby nie sugerował
   czasu ciągłego. Utrzymać jedną parametryzację ujemnego dwumianowego i wyraźnie
   zestawić ją z implementacją w R.
6. **Rozkład normalny i przekroczenie progu.** Dla obciążenia i wytrzymałości
   użyć rozkładu różnicy lub symulacji wspólnych realizacji. Przykłady hałasu
   muszą precyzować mierzoną wielkość i okres odniesienia.
7. **Czas życia.** Rdzeń ograniczyć do `F(t)`, `R(t)`, hazardu, rozkładu
   wykładniczego i Weibulla. Gammę omówić w zakresie wymaganym przez sylabus,
   z jawnymi założeniami. Estymację, cenzorowanie, pełną krzywą wannową i
   rozbudowane MTTF przenieść do rozszerzeń.
8. **Niezawodność systemów.** Zawsze podawać wspólny czas misji. Najpierw
   policzyć model niezależny, potem dodać jawne zdarzenie wspólnej przyczyny.
   Nie używać nieobjaśnionego „suwaka korelacji”.
9. **FTA.** Najpierw logika bez liczb, później rachunek z dokładnie zapisanymi
   założeniami. Zdarzenie szczytowe musi zawierać system, skutek i okres, np.
   „nieopanowany pożar magazynu w ciągu roku”. Oddzielić inicjację od zawodności
   wykrywania i tłumienia.
10. **Studium integracyjne.** Potraktować jako pracę projektową lub ograniczyć
    do czterech kroków: definicja zdarzenia, wybór modeli, analiza systemu/FTA,
    rekomendacja z ograniczeniami. Osiem pełnych etapów nie zmieści się
    sensownie w 90 minutach.

W wykładach 1, 9 i 10 powinna wracać stała ramka o skutkach, aby kurs naprawdę
dotyczył analizy ryzyka, a nie wyłącznie prawdopodobieństwa i niezawodności.
Rozkładu Poissona nie dodawałbym jako osobnego tematu, jeśli nie ma go w
sylabusie; trzeba jednak jawnie zaznaczyć tę granicę i opcjonalnie pokazać jego
związek z czasem wykładniczym oraz gammą. W całym materiale należy też poprawić
i ujednolicić zapis matematyczny w poprawnych delimiterach LaTeX.

Jeżeli dostępnych jest tylko siedem spotkań, nie należy po prostu ścisnąć planu
B. Lepsze będą dłuższe bloki, materiały przygotowawcze lub przeniesienie części
treści do ćwiczeń. Szczególnie Bayes, rozkłady czasu życia i FTA tracą wartość,
gdy stają się szybkim przeglądem wzorów.

## Zasady implementacji

Z planu B warto przejąć strukturę techniczną, ale wdrażać ją etapami:

1. Ustalić długość spotkań, rdzeń sylabusa i znaczenie danych firmy „Bananpol”.
   Wspólny model powinien zawierać jednostki, horyzont czasu,
   definicję zdarzenia, dopuszczalny zakres i informację, że dane są fikcyjne.
2. Przygotować jeden kompletny pionowy wycinek: wykład 1 ze ściągą, quizem,
   ćwiczeniami, testami obliczeń i kontrolą layoutu.
3. Dla każdego wykładu wybrać **jeden widget kluczowy**, maksymalnie dwa proste
   uzupełniające oraz jawnie oznaczyć rozszerzenia. Konstruktora systemów,
   drag-and-drop i edytowalne FTA nie traktować jako elementów pierwszego MVP.
4. Oddzielić czyste funkcje matematyczne od reaktywnego UI. Testować znane
   wyniki, przypadki brzegowe, jednostki i zgodność przyjętych parametryzacji z R.
5. Wprowadzić kryteria odbioru: uruchomienie bez błędów, smoke test, kontrakt
   designu, czytelność mobilna, obsługa klawiatury, brak informacji przekazywanej
   wyłącznie kolorem oraz poprawne objaśnienie każdego wyniku.
6. Dopiero po pomiarze kosztu pierwszego wykładu oszacować całą serię i ustalić
   liczbę pytań. Pula 40–60 pytań na każdy wykład nie powinna być domyślnym
   wymaganiem pierwszej wersji.

## Ostateczna rekomendacja

Przyjąć **plan A po korekcie** jako dokument nadrzędny. Z planu B włączyć firmę
„Bananpol”, rolę inspektora, motyw poślizgnięcia na skórce,
narastającą mapę przypadku, repozytoryjne konwencje oraz pomysł finałowego FTA
ponownie używającego wcześniejszych danych.

Przed kodowaniem trzeba obowiązkowo poprawić model obciążenie–wytrzymałość,
jednostki ekspozycji, interpretację gammy i krzywej wannowej, czas misji oraz
logikę FTA. Następnie warto wykonać jeden pełny wykład pilotażowy. Taki wariant
zachowuje największą siłę A — przemyślaną drogę uczenia — i największą siłę B —
spójny świat oraz konkret implementacyjny — bez przejmowania ich najsłabszych
elementów.
