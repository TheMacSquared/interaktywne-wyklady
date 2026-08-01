# Analiza ryzyka — plan kanoniczny

Status: przyjęty do realizacji. Wszystkie dziesięć wykładów ma uruchamialną
strukturę globalną; wykład 01 jest rozwiniętą aplikacją referencyjną. Dokument
scala plan A z wybranymi elementami planu B oraz werdyktem z
`ocena-planow.md`.

## Założenie dydaktyczne

Kurs prowadzi od intuicji i obserwowanych częstości do prostych modeli
prawdopodobieństwa, niezawodności oraz FTA. Każdy model pojawia się dopiero po
problemie, który uzasadnia jego użycie.

Stały rytm:

1. pytanie lub decyzja przed wzorem;
2. głosowanie indywidualne;
3. eksperyment albo obraz;
4. nazwa, minimalny zapis i obliczenie;
5. interpretacja w naturalnych częstościach;
6. decyzja inżynierska;
7. pułapka i ograniczenie modelu.

Po kursie student potrafi zdefiniować zdarzenie i ekspozycję, dobrać model do
pytania, sprawdzić najważniejsze założenia, policzyć prosty system i FTA oraz
przekazać wynik jako rekomendację z ograniczeniami.

## Przypadek przewodni: Bananpol

Bananpol jest fikcyjnym importerem bananów. Firma obejmuje rampę rozładunkową,
dojrzewalnię i chłodnię, magazyn wysokiego składowania, linię sortowania i
pakowania, wózki widłowe, instalację chłodniczą, alarmy oraz zabezpieczenia
przeciwpożarowe.

Student wciela się w nowego inspektora bezpieczeństwa. W kolejnych wykładach
uzupełnia mapę ryzyka Bananpolu, a w końcowym FTA wykorzystuje parametry poznane
wcześniej. Dane są jawnie fikcyjne i mają wspólne metadane: jednostkę, okres,
definicję zdarzenia oraz dopuszczalny zakres.

Powracającym lekkim motywem jest poślizgnięcie na skórce od banana. Żart służy
rozróżnieniu zagrożenia, ekspozycji, zdarzenia i skutku. Humor dotyczy sytuacji
i języka modeli, nie osoby poszkodowanej; opisy obrażeń i decyzji pozostają
rzeczowe.

Bananpol jest przypadkiem głównym, ale nie jedynym. Każdy blok zawiera co
najmniej jeden krótki przykład transferowy z innej domeny bezpieczeństwa.

## Mapa serii

| Nr | Wykład | Pytanie przewodnie | Rdzeń |
|---:|---|---|---|
| 01 | Język ryzyka | Co właściwie może się wydarzyć i spośród czego liczymy? | zdarzenia, przestrzeń, definicja klasyczna, częstość |
| 02 | Warunki zmieniają ocenę | Co zmienia się po poznaniu warunku? | warunkowe, iloczyn, całkowite, zależność |
| 03 | Alarm i prawda | Czy alarm oznacza awarię? | Bayes, naturalne częstości, częstość bazowa |
| 04 | Wiele prób | Ile zdarzeń wystąpi w ustalonej liczbie prób? | zmienna losowa, Bernoulli, dwumianowy |
| 05 | Ile prób do zdarzenia | Ile prób potrzeba do kolejnego wykrycia? | geometryczny i ujemny dwumianowy |
| 06 | Zmienność i próg | Jak często przekraczamy granicę? | normalny, standaryzacja, rozkład różnicy |
| 07 | Czas życia | Czy element psuje się losowo, czy się zużywa? | `F(t)`, `R(t)`, hazard, wykładniczy, gamma, Weibull |
| 08 | Niezawodność systemu | Jak architektura zmienia działanie całości? | system szeregowy, równoległy, czas misji |
| 09 | Drzewo błędów | Jak kombinacje przyczyn prowadzą do skutku? | FTA, AND/OR, wspólne przyczyny |
| 10 | Od modelu do decyzji | Którą barierę poprawić najpierw? | małe studium integracyjne i notatka decyzyjna |

## Reguły merytoryczne

- Ryzyko nie jest uniwersalnie definiowane jako iloczyn ocen
  „prawdopodobieństwo × skutek”. Macierz jest jakościowym narzędziem
  przesiewowym, a kurs skupia się głównie na składowej probabilistycznej.
- Każde prawdopodobieństwo ma nazwane zdarzenie, mianownik, jednostkę ekspozycji
  i okres odniesienia.
- Niezawodność `R(t)` zawsze odnosi się do wspólnego czasu misji.
- Niezależność nigdy nie jest założeniem domyślnym; model niezależny jest
  zestawiany ze wspólną przyczyną.
- Dla obciążenia `L` i wytrzymałości `S` awaria to `L > S`. Liczymy rozkład
  różnicy albo symulujemy pary; nie utożsamiamy nakładania gęstości z ryzykiem.
- Gamma jako czas do kolejnych zdarzeń jest omawiana z założeniami procesu;
  krzywa wannowa nie jest przedstawiana jako pojedynczy Weibull.
- W FTA iloczyn dla AND wymaga niezależności. Dla OR stosujemy dokładne
  dopełnienie, gdy założenia na to pozwalają; suma jest tylko oznaczonym
  przybliżeniem.
- Skutki wracają jawnie w wykładach 01, 09 i 10, aby kurs nie stał się wyłącznie
  kursem prawdopodobieństwa.

## Zakres wykładu 01 — MVP

Tytuł: **Od zagrożenia do prawdopodobieństwa**.

Efekty uczenia się — student potrafi:

1. odróżnić zagrożenie, ekspozycję, zdarzenie, skutek i zabezpieczenie;
2. wskazać przestrzeń wyników oraz zdarzenie jako jej podzbiór;
3. zastosować klasyczną definicję tylko dla wyników jednakowo możliwych;
4. porównać częstość empiryczną z prawdopodobieństwem modelowym;
5. wyjaśnić, dlaczego sama liczba wypadków bez mianownika i okresu nie wystarcza.

Interakcje MVP:

- **Sytuacja ryzykowna:** klasyfikowanie elementów historii o skórce od banana,
  ze sprawdzeniem i objaśnieniem każdego elementu;
- **Teoria kontra obserwacje:** symulacja kolejnych dni, która pokazuje
  stabilizację częstości poślizgnięć wokół przyjętego `p`;
- **Przestrzeń zdarzeń:** siatka jednakowo możliwych palet, na której student
  zmienia liczbę wyników sprzyjających i obserwuje licznik, mianownik oraz
  dopełnienie.

Wykład kończą: ściąga, krótki quiz z informacją zwrotną oraz ćwiczenie, w którym
student poprawia niepełny komunikat „w tym miesiącu były trzy wypadki”.

## Granice pierwszej wersji

- Jeden widget kluczowy i najwyżej dwa proste uzupełniające na wykład.
- Bez drag-and-drop; klasyfikacja używa dostępnych kontrolek i pełnej obsługi
  klawiatury.
- Bez edytora grafów oraz swobodnego konstruktora FTA w pierwszej wersji.
- Quiz MVP ma 5–8 sprawdzonych pytań, nie docelową pulę 40–60.
- Funkcje matematyczne są oddzielone od reaktywnego UI i mają testy przypadków
  brzegowych.

## Kryteria odbioru pionowego wycinka

- Aplikacja ładuje się bez błędów z katalogu głównego repozytorium.
- Pliki R przechodzą `parse()`.
- Funkcje obliczeniowe mają deterministyczne testy dla wartości typowych oraz
  granicznych.
- Kod używa `lecture_page()` i komponentów `lc_*`, bez zakazanych wzorców.
- Wszystkie wykresy mają polskie etykiety i tekst alternatywny.
- Informacja nie jest przekazywana wyłącznie kolorem; interakcje są dostępne z
  klawiatury.
- Dane Bananpolu mają jednostkę, okres, definicję zdarzenia i oznaczenie
  „dane fikcyjne”.

## Kolejność produkcyjna

1. własny snapshot systemu layoutu w `analiza-ryzyka/R/`;
2. wspólny model danych Bananpolu;
3. kompletny wykład 01 wraz z testami i README;
4. prototypy dwóch najbardziej ryzykownych interakcji: systemów i FTA;
5. wykłady 02–03, następnie 08–09, potem 04–07;
6. studium integracyjne dopiero z gotowych elementów wcześniejszych wykładów.

Po pilotażu wykładu 01 należy ponownie oszacować liczbę interakcji, pytań i czas
produkcji całej serii.
