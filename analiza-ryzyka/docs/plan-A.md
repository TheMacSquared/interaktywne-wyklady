# Plan A — interaktywny wykład z analizy ryzyka

## 1. Założenie dydaktyczne

Seria ma być odpowiednikiem interaktywnych wykładów ze statystyki: dużo intuicji,
krótkie porcje teorii, przykłady z inżynierii bezpieczeństwa i widgety, w których
student może zobaczyć skutek zmiany założeń. Matematyka ma porządkować intuicję,
a nie otwierać tematu.

Proponowany format to **10 spotkań po około 90 minut**. Każde spotkanie stanowi
osobny interaktywny wykład/aplikację. Jeżeli kurs ma tylko 9 spotkań, wykład 10
może zostać potraktowany jako ćwiczenie podsumowujące lub projekt zaliczeniowy.

Główna myśl kursu:

> Ryzyko nie jest jedną liczbą znalezioną we wzorze. Jest opowieścią o tym, co
> może się wydarzyć, jak często, pod jakimi warunkami, z jakimi skutkami i które
> zabezpieczenie najbardziej opłaca się poprawić.

### Efekt końcowy

Po kursie student powinien umieć:

1. przełożyć opis sytuacji na zdarzenia i prawdopodobieństwa;
2. rozpoznać, kiedy potrzebne jest prawdopodobieństwo warunkowe, schemat
   Bernoulliego albo konkretny rozkład;
3. odróżnić „liczbę awarii”, „czas do awarii” i „prawdopodobieństwo awarii”;
4. policzyć niezawodność prostego systemu szeregowego i równoległego;
5. zbudować i przeczytać proste drzewo błędów;
6. wskazać, które założenie lub element systemu ma największy wpływ na wynik;
7. zakomunikować wynik jako podstawę decyzji, a nie jako pozornie pewną prognozę.

## 2. Narracja całej serii

### Oś fabularna: od zdarzenia do systemu

Kurs prowadzi przez pięć kolejnych pytań:

1. **Co może się zdarzyć?** — zdarzenia losowe i podstawy prawdopodobieństwa.
2. **Co zmienia naszą ocenę?** — warunki, nowe informacje i Bayes.
3. **Ile razy lub kiedy?** — Bernoulli, rozkłady liczby zdarzeń i czasu życia.
4. **Co się stanie, gdy połączymy elementy?** — niezawodność systemów.
5. **Dlaczego może dojść do zdarzenia szczytowego?** — FTA i decyzje o
   zabezpieczeniach.

Przez serię powraca fikcyjny, ale realistyczny przypadek **instalacji sprężonego
gazu w hali produkcyjnej**. Ma zawór, czujniki, alarm, wentylację i zasilanie
awaryjne. W kolejnych wykładach pytamy m.in. o fałszywe alarmy, liczbę uszkodzeń,
czas pracy elementów i awarię całego układu. Dzięki temu student widzi, że wzory
nie są osobnymi tematami, lecz kolejnymi narzędziami do opisu tego samego systemu.

Obok przypadku głównego pojawiają się krótsze sytuacje z BHP, bezpieczeństwa
pożarowego, transportu, infrastruktury krytycznej i ochrony środowiska. Główny
przypadek nie powinien być jedynym kontekstem, aby student nauczył się przenosić
rozumowanie między domenami.

### Stały rytm pojedynczego wykładu

Każdy wykład powinien mieć podobną dramaturgię:

1. **Zdarzenie otwierające** — krótki problem lub decyzja bez podawania wzoru.
2. **Głosowanie intuicyjne** — student wybiera odpowiedź przed obliczeniami.
3. **Eksperyment** — widget pokazuje mechanizm na częstościach, obiektach albo
   osi czasu.
4. **Nazwa i zapis** — dopiero teraz pojawia się pojęcie oraz minimalny wzór.
5. **Decyzja inżynierska** — co wynik zmienia w projekcie, kontroli lub
   utrzymaniu systemu?
6. **Pułapka** — jedna typowa błędna interpretacja i kontrprzykład.
7. **Jednozdaniowy most** — po co potrzebny będzie następny wykład.

Zasada dla wzorów: **intuicja → obraz/częstości → wzór → interpretacja → decyzja**.
Nie wprowadzać wzoru, którego student nie użyje zaraz w przykładzie lub widgecie.

### Język kursu

- „Sukces” w rozkładzie oznacza wyróżnione zdarzenie, niekoniecznie coś dobrego;
  awaria może być „sukcesem” próby Bernoulliego.
- Zawsze nazywać licznik i mianownik prawdopodobieństwa warunkowego słowami.
- Obok wyniku dziesiętnego podawać naturalną częstość, np. „0,002 = około 2 na
  1000 podobnych okresów”.
- Oddzielać **zagrożenie**, **zdarzenie**, **skutek**, **prawdopodobieństwo** i
  **ryzyko**. Na początku można roboczo użyć modelu „ryzyko = możliwość zdarzenia
  + dotkliwość skutków”, a macierz (P \times S\) pokazać jako narzędzie
  porządkujące, nie jako uniwersalną definicję ryzyka.
- Każdy wynik kończyć zdaniem w formie decyzji: „co sprawdzić?”, „co poprawić?”,
  „czego jeszcze nie wiemy?”.

## 3. Mapa serii

| Nr | Tytuł wykładu | Pytanie przewodnie | Główne narzędzie |
|---:|---|---|---|
| 1 | Od zagrożenia do prawdopodobieństwa | Co właściwie mierzymy? | zdarzenia, klasyczna definicja |
| 2 | Warunki zmieniają ryzyko | Co się zmienia, gdy wiemy więcej? | warunkowe, iloczyn, całkowite |
| 3 | Alarm nie zawsze oznacza awarię | Jak odwracać warunek? | wzór Bayesa |
| 4 | Wiele prób, jedna szansa | Ile awarii pojawi się w ustalonej liczbie prób? | Bernoulli, dwumianowy |
| 5 | Jak długo do kolejnego zdarzenia? | Ile prób potrzeba do kolejnych awarii? | ujemny dwumianowy |
| 6 | Zmienność ciągła i próg bezpieczeństwa | Jak często przekroczymy granicę? | rozkład normalny |
| 7 | Czas życia elementu | Czy element zużywa się, czy psuje losowo? | wykładniczy, gamma, Weibull |
| 8 | Od elementu do systemu | Jak architektura zmienia niezawodność? | system szeregowy i równoległy |
| 9 | Drzewo błędów | Jak wiele przyczyn składa się na jeden wypadek? | FTA, AND/OR |
| 10 | Od modelu do decyzji | Które zabezpieczenie poprawić najpierw? | studium integracyjne |

## 4. Szczegółowy plan wykładów

### Wykład 1. Od zagrożenia do prawdopodobieństwa

**Pytanie otwierające:** czy zdarzenie „1 wypadek na 100 pracowników” jest bardziej
prawdopodobne niż „5 wypadków na 1000 pracowników”? Najpierw głosowanie, potem
rozmowa o mianowniku, okresie obserwacji i porównywalności ekspozycji.

**Narracja:** zanim zaczniemy liczyć ryzyko, musimy zbudować język zdarzeń i
ustalić przestrzeń możliwych wyników. Zaczynamy od prostych, symetrycznych
doświadczeń, a kończymy pytaniem, dlaczego prawdopodobieństwo awarii prawdziwego
urządzenia nie wynika z „liczby korzystnych przypadków”.

**Treść:**

- zagrożenie, ekspozycja, zdarzenie niepożądane, skutek i ryzyko;
- doświadczenie losowe, wynik, przestrzeń zdarzeń elementarnych, zdarzenie;
- klasyczna definicja (P(A)=|A|/|\Omega|) i warunek jednakowej możliwości
  wyników;
- dopełnienie, suma zdarzeń, część wspólna, zdarzenia rozłączne;
- częstość empiryczna a prawdopodobieństwo modelowe;
- krótka granica stosowalności definicji klasycznej w analizie realnych awarii.

**Widgety rdzeniowe:**

1. **„Mapa sytuacji ryzykownej”** — student przyporządkowuje karty opisu do
   kategorii: zagrożenie / ekspozycja / zdarzenie / skutek / zabezpieczenie.
   Natychmiastowy feedback koryguje utożsamianie zagrożenia z ryzykiem.
2. **„Zbuduj przestrzeń zdarzeń”** — wybór doświadczenia (moneta, dwie kostki,
   kontrola dwóch czujników), wizualna siatka wszystkich równoprawdopodobnych
   wyników i zaznaczanie zdarzenia. Widget pokazuje licznik, mianownik oraz
   wynik (P(A)).
3. **„Teoria kontra seria obserwacji”** — symulacja 10, 100, 1000 i 10 000 prób;
   wykres częstości zbiega do wartości modelowej. Możliwość przełączenia na
   „obciążony” mechanizm pokazuje, że sama klasyczna symetria może zawieść.
4. **„Zbiory zdarzeń”** — interaktywny diagram lub siatka obiektów dla (A\),
   (B), (A\cup B), (A\cap B) i (A^c); student najpierw zaznacza obszar,
   dopiero potem widzi zapis.

**Aha-moment:** mianownik jest częścią modelu. Bez określenia „spośród czego?”
liczba zdarzeń nie opisuje prawdopodobieństwa.

**Pułapka:** zdarzenia rozłączne nie są tym samym co niezależne.

**Most:** w prawdziwym systemie wyniki zwykle nie są równoprawdopodobne, a wiedza
o warunkach pracy zmienia ocenę — potrzebujemy prawdopodobieństwa warunkowego.

---

### Wykład 2. Warunki zmieniają ryzyko

**Pytanie otwierające:** ryzyko pożaru wynosi 1%, ale wśród przypadków z
przegrzaniem instalacji 12%. Która liczba opisuje sytuację po wykryciu
przegrzania?

**Narracja:** prawdopodobieństwo warunkowe to zawężenie świata do przypadków, w
których warunek już zaszedł. Drzewo prawdopodobieństwa pozwala następnie składać
etapy procesu i sumować różne drogi prowadzące do tego samego skutku.

**Treść:**

- intuicja „filtrujemy mianownik” dla (P(A\mid B));
- tablica 2×2, naturalne częstości i diagram drzewiasty jako trzy reprezentacje;
- reguła iloczynu (P(A\cap B)=P(A\mid B)P(B));
- niezależność jako szczególny przypadek, a nie domyślne założenie;
- wzór na prawdopodobieństwo całkowite jako suma rozłącznych dróg;
- zdarzenia zależne, wspólna przyczyna i dlaczego nie wolno automatycznie mnożyć.

**Widgety rdzeniowe:**

1. **„Filtruj halę”** — 1000 symbolicznych zmian roboczych; student filtruje
   kolejno zmiany z wysoką temperaturą, niesprawną wentylacją i incydentem.
   Widget na żywo zmienia mianownik i porównuje (P(A)), (P(A\mid B)) oraz
   (P(B\mid A)).
2. **„Ta sama sytuacja w trzech widokach”** — zsynchronizowana tablica 2×2,
   drzewo i diagram obszarów. Kliknięcie jednej gałęzi podświetla odpowiadające
   komórki i pokazuje regułę iloczynu.
3. **„Dwie drogi do zdarzenia”** — awaria może wystąpić w trybie normalnej pracy
   lub przeciążenia. Suwaki zmieniają udział trybów i warunkowe częstości awarii;
   wynik jest składany według wzoru na prawdopodobieństwo całkowite.
4. **„Niezależne czy tylko tak założyliśmy?”** — porównanie wyniku mnożenia z
   wynikiem dla awarii wywołanych wspólną utratą zasilania. Student widzi, jak
   niewielka zależność może zmienić ryzyko rzadkiego zdarzenia.

**Aha-moment:** warunek nie dodaje informacji do licznika — zmienia populację,
względem której liczymy.

**Pułapka:** (P(A\mid B)\neq P(B\mid A)), a (P(A\cap B)=P(A)P(B)) tylko przy
niezależności.

**Most:** potrafimy przejść od przyczyny do obserwacji. Alarm daje jednak
obserwację, a interesuje nas ukryta przyczyna — trzeba odwrócić warunek.

---

### Wykład 3. Alarm nie zawsze oznacza awarię — Bayes

**Pytanie otwierające:** czujnik wykrywa 99% wycieków i daje 1% fałszywych
alarmów. Czy po alarmie prawdopodobieństwo wycieku wynosi 99%? Student głosuje,
zanim pozna częstość bazową wycieku.

**Narracja:** Bayes nie jest magicznym wzorem, lecz porównaniem wszystkich dróg,
które mogły wytworzyć obserwowany alarm. Najpierw rozwiązujemy problem na 10 000
przypadków, później skracamy to rozumowanie do wzoru.

**Treść:**

- czułość, swoistość, wynik fałszywie dodatni i fałszywie ujemny w języku
  systemów detekcji;
- częstość bazowa (prior), wiarygodność obserwacji i prawdopodobieństwo po
  aktualizacji (posterior);
- wzór Bayesa wyprowadzony z drzewa i reguły iloczynu;
- błąd ignorowania częstości bazowej;
- aktualizacja po dodatkowej informacji lub drugim teście;
- koszt fałszywego alarmu i przeoczenia jako osobny problem decyzyjny — nie
  mieszać go z samym prawdopodobieństwem.

**Widgety rdzeniowe:**

1. **„10 000 zmian roboczych”** — ikony dzielą się na prawdziwe wycieki,
   prawdziwe alarmy i fałszywe alarmy. Suwaki: częstość wycieku, czułość,
   swoistość. Najważniejszym wynikiem jest „spośród alarmów, ile oznacza wyciek?”.
2. **„Od drzewa do Bayesa”** — animacja krokowa podświetla licznik (droga:
   wyciek i alarm) i mianownik (wszystkie drogi kończące się alarmem), po czym
   pokazuje skrócony wzór.
3. **„Pomyłka warunków”** — dwa duże kafle (P(alarm\mid wyciek)) i
   (P(wyciek\mid alarm)), z dynamicznym kontrprzykładem dla rzadkiego zdarzenia.
4. **„Drugi niezależny test?”** — student dodaje potwierdzenie innym czujnikiem
   i obserwuje aktualizację. Widget musi jawnie ostrzegać, że mnożenie dowodów
   wymaga założenia warunkowej niezależności testów.

**Aha-moment:** bardzo dobry czujnik może generować więcej fałszywych niż
prawdziwych alarmów, gdy zdarzenie jest bardzo rzadkie.

**Pułapka:** parametry czujnika nie są prawdopodobieństwem awarii po alarmie.

**Most:** pojedyncza próba odpowiada „alarm / brak alarmu” albo „awaria / brak
awarii”. Następny krok to wiele takich prób.

---

### Wykład 4. Wiele prób, jedna szansa — Bernoulli i rozkład dwumianowy

**Pytanie otwierające:** pojedynczy zawór ma 2% szansy niesprawności w teście.
Czy w partii 100 zaworów najbardziej prawdopodobne jest zero awarii?

**Narracja:** schemat Bernoulliego jest maszyną produkującą ciąg dwóch wyników.
Rozkład dwumianowy nie opisuje pojedynczej próby, tylko liczbę wyróżnionych
zdarzeń w z góry ustalonej liczbie prób.

**Treść:**

- próba Bernoulliego i znaczenie parametru (p);
- warunki: stałe (n), dwa wyniki, stałe (p), niezależność prób;
- (X\sim Bin(n,p)), intuicja składników wzoru i rola kombinacji;
- wartość oczekiwana (np), wariancja (np(1-p));
- pytania „dokładnie”, „co najmniej”, „nie więcej niż” oraz dopełnienie;
- prawdopodobieństwo co najmniej jednej awarii;
- rozróżnienie: oczekiwana liczba nie jest gwarantowaną liczbą.

**Widgety rdzeniowe:**

1. **„Linia prób Bernoulliego”** — kliknięcie uruchamia serię testów zaworów;
   widać każdą próbę, zliczenie awarii i zmienność między seriami.
2. **„Kształt (Bin(n,p))”** — suwaki (n) i (p), słupki rozkładu, zaznaczenie
   (E(X)) i obszaru dla wybranego pytania: dokładnie / co najmniej / najwyżej.
3. **„Co najmniej jedna”** — porównanie bezpośredniego sumowania z prostszym
   dopełnieniem (1-(1-p)^n). Kontekst: brak zadziałania przynajmniej jednej z
   wielu jednakowych barier podczas kontroli.
4. **„Sprawdź założenia”** — krótkie karty scenariuszy do przeciągnięcia na
   „dwumianowy / nie dwumianowy”; feedback wskazuje, które założenie jest
   naruszone.

**Aha-moment:** małe (p) nie oznacza małego prawdopodobieństwa, że zdarzenie
pojawi się gdziekolwiek w dużej liczbie prób.

**Pułapka:** (np=2) nie znaczy, że zawsze wystąpią dwie awarie.

**Most:** rozkład dwumianowy ustala liczbę prób i pyta o liczbę zdarzeń. Czasem
ustalamy liczbę zdarzeń i pytamy, jak długo trzeba na nie czekać.

---

### Wykład 5. Jak długo do kolejnego zdarzenia — rozkład ujemny dwumianowy

**Pytanie otwierające:** kontrole prowadzi się do wykrycia trzeciej niesprawnej
sztuki. Ile elementów trzeba będzie przeciętnie skontrolować i jak duży zapas
czasu przygotować?

**Narracja:** zmieniamy pytanie, choć mechanizm Bernoulliego zostaje ten sam.
Dwumianowy ma stałą liczbę prób; ujemny dwumianowy zatrzymuje doświadczenie po
ustalonej liczbie zdarzeń.

**Uwaga o parametryzacji:** od początku konsekwentnie definiować (X) jako
**liczbę prób potrzebnych do uzyskania (r)-tego zdarzenia**. W materiałach
dodać krótką notkę, że część podręczników i funkcji programistycznych liczy tylko
porażki przed (r)-tym sukcesem, przez co wyniki różnią się o (r).

**Treść:**

- od rozkładu geometrycznego ((r=1)) do ujemnego dwumianowego;
- stałe (r), zmienna liczba prób, parametr (p);
- intuicja kształtu i długiego prawego ogona;
- wartość oczekiwana liczby prób (r/p) w przyjętej parametryzacji;
- prawdopodobieństwo osiągnięcia (r) zdarzeń do określonej próby;
- dobór modelu: dwumianowy kontra ujemny dwumianowy;
- ograniczenia: zmienne (p), uczenie się, zmęczenie i zależność kolejnych prób.

**Widgety rdzeniowe:**

1. **„Zatrzymaj po (r)-tej awarii”** — animowana sekwencja kontroli kończy się
   po zadanej liczbie wykryć. Powtórzenie 1000 serii buduje histogram liczby
   potrzebnych prób.
2. **„Stałe (n) czy stałe (r)?”** — zsynchronizowane eksperymenty: po lewej
   dwumianowy, po prawej ujemny dwumianowy. Student wybiera pytanie, a aplikacja
   pokazuje właściwy model i uzasadnienie jednym zdaniem.
3. **„Plan zasobów kontroli”** — suwaki (p), (r) i limit liczby inspekcji;
   wyniki: średnia liczba prób oraz prawdopodobieństwo ukończenia zadania przed
   limitem.
4. **„Dwa sposoby liczenia”** — wizualna oś prób pokazuje różnicę między
   „liczbą wszystkich prób” a „liczbą porażek przed (r)-tym sukcesem”.

**Aha-moment:** zmiana tego, co jest stałe, całkowicie zmienia zmienną losową,
mimo że pojedyncze próby wyglądają identycznie.

**Pułapka:** nie mieszać liczby prób z liczbą porażek i zawsze sprawdzać
parametryzację rozkładu.

**Most:** liczba prób jest dyskretna, ale czas, temperatura, obciążenie i stężenie
zmieniają się w sposób ciągły.

---

### Wykład 6. Zmienność ciągła i próg bezpieczeństwa — rozkład normalny

**Pytanie otwierające:** średnie stężenie jest poniżej dopuszczalnego progu. Czy
to wystarcza, by uznać proces za bezpieczny?

**Narracja:** dla bezpieczeństwa często ważniejszy od średniej jest ogon
rozkładu. Rozkład normalny służy do przejścia od „typowej wartości” do pytania o
przekroczenie progu.

**Treść:**

- zmienna ciągła, gęstość i pole pod krzywą;
- dlaczego (P(X=x)=0), a interesują nas przedziały;
- parametry μ i σ oraz ich wpływ na położenie i rozrzut;
- reguła 68–95–99,7 jako intuicja, nie metoda do każdego zadania;
- standaryzacja i wynik (z) interpretowany jako odległość od średniej;
- prawdopodobieństwo przekroczenia progu i margines bezpieczeństwa;
- kiedy normalny jest złym modelem: skośność, wartości tylko dodatnie, ciężkie
  ogony i ekstrapolowanie bardzo rzadkich zdarzeń.

**Widgety rdzeniowe:**

1. **„Przesuń i rozszerz dzwon”** — suwaki μ i σ, kilka punktów odniesienia,
   jednoczesna zmiana krzywej i naturalnego opisu „typowo / nietypowo”.
2. **„Próg bezpieczeństwa”** — suwak progu stężenia lub obciążenia, cieniowany
   ogon i wynik jednocześnie jako procent oraz „około 1 na N pomiarów”.
3. **„Trzy drogi redukcji przekroczeń”** — obniżenie średniej, zmniejszenie
   zmienności albo podniesienie wytrzymałości. Student porównuje, która zmiana
   najbardziej zmniejsza pole ryzyka.
4. **„Ten sam z-score”** — różne jednostki (temperatura, stężenie, hałas) są
   sprowadzane do wspólnej skali odległości od średniej.
5. **„Normalny czy nie?”** — nakładanie krzywej normalnej na dane symetryczne,
   skośne i z obserwacjami odstającymi; nacisk na konsekwencje dla ogona.

**Aha-moment:** proces może mieć akceptowalną średnią, a mimo to zbyt często
przekraczać próg z powodu dużej zmienności.

**Pułapka:** dopasowanie środka rozkładu nie gwarantuje dobrego opisu rzadkich
zdarzeń w ogonie.

**Most:** pomiar stężenia opisuje stan w chwili. W niezawodności pytamy często o
całą oś czasu: jak długo element będzie działał?

---

### Wykład 7. Czas życia elementu — wykładniczy, gamma i Weibull

**Pytanie otwierające:** dwa urządzenia mają ten sam średni czas życia 1000 h.
Czy są równie niezawodne, jeśli jedno psuje się losowo, a drugie zużywa się wraz
z czasem?

**Narracja:** średni czas do awarii nie opisuje mechanizmu awarii. Potrzebujemy
trzech powiązanych widoków: rozkładu czasu życia, funkcji przeżycia (R(t)) i
hazardu (h(t)). Różne rozkłady są różnymi opowieściami o tym, jak ryzyko zmienia
się z wiekiem.

**Treść:**

- czas do awarii jako dodatnia zmienna losowa;
- gęstość (f(t)), dystrybuanta (F(t)), niezawodność/przeżycie
  (R(t)=P(T>t)=1-F(t)) i hazard (h(t));
- rozkład wykładniczy: stała intensywność awarii i bezpamięciowość;
- rozkład gamma: czas oczekiwania do kolejnych zdarzeń/etapów;
- rozkład Weibulla: parametr skali i kształtu; β<1, β=1, β>1;
- związek Weibulla z awariami wczesnymi, losowymi i zużyciowymi;
- MTTF jako podsumowanie, które nie zastępuje krzywej przeżycia;
- krzywa wannowa jako złożenie faz, a nie pojedynczy rozkład Weibulla;
- opcjonalnie: intuicyjne wyjaśnienie cenzorowania — działające elementy też
  niosą informację, choć formalna estymacja nie należy do programu.

**Widgety rdzeniowe:**

1. **„Jedna populacja, trzy widoki”** — wspólny suwak czasu podświetla ten sam
   punkt na (f(t)), (R(t)) i (h(t)); obok zdanie tłumaczące każde znaczenie.
2. **„Czy urządzenie pamięta wiek?”** — porównanie prawdopodobieństwa dalszego
   działania nowego i już długo działającego elementu dla rozkładu
   wykładniczego; potem kontrast z Weibullem.
3. **„Weibull jako opowieść o mechanizmie”** — suwaki β i η, dynamiczne krzywe
   przeżycia i hazardu oraz etykieta: awarie wczesne / losowe / zużycie.
4. **„Ten sam MTTF, inne ryzyko”** — porównanie dwóch rozkładów o podobnej
   średniej, ale różnych ogonach i hazardach. Student odczytuje (R(t)) dla
   planowanego okresu misji.
5. **„Gamma: czekamy na (k)-te zdarzenie”** — animowana oś czasu zdarzeń i
   rozkład czasu do (k)-tego incydentu; most do ujemnego dwumianowego.

**Aha-moment:** β Weibulla jest informacją o mechanizmie awarii, nie tylko
parametrem zmieniającym kształt wykresu.

**Pułapka:** stały hazard nie oznacza, że element „nie może się zestarzeć”; jest
to założenie modelu, które może być nieadekwatne.

**Most:** nawet dobrze opisany element pracuje jako część większej architektury.
Teraz pytamy, jak połączenia zmieniają niezawodność całości.

---

### Wykład 8. Od elementu do systemu — układy szeregowe i równoległe

**Pytanie otwierające:** dwa elementy po 90% niezawodności — czy system ma 81%,
90% czy 99%? Wszystkie trzy odpowiedzi mogą być sensowne, zależnie od
architektury i definicji sukcesu systemu.

**Narracja:** system nie jest średnią jego części. Najpierw definiujemy, kiedy
system działa, potem przekładamy logikę działania na zdarzenia, a dopiero na
końcu liczymy.

**Treść:**

- niezawodność elementu i systemu dla zadanego czasu misji;
- układ szeregowy: wszystkie elementy muszą działać;
- układ równoległy: wystarczy co najmniej jedna działająca gałąź;
- obliczenia przez zdarzenie przeciwne;
- mieszane układy szeregowo-równoległe i redukcja blok po bloku;
- redundancja i malejące korzyści z dokładania kolejnych elementów;
- założenie niezależności, wspólna przyczyna, wspólne zasilanie i wspólne
  środowisko;
- różnica między redundancją sprzętową a rzeczywistą niezależnością.

**Widgety rdzeniowe:**

1. **„Przełącz architekturę”** — te same elementy i ich niezawodności są
   łączone szeregowo albo równolegle; animacja pokazuje, które kombinacje awarii
   zatrzymują system.
2. **„Konstruktor prostego systemu”** — student dodaje 2–6 elementów i wybiera
   układ; diagram, wzór i wynik aktualizują się równocześnie. Zakres ograniczony
   do czytelnych struktur blokowych, bez pełnego edytora grafów.
3. **„Cena redundancji”** — liczba równoległych elementów kontra niezawodność,
   koszt i przyrost korzyści. Celem nie jest optymalizacja finansowa, lecz
   zobaczenie malejących przyrostów.
4. **„Wspólna przyczyna kasuje redundancję”** — suwak udziału awarii wspólnej,
   np. utraty zasilania lub zalania; porównanie modelu niezależnego z modelem
   uwzględniającym wspólną przyczynę.
5. **„Który element poprawić?”** — podniesienie niezawodności jednego elementu
   o ten sam koszt i obserwacja zmiany niezawodności systemu.

**Aha-moment:** redundancja pomaga tylko wtedy, gdy jej gałęzie nie zawodzą z
tej samej przyczyny.

**Pułapka:** nie uśredniać niezawodności elementów i nie mnożyć ich bez
sprawdzenia logiki systemu oraz niezależności.

**Most:** diagram blokowy zaczyna od sposobu działania systemu. Gdy zaczynamy od
niepożądanego skutku i pytamy „co może do niego doprowadzić?”, otrzymujemy drzewo
błędów.

---

### Wykład 9. Analiza drzewa błędów (FTA)

**Pytanie otwierające:** „w hali osiągnięto niebezpieczne stężenie gazu”. Jakie
kombinacje zdarzeń mogły do tego doprowadzić i gdzie warto dodać barierę?

**Narracja:** FTA jest gramatyką przyczyn. Zdarzenie szczytowe rozbijamy na
prostsze zdarzenia, aż do przyczyn, którym umiemy przypisać dane lub modele.
Bramki logiczne zamieniają się następnie w rachunek prawdopodobieństwa.

**Treść:**

- zdarzenie szczytowe, zdarzenie pośrednie i zdarzenie bazowe;
- konstrukcja drzewa od skutku do przyczyn;
- znaczenie bramek OR i AND; opcjonalnie bramka (k)-z-(n) jako rozszerzenie;
- obliczanie prawdopodobieństwa od dołu do góry;
- minimalne przekroje jakościowo: najmniejsze kombinacje wystarczające do
  wystąpienia zdarzenia szczytowego;
- powtórzone zdarzenia bazowe i ryzyko podwójnego liczenia;
- zależności i awarie wspólnej przyczyny;
- prosta analiza ważności/wrażliwości: który parametr najbardziej zmienia
  prawdopodobieństwo zdarzenia szczytowego;
- FTA jako model logiczny wymagający przeglądu eksperckiego, a nie dowód, że
  lista przyczyn jest kompletna.

**Widgety rdzeniowe:**

1. **„Zbuduj drzewo pytaniami”** — student wybiera zdarzenie szczytowe, a potem
   dla każdego węzła odpowiada „wystarczy jedna przyczyna czy potrzebna jest ich
   kombinacja?”. Z odpowiedzi powstaje niewielkie drzewo AND/OR.
2. **„Logika na żywo”** — włączanie i wyłączanie zdarzeń bazowych podświetla
   aktywne ścieżki do zdarzenia szczytowego. Najpierw logika binarna, bez liczb.
3. **„Policz od liści do korzenia”** — animacja krokowa: wartości przy liściach,
   kolejne bramki, wynik u korzenia. Możliwość zatrzymania i samodzielnego
   przewidzenia następnej wartości.
4. **„Ranking wpływu”** — suwaki prawdopodobieństw zdarzeń bazowych oraz wykres
   pokazujący zmianę prawdopodobieństwa zdarzenia szczytowego po poprawie każdego
   elementu o ustalony względny poziom.
5. **„Ukryta wspólna przyczyna”** — porównanie naiwnego drzewa niezależnych
   czujników z drzewem zawierającym wspólne zasilanie. Widget pokazuje zmianę
   struktury, nie tylko liczby.

**Aha-moment:** najczęstsza przyczyna bazowa nie zawsze jest najlepszym miejscem
interwencji — liczy się jej położenie w logice całego drzewa.

**Pułapka:** bramka OR nie zawsze oznacza proste dodawanie, a to samo zdarzenie
obecne w kilku gałęziach nie może być traktowane jak kilka niezależnych zdarzeń.

**Most:** znamy już wszystkie narzędzia. Ostatni wykład zaczyna się od surowego
opisu systemu, a kończy rekomendacją zabezpieczenia.

---

### Wykład 10. Studium integracyjne — od modelu do decyzji

**Przypadek:** instalacja sprężonego gazu w hali produkcyjnej. Zdarzeniem
szczytowym jest niebezpieczna ekspozycja pracowników. Dostępne są: częstości
wycieków, parametry czujników, wyniki kontroli zaworów, czasy życia wentylatorów,
architektura alarmu i wentylacji oraz koszty kilku możliwych działań.

**Narracja:** student nie dostaje nazwy rozkładu ani gotowego drzewa. Przechodzi
przez sekwencję decyzji analityka: definiuje zdarzenie, dobiera model, sprawdza
założenia, łączy elementy i komunikuje rekomendację wraz z ograniczeniami.

**Etapy i widgety:**

1. **„Co jest zdarzeniem?”** — uporządkowanie opisu na zagrożenie, inicjator,
   bariery i skutek; zdefiniowanie czasu misji i populacji odniesienia.
2. **„Czy alarm oznacza wyciek?”** — Bayes na parametrach czujnika i częstości
   bazowej; wybór reguły reakcji na jeden lub dwa alarmy.
3. **„Ile niesprawnych zaworów?”** — model dwumianowy dla partii kontrolnej i
   interpretacja prawdopodobieństwa przekroczenia limitu.
4. **„Czy wentylator dotrwa do przeglądu?”** — wybór między wykładniczym a
   Weibullem na podstawie opisu hazardu; odczyt (R(t)).
5. **„Czy układ zabezpieczeń zadziała?”** — redukcja prostego systemu
   szeregowo-równoległego z osobnym scenariuszem utraty wspólnego zasilania.
6. **„Zbuduj FTA”** — kompletne, ale niewielkie drzewo dla niebezpiecznej
   ekspozycji; obliczenie prawdopodobieństwa zdarzenia szczytowego.
7. **„Jedna poprawa w budżecie”** — wybór między lepszym czujnikiem, częstszym
   przeglądem, niezależnym zasilaniem i dodatkowym wentylatorem. Widget porównuje
   zmianę wyniku, ale student musi uzasadnić wybór także jakościowo.
8. **„Notatka dla kierownika”** — szablon czterech zdań: wynik, naturalna
   częstość, najważniejsze założenie, rekomendowana decyzja. Nie wystarczy podać
   samego procentu.

**Końcowy komunikat:** analiza ryzyka nie kończy się na wyniku liczbowym. Kończy
się na przejrzystej decyzji, informacji o założeniach i wskazaniu danych, które
najbardziej warto zebrać.

## 5. Typy widgetów wspólne dla całej serii

Żeby aplikacje były spójne, warto powtarzać kilka wzorców interakcji:

| Wzorzec | Rola dydaktyczna | Gdzie użyć |
|---|---|---|
| Głosowanie „zanim policzysz” | ujawnia intuicję i błąd poznawczy | początek każdego wykładu |
| Naturalne częstości / ikony | usuwa abstrakcję z małych prawdopodobieństw | warunkowe, Bayes, awarie rzadkie |
| Suwaki parametrów | pokazują wrażliwość wyniku | wszystkie rozkłady, niezawodność |
| Animacja krokowa | rozdziela proces na logiczne etapy | reguła iloczynu, Bayes, FTA |
| Dwa zsynchronizowane widoki | łączy reprezentacje | tabela–drzewo, gęstość–przeżycie |
| Presety scenariuszy | pozwalają szybko porównać mechanizmy | rozkłady i systemy |
| „Sprawdź założenia” | hamuje mechaniczne używanie wzorów | Bernoulli, normalny, systemy |
| Ranking wrażliwości | prowadzi od obliczenia do decyzji | systemy, FTA, studium końcowe |
| Reset i przycisk „losuj ponownie” | pokazują zmienność, nie chaos interfejsu | symulacje |

Każdy wykład powinien mieć **3–4 widgety rdzeniowe**. Dodatkowe widgety z listy
można oznaczyć jako rozszerzenia. Lepiej dopracować trzy interakcje z jasnym
wnioskiem niż przygotować osiem kalkulatorów bez narracji.

## 6. Zasady projektowania treści

### Co powinno znaleźć się w każdym wykładzie

- 3–5 celów zapisanych językiem działania („student potrafi rozpoznać…”, nie
  „student zna…”);
- jeden przypadek główny oraz jeden krótki kontrprzykład;
- 3–4 interakcje rdzeniowe;
- najwyżej 3 nowe symbole matematyczne w jednej sekcji;
- ramka „Jak rozpoznać, że to ten model?”;
- ramka „Kiedy ten model zawodzi?”;
- jedna decyzja inżynierska wynikająca z obliczeń;
- mini-quiz 3 pytań i jedno pytanie wyjściowe bez rachunków;
- ściąga: pytanie → model → założenia → wynik → interpretacja.

### Czego unikać

- rozpoczynania rozdziału od definicji i kilku wzorów;
- widgetów będących tylko kalkulatorami wartości po wpisaniu parametrów;
- przykładów wyłącznie z monetami i kostkami — mogą otwierać intuicję, ale
  powinny szybko ustąpić sytuacjom inżynierskim;
- utożsamiania prawdopodobieństwa z pełną miarą ryzyka bez uwzględnienia skutku;
- przedstawiania niezależności jako neutralnego lub zawsze bezpiecznego
  założenia;
- podawania wielu miejsc po przecinku bez naturalnej częstości i kontekstu;
- sugerowania, że rzadkie zdarzenie jest niemożliwe;
- budowania wielkich drzew FTA na ekranie — lepsze są małe drzewa, których logikę
  student rzeczywiście prześledzi.

### Stopniowanie formalizmu

1. opis słowny;
2. naturalne częstości lub diagram;
3. oznaczenia zdarzeń;
4. wzór z kolorami odpowiadającymi elementom diagramu;
5. jeden przykład liczbowy;
6. dopiero na ściądze zapis ogólny.

Pełne wyprowadzenia kombinatoryczne, własności funkcji gamma i formalne dowody
nie są potrzebne w rdzeniu. Mogą wystąpić w zwijanych sekcjach „dla chętnych”.

## 7. Sprawdzanie rozumienia

Ocena powinna premiować wybór modelu i interpretację bardziej niż ręczne
rachunki.

### Po każdym wykładzie

- jedno pytanie koncepcyjne z typową błędną odpowiedzią;
- jedno zadanie „dobierz model i uzasadnij”;
- jedno zadanie „co zmieniłbyś w systemie?”;
- automatyczny feedback wyjaśniający, dlaczego odpowiedź jest poprawna lub nie.

### Po blokach

- po wykładzie 3: mapa pojęć (P(A)), (P(A\mid B)), całkowite i Bayes;
- po wykładzie 5: selektor modelu „stałe (n), stałe (r), liczba czy czas?”;
- po wykładzie 7: dopasowanie opisu hazardu do wykładniczego/Weibulla/gamma;
- po wykładzie 9: małe zadanie FTA z jedną ukrytą wspólną przyczyną;
- wykład 10: krótka notatka decyzyjna zamiast testu z samych wzorów.

## 8. Zakres podstawowy i rozszerzony

### Rdzeń wymagany programem

- klasyczna definicja prawdopodobieństwa;
- warunkowe, iloczyn, całkowite i Bayes;
- schemat Bernoulliego;
- dwumianowy i ujemny dwumianowy;
- normalny;
- wykładniczy, gamma i Weibull;
- system szeregowy i równoległy;
- FTA z bramkami AND/OR.

### Rozszerzenia, jeśli wystarczy czasu

- prosta macierz ryzyka z dyskusją jej ograniczeń;
- hazard, MTTF i cenzorowanie na poziomie intuicyjnym;
- system (k)-z-(n);
- minimalne przekroje FTA;
- prosta analiza wrażliwości i awarie wspólnej przyczyny;
- koszt decyzji po alarmie.

Rozszerzenia wspierają kierunkowy charakter kursu, ale nie powinny wypierać
intuicyjnego opanowania treści programowej.

## 9. Pokrycie treści programowej

| Treść programowa | Wykład |
|---|---:|
| Podstawowe pojęcia rachunku prawdopodobieństwa | 1 |
| Klasyczna definicja prawdopodobieństwa | 1 |
| Prawdopodobieństwo warunkowe i iloczyn zdarzeń | 2 |
| Wzór na prawdopodobieństwo całkowite | 2 |
| Wzór Bayesa | 3 |
| Schemat Bernoulliego | 4 |
| Rozkład dwumianowy | 4 |
| Rozkład ujemny dwumianowy | 5 |
| Rozkład normalny | 6 |
| Rozkład wykładniczy, gamma i Weibull | 7 |
| Prawdopodobieństwo awarii systemów złożonych | 8 |
| Systemy szeregowe i równoległe | 8 |
| Analiza drzewa błędów (FTA) | 9 |
| Integracja i zastosowanie całości | 10 |

## 10. Priorytety realizacji materiału

Jeżeli plan będzie później implementowany, proponowana kolejność prac to:

1. przygotować wspólny opis przypadku instalacji gazowej i spójny słownik
   pojęć;
2. opracować wykłady 1–3 jako fundament języka prawdopodobieństwa;
3. opracować wykłady 8–9, aby wcześnie ustalić docelowy sposób myślenia o
   systemie i FTA;
4. uzupełnić rozkłady w wykładach 4–7;
5. dopiero na końcu złożyć studium integracyjne z gotowych wcześniej idei;
6. po każdym wykładzie przeprowadzić próbę: czy student bez patrzenia na wzór
   potrafi powiedzieć, co oznacza wynik i jakie założenie może go zepsuć?

Za sukces serii należy uznać nie to, że student pamięta wszystkie funkcje
rozkładów, lecz że widząc nowy problem potrafi odpowiedzieć kolejno:

> Jakie jest zdarzenie? Co jest warunkiem? Co jest stałe? Czy pytam o liczbę,
> czas czy działanie systemu? Jakie założenie łączy elementy? Jak wynik wpływa na
> decyzję o bezpieczeństwie?

# Krytyka

## Co jest dobre

1. **Spójna oś narracyjna.** Pięć pytań przewodnich (sekcja 2) i powracający
   przypadek instalacji gazowej dają serii ciągłość, której zwykle brakuje
   kursom „rozkład po rozkładzie”. Mosty między wykładami są konkretne i
   logicznie poprawne (dwumianowy → ujemny dwumianowy przez zamianę tego, co
   stałe, to dydaktycznie najlepszy fragment planu).
2. **Poprawna kolejność formalizmu.** Zasada intuicja → częstości → wzór →
   decyzja oraz naturalne częstości („2 na 1000 okresów”) są zgodne z badaniami
   nad rozumieniem prawdopodobieństwa (podejście Gigerenzera). Głosowanie przed
   obliczeniem to sprawdzony sposób ujawniania błędów poznawczych.
3. **Pułapki trafione w realne błędy studentów.** Rozłączność ≠ niezależność,
   P(A|B) ≠ P(B|A), ignorowanie częstości bazowej, np = 2 ≠ „zawsze dwie
   awarie”, wspólna przyczyna kasująca redundancję — to jest dokładnie ten
   zestaw nieporozumień, który trzeba adresować wprost.
4. **Uwaga o parametryzacji ujemnego dwumianowego** (wykład 5) to rzadko
   spotykana, a bardzo potrzebna przezorność — w R `dnbinom()` liczy porażki, nie
   próby, więc bez tej notki widgety i podręcznik będą sobie przeczyć.
5. **Struktura operacyjna.** Tabela pokrycia treści programowej, rozdział
   rdzeń/rozszerzenia, priorytety realizacji i wzorce widgetów wspólne dla serii
   czynią plan wykonalnym, a nie tylko wizją.

## Co jest źle lub wymaga decyzji

1. **Plan łamie własny limit widgetów.** Sekcja 5 mówi „3–4 widgety rdzeniowe”,
   a wykłady 6, 7, 8 i 9 wymieniają po 5 widgetów rdzeniowych, bez oznaczenia,
   które są rozszerzeniem. Trzeba to rozstrzygnąć przed implementacją, bo limit
   jest sensowny, a listy go ignorują.
2. **Wykład 7 jest przeładowany.** Wykładniczy + gamma + Weibull + cztery nowe
   funkcje (f, F, R, h) + MTTF + krzywa wannowa + cenzorowanie w 90 minut łamie
   zasadę „najwyżej 3 nowe symbole na sekcję” z sekcji 6. To najbardziej
   ryzykowny wykład serii — kandydat do podziału (np. wykładniczy + pojęcia
   przeżycia/hazardu osobno od Weibulla i gammy) albo do zepchnięcia gammy do
   rozszerzeń.
3. **Wykład 10 to 8 etapów w 90 minut.** Każdy etap odpowiada osobnemu widgetowi
   z wcześniejszego wykładu; realistycznie studenci przejdą 4–5 etapów. Plan
   powinien wskazać, które etapy są rdzeniem, a które można pominąć, albo
   przewidzieć pracę w parach na podzielonych etapach.
4. **Brak rozkładu Poissona jest nieumotywowany.** Kurs mówi o „liczbie zdarzeń”
   i wprowadza wykładniczy oraz gammę, których naturalnym partnerem jest proces
   Poissona. Jeżeli sylabus go nie zawiera — trzeba to napisać wprost w sekcji 8
   z jednym zdaniem uzasadnienia; jeżeli zawiera — w planie jest dziura.
5. **Tytuł wykładu 5 myli próby z czasem.** „Jak długo do kolejnego zdarzenia”
   i pytanie o „zapas czasu” sugerują zmienną ciągłą, a ujemny dwumianowy liczy
   dyskretne próby. To dokładnie ta konfuzja, którą wykład 7 ma potem
   prostować. Lepszy tytuł: „Ile prób do kolejnego zdarzenia”.
6. **Pojęcie zmiennej losowej pojawia się znikąd.** Między wykładem 3 (zdarzenia)
   a 4 (X ~ Bin, E(X), wariancja) nie ma żadnego pomostu wprowadzającego zmienną
   losową, wartość oczekiwaną i wariancję jako pojęcia ogólne. Wykład 4 musi to
   udźwignąć jawnie, inaczej E(X) będzie dla studentów magicznym symbolem.
7. **„Analiza ryzyka” prawie bez skutków.** Kurs jest de facto kursem
   prawdopodobieństwa i niezawodności; dotkliwość skutków i macierz ryzyka są
   tylko w rozszerzeniach, mimo że efekt końcowy 7 i sekcja „Język kursu”
   obiecują rozdzielanie prawdopodobieństwa od ryzyka. Minimum: jedna stała
   ramka „a co ze skutkiem?” w wykładach 1, 9 i 10, żeby obietnica nie była
   pusta.
8. **Zepsuta notacja matematyczna.** Wzory zapisano jako `(P(A)=|A|/|\Omega|)`
   — nawiasy zamiast delimiterów LaTeX, a w sekcji 2 jest niedomknięte
   `(P \times S\)`. Przed użyciem planu jako źródła treści trzeba ujednolicić
   zapis (albo czysty tekst, albo poprawny LaTeX).

## Ryzyka realizacyjne (nieadresowane w planie)

1. **Wykonalność widgetów w Shiny.** „Mapa sytuacji ryzykownej” (przeciąganie
   kart), „Sprawdź założenia” (drag & drop) i „Zbuduj drzewo pytaniami”
   (dynamiczna budowa drzewa) wykraczają poza wzorce istniejących aplikacji w
   `statystyka/` i będą wymagały dodatkowego JS lub pakietów typu
   `sortable`. Plan nie odróżnia widgetów tanich (suwak + ggplot) od drogich —
   to powinno wpłynąć na priorytety z sekcji 10.
2. **„Głosowanie intuicyjne” wymaga decyzji technicznej.** Lokalna aplikacja
   Shiny nie agreguje głosów całej sali; albo głosowanie jest indywidualne
   („zaznacz, potem sprawdź”), albo potrzebny jest wspólny serwer/inne narzędzie.
   Plan traktuje to jako oczywistość, a to determinuje architekturę.
3. **Brak odniesienia do konwencji repo.** Plan nie wspomina o systemie
   `lecture_layout`, kontrakcie designu ani strukturze modułów ch1/ch2 — przy
   implementacji trzeba będzie zdecydować, czy `analiza-ryzyka/` kopiuje system
   ze `statystyka/R/` (analogicznie do `ekonometria/`), i to powinno być punktem
   0 w sekcji 10.

## Werdykt

Plan jest dojrzały dydaktycznie i nadaje się jako podstawa implementacji po
korektach: odchudzeniu wykładów 7 i 10, rozstrzygnięciu sprawy Poissona i
zmiennej losowej, dopisaniu warstwy skutków oraz uzupełnieniu sekcji 10 o
decyzje techniczne (system layoutu, mechanizm głosowania, koszt widgetów
drag & drop). Największa siła planu — narracja i pułapki — jest jednocześnie
najtrudniejsza do utrzymania w implementacji, więc warto ją chronić przy każdym
cięciu zakresu.
