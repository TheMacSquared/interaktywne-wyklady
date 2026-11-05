# Wykład 04 — propozycja punktów stop i przegląd dostępnych prototypów

Stan przeglądu: 31 października 2026. Baza pierwotnego przeglądu: lokalny
HEAD `7032a5c`; aktualizacja obejmuje pobrany `origin/main` z commitem
`a8f6a039281a69a1052bf3322ac035a52767ef14`.
To ocena i propozycja dydaktyczna do omówienia. W kolejnym kroku dodano
do tekstu wykładu warianty B1–B8, opisane w [BRAINSTORM.md](BRAINSTORM.md);
oryginalne panele Claude’a zachowano do porównania.

## Źródło prototypów i zakres

Po ponownym pobraniu remote znaleziono właściwy zestaw w commicie `a8f6a03`
([commit na GitHubie](https://github.com/TheMacSquared/interaktywne-wyklady/commit/a8f6a039281a69a1052bf3322ac035a52767ef14)).
Pliki mają datę pracy 30 października, commit jest z 31 października, godz. 11:20.
Rozdział 1 (1a–1d) i Ryc. 2.1 (2a) są już oznaczone jako zaakceptowane.
Poniżej zachowuję ten status; rekomendacje dla nich dotyczą dopracowania.

Lokalne `main` i `origin/main` mają osobne commity, więc aktualizacja przez
fast-forward nie była możliwa. Ocenę wykonano na wiernej kopii `origin/main`
w `/tmp/ci-prototype-review`, bez scalania gałęzi i bez nadpisania lokalnej pracy.

Podstawa: `PUNKTY-STOP.md`, `SCENY.md`, `statystyka/R/DESIGN_CONTRACT.md`,
treść rozdziałów, `stop_proto_common.R`, wszystkie `stop_proto_ch1.R`–`ch5.R`
oraz `s4p_ch2.js`. Starsze sceny z 8 października oceniono dodatkowo jako
kontekst. Ocena obejmuje kod, narrację i kontrolę obliczeń w R; nie jest
pełnym testem interakcji w przeglądarce.

## Właściwe prototypy Claude’a — rekomendacje

| ID | Status / rekomendacja | Ocena i potrzebne dopracowanie |
|---|---|---|
| 1a — Cztery miarki | Zaakceptowany; zachować | Dobre porównanie estymatorów na tych samych próbach i wspólnych osiach. Poprawić fizyczną historię uszkodzonej miarki: samo „brakuje początku taśmy” nie określa kierunku błędu, podczas gdy kod jednoznacznie dodaje 2 cm. Empiryczny stos ilustruje, ale nie dowodzi nieobciążoności. Ręczną tabelę zastąpić `lc_table()`. |
| 1b — Tarcza | Zaakceptowany; zachować krótko | Czytelna galeria dwóch niezależnych cech, nie wymaga interakcji. Tarcza jest abstrakcyjną ilustracją pojęcia, nie sceną z życia. Nie powtarzać przy niej całej interpretacji czterech miarek. |
| 1c — 4× więcej osób | Zaakceptowany; zachować | Wspólna oś i rozkłady dla 25/100/400 dobrze pokazują tempo koncentracji. Rozdzielić od późniejszej opowieści o koszcie: tu zgodność i SE, w rozdz. 5 budżet. |
| 1d — Dwie pizzerie | Zaakceptowany; zachować | Najbardziej naturalny kontekst otwierający problem niepewności. Model daje średnią 4.38, SD średniej 0.446 przy 5 opiniach i 0.071 przy 200, zgodnie z narracją. Jasno mówić o ocenach dziennych, nie o kumulowanej ocenie w aplikacji. Obraz umieszczony po omówieniu n służy domknięciu, choć forma ma postać historii. |
| 2a — Przeciągnij x̄ | Zaakceptowany; zachować | Ruchomy środek przy stałym marginesie świetnie pokazuje równoważność dwóch zdarzeń. To demonstracja pojęcia; ruch nie symuluje losowania. Są pointer events, klawiatura i odczyt ARIA. Na krańcach 163/177 końce CI wychodzą poza odcinek osi 160–180, choć mieszczą się jeszcze w SVG; poszerzyć zakres osi do co najmniej 159.08–180.92 z zapasem. |
| 2b — Te same grupki, trzy poziomy | Zachować jako porównanie, najlepiej przenieść do rozdz. 5 | Metodycznie mocny punkt: te same x̄ i s, zmienia się wyłącznie poziom. Pokrycie jest liczone ze wszystkich prób, obraz pokazuje ostatnie 40. Dopisać podstawę odczytów. Nie stawiać trzech podobnych obrazów pokrycia jeden po drugim. |
| 2c — Suwak ufności | Zachować jako główną piaskownicę ALBO wybrać 2b | Utrzymuje te same 50 prób podczas zmiany poziomu, więc rzeczywiście izoluje przyczynę. Razem z siatką i 2b powtarza jednak cenę ufności. 50 przedziałów ilustruje zmianę, nie gwarantuje nominalnego odsetka trafień. |
| 2d — CI to nie zakres ludzi | Wysoki priorytet; zachować po poprawkach | Wypełnia ważną lukę i odpowiada mojemu punktowi 3. Zastąpić widoczne „log10(n)” etykietą „Liczba osób” i odczytem n. Kod przy każdej zmianie n losuje inną próbę z innym ziarnem: CI może skoczyć, a szerokość lokalnie wzrosnąć. Dla czystej puenty użyć kontrolowanego x̄ i s lub modelowego SE ze znanym σ; alternatywnie wyjaśnić zmienność rzeczywistych prób. |
| 3a — Rozsuń linie | Wysoki priorytet; zastąpić nim powtórzenia A–C/B3 | Piaskownica jest mocniejsza od kolejnej statycznej pułapki. Współczynniki Welcha są policzone poprawnie. Osie obu wykresów obecnie automatycznie zmieniają zakres z suwakiem; ustalić zakres dla całej eksploracji, osobny dla grup i różnicy. Nie nazywać strefy nierozstrzygającej „bez różnicy” w opisie dostępności. |
| 3b — Trójkąt błędów | Rozwinięcie opcjonalne | Poprawna geometria dla niezależnych estymatorów. Dwa dodatkowe suwaki SE i osobny obraz rozbijają główną historię. Związać SE z aktualnymi danymi 3a albo zostawić statyczne wyjaśnienie w rozwinięciu. Jawnie podać niezależność: bez niej dochodzi kowariancja. |
| 3c — Istotne, ale nieważne | Zachować ideę; zmienić kontekst | Stały efekt i zmienne n dobrze rozdzielają istotność od skali efektu. Pas ±3 IQ jest umowny, wymaga uzasadnienia jako założenie, a nie własność statystyki. Lepszy czas procesu i wcześniej ustalony próg użyteczności. Przy małym n wynik brzmi „nie wiadomo”, nie „nieważne”; tytuł nie powinien zdradzać jednego werdyktu dla wszystkich ustawień. |
| 4a — Zero braków na 50 | Wysoki priorytet; zachować | Najsilniejszy przypadek brzegowy w zestawie: Wald [0,0] przy 0/50, Wilson [0,7.13%]. Suwak x pozwala pokazać również ujemne granice Walda. Zacząć od x = 0 (obecnie 2), a zmienne n traktować jako dalszą eksplorację; tytuł uzgodnić z n, jeśli pozostaną trzy liczebności. |
| 4b — Ile naprawdę daje 95%? | Opcjonalne rozwinięcie po 4a | Dokładne sumowanie prawdopodobieństw dwumianowych, nie losowa symulacja. Dobre dla pytania, dlaczego metoda ma znaczenie. Dolna rama osi 50% ucina bardzo słabe pokrycie Walda blisko granic; pokazać 0–100%. Wilson też oscyluje i bywa poniżej 95% — unikać obietnicy stałego nominalnego pokrycia. |
| 5a — Pracownia dokupuje ankiety | Przebudować jako planowanie | W obecnym miejscu, po wzorach i przykładzie 1000 osób, to piaskownica kosztu, nie scena przed pojęciem. Kod nie zbiera odpowiedzi: utrzymuje środek 50% i liczy modelowy margines. Nazwać go planowaną precyzją przy p = 0.5; nie sugerować rzeczywistego CI z dokupionych danych. Stały koszt 20 zł/ankietę podać jako założenie. |
| 5b — Schody precyzji | Wybrać zamiast 5a albo scalić | Dobre domknięcie budżetu i precyzji, ale bezpośrednio po 5a powtarza dokładnie tę samą puentę. Stałe 54% przy rosnącym n to warianty kontrolowane, nie kolejne obserwowane wyniki jednego badania. |
| 5c — Poziom dobrany po fakcie | Wstrzymać przyjęcie w obecnej interpretacji | Zagnieżdżone CI sprawiają, że „którykolwiek dolny koniec ponad 26” jest dokładnie tym samym zdarzeniem co dla poziomu 80%. Symulacja porównuje jednostronne błędy 2.5% i 10%, bez dodatkowego efektu przeszukania czterech poziomów. Szczegóły poniżej. |
| D1 — Anatomia przedziału | Zachować jako domknięcie | Zbiera środek, SE, margines, czynniki szerokości i próg na jednym obrazie. CI 25.94–31.06 obejmuje 26, więc końcowy odczyt jest poprawny. Warianty n × 4 i s / 2 mają ten sam środek jako kontrolowane porównanie; nie przedstawiać ich jako wyniku nowych danych. |

### Najważniejsza korekta merytoryczna: 5c

Kod liczy `honest = sum(lo[, 2] > 26)` dla dwustronnych CI 95%, a `flex`
jako dowolne `lo > 26` dla 99%, 95%, 90%, 80%. Przy tych samych danych
największą dolną granicę ma zawsze CI 80%. Zatem wybieranie z czterech
poziomów jest tu dokładnie równoważne stosowaniu 80% od początku.

Pod prawdą μ = 26, w normalnym modelu, zdarzenia mają częstość odpowiednio
2.5% i 10%, nie 5% i nie sumę czterech ryzyk. Kontrola na 100 000 badań dała
2.45% i 9.93%. Obliczenia są poprawne; problemem jest przypisywana im puenta.

Dwie sensowne wersje do wyboru:

1. Zostawić mechanikę jako „Zmiana wymaganego poziomu zmienia ryzyko błędu”.
   Zastąpić moralizujące etykiety „uczciwy/elastyczny” przez „ustalone 95% /
   najniższy dostępny poziom 80%”. Powiedzieć wprost o 2.5% i 10% dla zdania
   „przekracza”. To jednak powtórka kompromisu ufność–precyzja.
2. Jeśli puentą ma być selekcja po fakcie, określić co jest raportowane:
   wybrany poziom, deklarowane ryzyko i wynik. Zademonstrować zniekształcenie
   pokrycia adaptacyjnej reguły lub selektywne publikowanie; mierzyć dokładnie
   tę własność. Wykład 05 będzie naturalniejszym miejscem na pełny eksperyment.

### Wykonana weryfikacja

- Wczytano pełne `app.R` z pobranego snapshotu w R bez błędu aplikacji.
- Wyrenderowano do PNG 19 wykresów prototypów, w tym stany 0/50,
  skrajne SE w 3b i małe n w 3c. Obejrzano eksporty 3a i 4b.
- Sprawdzono liczbowo model pizzerii, progi 3a i symulację 5c na 100 000 badań.
- Dokładne pokrycie Wilsona na siatce prototypu osiąga około 90.46% przy
  n = 20 i p = 0.995. To dodatkowy powód, by nie opisywać go jako gwarancji 95%.
- Nie testowano interakcji przeglądarkowych ani wszystkich stanów suwaków;
  kontrola renderowania nie potwierdza dostępności, responsywności ani cyklu
  życia widgetów.

### Jak nowy zestaw zmienia moją propozycję

Moje „Ludzie się nie zwężają” oraz „Zero wad” mają już odpowiedniki 2d i 4a.
Nie budować ich ponownie: dopracować istniejące prototypy. Punkt o praktycznej
użyteczności oprzeć na 3c, a odczyt różnicy na 3a. Zachować zaakceptowany
rozdział 1 i 2a. Nie przenosić zaakceptowanych elementów do opcjonalnych tylko
po to, by zrealizować wcześniejszą propozycję.

Proponowana dalsza decyzja: przyjąć po korektach 2d, 3a, 4a i D1;
wybrać jeden z 2b/2c i jeden z 5a/5b; 3b i 4b zostawić jako rozwinięcia;
3c oprzeć na progu praktycznym w jasnym kontekście; 5c przeformułować przed
przyjęciem. Duplikaty starych paneli usuwać dopiero przy wdrażaniu wybranego
obrazu — teraz pełnią rolę porównania prototypów.

## Ocena pierwotnej wersji wykładu (przed nowym zestawem)

Wykład ma już większość potrzebnych obrazów. Największy zysk da zmiana
kolejności i ograniczenie powtórzeń. Główna opowieść powinna prowadzić przez:
jedną próbę → zmienność oszacowania → przedział → znaczenie 95% → odczytanie
różnicy → ograniczenia metody → koszt precyzji.

Obecnie rozdział 3 wielokrotnie pokazuje zależność od n i nakładanie się
przedziałów. Część paneli jest dobrym materiałem do samodzielnej pracy,
ale nie potrzebuje osobnego przystanku podczas wykładu. Własności estymatorów
warto podporządkować temu, czego wymaga rozumienie przedziałów; nie budować
z nich drugiego, równoległego wykładu.

## Starsze prototypy z 8 października — kontekst

### „Zmierz grupkę: x̄ się waha” — zachować mechanikę, zmienić umiejscowienie

Źródło: `modules/ch1_estymacja.R`, `modules/scenes.js`, tryb `mean`.

Co działa:

- Kolejne grupki dają kolejne średnie, a średnie rzeczywiście odkładają się
  na stos. To pozwala zobaczyć rozkład wyników, a nie tylko jeden pomiar.
- Jedna oś łączy pomiary osób i położenie średniej. Przejście żetonu na stos
  pomaga odróżnić obserwację od wyniku całej próby.
- Stałe n = 25 i brak nadmiarowych opcji chronią jedną puentę.
- Losowanie jest normalne, s ma mianownik n − 1; podstawowa konstrukcja
  obliczeń odpowiada opisanemu modelowi.

Co osłabia punkt stop:

- W aktualnym miejscu to symulacja po definicji estymatora. Sama etykieta
  „scena” nie tworzy wprowadzenia przed pojęciem.
- Mierzący student nie ma podanego celu, dla którego mierzy następne grupki.
  Historia wymaga jednego prostego powodu, np. przygotowania oszacowania
  średniego wzrostu studentów wydziału. Powtórzenia trzeba jawnie pokazać jako
  możliwość demonstracji, której pojedyncze rzeczywiste badanie nie daje.
- Tekst przed panelem podaje μ = 170 cm, choć krok „μ” ma tę wartość dopiero
  ujawnić. Dotyczy to także kontekstu odziedziczonego z wykładu 03: samo
  zasłonięcie linii nie przywraca niewiedzy sali.
- Odczyt SD(x̄) i długa narracja o SE mogą zamienić początek wykładu
  w powtórkę CTG. W tym miejscu wystarczy: następna próba daje inną średnią.

Rekomendacja: krótko wykorzystać przed definicją estymatora albo zostawić
jako uczciwie nazwaną symulację po definicji. Nie dokładać kolejnej symulacji
o tej samej puencie. Jeśli ma działać odsłonięcie, nie zdradzać μ wcześniej.

### „Zarzuć siatkę: co znaczy 95%” — zachować jako główną symulację wykładu

Źródło: `modules/ch2_idea.R`, `modules/scenes.js`, tryb `net`.

Co działa:

- Parametr stoi, a przedziały zmieniają położenie i szerokość. To dokładnie
  obraz potrzebny do interpretacji ufności.
- Powtórzenia kumulują się, licznik obejmuje wszystkie losowania, a odsetek
  trafień wynika z wykonanych prób. To pełnoprawna symulacja.
- Obliczenia używają s z każdej próby i kwantyla t dla n = 25.
  Współczynnik jest zaokrąglony do 2.064; do demonstracji to wystarcza.
- Odsłonięcie stałej linii i chybionych przedziałów daje dobry moment na
  pytanie: czy przed odsłonięciem potrafiliśmy wskazać chybione?

Co zmienić:

- To również punkt po definicji i wzorach, więc w obecnej kolejności jest
  symulacją. Metafora siatki nie wymaga rozbudowywania osobnej sceny.
- Narracja przed panelem ponownie podaje μ. Dodatkowo wcześniejszy prototyp
  już je odsłonił. Proponuję zrezygnować tutaj z udawania tajemnicy:
  „W demonstracji znamy prawdę; w badaniu jej nie znamy”. Ukrycie linii może
  wtedy służyć zadaniu „wskaż chybione”, bez obietnicy niespodzianki.
- Na ekranie widać tylko ostatnie 36 przedziałów, ale pokrycie dotyczy
  całego licznika. Trzeba to jasno odróżnić w krótkim odczycie, np.
  „Ostatnie 36 · trafienia we wszystkich 1000 próbach: …”.
- Akapit opisuje 80% i 99%, lecz podłączony panel ma tylko 95% i nie ma
  przełącznika poziomu. Ten wątek przenieść do rozdziału 5 albo zapewnić
  rzeczywiste porównanie tych samych prób przy różnych poziomach.
- Nie oczekiwać dokładnie 95 trafień w każdej setce. Zatrzymać się najpierw
  przy 10 próbach, następnie przejść do dużej serii i odczytać jej wynik.

Rekomendacja: zostawić jeden centralny obraz interpretacji 95%, bez kolejnej
siatki z t* kontra 1.96. Ten wariant został świadomie usunięty w `76494fb`
i wymieniony w `SCENY.md` jako odrzucony techniczny szczegół.

### Uwagi techniczne przed ewentualnym wdrożeniem

Silnik ma lokalny `requestAnimationFrame`, opóźnienia i obserwatory bez
jawnego sprzątania po usunięciu rozdziału. Reset podczas animacji wymaga
sprawdzenia: nawigacja resetu nie jest blokowana przez `busy`, a zakończenie
animacji nadal może dopisać wynik do wyczyszczonego stanu. To ryzyko widoczne
w kodzie, nie potwierdzona regresja z testu przeglądarkowego.

SVG ma stały opis dostępności, który nie relacjonuje bieżącego licznika
i pokrycia. Przy dopracowaniu potrzebny jest krótki tekstowy odczyt wyniku.
Czytelność 36 wierszy, tryb rzutnika, klawiatura, powiększenie i zachowanie
przy zmianie rozdziału wymagają osobnej kontroli wizualnej. Sceny SVG
sterowane w JS działają bez serwera R, więc port do Chart.js nie daje tu
zysku offline ani w responsywności (kryterium z 2026-11-05 w
`R/DESIGN_CONTRACT.md` dotyczy wykresów R).

## Moja pierwotna propozycja głównej ścieżki — dziewięć punktów stop

Po pobraniu nowego zestawu tę propozycję należy czytać z mapowaniem na 2d,
3a, 3c, 4a i D1 podanym wyżej. Określenie „nowy obraz” dotyczy bazy sprzed
commita `a8f6a03`. Poniższe punkty zastępują lub porządkują istniejące panele. Pytania są
kwestiami do wypowiedzenia przez prowadzącego, a nie dodatkowym tekstem UI.

| Nr / miejsce | Rodzaj i moment | Obraz i działanie | Pytanie prowadzącego | Jednozdaniowa puenta | Relacja do obecnej wersji |
|---|---|---|---|---|---|
| 1 / początek rozdz. 1 | Scena, wprowadzenie | Student zbiera pomiary do oszacowania średniego wzrostu na wydziale; po pierwszej grupce zapisuje wynik, następna daje inną liczbę. | „Którą liczbę wpiszemy do raportu i jak dokładna ona jest?” | Jedna próba daje oszacowanie, które zależy od tego, kto do niej trafił. | Adaptacja „Zmierz grupkę”, przed nazwami estymator/estymata; rozbudowany stos może zostać jako rozwinięcie. |
| 2 / rozdz. 2, po nazwaniu CI | Symulacja, wprowadzenie znaczenia 95% | Kolejne przedziały na jednej osi, stałe μ, trafienia i chybienia; najpierw 10, potem 1000 prób. | „Czy chybiony przedział został policzony gorzej?” | 95% opisuje trafność procedury w powtarzanych badaniach. | Dopracowane „Zarzuć siatkę”; jedyna główna symulacja pokrycia średniej. |
| 3 / koniec rozdz. 2 | Piaskownica, przypadek brzegowy interpretacji | Dwa wyrównane obrazy: rozkład wzrostów osób i CI średniej; zmiana n zwęża tylko CI, rozkład populacji pozostaje ten sam. | „Czy po zmierzeniu większej grupy ludzie stali się do siebie podobniejsi?” | Precyzja oszacowania średniej i zróżnicowanie ludzi to dwie różne rzeczy. | Nowy obraz zamiast polegania wyłącznie na quizie 2.2; jeden z najważniejszych braków. |
| 4 / rozdz. 3, budowa | Pojęcie | Jedna zachowana próba → x̄ → SE → margines → końce CI; te same dane we wszystkich krokach. | „Która część wyznacza środek, a która szerokość?” | Przedział składa się z oszacowania i marginesu wynikającego z danych oraz metody. | Zachować Ryc. 3.2; krzywe t/normalny jako krótkie rozwinięcie, nie obowiązkowy przystanek. |
| 5 / rozdz. 3, po CI różnicy | Galeria, przypadek brzegowy | Obok dwóch CI grup zawsze CI różnicy z zerem; trzy celowe warianty A–C: rozłączne, nakładające się i nierozstrzygające, nakładające się mimo CI różnicy bez zera. | „Który obraz odpowiada wprost na pytanie o różnicę?” | O różnicy wnioskujemy z przedziału różnicy, a samo nakładanie się CI grup nie wystarcza. | Scalić istniejące scenariusze A–C; przykład B3 o czasie reakcji zostawić do pracy własnej. |
| 6 / koniec rozdz. 3 | Galeria, domknięcie odczytu | Trzy CI różnicy wobec zera i jawnie zadanego progu użytecznej zmiany: mały efekt precyzyjny, duży efekt nieprecyzyjny, użyteczny efekt precyzyjny. | „Co z tego wynika dla decyzji, poza samym przekroczeniem zera?” | Wniosek wymaga oceny wielkości efektu i niepewności względem sensownego progu. | Rozwinąć sens B4; zamiast IQ użyć jednego kontekstu procesu, np. oszczędności czasu w minutach. |
| 7 / rozdz. 4, po wzorze proporcji | Galeria, przypadek brzegowy | Kontrola 50 sztuk: 4 wadliwe, potem 0; dla tych samych danych pokazać Wald i Wilson. | „Nie znaleźliśmy wad: czy możemy zagwarantować, że ich nie ma?” | Brak zdarzeń w próbie nie usuwa niepewności o proporcji w populacji. | Nowy wyraźny obraz ograniczeń Walda; nie dubluje B3, które pokazuje szeroki CI różnicy. |
| 8 / rozdz. 5 | Piaskownica | Jedna stała oś i CI; zmieniać po jednym n, s, poziom ufności. Zestawić n = 25, 100, 400, zachowując środek i s jako kontrolowane założenia. | „Ile obserwacji kosztuje margines mniejszy o połowę?” | Węższy przedział wymaga więcej danych, mniejszej zmienności lub niższego poziomu ufności. | Rozwinąć Ryc. 5.1 do n co najmniej 400; Ryc. 5.3 włączyć jako porównanie na tej samej próbie. |
| 9 / koniec rozdz. 5 | Przypadek, domknięcie | Jeden plan kontroli: pytanie o średnią masę, wymagana dokładność, założone s, potrzebne n, uzyskany CI i odczyt wobec progu. | „Co ustalamy przed zbieraniem danych, a co dopiero z nich obliczamy?” | Użyteczny przedział zaczyna się od pytania i planu badania, a kończy interpretacją w jednostkach problemu. | Kalkulator 5.2 staje się krokiem przypadku; osobno pozostaje materiałem. Przykłady progowe z końca rozdziału mogą dostarczyć ostatniego kroku. |

W punkcie 3 dla czystości porównania można użyć znanego σ i modelowych
przedziałów wokół tego samego środka. Jeśli porównujemy hipotetyczne próby
z ustalonym x̄ i s, trzeba nazwać to porównaniem kontrolowanym, a nie udawać,
że zwiększanie n zawsze zachowuje średnią rzeczywistych danych.

W punkcie 8 czterokrotne n daje dokładnie połowę SE przy stałym s.
Margines z kwantylem t maleje nieco inaczej, bo t* także zależy od n.
W punkcie 9 wynik wzoru z z* jest planowaniem przybliżonym, a nie gwarancją
uzyskania zadanego marginesu przy nieznanym σ.

## Konkretne briefy nowych obrazów

### „Ludzie się nie zwężają” — punkt 3

Stały rozkład wzrostu populacji u góry, oszacowanie średniej na identycznej
osi poniżej. Sterowanie wyłącznie n. Na górze można zaznaczyć centralne 95%
modelowego rozkładu wzrostów, na dole 95% CI dla μ. Te dwa oznaczenia muszą
mieć różne etykiety: „wzrost osób” i „średni wzrost”. Nie nazywać górnego
pasma przedziałem predykcji ani przedziałem tolerancji.

Prowadzący zwiększa n i zatrzymuje obraz. Rozkład ludzi pozostaje bez zmian,
CI średniej się zwęża. Puenta po obrazie, następnie quiz jako sprawdzenie.

### „Zero wad” — punkt 7

Kontroler sprawdza 50 sztuk. Dwa gotowe wyniki kontroli: 4 wady oraz 0 wad.
To galeria po wzorze, a nie scena otwierająca i nie symulacja. W każdym
wariancie dwa CI na wspólnej osi proporcji. Dla 0/50 Wald daje [0, 0],
natomiast dwustronny 95% Wilson około [0, 7.13%].

Najpierw pytanie o wniosek, potem odsłonięcie porównania metod. Nie przycinać
ujemnej granicy Walda po cichu, jeśli dodamy wariant z jednym zdarzeniem.
Nie sugerować, że Wilson ma dokładnie 95% pokrycia dla każdego n i p.

### „Czy usprawnienie jest użyteczne?” — punkt 6

Jedna oś różnicy czasu, zero i ustalony wcześniej próg oszczędności
2 minuty. Przykładowe, hipotetyczne CI: [0.1, 0.7], [-1, 7], [3, 5] min.
Warianty są widoczne razem, w tej samej skali. Wartość dodatnia zawsze
oznacza oszczędność; podpis jasno określa kierunek odejmowania.

Sala odczytuje kolejno: efekt dodatni, lecz poniżej progu; wynik
nierozstrzygający co do kierunku i użyteczności; efekt przekraczający próg.
Nie nazywać każdego CI obejmującego zero „brakiem efektu”.

## Co poza główną ścieżką

- Trzy własności estymatora: opcjonalna galeria rozkładów oszacowań wokół
  wspólnego parametru; przesunięcie, rozrzut, zachowanie przy większym n.
  To trzy odczyty, nie jedna scena z trzema puentami. Symulacja średniej
  z rozkładu normalnego sama nie pokazuje wszystkich tych własności.
- Konstrukcja CI różnicy średnich (3.3) i proporcji (4.1–4.2): zachować jako
  objaśnienia pojęcia, uruchamiać zależnie od tempa zajęć. Nie przechodzić
  obowiązkowo przez każdą pełną sekwencję po raz kolejny.
- Pozostałe A1–C2/A1–C1: bank przykładów, wybierany pod kierunek studiów.
  Sama realistyczna etykieta nie czyni każdego z nich pełnym przypadkiem.
- Forest plot: możliwe domknięcie o wspólnej skali i różnej precyzji;
  unikać konkursu „najlepsza grupa” bez uwzględnienia projektu badania
  i wielokrotnych porównań.
- Quiz i zadania rozdziału 6: materiały. Wybrane zadanie może zostać
  przypadkiem na zajęciach, jeśli prowadzący przejdzie od pytania do wniosku.

## Kolejność prac, jeśli propozycja zostanie wybrana

1. Źródło prototypów już ustalono i ocenę uzupełniono; zastosować decyzje
   z tabeli aktualizacji, zachowując zaakceptowany rozdział 1 i 2a.
2. Uporządkować narrację dwóch obecnych prototypów i odsłanianie μ.
3. Dopracować 2d i 4a jako realizacje punktów 3 i 7, bez budowy duplikatów.
4. Scalić powtórzenia w rozdziale 3 i dopracować galerię praktycznej użyteczności.
5. Połączyć eksplorację szerokości z jednym przypadkiem planowania.

Przy zmianie narracji ograniczyć werdykty „potwierdza / wyklucza hipotezę”
na rzecz odczytu CI i siły przesłanek. Przecięcie progu oznacza brak
rozstrzygnięcia, a nie dowód równości. Dwustronnego 95% CI nie utożsamiać
z jednostronnym testem na poziomie 5%; to wymaga osobnego wyjaśnienia.

Każdy wdrażany obraz powinien zachować jedną puentę, stałą skalę do porównań,
stan przy powiększaniu, polskie jednostki i tekstowy odczyt. Końce CI wynikają
z danych i metody — nie dodawać niezależnego przeciągania granic.
