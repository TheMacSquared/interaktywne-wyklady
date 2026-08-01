# Analiza ryzyka — plan B: koncepcja interaktywnych wykładów

Przedmiot dla kierunku **inżynieria bezpieczeństwa**. Filozofia jak w `statystyka/`: intuicja przed formalizmem, każdy wzór poprzedzony symulacją albo obrazkiem, student najpierw *widzi* zjawisko, potem dostaje jego nazwę. Aplikacje R Shiny na systemie `lecture_page()`/`lecture_server()` (własna kopia `analiza-ryzyka/R/` ze `statystyka/R/`, zgodnie ze wzorcem równoległego przedmiotu).

## Narracja przewodnia: Zakład „Sokół"

Cały semestr kręci się wokół **jednego fikcyjnego zakładu produkcyjnego** (nazwa robocza: Zakład Przetwórstwa „Sokół" — hala produkcyjna, magazyn wysokiego składowania, lakiernia, park maszynowy, instalacja ppoż.). Student wciela się w rolę nowo zatrudnionego **inspektora ds. bezpieczeństwa**, który przez semestr uczy się czytać ryzyko tego zakładu coraz głębszymi narzędziami:

| Wykład | Warstwa zakładu | Pytanie inspektora |
|---|---|---|
| 01 | rejestr incydentów | „Jak często to się naprawdę zdarza?" |
| 02 | czujniki i alarmy ppoż. | „Alarm się włączył — pali się czy nie?" |
| 03 | wypadki załogi, kontrole | „Ile wypadków w kwartale? Kiedy pierwsza usterka?" |
| 04 | narażenia i obciążenia | „Czy hałas/obciążenie przekroczy normę?" |
| 05 | park maszynowy | „Po ilu godzinach padnie pompa?" |
| 06 | instalacja tryskaczowa | „Czy redundancja nas uratuje?" |
| 07 | pożar magazynu (FTA) | „Z czego naprawdę składa się katastrofa?" |

Mechanika narracyjna (wzorowana na „wiązce tropów" z `statystyka/12-projekt-badawczy`):

- Jedno źródło prawdy o zakładzie w `analiza-ryzyka/R/` lub wspólnym `helpers.R`: nazwy obiektów, parametry (λ pomp, p czujników, liczność załogi), spójne między wykładami — te same liczby wracają w kolejnych wykładach i w finałowym FTA.
- Każdy wykład otwiera hero z pytaniem inspektora (lead jako **pytanie**, nie stwierdzenie).
- Narastająca **mapa ryzyka zakładu** (odpowiednik tablicy tropów): panel pokazujący, które warstwy zakładu już umiemy analizować; w wykładzie 01 prawie pusta, w 07 kompletna.
- Wykład 07 (FTA) to klamra: drzewo błędów „pożar magazynu" jawnie ponownie używa liczb z wykładów 02 (zawodność czujnika), 05 (awaria pompy), 06 (niezadziałanie tryskaczy) — student widzi, że cały semestr budował elementy jednej analizy.
- Dopuszczalne pojedyncze wtręty spoza zakładu tylko tam, gdzie klasyka jest niezastąpiona dydaktycznie — ale domyślnie wszystko dzieje się w „Sokole".

## Struktura każdego wykładu

Kanoniczny szkielet ze statystyki: **rozdziały merytoryczne → ściąga → quiz (pula JSON, losowane 10 pytań) → ćwiczenia**. Ponieważ kierunek jest jeden, ćwiczenia nie mają dropdownu kierunków — zamiast tego 2–3 **scenariusze obiektowe** (np. magazyn / lakiernia / plac budowy) w jednym zestawie zadań z danymi CSV i kluczem odpowiedzi.

Konwencje techniczne: bootstrap `app.R` wg `CLAUDE.md`, komponenty `lc_*` z `lecture_layout.R`, każdy wykres przez `zoom_plot_ui/zoom_plot_server`, `figure_panel()` z kontrolkami w `column(4)` i wykresem w `column(8)`, `lc_formula_box()` na wzory (oszczędnie), `lc_stat_box()` na metryki na żywo, feedback przez `lc_feedback()` / `inline_callout()`. Interfejs po polsku, kod po angielsku, zgodność z `DESIGN_CONTRACT.md`.

## Wykład 01 — `01-jezyk-ryzyka` (podstawy prawdopodobieństwa, definicja klasyczna)

**Hook:** „Dyrektor mówi: u nas jest bezpiecznie. Rejestr incydentów mówi co innego. Kto ma rację — i co to w ogóle znaczy «bezpiecznie»?"

**Narracja:** od potocznego „ryzyka" do prawdopodobieństwa jako *ustabilizowanej częstości*. Ryzyko = szansa × skutek — ale ten wykład zajmuje się tylko „szansą".

Rozdziały:

1. **Czym jest ryzyko?** — matryca ryzyka 5×5 (prawdopodobieństwo × skutek), student klika incydenty z rejestru „Sokoła" i umieszcza je na matrycy. Puenta: oś „prawdopodobieństwo" wymaga liczb, nie przymiotników.
2. **Zdarzenia i przestrzeń zdarzeń** — interaktywny diagram: dzień pracy zakładu jako przestrzeń wyników; zdarzenia „wypadek na hali", „awaria maszyny" jako podzbiory. Suma, iloczyn, dopełnienie pokazane klikaniem obszarów (diagram Venna), z tłumaczeniem na język BHP: „co najmniej jedno", „oba naraz", „ani jedno".
3. **Skąd się biorą liczby: częstość → prawdopodobieństwo** — symulator z przyciskami „Symuluj 1 dzień / 10 dni / 100 dni / 1000 dni" (wzorzec `ch1_most.R` ze statystyki): częstość incydentów stabilizuje się wokół p. Widget: wykres zbieżności częstości + `lc_stat_box` z bieżącą częstością.
4. **Definicja klasyczna** — gdy wyniki są jednakowo prawdopodobne: losowanie pracownika do kontroli trzeźwości, losowy wybór palety do inspekcji. Widget: licznik zdarzeń sprzyjających/wszystkich na siatce obiektów (klikane palety w magazynie). Callout: kiedy definicja klasyczna *nie* działa (awarie nie są „jednakowo prawdopodobne").
5. **Reguły gry: własności prawdopodobieństwa** — bez aksjomatyki formalnej; suma dla zdarzeń rozłącznych, dopełnienie („P(żadnego wypadku) = 1 − P(co najmniej jednego)") — trik dopełnienia jako *najważniejsze narzędzie semestru*, wraca w wykładach 03 i 06. Widget: suwak p + pasek 1−p.
6. **Ściąga** | 7. **Quiz** | 8. **Ćwiczenia** (rejestr incydentów CSV: oszacuj częstości, policz P przez dopełnienie).

## Wykład 02 — `02-alarm-i-prawda` (warunkowe, iloczyn, całkowite, Bayes)

**Hook:** „Czujnik dymu w magazynie się włączył. Ochroniarz wzrusza ramionami: «znowu fałszywka». Czy ma rację?"

**Narracja:** prawdopodobieństwo warunkowe jako *zawężenie świata* do tego, co już wiemy. Bayes bez mistyki — na naturalnych częstościach (wzorzec ze `statystyka/08-metody-bayesowskie`).

Rozdziały:

1. **Zawężenie świata** — siatka 1000 dni pracy zakładu; student klika warunek („tylko dni z alarmem") i siatka wygasza resztę. P(pożar|alarm) jako ułamek *w obrębie podświetlonych*. Widget: population grid z filtrowaniem.
2. **Iloczyn zdarzeń i drzewo** — interaktywne drzewo prawdopodobieństw: gałęzie „pożar/brak pożaru" × „alarm/brak alarmu"; suwaki na P(pożar), czułość, fałszywe alarmy; liczby na liściach aktualizują się na żywo. P(A∩B) = P(A)·P(B|A) jako „mnożenie wzdłuż gałęzi".
3. **Niezależność — kiedy warunek nic nie zmienia** — dwa czujniki: kiedy P(oba zawiodą) = p²? Callout o awariach ze wspólnej przyczyny (zapowiedź wykładu 06).
4. **Prawdopodobieństwo całkowite** — części zamienne od trzech dostawców o różnej wadliwości: „jaka jest szansa, że losowa część z magazynu jest wadliwa?". Widget: suwaki udziałów dostawców + wadliwości, wykres słupkowy składników sumy.
5. **Wzór Bayesa: alarm się włączył — i co teraz?** — odwrócenie drzewa. Widget kluczowy wykładu: siatka 1000 dni w naturalnych częstościach (ile dni z pożarem i alarmem, ile z fałszywym alarmem), P(pożar|alarm) liczone „na palcach" zanim pojawi się wzór. Suwak częstości pożarów pokazuje **base rate fallacy**: świetny czujnik + rzadkie pożary = większość alarmów fałszywa. Puenta: ochroniarz ma *statystycznie* rację — i właśnie dlatego jest niebezpieczny.
6. **Ściąga** | 7. **Quiz** | 8. **Ćwiczenia** (test narkotykowy pracowników / alarm gazowy: policz PPV z naturalnych częstości).

## Wykład 03 — `03-liczenie-zdarzen` (schemat Bernoulliego, dwumianowy, ujemny dwumianowy)

**Hook:** „40 pracowników hali, każdy dzień to loteria. Ile wypadków w kwartale mamy *prawo* się spodziewać — i kiedy liczba wypadków to sygnał, a kiedy szum?"

**Narracja:** te same rzuty monetą, dwa pytania: „ile sukcesów w n próbach?" (dwumianowy) i „ile prób do k-tego sukcesu?" (ujemny dwumianowy). W bezpieczeństwie „sukces" bywa wypadkiem — czarny humor konwencji zostaje omówiony wprost.

Rozdziały:

1. **Schemat Bernoulliego** — animowana sekwencja prób (dni × pracownicy jako kafelki zapalające się przy zdarzeniu). Trzy warunki schematu (dwa wyniki, stałe p, niezależność) sprawdzane na przykładach z zakładu: które sytuacje *nie* są Bernoullim (zmęczenie rośnie → p rośnie).
2. **Rozkład dwumianowy** — suwaki n i p + żywy wykres pmf; nakładka symulacji (słupki empiryczne vs teoretyczne). `lc_stat_box`: E(X)=np, SD. Scenariusz: liczba wypadków w kwartale przy n=40, p=0.002/dzień.
3. **„Co najmniej jeden"** — najważniejsze zastosowanie w ryzyku: P(≥1 awaria) = 1 − (1−p)ⁿ. Widget: suwak n przy małym p — jak szybko „prawie niemożliwe" staje się „prawie pewne" (np. p=0.001 dziennie → rok pracy). Powrót triku dopełnienia z wykładu 01.
4. **Ujemny dwumianowy: czekanie na k-tą usterkę** — symulacja inspekcji palet do znalezienia k-tej wadliwej; histogram czasu oczekiwania. Przypadek k=1 (geometryczny) jako most do wykładu 05 (czas do pierwszej awarii). Suwaki k i p.
5. **Sygnał czy szum?** — w tym kwartale 5 wypadków zamiast typowych 2: powód do paniki? Widget: gdzie 5 leży na pmf; intuicja przedziału „zwykłej zmienności". (Bez testów istotności — tylko intuicja.)
6. **Ściąga** | 7. **Quiz** | 8. **Ćwiczenia** (dane inspekcji: dwumianowe P(≥1), oczekiwana liczba kontroli do wykrycia usterki).

## Wykład 04 — `04-rozklad-normalny` (rozkłady ciągłe na przykładzie normalnego)

**Hook:** „Norma hałasu na stanowisku: 85 dB. Średnia z pomiarów: 82 dB. Czyli jest OK? Średnia — tak. Ale co z wtorkami przy pełnym obciążeniu linii?"

**Narracja:** od zliczania zdarzeń (dyskretne) do pomiarów (ciągłe). Gęstość jako wygładzony histogram; pole = prawdopodobieństwo. Finał: obciążenie kontra wytrzymałość — pierwsza prawdziwie *inżynierska* miara ryzyka.

Rozdziały:

1. **Od histogramu do gęstości** — pomiary hałasu z hali; suwak szerokości kubełka, przy dużej liczbie pomiarów histogram → gładka krzywa. Puenta: P(dokładnie 82.000 dB) = 0, sens mają tylko przedziały.
2. **Rozkład normalny: μ i σ** — suwaki μ/σ z żywą krzywą; σ jako „typowy rozrzut". Dlaczego normalny jest wszędzie — mini-demo sumowania wielu małych wpływów (odwołanie do CTG bez formalizmu).
3. **Reguła 68–95–99.7** — podświetlane pasma ±1σ/±2σ/±3σ na rozkładzie hałasu; interpretacja: „ile dni w roku przekroczymy próg". `lc_stat_box` z liczbą dni.
4. **Przekraczanie progów i standaryzacja** — suwak progu (NDS hałasu, dopuszczalne obciążenie regału) + zacieniowane pole P(X > próg); z-score jako „ile σ od średniej" — uniwersalna linijka, bez tablic.
5. **Obciążenie kontra wytrzymałość** — dwa rozkłady na jednej osi: obciążenie regału (co stawiamy) i wytrzymałość (co wytrzyma); pole zakładki ≈ ryzyko awarii. Suwaki średnich i rozrzutów: margines bezpieczeństwa to nie odległość średnich, tylko *ogony*. Najważniejszy widget wykładu.
6. **Ściąga** | 7. **Quiz** | 8. **Ćwiczenia** (pomiary hałasu/obciążeń CSV: frakcja przekroczeń normy, porównanie dwóch stanowisk).

## Wykład 05 — `05-czas-zycia` (wykładniczy, gamma, Weibull)

**Hook:** „Pompa obiegowa w instalacji ppoż. ma 4 lata. Konserwator mówi: «stara, zaraz padnie». Producent: «bezawaryjna, λ stałe». Kto ma rację — i skąd to wiedzieć?"

**Narracja:** czas do awarii jako zmienna losowa; trzy rozkłady jako trzy *biografie* urządzenia: wieczna młodość (wykładniczy), awaria etapami (gamma), starzenie się lub choroby wieku dziecięcego (Weibull). Krzywa wannowa jako mapa całego życia.

Rozdziały:

1. **Czas do awarii** — symulacja: oś czasu, urządzenia „żyją" i gasną; histogram czasów życia. Funkcja niezawodności R(t) = P(T > t) wprowadzona jako „odsetek wciąż żywych" — czytana wprost z symulacji.
2. **Rozkład wykładniczy i brak pamięci** — demo memorylessness: dwie grupy pomp (nowe vs 4-letnie), rozkład *dalszego* życia identyczny. Suwak λ; MTTF = 1/λ. Puenta: przy stałym λ wymiana prewencyjna „na wszelki wypadek" nic nie daje — producent może mieć rację.
3. **Intensywność awarii (hazard)** — h(t) jako „ryzyko awarii *teraz*, pod warunkiem że dożyło": przełącznik widoku gęstość / R(t) / h(t) dla tego samego rozkładu. Hazard jako język, w którym spór konserwatora z producentem da się rozstrzygnąć.
4. **Gamma: awaria w k etapach** — pompa pada po k uszkodzeniach cząstkowych; suma czasów wykładniczych. Suwak k: od wykładniczego (k=1) do coraz bardziej „przewidywalnego". Most: to ujemny dwumianowy w wersji ciągłej (wykład 03).
5. **Weibull: kształt mówi wszystko** — suwak parametru kształtu: k<1 wady wieku dziecięcego, k=1 czysty przypadek, k>1 zużycie. Widget-gwóźdź: **krzywa wannowa** złożona z trzech reżimów; student dopasowuje kształt do danych serwisowych pompy i *rozstrzyga spór z hooka*.
6. **Ściąga** | 7. **Quiz** | 8. **Ćwiczenia** (czasy pracy maszyn CSV: dopasuj λ, porównaj R(t) dwóch pomp, zinterpretuj kształt Weibulla).

## Wykład 06 — `06-niezawodnosc-systemow` (systemy szeregowe i równoległe)

**Hook:** „Dlaczego samolot ma dwa silniki, hamulce dwa obwody — a nasza instalacja tryskaczowa jedną pompę?"

**Narracja:** z klocków (elementy o znanej niezawodności z wykładu 05) budujemy systemy. Szeregowy: łańcuch tak mocny jak najsłabsze ogniwo — i słabszy z każdym ogniwem. Równoległy: redundancja kupuje niezawodność, ale ma granice.

Rozdziały:

1. **System szeregowy** — łańcuch bloków; każdy blok z suwakiem niezawodności; R systemu = iloczyn, spada z każdym dodanym blokiem. Widget: dodawanie ogniw + wykres R(n). Puenta: 10 elementów po 0.95 → system 0.60.
2. **System równoległy** — bloki obok siebie: system pada, gdy padną *wszystkie*; P(awarii) = iloczyn q (trik dopełnienia z wykładu 01 po raz trzeci). Widget: ile pomp trzeba, by osiągnąć zadaną niezawodność instalacji tryskaczowej.
3. **Konstruktor systemów** — widget-gwóźdź: interaktywny budowniczy (klocki łączone szeregowo/równolegle, konfiguracje mieszane), niezawodność systemu liczona na żywo + `lc_stat_box`. Zadania typu „przebuduj system, by R ≥ 0.99 najmniejszym kosztem".
4. **Granice redundancji** — awarie ze wspólnej przyczyny (zalana rozdzielnia zasila obie pompy): suwak korelacji/wspólnej przyczyny pokazuje, jak „papierowa" niezawodność redundancji topnieje. Powrót niezależności z wykładu 02. Callout: Fukushima — wszystkie generatory zalała ta sama fala.
5. **Ściąga** | 6. **Quiz** | 7. **Ćwiczenia** (schematy instalacji „Sokoła": policz R, zaproponuj redundancję w budżecie).

## Wykład 07 — `07-drzewa-bledow` (FTA) — finał

**Hook:** „Pożar magazynu wysokiego składowania to nie jedno zdarzenie. To koniunkcja i alternatywa dziesiątek małych. Rozbierzmy go na części — mamy już wszystkie narzędzia."

**Narracja:** klamra semestru. Drzewo błędów „pożar magazynu Sokoła" budowane od zdarzenia szczytowego w dół; w liściach jawnie wracają liczby z poprzednich wykładów: zawodność czujnika (02), P(≥1 zaprószenie) (03), awaria pompy w czasie misji (05), niezadziałanie redundantnych tryskaczy (06).

Rozdziały:

1. **Od katastrofy do przyczyn** — dekonstrukcja krok po kroku (wzorzec step-buttons z `typy-danych`): zdarzenie szczytowe → gałęzie → zdarzenia bazowe; na każdym kroku wyjaśnienie *dlaczego* tak dzielimy.
2. **Bramki AND i OR** — mini-kalkulatory bramek: OR ≈ suma (dla małych p), AND = iloczyn; połączenie z sumą/iloczynem zdarzeń (01) i systemami (06): OR = szeregowy w języku awarii, AND = równoległy.
3. **Liczymy drzewo** — widget-gwóźdź semestru: interaktywne drzewo „pożar magazynu" z suwakami przy zdarzeniach bazowych; prawdopodobieństwo propaguje się na żywo do góry. Panel „skąd znamy te liczby" linkuje każdy liść do wykładu, z którego pochodzi.
4. **Minimalne przekroje: gdzie system jest naprawdę słaby** — podświetlanie najkrótszych ścieżek do katastrofy; ranking wkładu zdarzeń bazowych (ważność). Puenta: intuicja kieruje uwagę na spektakularne przyczyny, przekroje — na tanie pojedyncze punkty awarii.
5. **Co dalej: FTA w praktyce inspektora** — ograniczenia (kompletność drzewa, dane, wspólne przyczyny), mapa ryzyka zakładu w pełni odsłonięta; domknięcie narracji: raport inspektora dla dyrektora z wykładu 01.
6. **Ściąga (całego semestru)** | 7. **Quiz** | 8. **Ćwiczenia** (uzupełnij drzewo dla lakierni: dobierz bramki, policz top event, wskaż przekroje).

## Repertuar widgetów (mapowanie na wzorce ze statystyki)

| Widget | Wzorzec źródłowy | Wykłady |
|---|---|---|
| Symulacja z akumulacją (1×/10×/100×/1000×) | `02-rozklady.../ch1_most.R` | 01, 03, 05 |
| Siatka populacji / naturalne częstości | `08-metody-bayesowskie` | 02 |
| Suwaki parametrów + żywy pmf/gęstość | `02-rozklady-prawdopodobienstwa` | 03, 04, 05 |
| Step-buttons (krok po kroku) | `01-typy-danych` | 02 (drzewo), 07 (dekonstrukcja) |
| Quiz z puli JSON (10 z ~40–60) | `ch8_quiz.R` + JSON | wszystkie |
| Ćwiczenia z danymi CSV + klucz | `ch9_cwiczenia.R` + `cwiczenia/` | wszystkie |
| Narastająca tablica narracyjna | `12-projekt-badawczy` (tablica tropów) | mapa ryzyka zakładu, 01–07 |
| Nowe, specyficzne dla ryzyka | — | matryca ryzyka 5×5 (01), drzewo prawdopodobieństw (02), interferencja obciążenie–wytrzymałość (04), krzywa wannowa (05), konstruktor systemów (06), interaktywne FTA (07) |

## Struktura katalogów (docelowa)

```
analiza-ryzyka/
├── R/                  # kopia statystyka/R/ (palette, theme_upwr, shared, lecture_layout, DESIGN_CONTRACT)
├── docs/plan-B.md      # ten dokument
├── 01-jezyk-ryzyka/
│   ├── app.R
│   ├── modules/ (ch1..chN + helpers.R + quiz_*.json)
│   └── cwiczenia/ (dane CSV + generate_data.R + klucz_odpowiedzi.md)
├── 02-alarm-i-prawda/
├── 03-liczenie-zdarzen/
├── 04-rozklad-normalny/
├── 05-czas-zycia/
├── 06-niezawodnosc-systemow/
├── 07-drzewa-bledow/
└── README.md
```

## Kolejność implementacji (propozycja)

01 → 02 → 03 (fundamenty), potem 05 → 06 → 07 (ścieżka niezawodnościowa, największa wartość dla kierunku); 04 może powstać równolegle — zależy tylko od 01.

## Pokrycie treści programowej

| Punkt sylabusa | Wykład |
|---|---|
| Podstawowe pojęcia rachunku prawdopodobieństwa, definicja klasyczna | 01 |
| Prawdopodobieństwo warunkowe, iloczyn zdarzeń, całkowite, wzór Bayesa | 02 |
| Schemat Bernoulliego | 03 (rozdz. 1–2) |
| Rozkład dwumianowy i ujemny dwumianowy | 03 (rozdz. 2–4) |
| Rozkłady ciągłe na przykładzie normalnego | 04 |
| Rozkłady czasu życia: wykładniczy, gamma, Weibull | 05 |
| Systemy szeregowe i równoległe | 06 |
| Analiza drzewa błędów (FTA) | 07 |

# Krytyka

## Ocena ogólna

Plan B jest bardzo dobrą **koncepcją dydaktyczną**, ale nie jest jeszcze bezpiecznym planem implementacji. Największą zaletą jest spójna narracja „Sokoła” i konsekwentne prowadzenie od intuicji do modelu. Największym problemem są natomiast pojedyncze, lecz istotne uproszczenia matematyczne oraz brak oszacowania zakresu prac, kryteriów odbioru i wersji MVP. W obecnej postaci plan warto zachować jako podstawę projektu, ale przed kodowaniem poprawić niżej wskazane kwestie.

## Co jest zrobione dobrze

1. **Bardzo mocna oś narracyjna.** Jeden zakład i jedna rola studenta dają ciągłość, której zwykle brakuje w kursach rachunku prawdopodobieństwa. Powrót parametrów z wcześniejszych wykładów w finałowym FTA może dać prawdziwy efekt „aha”.
2. **Dobra progresja pojęć.** Kolejność: zdarzenia → warunkowanie → liczba zdarzeń → zmienne ciągłe → czas życia → system → FTA jest logiczna. Szczególnie dobre są mosty między dopełnieniem, niezależnością, niezawodnością i bramkami drzewa błędów.
3. **Trafna filozofia dydaktyczna.** Symulacja lub obraz przed wzorem, naturalne częstości przy Bayesie oraz interpretowanie parametrów w kontekście decyzji są odpowiednie dla studentów inżynierii bezpieczeństwa.
4. **Dobre hooki i pytania przewodnie.** Są konkretne, osadzone w sytuacji zawodowej i przeważnie prowadzą do rozstrzygnięcia, które można pokazać w widgecie.
5. **Jawne pokrycie sylabusa.** Tabela na końcu pozwala szybko sprawdzić kompletność programu i chroni projekt przed rozrostem w przypadkowe tematy.
6. **Uwzględnienie ograniczeń modeli.** Plan nie kończy się na naiwnym mnożeniu niezawodności: pojawiają się awarie wspólnej przyczyny, granice redundancji, kompletność FTA i zależności. To bardzo ważne dla tego kierunku.
7. **Zgodność z lokalnym systemem wykładów.** `lecture_page()`, komponenty `lc_*`, quiz JSON, ćwiczenia i `zoom_plot_*` faktycznie pasują do architektury `statystyka/`. Pomysł osobnej, świadomie zaadaptowanej kopii `R/` jest także zgodny z regułą równoległych przedmiotów opisaną w `CLAUDE.md`.
8. **Stały rytm aplikacji.** Powtarzalna struktura „treść → ściąga → quiz → ćwiczenia” ułatwi studentowi nawigację, a twórcom testowanie i utrzymanie kolejnych aplikacji.

## Co wymaga poprawy merytorycznej

### 1. Ryzyko nie powinno być zdefiniowane po prostu jako „szansa × skutek”

To może być roboczy model punktowy macierzy ryzyka, ale nie uniwersalna definicja ryzyka ani automatycznie poprawne działanie na skalach jakościowych 1–5. Plan powinien od początku rozróżniać: zagrożenie, ekspozycję, zdarzenie, prawdopodobieństwo, skutek i ryzyko. Jeżeli sylabus obejmuje głównie prawdopodobieństwo i niezawodność, należy wprost powiedzieć, że kurs modeluje przede wszystkim **składową prawdopodobieństwa**, a nie pełną ilościową analizę skutków i ryzyka.

### 2. „Pole zakładki” obciążenia i wytrzymałości nie jest prawdopodobieństwem awarii

To najważniejszy błąd w planie. Nakładanie się dwóch gęstości może być ilustracją podobieństwa rozkładów, ale nie daje wprost `P(obciążenie > wytrzymałość)`. Dla niezależnych zmiennych normalnych trzeba analizować różnicę `D = wytrzymałość − obciążenie` i obliczać `P(D < 0)`; jej wariancja jest sumą wariancji obu zmiennych. Przy zależności dochodzi składnik kowariancji. Widget może nadal pokazywać oba rozkłady, lecz właściwy obszar ryzyka powinien wynikać z rozkładu różnicy albo z symulowanych par „obciążenie–wytrzymałość”.

### 3. W przykładzie dwumianowym nie zgadzają się jednostki ekspozycji

„40 pracowników”, `p = 0,002/dzień` i „liczba wypadków w kwartale” nie tworzą modelu `n = 40`. Liczbą prób byłyby raczej pracownikodni, np. liczba pracowników × liczba dni pracy, o ile w ogóle można obronić stałe `p` i niezależność. Trzeba wszędzie jawnie podawać jednostkę prawdopodobieństwa, okres odniesienia i ekspozycję. Ten przykład jest też dobrym miejscem na pokazanie, dlaczego realne wypadki często nie spełniają założeń Bernoulliego.

### 4. Modele czasu życia są miejscami opisane zbyt szeroko

- Gamma jako „awaria po `k` etapach” jest dokładnie takim czasem oczekiwania tylko w szczególnym modelu procesu Poissona; dla całkowitego `k` jest to rozkład Erlanga. Warto nazwać założenia zamiast przedstawiać tę interpretację jako własność każdej gammy.
- Pojedynczy rozkład Weibulla ze stałym parametrem kształtu nie tworzy pełnej krzywej wannowej. Trzy fazy wymagają modelu złożonego, kilku mechanizmów awarii albo potraktowania krzywej wannowej jako osobnej syntezy jakościowej.
- Zdanie, że wymiana prewencyjna „nic nie daje”, jest prawdziwe dla idealnego modelu wykładniczego w zakresie ryzyka awarii, ale nie ogólnie: w praktyce mogą istnieć przeglądy, ukryte uszkodzenia, koszty przestoju, naprawy i inne mechanizmy starzenia.
- „Dopasowanie Weibulla do danych” jest znacznie trudniejsze niż manipulacja suwakiem. Trzeba ustalić, czy student ma tylko rozpoznać kształt hazardu, czy rzeczywiście estymować parametry, oraz jak potraktować obserwacje cenzurowane.

### 5. Niezawodność systemu wymaga czasu misji i jawnych założeń

Wzory dla układów szeregowych i równoległych powinny dotyczyć `R_i(t)` dla tego samego czasu misji `t` i zakładać niezależność elementów. Bez tego student może odnieść wrażenie, że „niezawodność 0,95” jest stałą cechą urządzenia bez horyzontu czasowego. Trzeba także odróżnić niezawodność od gotowości systemu naprawialnego. „Suwak korelacji” dla awarii wspólnej przyczyny jest ryzykownym uproszczeniem — lepiej użyć jawnego, prostego modelu zdarzenia wspólnego lub modelu typu beta-factor i dokładnie opisać jego założenia.

### 6. Logika bramek FTA jest podana zbyt skrótowo

`AND = iloczyn` obowiązuje przy niezależności zdarzeń wejściowych. Dla bramki OR dokładny wynik przy niezależności to `1 − ∏(1 − p_i)`, a suma jest tylko przybliżeniem dla małych prawdopodobieństw i pomijalnych części wspólnych. W rzeczywistym drzewie ten sam basic event może wystąpić w kilku miejscach, więc proste propagowanie wartości od liści do góry może dać błędny wynik.

Trzeba też precyzyjniej zdefiniować zdarzenie szczytowe i okres analizy, np. „nieopanowany pożar magazynu w ciągu roku”. Awaria czujnika, pompy lub tryskaczy nie powoduje zapłonu sama z siebie — wpływa na wykrycie albo eskalację. Drzewo powinno rozdzielać inicjację pożaru od nieskutecznego wykrycia i tłumienia. „Ranking wkładu” również wymaga nazwania wybranej miary ważności, ponieważ minimalny przekrój, prawdopodobieństwo przekroju i ważność elementu nie są tym samym.

### 7. Próg hałasu wymaga poprawnie zdefiniowanej mierzonej wielkości

Pojedynczy pomiar dB, średnia pomiarów i miara ekspozycji pracownika nie są automatycznie zamienne. Przykład może zostać, ale należy dokładnie określić, co jest zmienną losową i do jakiego okresu oraz rodzaju pomiaru odnosi się próg. Inaczej atrakcyjny widget nauczy zbyt prostego porównania „odczyt > norma”.

## Co wymaga poprawy projektowej i technicznej

1. **Zakres jest zbyt duży jak na siedem wykładów i jeden etap realizacji.** Wykład 02 łączy warunkowe, iloczyn, niezależność, prawdopodobieństwo całkowite i Bayesa; wykład 05 trzy rodziny rozkładów, hazard, dopasowanie i krzywą wannową. Bez podania czasu spotkania nie wiadomo, czy materiał jest realny. Konstruktor systemów oraz edytowalne FTA są ponadto osobnymi, stosunkowo dużymi projektami interfejsowymi.
2. **Brakuje MVP.** Dla każdego wykładu należy wskazać jeden widget rdzeniowy, treść obowiązkową oraz elementy opcjonalne. Najrozsądniejszy pierwszy pionowy wycinek to kompletny wykład 01 z quizem, ćwiczeniami, testami i wspólną konfiguracją „Sokoła”; dopiero po jego odbiorze warto powielać architekturę.
3. **Brakuje mierzalnych efektów uczenia się.** Hooki są dobre, ale każdy wykład powinien mieć 3–5 efektów w formie „student potrafi…”, a pytania quizowe i ćwiczenia powinny być do nich przypisane. Obecne „pokrycie treści” mówi, co zostanie pokazane, nie co student ma wykonać.
4. **Brakuje kryteriów odbioru.** Plan powinien określić co najmniej: uruchomienie każdej aplikacji bez błędów, testy funkcji obliczeniowych względem znanych wyników, smoke test aplikacji, kontrolę kontraktu designu, responsywność, obsługę klawiatury i alternatywne objaśnienia dla informacji przekazywanej kolorem.
5. **Nie ma planu testowania symulacji.** Potrzebne są deterministyczne ziarna do przykładów i testów, walidacja zakresów suwaków, testy przypadków brzegowych (`p = 0`, `p = 1`, bardzo małe prawdopodobieństwa) oraz oddzielenie funkcji matematycznych od reaktywnego UI.
6. **„Jedno źródło prawdy” jest za mało konkretne.** Wspólna konfiguracja powinna przechowywać nie tylko liczby, ale też jednostki, horyzont czasu, definicję zdarzenia, źródło lub oznaczenie „dane fikcyjne” oraz zakres dopuszczalnych wartości. Inaczej ta sama liczba może zostać użyta w różnych wykładach w niezgodnym znaczeniu.
7. **Kopiowanie `statystyka/R/` grozi rozjazdem wersji.** Jest zgodne z obecną organizacją repozytorium, ale plan powinien podać, czy jest to jednorazowy snapshot, które pliki będą adaptowane, z jakiej wersji pochodzą i jak będą przenoszone późniejsze poprawki wspólnego layoutu.
8. **Nie wszystkie wskazane wzorce istnieją w opisanej postaci.** Obecny `statystyka/08-metody-bayesowskie` dotyczy porównania wnioskowania bayesowskiego i częstościowego i nie zawiera wskazanego widgetu naturalnych częstości dla alarmów. Ten komponent trzeba zaprojektować od nowa albo wskazać rzeczywisty wzorzec. Z kolei quiz JSON i losowanie 10 pytań mają dobry, istniejący odpowiednik w `02-rozklady-prawdopodobienstwa`.
9. **Produkcja treści jest niedoszacowana.** Siedem pul po 40–60 pytań oznacza 280–420 pytań, do tego dane CSV, generatory, klucze i kilka scenariuszy na wykład. Potrzebne są etapy redakcji, walidacji poprawnych odpowiedzi i pilotażu, a nie tylko implementacja Shiny.
10. **Kolejność implementacji nie definiuje zależności ani kamieni milowych.** Informacja, że 04 „może powstać równolegle”, nie wystarcza. Trzeba oddzielić kolejność dydaktyczną od produkcyjnej oraz rozpisać: fundament techniczny, model danych „Sokoła”, prototypy ryzykownych widgetów, implementację treści, testy i przegląd merytoryczny.

## Zalecane poprawki przed rozpoczęciem kodowania

1. Poprawić model obciążenie–wytrzymałość, jednostki w przykładzie dwumianowym, założenia modeli czasu życia oraz wzory i semantykę FTA.
2. Dopisać definicje podstawowych terminów, czasu misji i wspólnych założeń modelowych obowiązujących w całym kursie.
3. Ustalić długość każdego spotkania i odchudzić wykłady 02 oraz 05 albo podzielić je na więcej spotkań.
4. Zdefiniować MVP każdego wykładu; konstruktor systemów i edytowalne drzewo FTA potraktować początkowo jako wersję rozszerzoną.
5. Rozpisać efekty uczenia się, kryteria odbioru, testy obliczeń i plan pilotażu ze studentami.
6. Zaprojektować jawny schemat danych „Sokoła” z jednostkami, horyzontami czasu i oznaczeniem danych fikcyjnych.
7. Po tych korektach wykonać jeden kompletny pionowy wycinek i dopiero na podstawie jego kosztu oszacować pozostałe sześć aplikacji.

Podsumowując: plan B ma **bardzo dobry pomysł na kurs i ponadprzeciętną spójność narracyjną**, ale agent AI dopracował przede wszystkim wizję, a nie wykonalny harmonogram produkcji. Po korekcie wymienionych błędów matematycznych i dodaniu MVP, kryteriów odbioru oraz planu testów będzie to mocna podstawa do realizacji.
