# Audyt statystyki i analizy ryzyka

Data: 11 września 2026 r. Audyt kodu i treści; bez modyfikowania aplikacji.

> Aktualizacja po ustaleniach z prowadzącym: kurs rozdzielono na `statystyka/` (podstawy) i `statystyka-2/` (symulacje, Bayes, kierunkowe, szeregi czasowe). Poniższy audyt opisuje stan przed podziałem i zachowuje historyczne lokalizacje. Dawne 07, 08, 11 i szeregi są teraz blokami 01–04 Statystyki 2; dawne 09, 10 i 12 są wykładami 07–09 Statystyki. W CASchools zachowano intuicyjną narrację o obiadach i sytuacji uczniów; skorygowano nieuzasadnione rekomendacje bez rozszerzania kursu o szczegółową metodologię. Pozostałe ustalenia nie są automatycznie rozwiązane przez podział.


## Ocena ogólna

Analiza ryzyka jest bardziej spójna jako przedmiot: ma jeden przypadek przewodni, jawne definicje zdarzeń, konsekwentną progresję od częstości do niezawodności i decyzji oraz przewodnik prowadzącego. Statystyka ma znacznie szerszy zakres i wartościowe interakcje, ale miejscami różne wykłady uczą sprzecznych reguł. Najpilniejsze poprawki dotyczą pochodzenia danych, założeń metod i interpretacji wyników, a nie wyglądu aplikacji.

Warstwa techniczna ma solidną podstawę: wspólny layout w obrębie przedmiotu, wydzielone funkcje obliczeniowe i działające testy. Zielone testy nie oznaczają jednak pełnej poprawności interakcji ani treści. Potwierdziłem błędy pomijane przez obecny zestaw kontroli.

## Zakres i sprawdzenia w chwili audytu (przed poprawkami)

- 13 aplikacji statystyki, 118 rozdziałów, w tym `statystyka/szeregi-czasowe`.
- 10 aplikacji analizy ryzyka, 98 rozdziałów.
- Ekonometria wyłączona; materiały archiwalne i prototyp grafów nie były traktowane jako aktywne wykłady.
- Przegląd struktur wszystkich aktywnych aplikacji, narracji i map przedmiotów; szczegółowy przegląd wybranych obliczeń, interpretacji i interakcji.
- `Rscript statystyka/scripts/run_tests.R`: **60 PASS, 0 FAIL, 0 WARN, 0 SKIP**; zależności dostępne.
- `Rscript analiza-ryzyka/tests/testthat.R`: **464 PASS, 0 FAIL, 0 WARN, 0 SKIP**.
- Obie kontrole designu w trybie `--strict`: bez wykrytych naruszeń. Kontrola statystyki obejmuje tylko 12 numerowanych wykładów.
- Niezależne załadowanie i wyrenderowanie HTML wszystkich 23 aplikacji: brak powtórzonych identyfikatorów HTML w początkowym UI.
- Odtworzenie generatora szeregów w katalogu tymczasowym: wszystkie 7 CSV identyczne bajtowo z plikami repozytorium; dwa ostrzeżenia o długościach wektorów.
- Osobne reprodukcje problemu p-wartości permutacyjnej i hazardu Weibulla.

Środowisko: R 4.6.0. Nie przeprowadzono pełnego przeklikania w przeglądarce, pomiaru obciążenia wielu sesji ani audytu dostępności. Ocenę tempa zajęć należy sprawdzić w pilotażu. Przegląd nie jest certyfikacją każdego zdania i każdego ustawienia widgetów.

## Weryfikacja po poprawkach 1–8

- Statystyka: 36 PASS, Statystyka 2: 73 PASS, analiza ryzyka: 472 PASS; łącznie 581 sprawdzeń, bez błędów, ostrzeżeń i pominięć.
- Wszystkie trzy kontrole designu w trybie `--strict` przeszły.
- Generator odtwarza siedem CSV bez ostrzeżeń; sprawdzono daty scenariuszy i prognozy z nowych danych.
- Test serwera sprawdza komunikaty o precyzji permutacji oraz reset wyniku po zmianie danych. Osobno sprawdzono i obejrzano wykres Weibulla dla parametrów skrajnych.
- Hub: powiększono bazę czcionki do 18 px i tytuły kafelków do 1,2 rem (21,6 px); siatka dopasowuje szerokość kafelków. Wykrywanie aplikacji i render HTML przeszły. Nie przeprowadzono testu huba w przeglądarce — w środowisku nie ma przeglądarki Chromium.

## Ustalenia według pilności

Opisy problemów poniżej dokumentują stan wyjściowy; bieżący wynik prac podaje pole „Status”.

P1 oznacza poprawkę przed wykorzystaniem danego fragmentu na zajęciach; P2 — istotną poprawkę obliczeniową lub dydaktyczną; P3 — porządkowanie utrzymania.

### 1. P1 — Dane syntetyczne są przedstawiane jako rzeczywiste

**Status: Wykonane. Syntetyczne pochodzenie jest widoczne w każdym rozdziale szeregów, na wykresach i w kolumnie `data_origin` wszystkich siedmiu CSV. Poprawiono opisy scenariuszy i dodano README danych.**

**Lokalizacja:** `statystyka/szeregi-czasowe/modules/ch1_motywacja.R:77`, `modules/ch2_dekompozycja.R:47`, `modules/ch15_anomalie.R:138`; pochodzenie: `dane/generate_data.R:4`.

Wprowadzenie zapowiada „cztery realne polskie szeregi”, a wykład o anomaliach pokazuje „Dane rzeczywiste” i historię pandemii. Tymczasem temperaturę, bezrobocie, noclegi, sprzedaż, pszenicę, PM10 i WIG20 generuje lokalny skrypt z ustalonym ziarnem losowania. Odtworzyłem wszystkie pliki identycznie.

Student może uznać zaprojektowane trendy i szoki za wyniki empiryczne, a następnie wyciągać wnioski o rzeczywistej Polsce. To szczególnie niespójne z wykładami o jakości danych i projektowaniu badań.

**Poprawka:** jednoznaczne oznaczenie „dane syntetyczne inspirowane…” we wprowadzeniu, podpisach i eksportach albo zastąpienie zbiorów danymi źródłowymi z metadanymi. Historie o zdarzeniach historycznych powinny być opisami scenariusza symulacji.

### 2. P1 — Resampling jest reklamowany jako metoda „bez założeń”

**Status: Wykonane. Usunięto hasła „bez założeń” także ze ściąg i rozwiązań ćwiczeń. Rozróżniono równość średnich od wymienności; dodano przykład konieczności zachowania par.**

**Lokalizacja:** `statystyka/07-symulacje-statystyczne/modules/ch4_permutacje.R:18`, `modules/ch8_kiedy.R:53` i `:194`.

Treść obiecuje przedziały i testy „bez założeń”, a tabela stwierdza, że test permutacyjny sprawdza tę samą H₀ co test t. W tym samym `ch8_kiedy.R:108` poprawnie pojawia się wymienność, więc kurs przeczy sam sobie.

Przetasowanie etykiet w implementowanym teście różnicy średnich wymaga odpowiedniego uzasadnienia wymienności lub randomizacji. Sama równość średnich nie zapewnia wymienności przy różnych rozkładach. Bootstrap również wymaga dopasowania schematu losowania do struktury danych. Zob. [Chung i Romano, Exact and asymptotically robust permutation tests](https://arxiv.org/abs/1304.5939).

**Poprawka:** konsekwentnie pisać „bez założenia normalności”, wymienić wymagania konkretnej procedury i dodać przykład, w którym swobodne mieszanie obserwacji jest niedozwolone, np. pomiary sparowane.

### 3. P1 — CASchools przechodzi od związku do nieuzasadnionej rekomendacji przyczynowej

**Status: Wykonane w uzgodnionym zakresie kursu podstawowego. Zachowano przykład dopłat do obiadów, poprawiono rekomendacje i doprecyzowano STR jako wskaźnik dystryktu, bez rozszerzania metodologii.**

**Lokalizacja:** `statystyka/10-case-studies/modules/ch1_caschools.R:303`–`:339`.

W podsumowaniu padają stwierdzenia, że więcej pieniędzy samo w sobie nie pomaga, za miliardy dolarów uzyska się kilka punktów oraz że skuteczniejsze mogą być inne interwencje. Pokazane modele nie szacują kosztów tych polityk ani efektów alternatywnych interwencji. Dopiero poniżej jest zastrzeżenie, że dane obserwacyjne nie pozwalają orzekać o przyczynowości.

To modelowy przykład konfliktu między poprawną notką o ograniczeniach a właściwą konkluzją raportu. Student uczy się, że zastrzeżenie wystarcza, aby następnie formułować mocniejsze twierdzenia.

**Poprawka:** oddzielić oszacowany związek warunkowy od hipotez o interwencjach. Rekomendować dalszą ocenę projektu badania i kosztów; usunąć niepoliczone kwoty i porównania skuteczności. Doprecyzować również, że STR jest relacją uczniów do nauczycieli na poziomie dystryktu, a nie bezpośrednim pomiarem liczebności pojedynczej klasy.

### 4. P2 — Generator szeregów powiela początek trendu na końcu danych

**Status: Wykonane. Trendy bezrobocia i pszenicy są interpolowane między konkretnymi datami; dodano kontrole długości. Poprawiono również przypisanie szoku noclegów do miesięcy. Regeneracja wszystkich CSV przebiega bez ostrzeżeń; testy sprawdzają zgodność plików, daty szoków i dopasowanie modeli.**

**Lokalizacja:** `statystyka/szeregi-czasowe/dane/generate_data.R:57`, `:65`, `:157`, `:164`.

`trend_bezr` i `base_price` mają po 240 elementów, a osie czasu i składniki sezonowe po 288. `head(x, 288)` nie wydłuża wektora. R wykonuje recykling pierwszych 48 wartości i zgłasza ostrzeżenie. Błąd jest obecny w dostarczonych CSV, co potwierdza identyczność plików po regeneracji.

Końcowe cztery lata zawierają powrót do początkowego fragmentu trendu zamiast zaplanowanej kontynuacji. Może to zmieniać diagnozę zmian strukturalnych, dopasowanie modeli i ocenę prognoz.

**Poprawka:** skonstruować trend dla dokładnie wszystkich dat; przed działaniami dodać kontrolę zgodności długości. Regeneracja wymaga ponownego sprawdzenia opisów i ćwiczeń zależnych od tych danych.

### 5. P2 — Losowy test permutacyjny zwraca p = 0

**Status: Wykonane. Testy permutacyjne i MC używają korekty `(b+1)/(B+1)`. UI pokazuje rozdzielczość i przedział niepewności symulacyjnej. Dodano testy zerowej liczby przekroczeń, remisów, kierunków alternatywy oraz reakcji widgetu.**

**Lokalizacja:** `statystyka/07-symulacje-statystyczne/modules/helpers.R:257` i `:284`; analogiczny surowy estymator MC w `:427`.

Implementacja liczy wyłącznie odsetek przekroczeń w B losowaniach. Dla parametrów dostępnych w widgetach — n=50 na grupę, efekt=20, rozkład normalny, B=1000 — uzyskałem zero.

Brak ekstremalnej permutacji w skończonej symulacji nie oznacza zerowej p-wartości. Dla używanego losowania permutacji należy uwzględnić obserwowaną konfigurację, typowo przez `(b+1)/(B+1)`, i pokazać ograniczoną rozdzielczość symulacji. Zob. [Phipson i Smyth, Permutation P-values Should Never Be Zero](https://gksmyth.github.io/pubs/PermPValuesPreprint.pdf). Estymator MC warto dodatkowo opatrzyć informacją o niepewności symulacyjnej.

**Kontrola po poprawce z katalogu repozytorium:**

```r
source("statystyka-2/R/palette.R")
source("statystyka-2/01-symulacje-statystyczne/modules/helpers.R")
d <- generate_two_groups_data(50, effect = 20, dist = "normal", seed = 1)
run_permutation_test_twosample(d, B = 1000, seed = 1)$p_value
# 0.000999001 = 1/1001
```

### 6. P2 — Hazard Weibulla znika w dozwolonym zakresie suwaków

**Status: Wykonane. Hazard jest liczony wzorem analitycznym, także dla t=0. Niezawodność i hazard mają osobne skale. Testy sprawdzają ogon, granice i zgodność z rozkładem wykładniczym; sprawdzono również render wykresu dla skrajnych suwaków.**

**Lokalizacja:** `analiza-ryzyka/R/risk_math.R`, funkcja `risk_weibull`; widget: `analiza-ryzyka/07-czas-zycia/modules/block.R:113`, `:226`.

Hazard jest liczony jako `density / reliability`, a przy numerycznym zerze niezawodności zastępowany przez NA. Dla β=4 i η=300, dostępnych w UI, 343 z 500 punktów krzywej mają NA; utrata zaczyna się około 1574 h. Tymczasem hazard nadal istnieje i rośnie: przy 5000 h wynosi około 61,7284 h⁻¹.

**Poprawka:** liczyć bezpośrednio `β/η * (t/η)^(β-1)` z obsługą t=0. Parametryzację potwierdza [dokumentacja Weibulla w R](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Weibull.html). Sprawdzić też czytelność wspólnej osi R(t) i skalowanego hazardu. Warto zauważyć, że fragment Weibulla w statystycznym wykładzie kierunkowym już używa wzoru analitycznego.

```r
source("analiza-ryzyka/R/risk_math.R")
t <- seq(1, 5000, length.out = 500)
sum(is.na(risk_weibull(t, shape = 4, scale = 300)$hazard))
# 0
```

### 7. P2 — Mapa metod miesza różne testy Wilcoxona i ich założenia

**Status: Wykonane. Rozdzielono Wilcoxona jednej próby, dla par i Manna–Whitneya; podano hipotezy i założenia. Ujednolicono tabelę, dynamiczny opis metody oraz skróty w normalności i ściądze.**

**Lokalizacja:** `statystyka/05-zalozenia-testow/modules/ch4_mapa.R:33`, `:77`; podobne skróty w `ch1_normalnosc.R:142`.

Po naruszeniu założenia braku silnej skośności mapa proponuje Wilcoxona jednej próby. Ten test sam wymaga symetrii w standardowej interpretacji. Dalej „Wilcoxon / Mann-Whitney” jest jednym wierszem z założeniem symetrii, co zaciera różnicę między testem rangowanych znaków a testem sumy rang dla dwóch prób.

**Poprawka:** osobne wiersze dla jednej próby, danych sparowanych i prób niezależnych; przy każdej metodzie wskazać testowany parametr/hipotezę. Nie przedstawiać zamiany testu średniej na test rangowy jako zachowania identycznego pytania. Zob. [dokumentacja `wilcox.test`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/wilcox.test.html).

### 8. P2 — Drzewo decyzyjne zawiera mylącą hipotezę korelacyjną

**Status: Wykonane. Obie wersje diagramu używają ρ i korelacji liniowej; objaśnienie odróżnia parametr populacji od r w próbie i przypomina o związku nieliniowym.**

**Lokalizacja:** `statystyka/04-wnioskowanie-statystyczne/modules/drzewo_data.R`, definicje `box_cc` oraz `b_cc`.

Graf używa „H0: r = 0 (brak związku)” i „Ha: r ≠ 0 (istotny związek)”. Hipoteza dotyczy parametru populacji ρ, a zerowa korelacja Pearsona nie oznacza braku dowolnego związku. „Istotność” jest własnością wyniku procedury, nie definicją alternatywy.

**Poprawka:** `H₀: ρ=0`, opis braku korelacji liniowej; `H₁: ρ≠0`. Ujednolicić wersje DOT i visNetwork. Ostrzeżenie o planie badania nad grafem jest wartościowe, ale nie naprawia błędnego opisu węzła.

### 9. P2 — Testy nie pokrywają większości zachowań serwerowych

**Status: częściowo wykonane.** Dodano test serwera widgetu permutacji: wynik i komunikat o precyzji, korelacja oraz reset po zmianie danych. Nadal pozostaje szersze pokrycie pozostałych interakcji i testy przeglądarkowe.

**Lokalizacja:** `statystyka/tests/testthat/test-app-smoke.R:1`, `analiza-ryzyka/tests/testthat/test-app-smoke.R:1`, `analiza-ryzyka/tests/testthat/test-mission-analysis.R`.

Smoke testy wczytują aplikacje i sprawdzają strukturę. Statystyka nie ma w obecnym zestawie testów wywołań `testServer`; analiza ryzyka ma je dla fragmentów 05, 09 i 10. Początkowy HTML nie uruchamia wszystkich renderów, nie zmienia kontrolek i nie sprawdza zachowania JavaScript.

**Poprawka:** dodać celowane testy odtworzonych błędów oraz reprezentatywnych sekwencji interakcji: zmiana danych po obliczeniu, wartości skrajne suwaków, kliknięcie sprawdzenia bez odpowiedzi, ponowne obliczenie. Dla najważniejszych demonstracji uzupełnić to krótkim scenariuszem przeglądarkowym. Sama liczba asercji nie jest miarą pokrycia.

### 10. P3 — Dokumentacja i zakres kontroli rozjechały się z aplikacjami

**Status: częściowo wykonane.** Zaktualizowano README obu części, opis projektu, numerację i nawigację. Kontrola designu wykrywa aplikacje automatycznie i obejmuje również szeregi czasowe. Pozostają wspólny katalog metadanych dla dokumentacji i testów oraz przegląd nieaktywnych modułów.

**Lokalizacja:** `statystyka/README.md`, `statystyka/scripts/check_design_contract.R:21`, główny `AGENTS.md`.

README opisuje 12 aplikacji, pomija szeregi czasowe i podaje nieaktualne liczby rozdziałów: wnioskowanie ma 13, regresja 9, projekt badawczy 9. Kontrola zależności i smoke test obejmują szeregi, ale kontrola designu już nie. Główny opis projektu pomija analizę ryzyka.

**Poprawka:** jeden katalog metadanych wykładów jako źródło list dla kontroli i dokumentacji, podobny do katalogu analizy ryzyka. Uzgodnić, które moduły są aktywnymi materiałami, a które pozostałością starszych wersji.

## Spójność przebiegu przedmiotów

### Statystyka

Rdzeń 01 → 02 → 03 → 04 jest logiczny: opis, model losowości, estymacja, testowanie. Wykłady 05–08 sensownie rozszerzają ten warsztat o diagnostykę, regresję, symulacje i Bayesa. Mocne strony to powracające ćwiczenia, wizualne interpretacje przedziałów i osobny rozdział o sile efektu.

Problemem jest kolejność organizacyjna: jakość danych i projekt badawczy pojawiają się jako 09 i 12, chociaż określają sens wcześniejszych analiz. Nie trzeba przenosić całych aplikacji. Warto przed 01 albo przed 03 wykorzystać krótką ścieżkę z 09 i 12: pytanie → jednostka obserwacji → pomiar → dobór próby → ograniczenia. Pełne aplikacje mogą pozostać późniejszą syntezą.

| Blok | Ocena i sugerowana rola |
|---|---|
| 01–03 | Dobry fundament. Przy CTG dopisać jawnie niezależność i identyczny rozkład w wersji podstawowej oraz mówić o standaryzowanej średniej/normalnym przybliżeniu. |
| 04–05 | Połączyć decyzję o teście z jego założeniami już w przykładzie; poprawić mapy i skróty interpretacyjne. |
| 06 | Dobrze rozwinięta synteza: mapa, kontekst i interakcje. Aktualna aplikacja jest bogatsza od README. |
| 07 | Wartościowy warsztat eksperymentalny, ale wymaga pilnego ujednolicenia założeń i korekty p-wartości. |
| 08 | Czytelna idea porównań, lecz 12 rozdziałów to raczej blok kilku zajęć lub materiał do wyboru. Rozdzielić podstawy Bayesa od przeglądu wszystkich modeli. |
| 09 | Część wprowadzająca potrzebna wcześniej, katalog przypadków może służyć później do samodzielnego audytu danych. |
| 10 | Jeden przypadek CASchools pozwala przejść całą analizę; najpierw poprawić wnioski. Kolejny przypadek warto oprzeć na innej strukturze badania, zamiast ponownie rozszerzać ten sam przykład. |
| 11 | Traktować jako rozgałęzienia kierunkowe, nie sześć obowiązkowych rozdziałów dla wszystkich. |
| 12 | Dobra klamra projektu, ale cel badania, pomiar i konspekt powinny wracać od początku kursu. |
| Szeregi czasowe | Osobny blok zaawansowany: 16 rozdziałów. Jawnie określić wymagania wstępne i miejsce w programie; najpierw poprawić dane. |

Ocena obciążenia jest rekomendacją redakcyjną, nie wynikiem pomiaru czasu prowadzenia. Brakuje równie wyraźnego jak w analizie ryzyka podziału na materiał obowiązkowy, rozszerzenia i czas spotkań.

### Analiza ryzyka

Sekwencja 01–03 buduje język i warunkowanie, 04–05 uczy rozpoznawać plan doświadczenia, 06 przechodzi do przekroczeń, 07–09 do czasu życia, systemów i FTA, a 10 integruje decyzję. To dobry ciąg zależności między pojęciami.

Szczególnie wartościowe są: rozróżnienie częstości i prawdopodobieństwa, jawne mianowniki, ograniczenia niezależności, różnica między misją a niedostępnością, oddzielenie pożaru w 09 od ochrony termicznej w 10, wspólne przyczyny, budżet i scenariusze. Nie znalazłem podstaw, aby postulować przebudowę tej osi kursu.

Przewodnik planuje 15 spotkań po 90 minut, a bloki 06–10 mają podział na dwa spotkania. Największe ryzyko przeciążenia widzę w 08–09: po 13 i 12 rozdziałów, z nowymi reprezentacjami systemów i zależności. W pilotażu warto mierzyć przede wszystkim czas samodzielnego rozumowania i obrony odpowiedzi, a nie czas przeklikania ekranów. Przewodnik już wskazuje elementy możliwe do skrócenia — warto powiązać te wskazówki z konkretnymi rozdziałami w aplikacjach.

Kurs świadomie skupia się na probabilistycznej części analizy ryzyka. To dopuszczalny zakres, opisany w planie. Dla mocniejszej realizacji tytułu przedmiotu warto jednak dodać w finale porównanie dwóch wariantów o podobnym prawdopodobieństwie, lecz różnych konsekwencjach. Obecne P(TOP), budżet i limit nie zastępują analizy skutków. To propozycja rozszerzenia, nie zarzut błędnego rachunku.

## Utrzymanie i dalsza weryfikacja

- Zachować oddzielne systemy przedmiotowe; własny snapshot analizy ryzyka jest świadomą decyzją. Poprawki wspólnych komponentów powinny mieć prosty rejestr przeniesień między kopiami.
- W Bayesie `stan_glm` działa synchronicznie, a `.bayes_fit_cache` przechowuje wyniki bez limitu (`modules/helpers.R:429`, `:496`, `:561`). Przy pracy wielu studentów to ryzyko blokowania procesu i wzrostu pamięci. Nie zmierzono przeciążenia: przed wdrożeniem klasowym ustalić limit cache i przetestować równoległe sesje.
- Dla dopasowań MCMC dodać ocenę diagnostyk przed interpretacją. Sprawdzić również komunikat o konieczności ponownego dopasowania po zmianie priora.
- Kontrola designu nie sprawdza całości kontraktu: pozytywny wynik nie potwierdza np. oszczędnych pogrubień, dostępności wykresów czy użyteczności klawiatury.

## Lista zadań po aktualizacji

Odhaczone są wyłącznie wykonane zmiany. Punkty częściowo wykonane rozbito na osobne zadania.

- [x] Rozdzielić zakres podstawowy i rozszerzony: „Statystyka” (9 aplikacji) oraz „Statystyka 2” (4 aplikacje).
- [x] Przenieść symulacje, Bayesa, materiały kierunkowe i szeregi czasowe do Statystyki 2, z własnymi komponentami i testami.
- [x] Uzgodnić numerację, nawigację i prezentację obu przedmiotów w hubie.
- [x] Opisać cele, wymagania wstępne i rolę materiałów kierunkowych w README obu części.
- [x] Poprawić wnioski CASchools, zachowując intuicyjny przykład dopłat do obiadów (ustalenie 3).
- [x] Zaktualizować dokumentację zakresu kursów i opis projektu (część ustalenia 10).
- [x] Objąć wszystkie aktywne aplikacje obu części automatyczną kontrolą designu, w tym szeregi czasowe (część ustalenia 10).
- [x] Zastąpić stare zakładki ćwiczeń w szeregach czasowych wyborem kierunku zgodnym z layoutem.
- [x] Sprawdzić podział testami: Statystyka 36 PASS, Statystyka 2 27 PASS; obie kontrole bez błędów, ostrzeżeń i pominięć. Potwierdzić wykrycie 9 i 4 aplikacji w hubie.
- [x] Poprawić oznaczenia syntetycznych danych w aplikacji szeregów czasowych i eksportach (ustalenie 1).
- [x] Poprawić generator szeregów i sprawdzić opisy po regeneracji danych (ustalenie 4).
- [x] Usunąć hasła „bez założeń” i ujednolicić wymagania resamplingu (ustalenie 2).
- [x] Poprawić p-wartości permutacyjne i dodać testy reprodukujące błąd (ustalenie 5).
- [x] Poprawić hazard Weibulla i dodać testy parametrów skrajnych (ustalenie 6).
- [x] Ujednolicić mapę testów Wilcoxona i hipotezy w diagramie korelacji (ustalenia 7–8).
- [x] Dodać test serwera permutacji oraz testy regresyjne obliczeń i danych.
- [x] Powiększyć czcionki i dopasować szerokości kafelków huba.
- [ ] Rozszerzyć testy zachowań pozostałych serwerów i dodać scenariusze przeglądarkowe (pozostała część ustalenia 9).
- [ ] Wprowadzić wspólny katalog metadanych dla dokumentacji i testów oraz uporządkować nieaktywne moduły (pozostała część ustalenia 10).
- [ ] Przeprowadzić pilotaż czasu zajęć i najważniejszych interakcji obu przedmiotów; doprecyzować zakres na poszczególne spotkania.
