# TODO — Interaktywne wykłady

Jedyny plik zadań w repozytorium. Zrobione punkty usuwamy — historia jest
w gicie.

Jak przypisać zadanie:

- **Globalne** — zmienia wspólne komponenty, konwencje lub testy używane przez
  więcej niż jeden kurs (np. wspólny rdzeń `shared/`).
- **Cały kurs** — zmienia wspólne `R/` jednego kursu albo dotyczy wszystkich
  jego wykładów.
- **Wykład** — zmienia tylko katalog jednego wykładu.

Jeśli praca nad wykładem ujawnia potrzebę zmiany wspólnej, dopisujemy osobne
zadanie globalne zamiast rozwiązywać ją lokalnie. Lokalny eksperyment nie
staje się wzorcem bez osobnej decyzji, dokumentacji i testów.

Oznaczenie **Decyzja:** to pytanie do prowadzącego z wariantami do wyboru.

---

## Teraz

Kolejność najbliższych prac (szczegóły w „Migracja widgetów do v2”):

1. [ ] Przegląd wykład po wykładzie: widgety (układy kolumn, odczyty, podpisy,
   tabele) i bloki tekstu (notki, pułapki, podsumowania, statusy).
   Widgety: statystyka 00–01, 03–10 i analiza ryzyka gotowe (audyt 3 października
   2026: 510 paneli, 0 przelewów, 0 błędów). Zostały statystyka 2 (niski
   priorytet) i bloki tekstu — lista w „Migracja widgetów do v2”. Świadome
   wyjątki w statystyce: quizy z długimi etykietami na radio (02, 04 Ryc. 3.4),
   wykres z kliknięciem na `zoom_plot_ui` (06 ćwiczenie z prostą), drzewo
   decyzyjne 04 z własnym przyciskiem pełnego ekranu, schemat
   `type-error.jpg` w 04 rozdz. 3 poza panelem.

---

## Globalne

### Przejście na strony statyczne (bez serwera R)

Decyzje (14 listopada 2026): statystyka i analiza ryzyka przechodzą na strony
statyczne, statystyka 2 zostaje na Shiny; treść nadal w R i `htmltools`,
uruchamianych tylko przy budowaniu; jeden plik HTML na wykład; wzory składane
przy budowaniu; całość na gałęzi `statyczny-js`, przełączenie na końcu. Zasada:
R liczy przy budowaniu, JS liczy tylko to, co zmienia student.

Stan: `Rscript scripts/check_static_ready.R` (licznik na wykład). 14 listopada
2026: gotowe 19 z 21 wykładów (analiza ryzyka 01–10, statystyka 00–06, 09 i 10, zbudowane i sprawdzone
przez `scripts/build_static.R` i `scripts/check_static_page.R`; panele zgodne
z wersją Shiny co do liczby wykresów, kontrolek i tekstu); zostało 66 wykresów
R (wykłady 07 i 08). Zostały tylko
wykłady statystyki 07 i 08; cała analiza ryzyka działa bez serwera. Wykresy analizy
ryzyka rysuje silnik `lcSF` przez `risk_sf_panel()` (dane ze stałym ziarnem
z R w `config`, funkcje kursu w `analiza-ryzyka/R/risk_figures.js`); wzorzec: 06. Quiz analizy ryzyka
(wszystkie 10 wykładów) ocenia już przeglądarka; głosowanie „Najpierw zdecyduj”
w 03–05 także (`risk_vote_panel(feedback = )`). Treść wybieraną kontrolką
(dawniej `uiOutput()` + `renderUI()` po `input$…`) zastępuje `lc_variants()`,
komentarz do wyboru `lc_choice_feedback()`, treść odsłanianą przyciskiem
`lc_reveal()`, opis kroku z serwera `lc_step_widget(texts = )`, a quiz
losujący pytania z puli `lc_tile_quiz()` (`shared/lc_quiz.js`). Warianty mogą
też zależeć od kliknięcia (`lc_variants(pick = TRUE)`), przycisku tam i z powrotem
(`lc_toggle_button()` + `lc_toggle_content()`) albo kroku (`lc_step_views()`);
`conditionalPanel()` działa na stronie statycznej dla warunków z `input.…`.

- [x] Faza 0: licznik zależności od serwera (`scripts/check_static_ready.R`).
- [x] Faza 1: tryb statyczny `lecture_page()`, `scripts/build_static.R`,
  `scripts/check_static_page.R`; pilot: analiza ryzyka 02.
- [x] Faza 2 (15 listopada 2026): wykłady bez wykresów R — analiza ryzyka 01–05
  i statystyka 00–04 — nie mają logiki serwera. Strona statyczna wstawia treść
  rozdziałów po wczytaniu skryptów (jak renderUI w Shiny).
- [ ] Faza 2, reszta: kontrolki bez Shiny (segment, lista, kroki `lc_step_nav`,
  `lc_chips`, `lc_drop_match`, `conditionalPanel`), głosowanie w pozostałych
  wykładach (06–10: dopisać `feedback` do `risk_vote_panel()`) i ćwiczenia
  w przeglądarce — przy wykładach z wykresami R (fazy 4–7).
- [ ] Opisy kroków scen w pozostałych wykładach statystyki: `scene_texts()`
  (serwer) → `lc_step_widget(texts = )`, tak jak w 00–04
  (`scripts/migracja/move_scene_texts.py`). Zostało 11 wywołań: 05 (4), 06 (4),
  07 (2), 08 (1). Ćwiczenia w wariantach kierunków przenosi
  `scripts/migracja/port_exercises.py` (`lc_static_fill()`).
- [ ] Wzory liczone w przeglądarce są składane w HTML (`.lc-hmath`, `.lc-hfrac`),
  bo strona statyczna nie ładuje MathJax; dziś tylko statystyka 01, Ryc. 3.5.
  Krój i wielkość różnią się od wzorów składanych przy budowaniu: do oceny na ekranie.
- [ ] Test losowy w `statystyka/tests/js/lc_charts.test.js` („symulacje (03):
  losowanie lcDist…”, asercja „SD średnich”) zawodzi mniej więcej raz na sześć
  uruchomień (zaobserwowane 14 listopada 2026, kod 03 bez zmian): ustawić ziarno
  albo poszerzyć tolerancję.
- [ ] Faza 3: `shared/lc_stats.js` — testy liczone w przeglądarce, wzorcem są
  wyniki R (`statystyka/tests/js/lc_stats.test.js`, wzorce generuje
  `statystyka/tests/js/fixtures/make_lc_stats.R`). Jest: Shapiro–Wilk, Levene,
  Bartlett, test t Studenta i Welcha, χ² i Fisher dla tabeli 2×2, rozkład F,
  kwantyle, wykres Q-Q, regresja liniowa (`lm()`: współczynniki, błędy, p,
  95% CI, R², AIC), ANOVA jednoczynnikowa, rozkład studentyzowanego rozstępu
  (`ptukey`, `qtukey`) i porównania Gamesa-Howella. Testy wykładu 05 (t jednej
  próby i par, dwumianowy, proporcji, korelacji, χ² i Fishera dla tabel r × c)
  są w `05-…/modules/figures.js` z własnymi wzorcami R (`tests/js/stat05.test.js`).
  Brakuje (dla 07–08): diagnostyka regresji, regresja wielomianowa, testy rangowe.
- [x] Faza 4 (15 listopada 2026): analiza ryzyka 06–10 (26 wykresów R). Gotowe: 06 (6 wykresów), 07 (8 wykresów; ryciny główne mają przedrostek `c7m_`, bo prototypy zajęły nazwy `c7_gamma` i `c7_weibull` — `lcSF.register` zgłasza teraz powtórzoną nazwę w konsoli), 08 (5 wykresów; schemat
  blokowy to trzy rysunki SVG z R przełączane listą — bez lupy, którą miał wykres),
  09 (2 wykresy; drzewo to rysunek SVG z R z liczbami wpisywanymi przez JS, panele
  z samymi odczytami to ryciny bez wykresu: `lc_sf_figure(chartless = TRUE)` /
  atrybut `data-lc-nochart`),
  10 (4 wykresy; wszystkie panele liczą na jednym kontrakcie misji w JS i czytają
  ustawienia z kart w innych rozdziałach — zdarzenie `lc:sf-redraw` odświeża ryciny).
- [ ] Test `analiza-ryzyka/tests/testthat/test-mission-analysis.R` „porównanie
  zmiennego p…” odwołuje się do widżetu wykładu 05 (`d5_variation`,
  `failure_data()`), którego nie było już przed migracją: usunąć albo przepisać.
  Pozostałe dwa stałe błędy zestawu (`test-content-contract.R`) też są sprzed migracji.
- [ ] Zaokrąglanie dokładnych połówek: `sprintf()` w R zaokrągla do parzystej,
  `toFixed()` w JS w górę (0.0625 → „0.062” w R, „0.063” w JS). `lcRisk.fixed()`
  i `lcRisk.fmtP()` (analiza ryzyka 06–10) robią to jak R; własne formatowanie
  w skryptach wykładów 03–05 analizy ryzyka i w statystyce nie było pod tym
  kątem sprawdzane.
- [x] Faza 5 (15 listopada 2026): statystyka 05–06 (29 wykresów R; Shapiro, Levene). Gotowe: 06
  05 (23 wykresy): testy „krok po kroku” (t jednej próby, dwumianowy, korelacji,
  χ², t dwóch grup) na wspólnym szkielecie w `05-…/modules/figures.js`
  (rozkład statystyki testowej, werdykt, role kroków), ANOVA z porównaniami
  Gamesa-Howella, rozdział o sile efektu jako warianty z R (`lc_sf_spec(key = )`),
  rozwiązania ćwiczeń składane przy budowaniu. Jedna świadoma różnica wobec
  wersji Shiny: w tabeli Ryc. 10.1 statystyka t ma dwa miejsca po kropce
  (dawniej kolumna zaokrąglała ją do całości: 1.5 pokazywała jako „2”).
  (6 wykresów; dane losowane na jednej rycinie testują panele obok — wspólny
  magazyn w `modules/figures.js` i zdarzenie `lc:sf-redraw`; stan przed
  losowaniem i warianty wyniku w elementach `[data-when]`).
- [ ] Faza 6: statystyka 07–08 (66 wykresów R).
- [x] Faza 7 (15 listopada 2026): statystyka 09–10 (20 wykresów R; modele na stałych zbiorach
  liczone przy budowaniu). Gotowe: 10 (5 wykresów; treści stałe przeniesione
  z serwera do dokumentu narzędziem `scripts/migracja/inline_static_outputs.py`,
  model własny liczy `lcStats.lm()` z kolumn danych w `config`, podgląd danych
  to cała tabela w dokumencie sortowana i okienkowana w przeglądarce);
  09 (15 wykresów; bez własnego JS — każdy wariant kontrolki liczy R, a rysuje
  ogólna rycina „opis z R”: `lc_sf_spec()` i funkcje `sf_*()` w
  `shared/lc_stat_figures.R`; model wieloraki to 32 warianty, po jednym na
  podzbiór predyktorów; przycisk „Dopasuj” zniknął, bo model zmienia się od razu).
- [ ] Drugi test losowy bez ziarna: `statystyka/tests/js/stops34.test.js` zawiódł raz
  na pięć uruchomień (15 listopada 2026), bez związku ze zmienianym kodem.
- [x] Ramki wzorów (`.lc-formula-box`) na stronach statycznych były wyrównane do lewej
  i mniejsze niż pod Shiny: reguła w `shared_styles.css` nie znała klasy `MathJax_SVG`
  wzorów złożonych przy budowaniu (poprawione 15 listopada 2026; dotyczyło wszystkich
  zbudowanych wykładów, więc strony trzeba zbudować ponownie).
- [ ] `scripts/check_static_page.R` nie zauważa ryciny, która przy każdym
  rysowaniu rzuca wyjątek (silnik łapie go i ponawia, strona się zawiesza):
  wykrywa to `scripts/migracja/gdzie_wisi.R`. Dodać taki test do kontroli strony.
- [ ] Faza 8: usunąć `app.R` i kod serwera, statyczny spis wykładów zamiast
  huba, wycofać `przedmioty/*/skrypt/build`, zaktualizować `AGENTS.md`,
  kontrakt i testy.
- [ ] Strona statyczna: spis wykładów kursu (`index.html`); zakładki górnego
  paska prowadzą dziś tylko do wykładów już zbudowanych w `dist/<kurs>/`.
- [ ] Kroje na stronie statycznej są wbudowane w każdy plik (decyzja
  14 listopada 2026): dwanaście plików TTF z `shared/fonts/`, wagi 400–700 jak
  w Google Fonts, ok. 3 MB na wykład. Do rozważenia: format WOFF2 (mniejszy).

### Wspólny rdzeń `shared/`

Stan (14 listopada 2026, gałąź `statyczny-js`): statystyka i analiza ryzyka
ładują layout, paletę, style i biblioteki JS z `shared/`; w `<kurs>/R/` zostały
`course.R`, pliki przedmiotowe i (analiza ryzyka) `course_styles.css`. Opis:
`shared/README.md`. Statystyka 2 ma nadal własny snapshot.

- [ ] **Decyzja:** odstępstwa analizy ryzyka w `analiza-ryzyka/R/course_styles.css`
  (sekcja „Odstępstwa od rdzenia”): wciśnięty segment w kolorze akcentu, dwa
  wykresy obok siebie od 640 px (rdzeń 760 px), szersze pola `lc_drop_match`
  (rdzeń: kolumny 120 px). Każde: zostawić, przyjąć zachowanie rdzenia albo
  przenieść do rdzenia dla obu kursów.
- [ ] **Decyzja:** przełączniki `.LC_COURSE` (`chapter_mode`, `plot_scaling`)
  zachowują dawne zachowanie każdego kursu. Znikają razem z serwerem Shiny;
  do tego czasu można je ujednolicić albo zostawić.
- [ ] Uzgodnić `analiza-ryzyka/R/DESIGN_CONTRACT.md` z `shared/DESIGN_CONTRACT.md`:
  zostawić w kursie tylko reguły własne (`risk_*`, nagłówki rozdziałów).
- [ ] Testy rdzenia (`test-widgets-v2.R`, `test-panel-layout.R`) są w dwóch
  kopiach w `<kurs>/tests/`; przenieść do jednego miejsca.
- [ ] Build offline w `przedmioty/*/skrypt/build/build_offline.R` czyta czcionki
  z `<kurs>/R/fonts` i śledzi zmiany w `<kurs>/R`; na tej gałęzi wymaga
  wskazania `shared/` (albo znika razem z przejściem na strony statyczne).

### Migracja widgetów do v2

Stan:

- Komponenty v2 (widgety, tabele, bloki tekstu) są we wspólnym `shared/`
  (`lecture_layout.R`, `shared_styles.css`, `lc_widgets.js`); statystyka 2 ma
  własną kopię w `statystyka-2/R/`. Zasady: `shared/DESIGN_CONTRACT.md`.
- Audyt z 3 października 2026 (`statystyka/scripts/audit_panels.R`,
  24 aplikacje, 1440 i 390 px, stan po wejściu do rozdziału, bez interakcji):
  510 paneli, 405 bez starych elementów, 0 wylewa się na telefonie,
  0 błędów Shiny.
- Statystyka 00–09 i analiza ryzyka 01–10 są przeniesione (skrypty
  `statystyka/scripts/migrate_v2_*.R` plus ręczne poprawki). Zostały
  pojedyncze wyjątki wypisane niżej. Prawie całe pozostałe stare elementy
  to statystyka 2.

Panele ze starymi elementami (audyt 3 października 2026; paneli / gotowe /
układ kolumn / pudełka statystyk / wykres o stałej wysokości / `lc_feedback`):

| Wykład | Pan. | Got. | Kol. | Stat. | Wykr. | Feedb. |
|---|---|---|---|---|---|---|
| statystyka-2 01-symulacje-statystyczne | 23 | 7 | 16 | 0 | 16 | 0 |
| statystyka-2 02-metody-bayesowskie | 13 | 3 | 10 | 0 | 10 | 10 |
| statystyka-2 03-kierunkowe | 14 | 0 | 14 | 11 | 14 | 3 |
| statystyka-2 04-szeregi-czasowe | 41 | 4 | 37 | 4 | 35 | 21 |

W statystyce i analizie ryzyka został tylko jeden wykres z kliknięciem
na `zoom_plot_ui` (statystyka 07, ćwiczenie z prostą).

Zostało:

- [ ] Statystyka 2 (niski priorytet): przegląd wykład po wykładzie tym samym
  wzorcem co statystyka — skrypty `migrate_v2_columns.R`,
  `migrate_v2_readouts.R` (report → apply → apply2), potem tabele i ręczne
  układy. W kodzie: `fluidRow` 112, `lc_stat_box` 76, `lc_feedback` 171,
  wykresy `zoom_plot_ui` o stałej wysokości 99, stare tabele 16.
- [ ] Radio w panelach: statystyka i analiza ryzyka zrobione (karty
  `lc-choices`, lista rozwijana w 02 Ryc. 2.1). Została statystyka 2 (1).
  Serie akcji („1× / 10× / 100×”) w statystyce i analizie ryzyka →
  `lc_action_group()` (4 października 2026).
- [ ] Wykresy bez tytułów (decyzja: żadnych tytułów). Statystyka i analiza
  ryzyka 02 zrobione 4 października 2026 (liczby → `lc_readout()`, objaśnienia
  → `lc_caption()`). Analiza ryzyka 01 Ćw. 2 przebudowane według prototypu B
  (prototypy usunięte). Została statystyka 2: `01-symulacje-statystyczne/modules/helpers.R` (6),
  `02-metody-bayesowskie/modules/helpers.R` (2),
  `04-szeregi-czasowe/modules/` `ch11_ets.R`, `ch5_acf.R`, `ch8_ar.R`,
  `ch9_ma_arma.R` (po 2).
- [ ] Usunąć `lc_table_region()` i klasy `lc-table*`, gdy statystyka 2 nie
  będzie już używać starych tabel.
- [ ] Legendy ggplot wychodzące poza wykres na telefonie (np. statystyka 01
  Ryc. 2.5) — audyt ich nie wykrywa, sprawdzać na zrzutach.
- [ ] Analiza ryzyka 08 „Krok po kroku” (redukcja układu C + A/B): do pełnej
  przebudowy — widget prawdopodobnie nie działa poprawnie. Na razie zostaje
  na kropkach (`lc_step_nav()`); przy przebudowie rozważyć
  `lc_step_widget(body = …)` albo schemat redukcji jako wykres.

Bloki tekstu: statystyka zrobiona 4 października 2026 (`inline_callout` →
`lc_note`/`lc_warn`, wstępy „Przykład:” → `lc_note`, numeracja → `lc_h3(num =)`,
martwy CSS po marginesie i starych calloutach usunięty we wszystkich kursach).
Zostało w statystyce 2:

- [ ] `inline_callout()` / `margin_*()` → `lc_note()` (11), pogrubiony wstęp
  (1), spacje przed interpunkcją po `tags$strong()` / `tags$em()` (11),
  emoji (4 linie). Analiza ryzyka: spacje przed interpunkcją (5).

Zasady migracji:

- Zmiany wspólnych komponentów wprowadzać równolegle we wszystkich
  snapshotach kursów, zachowując różnice kursowe.
- Reagować na szerokość kontenera, nie tylko viewportu.
- Nie usuwać informacji ani nie zmniejszać tekstu, żeby zmieścić widget.
- Przewijanie ograniczać do tabeli i traktować jako zabezpieczenie.
- Dla każdego widgetu osobno ustalić szerokość panelu i reorganizację
  sterowania, wykresów, tabel i podsumowań.
- Nie wprowadzać globalnego `fit-content` dla wykresów o szerokości 100%.
- Jeden commit = wspólna infrastruktura albo migracja jednego widgetu albo
  treść jednego rozdziału.

### Kandydaci na wspólne komponenty

Obecnie lokalne; uogólnienie wymaga osobnej decyzji.

- [ ] **Decyzja:** czy interaktywny łańcuch pojęć z analizy ryzyka 01 ma być
  wspólnym komponentem.
- [ ] Więcej wykresów JS (ECharts). Kryterium (2026-11-05, zasada
  w `shared/DESIGN_CONTRACT.md`): wykres JS działa w wersji
  offline dla studentów (`przedmioty/statystyka/skrypt/`, bez serwera R)
  i zwiększa responsywność, więc wierne odtworzenie wykresu R też jest
  zyskiem — nie tylko widgety ggplot + suwak. Prowadzący potwierdził na
  zajęciach, że wykresy Chart.js są wyraźnie bardziej responsywne niż
  renderowane w Shiny (2026-10-29). Wspólna obsługa: `shared/lc_charts.js`.
  Stan 2026-11-09: statystyka 01, 02, 03, 04 i 05 na ECharts. W 04 wszystkie
  ryciny rysuje ECharts: eksploratory w `ci_explorers.js`, wykresy ggplot przez
  most `ci_echarts.R` / `ci_echarts.js` (obliczenia w R, rysunek w JS).
  - [ ] Migracja statystyki z Chart.js na ECharts (decyzja 2026-11-05:
    renderer SVG, canvas dla tysięcy punktów; pełny `echarts.min.js`
    z cdnjs). Analiza ryzyka od 2026-11-13 też na ECharts: 04 w całości
    (sceny `sceny.js`, wykresy `wykresy.js` na `lcSF`, offline 7.5 → 3.1 MB);
    05 bez obliczeń w R poza głosowaniem i quizem (sceny z kadrami liczone
    w R z góry, sceny losowe i rozkład geometryczny w JS; wzory w odczytach
    scen jako HTML, bo offline nie ma MathJax; offline 3.3 → 2.9 MB, zero PNG).
    Od 2026-11-14 także trzy piaskownice z uchwytem w 05 (nb, plan, mix) na ECharts;
    wykład nie ładuje już Chart.js (offline 2.7 MB).
    03 bez obliczeń w R poza głosowaniem i quizem: `detektor.js` liczy tablicę 2×2,
    drzewo Bayesa (kroki nadal w `drzewo.js`) i ryciny lcSF (dwa alarmy, próg
    reakcji, Serpuchów); suwaki tablicy są wspólne dla kilku rozdziałów.
    02 bez obliczeń w R poza quizem: Monty Hall w `monty.js` (rycina lcSF),
    „Trzy widoki” to stały SVG i tabela z R (`monty_server.R` usunięty).
    01 bez obliczeń w R poza decyzją, ćwiczeniami i quizem: `jezyk.js` (łańcuch
    pojęć, ocena klasyfikacji Ćw. 1.1/1.2, ryciny lcSF częstości i siatki zdarzeń,
    kadry Demonstracji 1.1), Rycina 1.1 i Venn jako SVG z `rysunki.R`;
    offline 1.9 MB, zero PNG. Wspólna obsługa
    w `lc_charts.js` gotowa. Wykłady 01, 02, 03 i 05 w całości na ECharts
    (2026-11-05; mostek offline `widgets/01-ch2-jakosciowe.js` też), 04 od
    2026-11-09. Wykłady statystyki nie ładują już Chart.js (poza
    `prototypy/chart-dostepnosc`); zostało usunąć `styleAxes`,
    `create` z `lc_charts.js` (po sprawdzeniu testów, które ich używają)
    i rozważyć okrojoną bibliotekę. Wykresy R, których na razie nie przenosimy, są
    w wersji offline zamrożone (decyzja 2026-11-05).
  - [x] Statystyka 04: decyzje o prototypach i port wszystkich wykresów
    na ECharts (2026-11-09). Ryciny rysowane przez most `ci_echarts` nadal
    potrzebują serwera R do obliczeń; w pełni offline działają eksploratory,
    Ryc. 2.1 i 4.2 oraz sceny SVG.
  - [x] Most ggplot → ECharts we wspólnym `shared/lc_ggplot_echarts.*`
    (2026-11-09); wykład 02 w całości na ECharts (tabela krzyżowa, pudełka).
    Od 2026-11-13 wykład 02 działa w całości offline: tabela krzyżowa i pudełka
    w `survey_charts.js` na danych ankiety z wykładu 01; mostu już nie ładuje.
  - [x] Statystyka 01: wszystkie 30 wykresów ggplot przepisane w JS na danych
    ankiety (`stat_charts.js`, dane w stronie przez `survey_data.R`), 2026-11-10.
    Ryciny działają offline; opisy dla danych ankiety liczone w R z góry.
  - [x] Statystyka 03: wszystkie wykresy R przepisane w JS (2026-11-13). Rozkłady
    teoretyczne w `dist_charts.js`, symulacje w `sim_charts.js`, punkty stop
    w `stops12/34/56_charts.js`, wspólny silnik `shared/lc_stat_figures.*`,
    rozkłady i losowanie `shared/lc_dist.js`. Ryc. 1.2 (lejek) nadal dostaje serie
    z serwera.
  - [ ] Statystyka 05–10: audyt kandydatów (najwięcej suwaków: 05 i 07).
  - [ ] Statystyka 03 offline, Ryc. 1.2 (lejek): odczyty przechodzą zapisane
    stany, ale wykres zostaje pusty — serie przychodzą z serwera
    (`lejek_series`). Potrzebny mostek `"live": true` w `stany.json`
    i losowanie serii w `build/widgets/03-*.js` (sprawdzone 2026-11-05;
    dotyczyło też wersji Chart.js, bo mechanizm jest ten sam).
    Wzór mostka: 05 Ryc. 2.4 (`"live": true`, `build/widgets/05-ch2-hipotezy.js`).
    Widgety z `data-config` działają offline bez dodatkowych wpisów
    (`lcCharts` ustawia `data-ready`, build wstawia czystą kopię z pristine.js).
  - [ ] Wersje offline statystyki (`przedmioty/statystyka/skrypt/`): przebudować
    00, 04, 06–10 (`build_offline.R --check`). 03 wymaga tylko `--stamp --only=03`
    — po zrzucie zmienił się wyłącznie `shared/DESIGN_CONTRACT.md`. 01, 02, 03 (treść)
    i 05 aktualne na 2026-11-05.
  - [ ] Statystyka 03, quiz: przejrzeć parametry przykładowych realizacji
    (pole `example` w `modules/quiz_rozklady.json`). Część dobrana, bo pytania
    ich nie podają: m.in. wzrost N(178, 7), IQ N(100, 15), zarobki LogN
    z medianą 6000 zł, usterki kabla Pois(2), PM10 z medianą 30 µg/m³,
    trzęsienia ziemi Exp(15 lat), pliki LogN(mediana 0.5 MB, σ log 1.5).
  - [ ] Statystyka 03, rozdz. 6 (prototypy przyjęte 2026-11-06):
    sprawdzić, czy osadzony film (jvoxEYmQHNM) to ten o smokach i królikach.

- [ ] Przejrzeć widgety aktywnych wykładów i wskazać, które skorzystają na
  sterowaniu klawiaturą (Tab, strzałki, większy krok z Shiftem, Home/End),
  skalowaniu napisów wraz z S/M/L oraz lokalnym powiększeniu wykresu małą,
  ujednoliconą lupą z boku. Wzorzec: statystyka 03, Ryc. 4.3
  (`modules/area_chart.js`, `lc_chart_zoom_button()`, `shared/lc_charts.js`).
  Dobierać mechanizmy do celu dydaktycznego; zachować minimalizm. Pola liczbowe
  dodawać tylko tam, gdzie dokładna wartość jest istotna, nie automatycznie
  w demonstracjach pojęć (zaakceptowane 2026-10-31).
  Ocena wykładów statystyki 00–04 jest w
  [OCENA-WIDGETOW-00-04.md](statystyka/OCENA-WIDGETOW-00-04.md): pierwsza kolejka
  to dystrybuanta, kwantyl, percentyle, zgadywanie średniej/mediany i wartość
  odstająca. Pierwsza i druga kolejka zostały wdrożone (2026-10-31).
  Druga obejmuje pensję prezesa, kurtozę, siłę korelacji, nachylenie, dwie
  krzywe normalne, porównanie wyjątkowości i galerię przypadków normalnych.
  Trzecia kolejka (czytelność pozostałych pięciu widgetów Chart.js) również
  wdrożona: koło/słupki, odporność miar, Simpson, lejek i kalkulator z-score.
  Pozostają przegląd pozostałych aktywnych wykładów oraz ocena na zajęciach.

### Zapis liczb: kropka dziesiętna i zwykły minus

Decyzja: wszędzie kropka dziesiętna i zwykły minus `-`, jak w R i jamovi.
Helpery v2 (`lc_fmt()`, `lc_num()`, `lc_pval()`, `shared/lc_widgets.js`) już
to stosują.

- [x] Przestawić stare formatery na kropkę: `format_p_value()` / `format_p()` /
  `ui_p_value()` we wspólnym `shared/shared.R`, `risk_format_probability()` i
  lokalne formatery analizy ryzyka.
- [ ] Przejrzeć teksty wykładów we wszystkich kursach i zamienić przecinek
  dziesiętny na kropkę (np. „p = 0,10” w analizie ryzyka 05 obok widgetu
  pokazującego 0.1). Zmieniać osobnymi commitami per wykład. Zrobione:
  statystyka 01, 03–10 i słownik statystyki (także minus przed liczbą).
  Analiza ryzyka 01–10 zrobiona (z odczytami, 3 października 2026; zbiory
  {1,2} zostają z przecinkiem, N(82, 3) ze spacją).
  Zostało: statystyka 2.
- [ ] Typograficzny minus `−` w liczbach → `-` i etykiety ggplot bez
  `decimal.mark = ","`: statystyka i analiza ryzyka sprawdzone 4 października
  2026 (liczby ujemne już ze zwykłym minusem; `−` jako znak działania
  w wyrażeniach typu „Q3 − Q1” zostaje). Zostało: statystyka 2 (m.in.
  `03-kierunkowe/modules/helpers.R` z `decimal.mark = ","`, „θ₁ = −0.9”,
  „[−1, 1]”).

---

## Statystyka

### Cały kurs

- [ ] Ściągi „jak zrobić to w jamovi” (na razie bez instrukcji programowych
  w treści wykładów). Materiał usunięty z wykładów 01–05 (2026-10-03),
  do weryfikacji w jUPWR:
  - 03 rozdz. 3 (ch3_srednia.R): CI dla średniej — Analyses → T-Tests → One Sample T-Test, przeciągnąć zmienną ilościową do Dependent Variables, w panelu Additional Statistics zaznaczyć Confidence interval (domyślnie 95%); średnią i granice odczytać z kolumn Mean, Lower, Upper.
  - 03 rozdz. 3 (ch3_srednia.R): CI dla różnicy średnich w wersji Welcha — Independent Samples T-Test z zaznaczoną opcją Welch's; domyślnie zaznaczona jest opcja Student's (równe wariancje, wariancja łączona).
  - 03 rozdz. 4 (ch4_proporcja.R): CI dla proporcji — Analyses → Frequencies → 2 Outcomes — Binomial test → przeciągnąć zmienną binarną (np. zdany/niezdany) do pola zmiennych → zaznaczyć Confidence interval (domyślnie 95%); jamovi nie liczy przedziału Walda, tylko Cloppera-Pearsona; w tabeli odczytać kolumny Proportion, Lower, Upper.
  - 04 rozdz. 04 (ch2_jedna_ilosciowa.R): test t jednej próby — T-Tests → One Sample T-Test, wartość μ₀ wpisujemy w polu Test value.
  - 04 rozdz. 04 (ch2_jedna_ilosciowa.R): test jednostronny — kierunek Hₐ wybieramy w sekcji Hypothesis okna One Sample T-Test.
  - 04 rozdz. 05 (ch3_jedna_jakosciowa.R): test dwumianowy — Analyses → Frequencies → 2 Outcomes — Binomial test, wartość p₀ w polu Test value.
  - 04 rozdz. 05 (ch3_jedna_jakosciowa.R): tabela porównawcza — test dwumianowy: 2 Outcomes — Binomial test; test proporcji (z-test): N Outcomes — χ² Goodness of fit (dla dwóch kategorii odpowiada z-testowi bez poprawki).
  - 04 rozdz. 07 (ch5_dwie_jakosciowe.R): tabela χ² vs Fisher — w jamovi χ² jest domyślny, test Fishera: zaznacz Fisher's exact test.
  - 04 rozdz. 08 (ch6_dwie_grupy.R): Independent Samples T-Test ma domyślnie zaznaczoną opcję Student's; żeby dostać test Welcha (zgodny z panelami wykładu), zaznacz Welch's.
  - 04 rozdz. 08 (ch6_dwie_grupy.R): test t dla prób zależnych — Paired Samples T-Test, oba pomiary jako osobne kolumny.
  - 04 rozdz. 08 (ch6_dwie_grupy.R): ćwiczenia CASchools (akapit wprowadzający) — w jamovi zaznacz opcję Welch's, żeby wynik był zgodny z rozwiązaniem.
  - 04 rozdz. 09 (ch7_anova.R): w oknie One-Way ANOVA domyślnie liczony jest wariant Welcha, więc jamovi pokaże inną wartość F i niecałkowitą drugą liczbę stopni swobody niż klasyczna ANOVA.
  - 04 rozdz. 09 (ch7_anova.R): post hoc — One-Way ANOVA → Post-Hoc Tests → ✓ Games-Howell (dawna notka margin_code_note „W jamovi”).
  - 04 rozdz. 09 (ch7_anova.R): macierz p-wartości w panelu Ryc. 9.3 ma układ tabeli post hoc z jamovi („Tak wygląda tabela post hoc w jamovi — odczytaj p-wartość dla każdej pary grup”).
  - 04 rozdz. 09 (ch7_anova.R): ćwiczenia CASchools (akapit wprowadzający) — klasyczna ANOVA jak w rozwiązaniu: w oknie One-Way ANOVA zaznacz opcję Assume equal (Fisher's).
  - 05 rozdz. 01 (ch1_normalnosc.R): Okna testu t i ANOVA mają sekcję Assumption Checks z opcjami Normality test (Shapiro-Wilk) i Q-Q plot.
  - 05 rozdz. 02 (ch2_wariancje.R): Independent Samples T-Test ma domyślnie zaznaczoną opcję Student's; żeby dostać test Welcha, zaznacz Welch's.
  - 05 rozdz. 02 (ch2_wariancje.R): ANOVA Welcha jest domyślna w oknie One-Way ANOVA.
  - 05 rozdz. 04 (ch4_mapa.R): Test t Welcha trzeba zaznaczyć (opcja Welch's), bo okno Independent Samples T-Test domyślnie liczy wersję Studenta.
  - 05 rozdz. 04 (ch4_mapa.R): ANOVA Welcha — okno One-Way ANOVA liczy ją domyślnie.
  - 05 rozdz. 04 (ch4_mapa.R): tabela testów parametrycznych, test t dla prób niezależnych — „w jamovi zaznacz Welch's”.
  - 05 rozdz. 04 (ch4_mapa.R): selektor, test t dla prób niezależnych — „w jamovi zaznacz Welch's”; ANOVA — ANOVA Welcha „domyślna w jamovi”.
- [x] Wdrożyć `gloss()` we wszystkich wykładach: owijać pierwsze
  wprowadzenie kluczowego terminu w rozdziale, nie każde wystąpienie. Nowe
  hasła dopisywać do `statystyka/R/glossary.R`. Wzorzec:
  `04-przedzialy-ufnosci/modules/ch1_estymacja.R`.

### 01 — typy danych


### 02 — rozkłady prawdopodobieństwa


### 06 — regresja


### 08 — case studies

- [x] Rozbudowa poza CASchools: rozdz. 02 pingwiny (ANOVA Welcha,
  Games-Howell, płeć), rozdz. 03 Titanic (χ², zakłócanie płcią, regresja
  logistyczna); pytania `lc-choices` we wszystkich trzech (4 października
  2026). Case czasowy pominięty: szeregi czasowe są dopiero w statystyce 2.

---

## Statystyka 2

Robocze rozdziały „na kiedyś” — nie są teraz prowadzone. Zmiany wspólnych
komponentów trafiają tu automatycznie; treść odkładamy do wznowienia kursu.
Rekomendacje (do przyjęcia przy wznowieniu, bez dalszych decyzji teraz):

- Ryc. 3.2 (stabilność CI, `01-symulacje-statystyczne/modules/ch3_bootstrap_jednopr.R`)
  zawsze używa próby z widgetu Ryc. 3.1 — zostawić: jedna próba w rozdziale
  jest spójniejsza niż osobne losowanie.
- Tytuły z wynikami (lista w sekcji migracji) i tytuły w `plot_bootstrap_step()` /
  `plot_bootstrap_distribution()` (`01-symulacje-statystyczne/modules/helpers.R`,
  używane przez `ch1_idea.R`) oraz Ryc. 8.1 i 8.3 w `04-szeregi-czasowe/modules/ch8_ar.R`
  — przy migracji rozdziału przenieść liczby do opisu kroku albo odczytów,
  tytuły usunąć.
- Wykresy bootstrapu (rozdziały 1–2) na `plotOutput(height = "auto")` bez
  powiększenia — zostawić, dopóki kurs jest odłożony; przy wznowieniu dodać do
  `zoom_plot` obsługę wysokości liczonej w serwerze.
- Migracja etapu 3 (układy kolumn: 81 z 91 paneli, ok. 170 wywołań
  `lc_feedback()` → `lc_note()` / `lc_warn()` / `lc_status()` / `lc_caption()`,
  rozwiązania → `lc_more()`) — dopiero przy wznowieniu kursu. Po niej można
  usunąć `lc_feedback()` i klasy `.lc-feedback*` ze wspólnego `R/`.

---

## Analiza ryzyka

### 01 — język ryzyka

- [ ] Ocenić interaktywny łańcuch pojęć jako treść tego wykładu (uogólnienie —
  patrz sekcja globalna).
### 05 — ile prób do zdarzenia

- [ ] Sekcja `rte/parametryzacje`: dopisać zdanie o konwencji „+1” dla
  rozkładu geometrycznego — `dgeom`/`pgeom`/`qgeom` liczą porażki przed
  pierwszym sukcesem, dlatego w kodzie kursu dodaje się 1 (serwer już robi
  `rgeom() + 1`, `qgeom() + 1`, l. 449, 457; wykład nazywa tylko „+r”).
  Wstrzymane 4 października 2026: treść wykładów nie zawiera kodu R ani
  nazw funkcji — konwencja „+1” należy do materiałów z R/jRISK.
### 06 — zmienność i próg

Uwaga: powtórzone `id = "most"` w różnych rozdziałach nie jest błędem —
kotwice sekcji to `blok-rozdział-sekcja` (`R/risk_block.R`, l. 420).

### 10 — od modelu do decyzji

- [ ] **Zunifikować misję ochrony termicznej z danymi Bananpol z jRISK**
  (decyzja 2026-10-02: wariant A1, odłożone na osobną sesję). Cel: wątek
  ćwiczeń dane → parametr → model → decyzja domyka się w wykładzie 10.
  Wentylatory z dopasowania do `jRISK/data/bananpol.csv` (typ = wentylator;
  ćwiczenie 8.6): wykładniczy MTTF ≈ 15,36 mies., Weibull β ≈ 1,19,
  η ≈ 15,92 mies.; misja = półrocze między przeglądami (6 mies.); R_P = 0,98,
  R_C = 0,98 (STER z `bananpol_system`) na misję; P(I) = 0,005, czułość 0,95.
  Przeliczone wstępnie (model z `risk_mission_analysis`, u = 0,2):
  P(TOP) 0,000915 (wykł.) / 0,000766 (Weibull); limit demonstracyjny trzeba
  obniżyć z 0,002 do 0,001 (przy 0,002 nawet „bez działania” spełnia limit);
  ranking bez zmian (ograniczenie źródła ciepła wygrywa, czujnik przekracza
  limit w ostrożnym). Odnowa: wentylator R(18) / R(6)³ / R(3)⁶ = 0,314 / 0,392
  / 0,440, agregat (β ≈ 3,23, η ≈ 30) 0,826 / 0,984 / 0,997 — kontrast
  zużycia z danych zamiast β = 2. Na co uważać:
  - **Wykład 07 zostaje przy kartach katalogowych** (MTTF 1500 h, β = 2,
    η = 1700 h; decyzja 2026-10-02). Nie zestawiać ich liczbowo z danymi: przy
    pracy ciągłej 1500 h ≈ 2 mies., a dane dają MTTF ≈ 15 mies. (7× dłużej) —
    dwa niespójne „światy” wentylatorów. W 10 tylko zdanie, że w miejsce
    hipotez z 07 wchodzą modele dopasowane do danych eksploatacyjnych.
    Parametry `lifetime.*` w `R/bananpol.R` należą do 07 — dla 10 dodać osobne
    pola w `integration`, nie nadpisywać.
  - `risk_mission_analysis()` (`R/risk_math.R`) ma wpisane na sztywno 1500/1700
    i skalowanie `R(1000)^(t/1000)` — sparametryzować (model wentylatora
    i czas bazowy z rejestru), przemianować argumenty `*_r1000`.
  - Jednostki: cały wykład 10 jest w godzinach (suwak 100–3000 h, „po 400 h”,
    „w 200. godzinie”, przykłady 10.5–10.11, notatka 10.12, quiz o R(3000)
    i R(1000)³, ćwiczenia w `integracja_exercises`, wykres karty 3). Przejście
    na miesiące musi być konsekwentne — wykład uczy „wspólnej etykiety czasu”.
  - Horyzont roczny: 2 misje (półrocza) zamiast 3; wzór 10.9 z potęgą 2.
  - Przeliczyć wszystkie liczby w tekście (ok. 40 miejsc) i sprawdzić
    zdania jakościowe, które się zmieniają: różnica modeli wentylatora maleje
    z ≈ 1,7× do ≈ 1,2× (β obejmuje prawie 1 — dane zawęziły niepewność
    modelu); przykład 10.11(b) i „wygrywa aż do u = 0,45” — odwrócenie
    rankingu przy u = 0,5 trzeba sprawdzić na nowo.
  - Testy: `tests/testthat/test-mission-analysis.R` (exp(−1000/1500), .95,
    `i10_time = 1000`, limit .002) i `test-risk-math.R` (tylko 07 — zostaje).
    Uruchomić całość przed i po zmianie (`Rscript analiza-ryzyka/tests/testthat.R`).
  - Razem z wykładem zmienić: przewodnik prowadzącego (sekcja 10), listę
    ćwiczeń tygodnia 11 (11.1–11.3, 11.a, 11.b) i `jrisk-pokrycie-zadan.md`
    w `~/praca/dydaktyka/przedmioty/analiza-ryzyka/`; klucze 11.x liczyć z tych
    samych zaokrąglonych parametrów co wykład.
  - Rozważony i odrzucony głębszy wariant (D z przeoczeń w `bananpol_alarmy`,
    S = 1 − R linii z `bananpol_system`): miesza jednostki zmiana/półrocze.
