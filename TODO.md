# TODO — Interaktywne wykłady

Jedyny plik zadań w repozytorium. Zrobione punkty usuwamy — historia jest
w gicie.

Jak przypisać zadanie:

- **Globalne** — zmienia wspólne komponenty, konwencje lub testy używane przez
  więcej niż jeden kurs (np. snapshoty `statystyka/R/` i `analiza-ryzyka/R/`).
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
   Widgety: statystyka 00–09 i analiza ryzyka gotowe (audyt 3 października
   2026: 510 paneli, 0 przelewów, 0 błędów). Zostały statystyka 2 (niski
   priorytet) i bloki tekstu — lista w „Migracja widgetów do v2”. Świadome
   wyjątki w statystyce: quizy z długimi etykietami na radio (02, 04 Ryc. 3.4),
   wykres z kliknięciem na `zoom_plot_ui` (06 ćwiczenie z prostą), drzewo
   decyzyjne 04 z własnym przyciskiem pełnego ekranu, schemat
   `type-error.jpg` w 04 rozdz. 3 poza panelem.

---

## Globalne

### Migracja widgetów do v2

Stan:

- Komponenty v2 (widgety, tabele, bloki tekstu) są we wspólnym `R/`
  (`lecture_layout.R`, `shared_styles.css`, `lc_widgets.js`), identyczne we
  wszystkich kursach. Zasady: `R/DESIGN_CONTRACT.md`.
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
| analiza-ryzyka 01-jezyk-ryzyka | 16 | 10 | 1 | 3 | 1 | 0 |

W pozostałych wykładach statystyki i analizy ryzyka stare elementy to tylko
radio (patrz niżej) i jeden wykres z kliknięciem (statystyka 06).

Zostało:

- [ ] Statystyka 2 (niski priorytet): przegląd wykład po wykładzie tym samym
  wzorcem co statystyka — skrypty `migrate_v2_columns.R`,
  `migrate_v2_readouts.R` (report → apply → apply2), potem tabele i ręczne
  układy. W kodzie: `fluidRow` 112, `lc_stat_box` 76, `lc_feedback` 171,
  wykresy `zoom_plot_ui` o stałej wysokości 99, stare tabele 16.
- [ ] Analiza ryzyka 01 Ćw. 2 i prototypy: 16 `lc_stat_box` i jeden układ
  kolumn — po wyborze wariantu ćwiczenia.
- [ ] Radio w panelach (25): w analizie ryzyka po 2 w każdym wykładzie
  (sprawdzić, czy to wspólny element — wtedy jedna zmiana), statystyka 02 (2),
  04 Ryc. 3.4 (quiz z długimi etykietami, zostaje), statystyka 2 (1).
- [ ] Wykresy bez tytułów (decyzja: żadnych tytułów). Zostały dynamiczne
  tytuły z wynikami albo objaśnieniem oznaczeń; przy migracji widgetu liczby
  → `lc_readout()`, objaśnienie → `lc_caption()`, potem usunąć tytuł.
  Miejsca (stan 3 października 2026):
  - `analiza-ryzyka/01-jezyk-ryzyka/modules/block.R` (l. 1654, 2163, 2172, 2212)
  - `analiza-ryzyka/02-warunki/modules/monty_server.R` (l. 195)
  - `statystyka/01-typy-danych/modules/ch4_rozrzut.R` (l. 979)
  - `statystyka/04-wnioskowanie-statystyczne/modules/ch10_sila_efektu.R` (l. 803)
  - `statystyka/06-regresja/modules/ch3b_kontekst.R` (l. 281, 359)
  - statystyka 2: `01-symulacje-statystyczne/modules/helpers.R` (6),
    `02-metody-bayesowskie/modules/helpers.R` (2),
    `04-szeregi-czasowe/modules/` `ch11_ets.R`, `ch5_acf.R`, `ch8_ar.R`,
    `ch9_ma_arma.R` (po 2)
- [ ] Usunąć `lc_table_region()` i klasy `lc-table*`, gdy statystyka 2 nie
  będzie już używać starych tabel.
- [ ] Legendy ggplot wychodzące poza wykres na telefonie (np. statystyka 01
  Ryc. 2.5) — audyt ich nie wykrywa, sprawdzać na zrzutach.
- [ ] Analiza ryzyka 08 „Krok po kroku” (redukcja układu C + A/B): do pełnej
  przebudowy — widget prawdopodobnie nie działa poprawnie. Na razie zostaje
  na kropkach (`lc_step_nav()`); przy przebudowie rozważyć
  `lc_step_widget(body = …)` albo schemat redukcji jako wykres.

Bloki tekstu (stan 3 października 2026; ramki `lc-feedback`, zwijane
`tags$details` i ikony `case-icon` są już usunięte we wszystkich kursach):

- [ ] Wywołania `inline_callout()`, `margin_callout()`, `margin_note()`,
  `margin_code_note()` → `lc_note()` (statystyka 32, statystyka 2: 11;
  działają, ale są zakazane w nowym kodzie).
- [ ] Pogrubione wstępy `strong("Przykład:" / "Kontrprzykład:" / "Uwaga:" /
  "Zasada:")` na początku akapitu → `lc_note()` (statystyka 5,
  statystyka 2: 1).
- [ ] Spacje przed interpunkcją po `tags$strong()` / `tags$em()` → `b_()` /
  `em_()` (statystyka 3, statystyka 2: 11, analiza ryzyka 5).
- [ ] Ręczna numeracja podsekcji („(1) Nieobciążoność”, „A. …”) →
  `lc_h3("…", num = "1")` — do sprawdzenia, ile zostało.
- [ ] Emoji w treści statystyki 2 (4 linie); w statystyce zostało tylko pole
  `icon` w `08-case-studies/modules/helpers.R`.
- [ ] Usunąć martwy CSS po marginesie i starych blokach: `.lc-margin*`,
  `.lc-inline-callout*`, `.lc-callout-*`, `.lc-def*`, `.lc-example*`,
  `.lc-try` (po sprawdzeniu, że nic ich nie używa; `inline_callout()`
  renderuje się już jako `lc_note()`).

Decyzje:

- [ ] **Decyzja:** Ryc. 2.1 (statystyka 01) na rzutniku z dużym fontem
  dzieli się na dwie tabele, bo panel `text` ma stałe 680 px. Warianty:
  akceptujemy / szerokość `wide` dla wąskich widgetów tabelowych / szerokości
  paneli w `em`.
- [ ] **Decyzja:** wykresy w analizie ryzyka 02 (`w2_filter_plot`) i 03
  (`a3_grid`) dostały `ratio` i `max_height` szacunkowo z dawnej wysokości —
  obejrzeć i zatwierdzić.

Pozostałe:

- [ ] `lc_col(type = "num")`: dodać sufiks jednostki (np. `suffix = " cm"`,
  `"%"`). Teraz kolumna z jednostkami musi być gotowym tekstem i traci
  wyrównanie cyfr (statystyka 01, Ryc. 4.6, kolumna „Wartość”; cm i % oraz
  1 lub 2 miejsca po kropce w jednej kolumnie).

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

### Zapis liczb: kropka dziesiętna i zwykły minus

Decyzja: wszędzie kropka dziesiętna i zwykły minus `-`, jak w R i jamovi.
Helpery v2 (`lc_fmt()`, `lc_num()`, `lc_pval()`, `R/lc_widgets.js`) już
to stosują.

- [x] Przestawić stare formatery na kropkę: `format_p_value()` / `format_p()` /
  `ui_p_value()` we wspólnym `R/shared.R`, `risk_format_probability()` i
  lokalne formatery analizy ryzyka.
- [ ] Przejrzeć teksty wykładów we wszystkich kursach i zamienić przecinek
  dziesiętny na kropkę (np. „p = 0,10” w analizie ryzyka 05 obok widgetu
  pokazującego 0.1). Zmieniać osobnymi commitami per wykład. Zrobione:
  statystyka 01–09 i słownik statystyki (także minus przed liczbą).
  Analiza ryzyka 01–10 zrobiona (z odczytami, 3 października 2026; zbiory
  {1,2} zostają z przecinkiem, N(82, 3) ze spacją).
  Zostało: statystyka 2.
- [ ] Zamienić typograficzny minus `−` w liczbach na zwykły `-` (teksty,
  formatery, etykiety wykresów).
- [ ] Sprawdzić etykiety osi i liczby w ggplot (np. `scales::label_number`
  z `decimal.mark = ","`).

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
- [ ] Tekst bezosobowo w całym kursie: zamiast zwracania się do studenta
  („zmierzyłeś”, „policzyłeś”, „spróbuj odpowiedzieć sam”) formy
  bezosobowe („zmierzono”, „policzono”, „warto najpierw odpowiedzieć”).
  Unikamy w ten sposób odmiany przez rodzaj i zaimków. Przykład:
  03 rozdz. 3 (ch3_srednia.R), przypadek A1 „Zmierzyłeś wzrost 30 studentów”.
- [ ] Wdrożyć `gloss()` we wszystkich wykładach: owijać pierwsze
  wprowadzenie kluczowego terminu w rozdziale, nie każde wystąpienie. Nowe
  hasła dopisywać do `statystyka/R/glossary.R`. Wzorzec:
  `03-przedzialy-ufnosci/modules/ch1_estymacja.R`.

### 01 — typy danych

- [ ] Widget autobusów (rozdz. 4, kroki 3–4): w symulowanych danych żaden
  autobus nie przyjeżdża przed czasem, więc „zdążysz” wynosi zawsze 100%
  dla obu linii niezależnie od suwaka. Widget do przebudowy (np. czas
  oczekiwania zamiast „zdążysz”).
- [ ] Reguła empiryczna (rozdz. 4): na danych ankiety widget nigdy nie
  pokazuje stanu „słaba zgodność” (wszystkie zmienne mieszczą się w progu).

### 02 — rozkłady prawdopodobieństwa

- [ ] Dystrybuanta (rozdz. 4, sekcja `ch4-dystrybuanta`): dodać wersję
  skrótową — wzór i wykres. Hasło „dystrybuanta” dopisać do
  `R/glossary.R`.
- [ ] Punkt równowagi (rozdz. 2): prawdopodobieństwa są normalizowane
  dopiero przy odchyleniu sumy od 1 o ponad 0,05, więc przy sumie np. 1,04
  E(X) na wykresie i w obliczeniu jest lekko błędne.
- [ ] Widget krokowy (rozdz. 4): wygładzona krzywa wychodzi poza dziedzinę
  dla rozkładu wykładniczego i jednostajnego.

### 03 — przedziały ufności

- [ ] Ryc. 1.1: zmiana suwaka n nie czyści historii estymat (czyści ją tylko
  zmiana rozkładu).
- [ ] Case studies w rozdz. 3: ostrzeżenia ggplot („Removed rows…”
  w `geom_point`, przestarzały `geom_errorbarh`).

### 04 — wnioskowanie statystyczne

- [ ] Post hoc w ANOVA (rozdz. 09) i porównanie χ²/Fisher (rozdz. 07) czytają
  dane przez `isolate()`: po nowym losowaniu pokazują wyniki dla starych
  danych, dopóki ktoś nie kliknie przycisku ponownie.
- [ ] Ryc. 3.2: statbox „Błąd I” pokazuje α z panelu mocy, a p-wartość jest
  tylko w podpisie; suwak n zmienia tylko symulację pod H₀, obserwowana
  różnica zawsze pochodzi z n = 40.
- [ ] Ryc. 3.3 (`wsRenderPValueChart` w `app.R`): zacieniowane pole p-wartości
  nazywa się w kodzie „Obszar odrzucenia”; nieużywany parametr `alpha`;
  oś x bez podpisu.
- [ ] Ryc. 5.3: z liczone bez poprawki na ciągłość, p-wartość obok
  z poprawką (`prop.test(correct = TRUE)`).
- [ ] Ryc. 6.x: PNG `anscombe-quartet.png` i `correlation-nonlinear.png` mają
  kropkę dziesiętną i nie mają skryptu generującego.
- [ ] Ryc. 10.5: η² z próby (seed 202) wyraźnie mniejsze niż η² populacji
  w tabeli; kolumna „x̄” pokazuje średnie populacji.
- [ ] `helpers.R`: `step_null_plot` — etykiety „H0”, „Ha” bez indeksów.

### 05 — założenia testów

- [ ] Ryc. 1.2 czyta dane przez `isolate()`: po „Generuj dane” wynik testu
  dotyczy starych danych, dopóki nie kliknie się testu ponownie.
- [ ] Ryc. 1.3: dwa wykresy Q-Q (surowe dane, logarytm) bez podpisów —
  rozróżnia je tylko kolor.
- [ ] Ryc. 2.3: jedno n dla obu grup — przy równych n test Studenta i Welcha
  dają identyczne t, więc panel nie pokazuje, kiedy Student zawodzi
  (osobne suwaki n₁, n₂).
- [ ] Ryc. 2.x: iloraz wariancji kolorowany ukrytym progiem 4.
- [ ] Symulacja χ² a Fisher (rozdz. 03): tylko kategorie 50/50 (nie pokazuje
  liberalnego χ² przy rzadkich kategoriach); konserwatywny Fisher
  kolorowany jak „porażka”.

### 06 — regresja

- [ ] Rozdział 00 „Mapa wykładu” jest pisany do prowadzącego („wybierz cel
  zajęć”). Przepisać na wstęp dla czytelnika: nawiązanie do korelacji
  (wykład 04) i założeń na resztach (05), jak czytać rdzeń i pogłębienia,
  dlaczego CASchools i pingwiny. Tabela tematów zostaje.
- [ ] Ryc. 4.1 i 4.2 prawie się dublują (ten sam generator i modele; 4.2 ma
  suwak n). W arenie (4.2) RMSE liczone na danych uczących, więc
  wyróżnienie „najlepszej” wartości zawsze trafia w największy model.
- [ ] Ryc. 1.2: scenariusz „Ten sam trend, mała próba” ma też większy szum
  (σ = 5 zamiast 3) — etykieta sugeruje, że różni się tylko n.
- [ ] Ryc. 1.4: w tabeli surowe nazwy zmiennych („income”); na liście X
  zmienna 0/1 „grades” w rozdziale o regresji prostej.
- [ ] Ryc. 1.1b, krok 5: równanie sklejane jako „b₀ + b₁X” — przy ujemnym b₁
  dałoby „+ -”.
- [ ] Panel współliniowości (rozdz. 03): pokazuje tylko chmurę X₁–X₂,
  niestabilności β nie widać bez wielokrotnego losowania; w tabeli `x1`/`x2`,
  na wykresie X₁/X₂.
- [ ] Rozdz. 03B: nagłówek „p-value” w tabeli modelu z interakcją → „p”.
- [ ] Rozdz. 05: widget liniowa a logistyczna pokazuje identyczną dokładność
  obu modeli (różnicę niesie tylko „poza [0, 1]”); `ch5_model_summary`
  liczy nieużywane `coefs`.
- [ ] Ściąga: k w dwóch znaczeniach (liczba predyktorów w R² skorygowanym,
  liczba parametrów w AIC/BIC) — ujednolicić z rozdz. 04 (k = liczba
  predyktorów, kara 2(k + 2)).
- [ ] Quiz interpretacji b₁ w jednostkach w `ch1_liniowa.R`, sekcja
  `ch1-caschool`: „read ~ income”, b₁ = 1,88 — co znaczy wzrost dochodu
  o 1 tys. USD? Dystraktory: mylone jednostki i skale.
- [ ] Rozważyć widget obserwacji wpływowych w `ch2_jakosc.R`: scatter
  z wyróżnioną odległością Cooka i opcją „usuń i przelicz”.
- [ ] Rozważyć callout w ch2 lub ch4: kwartet Anscombe'a dla regresji (różne
  wzorce reszt przy tym samym R²) albo spurious regression; resztę pułapek
  odesłać do wykładu o korelacji.
- [ ] Rozważyć mini-widget regresji do średniej w `ch1_liniowa.R`: suwak `r`,
  na wykresie główna oś elipsy i linia regresji, na żywo
  `b = r × (sd_y / sd_x)`; przykład „x = +2 SD → oczekiwane y = 2r SD”.
  Odniesienie: ryc. 6.1–6.3 w `04-wnioskowanie-statystyczne/modules/ch4_korelacja.R`
  i `scripts/regen_correlation_assets.R`.

### 07 — dobre dane

- [ ] Ryc. 3.x (moc, `ch3_grupa.R`): punkt bieżącego n przeskakuje na siatkę
  co 5 (przy n = 8 rysowany przy n = 5, moc ~2% zamiast ~9%); przy
  nieparzystym n grupy dzielą się nierówno (`rnorm(n/2)`); symulacja
  (40 × 500 testów) przelicza się przy każdym ruchu suwaka.
- [ ] Kawiarnia: wykres sprzedaży dzień po dniu (`tab10_lineplot`) usuwa
  braki przed rysowaniem, więc linia łączy dni niesąsiadujące.
- [ ] Panele bez tytułu (sama etykieta „Ryc. …”): Ryc. 5.1 (`ch5_tarantino.R`),
  Ryc. 6.1–6.7 (`ch6_hotel.R`), Ryc. 7.1 (`ch7_wynagrodzenia.R`), ćwiczenie
  z klasyfikacją rekordów w rozdz. 9 (`ch9_laboratorium.R`, panel „Ćwiczenie”).
  Rozdz. 12: dwie tabele (podsumowanie 10 zbiorów, dopasowanie analizy) w ręcznym
  `div(class = "lc-figure-panel")` bez etykiety i tytułu — przy dopisaniu tytułu
  zamienić na `figure_panel()`.
- [ ] Martwy kod w `ch1_katalog.R` (wykresy problemów 2–3: `pct_45`, `r2`,
  `title_txt`).

### 08 — case studies

- [ ] Panele Ryc. 1.6 i 1.7 są puste do kliknięcia przycisku.
- [ ] Wynik ANOVA w kroku 3 ma na sztywno „p < 0.001” zamiast liczonej
  p-wartości; `geom_errorbarh()` przestarzały w ggplot2 4.0.
- [ ] Komentarze w `app.R` bez polskich znaków („Kazdy rozdzial”, „MODULY”).
- [ ] Rozbudować wykład poza jedyny rozdział CASchools; dodać quizy.
  Kandydaci: `palmerpenguins` (ANOVA/korelacja), case binarny (regresja
  logistyczna), case czasowy.

### 09 — projekt badawczy

- [ ] Rozdz. 7: `geom_errorbarh()` przestarzały w ggplot2 4.0; panel modelu
  używa `lc_stat_box` zamiast `lc_readout`.
- [ ] Rozdz. 1: karta celu ma ręczną klasę `lc-feedback lc-feedback-warning
  lecture-goal-card` — zastąpić komponentem z layoutu.

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

- [ ] **Decyzja:** ćwiczenie 2 — dotychczasowy widget czy prototyp A, B lub C.
  Po wyborze usunąć pozostały kod serwera i CSS `.lc-proto-*`. Nie migrować
  prototypów do wspólnych komponentów przed wyborem.
- [ ] Ocenić interaktywny łańcuch pojęć jako treść tego wykładu (uogólnienie —
  patrz sekcja globalna).
- [ ] Stara tabela `lc-table` w `modules/block.R` (ok. l. 1406) → `lc_table()`
  (nie wylewała się w audycie; etap 3 migracji).
- [ ] Intro quizu (`jezyk_quiz`, l. 4–7): „definicji klasycznej (1.2) oraz
  działań na zdarzeniach (1.5)” → „definicji klasycznej (wzór 1.2) oraz reguły
  sumy (wzór 1.5)”. Numer w nawiasie myli się z numerem definicji, a pytanie 4
  dotyczy reguły sumy.

### 02 — warunki

- [ ] Stara tabela „Wniosek / Czy wynika z danych? / Co dalej?” w owijce
  `lc-table-wrap` (`modules/block.R`, ok. l. 312) → `lc_table()` (etap 3).

### 03 — alarm i prawda

- [ ] Tablica 2×2 (`a3_table`) po migracji ma polskie nagłówki (Stan, Alarm,
  Brak alarmu, Razem), nadgłówek „Odczyt detektora” i wiersz sum; wcześniej
  surowe nazwy `state`, `alarm`, `no_alarm` i liczby typu 95.00. Obejrzeć
  i zatwierdzić.

Odsyłacze do rozdziałów opisem zamiast tytułem (tytuły są tagami pojęć, więc
poprawiamy odsyłacze):

- [ ] l. 153: „w rozdziale o naturalnych częstościach” → „w rozdziale
  „Wzór Bayesa””.
- [ ] l. 487: „w rozdziale o języku detektora” → „w rozdziale „Czułość
  i swoistość””.

### 04 — wiele prób

- [ ] `p4_chk_zalozenia` (ok. l. 248) — ustalono 2.10.2026: reguła
  „po wykryciu sprawdzam dokładniej” łamie **niezależność** (zmianę wywołuje
  wynik wcześniejszej próby; p przy okazji przestaje być stałe). Zmienić
  poprawną odpowiedź i wyjaśnienie oraz l. 242 („zmienia samą definicję
  próby”). Reguła do zachowania w całym kursie: zmiana wywołana historią
  wyników łamie niezależność, zmiana z przyczyn zewnętrznych (dostawy, dryf)
  łamie stałość p.
- [ ] l. 537: „to wzór (4.6) z k = 0 zapisany od drugiej strony” → „to warunek
  P(X ≥ 1) = 0,95 ze wzoru (4.6) zapisany od drugiej strony” (wzór 4.6 nie
  ma k).
- [ ] Model hipergeometryczny — ustalono 2.10.2026: jedno zdanie w sekcji
  `bernoulli/zalozenia`: losowanie bez zwracania dużej części małej partii to
  model hipergeometryczny, dwumianowy jest jego przybliżeniem, gdy próbka jest
  mała względem partii. Przewodnik i lista ćwiczeń (4.a) już tego wymagają,
  wykład dotąd o tym nie wspomina.
- [ ] l. 397: liczby z symulacji (0–7, 156, 67, 2,03) dotyczą n = 100,
  p = 0,02 i ziarna 2404, a histogram bierze n i p z suwaków poprzedniego
  rozdziału — ustalono 2.10.2026: dopisać jawnie „przy n = 100, p = 0,02”
  zamiast samego „przy domyślnych ustawieniach”.

### 05 — ile prób do zdarzenia

- [ ] l. 227: „w rozdziale o tym, kiedy model zawodzi” → „w rozdziale
  „Założenia modelu””.
- [ ] Sekcja `rte/parametryzacje`: dopisać zdanie o konwencji „+1” dla
  rozkładu geometrycznego — `dgeom`/`pgeom`/`qgeom` liczą porażki przed
  pierwszym sukcesem, dlatego w kodzie kursu dodaje się 1 (serwer już robi
  `rgeom() + 1`, `qgeom() + 1`, l. 449, 457; wykład nazywa tylko „+r”).
- [ ] l. 388: etykieta suwaka „Odchylenie p przed ograniczeniem do
  [0,005; 0,95]” → „Zmienność jakości partii (odchylenie p)”; informację
  o obcięciu p przenieść do notki pod widgetem.

### 06 — zmienność i próg

- [ ] l. 184: Φ pojawia się przed definicją (l. 268–270) → „dokładnie
  P(79 < T ≤ 85) ≈ 0,683”, bez Φ.
- [ ] l. 473: „rachunki z rozdziałów 3–5 korzystały z funkcji Φ” → „z
  rozdziałów 2–5” (przykład 6.3 w rozdziale 2 też używa Φ).
- [ ] Rozdział `nienormalny` (Q–Q) — ustalono 2.10.2026: zostaje
  rozszerzeniem (`extension = TRUE`); ćwiczenie 2 i pytanie `z6_chk_qq`
  oznaczyć etykietą „rozszerzenie”. Tak samo zadania z Q–Q w
  `~/praca/dydaktyka/materialy/analiza-ryzyka/cwiczenia-listy-zadan.md`
  (7.2 część Q–Q, 7.d).

Uwaga: powtórzone `id = "most"` w różnych rozdziałach nie jest błędem —
kotwice sekcji to `blok-rozdział-sekcja` (`R/risk_block.R`, l. 420).

### 07 — czas życia

- [ ] **Decyzja:** wzór (7.2) λ̂ = d/Σtᵢ (`modules/block.R`, ok. l. 162)
  kłóci się z zapowiedzią „bez estymacji parametrów”, ale pokazuje użycie
  obserwacji cenzorowanych. Warianty: zostaje / przenieść do
  `risk_derivation()` / usunąć i przenumerować (7.3)–(7.17). Rozstrzygnąć
  przed zatwierdzeniem treści wykładu.

- [ ] l. 520 i 523: instrukcja eksperymentu z Weibullem każe ustawić η = 1500 h,
  a odczyt podaje wartości dla η = 1700 h (przy 1500 h hazard w 100 h to ok.
  0,0013, nie 0,0012). Ujednolicić instrukcję albo przeliczyć odczyt.
- [ ] l. 456: usunąć z tekstu dla studentów notatkę autorską „To krótki kontekst
  dla gamma, nie dodatkowy rozbudowany dział.”
- [ ] l. 417: ujednolicić oznaczenie liczby zdarzeń („r-te wykrycie” obok
  „suma k oczekiwań geometrycznych”).
- [ ] **Decyzja:** kolejność definicji — wzór (7.1) i przykład 7.1 używają f(t)
  i R(t) przed definicją 7.4, a rozdział o cenzorowaniu opiera się na modelu
  wykładniczym z rozdziału 4. Warianty: zostaje z jawnym odesłaniem w przód /
  przestawić rozdziały (`jezyk` przed `mttf` i `cenzorowanie`). Powiązane
  z decyzją o wzorze (7.2).
- [ ] **Decyzja:** proces i rozkład Poissona pojawiają się w l. 428–456 bez
  definicji (nigdzie w kursie). Warianty: jedno zdanie definicji w sekcji
  `gamma` / zostaje jako „most” bez definicji.

### 08 — niezawodność systemu

Plik: `modules/block.R`.

- [ ] **Decyzja:** widget czasu (ok. l. 679, przykład 8.6) używa MTTF
  1800 / 2000 / 2500 h spoza danych Bananpolu — zostaje czy ujednolicić?
- [ ] **Decyzja:** sterownik C ma R = 0,98, tyle co zasilanie Bananpolu —
  zostaje czy zmienić (np. 0,97) i przeliczyć przykłady 8.5, 8.7, 8.11
  (ok. l. 370, 562)?
- [ ] Zweryfikować komunikację fikcyjnego progu 14,5 °C w definicji sukcesu.

- [ ] l. 489: pułapka mówi o „suwaku korelacji”, którego w wykładzie nie ma
  (jest suwak P(utraty wspólnego zasilania), `s8_common`) — przeredagować.
- [ ] l. 495: „pierwsza dodatkowa gałąź redukuje ryzyko … z 0,01 do 0,001” —
  to spadek po drugiej dodatkowej gałęzi (przy r = 0,9: 0,1 → 0,01 → 0,001);
  poprawić i podać r.
- [ ] l. 590: intro ściągi opisuje quiz i ćwiczenia niezgodnie z zawartością
  (quiz obejmuje też koherentność i rezerwę oczekującą; ćwiczenia zaczynają
  się od „Struktury” i kończą na „Beta-factor” i „Ile gałęzi”).
- [ ] l. 361: „porządek, który za wykład wróci w drzewach błędów” — poprawić
  szyk.
- [ ] **Decyzja:** rezerwa oczekująca jest wprowadzona jednym zdaniem (l. 242),
  a jest przedmiotem pytania quizu 5. Warianty: dopisać krótką definicję
  (stan w oczekiwaniu, przełącznik) / zostaje jako wzmianka.
- [ ] **Decyzja:** układ k-z-n nie występuje w wykładzie, a używa go zadanie
  9.5 (2-z-3) w `materialy/analiza-ryzyka/cwiczenia-listy-zadan.md`.
  Warianty: dodać krótką sekcję w wykładzie / usunąć z listy ćwiczeń /
  zostaje jako zadanie dodatkowe.
- [ ] **Decyzja:** tytuł rozdziału `redundancja` („Istotność Birnbauma”) nie
  obejmuje dwóch pierwszych sekcji (malejąca korzyść redundancji, kopie
  zapasowe). Warianty: zostaje (tytuł = tag pojęcia) / „Redundancja
  i istotność”.

### 09 — drzewo błędów

- [ ] **Decyzja:** ranking potencjalnej redukcji (`f9_rank_plot`, ok. l. 607)
  liczy na stałych bazowych (0,005; 0,05; 0,08), więc suwak zmienia tylko
  skalę, nie kolejność. Warianty: zostaje / podpiąć suwaki z rozdziału 3 /
  pokazać redukcję względną lub istotność krytyczną obok Birnbauma.

- [ ] **Decyzja (merytoryczna):** l. 73 mówi „detekcja ma osobne zasilanie”,
  a l. 158, 403, 432 traktują utratę zasilania jako wspólną przyczynę
  wyłączającą detekcję i tłumienie (także checkbox w l. 157). Warianty:
  detekcja ma osobne zasilanie, a przykład wspólnej przyczyny dotyczy innego
  zasobu / detekcja i tłumienie dzielą zasilanie (zmienić l. 73 i opis
  danych).
- [ ] l. 124: „wrócimy w ostatnim rozdziale” → w rozdziale „Granice drzewa
  błędów” (przedostatni).
- [ ] Ujednolicić „P(top)” / „Top event” → „P(TOP)” / „zdarzenie szczytowe”
  (l. 66, 254, 444, 446, 546, 593, ćw. 2, etykiety widgetów).
- [ ] l. 588: „i 2 wejść” → poprawna odmiana dla n = 2–4.
- [ ] Kolejność: definicja 9.2 (l. 120) używa bramki przed definicją 9.3
  (l. 148); wzór (9.10) wprowadza I_CR przed definicją 9.5 — przestawić albo
  dodać odesłanie.

### 10 — od modelu do decyzji

- [ ] **Decyzja:** horyzont roczny (sekcja `id = "rok"`, ok. l. 393,
  wzór 10.9). Obecnie wynik główny to jedna misja, a
  P_rok = 1 − (1 − P(TOP))³ ≈ 0,005 jest rozszerzeniem; 1 − R_sys³ ≈ 0,641
  pokazano jako pułapkę. Warianty: zostaje / horyzont roczny jako wynik główny
  (zmiana serwera i `risk_mission_analysis()`).
- [ ] Wzór (10.11, l. 504) nie zawiera skalowania prawdopodobieństwa
  przeoczenia, które opisuje tekst (l. 501) i liczy kod (`R/risk_math.R`,
  l. 244) — uzupełnić wzór.
- [ ] l. 320: „Równość z wynikiem b) jest przypadkową cechą β = 2” — dla β = 2
  równość R(2000)/R(1000) = R(1000)³ zachodzi dla każdego η; przypadkowa jest
  tylko bliskość do połowy. Przeredagować.
- [ ] l. 91 i 88: odsyłacze do „ramki z danymi” — skuteczność 50% jako
  hipoteza jest opisana dopiero we wstępie rozdziału `interwencje` (l. 459).
- [ ] l. 421: odsyłacz do „pierwszego pytania quizu” nie pasuje (quiz 1 dotyczy
  awarii a niedostępności, nie 1 − R_sys³).
- [ ] l. 287 i l. 398: ta sama różnica modeli opisana jako „prawie dwukrotnie”
  i „o około 40%” — ujednolicić ujęcie.
- [ ] l. 526: „wygrywa aż do u = 0,45” jest na granicy (0,00155 wobec 0,00156) —
  rozważyć „do około u = 0,45”.
- [ ] Ujednolicić „P(top)” → „P(TOP)” w ćwiczeniach 1 i 3; „blok 07” →
  „wykład 07” (l. 73).
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
    w `~/praca/dydaktyka/materialy/analiza-ryzyka/`; klucze 11.x liczyć z tych
    samych zaokrąglonych parametrów co wykład.
  - Rozważony i odrzucony głębszy wariant (D z przeoczeń w `bananpol_alarmy`,
    S = 1 − R linii z `bananpol_system`): miesza jednostki zmiana/półrocze.
