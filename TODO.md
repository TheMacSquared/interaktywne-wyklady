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

W statystyce i analizie ryzyka został tylko jeden wykres z kliknięciem
na `zoom_plot_ui` (statystyka 06, ćwiczenie z prostą).

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
- [ ] Wdrożyć `gloss()` we wszystkich wykładach: owijać pierwsze
  wprowadzenie kluczowego terminu w rozdziale, nie każde wystąpienie. Nowe
  hasła dopisywać do `statystyka/R/glossary.R`. Wzorzec:
  `03-przedzialy-ufnosci/modules/ch1_estymacja.R`.

### 01 — typy danych


### 02 — rozkłady prawdopodobieństwa

- [ ] Dystrybuanta (rozdz. 4, sekcja `ch4-dystrybuanta`): dodać wersję
  skrótową — wzór i wykres. Hasło „dystrybuanta” dopisać do
  `R/glossary.R`.
- [ ] Widget krokowy (rozdz. 4): wygładzona krzywa wychodzi poza dziedzinę
  dla rozkładu wykładniczego i jednostajnego.

### 06 — regresja

- [ ] Panel współliniowości (rozdz. 03): pokazuje tylko chmurę X₁–X₂,
  niestabilności β nie widać bez wielokrotnego losowania.
- [ ] Rozdz. 05: widget liniowa a logistyczna pokazuje identyczną dokładność
  obu modeli (różnicę niesie tylko „poza [0, 1]”).
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

### 08 — case studies

- [ ] Rozbudować wykład poza jedyny rozdział CASchools; dodać quizy.
  Kandydaci: `palmerpenguins` (ANOVA/korelacja), case binarny (regresja
  logistyczna), case czasowy.

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
    w `~/praca/dydaktyka/materialy/analiza-ryzyka/`; klucze 11.x liczyć z tych
    samych zaokrąglonych parametrów co wykład.
  - Rozważony i odrzucony głębszy wariant (D z przeoczeń w `bananpol_alarmy`,
    S = 1 − R linii z `bananpol_system`): miesza jednostki zmiana/półrocze.
