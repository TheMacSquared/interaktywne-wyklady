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

Kolejność najbliższych prac:

1. [ ] Statystyka 01 — dopracować tabelę częstości (szczegóły niżej).
2. [ ] Przetestować ją w macierzy szerokości: pełne okno, połowa okna,
   szerokości pośrednie, telefon, powiększony tekst.
3. [ ] Tą samą macierzą sprawdzić analizę ryzyka 07, rozdziały 2 i 4.
4. [ ] Po obu pilotażach zatwierdzić wspólny wzorzec, zrobić inwentaryzację
   widgetów i rozpocząć migrację pozostałych wykładów małymi commitami.

---

## Globalne

### Responsywne panele, tabele i widgety

Stan:

- `figure_panel(width_mode = ...)`: `compact` (do treści, maks. 680 px),
  `text` (do 680 px), `wide` (do 980 px). Istniejące wywołania zachowują
  dotychczasowe zachowanie.
- `lc_table_region()` ogranicza przewijanie do tabeli, `lc_controls_row()`
  reorganizuje sterowanie, `lc_widget_layout()` stawia sterowanie nad
  wykresem lub obok, zależnie od szerokości kontenera.
- Pilotaże: statystyka 01 rozdz. 2 (tabela krokowa), statystyka 02 rozdz. 2
  („Dane a model”), analiza ryzyka 07 rozdz. 2 i 4.
- Testy komponentów i skany kontraktu przechodzą w obu kursach; 10 aplikacji
  analizy ryzyka przechodzi kontrolę ładowania.

Zadania:

- [ ] Powtórzyć pełną kontrolę ładowania statystyki na stacjonarnym
  (wcześniej przerwana limitem czasu).
- [ ] Po pilotażach: inwentaryzacja widgetów w obu kursach i plan migracji.

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

- [ ] **Decyzja:** czy role tekstu `.life-*` z analizy ryzyka 07 mają być
  wzorcem dla innych wykładów.
- [ ] **Decyzja:** czy interaktywny łańcuch pojęć z analizy ryzyka 01 ma być
  wspólnym komponentem.

---

## Statystyka

### Cały kurs

- [ ] Wdrożyć `gloss()` we wszystkich wykładach: owijać pierwsze
  wprowadzenie kluczowego terminu w rozdziale, nie każde wystąpienie. Nowe
  hasła dopisywać do `statystyka/R/glossary.R`. Wzorzec:
  `03-przedzialy-ufnosci/modules/ch1_estymacja.R`.

### 01 — typy danych: tabela częstości

Kontekst: tabela ma minimum 600 px, panel 680 px daje ok. 622 px treści;
w kroku 4 sześć kolumn z długimi nagłówkami wymusza przewijanie.

- [ ] Skrócić nagłówki do `n`, `f`, `%`, `N skum.`, `% skum.` i dodać
  widoczne objaśnienie oznaczeń.
- [ ] Liczebności jako liczby całkowite, procenty bez zbędnych zer
  końcowych; kategorie wyrównane do lewej.
- [ ] Na wąskim kontenerze porównać jedną tabelę z dwiema (zwykłe
  i skumulowane, kategoria powtórzona w obu).
- [ ] W pierwszych krokach (dwie kolumny) nie wymuszać minimum 600 px.
- [ ] Sprawdzić wszystkie kroki dla zmiennej nominalnej i porządkowej,
  zwłaszcza długie nazwy kategorii; nie zmieniać obliczeń.

Pliki: `01-typy-danych/modules/ch2_jakosciowe.R`, `R/lecture_layout.R`,
`R/shared_styles.css`, `R/DESIGN_CONTRACT.md`.

### 04 — wnioskowanie statystyczne

- [ ] Ograniczyć `tags$strong()` / `tags$b()` w `modules/ch1_logika.R`
  (ok. 37 wystąpień) do etykiet, werdyktów i statusów.

### 05 — założenia testów

- [ ] **Decyzja:** `ks.test()` w `modules/ch1_normalnosc.R` — zostaje jako
  wyjątek dydaktyczny czy zamiana na `rstatix`? Reszta modułu używa już
  `shapiro_test()`.

### 06 — regresja

- [ ] Przeredagować `lead` sześciu rozdziałów na pytania-hooki tam, gdzie to
  naturalne (wzorzec: 04 ch1, ch4, ch6).
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

Brak zadań.

---

## Analiza ryzyka

### 01 — język ryzyka

- [ ] **Decyzja:** ćwiczenie 2 — dotychczasowy widget czy prototyp A, B lub C.
  Po wyborze usunąć pozostały kod serwera i CSS `.lc-proto-*`. Nie migrować
  prototypów do wspólnych komponentów przed wyborem.
- [ ] Ocenić interaktywny łańcuch pojęć jako treść tego wykładu (uogólnienie —
  patrz sekcja globalna).
- [ ] Intro quizu (`jezyk_quiz`, l. 4–7): „definicji klasycznej (1.2) oraz
  działań na zdarzeniach (1.5)” → „definicji klasycznej (wzór 1.2) oraz reguły
  sumy (wzór 1.5)”. Numer w nawiasie myli się z numerem definicji, a pytanie 4
  dotyczy reguły sumy.

### 03 — alarm i prawda

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

- [ ] Sprawdzić rozdziały 2 i 4 macierzą szerokości (patrz „Teraz”).
- [ ] Jeśli `lc_table_region()` wystarcza, zastąpić nim `.life-table-scroll`.
- [ ] **Decyzja:** wzór (7.2) λ̂ = d/Σtᵢ (`modules/block.R`, ok. l. 162)
  kłóci się z zapowiedzią „bez estymacji parametrów”, ale pokazuje użycie
  obserwacji cenzorowanych. Warianty: zostaje / przenieść do
  `risk_derivation()` / usunąć i przenumerować (7.3)–(7.17). Rozstrzygnąć
  przed zatwierdzeniem treści wykładu.

### 08 — niezawodność systemu

Plik: `modules/block.R`.

- [ ] **Decyzja:** widget czasu (ok. l. 679, przykład 8.6) używa MTTF
  1800 / 2000 / 2500 h spoza danych Bananpolu — zostaje czy ujednolicić?
- [ ] **Decyzja:** sterownik C ma R = 0,98, tyle co zasilanie Bananpolu —
  zostaje czy zmienić (np. 0,97) i przeliczyć przykłady 8.5, 8.7, 8.11
  (ok. l. 370, 562)?
- [ ] Zweryfikować komunikację fikcyjnego progu 14,5 °C w definicji sukcesu.

### 09 — drzewo błędów

- [ ] **Decyzja:** ranking potencjalnej redukcji (`f9_rank_plot`, ok. l. 607)
  liczy na stałych bazowych (0,005; 0,05; 0,08), więc suwak zmienia tylko
  skalę, nie kolejność. Warianty: zostaje / podpiąć suwaki z rozdziału 3 /
  pokazać redukcję względną lub istotność krytyczną obok Birnbauma.

### 10 — od modelu do decyzji

- [ ] **Decyzja:** horyzont roczny (sekcja `id = "rok"`, ok. l. 393,
  wzór 10.9). Obecnie wynik główny to jedna misja, a
  P_rok = 1 − (1 − P(TOP))³ ≈ 0,005 jest rozszerzeniem; 1 − R_sys³ ≈ 0,641
  pokazano jako pułapkę. Warianty: zostaje / horyzont roczny jako wynik główny
  (zmiana serwera i `risk_mission_analysis()`).
