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

### 04 — wiele prób

- [ ] **Decyzja:** które założenie łamie reguła „po wykryciu sprawdzam
  dokładniej” (`modules/block.R`, `p4_chk_zalozenia`, ok. l. 248)?
  Obecnie poprawna odpowiedź to „stałość p”, wyjaśnienie wspomina zależność
  od historii serii, a starszy tekst mówi o „zmianie definicji próby”.
  Warianty: stałość p / niezależność / definicja próby (wtedy przeredagować
  pytanie i wyjaśnienie).

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
