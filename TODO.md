# TODO — Interaktywne wykłady

Jedno miejsce do ustalania kolejności prac w całym repozytorium. Szczegółowe
notatki historyczne mogą pozostać w katalogach kursów, ale nowe zadania i ich
status zapisujemy tutaj.

## Teraz

1. [ ] Dopracować tabelę częstości w `statystyka/01-typy-danych`.
2. [ ] Sprawdzić pilotaże responsywności przy pełnym i połowie okna,
   szerokościach pośrednich, na telefonie oraz z powiększonym tekstem.
3. [ ] Tą samą macierzą sprawdzić pilotaże `analiza-ryzyka/07-czas-zycia`.
4. [ ] Dopiero po obu pilotażach zatwierdzić wspólny wzorzec i rozpocząć
   migrację pozostałych widgetów.

## Globalne

### Responsywne panele, tabele i widgety

Stan:

- `figure_panel(width_mode = ...)` obsługuje `compact`, `text` i `wide`.
- Dostępne są `lc_table_region()`, `lc_controls_row()` oraz
  `lc_widget_layout()`.
- Pilotaże obejmują statystykę 01 i 02 oraz analizę ryzyka 07.
- Testy komponentów i skany kontraktu przechodzą w obu kursach.
- Pełna kontrola ładowania statystyki wymaga powtórzenia na stacjonarnym;
  wcześniejsze uruchomienie przerwał limit czasu.

Zasady:

- [ ] Wspólne komponenty zmieniać równolegle w snapshotach właściwych kursów,
  zachowując różnice kursowe.
- [ ] Reagować na szerokość kontenera, nie wyłącznie viewportu.
- [ ] Nie usuwać informacji ani nie zmniejszać tekstu tylko po to, żeby
  zmieścić widget.
- [ ] Przewijanie ograniczać do tabeli i zostawiać jako zabezpieczenie.
- [ ] Dla każdego widgetu osobno określać szerokość panelu oraz reorganizację
  sterowania, wykresów, tabel i podsumowań.
- [ ] Nie wprowadzać globalnego `fit-content` dla wykresów o szerokości 100%.
- [ ] Każdy commit ograniczać do wspólnej infrastruktury, migracji jednego
  widgetu albo zmiany treści konkretnego rozdziału.

### Granice pilotaży

- Prototypy A/B/C i łańcuch pojęć w analizie ryzyka 01 są lokalnymi
  eksperymentami dydaktycznymi.
- Klasy `.life-*` z analizy ryzyka 07 są lokalnym pilotażem ról tekstu.
- Żaden z tych wzorców nie staje się częścią systemu bez osobnej decyzji,
  dokumentacji i testów.

## Statystyka

### 01 — tabela częstości

- [ ] Skrócić nagłówki do `n`, `f`, `%`, `N skum.`, `% skum.` i dodać
  widoczne objaśnienie oznaczeń.
- [ ] Liczebności pokazywać jako liczby całkowite, a procenty bez zbędnych
  końcowych zer; kategorie wyrównać do lewej.
- [ ] Porównać jedną tabelę z dwiema tabelami na wąskim kontenerze.
- [ ] Dla pierwszych kroków z dwiema kolumnami nie wymuszać minimum 600 px.
- [ ] Sprawdzić wszystkie kroki dla zmiennej nominalnej i porządkowej,
  zwłaszcza długie nazwy kategorii; nie zmieniać obliczeń.

Pliki: `statystyka/01-typy-danych/modules/ch2_jakosciowe.R`,
`statystyka/R/lecture_layout.R`, `statystyka/R/shared_styles.css`,
`statystyka/R/DESIGN_CONTRACT.md`.

### Słownik `gloss()`

- [ ] W kolejnych rozdziałach owijać `gloss()` pierwsze wprowadzenie
  kluczowego terminu, a nie każde wystąpienie.
- [ ] Nowe hasła dopisywać do `statystyka/R/glossary.R`.

Wzorzec: `statystyka/03-przedzialy-ufnosci/modules/ch1_estymacja.R`.

### 06 — regresja

- [ ] Rozważyć przykład obserwacji wpływowych z odległością Cooka.
- [ ] Przeredagować leady sześciu rozdziałów na pytania-hooki tam, gdzie jest
  to naturalne.
- [ ] Rozważyć krótki callout o kwartecie Anscombe'a lub spurious regression.
- [ ] Dodać quiz interpretacji b₁ w jednostkach na przykładzie CASchools.
- [ ] Rozważyć mini-widget pokazujący regresję do średniej:
  `b = r × (sd_y / sd_x)`.

### Jakość kodu i treści

- [ ] Ograniczyć nadmierne `tags$strong()` / `tags$b()` w
  `statystyka/04-wnioskowanie-statystyczne/modules/ch1_logika.R`.
- [ ] Rozstrzygnąć, czy użycie `ks.test()` w
  `statystyka/05-zalozenia-testow/modules/ch1_normalnosc.R` jest uzasadnionym
  wyjątkiem od preferencji dla `rstatix`.

### 08 — case studies

- [ ] Rozbudować wykład poza pojedynczy rozdział CASchools.
- [ ] Rozważyć case z `palmerpenguins`, analizę binarną oraz case czasowy.

## Statystyka 2

Brak wpisanych zadań. Nowe zadania dla tego kursu dodajemy w tej sekcji.

## Analiza ryzyka

### 01 — język ryzyka

- [ ] Porównać dotychczasowy widget ćwiczenia 2 z prototypami A/B/C.
- [ ] Wybrać jeden wariant i usunąć pozostały kod serwera oraz CSS
  `.lc-proto-*`.
- [ ] Osobno ocenić interaktywny łańcuch pojęć; ewentualne uogólnienie
  potraktować jako nowe zadanie systemowe.

### 04 — wiele prób

- [ ] Rozstrzygnąć, które założenie łamie reguła „po wykryciu sprawdzam
  dokładniej”: stałość p, niezależność czy definicję próby.

### 07 — czas życia

- [ ] Tą samą macierzą szerokości co w statystyce 01 sprawdzić rozdziały 2 i 4.
- [ ] Jeśli wspólny komponent wystarcza, zastąpić `.life-table-scroll` przez
  `lc_table_region()`.
- [ ] Oddzielnie zdecydować, czy lokalne role tekstu `.life-*` pozostają
  lokalne, czy zasługują na uogólnienie.
- [ ] Rozstrzygnąć los wzoru λ̂ = d/Σtᵢ: pozostawić, przenieść do
  `risk_derivation()` albo usunąć i poprawić numerację.

### 08 — niezawodność systemu

- [ ] Rozstrzygnąć, czy ujednolicić MTTF widgetu z danymi Bananpolu.
- [ ] Rozważyć zmianę R_C z 0,98 na niekolidującą wartość i przeliczyć
  przykłady 8.5, 8.7 i 8.11.
- [ ] Zweryfikować komunikację fikcyjnego progu 14,5 °C.

### 09 — drzewo błędów

- [ ] Zdecydować o losie rankingu potencjalnej redukcji: pozostawić, podpiąć
  parametry z rozdziału 3 albo pokazać inną miarę obok Birnbauma.

### 10 — od modelu do decyzji

- [ ] Zdecydować, czy horyzont roczny pozostaje rozszerzeniem, czy staje się
  wynikiem głównym; druga opcja wymaga zmiany serwera i
  `risk_mission_analysis()`.

## Analiza ryzyka — szczegóły historyczne

Pełne opisy otwartych decyzji po przeróbce wykładów na skrypt pozostają w
`analiza-ryzyka/docs/TODO-skrypt.md` do czasu ich rozstrzygnięcia.

## Poza aktywnym zakresem

`deprecated/ekonometria/` jest archiwum i nie wchodzi do aktywnego backlogu.
