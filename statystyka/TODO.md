# Archiwum TODO — statystyka

Aktualny, nadrzędny backlog projektu znajduje się w [`../TODO.md`](../TODO.md).
Ten plik zachowuje szczegółowe notatki i kontekst historyczny; nowych zadań
nie należy już dopisywać tutaj.

Lista rzeczy zauważonych przy okazji innej pracy, które warto kiedyś zrobić,
ale nie blokują obecnego zadania. Posortowane luźno wg modułu.

---

## Mapa bieżącej pracy — kolejność i granice zakresu (2026-10-02)

Równolegle trwają zmiany o różnym zasięgu. Żeby nie zatwierdzać lokalnego
eksperymentu jako reguły dla całego repozytorium, prace dzielimy na cztery
niezależne strumienie:

1. **System layoutu (oba kursy).** Stabilizujemy API `width_mode`,
   `lc_table_region()`, `lc_controls_row()` i `lc_widget_layout()`. Zmiany we
   wspólnych komponentach wprowadzamy równolegle do snapshotów
   `statystyka/R/` i `analiza-ryzyka/R/`, z zachowaniem różnic kursowych.
2. **Pilotaże responsywności.** Najpierw dopracowujemy tabelę krokową w
   statystyce 01, potem sprawdzamy na tych samych szerokościach widgety
   analizy ryzyka 07 (rozdziały 2 i 4). Dopiero po akceptacji obu typów treści
   rozpoczynamy migrację pozostałych wykładów.
3. **Eksperymenty konkretnego wykładu.** Prototypy A/B/C i schemat łańcucha
   pojęć w analizie ryzyka 01 oraz lokalne komponenty `.life-*` w wykładzie 07
   nie są automatycznie częścią systemu. Najpierw wymagają decyzji
   dydaktycznej i usunięcia niewybranych wariantów.
4. **Backlog treści.** Decyzje merytoryczne dla analizy ryzyka 04, 07, 08, 09
   i 10 oraz pozostałe zadania statystyki realizujemy osobno od migracji
   layoutu, chyba że dana decyzja bezpośrednio zmienia testowany widget.

Kolejność najbliższych prac:

- [ ] Zamknąć tabelę częstości w statystyce 01 według listy poniżej.
- [ ] Przetestować ją przy szerokości pełnej, połowie okna, wartościach
  pośrednich, telefonie i powiększonym tekście.
- [ ] Tą samą macierzą sprawdzić dwa pilotaże analizy ryzyka 07; zastąpić
  lokalne `.life-table-scroll` przez `lc_table_region()`, jeśli wspólny
  komponent pokrywa potrzeby tabel.
- [ ] Po testach zdecydować, które lokalne wzorce z wykładu 07 są warte
  uogólnienia. Uogólniać osobnym commitem wraz z dokumentacją i testami.
- [ ] Dopiero wtedy przygotować inwentaryzację i serię małych migracji
  pozostałych widgetów w obu kursach.

Każdy commit powinien należeć do jednego poziomu: infrastruktura wspólna,
migracja jednego widgetu albo zmiana treści konkretnego rozdziału. Wyjątkiem
jest minimalny test/pilotaż konieczny do zweryfikowania nowego komponentu.

---

## Responsywne widgety i tabele — kontynuacja pilotażu (2026-10-02)

Zakres: statystyka i analiza ryzyka. Wspólne komponenty mają uwzględniać
szerokość dostępnego kontenera, także przy połowie okna desktopowego,
otwartym sidebarze, powiększeniu przeglądarki i zmianie rozmiaru tekstu.
Układ mobilny i desktopowy wymagają projektowania treści, nie tylko CSS.
Nie usuwać informacji ani zmniejszać tekstu wyłącznie po to, żeby zmieścić widget.

### Stan wdrożenia

- Dostępne tryby `figure_panel(width_mode = ...)`: `compact` (dopasowanie do
  treści, maks. 680 px), `text` (do 680 px), `wide` (do 980 px).
  Istniejące wywołania zachowują poprzednie zachowanie; migracja jest stopniowa.
- `lc_table_region()` ogranicza przewijanie do tabeli; `lc_controls_row()`
  reorganizuje sterowanie; `lc_widget_layout()` obsługuje układ nad wykresem
  lub obok niego, zależnie od szerokości kontenera.
- Pilotaże: statystyka 01, rozdział 2 (tabela krokowa); statystyka 02,
  rozdział 2 („Dane a model”); analiza ryzyka 07, rozdziały 2 i 4.
- Testy nowych komponentów i kontraktu designu przeszły w obu kursach.
  Wszystkie 10 aplikacji analizy ryzyka przeszły kontrolę ładowania.
  Pełne testy ładowania statystyki przerywały limity czasu — wynik pozostaje
  niepełny i wymaga ponownego sprawdzenia na stacjonarnym.

### Następny krok: tabela częstości w statystyce 01

Obecne minimum tabeli to 600 px, a panel 680 px daje około 622 px treści.
W kroku 4 sześć kolumn z długimi nagłówkami szybko wymusza przewijanie.
To zabezpiecza dostęp do danych, ale nie zapewnia wygodnego porównywania.

- [ ] Skrócić nagłówki: `n`, `f`, `%`, `N skum.`, `% skum.`, z widocznym
  objaśnieniem oznaczeń. Zachować znaczenie i wszystkie dane.
- [ ] Liczebności pokazywać jako liczby całkowite, procenty bez zbędnych
  końcowych zer; nazwy kategorii wyrównać do lewej.
- [ ] Na węższym kontenerze rozważyć dwie tabele: częstości zwykłe
  i skumulowane, z kategorią powtórzoną w obu. Porównać czytelność
  z jedną tabelą przed przyjęciem tego jako wzorca.
- [ ] Dla pierwszych kroków (dwie kolumny) nie wymuszać minimum 600 px.
- [ ] Ustalać zmianę układu względem szerokości komponentu, nie samego
  breakpointu telefonu. Przewijanie zostawić jako zabezpieczenie.
- [ ] Sprawdzić wszystkie kroki dla zmiennej nominalnej i porządkowej,
  szczególnie długie nazwy kategorii; nie zmieniać obliczeń przy reformacie.

### Zasady dalszej migracji

- [ ] Dla każdego widgetu oddzielnie określić szerokość panelu i sposób
  reorganizacji treści: tabele, pojedyncze wykresy, porównania wykresów,
  sterowanie i podsumowania mają różne potrzeby.
- [ ] Testować duży ekran, połowę okna i telefon, także szerokości pomiędzy
  breakpointami oraz powiększony tekst. Kontrolować zawijanie, ucięcie
  danych, lokalne przewijanie i zmiany układu po aktualizacji danych.
- [ ] Po zaakceptowaniu poprawionego pilotażu stopniowo migrować oba kursy.
  Nie wprowadzać globalnego `fit-content` dla wykresów o szerokości `100%`.

Pliki startowe: `01-typy-danych/modules/ch2_jakosciowe.R`,
`R/lecture_layout.R`, `R/shared_styles.css`, `R/DESIGN_CONTRACT.md`;
analogiczne komponenty są w `../analiza-ryzyka/R/`.

---

## Ogólne: wdrożenie gloss() na całość projektu

System klikalnych terminów słownikowych (`gloss()`) jest gotowy i przetestowany
w `03-przedzialy-ufnosci/modules/ch1_estymacja.R`. Słownik 163 haseł jest w
`R/glossary.R`; `gloss("hasło", "forma")` obsługuje odmianę.

Do zrobienia: przejrzeć moduły wszystkich wykładów i owinąć `gloss()` pierwsze
wprowadzenia kluczowych terminów (nie każde wystąpienie — tylko to gdzie pojęcie
jest wprowadzane po raz pierwszy w danym rozdziale).

Powiązane pliki:
- [R/glossary.R](R/glossary.R) — słownik terminów (tu też dopisywać nowe hasła)
- [03-przedzialy-ufnosci/modules/ch1_estymacja.R](03-przedzialy-ufnosci/modules/ch1_estymacja.R) — wzorzec użycia

---

## Wykład: regresja

### Rozbudowa ch2 „Co czyni model dobrym?" — pozostałe opcje

~~Q-Q plot reszt~~ — dodany jako trójpanel w Ryc. 2.1 ✓
~~Ekstrapolacja~~ — dodana jako nowa sekcja Ryc. 2.4 ✓

Do ewentualnego rozważenia (nie krytyczne):
- **Outliery wpływowe (odległość Cooka)** — które obserwacje ciągną linię.
  Widget: scatter z wyróżnionymi punktami o wysokiej Cook's distance + opcja „usuń i przelicz".

Powiązane pliki:
- [06-regresja/modules/ch2_jakosc.R](06-regresja/modules/ch2_jakosc.R)
- [06-regresja/modules/helpers.R](06-regresja/modules/helpers.R) — `generate_assumption_data()` ma 5 scenariuszy

### Hero rozdziałów regresji — przekształcić `lead` w pytanie-hook

Wykład o wnioskowaniu statystycznym (ch1, ch4, ch6) otwiera każdy rozdział
konkretnym pytaniem („Czy wraz ze wzrostem temperatury rośnie sprzedaż lodów?").
W rozdziałach regresji `lead` to obecnie stwierdzenia („Korelacja mówiła,
czy dwie zmienne są powiązane. Regresja idzie dalej..."). Stwierdzenia
informują, pytania zaczepiają. Warto przejrzeć 6 hero i przeformułować
ledy w pytania, gdzie to naturalne.

Powiązane pliki:
- [06-regresja/modules/ch1_liniowa.R](06-regresja/modules/ch1_liniowa.R) i kolejne ch2-ch6

### Sekcja „Pułapki regresji" — potencjalny przyszły rozdział

Większość tematów omówiona przy korelacji (kwartet Anscombe'a, korelacja pozorna,
Simpson, nieliniowość, outlier) — można dać odnośnik do tamtego wykładu.
~~Ekstrapolacja~~ dodana do ch2 ✓.

Pozostaje do ewentualnego rozszerzenia:
- kwartet Anscombe'a specyficznie dla regresji (wzorce reszt różne przy tym samym R²)
- spurious regression

Nie wymaga osobnego rozdziału — mogłoby wejść jako callout w ch2 lub ch4.

### Quiz interpretacji b₁ w jednostkach

W ch1 sekcja CASchools pokazuje, jak czytać tabelę regresji. Można dodać
prosty quiz: dane („read ~ income", b₁ = 1.88), pytanie „Co to znaczy
dla okręgu, którego dochód rośnie o 1 tys. USD?", odpowiedzi wielokrotnego
wyboru z dystraktorami (mylące jednostki, mylące skale). Aktywizuje
umiejętność czytania jednostek, którą w ch1 wprowadzamy ale słabo trenujemy.

Powiązane pliki:
- [06-regresja/modules/ch1_liniowa.R](06-regresja/modules/ch1_liniowa.R) — sekcja `ch1-caschool`

### Regresja do średniej — mini-widget

W wykładzie o korelacji (`wnioskowanie-statystyczne/modules/ch4_korelacja.R`,
ryc. 6.1/6.2/6.3) elipsy 95% pokazują rozkład punktów. Widać tam, że
**linia regresji nie pokrywa się z główną osią elipsy** — jest mniej stroma.
Dla małego r (np. r=0.31 w Ryc. 6.3 panel "Duży rozrzut") rozjazd jest
najwyraźniejszy: elipsa biegnie po skosie 1:1, regresja jest prawie pozioma.

To klasyczny obraz **regresji do średniej**: przewidywany y jest zawsze
bliżej zera niż wskazywałby kształt chmury. Wzór:

```
b = r × (sd_y / sd_x)
```

Pomysł na widget w `regresja/modules/ch1_liniowa.R` (lub osobnym module):

- scatter plot ze suwakiem `r` (np. 0.1–0.95)
- dwie linie na wykresie: główna oś elipsy (linia 1:1 przy `sd_x = sd_y`)
  i linia regresji `y ~ x`
- live'owe pokazanie wartości `b = r × (sd_y / sd_x)` poniżej
- przykład numeryczny: "ucznia z x = +2 SD spodziewasz się y = `r × 2` SD,
  nie y = +2 SD" — spłaszczenie ku średniej

Aktualnie w wykładzie o korelacji **nic o tym nie mówimy**, świadomie —
żeby nie odciągać od głównej puenty (r mierzy ciasność, nie nachylenie).
Ale w wykładzie o regresji to powinno wybrzmieć.

Powiązane pliki:
- [06-regresja/modules/ch1_liniowa.R](06-regresja/modules/ch1_liniowa.R)
- [04-wnioskowanie-statystyczne/modules/ch4_korelacja.R](04-wnioskowanie-statystyczne/modules/ch4_korelacja.R) (ryc. 6.1–6.3 jako odniesienie)
- [scripts/regen_correlation_assets.R](scripts/regen_correlation_assets.R) (generator elips)

---

## Jakość kodu: bold overuse w wnioskowanie-statystyczne

`wnioskowanie-statystyczne/modules/ch1_logika.R` ma ~37 wystąpień `tags$strong()` /
`tags$b()` — znacznie więcej niż inne wykłady. CLAUDE.md ogranicza bold do:
krótkich etykiet z dwukropkiem, one-word werdyktów, status-tagów.

Warto przejrzeć ten plik i ograniczyć bold do semantycznych oznaczeń.
Pozostałe wykłady (rozklady-prawdopodobienstwa, zalozenia-testow) używają bold oszczędnie — wzorzec do naśladowania.

Powiązane pliki:
- [wnioskowanie-statystyczne/modules/ch1_logika.R](wnioskowanie-statystyczne/modules/ch1_logika.R)

---

## Jakość kodu: rstatix w zalozenia-testow ch1

`zalozenia-testow/modules/ch1_normalnosc.R` używa `ks.test()` (base R) zamiast
rstatix. CLAUDE.md nakazuje preferować rstatix. Wyjątek może być uzasadniony
dydaktycznie (pokazujemy składnię KS), ale warto rozważyć ujednolicenie.

Widget 2 (testy normalności) używa już `shapiro_test()` z rstatix — KS jest jedynym
odstępstwem w tym module.

Powiązane pliki:
- [zalozenia-testow/modules/ch1_normalnosc.R](zalozenia-testow/modules/ch1_normalnosc.R)

---

## Infrastruktura: fullscreen jako globalny pattern ✓

Zrobione: globalny helper `lc_plot_fullscreen(outputId, ...)` jest w
[R/lecture_layout.R](R/lecture_layout.R), style są w
[R/shared_styles.css](R/shared_styles.css), a obsługa Fullscreen API w
[R/shared_toc.js](R/shared_toc.js).

Powiązane pliki:
- [06-regresja/modules/ch3_wieloraka.R](06-regresja/modules/ch3_wieloraka.R) — referencyjna implementacja lokalna

---

## Rozbudowa: case-studies — więcej rozdziałów

`case-studies` ma tylko 1 rozdział (ch1_caschools — dane CASchools z AER).
Brak quizów i nawigacji między rozdziałami. W porównaniu do innych wykładów
(9–13 rozdziałów) wykład jest szczątkowy.

Potencjalne rozdziały:
- ch2: case study z danymi palmerpenguins (ANOVA/korelacja)
- ch3: case study binarna — dane medyczne (regresja logistyczna)
- ch4: case study czasowy — symulacja zmian w czasie

Powiązane pliki:
- [case-studies/app.R](case-studies/app.R)
- [case-studies/modules/ch1_caschools.R](case-studies/modules/ch1_caschools.R)
