# TODO — pomysły do późniejszej realizacji

Lista rzeczy zauważonych przy okazji innej pracy, które warto kiedyś zrobić,
ale nie blokują obecnego zadania. Posortowane luźno wg modułu.

---

## Ogólne: wdrożenie gloss() na całość projektu

System klikalnych terminów słownikowych (`gloss()`) jest gotowy i przetestowany
w `przedzialy-ufnosci/modules/ch1_estymacja.R`. Słownik 31 haseł jest w
`R/glossary.R`.

Do zrobienia: przejrzeć moduły wszystkich wykładów i owinąć `gloss()` pierwsze
wprowadzenia kluczowych terminów (nie każde wystąpienie — tylko to gdzie pojęcie
jest wprowadzane po raz pierwszy w danym rozdziale).

Powiązane pliki:
- [R/glossary.R](R/glossary.R) — słownik terminów (tu też dopisywać nowe hasła)
- [przedzialy-ufnosci/modules/ch1_estymacja.R](przedzialy-ufnosci/modules/ch1_estymacja.R) — wzorzec użycia

---

## Wykład: regresja

### Rozbudowa ch2 „Co czyni model dobrym?" do pełnej diagnostyki

Obecny ch2 jest w wersji MVP: wzorzec reszt + R² + RMSE. Świadomie pominięto
dwie dalsze diagnostyki, które naturalnie do tego rozdziału pasują:

- **Q-Q plot reszt** — czy reszty są w przybliżeniu normalne (sygnalizuje,
  czy testy istotności współczynników są wiarygodne, czy nie). Pasowałby
  obok reszt vs fitted: scatter X-Y + reszty vs fitted + Q-Q jako trójpanel.
- **Outliery wpływowe (odległość Cooka)** — które pojedyncze obserwacje
  ciągną linię na siebie. Widget mógłby pokazywać scatter z wyróżnionymi
  punktami o wysokiej Cook's distance i opcją „usuń te punkty i przelicz".

Po dodaniu tych dwóch ch2 byłby pełnym wprowadzeniem do diagnostyki modelu
liniowego — wciąż dla pojedynczego modelu, wciąż bez porównań.

Powiązane pliki:
- [05-regresja/modules/ch2_jakosc.R](05-regresja/modules/ch2_jakosc.R) — obecny MVP
- [05-regresja/modules/helpers.R](05-regresja/modules/helpers.R) — `generate_assumption_data()` ma już 5 scenariuszy, można wykorzystać

### Hero rozdziałów regresji — przekształcić `lead` w pytanie-hook

Wykład o wnioskowaniu statystycznym (ch1, ch4, ch6) otwiera każdy rozdział
konkretnym pytaniem („Czy wraz ze wzrostem temperatury rośnie sprzedaż lodów?").
W rozdziałach regresji `lead` to obecnie stwierdzenia („Korelacja mówiła,
czy dwie zmienne są powiązane. Regresja idzie dalej..."). Stwierdzenia
informują, pytania zaczepiają. Warto przejrzeć 6 hero i przeformułować
ledy w pytania, gdzie to naturalne.

Powiązane pliki:
- [05-regresja/modules/ch1_liniowa.R](05-regresja/modules/ch1_liniowa.R) i kolejne ch2-ch6

### Sekcja „Pułapki regresji" — nowy rozdział lub dodatek do istniejących

W wykładzie o korelacji (ch6 ch4_korelacja) jest świetna sekcja „Pułapki
korelacji" (kwartet Anscombe'a, korelacja pozorna, Simpson, nieliniowość,
outlier). W regresji analog brzmiałby:

- kwartet Anscombe'a dla regresji (4 zbiory z tym samym b₀, b₁, R², ale
  zupełnie różnymi wzorcami reszt — silny argument za diagnostyką),
- outliery wpływowe (pokrywa się z Cookiem powyżej),
- spurious regression (regresja na pozornie powiązanych zmiennych),
- ekstrapolacja (decyzja świadomie odrzucona w ch1; może wrócić).

Mógłby być osobnym rozdziałem (po porównaniu modeli, przed logistyczną)
albo dodatkiem do ch2 jakości.

### Mini-widget reszt vs fitted już istnieje w ch2 — patrz wyżej

To samo, co było pierwszą sugestią z mojej strony, ale po reorganizacji
wszedł naturalnie do nowego ch2. Pozostawione tu jako notatka, że temat
jest zaadresowany.

### Quiz interpretacji b₁ w jednostkach

W ch1 sekcja CASchools pokazuje, jak czytać tabelę regresji. Można dodać
prosty quiz: dane („read ~ income", b₁ = 1.88), pytanie „Co to znaczy
dla okręgu, którego dochód rośnie o 1 tys. USD?", odpowiedzi wielokrotnego
wyboru z dystraktorami (mylące jednostki, mylące skale). Aktywizuje
umiejętność czytania jednostek, którą w ch1 wprowadzamy ale słabo trenujemy.

Powiązane pliki:
- [05-regresja/modules/ch1_liniowa.R](05-regresja/modules/ch1_liniowa.R) — sekcja `ch1-caschool`

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
- [05-regresja/modules/ch1_liniowa.R](05-regresja/modules/ch1_liniowa.R)
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

## Infrastruktura: fullscreen jako globalny pattern

W `05-regresja/modules/ch3_wieloraka.R` w widgecie `ch3-budowanie` ("Predykcja
średniej ocen") dodany został przycisk fullscreen na wykresie scatter.
Implementacja jest lokalna: CSS + JS są w `tags$head` wewnątrz `ch3_ui`,
klasa `.lc-plot-fullscreen-wrap` używa natywnego HTML5 Fullscreen API.

Jeśli okaże się przydatne w innych widgetach, najczyściej wynieść:
- CSS → [R/shared_styles.css](R/shared_styles.css)
- JS → [R/shared_toc.js](R/shared_toc.js) (lub osobny `shared_fullscreen.js`)
- helper `lc_plot_fullscreen(outputId, ...)` w
  [R/lecture_layout.R](R/lecture_layout.R) wokół `plotOutput`

Powiązane pliki:
- [05-regresja/modules/ch3_wieloraka.R](05-regresja/modules/ch3_wieloraka.R) — referencyjna implementacja lokalna

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
