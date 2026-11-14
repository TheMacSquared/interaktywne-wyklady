# Wspólny rdzeń wykładów

Jeden system layoutu, stylów i bibliotek dla kursów `statystyka/` i
`analiza-ryzyka/`. Zmianę robi się tutaj raz i obowiązuje w obu kursach.
`statystyka-2/` ma osobny, starszy snapshot we własnym `R/` i z tego katalogu
nie korzysta.

## Zawartość

| Plik | Rola |
|---|---|
| `palette.R` | paleta UPWr (role, kolory kategoryczne, skale) |
| `theme_upwr.R` | motyw ggplot2 |
| `shared.R` | kroje wykresów, generatory danych, `zoom_plot_ui()` / `zoom_plot_server()` |
| `lecture_layout.R` | `lecture_page()`, `lecture_server()` i komponenty `lc_*` |
| `lc_stat_figures.R`, `lc_stat_figures.js` | ryciny liczone w przeglądarce (`window.lcSF`) |
| `lc_charts.js` | wspólna obsługa wykresów ECharts (`window.lcCharts`) |
| `lc_dist.js` | rozkłady i losowanie (`window.lcDist`) |
| `lc_widgets.js`, `lc_dragdrop.js`, `lc_formula.js`, `shared_toc.js` | kontrolki v2, przeciąganie kart, wzór ze znacznikami, spis sekcji |
| `lc_quiz.js` | quiz kafelkowy `lc_tile_quiz()`: losowanie pytań, ocena, wynik (`window.lcTileQuiz`) |
| `shared_styles.css` | style powłoki i komponentów |
| `halloween.css`, `halloween.js` | tryb sezonowy (wyłączony) |
| `fonts/` | IBM Plex Sans i Mono (wagi 400–700): wykresy R i strony statyczne |
| `DESIGN_CONTRACT.md`, `LECTURE_LAYOUT.md` | kontrakt designu i opis komponentów |

## Co zostaje w kursie

Katalog `<kurs>/R/` zawiera tylko pliki własne kursu:

- `course.R` — lista `.LC_COURSE`: nazwa i znak w logo, moduły górnego paska,
  mapowanie `lecture_id → moduł` oraz przełączniki zachowania (niżej). Po tym
  pliku hub i `.find_app_dir()` rozpoznają katalog kursu.
- `course_styles.css` (opcjonalnie, wymieniony w `.LC_COURSE$styles`) — arkusz
  ładowany po `shared_styles.css`: elementy własne kursu i świadome odstępstwa
  od rdzenia.
- pliki przedmiotowe, np. `statystyka/R/ankieta.R`, `statystyka/R/glossary.R`,
  `analiza-ryzyka/R/risk_block.R`.

`app.R` wykładu ładuje najpierw `course.R`, potem pliki rdzenia:

```r
app_dir <- .find_app_dir()
project_root <- dirname(app_dir)
shared_root <- file.path(dirname(project_root), "shared")

source(file.path(project_root, "R", "course.R"), local = TRUE)

source(file.path(shared_root, "palette.R"),        local = TRUE)
source(file.path(shared_root, "theme_upwr.R"),     local = TRUE)
source(file.path(shared_root, "shared.R"),         local = TRUE)
source(file.path(shared_root, "lecture_layout.R"), local = TRUE)
```

## Przełączniki kursu

Rdzeń powstał ze scalenia dwóch kopii (listopad 2026). Tam, gdzie kursy
zachowywały się inaczej, różnica jest jawnym polem w `.LC_COURSE`, a nie
osobną kopią pliku:

| Pole | Statystyka | Analiza ryzyka | Znaczenie |
|---|---|---|---|
| `chapter_mode` | `"single"` | `"all"` | rozdział wstawiany do strony przy wejściu albo wszystkie rozdziały zamontowane od razu (nieaktywne ukryte) |
| `plot_scaling` | `FALSE` | `TRUE` | wykresy R: baza 14 pt bez skalowania albo baza 11 pt i skalowanie czcionek w `zoom_plot_server()` |
| `styles` | brak | `course_styles.css` | arkusze kursu ładowane po rdzeniu |

Przełączniki `chapter_mode` i `plot_scaling` dotyczą kodu, który znika razem
z serwerem Shiny (wykresy ggplot, `renderUI` rozdziału).

## Strony statyczne

`lecture_page()` ma dwa tryby. Z serwerem Shiny (domyślnie) rozdział wstawia
`renderUI`. W trybie statycznym (`options(lc.static = TRUE)`, ustawia go
`scripts/build_static.R`) wszystkie rozdziały są w dokumencie, a przełącza je
`shared_toc.js`; strona nie ładuje Shiny.

| Skrypt | Rola |
|---|---|
| `scripts/check_static_ready.R` | licznik zależności wykładów od serwera (wykresy, `renderUI`, reakcje, wejścia); wykład z samymi zerami można budować statycznie |
| `scripts/build_static.R` | buduje `dist/<kurs>/<wykład>.html`: style i skrypty wbudowane w plik, biblioteki z sieci pobrane raz do `dist/.cache/`, kroje IBM Plex wbudowane w plik, wzory złożone jako SVG |
| `scripts/check_static_page.R` | otwiera zbudowaną stronę w Chromium bez okna: błędy konsoli, żądania sieciowe, nawigacja, suwaki, quiz, niezłożony TeX, szerokość 390 px |

Co działa bez serwera we wspólnych komponentach:

- nawigacja rozdziałów, spis sekcji, „Dalej” (`lcGoToChapter()` w `shared_toc.js`);
- suwak `lc_slider()` (ionRangeSlider uruchamiany w `lc_widgets.js`);
- karty odpowiedzi z kluczem i quiz z omówieniem (`lc_widgets.js`, sekcja „Quiz
  z kluczem”; w analizie ryzyka dane pytań wstawia `risk_quiz_data()`);
- treść wybierana kontrolką: `lc_variants()` trzyma wszystkie warianty
  w dokumencie i pokazuje ten, który odpowiada wartości radia, segmentu albo
  listy (opcjonalnie dopiero po kliknięciu przycisku); zastępuje `uiOutput()`
  z `renderUI()` zależnym od `input$…`;
- komentarz do wybranej odpowiedzi: `lc_choice_feedback()` z przyciskiem
  `lc_check_button()` (w analizie ryzyka także `risk_vote_panel(feedback = )`);
- treść odsłaniana przyciskiem, np. tok rozwiązania: `lc_reveal()`;
- opisy kroków widgetu krokowego: `lc_step_widget(texts = )`;
- quiz losujący pytania z puli: `lc_tile_quiz()` (także ze stałymi odpowiedziami
  w kolorach — `options`, z częścią odpowiedzi w pytaniu — `option_count`,
  i z treścią po odpowiedzi, np. wykresem — `extra`);
- gotowa treść z miejscami po dawnych wyjściach serwera: `lc_static_fill()`
  (np. rozwiązania ćwiczeń w wariantach kierunków);
- treść wybierana kliknięciem (`lc_variants(pick = TRUE)` + atrybuty `lc_pick()`),
  przełączana przyciskiem (`lc_toggle_button()` + `lc_toggle_content()`) albo
  krokiem widgetu (`lc_step_views()`);
- `conditionalPanel()` z warunkami na `input.…` (ocenia je `lc_widgets.js`);
- obrazki z katalogów `addResourcePath()` (build wbudowuje je w plik);
- ryciny `lcSF`, wykresy `lcCharts`, wzór ze znacznikami.

Skrypty strony statycznej są na końcu dokumentu (w nagłówku zostają tylko
oznaczone `data-lc-early`), a treść rozdziałów czeka w `<template>` i trafia
do dokumentu jednym ruchem po ich wczytaniu — tak jak w Shiny, gdzie rozdział
wstawia `renderUI`. Widgety nie zależą więc od kolejności skryptów.

Narzędzia pomocnicze migracji są w `scripts/migracja/`: porównanie paneli
strony statycznej z wersją Shiny (`porownaj_panele.R`), uruchomienie skryptu
sprawdzającego na działającej aplikacji (`z_shiny.R`), zrzuty i JS na stronie
(`zrzut.R`, `js_na_stronie.R`), wszystkie kontrole naraz (`wszystkie_kontrole.sh`).

Wzory wpisane w treść składa MathJax przy budowaniu. Tekst, który skrypt
wykładu tworzy w przeglądarce, nie może zawierać TeX-a: na stronie statycznej
MathJax nie jest ładowany (`check_static_page.R` wykrywa niezłożony zapis).

## Testy

Testy rdzenia są na razie w obu kursach (`<kurs>/tests/`, m.in.
`test-widgets-v2.R` i `test-panel-layout.R` w dwóch niemal identycznych kopiach).
