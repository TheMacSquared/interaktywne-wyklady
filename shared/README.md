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
| `shared_styles.css` | style powłoki i komponentów |
| `halloween.css`, `halloween.js` | tryb sezonowy (wyłączony) |
| `fonts/` | IBM Plex Sans i Mono dla wykresów R |
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

## Testy

Testy rdzenia są na razie w obu kursach (`<kurs>/tests/`, m.in.
`test-widgets-v2.R` i `test-panel-layout.R` w dwóch niemal identycznych kopiach).
