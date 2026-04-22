# Migracja wykładu do nowego layoutu (`lecture_page`)

Nowy design zaimplementowany w gałęzi `design-prototype`.
Pilot: `typy-danych/`. Pliki współdzielone: `R/lecture_layout.R`, `R/shared_styles.css`, `R/shared_toc.js`.

---

## Kroki migracji

### 1. `app.R` — 4 zmiany

**a) Dodaj source `lecture_layout.R`** (po `source(shared.R, ...)`):

```r
source(file.path(project_root, "R", "lecture_layout.R"), local = TRUE)
```

**b) Zdefiniuj listę rozdziałów jako zmienną** (zamiast przekazywać inline):

```r
.chapters <- list(ch1_ui, ch2_ui, ch3_ui, ...)
```

**c) Zamień `navbarPage(...)` na `lecture_page(...)`**:

```r
ui <- lecture_page(
  lecture_id    = "nazwa-apki",    # slug folderu, np. "rozklady-prawdopodobienstwa"
  lecture_num   = "02",            # numer do wyświetlenia w nagłówku
  lecture_title = "Tytuł wykładu",
  module_label  = "Moduł II",
  chapters      = .chapters,
  header_extras = app_extras       # opcjonalnie: Chart.js, custom CSS itp.
)
```

Mapowanie `lecture_id` → moduł zdefiniowane w `lecture_layout.R` (`.LC_LECTURE_MODULE`).
Jeśli dodajesz nową apkę, dopisz ją tam.

**d) W `server()` — wywołaj `lecture_server()` i zamień nawigację**:

```r
server <- function(input, output, session) {
  lc <- lecture_server(.chapters, input, output, session)

  # zamiast updateNavbarPage(session, "main_nav", selected = "..."):
  observeEvent(input$ch1_next,   { lc$switch_to("ch-drugi")   })
  observeEvent(input$ch2_next,   { lc$switch_to("ch-trzeci")  })
  # itd.

  ch1_server(input, output, session)
  ch2_server(input, output, session)
  # itd.
}
```

Nawigacja z poziomu modułu chaptera (np. przycisk "Wróć"):
```r
session$sendCustomMessage("switchToChapter", "ch-slug")
```

---

### 2. Moduły `ch*.R` — format listy zamiast `tabPanel`

```r
# BYŁO
ch1_ui <- tabPanel("1. Tytuł",
  fluidRow(column(8, offset = 2,
    ...treść...
  ))
)

# JEST — usuń tabPanel + fluidRow/column, zostaw samą treść
ch1_ui <- list(
  id      = "ch-slug",     # unikalne ID, używane w nawigacji (lc$switch_to, sendCustomMessage)
  num     = "01",          # numer wyświetlany w sidebarze
  title   = "Tytuł",      # tytuł w sidebarze
  content = tagList(
    ...treść...            # identyczna treść co poprzednio
  )
)
```

**Uwaga na nawiasy:** usunięcie `tabPanel("...", fluidRow(column(8, offset=2, TREŚĆ)))` oznacza
usunięcie dwóch zamykających `))` na końcu bloku `ch*_ui`. Sprawdź balans po zmianie:
```bash
Rscript -e "source('modules/ch1_typy.R')"
```

---

### 3. Nagłówki sekcji — żeby TOC w sidebarze działał

JS odkrywa sekcje szukając `h2[id]` z klasą `section-title` (lub `data-lc-section`).

```r
# BYŁO (div bez id — TOC go nie widzi)
div(class = "section-title", "Tytuł sekcji")

# JEST (h2 z id — TOC linkuje do niego)
h2(id = "ch1-slug", class = "section-title", "Tytuł sekcji")
```

- Konwertuj tylko główne sekcje nawigacyjne (5–8 na rozdział)
- Pierwszy div intro chaptera zostaw jako `div` (nie chcemy go w TOC)
- ID format: `ch{N}-slug`, np. `ch3-mediana`, `ch4-boxplot`

---

### 4. Nawigacja wsteczna w modułach

```r
# BYŁO
observeEvent(input$back_btn, {
  updateNavbarPage(session, "main_nav", selected = "3. Tytuł")
})

# JEST
observeEvent(input$back_btn, {
  session$sendCustomMessage("switchToChapter", "ch-slug")
})
```

---

## Wskazówki

### Callouty w prawym marginesie
`callout-danger`, `callout-warning`, `callout-success`, `callout-info` jako **bezpośrednie dzieci** sekcji rozdziału automatycznie floatują do prawego marginesu (≥ 900px). Callout wewnątrz `.widget-block` pozostaje inline.

### Pełnoszerokościowy wykres
```r
div(class = "widget-block lc-wide", plotOutput("plot"))
```
Bez `lc-wide` wykres mieści się w kolumnie tekstu (~680px). Z `lc-wide` czyści floaty i rozciąga się do pełnej szerokości kontentu.

### Nowe komponenty (opcjonalne)
Dostępne w `lecture_layout.R`:
- `lc_chapter_hero(kicker, num, title, lead)` — okładka rozdziału z dużą cyfrą, tytułem, squiggle-underline i leadem. Używaj jako pierwszy element `content` rozdziału (zamiast `section-title`).
- `lc_h2(id, num, title)` — nagłówek sekcji z numerem §N w sidebarze
- `lc_grid(...)` — jawna siatka tekst + prawy margines
- `margin_callout(label, ..., color)` — callout na prawym marginesie z etykietą. Warianty `color`: `"uwaga"` (burgundy, `※`), `"ok"` (sage, `✓`), `"wskazowka"` (gold, `ℓ` — domyślny)
- `margin_code_note(code, description, label)` — callout "W kodzie" z blokiem monospace (gold, `⌘`)
- `margin_note(...)` — lżejsza notka marginalna
- `figure_panel(label, ..., title, color)` — ramka z plakietką (np. `label = "Ryc. 1.1"`)
- `lc_chapter_next(num, title, lead, target_id)` — navigational link "→ Dalej" do następnego rozdziału (na marginesie)

### Dodawanie nowego wykładu do górnego paska
W `R/lecture_layout.R`, lista `.LC_MODULES` i mapa `.LC_LECTURE_MODULE`:
```r
.LC_MODULES <- list(
  list(num = "I",  slug = "opisowa",  title = "Statystyka opisowa", href = "#"),
  ...
)
.LC_LECTURE_MODULE <- list(
  "typy-danych" = "opisowa",
  "nowa-apka"   = "slug-modulu",  # ← dodaj tutaj
  ...
)
```

---

## Status migracji

| Wykład | Folder | Status |
|--------|--------|--------|
| Statystyka opisowa | `typy-danych/` | ✅ zmigrowany |
| Rozkłady | `rozklady-prawdopodobienstwa/` | ⬜ do zmigrowania |
| Przedziały ufności | `przedzialy-ufnosci/` | ⬜ do zmigrowania |
| Wnioskowanie | `wnioskowanie-statystyczne/` | ✅ zmigrowany |
| Założenia testów | `zalozenia-testow/` | ⬜ do zmigrowania |
| Regresja | `regresja/` | ⬜ do zmigrowania |
| Metody bayesowskie | `metody-bayesowskie/` | ⬜ do zmigrowania |
| Symulacje | `symulacje-statystyczne/` | ⬜ do zmigrowania |
| Case studies | `case-studies/` | ⬜ do zmigrowania |
