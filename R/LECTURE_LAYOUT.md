# Migracja wykładu do nowego layoutu (`lecture_page`)

Nowy design w gałęzi `design-prototype`.
Pliki współdzielone: `R/lecture_layout.R`, `R/palette.R`, `R/theme_upwr.R`, `R/shared.R`, `R/shared_styles.css`, `R/shared_toc.js`.

Zmigrowane wzorce do naśladowania: `typy-danych/`, `wnioskowanie-statystyczne/`, `rozklady-prawdopodobienstwa/`.

---

## 1. `app.R` — 5 zmian

### a) Usuń `library(bslib)` i blok kolorów inline

Stare apki definiowały kolory bezpośrednio w `app.R` (`col_primary <- "#3498db"` itp.) i używały `bs_theme(bootswatch = "sandstone")`. Wszystko to wylatuje.

### b) Dodaj sourcing palety i layoutu (po `.find_app_dir()`)

```r
source(file.path(project_root, "R", "palette.R"),        local = TRUE)
source(file.path(project_root, "R", "theme_upwr.R"),     local = TRUE)
source(file.path(project_root, "R", "shared.R"),         local = TRUE)
source(file.path(project_root, "R", "lecture_layout.R"), local = TRUE)

lc_apply_ggplot_defaults()   # motyw upwr + Atkinson dla wszystkich geom-ów
```

### c) Zdefiniuj listę rozdziałów i zastąp `navbarPage` → `lecture_page`

```r
.chapters <- list(ch1_ui, ch2_ui, ...)

ui <- lecture_page(
  lecture_id    = "nazwa-folderu",   # slug folderu apki, np. "rozklady-prawdopodobienstwa"
  lecture_num   = "02",
  lecture_title = "Tytuł wykładu",
  module_label  = "Moduł II",
  chapters      = .chapters
)
```

Mapowanie `lecture_id → moduł` zdefiniowane w `.LC_LECTURE_MODULE` w `lecture_layout.R` — dopisz nową apkę jeśli potrzeba.

### d) Zastąp server — usuń `observeEvent(input$ch*_next, updateNavbarPage(...))`

Nawigacja między rozdziałami obsługiwana jest przez `lc_chapter_next()` w modułach. W `server()` wystarczy:

```r
server <- function(input, output, session) {
  lc <- lecture_server(.chapters, input, output, session)

  ch1_server(input, output, session)
  ch2_server(input, output, session)
  # ...
}
```

### e) Usuń `global_header`

`lecture_page()` samodzielnie wstrzykuje CSS, JS i fonty. Blok `global_header <- tagList(withMathJax(), tags$head(...))` wylatuje. Jeśli apka potrzebuje dodatkowych zasobów (Chart.js, custom CSS), przekaż je przez parametr `header_extras`.

---

## 2. Moduły `ch*.R` — lista zamiast `tabPanel`

```r
# BYŁO
ch1_ui <- tabPanel("1. Tytuł",
  fluidRow(column(8, offset = 2,
    div(class = "chapter-recap", "Krótkie przypomnienie..."),
    ...treść...
    div(class = "chapter-transition",
      actionButton("ch1_next", "Dalej: 2. Tytuł →", class = "btn-primary btn-lg")
    ),
    br(), br()
  ))
)

# JEST
ch1_ui <- list(
  id      = "ch-slug",
  num     = "01",
  title   = "Tytuł",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 01 · Nazwa wykładu",
      num    = "01",
      title  = "Tytuł rozdziału.",
      lead   = "Tekst z chapter-recap — 1–2 zdania wprowadzenia."
    ),

    ...treść...

    lc_chapter_next(
      num       = "02",
      title     = "Tytuł następnego",
      lead      = "krótki lead co będzie dalej",
      target_id = "ch-nastepny"
    )
  )
)
```

**Transformacje element po elemencie:**

| Element stary | Element nowy |
|---|---|
| `div(class="chapter-recap", "...")` | `lead` w `lc_chapter_hero()` |
| `div(class="section-title", "Tytuł")` | `h2(id="ch1-slug", class="section-title", "Tytuł")` |
| `div(class="widget-block", h4("Tytuł"), ...)` | `figure_panel(label="Ryc. 1.1", title="Tytuł", ..., full_width=TRUE)` |
| `div(class="callout-*", ...)` statyczny | `margin_callout(label="...", "treść", color="uwaga/ok/wskazowka")` |
| `div(class="callout-*", ...)` w `renderUI()` | zostaw bez zmian — te są dynamiczne |
| `div(class="chapter-transition", actionButton(...))` | `lc_chapter_next(num, title, lead, target_id)` |
| `br(), br()` na końcu | usuń — layout sam zarządza odstępami |

---

## 3. Nagłówki sekcji — TOC w sidebarze

JS wykrywa sekcje przez `h2[id]`.

```r
# BYŁO (bez id — TOC go nie widzi)
div(class = "section-title", "Tytuł sekcji")

# JEST
h2(id = "ch1-slug", class = "section-title", "Tytuł sekcji")
```

Format ID: `ch{N}-slug`, np. `ch3-mediana`, `ch4-boxplot`. Konwertuj tylko główne sekcje (5–8 na rozdział).

---

## 4. Kolory — refaktoryzacja, nie aliasy

Stary kod używał `col_primary`, `col_secondary` itp. Zamiast definiować aliasy w `helpers.R`, zamień bezpośrednio w kodzie:

| Stara zmienna | Nowa wartość |
|---|---|
| `col_primary` | `unname(upwr_cat["niebo"])` |
| `col_secondary` | `unname(upwr_cat["terakota"])` |
| `col_success` | `unname(upwr_cat["szalwia"])` |
| `col_warning` | `unname(upwr_cat["bursztyn"])` |
| `col_dark` | `upwr_secondary` |
| `col_scenario[seq_len(n)]` | `upwr_cat_n(n)` |

**Semantyczne stałe domenowe** (np. `col_normal`, `col_discrete`) mogą zostać w `helpers.R` jeśli są używane w wielu miejscach — ale jako `unname(upwr_cat["nazwa"])`, nie jako hardcoded hex:

```r
# helpers.R — OK jeśli używane w wielu funkcjach tego wykładu
col_normal   <- unname(upwr_cat["wrzos"])
col_binomial <- unname(upwr_cat["bursztyn"])
col_poisson  <- unname(upwr_cat["szalwia"])
```

Pełna paleta w `R/palette.R`: `upwr_cat` (8 kolorów: grafit, bursztyn, niebo, szalwia, kurkuma, indygo, terakota, wrzos), `upwr_accent` (burgund), `upwr_secondary` (grafit ciemny), `upwr_reference` (szarobeżowy).

---

## 5. Motyw ggplot — zamień bezpośrednio

```r
# BYŁO
theme_educational()
theme_educational(base_size = 12)

# JEST
theme_upwr()
theme_upwr(base_size = 12)
```

Zamieniaj bezpośrednio, bez aliasów. `lc_apply_ggplot_defaults()` ustawia `theme_upwr` jako globalny domyślny motyw — callów `+ theme_upwr()` można unikać w nowych wykresach, ale nie trzeba usuwać istniejących.

---

## 6. Reactive zamiast observe + reactiveVal

`lecture_page` renderuje tylko aktywny rozdział — przy starcie inputy nieaktywnych rozdziałów są `NULL`. `observe({})` strzela natychmiast i crashuje przy `switch(NULL,...)`.

**Właściwy wzorzec: `reactive()` zamiast `reactiveVal + observe`.**

```r
# ŹLE — observe strzela przy starcie z NULL inputem
data <- reactiveVal(NULL)
observe({
  n    <- input$n        # NULL → crash
  data(rnorm(n))
})

# DOBRZE — reactive() propaguje NULL naturalnie i jest keszowany
data <- reactive({
  req(input$n)
  rnorm(input$n)
})
```

**Przycisk "Symuluj ponownie"** — dodaj trigger:

```r
sim_trigger <- reactiveVal(0)
observeEvent(input$simulate, sim_trigger(sim_trigger() + 1))

data <- reactive({
  sim_trigger()          # zależy od przycisku
  req(input$n, input$dist)
  switch(input$dist, "normal" = rnorm(input$n), ...)
})
```

**Aktualizacja slidera przy zmianie inputu** (side effect) — użyj `observeEvent`, nie `observe`:

```r
# ŹLE
observe({ req(input$dist); if (input$dist == "norm") updateSliderInput(...) })

# DOBRZE — observeEvent ma ignoreNULL=TRUE domyślnie
observeEvent(input$dist, {
  if (input$dist == "norm") updateSliderInput(session, ...)
})
```

**Reset stanu przy zmianie danych** — osobny `observeEvent`:

```r
step <- reactiveVal(0)
data <- reactive({ req(input$n, input$dist); switch(input$dist, ...) })
observeEvent(list(input$dist, input$n), step(0), ignoreInit = TRUE)
```

Nie używaj `ignoreNULL = FALSE` w `observeEvent` — powoduje strzał przy NULL na starcie.

---

## 7. Callouty — pozycja po migracji

Statyczne `div(class="callout-*")` jako **bezpośrednie dzieci** `.lc-chapter` automatycznie floatują prawostronnie (≥900px). Callout wewnątrz `renderUI()` lub `figure_panel()` pozostaje inline.

Callout floatuje do prawej i **tekst po nim owijał się dookoła**. Zatem:
- Callout który był *po* widgecie jako podsumowanie → przenieś **przed następną sekcję** (h2 + narracja), żeby owijanie miało sens
- `figure_panel(full_width=TRUE)` czyści floaty (`clear: both`) — elementy po nim zaczynają poniżej calloutów

```r
# Typowy pattern: callout floatuje, narracja się owija
margin_callout(label = "Zapamiętaj", "treść", color = "uwaga"),
h2(id = "ch1-nastepna", class = "section-title", "Następna sekcja"),
div(class = "narrative", p("Ten tekst owija się obok callout...")),
figure_panel(label = "Ryc. 1.2", title = "...", full_width = TRUE,  # clears float
  plotOutput("plot")
),
```

---

## 8. Składnia R — typograficzne cudzysłowy

R akceptuje wyłącznie ASCII `"` (0x22) jako ogranicznik stringa. Nie używaj typograficznych `"…"` (U+201C/U+201D) — wyglądają identycznie ale powodują `unexpected input`.

Szczególnie ryzykowne: wklejanie tekstu z edytorów które automatycznie zamieniają cudzysłowy. Sprawdź nowe stringi przez `parse(file)` przed uruchomieniem.

Polskie cudzysłowy typograficzne `„..."` (U+201E/U+201D) mogą być **wewnątrz** stringów jako treść, ale tylko jeśli nie kolidują z ogranicznikiem — np. w stringu ograniczonym przez `'...'` albo jeśli używasz `„`.

---

## Dostępne komponenty (`lecture_layout.R`)

| Funkcja | Opis |
|---|---|
| `lc_chapter_hero(kicker, num, title, lead)` | okładka rozdziału — duża cyfra, tytuł, lead |
| `lc_h2(id, num, title)` | nagłówek sekcji z numerem §N |
| `lc_h3(title)` | podsekcja |
| `lc_p(..., drop)` | akapit narracyjny (z opcjonalną drop-cap) |
| `lc_grid(...)` | jawna siatka tekst + prawy margines |
| `margin_callout(label, ..., color)` | callout na marginesie; color: `"uwaga"` / `"ok"` / `"wskazowka"` |
| `margin_note(...)` | lżejsza notka marginalna (kursywa, bez etykiety) |
| `margin_code_note(code, description, label)` | callout kodu (gold, `⌘`) |
| `figure_panel(label, ..., title, full_width)` | ramka z plakietką; `full_width=TRUE` czyści floaty |
| `lc_chapter_next(num, title, lead, target_id)` | link „→ Dalej" na marginesie |

---

## Status migracji

| Wykład | Folder | Status |
|--------|--------|--------|
| Statystyka opisowa | `typy-danych/` | ✅ |
| Rozkłady prawdopodobieństwa | `rozklady-prawdopodobienstwa/` | ✅ |
| Wnioskowanie statystyczne | `wnioskowanie-statystyczne/` | ✅ |
| Przedziały ufności | `przedzialy-ufnosci/` | ⬜ |
| Założenia testów | `zalozenia-testow/` | ⬜ |
| Regresja | `regresja/` | ⬜ |
| Metody bayesowskie | `metody-bayesowskie/` | ⬜ |
| Symulacje | `symulacje-statystyczne/` | ⬜ |
| Case studies | `case-studies/` | ⬜ |
