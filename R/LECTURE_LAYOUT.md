# Layout wykładów (`lecture_page`)

Pliki współdzielone: `R/lecture_layout.R`, `R/palette.R`, `R/theme_upwr.R`, `R/shared.R`, `R/shared_styles.css`, `R/shared_toc.js`.

Wzorce do naśladowania: `typy-danych/`, `wnioskowanie-statystyczne/`, `rozklady-prawdopodobienstwa/`, `przedzialy-ufnosci/`, `zalozenia-testow/`, `regresja/`.

To jest specyfikacja docelowego systemu. Aplikacje wykładowe używają `lecture_page()` i komponentów `lc_*`.

---

## 1. `app.R` — 5 zmian

### a) Nie ładuj `bslib` i nie definiuj kolorów inline

Kolory pochodzą z `R/palette.R`, a shell strony z `lecture_page()`. Nie definiuj lokalnych palet hex w `app.R`.

### b) Dodaj sourcing palety i layoutu (po `.find_app_dir()`)

```r
source(file.path(project_root, "R", "palette.R"),        local = TRUE)
source(file.path(project_root, "R", "theme_upwr.R"),     local = TRUE)
source(file.path(project_root, "R", "shared.R"),         local = TRUE)
source(file.path(project_root, "R", "lecture_layout.R"), local = TRUE)

lc_apply_ggplot_defaults()   # motyw upwr + Atkinson dla wszystkich geom-ów
```

### c) Zdefiniuj listę rozdziałów i użyj `lecture_page`

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

### d) Server

Nawigacja między rozdziałami obsługiwana jest przez `lc_chapter_next()` w modułach. W `server()`:

```r
server <- function(input, output, session) {
  lc <- lecture_server(.chapters, input, output, session)

  ch1_server(input, output, session)
  ch2_server(input, output, session)
  # ...
}
```

### e) Zasoby w `<head>`

`lecture_page()` samodzielnie wstrzykuje CSS, JS i fonty. Jeśli apka potrzebuje dodatkowych zasobów (Chart.js, custom CSS), przekaż je przez parametr `header_extras`.

---

## 2. Moduły `ch*.R`

```r
ch1_ui <- list(
  id      = "ch-slug",
  num     = "01",
  title   = "Tytuł",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 01 · Nazwa wykładu",
      num    = "01",
      title  = "Tytuł rozdziału.",
      lead   = "Jedno lub dwa zdania wprowadzenia."
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

**Docelowe komponenty treści:**

| Potrzeba | Komponent |
|---|---|
| Okładka rozdziału | `lc_chapter_hero()` |
| Sekcja widoczna w TOC | `lc_h2(id, num, title)` |
| Podsekcja | `lc_h3(title)` |
| Akapit narracyjny | `lc_p(...)` |
| Widget/wykres/tabela | `figure_panel(label, title, ..., full_width = TRUE/FALSE)` |
| Notka na marginesie | `margin_callout()` albo `margin_note()` |
| Następny rozdział | `lc_chapter_next()` |

---

## 3. Nagłówki sekcji — TOC w sidebarze

JS wykrywa sekcje przez `h2[id]`.

```r
lc_h2(id = "ch1-slug", num = "§ 1", title = "Tytuł sekcji")
```

Format ID: `ch{N}-slug`, np. `ch3-mediana`, `ch4-boxplot`. Konwertuj tylko główne sekcje (5–8 na rozdział).

---

## 4. Kolory

Korzystaj bezpośrednio z palety UPWr:

| Rola | Wartość |
|---|---|
| Akcent | `upwr_accent` |
| Tekst/kontekst | `upwr_secondary` |
| Linia referencyjna | `upwr_reference` |
| Kategorie | `upwr_cat` albo `upwr_cat_n(n)` |
| Skale ciągłe | `scale_fill_upwr_seq()`, `scale_color_upwr_seq()` |

**Semantyczne stałe domenowe** (np. `col_normal`, `col_discrete`) mogą zostać w `helpers.R` jeśli są używane w wielu miejscach — ale jako `unname(upwr_cat["nazwa"])`, nie jako hardcoded hex:

```r
# helpers.R — OK jeśli używane w wielu funkcjach tego wykładu
col_normal   <- unname(upwr_cat["wrzos"])
col_binomial <- unname(upwr_cat["bursztyn"])
col_poisson  <- unname(upwr_cat["szalwia"])
```

Pełna paleta w `R/palette.R`: `upwr_cat` (8 kolorów: grafit, bursztyn, niebo, szalwia, kurkuma, indygo, terakota, wrzos), `upwr_accent` (burgund), `upwr_secondary` (grafit ciemny), `upwr_reference` (szarobeżowy).

---

## 5. Motyw ggplot

```r
theme_upwr()
theme_upwr(base_size = 12)
```

`lc_apply_ggplot_defaults()` ustawia `theme_upwr` jako globalny domyślny motyw — callów `+ theme_upwr()` można unikać, ale można je zostawić tam, gdzie lokalnie doprecyzowują wykres.

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

Statyczne notki marginalne twórz przez `margin_callout()` lub `margin_note()`. Dynamiczne komunikaty wewnątrz `renderUI()` mogą nadal używać lekkich klas informacyjnych, ale nie powinny sterować strukturą layoutu.

Callout floatuje do prawej i **tekst po nim owijał się dookoła**. Zatem:
- Callout który był *po* widgecie jako podsumowanie → przenieś **przed następną sekcję** (h2 + narracja), żeby owijanie miało sens
- `figure_panel(full_width=TRUE)` czyści floaty (`clear: both`) — elementy po nim zaczynają poniżej calloutów

```r
# Typowy pattern: callout floatuje, narracja się owija
margin_callout(label = "Zapamiętaj", "treść", color = "uwaga"),
lc_h2(id = "ch1-nastepna", num = "§ 2", title = "Następna sekcja"),
lc_p("Ten tekst owija się obok callout..."),
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
| `lc_formula_box(...)` | blok wzoru lub krótkiego zapisu matematycznego |
| `lc_stat_grid(..., columns)` | siatka metryk/statystyk |
| `lc_stat_box(label, value, ..., caption, color)` | pojedyncza metryka z lewym akcentem |
| `lc_feedback(..., type)` | dynamiczny komunikat w `renderUI()`; `type`: `"info"` / `"ok"` / `"warning"` / `"danger"` |
| `lc_chapter_next(num, title, lead, target_id)` | link „→ Dalej" na marginesie |

---

## Status migracji

| Wykład | Folder | Status |
|--------|--------|--------|
| Statystyka opisowa | `typy-danych/` | ✅ |
| Rozkłady prawdopodobieństwa | `rozklady-prawdopodobienstwa/` | ✅ |
| Wnioskowanie statystyczne | `wnioskowanie-statystyczne/` | ✅ |
| Przedziały ufności | `przedzialy-ufnosci/` | ✅ |
| Założenia testów | `zalozenia-testow/` | ✅ |
| Regresja | `regresja/` | ✅ |
| Metody bayesowskie | `metody-bayesowskie/` | ⬜ |
| Symulacje | `symulacje-statystyczne/` | ⬜ |
| Case studies | `case-studies/` | ⬜ |
