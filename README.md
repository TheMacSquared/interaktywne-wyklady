# Interaktywne Wykłady ze Statystyki

Portal do nauki statystyki: narracja w Quarto Markdown + interaktywne widgety Shiny.
Jedna aplikacja, nawigacja zakładkami między wykładami, bez przeładowań.

## Architektura

```
content/*.qmd       → narracja (Markdown + LaTeX + callouty)
                        z markerami <!-- widget: nazwa -->
        ↓ quarto render + split
content_html/*      → fragmenty HTML (generowane automatycznie)
        ↓ includeHTML()
modules/*.R         → UI (HTML fragmenty + widgety Shiny) + server logic
        ↓
app.R               → navbarPage z navbarMenu per wykład
```

**Workflow:**

1. Edytujesz `content/*.qmd` — czysta narracja z markerami `<!-- widget: nazwa -->`
2. Uruchamiasz `shiny::runApp()` — `global.R` automatycznie renderuje QMD i dzieli HTML
3. Moduły składają: `includeHTML(narracja)` + widgety Shiny + `includeHTML(narracja)`

## Wymagania

- R (>= 4.0)
- Quarto CLI
- Pakiety R: `shiny`, `bslib`, `ggplot2`, `dplyr`

```r
install.packages(c("shiny", "bslib", "ggplot2", "dplyr"))
```

## Uruchamianie

```r
shiny::runApp(".")
```

Lub z terminala:

```bash
Rscript -e 'shiny::runApp(".", port = 7670)'
```

## Struktura projektu

```
app.R                    # główna aplikacja Shiny (navbarPage)
global.R                 # auto-build: quarto render + split content
R/split_qmd.R            # dzieli HTML po markerach <!-- widget: -->
content/                 # narracja Markdown (tu edytujesz)
  _quarto.yml            # minimalna konfiguracja renderowania
  ch1_estymacja.qmd
  ch2_idea.qmd
  ch_reg1_liniowa.qmd
content_html/            # (generowane) fragmenty HTML
modules/                 # moduły Shiny (UI + server per rozdział)
  helpers.R              # kolory, theme, stat_box(), generatory danych
  home.R                 # strona główna
  ch1_estymacja.R
  ch2_idea.R
  ch_reg1_liniowa.R
www/styles.css           # style widgetów
_archive/                # oryginalne materiały (źródło treści do migracji)
  shiny-apps/            # 8 oryginalnych Shiny apps z modułami
  quarto-qmd/            # QMD z pierwszej migracji Quarto
  quarto-misc/           # stara infrastruktura Quarto
```

## Dodawanie nowego rozdziału

1. Utwórz `content/ch_nazwa.qmd` — narracja z markerami `<!-- widget: id_widgetu -->`
2. Utwórz `modules/ch_nazwa.R` — `tabPanel()` z `include_content()` + widgetami + server
3. W `app.R` dodaj `source()` i wstaw do odpowiedniego `navbarMenu()`

## Status

Prototyp — 2 wykłady (przedziały ufności, regresja), 3 rozdziały.
Oryginalne treści 8 wykładów w `_archive/shiny-apps/`.
