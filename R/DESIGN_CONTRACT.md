# Kontrakt Designu

Ten dokument opisuje docelowy system dla interaktywnych wykładów. Nie jest instrukcją migracji i nie opisuje starego layoutu. Nowy kod ma trzymać się tych reguł bez warstw kompatybilności.

## Zakres

Kontrakt dotyczy wykładów opartych o `lecture_page()`:

- `typy-danych/`
- `rozklady-prawdopodobienstwa/`
- `przedzialy-ufnosci/`
- `wnioskowanie-statystyczne/`
- `regresja/`

Niezmigrowane wykłady mogą być zepsute po usunięciu starych aliasów i stylów. Nie naprawiamy ich przy pracy nad kontraktem nowego systemu.

## Shell Aplikacji

Każdy wykład używa `lecture_page()` z `R/lecture_layout.R`. Rozdziały są listami tworzonymi przez `lecture_chapter()` albo jawne listy z polami `id`, `num`, `title`, `content`.

Kanoniczny `app.R`:

```r
source(file.path(project_root, "R", "palette.R"),        local = TRUE)
source(file.path(project_root, "R", "theme_upwr.R"),     local = TRUE)
source(file.path(project_root, "R", "shared.R"),         local = TRUE)
source(file.path(project_root, "R", "lecture_layout.R"), local = TRUE)

lc_apply_ggplot_defaults()

.chapters <- list(ch1_ui, ch2_ui, ch3_ui)

ui <- lecture_page(
  lecture_id    = "nazwa-folderu",
  lecture_num   = "01",
  lecture_title = "Tytuł wykładu",
  module_label  = "Moduł I",
  chapters      = .chapters
)

server <- function(input, output, session) {
  lc <- lecture_server(.chapters, input, output, session)
  ch1_server(input, output, session)
  ch2_server(input, output, session)
  ch3_server(input, output, session)
}
```

Nie używamy `bslib` do budowy strony. Jeżeli aplikacja potrzebuje zasobów w `<head>`, przekazuje je przez `header_extras`.

## Komponenty Treści

Kanoniczne komponenty:

| Potrzeba | Komponent |
|---|---|
| Okładka rozdziału | `lc_chapter_hero()` |
| Sekcja w TOC | `lc_h2()` |
| Podsekcja | `lc_h3()` |
| Akapit narracyjny | `lc_p()` |
| Siatka tekst + margines | `lc_grid()` |
| Wykres, tabela, widget | `figure_panel()` |
| Wzór | `lc_formula_box()` |
| Metryki i statystyki | `lc_stat_grid()` + `lc_stat_box()` |
| Dynamiczny feedback | `lc_feedback()` |
| Notka marginalna | `margin_callout()` albo `margin_note()` |
| Notka z kodem | `margin_code_note()` |
| Przejście do następnego rozdziału | `lc_chapter_next()` |

TOC wykrywa tylko sekcje tworzone przez `lc_h2()` albo zgodne z atrybutem `data-lc-section`.

## Zakazane Wzorce

W nowym kodzie nie dodajemy:

- `fluidPage()`, `navbarPage()`, `sidebarLayout()`, `tabPanel()` jako struktury rozdziałów
- `bslib::page_*()`, `bs_theme()`, lokalnych motywów Bootswatch
- klas strukturalnych `section-title`, `chapter-title`, `widget-block`, `narrative`
- klas calloutów `callout-info`, `callout-warning`, `callout-success`, `callout-danger`
- klas przycisków Bootstrap typu `btn-primary`, `btn-outline-*`, `btn-sm`, `btn-lg`; używaj `lc-btn-*`
- klas tabel Bootstrap typu `table`, `table-bordered`, `table-striped`, `table-sm`; używaj `lc-table*`
- dawnych aliasów kolorów typu `col_primary`, `col_secondary`, `col_success`, `col_warning`, `col_dark`
- dawnych hexów UI typu `#7f8c8d`, `#f8f9fa`, `#2c3e50`, `#3498db`; używaj `upwr_*` albo `var(--upwr-*)`
- `theme_educational()` i `theme_minimal()` jako lokalnego standardu wizualizacji
- lokalnego `includeCSS()` dla wspólnego layoutu

Jeżeli dynamiczny `renderUI()` potrzebuje komunikatu statusu, najpierw dodaj mały komponent w `R/lecture_layout.R`, zamiast przywracać klasę z poprzedniego systemu.

Migracja dawnych fragmentów powinna iść wprost:

| Dawny wzorzec | Docelowy komponent |
|---|---|
| Nagłówek sekcji | `lc_h2()` |
| Blok narracji | `lc_p()` albo `lc_grid()` |
| Panel z widgetem | `figure_panel()` |
| Blok wzoru | `lc_formula_box()` |
| Kafelki metryk | `lc_stat_grid()` + `lc_stat_box()` |
| Feedback po interakcji | `lc_feedback()` |
| Notka boczna | `margin_callout()` albo `margin_note()` |
| Przycisk | klasy `lc-btn-primary`, `lc-btn-outline`, `lc-btn-ok`, `lc-btn-warning`, `lc-btn-danger`, `lc-btn-secondary-outline` |
| Tabela HTML | klasy `lc-table`, `lc-table-bordered`, `lc-table-striped`, `lc-table-sm` |
| Pionowa grupa kontrolek | `lc_stack()` |
| Krótki rząd kontrolek | `lc_inline_row()` albo `step-buttons` dla kroków |
| Wyśrodkowany blok statusu | `lc_center()` |
| Świadomy odstęp końcowy | `lc_spacer()` |

## Kolory i Wykresy

Źródłem prawdy dla kolorów jest `R/palette.R`.

Stosuj:

- `upwr_accent` dla głównego akcentu
- `upwr_secondary` dla kontekstu i ciemnego tekstu w wykresach
- `upwr_reference` dla linii referencyjnych
- `upwr_cat` i `upwr_cat_n(n)` dla kategorii
- `scale_fill_upwr_seq()` i `scale_color_upwr_seq()` dla skal ciągłych
- `theme_upwr()` dla wykresów ggplot2

Semantyczne kolory domenowe są dopuszczalne, jeśli realnie poprawiają czytelność kodu w obrębie jednego wykładu, ale ich wartości muszą pochodzić z palety UPWr.

## CSS

`R/shared_styles.css` jest CSS-em nowego systemu. Nie dodajemy do niego fallbacków dla starych klas.

Style specyficzne dla jednego wykładu mogą trafić do `header_extras`, ale powinny:

- używać tokenów `--upwr-*`
- dotyczyć unikalnych klas danego wykładu
- nie redefiniować komponentów `lc-*`

## Kontrola

Uruchom:

```sh
Rscript scripts/check_design_contract.R
```

Na razie skrypt raportuje naruszenia informacyjnie. Tryb twardy:

```sh
Rscript scripts/check_design_contract.R --strict
```

Tryb `--strict` kończy się błędem, jeśli znajdzie zakazane wzorce.
