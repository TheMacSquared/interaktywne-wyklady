# Kontrakt Designu

Ten dokument opisuje docelowy system dla interaktywnych wykładów. Nie jest instrukcją migracji i nie opisuje starego layoutu. Nowy kod ma trzymać się tych reguł bez warstw kompatybilności.

## Zakres

Kontrakt dotyczy wszystkich aplikacji w `statystyka/` opartych o `lecture_page()`. Lista aplikacji znajduje się w [README przedmiotu](../README.md).

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
| Termin słownikowy z definicją | `gloss()` |

TOC wykrywa tylko sekcje tworzone przez `lc_h2()` albo zgodne z atrybutem `data-lc-section`.

`gloss("hasło", "forma w tekście")` owija pierwsze wprowadzenie kluczowego terminu w każdym rozdziale, nie każde wystąpienie. Hasło musi istnieć w `.GLOSSARY` (`R/glossary.R`), drugi argument to forma odmieniona. Statyczne ramki `lc_feedback()` w UI (np. Problem / Zasada / Werdykt, ściągi) traktujemy jak narrację. Nie owijaj terminów w nagłówkach, przyciskach, quizach, etykietach wykresów ani w tekstach generowanych po stronie serwera (dynamiczny feedback, `renderUI()`).

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

Reaktywna przekazana do `zoom_plot_server()` musi zwracać obiekt (ggplot, patchwork, grob/gtable), nigdy rysować przez efekt uboczny. Ta sama reaktywna obsługuje mały wykres i modal powiększenia, a modal dostaje wynik z cache. Do składania paneli używaj `patchwork` albo `gridExtra::arrangeGrob()`, nie `gridExtra::grid.arrange()`.

## CSS

`R/shared_styles.css` jest CSS-em nowego systemu. Nie dodajemy do niego fallbacków dla starych klas.

Style specyficzne dla jednego wykładu mogą trafić do `header_extras`, ale powinny:

- używać tokenów `--upwr-*`
- dotyczyć unikalnych klas danego wykładu
- nie redefiniować komponentów `lc-*`

## Kontrola

Uruchom:

```sh
Rscript statystyka/scripts/check_design_contract.R
```

Na razie skrypt raportuje naruszenia informacyjnie. Tryb twardy:

```sh
Rscript statystyka/scripts/check_design_contract.R --strict
```

Tryb `--strict` kończy się błędem, jeśli znajdzie zakazane wzorce.

## Szerokość paneli i układ widgetów

Nowe i przebudowywane panele wybierają `figure_panel(width_mode = ...)`:

- `"compact"`: szerokość wynikająca z zawartości, do 680 px i dostępnego miejsca;
  krótkie tabele i proste treści bez wykresów oraz procentowych siatek.
- `"text"`: stabilna kolumna 680 px; opisowe lub dynamiczne tabele i pojedyncze wykresy.
- `"wide"`: stabilna kolumna do 980 px; uzasadnione porównania i większe diagramy.

Istniejące wywołania bez `width_mode` zachowują dotychczasowe zachowanie.
Nie łącz `width_mode` z `full_width` w jednym wywołaniu. Nie stosuj trybu
`compact` do wykresu o szerokości 100% — wykres potrzebuje szerokości kontenera.

Tabele osadzaj w `lc_table_region(..., label = "Opis tabeli")`. Region przewija
się poziomo, jeśli zawartość nie mieści się w panelu, i jest dostępny z klawiatury.
Opcjonalne `min_width` (liczba pikseli) chroni tabelę opisową przed zbyt ciasnym
zawijaniem. Dynamiczna tabela zachowuje szerokość panelu po zmianie kolumn.

`lc_widget_layout(controls, content, layout = "above")` umieszcza sterowanie
nad wykresem. Wariant `"beside"` przechodzi do dwóch kolumn dopiero wtedy,
gdy sam widget ma co najmniej 720 px. `lc_controls_row()` rozmieszcza grupy
sterowania w kolumnach, które automatycznie przechodzą do jednego rzędu pionowego.
