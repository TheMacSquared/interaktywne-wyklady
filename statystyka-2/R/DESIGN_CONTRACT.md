# Kontrakt Designu

Ten dokument opisuje docelowy system dla interaktywnych wykładów. Nie jest instrukcją migracji i nie opisuje starego layoutu. Nowy kod ma trzymać się tych reguł bez warstw kompatybilności.

## Zakres

Kontrakt dotyczy wszystkich aplikacji w `statystyka-2/` opartych o `lecture_page()`. Lista aplikacji znajduje się w [README przedmiotu](../README.md).

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
Rscript statystyka-2/scripts/check_design_contract.R
```

Na razie skrypt raportuje naruszenia informacyjnie. Tryb twardy:

```sh
Rscript statystyka-2/scripts/check_design_contract.R --strict
```

Tryb `--strict` kończy się błędem, jeśli znajdzie zakazane wzorce.

## Widgety v2 i tabele v2

Nowe i przebudowywane widgety oraz tabele korzystają z komponentów v2
(sekcja „WIDGETY V2 I TABELE V2” w `R/lecture_layout.R`, style w
`R/shared_styles.css`, logika klienta w `R/lc_widgets.js`). Sekcja jest
identyczna we wszystkich kursach; zmiany wprowadzamy równolegle.

Style v2 obejmują każdy `figure_panel()`; nie ma osobnej flagi. Elementy
sprzed v2 (`fluidRow`, `sliderInput()`, `lc_stat_box()`, stare tabele,
wykresy o stałej wysokości) nadal działają i są migrowane widget po widgecie.
Wygląd suwaka v2 dotyczy tylko `lc_slider()`; zwykły `sliderInput()` zachowuje
dymek z wartością. W analizie ryzyka `risk_widget_panel()` składa pasek
z odczytami, wykres i podpis.

### Widgety

| Potrzeba | Komponent |
|---|---|
| Pasek sterowania nad treścią | `lc_toolbar()` |
| Grupa z etykietą | `lc_group()` |
| Radio z ≤ 4 krótkimi opcjami | `lc_segmented()` |
| Seria akcji (np. +1 · +10 · +100) | `lc_action_group()` |
| Pojedyncza akcja | `lc_action(variant = "outline" / "solid" / "ghost")` |
| Suwak z wartością w etykiecie | `lc_slider()` |
| Liczba w pasku zamiast pudełka | `lc_readouts()` + `lc_readout()` |
| Jedno zdanie pod wykresem | `lc_caption(tone = "ok" / "info")` |
| Wykres z wysokością z proporcji | `lc_plot()`, dwa obok siebie: `lc_plots()` |
| Pusty stan | `lc_empty()` |
| Porównanie bez wykresu | `lc_compare_rows()` |
| Demonstracja krokowa | `lc_step_nav()` + `lc_step_text()` |

Zasady:

1. Sterowanie stoi nad wykresem w jednym pasku, który zawija się sam. Bez
   `fluidRow(column(4), column(8))`.
2. Serie akcji to jeden segment; reset to ikona (`lc_action(icon = "reset",
   variant = "ghost")`).
3. Radio z ≤ 4 krótkimi opcjami to segment. Segmenty wykluczające się (np.
   zmienna w wierszach i kolumnach) łączy `exclusive_with`.
4. Odczyty (`lc_readout()`) zastępują `lc_stat_box()` w widgetach. Gdy
   kolor odczytu jest kolorem serii (`swatch = TRUE`), odczyt zastępuje
   legendę ggplot.
5. Żaden wykres nie ma tytułu ani podtytułu (`labs(title / subtitle)`,
   `ggtitle()`, tytuły `plot_annotation()`). Opis idzie do tytułu panelu,
   a wyniki liczbowe do `lc_readout()` albo `lc_caption()`.
   Wykresy rysuje ragg krojem IBM Plex Sans z `R/fonts/` (ten sam co strona);
   x̄, p̂, grekę i indeksy dolne (μ₁, σ, β₀) piszemy wprost w Unicode.
6. Suwak bez podziałki i dymka; wartość w etykiecie, min i max pod torem.
7. Feedback pod wykresem to jedno zdanie `lc_caption()`. `lc_feedback()`
   zostaje dla treści w toku tekstu.
8. Wykres ma wysokość z proporcji (`lc_plot()`), nie `height = "250px"`.
   Serwer bez zmian: `zoom_plot_server()` rysuje w rozmiarze kontenera.
9. Rozmiary w `em`, progi z szerokości panelu (container queries), więc tryb
   rzutnika skaluje cały widget.
10. Przyciski v2 mają klasy `lc-action`, nie `lc-btn-*` ani Bootstrapa.

### Tabele

Wszystkie nowe tabele powstają przez `lc_table()` i deklaracje kolumn
`lc_col()`. Ręczne `tags$table(class = "lc-table …")`, `renderTable()`
i `DT::datatable()` wycofujemy: `renderTable()` zastępuje
`renderUI(lc_table(...))`. Inline `font-size` usuwamy.

| Potrzeba | Komponent |
|---|---|
| Tabela z deklaracją kolumn | `lc_table(df, cols)` + `lc_col()` |
| Jedna tabela szeroko, dwie wąsko | `lc_table_split()` |
| Podgląd surowych danych | `lc_table_preview()` |
| Stronicowanie | `lc_table(page_size = )`; duże zbiory z `page_input` |
| Tabela krzyżowa z sumami | `lc_crosstab()` |
| Pusta tabela | `lc_table_empty()` |
| Liczba w komórce / tekście | `lc_num()` / `lc_fmt()` |
| Wartość p | `lc_pval()` |

Zasady:

1. Liczby: kropka dziesiętna i zwykły minus `-` (jak w R i jamovi), bez
   końcowych zer, mono z cyframi tabelarycznymi.
2. Kolumny z liczbami są wyśrodkowane. `lc_num()` dopełnia brakujące cyfry
   niewidocznymi zerami (z lewej do najdłuższej części całkowitej w kolumnie,
   z prawej do liczby miejsc po kropce), więc kropki i jedności stoją w jednej
   linii. Tekst i nagłówki wierszy są wyrównane do lewej.
3. Tabela liczbowa nie rozciąga się na pełną szerokość (`fit`); tabele
   tekstowe zajmują 100%.
4. Skróty nagłówków (`short`, `desc` w `lc_col()`) zawsze mają widoczną
   legendę nad tabelą.
5. Sumy: wiersz w `foot` (gruba linia nad nim), kolumna sum w tabeli
   krzyżowej z cienką linią pionową.
6. W tabeli krzyżowej zdanie nad tabelą mówi, względem czego liczono
   procenty; podstawa i komórka opisywana w tekście są wyróżnione.
7. Wąski kontener, w tej kolejności: krótsze etykiety z legendą, podział na
   dwie tabele (`lc_table_split()`), karty lub ostatnia kolumna pod wierszem
   dla tabel tekstowych (`narrow = "cards" / "stack-last"`), przewijanie tylko
   dla danych surowych (`scroll = TRUE`). Tekstu nie zmniejszamy.
8. Stronicowanie: `lc_table(page_size = 10)` przełącza strony w przeglądarce
   (w HTML są wszystkie wiersze; tylko małe tabele). Duże zbiory renderują
   jedną stronę na serwerze: `lc_table(..., page = input$x_page,
   page_input = "x_page")`.
9. Stany komórek i kolumn: `is-new` (dodane w bieżącym kroku), `is-best`,
   `is-dim` (bez interpretacji), `is-target`, `is-base`.
10. Do 3 liczb o jednym obiekcie: `lc_readout()` w pasku. Co najmniej
   2 obiekty × 2 miary: tabela.
11. Tabela interaktywna stoi w `figure_panel()`. Tabela referencyjna
    stoi w toku tekstu (`lc_table(..., prose = TRUE, caption = ...)`).

`lc_table_region()` zostaje dla tabel jeszcze niezmigrowanych; nowe tabele
korzystają z `lc_table(..., scroll = TRUE)`.

Tekst pomocniczy (`--upwr-ink-subtle`) ma w jasnym motywie kolor `#6e665c`
(kontrast 4,5:1 na białym tle).

## Bloki treści v2

Hierarchię niesie typografia: etykiety, numeracja, odstępy, kreski. Tło mają
tylko widgety (`figure_panel()`) i pułapki (`lc_warn()`). Marginesu bocznego
nie ma; treść stoi w jednej kolumnie.

| Blok | Komponent |
|---|---|
| Akapit | `lc_p()` |
| Notka z etykietą (Uwaga, Przykład, Jak czytać, Zasada…) | `lc_note(label, ...)`; „Zasada”: `rule = TRUE` |
| Wzór | `lc_formula_box()` (bez tła) |
| Pułapka | `lc_warn(label, ...)` |
| Podsumowanie sekcji | `lc_recap(...)` |
| Widget | `figure_panel()` |
| Treść zwinięta: rozwiązanie, odpowiedź, rozwinięcie opcjonalne | `lc_more(label, ...)` |
| Podsekcja z numerem | `lc_h3(title, num = "1")` |
| Śledzona zmienna | `lc_tracker(label, stats)` |
| Spis przykładów | `lc_index(items)` |
| Status widgetu (opis kroku, wynik testu) | `lc_status()` + `lc_verdict()` w `renderUI()` wewnątrz panelu |
| Przełączniki drugorzędne (np. hipotezy) | `lc_chips()` |
| Pogrubienie / kursywa przed interpunkcją | `b_()` / `em_()` |

Zasady:

1. Notki się nie zwijają. Zwijamy tylko rozwiązania, odpowiedzi i opcjonalne
   rozwinięcia (`lc_more()`). Rozwiązanie ćwiczenia to
   `lc_more("Rozwiązanie", uiOutput(...))`, bez przycisku „Pokaż rozwiązanie”;
   przycisk zostaje tylko wtedy, gdy odsłania coś w samym widgecie (np.
   odpowiedzi w tabeli).
2. Etykieta stoi w wiszącej kolumnie notki, nie w ramce. Pogrubione wstępy typu
   `tags$strong("Przykład:")` na początku akapitu zamieniamy na
   `lc_note("Przykład", ...)`.
3. Najwyżej jedna `lc_note(rule = TRUE)` i jedna `lc_warn()` na sekcję `lc_h2()`.
   Pułapka jest na błędy, które student realnie popełnia.
4. `lc_caption()` to jedno zdanie z kropką statusu pod wykresem; `lc_status()`
   to dłuższy opis kroku lub wynik testu. Oba stoją wewnątrz panelu, bez
   osobnej ramki pod widgetem. Kolor niesie tylko werdykt
   (`lc_verdict(type = "ok" | "warning" | "danger")`), nie tło.
5. Kroki demonstracji: `lc_step_widget()` (pasek z nazwami) albo
   `lc_step_nav()` (kropki); zob. „Widgety krokowe”. Opcje dodatkowe obok
   kroków: `lc_chips()`.
6. Bez emoji w tytułach, listach, trackerze i statusach. Kod przykładu (A1, B2)
   trafia do plakietki panelu albo do `lc_index()`.
7. Podsekcje bez kursywy i ręcznej numeracji: `lc_h3("…", num = "1")`.
8. Liczby w trackerze i statusach w mono, z kropką dziesiętną (`lc_fmt()`).

Dawne `margin_callout()`, `inline_callout()`, `margin_note()`
i `margin_code_note()` działają dalej, ale renderują się jako `lc_note()`.
W nowym kodzie ich nie używamy.

Zakazane wzorce (uzupełnienie):

- `inline_callout()` i zwijane `tags$details` dla notek krótszych niż trzy
  zdania;
- emoji w tytułach, listach i statusach;
- `lc_feedback()` jako osobna ramka pod `figure_panel()`; statyczne
  `lc_feedback()` w toku tekstu zastępują `lc_note()` albo `lc_warn()`;
- więcej niż jedna `lc_warn()` i jedna `lc_note(rule = TRUE)` na sekcję.

## Kolumna treści

Treść rozdziału stoi w jednej kolumnie o szerokości `--lc-col` (51.25em, ok.
820 px przy zwykłym rozmiarze tekstu; skaluje się z trybem rzutnika). Tekst,
notki, tabele i panele mają tę samą szerokość; prawego marginesu nie ma.
`width_mode = "text"` i `"wide"` dają ten sam panel na całą szerokość kolumny,
`"compact"` dopasowuje panel do treści. Akapity i notki są justowane
z dzieleniem wyrazów (`lang="pl"`), a na wąskim ekranie wyrównane do lewej.
`lc_chapter_next()` to blok w treści, wyrównany do prawej.

## Widgety krokowe

Dwa wzorce, wybierane per widget:

- `lc_step_widget()` — pasek kroków z numerami i nazwami (≤ 3 słowa),
  wykres o stałej proporcji, opis kroku i nawigacja „‹ Wstecz” / „Dalej ›”,
  strzałki ← →. Gdy nazwy kroków niosą narrację (np. budowa histogramu).
  Serwer: `s <- lc_step_server(id, input); s$step()`; opis w
  `output$<id>_text` jako tekst inline; kontrolka aktywna od kroku k w
  `lc_step_from(k, ...)` (wcześniej wyszarzona).
- `lc_step_nav()` + `lc_step_text()` — kropki i „Dalej”, gdy liczy się
  miejsce (np. krótka sekwencja tabeli).

Zasady wspólne: kroki od 1 (bez pustego kroku startowego, chyba że widget
celowo zaczyna od pustego stanu), zmiana kontrolki nie zmienia kroku, reset
wraca do kroku 1, bez osobnego licznika „Krok X z N” na szerokim ekranie.

Gramatyka wykresu krokowego (`R/theme_upwr.R`): każdy element ma w danym
kroku jedną rolę — `data` (niebo), `group` (bursztyn, druga kategoria),
`new` (burgund, tylko w kroku wprowadzenia), `known` (grafit, z wcześniejszych
kroków), `background` (niebo, mocno przezroczyste). Kontury słupków i pudełek
czarne (`step_result()`), linie pomocnicze przerywane (`step_line()`), stała
rama osi we wszystkich krokach (`step_frame()`), etykiety przy elementach
(`step_label()`), bez tytułów i legend.
