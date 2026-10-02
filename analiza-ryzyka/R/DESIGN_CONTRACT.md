# Kontrakt Designu

Ten dokument opisuje docelowy system dla interaktywnych wykładów z analizy
ryzyka. Jest adaptacją kontraktu ze `statystyka/R/`; nowy kod ma trzymać się
tych reguł bez warstw kompatybilności.

## Zakres

Kontrakt dotyczy wszystkich aplikacji w `analiza-ryzyka/` opartych o
`lecture_page()`. Wszystkie wykłady 01–10 zapisują treść w `modules/block.R`
jako listę konfiguracyjną renderowaną przez `risk_block_chapters()` z `R/risk_block.R`.

## Shell Aplikacji

Każdy wykład używa `lecture_page()` z `R/lecture_layout.R`. Rozdziały są listami tworzonymi przez `lecture_chapter()` albo jawne listy z polami `id`, `num`, `title`, `content`.

Kanoniczny `app.R`:

```r
source(file.path(project_root, "R", "palette.R"),        local = TRUE)
source(file.path(project_root, "R", "theme_upwr.R"),     local = TRUE)
source(file.path(project_root, "R", "shared.R"),         local = TRUE)
source(file.path(project_root, "R", "lecture_layout.R"), local = TRUE)
source(file.path(project_root, "R", "risk_block.R"),     local = TRUE)
source(file.path(app_dir, "modules", "block.R"),         local = TRUE)

lc_apply_ggplot_defaults()

.chapters <- nazwa_chapters   # risk_block_chapters(nazwa_block)

ui <- lecture_page(
  lecture_id    = "nazwa-folderu",
  lecture_num   = "01",
  lecture_title = "Tytuł wykładu",
  module_label  = "Moduł I",
  chapters      = .chapters
)

server <- function(input, output, session) {
  lc <- lecture_server(.chapters, input, output, session)
  nazwa_server(input, output, session)
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
| Wzór z adnotacjami | `risk_annotated_formula()` |
| Metryki i statystyki | `lc_stat_grid()` + `lc_stat_box()` |
| Dynamiczny feedback | `lc_feedback()` |
| Notka marginalna | `margin_callout()` albo `margin_note()` |
| Notka z kodem | `margin_code_note()` |
| Przejście do następnego rozdziału | `lc_chapter_next()` |

### Wzór z adnotacjami

`risk_annotated_formula(items, ops)` pokazuje wzór policzony na konkretnych liczbach: w pierwszym rzędzie symbole, w drugim te same wyrazy jako liczby, a pod nimi opisy połączone strzałkami. Zamiast rzędu kafelków ze statystykami i osobnego pola z komentarzem.

- `items` to lista `list(symbol, value, note, color)`, jeden element na wyraz; `ops` to znaki między wyrazami (o jeden mniej), np. `c("=", "×")`.
- `note`: jedno–dwa krótkie zdania, np. „12 na 100 zmian z przegrzaniem”; wyjaśnienie mianownika mieści się w notce, nie w osobnym callout.
- `color` łączy wyraz z jego notą; te same kolory, co w reszcie przykładu.
- Używamy, gdy chcemy pokazać, skąd każda liczba wzoru się bierze. Nie używamy do wzorów ogólnych (wtedy `risk_formula()`).

TOC wykrywa tylko sekcje tworzone przez `lc_h2()` albo zgodne z atrybutem `data-lc-section`. Spis sekcji rozdziału jest dostępny w sidebarze; nie powielamy go pod nagłówkiem rozdziału.

## Nagłówki Rozdziałów

Każdy rozdział ma dwa nagłówki o rozdzielonych rolach. Nie mieszamy ich i nie zamieniamy miejscami.

| Miejsce | Rola | Źródło w kodzie |
|---|---|---|
| Sidebar, karta „następny rozdział” | Tag — lokalizacja pojęciowa | `lecture_chapter(title = )`, `lc_chapter_next(title = )`; w blokach konfiguracyjnych pole `title` |
| Duży tytuł obok numeru | Hak — zaciekawienie | `lc_chapter_hero(title = )`; w blokach konfiguracyjnych pole `hook` |
| Pierwsze zdanie leadu | Most — łączy hak z pojęciem | `lc_chapter_hero(lead = )`; pole `lead` |

Tag:

- nazwa pojęcia w brzmieniu z definicji rozdziału (`risk_definition()`), jeśli rozdział ma definicję; w przeciwnym razie termin, który trafia do ściągi;
- 1–3 słowa, forma rzeczownikowa, bez pytań i bez kropki;
- rozdziały podsumowujące mają tagi funkcjonalne: „Ściąga”, „Quiz”, „Ćwiczenia” albo „Ściąga i sprawdzenie”;
- test: student szukający pojęcia przed kolokwium trafia do rozdziału po samym sidebarze.

Hak:

- twierdzenie, nie pytanie; do około 7 słów;
- każde słowo zrozumiałe przed lekturą rozdziału: konkret z historii Bananpolu (skórka, paleta, alarm, wentylator) albo codzienny język;
- nie używa terminów wprowadzanych w tym rozdziale ani żargonu matematycznego („mianownik”, „model”, „zdarzenie”);
- zawiera napięcie lub zaskoczenie, które rozdział rozwiązuje;
- w blokach konfiguracyjnych kropkę na końcu dopisuje `risk_chapter_from_config()`.

Lead:

- pierwsze zdanie nazywa pojęcie z tagu i wiąże je z sytuacją z haka.

Przykład:

```text
sidebar:  Przestrzeń zdarzeń
tytuł:    Szansę można znać, zanim coś się stanie.
lead:     Nie zawsze potrzebujemy rejestru wypadków. Gdy losujemy paletę do
          kontroli, […] wystarczy wypisać przestrzeń zdarzeń […]
```

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
Rscript analiza-ryzyka/scripts/check_design_contract.R
```

Na razie skrypt raportuje naruszenia informacyjnie. Tryb twardy:

```sh
Rscript analiza-ryzyka/scripts/check_design_contract.R --strict
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

## Widgety v2 i tabele v2

Nowe i przebudowywane widgety oraz tabele korzystają z komponentów v2
(sekcja „WIDGETY V2 I TABELE V2” w `R/lecture_layout.R`, style w
`R/shared_styles.css`, logika klienta w `R/lc_widgets.js`). Sekcja jest
identyczna we wszystkich kursach; zmiany wprowadzamy równolegle.

Panel widgetu włącza style v2 przez `figure_panel(..., v2 = TRUE)`. Bez tego
argumentu panel wygląda jak dotąd, więc migracja idzie widget po widgecie.
W analizie ryzyka `risk_widget_panel(..., v2 = TRUE)` składa pasek, wykres
i podpis.

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
5. Wykresy w widgetach nie mają `labs(title / subtitle)`; treść idzie do
   tytułu panelu.
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
8. Stany komórek i kolumn: `is-new` (dodane w bieżącym kroku), `is-best`,
   `is-dim` (bez interpretacji), `is-target`, `is-base`.
9. Do 3 liczb o jednym obiekcie: `lc_readout()` w pasku. Co najmniej
   2 obiekty × 2 miary: tabela.
10. Tabela interaktywna stoi w `figure_panel(v2 = TRUE)`. Tabela referencyjna
    stoi w toku tekstu (`lc_table(..., prose = TRUE, caption = ...)`).

`lc_table_region()` zostaje dla tabel jeszcze niezmigrowanych; nowe tabele
korzystają z `lc_table(..., scroll = TRUE)`.

Tekst pomocniczy w komponentach v2 (etykiety odczytów, legenda, notki) ma
w jasnym motywie kolor `#6e665c` (kontrast 4,5:1); globalny token
`--upwr-ink-subtle` pozostaje bez zmian.
