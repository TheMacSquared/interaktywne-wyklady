# Kontrakt Designu

Ten dokument opisuje docelowy system dla interaktywnych wykładów z analizy
ryzyka. Jest adaptacją kontraktu ze `statystyka/R/`; nowy kod ma trzymać się
tych reguł bez warstw kompatybilności.

## Zakres

Kontrakt dotyczy wszystkich aplikacji w `analiza-ryzyka/` opartych o
`lecture_page()`. Pierwszą aplikacją referencyjną jest `01-jezyk-ryzyka/`.

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
| Wzór z adnotacjami | `risk_annotated_formula()` |
| Metryki i statystyki | `lc_stat_grid()` + `lc_stat_box()` |
| Dynamiczny feedback | `lc_feedback()` |
| Notka marginalna | `margin_callout()` albo `margin_note()`; w skrypcie `risk_note()` |
| Notka z kodem | `margin_code_note()` |
| Przejście do następnego rozdziału | `lc_chapter_next()` |

### Wzór z adnotacjami

`risk_annotated_formula(items, ops)` pokazuje wzór policzony na konkretnych liczbach: w pierwszym rzędzie symbole, w drugim te same wyrazy jako liczby, a pod nimi opisy połączone strzałkami. Zamiast rzędu kafelków ze statystykami i osobnego pola z komentarzem.

- `items` to lista `list(symbol, value, note, color)`, jeden element na wyraz; `ops` to znaki między wyrazami (o jeden mniej), np. `c("=", "×")`.
- `note`: jedno–dwa krótkie zdania, np. „12 na 100 zmian z przegrzaniem”; wyjaśnienie mianownika mieści się w notce, nie w osobnym callout.
- `color` łączy wyraz z jego notą; te same kolory, co w reszcie przykładu.
- Używamy, gdy chcemy pokazać, skąd każda liczba wzoru się bierze. Nie używamy do wzorów ogólnych (wtedy `risk_formula()`).

### Oznaczenia w skrypcie (wykłady 02–10)

Proza w `body` nie powinna iść ciągiem akapitów. Akapit, który pełni jedną z funkcji poniżej, dostaje odpowiedni element. Słowa zostają; zmienia się tylko oprawa.

| Funkcja fragmentu | Helper | Uwagi |
|---|---|---|
| Wniosek, który ma zostać po sekcji | `risk_keypoint()` | 2–3 na rozdział, nie więcej; pole `takeaway` renderuje się tak samo |
| Typowy błąd | `risk_pitfall()` | w miejscu, gdzie tekst o nim mówi; pole `pitfall` sekcji lub rozdziału też |
| Odczyt widgetu, komentarz do rozwiązanego przykładu | `risk_reading()` | etykieta „Jak czytać wynik”; inną daje `risk_box("reading", label, text)` |
| Twierdzenie wynikające z definicji | `risk_property(name, text)` | osobno od `risk_definition()` |
| Procedura w krokach | `risk_steps(...)` | numer kroku rysuje plakietka; tekst kroku bez „Krok pierwszy:” |
| Zdanie objaśniające wzór | `risk_formula_note()` | tuż pod `risk_formula()` |
| Wyliczenie w prozie | `risk_list()` | pole `bullets` też; ta sama typografia co akapit |
| Dygresja, analogia, zapowiedź | `risk_note(label, text)` | margines; widoczna także na wąskim ekranie; do ~300 znaków |
| Tabela | `risk_table(header, rows)` | zamiast ręcznego `tags$table` |
| Pojęcie wprowadzane w tekście | `[[termin]]` w stringu | kolor i kursywa, nie pogrubienie; tylko przy pierwszym użyciu |

Ćwiczenia w `risk_assessment_ui()` przyjmują pole `type` (np. „Bananpol”, „Transfer”), które pokazuje się jako plakietka; tekst zadania zaczyna się wtedy od treści, bez prefiksu. Rozdział z co najmniej dwiema sekcjami dostaje pod nagłówkiem mapę „W tym rozdziale”.

TOC wykrywa tylko sekcje tworzone przez `lc_h2()` albo zgodne z atrybutem `data-lc-section`.

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
