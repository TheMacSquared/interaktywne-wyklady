# AGENTS.md - Instrukcje dla projektu Interaktywne Wykłady

## Kontekst projektu

- **Cel:** interaktywne wykłady do nauczania przedmiotów ilościowych, jako strony statyczne (bez serwera R).
- **Struktura:** każdy przedmiot ma własny katalog na najwyższym poziomie repo.
- **Obecne przedmioty:** `statystyka/` i `analiza-ryzyka/`.
- **Odbiorcy:** studenci na zajęciach akademickich.
- **Język interfejsu:** polski.
- **Język kodu:** angielski dla nazw zmiennych i funkcji.

## Organizacja repo

```text
interaktywne-wyklady/
├── shared/               # wspólny rdzeń statystyki i analizy ryzyka
├── statystyka/
│   ├── R/                # course.R + pliki własne kursu
│   ├── scripts/
│   ├── README.md
│   └── */app.R
├── analiza-ryzyka/
│   ├── R/                # course.R, course_styles.css + pliki własne kursu
│   └── */app.R
└── scripts/              # build_static.R, build_index.R, kontrole stron
```

Statystyka i analiza ryzyka korzystają z jednego wspólnego rdzenia w `shared/` (layout, paleta, motyw wykresów, style, biblioteki JS). Zmianę rdzenia robi się raz, dla obu kursów. Katalog `<kurs>/R/` zawiera tylko to, co własne dla kursu: `course.R` (nazwa, logo, mapa wykładów, tryb rozdziałów), ewentualny `course_styles.css` (elementy kursu i świadome odstępstwa od rdzenia) oraz pliki przedmiotowe. Opis w `shared/README.md`. Narzędzia pomocnicze statystyki są w `statystyka/scripts/`.

Statystyka i analiza ryzyka działają jako strony statyczne (bez serwera R): wszystkie 21 wykładów budują się do `dist/<kurs>/<wykład>.html` (`Rscript scripts/build_static.R <kurs>/<wykład>`, spis: `Rscript scripts/build_index.R`). Plan i stan są w `TODO.md` („Przejście na strony statyczne”), narzędzia w `scripts/` (`check_static_ready.R`, `build_static.R`, `check_static_page.R`), opis w `shared/README.md`. Nowy kod widgetów nie dodaje wyjść ani wejść serwera (`render*`, `observe*`, `input$…`): to, co zależy od ruchu studenta, liczy JS (silnik ryciny `lcSF`, `shared/lc_stat_figures.js`), resztę R przy budowaniu (`lc_sf_spec()`, wykres ggplot na stałych danych: `lc_sf_ggplot()`, treść wybierana kontrolką: `lc_variants()`, odsłaniana przyciskiem: `lc_reveal()`). Kodu serwera (`server`, `chN_server`, `lecture_server()`, `zoom_plot_server()`) już nie ma: `app.R` opisuje tylko treść.

Ostatnią wersję z aplikacjami Shiny (z hubem, kursem „Statystyka 2” i archiwum ekonometrii) oznacza tag `shiny-final`.

## Konwencje kodowania

Kanoniczne reguły designu są w `shared/DESIGN_CONTRACT.md`. Przy pracy nad wykładami traktuj ten dokument jako nadrzędny kontrakt; reguły własne analizy ryzyka uzupełnia `analiza-ryzyka/R/DESIGN_CONTRACT.md`.

### Struktura wykładu

Każdy wykład statystyki i analizy ryzyka używa systemu layoutu z `shared/lecture_layout.R`. `app.R` wykładu statystyki i analizy ryzyka opisuje treść (`ui`); nie ma funkcji `server` ani `shinyApp()`, bo strona jest statyczna (`Rscript scripts/build_static.R <kurs>/<wykład>`). Nie twórz nowych wykładów na `fluidPage()`, `navbarPage()`, `sidebarLayout()` ani `bslib::page_*()`.

```r
# Tytuł aplikacji
# Opis jednolinijkowy

library(shiny)
library(ggplot2)
library(dplyr)

# ============================================================================
# BOOTSTRAP PROJEKTU
# ============================================================================

app_dir <- .find_app_dir()
project_root <- dirname(app_dir)
shared_root <- file.path(dirname(project_root), "shared")

source(file.path(project_root, "R", "course.R"), local = TRUE)

source(file.path(shared_root, "palette.R"),        local = TRUE)
source(file.path(shared_root, "theme_upwr.R"),     local = TRUE)
source(file.path(shared_root, "shared.R"),         local = TRUE)
source(file.path(shared_root, "lecture_layout.R"), local = TRUE)

lc_apply_ggplot_defaults()

# ============================================================================
# MODUŁY
# ============================================================================

source(file.path(app_dir, "modules", "ch1_intro.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch2_topic.R"), local = TRUE)

.chapters <- list(ch1_ui, ch2_ui)

ui <- lecture_page(
  lecture_id    = "nazwa-folderu",
  lecture_num   = "01",
  lecture_title = "Tytuł wykładu",
  module_label  = "Moduł I",
  chapters      = .chapters
)
```

`project_root` to katalog kursu (`dirname(app_dir)`), a `shared_root` to `shared/` obok katalogów kursów. `.find_app_dir()` rozpoznaje katalog kursu po pliku `R/course.R`.

### Nazewnictwo

| Element | Konwencja | Przykład |
|---------|-----------|----------|
| Foldery aplikacji | kebab-case | `model-liniowy`, `typy-danych` |
| Funkcje R | snake_case | `generate_data`, `calculate_stats` |
| Zmienne reactive | snake_case | `collected_data`, `current_step` |
| Identyfikatory UI | snake_case | `main_plot`, `step_explanation` |

## Polskie znaki w plikach R

Pisz polskie znaki bezpośrednio jako UTF-8 (`ó`, `ą`, `ę`, `ł`, `ś`, `ż`, `ź`, `ć`, `ń`), nie jako escape'y `\uXXXX`.

```r
# DOBRZE
p("Średnia próby wynosi 42")

# ŹLE
p("\\u015arednia próby wynosi 42")
```

To dotyczy też znaków typograficznych: `≥`, `—`, `×`, `μ`, `σ`, `λ`.

## Preferowane pakiety R

### Podstawowe

```r
library(shiny)
library(ggplot2)
library(dplyr)
```

### Statystyka

```r
library(rstatix)
library(broom)
library(lmtest)
```

### Ekonometria

Dla nowych wykładów ekonometrycznych preferuj pakiety dobrze wspierające modele i diagnostykę:

```r
library(broom)
library(lmtest)
library(sandwich)
library(modelsummary)
```

Nie dodawaj ciężkich zależności bez potrzeby dydaktycznej.

## Mapowanie funkcji statystycznych

| Zadanie | Używaj | Zamiast |
|---------|--------|---------|
| Test t | `rstatix::t_test()` | `t.test()` |
| Test Wilcoxona | `rstatix::wilcox_test()` | `wilcox.test()` |
| Test Shapiro-Wilka | `rstatix::shapiro_test()` | `shapiro.test()` |
| Test Levene'a | `rstatix::levene_test()` | `car::leveneTest()` |
| Korelacja | `rstatix::cor_test()` | `cor.test()` |
| ANOVA | `rstatix::anova_test()` | `aov()` |
| Porządkowanie modeli | `broom::tidy()`, `broom::glance()` | ręczne wyciąganie |

## Pogrubienia w tekście

Używaj `tags$strong()` / `strong()` oszczędnie, tylko jako wizualnej nawigacji, nie jako emfazy.

Zostaw bold dla krótkich etykiet z dwukropkiem, numerowanych markerów listy, krótkich werdyktów quizowych i statusów na początku calloutu.

Nie używaj bold dla całych zdań opisowych, emfazy w środku narracji ani markdown `**tekst**` w tekstach wykładowych.

Pattern `strong("Label: treść")` rozbij na `strong("Label:"), " treść"`.

## Styl wizualizacji

```r
lc_apply_ggplot_defaults()

theme_upwr()
upwr_accent
upwr_secondary
upwr_reference
upwr_cat_n(4)

labs(
  title = "Tytuł wykresu",
  x = "Oś X (jednostki)",
  y = "Oś Y (jednostki)"
)
```

Etykiety wykresów zawsze pisz po polsku.

## Struktura README dla nowych aplikacji

```markdown
# Tytuł aplikacji

Jednolinijkowy opis.

## Wymagania

- R (wersja >= 4.0)
- Pakiety R: `shiny`, `ggplot2`, `dplyr`

## Uruchamianie

[kod R]

## Jak używać na zajęciach

### Scenariusze pedagogiczne
[opis scenariuszy]

### Interaktywne elementy
[opis kontrolek]

## Koncepcje pedagogiczne

### Co ilustruje to narzędzie?
[lista koncepcji]

## Techniczne szczegóły

[szczegóły implementacji]
```

## Punkty stop

Punkt stop to miejsce, w którym prowadzący zatrzymuje się przy obrazie
i opowiada. Rodzaje (scena, piaskownica, galeria, symulacja, przypadek,
pojęcie, przykłady) i momenty (wprowadzenie, przypadek brzegowy, domknięcie) są
zdefiniowane w `PUNKTY-STOP.md`. Gdy polecenie mówi „tu potrzebuję
piaskownicy” albo „galeria do …”, chodzi o te znaczenia.

## Sceny

Scena to widget, który opowiada intuicyjną i realną sytuację, zanim padnie
pojęcie; nie musi być krokowa ani losowa. Polecenie „zrób scenę do …”
realizuj według `SCENY.md` (formy, zasady wspólne, brief do akceptacji przed
kodem).

## Typy aplikacji

| Typ | Wzorzec | Opis |
|-----|---------|------|
| Krok po kroku | `typy-danych` | przyciski kroków, wyjaśnienia |
| Eksploracja | `rozklady-prawdopodobienstwa` | slidery parametrów, dynamiczny wykres |
| Symulacja | `symulacje-statystyczne` | zbieranie danych, statystyki na żywo |
| Case study | `case-studies` | kompletna analiza od danych do wniosku |
