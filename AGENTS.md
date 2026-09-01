# AGENTS.md - Instrukcje dla projektu Interaktywne Wykłady

## Kontekst projektu

- **Cel:** interaktywne narzędzia R Shiny do nauczania przedmiotów ilościowych.
- **Struktura:** każdy przedmiot ma własny katalog na najwyższym poziomie repo.
- **Obecne przedmioty:** `statystyka/` oraz startowy katalog `ekonometria/`.
- **Odbiorcy:** studenci na zajęciach akademickich.
- **Język interfejsu:** polski.
- **Język kodu:** angielski dla nazw zmiennych i funkcji.

## Organizacja repo

```text
interaktywne-wyklady/
├── statystyka/
│   ├── R/
│   ├── scripts/
│   ├── README.md
│   └── */app.R
└── ekonometria/
    └── README.md
```

Materiały statystyczne są samowystarczalnym zestawem w `statystyka/`. Wspólne komponenty statystyki są w `statystyka/R/`, a narzędzia pomocnicze w `statystyka/scripts/`.

Katalog `ekonometria/` jest oddzielnym projektem równoległym. Nowe aplikacje ekonometryczne powinny powstawać wewnątrz `ekonometria/`, z własnym katalogiem `R/` albo świadomie skopiowanym/adaptowanym systemem ze `statystyka/R/`.

## Konwencje kodowania

Kanoniczne reguły designu dla istniejącej statystyki są w `statystyka/R/DESIGN_CONTRACT.md`. Przy pracy nad wykładami statystycznymi traktuj ten dokument jako nadrzędny kontrakt.

### Struktura aplikacji Shiny

Każda aplikacja wykładowa ma używać systemu layoutu z właściwego katalogu przedmiotu, np. `statystyka/R/lecture_layout.R` dla statystyki. Nie twórz nowych aplikacji na `fluidPage()`, `navbarPage()`, `sidebarLayout()` ani `bslib::page_*()`.

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
subject_root <- dirname(app_dir)
project_root <- subject_root

source(file.path(project_root, "R", "palette.R"),        local = TRUE)
source(file.path(project_root, "R", "theme_upwr.R"),     local = TRUE)
source(file.path(project_root, "R", "shared.R"),         local = TRUE)
source(file.path(project_root, "R", "lecture_layout.R"), local = TRUE)

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

server <- function(input, output, session) {
  lc <- lecture_server(.chapters, input, output, session)

  ch1_server(input, output, session)
  ch2_server(input, output, session)
}

shinyApp(ui = ui, server = server)
```

Istniejące aplikacje statystyczne używają `project_root <- dirname(app_dir)` i ten wzorzec pozostaje poprawny po przeniesieniu do `statystyka/`.

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

## Typy aplikacji

| Typ | Wzorzec | Opis |
|-----|---------|------|
| Krok po kroku | `typy-danych` | przyciski kroków, wyjaśnienia |
| Eksploracja | `rozklady-prawdopodobienstwa` | slidery parametrów, dynamiczny wykres |
| Symulacja | `symulacje-statystyczne` | zbieranie danych, statystyki na żywo |
| Case study | `case-studies` | kompletna analiza od danych do wniosku |
