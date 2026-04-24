# CLAUDE.md - Instrukcje dla projektu Interaktywne

## Kontekst projektu

- **Cel:** Interaktywne narzędzia R Shiny do nauczania statystyki
- **Odbiorcy:** Studenci na zajęciach akademickich
- **Język interfejsu:** Polski
- **Język kodu:** Angielski (nazwy zmiennych, funkcji)

## Konwencje kodowania

Kanoniczne reguły nowego designu są w `R/DESIGN_CONTRACT.md`. Przy pracy nad zmigrowanymi wykładami traktuj ten dokument jako nadrzędny kontrakt.

### Struktura aplikacji Shiny

Każda aplikacja wykładowa ma używać wyłącznie nowego systemu layoutu z `R/lecture_layout.R`.
Nie twórz nowych aplikacji na `fluidPage()`, `navbarPage()`, `sidebarLayout()` ani `bslib::page_*()`.

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

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  lc <- lecture_server(.chapters, input, output, session)

  ch1_server(input, output, session)
  ch2_server(input, output, session)
}

shinyApp(ui = ui, server = server)
```

### Nazewnictwo

| Element | Konwencja | Przykład |
|---------|-----------|----------|
| Foldery aplikacji | kebab-case | `box-plot-builder`, `srednia-vs-mediana` |
| Funkcje R | snake_case | `generate_data`, `calculate_stats` |
| Zmienne reactive | snake_case | `collected_data`, `current_step` |
| Identyfikatory UI | snake_case | `main_plot`, `step_explanation` |

## Polskie znaki w plikach R

Pisz polskie znaki **bezpośrednio jako UTF-8** (`ó`, `ą`, `ę`, `ł`, `ś`, `ż`, `ź`, `ć`, `ń`), nie jako escape'y `\uXXXX`.

```r
# DOBRZE
p("Średnia próby wynosi 42")

# ŹLE — escape'y \uXXXX
p("Średnia próby wynosi 42")
```

To dotyczy też znaków typograficznych: `≥` (nie `≥`), `—` (nie `—`), `×` (nie `×`), `μ`, `σ`, `λ`.

Powód: escape'y są nieczytelne w edytorze, utrudniają `grep`, sed i Edit, dają nieczytelne diffy w git. Środowisko (Linux/WSL, R ≥ 4.0, locale UTF-8) obsługuje surowe UTF-8 bez problemu.

## Preferowane pakiety R

### Podstawowe (używaj zawsze)

```r
library(shiny)      # Framework aplikacji
library(ggplot2)    # Wizualizacje
library(dplyr)      # Manipulacja danych
```

### Statystyka (preferuj te pakiety)

```r
library(rstatix)    # Testy statystyczne (zamiast base R)
library(broom)      # Porządkowanie wyników modeli
library(lmtest)     # Testy diagnostyczne regresji
```

### Bayesowskie (dla metody-bayesowskie/)

```r
library(BayesFactor)  # ttestBF, anovaBF, correlationBF, contingencyTableBF
library(rstanarm)     # stan_glm (regresja bayesowska, MCMC pod maską)
```

### Mapowanie funkcji statystycznych

| Zadanie | Używaj | Zamiast |
|---------|--------|---------|
| Test t | `rstatix::t_test()` | `t.test()` |
| Test Wilcoxona | `rstatix::wilcox_test()` | `wilcox.test()` |
| Test Shapiro-Wilka | `rstatix::shapiro_test()` | `shapiro.test()` |
| Test Levene'a | `rstatix::levene_test()` | `car::leveneTest()` |
| Korelacja | `rstatix::cor_test()` | `cor.test()` |
| ANOVA | `rstatix::anova_test()` | `aov()` |
| Porządkowanie modeli | `broom::tidy()`, `broom::glance()` | ręczne wyciąganie |

## Pogrubienia w tekście (bold)

Używaj `tags$strong()` / `strong()` **oszczędnie** — tylko jako wizualnej nawigacji, nie jako emfazy. Za dużo boldów rozprasza i zmniejsza ich skuteczność.

**ZOSTAW bold** gdy to:
- Krótka etykieta z dwukropkiem: `strong("Problem:")`, `strong("Zasada:")`, `strong("Uwaga:")`, `strong("Wniosek:")`, `strong("Wskazówka:")`, `strong("Interpretacja:")`, `strong("Przykład:")`, `strong("Krok 1:")`
- Numerowany marker listy: `strong("1.")`, `strong("2.")`
- Jedno-słowo werdykt quiz-feedback: `strong("Dokładnie!")`, `strong("Nie do końca.")`
- Krótki status-tag na początku callout-u: `strong("Dobry zbiór!")`, `strong("KRYTYCZNE")`

**NIE używaj bold** dla:
- Całych zdań opisowych — wystarczy, że są w `callout-*` (tam jest już wizualny sygnał)
- Emfazy w środku paragrafu narracji — lepiej przeformułować zdanie
- Markdown `**tekst**` w narracji — zwykły tekst

**Pattern `strong("Label: treść")`** → rozbij na `strong("Label:"), " treść"` — pogrubiona ma być tylko etykieta.

Uwaga: globalny CSS (`R/shared_styles.css`) ustawia `strong { font-weight: 600 }` — boldy są już wyciszone do półgrubej wagi. Nie próbuj tego obchodzić przez `style="font-weight: 700"`.

## Styl wizualizacji

```r
# Domyślne dla całej aplikacji, po source() palety i motywu
lc_apply_ggplot_defaults()

# Motyw i paleta UPWr
theme_upwr()
upwr_accent
upwr_secondary
upwr_reference
upwr_cat_n(4)

# Etykiety - zawsze w języku polskim
labs(
  title = "Tytuł wykresu",
  x = "Oś X (jednostki)",
  y = "Oś Y (jednostki)"
)
```

## Struktura README dla nowych aplikacji

```markdown
# 📦 Tytuł Aplikacji

Jednolinijkowy opis.

## 📋 Wymagania

- R (wersja ≥ 4.0)
- Pakiety R: `shiny`, `ggplot2`, `dplyr`

## 🚀 Uruchamianie

[kod R]

## 📚 Jak używać na zajęciach

### Scenariusze pedagogiczne
[opis scenariuszy]

### Interaktywne elementy
[opis kontrolek]

## 🎯 Koncepcje pedagogiczne

### Co ilustruje to narzędzie?
[lista koncepcji]

## 💡 Scenariusze na zajęciach
[konkretne scenariusze użycia]

## 🛠️ Techniczne szczegóły
[szczegóły implementacji]

## 🐛 Rozwiązywanie problemów
[FAQ]
```

## Typy aplikacji (wzorce do naśladowania)

| Typ | Wzorzec | Opis |
|-----|---------|------|
| Krok po kroku | `box-plot-builder` | Przyciski kroków, wyjaśnienia |
| Eksploracja | `distribution-explorer` | Slidery parametrów, dynamiczny wykres |
| Symulacja | `losowanie_spoznienia` | Zbieranie danych, statystyki na żywo |
| Gra | `gra-estymacja` | Rundy, punkty, feedback |
| Porównanie | `srednia-vs-mediana` | Scenariusze, dodawanie outlierów |

## Elementy UI (standardowe)

```r
# Przyciski akcji
actionButton("action", "Etykieta", class = "btn-primary", width = "100%")

# Przyciski kroków
actionButton("step1", "1. Nazwa kroku", class = "btn-outline-primary", width = "100%")

# Slidery
sliderInput("param", "Etykieta:",
            min = 0, max = 100, value = 50, step = 1)

# Checkbox
checkboxInput("show_stats", "Pokaż statystyki", value = FALSE)

# Warunkowe panele
conditionalPanel(
  condition = "input.show_stats == true",
  verbatimTextOutput("stats")
)
```

## Rozszerzanie projektu

### Dodawanie nowej aplikacji

1. **Utwórz folder** w konwencji `nazwa-aplikacji/`
2. **Skopiuj wzorzec** z aplikacji używającej `lecture_page()`
3. **Zaimplementuj logikę** z komponentami `lc_*`, `figure_panel()` i `margin_callout()`
4. **Dodaj README.md** ze scenariuszami pedagogicznymi
5. **Zaktualizuj główny README.md** - dodaj aplikację do tabeli

## Debugowanie

Gdy coś nie działa:

1. **Diagnozuj błąd** - sprawdź konsolę R
2. **Zaproponuj opcje naprawy** - minimum 2 alternatywy
3. **NIE porzucaj pakietu** na rzecz "prostszego" bez uzasadnienia

## Język

- **UI:** Polski (tytuły, etykiety, wyjaśnienia)
- **Kod:** Angielski (nazwy zmiennych, funkcji)
- **Komentarze:** Polski lub angielski (konsekwentnie w pliku)
- **README:** Polski
