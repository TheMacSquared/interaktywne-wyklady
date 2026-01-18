# CLAUDE.md - Instrukcje dla projektu Interaktywne

## Kontekst projektu

- **Cel:** Interaktywne narzędzia R Shiny do nauczania statystyki
- **Odbiorcy:** Studenci na zajęciach akademickich
- **Język interfejsu:** Polski
- **Język kodu:** Angielski (nazwy zmiennych, funkcji)

## Konwencje kodowania

### Struktura aplikacji Shiny

Każda aplikacja ma strukturę:

```r
# Tytuł aplikacji
# Opis jednolinijkowy

library(shiny)
library(ggplot2)
library(dplyr)
# ... inne biblioteki

# ============================================================================
# FUNKCJE POMOCNICZE
# ============================================================================

generate_xxx_data <- function(n) {
  set.seed(NULL)  # Losowe seed za każdym razem
  # ...
}

# ============================================================================
# SCENARIUSZE (jeśli aplikacja ma wiele scenariuszy)
# ============================================================================

scenarios <- list(
  nazwa = list(
    title = "Tytuł",
    generator = generate_xxx_data,
    # ... parametry
  )
)

# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(
  titlePanel("Tytuł aplikacji"),
  sidebarLayout(
    sidebarPanel(
      # Kontrolki
      width = 3
    ),
    mainPanel(
      # Wykresy i wyniki
      width = 9
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  # Reactive values
  # Observery
  # Renderowanie outputów
}

# Uruchomienie aplikacji
shinyApp(ui = ui, server = server)
```

### Nazewnictwo

| Element | Konwencja | Przykład |
|---------|-----------|----------|
| Foldery aplikacji | kebab-case | `box-plot-builder`, `srednia-vs-mediana` |
| Funkcje R | snake_case | `generate_data`, `calculate_stats` |
| Zmienne reactive | snake_case | `collected_data`, `current_step` |
| Identyfikatory UI | snake_case | `main_plot`, `step_explanation` |

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

## Styl wizualizacji

```r
# Theme dla wszystkich wykresów
theme_minimal(base_size = 14)

# Kolory standardowe
col_primary <- "#3498db"    # niebieski
col_secondary <- "#e74c3c"  # czerwony
col_success <- "#27ae60"    # zielony
col_warning <- "#f39c12"    # pomarańczowy
col_dark <- "#2c3e50"       # ciemny

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
2. **Skopiuj szablon** z istniejącej aplikacji podobnego typu
3. **Zaimplementuj logikę** zgodnie z konwencjami
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
