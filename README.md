# 📊 Interaktywne Wykłady ze Statystyki

Aplikacje R Shiny w formie interaktywnych skryptów wykładowych. Każda aplikacja to scrollowalny przewodnik z osadzonymi widgetami — student czyta narrację i eksperymentuje z danymi w kontekście.

## 📋 Wymagania

- R (wersja ≥ 4.0)
- Pakiety R: `shiny`, `ggplot2`, `dplyr`, `e1071`, `gridExtra`, `rstatix`, `broom`, `tidyr`, `lmtest`

## 🚀 Instalacja pakietów

```r
install.packages(c("shiny", "ggplot2", "dplyr", "e1071", "gridExtra"))

# Dodatkowe (dla przedzialy-ufnosci, wnioskowanie-statystyczne, regresja, zalozenia-testow)
install.packages(c("rstatix", "broom", "tidyr", "knitr", "lmtest", "sandwich"))

# Dodatkowe (dla dobre-dane)
install.packages(c("DT", "bslib", "AER", "palmerpenguins", "ISLR", "fivethirtyeight"))
```

## ▶️ Uruchamianie

```r
# Z R/RStudio
shiny::runApp("typy-danych")
shiny::runApp("rozklady-prawdopodobienstwa")
shiny::runApp("przedzialy-ufnosci")
shiny::runApp("wnioskowanie-statystyczne")
shiny::runApp("regresja")
shiny::runApp("zalozenia-testow")
shiny::runApp("case-studies")
shiny::runApp("dobre-dane")
shiny::runApp("symulacje-statystyczne")
```

## 📚 Aplikacje

| Aplikacja | Temat | Rozdziały / zakres |
|-----------|-------|--------------------|
| [typy-danych](typy-danych/) | Statystyka opisowa | 8 rozdziałów: typy danych, zmienne jakościowe, statystyki położenia, rozrzutu, kształt rozkładu, ściąga, quiz + **ćwiczenia z dropdownem kierunków** (BHP/Rolnictwo/Żywność) |
| [rozklady-prawdopodobienstwa](rozklady-prawdopodobienstwa/) | Rozkłady prawdopodobieństwa | 9 rozdziałów: od danych do prawdopodobieństwa, wartość oczekiwana i wariancja, rozkłady dyskretne, ciągłe, normalny, CTG, ściąga, quiz + **ćwiczenia z dropdownem kierunków** (BHP/Rolnictwo/Żywność) |
| [przedzialy-ufnosci](przedzialy-ufnosci/) | Przedziały ufności | 7 rozdziałów: estymacja punktowa, idea przedziałów, przedział dla średniej, proporcji, czynniki szerokości, ściąga + **ćwiczenia z dropdownem kierunków** (Edukacja/BHP/Rolnictwo/Żywność) |
| [wnioskowanie-statystyczne](wnioskowanie-statystyczne/) | Wnioskowanie statystyczne | 9 rozdziałów: logika testowania, formułowanie hipotez, jedna ilo./jako., korelacja, dwie jakościowe, dwie grupy, ANOVA, ściąga |
| [regresja](regresja/) | Regresja | 5 rozdziałów: liniowa prosta, wieloraka, logistyczna, porównanie modeli (R², AIC, BIC, RMSE), ściąga |
| [zalozenia-testow](zalozenia-testow/) | Założenia testów | 6 rozdziałów: normalność, jednorodne wariancje, założenia regresji, χ²/Fisher, mapa metod z alternatywami, ściąga |
| [case-studies](case-studies/) | Case studies | Kompletne analizy od A do Z. Każdy rozdział = jeden zbiór danych, hipotezy, analizy, wnioski. Na razie: CASchools |
| [dobre-dane](dobre-dane/) | Jakość danych | 11 zbiorów — kiedy dane nadają się do klasycznej statystyki? (CASchools, pingwiny, Tarantino, Wage, ankieta firmowa, mieszkania, studenci, powietrze, ankieta SU) + ściąga |
| [symulacje-statystyczne](symulacje-statystyczne/) | Symulacje statystyczne | 10 rozdziałów: idea resamplingowa, bootstrap CI, bootstrap jednej próby, testy permutacyjne, jackknife, cross-validation, Monte Carlo (moc + H₀), kiedy stosować?, ściąga + **ćwiczenia z dropdownem kierunków** (Rolnictwo/TŻ/BHP/Edukacja) |

## 📁 Struktura projektu

```
interaktywne-wyklady/
├── typy-danych/                    # Statystyka opisowa
│   ├── app.R                       # Główny plik: dane, kolory, CSS/JS, nawigacja
│   ├── quiz_typy_zmiennych.json    # Pytania do quizu (ch7)
│   └── modules/                    # Moduły rozdziałów
│       ├── helpers.R               # Funkcje pomocnicze (taksonomia, wykresy good/bad)
│       ├── ch1_typy.R              # 1. Typy danych
│       ├── ch2_jakosciowe.R        # 2. Zmienne jakościowe
│       ├── ch3_polozenie.R         # 3. Statystyki położenia
│       ├── ch4_rozrzut.R           # 4. Statystyki rozrzutu
│       ├── ch5_ksztalt.R           # 5. Kształt rozkładu
│       ├── ch6_sciaga.R            # 6. Ściąga
│       ├── ch7_quiz.R              # 7. Quiz (rozpoznaj typ zmiennej)
│       └── ch8_cwiczenia.R         # 8. Ćwiczenia (dropdown: BHP/Rolnictwo/Żywność)
├── rozklady-prawdopodobienstwa/    # Rozkłady prawdopodobieństwa
│   ├── app.R                       # Główny plik: kolory, CSS/JS, nawigacja
│   ├── cwiczenia/                  # Dane i opisy do ch9_cwiczenia (BHP/Rolnictwo/Żywność)
│   └── modules/
│       ├── helpers.R               # Funkcje symulacyjne, theme
│       ├── quiz_rozklady.json      # Pytania do quizu (ch8)
│       ├── ch1_most.R              # 1. Od danych do prawdopodobieństwa
│       ├── ch2_ev_var.R            # 2. Wartość oczekiwana i wariancja
│       ├── ch3_dyskretne.R         # 3. Rozkłady dyskretne (jednostajny, dwumianowy, Poissona)
│       ├── ch4_ciagle.R            # 4. Rozkłady ciągłe (histogram→PDF, jednostajny, wykładniczy)
│       ├── ch5_normalny.R          # 5. Rozkład normalny (μ/σ, reguła 68-95-99.7, z-score)
│       ├── ch6_ctg.R               # 6. Centralne Twierdzenie Graniczne
│       ├── ch7_sciaga.R            # 7. Ściąga
│       ├── ch8_quiz.R              # 8. Quiz
│       └── ch9_cwiczenia.R         # 9. Ćwiczenia (dropdown: BHP/Rolnictwo/Żywność)
├── przedzialy-ufnosci/             # Przedziały ufności
│   ├── app.R                       # Główny plik: kolory, CSS/JS, nawigacja
│   ├── dane/                       # Zbiory CSV do ćwiczeń kierunkowych
│   └── modules/
│       ├── helpers.R               # Generatory danych, symulacje pokrycia, theme
│       ├── ch1_estymacja.R         # 1. Od próby do populacji (estymacja punktowa)
│       ├── ch2_idea.R              # 2. Idea przedziałów (100 CI, krok po kroku, quiz)
│       ├── ch3_srednia.R           # 3. Przedział dla średniej (z vs t, kalkulator)
│       ├── ch4_proporcja.R         # 4. Przedział dla proporcji (Wald vs Wilson)
│       ├── ch5_czynniki.R          # 5. Co wpływa na szerokość? (n, CL, s)
│       ├── ch6_sciaga.R            # 6. Ściąga (wzory, drzewo decyzyjne, R)
│       └── ch7_cwiczenia.R         # 7. Ćwiczenia (dropdown: Edukacja/BHP/Rolnictwo/Żywność)
├── wnioskowanie-statystyczne/      # Wnioskowanie statystyczne (testy hipotez)
│   ├── app.R                       # Główny plik: kolory, CSS/JS, nawigacja
│   ├── assets/                     # Obrazki do rozdziałów (Anscombe, Simpson, itp.)
│   └── modules/
│       ├── helpers.R               # Dane studenckie, formatowanie wyników, theme
│       ├── ch1_logika.R            # 1. Logika testowania (p-wartość, błędy, moc)
│       ├── ch2_hipotezy.R          # 2. Formułowanie hipotez (pytanie↔hipoteza, quizy, jedno/dwustronny)
│       ├── ch2_jedna_ilosciowa.R   # 3. Jedna zmienna ilościowa (t, Wilcoxon)
│       ├── ch3_jedna_jakosciowa.R  # 4. Jedna zmienna jakościowa (χ², dwumianowy)
│       ├── ch4_korelacja.R         # 5. Dwie ilościowe (Pearson, Spearman)
│       ├── ch5_dwie_jakosciowe.R   # 6. Dwie jakościowe (χ² niezależności, Fisher)
│       ├── ch6_dwie_grupy.R        # 7. Ilościowa vs jakościowa, 2 grupy (t, M-W, parowe)
│       ├── ch7_anova.R             # 8. ANOVA (jednoczynnikowa, Kruskal-Wallis, post-hoc)
│       └── ch8_sciaga.R            # 9. Ściąga (drzewo decyzyjne, tabele, kod R)
├── regresja/                        # Regresja liniowa i logistyczna
│   ├── app.R                       # Główny plik: kolory, CSS/JS, nawigacja
│   └── modules/
│       ├── helpers.R               # Generatory danych regresyjnych, metryki, theme
│       ├── ch1_liniowa.R           # 1. Regresja liniowa prosta (scatter, reszty, R²)
│       ├── ch2_wieloraka.R         # 2. Regresja wieloraka (predyktory, adj.R², stepwise)
│       ├── ch3_logistyczna.R       # 3. Regresja logistyczna (sigmoida, OR, predykcja)
│       ├── ch4_porownanie.R        # 4. Porównanie modeli (R², AIC, BIC, RMSE, overfitting)
│       └── ch5_sciaga.R            # 5. Ściąga (wzory, metryki, kod R)
├── zalozenia-testow/               # Założenia testów statystycznych
│   ├── app.R                       # Główny plik: kolory, CSS/JS, nawigacja
│   └── modules/
│       ├── helpers.R               # Generatory danych z naruszeniami, theme
│       ├── ch1_normalnosc.R        # 1. Normalność (Q-Q, Shapiro, transformacje)
│       ├── ch2_wariancje.R         # 2. Jednorodne wariancje (Levene, Bartlett, Welch)
│       ├── ch3_regresja.R          # 3. Założenia regresji (diagnostyka, BP, DW)
│       ├── ch4_chi_fisher.R        # 4. Założenia χ² i Fishera (min. liczności)
│       ├── ch5_mapa.R              # 5. Mapa metod (metoda → założenia → alternatywa)
│       └── ch6_sciaga.R            # 6. Ściąga (testy diagnostyczne, quick reference)
├── case-studies/                    # Case studies - kompletne analizy
│   ├── app.R                       # Główny plik: kolory, CSS/JS
│   └── modules/
│       ├── helpers.R               # Formatowanie, theme
│       └── ch1_caschools.R         # 1. CASchools (EDA, korelacja, t-test, ANOVA, regresja)
├── dobre-dane/                     # Jakość danych (case studies)
│   └── app.R                       # Jednoplikowa aplikacja: 11 zbiorów (CASchools, n=8, pingwiny, Tarantino, ankieta firmowa, Wage, ankieta SU, mieszkania, studenci, powietrze) + wprowadzenie i ściąga
├── symulacje-statystyczne/         # Symulacje: bootstrap, permutacje, CV, MC
│   ├── app.R                       # Główny plik: kolory, CSS/JS, nawigacja
│   └── modules/
│       ├── helpers.R               # Generatory, run_bootstrap/jackknife/permutation, compute_skewness, theme
│       ├── ch1_idea.R              # 1. Idea resamplingu
│       ├── ch2_bootstrap_ci.R      # 2. Bootstrap CI (percentyl, basic, klasyczny)
│       ├── ch3_bootstrap_jednopr.R # 3. Bootstrap dla jednej próby
│       ├── ch4_permutacje.R        # 4. Testy permutacyjne
│       ├── ch5_jackknife.R         # 5. Jackknife (bias, SE, bias-correction)
│       ├── ch6_cv.R                # 6. Cross-validation (K-fold, LOOCV)
│       ├── ch7_monte_carlo.R       # 7. Monte Carlo (moc testu, rozkład pod H₀)
│       ├── ch8_kiedy.R             # 8. Kiedy stosować?
│       ├── ch9_sciaga.R            # 9. Ściąga
│       └── ch10_cwiczenia.R        # 10. Ćwiczenia (dropdown: Rolnictwo/TŻ/BHP/Edukacja)
├── R/                              # Współdzielone zasoby (shared.R, shared_styles.css, shared_toc.js)
├── README.md                       # Ten plik
└── CLAUDE.md                       # Instrukcje dla AI
```

## 🏗️ Architektura aplikacji wykładowej

Każda aplikacja wykładowa (np. `typy-danych`) stosuje wzorzec **scrollowalnego skryptu**:

- **navbarPage** z zakładkami = rozdziały wykładu
- **Sticky TOC** — spis treści z auto-podświetlaniem bieżącej sekcji
- **Variable tracker** — student wybiera zmienną w rozdziale 1 i śledzi ją przez cały kurs
- **Osadzone widgety** — interaktywne ćwiczenia wplecione w narrację
- **MathJax** — wzory matematyczne renderowane profesjonalnie
- **Chart.js** — wykresy kołowe/słupkowe w HTML5 Canvas

### Modularyzacja

Kod rozdziałów jest rozbity na osobne pliki w katalogu `modules/` (nie `R/`, bo Shiny automatycznie sourcuje `R/` przed `app.R`). Każdy moduł eksportuje:

```r
# modules/ch3_polozenie.R
ch3_ui <- tabPanel("3. Statystyki położenia", ...)
ch3_server <- function(input, output, session) { ... }
```

Główny `app.R` łączy je:

```r
source(file.path(app_dir, "modules", "ch1_typy.R"), local = TRUE)
# ...
ui <- navbarPage(..., ch1_ui, ch2_ui, ch3_ui, ch4_ui, ch5_ui, ch6_ui)
server <- function(input, output, session) {
  # nawigacja + tracker
  ch1_server(input, output, session)
  # ...
}
```

## 🎓 Użycie dydaktyczne

Aplikacje są zaprojektowane do:

1. **Demonstracji na wykładach** — prowadzący scrolluje i omawia, studenci widzą na projektorze
2. **Samodzielnej eksploracji** — studenci uruchamiają lokalnie i eksperymentują w swoim tempie
3. **Materiałów do powtórki** — ściąga (rozdział 6) jako kompaktowe podsumowanie

## 🛠️ Dodawanie nowej aplikacji

1. Utwórz folder z nazwą w formacie `nazwa-aplikacji/`
2. Utwórz `app.R` zgodnie z konwencjami projektu (patrz `CLAUDE.md`)
3. Dla dużych aplikacji: rozbij na moduły w `modules/`
4. Zaktualizuj ten `README.md`

## 🐛 Rozwiązywanie problemów

### Aplikacja nie uruchamia się

```r
# Sprawdź czy pakiety są zainstalowane
installed.packages()[c("shiny", "ggplot2", "dplyr"), ]

# Jeśli brak któregoś:
install.packages("nazwa_pakietu")
```

### Błąd "cannot open file"

Upewnij się, że uruchamiasz przez `shiny::runApp("typy-danych")` z katalogu nadrzędnego, nie przez `Rscript app.R`.

## 📝 Licencja

Projekt edukacyjny. Wolne do użytku i modyfikacji w celach dydaktycznych.
