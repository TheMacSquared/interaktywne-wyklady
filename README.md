# 📊 Interaktywne Narzędzia do Nauczania Statystyki

Zbiór aplikacji R Shiny do interaktywnego nauczania statystyki na zajęciach akademickich. Każda aplikacja ilustruje kluczowe koncepcje statystyczne poprzez wizualizację i eksperymentowanie.

## 📋 Wymagania

- R (wersja ≥ 4.0)
- Pakiety R: `shiny`, `ggplot2`, `dplyr`, `rstatix`, `broom`, `lmtest`

## 🚀 Instalacja pakietów

```r
# Podstawowe (wszystkie aplikacje)
install.packages(c("shiny", "ggplot2", "dplyr"))

# Dodatkowe (dla zaawansowanych aplikacji)
install.packages(c("rstatix", "broom", "lmtest"))
```

## ▶️ Uruchamianie aplikacji

### Metoda 1: Shiny Server (Docker) - zalecana do hostowania

Wszystkie 14 aplikacji dostępnych jako jedna strona internetowa z obliczeniami po stronie serwera.

**Wymagania:** Docker i Docker Compose

```bash
# Zbuduj i uruchom serwer
docker compose up -d

# Aplikacje dostępne pod adresem:
# http://localhost:3838
```

Portal (strona główna) zawiera linki do wszystkich aplikacji pogrupowanych tematycznie.

Poszczególne aplikacje dostępne pod adresami:
- `http://localhost:3838/box-plot-builder/`
- `http://localhost:3838/histogram-builder/`
- `http://localhost:3838/gra-estymacja/`
- itd.

```bash
# Zatrzymanie serwera
docker compose down

# Przebudowanie po zmianach w kodzie
docker compose up -d --build
```

### Metoda 2: Z R/RStudio (pojedyncza aplikacja)

```r
# Ustaw katalog roboczy na folder z app.R
setwd("ścieżka/do/aplikacji")
shiny::runApp()
```

### Metoda 3: Bezpośrednio z pliku

1. Otwórz plik `app.R` w RStudio
2. Kliknij przycisk **"Run App"** w prawym górnym rogu edytora

### Metoda 4: Z konsoli R

```r
shiny::runApp("ścieżka/do/aplikacji")
```

## 📚 Aplikacje

### Podstawy statystyki opisowej

| Aplikacja | Opis | Koncepcje |
|-----------|------|-----------|
| [losowanie_spoznienia](losowanie_spoznienia/) | Symulator zbierania danych (spóźnienia autobusu) | Populacja vs próba, zmienność próbkowania, rozkłady skośne |
| [histogram-builder](histogram-builder/) | Budowanie histogramu krok po kroku | Wizualizacja danych, binning, częstości |
| [box-plot-builder](box-plot-builder/) | Konstrukcja wykresu pudełkowego | Kwartyle, IQR, outliery, 5-liczba podsumowanie |
| [srednia-vs-mediana](srednia-vs-mediana/) | Porównanie miar centralności | Wrażliwość na outliery, rozkłady skośne |
| [odchylenie-standardowe](odchylenie-standardowe/) | Intuicja odchylenia standardowego | Rozproszenie danych, reguła 68-95-99.7 |
| [moments-explorer](moments-explorer/) | Momenty rozkładu | Skośność, kurtoza |
| [gra-estymacja](gra-estymacja/) | Gra w zgadywanie statystyk | Intuicja statystyczna, gamifikacja |

### Rozkłady prawdopodobieństwa

| Aplikacja | Opis | Koncepcje |
|-----------|------|-----------|
| [distribution-explorer](distribution-explorer/) | Wizualizacja rozkładów teoretycznych | Normalny, t, chi-kwadrat, gamma, beta, Poisson |
| [sampling-explorer](sampling-explorer/) | Próbkowanie z rozkładów | Prawdopodobieństwo empiryczne vs teoretyczne |

### Testowanie hipotez

| Aplikacja | Opis | Koncepcje |
|-----------|------|-----------|
| [zalozenia-testow](zalozenia-testow/) | Założenia testów statystycznych | Normalność, homogeniczność wariancji, outliery |
| [test-t-builder](Testowanie-hipotez/test-t-builder/) | Test t krok po kroku | Hipotezy, statystyka t, wartość p, decyzja |
| [chi-kwadrat-builder](Testowanie-hipotez/chi-kwadrat-builder/) | Test chi-kwadrat | Tabele kontyngencji, niezależność |
| [korelacja-builder](Testowanie-hipotez/korelacja-builder/) | Korelacja Pearsona vs Spearmana | Związki liniowe i monotoniczne |

### Regresja i modelowanie

| Aplikacja | Opis | Koncepcje |
|-----------|------|-----------|
| [regresja-interakcja](regresja-interakcja/) | Porównanie modelu addytywnego i z interakcją | Interakcja zmiennych, równoległe vs przecinające się linie, test F |

## 📁 Struktura projektu

```
Interaktywne/
├── Dockerfile                  # Konfiguracja kontenera Docker
├── docker-compose.yml          # Orkiestracja Docker Compose
├── shiny-server.conf           # Konfiguracja Shiny Server
├── portal/                     # Strona główna (landing page)
│   └── app.R
├── losowanie_spoznienia/       # Symulator zbierania danych
│   ├── app.R
│   └── README.md
├── histogram-builder/          # Budowanie histogramu
│   └── app.R
├── box-plot-builder/           # Wykres pudełkowy krok po kroku
│   ├── app.R
│   └── README.md
├── srednia-vs-mediana/         # Porównanie średniej i mediany
│   ├── app.R
│   └── README.md
├── odchylenie-standardowe/     # Intuicja SD
│   ├── app.R
│   └── README.md
├── moments-explorer/           # Momenty rozkładu
│   └── app.R
├── gra-estymacja/              # Gra w estymację statystyk
│   ├── app.R
│   └── README.md
├── distribution-explorer/      # Rozkłady teoretyczne
│   └── app.R
├── sampling-explorer/          # Próbkowanie z rozkładów
│   └── app.R
├── zalozenia-testow/           # Założenia testów
│   ├── app.R
│   └── README.md
├── Testowanie-hipotez/         # Aplikacje do testowania hipotez
│   ├── test-t-builder/
│   │   └── app.R
│   ├── chi-kwadrat-builder/
│   │   └── app.R
│   └── korelacja-builder/
│       └── app.R
├── regresja-interakcja/        # Regresja z interakcją
│   ├── app.R
│   └── README.md
├── README.md                   # Ten plik
└── CLAUDE.md                   # Instrukcje dla AI
```

## 🎓 Użycie dydaktyczne

Aplikacje są zaprojektowane do:

1. **Demonstracji na wykładach** - projektor + interaktywna eksploracja
2. **Ćwiczeń laboratoryjnych** - studenci samodzielnie eksperymentują
3. **Zadań domowych** - np. gra-estymacja jako zadanie zaliczeniowe
4. **Dyskusji grupowych** - wspólne odkrywanie wzorców

Każda aplikacja z plikiem README.md zawiera:
- Scenariusze pedagogiczne
- Pytania dla studentów
- Wskazówki do dyskusji

## 🛠️ Rozszerzanie projektu

Aby dodać nową aplikację:

1. Utwórz folder z nazwą w formacie `nazwa-aplikacji/`
2. Utwórz plik `app.R` zgodnie z konwencjami projektu (patrz `CLAUDE.md`)
3. Opcjonalnie: dodaj `README.md` ze scenariuszami pedagogicznymi
4. Zaktualizuj główny `README.md` - dodaj aplikację do odpowiedniej tabeli

## 🐛 Rozwiązywanie problemów

### Aplikacja nie uruchamia się

```r
# Sprawdź czy pakiety są zainstalowane
installed.packages()[c("shiny", "ggplot2", "dplyr"), ]

# Jeśli brak któregoś:
install.packages("nazwa_pakietu")
```

### Błąd "cannot open file 'app.R'"

Upewnij się, że jesteś w poprawnym katalogu:

```r
getwd()  # Sprawdź aktualny katalog
setwd("poprawna/ścieżka")  # Ustaw właściwy
```

## 📝 Licencja

Projekt edukacyjny. Wolne do użytku i modyfikacji w celach dydaktycznych.
