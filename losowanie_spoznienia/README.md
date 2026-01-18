# 🚌 Symulator Spóźnień Autobusu

Interaktywne narzędzie R Shiny do nauczania podstawowych koncepcji statystyki: populacja, próba, rozkład, niepewność próbkowania.

## 📋 Wymagania

- R (wersja ≥ 4.0)
- Pakiety R: `shiny`, `ggplot2`, `dplyr`

## 🚀 Instalacja pakietów

Przed pierwszym uruchomieniem zainstaluj wymagane pakiety. W konsoli R wykonaj:

```r
install.packages(c("shiny", "ggplot2", "dplyr"))
```

## ▶️ Uruchamianie aplikacji

### Metoda 1: Z R/RStudio (najłatwiejsza)

1. Otwórz RStudio
2. Ustaw katalog roboczy na folder z `app.R`:
   ```r
   setwd("ścieżka/do/losowanie_spoznienia")
   ```
3. Uruchom aplikację:
   ```r
   shiny::runApp()
   ```

### Metoda 2: Bezpośrednio z pliku

1. Otwórz plik `app.R` w RStudio
2. Naciśnij przycisk **"Run App"** w prawym górnym rogu edytora
3. Lub zaznacz cały kod i naciśnij `Ctrl+Enter` / `Cmd+Enter`

### Metoda 3: Z linii poleceń R

```r
library(shiny)
runApp("ścieżka/do/losowanie_spoznienia")
```

### Metoda 4: Z terminala (WSL/Linux)

```bash
cd /home/maciek/neural-notes/praca-akademicka/dydaktyka/04-szablony/narzedzia/Interaktywne/losowanie_spoznienia
Rscript -e "shiny::runApp()"
```

## 📚 Jak używać na zajęciach

### Scenariusz pedagogiczny

**Kontekst:** "Jeździsz codziennie autobusem o 8:00. Mierzysz spóźnienia. Jak zdecydować, o której wyjść z domu?"

#### Faza 1: Pojedyncze obserwacje (n=1→10)
- Kliknij **"+1 dzień"** kilka razy
- Pokaż studentom, że z małą próbą histogram jest chaotyczny
- Pytanie: "Czy możemy już podjąć decyzję?"

#### Faza 2: Tydzień danych (n=10→20)
- Kliknij **"+10 dni"**
- Wzorzec zaczyna się rysować, ale wciąż niestabilny

#### Faza 3: Miesiąc (n=20→50)
- Kilka razy **"+10 dni"**
- Wyraźny wzorzec, rozkład stabilizuje się

#### Faza 4: Rok (n→250+)
- Kliknij **"+100 dni"** 2-3 razy
- Bardzo stabilny rozkład pokazuje "prawdziwy kształt" populacji

### Interaktywne elementy

#### 🎚️ Suwak decyzyjny
- Ustaw "O ile minut wcześniej wychodzę?"
- Histogram się koloruje:
  - 🟢 **Zielone słupki** = Zdążysz (autobus bardziej spóźniony)
  - 🔴 **Czerwone słupki** = Spóźnisz się (autobus mniej spóźniony)
- **Prawdopodobieństwo zdążenia** aktualizuje się na żywo

#### 📊 Statystyki opisowe
- **Domyślnie wyłączone** (użyj na początku bez statystyk)
- Zaznacz checkbox "Pokaż statystyki opisowe", gdy będziesz już omawiać średnią, medianę, odchylenie standardowe

#### 🔄 Przycisk Reset
- Resetuje dane i zaczyna od nowa
- Użyj do pokazania zmienności próbkowania: "Zbierzmy dane jeszcze raz z tego samego autobusu"

## 🎯 Koncepcje pedagogiczne

### Co ilustruje to narzędzie?

1. **Populacja vs Próba**
   - Populacja = wszystkie możliwe czasy przyjazdu autobusu (nieskończone)
   - Próba = 30/100/250 pomiarów zebranych przez studenta

2. **Zmienność próbkowania**
   - Przycisk Reset → losuj nową próbę
   - Różne próby z tej samej populacji wyglądają trochę inaczej

3. **Wartość dużej próby**
   - n=5: Chaotyczny histogram
   - n=50: Wyraźny wzorzec
   - n=250: Bardzo stabilny rozkład

4. **Rozkład skośny (right-skewed)**
   - Większość wartości: 0-2 min (typowe lekkie spóźnienie)
   - Długi prawy ogon: Rzadkie duże spóźnienia (5-15 min)
   - **Pedagogiczna wartość:** Średnia ≠ Typowa wartość (średnia wyższa przez outliers)

5. **Podejmowanie decyzji pod niepewnością**
   - "Jeśli wychodzę 5 min wcześniej, jaki mam % szans zdążyć?"
   - Trade-off: Wczesne wyjście = większa pewność, ale dłuższe czekanie

## 🛠️ Parametry techniczne rozkładu

Aplikacja używa **Gamma distribution** przesuniętego o -1:
```r
rgamma(n, shape = 2, scale = 1.5) - 1
```

Charakterystyka:
- **Moda:** ~0.5 min (najczęstsza wartość)
- **Średnia:** ~2 min
- **Mediana:** ~1.5 min
- **Minimum:** -1 min (rzadkie wcześniejsze przyjazdy)
- **Maximum:** 20 min (ekstremalne opóźnienia)
- **Rozkład:** Skośny w prawo (realistyczny dla transportu publicznego)

## 🌐 Opcje hostowania online (opcjonalnie)

### ShinyApps.io (darmowy hosting)

1. Zainstaluj pakiet:
   ```r
   install.packages("rsconnect")
   ```

2. Zarejestruj się na https://www.shinyapps.io/

3. Połącz konto (skopiuj token z dashboardu):
   ```r
   rsconnect::setAccountInfo(name='twoje-konto',
                             token='TWOJ_TOKEN',
                             secret='TWOJ_SECRET')
   ```

4. Wdepluj aplikację:
   ```r
   rsconnect::deployApp('ścieżka/do/losowanie_spoznienia')
   ```

## 🐛 Rozwiązywanie problemów

### Aplikacja nie uruchamia się
```r
# Sprawdź czy pakiety są zainstalowane
installed.packages()[c("shiny", "ggplot2", "dplyr"), ]

# Jeśli brak któregoś:
install.packages("nazwa_pakietu")
```

### Błąd "cannot open file 'app.R'"
- Upewnij się, że jesteś w poprawnym katalogu:
  ```r
  getwd()  # Sprawdź aktualny katalog
  setwd("poprawna/ścieżka")  # Ustaw właściwy
  ```

### Aplikacja działa, ale wykres się nie aktualizuje
- Kliknij **Reset** i spróbuj ponownie
- Sprawdź czy nie ma błędów w konsoli R

## 📧 Kontakt

Jeśli masz pytania lub pomysły na rozszerzenia, zapisz w pliku `feedback.md` w tym folderze.

## 📝 Licencja

Narzędzie stworzone do celów edukacyjnych. Wolne do użytku i modyfikacji.