# 📊 Średnia vs Mediana - Kiedy co?

Interaktywne narzędzie R Shiny do zrozumienia różnicy między średnią a medianą i kiedy która miara jest bardziej odpowiednia.

## 📋 Wymagania

- R (wersja ≥ 4.0)
- Pakiety R: `shiny`, `ggplot2`, `dplyr`

## 🚀 Uruchamianie

```r
# W RStudio lub R:
setwd("ścieżka/do/srednia-vs-mediana")
shiny::runApp()

# Lub otwórz app.R i kliknij "Run App"
```

## 📚 Jak używać na zajęciach

### Scenariusze z życia wziętego

Aplikacja zawiera **5 realistycznych scenariuszy**, każdy ilustrujący inny typ rozkładu. Każdy scenariusz **generuje losowo 80 obserwacji** z odpowiedniego rozkładu statystycznego.

#### 1. **Zarobki w firmie** (rozkład skośny prawo - Gamma)
- **Charakterystyka:** Większość pracowników zarabia podobnie (4000-9000 PLN), ale kilku managerów ma wysokie zarobki (15000+ PLN)
- **Rozkład:** Gamma (shape=2, scale=2000) + 3000
- **Pedagogiczny cel:** Pokazać jak outliers podnoszą średnią
- **Pytanie dla studentów:** "Czy średnia zarobków dobrze reprezentuje typowego pracownika?"

#### 2. **Wyniki egzaminu** (rozkład normalny/symetryczny)
- **Charakterystyka:** Klasyczny rozkład dzwonowy, większość wyników wokół 70 pkt (zakres 45-95)
- **Rozkład:** Normalny (mean=70, sd=12), ograniczony do 0-100
- **Pedagogiczny cel:** Gdy rozkład symetryczny, średnia ≈ mediana
- **Pytanie dla studentów:** "Czy w tym przypadku średnia i mediana są podobne?"

#### 3. **Czas dojazdu do pracy** (rozkład skośny prawo - lekki)
- **Charakterystyka:** Większość osób dojeżdża 15-40 minut, nieliczni > 60 minut
- **Rozkład:** Gamma (shape=3, scale=8) + 5
- **Pedagogiczny cel:** Realistyczny przykład czasu (nie może być ujemny)
- **Pytanie dla studentów:** "Czy średni czas dojazdu dobrze opisuje typowe doświadczenie?"

#### 4. **Ceny mieszkań** (rozkład skośny z outlierami)
- **Charakterystyka:** Większość mieszkań 250-600 tys. PLN, ale są luksusowe apartamenty (1200-1800 tys. PLN)
- **Rozkład:** Gamma (shape=3, scale=80) + 200, plus 4 outliers (1200-1800)
- **Pedagogiczny cel:** Mediana lepiej reprezentuje "typową" cenę
- **Pytanie dla studentów:** "Gdybyś kupował mieszkanie, która miara jest bardziej użyteczna?"

#### 5. **Czas odpowiedzi email** (rozkład wykładniczy - długi ogon)
- **Charakterystyka:** Większość odpowiedzi w 1-10 godzin, niektórzy czekają 20-30+ godzin
- **Rozkład:** Wykładniczy (rate=0.2) + 1
- **Pedagogiczny cel:** Bardzo skośny rozkład, charakterystyczny dla "czasów oczekiwania"
- **Pytanie dla studentów:** "Dlaczego średni czas odpowiedzi jest dużo wyższy niż mediana?"

### Interaktywne eksperymenty

#### 🎚️ Dodawanie nowych obserwacji
1. **Slider:** Ustaw wartość
2. **Przycisk "Dodaj wartość":** Dodaje obserwację do danych
3. **Obserwuj:** Jak zmieniają się średnia (🔴) i mediana (🔵)

#### ⚠️ Dodawanie outlierów
1. **Przycisk "Dodaj outlier":** Automatycznie dodaje wartość znacznie odstającą
2. **Efekt:**
   - Średnia (🔴) **skacze** znacząco
   - Mediana (🔵) pozostaje **stabilna** lub zmienia się nieznacznie
3. **Pedagogiczny wniosek:** Mediana jest **odporna na outliery** (robust)

#### 🔄 Reset
- **Przycisk "Reset do początku":** Losuje **nowe 80 obserwacji** z tego samego rozkładu
- **Użycie:** Pokazać zmienność próbkowania - różne próby z tego samego rozkładu
- **Pedagogiczny cel:** Średnia/mediana mogą się nieznacznie różnić między próbami, ale wzorzec pozostaje podobny

### Wizualizacje

#### Górny wykres: Histogram
- **🔴 Czerwona linia (ciągła):** Średnia
- **🔵 Niebieska linia (przerywana):** Mediana
- **Interpretacja:**
  - Linie blisko siebie → rozkład symetryczny
  - Czerwona linia wyżej → rozkład skośny prawo (outliers w górę)
  - Czerwona linia niżej → rozkład skośny lewo

#### Dolny wykres: Strip plot
- **Każdy punkt** = jedna obserwacja
- **Cel:** Pokazać surowe dane, nie tylko zagregowane
- **Linie:** Te same jak na histogramie (średnia i mediana)

### Panel statystyk
```
🔴 Średnia: X.XX
🔵 Mediana: Y.YY
Różnica: Z.ZZ
```
- **Różnica bliska 0:** Rozkład symetryczny
- **Duża różnica:** Rozkład skośny lub outliers

## 🎯 Koncepcje pedagogiczne

### Co ilustruje to narzędzie?

1. **Średnia vs Mediana - definicje w praktyce**
   - Średnia: "środek ciężkości" danych (wrażliwa na wszystkie wartości)
   - Mediana: "wartość środkowa" (50% danych poniżej, 50% powyżej)

2. **Wpływ outlierów**
   - Średnia: Bardzo wrażliwa na wartości odstające
   - Mediana: Odporna (robust) na outliery

3. **Kiedy użyć której miary?**
   - **Średnia:** Gdy rozkład symetryczny, brak outlierów
   - **Mediana:** Gdy rozkład skośny lub outliers obecne
   - **Przykład:** Mediana zarobków lepiej opisuje "typowego" pracownika

4. **Rozkłady skośne**
   - Skośny prawo: Średnia > Mediana (długi prawy ogon)
   - Skośny lewo: Średnia < Mediana (długi lewy ogon)
   - Symetryczny: Średnia ≈ Mediana

5. **Dane życiowe vs akademickie**
   - W życiu większość rozkładów jest **skośna** (zarobki, ceny, czasy)
   - W podręcznikach często pokazujemy rozkłady **symetryczne** (łatwiejsze matematycznie)
   - To narzędzie pokazuje **realistyczne** rozkłady

## 💡 Scenariusze na zajęciach

### Scenariusz 1: Odkrywanie różnicy
1. Wybierz "Wyniki egzaminu" (symetryczne)
2. Pokaż: Średnia ≈ Mediana
3. Zmień na "Zarobki w firmie"
4. Dodaj kilka outlierów (przycisk "Dodaj outlier")
5. Pokaż: Średnia rośnie, mediana stabilna
6. **Pytanie:** "Która miara lepiej opisuje typowego pracownika?"

### Scenariusz 2: Eksperyment live
1. Wybierz dowolny scenariusz
2. Poproś studentów o zgadywanie: "Jak zmieni się średnia jeśli dodam wartość 10000?"
3. Dodaj wartość
4. Sprawdź wynik
5. **Efekt:** Studenci budują intuicję predykcyjną

### Scenariusz 3: Pytania decyzyjne
- "Szukasz pracy - czy wolisz wiedzieć średnią czy medianę zarobków?"
- "Kupujesz mieszkanie - która miara cen jest bardziej użyteczna?"
- "Oceniasz egzamin - czy użyć średniej czy mediany do określenia 'typowego' wyniku?"

## 🛠️ Techniczne szczegóły

### Dane generowane losowo
Wszystkie scenariusze generują **80 obserwacji** z odpowiednich rozkładów statystycznych:
- **Zarobki:** Gamma(shape=2, scale=2000) + 3000
- **Egzamin:** Normal(mean=70, sd=12), ograniczony do [0,100]
- **Dojazd:** Gamma(shape=3, scale=8) + 5
- **Mieszkania:** Gamma(shape=3, scale=80) + 200, plus 4 outliers uniform(1200,1800)
- **Email:** Exponential(rate=0.2) + 1

Każda zmiana scenariusza lub Reset generuje **nowe losowe dane** z tego samego rozkładu.

### Dynamiczne osie
Osie X dostosowują się automatycznie do zakresu danych w każdym scenariuszu.

### Outlier automatyczny
Przycisk "Dodaj outlier" generuje wartość będącą wielokrotnością obecnego maksimum:
- **Zarobki:** 3x max
- **Egzamin:** 0.3x max (pokazuje słaby wynik jako outlier)
- **Dojazd:** 2x max
- **Mieszkania:** 2.5x max
- **Email:** 3x max

Outlier jest ograniczony do zakresu slidera dla każdego scenariusza.

## 🐛 Rozwiązywanie problemów

### Osie nie mieszczą się
- Dynamiczne osie czasem mogą sprawić, że etykiety nachodzą
- **Rozwiązanie:** Kliknij "Reset" aby przywrócić czytelne zakresy

### Chcę własne dane
- To narzędzie ma predefiniowane scenariusze
- Jeśli chcesz własne dane, możesz je dodać edytując `scenarios` list w `app.R`

## 📧 Kontakt

Jeśli masz pytania lub pomysły na rozszerzenia, zapisz w pliku `feedback.md` w tym folderze.