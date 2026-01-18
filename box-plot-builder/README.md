# 📦 Box Plot Builder - Krok po kroku

Interaktywne narzędzie R Shiny do zrozumienia budowy wykresu pudełkowego (box plot) poprzez wizualizację kolejnych kroków konstrukcji.

## 📋 Wymagania

- R (wersja ≥ 4.0)
- Pakiety R: `shiny`, `ggplot2`, `dplyr`

## 🚀 Uruchamianie

```r
# W RStudio lub R:
setwd("ścieżka/do/box-plot-builder")
shiny::runApp()

# Lub otwórz app.R i kliknij "Run App"
```

## 📚 Jak używać na zajęciach

### Dane przykładowe

Aplikacja używa **predefiniowanych danych**: wyniki egzaminu 30 studentów (0-100 punktów).
- Większość wyników: 45-100 pkt
- 2 outliery: 18 i 22 pkt (studenci którzy źle napisali egzamin)
- n = 30 obserwacji

### 8 Kroków budowy box plotu

Aplikacja prowadzi studentów przez **8 sekwencyjnych kroków**, każdy ilustrujący jeden element konstrukcji:

#### **Krok 1: Surowe dane**
- Pokazuje 30 punktów (nieposortowane)
- **Cel:** "To są nasze dane wyjściowe"
- Punkty są rozrzucone losowo na osi

#### **Krok 2: Sortuj dane**
- Dane zostają posortowane od najmniejszej do największej wartości
- **Cel:** Sortowanie jest kluczowe do znalezienia percentyli
- Punkty ustawiają się w linię od lewej do prawej

#### **Krok 3: Znajdź medianę (Q2)**
- Czerwona linia pionowa: mediana (50. percentyl)
- **Definicja:** Połowa danych poniżej, połowa powyżej
- **Wyjaśnienie:** "Mediana = środkowa wartość posortowanych danych"

#### **Krok 4: Znajdź Q1 i Q3**
- Niebieskie linie przerywane: Q1 (25. percentyl) i Q3 (75. percentyl)
- Czerwona linia: Q2 (mediana)
- **Wyjaśnienie:**
  - Q1: 25% danych poniżej, 75% powyżej
  - Q3: 75% danych poniżej, 25% powyżej
  - Środkowe 50% danych znajduje się między Q1 a Q3

#### **Krok 5: Oblicz IQR (rozstęp międzykwartylowy)**
- Niebieski prostokąt: IQR = Q3 - Q1
- **Definicja:** Rozstęp środkowych 50% danych
- **Interpretacja:** Większy IQR = bardziej rozproszone dane

#### **Krok 6: Dodaj wąsy**
- Czarne linie poziome: wąsy (whiskers)
- **Reguła wąsów:**
  - Dolny wąs: Q1 - 1.5 × IQR (ale nie niżej niż min)
  - Górny wąs: Q3 + 1.5 × IQR (ale nie wyżej niż max)
- **Cel:** Pokazać zakres "typowych" wartości

#### **Krok 7: Zaznacz outliery**
- Czerwone punkty: outliery (wartości poza wąsami)
- Szare punkty: typowe wartości
- **Definicja outliera:** Wartość poza przedziałem [Q1 - 1.5×IQR, Q3 + 1.5×IQR]
- W tym przykładzie: 2 outliery (18, 22 pkt)

#### **Krok 8: Pokaż pełny box plot**
- Kompletny box plot (horizontal)
- Pudełko = Q1 do Q3
- Linia w środku = mediana
- Wąsy = zakres typowych wartości
- Czerwone punkty = outliery
- **Łatwo porównać z histogramem poniżej**

### Histogram dla porównania

**Dolny panel** pokazuje statyczny histogram tych samych danych.

**Pedagogiczny cel:**
- Studenci widzą **dwa sposoby wizualizacji tych samych danych**
- Box plot kompresuje informację (5-liczba podsumowanie)
- Histogram pokazuje pełny rozkład

**Pytanie dla studentów:**
- "Co widzimy na histogramie, czego nie ma na box plocie?"
- "Jakie informacje są lepiej widoczne na box plocie?"

## 🎯 Koncepcje pedagogiczne

### Co ilustruje to narzędzie?

1. **Box plot to wizualizacja 5-liczby**
   - Minimum (dolny wąs)
   - Q1 (lewy bok pudełka)
   - Mediana (linia w pudełku)
   - Q3 (prawy bok pudełka)
   - Maximum (górny wąs)
   - + Outliery (osobne punkty)

2. **Percentyle i kwartyle**
   - Q1 = 25. percentyl
   - Q2 = 50. percentyl = mediana
   - Q3 = 75. percentyl
   - Każdy kwartyl zawiera 25% danych

3. **IQR jako miara rozproszenia**
   - IQR = Q3 - Q1
   - Pokazuje rozstęp środkowych 50% danych
   - Odporna na outliery (robust measure)

4. **Reguła 1.5 × IQR dla outlierów**
   - Standardowa definicja outliera w box plocie
   - Outlier jeśli wartość < Q1 - 1.5×IQR lub > Q3 + 1.5×IQR
   - ~99.3% danych normalnych mieści się w tym przedziale

5. **Box plot vs histogram**
   - Box plot: kompaktowy, łatwy do porównań grup
   - Histogram: pełniejszy obraz rozkładu, widać bimodalność
   - Box plot traci informację o kształcie rozkładu

## 💡 Scenariusze na zajęciach

### Scenariusz 1: Wprowadzenie do box plotu
1. "Co to jest box plot? Zobaczmy jak powstaje!"
2. Klikaj kolejne kroki od 1 do 8
3. Przy każdym kroku wyjaśniaj co się dzieje
4. **Efekt:** Studenci rozumieją konstrukcję, nie tylko interpretację

### Scenariusz 2: Pytania podczas kroków
- **Po kroku 3:** "Ile studentów dostało więcej niż mediana punktów?"
- **Po kroku 4:** "Jaki procent studentów ma wyniki między Q1 a Q3?"
- **Po kroku 7:** "Czy outliery to błąd pomiarowy czy prawdziwe wyniki?"
- **Po kroku 8:** "Jak szybko można ocenić rozproszenie danych na box plocie?"

### Scenariusz 3: Porównanie z histogramem
1. Pokaż pełny box plot (krok 8)
2. Porównaj z histogramem poniżej
3. **Pytania:**
   - "Które informacje są lepiej widoczne na histogramie?"
   - "Które informacje są lepiej widoczne na box plocie?"
   - "Czy widzicie bimodalność na box plocie?"

### Scenariusz 4: Reset i powtórka
1. Kliknij "Reset"
2. Poproś studentów o przewidzenie: "Co stanie się w kroku 5?"
3. Sprawdź ich przewidywanie
4. **Efekt:** Aktywne uczenie się, sprawdzanie zrozumienia

## 🛠️ Techniczne szczegóły

### Dane przykładowe
```r
c(45, 52, 55, 58, 60, 62, 65, 68, 70, 72,
  74, 75, 76, 78, 80, 82, 84, 85, 86, 88,
  89, 90, 92, 94, 95, 96, 22, 18, 98, 100)
```
- n = 30
- Zakres: 18-100 pkt
- Outliery: 18, 22 (celowo dodane)

### Obliczenia kwartyli
Aplikacja używa standardowych funkcji R:
- `quantile(data, 0.25)` dla Q1
- `median(data)` dla Q2
- `quantile(data, 0.75)` dla Q3

### Reguła wąsów
```r
lower_whisker = max(min(data), Q1 - 1.5 × IQR)
upper_whisker = min(max(data), Q3 + 1.5 × IQR)
```
Wąsy nie wykraczają poza rzeczywiste min/max danych.

### Box plot horizontal
Box plot jest pokazany **horizontal** (poziomy), nie vertical:
- Łatwiej porównać z histogramem poniżej (te same osie X)
- Intuicyjniejszy dla studentów (oś X = wartości)

## 📊 Interpretacja wyników w tym przykładzie

### Statystyki
- **Mediana (Q2):** ~79 pkt
- **Q1:** ~66 pkt
- **Q3:** ~90 pkt
- **IQR:** ~24 pkt
- **Wąsy:** ~45-100 pkt (w przybliżeniu)
- **Outliery:** 18, 22 pkt

### Interpretacja
- **Typowy student:** 66-90 pkt (środkowe 50%)
- **Mediana:** Połowa studentów dostała ≤79 pkt
- **Outliery:** 2 studentów z bardzo słabymi wynikami (18, 22)
- **Rozkład:** Lekko skośny lewo (Q2 bliżej Q3 niż Q1)

## 🎓 Rozszerzenia pedagogiczne

### Pytania dyskusyjne
1. "Czy outliery (18, 22) powinny być usunięte z analizy?"
2. "Jak zmieni się box plot jeśli usuniemy outliery?"
3. "Czy mediana lepiej opisuje 'typowy' wynik niż średnia w tym przykładzie?"

### Porównanie z innymi wykresami
- Histogram: Pełny rozkład, ale trudny do porównań grup
- Box plot: Kompaktowy, łatwy do porównań wielu grup
- Violin plot: Box plot + rozkład (advanced)

### Kiedy używać box plotu?
- ✅ Porównywanie wielu grup (np. wyniki 5 klas)
- ✅ Szybka identyfikacja outlierów
- ✅ Porównanie mediany i rozproszenia
- ❌ Pokazanie dokładnego kształtu rozkładu (użyj histogramu)
- ❌ Mała liczba obserwacji (n < 10)

## 🐛 Rozwiązywanie problemów

### Przyciski kroków nie działają
- Upewnij się, że klikasz przyciski w kolejności
- Reset i spróbuj ponownie

### Chcę własnych danych
- Edytuj `original_data` w `app.R`
- Zmień wartości na swoje
- Pamiętaj: min 10-15 obserwacji dla sensownego box plotu

### Box plot wygląda inaczej w innych narzędziach
- Różne narzędzia mogą używać różnych metod obliczania kwartyli
- R używa domyślnie metody Type 7 (najczęstsza)
- Wyniki mogą się nieznacznie różnić, ale interpretacja ta sama

## 📧 Kontakt

Jeśli masz pytania lub pomysły na rozszerzenia, zapisz w pliku `feedback.md` w tym folderze.