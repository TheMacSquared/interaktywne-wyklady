# 📏 Odchylenie Standardowe - Intuicja

Interaktywne narzędzie R Shiny do zrozumienia odchylenia standardowego jako miary rozproszenia danych.

## 📋 Wymagania

- R (wersja ≥ 4.0)
- Pakiety R: `shiny`, `ggplot2`, `dplyr`

## 🚀 Uruchamianie

```r
# W RStudio lub R:
setwd("ścieżka/do/odchylenie-standardowe")
shiny::runApp()

# Lub otwórz app.R i kliknij "Run App"
```

## 📚 Jak używać na zajęciach

### Scenariusze danych

Aplikacja zawiera **3 scenariusze** ilustrujące różne poziomy rozproszenia:

#### 1. **Bardzo skupione (SD ≈ 2)**
- Dane są bardzo blisko średniej
- Mała zmienność
- **Przykład życiowy:** Temperatura w klimatyzowanym biurze (20-24°C)
- **Kolor:** 🟢 Zielony

#### 2. **Umiarkowanie rozproszone (SD ≈ 5)**
- Umiarkowana zmienność wokół średniej
- **Przykład życiowy:** Wyniki studentów na egzaminie
- **Kolor:** 🟠 Pomarańczowy

#### 3. **Bardzo rozproszone (SD ≈ 10)**
- Dane znacznie oddalone od średniej
- Duża zmienność
- **Przykład życiowy:** Dochody w społeczeństwie
- **Kolor:** 🔴 Czerwony

### Wizualizacje

#### **Panel 1: Dane i średnia**
- Strip plot pokazujący wszystkie punkty danych
- 🔴 Czerwona linia: Średnia
- **Cel:** Zobaczyć surowe dane i punkt odniesienia (średnią)

#### **Panel 2: Odległości od średniej**
- Linie łączące każdy punkt ze średnią
- **Kodowanie kolorowe:**
  - 🟢 Zielone linie: Punkty blisko średniej
  - 🔴 Czerwone linie: Punkty daleko od średniej
- **Zaznaczone przedziały:**
  - Ciemniejszy niebieski: **±1SD** (68% danych)
  - Jaśniejszy niebieski: **±2SD** (95% danych)
- **Cel:** Wizualne pokazanie "jak daleko" są dane od średniej

### Interaktywność

#### 🔄 Losowanie nowych danych
- **Przycisk "🔄 Losuj nowy zestaw danych"**
- Generuje nowe 20 punktów z tego samego rozkładu
- **Pedagogiczny cel:** Pokazać że SD jest **własnością rozkładu**, nie konkretnej próby
  - Różne próby z tego samego rozkładu mają podobne SD
  - Scenariusz "skupiony" zawsze ma małe SD, "rozproszony" zawsze duże

#### ☑️ Pokazywanie obliczeń
- **Checkbox "Pokaż obliczenia krok po kroku"**
- Wyświetla pełny wzór matematyczny SD z krokami:
  1. Oblicz odległości od średniej (dewiacje)
  2. Podnieś do kwadratu każdą odległość
  3. Zsumuj wszystkie kwadraty
  4. Podziel przez (n-1) → WARIANCJA
  5. Wyciągnij pierwiastek → ODCHYLENIE STANDARDOWE
- **Użycie:** Domyślnie wyłączone - włącz gdy będziesz wprowadzać wzór

### Porównywanie scenariuszy

**Eksperyment na zajęciach:**
1. Wybierz "Bardzo skupione"
2. Pokaż studentom: SD ≈ 2, dane blisko średniej
3. Zmień na "Bardzo rozproszone"
4. Pokaż: SD ≈ 10, dane daleko od średniej
5. **Pytanie:** "Co oznacza większe SD w praktyce?"

## 🎯 Koncepcje pedagogiczne

### Co ilustruje to narzędzie?

1. **Odchylenie standardowe jako "typowa odległość"**
   - SD = średnia odległość punktu od średniej (w przybliżeniu)
   - Większe SD = bardziej rozproszone dane

2. **Wizualizacja wzoru matematycznego**
   - Wzór SD wygląda skomplikowanie: `SD = √[Σ(xᵢ - x̄)² / (n-1)]`
   - Ale intuicja jest prosta: "Jak daleko są punkty od średniej?"
   - Kwadraty używamy żeby uniknąć ujemnych wartości

3. **Reguła 68-95-99.7 (dla rozkładu normalnego)**
   - **68% danych** mieści się w przedziale ±1SD
   - **95% danych** mieści się w przedziale ±2SD
   - **99.7% danych** mieści się w przedziale ±3SD
   - To narzędzie wizualizuje ±1SD i ±2SD

4. **SD jako właściwość rozkładu**
   - Różne próby z tego samego rozkładu mają podobne SD
   - Przycisk "Losuj nowy zestaw" pokazuje tę stabilność
   - SD opisuje **populację/proces**, nie konkretne dane

5. **Porównywanie grup przez SD**
   - Mniejsze SD = bardziej przewidywalne, homogeniczne
   - Większe SD = mniej przewidywalne, heterogeniczne
   - **Przykład:** Produkcja przemysłowa chce małego SD (kontrola jakości)

## 💡 Scenariusze na zajęciach

### Scenariusz 1: Wprowadzenie do SD
1. Zacznij od "Bardzo skupione"
2. Pokaż Panel 1: "To są nasze dane"
3. Pokaż Panel 2: "Te linie pokazują odległości od średniej"
4. **Pytanie:** "Jak byście opisali 'typową odległość' tutaj?"
5. Włącz obliczenia: "Tak właśnie liczymy SD!"

### Scenariusz 2: Porównanie rozproszenia
1. Przełączaj między scenariuszami
2. Obserwuj jak zmieniają się:
   - Długości linii w Panel 2
   - Szerokość przedziału ±1SD
   - Wartość SD w statystykach
3. **Pytanie:** "Jak SD łączy się z tym co widzicie na wykresie?"

### Scenariusz 3: Stabilność SD
1. Wybierz scenariusz
2. Kliknij "Losuj nowy zestaw" 5-10 razy
3. Obserwuj: SD zmienia się nieznacznie (~±0.5)
4. **Wniosek:** "SD opisuje proces, nie konkretne punkty"

### Scenariusz 4: Pytania decyzyjne
- "Jesteś kierownikiem produkcji. Wolisz proces z SD=2 czy SD=10? Dlaczego?"
- "Porównujesz dwie klasy na egzaminie. Klasa A: SD=5, Klasa B: SD=15. Co to oznacza?"
- "Lekarz mierzy ciśnienie. Pacjent A: SD=3, Pacjent B: SD=20. Który jest stabilniejszy?"

## 🛠️ Techniczne szczegóły

### Generowanie danych
- Wszystkie scenariusze używają rozkładu normalnego: `rnorm(20, mean=50, sd=X)`
- n = 20 obserwacji (wystarczająco by pokazać wzorzec, nie za dużo)
- Średnia zawsze 50 (dla porównywalności między scenariuszami)

### Osie
- Osie X ustalone: 20-80 (by scenariusze były porównywalne)
- Jeśli dane wykraczają poza ten zakres, mogą być przycięte wizualnie

### Kolory linii w Panel 2
- Gradient: Zielony (blisko) → Czerwony (daleko)
- Normalizacja: Najdalsza odległość = najbardziej czerwona

### Wzór SD
Używamy wzoru z próbą (dzielenie przez n-1, nie n):
```
SD = √[Σ(xᵢ - x̄)² / (n-1)]
```
To jest funkcja `sd()` w R.

## 🐛 Rozwiązywanie problemów

### SD nie zgadza się z moimi ręcznymi obliczeniami
- Upewnij się że używasz dzielenia przez (n-1), nie n
- R domyślnie używa "sample SD" (n-1), nie "population SD" (n)

### Przedziały ±1SD nie zawierają dokładnie 68% danych
- Reguła 68-95-99.7 działa **idealnie dla rozkładu normalnego**
- Małe próby (n=20) mogą odchylać się od tej reguły
- To normalne i pedagogicznie cenne - pokazuje różnicę teoria vs praktyka

### Chcę więcej punktów danych
- Edytuj `app.R`, zmień `rnorm(20, ...)` na `rnorm(50, ...)` lub inną wartość

## 📧 Kontakt

Jeśli masz pytania lub pomysły na rozszerzenia, zapisz w pliku `feedback.md` w tym folderze.