# 🎮 Gra w Estymację Statystyk

Interaktywna gra R Shiny do ćwiczenia rozpoznawania statystyk opisowych (średnia, mediana, odchylenie standardowe) na podstawie histogramu.

## 📋 Wymagania

- R (wersja ≥ 4.0)
- Pakiety R: `shiny`, `ggplot2`, `dplyr`

## 🚀 Uruchamianie

```r
# W RStudio lub R:
setwd("ścieżka/do/gra-estymacja")
shiny::runApp()

# Lub otwórz app.R i kliknij "Run App"
```

## 🎮 Jak grać

### **1. Ekran startowy - Ustawienia gry**

Przed rozpoczęciem gry ustaw parametry:

#### Liczba rund
- **3 rundy** - szybka gra (~5 min)
- **5 rund** - standardowa gra (~10 min)
- **10 rund** - długa gra, więcej ćwiczenia (~20 min)

#### Poziom trudności

**🟢 Łatwy** (polecany na początek)
- Bardzo różne rozkłady danych
- Łatwo rozpoznać średnią i medianę
- Typy rozkładów: normalny niski/wysoki, jednostajny, skośny, bimodalny
- **Dla kogo:** Pierwsze zajęcia, wprowadzenie do statystyk

**🟡 Średni**
- Subtelne różnice między rozkładami
- Średnia 40-60, odchylenie standardowe 5-15
- Wymaga dokładniejszej obserwacji
- **Dla kogo:** Po pierwszych tygodniach kursu

**🔴 Trudny**
- Bardzo podobne rozkłady
- Średnia 48-52, odchylenie standardowe 8-12
- Trudno odróżnić różnice
- **Dla kogo:** Zaawansowani studenci, sprawdzian umiejętności

#### Co zgadywać?

☑️ **Średnia** - Najłatwiejsza do estymacji (punkt ciężkości danych)
☑️ **Mediana** - Wartość środkowa (50% danych po lewej/prawej)
☑️ **Odchylenie standardowe** - Najtrudniejsze! (rozproszenie danych)

**Rekomendacja na początek:** Średnia + Mediana

---

### **2. Rozgrywka - Każda runda**

#### Krok 1: Obserwuj histogram
- Dostaniesz histogram z danymi (0-100)
- **Bez żadnych statystyk** - musisz zgadnąć!

#### Krok 2: Ustaw swoje estymaty
- Użyj sliderów do ustawienia wartości
- **Średnia:** 0-100
- **Mediana:** 0-100
- **Odchylenie std.:** 0-30

#### Krok 3: SPRAWDŹ
- Kliknij "✓ SPRAWDŹ"
- Zobaczysz prawdziwe wartości vs Twoje estymaty
- **Linie na histogramie:**
  - 🔴 Czerwona linia (ciągła) = Prawdziwa wartość
  - 🟠 Pomarańczowa linia (przerywana) = Twoja estymata

#### Krok 4: Punkty
- **Max 100 punktów za statystykę**
- Punkty obliczane: `max(0, 100 - błąd × 5)`
- Przykład: Błąd 5 → 100 - 5×5 = 75 pkt
- Przykład: Błąd 20 → 0 pkt (za duży błąd)

#### Krok 5: Następna runda
- Kliknij "➡ NASTĘPNA RUNDA"
- Nowy histogram, nowe zgadywanie!

---

### **3. Podsumowanie gry**

Po ostatniej rundzie zobaczysz:

#### Suma punktów
```
Suma punktów: 850 / 1000
```
(Dla 5 rund × 2 statystyki)

#### Ocena końcowa
- **< 40%:** ❌ Spróbuj ponownie!
- **40-60%:** ⭐ Nieźle!
- **60-80%:** ⭐⭐ Dobrze!
- **80-90%:** ⭐⭐⭐ Bardzo dobrze!
- **> 90%:** ⭐⭐⭐⭐ EKSPERT STATYSTYKI!

#### Szczegóły wszystkich rund
Tabela z punktami i błędami dla każdej rundy.

---

## 📚 Jak używać na zajęciach

### Scenariusz 1: Indywidualna gra (samodzielne ćwiczenie)
1. Studenci grają samodzielnie na swoich komputerach
2. Każdy wybiera własny poziom trudności
3. Po grze: dyskusja - "Która statystyka była najtrudniejsza?"
4. **Czas:** 10-15 min

### Scenariusz 2: Turniej grupowy (rywalizacja)
1. Podziel klasę na grupy 3-4 osoby
2. Każda grupa gra jedną grę (5 rund, poziom średni)
3. Grupy zapisują swój wynik końcowy
4. Wygrywa grupa z najwyższym wynikiem!
5. **Nagroda:** Punkty bonusowe? Cukierki? 🍬
6. **Czas:** 15-20 min

### Scenariusz 3: Demonstracja na wykładzie
1. Wyświetl grę na projektorze
2. **Wersja 1:** Ty grasz, studenci podpowiadają estymaty
3. **Wersja 2:** Losowy student wychodzi i gra przy tablicy
4. Po każdej rundzie: dyskusja "Jak zgadywaliście?"
5. **Czas:** 10 min (3 rundy)

### Scenariusz 4: Zadanie domowe
1. Każdy student gra w domu (10 rund, poziom trudny)
2. Robi screenshot wyniku końcowego
3. Przesyła do Ciebie (zadanie zaliczeniowe)
4. **Próg zaliczenia:** > 60% punktów
5. **Czas:** 15-20 min (w domu)

---

## 🎯 Koncepcje pedagogiczne

### Co ćwiczy ta gra?

1. **Wizualna interpretacja histogramu**
   - Rozpoznawanie gdzie jest "środek" danych
   - Ocena rozproszenia "na oko"
   - Asymetria rozkładu

2. **Intuicja statystyczna**
   - Średnia vs mediana - jak się różnią?
   - Co to znaczy "typowa odległość od średniej"? (SD)
   - Jak kształt rozkładu wpływa na statystyki?

3. **Feedback i uczenie się z błędów**
   - Natychmiastowy feedback: prawdziwa vs estymata
   - Wizualizacja błędu na histogramie (linie)
   - Progresja trudności (łatwy → trudny)

4. **Gamifikacja statystyki**
   - Punkty, rankingi, rywalizacja
   - Motywacja do ćwiczenia
   - "Statystyka może być zabawna!"

### Które statystyki są najtrudniejsze?

**Z doświadczenia:**
1. **Średnia** - Najłatwiejsza (punkt równowagi)
2. **Mediana** - Średnia trudność (środek, ale inaczej niż średnia)
3. **Odchylenie standardowe** - Najtrudniejsze! (abstrakcyjna miara)

**Strategia nauczania:**
- Zacznij od gier tylko ze średnią i medianą
- Później dodaj SD gdy omówisz ten temat

---

## 💡 Wskazówki dla graczy

### Jak zgadywać średnią?
- Szukaj "punktu równowagi" histogramu
- Wyobraź sobie że histogram to huśtawka - gdzie byłby punkt podparcia?
- Outliers przesuwają średnią w swoją stronę

### Jak zgadywać medianę?
- Podziel histogram na dwie połowy (równa liczba obserwacji)
- Mediana to granica między lewą a prawą połową
- W rozkładzie symetrycznym: mediana ≈ średnia
- W rozkładzie skośnym: mediana bliżej mody niż średnia

### Jak zgadywać odchylenie standardowe?
- SD = "typowa odległość od średniej"
- Dane bardzo skupione → małe SD (~5)
- Dane bardzo rozproszone → duże SD (~15-20)
- ~68% danych mieści się w przedziale średnia ± SD

---

## 🛠️ Techniczne szczegóły

### Generowanie danych

#### Poziom łatwy
5 typów rozkładów:
- **Normalny niski:** mean=30, sd=5
- **Normalny wysoki:** mean=70, sd=8
- **Jednostajny:** uniform(20, 80)
- **Skośny prawo:** Gamma(shape=2, scale=10) + 20
- **Bimodalny:** mieszanka 2 normalnych (mean=30 i mean=70)

#### Poziom średni
- Rozkład normalny
- Średnia: losowa 40-60
- SD: losowe 5/10/15

#### Poziom trudny
- Rozkład normalny
- Średnia: losowa 48-52 (bardzo podobne!)
- SD: losowe 8-12

### System punktowy

```r
error = |estymata - prawdziwa_wartość|
points_per_stat = max(0, 100 - error × 5)
```

**Przykłady:**
- Błąd 0 → 100 pkt (perfekcyjnie!)
- Błąd 5 → 75 pkt
- Błąd 10 → 50 pkt
- Błąd 15 → 25 pkt
- Błąd ≥ 20 → 0 pkt

**Suma punktów:**
```
Max punkty = liczba_rund × liczba_statystyk × 100
```

Np. 5 rund × 2 statystyki = max 1000 pkt

---

## 🎓 Rozszerzenia pedagogiczne

### Dyskusja po grze

**Pytania dla studentów:**
1. "Która statystyka była najtrudniejsza do zgadnięcia? Dlaczego?"
2. "Jak rozróżnić średnią od mediany na histogramie?"
3. "Czy łatwiej zgadywać statystyki dla rozkładów symetrycznych czy skośnych?"
4. "Co by pomogło w lepszym zgadywaniu? (wskazówki wizualne, więcej ćwiczeń?)"

### Warianty gry

**Wariant 1: Praca w parach**
- Dwóch studentów gra razem
- Muszą uzgodnić estymaty przed sprawdzeniem
- Punkty dzielone 50/50

**Wariant 2: Ograniczony czas**
- Każda runda: 30 sekund na zgadnięcie
- Dodaje stres i sprawdza szybkość intuicji
- Wymaga timera (można dodać do app)

**Wariant 3: Bez sliderów**
- Studenci wpisują wartości ręcznie (numeric input)
- Trudniejsze, ale bardziej precyzyjne

---

## 🐛 Rozwiązywanie problemów

### Gra się nie uruchamia
- Sprawdź czy zaznaczono co najmniej jedną statystykę
- Kliknij "START GRY"

### Chcę zmienić zakres wartości
- Edytuj `app.R`
- Zmień `xlim(0, 100)` na inny zakres
- Dostosuj zakres sliderów

### Chcę inne typy rozkładów
- Edytuj funkcję `generate_round_data()` w `app.R`
- Dodaj własne scenariusze (np. rozkład wykładniczy)

### Punkty wydają się za łatwe/trudne
- Edytuj wzór punktów: `100 - error × 5`
- Zmień mnożnik `5` na wyższą/niższą wartość

---

## 📊 Statystyki graczy (opcjonalnie)

Jeśli chcesz zbierać statystyki:
- Dodaj `write.csv()` w podsumowaniu gry
- Zapisuj: imię, poziom, punkty, czas gry
- Analiza: która statystyka najtrudniejsza? (zbiorowe dane)

---

## 📧 Kontakt

Jeśli masz pytania lub pomysły na rozszerzenia, zapisz w pliku `feedback.md` w tym folderze.

---

## 🏆 High Scores (przykładowe)

```
===========================================
    HALL OF FAME - Gra w Estymację
===========================================
1. Anna K.    - 980/1000 pkt (98%) ⭐⭐⭐⭐
2. Piotr M.   - 920/1000 pkt (92%) ⭐⭐⭐⭐
3. Kasia W.   - 850/1000 pkt (85%) ⭐⭐⭐
4. Tomek B.   - 780/1000 pkt (78%) ⭐⭐
5. Magda S.   - 720/1000 pkt (72%) ⭐⭐
===========================================
Czy pobijesz rekord? 🎮
```

_(Możesz wydrukować i powiesić w klasie!)_