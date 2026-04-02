# Klucz odpowiedzi: Rozkłady prawdopodobieństwa w BHP

---

## Blok 1: Kalkulator rozkładów

### Zadanie 1 — Szkolenie BHP

a) **B(25, 0.5)** — 25 prób Bernoulliego, p = 0.5 (losowe T/F)
b) **P(X ≥ 20) = 0.0020** (~0.2%) — praktycznie niemożliwe zdać zgadując
c) **E(X) = np = 25 × 0.5 = 12.5** odpowiedzi poprawnych
d) **P(X ≥ 15) = 0.2122** (~21%) — dużo łatwiej, ale nadal mało prawdopodobne

### Zadanie 2 — Wypadki przy pracy

a) **Pois(λ = 2.5)** — zliczamy zdarzenia w ustalonym czasie
b) **P(X = 5) = 0.0668** (~6.7%)
c) **P(X = 0) = 0.0821** (~8.2%)
d) **P(X > 4) = 0.1088** (~10.9%)
e) W kwartale: **Pois(λ = 7.5)** (addytywność Poissona). **P(X ≥ 10) = 0.2236** (~22.4%)

### Zadanie 3 — Poziom hałasu

a) **P(X > 85) = 0.2266** (~22.7%) — ponad 1/5 pomiarów przekracza normę!
b) **P(78 < X < 86) = 0.6827** (~68.3%) — to dokładnie reguła μ±σ
c) **Kwantyl 95%: 88.6 dB** — 5% pomiarów jest powyżej tej wartości
d) **P(X > 90) = 0.0228** (~2.3%) — „prawie nigdy" to ~2%, więc pracodawca ma rację, ale nadal to nie jest 0

### Zadanie 4 — Niezawodność czujnika dymu

a) **λ = 1/365 ≈ 0.00274** (awarii na dzień)
b) **P(X < 180) = 0.3893** (~38.9%) — prawie 2 na 5 czujników padnie przed pół rokiem
c) **P(X > 730) = 0.1353** (~13.5%)
d) **Nie zmienia** — rozkład wykładniczy jest **bezpamięciowy**. P(X > 200+180 | X > 200) = P(X > 180) = 0.3893. Czas już przepracowany nie wpływa na przyszłość. W praktyce: stary czujnik nie jest bardziej „zmęczony" niż nowy (w modelu wykładniczym — w rzeczywistości degradacja istnieje, ale ten model jej nie uwzględnia).

---

## Blok 2: Rozpoznawanie rozkładów

### Zadanie 5

| | Sytuacja | Rozkład | Parametry |
|---|---|---|---|
| a) | 20 stanowisk, 10% naruszenie, ile naruszeń? | **Dwumianowy** | B(20, 0.1) |
| b) | 3 alarmy/tydzień, ile w następnym? | **Poissona** | Pois(3) |
| c) | Czas karetki, średnia 8, sd 2, symetryczny | **Normalny** | N(8, 2) |
| d) | Losowy moment w 8h zmianie | **Jednostajny ciągły** | U(0, 8) |
| e) | 50 gaśnic, 4% przeterminowanych | **Dwumianowy** | B(50, 0.04) |
| f) | Sprawdza budynki do pierwszego naruszenia, 15% | **Geometryczny** | Geom(0.15) |
| g) | 1 wypadek co 20 dni, ile dni do następnego? | **Wykładniczy** | Exp(λ = 1/20) |
| h) | Waga ładunku, średnia 500, sd 30 | **Normalny** | N(500, 30) |

### Zadanie 6a — Kolizje drogowe

- **Liczba** kolizji w miesiącu: **Pois(λ = 4)**
- **Czas** między kolizjami: **Exp(rate = 4/30)** → średnio co **7.5 dnia**
- **Związek:** Jeśli zdarzenia zachodzą wg procesu Poissona z intensywnością λ, to czas między nimi ma rozkład wykładniczy z tym samym λ (przeliczonym na tę samą jednostkę czasu). Poisson zlicza, wykładniczy mierzy odstępy.

### Zadanie 6b — Rękawice ochronne

- Liczba wadliwych: **B(100, 0.03)**
  - E(X) = 100 × 0.03 = **3 rękawice**
  - **P(X ≥ 5) = 0.1821** (~18.2%)
- Numer pierwszej wadliwej: **Geom(p = 0.03)**
  - **E(X) = 1/0.03 ≈ 33.3** — średnio co 33. rękawica jest wadliwa

### Zadanie 6c — Stężenie pyłu

- P(X > 5.0) przy N(4.2, 0.8) = **0.1587** (~15.9%) — za dużo!
- Aby P(X > 5.0) < 0.05: potrzebne 5.0 = μ + 1.645 × 0.8 → **μ ≤ 3.68 mg/m³**
- Trzeba obniżyć średnie stężenie z 4.2 do **3.68 mg/m³** (redukcja o 0.52)

---

## Blok 3: Analiza danych w Jamovi

### Zadanie 7 — Wypadki miesięcznie

*Uwaga: wartości empiryczne zależą od generatora (set.seed(2026)). Poniżej wartości orientacyjne.*

a) Histogram: dyskretny, wartości 0–7, skupione wokół 2-3
b) Średnia ≈ 2.1, wariancja ≈ 2.4 — **zbliżone** do siebie
c) **Poisson** z λ ≈ średnia z danych (~2.1)
d) Przy λ = 2.1:
   - P(X ≥ 5) ≈ 0.05 (teoretyczne)
   - P(X = 0) ≈ 0.12 (teoretyczne)
e) Porównanie: empiryczne częstości powinny być zbliżone do teoretycznych (±kilka pp)

### Zadanie 8 — Hałas na stanowiskach

a-b) Statystyki:
   - **Stanowisko A:** średnia ≈ 82, sd ≈ 3.4, skośność ≈ 0 — **symetryczne**
   - **Stanowisko B:** średnia ≈ 79, sd ≈ 11, skośność > 0 — **skośne prawo**
c) **Stanowisko A** jest bliższe normalnemu — symetryczny histogram, skośność bliska 0, średnia ≈ mediana
d) Przy N(82, 3.4): P(X > 85) ≈ 19%
e) Hipoteza: spawalnia generuje sporadyczne szczyty hałasu (np. uruchomienie spawarki, szlifowanie) — większość czasu cicho, ale z ostrymi pikami → rozkład skośny prawostronnie

### Zadanie 9 — Kontrola kasków

a) Histogram: wartości 0–5, najczęściej 0–2
b) Średnia ≈ 1.6 wadliwych na partię
c) p = 1.6/30 ≈ **0.053** (~5.3%)
d) Przy B(30, 0.053): P(X ≥ 3) ≈ 0.20 (~20%)
e) P(X ≥ 4) ≈ 0.08 (~8%) — co ~12-13 partia byłaby odrzucona

### Zadanie 10 — Czas między incydentami

a) Histogram: **silnie skośny prawo** — dużo krótkich odstępów, mało bardzo długich
b) Średnia ≈ 14 dni, sd ≈ 13 dni — **średnia ≈ sd** (cecha rozkładu wykładniczego: E(X) = SD(X) = 1/λ)
c) λ = 1/14 ≈ **0.071** incydentu/dzień
d) P(X < 7) = 1 − e^(−7/14) ≈ **0.39** (~39%)
e) Przeliczenie: jeśli średnio co 14 dni, to w 30 dniach oczekujemy **30/14 ≈ 2.14** incydentu → **Pois(λ = 2.14)**
   - P(X ≥ 3) ≈ 0.33 (~33%)

---

## Pytania podsumowujące

1. **Poisson i wykładniczy** — najczęstsze w BHP, bo wypadki/incydenty to rzadkie zdarzenia w czasie. Normalny — przy pomiarach środowiskowych (hałas, zanieczyszczenia).
2. Histogram wykładniczy: **silnie skośny prawo**, zaczyna się wysoko i opada. Normalny: **symetryczny dzwon**.
3. Bezpamięciowość: w modelu wykładniczym czujnik, który pracuje 2 lata, ma taką samą szansę awarii jutro jak nowy. W praktyce to uproszczenie — rzeczywiste urządzenia się zużywają (→ rozkład Weibulla). Ale model wykładniczy sprawdza się dla awarii losowych (przepięcia, uderzenia pioruna), nie mechanicznego zużycia.
