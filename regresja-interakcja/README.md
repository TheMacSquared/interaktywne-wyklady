# Regresja z Interakcją - Porównanie Modeli

Interaktywna wizualizacja różnicy między modelem addytywnym (bez interakcji) a modelem z interakcją w regresji liniowej.

## Wymagania

- R (wersja >= 4.0)
- Pakiety R: `shiny`, `ggplot2`, `dplyr`

## Uruchamianie

```r
shiny::runApp("regresja-interakcja")
```

## Jak używać na zajęciach

### Koncepcja pedagogiczna

Aplikacja pomaga studentom zrozumieć:

1. **Model addytywny**: `y = β₀ + β₁x + β₂grupa`
   - Linie regresji są **równoległe**
   - Grupa wpływa tylko na wyraz wolny (przesunięcie w pionie)
   - Nachylenie jest takie samo dla obu grup

2. **Model z interakcją**: `y = β₀ + β₁x + β₂grupa + β₃(x × grupa)`
   - Linie regresji mogą mieć **różne nachylenia**
   - Efekt X zależy od grupy (i odwrotnie)
   - Bardziej elastyczny, ale wymaga uzasadnienia

### Predefiniowane scenariusze

| Scenariusz | Opis | Kiedy używać |
|------------|------|--------------|
| **Linie równoległe** | Takie samo nachylenie, różne wyrazy wolne | Pokazanie, że interakcja nie zawsze jest potrzebna |
| **Linie przecinające się** | Przeciwne nachylenia | Silna interakcja - konieczność uwzględnienia |
| **Linie rozbieżne** | Różne nachylenia, ten sam kierunek | Umiarkowana interakcja |
| **Brak efektu grupy** | Identyczne linie | Kontrola - ani grupa, ani interakcja nie są istotne |

### Interaktywne elementy

- **Nachylenia i wyrazy wolne** - osobno dla każdej grupy
- **Poziom szumu** - wpływa na rozrzut punktów i istotność testów
- **Liczba obserwacji** - więcej danych = większa moc testu

## Scenariusze na zajęciach

### 1. Wprowadzenie do interakcji (15 min)

1. Zacznij od scenariusza "Linie równoległe"
2. Pokaż, że model addytywny dobrze pasuje (R² podobne)
3. Przełącz na "Linie przecinające się"
4. Omów, dlaczego teraz interakcja jest konieczna

### 2. Test istotności interakcji (10 min)

1. Użyj scenariusza "Linie rozbieżne"
2. Zwiększaj poziom szumu - obserwuj jak p-value rośnie
3. Zmniejszaj liczbę obserwacji - obserwuj spadek mocy testu
4. Dyskusja: kiedy interakcja jest "prawdziwa" vs. kiedy nie mamy mocy jej wykryć

### 3. Zasada parsymonii (10 min)

1. Pokaż scenariusz z nieistotną interakcją
2. Porównaj AIC obu modeli
3. Omów: "prostszy model jest lepszy, gdy złożony nie jest istotnie lepszy"

## Techniczne szczegóły

- Test porównania modeli: `anova(model_additive, model_interaction)`
- Kryterium istotności: p < 0.05
- Wyświetlane metryki: R², AIC, równania linii regresji
