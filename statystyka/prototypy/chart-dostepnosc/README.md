# Sterowanie wykresem i czytelność na rzutniku

Osobny podgląd minimalistycznego widgetu z wykładu 03: przeciąganie, klawiatura i mała lupa.

## Wymagania

- R >= 4.0
- Pakiety R: `shiny`, `ggplot2`, `dplyr` i zależności wspólnego layoutu kursu.
- Dostęp do internetu do załadowania Chart.js i fontów, jak w obecnych wykładach.

## Uruchamianie

Z głównego katalogu repo:

```r
shiny::runApp("statystyka/prototypy/chart-dostepnosc")
```

## Jak używać na zajęciach

### Scenariusze pedagogiczne

1. Zwężaj i poszerzaj przedział, obserwując prawdopodobieństwo.
2. Użyj wyłącznie Tab i strzałek.
3. Wybierz lupę po prawej, aby powiększyć wykres i napisy.

### Interaktywne elementy

- Przeciąganie granic.
- Uchwyty a/b dostępne klawiszem Tab, z obwódką widoczną po wejściu klawiaturą.
- Mała lupa z boku zamiast przycisku tekstowego.
- Strzałki: krok 0.5 cm; Shift + strzałka: 5 cm; Home/End na uchwycie: krańce zakresu.
- Skalowanie tekstu i etykiet osi, motywy jasny/ciemny.

## Koncepcje pedagogiczne

### Co ilustruje to narzędzie?

Pole między granicami pod gęstością normalną. Podgląd pokazuje też możliwość
obsługi bez myszy i czytelności na rzutniku, bez dodawania pól liczbowych.

## Techniczne szczegóły

Podgląd korzysta z `lecture_page()`, wspólnego `lc_charts.js` i produkcyjnego
`area_chart.js`. Tego samego widgetu używa wykład 03, Ryc. 4.3. Sam podgląd
nie jest włączony do huba.

Uchwyty mają semantykę ARIA slider, a krótkie podsumowanie
jest ogłaszane po przeciągnięciu granicy i operacji klawiaturowej. To demonstracja
proponowanej obsługi, nie deklaracja pełnej zgodności z WCAG.
