# Regresja — pingwiny

Wariant wykładu o regresji na danych palmerpenguins, dobranych pod kątem wyraźnych
interakcji i paradoksu Simpsona. Te same widgety i struktura co w `06-regresja/`,
inny zbiór danych.

## Wymagania

- R (wersja >= 4.0)
- Pakiety R: `shiny`, `ggplot2`, `dplyr`, `broom`

Dane (`dane/penguins.csv`) są dołączone do repozytorium — aplikacja jest samowystarczalna.
Plik wygenerowano z pakietu `palmerpenguins` (`na.omit(palmerpenguins::penguins)`, 333 pełne wiersze).

## Uruchamianie

```r
shiny::runApp("statystyka/06-regresja-pingwiny")
```

## Jak używać na zajęciach

### Scenariusze pedagogiczne

- **Regresja prosta (ch1):** masa ciała w zależności od długości płetwy; predykcja i kodowanie
  zmiennej dwustanowej (płeć).
- **Jakość modelu (ch2):** reszty modelu `wysokość dzioba ~ długość dzioba` rozpadają się na
  trzy chmury gatunkowe — wizualna zapowiedź potrzeby regresji wielorakiej.
- **Regresja wieloraka (ch3):** budowanie modelu masy ciała z wielu wymiarów oraz
  **paradoks Simpsona** (`wysokość dzioba ~ długość dzioba`): współczynnik zmienia znak
  z ujemnego na dodatni po dodaniu gatunku.
- **Interakcje (ch3a):** `masa ~ długość płetwy × gatunek` — nachylenia linii predykcji
  są wyraźnie różne między gatunkami (istotna interakcja, p ≈ 0.002).
- **Regresja logistyczna (ch5):** masa ciała progowana na „ciężki / lekki" pingwin.
- **Ćwiczenia (ch7):** zadania na zmiennych pingwinów + jeden symulowany przykład logistyczny.

### Interaktywne elementy

Suwaki parametrów, wybór predyktorów (checkboxy), przełączniki modelu addytywny/interakcyjny,
kroki widgetów, powiększanie wykresów, ujawnianie rozwiązań ćwiczeń — identyczne jak w wykładzie bazowym.

## Koncepcje pedagogiczne

### Co ilustruje to narzędzie?

- Interpretacja współczynników i predykcja w regresji prostej i wielorakiej.
- Diagnostyka reszt i wykrywanie pominiętej zmiennej grupującej.
- Paradoks Simpsona i sens kontroli zmiennych (ceteris paribus).
- Interakcja: kiedy efekt jednego predyktora zależy od poziomu drugiego.
- Regresja logistyczna dla zmiennej 0/1.

## Techniczne szczegóły

- Aplikacja korzysta ze współdzielonego systemu layoutu w `statystyka/R/`
  (`palette.R`, `theme_upwr.R`, `shared.R`, `lecture_layout.R`) — `lecture_id = "regresja"`.
- Moduły w `modules/` są adaptacją modułów z `06-regresja/`; identyfikator danych `.cas_data`
  jest wspólny dla wszystkich rozdziałów i wskazuje `dane/penguins.csv`.
- Rozdziały ch4 (porównanie modeli) i ch6 (ściąga) używają danych generowanych/statycznych
  i są identyczne jak w wykładzie bazowym.
