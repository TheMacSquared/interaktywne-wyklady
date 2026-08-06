# Regresja

Jeden, pełny wykład o regresji liniowej, wielorakiej i logistycznej. Materiał
jest obszerny celowo: prowadzący wybiera rozdziały i pogłębienia odpowiednio do
czasu oraz kierunku studiów, bez utrzymywania osobnych wersji aplikacji.

## Wymagania

- R w wersji co najmniej 4.1;
- pakiety: `shiny`, `ggplot2`, `dplyr`, `broom`, `palmerpenguins`.

## Uruchamianie

```r
shiny::runApp("statystyka/06-regresja")
```

## Organizacja materiału

Pierwszy rozdział jest mapą dla prowadzącego. Pokazuje dwanaście tematów
składających się na wspólną historię i pomaga zdecydować, co omówić podczas
konkretnego spotkania. Nie tworzy osobnych wariantów aplikacji.

Materiał wykorzystuje dwa przypadki:

- `CASchools` — regresja prosta, diagnostyka, model wieloraki i kontekst
  społeczno-ekonomiczny;
- `Palmer Penguins` — predyktory jakościowe, pominięta zmienna, paradoks
  Simpsona i interakcje.

Pingwiny nie zastępują danych szkolnych. Każdy przypadek jest używany tam,
gdzie najczytelniej pokazuje konkretny mechanizm.

## Proponowany rytm

- rdzeń: pytanie → model liniowy → output → R²/RMSE → model wieloraki →
  porównanie modeli → logistyczna;
- pogłębienie: założenia, ekstrapolacja, zmienne jakościowe, paradoks Simpsona,
  interakcje i train/test;
- zakończenie: ściąga i ćwiczenia na obu przypadkach.

## Koncepcje pedagogiczne

- interpretacja współczynników w języku problemu;
- różnica między predykcją i wyjaśnianiem;
- reszty, R², RMSE, AIC i BIC;
- interpretacja ceteris paribus;
- predyktory jakościowe i poziom odniesienia;
- pominięta zmienna oraz paradoks Simpsona;
- interakcja jako zależność efektu od kontekstu;
- prawdopodobieństwo, iloraz szans i próg decyzji w regresji logistycznej;
- utrata informacji przy sztucznym progowaniu zmiennej ciągłej.
