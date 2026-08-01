# Wspólne dane kierunkowe

## `satelitarne_obserwacje.csv`

Syntetyczny zbiór dydaktyczny dla kierunku **Inżynieria danych satelitarnych i
kosmicznych**. Jeden wiersz odpowiada jednej lokalizacji obserwowanej jednego
dnia. Tabela reprezentuje dane po wstępnym przygotowaniu, a nie surowe obrazy
satelitarne ani produkt konkretnej misji.

Najważniejsze zmienne:

- `typ_pokrycia`, `strefa`, `region` — zmienne jakościowe;
- `jakosc_pomiaru` — zmienna porządkowa;
- `sat_temp_c`, `grunt_temp_c`, `roznica_temp_c` — pomiary temperatury i ich różnica;
- `ndvi` — prosty wskaźnik roślinności, używany tu wyłącznie jako zmienna liczbowa;
- `zachmurzenie_pct`, `wysokosc_m` — warunki i cechy lokalizacji;
- `pomiar_dostepny` — informacja, czy pomiar przeszedł prostą kontrolę jakości.

Zbiór odtwarza skrypt:

```sh
Rscript statystyka/scripts/generate_satellite_data.R
```

Wartości zostały zaprojektowane tak, aby ilustrować niewielkie systematyczne
zawyżanie temperatury przez sensor, różnice między terenami miejskimi i
zielonymi oraz wpływ zachmurzenia na jakość obserwacji. Nie należy interpretować
ich jako empirycznych parametrów rzeczywistego instrumentu.
