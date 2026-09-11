# Dane syntetyczne do szeregów czasowych

Wszystkie siedem zbiorów CSV generuje `generate_data.R` z ziarnem 2024. Nie są to dane GUS, IMGW, giełdy ani innych instytucji. Nazwy miejsc i kalendarz służą kontekstowi dydaktycznemu. Kolumna `data_origin` zachowuje tę informację po skopiowaniu pliku.

Trendy bezrobocia i cen pszenicy interpolujemy między datami scenariusza. Szok cenowy jest zaprojektowany na 2022 r.; przejściowy spadek noclegów na lata 2020–2021. Nie należy interpretować ich wartości jako oszacowań skutków rzeczywistych wydarzeń.

## Regeneracja

Z tego katalogu:

```sh
Rscript generate_data.R
```

Aby wygenerować pliki do istniejącego katalogu tymczasowego z katalogu repozytorium:

```sh
TS_DATA_OUTPUT_DIR=/tmp Rscript statystyka-2/04-szeregi-czasowe/dane/generate_data.R
```

Testy sprawdzają zgodność wszystkich CSV z generatorem, długości trendów oraz daty szoków. Pierwszy `log_return` w WIG20 ma wartość NA: brak poprzedniego tygodnia do wyliczenia zmiany.
