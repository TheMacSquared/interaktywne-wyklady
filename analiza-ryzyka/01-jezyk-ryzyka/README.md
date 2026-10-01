# Od zagrożenia do prawdopodobieństwa

Pierwszy interaktywny wykład z analizy ryzyka. Student porządkuje sytuację
ryzykowną, obserwuje stabilizację częstości i buduje przestrzeń jednakowo
możliwych wyników.

## Uruchamianie

```r
shiny::runApp("analiza-ryzyka/01-jezyk-ryzyka")
```

## Efekty uczenia się

Po wykładzie student potrafi:

- rozdzielić zagrożenie, ekspozycję, zdarzenie, skutek i zabezpieczenie;
- nazwać licznik, mianownik, jednostkę oraz okres obserwacji;
- odróżnić częstość empiryczną od prawdopodobieństwa modelowego;
- rozpoznać warunki stosowania klasycznej definicji prawdopodobieństwa;
- zakwestionować porównanie oparte na samych licznikach zdarzeń.

## Interaktywne elementy

1. Klasyfikacja historii o poślizgnięciu na skórce od banana.
2. Symulacja zmian pokazująca stabilizację częstości.
3. Siatka palet dla klasycznej definicji prawdopodobieństwa i dopełnienia.
4. Siatka stu kontroli dla sumy, części wspólnej i dopełnienia zdarzeń.
5. Porównanie prawdopodobieństwa, skutków i kryteriów decyzji.
6. Rozdział „Ściąga i sprawdzenie”: ściąga, quiz z pięcioma pytaniami i dwanaście
   ćwiczeń — cztery interaktywne (raport, zbiory, dobór modelu, transfer kontekstu)
   i osiem z odpowiedziami zwiniętymi pod treścią.

Treść jest zapisana w `modules/block.R` w tym samym formacie co wykłady 02–10
(`risk_block_chapters()` z `R/risk_block.R`); `modules/helpers.R` zawiera czyste
funkcje widżetów, testowane w `tests/testthat/test-lecture-01-helpers.R`.

Wartości liczbowe są fikcyjnymi parametrami dydaktycznymi, nie estymacjami
rzeczywistego ryzyka zawodowego.
