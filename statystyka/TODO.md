# TODO — pomysły do późniejszej realizacji

Lista rzeczy zauważonych przy okazji innej pracy, które warto kiedyś zrobić,
ale nie blokują obecnego zadania. Posortowane luźno wg modułu.

---

## Wykład: regresja

### Regresja do średniej — mini-widget

W wykładzie o korelacji (`wnioskowanie-statystyczne/modules/ch4_korelacja.R`,
ryc. 6.1/6.2/6.3) elipsy 95% pokazują rozkład punktów. Widać tam, że
**linia regresji nie pokrywa się z główną osią elipsy** — jest mniej stroma.
Dla małego r (np. r=0.31 w Ryc. 6.3 panel "Duży rozrzut") rozjazd jest
najwyraźniejszy: elipsa biegnie po skosie 1:1, regresja jest prawie pozioma.

To klasyczny obraz **regresji do średniej**: przewidywany y jest zawsze
bliżej zera niż wskazywałby kształt chmury. Wzór:

```
b = r × (sd_y / sd_x)
```

Pomysł na widget w `regresja/modules/ch1_liniowa.R` (lub osobnym module):

- scatter plot ze suwakiem `r` (np. 0.1–0.95)
- dwie linie na wykresie: główna oś elipsy (linia 1:1 przy `sd_x = sd_y`)
  i linia regresji `y ~ x`
- live'owe pokazanie wartości `b = r × (sd_y / sd_x)` poniżej
- przykład numeryczny: "ucznia z x = +2 SD spodziewasz się y = `r × 2` SD,
  nie y = +2 SD" — spłaszczenie ku średniej

Aktualnie w wykładzie o korelacji **nic o tym nie mówimy**, świadomie —
żeby nie odciągać od głównej puenty (r mierzy ciasność, nie nachylenie).
Ale w wykładzie o regresji to powinno wybrzmieć.

Powiązane pliki:
- [05-regresja/modules/ch1_liniowa.R](05-regresja/modules/ch1_liniowa.R)
- [04-wnioskowanie-statystyczne/modules/ch4_korelacja.R](04-wnioskowanie-statystyczne/modules/ch4_korelacja.R) (ryc. 6.1–6.3 jako odniesienie)
- [scripts/regen_correlation_assets.R](scripts/regen_correlation_assets.R) (generator elips)
