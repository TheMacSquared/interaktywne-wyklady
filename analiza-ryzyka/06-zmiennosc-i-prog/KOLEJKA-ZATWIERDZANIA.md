# Wykład 06 — kolejka zatwierdzania prototypów

Stan: 14 listopada 2026. Opis i uzasadnienie w
[PUNKTY-STOP-PROPOZYCJA.md](PUNKTY-STOP-PROPOZYCJA.md).

Oglądanie (z katalogu repo, osobny port, bez restartu huba):

```r
shiny::runApp("analiza-ryzyka/06-zmiennosc-i-prog", port = 8780)
```

Każdy prototyp ma złotą plakietkę „Prototyp Pn · rodzaj” i stoi w miejscu,
w którym miałby zostać. Tam, gdzie są warianty, nad panelem jest notka
„Wariant: …”. Istniejące widgety zostały obok do porównania.

## Inwentarz

| Rozdz. | Prototyp | Rodzaj | Plik | Decyzja do podjęcia |
|---|---|---|---|---|
| 1 | P1 „Trzy wentylatory, jedna średnia” | scena | ra | miejsce: przed głosowaniem (B) czy po nim (A); stoi w obu |
| 1 | P2A „Zmiana po zmianie” | symulacja | rb | P2A czy P2B (jedna puenta); zastępuje histogram z suwakiem |
| 1 | P2B „Ogon dochodzi ostatni” | symulacja | rb | jak wyżej |
| 2 | P3 „Pole między dwiema kreskami” | pojęcie | rb | przyjąć / odrzucić |
| 2 | P5 „Linijka σ” | pojęcie | rb | przyjąć / odrzucić |
| 2 | P4 „Przesuń i rozszerz krzywą” | piaskownica | ra | zastępuje widget ggplot „Model” |
| 3 | P6 „Temperatura i ciśnienie na jednej linijce” | pojęcie (kadry) | rc | przyjąć / odrzucić |
| 3 | P7 „Próg temperatury łożyska” | piaskownica | rc | zastępuje widget ggplot „Ogon” |
| 3 | P8 „Dzienniki zmian” | scena | rc | 8b jako przełącznik w scenie czy osobny obraz P8b |
| 3 | P8b „Te same pomiary, inne zmiany” | przypadek brzegowy | rc | jak wyżej |
| 4 | P9A „Trzy interwencje” | galeria | rd | P9A (przełącznik) czy P9B (obok siebie); zastępuje listę wyboru |
| 4 | P9B „Trzy interwencje obok siebie” | galeria | rd | jak wyżej |
| 4 | P10A „Próg poniżej średniej” | przypadek brzegowy | rd | P10A (suwak σ) czy P10B (galeria) |
| 4 | P10B „Stabilizacja przy progu 80°C” | przypadek brzegowy | rd | jak wyżej |
| 5 | P11 „Zawiesie dźwigu” | scena | re | zastępuje chmurę ggplot; stoi na otwarciu rozdziału |
| 5 | P12 „Od chmury par do marginesu” | pojęcie (kadry) | re | przyjąć / odrzucić |
| 5 | P13 „Pole nakładania to nie ryzyko” | przypadek brzegowy | rd | przyjąć / odrzucić |
| 6 | P14A „Ten sam środek, inny ogon” | galeria | rf | P14A (jedna oś, powiększenie ogona) czy P14B (obok siebie) |
| 6 | P14B „Trzy modele obok siebie” | galeria | rf | jak wyżej |
| 6 | P15A „Pięć pomiarów obok modelu” | pojęcie (kadry) | rf | P15A, P15B albo żaden |
| 6 | P15B „Wykres Q–Q z pięciu pomiarów” | pojęcie | rf | jak wyżej |
| 7 | P16A „Raport, wersja poprawiona” | domknięcie | rg | P16A, P16B albo oba |
| 8 | P16B „Od pytania do działania” | domknięcie | rg | jak wyżej |

Pliki: `modules/stop_proto_<plik>.R` i `modules/proto_<plik>.js`; wspólne
narzędzia w `modules/stop_proto_common.R` i `modules/sceny.js`.

## Znane słabości

- P14B: pole powyżej 91°C jest z natury ledwo widoczne; różnicę niosą liczby
  nad wykresami. P14A pokazuje ją w powiększeniu ogona.
- P2A, P2B: przy kilku–kilkunastu pomiarach najwyższy słupek wychodzi ponad
  stałą oś (0.3) i jest ucięty.
- P8, P11: pierwszy widok to jedna zmiana albo jedno podniesienie, więc
  licznik startuje od 100%.
- P9A, P10A: w stanie bazowym przerywany kontur pokrywa się z krzywą i widać
  go dopiero po zmianie.
- P16A, P16B: nie sprawdzone na wąskim ekranie.

## Po decyzjach

- Przyjęte: `proto = FALSE` w `z6p_panel()` (zwykła plakietka rodzaju),
  usunąć notki `z6p_variant()` i zastąpiony widget ggplot wraz z jego
  kodem serwera w `block.R`; dopasować tekst wokół (polecenia `risk_try()`
  odnoszą się dziś do starych widgetów).
- Odrzucone: usunąć obiekt i jego rycinę albo scenę z plików grupy oraz
  wiersz z testu `tests/testthat/test-z6-browser.R`.
- Pliki `ra`–`rg` nazwać według rozdziałów, gdy lista się ustali.
