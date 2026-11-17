# Wykład 06 — kolejka zatwierdzania prototypów

Stan: 14 listopada 2026. Opis i uzasadnienie w
[PUNKTY-STOP-PROPOZYCJA.md](PUNKTY-STOP-PROPOZYCJA.md).

Oglądanie (z katalogu repo, osobny port, bez restartu huba):

```r
system("Rscript scripts/build_static.R analiza-ryzyka/06-zmiennosc-i-prog")   # → dist/analiza-ryzyka/06-zmiennosc-i-prog.html
```

Każdy prototyp ma złotą plakietkę „Prototyp Pn · rodzaj” i stoi w miejscu,
w którym miałby zostać. Tam, gdzie są warianty, nad panelem jest notka
„Wariant: …”. Istniejące widgety zostały obok do porównania.

## Inwentarz

| Rozdz. | Prototyp | Rodzaj | Plik | Decyzja do podjęcia |
|---|---|---|---|---|
| 1 | P1 „Trzy wentylatory, jedna średnia” | scena | ra | **przyjęta 17.11**: po głosowaniu (A); w głosowaniu „Nie — potrzebujemy rozrzutu” → „Nie wiadomo” |
| 1 | P2B „Ogon dochodzi ostatni” | symulacja | rb | **przyjęta 17.11** zamiast histogramu z suwakiem; P2A odrzucona i usunięta |
| 2 | P3 „Pole między dwiema kreskami” | pojęcie | rb | **przyjęta 17.11** |
| 2 | P5 „Linijka σ” | pojęcie | rb | **przyjęta 17.11** |
| 2 | P4 „Przesuń i rozszerz krzywą” | piaskownica | ra | **przyjęta 17.11**, dawny panel „Model” usunięty |
| 3 | P6 „Temperatura i ciśnienie na jednej linijce” | pojęcie (kadry) | rc | **przyjęta 17.11** |
| 3 | P7 „Próg temperatury łożyska” | piaskownica | rc | **przyjęta 17.11**, dawny panel „Ogon” usunięty |
| 3 | P8 „Dzienniki zmian” | scena | rc | **przyjęta 17.11**: przełącznik pytania (o pomiar / o zmianę / oba), tylko pomiary niezależne, oko = model, ramka „Dwa pytania” w tekście |
| 3 | P8b „Te same pomiary, inne zmiany” | przypadek brzegowy | rc | **przyjęta 17.11** jako osobny obraz (jedyne miejsce pomiarów skorelowanych) |
| 4 | P9A „Trzy interwencje” | galeria | rd | **przyjęta 17.11** zamiast listy wyboru; P9B odrzucona i usunięta |
| 4 | P17 „Interwencje w praktyce” | przykłady | rh | **dodana i przyjęta 17.11**: 7 działań z zakładu na 6 slajdach (oba podniesienia progu na jednym) i zestawienie; sekcja „Kolejność działań” |
| 4 | P10A „Próg poniżej średniej” | przypadek brzegowy | rd | **przyjęta 17.11**; P10B odrzucona i usunięta |
| 5 | P11 „Lina dźwigu” | scena | re | **przyjęta 17.11** zamiast chmury par z suwakami (usunięta); tekst sekcji „Chmura par L i S” przepisany pod scenę, kN zamiast „jednostek” |
| 5 | P18 „Obciążenie i wytrzymałość w praktyce” | przykłady | ri | **dodana i przyjęta 17.11**: 5 sytuacji L–S (smar, regał, wyłącznik, dach, wał) i zestawienie z liną; po scenie P11 |
| 5 | P12 „Od chmury par do marginesu” | pojęcie (kadry) | re | **przyjęta 17.11**, przeniesiona za wzory (6.9)–(6.10) do sekcji „Różnica dwóch zmiennych normalnych” |
| 5 | P13 „Pole nakładania to nie ryzyko” | przypadek brzegowy | rd | **przyjęta 17.11** |
| 6 | P14A „Ten sam środek, inny ogon” | galeria | rf | **przyjęta 17.11** zamiast histogramu kształtów; P14B odrzucona i usunięta; stary panel został jako sam wykres Q–Q w sekcji „Jak czytać wykres kwantylowy” |
| 6 | P15A „Pięć pomiarów obok modelu” | pojęcie (kadry) | rf | **przyjęta 17.11**; P15B odrzucona i usunięta |
| 7 | P16A „Raport, wersja poprawiona” | domknięcie | rg | **przyjęta 17.11** (obie) |
| 8 | P16B „Od pytania do działania” | domknięcie | rg | **przyjęta 17.11**; układ zmieniony na pionową listę kroków |

Pliki: `modules/stop_proto_<plik>.R` i `modules/proto_<plik>.js`; wspólne
narzędzia w `modules/stop_proto_common.R` i `modules/sceny.js`.

## Znane słabości

- P8, P11: pierwszy widok to jedna zmiana albo jedno podniesienie, więc
  licznik startuje od 100%.
- P9A, P10A: w stanie bazowym przerywany kontur pokrywa się z krzywą i widać
  go dopiero po zmianie.

## Po decyzjach

- Przyjęte: `proto = FALSE` w `z6p_panel()` (zwykła plakietka rodzaju),
  usunąć notki `z6p_variant()` i zastąpiony widget ggplot wraz z jego
  kodem serwera w `block.R`; dopasować tekst wokół (polecenia `risk_try()`
  odnoszą się dziś do starych widgetów).
- Odrzucone: usunąć obiekt i jego rycinę albo scenę z plików grupy oraz
  wiersz z testu `tests/testthat/test-z6-browser.R`.
- Pliki `ra`–`rg` nazwać według rozdziałów, gdy lista się ustali.
