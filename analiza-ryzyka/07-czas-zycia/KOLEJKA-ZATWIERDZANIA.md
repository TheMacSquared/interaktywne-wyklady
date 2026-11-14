# Wykład 07 — kolejka zatwierdzania prototypów

Stan: 14 listopada 2026. Opis i uzasadnienie w
[PUNKTY-STOP-PROPOZYCJA.md](PUNKTY-STOP-PROPOZYCJA.md).

Oglądanie (z katalogu repo, osobny port, bez restartu huba):

```r
shiny::runApp("analiza-ryzyka/07-czas-zycia", port = 8810)
```

Każdy prototyp ma złotą plakietkę „Prototyp Pn · rodzaj” i stoi w miejscu,
w którym miałby zostać. Tam, gdzie są warianty, nad panelem jest notka
„Wariant: …”. Istniejące widgety zostały obok do porównania (z poprawionymi
osiami i odczytami).

## Inwentarz

| Rozdz. | Prototyp | Rodzaj | Plik | Decyzja do podjęcia |
|---|---|---|---|---|
| 1 | P1 „Dwie oferty, jedna średnia” | scena | ra | miejsce: na otwarcie rozdziału, przed głosowaniem (B) czy po nim (A); stoi w obu |
| 1 | P2A „Dwie gęstości, jedna średnia” | galeria | rb | P2A (jedna oś) czy P2B (dwa wykresy) |
| 1 | P2B „Dwie oferty, jedna średnia: gęstości osobno” | galeria | rb | jak wyżej |
| 2 | P3 „Badanie kończy się po 1200 h” | scena | rc | przyjąć / odrzucić |
| 2 | P4 „Koniec badania a średnia z awarii” | piaskownica | rd | zastępuje dwa panele ggplot (suwak i oś czasu) |
| 2 | P5 „Łączny czas pracy” | pojęcie (kadry) | rc | przyjąć / odrzucić |
| 2 | P6 „To samo badanie powtórzone wiele razy” | symulacja | rd | przyjąć / odrzucić |
| 3 | P7 „Te same awarie, dwa mianowniki” | piaskownica | re | przyjąć / odrzucić; szerokość okna (300 h) |
| 3 | P8 „Cztery funkcje, jedna chwila” | pojęcie | rb | zastępuje widget „Wspólny suwak czasu” |
| 3 | P9 „MTTF to pole pod R(t)” | pojęcie (kadry) | re | przyjąć / odrzucić |
| 3 | P10 „Pole pod hazardem” | pojęcie | rb | przyjąć / odrzucić |
| 4 | P11 „Ocalali wyglądają jak nowi” | pojęcie (kadry) | re | przyjąć / odrzucić; czwarty kadr (oferta B) zostaje czy idzie do P22 |
| 4 | P12 „Stały hazard: jedna krzywa, inna skala” | piaskownica | ra | zastępuje widget „Stały hazard” |
| 4 | P13 „Magazyn z trzema zapasowymi wentylatorami” | scena | rg | przyjąć / odrzucić |
| 4 | P14 „Czas do k-tej awarii” | piaskownica | rf | zastępuje widget gamma |
| 5 | P15 „Trzy dzienniki awarii” | scena | ri | przyjąć / odrzucić (otwarcie części B) |
| 5 | P16B „Rodzina Weibulla: cztery kształty, jedna skala” | galeria | rf | P16A, P16B albo oba (różne miejsca) |
| 5 | P16A „β obraca hazard, η rozciąga oś” | piaskownica | rf | zastępuje widget „R(t) i h(t) reagują razem” |
| 5 | P17A „Ten sam MTTF, inny ranking” | piaskownica | rf | P17A (krzywe) czy P17B (parki); zastępuje widget trzech modeli |
| 5 | P17B „Trzy parki, jedna średnia” | piaskownica | re | jak wyżej |
| 6 | P18 „Trzy zegary jednego wentylatora” | scena | ri | przyjąć / odrzucić; założenie skali czasu |
| 6 | P20 „Jeden Weibull nie zrobi wanny” | przypadek brzegowy | rh | przyjąć / odrzucić |
| 6 | P19 „Wanna: suma trzech mechanizmów” | piaskownica | rh | zastępuje widget wanny |
| 7 | P21A „Termin przeglądu na krzywej R(t)” | piaskownica | rh | P21A czy P21B; zastępuje panel „Decyzja” |
| 7 | P21B „Ryzyko jako pole przed kreską przeglądu” | piaskownica | rh | jak wyżej |
| 7 | P22 „Używany czy nowy na kolejne 500 h?” | piaskownica | rh | przyjąć / odrzucić; trzeci model (β = 0.7) jako przypadek brzegowy |
| 8 | P23A „Karty katalogowe, wersja poprawiona” | domknięcie | ri | P23A, P23B albo oba |
| 8 | P23B „Pięć pytań o wentylator Bananpolu” | domknięcie | ri | jak wyżej |

Pliki: `modules/stop_proto_<plik>.R` i `modules/proto_<plik>.js` (grupy rc i ri
mają tylko kadry z R, bez JS); wspólne narzędzia w
`modules/stop_proto_common.R` i `modules/sceny.js`.

## Nowe liczby (nie ma ich w tekście wykładu)

- P6: mediana średnich z awarii około 500–520 h, mediana szacunków (7.2) około
  1500 h (symulacja, ziarno 2026; teoretyczna średnia warunkowa 521 h).
- P7: ułamki liczone na 200 liniach parku (0.184 zamiast 0.181 z modelu;
  odczyt „z modelu” stoi obok).
- P15: awarie na działających w oknach po 500 h (0.42 → 0.22, 0.28 → 0.29,
  0.04 → 0.79) dla modeli z przykładu 7.8.
- P18: hazardy względne z przykładu 7.9 podzielone przez 1000 na godzinę;
  na obrazie nie ma liczb hazardu.
- P19: hazard w dnie wanny 0.184 (także w poprawionym widgecie ggplot).
- P22 i P23A: wczesne defekty β = 0.7, η = 1185 h — używany 0.747, nowy 0.579.
- P21A: terminy dla R* = 0.95 i 0.80 (385 i 803 h; wykładniczy 77 i 335 h).

## Znane słabości

- P1, P9, P11, P17B: linie parku to kwantyle modelu, nie losowanie; kadr 2
  sceny P1 („w kolejności montażu”) pokazuje te same linie przetasowane.
  Linie oferty A dłuższe niż 4500 h są ucięte na krawędzi osi.
- P6, P13: pierwszy widok to jedno badanie albo jeden okres, więc słupek
  wychodzi ponad stałą oś, a licznik startuje od 100%.
- P6: pasek odczytów zawija się do dwóch wierszy.
- P8: przy t = 0 napis „t = 0 h” zachodzi na początek krzywej R(t).
- P12, P16A: gdy suwaki stoją na modelu odniesienia (MTTF 1500 h, β = 1),
  przerywany kontur pokrywa się z krzywą.
- P15: kreski dziennika są równo rozłożone (kwantyle), więc dziennik wygląda
  na gładszy niż prawdziwy; okno szóste dziennika 3 opiera się na 14 egzemplarzach.
- P20: przy β = 1 napis „punkt obrotu” leży na krzywej wannowej.
- P21A: przy R* = 0.95 napis terminu stoi ciasno pod linią wymagania.
- P23A, P23B i sceny z kadrami: nie sprawdzone na wąskim ekranie.
- Tryb ciemny i powiększenie wykresu (lupa) nie były sprawdzane.

## Po decyzjach

- Przyjęte: `proto = FALSE` w `c7p_panel()` (zwykła plakietka rodzaju),
  usunąć notki `c7p_variant()` i zastąpiony widget ggplot wraz z jego
  kodem serwera w `block.R`; dopasować tekst wokół (polecenia `zycie_try()`
  i odczyty `zycie_reading()` odnoszą się dziś do starych widgetów).
- Odrzucone: usunąć obiekt i jego rycinę albo scenę z plików grupy oraz
  wpis z testu `tests/testthat/test-c7-browser.R`.
- Pliki `ra`–`ri` nazwać według rozdziałów, gdy lista się ustali.
