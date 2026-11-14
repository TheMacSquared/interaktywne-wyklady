# Wykład 09 — kolejka zatwierdzania prototypów

Stan: 14 listopada 2026. Opis i uzasadnienie w
[PUNKTY-STOP-PROPOZYCJA.md](PUNKTY-STOP-PROPOZYCJA.md).

Oglądanie (z katalogu repo, osobny port, bez restartu huba):

```r
shiny::runApp("analiza-ryzyka/09-drzewo-bledow", port = 8830)
```

Każdy prototyp ma złotą plakietkę „Prototyp Pn · rodzaj” i stoi w miejscu,
w którym miałby zostać. Tam, gdzie są warianty, nad panelem jest notka
„Wariant: …”. Istniejące widgety zostały obok do porównania.

## Inwentarz

| Rozdz. | Prototyp | Rodzaj | Plik | Decyzja do podjęcia |
|---|---|---|---|---|
| 1 | P1 „Protokół po pożarze” | scena | sc | przyjąć / odrzucić; historia ogólna czy prawdziwe zdarzenie |
| 1 | P2 „Od »awarii chłodni« do zdarzenia szczytowego” | pojęcie (kadry) | sc | przyjąć / odrzucić |
| 1 | P3 „Trzy piętra drzewa” | pojęcie (kadry) | dr | przyjąć / odrzucić |
| 2 | P4A „Jutro egzamin o ósmej” | scena | sc | P4A (budzik) czy P4B (Bananpol) |
| 2 | P4B „Ta sama para urządzeń, dwie instalacje” | scena | sc | jak wyżej |
| 2 | P5 „Utrata kontroli nad zapłonem” | piaskownica | sc | zastępuje konstruktor z listą wyboru |
| 2 | P6A „Zapal liście, sprawdź szczyt” | piaskownica | sc | P6A, P6B albo oba; P6A zastępuje panel pól wyboru |
| 2 | P6B „Osiem kombinacji, trzy zapalają szczyt” | galeria | dr | jak wyżej |
| 2 | P7 „Ten sam system w dwóch językach” | pojęcie | dr | przyjąć / odrzucić |
| 3 | P8 „Liczby wchodzą od dołu” | pojęcie (kadry) | dr | przyjąć / odrzucić |
| 3 | P9 „Parametry małego drzewa” | piaskownica | sc | zastępuje widget ggplot „Obliczenia” |
| 3 | P10 „Suma liczy narożnik dwa razy” | przypadek brzegowy | wy | przyjąć / odrzucić |
| 4 | P11A „Najkrótsze drogi do szczytu” | galeria | dr | P11A (przełącznik) czy P11B (obok siebie); zastępuje „Wybierz przekrój” |
| 4 | P11B „Dwa minimalne przekroje” | galeria | dr | jak wyżej |
| 4 | P12 „Dwa wystąpienia, jedno źródło” | przypadek brzegowy | wy | zastępuje panel z samymi odczytami |
| 4 | P13 „Dwa kanały, jedno zasilanie” | pojęcie (kadry) | dr | przyjąć / odrzucić |
| 4 | P14 „Wspólna przyczyna jako most” | piaskownica | sc | zastępuje „Trzy minimalne przekroje”; wymaga zmiany dwóch zdań tekstu (niżej) |
| 5 | P15 „Dwa światy jednego liścia” | pojęcie | dr | przyjąć / odrzucić |
| 5 | P16A „Ta sama redukcja każdego liścia” | piaskownica | wy | P16A (słupki) czy P16B (pole); zastępuje widget ggplot |
| 5 | P16B „Ryzyko jako pole prostokąta” | piaskownica | wy | jak wyżej |
| 6 | P17 „Zapomniany zawór” | przypadek brzegowy (kadry) | dr | przyjąć / odrzucić |
| 6 | P18 „Przedział zamiast punktu” | pojęcie | wy | przyjąć / odrzucić |
| 7 | P19A „Jedno drzewo, pięć pytań” (przełącznik) | domknięcie | dr | P19A, P19B albo oba |
| 7 | P19B „Jedno drzewo, pięć pytań” (karta) | domknięcie | dr | jak wyżej |

Pliki: `modules/stop_proto_<plik>.R` i `modules/proto_<plik>.js` (grupa `dr`
nie ma własnego JS: jej sceny rysuje `modules/sceny.js`); wspólne narzędzia
w `modules/stop_proto_common.R` i `modules/sceny.js`.

## Poprawki istniejących obrazów (już w `block.R`)

- „Parametry małego drzewa”: liczby z suwaków stoją przy węzłach, liście D i S
  są pod bramką OR (wcześniej gałęzie krzyżowały się pod inicjacją), bez
  siatki i osi.
- „Ta sama redukcja względna”: stała oś Y do 0.00063 z linią wyjściowego
  P(TOP), wartości nad słupkami, oś bez notacji 1e-04.
- Nie zmienione: panel „Trzy minimalne przekroje” nadal czyta suwaki
  z rozdziału 3, bo tekst wykładu opisuje tę zależność wprost.

## Znane słabości

- P1: historia jest fikcyjna i nie należy do Bananpolu; kartki to zwykłe
  prostokąty, bez symboli bramek (celowo, pojęcie pada później).
- P6B: w miniaturach zdarzenie pośrednie ma napis „D ∪ S”, ciasno w ramce.
- P8, P9: przy stanie bazowym 6 kratek z 10 000 to mały znak w rogu siatki;
  taka jest skala zjawiska. Liczba kratek jest zaokrąglona (6 przy 6.3).
- P9: suwaki mają te same zakresy co stary widget; przy maksimum P(TOP)
  rośnie 24-krotnie i blok kratek ma 10 × 30.
- P12: przy q = 0.01 długi odczyt „100 razy za mało” spycha lupę do nowego
  wiersza.
- P13, kadr 5: drzewo naiwne (cztery liście) jest wyraźnie mniejsze od
  zredukowanego.
- P14: panel jest wysoki (drzewo i wykres); na niskim ekranie nie mieści się
  w jednym widoku. Liść C ma pod sobą literę „q”, nie liczbę.
- P16A: oś Y kończy się na 100% bez podpisu ostatniej kreski.
- P16B: osie mają różne skale, więc proporcje boków prostokąta nic nie znaczą;
  znaczenie ma tylko pole.
- P18: oś Y w jednostkach „na 10 000 magazyno-lat”, a napisy przy punktach
  w prawdopodobieństwach.
- P19A, widok „Co najpierw?”: procenty stoją w miejscu prawdopodobieństw liści.
- Żaden panel nie był sprawdzany na wąskim ekranie ani w trybie ciemnym.

## Po decyzjach

- Przyjęte: `proto = FALSE` w `f9p_panel()` (zwykła plakietka rodzaju),
  usunąć notki `f9p_variant()` i zastąpiony widget wraz z jego kodem serwera
  w `block.R`; dopasować tekst wokół (polecenia `risk_try()` odnoszą się dziś
  do starych widgetów).
- P14 po przyjęciu: zmienić zdanie „Panel korzysta z suwaków detekcji
  i tłumienia z rozdziału o rachunku…” oraz koniec polecenia „…wróć do
  rozdziału o rachunku i zmniejsz tam P(braku detekcji) do zera”.
- P9 po przyjęciu: usunąć `f9_init`, `f9_detect`, `f9_suppress` dopiero razem
  ze starym panelem wspólnej przyczyny, który z nich korzysta.
- Odrzucone: usunąć obiekt i jego scenę albo rycinę z plików grupy oraz
  wiersz z testu `tests/testthat/test-f9-browser.R`.
- Pliki `dr`, `sc`, `wy` nazwać według rozdziałów, gdy lista się ustali.
