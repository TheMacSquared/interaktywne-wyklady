# Wykład 08 — kolejka zatwierdzania prototypów

Stan: 14 listopada 2026. Opis i uzasadnienie w
[PUNKTY-STOP-PROPOZYCJA.md](PUNKTY-STOP-PROPOZYCJA.md).

Oglądanie (z katalogu repo, osobny port, bez restartu huba):

```r
shiny::runApp("analiza-ryzyka/08-niezawodnosc-systemu", port = 8820)
```

Każdy prototyp ma złotą plakietkę „Prototyp Pn · rodzaj” i stoi w miejscu,
w którym miałby zostać. Tam, gdzie są warianty, nad panelem jest notka
„Wariant: …”. Istniejące widgety zostały obok do porównania.

## Inwentarz

| Rozdz. | Prototyp | Rodzaj | Plik | Decyzja do podjęcia |
|---|---|---|---|---|
| 1 | P1 „Nocna awaria, trzy raporty” | scena | ra | miejsce: przed głosowaniem (B) czy po nim (A); stoi w obu |
| 1 | P2A „Sto misji, trzy logiki” | galeria | rc | P2A (dokładne kratki) czy P2B (symulacja) |
| 1 | P2B „Misja za misją” | symulacja | rc | jak wyżej |
| 2 | P3 „Co znaczy »działa«?” | scena | rd | przyjąć / odrzucić; wynik 0.902 jest nowy |
| 2 | P4A „Dwa horyzonty na jednej osi czasu” | pojęcie (kadry) | rd | P4A (kadry) czy P4B (suwak) |
| 2 | P4B „Wspólny czas misji” | piaskownica | re | jak wyżej |
| 2 | P5A „Zepsuj blok, szukaj drogi” | piaskownica | rc | P5A (przełącznik) czy P5B (obok siebie); zastępuje widget „Schemat” |
| 2 | P5B „Trzy schematy obok siebie” | galeria | rc | jak wyżej |
| 3 | P6 „Osiem stanów, jedna wygrana” | galeria | rb | zastępuje pola wyboru szeregu |
| 3 | P7 „Długi łańcuch” | piaskownica | ra | przyjąć / odrzucić |
| 3 | P8 „Cztery stany, jedna przegrana” | galeria | rb | zastępuje pola wyboru układu równoległego |
| 3 | P9A „Kwadrat dwóch elementów” | piaskownica | re | P9A (pola) czy P9B (słupki); zastępuje widget „Architektura” |
| 3 | P9B „Te same R, dwa systemy” | piaskownica | re | jak wyżej |
| 4 | P10 „Zwijanie schematu” | pojęcie (kadry) | rd | zastępuje kroki redukcji bez obrazu |
| 4 | P11A „System pod najsłabszym szeregiem” | piaskownica | re | P11A czy P11B (jedna puenta); zastępuje widget „Czas” |
| 4 | P11B „Ile daje druga gałąź” | piaskownica | re | jak wyżej |
| 4 | P12 „Tabela stanów jako osiem schematów” | pojęcie | rb | przyjąć / odrzucić |
| 4 | P13 „Między szeregiem a układem równoległym” | galeria | rb | osobno czy jako przełącznik w P12 |
| 4 | P14 „Naprawa, która gasi system” | przypadek brzegowy | rb | przyjąć / odrzucić |
| 5 | P15 „Jedna rozdzielnica” | scena | rd | przyjąć / odrzucić; stoi na otwarciu rozdziału |
| 5 | P16A „Ile zostaje z obietnicy 0.996” | piaskownica | rf | P16A (linia) czy P16B (skład ryzyka); zastępuje widget „Zależność” |
| 5 | P16B „Z czego składa się ryzyko” | piaskownica | rf | jak wyżej |
| 5 | P17 „β: awarie własne i wspólne” | piaskownica | rf | przyjąć / odrzucić |
| 6 | P18 „Czwarty wentylator” | scena | rd | przyjąć / odrzucić; stoi na otwarciu rozdziału |
| 6 | P19A „Krzywa nasycenia z sufitem” | piaskownica | rg | P19A (oś liniowa) czy P19B (logarytmiczna); zastępuje widget „Trade-off” |
| 6 | P19B „Ryzyko gałąź po gałęzi” | piaskownica | rg | jak wyżej |
| 6 | P20 „k z n: od układu równoległego do szeregu” | galeria | rb | przyjąć / odrzucić |
| 6 | P21 „Trzy kopie, jedna serwerownia” | scena | rc | przyjąć / odrzucić |
| 6 | P22 „Kiedy element rozstrzyga?” | galeria | rc | przyjąć / odrzucić |
| 6 | P23 „Ta sama poprawa, trzy miejsca” | piaskownica | rg | zastępuje panel „Porównanie” |
| 7 | P24A „Pięć kroków na jednym schemacie” | domknięcie (kadry) | rd | P24A, P24B albo oba |
| 7 | P24B „Pięć kroków i ich wyniki” | domknięcie | rd | jak wyżej |

Pliki: `modules/stop_proto_<plik>.R` i `modules/proto_<plik>.js` (grupy rb
i rd nie mają własnego JS); wspólne narzędzia w `modules/stop_proto_common.R`
i `modules/sceny.js`.

## Znane słabości

- Kafle stanów wracają sześć razy (P6, P8, P12, P13, P14, P20). To celowe,
  ale przy oglądaniu pod rząd może nużyć; P13 i P14 mogą być przełącznikami
  w P12, a P8 przełącznikiem w P6.
- P20: napis „2 z 3” w skrzynce głosowania jest mały.
- P22: dwa schematy obok siebie są pomniejszone; nazwy bloków czytelne,
  liczby R pod nazwami na granicy czytelności.
- P4B: krzywa wentylatora i krzywa systemu prawie się pokrywają do 2000 h
  (cecha modelu); napis „karta: 0.95” leży na krzywej czujnika.
- P9A: prostokąt nie jest kwadratem (proporcje panelu), więc pola są
  proporcjonalne, ale boki mają różne skale.
- P9A, P9B, P16A: wartość 0.004 pokazuje się jako „0.00400”, tak jak
  formatuje `risk_fmt_p()` w całym kursie.
- P16B: pasek ryzyka wentylatorów (0.004) ma kilka pikseli; taka jest puenta,
  ale na projektorze może zniknąć.
- P19B: przy r powyżej 0.9 linia niezależnych gałęzi kończy się na dolnej
  krawędzi osi (10⁻⁶); dalsze punkty nie są rysowane.
- P2B: pierwszy widok jest pusty do pierwszego kliknięcia.
- P15: w kadrze z kratkami 14 kratek na 1000 jest małych; liczby stoją
  w odczycie.
- P3: wynik 0.902 i P24: wynik 0.966 nie padają w tekście wykładu.
- Żaden prototyp nie był sprawdzany na wąskim ekranie ani w trybie ciemnym.

## Po decyzjach

- Przyjęte: `proto = FALSE` w `s8p_panel()` (zwykła plakietka rodzaju),
  usunąć notki `s8p_variant()` i zastąpiony widget wraz z jego kodem serwera
  w `block.R`; dopasować tekst wokół (polecenia `risk_try()` i akapity
  „panel pokazuje…” odnoszą się dziś do starych widgetów).
- Odrzucone: usunąć obiekt i jego rycinę albo scenę z plików grupy oraz
  wiersz z testu `tests/testthat/test-s8-browser.R`.
- Pliki `ra`–`rg` nazwać według rozdziałów albo technik, gdy lista się ustali.
