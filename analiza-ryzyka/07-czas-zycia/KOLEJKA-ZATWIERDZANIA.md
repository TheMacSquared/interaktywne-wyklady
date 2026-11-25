# Wykład 07 — kolejka zatwierdzania prototypów

Stan: 24 listopada 2026 (część A rozstrzygnięta, część B czeka). Opis i uzasadnienie w
[PUNKTY-STOP-PROPOZYCJA.md](PUNKTY-STOP-PROPOZYCJA.md).

Oglądanie (z katalogu repo, osobny port, bez restartu huba):

```r
system("Rscript scripts/build_static.R analiza-ryzyka/07-czas-zycia")   # → dist/analiza-ryzyka/07-czas-zycia.html
```

Każdy nierozstrzygnięty prototyp ma złotą plakietkę „Prototyp Pn · rodzaj” i stoi w miejscu,
w którym miałby zostać. Tam, gdzie są warianty, nad panelem jest notka
„Wariant: …”. Istniejące widgety zostały obok do porównania (z poprawionymi
osiami i odczytami).

## Decyzje: część A (rozdziały 1–4), 24 listopada 2026

Przyjęte, bez złotej plakietki (`c7p_accepted` w `modules/stop_proto_common.R`):

| Rozdz. | Punkt | Decyzja |
|---|---|---|
| 1 | P1 „Dwie oferty, jedna średnia” | przyjęty; stoi raz, po głosowaniu (wariant A) |
| 1 | P2B „Dwie oferty, jedna średnia: gęstości osobno” | przyjęty; P2A (jedna oś) odrzucony i usunięty |
| 2 | P3 „Badanie kończy się po 1200 h” | przyjęty; od 25.11 zaczyna od rejestru awarii (cztery wpisy, 612.5 h) |
| 2 | P5 „Łączny czas pracy” | przyjęty; od 25.11 pięć kadrów, z dolną granicą 7250/8 ≈ 906 h |
| 2 | P6 „To samo badanie powtórzone wiele razy” | przyjęty 24.11, usunięty 25.11: symulacja nie pasowała do narracji o ośmiu wentylatorach |
| 2 | P4 „Trzy rachunki a koniec badania” | przyjęty; od 25.11 pokazuje rejestr, dolną granicę i szacunek (7.2), stoi po przykładzie 7.2 |
| 3 | P7 „Te same awarie, dwa mianowniki” | przyjęty; okno zostaje 300 h |
| 3 | P8 „Cztery funkcje, jedna chwila” | przyjęty; zastąpił „Wspólny suwak czasu”, polecenie i odczyt przepisane |
| 3 | P9, P10 | przyjęte |
| 4 | P11 „Ocalali wyglądają jak nowi” | przyjęty; czwarty kadr (oferta B) zostaje |
| 4 | P12 „Stały hazard” | przyjęty; zastąpił widget „Stały hazard” |
| 4 | P13 „Magazyn” | przyjęty |
| 4 | P14 „Czas do k-tej awarii” | przyjęty; zastąpił widget gamma |

Zostało po części A:

- `modules/wykresy.js` nadal rejestruje ryciny `c7m_timeline`, `c7m_functions`,
  `c7m_exp` i `c7m_gamma`, a `modules/wykresy.R` liczy `c7_timeline_config`.
  Strona ich nie używa; funkcje pomocnicze sprawdza `test-c7-wykresy.R`.
- P2B: przy wąskim ekranie napisy „wymagane 1000 h” i „MTTF” nachodzą na siebie.
- Pliki `ra`–`ri` czekają na nazwy według rozdziałów do decyzji o części B.

## Inwentarz: część B (rozdziały 5–8), do decyzji

| Rozdz. | Prototyp | Rodzaj | Plik | Decyzja do podjęcia |
|---|---|---|---|---|
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

- P7: ułamki liczone na 200 liniach parku (0.184 zamiast 0.181 z modelu;
  odczyt „z modelu” stoi obok).
- P15: awarie na działających w oknach po 500 h (0.42 → 0.22, 0.28 → 0.29,
  0.04 → 0.79) dla modeli z przykładu 7.8.
- P18: hazardy względne z przykładu 7.9 podzielone przez 1000 na godzinę;
  na obrazie nie ma liczb hazardu.
- P19: hazard w dnie wanny 0.184 (także w poprawionym widgecie ggplot).
- P22 i P23A: wczesne defekty β = 0.7, η = 1185 h — używany 0.747, nowy 0.579.
- P21A: terminy dla R* = 0.95 i 0.80 (383 i 800 h; wykładniczy 77 i 335 h).

## Znane słabości

- P1, P9, P11, P17B: linie parku to kwantyle modelu, nie losowanie; kadr 2
  sceny P1 („w kolejności montażu”) pokazuje te same linie przetasowane.
  Linie oferty A dłuższe niż 4500 h są ucięte na krawędzi osi.
- P13: pierwszy widok to jeden okres, więc słupek
  wychodzi ponad stałą oś, a licznik startuje od 100%.
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
