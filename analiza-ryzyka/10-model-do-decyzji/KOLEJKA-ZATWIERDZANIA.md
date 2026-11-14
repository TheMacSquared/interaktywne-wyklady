# Wykład 10 — kolejka zatwierdzania prototypów

Stan: 14 listopada 2026. Opis i uzasadnienie w
[PUNKTY-STOP-PROPOZYCJA.md](PUNKTY-STOP-PROPOZYCJA.md).

Oglądanie (z katalogu repo, osobny port, bez restartu huba):

```r
shiny::runApp("analiza-ryzyka/10-model-do-decyzji", port = 8840)
```

Każdy prototyp ma złotą plakietkę „Prototyp Pn · rodzaj” i stoi w miejscu,
w którym miałby zostać. Tam, gdzie są warianty, nad panelem jest notka
„Wariant: …”. Istniejące widgety zostały obok do porównania.

## Inwentarz

| Rozdz. | Prototyp | Rodzaj | Plik | Decyzja do podjęcia |
|---|---|---|---|---|
| 1 | P1 „Cztery liczby na biurku” | scena | ra | miejsce: przed głosowaniem (B) czy po nim (A); stoi w obu |
| 1 | P2 „Cztery protokoły misji” | przykłady | rb | przyjąć / odrzucić (obraz przykładu 10.1) |
| 1 | P3 „Awaria to nie niedostępność” | przypadek brzegowy | rb | przyjąć / odrzucić; oś roku jest wymyślona |
| 1 | P4 „Osiem pytań, osiem modeli” | domknięcie (mapa kursu) | rb | zastępuje listę „Wybierz pytanie” albo stoi obok |
| 2 | P5A „10 000 misji, dwa pytania” | pojęcie (kadry) | rc | P5A, P5B albo oba (różne miejsca) |
| 2 | P5B „Co zmienia FPR, a co czułość” | piaskownica | rc | zastępuje panel „Karta 1”? (jej suwaki zasilają dziś dalsze rozdziały) |
| 2 | P6A „Zero wad w n kontrolach” | piaskownica | rg | P6A, P6B albo żaden (karta 2 nie zasila drzewa) |
| 2 | P6B „Dziesięć razy niżej, dziesięć razy więcej” | pojęcie | rg | jak wyżej |
| 2 | P7 „Ta sama średnia, inne ryzyko” | galeria | ra | zastępuje widget ggplot „Karta 3” |
| 2 | P8 „Trzy polityki odnowy” | galeria | rg | przyjąć / odrzucić |
| 3 | P9 „Redukcja schematu” | pojęcie (kadry) | re | przyjąć / odrzucić |
| 3 | P10 „Wspólny czas misji” | piaskownica | re | zastępuje panel „Redukcja” |
| 3 | P11B „Lejek 10 000 misji” | pojęcie (kadry) | re | P11A, P11B albo oba |
| 3 | P11A „Drzewo z liczbami” | galeria | rd | zastępuje słupki „Integracja” |
| 3 | P12 „Tysiąc lat po trzy misje” | przypadek brzegowy | re | przyjąć / odrzucić |
| 4 | P13A „Cztery interwencje na jednej osi” | galeria | rd | P13A czy P13B; zastępuje widget „Opcje” |
| 4 | P13B „Który liść się zmienia” | galeria | rd | jak wyżej |
| 4 | P15 „Ranking się odwraca” | przypadek brzegowy | rd | przyjąć / odrzucić |
| 4 | P14 „Wynik to przedział” | piaskownica | rd | zastępuje widget „Niepewność” |
| 5 | P16 „Ranking to nie kryterium” | piaskownica | rd | przyjąć / odrzucić; stoi przed panelem notatki |
| 5 | P17 „Notatka na jednej stronie” | domknięcie | rf | przyjąć / odrzucić |
| 5 | P18A „Od danych do decyzji” | domknięcie kursu (kadry) | rf | P18A, P18B albo oba |
| 5 | P18B „Cztery liczby wracają” | domknięcie kursu | rf | jak wyżej; ma sens tylko razem z P1 |

Pliki: `modules/stop_proto_<plik>.R` i `modules/proto_<plik>.js` (grupa `rf`
nie ma pliku JS); wspólne narzędzia w `modules/stop_proto_common.R`
i `modules/sceny.js`.

## Parametry i odłożona unifikacja misji

Prototypy używają obecnych liczb i jednostek wykładu (godziny, MTTF 1500 h,
η = 1700 h, misja 1000 h, limit demonstracyjny 0.002). Zadanie z `TODO.md`
(„10 — od modelu do decyzji”: przejście na miesiące i dane Bananpolu) nie
zostało wykonane.

Wszystkie parametry stoją w jednym miejscu: `i10p_cfg`
w `modules/stop_proto_common.R` (z `bananpol$integration`,
`bananpol$lifetime`, `bananpol$interventions`). Kadry w R liczą przez
`i10p_mission()` (opakowanie `risk_mission_analysis()`), sceny i ryciny
w przeglądarce przez `lcI10.util.mission(cfg, …)` z `data-config`. W plikach
JS nie ma liczb modelu. Po unifikacji wystarczy zmienić `i10p_cfg` (w tym
`unit`, `time`, `base_time`, `time_max`, `limit`, `top_axis`) i:

- `stopifnot()` w `stop_proto_common.R` pilnuje, że `i10p_cfg` opisuje ten
  sam model wentylatora co `risk_mission_analysis()`; po sparametryzowaniu
  tej funkcji trzeba przekazać jej parametry z `i10p_cfg`;
- przeczytać zdania kadrów pisane w R (P1, P2, P9, P11B, P12, P17, P18A,
  P18B): liczby wstawia `sprintf`, ale słowa („trzy misje”, „1000 h”
  w podpisach przykładu 10.1: zatrzymania po 0.4 i 0.2 misji) są pisane ręcznie;
- zakresy suwaków P6A/P6B (n), P16 (limit 0.0005–0.003) i P3 (dni roku) są
  ustawione w R obok obiektów; oś P13A to `top_axis / 2`, oś P15 `0.75 · top_axis`;
- test `tests/js/i10_sceny.test.js` ma liczby kontrolne obecnego modelu.

## Poprawki istniejących widgetów (już w `block.R`)

- Karta 3: oba modele na stałych osiach (wybrany ciągłą, drugi przerywaną),
  punkt odczytu, drugi odczyt R(t).
- „Redukcja”: odczyt modelu wentylatora z karty 3.
- Słupki FTA: kolejność I, D, S, TOP zamiast alfabetycznej, stała oś 0–1,
  wartości nad słupkami, odczyt „Ustawienia z kart”.
- „Opcje”: kreska stanu bez działania, obrys wybranej interwencji, stałe
  przypisanie kolorów budżetu.
- „Niepewność”: stała oś (suwak u zmienia słupki, nie skalę), kolejność opcji
  jak w tabeli interwencji, opcje poza budżetem wyblakłe, scenariusze
  w tekście w kolejności optymistyczny–bazowy–ostrożny, budżet i ustawienia
  kart w tekście.
- Notatka: wiersz z ustawieniami z poprzednich rozdziałów.

Do decyzji: w widgecie „Opcje” kolor „w budżecie” nie zmienia już znaczenia,
więc przy budżecie 4 kolor zmieniają dwie opcje, a nie wszystkie. Zdanie
w tekście („przy budżecie 4 wszystkie opcje zmieniają kolor”) opisuje stare
zachowanie i wymaga poprawki przy redakcji (tekstu nie ruszano).

## Znane słabości

- Format liczb: prototypy używają formatu odczytów wykładu (`risk_fmt_p`,
  trzy cyfry znaczące: 0.000397, 0.00487, 0.00200), a proza wykładu zaokrągla
  inaczej (0.00040, 0.0049, 0.002). Do ujednolicenia w jedną stronę.
- P3: okna awarii są ilustracją (podpis to mówi); układ jest niedostępny
  tylko 7 dni, więc czerwony pasek jest wąski.
- P5B: wiersz częstości pod paskami łamie się na dwie linie.
- P6B: podpis 0.00299 przy prawym krańcu nachodzi na linię (ma tło).
- P8: dla modelu wykładniczego trzy krzywe pokrywają się i widać jedną
  (to jest puenta, ale legenda pokazuje trzy).
- P10: przy 3000 h wskaźniki wentylatorów są prawie puste.
- P11A, P13B: nazwa węzła „D ∪ S: brak detekcji lub chłodzenia” jest ciasna
  w ramce.
- P12: 5 lat z utratą ochrony to wartość oczekiwana, położenia kratek są
  losowe ze stałym ziarnem; kratki „układ by zawiódł” i „zapotrzebowanie” są
  losowane niezależnie (to przybliżenie, nie model zależności).
- P16: przy dłuższych odczytach pasek odczytów łamie się na dwa wiersze.
- P14, P15, P16: etykieta limitu ma format 0.00200.
- P17, P18B: nie sprawdzone na wąskim ekranie.
- Wszystkie: nie sprawdzone w trybie ciemnym ani w powiększeniu wykresu (lupa).

## Po decyzjach

- Przyjęte: `proto = FALSE` w `i10p_panel()` (zwykła plakietka rodzaju),
  usunąć notki `i10p_variant()` i zastąpiony widget wraz z jego kodem
  serwera w `block.R`; dopasować tekst wokół (polecenia `risk_try()` i opisy
  „panel pokazuje…” odnoszą się dziś do starych widgetów).
- Uwaga przy zastępowaniu: suwaki kart 1 i 3 oraz panelu „Redukcja”
  (`i10_init`, `i10_sens`, `i10_life_model`, `i10_time`, `i10_power`,
  `i10_controller`) zasilają słupki FTA, opcje, scenariusze i notatkę.
  Prototypy są niezależne (każdy liczy z `i10p_cfg`), więc po zastąpieniu
  widgetów trzeba zdecydować, czy notatka (rozdz. 5) zostaje na serwerze R
  z własnymi kontrolkami, czy też przechodzi do przeglądarki.
- Odrzucone: usunąć obiekt i jego rycinę albo scenę z plików grupy oraz
  wiersz z testu `tests/testthat/test-i10-browser.R`.
- Pliki `ra`–`rg` nazwać według rozdziałów, gdy lista się ustali.
