# Wykład 05 — propozycja punktów stop

Stan przeglądu: 3 listopada 2026, lokalny HEAD `c593f02`.
To propozycja dydaktyczna do omówienia; prototypy stoją w kodzie wykładu
(sekcja „Prototypy w tekście wykładu”), tekst wykładu bez zmian.

Podstawa: `PUNKTY-STOP.md`, `SCENY.md`, `05-do-zdarzenia/modules/block.R`
(6 rozdziałów, 4 obrazy). Liczby sprawdzone w R (szczegóły na końcu).

## Stan obecny

Wszystkie 4 obrazy stoją w środku wykładu; otwarcie i domknięcie nie mają
obrazu.

| Obraz | Rodzaj | Uwagi |
|---|---|---|
| Geo — „Ile kontroli do pierwszej wady?” (rozdz. 2) | prawie symulacja | Każde kliknięcie losuje od razu 400 serii, nic nie narasta, więc kształt nie „wyłania się” z powtórzeń. Oś X skacze między losowaniami (`xlim = min(80, max(...))`). |
| NB — „Łączna liczba kontroli” (rozdz. 3) | piaskownica | Pasuje. Oś automatycznie zmienia się z `p` i `r`, więc znika puenta „przesuwa się szczyt”. Ustalić oś. |
| Kalkulator limitu (rozdz. 4) | materiał / piaskownica | Dobry, ale stoi po tym, jak intro rozdziału zdradziło wynik 59%. |
| Stałe kontra zmienne p (rozdz. 5) | piaskownica · przypadek brzegowy | Dobry; brakuje odczytu, co to znaczy dla planu (punkt 11). |

Bez obrazu zostają trzy miejsca z najsilniejszą puentą: reguła zatrzymania,
brak pamięci i tożsamość (5.7).

## Propozycja

| # | Gdzie | Rodzaj · moment | Puenta |
|---|---|---|---|
| 1 | rozdz. 1, przed def. 5.1 | scena · wprowadzenie | Dwóch audytorów ogląda te same palety; jeden kończy po 50 kontrolach, drugi po trzeciej wadzie. Ten sam protokół, różne pytania. |
| 2 | rozdz. 2, zamiast obecnego geo | symulacja · wprowadzenie | Najczęstszy wynik to pierwsza kontrola, a średnio czekamy 10; ogon wyłania się dopiero przy wielu seriach. |
| 3 | rozdz. 2, „Brak pamięci” | scena · wprowadzenie | Kierownik: „20 kontroli bez wady, więc zaraz się pojawi”. Odpowiedź przez obraz, nie wzór. |
| 4 | po 3 albo przełącznik w 3 | symulacja · domknięcie pojęcia | Weź tylko serie, które przetrwały 20 kontroli, i licz od 20: histogram reszty oczekiwania wygląda jak na starcie. |
| 5 | rozdz. 3, „Suma czasów” | pojęcie | Taśma kontroli z trzema znacznikami to trzy sklejone oczekiwania geometryczne: G₁ + G₂ + G₃. |
| 6 | rozdz. 3, przykład 5.3(a) | pojęcie (krokowe) | C(4,2) = 6 układów; audyt kończy się na r-tej wadzie, więc rozmieszczamy tylko r−1 wad wśród x−1 kontroli. |
| 7 | rozdz. 3, obecny NB | piaskownica | r = 1 daje geometryczny; większe r przesuwa szczyt i symetryzuje rozkład (warunek: stała oś). |
| 8 | rozdz. 4, otwarcie | scena · wprowadzenie | Dwadzieścia oddziałów planuje po 30 kontroli, czyli „średnią”. Odliczamy, komu zabrakło: około 8 z 20. |
| 9 | rozdz. 4, przy (5.7) | pojęcie | Ta sama taśma przecięta na n: „skończyliśmy do n” = „w n kontrolach ≥ r wady”. Most do wykładu 04. |
| 10 | rozdz. 4, „Ile kontroli zaplanować?” | galeria · domknięcie | Koszt pewności na jednej osi: średnia 30, mediana 27, 90% → 52, 95% → 61, 99% → 81. |
| 11 | rozdz. 5, obecny widget | piaskownica · przypadek brzegowy | Plan na 61 kontroli przy dwóch dostawcach (p = 0.05 i 0.15) wystarcza tylko w ok. 80% audytów; prawdziwe 95% to ok. 105. |
| 12 | rozdz. 6 | domknięcie | Powrót do taśmy z punktu 1: trzy reguły zatrzymania i trzy zmienne, każda z wynikiem z wykładu. |

Punkty 4 i 9 mogą być przełącznikami w obrazach 3 i 8; wtedy zostaje
10 przystanków.

## Uzasadnienia

- Punkt 8 to serce wykładu. Hook rozdziału 4 („Średnio wystarczy, a i tak
  zabraknie”) jest dziś od razu rozwiązany w intro liczbą 59%. Sala powinna
  najpierw zobaczyć oddziały przekraczające limit, a dopiero potem kwantyl
  i def. 5.4. Liczbę 59% usunąć z intro. Brief sceny według `SCENY.md` do
  akceptacji przed kodem.
- Punkty 3–4 wypełniają największą lukę: brak pamięci to najczęstszy błąd
  intuicji w tym temacie, a dziś stoi przy nim tylko wzór. Reszta oczekiwania
  po 20 kontrolach ma średnią ok. 10, tak jak na starcie.
- Obrazy 1, 5, 6, 9 i 12 to jedna taśma kontroli w kolejnych rolach: reguła,
  suma, układy, przecięcie, domknięcie. Wspólny język wizualny spina wykład;
  mapa z otwarcia wraca jako domknięcie, więc jest przystankiem, nie tylko
  nawigacją.
- Punkt 11 ma najmocniejszą liczbę wykładu. Widget pokazuje dziś tylko różnicę
  średnich; odczyt „P(ukończenia w planie 61)” przenosi przypadek brzegowy
  prosto na decyzję z rozdziału 4.

## Prototypy w tekście wykładu (2026-11-03)

Wszystkie punkty mają prototyp w `block.R` w przewidzianym miejscu, ze złotą
plakietką „Prototyp · <rodzaj>” i komentarzem `# PROTOTYP Pn`. Istniejące
widgety zostały obok do porównania. Pliki: `modules/stop_proto_*.R`
(`stop_proto_common.R` zawiera panel, CSS i taśmę kontroli).

| Pn | Obiekt | Plik | Miejsce |
|---|---|---|---|
| P1 | `d5p_rule_ui` | tasma | przed def. 5.1 |
| P2 | `d5p_geo_ui` | geo | pod istniejącym widgetem geo |
| P3 | (wchłonięty przez P4: cytat kierownika w podpisie) | — | — |
| P4 | `d5p_memory_ui` | geo | po wzorze 5.4 |
| P5 | `d5p_sum_ui` | suma | przed wzorem 5.6 (E i Var) |
| P6 | `d5p_arr_ui` | suma | po przykładzie 5.3 |
| P7 | poprawka osi `d5_nb` (oś zależy tylko od p) | block.R | — |
| P8 | `d5p_branches_ui` | oddzialy | początek sekcji „Średnia a kwantyl” |
| P9 | `d5p_cut_ui` | oddzialy | po wzorze 5.7 |
| P10 | `d5p_cost_ui` | plan | koniec sekcji „Ile kontroli zaplanować?” |
| P11 | `d5p_suppliers_ui` | plan | pod istniejącym widgetem mieszanki |
| P12 | `d5p_close_ui` | tasma | sekcja „Podsumowanie” |

Do decyzji przy przeglądzie:

- Intro rozdziału 4 nadal zdradza wynik 59% przed sceną P8.
- P4 stoi między wzorem 5.4 a dowodem; być może lepiej po dowodzie.
- P11 przy d ≥ 0.07: prawdziwy 95. percentyl wychodzi poza oś 150 (etykieta ze
  strzałką).
- Taśmy w P8 są drobne (120 kontroli na szerokość); na rzutniku sprawdzić
  czytelność.

## Poprawki istniejących widgetów

1. Geo: zbierać serie (+1 / +10 / +100) z licznikiem, ustalona oś 1–80.
2. NB: stała oś X dla całego zakresu suwaków.
3. Kalkulator limitu: materiał po punkcie 10 albo piaskownica po def. 5.4,
   bez wcześniejszego zdradzenia wyniku.
4. Rozdz. 5, niezależność: brak obrazu. Opcjonalnie drugi przypadek brzegowy,
   galeria trzech protokołów: niezależne kontrole, wady w skupiskach,
   kontroler, który się uczy.

## Weryfikacja liczb

Sprawdzone w R; zgodne z tekstem wykładu:

- P(X ≤ 30) dla r = 3, p = 0.1: 0.589 (`pnbinom(27, 3, 0.1)`).
- Mediana i kwantyle 90/95/99% łącznej liczby kontroli: 27, 52, 61, 81.
- Geometryczny p = 0.1: 90. percentyl 22, P(X > 22) ≈ 0.098, P(X > 20) ≈ 0.122.
- Średnia reszty oczekiwania po 20 kontrolach bez wady: ≈ 10.07
  (100 000 serii).

Nowe liczby (mieszanka dostawców 50/50, p = 0.05 i 0.15, 1 mln serii):

- średnia 40 (dokładnie 3 · E(1/p));
- kwantyl 95% ≈ 105;
- P(ukończenia do 61) ≈ 0.795.

Interakcje prototypów P1–P12 sprawdzone w przeglądarce (1280×900), bez błędów
konsoli i serwera.

## Stan (2026-11-04)

Wszystkie punkty stop zostały przejrzane z wariantami B (Codex) i zaakceptowane; plakietka „Prototyp” została zdjęta. Warianty B zostały usunięte.
