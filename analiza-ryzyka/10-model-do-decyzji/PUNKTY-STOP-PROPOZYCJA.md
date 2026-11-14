# Wykład 10 — propozycja punktów stop

Stan przeglądu: 14 listopada 2026, lokalny HEAD `f36fe81`.
Prototypy wszystkich punktów i wariantów stoją w tekście wykładu ze złotą
plakietką „Prototyp Pn · rodzaj” (stan: 14 listopada 2026); decyzje zbiera
[KOLEJKA-ZATWIERDZANIA.md](KOLEJKA-ZATWIERDZANIA.md). Usterki istniejących
obrazów z tabeli „Stan obecny” są poprawione w `modules/block.R`. Tekst
wykładu bez zmian; istniejące widgety zostały obok do porównania.

Podstawa: `PUNKTY-STOP.md`, `SCENY.md`, `10-model-do-decyzji/modules/block.R`
(5 rozdziałów, 10 widgetów, z czego 4 z wykresem ggplot po stronie serwera R).
Liczby w tabelach pochodzą z tekstu wykładu i z `risk_mission_analysis()`
(`R/risk_math.R`) na parametrach `bananpol$integration`. Jednostki i parametry
są obecne (godziny, MTTF 1500 h, η = 1700 h, limit demonstracyjny 0.002);
odłożona unifikacja z danymi Bananpolu (`TODO.md`, „10 — od modelu do
decyzji”) nie jest tu wykonywana.

## Stan obecny

| Obraz | Rodzaj | Uwagi |
|---|---|---|
| „Co sprawdzono?” (rozdz. 1) | materiał | Cztery pola wyboru i licznik. Bez obrazu; nie niesie puenty. |
| „Wybierz pytanie” (rozdz. 1) | materiał | Lista wyboru i nazwa modelu. Mapa kursu, o której mówi tekst, nie ma rysunku. |
| Karta 1: detektor (rozdz. 2) | piaskownica bez obrazu | Trzy suwaki i dwa odczyty. Puenta „FPR zmienia wiarygodność alarmu, nie przeoczenia” pada w tekście trzy razy i nie ma rysunku. Suwaki P(I) i czułości zasilają dalsze rozdziały (tekst to mówi). |
| Karta 2: partia zaworów (rozdz. 2) | piaskownica bez obrazu | Dwa suwaki i trzy odczyty. |
| Karta 3: niezawodność wentylatora (rozdz. 2) | piaskownica | Rysuje tylko wybrany model, więc przecięcia krzywych przy 1927 h (przykład 10.5) nie widać. Oś pionowa zaczynała się przy 0.13 zamiast przy zerze; brak punktu odczytu. |
| „Elementy w tej samej misji” (rozdz. 3) | piaskownica bez obrazu | Pięć odczytów. Wynik zależy od modelu i czasu z karty 3; model nie był pokazany. |
| „Utrata ochrony termicznej w misji” (rozdz. 3) | pojęcie | Słupki w kolejności alfabetycznej (D, I, S, TOP) zamiast kolejności drzewa, auto-skalowana oś, słupki P(I) i P(TOP) bez wartości. Zależy od suwaków z rozdziałów 2–3 (tekst to mówi), ale panel nie pokazywał, przy jakich ustawieniach liczy. |
| „Ryzyko po działaniu” (rozdz. 4) | galeria | Brak stanu bazowego 0.00162 na rysunku, auto-skalowana oś, wybrana interwencja nie była wyróżniona. |
| „Opcje w trzech scenariuszach” (rozdz. 4) | piaskownica | Oś rozciągała się z suwakiem u, więc słupki prawie się nie ruszały; kolejność opcji inna niż na poprzednim wykresie; scenariusze w tekście pod wykresem w kolejności alfabetycznej; zwycięzców liczy „w budżecie” z suwaka poprzedniej sekcji, czego nie było widać. |
| Notatka (rozdz. 5) | materiał | Tekst generowany z ustawień wszystkich rozdziałów; bez obrazu. Kryterium (limit) nie stoi na żadnym rysunku. |

Bez obrazu: otwarcie wykładu, kontrakt zdarzeń i przykład 10.1, granica modelu
(awaria a niedostępność), polityki odnowy, schemat układu, drzewo z liczbami,
pułapka horyzontu rocznego (0.005 a 0.64), odwrócenie rankingu, ranking
a kryterium, domknięcie kursu.

## Propozycja

| # | Gdzie | Rodzaj · moment | Puenta |
|---|---|---|---|
| 1 | rozdz. 1, „Od czego zacząć?”, przed głosowaniem (A) albo po nim (B) | scena · wprowadzenie | Na biurku zarządu leżą cztery liczby o tej samej komorze: 91%, 29%, 64% i 16 na 10 000. Wszystkie są prawdziwe; dopiero zdanie z warunkiem mówi, która odpowiada na pytanie zarządu. |
| 2 | rozdz. 1, przykład 10.1 | przykłady | Cztery protokoły misji na osi czasu: TOP zachodzi tylko wtedy, gdy jest zapotrzebowanie i zawiodła detekcja albo chłodzenie; misja (d) traci ochronę przy sprawnym chłodzeniu. |
| 3 | rozdz. 1, „Granica modelu” | przypadek brzegowy | Wentylator A padał w roku dwa razy, a ochrony zabrakło tylko wtedy, gdy zapotrzebowanie trafiło w chwilę, w której nie działał cały układ. Awaria to nie niedostępność. |
| 4 | rozdz. 1, „Mapa wyboru modelu” | domknięcie (mapa kursu) | Osiem pytań kursu to osiem modeli; do drzewa Bananpolu trafiają wyniki trzech. |
| 5A | rozdz. 2, przykład 10.3 | pojęcie (kadry) | 10 000 misji: operator czyta kolumnę alarmów (47.5 z 545, czyli 0.087), analityk wiersz zapotrzebowań (2.5 z 50, czyli 0.05). Ta sama tabela, dwa pytania. |
| 5B | rozdz. 2, zamiast panelu „Karta 1” | piaskownica | FPR rozdyma fałszywe alarmy i zmienia wiarygodność alarmu; pasek przeoczeń stoi w miejscu, dopóki nie ruszy się czułość. |
| 6A | rozdz. 2, wyprowadzenie granicy przy zerze zdarzeń | piaskownica | Zero wad w n kontrolach nie wyklucza p poniżej granicy: krzywa (1 − p)ⁿ przecina 0.05 w 0.259 / 0.0295 / 0.003 dla n = 10 / 100 / 1000. |
| 6B | to samo miejsce, druga forma | pojęcie | Granica jako funkcja n na osiach logarytmicznych jest prostą: dziesięć razy niższa granica kosztuje dziesięć razy większą próbę (reguła trzech). |
| 7 | rozdz. 2, zamiast panelu „Karta 3” | galeria | Dwa modele o tej samej średniej na jednej osi: przy 1000 h 0.513 i 0.707, przy 3000 h 0.135 i 0.044; krzywe przecinają się przy 1927 h. |
| 8 | rozdz. 2, przykład 10.6 | galeria · przypadek brzegowy w przełączniku | Trzy polityki odnowy na jednej osi 0–3000 h: 0.044 / 0.354 / 0.595 dla Weibulla. Po przełączeniu na model wykładniczy trzy krzywe zlewają się w jedną (0.135): wymiana nie odmładza. |
| 9 | rozdz. 3, przykład 10.7 | pojęcie (kadry) | Schemat P – C – (A ∥ B) redukuje się krok po kroku: 0.513 i 0.513 → 0.763 → 0.711. |
| 10 | rozdz. 3, zamiast panelu „Redukcja” | piaskownica | Suwak czasu misji przelicza wszystkie cztery bloki naraz: przy 2000 h 0.960 · 0.902 · 0.458 = 0.397. Etykieta czasu jest wspólna. |
| 11A | rozdz. 3, zamiast słupków FTA | galeria | Drzewo z liczbami przy liściach: w stanie bazowym 0.00162; idealny detektor obniża wynik o jedną dziesiątą (0.00145), zmiana modelu wentylatora o 40% (0.00096). |
| 11B | rozdz. 3, przykład 10.8 | pojęcie (kadry) | Lejek 10 000 misji: 50 z zapotrzebowaniem, z nich 2.5 przeoczone i 13.7 z awarią chłodzenia, razem około 16. |
| 12 | rozdz. 3, „Horyzont roczny” | przypadek brzegowy | 1000 lat po trzy misje: w 641 układ zawiódłby, gdyby musiał pracować; zapotrzebowanie jest w 15; ochronę tracimy w 5. Liczba 0.64 zgubiła warunek I. |
| 13A | rozdz. 4, zamiast panelu „Opcje” | galeria | Cztery interwencje na stałej osi P(TOP) z kreską stanu bazowego 0.00162 i kosztem: 0.00081 / 0.00109 / 0.00154 / 0.00156. |
| 13B | to samo miejsce, druga forma | galeria | To samo na drzewie: każda interwencja zmienia jeden liść; ograniczenie źródła ciepła zmienia czynnik, który mnoży cały wzór. |
| 14 | rozdz. 4, zamiast panelu „Niepewność” | piaskownica | Wynik to przedział, nie liczba: rozrzut między scenariuszami jest dla każdej opcji większy niż różnice między opcjami. |
| 15 | rozdz. 4, przykład 10.11 b | przypadek brzegowy | Ranking się odwraca: dla Weibulla w scenariuszu ostrożnym linia ograniczenia źródła ciepła przecina linię wentylatora przy u ≈ 0.47 (0.00172 wobec 0.00168 przy u = 0.5). |
| 16 | rozdz. 5, przed panelem notatki | piaskownica | Ranking to nie kryterium: przy limicie 0.002 czujnik w najgorszym scenariuszu przekracza kreskę (0.00230), ograniczenie źródła ciepła nie (0.00144); przy limicie 0.001 nie przechodzi nikt. |
| 17 | rozdz. 5, przykład 10.12 | domknięcie | Notatka na jednej stronie: pięć punktów, każda liczba z odsyłaczem do wzoru, z którego pochodzi. |
| 18A | rozdz. 5, „Podsumowanie” | domknięcie kursu (kadry) | Łańcuch dane → parametr → model → decyzja: ogniwo po ogniwie dochodzi liczba i wykład, z którego pochodzi (0.005, 0.05, 0.513, 0.711, 0.00162, 0.00081, limit 0.002). |
| 18B | rozdz. 5, „Podsumowanie”, druga forma | domknięcie kursu | Cztery liczby z biurka wracają ze zdaniami: przy każdej warunek, pytanie i miejsce w notatce. Liczba jest tak wiarygodna jak zdanie, które ją opisuje. |

Razem 18 punktów i 23 obrazy. Pary 5A/5B, 6A/6B, 11A/11B, 13A/13B i 18A/18B
to dwie formy tego samego przystanku (5A i 11B mogą też zostać razem z drugą
formą: jedna stoi przy przykładzie, druga zastępuje widget).

## Uzasadnienia

- Punkt 1 otwiera wykład historią zamiast definicją teczki. Cztery liczby
  pochodzą z tekstu (1 − 0.087, P(S | I) ≈ 0.289, 1 − R_sys³ ≈ 0.641, 16 na
  10 000) i każda wraca we własnym rozdziale. Punkt 18B zamyka tę samą scenę,
  tak jak raport w wykładzie 06.
- Punkty 5B, 12 i 16 dają obraz puentom, które tekst powtarza bez rysunku:
  „FPR nie zmienia przeoczeń” (tekst karty 1, sprawdzenie, quiz), „0.005,
  a nie 0.64” (przykład 10.9, sprawdzenie, quiz, ćwiczenie 5), „ranking
  i kryterium to dwa sprawdzenia” (tekst notatki, sprawdzenie, quiz,
  ćwiczenie 3).
- Punkty 14 i 15 są obrazem niepewności: wynik jako przedział na stałej osi
  i ranking, który się odwraca. Dziś oś rozciąga się razem z suwakiem u.
- Punkty 7, 11A, 13A i 14 zastępują cztery wykresy ggplot wersjami
  w przeglądarce, ze stałymi osiami i odczytami liczb z tekstu.
- Punkty 4 i 18A spinają kurs: mapa pytań i modeli na początku, łańcuch
  z liczbami na końcu.
- Wspólny język wizualny: stała oś P(TOP) na misję (0–0.004) z czarną
  przerywaną kreską limitu 0.002 i szarą kreską stanu bazowego 0.00162
  (punkty 13A, 14, 15, 16); drzewo I – (D ∪ S) z liczbami przy liściach
  (11A, 13B); 10 000 misji jako częstości naturalne (5A, 11B); kartki z biurka
  (1, 18B). Akcent oznacza utratę ochrony albo przekroczenie limitu, szałwia
  „spełnia”.
- Parametry prototypów przychodzą z R w `data-config` (`i10p_cfg` w
  `modules/stop_proto_common.R`, liczone z `bananpol$integration`,
  `bananpol$interventions` i `risk_mission_analysis()`); JS nie ma wpisanych
  liczb modelu. Po unifikacji z danymi Bananpolu wystarczy zmienić konfigurację.

## Kolejność

| Priorytet | Punkty | Dlaczego |
|---|---|---|
| A | 1, 12, 14, 15, 16 | najsilniejsze puenty bez obrazu; niepewność i kryterium |
| A | 7, 11A, 13A | istniejące wykresy z usterką |
| B | 2, 5A/5B, 8, 9, 10, 11B, 18A/18B | porządkują główny wątek i domykają kurs |
| C | 3, 4, 6A/6B, 13B, 17 | pojęcia pomocnicze, karta spoza drzewa, mapa |

## Otwarte decyzje

- Scena 1 przed głosowaniem czy po nim (stoi w obu miejscach).
- Punkt 3 używa wymyślonej osi roku (dwie awarie A, jedna B); w tekście jest
  tylko zdanie serwisanta. Liczby na osi są ilustracją, nie danymi.
- Punkt 12: 1000 lat jako kratki to częstości oczekiwane (641, 15, 5), nie
  symulacja.
- Karta 2 (punkty 6A/6B) nie zasila drzewa; można ją zostawić bez obrazu.
- Po unifikacji misji z danymi Bananpolu (`TODO.md`) zmienią się liczby
  w podpisach kadrów pisanych w R (punkty 1, 2, 9, 11B, 12, 17, 18): są
  liczone z konfiguracji, ale zdania wokół nich trzeba przeczytać ponownie.
