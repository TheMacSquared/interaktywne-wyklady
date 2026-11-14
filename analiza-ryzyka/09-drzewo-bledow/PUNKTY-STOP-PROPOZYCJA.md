# Wykład 09 — propozycja punktów stop

Stan przeglądu: 14 listopada 2026, lokalny HEAD `f36fe81`.
Prototypy wszystkich punktów i wariantów stoją w tekście wykładu ze złotą
plakietką „Prototyp Pn · rodzaj” (stan: 14 listopada 2026); decyzje zbiera
[KOLEJKA-ZATWIERDZANIA.md](KOLEJKA-ZATWIERDZANIA.md). Usterki istniejących
obrazów z tabeli „Stan obecny” są poprawione w `modules/block.R` (poza jedną,
opisaną niżej). Tekst wykładu bez zmian; istniejące widgety zostały obok do
porównania.

Podstawa: `PUNKTY-STOP.md`, `SCENY.md`, `09-drzewo-bledow/modules/block.R`
(7 rozdziałów, 7 widgetów, z czego tylko 2 mają obraz; oba ggplot po stronie
serwera R). Liczby w tabelach pochodzą z tekstu wykładu, z `R/risk_math.R`
(`risk_gate_or`, `risk_fta_top`) i `R/bananpol.R` (`bananpol$fta`).

## Stan obecny

| Obraz | Rodzaj | Uwagi |
|---|---|---|
| Głosowanie „Która definicja jest audytowalna?” (rozdz. 1) | materiał | Bez obrazu; w porządku jako pytanie do sali. |
| „Utrata kontroli nad zapłonem” — konstruktor (rozdz. 2) | piaskownica bez obrazu | Lista wyboru, pola wyboru i jedno zdanie statusu. Bramki nie widać, wejścia się nie „zapalają”; sala nie widzi różnicy między OR i AND. |
| „Aktywuj zdarzenia bazowe” (rozdz. 2) | piaskownica bez obrazu | Trzy pola wyboru i werdykt. Tekst mówi o ośmiu kombinacjach i trzech aktywujących TOP, ale tabeli stanów ani drzewa nie ma na ekranie. |
| „Parametry małego drzewa” (rozdz. 3) | piaskownica | Jedyny rysunek drzewa w wykładzie: punkty ggplot na siatce z osiami, bez liczb. Suwaki zmieniają tylko dwa odczyty, rysunek stoi. Kształty (koło, trójkąt, kwadrat) nie są symbolami FTA. |
| „Wybierz przekrój” (rozdz. 4) | galeria bez obrazu | Przełącznik i jedno zdanie. Przekrój to ścieżka w drzewie, a drzewa nie ma. |
| „Dwa wystąpienia, jedno źródło” (rozdz. 4) | przypadek brzegowy bez obrazu | Suwak q i trzy odczyty. Puenty „im mniejsze q, tym gorzej wypada AND” nie widać bez przesuwania i liczenia w pamięci. |
| „Trzy minimalne przekroje” (rozdz. 4) | piaskownica bez obrazu | Ukryta zależność: d₀ i s₀ pochodzą z suwaków rozdziału 3 (`input$f9_detect`, `input$f9_suppress`, `input$f9_init`). Tekst wykładu opisuje tę zależność wprost („Panel korzysta z suwaków…”, polecenie „wróć do rozdziału o rachunku”), więc nie została rozplątana — to decyzja dla autora. |
| „Ta sama redukcja względna każdego liścia” (rozdz. 5) | piaskownica | Oś Y dopasowuje się automatycznie, więc suwak r zmienia liczby na osi, a nie słupki; puenta „r skaluje słupki, kolejność zostaje” znika. Oś w notacji 1e-04. Wartości z tekstu (0.000315, 0.000190, 0.000115) nie stoją przy słupkach. |

Bez obrazu: otwarcie (kierunek „od skutku w dół”), cztery składniki definicji,
trzy poziomy drzewa, dualność z wykładem 08, rachunek krok po kroku i skala
10 000 magazyno-lat, przybliżenie rzadkich zdarzeń, przykład 9.6 (dwa kanały,
jedno zasilanie), dwa światy Birnbauma, zapomniany zawór, przedział
niepewności, domknięcie.

## Wspólny język wizualny

Jedno drzewo w całym wykładzie, rysowane w SVG w przeglądarce
(`modules/sceny.js`, `lcF9.util.tree`): zdarzenie szczytowe i pośrednie jako
prostokąt, pod nim symbol bramki (AND z płaską podstawą, OR ze szpicem),
zdarzenie bazowe jako koło z literą (I, D, S). Topologia jak na dotychczasowym
rysunku ggplot: inicjacja po lewej, bramka OR barier po prawej. Zdarzenie,
które zachodzi, „zapala się” kolorem akcentu razem z gałęzią do rodzica; liść,
który na pewno nie zachodzi, jest w szałwii; liczby stoją w mono przy węźle
(prawdopodobieństwo liścia pod kołem, wynik bramki obok symbolu). To samo
drzewo niesie logikę (rozdz. 2), rachunek (3), przekroje (4), istotność (5),
brakującą gałąź (6) i domknięcie (7).

## Propozycja

| # | Gdzie | Rodzaj · moment | Puenta |
|---|---|---|---|
| 1 | rozdz. 1, „Od sukcesu do awarii”, przed głosowaniem | scena · wprowadzenie | Protokół po pożarze czyta się od końca: spalona hala → był ogień i nikt go nie zatrzymał → czujka milczała; w drugim protokole zamknięty zawór. Zapłon jest w obu, bariera za każdym razem zawiodła inna i jedna wystarczyła. |
| 2 | rozdz. 1, przykład 9.1 | pojęcie (kadry) | „Awaria chłodni” staje się zdarzeniem szczytowym dopiero z czterema składnikami: stan, granice, kryterium, horyzont. Każdy kadr dokłada jeden. |
| 3 | rozdz. 1, „Zdarzenia szczytowe, pośrednie i bazowe” | pojęcie (kadry) | Drzewo ma trzy piętra: skutek, mechanizm, przyczyny z liczbami. Gdzie drzewo się kończy i co zostaje poza nim, decyduje analityk. |
| 4 | rozdz. 2, otwarcie, przed definicją 9.3 | scena · wprowadzenie | Te same dwie usterki: w łańcuchu wystarczy jedna (3 układy z 4), przy dwóch samodzielnych urządzeniach potrzeba obu (1 z 4). Wariant A: budzik przed egzaminem; wariant B: instalacja gaśnicza Bananpolu. |
| 5 | rozdz. 2, obecny konstruktor | piaskownica | Bramka OR zapala się od jednego wejścia, AND dopiero od wszystkich; przy trzech wejściach to 7 z 8 kombinacji wobec 1 z 8. |
| 6 | rozdz. 2, „Nasze drzewo”, obecny panel pól wyboru | piaskownica (A) albo galeria (B) | Z ośmiu kombinacji liści TOP zapalają trzy i każda zawiera inicjację. A: klikane drzewo z tabelą stanów, która się wypełnia; B: osiem drzewek obok siebie. |
| 7 | rozdz. 2, „Dualność”, przykład 9.2 | pojęcie | Szereg to OR awarii, układ równoległy to AND awarii: ten sam system w dwóch językach daje 0.126 i 0.004. |
| 8 | rozdz. 3, przykład 9.3 | pojęcie (kadry) | Liczby wchodzą od dołu: każda bramka zamienia się w jedną liczbę, 0.126, potem 0.00063, czyli około 6 z 10 000 magazyno-lat (z 50 z inicjacją). |
| 9 | rozdz. 3, obecny panel „Parametry małego drzewa” | piaskownica | Podwojenie inicjacji podwaja P(TOP); podwojenie braku detekcji podnosi je o 37%. Liczby stoją na drzewie, obok stała siatka 10 000 magazyno-lat. |
| 10 | rozdz. 3, „Przybliżenie rzadkich zdarzeń”, przykład 9.4 | przypadek brzegowy | Suma liczy narożnik d · s dwa razy: przy 0.05 i 0.08 to 3% błędu, przy 0.30 i 0.30 już 18%. |
| 11 | rozdz. 4, obecny panel „Wybierz przekrój” | galeria | Minimalny przekrój to zapalona ścieżka, z której nie da się zgasić żadnego liścia: {I, D} i {I, S} tak, {I, D, S} nie. Oba przechodzą przez inicjację. A: przełącznik; B: obok siebie. |
| 12 | rozdz. 4, obecny panel „Dwa wystąpienia, jedno źródło” | przypadek brzegowy | Powtórzony liść policzony jak dwa niezależne: pod OR prawie dwa razy za dużo, pod AND dwadzieścia razy za mało (q = 0.05), a im mniejsze q, tym gorzej. |
| 13 | rozdz. 4, przykład 9.6 | pojęcie (kadry) | Zasilanie Z narysowane dwa razy to jedno zdarzenie: zapala oba kanały naraz. Po redukcji drzewo ma pojedynczy punkt awarii {Z}, a wynik to 0.0298 zamiast naiwnego 0.0139. |
| 14 | rozdz. 4, obecny panel „Trzy minimalne przekroje” | piaskownica | Wspólna przyczyna dodaje przekrój {I, C}, którego nie usunie poprawianie urządzeń: w łańcuchu +7%, przy dwóch samodzielnych barierach wzrost 3.49-krotny (q = 0.01). Własne, stałe parametry. |
| 15 | rozdz. 5, przykład 9.8 | pojęcie (dwa światy) | Istotność Birnbauma to różnica P(TOP) między światem, w którym liść na pewno zachodzi, a światem, w którym na pewno nie zachodzi: 0.126, 0.0046, 0.00475. |
| 16 | rozdz. 5, obecny panel rankingu | piaskownica | Ta sama względna redukcja: inicjacja zabiera 50% P(TOP), tłumienie 30%, detekcja 18% (r = 0.5); r skaluje słupki, nie kolejność. A: słupki na stałej osi; B: P(TOP) jako pole prostokąta P(I) × P(D ∪ S | I). |
| 17 | rozdz. 6, przykład 9.9 | przypadek brzegowy (kadry) | Przyczyna, której nie ma w drzewie, ma w rachunku prawdopodobieństwo zero. Dopisany zawór V podnosi P(TOP) z 0.00063 do 0.000717 (+14%) i dodaje przekrój {I, V}. |
| 18 | rozdz. 6, akapit o niepewności danych | pojęcie | Jeśli d leży między 0.02 a 0.10, to P(TOP) leży między 0.000492 a 0.00086; raport podaje przedział obok punktu 0.00063. |
| 19 | rozdz. 7, „Podsumowanie” | domknięcie | To samo drzewo odpowiada kolejno na pięć pytań: co, jak, ile, którędy, co najpierw. A: jedno drzewo z przełącznikiem warstw; B: karta z pięcioma wierszami. |

19 przystanków, 24 panele z wariantami (4A/4B, 6A/6B, 11A/11B, 16A/16B,
19A/19B). Punkty 10, 12 i 17 to przypadki brzegowe; 17 i 18 mają niższy
priorytet (rozdział o granicach jest krótki).

## Uzasadnienia

- Punkt 1 realizuje zdanie, którym otwiera się rozdział 2 („Protokół po
  pożarze nie pyta, dlaczego doszło do zapłonu — pyta, dlaczego nie udało się
  go opanować”) i przenosi je przed pojęcie, na otwarcie wykładu. Dwa
  protokoły dają intuicję obu bramek bez ich nazywania: zapłon „był w obu”,
  bariera „wystarczyła jedna”.
- Punkt 4 odpowiada na „najczęstszy błąd początkujących” z rozdziału 2
  (spójnik „i” jako AND). Tekst tłumaczy go trzy razy (intro, po definicji
  9.3, quiz); scena pokazuje go jako dwa schematy z tymi samymi usterkami.
  Budzik (wariant A) jest spoza Bananpolu; wariant B zostaje w świecie kursu.
- Punkty 5, 6, 9 i 11 dają obraz czterem widgetom, które dziś są formularzami.
  Wszystkie cztery to to samo drzewo w innym stanie.
- Punkt 12: pułapka powtórzonego zdarzenia pada pięć razy (tekst, pułapka
  sekcji, quiz, sprawdzenie, ćwiczenie 3). Wykres trzech krzywych nad osią q
  pokazuje od razu obie strony błędu i to, że AND „przykleja się” do zera.
- Punkt 14 usuwa ukrytą zależność od suwaków rozdziału 3 (własne stałe
  parametry) i dokłada drugi wariant architektury z przykładu 9.7(b), w którym
  wspólna przyczyna boli najbardziej.
- Punkt 16 naprawia automatyczną oś Y; wariant B tłumaczy geometrycznie,
  dlaczego wygrywa inicjacja: obcina całą wysokość prostokąta, a bariery tylko
  pasek szerokości.
- Punkty 8 i 9 używają tej samej siatki 10 000 magazyno-lat, o której mówi
  przykład 9.3 („inicjacja w 50, bariery zawodzą w 12.6% z nich”).

## Kolejność

| Priorytet | Punkty | Dlaczego |
|---|---|---|
| A | 5, 6, 9, 11 | widgety bez obrazu; wspólne drzewo |
| A | 1, 4, 12, 16 | otwarcia scenami, najczęściej powtarzana pułapka, usterka osi |
| B | 3, 7, 8, 13, 14, 15 | porządkują główny wątek, przykłady z liczbami tylko w tekście |
| C | 2, 10, 17, 18, 19 | pojęcia pomocnicze, granice, domknięcie |

## Otwarte decyzje

- Punkt 1: historia jest fikcyjna i ogólna („hala magazynowa”), nie Bananpol,
  bo w Bananpolu pożar „na szczęście nie występuje w rejestrach”. Można ją
  osadzić w prawdziwym zdarzeniu, jeśli autor ma ulubiony przykład.
- Punkt 4: budzik (A) czy instalacja Bananpolu (B); oba na tym samym silniku.
- Punkt 6: klikane drzewo z tabelą (A) czy osiem drzewek (B). A zastępuje
  widget, B może stać dodatkowo jako odpowiedź po ćwiczeniu „Wypróbuj”.
- Punkt 14 i istniejący panel: tekst wykładu opisuje zależność od suwaków
  rozdziału 3. Po przyjęciu prototypu dwa zdania tekstu („Panel korzysta
  z suwaków…”, polecenie „wróć do rozdziału o rachunku…”) trzeba dopasować.
- Punkt 16: słupki (A) czy pole (B); B wymaga jednego zdania objaśnienia
  w tekście, bo osie mają różne skale.
- Technika: wszystkie prototypy działają w przeglądarce bez serwera R
  (`sceny.js` + ryciny lcSF na ECharts), jak w wykładach 01–06.
