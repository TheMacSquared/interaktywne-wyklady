# Wykład 08 — propozycja punktów stop

Stan przeglądu: 14 listopada 2026, lokalny HEAD `f36fe81`.
Prototypy wszystkich punktów i wariantów stoją w tekście wykładu ze złotą
plakietką „Prototyp Pn · rodzaj” (stan: 14 listopada 2026); decyzje zbiera
[KOLEJKA-ZATWIERDZANIA.md](KOLEJKA-ZATWIERDZANIA.md). Usterki istniejących
obrazów z tabeli „Stan obecny” są poprawione w `modules/block.R`. Tekst
wykładu bez zmian; istniejące widgety zostały obok do porównania.

Podstawa: `PUNKTY-STOP.md`, `SCENY.md`, `08-niezawodnosc-systemu/modules/block.R`
(7 rozdziałów, 9 widgetów, z czego 5 to wykresy ggplot po stronie serwera R,
a 4 nie mają obrazu). Liczby w tabelach pochodzą z tekstu wykładu; nowe są
policzone w R i oznaczone „nowe”.

## Stan obecny

| Obraz | Rodzaj | Uwagi |
|---|---|---|
| Głosowanie 0.81 / 0.90 / 0.99 (rozdz. 1) | materiał | Dobre pytanie; odpowiedź pada tekstem, bez obrazu trzech systemów. |
| „Trzy architektury tych samych elementów” (rozdz. 2) | galeria | Statyczny rysunek: wszystkie bloki w jednym kolorze, nic nie świeci ani nie gaśnie. Polecenie „wskaż, ile bloków musi zawieść” nie ma jak dostać odpowiedzi z obrazu. Brak wejścia i wyjścia schematu. |
| „Wszystkie muszą działać” i „Co najmniej jedna gałąź” (rozdz. 3) | piaskownica bez obrazu | Pola wyboru i jedno zdanie statusu. Schematu, o którym mówi rozdział 2, tu nie ma. |
| „Te same R, inny system” (rozdz. 3) | piaskownica | Naraz widać tylko jedną architekturę, więc porównanie 0.874 a 0.996 wymaga pamięci. Słupki bez wartości, kolory bez znaczenia, brak ryzyka w odczytach (tekst porównuje ryzyko). |
| „Sterownik C oraz wentylatory A/B” (rozdz. 4) | pojęcie bez obrazu | Trzy kroki redukcji jako trzy zdania; schemat się nie zwija, liczby 0.996 i 0.976 nie padają. |
| „Elementy i system mieszany” (rozdz. 4) | piaskownica | Krzywa systemu ma kolor jak pozostałe i ginie między nimi; oś Y nie zaczyna się od zera; brak punktów w chwili misji i odczytów R elementów, które cytuje przykład 8.6 (0.574, 0.607, 0.670). |
| „Wspólne zasilanie” (rozdz. 5) | piaskownica | Linia bez punktu odniesienia: nie widać obiecanego 0.996 ani tego, ile ryzyka pochodzi ze wspólnej przyczyny (72% z przykładu 8.8). Odczyt tylko R. |
| „Liczba gałęzi i koszt” (rozdz. 6) | piaskownica | Oś Y dopasowuje się do suwaka R gałęzi, więc przy r = 0.6 krzywa wygląda tak samo jak przy 0.9, zmieniają się tylko liczby na osi. Polecenie każe zapisywać przyrost, a odczytu przyrostu nie ma. Sufitu 1 − q z tekstu nie ma na rysunku. |
| „Spadek ryzyka po poprawie R o 0.02” (rozdz. 6) | materiał bez obrazu | Trzy odczyty; pytanie z polecenia („gdyby zasilanie miało 0.90”) nie ma kontrolki. Układu mieszanego, w którym różnica jest dwudziestokrotna, nie ma wcale. |

Bez obrazu: otwarcie (nocna awaria i trzy liczby), definicja sukcesu, dwa
horyzonty z przykładu 8.2, długi łańcuch (0.95¹⁰ ≈ 0.60), tabela stanów
i funkcja struktury, oszacowanie (8.9), wspólna rozdzielnica jako historia,
beta-factor, układ k z n, kopie zapasowe, istotność Birnbauma i domknięcie.

## Wspólny język wizualny

Schemat blokowy niezawodności z lampką na wyjściu. Blok działający ma
obrys i nazwę, blok zepsuty jest w kolorze akcentu i przekreślony. Linie
drogi sukcesu świecą szałwią tam, dokąd da się dojść od wejścia przez
działające bloki, i gasną za zepsutym blokiem. Lampka „chłodzenie” na
prawym końcu świeci, gdy dochodzi do niej choć jedna droga. Ten sam
schemat wraca w trzech skalach: duży i klikalny (5, 21, 22), w kadrach
historii (1, 3, 10, 15, 18, 24) i jako kafel stanu w tabeli stanów
(6, 8, 12, 13, 14, 20). Drugi motyw to kratki misji: sto albo tysiąc
misji, zawiedzione w kolorze akcentu (2, 15, 18).

## Propozycja

| # | Gdzie | Rodzaj · moment | Puenta |
|---|---|---|---|
| 1 | rozdz. 1, przy głosowaniu | scena · wprowadzenie | Nocą staje jeden z dwóch wentylatorów. Rano trzy osoby piszą trzy raporty: „system zawiódł”, „nic się nie stało”, „nie dotyczy”. Te same części i ta sama noc, a werdykt zależy od tego, co uznano za sukces. |
| 2 | rozdz. 1, po przykładzie 8.1 | galeria albo symulacja | Sto misji tych samych dwóch elementów: 81 razy działają oba, 9 + 9 razy jeden, raz żaden. Trzy logiki czytają te same kratki jako 19, 10 i 1 zawiedzioną misję. |
| 3 | rozdz. 2, otwarcie sekcji „Niezawodność systemu na czas misji” | scena · wprowadzenie | Ta sama instalacja z dwiema definicjami sukcesu: „utrzymać 14.5 °C” wymaga wentylatora i zasilania (0.902, nowe), „utrzymać i zaalarmować” dokłada czujnik (0.857). Kto wymaga więcej, dostaje dłuższy szereg. |
| 4 | rozdz. 2, przykład 8.2 | pojęcie (kadry) · przypadek brzegowy | 0.92 i 0.95 z karty leżą na osi czasu w dwóch różnych miejscach: 1000 h i 8760 h. Mnożyć wolno tylko punkty z jednej pionowej kreski: 0.92 · 0.994 = 0.915, nie 0.874. |
| 5 | rozdz. 2, obecny „Schemat” | piaskownica | Kliknięty blok gaśnie, a za nim gaśnie droga. Szereg traci lampkę po jednym kliknięciu, układ równoległy po dwóch, w mieszanym wystarczy sterownik C. |
| 6 | rozdz. 3, zamiast pól wyboru szeregu | galeria | Osiem możliwych stanów trzech elementów jako osiem małych schematów: w szeregu świeci jeden. Stąd iloczyn R. |
| 7 | rozdz. 3, przy „dziesięć elementów po 0.95” i przykładzie 8.3 | piaskownica | Każde ogniwo mnoży R: dziesięć ogniw po 0.95 daje 0.60, a moduły po 0.99 mieszczą się nad progiem 0.90 tylko do dziesiątego. |
| 8 | rozdz. 3, zamiast pól wyboru układu równoległego | galeria | Cztery stany dwóch wentylatorów: gaśnie jeden kafel, 0.08 · 0.05 = 0.004. Łatwiej policzyć jedną przegraną niż trzy wygrane. Przełącznik na szereg pokazuje lustro: świeci jeden kafel, 0.874. |
| 9 | rozdz. 3, obecny „Przełącznik architektury” | piaskownica | Kwadrat o boku 1 podzielony przez R₁ i R₂ na cztery pola. Szereg to jedno pole, układ równoległy to wszystko poza małym narożnikiem. Suwaki zmieniają pola, nie osie. |
| 10 | rozdz. 4, obecne kroki redukcji | pojęcie (kadry) | Schemat zwija się od środka: A i B w blok 0.996, potem szereg z C w jeden blok 0.976. Z ryzyka 0.024 aż 0.020 to sterownik. |
| 11 | rozdz. 4, obecny panel „Czas” | piaskownica | Krzywa systemu leży pod krzywą sterownika przy każdym t (0.558 a 0.670 przy 1000 h). Druga puenta do wyboru: mnożnik redundancji rośnie z 0.05 przy 100 h do 0.78 przy 3000 h. |
| 12 | rozdz. 4, przykład 8.7 | pojęcie | Tabela stanów jako osiem schematów z prawdopodobieństwami: świecą trzy, 0.85652 + 0.04508 + 0.07448 = 0.97608. |
| 13 | rozdz. 4, przy oszacowaniu (8.9) | galeria | Te same osiem kafli dla szeregu, układu mieszanego i równoległego: świeci 1, 3 i 7. Każdy układ koherentny leży między 0.857 a 0.99992. |
| 14 | rozdz. 4, przy sprawdzeniu koherentności | przypadek brzegowy | φ = x_A(1 − x_B): naprawa B gasi lampkę. Takiego układu nie da się narysować schematem blokowym. |
| 15 | rozdz. 5, otwarcie, przed definicją 8.7 | scena · wprowadzenie | Schemat obiecuje 4 zawiedzione misje na 1000. W hali oba wentylatory wiszą na jednej rozdzielnicy: gdy pada, stają oba w tej samej sekundzie. Na schemacie brakowało bloku; z nim jest 14 na 1000. |
| 16 | rozdz. 5, obecny panel „Zależność” | piaskownica | Obiecane 0.996 jako linia odniesienia i ryzyko rozłożone na dwie części: przy q = 0.01 wspólne zasilanie to 72% ryzyka. |
| 17 | rozdz. 5, model beta-factor | piaskownica | β przenosi część awarii z gałęzi do wspólnego bloku: przy β = 0.1 ryzyko rośnie z 0.0064 do 0.0135, choć każdy wentylator ma nadal 0.92. |
| 18 | rozdz. 6, otwarcie | scena · wprowadzenie | Kierownik dokłada wentylatory po 100 jednostek. Na 10 000 misji pierwszy zapas ratuje 900, drugi 90, trzeci 9. Cena ta sama, zakup dziesięć razy mniejszy. |
| 19 | rozdz. 6, obecny panel „Trade-off” | piaskownica + przypadek brzegowy | Stała oś: przy r = 0.9 krzywa kładzie się po drugiej gałęzi, przy r = 0.6 rośnie dłużej. Przełącznik wspólnego zasilania kładzie sufit 0.99, pod którym cel 0.9999 jest nieosiągalny. |
| 20 | rozdz. 6, „Układ k z n” | galeria | Osiem kafli trzech czujników: przy k = 1 świeci 7, przy k = 2 cztery, przy k = 3 jeden; 0.999, 0.972, 0.729. k przesuwa układ od równoległego do szeregu. |
| 21 | rozdz. 6, „Kopie zapasowe” | scena (jeden obraz z „co, jeśli”) | Trzy kopie w jednej serwerowni przeżywają awarię dysku, ale nie pożar ani ransomware. Układ 3-2-1 przeżywa wszystkie trzy zdarzenia. |
| 22 | rozdz. 6, po definicji 8.8 | galeria | Element rozstrzyga tylko w niektórych stanach pozostałych: sterownik prawie zawsze (0.996), wentylator A tylko wtedy, gdy B już stoi (0.049). |
| 23 | rozdz. 6, obecne „Porównanie” | piaskownica | Ta sama poprawa o 0.02 w trzech miejscach szeregu: 0.019, 0.018, 0.017. Gdy zasilanie spada do 0.90, kolejność się odwraca. |
| 24 | rozdz. 7, podsumowanie albo „Pięć kroków” | domknięcie | Pięć kroków na jednym schemacie, każdy z wynikiem: sukces, misja 1000 h, redukcja 0.976, wspólne zasilanie 0.966 (nowe), ranking interwencji. |

Punkty 6, 8, 12, 13, 14 i 20 to jeden obraz (kafle stanów) w sześciu rolach;
jeśli to za dużo powtórzeń, 13 i 14 mogą być przełącznikami w 12, a 8
przełącznikiem w 6. Wtedy zostaje 20 przystanków.

## Warianty

| # | Wariant A | Wariant B |
|---|---|---|
| 1 | scena po głosowaniu | scena przed głosowaniem |
| 2 | kratki z przełącznikiem logiki (dokładne 19 / 10 / 1) | symulacja „misja za misją” (+1 / +10 / +100) |
| 4 | kadry na osi czasu | piaskownica z suwakiem wspólnego czasu misji |
| 5 | jeden schemat z przełącznikiem architektury | trzy schematy obok siebie |
| 9 | kwadrat czterech pól | słupki obu architektur naraz |
| 11 | krzywe elementów i systemu | blok A/B a sam wentylator A: mnożnik redundancji |
| 16 | linia R(q) z obiecanym 0.996 | pasek ryzyka w podziale na przyczyny |
| 19 | R na osi liniowej (nasycenie) | ryzyko na osi logarytmicznej z celem 0.9999 |
| 24 | kadry: schemat rośnie krok po kroku | mapa pięciu kroków z wynikami |

## Uzasadnienia

- Punkt 1 pokazuje to, co intro rozdziału 1 tylko zapowiada („trzy osoby
  podają trzy różne wartości”). Głosowanie zostaje uczciwym pytaniem, scena
  je rozstrzyga.
- Punkt 5 naprawia główny brak wykładu: definicja 8.2 mówi o „drodze przez
  działające bloki”, a nigdzie nie można zepsuć bloku i zobaczyć, co się
  dzieje z drogą. Ten sam silnik niesie potem punkty 21 i 22.
- Punkty 6 i 8 zastępują pola wyboru bez obrazu. Tekst sam liczy kombinacje
  („wygrywająca jest dokładnie jedna”, „przegrywająca jest jedna”), więc
  obraz pokazuje je wszystkie naraz, zamiast kazać je przeklikać.
- Punkt 12 daje obraz wzorowi (8.8), a 13 oszacowaniu (8.9): przejście od
  szeregu do układu równoległego to dokładanie świecących kafli.
- Punkt 15 odpowiada na ostrzeżenie powtarzane pięć razy tekstem (pułapki
  w rozdz. 3, przykład 8.8, sprawdzenie, quiz, ćwiczenie 3). Historia stoi
  przed pojęciem, zgodnie z `SCENY.md`.
- Punkt 19 naprawia automatyczną oś i dokłada sufit 1 − q, który tekst
  opisuje słowami („realna krzywa ma sufit”).
- Punkt 21 bierze sytuację spoza Bananpolu, którą zna każdy student; reguła
  3-2-1 to wspólna przyczyna w praktyce.

## Kolejność

| Priorytet | Punkty | Dlaczego |
|---|---|---|
| A | 1, 5, 15, 10 | najsilniejsze puenty bez obrazu i wspólny język |
| A | 9, 11, 16, 19 | istniejące obrazy z usterką |
| B | 2, 6, 8, 12, 18, 22, 23 | porządkują główny wątek |
| C | 3, 4, 7, 13, 14, 17, 20, 21, 24 | pojęcia pomocnicze, rozszerzenia, domknięcie |

## Otwarte decyzje

- Scena 1 przed głosowaniem czy po nim (jak w wykładzie 06).
- Ile ról kafli stanów zostaje jako osobne przystanki (zob. uwaga pod tabelą).
- Punkt 3 używa liczb karty Bananpolu (wentylator 0.92, czujnik 0.95,
  zasilanie 0.98); wynik 0.902 dla węższej definicji jest nowy, 0.857 pada
  w ćwiczeniu 2.
- Punkt 11: jedna puenta do wyboru; druga zostaje w tekście.
- Punkt 24: wynik 0.966 (układ mieszany ze wspólnym zasilaniem q = 0.01)
  nie pada w tekście; łączy przykłady 8.5 i 8.8.
- Technika jak w wykładach 04–06: sceny w `modules/sceny.js`
  (`window.lcS8`), wykresy jako ryciny lcSF na ECharts.
