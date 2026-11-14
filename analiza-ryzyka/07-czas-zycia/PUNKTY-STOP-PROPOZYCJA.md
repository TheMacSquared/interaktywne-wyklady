# Wykład 07 — propozycja punktów stop

Stan przeglądu: 14 listopada 2026, lokalny HEAD `f36fe81`.
Prototypy wszystkich punktów i wariantów stoją w tekście wykładu ze złotą
plakietką „Prototyp Pn · rodzaj” (stan: 14 listopada 2026); decyzje zbiera
[KOLEJKA-ZATWIERDZANIA.md](KOLEJKA-ZATWIERDZANIA.md). Usterki istniejących
obrazów z tabeli „Stan obecny” są poprawione w `modules/block.R`. Tekst
wykładu bez zmian; istniejące widgety zostały obok do porównania.

Podstawa: `PUNKTY-STOP.md`, `SCENY.md`, `07-czas-zycia/modules/block.R`
(8 rozdziałów, 8 obrazów, wszystkie ggplot po stronie serwera R). Wykład
rozkłada się na dwa spotkania; „Część B” zaczyna się od rozdziału o Weibullu.
Liczby w tabelach pochodzą z tekstu wykładu; nowe są policzone w R i oznaczone
„nowa liczba”. Parametry zostają przy kartach katalogowych (MTTF 1500 h,
β = 2, η = 1700 h; decyzja z 2026-10-02) i nie są zestawiane z danymi
eksploatacyjnymi.

## Stan obecny

| Obraz | Rodzaj | Uwagi |
|---|---|---|
| „Oś czasu: kiedy kończy się nasza wiedza” (rozdz. 2) | piaskownica | Oś stała, ale liczby, o które chodzi w tekście (naiwna średnia 487 → 612.5 → 1121 h, szacunek 1812.5 h), nie stoją przy obrazie; odczyt podaje tylko liczbę awarii i łączny czas. Suwak i wykres są w dwóch osobnych panelach. |
| „Wspólny suwak czasu” (rozdz. 3) | pojęcie | Cztery funkcje na jednej osi po przeskalowaniu (f × 3000, h × 1500): sala czyta liczby, których nie ma w żadnym wzorze. Odczyt podaje F i R, a przykład 7.3 pyta też o f i h. Model tylko wykładniczy, więc nie widać, że „f ∝ R” to cecha modelu. |
| „Stały hazard” (rozdz. 4) | piaskownica | Odczyt R(1000 h) nie ma kreski na wykresie. Punkt (MTTF, e⁻¹), o którym mówi tekst, nie jest zaznaczony. |
| „Czas oczekiwania o kształcie k” (rozdz. 4) | piaskownica | Oś Y dopasowuje się automatycznie i ma etykiety w notacji 4e-04: przy zmianie k zmienia się skala, nie rysunek. Dla k = 0.5 gęstość w zerze jest nieskończona i psuje skalę. Brak szczytu i P(T > 1000 h) z przykładu 7.6. |
| „R(t) i h(t) reagują razem” (rozdz. 5) | piaskownica | Panel hazardu ma wolną oś Y (facet `free_y`): β i η zmieniają skalę, a dla β < 1 wykres jest literą L. Hazard stoi nad R(t), choć tekst mówi „dolny wykres”. Legenda powtarza tytuły paneli. |
| „Modele skalibrowane do MTTF = 1500 h” (rozdz. 5) | piaskownica | Polecenie „odczytaj trzy wartości na pionowej linii”, a odczytu nie ma: stały podpis zamiast 0.41 / 0.51 / 0.76. |
| „Suma trzech składowych” (rozdz. 6) | piaskownica | Oś Y dopasowuje się do suwaka zużycia, więc prawe ramię „stoi”, a zmieniają się liczby na osi. Dno wanny (1310, 1160, 1700 w tekście) nie jest zaznaczone ani odczytane. Legenda w kolejności alfabetycznej, suma w środku. |
| „Czy wentylator dotrwa do końca misji?” (rozdz. 7) | piaskownica bez obrazu | Suwak i dwa odczyty. Wymaganie R ≥ 0.90 i termin B10 = 552 h nie mają rysunku. |

Bez obrazu: otwarcie (dwie oferty), cenzorowanie przed definicją, łączny czas
pracy ze wzoru (7.2), systematyczność błędu naiwnej średniej, hazard jako
„ci, którzy dożyli”, pole pod hazardem, MTTF jako pole pod R(t), brak pamięci,
magazyn zapasowych wentylatorów, otwarcie części B, trzy mechanizmy przed
wzorem (7.15), pułapka „jeden Weibull = wanna”, stary czy nowy, domknięcie.

## Wspólny język wizualny

Oś czasu pracy w godzinach z kreską misji 1000 h. Wentylator to pozioma linia
życia zakończona kreską awarii (akcent) albo trójkątem „działa” (szałwia).
Park wentylatorów to linie życia ułożone od najkrótszej do najdłuższej: ich
obwiednia jest krzywą R(t), a łączna długość podzielona przez liczbę linii to
MTTF. Linie parku nie są losowane: wentylator i ma czas życia równy kwantylowi
(i − 0.5)/n modelu, więc liczby na obrazie zgadzają się z tekstem (51 i 71
z 100 po 1000 h). Oferta A (wykładnicza) i oferta B (Weibull) mają stałe
kolory w całym wykładzie. Nawiązanie do wykładu 06: gęstość, pole i próg na
osi wracają w punktach 2 i 21B.

## Propozycja

| # | Gdzie | Rodzaj · moment | Puenta |
|---|---|---|---|
| 1 | rozdz. 1, po głosowaniu, przed przykładem 7.1 (wariant: przed głosowaniem) | scena · wprowadzenie | Dwie karty katalogowe z tym samym MTTF. Sto linii życia z każdej oferty: po 1000 h działa 51 i 71, po 3000 h 14 i 4. Ta sama średnia, inna misja. |
| 2 | rozdz. 1, po przykładzie 7.1 („Skąd bierze się ta różnica?”) | galeria | Dwie gęstości z tym samym MTTF: A najwyższa w zerze, B z garbem przy 1200 h. Pole za kreską misji 500 / 1000 / 3000 h daje liczby z przykładu 7.1. |
| 3 | rozdz. 2, otwarcie, przed definicją 7.3 | scena · wprowadzenie | Badanie kończy się po 1200 h: cztery awarie, cztery wentylatory działają. Średnia z samych awarii to 612.5 h, a każdy z pominiętych żył dłużej niż 1200 h. |
| 4 | rozdz. 2, zamiast obecnej osi czasu | piaskownica | Koniec badania przesuwa naiwną średnią (487 → 612.5 → 1121 h), choć wentylatory są te same; prawdziwe 1368.75 h stoi w miejscu. |
| 5 | rozdz. 2, przy wzorze (7.2) i przykładzie 7.2 | pojęcie (kadry) | Wszystkie godziny pracy sklejone w jedną taśmę 7250 h z czterema awariami: jedna awaria na 1812.5 h. Godziny działających też są w mianowniku. |
| 6 | rozdz. 2, po przykładzie 7.2 | symulacja | Badanie ośmiu wentylatorów powtórzone wiele razy: naiwna średnia skupia się przy 520 h (nowa liczba), szacunek (7.2) rozrzuca się wokół 1500 h. Błąd naiwnej średniej jest systematyczny, nie losowy. |
| 7 | rozdz. 3, przed definicją 7.5 (hazard) | piaskownica · wprowadzenie | Okno 300 h przesuwane po parku 200 wentylatorów: te same awarie w oknie dzielone przez wszystkie nowe (gęstość) albo przez te, które dożyły (hazard). W ofercie A drugi ułamek stoi przy 0.18, w B rośnie. |
| 8 | rozdz. 3, zamiast „Wspólnego suwaka czasu” | pojęcie | Cztery funkcje w jednej chwili bez przeskalowania: F i R na osi prawdopodobieństwa, f i h na wspólnej osi 1/h. Dla t = 1000 h liczby z przykładu 7.3. |
| 9 | rozdz. 3, przy wzorze (7.7) | pojęcie (kadry) | Linie życia ułożone rosnąco dają krzywą R(t); średnia długość linii to pole pod nią. Oferty A i B mają to samo pole i inny kształt. |
| 10 | rozdz. 3, po przykładzie 7.4 (wzór 7.6) | pojęcie | O niezawodności decyduje pole pod hazardem do chwili t: przy 1000 h hazardy ofert są prawie równe, a pola wynoszą 0.667 i 0.346. |
| 11 | rozdz. 4, przed wzorem (7.9) | pojęcie (kadry) | Park po 1000 h: 103 ocalałych z 200. Po wyzerowaniu ich zegara wyglądają jak nowi: 500 h dalej działa 0.72 ocalałych i 0.72 nowych. |
| 12 | rozdz. 4, zamiast widgetu „Stały hazard” | piaskownica | MTTF tylko rozciąga oś czasu: krzywa zawsze przechodzi przez (MTTF, 0.37), więc 63% zawodzi przed średnią. |
| 13 | rozdz. 4, otwarcie sekcji o gamma, przed definicją 7.7 | scena · wprowadzenie | Magazyn z trzema zapasowymi wentylatorami: okres po okresie trzecia awaria przychodzi średnio po 1500 h, ale w co trzecim okresie przed 1000 h. |
| 14 | rozdz. 4, zamiast widgetu gamma | piaskownica | Każdy kolejny etap przesuwa garb o 500 h w prawo i dokłada 500 h średniej; stałe osie, pole za 1000 h (0.677 dla k = 3). |
| 15 | rozdz. 5, otwarcie części B, przed definicją 7.8 | scena · wprowadzenie | Trzy dzienniki awarii o tej samej średniej: w pierwszym awarie gęste na początku, w drugim równo rzedną, w trzecim zbijają się późno. Ryzyko dla działającego maleje, stoi albo rośnie. |
| 16 | rozdz. 5, obecny panel β i η oraz tabela „Jak czytać β” | piaskownica (16A) i galeria (16B) | β obraca hazard, η tylko rozciąga oś; każda krzywa R(t) przechodzi przez (η, 0.37). |
| 17 | rozdz. 5, obecny panel trzech modeli | piaskownica (17A) albo trzy parki (17B) | Ranking trzech modeli o MTTF 1500 h odwraca się z czasem misji: do 1830 h prowadzi β = 2.5, potem β = 1, od 2600 h β = 0.7. |
| 18 | rozdz. 6, przed wzorem (7.15) | scena · wprowadzenie | Wentylator ma trzy zegary (lut, przepięcie, łożysko) i psuje go ten, który zadzwoni pierwszy. Wcześnie dzwoni lut, późno łożysko. |
| 19 | rozdz. 6, zamiast widgetu wanny | piaskownica | Suwak zużycia rusza tylko prawe ramię; dno przesuwa się z 1310 do 1160 albo 1700. Pod osią: który mechanizm dominuje i jakie ma lekarstwo. |
| 20 | rozdz. 6, przy pułapce | przypadek brzegowy | Hazard jednego Weibulla obraca się wokół punktu, ale nigdy nie zawraca: pasuje do lewego albo prawego ramienia wanny, nigdy do obu. |
| 21 | rozdz. 7, zamiast suwaka bez obrazu | piaskownica (21A na R(t), 21B na gęstości) | Wymaganie R ≥ 0.90 wyznacza termin: 552 h dla Weibulla, 158 h dla wykładniczego. Ryzyko przed przeglądem to pole pod gęstością przed kreską. |
| 22 | rozdz. 7, przykład 7.11 | piaskownica z przypadkiem brzegowym | Używany a nowy na kolejne 500 h: przy zużyciu 0.649 wobec 0.917, przy stałym hazardzie 0.717 i 0.717, przy wczesnych defektach używany jest lepszy (0.747 wobec 0.579, nowa liczba). |
| 23 | rozdz. 8 | domknięcie (23A karty, 23B pięć pytań) | Karty katalogowe z punktu 1 wracają poprawione: zamiast samego MTTF kierunek hazardu, R(1000 h), B10 i sens wymiany. |

Punktów jest 23, bo wykład zajmuje dwa spotkania: 14 w części A (rozdz. 1–4)
i 9 w części B (rozdz. 5–8). Osiem z nich zastępuje istniejące widgety
ggplot (4, 8, 12, 14, 16A, 17A, 19, 21).

## Uzasadnienia

- Punkt 1 jest historią, od której zaczyna tekst („dwie oferty, kierownik pyta
  o 1000 h”), ale dziś ma tylko wzory. Linie życia pokazują odpowiedź przed
  rachunkiem i wprowadzają język parku, którym mówią punkty 7, 9, 11 i 17B.
- Punkt 3: cenzorowanie zaczyna się dziś od definicji. Scena z analitykiem,
  który wyrzuca działające egzemplarze, stawia błąd przed pojęciem.
- Punkt 6 odpowiada na zdanie powtarzane czterokrotnie (pułapka rozdziału,
  sprawdzenie, ćwiczenie 2, odpowiedź przykładu 7.2): „błąd jest
  systematyczny”. To jedyna symulacja w wykładzie.
- Punkt 7: najtrudniejsze pojęcie wykładu („gęstość maleje, choć hazard stoi”)
  ma dziś tylko wykres z przeskalowanymi krzywymi. Okno na parku pokazuje oba
  mianowniki na tych samych awariach: 19 z 200 i 19 z 103 przy 1000 h, 5 z 200
  i 5 z 27 przy 3000 h.
- Punkt 10 daje obraz odpowiedzi przykładu 7.4: równy hazard w chwili t, różne
  pola do chwili t.
- Punkt 11: brak pamięci pada w tekście sześć razy (wzór, przykład 7.5,
  sprawdzenie, quiz, pułapka, podsumowanie). `SCENY.md` ostrzega, że kostka
  „po passie” nie potrzebuje sceny, dlatego to pojęcie w kadrach na parku,
  a nie scena.
- Punkt 13 przenosi historię magazynu (przykład 7.6) przed definicję. To
  historia z powtórzeniem: puentę niesie magazynier, który liczy okresy.
- Punkt 15 otwiera drugie spotkanie obrazem zamiast przypomnienia wzorów.
  Używa trzech modeli z przykładu 7.8.
- Punkt 20: pułapka „Weibull = wanna” pada cztery razy i zawsze jako zakaz.
- Punkt 22 łączy przykład 7.11 z ostatnim akapitem rozdziału 7 („wymiana może
  zaszkodzić”), który dziś nie ma ani liczby, ani obrazu.
- Punkty 4, 8, 14, 16A, 19 i 21 naprawiają to, co dziś psuje puentę:
  automatyczne osie, przeskalowane krzywe, brak odczytów, widget bez obrazu.

## Kolejność

| Priorytet | Punkty | Dlaczego |
|---|---|---|
| A | 1, 3, 7, 15, 22 | najsilniejsze puenty bez obrazu |
| A | 8, 14, 16A, 19, 21 | istniejące obrazy z usterką |
| B | 4, 5, 10, 11, 13, 17, 18, 20 | porządkują główny wątek |
| C | 2, 6, 9, 12, 16B, 23 | pojęcia pomocnicze, warianty, domknięcie |

## Otwarte decyzje

- Scena 1 przed głosowaniem czy po nim (jak w wykładzie 06). Stoi w obu.
- Punkt 2: jedna oś (2A) czy dwa wykresy jeden pod drugim (2B).
- Punkt 6 używa modelu wykładniczego z MTTF 1500 h jako „prawdy”; osiem
  czasów z tekstu (220 … 3100 h) to jedna realizacja i zostaje w punktach 3–5.
- Punkt 7 liczy ułamki na 200 liniach, więc wychodzi 0.184 zamiast dokładnego
  0.181 (1 − e^(−0.2)); okno 300 h jest za szerokie na wzór h(t)Δt. Mniejsze
  okno wymaga większego parku.
- Punkt 16: suwaki (16A) czy galeria czterech β (16B), czy oba (różne miejsca).
- Punkt 17: krzywe z suwakiem misji (17A) czy trzy parki (17B).
- Punkt 18 potrzebuje skali czasu dla hazardów „w jednostkach względnych”
  z przykładu 7.9; scena przyjmuje h/1000 na godzinę (nowe założenie, bez
  liczb na obrazie).
- Punkt 21: termin na krzywej R(t) (21A) czy pole pod gęstością (21B, język
  wykładu 06).
- Punkt 23: karty (23A), pięć pytań (23B) czy oba.
