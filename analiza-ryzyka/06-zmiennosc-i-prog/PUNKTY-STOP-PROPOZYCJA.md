# Wykład 06 — propozycja punktów stop

Stan przeglądu: 14 listopada 2026, lokalny HEAD `b201a5b`.
Prototypy wszystkich punktów i wariantów stoją w tekście wykładu ze złotą
plakietką „Prototyp Pn · rodzaj” (stan: 14 listopada 2026); decyzje zbiera
[KOLEJKA-ZATWIERDZANIA.md](KOLEJKA-ZATWIERDZANIA.md). Usterki istniejących
obrazów z tabeli „Stan obecny” są poprawione w `modules/block.R`. Tekst
wykładu bez zmian; istniejące widgety zostały obok do porównania.

Podstawa: `PUNKTY-STOP.md`, `SCENY.md`, `06-zmiennosc-i-prog/modules/block.R`
(8 rozdziałów, 6 obrazów, wszystkie ggplot po stronie serwera R).
Liczby w tabelach pochodzą z tekstu wykładu; nowe są oznaczone „do policzenia”.

## Stan obecny

| Obraz | Rodzaj | Uwagi |
|---|---|---|
| Histogram a liczebność (rozdz. 1) | prawie symulacja | Suwak n przy stałym ziarnie: nic nie narasta, sala nie widzi, jak kształt „wyłania się” z pomiarów. Tekst niesie dwie puenty (gęstość jako granica histogramu; ogon jest najsłabiej obsadzony). |
| „Przesuń i rozszerz krzywą” (rozdz. 2) | piaskownica | Oś Y dopasowuje się automatycznie, więc przy zmianie σ krzywa nie robi się „szersza i niższa”, tylko zmieniają się liczby na osi. Progu 85°C nie ma na rysunku, jest tylko odczyt z. |
| „Próg temperatury łożyska” (rozdz. 3) | piaskownica | Rysuje rozkład z suwaków μ i σ z rozdziału 2 (`input$z6_mean`, `input$z6_sd`). Liczby w tekście (0.500 → 0.434) zgadzają się tylko przy ustawieniach domyślnych tamtego panelu. Oś X też od nich zależy. |
| „Która interwencja…” (rozdz. 4) | galeria bez obrazu | Lista wyboru i dwa odczyty. Najmocniejsza puenta wykładu (chłodzenie i wyższy próg dają to samo pole, a fizycznie są czymś innym) nie ma rysunku. |
| Chmura par L i S (rozdz. 5) | piaskownica | 700 punktów ze stałym ziarnem, „jednostki” zamiast kN; historia zawiesia przychodzi dopiero po wzorach. Suwaki tylko dla średnich, choć przykład 6.9 działa na σ. |
| Trzy rozkłady (rozdz. 6) | galeria | Liczby z przykładu 6.10 (P(T > 85), P(T > 91)) nie stoją przy obrazie; odczyt to stały podpis. Histogram liczebności z 60 słupkami nie pokazuje ogona, o który chodzi. |

Bez obrazu: otwarcie (trzy łożyska), gęstość i P(X = x) = 0, standaryzacja,
pomiar a zmiana robocza, pułapka pola nakładania, komunikat i domknięcie.

## Propozycja

| # | Gdzie | Rodzaj · moment | Puenta |
|---|---|---|---|
| 1 | rozdz. 1, po głosowaniu, zamiast przykładu 6.1 jako tekstu | scena · wprowadzenie | Raport podaje trzy wentylatory ze średnią 82°C i adnotacją „w normie”. Dzienniki pomiarów pokazują 1, 159 i 274 przekroczenia na 1000. Ta sama średnia, inne ryzyko. |
| 2 | rozdz. 1, zamiast obecnego histogramu | symulacja | Pomiary dochodzą zmiana po zmianie (+10 / +100 / +1000): środek histogramu ustala się szybko, odsetek ponad 85°C najpóźniej. |
| 3 | rozdz. 2, „Dlaczego P(X = x) = 0” | pojęcie | Prawdopodobieństwo to pole między dwiema kreskami. Zsuwane kreski (79–85 → 81.95–82.05 → punkt) zostawiają wysokość 0.133 i pole zero. |
| 4 | rozdz. 2, obecny panel μ i σ | piaskownica | μ przesuwa krzywą, σ ją rozszerza i obniża; próg 85°C stoi w miejscu. Stałe osie, blady kontur N(82, 3), zacieniowany ogon. |
| 5 | rozdz. 2, przykład 6.3 | pojęcie | Linijka σ pod osią temperatury: 68% w ±1σ, 95% w ±2σ. Ta sama linijka wraca w punktach 6, 7, 9 i 12. |
| 6 | rozdz. 3, przykład 6.4 | pojęcie (kadry) | Temperatura w °C i ciśnienie w barach mają osobne osie, dopóki nie przyłożymy linijki σ: progi lądują w z = 1 i z = 3, pola 0.159 i 0.00135. |
| 7 | rozdz. 3, obecny panel progu | piaskownica | Te same pół stopnia kosztuje 6.6 pp przy średniej i 0.06 pp w ogonie. Własne, stałe parametry N(82, 3) i stała oś. |
| 8 | rozdz. 3, „Pomiar a zmiana robocza”, przed wzorem (6.7) | scena · wprowadzenie | Kierownik zmiany przegląda dzienniki: 16% kratek jest czerwonych, a alarm ma około trzech zmian na cztery. Odsetek pomiarów to nie odsetek zmian. |
| 8b | przełącznik w 8 | przypadek brzegowy | Pomiary skorelowane (ρ = 0.8 między kolejnymi godzinami): czerwone kratki zbijają się w te same zmiany i alarm ma około 45% zmian zamiast 75%. |
| 9 | rozdz. 4, zamiast listy wyboru | galeria | Stan bazowy, chłodzenie, stabilizacja, wyższy próg na jednej osi z konturem stanu bazowego. Chłodzenie i wyższy próg dają to samo pole 0.048, ale krzywa rusza się tylko w jednym. |
| 10 | rozdz. 4, przy wyprowadzeniu „kiedy zmniejszenie σ pomaga” | przypadek brzegowy | Próg 80°C poniżej średniej: zwężenie rozkładu podnosi ryzyko z 0.748 do 0.841. Może być przełącznikiem w 4 albo 9. |
| 11 | rozdz. 5, otwarcie, przed definicją 6.6 | scena · wprowadzenie | Dźwigowy podnosi ładunek za ładunkiem; każde podniesienie to para: ten ładunek trafił na to zawiesie. Średnio 10 kN zapasu, a około 173 podniesień na 1000 kończy się pod przekątną. |
| 12 | rozdz. 5, przy wzorach (6.8)–(6.10) | pojęcie (kadry) | Chmura par → odległość od przekątnej → histogram marginesu D z progiem zero. Dwie zmienne sprowadzają się do jednej, β to z marginesu, a D jest szersze niż L i S osobno. |
| 13 | rozdz. 5, przy pułapce | przypadek brzegowy | Pole wspólne dwóch gęstości wynosi 0.50 i nie zmienia się po zamianie krzywych miejscami, a P(L > S) skacze z 0.173 na 0.827. Pole nakładania nie wie, która krzywa jest która. |
| 14 | rozdz. 6, obecny panel kształtów | galeria | Trzy modele o tej samej średniej i σ na jednej osi z progami 85 i 91°C: przy 1σ dają 0.159 / 0.149 / 0.091, przy 3σ różnią się 5–9 razy. Odczyty przy obrazie, widok ogona w powiększeniu. |
| 15 | rozdz. 6, przykład 6.11 | pojęcie (kadry) | Pięć pomiarów ustawionych rosnąco obok pięciu kwantyli N(82, 3); ostatni odstaje o 2.8°C. Tak powstaje punkt wykresu Q–Q. |
| 16 | rozdz. 7 albo 8 | domknięcie | Raport z punktu 1 wraca poprawiony: zamiast „średnia 82°C, w normie” trzy zdania komunikatu, każde ze źródłem (wzór, obraz). |

Punkty 8b i 10 mogą być przełącznikami; wtedy zostaje 14 przystanków.
Rozdział 6 jest rozszerzeniem, więc 14 i 15 mają niższy priorytet.

## Uzasadnienia

- Punkt 8 odpowiada na ostrzeżenie, które wykład powtarza sześć razy tekstem
  (karta danych, przykład 6.6, sprawdzenie, quiz, ćwiczenie 1, komunikat).
  Jeden obraz zdejmuje ciężar z powtórzeń. Dziennik zmiany jako rząd ośmiu
  kratek to taśma kontroli z wykładu 05 w nowej roli.
- Punkt 9 daje obraz najmocniejszej puencie rozdziału 4. Dziś sala widzi
  tylko liczby 0.048 i 0.048; na rysunku widać, że w jednym wariancie
  przesunęła się krzywa, a w drugim kreska.
- Punkt 13: pułapka pola nakładania pada cztery razy (tekst, pułapka
  rozdziału, quiz, ćwiczenie 3) i zawsze jako zakaz. Zamiana krzywych
  miejscami pokazuje powód jednym ruchem.
- Punkty 4 i 7 naprawiają to, co dziś psuje puentę: automatyczną oś Y
  i ukrytą zależność panelu progu od suwaków z poprzedniego rozdziału.
- Punkt 11 przenosi historię zawiesia przed pojęcie, zgodnie z `SCENY.md`.
  Przykład transferowy 6.8 zostaje jako rachunek na tych samych liczbach.
- Wspólny język wizualny: stała oś 70–95°C, blady kontur N(82, 3), linijka σ
  pod osią i dziennik pomiarów w kratkach. Scena 1 i domknięcie 16 używają
  tego samego raportu.

## Kolejność

| Priorytet | Punkty | Dlaczego |
|---|---|---|
| A | 1, 8, 9, 13 | najsilniejsze puenty bez obrazu |
| A | 4, 7 | istniejące obrazy z usterką |
| B | 2, 5, 6, 11, 12 | porządkują główny wątek |
| C | 3, 14, 15, 16 | pojęcia pomocnicze, rozszerzenie, domknięcie |

## Otwarte decyzje

- Scena 1 przed głosowaniem czy po nim. Przed: zdradza odpowiedź. Po:
  głosowanie zostaje uczciwym pytaniem, scena je rozstrzyga.
- Punkt 2: jedna puenta do wyboru (kształt się stabilizuje albo ogon jest
  najsłabiej obsadzony); druga zostaje w tekście albo jako odczyt.
- Punkt 15 dubluje prototyp 6B „kolejka” ze statystyki 06 (do oceny). Można
  użyć tego samego silnika z pięcioma pomiarami łożyska albo poczekać na
  decyzję w statystyce.
- Technika: wykłady 01–05 mają już ryciny w przeglądarce (ECharts, lcSF,
  `sceny.js`). Punkty 4, 7, 9 i 14 mogą iść na wzorzec „Dwie krzywe normalne”
  ze statystyki 03 i `nb_chart.js` z wykładu 05.
