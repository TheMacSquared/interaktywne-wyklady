# Sceny — historia przed pojęciem

Scena to widget, który **opowiada sytuację, zanim padnie pojęcie**. Na scenie
jest postać albo obiekt ze świata wykładu i coś się dzieje: ktoś coś robi, coś
z czegoś wynika, coś się zmienia. Pojęcie formalne (definicja, wzór, „Zasada”)
przychodzi w tekście po scenie i nazywa to, co student już zobaczył, tymi
samymi słowami.

Scena nie musi być krokowa i nie musi losować. Losowanie, kroki i powtórzenia
to tylko jedna z form (niżej). O tym, czy coś jest sceną, decyduje historia,
a nie layout.

Polecenie „zrób scenę do …” oznacza: zaproponuj sytuację i formę według tego
dokumentu (brief na końcu), a po akceptacji zbuduj widget w przedmiocie
i wykładzie wskazanym w poleceniu.

## Co sceną nie jest

- **Symulacja w kostiumie.** Abstrakcyjny eksperyment (losuj próbę z rozkładu,
  licz odsetek odrzuceń) przebrany za postacie. Jeśli bez rysunku zostaje
  zwykła symulacja, to nie jest scena.
- **Sztuczna perspektywa.** Sytuacja opowiedziana z miejsca, z którego nikt jej
  nie przeżywa (pasażer „przychodzi w losowej chwili”, choć z jego punktu
  widzenia losowy jest autobus).
- **Abstrakcyjny aparat.** Worek z kulkami, deska Galtona, latarka. Zastępujemy
  je sytuacją, gdy da się pokazać to samo na ludziach i przedmiotach.
- **Suwak na krzywej.** Eksploracja parametrów to osobny typ widgetu.

## Formy

Formę dobiera się do puenty. Przykłady to sceny, które już działają.

| Forma | Kiedy | Przykłady |
|---|---|---|
| **Kadry historii** | pojęcie wynika z kolejnych zdarzeń albo z budowy czegoś; bez losowania | student wchodzi do tabeli i jego cechy stają się wierszem (statystyka 00, `ch1_tabela`) |
| **Jeden obraz z „co, jeśli”** | pojęcie to struktura sytuacji; interakcja zmienia jej warunek | plac z paletami: Ω, A, Aᶜ, losowanie a wybór „na oko” (analiza ryzyka 01, `omega.js`); macierz ryzyka z przełącznikiem horyzontu (`riskmatrix.js`) |
| **Dwa światy obok siebie** | puentą jest różnica między dwoma sposobami albo dwoma sytuacjami | telefon do losowych osób a ankieta w bibliotece (statystyka 00, `ch3_latarka`); linie autobusowe A i K: ta sama średnia, inne ryzyko (statystyka 01) |
| **Historia z powtórzeniem** | puentą naprawdę jest to, co wychodzi przy wielu powtórzeniach | grupki zza drzwi EGZAMIN i p̂ (statystyka 00, `ch3_worek`); herbata z mlekiem (`ch4_herbata`); eksperymenty z kostkami, deszczem, wagą (statystyka 02, `experiment.js`); zdrapka; ocena prowadzącego i CTG; grupki z miarką i siatki przedziałów (statystyka 03) |

Możliwa jest też forma bez żadnej interakcji (animowany ciąg kadrów), jeśli
historia tego nie potrzebuje.

## Zasady wspólne

1. **Świat wykładu.** Scena korzysta z ludzi, danych i miejsc, które już są
   w wykładzie (wydział i egzamin, ankieta studentów, Bananpol). Sytuacja ma być
   z życia studenta albo inżyniera.
2. **Ktoś albo coś działa.** Postać lub obiekt, który robi coś rozpoznawalnego.
   Przycisk, jeśli jest, nazywa tę czynność („Zapytaj grupkę”, „Kup i zdrap los”).
3. **Interakcja służy historii.** Przesuwa ją albo zmienia jej warunek. Nie
   dodajemy przełączników „na zapas”: jedna scena, jedna puenta.
4. **Pojęcie po obrazie.** Symbol, jeśli jest potrzebny, stoi przy obiekcie,
   który oznacza (ω pod paletą, X̄ przy grupce). Definicja i wzór są w tekście po
   scenie i używają tych samych słów.
5. **Mało tekstu.** Na wykresach nie ma podpisów linii ani zdań: linie
   rozróżnia styl, wartości stoją w jednym krótkim odczycie pod wykresem
   („E(X) = -1.20 zł · Var(X) = 197.56 zł²”). Na scenie najwyżej jedna liczba
   przy obiekcie. Teksty kroków mają 1–3 krótkie zdania. Elementarnych rzeczy nie
   tłumaczymy. Wnioski i liczby z R idą do narracji (`lc_p()`) po scenie.
6. **Dla inżyniera, nie matematyka.** Scena buduje intuicję zastosowania
   i interpretacji, a nie uzasadnia wzoru (bez n − 1, momentów, porównań 1.96
   z t*, wyprowadzeń).
7. **Bez dublowania.** Scena nie powtarza widgetu, który już stoi w wykładzie.
   Jeśli robi to samo lepiej, zastępuje go (statystyka 03: sceny z grupką
   i siatką zastąpiły Ryc. 1.1, 1.2 i 2.1). Jedna historia może iść przez kilka
   rozdziałów, każdy fragment z jedną puentą.
8. **Kolor niesie znaczenie.** Akcent dla zdarzenia lub trafienia, szałwia dla
   „dobrze” lub dopełnienia, ink przerywany dla prawdziwej wartości. Tokeny
   `--upwr-*`, własne kolory tylko dla materiałów (drewno, herbata).
9. **Liczby z kropką dziesiętną**, także w SVG.

## Wskazówki dla form

- **Historia z powtórzeniem.** Prawdziwa wartość (parametr, model) jest ukryta
  do ostatniego kroku. Kroków tyle, ile trzeba; jeśli nazwa liczby jest
  oczywista, pierwszy krok od razu ją pokazuje. Powtórzenia: +10/+100/+1000,
  pierwsze wykonania animowane, seryjne szybko, +1000 bez animacji. Przełącznik,
  który zmienia świat, czyści liczniki.
- **Jeden obraz z „co, jeśli”.** Skrajne ustawienia muszą działać i mieć krótki
  status (|A| = 0 to zdarzenie niemożliwe). Kolejność działań może prowadzić
  tekst (`risk_try()` w analizie ryzyka).
- **Dwa światy.** Oba światy na tej samej osi i w tej samej skali poziomej;
  wysokości mogą mieć osobne skale, gdy liczy się kształt.
- **Kadry.** Nazwy kadrów to najwyżej 3 słowa; każdy kadr dokłada jedną rzecz.

## Implementacje w repo

To narzędzia, nie wymóg. Nową scenę buduje się tym, co pasuje do formy.

- **Silnik scen krokowych** (statystyka 00–03, 05–07): `modules/scenes.js`
  z obiektami `KINDS[kind] = function (cfg, api) { return { render, reset,
  opt, go, many } }`, warstwy SVG `stage` / `low` / `fly`, krok czytany
  z `data-lc-step` widgetu `lc_step_widget()`. Po stronie R `scene_widget()`
  i `scene_texts()` w `modules/helpers.R` danego wykładu; konfiguracja trafia
  do JS jako JSON (`data-config`), żeby scena i tekst liczyły z tych samych
  danych. Wspólne funkcje odczytu: `readout()`, `markDash()`.
- **Eksperyment → rozkład** (statystyka 02): `experiment.js` i `exp_widget()`.
- **Jeden obraz z przełącznikami** (analiza ryzyka 01): `omega.js`,
  `riskmatrix.js`, widget w `figure_panel()` z `lc_toolbar()`, `lc_readouts()`
  i jednym zdaniem statusu (`aria-live`).

Wszystkie respektują `prefers-reduced-motion`, startują przez `scan()`
z `MutationObserver` i działają w przeglądarce bez serwera (R podaje
konfigurację i teksty).

## Brief: „zrób scenę do …”

Przed kodem ustalam i pokazuję do akceptacji:

1. **Puenta** — co student ma zobaczyć i jakie pojęcie nazwie potem tekst.
2. **Sytuacja** — dwie–trzy propozycje ze świata wykładu: kto, co robi, co
   z tego wynika.
3. **Forma** — kadry, obraz z „co, jeśli”, dwa światy, powtórzenie albo bez
   interakcji; z jednym zdaniem, dlaczego ta.
4. **Interakcja** — co student robi (jeśli cokolwiek) i co to zmienia.
5. **Miejsce w rozdziale** — co stoi przed sceną i po niej, którą rycinę
   scena zastępuje albo dlaczego niczego nie dubluje.

Po zbudowaniu: uruchomić wykład na osobnym porcie (nie restartować huba),
przejść scenę w przeglądarce, obejrzeć zrzuty.

## Lekcje z odrzuconych prototypów (2026-10-08)

- **Rzut po passie** (kostka „nie pamięta”) — nie przemawiał; teza jest prosta
  i nie potrzebuje sceny.
- **Przyjdź na przystanek** (pole nad przedziałem) — sztuczna perspektywa.
- **Sto pracowni, przetasuj kartki** (α, moc, p-wartość) — symulacja
  w kostiumie: nieporadne i mało intuicyjne.
- **Porównaj wszystkie pary** — istniejąca Ryc. 9.1 jest prostsza w śledzeniu.
- **Za mała siatka** (1.96 a t*) — techniczny szczegół, nie dla inżynierów.

Prototypy jeszcze do oceny: hala i Welch (statystyka 05), drugie kolokwium
i rozkład b₁ (statystyka 06), sezon w kawiarni (statystyka 07). Wszystkie
prototypy mają w kodzie komentarz `# PROTOTYP SCENY` i panel „Prototyp sceny”.
