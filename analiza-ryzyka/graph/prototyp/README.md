# Prototyp archipelagu grafów wiedzy

Minimalny eksperyment sprawdzający model: osobne grafy są źródłami prawdy,
a wspólne pozostają format, walidator i lekki indeks.

## Co zawiera prototyp

- `ontologia-bazowa.yaml` — mały, wspólny słownik typów i relacji;
- `indeks.yaml` — katalog niezależnych grafów;
- `grafy/kurs-analiza-ryzyka/` — pierwszy graf problemowy;
- `narzedzia/graph_core.R` — wczytywanie, walidacja i kompilacja;
- `kompiluj.R` — interfejs wiersza poleceń;
- `testy/test_core.R` — testy najważniejszych reguł.

Prototyp zawiera lekką przeglądarkę pierwszego grafu, ale celowo nie zawiera
mostów między grafami ani globalnego grafu pojęć.

## Stan pierwszego grafu

Graf kursu zawiera obecnie 42 węzły i 80 relacji:

- pojęcia i metody przeniesione z pierwotnego szkicu;
- dziesięć wykładów połączonych relacją `poprzedza`;
- dziesięć pytań przewodnich;
- trzy jawne twierdzenia projektowe;
- dwa lokalne źródła.

Bieżący raport pozostawia świadomie dwie decyzje: pytanie końcowe o wybór bariery
nie ma jeszcze metody odpowiadającej, a twierdzenie Bayesa nie ma przypisanego
źródła. Wszystkie pozostałe treści są omawiane w co najmniej jednym wykładzie;
nie ma węzłów odłączonych.

## Wymagania

- R >= 4.1;
- pakiety `yaml` i `jsonlite`;
- do testów: `testthat`.

## Użycie

Z katalogu `prototyp/`:

```sh
Rscript kompiluj.R
Rscript kompiluj.R kurs-analiza-ryzyka
Rscript testy/test_core.R
```

Pierwsze polecenie kompiluje wszystkie grafy z indeksu. Drugie kompiluje tylko
wskazany graf. Wynik powstaje jako `graf.json` wewnątrz katalogu danego grafu.

Kompilator raportuje również decyzje wymagające uwagi autora: treści nieomawiane
w żadnym wykładzie, pytania bez odpowiedzi, twierdzenia bez źródeł oraz węzły
odłączone. Nie są to automatycznie błędy — widoczna luka może być wartościowym
wynikiem pracy z grafem.

## Przeglądarka

Najpierw skompiluj graf, a następnie uruchom prosty lokalny serwer:

```sh
Rscript kompiluj.R
python3 -m http.server 8000
```

Otwórz `http://localhost:8000/przegladarka/`. Widok ma trzy tryby:

- `Prerekwizyty` — hierarchia relacji `wymaga` i `uogolnia`;
- `Sylabus` — wykłady oraz omawiane w nich treści;
- `Pytania i źródła` — odpowiedzi, twierdzenia i ich uzasadnienia.

Kliknięcie węzła pokazuje treść oraz wszystkie jego relacje. Filtr wykładu
ogranicza widok do jego treści i wymaganych fundamentów.

## Reguły eksperymentu

1. Granicą grafu jest projekt lub pytanie, nie cała dziedzina.
2. Graf pozostaje samodzielny i można go kompilować bez innych grafów.
3. Ontologia lokalna może dodawać typy i relacje, ale nie redefiniuje bazowych.
4. Duplikacja pojęć pomiędzy grafami jest dozwolona.
5. Integrację projektujemy dopiero po powstaniu drugiego rzeczywistego grafu.
