# Handout: baza wiedzy analizy ryzyka (graf + warstwa sylabusa)

## Kontekst i cel

Osobiste narzędzie użytkownika do porządkowania wiedzy podczas nauki i przygotowań
do wykładu z analizy ryzyka: graf pojęć, metod i rozkładów budowany plikami
Markdown. Baza grafu znajduje się w podkatalogu projektu wykładu; katalog nadrzędny
zawiera materiały wykładowe użytkownika (plan, notatki, ewentualne slajdy).

Nadrzędne kryterium projektowe: **minimalny koszt dopisania wiedzy**. Każda decyzja,
która wydłuża dodanie węzła ponad ~30 sekund, jest zła, nawet jeśli „czystsza".
Nie generalizuj kodu na zapas — to narzędzie pod jeden konkretny użytek.

## Pliki bazy grafu (podkatalog)

- `ontologia.yaml` — typy węzłów, typy relacji z dziedzinami i acyklicznością, statusy. Źródło prawdy.
- `SZABLON.md` — szablon pliku węzła (frontmatter YAML + treść Markdown).
- `wezly/*.md` — węzły startowe; jeden plik = jeden węzeł.
- `kompiluj.py` — skleja `wezly/` w `graf.json`, waliduje (istnienie celów,
  dziedziny relacji, acykliczność `wymaga`/`uogolnia`). Python 3 + pyyaml.
- `README.md` — konwencje pracy.

## Twarde reguły dla agenta

1. **Nie pisz własnych wyjaśnień merytorycznych w węzłach.** Sekcje „Intuicja"
   i „Pułapki" wypełnia użytkownik w trakcie nauki — na tym polega narzędzie
   (graf ma odwzorowywać jego rozumienie, pole `status` jest tego miarą).
   Dozwolony wyjątek: **import treści z materiałów wykładowych użytkownika**
   (zadanie B) — to jego własne słowa; każdy zaimportowany fragment oznacz
   odnośnikiem do pliku źródłowego. Nie zmieniaj statusów węzłów.
2. Nie zmieniaj istniejących relacji ani ontologii poza zakresem jawnie opisanym
   w zadaniu B.
3. Po każdej zmianie w `wezly/` lub ontologii uruchom `kompiluj.py`; commit tylko
   przy czystej walidacji.
4. Kompilator jest częścią kontraktu: jeśli zmieniasz format, najpierw zmień
   kompilator i szablon, potem dane.

## Zadanie B: warstwa sylabusa + import z materiałów wykładowych

Wykonaj przed zadaniem A (jest tańsze i podnosi wartość wizualizacji).

1. **Inwentaryzacja.** Przejrzyj katalog nadrzędny, zidentyfikuj materiały
   wykładowe (plan wykładów z pytaniami przewodnimi, notatki per wykład).
   Wynik inwentaryzacji — co znalazłeś i jak mapujesz na wykłady 01–10 —
   zapisz na początku raportu z zadania.
2. **Rozszerzenie ontologii** o typ `wyklad` i relacje:
   - `omawia` (dziedzina: `wyklad -> pojecie|rozklad|metoda|problem`),
   - `poprzedza` (dziedzina: `wyklad -> wyklad`, acykliczna).
3. **Węzły wykładów.** Utwórz `wyklad-01` … `wyklad-10` z tytułem i pytaniem
   przewodnim zaimportowanymi z planu. Relacje `omawia` wyprowadź z dwóch źródeł:
   istniejących tagów `wyklad-NN` w węzłach oraz treści materiałów (jeśli notatka
   wykładu omawia pojęcie obecne w bazie). Tagi zostają jako mechanizm autorski.
4. **Import treści.** Jeśli materiały zawierają definicje/przykłady pasujące do
   istniejących węzłów-zalążków, przenieś je do sekcji węzła jako cytowany
   fragment z odnośnikiem (`> ... — źródło: ../sciezka/plik.md`). Nie parafrazuj,
   nie dopisuj od siebie. Pojęcia obecne w materiałach, a nieobecne w bazie,
   dodaj jako zalążki (frontmatter + nagłówek + definicja z materiału).
5. **Kontrola spójności dydaktycznej w kompilatorze.** Dla każdej relacji
   `wymaga` sprawdź, czy pojęcie wymagane jest omawiane w wykładzie
   niepóźniejszym (wg `poprzedza`) niż pojęcie wymagające. Naruszenia raportuj
   jako OSTRZEŻENIA (nie błędy). Pojęcia nieomawiane w żadnym wykładzie —
   osobna sekcja raportu: to lista decyzji dla użytkownika, nie usterka.

## Zadanie A: wizualizacja grafu

Wejście: `graf.json`. Stos: Vite + TypeScript + Cytoscape.js (bez frameworka UI,
bez backendu). Wymagania:

1. Rozróżnienie wizualne typów węzłów oraz **statusów** (zalazek / w-trakcie /
   rozumiem / wymaga-powrotu) — status kolorem wypełnienia lub obrysem;
   graf ma być mapą rozumienia, nie tylko mapą dziedziny.
2. Kierunek relacji strzałkami, typ relacji stylem krawędzi; noty i treść
   metadanych w panelu szczegółów po kliknięciu.
3. Filtry: po typie węzła, po statusie, po wykładzie (relacja `omawia`).
4. Widoki nazwane (przyciski):
   - „Fundamenty pojęcia X" — domknięcie tranzytywne `wymaga` w dół od węzła;
   - „Wykład NN z fundamentami" — węzły omawiane + ich domknięcie `wymaga`;
   - „Czego uczyć się dalej" — węzły `zalazek`/`wymaga-powrotu` będące celem
     `wymaga` z węzłów `w-trakcie`;
   - „Oś rozwoju" — węzły z polem `powstanie` chronologicznie (niski priorytet).
5. Układ warstwowy zgodny z kierunkiem `wymaga` (fundamenty na dole) — graf
   prerekwizytów to DAG, hierarchia jest treścią; nie używaj układu siłowego.
   Filtry ukrywają i pokazują elementy bez przeliczania układu — pozycje
   widocznych węzłów mają pozostawać stabilne.

## Kryteria akceptacji

1. Kompilator po zadaniu B wykrywa sztucznie wprowadzony cykl w `poprzedza`
   i raportuje pojęcia nieomawiane (przetestuj oba przypadki, potem cofnij).
2. Widok „Czego uczyć się dalej" na danych startowych zwraca niepustą, poprawną
   listę (w bazie jest węzeł `w-trakcie` z zależnościami).
3. „Fundamenty pojęcia" dla `fta` prowadzi aż do `zdarzenie-losowe`
   i `przestrzen-zdarzen`.
4. Filtrowanie po statusie nie zmienia pozycji pozostałych węzłów.
5. Każdy fragment merytoryczny dodany do węzłów w zadaniu B ma odnośnik do pliku
   źródłowego w materiałach użytkownika; poza importem żaden węzeł nie zyskał
   treści napisanej przez agenta.

## Pytania otwarte (podejmij rozsądne decyzje, odnotuj w README)

- Czy porządek wykładów wynika wyłącznie z `poprzedza`, czy też z pola
  liczbowego? (Sugestia: tylko `poprzedza` + numer w id; bez dat.)
- Format raportu ostrzeżeń: stdout wystarczy, czy plik `raport.md`?
  (Sugestia: stdout + opcja `--raport raport.md`.)
- Jeśli materiały wykładowe mają strukturę inną niż 10 wykładów z tabeli planu,
  zmapuj najbliżej jak się da i opisz rozbieżności w raporcie zamiast zgadywać.
