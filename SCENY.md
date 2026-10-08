# Sceny — schemat widgetu „od intuicji do formalizmu”

Scena to widget, w którym student sam wykonuje konkretne, codzienne
doświadczenie losowe (dzwoni do kolegów, waży ludzi, losuje paletę), a kolejne
kroki nazywają to, co widać, aż do pojęcia formalnego (p̂, X, ω ∈ A, rozkład,
P(A)). Scena pokazuje proces, który wytwarza liczbę, a nie gotowy wynik.
Formalizm (definicja, wzór, „Zasada”) przychodzi w tekście po scenie i używa
tych samych nazw, które student widział na scenie.

Polecenie „zrób scenę do …” oznacza: zaprojektuj i zbuduj widget według tego
dokumentu, w przedmiocie i wykładzie wskazanym w poleceniu.

## Istniejące sceny (wzorce)

| Wykład | Widget | `kind` | Doświadczenie | Pojęcie docelowe | Kroki |
|---|---|---|---|---|---|
| statystyka 00, Ryc. 1.1 | `ch1_tabela` | `rows` | student podchodzi do stołu, jego cechy lądują w wierszu | obserwacja, zmienna, n | Obserwacja · Zmienna · Rośnie n |
| statystyka 00, Ryc. 2.1 | `ch2_populacja` | `pop` | telefon do kolegów po egzaminie, potem wyniki w USOS | populacja, operat, próba | Populacja · Lista · Próba · Rzeczywistość |
| statystyka 00, rozdz. 3 | `ch3_worek` | `bag` | grupki wychodzą zza drzwi z napisem EGZAMIN | statystyka p̂, parametr p, zmienność próbkowa | Grupka · Statystyka · Powtarzamy · Drzwi |
| statystyka 00, rozdz. 3 | `ch3_latarka` | `spot` | telefon do losowych osób vs ankieta w bibliotece | próba wygodna, obciążenie | Telefon · Biblioteka · Powtarzamy |
| statystyka 00, Ryc. 4.1 | `ch4_herbata` | `tea` | pani rozpoznaje herbatę z mlekiem, zgadujący próbują to samo | rozkład przy samym przypadku, ogon | Pani · Zgadujący · Tłum · Werdykt |
| statystyka 02, rozdz. 3–6 | `ch3_exp`, `ch3_exp_pois`, `ch3_exp_geom`, `ch4_expo`, `ch5_waga`, `ch6_sumdice` | `dice`, `poisson`, `geometric`, `expo`, `weight`, `sumdice` | rzut kostkami, godzina w sklepie, rzuty do szóstki, czekanie na koniec deszczu, ważenie ludzi, suma oczek | zmienna losowa X, rozkład, model (dwumianowy, Poissona, geometryczny, wykładniczy, normalny, CTG) | Eksperyment · Zmienna losowa · Powtarzamy · Rozkład |
| analiza ryzyka 01, ćw. 3 | `jezyk_omega_widget` | (jeden) | losowa kontrola jednej palety na placu Bananpolu | Ω, ω, zdarzenie A, Aᶜ, P(A) = \|A\|/\|Ω\|, ∅ i Ω | bez kroków: plac + odczyty |

Kod:

- statystyka 00: `statystyka/00-dane-i-populacja/modules/scenes.js`, `scenes.css`,
  `scene_widget()` i `scene_texts()` w `modules/helpers.R`;
- statystyka 02: `statystyka/02-rozklady-prawdopodobienstwa/modules/experiment.js`,
  `exp_widget()` i `exp_texts()` w `modules/ch3_dyskretne.R`;
- analiza ryzyka 01: `analiza-ryzyka/01-jezyk-ryzyka/modules/omega.js`,
  `jezyk_omega_widget` w `modules/block.R`, CSS `.lc-om-*` w `app.R`.

## Łuk scenariusza

Kanoniczna scena ma cztery kroki. Każdy krok dokłada jedną warstwę, scena
pod spodem jest ta sama.

1. **Doświadczenie.** Jedno wykonanie, bez symboli i bez wykresu. Student
   klika i widzi, co się dzieje (kostki się toczą, grupka wychodzi zza drzwi,
   osoba wchodzi na wagę). Pod sceną dziennik ostatnich wykonań.
2. **Nazwa.** Z wyniku wyciągamy jedną liczbę albo przynależność i nadajemy
   jej symbol na scenie, przy obiekcie: „X = 2”, „p̂ = 14/25 = 0.56”, „ω = 7”.
   Tu pada termin (zmienna losowa, statystyka, zdarzenie).
3. **Powtarzamy.** Każde wykonanie spada żetonem do histogramu. Pojawiają się
   przyciski +10, +100, +1000 (`lc_step_from(3, …)`). Pierwsze wykonania są
   animowane powoli, seryjne szybko, a +1000 bez animacji.
4. **Ujawnienie / model.** Odkrywamy to, czego nie widać w praktyce: parametr
   (drzwi się otwierają, wyniki wchodzą do USOS) albo model (kółka P(X = k),
   krzywa gęstości). Częstości przechodzą na względne i stają obok modelu.

Warianty:

- **Kontrast.** Ten sam mechanizm z przełącznikiem, który łamie założenie
  formalizmu: biblioteka zamiast telefonu (`spot`), wybór „na oko” zamiast
  losowania (omega). Pokazuje, kiedy wzór przestaje działać, i zwykle
  prowadzi do pułapki lub „Zasady” w tekście.
- **Krótki łuk (3 kroki).** Gdy pojęcie nie ma rozkładu do pokazania
  (`rows`: Obserwacja · Zmienna · Rośnie n).
- **Bez kroków.** Gdy rozdział prowadzi tekst w formacie bloków (analiza
  ryzyka): jeden plac z przełącznikami, odczytami (`lc_readouts`) i jednym
  zdaniem statusu (`aria-live`). Kolejność kroków przejmuje wtedy tekst:
  `risk_try()` mówi, co zrobić po kolei, a po widgecie akapity z wnioskami,
  potem sekcja „Od … do definicji”.

## Zasady treści

Wynikają z kolejnych poprawek scen (kulki → buźki, worek → drzwi, latarka →
telefon i biblioteka, deska Galtona → ważenie ludzi, lineup → herbata):

1. **Doświadczenie z życia, nie urządzenie.** Ludzie, sytuacje i przedmioty,
   które student zna (egzamin, telefon, biblioteka, deszcz, waga, sklep,
   palety w magazynie). Abstrakcyjne aparaty (worek z kulkami, deska Galtona,
   latarka) zastępujemy, gdy da się pokazać to samo na sytuacji.
2. **Jedna historia w wykładzie.** Scena korzysta z danych i świata, które już
   są w wykładzie (wydział i egzamin w statystyce 00, Bananpol w analizie
   ryzyka). Różne przykłady między wykładami statystyki są w porządku.
3. **Ktoś wykonuje akcję.** Postać na scenie (student z telefonem i dymkiem,
   inspektor przy bramie, pani z filiżankami) i przycisk nazwany czasownikiem
   tej akcji: „Wywołaj grupkę”, „Zważ osobę”, „Losuj paletę”, „Zacznij
   deszcz”. Podpis przycisku może zmieniać się z krokiem (`labels`).
4. **Symbole dopiero w kroku 2 i na scenie.** Symbol stoi przy obiekcie, który
   oznacza (ω pod paletą, X nad kostkami), a nie w osobnej legendzie.
5. **Prawda ukryta do końca.** Parametr i model pojawiają się dopiero w ostatnim
   kroku albo jako odczyt obok częstości. Student najpierw widzi zmienność,
   potem to, wokół czego się kręci.
6. **Kolor niesie znaczenie, nie dekorację.** Akcent dla zdarzenia/trafienia,
   szałwia dla dopełnienia lub „dobrze”, ink dla parametru (linia przerywana).
   Tokeny `--upwr-*`; własne kolory tylko dla materiałów (drewno palet,
   herbata, worek).
7. **Przełączniki zmieniają świat i czyszczą liczniki.** n, liczba trafień pani,
   |A|. Skrajności muszą działać i mieć komunikat (|A| = 0 → zdarzenie
   niemożliwe, |A| = |Ω| → pewne).
8. **Tekst kroku mówi, co zrobić i co zauważyć; wnioski idą do narracji.**
   Teksty kroków renderuje R (`scene_texts` / `exp_texts`), 2–4 zdania.
   Wartości liczbowe i morał stoją w `lc_p()` po scenie, liczone w R z tych
   samych danych (np. typowy zakres p̂ dla n = 25 i n = 100). Zob. zasadę
   „skrypt, nie widgety”.
9. **Po scenie formalizm tymi samymi słowami.** Definicja / wzór / `lc_note(
   "Zasada", rule = TRUE)` nazywa to, co było na scenie („Wszystko, co widać na
   placu, ma w rachunku prawdopodobieństwa stałe nazwy”).
10. **Liczby z kropką dziesiętną**, także w SVG (zob. decyzję w TODO.md).
11. **Wykres bez tekstu.** Na wykresach w scenie nie ma podpisów linii, objaśnień
    ani zdań. Linie parametru i modelu rozróżnia styl (przerywana, kropkowana),
    a ich wartości stoją w jednym krótkim odczycie pod wykresem, np.
    „E(X) = -1.20 zł · Var(X) = 197.56 zł²”. Na scenie najwyżej jedna liczba
    przy obiekcie (X = -1 zł), bez rozpisanego rachunku. Elementarnych rzeczy
    nie tłumaczymy.
12. **Scena dla inżyniera, nie matematyka.** Buduje intuicję zastosowania
    i interpretacji (co znaczy wynik, jaką decyzję podjąć), a nie uzasadnia
    wzoru (bez n − 1, momentów, wyprowadzeń). Kroków tyle, ile trzeba: jeśli
    nazwa liczby jest oczywista, krok „Doświadczenie” i „Nazwa” łączymy w jeden.

## Architektura techniczna

Scena działa w całości w przeglądarce; serwer R podaje tylko konfigurację
i teksty kroków.

**R (UI).** `lc_step_widget()` w `figure_panel(label = "Ryc. …", width_mode =
"text")`, z paskiem kroków (nazwy ≤ 3 słowa), `lc_toolbar()`:

- przełączniki opcji: `lc-seg` z przyciskami `data-sc-opt = "nazwa:wartość"`,
  `aria-pressed`, aktywne od kroku k przez `lc_step_from(k, …)`;
- główny przycisk `lc-action is-solid` z `data-sc-act = "go"`, ikoną
  `lc_icon("shuffle")` i `data-labels` (podpisy wg kroku);
- seria `data-sc-act = "m10" | "m100" | "m1000"` od kroku 3;
- ciało: `tags$div(class = "lc-sc", data-config = JSON)`; konfiguracja
  z `jsonlite::toJSON(config, auto_unbox = TRUE, digits = NA)`, w niej `kind`,
  parametry i `aria` (opis sceny dla czytnika ekranu). Dane populacji
  przekazujemy wektorem (np. `z = as.integer(faculty$zdal)`), żeby scena
  i tekst liczyły z tego samego.

W statystyce 00 to wszystko składa `scene_widget(id, title, steps, config,
labels, options, more_from, more)`, a teksty `scene_texts(input, output, id,
list(...))`.

**JS (silnik).** Jeden plik na wykład, IIFE, bez bibliotek:

- `KINDS[kind] = function (cfg, api) { return { render, reset, opt(name, v),
  go(done), many(m, done) } }`;
- `api.stage` (scena), `api.low` (dziennik lub histogram), `api.fly` (żeton
  w locie) to warstwy `<g>` jednego SVG o `viewBox` 640 × H;
  `api.step()` czyta `data-lc-step` z `.lc-stepper`;
- wspólne `init`: delegacja kliknięć na `.lc-stepper`, flaga `busy`
  blokująca przyciski w trakcie animacji, `MutationObserver` na
  `data-lc-step` (zmiana kroku = `render`, bez zmiany stanu), reset na
  `[data-lc-nav="reset"]`;
- animacje przez `tween(ms, step, done)` z `requestAnimationFrame`;
  `prefers-reduced-motion` skraca je do zera;
- `scan()` + `MutationObserver` na dokumencie (widget może pojawić się
  później).

CSS sceny: klasy z prefiksem wykładu (`lc-sc-*`, `lc-exp-*`, `lc-om-*`),
fonty i kolory z tokenów `--upwr-*`, ładowane w `header_extras` aplikacji.

Nowa scena w statystyce 00 to nowy `KINDS.<nazwa>` w `scenes.js`. W innym
wykładzie: nowy plik `modules/<nazwa>.js` na tym samym wzorcu (`init`
z `scenes.js`) i helper R jak `scene_widget()` w `modules/helpers.R`.

## Brief: „zrób scenę do …”

Przed kodem ustalam i pokazuję do akceptacji:

1. **Pojęcie docelowe** — co ma paść w kroku 2 i 4 (symbol, termin, model).
2. **Doświadczenie** — sytuacja z życia lub ze świata wykładu, postać, akcja,
   podpis przycisku. Dwie–trzy propozycje do wyboru.
3. **Liczba z jednego wykonania** — co wyciągamy (X, p̂, ω ∈ A).
4. **Ujawnienie** — parametr albo model, z którym porównujemy częstości.
5. **Kontrast** (opcjonalnie) — przełącznik, który łamie założenie.
6. **Kroki i przełączniki** — nazwy kroków, opcje i od którego kroku działają.
7. **Miejsce w rozdziale** — tekst przed sceną, akapit z wnioskami po niej
   i formalizm (definicja / wzór / Zasada), który z niej korzysta.

Po zbudowaniu: uruchomić aplikację i przejść wszystkie kroki, skrajne
ustawienia przełączników, +1000 i reset.
