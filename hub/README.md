# Hub wykładów

Jeden launcher dla wszystkich przedmiotów. Uruchamiasz go raz na początku zajęć,
resztę klikasz w przeglądarce.

## Uruchamianie

Dwuklik w `Wyklady.bat` (Windows) lub `Wyklady.command` (macOS, Finder)
albo z terminala:

```bash
scripts/hub
```

Hub startuje na `http://127.0.0.1:7700` i sam otwiera przeglądarkę.
Inny port: `PORT=8000 scripts/hub`.

Pliki do dwukliku uruchamiają `hub/start.R`, który przed startem:

- sprawdza wersję R (≥ 4.1) i pakiety huba (`shiny`, `processx`) — bez nich kończy
  z gotowym poleceniem `install.packages(...)`
- zbiera pakiety wykładów parserem R z `app.R`, `modules/` i `<przedmiot>/R/`
  (bez uruchamiania kodu; pakiety za `requireNamespace()` mają fallback i są pomijane),
  wypisuje, których wykładów dotyczą braki, i pyta, czy je zainstalować; odmowa
  nie blokuje huba — nie zadziałają tylko wykłady z brakami
- gdy hub już działa na tym porcie, tylko otwiera go w przeglądarce

`Wyklady.bat` szuka R w `PATH`, w rejestrze (`HKLM`/`HKCU\SOFTWARE\R-core\R`)
i w `Program Files\R` / `%LOCALAPPDATA%\Programs\R`. Plik musi mieć końce linii
CRLF — pilnuje tego `.gitattributes`.

## Jak to działa na zajęciach

Kliknięcie kafelka uruchamia wykład jako **osobny proces R** na własnym porcie
(7701 w górę) i otwiera go w nowej karcie. Raz uruchomiony wykład zostaje żywy,
więc powrót do niego — kliknięciem w hubie albo przełączeniem karty — pokazuje go
w tym samym stanie: suwaki, quizy i zebrane próby zostają na miejscu.

- **lista przedmiotów** w nagłówku zawęża spis do jednego przedmiotu; wybór jest
  zapamiętywany, więc po odświeżeniu strony zostaje ten sam
- **Zatrzymaj** na kafelku — ubija proces jednego wykładu (zwalnia pamięć)
- **Zatrzymaj wszystkie** — sprząta wszystko bez zamykania huba
- logo w nagłówku wykładu wraca do spisu (tylko gdy wykład uruchomił hub)

Zamknięcie huba (Ctrl+C lub zamknięcie okna Terminala) zatrzymuje wszystkie
wykłady naraz — nie zostają procesy trzymające porty.

## Dodanie nowego wykładu

Nic nie trzeba robić. Hub skanuje repo przy starcie i po kliknięciu
„Odśwież listę": wykładem jest każdy katalog `<przedmiot>/<wyklad>/app.R`
w przedmiocie mającym własne `R/lecture_layout.R`. Tytuł, numer i moduł hub
czyta z wywołania `lecture_page()`, a liczbę rozdziałów z `.chapters` — oba
przez parser R, nie przez uruchamianie kodu.

## Gdy coś nie działa

| Objaw | Przyczyna | Co zrobić |
|-------|-----------|-----------|
| „Przeglądarka zablokowała nową kartę" | blokada wyskakujących okien | zezwól na okna dla `127.0.0.1` albo kliknij link z komunikatu |
| „Nie udało się uruchomić wykładu" | błąd w kodzie wykładu | komunikat zawiera koniec logu; pełny log jest w `tempdir()` jako `hub-<wyklad>.log` |
| port zajęty | został proces po poprzedniej sesji | `pkill -f "shiny::runApp"` |
| brak pakietu `processx` | jedyna zależność huba | `install.packages("processx")` |
| „Nie znaleziono R” (Windows) | R nie jest zainstalowane | zainstaluj R z <https://cran.r-project.org> i uruchom plik ponownie |
| „System Windows ochronił komputer” | SmartScreen przy pierwszym uruchomieniu | „Więcej informacji” → „Uruchom mimo to” |

## Struktura

```text
hub/
├── app.R                # UI, obsługa kliknięć, statusy
├── start.R              # start z Wyklady.bat / .command: R, pakiety, drugi start
└── R/
    ├── discovery.R      # skan repo i metadane wykładów
    ├── processes.R      # start, status, zatrzymywanie procesów
    └── hub_styles.css   # styl kafelków (paleta UPWr)
```

Hub jest infrastrukturą zajęciową, nie wykładem, więc świadomie nie używa
`lecture_page()` ani `DESIGN_CONTRACT.md`. To rozwiązanie lokalne, na zajęcia —
nie zastępuje docelowego portalu na stronie.


## Aktualny zakres

Hub pokazuje trzy aktywne przedmioty: Statystykę (9 aplikacji), Statystykę 2 (4) i analizę ryzyka (10). Wykłady w `deprecated/ekonometria/` pozostają archiwum i nie pojawiają się na liście, ponieważ skan obejmuje tylko poziom `<przedmiot>/<wykład>/app.R`.

Tekst interfejsu ma bazowo 18 px, a tytuły kafelków 1,2 rem (około 22 px). Siatka dopasowuje liczbę kolumn do szerokości okna.
