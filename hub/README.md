# Hub wykładów

Jeden launcher dla wszystkich przedmiotów. Uruchamiasz go raz na początku zajęć,
resztę klikasz w przeglądarce.

## Uruchamianie

Dwuklik w `Wyklady.command` (Finder) albo z terminala:

```bash
scripts/hub
```

Hub startuje na `http://127.0.0.1:7700` i sam otwiera przeglądarkę.
Inny port: `PORT=8000 scripts/hub`.

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

## Struktura

```text
hub/
├── app.R                # UI, obsługa kliknięć, statusy
└── R/
    ├── discovery.R      # skan repo i metadane wykładów
    ├── processes.R      # start, status, zatrzymywanie procesów
    └── hub_styles.css   # styl kafelków (paleta UPWr)
```

Hub jest infrastrukturą zajęciową, nie wykładem, więc świadomie nie używa
`lecture_page()` ani `DESIGN_CONTRACT.md`. To rozwiązanie lokalne, na zajęcia —
nie zastępuje docelowego portalu na stronie.
