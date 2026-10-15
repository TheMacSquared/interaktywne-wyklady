# Statystyka — wersja offline

Statyczny zrzut wykładów z `statystyka/` do plików HTML, które działają po pobraniu, bez R i bez instalowania czegokolwiek. Przeznaczony dla studentów zaliczających kurs zdalnie.

## Co jest w środku

- `wyklady/` — gotowy materiał do spakowania i wysłania:
  - `index.html` — spis wykładów,
  - `NN-nazwa.html` — jeden plik na wykład (wykresy wbudowane jako obrazki),
  - `fonts/` — kroje IBM Plex używane przez strony.
- `build/` — skrypt generujący zrzut.

Student rozpakowuje archiwum i otwiera `index.html` w przeglądarce. Internet nie jest potrzebny; wyjątkiem są osadzone filmy z YouTube.

## Co działa, a co nie

| Element | Wersja offline |
|---|---|
| Tekst, wzory, wykresy, tabele | tak (wykresy w stanie domyślnym) |
| Nawigacja po rozdziałach, spis sekcji, rozmiar tekstu, tryb | tak |
| Sceny (`scenes.js`) i rozwijane bloki | tak, interaktywne |
| Suwaki, przełączniki, przyciski liczone w R | wyszarzone, zamrożone w stanie domyślnym |
| Ściągi, quizy, ćwiczenia, koło nagród (09) | pominięte (zadania są w osobnych listach) |

## Generowanie

Z katalogu głównego repozytorium:

```sh
Rscript statystyka-offline/build/build_offline.R
```

Wybrane wykłady:

```sh
Rscript statystyka-offline/build/build_offline.R --only=01,04
```

Skrypt uruchamia każdy wykład lokalnie (porty od 7951, zajęte omija), przechodzi rozdziały w przeglądarce bez okna i zapisuje wynik w `wyklady/`. Opcja `--src=DIR` pozwala wygenerować zrzut z kopii katalogu `statystyka/`, np. zamrożonej na czas pracy nad wykładem.

Wymagania: pakiety `chromote`, `processx`, `jsonlite` i zależności samych wykładów, a także przeglądarka oparta na Chromium. Jeśli `chromote` jej nie znajduje, wskaż ją zmienną `CHROMOTE_CHROME`, np. Chromium z Playwright:

```sh
export CHROMOTE_CHROME=~/.cache/ms-playwright/chromium-1234/chrome-linux64/chrome
```

## Pliki w `build/`

- `build_offline.R` — orkiestracja: aplikacja, przeglądarka, składanie pliku HTML.
- `capture.js` — wstrzykiwany do działającej aplikacji: przełącza rozdziały, czeka na wykresy, zamienia canvas na obrazki, zamraża kontrolki, przełącza MathJax na SVG.
- `runtime.js` — dołączany do pliku offline: zastępuje Shiny przy nawigacji między rozdziałami.
- `offline.css` — style wersji offline (ukryte rozdziały, wyszarzone kontrolki, notka w sidebarze).

`wyklady/` i archiwum zip nie są w repozytorium (`.gitignore`); generuje się je lokalnie. Archiwum do wysłania:

```sh
cd statystyka-offline/wyklady && zip -qr ../statystyka-wyklady-offline.zip .
```
