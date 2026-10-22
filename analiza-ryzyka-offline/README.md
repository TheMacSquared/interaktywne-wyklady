# Analiza ryzyka — wersja offline

Statyczny zrzut wykładów z `analiza-ryzyka/` do plików HTML, które działają po pobraniu, bez R i bez instalowania czegokolwiek.

## Co jest w środku

- `wyklady/` — gotowy materiał do spakowania i wysłania:
  - `index.html` — spis wykładów,
  - `NN-nazwa.html` — jeden plik na wykład (wykresy wbudowane jako obrazki),
  - `fonts/` — kroje IBM Plex używane przez strony.

Student rozpakowuje archiwum i otwiera `index.html` w przeglądarce.

Zakres (co działa, a co jest zamrożone lub pominięte) jest taki sam jak w [wersji offline Statystyki](../statystyka-offline/README.md#co-działa-a-co-nie).

## Generowanie

Generator jest wspólny i leży w `statystyka-offline/build/`. Z katalogu głównego repozytorium:

```sh
Rscript statystyka-offline/build/build_offline.R --course=analiza-ryzyka
Rscript statystyka-offline/build/build_offline.R --course=analiza-ryzyka --only=01,04
```

Bez Google Chrome wskaż inną przeglądarkę opartą na Chromium, np. Brave:

```sh
export CHROMOTE_CHROME="/Applications/Brave Browser.app/Contents/MacOS/Brave Browser"
```

`wyklady/` i archiwum zip nie są w repozytorium (`.gitignore`). Archiwum do wysłania:

```sh
cd analiza-ryzyka-offline/wyklady && zip -qr ../analiza-ryzyka-wyklady-offline.zip .
```
