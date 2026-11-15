# Interaktywne wykłady

Interaktywne wykłady do nauczania przedmiotów ilościowych: statystyki i analizy ryzyka. Każdy wykład to jedna strona HTML z rozdziałami, rycinami, suwakami, quizami i ćwiczeniami. Strona działa bez serwera R i bez internetu — wystarczy ją otworzyć w przeglądarce. Wszystko, co zależy od ruchu studenta, liczy przeglądarka; resztę (dane, modele, wzory) liczy R przy budowaniu strony.

## Struktura

```text
interaktywne-wyklady/
├── shared/         # wspólny rdzeń: layout, paleta, style, silnik rycin (JS)
├── statystyka/     # podstawowy kurs statystyki (11 wykładów)
├── analiza-ryzyka/ # wykłady z analizy ryzyka (10 wykładów)
├── scripts/        # budowanie i kontrola stron statycznych
└── audyty/         # ustalenia audytu i stan poprawek
```

| Folder | Wykłady | Dokumentacja |
|--------|---------|--------------|
| `statystyka/` | 00–10 | [statystyka/README.md](statystyka/README.md) |
| `analiza-ryzyka/` | 01–10 | [analiza-ryzyka/README.md](analiza-ryzyka/README.md) |

Opis rdzenia i silnika rycin: [shared/README.md](shared/README.md). Reguły designu: [shared/DESIGN_CONTRACT.md](shared/DESIGN_CONTRACT.md).

## Budowanie stron

Z katalogu głównego repozytorium:

```sh
Rscript scripts/build_static.R statystyka/01-typy-danych   # jeden wykład → dist/statystyka/01-typy-danych.html
Rscript scripts/build_index.R                              # spis: dist/index.html i dist/<kurs>/index.html
```

Każdy plik w `dist/` jest samodzielny (style, skrypty, kroje i wzory w środku) i działa także otwarty z dysku. Paczkę offline dla studentów składa `przedmioty/<przedmiot>/skrypt/build/build_offline.R` w repozytorium `dydaktyka`.

## Publikacja (GitHub Pages)

Strony są publikowane z gałęzi `gh-pages` pod adresem <https://themacsquared.github.io/interaktywne-wyklady/>:

```sh
scripts/publish_pages.sh            # publikuje zbudowane dist/
scripts/publish_pages.sh --build    # najpierw buduje wszystkie wykłady i spis
```

Każda strona ma `<meta name="robots" content="noindex, nofollow">`: nie trafia do wyszukiwarek, ale jest dostępna dla każdego, kto zna adres (GitHub Pages nie ma kontroli dostępu poza GitHub Enterprise Cloud). Jednorazowo w ustawieniach repozytorium: Settings → Pages → Deploy from a branch → `gh-pages`, folder `/ (root)`.

Do składania wzorów przy budowaniu potrzebna jest przeglądarka oparta na Chromium (zmienna `CHROMOTE_CHROME`, jeśli `chromote` jej nie znajduje); szczegóły w [shared/README.md](shared/README.md).

## Kontrole

```sh
Rscript scripts/check_static_ready.R              # czy kod wykładów nie zależy od serwera
Rscript scripts/check_static_page.R dist/statystyka/01-typy-danych.html   # strona w przeglądarce
Rscript statystyka/tests/testthat.R
Rscript analiza-ryzyka/tests/testthat.R
node --test statystyka/tests/js/*.test.js analiza-ryzyka/tests/js/*.test.js
Rscript analiza-ryzyka/scripts/check_design_contract.R --strict
```

## Historia

Do listopada 2026 wykłady były aplikacjami R Shiny uruchamianymi przez hub; repozytorium zawierało też kurs „Statystyka 2” i archiwum ekonometrii. Ostatnią wersję z Shiny oznacza tag `shiny-final`.
