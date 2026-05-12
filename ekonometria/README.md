# Interaktywne wykłady z ekonometrii

Ten katalog zawiera równoległy zestaw aplikacji R Shiny do nauczania ekonometrii. Każdy katalog numerowany (`01-*`, `02-*`, …) jest osobnym wykładem i osobną aplikacją Shiny — żeby pojedynczy proces nie wczytywał całego kursu naraz.

Framework, layout i komponenty UI są skopiowane z `statystyka/R/`, z osobną konfiguracją nawigacji dla ekonometrii.

## Struktura

```text
ekonometria/
├── README.md
├── R/                              # wspólne komponenty i style dla wszystkich wykładów
│   ├── econometrics_helpers.R      # eco_fmt, eco_regression_data, eco_diagnostic_data, eco_ts_data, eco_lp_vertices
│   ├── lecture_layout.R            # lc_*, figure_panel, inline_callout, lc_chapter_hero/next itd.
│   ├── palette.R, theme_upwr.R     # paleta i motyw UPWr
│   ├── shared.R, shared_styles.css, shared_toc.js
├── ekonometria-pd000000peks-l4-0561-24.pdf   # syllabus
├── 01-model-ekonometryczny/
├── 02-regresja-liniowa-kmnk/
├── 03-estymatory-bledy-standardowe/
├── 04-weryfikacja-modelu/
├── 05-szeregi-prognozowanie/
├── 06-optymalizacja-liniowa/
└── 07-simpleks-dualizm/
```

Każdy wykład ma identyczną strukturę:

```text
NN-tytul/
├── app.R                # bootstrap + lecture_page() + serwery
└── modules/
    ├── ch1_*.R          # rozdział 1 (UI + server)
    ├── ch2_*.R          # rozdział 2
    ├── ch3_*.R          # rozdział 3
    └── ch4_cwiczenie.R  # rozdział 4 — ćwiczenie (quiz / interpretacja tabeli)
```

## Uruchamianie wykładu

Z katalogu głównego repozytorium:

```r
shiny::runApp("ekonometria/01-model-ekonometryczny")
shiny::runApp("ekonometria/02-regresja-liniowa-kmnk")
shiny::runApp("ekonometria/03-estymatory-bledy-standardowe")
shiny::runApp("ekonometria/04-weryfikacja-modelu")
shiny::runApp("ekonometria/05-szeregi-prognozowanie")
shiny::runApp("ekonometria/06-optymalizacja-liniowa")
shiny::runApp("ekonometria/07-simpleks-dualizm")
```

Wymagane pakiety: `shiny`, `ggplot2`, `dplyr`, `broom`. Opcjonalnie `patchwork` (wykorzystywany w niektórych dwupanelowych wykresach diagnostycznych — jeśli niezainstalowany, kod ma fallback do jednego panelu).

## Zakres tematyczny (mapowanie na syllabus PD000000PEKS.L4.0561.24)

| Wykład | Temat | Punkty syllabusa |
|--------|-------|------------------|
| 01 | Model ekonometryczny — pojęcie, klasyfikacja zmiennych, składnik losowy, postępowanie ekonometryczne | 1 |
| 02 | Regresja liniowa z jedną zmienną i założenia KMNK | 2 |
| 03 | Estymatory parametrów strukturalnych i błędy standardowe w KMNK | 3, 4 |
| 04 | Weryfikacja merytoryczna i statystyczna; R², SE reszt; istotność zmiennej | 5, 6 |
| 05 | Szeregi czasowe, modele dynamiczne, prognoza punktowa i przedziałowa, miary ex ante / ex post | 7, 8 |
| 06 | Optymalizacja liniowa — problem decyzyjny, postać klasyczna i standardowa, metoda graficzna | 9, 10, 11 |
| 07 | Metoda simpleks i dualizm programowania liniowego | 12, 13 |

## Wzorzec dydaktyczny (per rozdział)

Każdy z czterech rozdziałów wykładu trzyma się następującej sekwencji komponentów:

1. **`lc_chapter_hero`** — kicker (numer wykładu) + tytuł + jednolinijkowy lead-haczyk z konkretnym przykładem.
2. **„Po co?”** — paragraf z mini-historyjką ekonomiczną (piekarnia, sklep z lodami, gospodarstwo rolne, doradca HR, stolarz).
3. **Formuła** w `lc_formula_box` z opisem oznaczeń pod nią.
4. **`figure_panel`** z widgetem (slidery + plot + werdykt słowny).
5. **Werdykt** w `lc_feedback` lub `lc_stat_grid` — interpretacja „jak w raporcie”, nie surowa liczba.
6. **`inline_callout`** — Wskazówka / Pułapka / Zapamiętaj.
7. **`lc_chapter_next`** — most do następnego rozdziału.

Czwarty rozdział każdego wykładu jest **ćwiczeniowy** — typowo zawiera tabelę wyników (jakby z konsoli statystycznej) i serię pytań `radioButtons` z dynamicznymi werdyktami.

## Konwencje kodowania

- UI po polsku, kod po angielsku (nazwy funkcji, zmiennych, identyfikatorów).
- Polskie znaki natywnie UTF-8 (bez `\uXXXX`).
- Identyfikatory inputów Shiny **prefiksowane per rozdział** (`ch1_*`, `ch2_*`, `ch3_*`, `ch4_*`) — żeby moduły jednego wykładu nie kolidowały ze sobą.
- Helpery danych syntetycznych (`eco_*`) leżą w `R/econometrics_helpers.R` i są reużywane między wykładami.
