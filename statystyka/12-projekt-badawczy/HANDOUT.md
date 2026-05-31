# Handout: wykład 12-projekt-badawczy

## Kontekst

Wykład `statystyka/12-projekt-badawczy/` to interaktywna aplikacja Shiny prowadząca
studentów przez mock-badanie empiryczne na danych `AER::TeachingRatings` (oceny
ewaluacyjne nauczycieli akademickich, n=463). Celem jest nauka iteracyjnego myślenia
badawczego — statystyka jest narzędziem, nie celem.

## Idea spinająca: jeden cel, wiązka tropów

Cały wykład realizuje **jeden cel badawczy** (zdefiniowany jako `tr_goal` w
`modules/helpers.R`):

> Czy ocena z ankiety (`eval`) mierzy jakość nauczania, czy raczej mieszankę jakości
> zajęć, sympatii, stereotypów i okoliczności kursu?

Tego celu nie da się rozstrzygnąć jedną hipotezą. Realizujemy go **wiązką 5 tropów**
(konkurujących hipotez), które śledzimy przez WSZYSTKIE rozdziały — od ciekawości po
wniosek. Wiązka jest jednym źródłem prawdy: `tr_tropy` + `tr_trop_order` w `helpers.R`.

| Trop | Zmienna | Narzędzie |
|------|---------|-----------|
| Atrakcyjność | `beauty` | korelacja |
| Płeć | `gender` | test t |
| Native speaker | `native` | Mann-Whitney |
| Mniejszość | `minority` | Mann-Whitney |
| Response rate | `response.rate` | korelacja |

Spinający mechanizm wizualny: **narastająca tablica tropów** (`tr_board_ui()`), pusta w
ch1, wypełniana w ch5, pełna w ch6 i ch9. Wyniki liczone raz przy starcie
(`tr_board_summary`).

## Struktura (kolejność = numeracja w UI)

| Poz. | Plik | num | Tytuł |
|------|------|-----|-------|
| 1 | `ch1_ciekawosc.R` | 01 | Od ciekawości do celu (cel + wiązka + pusta tablica) |
| 2 | `ch2_pytanie.R` | 02 | Jak obracać pytanie (4 ramy naraz) |
| 3 | `ch3_hipotezy.R` | 03 | Hipotezy jako tropy (cała wiązka naraz) |
| 4 | `ch4_pomiar.R` | 04 | Co właściwie mierzymy (4 construct mapy naraz) |
| 5 | `ch5_sprawdzenia.R` | 05 | Pierwsze sprawdzenia (wyniki wiązki + tablica + zakłócacze) |
| 6 | `ch6_iteracja.R` | 06 | Wynik nie kończy badania (pełna tablica + co mówi o celu) |
| 7 | `ch_projekt_badania.R` | 07 | Jak zaprojektować lepsze badanie (4 projekty naraz) |
| 8 | `ch8_dodatek_model.R` | 08 | Model kontrolny (cała wiązka w jednym modelu) |
| 9 | `ch7_checklist.R` | 09 | Checklist projektu grupowego (domknięcie + tablica) |

Uwaga: nazwy plików `ch7_*`/`ch8_*` są historyczne. W aplikacji ich pozycja i numer to
09 (checklist) i 08 (model) — kolejność ustawiana w `.chapters` w `app.R`, numery w
`lc_chapter_hero()` i `lecture_chapter(num=...)` każdego modułu.

## Tryb prowadzenia: projekcja

Wykład jest projektowany pod prowadzenie z projekcji (prowadzący omawia, studenci
patrzą). Dlatego selektory nawigacyjne (`selectInput`/`radioButtons` „wybierz 1 z N")
zostały **rozłożone**: w ch2/ch3/ch4/ch5/ch6/ch_projekt cała zawartość jest widoczna
naraz (karty `.trop-card` / `.question-card` jedna pod drugą, tablica `.tropy-board`).
Zero klikania w trakcie narracji.

Interakcja została tylko tam, gdzie klik jest puentą:
- ch1 — podgląd danych (sortowanie, zakres wierszy/kolumn),
- ch8 — budowanie własnego modelu (checkbox kontroli),
- ch9 — autodiagnoza projektu (checklist, 10 pozycji).

## Wzorce UI (kanon — `R/DESIGN_CONTRACT.md`)

Projekt używa shared layout z `statystyka/R/`. **Nie wolno** `fluidPage`, `navbarPage`,
`bslib::page_*`. Kanon: `lc_chapter_hero`, `lc_h2`, `lc_p`, `figure_panel`, `lc_feedback`,
`lc_stat_grid`+`lc_stat_box`, `margin_callout`, `lc_chapter_next`,
`zoom_plot_ui`/`zoom_plot_server`, `lc-table*`. Kolory: `proj_col_*` z `helpers.R`.
Style specyficzne wykładu (`.tropy-board`, `.trop-card`, `.trop-stack`) są w
`header_extras` w `app.R`, na tokenach `--upwr-*`.

## Uruchamianie

```r
shiny::runApp("statystyka/12-projekt-badawczy")
```

## Weryfikacja po zmianach

1. Przejść 9 rozdziałów w kolejności — cel z ch1 wraca w leadzie każdego rozdziału,
   wiązka jedzie jako jedna całość (nie rozłączne menu).
2. Tablica tropów: pusta w ch1 → pełna w ch5/ch6/ch9, spójna z wynikami testów.
3. Numer w hero == pozycja w sidebarze == cel w `lc_chapter_next` dla każdego rozdziału.
4. `Rscript statystyka/scripts/check_design_contract.R` — bez nowych naruszeń.
