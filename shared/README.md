# Wspólny rdzeń wykładów

Jeden system layoutu, stylów i bibliotek dla kursów `statystyka/` i
`analiza-ryzyka/`. Zmianę robi się tutaj raz i obowiązuje w obu kursach.

## Zawartość

| Plik | Rola |
|---|---|
| `palette.R` | paleta UPWr (role, kolory kategoryczne, skale) |
| `theme_upwr.R` | motyw ggplot2 |
| `shared.R` | kroje wykresów, generatory danych |
| `lecture_layout.R` | `lecture_page()` i komponenty `lc_*` |
| `lc_stat_figures.R`, `lc_stat_figures.js` | ryciny liczone w przeglądarce (`window.lcSF`) |
| `lc_charts.js` | wspólna obsługa wykresów ECharts (`window.lcCharts`) |
| `lc_dist.js` | rozkłady i losowanie (`window.lcDist`) |
| `lc_widgets.js`, `lc_dragdrop.js`, `lc_formula.js`, `shared_toc.js` | kontrolki v2, przeciąganie kart, wzór ze znacznikami, spis sekcji |
| `lc_quiz.js` | quiz kafelkowy `lc_tile_quiz()`: losowanie pytań, ocena, wynik (`window.lcTileQuiz`) |
| `shared_styles.css` | style powłoki i komponentów |
| `season.js` | kalendarz trybów sezonowych: `lcSeason()`, `lcDomyslnyTryb()` |
| `winter.css`, `winter.js`, `winter_peek.png` | tryb zimowy (1.12–6.01): tokeny `--upwr-*`, wzorek tła, ozdoby; żart z postacią wychylającą się zza krawędzi (bez pliku PNG wyłączony) |
| `halloween.css`, `halloween.js` | tryb halloweenowy (wyłączony) |
| `fonts/` | IBM Plex Sans i Mono (wagi 400–700): wykresy R i strony statyczne |
| `DESIGN_CONTRACT.md`, `LECTURE_LAYOUT.md` | kontrakt designu i opis komponentów |

## Co zostaje w kursie

Katalog `<kurs>/R/` zawiera tylko pliki własne kursu:

- `course.R` — lista `.LC_COURSE`: nazwa i znak w logo, moduły górnego paska,
  mapowanie `lecture_id → moduł` oraz przełączniki zachowania (niżej). Po tym
  pliku `.find_app_dir()` rozpoznaje katalog kursu.
- `course_styles.css` (opcjonalnie, wymieniony w `.LC_COURSE$styles`) — arkusz
  ładowany po `shared_styles.css`: elementy własne kursu i świadome odstępstwa
  od rdzenia.
- pliki przedmiotowe, np. `statystyka/R/ankieta.R`, `statystyka/R/glossary.R`,
  `analiza-ryzyka/R/risk_block.R`.

`app.R` wykładu ładuje najpierw `course.R`, potem pliki rdzenia:

```r
app_dir <- .find_app_dir()
project_root <- dirname(app_dir)
shared_root <- file.path(dirname(project_root), "shared")

source(file.path(project_root, "R", "course.R"), local = TRUE)

source(file.path(shared_root, "palette.R"),        local = TRUE)
source(file.path(shared_root, "theme_upwr.R"),     local = TRUE)
source(file.path(shared_root, "shared.R"),         local = TRUE)
source(file.path(shared_root, "lecture_layout.R"), local = TRUE)
```

## Przełączniki kursu

Rdzeń powstał ze scalenia dwóch kopii (listopad 2026). Tam, gdzie kursy
zachowywały się inaczej, różnica jest jawnym polem w `.LC_COURSE`, a nie
osobną kopią pliku:

| Pole | Statystyka | Analiza ryzyka | Znaczenie |
|---|---|---|---|
| `chapter_mode` | `"single"` | `"all"` | rozdział wstawiany do strony przy wejściu albo wszystkie rozdziały zamontowane od razu (nieaktywne ukryte) |
| `plot_scaling` | `FALSE` | `TRUE` | baza tekstu `theme_upwr()`: 14 pt albo 11 pt |
| `styles` | brak | `course_styles.css` | arkusze kursu ładowane po rdzeniu |

`chapter_mode` nie ma już znaczenia dla stron statycznych (wszystkie rozdziały są
w dokumencie); zostaje w `course.R` dla zgodności.

## Strony statyczne

`lecture_page()` składa stronę statyczną: wszystkie rozdziały są w dokumencie,
a przełącza je `shared_toc.js`; strona nie ładuje Shiny. Wykłady nie mają serwera.

| Skrypt | Rola |
|---|---|
| `scripts/check_static_ready.R` | licznik zależności wykładów od serwera (wykresy, `renderUI`, reakcje, wejścia); wykład z samymi zerami można budować statycznie |
| `scripts/build_static.R` | buduje `dist/<kurs>/<wykład>.html`: style i skrypty wbudowane w plik, biblioteki z sieci pobrane raz do `dist/.cache/`, kroje IBM Plex wbudowane w plik, wzory złożone jako SVG |
| `scripts/check_static_page.R` | otwiera zbudowaną stronę w Chromium bez okna: błędy konsoli, żądania sieciowe, nawigacja, suwaki, quiz, niezłożony TeX, szerokość 390 px |

Co działa bez serwera we wspólnych komponentach:

- nawigacja rozdziałów, spis sekcji, „Dalej” (`lcGoToChapter()` w `shared_toc.js`);
- suwak `lc_slider()` (ionRangeSlider uruchamiany w `lc_widgets.js`);
- karty odpowiedzi z kluczem i quiz z omówieniem (`lc_widgets.js`, sekcja „Quiz
  z kluczem”; w analizie ryzyka dane pytań wstawia `risk_quiz_data()`);
- treść wybierana kontrolką: `lc_variants()` trzyma wszystkie warianty
  w dokumencie i pokazuje ten, który odpowiada wartości radia, segmentu albo
  listy (opcjonalnie dopiero po kliknięciu przycisku); zastępuje `uiOutput()`
  z `renderUI()` zależnym od `input$…`;
- komentarz do wybranej odpowiedzi: `lc_choice_feedback()` z przyciskiem
  `lc_check_button()` (w analizie ryzyka także `risk_vote_panel(feedback = )`);
- treść odsłaniana przyciskiem, np. tok rozwiązania: `lc_reveal()`;
- opisy kroków widgetu krokowego: `lc_step_widget(texts = )`;
- quiz losujący pytania z puli: `lc_tile_quiz()` (także ze stałymi odpowiedziami
  w kolorach — `options`, z częścią odpowiedzi w pytaniu — `option_count`,
  i z treścią po odpowiedzi, np. wykresem — `extra`);
- gotowa treść z miejscami po dawnych wyjściach serwera: `lc_static_fill()`
  (np. rozwiązania ćwiczeń w wariantach kierunków);
- treść wybierana kliknięciem (`lc_variants(pick = TRUE)` + atrybuty `lc_pick()`),
  przełączana przyciskiem (`lc_toggle_button()` + `lc_toggle_content()`) albo
  krokiem widgetu (`lc_step_views()`);
- `conditionalPanel()` z warunkami na `input.…` (ocenia je `lc_widgets.js`);
- obrazki z katalogów `addResourcePath()` (build wbudowuje je w plik);
- ryciny `lcSF`, wykresy `lcCharts`, wzór ze znacznikami.

Skrypty strony statycznej są na końcu dokumentu (w nagłówku zostają tylko
oznaczone `data-lc-early`), a treść rozdziałów czeka w `<template>` i trafia
do dokumentu jednym ruchem po ich wczytaniu — tak jak w Shiny, gdzie rozdział
wstawia `renderUI`. Widgety nie zależą więc od kolejności skryptów.

Narzędzia pomocnicze migracji są w `scripts/migracja/`: porównanie paneli
strony statycznej z wersją Shiny (`porownaj_panele.R`), uruchomienie skryptu
sprawdzającego na działającej aplikacji (`z_shiny.R`), zrzuty i JS na stronie
(`zrzut.R`, `js_na_stronie.R`), wszystkie kontrole naraz (`wszystkie_kontrole.sh`).

Wzory wpisane w treść składa MathJax przy budowaniu. Tekst, który skrypt
wykładu tworzy w przeglądarce, nie może zawierać TeX-a: na stronie statycznej
MathJax nie jest ładowany (`check_static_page.R` wykrywa niezłożony zapis).

## Testy

Testy rdzenia są na razie w obu kursach (`<kurs>/tests/`, m.in.
`test-widgets-v2.R` i `test-panel-layout.R` w dwóch niemal identycznych kopiach).

## Testy statystyczne w przeglądarce

`lc_stats.js` (`window.lcStats`, ładować po `lc_dist.js`) liczy to, co dawniej
liczył serwer R: statystyki opisowe, kwantyle (typ 7), punkty i prostą wykresu
Q-Q, test Shapiro–Wilka (algorytm AS R94, jak `shapiro.test()`), test Levene'a
(środek: mediana, jak `rstatix::levene_test()`), test Bartletta, test t Studenta
i Welcha, χ² bez poprawki i test dokładny Fishera dla tabeli 2×2, dystrybuantę
rozkładu F oraz regresję liniową `lm(columns, y)` (rozkład QR; współczynniki,
błędy standardowe, p, 95% CI, R², skorygowany R², AIC). `fmtPValue()` odpowiada `format_p_value()`.

Wzorcem są wyniki R: `statystyka/tests/js/lc_stats.test.js` porównuje funkcje
z plikiem `statystyka/tests/js/fixtures/lc_stats.json`, który generuje
`Rscript statystyka/tests/js/fixtures/make_lc_stats.R`.

Rycina `lcSF` może nie mieć wykresu (same odczyty, wynik testu albo rysunek
z R z liczbami w `lc_sf_value()`): `lc_sf_figure(chartless = TRUE)` albo
kontener z atrybutem `data-lc-nochart` i figura z samym `update()`. Rycinę
zależną od kontrolek spoza własnego kontenera skrypt wykładu odświeża
zdarzeniem `lc:sf-redraw`. `lcSF.register()` zgłasza w konsoli nazwę ryciny
zarejestrowaną dwa razy.

## Rycina „opis z R”

Wykres na stałych danych nie potrzebuje własnego kodu JS. R składa przy
budowaniu opis rysunku (zakresy osi, warstwy, podpisy), a rysuje go wbudowana
rycina `lc_spec`:

```r
lc_sf_spec(sf_spec(sf_range(d$x), sf_range(d$y),
  c(list(sf_points(d$x, d$y, color = "@secondary", alpha = 0.4)),
    sf_smooth_lm(d$x, d$y, color = "@szalwia")),
  x_name = "Oś X", y_name = "Oś Y", aria = "Opis wykresu"))
```

Warstwy: `sf_points()`, `sf_line()`, `sf_vline()`, `sf_hline()`, `sf_bars()`,
`sf_cols()`, `sf_hist()`, `sf_boxes()` (statystyki: `sf_box_stats()`),
`sf_segs()`, `sf_errorbars_h()`, `sf_errorbars_v()`, `sf_text()`, `sf_texts()`,
`sf_polygon()`, `sf_ribbon()`, `sf_smooth_lm()`, `sf_qq()`; legenda:
`sf_legend()`. Barwy to nazwy z palety z „@” (`"@niebo"`, `"@accent"`,
`"@ref"`, `"@wrzos/0.6"` z przezroczystością).

Gdy wykres zależy od kontrolki o skończonej liczbie ustawień (lista, segment,
pole wyboru, grupa pól), R liczy każdy wariant: `lc_sf_spec(variants = list(wartość = opis, …),
reads = …, html = …, control = …)`. `reads` wypełnia odczyty `lc_sf_value()`,
`html` wstawia gotowe fragmenty (np. tabelę) do elementów `[data-role]`.
Klucz grupy pól wyboru to zaznaczone wartości złączone „+”, a pustej — `none`.
Gdy wariant zależy od kilku kontrolek, `key = c("id1", "id2")` składa klucz
z ich wartości (`"TZ|0.5"`).
Wzorzec: `statystyka/09-case-studies/modules/figures.R`.

Kilka wykresów w rycinie: `lc_sf_figure(charts = 2, pair = TRUE)` stawia dwa
obok siebie już w kolumnie tekstu, `triple = TRUE` trzy w jednym rzędzie (jak
panele `facet_wrap()`). Opis osi przyjmuje też `xInterval` / `yInterval`
(stały odstęp siatki, np. co 1 w ćwiczeniu z odczytem współrzędnych).
Ryciny krokowe opisane w całości w R (rysunek, opis i tabela każdego kroku)
pokazuje wzorzec `s07_steps` w `statystyka/07-regresja/modules/figures.js`.

## Wykres ggplot jako rycina (`lc_sf_ggplot.R`)

Wykres na stałych danych, który był rysowany na serwerze, można przenieść bez
przepisywania: `lc_sf_ggplot(p, aria = "…", ratio = "2/1")` buduje obiekt ggplot
(`ggplot_build()`), odczytuje warstwy, zakresy, osie kategorii, legendę i panele
i składa opis ryciny `lc_spec`. Lista nazwanych wykresów to warianty dla
kontrolki: `lc_sf_ggplot(list(raw = p1, clean = p2), control = lc_segmented(…))`.
Barwy palety stają się tokenami motywu (tryb ciemny działa). Obsługiwane:
punkty, linie (braki przerywają linię), smooth z pasem, słupki i histogramy,
prostokąty, kafelki, teksty i etykiety, hline/vline/abline, odcinki i strzałki,
pudełka, słupki błędów, wstęgi, gęstość, `facet_wrap()`/`facet_grid()`.
Nieobsługiwana geometria daje ostrzeżenie przy budowaniu; `coord_flip()` zastąp
odwróconym mapowaniem (`aes(y = kategoria, x = wartość)`). Wzorzec: wykład 08.
