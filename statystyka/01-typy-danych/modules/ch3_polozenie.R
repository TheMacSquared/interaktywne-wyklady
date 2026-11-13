# ============================================================================
# CHAPTER 3: Statystyki położenia
# ============================================================================

# Drabina operacji (Ryc. 3.11): każdy typ dziedziczy operacje typów niższych
# i dokłada własne. Klocki od dołu; kolor klocka to typ, który go wnosi.
ch3_ops_levels <- list(
  list(sym = "= ≠", stat = "liczebność, dominanta", color = type_colors[["nominalna"]]),
  list(sym = "< >", stat = "mediana, percentyle",  color = type_colors[["porzadkowa"]]),
  list(sym = "+ −", stat = "średnia",              color = NA)
)

# Pięć pensji (tys. zł) do Ryc. 3.5, celowo nieposortowanych: sortowanie to
# pierwszy krok mediany. Małe liczby, żeby rachunek dało się zrobić w pamięci.
# Średnia i mediana wynoszą 5.
ch3_six_base <- c(5, 3, 6, 4, 7)

# Ryc. 3.4 i 3.6 (salary_chart.js): pensje losowane w przeglądarce jak dawniej w R,
# round(rgamma(n, shape = 3, scale = 1500) + 2000); oś do hi zł, do hi_wide po
# pojawieniu się pensji powyżej hi.
ch3_salary_config <- function(n, trim = 0, ceo = FALSE, hi, hi_wide = NULL,
                              bin, bin_wide = NULL) {
  jsonlite::toJSON(list(n = n, shape = 3, scale = 1500, shift = 2000, trim = trim,
                        ceo = ceo, ceo_value = 50000, hi = hi, hi_wide = hi_wide,
                        bin = bin, bin_wide = bin_wide, snap = 100),
                   auto_unbox = TRUE, digits = NA)
}

ch3_reset_button <- function(label) {
  tags$button(type = "button", class = "lc-action is-ghost is-icon", `data-s-act` = "reset",
              `aria-label` = label, title = label, lc_icon("reset"))
}

# Etykieta z próbką linii (ciągła / przerywana / kropkowana) zamiast legendy wykresu.
ch3_line_label <- function(label, color, style = NULL) {
  col <- if (color == "accent") "var(--upwr-accent)" else paste0("var(--upwr-cat-", color, ")")
  tagList(tags$i(class = paste("lc-sf-line-sw", style), `aria-hidden` = "true",
                 style = paste0("--lc-sw:", col, ";")), label)
}

# Tabela Ryc. 3.6: wartości i zmiany wpisuje salary_chart.js.
ch3_rob_table <- function() {
  cell <- function(role) tags$span(`data-role` = role, "–")
  df <- data.frame(stat = I(list(
    ch3_line_label("Średnia", "accent"),
    ch3_line_label("Mediana", "niebo", "is-dashed"),
    ch3_line_label("Średnia ucięta (10%)", "szalwia", "is-dotted"))))
  df$value <- I(list(cell("t-mean"), cell("t-median"), cell("t-trim")))
  df$change <- I(list(cell("d-mean"), cell("d-median"), cell("d-trim")))
  lc_table(df,
    cols = list(
      lc_col("stat", "Statystyka", "row"),
      lc_col("value", "Wartość (zł)"),
      lc_col("change", "Zmiana (zł)", short = "Zmiana",
             desc = "różnica względem danych bez dodanych wartości")
    ),
    label = "Statystyki położenia po dodaniu wartości")
}

ch3_ops_ladder <- function() {
  cols <- list(
    list(label = "Nominalne",  level = 1, color = type_colors[["nominalna"]]),
    list(label = "Porządkowe", level = 2, color = type_colors[["porzadkowa"]]),
    list(label = "Dyskretne",  level = 3, color = type_colors[["ilosciowa_dyskretna"]]),
    list(label = "Ciągłe",     level = 3, color = type_colors[["ilosciowa_ciagla"]])
  )
  block <- function(k, col) {
    lev <- ch3_ops_levels[[k]]
    color <- if (is.na(lev$color)) col$color else lev$color
    tags$div(
      class = paste("ops-step", if (k < col$level) "is-inherited"),
      style = paste0("--ops-c:", color, ";"),
      tags$div(class = "ops-sym", lev$sym),
      tags$div(class = "ops-stat", lev$stat)
    )
  }
  tags$div(class = "ops-ladder", role = "img",
    `aria-label` = "Drabina operacji: nominalne = ≠ (liczebność, dominanta), porządkowe dodatkowo < > (mediana, percentyle), ilościowe dodatkowo + − (średnia).",
    lapply(cols, function(col) {
      tags$div(class = "ops-col",
        tags$div(class = "ops-stack", lapply(rev(seq_len(col$level)), block, col = col)),
        tags$div(class = "ops-label", style = paste0("--ops-c:", col$color, ";"), col$label)
      )
    })
  )
}

# Ryc. 3.2, 3.3, 3.7, 3.8 rysuje stat_charts.js; opisy dla każdej opcji liczymy
# tu z góry (dane ankiety są stałe), a przeglądarka pokazuje właściwy.
ch3_mean_choices <- c("Wzrost (cm)" = "wzrost", "Waga (kg)" = "waga", "Średnia ocen" = "srednia_ocen")
ch3_median_choices <- c("Wzrost (cm)" = "wzrost", "Czas dojazdu (min)" = "czas_dojazdu",
                        "Średnia ocen" = "srednia_ocen")
ch3_disc_choices <- c("Liczba nieobecności" = "liczba_nieobecnosci", "Liczba kursów" = "liczba_kursow")

ch3_mean_notes <- function() {
  lapply(stats::setNames(nm = unname(ch3_mean_choices)), function(v) {
    x <- student_data[[v]]
    lc_status(paste0("$$\\bar{x} = \\frac{", round(sum(x), 1), "}{", length(x), "} = ",
                     round(mean(x), 2), "$$"))
  })
}

ch3_median_notes <- function() {
  lapply(stats::setNames(nm = unname(ch3_median_choices)), function(v) {
    x <- student_data[[v]]
    lc_status(paste0("Mediana = ", round(median(x), 1)), " | Średnia = ", round(mean(x), 2),
              " | Różnica = ", round(abs(mean(x) - median(x)), 2))
  })
}

ch3_disc_notes <- function() {
  lapply(stats::setNames(nm = unname(ch3_disc_choices)), function(v) {
    x <- student_data[[v]]
    mode_val <- as.numeric(names(sort(table(x), decreasing = TRUE))[1])
    lc_readouts(
      lc_readout("Średnia", lc_fmt(mean(x), 2)),
      lc_readout("Mediana", median(x)),
      lc_readout("Moda", mode_val),
      lc_readout("SD", lc_fmt(sd(x), 2)),
      lc_readout("Zakres", paste0(min(x), "–", max(x)))
    )
  })
}

ch3_modal_notes <- function() list(
  unimodal = lc_status(
    tags$b("Rozkład unimodalny: "), "jeden szczyt. ",
    "Dla rozkładu symetrycznego moda ≈ średnia ≈ mediana. ",
    "Większość statystyk opisowych zakłada właśnie taki rozkład."),
  bimodal = lc_status(
    lc_verdict(tags$b("Rozkład bimodalny: "), type = "warning"),
    "dwa szczyty, osobno dla kobiet ", "i mężczyzn. Średnia całości wypada między szczytami, ",
    "gdzie obserwacji jest niewiele."),
  multimodal = lc_status(
    lc_verdict(tags$b("Rozkład wielomodalny: "), type = "warning"),
    "trzy szczyty = trzy podgrupy. ",
    "Każda podgrupa (rowerzyści, pasażerowie autobusów, kierowcy) ", "ma własną typową wartość.")
)

ch3_ui <- list(
  id = "ch-polozenie", num = "03", title = "Statystyki położenia",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 03 · Statystyka opisowa",
      num    = "03",
      title  = "Statystyki położenia.",
      lead   = "Dwustu wyników nikt nie przeczyta jeden po drugim. Potrzebujemy jednej
                liczby, która powie, gdzie leży typowa wartość. Średnia, mediana
                i percentyle robią to na różne sposoby i każda z nich ma słabe miejsca."
    ),

    uiOutput("tracker_ch3"),

    lc_p("W poprzednim rozdziale opisywaliśmy zmienne jakościowe: liczyliśmy
      kategorie i szukaliśmy najczęstszej z nich. ",
      gloss("zmienna ilościowa", "Zmienne ilościowe"), ", takie jak wzrost czy
      czas dojazdu, wymagają innych narzędzi, bo ich wartości są liczbami,
      a nie etykietami. Zaczniemy od tego, żeby takie dane zobaczyć, a potem
      streścimy je jedną liczbą."),

    # ========================================================================
    # WIDGET: Histogram krok po kroku
    # ========================================================================
    lc_h2("ch3-histogram", "Histogram — krok po kroku"),

    lc_p(gloss("histogram", "Histogram"), " to podstawowy wykres dla ",
      gloss("zmienna ciągła", "zmiennych ciągłych"), ". Zakres wartości dzielimy
      na przedziały równej szerokości (biny), a wysokość słupka nad przedziałem
      pokazuje, ile ", gloss("obserwacja", "obserwacji"), " do niego wpadło. Poniżej budujemy histogram
      krok po kroku na danych z ankiety 200 studentów."),

    figure_panel(
      label = "Ryc. 3.1",
      # Stosy (kroki 5–6) i trzy histogramy pod sobą (krok 7) potrzebują więcej wysokości.
      tags$style(HTML("
        #ch3_hist[data-lc-step='5'] .lc-sf-chart[data-k='0'],
        #ch3_hist[data-lc-step='6'] .lc-sf-chart[data-k='0'] { aspect-ratio: 1.8 / 1 !important; max-height: 460px !important; }
        #ch3_hist[data-lc-step='7'] .lc-sf-chart { aspect-ratio: 3.6 / 1 !important; min-height: 9em; }
      ")),
      lc_sf_figure("hist", bare = TRUE, body = lc_step_widget("ch3_hist",
        title = "Budowa histogramu",
        steps = c("Surowe dane", "Posortuj dane", "Podziel na przedziały",
                  "Przypisz do binów", "Zlicz obserwacje", "Zbuduj słupki",
                  "Wpływ szerokości binu"),
        toolbar = lc_toolbar(
          lc_sf_ctl(lc_group("Zmienna", lc_select("ch3_hist_var",
            c("Wzrost (cm)" = "wzrost", "Waga (kg)" = "waga",
              "Czas dojazdu (min)" = "czas_dojazdu", "Średnia ocen" = "srednia_ocen"),
            aria_label = "Zmienna"))),
          lc_step_from(3, lc_sf_slider("ch3_hist_bin_width", "Szerokość binu", 1, 15, 5, 1)),
          lc_chart_zoom_button()
        ),
        body = tagList(lc_sf_chart(0, ratio = "2.5/1", max_height = "420px"),
                       lc_sf_chart(1, ratio = "3.6/1", max_height = "240px"),
                       lc_sf_chart(2, ratio = "3.6/1", max_height = "240px")),
        extra = tags$div(`data-role` = "table")
      ))
    ),

    lc_p("Ostatni krok pokazuje rzecz, o której łatwo zapomnieć: kształt
      histogramu zależy od szerokości binu. Zbyt wąskie przedziały dają
      poszarpany wykres, w którym przypadkowe wahania przesłaniają kształt
      rozkładu. Zbyt szerokie zlewają dane w kilka słupków i ukrywają
      szczegóły. Domyślne ustawienie programu jest tylko punktem wyjścia."),

    lc_p("Histogram pokazuje cały rozkład, ale do porównań i raportów potrzebujemy
      czegoś krótszego: liczby, która mówi, gdzie leży środek danych. Takie
      liczby nazywamy ", gloss("statystyka", "statystykami"), " położenia."),

    # ========================================================================
    # WIDGET 0a: Mean introduction
    # ========================================================================
    lc_h2("ch3-srednia", "Średnia arytmetyczna"),

    lc_p("Najbardziej znaną statystyką położenia jest ",
      gloss("średnia", "średnia arytmetyczna"), ": sumujemy wszystkie wartości
      i dzielimy przez ich liczbę."),

    lc_formula_annotated(
      "⟦1|\\bar{x}⟧ = \\frac{1}{⟦2|n⟧} ⟦3|\\sum_{i=1}^{n}⟧ ⟦4|x_i⟧ = \\frac{x_1 + x_2 + \\ldots + x_n}{n}",
      notes = list(
        "średnia",
        "liczba obserwacji",
        "suma",
        "i-ta obserwacja"
      ),
      colors = c("accent", "szalwia", "bursztyn", "niebo"),
      above = c(3, 4)
    ),

    lc_p("Średnią można rozumieć jako punkt równowagi. Gdyby każdą obserwację
      położyć jako jednakowy ciężarek na linijce, linijka balansowałaby
      dokładnie w punkcie średniej."),

    figure_panel(
      label = "Ryc. 3.2",
      title = "Średnia jako punkt równowagi",
      lc_sf_figure("mean", ratio = "2.4/1", max_height = "320px",
        control = lc_group("Zmienna", lc_select("ch3_mean_var", ch3_mean_choices,
                                                aria_label = "Zmienna")),
        selected = "wzrost", notes = ch3_mean_notes())
    ),

    lc_p("Średni wzrost w naszej ankiecie wynosi 171.1 cm. Do sumy trafia każda
      wartość, więc każda ciągnie średnią w swoją stronę, także wartości skrajne.
      Obserwacja odległa od reszty przesuwa punkt równowagi bardziej niż kilka
      obserwacji leżących blisko środka. To zaleta, gdy chcemy uwzględnić
      wszystkie dane, i słabość, gdy w danych trafiają się wartości nietypowe.
      Dlatego potrzebujemy drugiej miary, która na skrajności nie reaguje."),

    # ========================================================================
    # WIDGET 0b: Median introduction
    # ========================================================================
    lc_h2("ch3-mediana", "Mediana"),

    lc_p(gloss("mediana", "Mediana"), " to wartość środkowa: po posortowaniu danych
      połowa obserwacji leży poniżej niej, a połowa powyżej. Przy nieparzystej
      liczbie obserwacji jest to środkowy element, przy parzystej — średnia
      z dwóch środkowych. Zapis \\(x_{(i)}\\) oznacza i-tą wartość po posortowaniu."),

    lc_formula_annotated(
      "\\begin{aligned} n \\text{ nieparzyste:} \\quad \\text{Me} &= ⟦1|x_{((n+1)/2)}⟧ \\\\[10pt] n \\text{ parzyste:} \\quad \\text{Me} &= ⟦2|\\dfrac{x_{(n/2)} + x_{(n/2+1)}}{2}⟧ \\end{aligned}",
      notes = list(
        "środkowa obserwacja",
        "średnia dwóch środkowych obserwacji"
      ),
      colors = c("wrzos", "indygo"),
      side = "right"
    ),

    lc_p("Mediana zależy tylko od kolejności obserwacji. Nie ma znaczenia, jak
      daleko od środka leżą wartości skrajne, liczy się tylko to, po której
      stronie się znajdują."),

    figure_panel(
      label = "Ryc. 3.3",
      title = "Mediana dzieli dane na pół",
      lc_sf_figure("median", ratio = "2.4/1", max_height = "320px",
        control = lc_group("Zmienna", lc_select("ch3_median_var", ch3_median_choices,
                                                selected = "czas_dojazdu", aria_label = "Zmienna")),
        selected = "czas_dojazdu", notes = ch3_median_notes())
    ),

    lc_p("Porównaj obie miary dla dwóch zmiennych. Dla wzrostu mediana (170.6 cm)
      i średnia (171.1 cm) prawie się pokrywają, bo rozkład jest w przybliżeniu
      symetryczny. Dla czasu dojazdu różnica jest wyraźniejsza: mediana wynosi
      32.9 min, a średnia 35.7 min. Rozkład czasu dojazdu ma długi prawy ogon:
      19 osób dojeżdża dłużej niż godzinę i to one podnoszą średnią. Mediana
      tylko odnotowuje, że leżą powyżej środka."),

    # ========================================================================
    # WIDGET 1: Mean vs Median -- comparison
    # ========================================================================
    lc_h2("ch3-srednia-vs-mediana", "Średnia vs mediana — kiedy się różnią?"),

    lc_p("Różnica między średnią a medianą mówi więc coś o kształcie rozkładu.
      Mechanizm najlepiej widać na danych, w których skrajności są normą:
      na zarobkach. W typowej firmie większość pracowników zarabia umiarkowanie,
      a nieliczni bardzo dużo. W panelu poniżej można dopisywać kolejne pensje
      do wylosowanej listy 30 wynagrodzeń, w tym ",
      gloss("wartość odstająca", "wartość odstającą"), " — pensję prezesa."),

    figure_panel(
      label = "Ryc. 3.4",
      title = "Zarobki w firmie: średnia vs mediana",

      tags$div(class = "lc-salary",
        `data-config` = ch3_salary_config(n = 30, ceo = TRUE, hi = 25000, hi_wide = 60000,
                                          bin = 1000, bin_wide = 2000),
        lc_toolbar(
          tags$button(type = "button", class = "lc-action is-solid", `data-s-act` = "ceo",
                      "Dodaj pensję prezesa"),
          ch3_reset_button("Nowe losowanie 30 pensji"),
          lc_readouts(
            lc_readout("n", tags$span(`data-role` = "n", "30")),
            lc_readout(ch3_line_label("Średnia", "accent"), tags$span(`data-role` = "mean", "–"),
                       color = "var(--upwr-accent)"),
            lc_readout(ch3_line_label("Mediana", "niebo", "is-dashed"), tags$span(`data-role` = "median", "–"),
                       color = "var(--upwr-cat-niebo)"),
            lc_readout("Różnica", tags$span(`data-role` = "diff", "–"), color = "var(--upwr-cat-szalwia)")
          ),
          lc_chart_zoom_button()
        ),
        tags$div(class = "lc-salary-hist",
          tags$div(class = "lc-ec", role = "img", `data-role` = "hist",
            `aria-label` = "Histogram zarobków z liniami średniej i mediany")),
        tags$div(class = "lc-salary-strip",
          tags$div(class = "lc-ec", role = "img", `data-role` = "strip",
            `aria-label` = "Pensje jako punkty na osi zarobków; punkt prezesa można przeciągać")),
        lc_caption("Kliknij na wykresie, aby dopisać pensję w tym miejscu. Pensję prezesa
          (duży punkt) przeciągnij po pasku pod histogramem lub wybierz Tabem i przesuń strzałkami.")
      )
    ),

    lc_p("Po dopisaniu pensji podobnych do pozostałych średnia i mediana
      przesuwają się nieznacznie i trzymają się blisko siebie. Jedna pensja
      prezesa zmienia obraz: przy 30 pensjach średnia skacze o ponad tysiąc złotych, a mediana
      przesuwa się najwyżej o pół pozycji w posortowanej liście."),

    lc_p("Obie liczby odpowiadają na inne pytania. Średnia mówi, ile przypadłoby
      na osobę, gdyby sumę wszystkich pensji podzielić po równo. Mediana mówi,
      ile zarabia osoba stojąca w środku kolejki. Przy zarobkach to drugie pytanie
      zwykle lepiej opisuje typowego pracownika, dlatego w raportach
      o wynagrodzeniach obok średniej podaje się medianę."),

    # ========================================================================
    # WIDGET 1b: Sixth observation -- formulas step by step
    # ========================================================================
    lc_p("Ten sam mechanizm widać w samych wzorach. Poniżej jest pięć pensji
      z małego zespołu, w tysiącach złotych, i puste miejsce na szóstą. Wpisz
      dowolną kwotę i porównaj,
      co zmienia się w rachunku średniej, a co w rachunku mediany."),

    figure_panel(
      label = "Ryc. 3.5",
      title = "Szósta obserwacja we wzorach",

      lc_toolbar(
        lc_action("ch3_six_low", "Praktykant: 1 tys. zł", variant = "outline"),
        lc_action("ch3_six_ceo", "Prezes: 60 tys. zł", variant = "outline"),
        lc_action("ch3_six_clear", icon = "reset", variant = "ghost", aria_label = "Wyczyść"),
        lc_readouts(uiOutput("ch3_six_stats"))
      ),

      tags$div(class = "ch3-six-row", role = "group", `aria-label` = "Obserwacje",
        lapply(seq_along(ch3_six_base), function(i) {
          tags$div(class = "ch3-six-cell",
            tags$span(class = "ch3-six-idx", paste0("x", i)),
            tags$span(class = "ch3-six-val", ch3_six_base[i])
          )
        }),
        tags$div(class = "ch3-six-cell is-input",
          tags$label(class = "ch3-six-idx", `for` = "ch3_six_x", "x6"),
          tags$input(id = "ch3_six_x", type = "number", class = "form-control",
                     min = 0, step = 1, placeholder = "wpisz")
        )
      ),

      uiOutput("ch3_six_formulas")
    ),

    lc_p("W liczniku średniej stoi suma, więc szósta pensja wchodzi do niej całą
      swoją wartością: im dalej od reszty, tym mocniej przesuwa wynik. Do mediany
      trafiają tylko dwie środkowe wartości po posortowaniu. Nowa obserwacja
      decyduje jedynie o tym, które to będą, a jej wielkość nie ma znaczenia.
      Dlatego 60 i 7 tys. zł dają tę samą medianę."),

    # ========================================================================
    # WIDGET 2: Robustness mini-demo
    # ========================================================================
    lc_h2("ch3-odpornosc", "Odporność miar na wartości odstające"),

    lc_p("Własność, którą właśnie zaobserwowaliśmy, ma nazwę: ",
      gloss("odporność"), ". Statystyka jest odporna, jeśli pojedyncze skrajne
      obserwacje nie zmieniają jej znacząco. Średnia nie jest odporna,
      mediana jest."),

    lc_p("Między nimi leży ", gloss("średnia ucięta"), ". Odrzucamy ustalony
      odsetek najmniejszych i największych wartości, a ze środka liczymy zwykłą
      średnią. W panelu poniżej ucinamy po 10% z każdej strony: przy 40 pensjach
      odpadają cztery najniższe i cztery najwyższe."),

    figure_panel(
      label = "Ryc. 3.6",
      title = "Odporność: średnia vs mediana vs średnia ucięta",

      tags$div(class = "lc-salary",
        `data-config` = ch3_salary_config(n = 40, trim = 0.1, hi = 60000, bin = 1500),
        lc_toolbar(
          tags$button(type = "button", class = "lc-action is-solid", `data-s-act` = "out1",
                      "Dodaj wartość odstającą (ok. 50 000 zł)"),
          tags$button(type = "button", class = "lc-action is-solid", `data-s-act` = "out5",
                      "Dodaj 5 wartości odstających"),
          ch3_reset_button("Nowe losowanie 40 pensji"),
          lc_readouts(
            lc_readout("Dodane wartości", tags$span(`data-role` = "count", "0"),
                       color = "var(--upwr-secondary)")
          ),
          lc_chart_zoom_button()
        ),
        tags$div(class = "lc-salary-hist",
          tags$div(class = "lc-ec", role = "img", `data-role` = "hist",
            `aria-label` = "Histogram zarobków z liniami średniej, mediany i średniej uciętej")),
        tags$div(class = "lc-salary-strip",
          tags$div(class = "lc-ec", role = "img", `data-role` = "strip",
            `aria-label` = "Pensje jako punkty; puste kółka to wartości odrzucone w średniej uciętej")),
        lc_caption("Kliknij na wykresie, aby dopisać pensję w tym miejscu. Puste kółka
          na pasku to pensje odrzucone przy liczeniu średniej uciętej."),
        ch3_rob_table()
      )
    ),

    lc_p("Pierwsza wartość odstająca wyraźnie podnosi średnią, a średnia ucięta
      i mediana prawie stoją w miejscu. Średnia ucięta chroni jednak tylko
      do pewnej granicy. Gdy wartości odstających jest więcej, niż wynosi
      obcięty odsetek, część z nich trafia do obliczeń. Przy pięciu dodanych
      pensjach obcinamy cztery największe, więc piąta już podnosi wynik.
      Mediana wytrzymuje znacznie więcej: zmienia się wyraźnie dopiero wtedy,
      gdy skrajne wartości stanowią blisko połowę danych."),

    lc_note("Zasada", rule = TRUE,
      "Przy rozkładach skośnych, takich jak zarobki, ceny czy czasy oczekiwania,
       podawaj medianę obok średniej albo zamiast niej."
    ),

    # ========================================================================
    # WIDGET 2b: Discrete variables
    # ========================================================================
    lc_h2("ch3-dyskretna", "Zmienne dyskretne — te same statystyki, inne wykresy"),

    lc_p("Dotąd pracowaliśmy na zmiennych ciągłych: wzroście i zarobkach. ",
      gloss("zmienna dyskretna", "Zmienne dyskretne"), ", takie jak liczba kursów
      czy liczba nieobecności, przyjmują tylko wartości całkowite. Średnią
      i medianę liczymy dla nich tak samo, ale wykres trzeba wybrać ostrożniej."),

    figure_panel(
      label = "Ryc. 3.7",
      title = "Dyskretna vs ciągła — porównanie wizualizacji",
      lc_sf_figure("disc", charts = 2, ratio = "1.5/1", max_height = "300px",
        headings = list("Wykres słupkowy (poprawny)", "Histogram (problematyczny)"),
        control = lc_segmented("ch3_disc_var", "Zmienna dyskretna", choices = ch3_disc_choices),
        selected = "liczba_nieobecnosci", notes = ch3_disc_notes())
    ),

    lc_p(gloss("wykres słupkowy", "Wykres słupkowy"), " rysuje osobny słupek dla każdej wartości: zero, jednej,
      dwóch nieobecności i tak dalej, więc pokazuje liczebności dokładnie.
      Histogram dzieli oś na przedziały, których granice nie pokrywają się
      z liczbami całkowitymi. Dwie sąsiednie wartości mogą trafić do jednego
      słupka, a niektóre przedziały zostają puste, choć w danych nie ma
      żadnej luki."),

    lc_p("Zwróć też uwagę na średnią. Średnia liczba nieobecności wynosi 2.87,
      choć nikt nie opuścił 2.87 zajęć. Średnia nie musi być wartością, którą
      ktokolwiek faktycznie przyjmuje. Mediana zmiennej dyskretnej zwykle jest
      jedną z jej wartości, tutaj wynosi 3."),

    # ========================================================================
    # WIDGET 2c: Multimodality in continuous distributions
    # ========================================================================
    lc_h2("ch3-modalnosc", "Modalność rozkładu — ile „górek” ma histogram?"),

    lc_p("W rozdziale o ", gloss("zmienna jakościowa", "zmiennych jakościowych"),
      " ", gloss("dominanta", "dominantą"), " nazywaliśmy najczęstszą kategorię. Dla zmiennej ciągłej to
      pojęcie trzeba przerobić: prawie każda wartość występuje tylko raz, więc
      zamiast najczęstszej wartości szukamy najwyższego miejsca histogramu,
      czyli szczytu rozkładu. Szczyt nazywamy ", gloss("moda", "modą"), "."),

    lc_p("Ważniejsze od położenia szczytu bywa to, ile ich jest. Rozkład może
      mieć jeden szczyt (unimodalny), dwa (bimodalny) albo więcej (wielomodalny)."),

    figure_panel(
      label = "Ryc. 3.8",
      title = "Unimodalny vs bimodalny vs wielomodalny",
      lc_sf_figure("modal", ratio = "1.9/1", max_height = "360px", legend = TRUE,
        control = lc_segmented("ch3_modal_scenario", "Scenariusz",
          choices = c("Unimodalny" = "unimodal", "Bimodalny" = "bimodal",
                      "Wielomodalny" = "multimodal")),
        selected = "unimodal", notes = ch3_modal_notes())
    ),

    lc_p("Kilka szczytów to zwykle znak, że w danych są pomieszane różne grupy.
      Wtedy jedna statystyka położenia może opisywać wartość, której prawie nikt
      nie ma: średnia wzrostu kobiet i mężczyzn razem wypada pomiędzy szczytami.
      Przed policzeniem średniej obejrzyj histogram. Jeśli widać kilka szczytów,
      opisz grupy osobno."),

    # ========================================================================
    # WIDGET 3: Percentile explorer
    # ========================================================================
    lc_h2("ch3-percentyle", "Percentyle i kwantyle"),

    lc_p("Mediana dzieli dane na dwie połowy. Ten sam pomysł można uogólnić na
      dowolny podział. ", gloss("percentyl", "Percentyl"), " rzędu p to wartość,
      poniżej której leży p% obserwacji. Na przykład 75. percentyl wzrostu to
      wzrost, którego nie przekracza 75% studentów. Ogólniej mówimy
      o ", gloss("kwantyl", "kwantylach"), " rzędu q, gdzie q jest ułamkiem od 0 do 1."),

    lc_p("Najczęściej używamy trzech ", gloss("kwartyl", "kwartyli"),
      ", które dzielą dane na cztery równe części:"),
    tags$ul(
      tags$li(tags$strong("Q1 (25. percentyl)"), " — poniżej leży pierwsza ćwiartka danych"),
      tags$li(tags$strong("Q2 (50. percentyl)"), " — mediana"),
      tags$li(tags$strong("Q3 (75. percentyl)"), " — powyżej leży ostatnia ćwiartka danych")
    ),

    figure_panel(
      label = "Ryc. 3.9",
      title = "Percentyle wzrostu studentów",

      tags$div(class = "lc-pctl",
        `data-config` = jsonlite::toJSON(list(values = student_data$wzrost, p = 50, bin = 2,
                                              unit = "cm"), auto_unbox = TRUE, digits = NA),
        lc_toolbar(
          lc_group("Skróty", tags$div(class = "lc-seg", role = "group", `aria-label` = "Skróty",
            tags$button(type = "button", `data-p-set` = 25, `aria-pressed` = "false", "Q1 (25%)"),
            tags$button(type = "button", `data-p-set` = 50, `aria-pressed` = "true", "Mediana (50%)"),
            tags$button(type = "button", `data-p-set` = 75, `aria-pressed` = "false", "Q3 (75%)"))),
          lc_readouts(
            lc_readout("Rząd", tags$span(`data-role` = "p", "50%")),
            lc_readout("Percentyl", tags$span(`data-role` = "q", "–"), color = "var(--upwr-accent)"),
            lc_readout("Obserwacje ≤", tags$span(`data-role` = "share", "–"),
                       color = "var(--upwr-cat-niebo)")
          ),
          lc_chart_zoom_button()
        ),
        tags$div(class = "lc-pctl-hist",
          tags$div(class = "lc-ec", role = "img", `data-role` = "hist",
            `aria-label` = "Histogram wzrostu z przeciąganą linią percentyla")),
        tags$div(class = "lc-pctl-box",
          tags$div(class = "lc-ec", role = "img", `data-role` = "box",
            `aria-label` = "Wykres pudełkowy wzrostu z zaznaczonym percentylem")),
        lc_caption("Przeciągnij linię percentyla lub użyj Tab i strzałek."),
        lc_status(p(`data-role` = "text", ""))
      )
    ),

    lc_p("W naszej ankiecie Q1 wzrostu wynosi 165.5 cm, a Q3 177.0 cm, więc
      połowa studentów ma wzrost między tymi wartościami. Odległość Q3 − Q1,
      tutaj 11.5 cm, to ",
      gloss("rozstęp międzykwartylowy"), " (IQR). Jest to miara rozrzutu
      odporna na wartości odstające, z tego samego powodu co mediana. Korzysta
      z niej ", gloss("wykres pudełkowy"), " pod histogramem; wrócimy do niego w następnym
      rozdziale."),

    # ====================================================================
    # WIDGET 4: Guess the statistic game
    # ====================================================================
    lc_h2("ch3-gra", "Gra: zgadnij średnią i medianę"),

    lc_p("Na koniec ćwiczenie: odczytanie obu miar z samego
      histogramu. Wskaż dwie odpowiedzi: typowaną średnią i medianę. Możesz
      kliknąć na wykresie lub użyć Tab i strzałek. Najpierw ustal, w którą stronę
      ciągnie ogon rozkładu i po której stronie mediany powinna wtedy
      leżeć średnia."),

    figure_panel(
      label = "Ryc. 3.10",
      title = "Wskaż średnią i medianę",
      tags$div(class = "lc-guess",
        lc_toolbar(
          tags$button(type = "button", class = "lc-action is-solid", `data-g-act` = "new", "Nowa runda"),
          tags$button(type = "button", class = "lc-action is-outline", `data-g-act` = "reveal",
                      disabled = NA, "Pokaż odpowiedź"),
          lc_chart_zoom_button()
        ),
        tags$div(`data-role` = "banner", class = "lc-caption", `data-tone` = "info",
                 tags$div("Wskaż średnią: kliknij na wykresie lub użyj Tab i strzałek.")),
        tags$div(class = "lc-guess-canvas",
          tags$div(class = "lc-ec", role = "img", `data-role` = "hist",
            `aria-label` = "Histogram losowych danych; mysz lub klawiatura wskazują typowaną średnią i medianę")),
        tags$div(`data-role` = "feedback", class = "lc-status", role = "status",
                 `aria-live` = "polite", hidden = NA)
      )
    ),

    # ====================================================================
    # Klamra: drabina operacji
    # ====================================================================
    lc_h2("ch3-drabina", "Która miara dla którego typu"),

    lc_p("Miary położenia z tego i poprzedniego rozdziału układają się w drabinę.
      Każdy typ zmiennej pozwala na operacje typów niższych i dokłada własne.
      Kategorie nominalne można tylko rozróżnić, więc zostaje dominanta.
      Porządkowe da się uszeregować, więc dochodzą mediana i percentyle.
      Dopiero na liczbach wolno dodawać, więc dopiero tam ma sens średnia.
      Dla zmiennej ilościowej wolno policzyć wszystkie trzy miary, a wybór
      między nimi zależy od kształtu rozkładu."),

    figure_panel(
      label = "Ryc. 3.11",
      title = "Drabina operacji",
      ch3_ops_ladder()
    ),

    lc_chapter_next(
      num       = "04",
      title     = "Statystyki rozrzutu",
      lead      = "dwie grupy z tą samą średnią mogą wyglądać zupełnie inaczej — różni je rozrzut.",
      target_id = "ch-rozrzut"
    ),

    # Spacer at bottom
    lc_spacer("lg")

  )
)

# --------------------------------------------------------------------------
# Chapter 3 Server
# --------------------------------------------------------------------------

ch3_server <- function(input, output, session) {

  # Ryc. 3.1 (budowa histogramu) rysuje stat_charts.js; suwak i tabela w przeglądarce.

  # --------------------------------------------------------------------------
  # Widget 1b: Sixth observation -- formulas step by step
  # --------------------------------------------------------------------------

  observeEvent(input$ch3_six_low, {
    session$sendInputMessage("ch3_six_x", list(value = 1))
  })

  observeEvent(input$ch3_six_ceo, {
    session$sendInputMessage("ch3_six_x", list(value = 60))
  })

  observeEvent(input$ch3_six_clear, {
    session$sendInputMessage("ch3_six_x", list(value = ""))
  })

  # Szósta wartość albo NULL, gdy pole jest puste lub niepoprawne
  ch3_six_new <- reactive({
    v <- input$ch3_six_x
    if (is.null(v) || !is.numeric(v) || !is.finite(v)) NULL else v
  })

  ch3_six_values <- reactive(c(ch3_six_base, ch3_six_new()))

  # Liczba w TeX-u: spacja tysięcy jako \, , najwyżej jedno miejsce po przecinku
  ch3_six_tex <- function(v) gsub(" ", "\\\\,", lc_fmt(v, 1, big_mark = " "))

  output$ch3_six_stats <- renderUI({
    x <- ch3_six_values()
    tagList(
      lc_readout("Średnia", paste0(lc_fmt(mean(x), 1), " tys. zł"), color = upwr_accent),
      lc_readout("Mediana", paste0(lc_fmt(median(x), 1), " tys. zł"), color = upwr_cat[["niebo"]])
    )
  })

  output$ch3_six_formulas <- renderUI({
    x <- ch3_six_values()
    n <- length(x)
    has_new <- n > length(ch3_six_base)
    m <- mean(x)
    med <- median(x)

    # Licznik średniej w kolejności wpisania; szósta wartość wyróżniona
    terms <- ch3_six_tex(x)
    if (has_new) terms[n] <- paste0("\\class{ch3-six-new}{", terms[n], "}")
    m_eq <- if (abs(round(m, 1) - m) > 1e-9) "\\approx" else "="
    mean_tex <- paste0(
      "\\bar{x} = \\frac{", paste(terms, collapse = " + "), "}{", n, "} = \\frac{",
      ch3_six_tex(sum(x)), "}{", n, "} ", m_eq, " ", ch3_six_tex(m)
    )

    # Posortowane obserwacje; środkowe pozycje i nowa wartość wyróżnione
    ord <- order(x)
    mid <- if (n %% 2 == 1) (n + 1) / 2 else c(n / 2, n / 2 + 1)
    sorted_cells <- lapply(seq_len(n), function(i) {
      tags$div(
        class = paste("ch3-six-cell", if (i %in% mid) "is-mid", if (has_new && ord[i] == n) "is-new"),
        tags$span(class = "ch3-six-idx", paste0("x(", i, ")")),
        tags$span(class = "ch3-six-val", lc_fmt(x[ord][i], 1, big_mark = " "))
      )
    })
    xs <- ch3_six_tex(x[ord])
    median_tex <- if (n %% 2 == 1) {
      paste0("n = ", n, " \\text{ (nieparzyste):} \\quad \\text{Me} = x_{(", mid, ")} = ", xs[mid])
    } else {
      paste0("n = ", n, " \\text{ (parzyste):} \\quad \\text{Me} = \\frac{x_{(", mid[1], ")} + x_{(", mid[2],
             ")}}{2} = \\frac{", xs[mid[1]], " + ", xs[mid[2]], "}{2} = ", ch3_six_tex(med))
    }

    base_m <- mean(ch3_six_base)
    base_med <- median(ch3_six_base)
    signed <- function(d) paste0(if (d > 0) "+" else if (d < 0) "−", lc_fmt(abs(d), 1), " tys. zł")

    tagList(
      tags$div(class = "ch3-six-label", "Średnia: suma wszystkich wartości przez ich liczbę"),
      lc_formula_box(withMathJax(paste0("$$", mean_tex, "$$"))),
      tags$div(class = "ch3-six-label", "Mediana: najpierw sortujemy, potem bierzemy środek"),
      tags$div(class = "ch3-six-row is-sorted", sorted_cells),
      lc_formula_box(withMathJax(paste0("$$", median_tex, "$$"))),
      if (has_new) {
        lc_caption(paste0("Względem pięciu pensji średnia zmieniła się o ", signed(m - base_m),
                          ", a mediana o ", signed(med - base_med), "."))
      } else {
        lc_caption("Pole x6 jest puste: liczymy z pięciu obserwacji.")
      }
    )
  })


}
