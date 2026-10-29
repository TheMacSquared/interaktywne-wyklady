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
  tagList(tags$i(class = paste("lc01-line-sw", style), `aria-hidden` = "true",
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
        #ch3_hist[data-lc-step='5'] .lc-step-plot,
        #ch3_hist[data-lc-step='6'] .lc-step-plot { --lc-plot-ratio: 1.8/1 !important; }
        #ch3_hist[data-lc-step='7'] .lc-step-plot { --lc-plot-ratio: 1.2/1 !important;
          --lc-plot-ratio-narrow: 3/4 !important; --lc-plot-max: 600px; }
      ")),
      lc_step_widget("ch3_hist",
        title = "Budowa histogramu",
        steps = c("Surowe dane", "Posortuj dane", "Podziel na przedziały",
                  "Przypisz do binów", "Zlicz obserwacje", "Zbuduj słupki",
                  "Wpływ szerokości binu"),
        toolbar = lc_toolbar(
          selectInput("ch3_hist_var", "Zmienna",
            choices = c("Wzrost (cm)" = "wzrost", "Waga (kg)" = "waga",
                        "Czas dojazdu (min)" = "czas_dojazdu",
                        "Średnia ocen" = "srednia_ocen"),
            selected = "wzrost"
          ),
          lc_step_from(3, uiOutput("ch3_hist_bin_slider"))
        ),
        plot_id = "ch3_hist_plot",
        extra = uiOutput("ch3_hist_table")
      )
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
      selectInput("ch3_mean_var", "Zmienna:",
        choices = c("Wzrost (cm)" = "wzrost", "Waga (kg)" = "waga",
                    "Średnia ocen" = "srednia_ocen"),
        selected = "wzrost"
      ),
      lc_plot("ch3_mean_plot", max_height = "300px"),
      uiOutput("ch3_mean_text")
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
      selectInput("ch3_median_var", "Zmienna:",
        choices = c("Wzrost (cm)" = "wzrost", "Czas dojazdu (min)" = "czas_dojazdu",
                    "Średnia ocen" = "srednia_ocen"),
        selected = "czas_dojazdu"
      ),
      lc_plot("ch3_median_plot", max_height = "300px"),
      uiOutput("ch3_median_text")
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
          )
        ),
        tags$div(class = "lc-salary-hist",
          tags$canvas(`data-role` = "hist",
            `aria-label` = "Histogram zarobków z liniami średniej i mediany")),
        tags$div(class = "lc-salary-strip",
          tags$canvas(`data-role` = "strip",
            `aria-label` = "Pensje jako punkty na osi zarobków; punkt prezesa można przeciągać")),
        lc_caption("Kliknij na wykresie, aby dopisać pensję w tym miejscu. Pensję prezesa
          (duży punkt) przeciągnij po pasku pod histogramem.")
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
          )
        ),
        tags$div(class = "lc-salary-hist",
          tags$canvas(`data-role` = "hist",
            `aria-label` = "Histogram zarobków z liniami średniej, mediany i średniej uciętej")),
        tags$div(class = "lc-salary-strip",
          tags$canvas(`data-role` = "strip",
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
      lc_toolbar(
        lc_segmented("ch3_disc_var", "Zmienna dyskretna",
          choices = c("Liczba nieobecności" = "liczba_nieobecnosci",
                      "Liczba kursów" = "liczba_kursow")),
        lc_readouts(uiOutput("ch3_disc_stats"))
      ),
      lc_plots(
        tags$div(
          tags$h4("Wykres słupkowy (poprawny)"),
          lc_plot("ch3_disc_bar", max_height = "300px")
        ),
        tags$div(
          tags$h4("Histogram (problematyczny)"),
          lc_plot("ch3_disc_hist", max_height = "300px")
        )
      )
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
      lc_toolbar(
        lc_segmented("ch3_modal_scenario", "Scenariusz",
          choices = c("Unimodalny" = "unimodal", "Bimodalny" = "bimodal",
                      "Wielomodalny" = "multimodal"))
      ),
      lc_plot("ch3_modal_plot", ratio = "1.8/1", max_height = "350px"),
      uiOutput("ch3_modal_text")
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
          )
        ),
        tags$div(class = "lc-pctl-hist",
          tags$canvas(`data-role` = "hist",
            `aria-label` = "Histogram wzrostu z przeciąganą linią percentyla")),
        tags$div(class = "lc-pctl-box",
          tags$canvas(`data-role` = "box",
            `aria-label` = "Wykres pudełkowy wzrostu z zaznaczonym percentylem")),
        lc_caption("Przeciągnij linię percentyla po histogramie."),
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
      histogramu. Kliknij na wykres dwa razy: pierwszy punkt to typowana
      średnia, drugi — mediana. Przed kliknięciem ustal, w którą stronę
      ciągnie ogon rozkładu i po której stronie mediany powinna wtedy
      leżeć średnia."),

    figure_panel(
      label = "Ryc. 3.10",
      title = "Kliknij na wykres, aby umieścić średnią i medianę",
      tags$div(class = "lc-guess",
        lc_toolbar(
          tags$button(type = "button", class = "lc-action is-solid", `data-g-act` = "new", "Nowa runda"),
          tags$button(type = "button", class = "lc-action is-outline", `data-g-act` = "reveal",
                      disabled = NA, "Pokaż odpowiedź")
        ),
        tags$div(`data-role` = "banner", class = "lc-caption", `data-tone` = "info",
                 tags$div("Kliknij na wykresie w miejscu, gdzie powinna leżeć średnia.")),
        tags$div(class = "lc-guess-canvas",
          tags$canvas(`data-role` = "hist",
            `aria-label` = "Histogram losowych danych; kliknięcia wskazują średnią i medianę")),
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

  # --------------------------------------------------------------------------
  # Widget: Histogram krok po kroku
  # --------------------------------------------------------------------------

  # Krok widgetu (1..8) żyje w przeglądarce; zmiana zmiennej nie cofa kroku.
  ch3_hist_step <- lc_step_server("ch3_hist", input)$step

  # Default bin widths per variable
  ch3_hist_defaults <- list(
    wzrost = list(min = 1, max = 15, value = 5, step = 1, unit = "cm"),
    waga = list(min = 2, max = 20, value = 5, step = 1, unit = "kg"),
    czas_dojazdu = list(min = 2, max = 20, value = 5, step = 1, unit = "min"),
    srednia_ocen = list(min = 0.1, max = 1, value = 0.3, step = 0.05, unit = "pkt")
  )

  output$ch3_hist_bin_slider <- renderUI({
    d <- ch3_hist_defaults[[input$ch3_hist_var]]
    lc_slider("ch3_hist_bin_width", "Szerokość binu", d$min, d$max, d$value, d$step)
  })

  # Compute bin breaks
  ch3_hist_breaks <- reactive({
    req(input$ch3_hist_bin_width)
    x <- student_data[[input$ch3_hist_var]]
    w <- input$ch3_hist_bin_width
    start <- floor(min(x) / w) * w
    end <- ceiling(max(x) / w) * w + w
    seq(start, end, by = w)
  })

  # Data with bin assignments
  ch3_hist_binned <- reactive({
    req(input$ch3_hist_bin_width)
    x <- student_data[[input$ch3_hist_var]]
    breaks <- ch3_hist_breaks()
    df <- data.frame(value = x)
    df$bin <- cut(df$value, breaks = breaks, include.lowest = TRUE, right = FALSE)
    df$bin_num <- as.numeric(df$bin)
    df
  })

  # Bin statistics
  ch3_hist_stats <- reactive({
    df <- ch3_hist_binned()
    breaks <- ch3_hist_breaks()
    all_bins <- data.frame(
      bin_start = breaks[-length(breaks)],
      bin_end = breaks[-1]
    )
    all_bins$bin_mid <- (all_bins$bin_start + all_bins$bin_end) / 2
    all_bins$bin_num <- seq_len(nrow(all_bins))

    counts <- df %>%
      filter(!is.na(bin)) %>%
      group_by(bin_num) %>%
      summarise(count = n(), .groups = "drop")
    all_bins <- all_bins %>% left_join(counts, by = "bin_num")
    all_bins$count[is.na(all_bins$count)] <- 0

    # Trim to relevant range
    min_d <- min(all_bins$bin_num[all_bins$count > 0])
    max_d <- max(all_bins$bin_num[all_bins$count > 0])
    all_bins %>%
      filter(bin_num >= max(1, min_d - 1),
             bin_num <= min(nrow(all_bins), max_d + 1))
  })

  # Obserwacje z numerem w stosie swojego binu (kroki 5–6)
  ch3_hist_stacked <- reactive({
    stats <- ch3_hist_stats()
    ch3_hist_binned() %>%
      filter(!is.na(bin)) %>%
      arrange(bin_num, value) %>%
      group_by(bin_num) %>%
      mutate(k = row_number()) %>%
      ungroup() %>%
      inner_join(stats %>% select(bin_num, bin_start, bin_end, bin_mid), by = "bin_num")
  })

  # Kropka mniejsza, gdy stosy są wysokie, żeby się nie zlewały
  ch3_hist_dot_size <- function(max_count) max(0.8, min(2.2, 60 / max_count))

  # Oś X z granicami binów (co druga podpisana)
  ch3_hist_edge_axis <- function(breaks) {
    scale_x_continuous(breaks = breaks[seq(1, length(breaks), by = 2)],
                       labels = function(v) lc_fmt(v, 2), minor_breaks = NULL)
  }

  # Variable labels
  ch3_hist_var_labels <- c(
    "wzrost" = "Wzrost (cm)", "waga" = "Waga (kg)",
    "czas_dojazdu" = "Czas dojazdu (min)",
    "srednia_ocen" = "Średnia ocen"
  )

  zoom_plot_server("ch3_hist_plot", reactive({
    step <- ch3_hist_step()
    var_name <- input$ch3_hist_var
    req(var_name)
    x <- student_data[[var_name]]
    x_label <- ch3_hist_var_labels[var_name]
    n <- length(x)

    x_lo <- min(x) - diff(range(x)) * 0.05
    x_hi <- max(x) + diff(range(x)) * 0.05

    strip_theme <- theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
    # Dwie grupy przynależności do binów: naprzemiennie dane / grupa.
    bin_role_colour <- function(bin_num) {
      ifelse(bin_num %% 2 == 1, STEP_ROLES$data$colour, STEP_ROLES$group$colour)
    }

    if (step == 1) {
      df <- data.frame(value = x)
      ggplot(df, aes(x = value, y = 0)) +
        step_layer(geom_jitter, "data", height = 0.3, size = 3) +
        labs(x = x_label, y = "") + strip_theme +
        step_frame(xlim = c(x_lo, x_hi), ylim = c(-0.5, 0.5))

    } else if (step == 2) {
      # Wartości po kolei: płasko tam, gdzie dane leżą gęsto, stromo na rzadkich końcach.
      df <- data.frame(i = seq_along(x), value = sort(x))
      ggplot(df, aes(x = i, y = value)) +
        step_layer(geom_point, "data", size = 2) +
        labs(x = "Numer obserwacji po posortowaniu", y = x_label) +
        theme(panel.grid.minor = element_blank(), legend.position = "none")

    } else if (step %in% 3:4) {
      # Przedziały jako pasy na zmianę w dwóch kolorach; w kroku 4 punkty biorą kolor pasa.
      breaks <- ch3_hist_breaks()
      bands <- data.frame(xmin = breaks[-length(breaks)], xmax = breaks[-1],
                          bin_num = seq_len(length(breaks) - 1)) %>%
        filter(xmax > x_lo, xmin < x_hi)
      df <- ch3_hist_binned() %>% filter(!is.na(bin))
      set.seed(11)
      df$jy <- runif(nrow(df), -0.3, 0.3)
      w <- input$ch3_hist_bin_width
      mid_bin <- bands[ceiling(nrow(bands) / 2), ]
      point_colour <- if (step == 3) upwr_ink_soft else bin_role_colour(df$bin_num)

      p <- ggplot() +
        geom_rect(data = bands, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                  fill = bin_role_colour(bands$bin_num), alpha = if (step == 3) 0.18 else 0.12) +
        geom_vline(xintercept = breaks, colour = upwr_ink_soft, linewidth = 0.3) +
        geom_point(data = df, aes(x = value, y = jy), colour = point_colour,
                   alpha = if (step == 3) 0.55 else 0.9, size = 2.4) +
        ch3_hist_edge_axis(breaks) +
        labs(x = x_label, y = "") + strip_theme +
        step_frame(xlim = c(x_lo, x_hi), ylim = c(-0.45, if (step == 3) 0.8 else 0.45))
      if (step == 3) {
        p <- p +
          annotate("segment", x = mid_bin$xmin, xend = mid_bin$xmax, y = 0.55, yend = 0.55,
                   colour = STEP_ROLES$new$colour, linewidth = 0.9,
                   arrow = arrow(ends = "both", length = unit(4, "pt"))) +
          annotate("text", x = (mid_bin$xmin + mid_bin$xmax) / 2, y = 0.7,
                   label = paste("szerokość", lc_fmt(w, 2), ch3_hist_defaults[[var_name]]$unit),
                   colour = STEP_ROLES$new$colour, size = 4, fontface = "bold")
      }
      p

    } else if (step == 5) {
      # Stosy kropek: każda obserwacja staje w kolumnie swojego binu.
      stats <- ch3_hist_stats()
      df <- ch3_hist_stacked()
      ggplot() +
        geom_vline(xintercept = ch3_hist_breaks(), colour = upwr_rule, linewidth = 0.3) +
        geom_point(data = df, aes(x = bin_mid, y = k - 0.5), colour = bin_role_colour(df$bin_num),
                   size = ch3_hist_dot_size(max(stats$count))) +
        geom_text(data = filter(stats, count > 0), aes(x = bin_mid, y = count + max(stats$count) * 0.05, label = count),
                  size = 3.8, fontface = "bold", family = lc_mono_family, colour = STEP_ROLES$known$colour) +
        ch3_hist_edge_axis(ch3_hist_breaks()) +
        labs(x = x_label, y = "Liczba obserwacji") +
        theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank()) +
        step_frame(xlim = c(x_lo, x_hi), ylim = c(0, max(stats$count) * 1.15))

    } else if (step == 6) {
      # Kafelki: każda obserwacja to prostokąt pełnej szerokości binu; razem tworzą słupek.
      stats <- ch3_hist_stats()
      df <- ch3_hist_stacked()
      ggplot() +
        geom_rect(data = df, aes(xmin = bin_start, xmax = bin_end, ymin = k - 1, ymax = k),
                  fill = STEP_ROLES$data$colour, alpha = 0.55, colour = "white", linewidth = 0.25) +
        geom_rect(data = filter(stats, count > 0),
                  aes(xmin = bin_start, xmax = bin_end, ymin = 0, ymax = count),
                  fill = NA, colour = STEP_EDGE$colour, linewidth = 0.45) +
        geom_text(data = filter(stats, count > 0), aes(x = bin_mid, y = count + max(stats$count) * 0.05, label = count),
                  size = 3.8, fontface = "bold", family = lc_mono_family, colour = STEP_ROLES$known$colour) +
        ch3_hist_edge_axis(ch3_hist_breaks()) +
        labs(x = x_label, y = "Liczba obserwacji") +
        theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank()) +
        step_frame(xlim = c(x_lo, x_hi), ylim = c(0, max(stats$count) * 1.15))

    } else if (step == 7) {
      # Trzy szerokości pod sobą: najwęższa z suwaka, wybrana, najszersza.
      d <- ch3_hist_defaults[[var_name]]
      w <- input$ch3_hist_bin_width
      widths <- unique(c(d$min, w, d$max))
      notes <- c("najwęższa", "z suwaka", "najszersza")[match(widths, c(d$min, w, d$max))]
      cols <- ifelse(widths == w, upwr_accent, upwr_ink_soft)
      df <- data.frame(value = x)
      plots <- lapply(seq_along(widths), function(i) {
        ggplot(df, aes(x = value)) +
          geom_histogram(binwidth = widths[i], boundary = floor(min(x) / w) * w, closed = "left",
                         fill = cols[i], alpha = 0.75, colour = "white", linewidth = 0.3) +
          labs(x = if (i == length(widths)) x_label else NULL, y = NULL,
               subtitle = paste0("szerokość ", lc_fmt(widths[i], 2), " ", d$unit, " (", notes[i], ")")) +
          coord_cartesian(xlim = c(x_lo, x_hi)) +
          theme_upwr(base_size = 12, base_family = theme_get()$text$family) +
          theme(panel.grid.minor = element_blank(),
                plot.margin = margin(t = 4, r = 12, b = 2, l = 8),
                plot.subtitle = element_text(colour = cols[i], face = "bold", size = 12,
                                             margin = margin(b = 2)))
      })
      gridExtra::arrangeGrob(grobs = plots, ncol = 1)
    }
  }))

  output$ch3_hist_text <- renderUI({
    step <- ch3_hist_step()
    var_name <- input$ch3_hist_var
    req(var_name)
    x <- student_data[[var_name]]
    n <- length(x)
    unit <- ch3_hist_defaults[[var_name]]$unit

    txt <- switch(as.character(step),
      "1" = paste0("Mamy ", n, " obserwacji — każdy punkt to jedna wartość. ",
                   "Trudno z tego odczytać rozkład, prawda?"),
      "2" = paste0("Sortujemy wartości od najmniejszej (", lc_fmt(min(x), 1), " ", unit,
                   ") do największej (", lc_fmt(max(x), 1), " ", unit, ") i rysujemy je po kolei. ",
                   "Tam, gdzie wykres jest płaski, wartości leżą gęsto; strome końce to rzadkie wartości skrajne."),
      "3" = paste0("Dzielimy oś na równe przedziały (biny) o szerokości ",
                   lc_fmt(input$ch3_hist_bin_width, 2), " ", unit,
                   ". Każdy bin to „koszyk” na obserwacje."),
      "4" = "Każda obserwacja trafia do swojego binu i przyjmuje jego kolor.",
      "5" = "Każda obserwacja ląduje w kolumnie swojego binu i staje na poprzedniej. Wysokość stosu to liczba obserwacji w binie.",
      "6" = "Stos zamieniamy w słupek tej samej wysokości. Każdy kafelek to jedna obserwacja, a słupek to ich liczba. To już jest histogram.",
      "7" = paste0("Te same dane przy trzech szerokościach binu: najwęższej, wybranej suwakiem i najszerszej. ",
                   "Za wąskie biny dają poszarpany wykres, za szerokie ukrywają kształt.")
    )
    txt
  })

  output$ch3_hist_table <- renderUI({
    if (ch3_hist_step() < 5) return(NULL)
    stats <- ch3_hist_stats()
    n <- length(student_data[[input$ch3_hist_var]])

    result <- stats %>% filter(count > 0) %>%
      mutate(pct = round(count / n * 100, 1))
    out <- data.frame(
      bin = paste0("[", result$bin_start, ", ", result$bin_end, ")"),
      count = result$count,
      pct = result$pct
    )
    lc_table(out, cols = list(
      lc_col("bin", "Przedział", "row"),
      lc_col("count", "Liczba obs."),
      lc_col("pct", "Procent (%)", digits = 1)
    ))
  })

  # --------------------------------------------------------------------------
  # Widget 0a: Mean introduction
  # --------------------------------------------------------------------------

  zoom_plot_server("ch3_mean_plot", reactive({
    var_name <- input$ch3_mean_var
    req(var_name)
    x <- student_data[[var_name]]
    m <- mean(x)
    var_labels <- c("wzrost" = "Wzrost (cm)", "waga" = "Waga (kg)",
                    "srednia_ocen" = "Średnia ocen")
    df <- data.frame(val = x)

    ggplot(df, aes(x = val)) +
      geom_histogram(bins = 25, fill = upwr_rule, color = "white", alpha = 0.8) +
      geom_vline(xintercept = m, color = upwr_accent, linewidth = 1.5, linetype = "solid") +
      annotate("text", x = m, y = Inf, label = paste0("Średnia = ", round(m, 2)),
               vjust = 2, hjust = -0.1, color = upwr_accent, size = 5, fontface = "bold") +
      annotate("segment", x = min(x), xend = m, y = -0.5, yend = -0.5,
               color = upwr_cat["niebo"], linewidth = 2,
               arrow = arrow(length = unit(0.2, "cm"), ends = "last")) +
      annotate("segment", x = max(x), xend = m, y = -0.5, yend = -0.5,
               color = upwr_cat["niebo"], linewidth = 2,
               arrow = arrow(length = unit(0.2, "cm"), ends = "last")) +
      labs(x = var_labels[var_name], y = "Liczebność") +
      theme()
  }))

  output$ch3_mean_text <- renderUI({
    var_name <- input$ch3_mean_var
    req(var_name)
    x <- student_data[[var_name]]
    m <- mean(x)
    s <- sum(x)
    n <- length(x)
    lc_status(
      withMathJax(paste0(
        "$$\\bar{x} = \\frac{", round(s, 1), "}{", n, "} = ", round(m, 2), "$$"
      ))
    )
  })

  # --------------------------------------------------------------------------
  # Widget 0b: Median introduction
  # --------------------------------------------------------------------------

  zoom_plot_server("ch3_median_plot", reactive({
    var_name <- input$ch3_median_var
    req(var_name)
    x <- student_data[[var_name]]
    med <- median(x)
    x_sorted <- sort(x)
    n <- length(x_sorted)
    n_below <- sum(x_sorted < med)
    n_above <- sum(x_sorted > med)
    var_labels <- c("wzrost" = "Wzrost (cm)", "czas_dojazdu" = "Czas dojazdu (min)",
                    "srednia_ocen" = "Średnia ocen")
    df <- data.frame(val = x)

    ggplot(df, aes(x = val)) +
      geom_histogram(bins = 25, fill = upwr_rule, color = "white", alpha = 0.8) +
      geom_vline(xintercept = med, color = upwr_cat["indygo"], linewidth = 1.5) +
      annotate("rect", xmin = min(x) - 1, xmax = med, ymin = -Inf, ymax = Inf,
               fill = upwr_cat["niebo"], alpha = 0.08) +
      annotate("rect", xmin = med, xmax = max(x) + 1, ymin = -Inf, ymax = Inf,
               fill = upwr_accent, alpha = 0.08) +
      annotate("text", x = (min(x) + med) / 2, y = Inf,
               label = paste0("50% (", n_below, " obs.)"),
               vjust = 2, color = upwr_secondary, size = 5, fontface = "bold") +
      annotate("text", x = (max(x) + med) / 2, y = Inf,
               label = paste0("50% (", n_above, " obs.)"),
               vjust = 2, color = upwr_secondary, size = 5, fontface = "bold") +
      annotate("text", x = med, y = Inf, label = paste0("Me = ", round(med, 1)),
               vjust = 4, hjust = -0.1, color = upwr_cat["indygo"], size = 5, fontface = "bold") +
      geom_histogram(bins = 25, fill = upwr_rule, color = "white", alpha = 0.8) +
      geom_vline(xintercept = med, color = upwr_cat["indygo"], linewidth = 1.5) +
      labs(x = var_labels[var_name], y = "Liczebność") +
      theme()
  }))

  output$ch3_median_text <- renderUI({
    var_name <- input$ch3_median_var
    req(var_name)
    x <- student_data[[var_name]]
    med <- median(x)
    m <- mean(x)
    diff <- abs(m - med)

    lc_status(
      paste0("Mediana = ", round(med, 1)),
      " | Średnia = ",
      round(m, 2),
      " | Różnica = ",
      round(diff, 2)
    )
  })

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

  # --------------------------------------------------------------------------
  # Widget 2b: Discrete variables

  zoom_plot_server("ch3_disc_bar", reactive({
    var_name <- input$ch3_disc_var
    req(var_name)
    vals <- student_data[[var_name]]
    df <- data.frame(x = factor(vals))

    ggplot(df, aes(x = x)) +
      geom_bar(fill = type_colors["ilosciowa_dyskretna"], color = "white", alpha = 0.85) +
      geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 4) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(x = variable_meta[[var_name]]$label, y = "Liczebność") +
      theme()
  }))

  zoom_plot_server("ch3_disc_hist", reactive({
    var_name <- input$ch3_disc_var
    req(var_name)
    vals <- student_data[[var_name]]

    ggplot(data.frame(x = vals), aes(x = x)) +
      geom_histogram(bins = 15, fill = upwr_accent, color = "white", alpha = 0.6) +
      labs(x = variable_meta[[var_name]]$label, y = "Liczebność") +
      theme()
  }))

  output$ch3_disc_stats <- renderUI({
    var_name <- input$ch3_disc_var
    req(var_name)
    vals <- student_data[[var_name]]

    mode_val <- as.numeric(names(sort(table(vals), decreasing = TRUE))[1])

    tagList(
      lc_readout("Średnia", lc_fmt(mean(vals), 2)),
      lc_readout("Mediana", median(vals)),
      lc_readout("Moda", mode_val),
      lc_readout("SD", lc_fmt(sd(vals), 2)),
      lc_readout("Zakres", paste0(min(vals), "–", max(vals)))
    )
    })

  # Widget 2c: Multimodality in continuous distributions
  # --------------------------------------------------------------------------

  zoom_plot_server("ch3_modal_plot", reactive({
    scenario <- input$ch3_modal_scenario
    req(scenario)

    set.seed(42)
    if (scenario == "unimodal") {
      x <- rnorm(500, mean = 165, sd = 6)
      df <- data.frame(val = x)
      ggplot(df, aes(x = val)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30,
                       fill = upwr_cat["niebo"], color = "white", alpha = 0.7) +
        geom_density(linewidth = 1.2, color = upwr_secondary) +
        geom_vline(xintercept = mean(x), color = upwr_accent, linewidth = 1, linetype = "dashed") +
        annotate("text", x = mean(x) + 1, y = Inf, label = "moda ≈ średnia ≈ mediana",
                 hjust = 0, vjust = 2, color = upwr_accent, size = 4.5, fontface = "bold") +
        labs(x = "Wzrost kobiet (cm)", y = "Gęstość") +
        theme()

    } else if (scenario == "bimodal") {
      x_k <- rnorm(250, mean = 162, sd = 5)
      x_m <- rnorm(250, mean = 182, sd = 5)
      df <- data.frame(val = c(x_k, x_m),
                       grupa = rep(c("Kobiety", "Mężczyźni"), each = 250))
      ggplot(df, aes(x = val)) +
        geom_histogram(aes(y = after_stat(density)), bins = 35,
                       fill = upwr_reference, color = "white", alpha = 0.5) +
        geom_density(linewidth = 1.2, color = upwr_secondary) +
        geom_density(aes(color = grupa), linewidth = 0.8, linetype = "dashed") +
        scale_color_manual(values = c("Kobiety" = upwr_accent, "Mężczyźni" = upwr_col("niebo"))) +
        labs(x = "Wzrost (cm)", y = "Gęstość", color = NULL) +
                theme(legend.position = "top")

    } else {
      x1 <- rnorm(150, mean = 12, sd = 3)
      x2 <- rnorm(120, mean = 25, sd = 4)
      x3 <- rnorm(130, mean = 40, sd = 5)
      df <- data.frame(val = c(x1, x2, x3),
                       grupa = c(rep("Rower", 150), rep("Autobus", 120), rep("Auto", 130)))
      ggplot(df, aes(x = val)) +
        geom_histogram(aes(y = after_stat(density)), bins = 40,
                       fill = upwr_reference, color = "white", alpha = 0.5) +
        geom_density(linewidth = 1.2, color = upwr_secondary) +
        geom_density(aes(color = grupa), linewidth = 0.8, linetype = "dashed") +
        scale_color_manual(values = c("Rower" = upwr_col("szalwia"), "Autobus" = upwr_col("bursztyn"), "Auto" = upwr_accent)) +
        labs(x = "Czas dojazdu (min)", y = "Gęstość", color = NULL) +
                theme(legend.position = "top")
    }
  }))

  output$ch3_modal_text <- renderUI({
    scenario <- input$ch3_modal_scenario
    req(scenario)

    if (scenario == "unimodal") {
      lc_status(
        tags$b("Rozkład unimodalny: "),
        "jeden szczyt. ",
        "Dla rozkładu symetrycznego moda ≈ średnia ≈ mediana. ",
        "Większość statystyk opisowych zakłada właśnie taki rozkład."
      )
    } else if (scenario == "bimodal") {
      lc_status(
        lc_verdict(tags$b("Rozkład bimodalny: "), type = "warning"),
        "dwa szczyty, osobno dla kobiet ",
        "i mężczyzn. Średnia całości wypada między szczytami, ",
        "gdzie obserwacji jest niewiele."
      )
    } else {
      lc_status(
        lc_verdict(tags$b("Rozkład wielomodalny: "), type = "warning"),
        "trzy szczyty = trzy podgrupy. ",
        "Każda podgrupa (rowerzyści, pasażerowie autobusów, kierowcy) ",
        "ma własną typową wartość."
      )
    }
  })

}
