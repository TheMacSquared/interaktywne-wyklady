# ============================================================================
# Ryciny wykładu 08: opisy liczone w R przy budowaniu strony
# ============================================================================
# Wszystkie zbiory wykładu są stałe (helpers.R, set.seed(2025)), więc wykresy
# ggplot dawnego serwera zamienia na opis ryciny lc_sf_ggplot() (shared/lc_sf_ggplot.R),
# a przełączniki widoku wybierają gotowe warianty. Interakcje liczone w przeglądarce
# są w modules/figures.js.
# Wymaga: helpers.R, diagramy.R, lc_stat_figures.R, lc_sf_ggplot.R.

s08_html <- function(...) as.character(tagList(...))

# Rycina z przełącznikiem widoku: wykres i tabela każdego wariantu z R.
# plots / tables: nazwane listy (nazwa = wartość przełącznika).
s08_view_figure <- function(control, plots, tables, aria, ratio = "1.4/1", max_height = "280px") {
  lc_sf_ggplot(plots, aria = aria, control = control,
    html = lapply(tables, function(x) list(table = s08_html(x))),
    body = lc_plots(tags$div(`data-role` = "table"), lc_sf_chart(0, ratio, max_height)))
}

# Rycina bez przełącznika: tabela danych obok wykresu.
s08_static_figure <- function(plot, table, aria, ratio = "1.4/1", max_height = "280px") {
  lc_sf_ggplot(plot, aria = aria, body = lc_plots(tags$div(table), lc_sf_chart(0, ratio, max_height)))
}

# Ryc. 1.3: tabela i dwa małe wykresy jeden pod drugim.
s08_cat2_figure <- function() {
  a <- sf_ggplot(cat2_plot_zadowolenie_gg(), aria = "Rozkład ocen zadowolenia (1–5).")
  b <- sf_ggplot(cat2_plot_gg(), aria = "Wynagrodzenie a staż pracy.")
  lc_sf_spec(list(charts = list(a, b)), charts = 2,
    body = lc_plots(tags$div(cat2_table_html()),
                    tags$div(lc_sf_chart(0, "2/1", "200px"), lc_sf_chart(1, "2/1", "200px"))))
}

# Lista kontrolna jakości danych: pola wyboru i pasek oceny w jednym kontenerze
# (pasek liczy przeglądarka, rycina s08_checklist).
s08_checklist <- function(...) {
  tags$div(class = "lc-sf", `data-fig` = "s08_checklist", `data-lc-nochart` = NA,
    `data-config` = as.character(jsonlite::toJSON(list(colors = list(bad = data_bad, mixed = data_mixed, good = data_good)), auto_unbox = TRUE)),
    ...)
}

# ---------------------------------------------------------------------------
# Rozdział 2 (CASchools)
# ---------------------------------------------------------------------------
s08_tab1_vars <- c("read", "math", "expenditure", "income", "english", "lunch", "students", "teachers", "calworks")

# Ryc. 2.2: histogram i podsumowanie (summary()) każdej zmiennej.
s08_tab1_hist_figure <- function() {
  vars <- stats::setNames(s08_tab1_vars, s08_tab1_vars)
  lc_sf_ggplot(lapply(vars, tab1_hist_gg), aria = "Histogram wybranej zmiennej CASchools.",
    control = selectInput("tab1_var", "Zmienna", choices = .ch2_choices(s08_tab1_vars), selectize = FALSE),
    html = lapply(vars, function(v) list(summary = s08_html(tags$pre(class = "shiny-text-output",
      paste(utils::capture.output(summary(CASchools[[v]])), collapse = "\n"))))),
    ratio = "2.2/1", max_height = "300px", after = tags$div(`data-role` = "summary"))
}

# Ryc. 2.3: wykres rozrzutu z prostą dla każdej pary X, Y.
s08_tab1_scatter_figure <- function() {
  xs <- c("expenditure", "income", "english", "lunch", "calworks", "students"); ys <- c("read", "math")
  grid <- expand.grid(x = xs, y = ys, stringsAsFactors = FALSE)
  plots <- lapply(seq_len(nrow(grid)), function(i) suppressMessages(tab1_scatter_plot_gg(c(grid$x[i], grid$y[i]))))
  names(plots) <- paste0(grid$x, "|", grid$y)
  lc_sf_ggplot(plots, aria = "Wynik testu a wybrana cecha okręgu z prostą regresji.",
    control = list(
      selectInput("tab1_x", "Zmienna X", choices = .ch2_choices(xs), selected = "income", selectize = FALSE),
      selectInput("tab1_y", "Zmienna Y", choices = .ch2_choices(ys), selected = "read", selectize = FALSE)),
    key = c("tab1_x", "tab1_y"), ratio = "2/1", max_height = "350px")
}

# ---------------------------------------------------------------------------
# Rozdział 3: liczba obserwacji a precyzja i moc (rycina s08_n)
# ---------------------------------------------------------------------------
# Dawniej: set.seed(42); godziny <- rnorm(n, 15, 5); oceny <- rnorm(n, 3.8, 0.6).
# rnorm() z jednego ziarna to jeden strumień, więc oceny dla danego n to wartości
# n+1…2n strumienia z: przeglądarka bierze je z config.z.
s08_n_config <- local({
  set.seed(42)
  z <- rnorm(400)
  ns <- 5:200
  list(z = round(z, 8),
       power = round(vapply(ns, function(k) power.t.test(n = floor(k / 2), delta = 0.5, sd = 1)$power, 0), 6))
})
s08_n_figure <- function() {
  lc_sf_figure("s08_n", config = s08_n_config,
    control = lc_sf_slider("tab2_n", "Liczba obserwacji", 5, 200, 8, 1),
    body = tagList(lc_plots(lc_sf_chart(0, "1.4/1", "280px"), lc_sf_chart(1, "1.4/1", "280px")),
                   lc_sf_chart(2, "2.2/1", "280px")))
}

# ---------------------------------------------------------------------------
# Rozdział 5: quiz „Jaka analiza tu pasuje?” (kafelki, rycina s08_tiles)
# ---------------------------------------------------------------------------
s08_tab4_quiz <- function() {
  choices <- list(
    list(letter = "A", value = "Test t", text = "Test t"),
    list(letter = "B", value = "Korelacja", text = "Korelacja"),
    list(letter = "C", value = "Regresja", text = "Regresja"),
    list(letter = "D", value = "Zadna z klasycznych", text = "Żadna z klasycznych"))
  result <- function(answer) s08_html(if (answer == "Zadna z klasycznych") {
    lc_status(lc_verdict(tags$strong("Dokładnie."), type = "ok"),
      " Wiersz to zdarzenie, a nie niezależna obserwacja, więc żaden
      z klasycznych testów nie pasuje do tabeli w tej postaci.")
  } else {
    lc_status(lc_verdict(tags$strong("Nie do końca."), type = "danger"),
      paste0(" ", answer, " wymaga niezależnych obserwacji i zmiennych
      mierzonych na tej samej jednostce. Tu wiersz to jedno przekleństwo
      albo jedna śmierć. Poprawna odpowiedź: „Żadna z klasycznych”."))
  })
  tags$div(class = "lc-sf", `data-fig` = "s08_tiles", `data-lc-nochart` = NA,
    `data-config` = as.character(jsonlite::toJSON(list(results = stats::setNames(
      lapply(choices, function(o) result(o$value)), vapply(choices, `[[`, "", "value"))), auto_unbox = TRUE)),
    tags$div(class = "quiz-tiles quiz-cols-4", `data-role` = "tiles",
      lapply(choices, function(opt) tags$button(type = "button", class = "btn btn-default quiz-tile",
        `data-stat-act` = paste0("ans:", opt$value),
        tags$div(class = "tile-letter", opt$letter), tags$div(class = "tile-text", opt$text)))),
    tags$div(`data-role` = "result"))
}

# ---------------------------------------------------------------------------
# Rozdział 9: ćwiczenie „Błąd czy prawdziwa wartość odstająca?” (rycina s08_quiz5)
# ---------------------------------------------------------------------------
s08_quiz5 <- function(...) {
  config <- list(
    correct = c("Błąd danych", "Błąd danych", "Błąd danych", "Błąd danych", "Prawdziwa wartość odstająca"),
    explanations = c(
      "Ujemna hemoglobina jest fizycznie niemożliwa. Prawdopodobnie minus pojawił się przez błąd klawiatury lub importu. Błąd danych.",
      "Hemoglobina 1420 g/dL — norma to 12–17 g/dL. Zgubiona kropka dziesiętna: powinno być 14.20. Błąd danych (błąd zapisu).",
      "Ujemne ciśnienie tętnicze jest niemożliwe fizjologicznie. Znak minus musiał pojawić się przez błąd wprowadzania. Błąd danych.",
      "Glukoza 11 000 mg/dL — norma to 70–110 mg/dL, a nawet w śpiączce cukrzycowej rzadko przekracza 1000. Powinno być 110 mg/dL (3 zera za dużo). Błąd danych.",
      "Glukoza 310 mg/dL jest wysoka, ale medycznie możliwa — taki poziom zdarza się u pacjentów z niekontrolowaną cukrzycą. Reszta parametrów wygląda spójnie. Prawdziwa wartość odstająca: warto ją odnotować, ale nie usuwać."))
  tags$div(class = "lc-sf", `data-fig` = "s08_quiz5", `data-lc-nochart` = NA,
    `data-config` = as.character(jsonlite::toJSON(config, auto_unbox = TRUE)), ...)
}
