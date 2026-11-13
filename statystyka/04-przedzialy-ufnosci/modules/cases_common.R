# ============================================================================
# PRZYKŁADY Z HIPOTEZĄ (rozdz. 3 i 4): wspólna rycina lcSF „s4-case” (sf_examples.js)
# Dane przykładów są stałe, więc próby, przedziały, zakresy osi, napisy
# i werdykty liczymy w R, przy budowie UI. Przeglądarka rysuje krok budowy,
# obszar wybranej hipotezy i odsłania werdykt — także offline.
# Przykłady średnich: ch3_srednia.R; proporcji: ch4_proporcja.R.
# ============================================================================

# Werdykt hipotezy: dir = "gt" (CI > bound) / "lt" (CI < bound) → "yes" / "no" / "maybe".
s4_case_verdict <- function(lower, upper, bound, dir) {
  if (dir == "gt") {
    if (lower > bound) "yes" else if (upper < bound) "no" else "maybe"
  } else {
    if (upper < bound) "yes" else if (lower > bound) "no" else "maybe"
  }
}

# Margines coord_cartesian() (5% zakresu z każdej strony).
s4_expand <- function(l, mult = 0.05) l + c(-1, 1) * diff(l) * mult

# Losowanie z ziarnem bez zmiany globalnego stanu RNG.
s4_with_seed <- function(seed, code) {
  old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv())
  on.exit(if (is.null(old)) rm(".Random.seed", envir = globalenv())
          else assign(".Random.seed", old, envir = globalenv()))
  set.seed(seed)
  code
}

s4_is_pairwise <- function(h) identical(h$kind, "pairwise")

# Zakres osi x bez hipotezy ("0") i z każdą hipotezą ("1", "2"): raw — zakres
# z R przed marginesem (tam stoją napisy przy krawędzi), x — z marginesem.
s4_lims_by_hyp <- function(hyps, f) {
  one <- function(h) { raw <- f(h); list(raw = raw, x = s4_expand(raw)) }
  out <- list("0" = one(NULL))
  for (i in seq_along(hyps)) {
    out[[as.character(i)]] <- one(if (s4_is_pairwise(hyps[[i]])) NULL else hyps[[i]])
  }
  out
}

# Hipotezy dla JS: granica, kierunek i napis przy linii.
s4_case_hyps <- function(hyps) {
  lapply(hyps, function(h) {
    if (s4_is_pairwise(h)) list(pairwise = TRUE)
    else list(bound = h$bound, dir = h$dir,
              label = paste0(if (h$dir == "gt") "≥ " else "≤ ", h$bound))
  })
}

# Szybka mapa par (✓ rozłączne CI, × nakładające się).
s4_pairwise_table <- function(mat) {
  groups <- rownames(mat)
  k <- length(groups)
  sym <- function(i, j) if (i == j) "—" else if (mat[i, j]) "✓" else "×"
  cls <- function(i, j) if (i == j) "is-dim" else if (mat[i, j]) "is-best" else NA_character_
  df <- data.frame(group = groups)
  cell_class <- list()
  for (j in seq_len(k)) {
    key <- paste0("g", j)
    df[[key]] <- vapply(seq_len(k), function(i) sym(i, j), character(1))
    cell_class[[key]] <- vapply(seq_len(k), function(i) cls(i, j), character(1))
  }
  lc_table(df,
    cols = c(list(lc_col("group", "", "row")),
             lapply(seq_len(k), function(j) lc_col(paste0("g", j), groups[j], "text"))),
    cell_class = cell_class, fit = TRUE)
}

# Opisy pod rycinę: dla każdej hipotezy i — przed odsłonięciem (klucz "h<i>")
# i po kliknięciu „Pokaż werdykt” ("h<i>r"). verdict(h) zwraca "yes"/"no"/"maybe";
# pairwise(h) zwraca treść werdyktu hipotezy „które grupy się różnią”.
s4_case_notes <- function(id, hyps, verdict, pairwise = NULL) {
  notes <- list()
  for (i in seq_along(hyps)) {
    hyp <- hyps[[i]]
    notes[[paste0("h", i)]] <- lc_status(
      p(tags$strong(paste0("Hipoteza ", i, ":")), " ", hyp$text),
      p("Spójrz na wykres: gdzie leży CI względem obszaru hipotezy?
        Co o tym sądzicie?"),
      lc_sf_action("reveal", paste0(id, "_reveal", i), "Pokaż werdykt", variant = "solid")
    )
    notes[[paste0("h", i, "r")]] <- if (s4_is_pairwise(hyp)) pairwise(hyp) else {
      v <- verdict(hyp)
      label <- switch(v, "yes" = "TAK", "no" = "NIE", "maybe" = "NIEPEWNE")
      body <- if (v == "yes" && !is.null(hyp$explain_yes)) {
        p(hyp$explain_yes)
      } else if (v == "no" && !is.null(hyp$explain_no)) {
        p(hyp$explain_no)
      } else if (v == "maybe" && !is.null(hyp$explain_maybe)) {
        p(hyp$explain_maybe)
      } else {
        p("CI przecina granicę hipotezy — nie możemy jednoznacznie
          stwierdzić, czy jest prawdziwa.")
      }
      lc_status(
        p(tags$strong(paste0("Hipoteza ", i, ":")), " ", hyp$text),
        p(tags$strong("Werdykt:"), " ",
          if (v %in% c("yes", "no")) lc_verdict(label, type = if (v == "yes") "ok" else "danger") else label),
        body
      )
    }
  }
  lapply(names(notes), function(k) tags$div(`data-when` = k, hidden = NA, notes[[k]]))
}

# Pojedynczy wykres ryciny (k od 0); bez proporcji, gdy rozciąga się w siatce.
s4_case_chart <- function(k, ratio = NULL, max_height = "460px", style = NULL) {
  if (!is.null(ratio)) return(lc_sf_chart(k, ratio = ratio, max_height = max_height))
  tags$div(class = "lc-sf-chart", `data-k` = k, style = style,
    tags$div(class = "lc-ec", `data-chart` = k, role = "img", `aria-label` = "Wykres"))
}

# Układ wykresów według typu przykładu (jak patchwork w dawnej wersji R).
s4_case_body <- function(type) {
  grid <- function(cols, ...) tags$div(
    style = paste0("display:grid;grid-template-columns:", cols, ";gap:.4em;align-items:stretch"), ...)
  switch(type,
    single_mean = , compare_n = , compare_n_prop = s4_case_chart(0, "2.4/1"),
    forest = , forest_prop = s4_case_chart(0, "1.6/1"),
    diff_means = tagList(s4_case_chart(0, "2.4/1"), s4_case_chart(1, "4.8/1", "200px")),
    # Słupki TAK/NIE | oś proporcji (szerokości 1 : 2.5).
    single_prop = grid("minmax(0,1fr) minmax(0,2.5fr)",
      s4_case_chart(0, style = "min-height:12em"), s4_case_chart(1, "1.71/1")),
    # Słupki dwóch grup | dwie p̂ nad różnicą (szerokości 1 : 2).
    diff_props = grid("minmax(0,1fr) minmax(0,2fr)",
      s4_case_chart(0, style = "grid-row:span 2;min-height:12em"),
      s4_case_chart(1, "2.13/1"), s4_case_chart(2, "2.13/1"))
  )
}

# Widget przykładu: krokowa rycina lcSF; hipotezy jako przełączniki od ostatniego kroku.
s4_case_widget <- function(id, cfg, config, notes) {
  n_core <- length(cfg$steps)
  hyp_n <- seq_along(cfg$hypotheses)
  chips <- lc_chips(paste0(id, "_hyp"), stats::setNames(as.character(hyp_n), paste("Hipoteza", hyp_n)),
                    label = "Sprawdź")
  # Przełącznik obsługuje też rycina (data-stat-act = hyp1, hyp2).
  chips <- htmltools::tagQuery(chips)$find("button")$each(function(el, i)
    el$attribs[["data-stat-act"]] <- paste0("hyp", el$attribs[["data-value"]]))$allTags()
  config$n <- n_core
  config$type <- cfg$type
  config$xlab <- cfg$xlab
  config$hyps <- s4_case_hyps(cfg$hypotheses)
  config$text <- "Przedział gotowy. Wybierz hipotezę, żeby sprawdzić ją na wykresie."
  lc_sf_figure("s4-case", bare = TRUE, config = config,
    body = lc_step_widget(id,
      steps = sub("^[0-9]+\\.\\s*", "", cfg$steps),
      toolbar = lc_toolbar(lc_step_from(n_core, chips), lc_chart_zoom_button()),
      body = s4_case_body(cfg$type),
      extra = tags$div(class = "s4-case-notes", notes)
    ))
}
