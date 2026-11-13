# ============================================================================
# CHAPTER 3: Przedział dla średniej
# ============================================================================

# ============================================================================
# PRZYKŁADY A1–C2 (case studies): rycina lcSF „s4-case” (sf_examples.js)
# Dane przykładów są stałe, więc próby, przedziały, zakresy osi, napisy
# i werdykty liczymy tu, przy budowie UI. Przeglądarka rysuje krok budowy,
# obszar wybranej hipotezy i odsłania werdykt — także offline.
# Wspólne części (s4_case_*) używa też ch4_proporcja.R.
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

# ---- Przykłady rozdziału 3: średnie ----
ch3_case_ci_mean <- function(xbar, s, n, conf = 0.95) {
  t_star <- qt(1 - (1 - conf) / 2, df = n - 1)
  me <- t_star * s / sqrt(n)
  list(lower = xbar - me, upper = xbar + me, me = me, t_star = t_star, se = s / sqrt(n))
}
ch3_case_ci_diff <- function(x1, s1, n1, x2, s2, n2, conf = 0.95) {
  se <- sqrt(s1^2 / n1 + s2^2 / n2)
  df_w <- (s1^2 / n1 + s2^2 / n2)^2 /
          ((s1^2 / n1)^2 / (n1 - 1) + (s2^2 / n2)^2 / (n2 - 1))
  t_star <- qt(1 - (1 - conf) / 2, df = df_w)
  diff <- x1 - x2
  me <- t_star * se
  list(diff = diff, lower = diff - me, upper = diff + me, me = me, df = df_w, se = se, t_star = t_star)
}
# Próba o dokładnie zadanej średniej i odchyleniu.
ch3_case_sample <- function(n, m, s) { x <- rnorm(n, m, s); (x - mean(x)) / sd(x) * s + m }

# Każdy przykład: type, data, xlab, steps (etykiety kroków), hypotheses (lista).
ch3_cases <- list(
  A1 = list(
    type = "single_mean",
    data = list(xbar = 173.4, s = 8.2, n = 30),
    xlab = "Wzrost (cm)",
    steps = c("1. Próba", "2. Średnia", "3. ± SE", "4. Przedział"),
    hypotheses = list(
      list(text = "Średni wzrost przekracza 168 cm",
           bound = 168, dir = "gt",
           explain_yes = "Dolna granica CI (≈ 170.3) leży powyżej 168, cały przedział jest w obszarze hipotezy. Z 95% ufnością średni wzrost w populacji przekracza 168 cm."),
      list(text = "Średni wzrost przekracza 180 cm",
           bound = 180, dir = "gt",
           explain_no = "Górna granica CI (≈ 176.5) leży poniżej 180, cały przedział jest poza obszarem hipotezy. Dane wykluczają średni wzrost powyżej 180 cm.")
    )
  ),
  A2 = list(
    type = "compare_n",
    data = list(xbar = 32.0, s = 8.0, ns = c(10, 50, 200)),
    xlab = "Stężenie (µg/m³)",
    steps = c("1. n = 10", "2. n = 50", "3. n = 200"),
    hypotheses = list(
      list(text = "Stężenie przekracza 25 µg/m³",
           bound = 25, dir = "gt",
           explain_yes = "Wszystkie trzy przedziały leżą powyżej 25, nawet najszerszy (n = 10) ma dolną granicę ≈ 26.3. Każde z badań potwierdza hipotezę, większe n daje tylko węższy przedział."),
      list(text = "Stężenie przekracza 35 µg/m³",
           bound = 35, dir = "gt",
           explain_no = "Werdykt dotyczy najdokładniejszego badania: dla n = 200 górna granica (≈ 33.1) leży poniżej 35. Dla n = 10 przedział sięga do 37.7 i przecina 35, więc samo małe badanie by tego nie rozstrzygnęło.")
    )
  ),
  B1 = list(
    type = "diff_means",
    data = list(x1 = 12.3, s1 = 4.5, n1 = 40, x2 = 4.1, s2 = 4.2, n2 = 40,
                label1 = "Lek", label2 = "Placebo",
                unit = "mmHg", diff_label = "Lek − placebo"),
    xlab = "Obniżenie ciśnienia (mmHg)",
    steps = c("1. Próby", "2. Średnie", "3. Różnica", "4. ± SE", "5. Przedział"),
    hypotheses = list(
      list(text = "Lek skuteczniej obniża ciśnienie niż placebo (różnica > 0)",
           bound = 0, dir = "gt",
           explain_yes = "Cały przedział dla różnicy (≈ 6.3–10.1 mmHg) leży powyżej 0: lek obniża ciśnienie skuteczniej niż placebo."),
      list(text = "Lek działa o więcej niż 12 mmHg lepiej niż placebo",
           bound = 12, dir = "gt",
           explain_no = "Górna granica CI (≈ 10.1) leży poniżej 12. Efekt leku jest wyraźny, ale mniejszy, niż głosi hipoteza.")
    )
  ),
  B2 = list(
    type = "diff_means",
    data = list(x1 = 8.4, s1 = 1.2, n1 = 25, x2 = 8.1, s2 = 1.3, n2 = 25,
                label1 = "Nawóz X", label2 = "Nawóz Y",
                unit = "t/ha", diff_label = "X − Y"),
    xlab = "Plon (t/ha)",
    steps = c("1. Próby", "2. Średnie", "3. Różnica", "4. ± SE", "5. Przedział"),
    hypotheses = list(
      list(text = "Różnica plonów jest mniejsza niż 2 t/ha",
           bound = 2, dir = "lt",
           explain_yes = "Cały przedział dla różnicy (≈ -0.4–1.0 t/ha) leży poniżej 2 t/ha. Nawet jeśli któryś nawóz jest lepszy, przewaga nie sięga 2 t/ha."),
      list(text = "Nawóz X daje plon większy o ponad 2 t/ha niż Y",
           bound = 2, dir = "gt",
           explain_no = "Górna granica CI (≈ 1.0) leży poniżej 2. Przedział obejmuje też wartości ujemne, więc dane nie mówią nawet, który nawóz jest lepszy.")
    )
  ),
  B3 = list(
    type = "diff_means",
    data = list(x1 = 350, s1 = 45, n1 = 150, x2 = 362, s2 = 45, n2 = 150,
                label1 = "Grupa A", label2 = "Grupa B",
                unit = "ms", diff_label = "A − B"),
    xlab = "Czas reakcji (ms)",
    steps = c("1. Próby", "2. Średnie", "3. Różnica", "4. ± SE", "5. Przedział"),
    hypotheses = list(
      list(text = "Grupa A reaguje szybciej niż B (różnica < 0)",
           bound = 0, dir = "lt",
           explain_yes = "Przedziały grup się nakładają, ale przedział dla różnicy (≈ -22.2 do -1.8 ms) leży w całości poniżej 0. Grupa A reaguje szybciej."),
      list(text = "Grupa A jest szybsza o co najmniej 25 ms",
           bound = -25, dir = "lt",
           explain_no = "Dolna granica CI (≈ -22) nie sięga -25, cały przedział leży powyżej tej wartości. Grupa A jest szybsza o około 2–22 ms, nie o 25 ms lub więcej.")
    )
  ),
  B4 = list(
    type = "diff_means",
    data = list(x1 = 100.4, s1 = 15, n1 = 20000, x2 = 100.0, s2 = 15, n2 = 20000,
                label1 = "Wojew. A", label2 = "Wojew. B",
                unit = "pkt IQ", diff_label = "A − B"),
    xlab = "IQ (punkty)",
    steps = c("1. Próby", "2. Średnie", "3. Różnica", "4. ± SE", "5. Przedział"),
    hypotheses = list(
      list(text = "Województwo A ma wyższe średnie IQ niż B (różnica > 0)",
           bound = 0, dir = "gt",
           explain_yes = "Przy n = 20 000 w każdej grupie przedział (≈ 0.11–0.69 pkt) jest bardzo wąski i nie obejmuje 0. Różnica jest istotna statystycznie."),
      list(text = "Różnica wynosi co najmniej 1 punkt IQ",
           bound = 1, dir = "gt",
           explain_no = "Cały przedział leży poniżej 1 (górna granica ≈ 0.7). Różnica 0.4 punktu IQ to około 0.03 SD.")
    )
  ),
  C1 = list(
    type = "forest",
    data = list(
      groups = c("Tradycyjna", "E-learning", "Flipped class", "Tutoring"),
      means  = c(28.5, 30.2, 31.8, 33.4),
      sds    = c(5.2, 5.8, 5.5, 4.9),
      ns     = c(25, 25, 25, 25)
    ),
    xlab = "Średni wynik egzaminu (0–40 pkt)",
    steps = c("1. Punkty", "2. Średnie", "3. CI"),
    hypotheses = list(
      list(kind = "pairwise",
           text = "Które metody nauczania różnią się istotnie?",
           unit = "pkt")
    )
  ),
  C2 = list(
    type = "forest",
    data = list(
      groups = c("Kardiologia", "Neurologia", "Ortopedia", "Pulmonologia", "SOR"),
      means  = c(22, 28, 25, 31, 75),
      sds    = c(8, 10, 9, 11, 25),
      ns     = c(60, 55, 70, 50, 80)
    ),
    xlab = "Średni czas oczekiwania (min)",
    steps = c("1. Punkty", "2. Średnie", "3. CI"),
    hypotheses = list(
      list(kind = "pairwise",
           text = "Które oddziały różnią się istotnie czasem oczekiwania?",
           unit = "min")
    )
  )
)

# Dane ryciny dla przykładu (punkty z ustalonymi ziarnami, przedziały, osie, napisy).
ch3_case_config <- function(cfg) {
  d <- cfg$data; hyps <- cfg$hypotheses
  r4 <- function(x) round(x, 4)
  switch(cfg$type,
    single_mean = {
      ci <- ch3_case_ci_mean(d$xbar, d$s, d$n)
      samp <- s4_with_seed(42, ch3_case_sample(d$n, d$xbar, d$s))
      jit <- s4_with_seed(7, runif(d$n, min = 0.15, max = 0.55))
      lim <- function(h) {
        xl <- range(c(samp, d$xbar - 1.2 * ci$me, d$xbar + 1.2 * ci$me, h$bound))
        xl + c(-1, 1) * diff(xl) * 0.05
      }
      list(xbar = d$xbar, se = ci$se, lo = ci$lower, hi = ci$upper,
           pts = unname(cbind(r4(samp), r4(jit))), y = s4_expand(c(-0.55, 0.75)),
           lims = s4_lims_by_hyp(hyps, lim),
           lab_mean = paste0("x̄ = ", round(d$xbar, 2)), lab_se = paste0("± SE = ±", round(ci$se, 2)),
           lab_ci = paste0("95% CI: [", round(ci$lower, 2), " ; ", round(ci$upper, 2), "]"))
    },
    compare_n = {
      cis <- lapply(d$ns, function(n) ch3_case_ci_mean(d$xbar, d$s, n))
      lo <- vapply(cis, `[[`, 0, "lower"); hi <- vapply(cis, `[[`, 0, "upper")
      lim <- function(h) {
        xl <- range(c(min(lo), max(hi), h$bound))
        xl + c(-1, 1) * diff(xl) * 0.1
      }
      list(xbar = d$xbar, lo = lo, hi = hi, y = s4_expand(c(0.3, length(d$ns) + 0.7)),
           lims = s4_lims_by_hyp(hyps, lim),
           lab_n = paste0("n = ", d$ns),
           lab_ci = paste0("[", round(lo, 2), " ; ", round(hi, 2), "]"))
    },
    diff_means = {
      cid <- ch3_case_ci_diff(d$x1, d$s1, d$n1, d$x2, d$s2, d$n2)
      # Ta sama kolejność losowań co w dawnej wersji R; przy n > 80 losowa podpróba.
      samp <- s4_with_seed(11, {
        s1 <- ch3_case_sample(d$n1, d$x1, d$s1)
        set.seed(17)
        s2 <- ch3_case_sample(d$n2, d$x2, d$s2)
        if (d$n1 > 80) s1 <- sample(s1, 80)
        if (d$n2 > 80) s2 <- sample(s2, 80)
        list(s1, s2)
      })
      jit1 <- s4_with_seed(42, runif(length(samp[[1]]), 1.55, 2.05))
      jit2 <- s4_with_seed(43, runif(length(samp[[2]]), 0.75, 1.25))
      top <- range(unlist(samp))
      top <- top + c(-1, 1) * diff(top) * 0.06
      lim <- function(h) {
        xl <- range(c(0, cid$diff - 1.3 * cid$me, cid$diff + 1.3 * cid$me, h$bound))
        xl + c(-1, 1) * diff(xl) * 0.1
      }
      list(x1 = d$x1, x2 = d$x2, label1 = d$label1, label2 = d$label2,
           pts1 = unname(cbind(r4(samp[[1]]), r4(jit1))), pts2 = unname(cbind(r4(samp[[2]]), r4(jit2))),
           top = list(raw = top, x = s4_expand(top)), y = s4_expand(c(0.35, 2.25)),
           diff = cid$diff, se = cid$se, lo = cid$lower, hi = cid$upper,
           y_bot = s4_expand(c(-0.55, 0.65)), lims = s4_lims_by_hyp(hyps, lim),
           xlab_bot = paste0("Różnica (", d$unit, ")  —  ", d$diff_label),
           lab_x1 = paste0("x̄₁ = ", round(d$x1, 2)), lab_x2 = paste0("x̄₂ = ", round(d$x2, 2)),
           lab_diff = paste0("x̄₁ − x̄₂ = ", round(cid$diff, 2)),
           lab_se = paste0("± SE = ±", round(cid$se, 2)),
           lab_ci = paste0("95% CI: [", round(cid$lower, 2), " ; ", round(cid$upper, 2), "]"))
    },
    forest = {
      k <- length(d$groups)
      cis <- lapply(seq_len(k), function(i) ch3_case_ci_mean(d$means[i], d$sds[i], d$ns[i]))
      lo <- vapply(cis, `[[`, 0, "lower"); hi <- vapply(cis, `[[`, 0, "upper")
      pts <- do.call(rbind, lapply(seq_len(k), function(i) {
        samp <- s4_with_seed(50 + i, {
          x <- ch3_case_sample(d$ns[i], d$means[i], d$sds[i])
          if (d$ns[i] > 60) x <- sample(x, 60)
          x
        })
        jit <- s4_with_seed(100 + i, runif(length(samp), min = i - 0.25, max = i + 0.25))
        cbind(r4(samp), r4(jit))
      }))
      lim <- function(h) {
        xl <- range(c(lo, hi, h$bound))
        xl + c(-1, 1) * diff(xl) * 0.12
      }
      list(groups = d$groups, means = d$means, lo = lo, hi = hi, pts = unname(pts),
           y = s4_expand(c(0.3, k + 0.7)), lims = s4_lims_by_hyp(hyps, lim))
    }
  )
}

# Werdykt i szybka mapa par dla przykładów rozdziału 3.
ch3_case_verdict <- function(cfg) function(hyp) {
  d <- cfg$data
  ci <- switch(cfg$type,
    single_mean = ch3_case_ci_mean(d$xbar, d$s, d$n),
    # Werdykt dla najdokładniejszego badania (największe n).
    compare_n   = ch3_case_ci_mean(d$xbar, d$s, max(d$ns)),
    diff_means  = ch3_case_ci_diff(d$x1, d$s1, d$n1, d$x2, d$s2, d$n2))
  s4_case_verdict(ci$lower, ci$upper, hyp$bound, hyp$dir)
}

# TRUE: rozłączne 95% CI — mocny sygnał różnicy; FALSE: nakładanie nie rozstrzyga.
ch3_case_pairs <- function(d) {
  k <- length(d$groups)
  cis <- lapply(seq_len(k), function(i) ch3_case_ci_mean(d$means[i], d$sds[i], d$ns[i]))
  m <- matrix(FALSE, nrow = k, ncol = k, dimnames = list(d$groups, d$groups))
  for (i in seq_len(k)) for (j in seq_len(k)) {
    if (i == j) next
    m[i, j] <- (cis[[i]]$upper < cis[[j]]$lower) || (cis[[j]]$upper < cis[[i]]$lower)
  }
  m
}

# Narracja „jak w raporcie” dla mapy par.
ch3_case_narrative <- function(d, mat, unit = "") {
  groups <- d$groups
  means <- d$means
  k <- length(groups)
  unit_str <- if (nzchar(unit)) paste0(" ", unit) else ""

  # Pary z rozłącznymi CI z górnego trójkąta.
  diff_pairs <- list()
  for (i in seq_len(k - 1)) for (j in seq(i + 1, k)) {
    if (mat[i, j]) {
      diff_pairs[[length(diff_pairs) + 1]] <- if (means[i] > means[j]) {
        list(hi = groups[i], lo = groups[j])
      } else {
        list(hi = groups[j], lo = groups[i])
      }
    }
  }
  n_diff <- length(diff_pairs)

  if (n_diff == 0) {
    return(paste0(
      "Żadna para nie ma rozłącznych 95% CI. Ta szybka ocena nie wskazała ",
      "oczywistych różnic, ale nakładanie się przedziałów nie dowodzi ich braku. ",
      "Aby rozstrzygnąć konkretną parę, policz przedział dla różnicy średnich."
    ))
  }

  pair_str <- function(pp) paste0(pp$hi, " > ", pp$lo)
  join_pairs <- function(strs) {
    if (length(strs) == 1) strs
    else paste0(paste(strs[-length(strs)], collapse = ", "), " oraz ", strs[length(strs)])
  }

  # Czy jedna grupa odstaje od wszystkich innych (np. SOR wobec reszty)?
  standout_idx <- which(sapply(seq_len(k), function(i) all(mat[i, -i])))
  if (length(standout_idx) == 1) {
    i <- standout_idx
    others <- means[-i]
    direction <- if (means[i] > max(others)) "wyższą" else "niższą"
    rest_pairs <- Filter(function(pp) pp$hi != groups[i] && pp$lo != groups[i], diff_pairs)
    rest_txt <- if (length(rest_pairs) == 0) {
      ", a ich CI nakładają się — ten wykres nie rozstrzyga różnic między nimi."
    } else {
      paste0(". Wśród nich rozłączne CI mają jeszcze: ",
             join_pairs(sapply(rest_pairs, pair_str)),
             "; pozostałe pary się nakładają.")
    }
    return(paste0(
      "Spośród wszystkich badanych grup wyraźnie odstaje ",
      groups[i], " (średnia ", round(means[i], 1), unit_str,
      ") — szybkie porównanie wskazuje na ", direction, " wartość niż w każdej z pozostałych grup ",
      "(jej 95% CI nie nakłada się z żadnym innym). ",
      "Pozostałe grupy mają średnie w przedziale ",
      round(min(others), 1), "–", round(max(others), 1), unit_str,
      rest_txt
    ))
  }

  intro <- if (n_diff == 1) {
    "Spośród wszystkich porównań jedynie jedna para ma rozłączne 95% CI: "
  } else {
    paste0("Rozłączne 95% CI, wskazujące wyraźne różnice, mają ", n_diff, " pary: ")
  }
  paste0(
    intro, join_pairs(sapply(diff_pairs, pair_str)), ". ",
    "Dla pozostałych par 95% CI nakładają się. To wynik nierozstrzygający, ",
    "a nie dowód braku różnicy; konkretną parę sprawdzamy przedziałem dla różnicy średnich."
  )
}

ch3_case_ui <- function(case_id) {
  cfg <- ch3_cases[[case_id]]
  id <- paste0("ch3_case", case_id)
  pairwise <- function(hyp) {
    mat <- ch3_case_pairs(cfg$data)
    lc_status(
      p(tags$strong("Hipoteza:"), " ", hyp$text),
      p(tags$strong("Szybka mapa porównań:")),
      lc_caption("✓ = rozłączne CI: wyraźny sygnał różnicy; ",
                 "× = CI nakładają się: wykres nie rozstrzyga"),
      s4_pairwise_table(mat),
      p(tags$strong("Jak to opisać na tym etapie:")),
      p(tags$em(HTML(ch3_case_narrative(cfg$data, mat, if (!is.null(hyp$unit)) hyp$unit else ""))))
    )
  }
  s4_case_widget(id, cfg, ch3_case_config(cfg),
                 s4_case_notes(id, cfg$hypotheses, ch3_case_verdict(cfg), pairwise))
}

# ============================================================================
# PRZYKŁADY A–C: CI grup a CI różnicy — rycina lcSF „s04-comp” (sf_steps.js)
# Dane są stałe, więc przedziały i werdykty liczymy tu, przy budowie UI;
# przeglądarka rysuje oba wykresy (także offline).
# ============================================================================

# --- Dane (statyczne, przygotowane z ustalonymi seedami) ---
ch3_comp_data <- list(
  A = list(
    g1_name = "Dostawca A",
    g2_name = "Dostawca B",
    unit    = "zawartość białka (%)",
    g1 = c(13.17,11.08,11.38,11.55,11.22,11.23,12.25,11.73,11.89,13.11,
           12.01,13.43,13.17,11.99,12.94,12.08,11.26,11.62,11.80,12.39,
           12.30,12.22,12.58,10.97,12.56,11.91,12.25,12.16,11.21,11.63,
           11.28,12.23,11.87,11.75,11.55,11.46,12.40,11.14,11.71,11.99),
    g2 = c(11.63,10.48,10.73,10.11,10.67,10.66,11.71,11.25,10.96,11.46,
           10.74,10.90,11.12,11.92,11.33,11.19, 9.96,11.09,11.00,10.36,
           10.95,11.00,11.23,11.32,11.09,11.57,11.36,11.59,11.66,11.32,
           11.16,10.35,10.53,10.38, 9.92,10.10,10.37,10.57,10.86,12.35)
  ),
  B = list(
    g1_name = "Szkło",
    g2_name = "Plastik",
    unit    = "zawartość tłuszczu (%)",
    g1 = c(3.03,3.19,2.80,2.84,3.47,2.95,3.51,3.34,3.17,2.93,
           2.97,3.09,2.80,3.12,2.89,3.18,3.12,3.40,3.03,3.02,
           3.01,3.18,3.07,3.27,3.20,3.18,3.13,2.99,3.12,2.93),
    g2 = c(2.93,2.98,3.38,2.82,2.99,3.33,3.16,3.60,3.06,3.12,
           2.80,3.22,3.43,2.99,3.43,3.12,2.66,3.43,3.39,3.26,
           3.41,3.15,3.01,3.33,3.25,3.35,3.17,3.32,3.58,3.23)
  ),
  C = list(
    g1_name = "Linia 1",
    g2_name = "Linia 2",
    unit    = "zawartość błonnika (g / 100 g)",
    # Wygenerowane: set.seed(3); round(rnorm(120, 9.3, 1.0), 2), round(rnorm(120, 9.0, 1.0), 2)
    g1 = c(8.34,8.99,10.62,7.52,9.70,10.56,10.25, 9.04, 8.49,10.13,
           7.94,10.82,10.78, 7.98,11.47, 9.16,10.82, 8.87, 7.83, 8.53,
           8.35,12.44, 9.78, 9.70,11.17, 8.54, 9.25, 8.43,10.52, 8.06,
           8.64, 8.22,10.67, 9.71, 9.80,10.42,10.19,11.20,10.62, 9.83,
           8.61, 9.36, 9.62, 9.18, 9.76, 9.53,10.91,10.57, 9.08, 9.46,
           10.25, 7.21, 9.78, 9.64, 8.19,10.27, 9.46, 9.29,10.25, 7.76,
           11.24, 9.04, 9.85,10.18,11.01, 8.86, 9.24, 9.28, 9.09, 8.29,
           9.87,10.10, 9.17, 9.41,11.07, 9.18, 9.19,10.50, 9.79, 8.65,
           8.85, 9.22,10.81,10.12, 8.36, 8.92, 8.70,10.63, 9.81, 9.07,
           9.44, 9.43, 8.67,11.13, 7.88,10.15, 9.56, 9.78, 8.75, 9.82,
           8.51, 8.41, 9.77, 9.08, 7.55, 9.04,10.43, 7.98, 7.29, 8.58,
           8.12, 8.16, 9.74, 9.25, 8.69,10.36, 7.22,10.15, 9.52, 8.95),
    g2 = c(8.30,10.15, 9.77, 7.74, 9.45, 7.68, 7.64, 9.92, 9.36, 8.28,
           9.14,10.19, 9.85, 7.83, 9.09, 9.45, 7.63, 9.09, 8.52, 9.26,
           8.20,10.05,11.76, 8.75,10.03, 9.95, 9.54, 8.60, 8.70, 7.74,
           8.94, 9.74, 9.41,10.51, 7.75, 7.99, 9.08, 9.20, 8.13, 7.76,
           8.31, 9.10, 9.40,10.24, 9.16,11.00, 9.25,10.82, 8.96, 9.42,
           8.10,10.32, 9.01, 8.35, 9.36,10.20, 9.71, 7.18, 8.33, 8.24,
           9.56, 8.10, 8.83, 9.32, 8.00, 8.33, 8.20,10.19,10.94, 8.35,
           8.87, 7.86, 9.56, 9.09,10.51, 9.30, 7.26, 8.50, 8.16, 8.30,
           8.45, 8.04,10.66, 7.19,10.16, 8.73, 8.61, 9.17, 8.85, 9.57,
           9.28, 9.64,10.54, 8.79,11.00,10.24, 9.75,10.32, 8.96, 7.31,
           9.35, 8.64, 9.17,10.40, 7.64, 9.23, 9.86, 9.59, 9.48, 9.27,
           8.32, 8.92, 9.76, 9.57, 8.72, 8.07, 7.55, 9.74, 9.43, 9.32)
  )
)

# Helper: liczy CI dla dwóch grup + CI różnicy (Welch)
ch3_comp_cis <- function(g1, g2, conf = 0.95) {
  alpha <- 1 - conf
  n1 <- length(g1); n2 <- length(g2)
  m1 <- mean(g1);   m2 <- mean(g2)
  s1 <- sd(g1);     s2 <- sd(g2)
  se1 <- s1 / sqrt(n1); se2 <- s2 / sqrt(n2)
  ci1 <- m1 + c(-1, 1) * qt(1 - alpha / 2, n1 - 1) * se1
  ci2 <- m2 + c(-1, 1) * qt(1 - alpha / 2, n2 - 1) * se2
  se_d <- sqrt(se1^2 + se2^2)
  df_w <- (se1^2 + se2^2)^2 / (se1^4 / (n1 - 1) + se2^4 / (n2 - 1))
  ci_d <- (m1 - m2) + c(-1, 1) * qt(1 - alpha / 2, df_w) * se_d
  list(m1 = m1, m2 = m2, ci1 = ci1, ci2 = ci2,
       md = m1 - m2, ci_d = ci_d,
       overlap_lo = max(ci1[1], ci2[1]),
       overlap_hi = min(ci1[2], ci2[2]))
}

# Helper: werdykt tekstowy
ch3_comp_verdict <- function(scenario_key) {
  dat <- ch3_comp_data[[scenario_key]]
  cis <- ch3_comp_cis(dat$g1, dat$g2)
  overlap_present <- cis$overlap_lo <= cis$overlap_hi
  diff_excludes_0 <- !(cis$ci_d[1] <= 0 & 0 <= cis$ci_d[2])
  overlap_w <- if (overlap_present) cis$overlap_hi - cis$overlap_lo else 0

  fmt <- function(x) sprintf("%.2f", x)
  ci_txt <- function(ci) paste0("[", fmt(ci[1]), "; ", fmt(ci[2]), "]")

  # Same fakty liczbowe; wnioski ze scenariuszy są w narracji pod nimi.
  lc_status(
    p(lc_verdict(tags$b("Co widzimy:"), type = if (scenario_key == "C") "warning" else "ok")),
    tags$ul(
      tags$li(dat$g1_name, ": średnia ", fmt(cis$m1),
              ", 95% CI ", ci_txt(cis$ci1)),
      tags$li(dat$g2_name, ": średnia ", fmt(cis$m2),
              ", 95% CI ", ci_txt(cis$ci2)),
      tags$li("CI różnicy (", dat$g1_name, " − ", dat$g2_name, "): ",
              ci_txt(cis$ci_d))
    ),
    tags$ul(
      tags$li("Czy CI grup się nakrywają? ",
              tags$b(if (overlap_present)
                paste0("TAK (na odcinku szerokości ", fmt(overlap_w), ")")
              else "NIE")),
      tags$li("Czy CI różnicy zawiera 0? ",
              tags$b(if (diff_excludes_0) "NIE" else "TAK"))
    )
  )
}

# Dwa wykresy jeden pod drugim (jak patchwork w R): CI grup i CI różnicy.
ch3_comp_figure <- function(key) {
  dat <- ch3_comp_data[[key]]
  cis <- ch3_comp_cis(dat$g1, dat$g2)
  tagList(
    htmltools::tagAppendAttributes(
      lc_sf_figure("s04-comp",
        config = list(g1_name = dat$g1_name, g2_name = dat$g2_name, unit = dat$unit,
                      m1 = cis$m1, m2 = cis$m2, ci1 = cis$ci1, ci2 = cis$ci2,
                      md = cis$md, ci_d = cis$ci_d),
        body = tagList(lc_sf_chart(0, ratio = "3.6/1", max_height = "170px"),
                       lc_sf_chart(1, ratio = "3.6/1", max_height = "170px"))),
      `data-scenario` = key),
    ch3_comp_verdict(key)
  )
}

ch3_ui <- list(
  id    = "ch-srednia",
  num   = "03",
  title = "Przedział dla średniej",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 03 · Przedziały ufności",
      num    = "03",
      title  = "Przedział dla średniej.",
      lead   = "Przedział dla średniej składa się z trzech liczb: średniej z próby,
                błędu standardowego i wartości krytycznej. Gdy odchylenie
                standardowe populacji trzeba oszacować z danych, wartość krytyczną
                bierzemy z rozkładu t-Studenta, a nie z rozkładu normalnego."
    ),

    lc_p("W poprzednim rozdziale przedziały ufności pojawiały się jako gotowe
      siatki: scena mierzyła grupkę, zarzucała przedział i sprawdzała, czy
      złapał μ. Teraz zajrzymy do środka. Zobaczymy, z czego składa się
      przedział dla średniej, zbudujemy go krok po kroku, a potem rozszerzymy
      tę samą konstrukcję na różnicę dwóch średnich."),

    lc_h2("ch3-wzor", "Wzór"),

    lc_p("Punktem wyjścia jest ", gloss("centralne twierdzenie graniczne"), " z wykładu 03.
      Średnia z próby \\(\\bar{x}\\) leży w około 95% prób nie dalej niż
      1.96 błędu standardowego \\(\\sigma/\\sqrt{n}\\) od średniej ", gloss("populacja", "populacji"), " μ.
      Skoro μ rzadko leży dalej od \\(\\bar{x}\\) niż 1.96 błędu standardowego,
      możemy odwrócić to zdanie: zakres \\(\\bar{x} \\pm 1.96 \\cdot \\sigma/\\sqrt{n}\\)
      obejmuje μ w 95% prób. Przeszkoda jest jedna: σ populacji zwykle nie znamy."),

    lc_p("Zastępujemy je więc ", gloss("odchylenie standardowe", "odchyleniem
      standardowym"), " z próby \\(s\\), a mnożnik 1.96 wartością \\(t^*\\) z ",
      gloss("rozkład t-Studenta", "rozkładu t-Studenta"), ". Ten rozkład jest podobny
      do normalnego, ale ma cięższe ogony, więc lepiej oddaje niepewność, zwłaszcza
      przy małej liczbie obserwacji. Tak powstaje ", gloss("przedział ufności"),
      " dla średniej populacji:"),

    lc_formula_annotated(
      "⟦1|CI⟧ = ⟦2|\\bar{x}⟧ \\pm ⟦3|t^*⟧ \\cdot \\frac{⟦4|s⟧}{\\sqrt{⟦5|n⟧}}",
      notes  = list("przedział ufności", "średnia z próby", "wartość krytyczna",
                    "odchylenie standardowe", "liczba obserwacji"),
      colors = c("niebo", "accent", "terakota", "wrzos", "szalwia"),
      above  = c(2, 4)
    ),

    lc_p("Środkiem przedziału jest średnia z próby \\(\\bar{x}\\). Jego połowę
      szerokości, czyli ", gloss("margines błędu"), " \\(ME = t^* \\cdot SE\\),
      wyznaczają dwa czynniki. Błąd standardowy mówi, jak bardzo średnia
      waha się z próby na próbę. ", gloss("wartość krytyczna", "Wartość krytyczna"),
      " \\(t^*\\) mówi, ile błędów standardowych trzeba odłożyć w każdą stronę,
      żeby osiągnąć wybrany ", gloss("poziom ufności"), ". Dla 95% jest to
      około 2, a dokładną wartość podaje program. Warto jednak raz zobaczyć,
      jak powstaje przedział."),

    # Rozkład t a normalny, wartości krytyczne
    figure_panel(
      label = "Ryc. 3.1", title = "Rozkład t ma cięższe ogony",
      width_mode = "text",
      lc_sf_figure("s04-tnorm", ratio = "2/1", max_height = "380px"),
      lc_caption("t* dla 95%: 2.78 przy n = 5, 2.06 przy n = 25, 1.96 w rozkładzie normalnym.")
    ),

    lc_h2("ch3-budowa", "Budowa przedziału — krok po kroku"),

    lc_p("Panel składa przedział z próby 25 pomiarów wzrostu losowanej
      z populacji o średniej μ = 170 cm i odchyleniu standardowym σ = 10 cm.
      Kolejne kroki dodają do wykresu średnią, pasek ±1 SE i pełny 95% przedział.
      Przycisk „Nowa próba” losuje kolejne 25 osób."),

    figure_panel(
      label = "Ryc. 3.2",
      full_width = TRUE,
      lc_step_widget("ch3_step",
        title = "Konstruowanie przedziału",
        steps = c("Próba", "Średnia", "± SE", "Przedział"),
        toolbar = lc_toolbar(
          lc_action("ch3_step_new_sample", "Nowa próba", icon = "shuffle",
                    variant = "outline"), lc_chart_zoom_button()
        ),
        body = ci_construction_ui("mean")
      )
    ),

    lc_p("Pojedyncze pomiary rozrzucone są szeroko, zwykle o kilka do
      kilkunastu centymetrów od 170 cm. Średnia z 25 pomiarów jest dużo
      stabilniejsza: przy σ = 10 cm jej błąd standardowy wynosi około
      \\(10/\\sqrt{25} = 2\\) cm, pięć razy mniej niż rozrzut pojedynczych
      osób. Uśrednianie znosi większość przypadkowych odchyleń w górę i w dół."),

    lc_p("Pasek ±1 SE obejmuje μ tylko w około dwóch trzecich prób. Żeby
      osiągnąć 95%, mnożymy SE przez \\(t^*\\) = 2.06 (df = 24). Przy SE
      równym około 2 cm margines błędu wynosi około 4.1 cm, a cały przedział
      ma około 8 cm szerokości. Większa część tego poszerzenia wynika
      z wyboru poziomu ufności: dla 95% potrzeba około dwóch błędów
      standardowych, a nie jednego. Każda nowa próba daje inną średnią, inne
      \\(s\\) i inny przedział. Tak jak w scenie z siatkami z poprzedniego rozdziału,
      mniej więcej co dwudziesty z nich ominie μ = 170 cm."),

    lc_h2("ch3-roznica", "Budowa przedziału dla różnicy średnich"),

    lc_p("Pojedyncza średnia rzadko jest celem badania. Częściej porównujemy
      dwie grupy: mężczyzn i kobiety, lek i placebo, dwóch dostawców.
      Interesuje nas wtedy różnica średnich populacji \\(\\mu_1 - \\mu_2\\),
      a jej estymatą jest różnica średnich z prób \\(\\bar{x}_1 - \\bar{x}_2\\).
      Obie średnie mają własną niepewność. Dla niezależnych prób
      ", gloss("wariancja", "wariancje"), " ", gloss("estymator", "estymatorów"), " się dodają, więc błąd standardowy różnicy to
      pierwiastek z sumy kwadratów błędów standardowych obu średnich:"),

    lc_formula_annotated(
      "CI = (⟦1|\\bar{x}_1 - \\bar{x}_2⟧) \\pm ⟦2|t^*⟧ \\cdot \\sqrt{\\frac{⟦3|s_1^2⟧}{⟦4|n_1⟧} + \\frac{s_2^2}{n_2}}",
      notes  = list("różnica średnich z prób", "wartość krytyczna",
                    "wariancja grupy 1", "liczebność grupy 1"),
      colors = c("accent", "terakota", "wrzos", "szalwia"),
      above  = c(1, 3)
    ),

    lc_p("Każda grupa ma tu własne odchylenie standardowe, nie zakładamy
      równych wariancji. To wersja Welcha i tak liczą ją panele w tym wykładzie."),

    lc_p("Panel porównuje wzrost 25 mężczyzn i 25 kobiet. Próby losowane są
      z populacji o średnich 178 cm (σ = 7 cm) i 165 cm (σ = 6 cm), więc
      prawdziwa różnica wynosi 13 cm. Od trzeciego kroku dolny panel przechodzi
      na skalę różnicy, na której zero oznacza brak różnicy."),

    figure_panel(
      label = "Ryc. 3.3",
      full_width = TRUE,
      lc_step_widget("ch3_dstep",
        title = "Konstruowanie CI dla różnicy",
        steps = c("Dwie próby", "Dwie średnie", "Różnica", "± SE", "Przedział"),
        toolbar = lc_toolbar(
          lc_action("ch3_dstep_new_sample", "Nowe próby", icon = "shuffle",
                    variant = "outline"), lc_chart_zoom_button()
        ),
        body = ci_construction_ui("diff")
      )
    ),

    lc_p("Przy tych parametrach błąd standardowy różnicy wynosi około
      \\(\\sqrt{7^2/25 + 6^2/25} \\approx 1.84\\) cm, liczba stopni swobody
      Welcha około 47, a \\(t^* \\approx 2.01\\). Margines błędu to około
      3.7 cm, więc przedział leży typowo w okolicach 9–17 cm, daleko od zera.
      Przy tak dużej różnicy wniosek, że mężczyźni są średnio wyżsi, nie
      zależy od tego, którą próbę wylosujemy."),

    lc_p("Warto zauważyć jedną rzecz. Błędy standardowe obu średnich to
      około 1.4 cm i 1.2 cm, razem 2.6 cm. Błąd standardowy różnicy jest
      mniejszy, bo dodają się wariancje, a nie błędy standardowe:
      \\(\\sqrt{1.4^2 + 1.2^2} \\approx 1.84\\). Działa to jak
      w trójkącie prostokątnym: przeciwprostokątna jest krótsza niż suma
      przyprostokątnych. Ta własność ma praktyczne
      skutki przy porównywaniu przedziałów dwóch grup na wykresie."),

    lc_h2("ch3-scenariusze", "Dwa CI grup czy CI różnicy? — trzy scenariusze"),

    lc_p("Wyniki dla dwóch grup można odczytać na dwa sposoby. Pierwszy to
      narysować przedział ufności dla każdej grupy osobno i sprawdzić, czy
      się nakrywają. Drugi to policzyć przedział dla różnicy
      \\(\\mu_1 - \\mu_2\\) i sprawdzić, czy zawiera zero. Zwykle oba
      sposoby prowadzą do tego samego wniosku, ale nie zawsze. Trzy przykłady
      z technologii żywności pokazują oba spojrzenia na tych samych danych.
      Górny wykres to przedziały grup z zaznaczonym na żółto odcinkiem, na
      którym się nakrywają, dolny to przedział różnicy (wersja Welcha)."),

    # --- Scenariusz A ---
    figure_panel(
      label = "Przykład A",
      title = "Dwaj dostawcy mąki — zgodne sygnały, różnica istotna",
      p("Zakład piekarniczy porównuje dwóch dostawców mąki pszennej typu 550
        pod względem zawartości białka (%). Pobrano po 40 partii od każdego dostawcy."),
      ch3_comp_figure("A")
    ),

    # --- Scenariusz B ---
    figure_panel(
      label = "Przykład B",
      title = "Jogurt w szkle i w plastiku — zgodne sygnały, brak różnicy",
      p("Technolog sprawdza, czy materiał opakowania wpływa na zawartość tłuszczu (%)
        w jogurcie naturalnym po 7 dniach przechowywania. Po 30 próbek z każdego typu."),
      ch3_comp_figure("B")
    ),

    # --- Scenariusz C (pułapka) ---
    figure_panel(
      label = "Przykład C",
      title = "Dwie linie płatków — pułapka wzrokowa",
      p("Zakład sprawdza, czy dwie linie produkcyjne płatków śniadaniowych
        dają produkt o tej samej zawartości błonnika (g / 100 g).
        Po 120 partii z każdej linii."),
      ch3_comp_figure("C")
    ),

    lc_p("W scenariuszu A oba spojrzenia się zgadzają. Przedziały dostawców,
      [11.76, 12.16] i [10.80, 11.15], są rozłączne, a przedział różnicy
      [0.72, 1.25] leży w całości powyżej zera. Mąka dostawcy A zawiera
      średnio o 0.7–1.3 punktu procentowego więcej białka."),

    lc_p("W scenariuszu B też nie ma sprzeczności, choć wynik jest bliski
      granicy. Przedziały dla szkła [3.03, 3.17] i plastiku [3.11, 3.28]
      nakładają się, a przedział różnicy [-0.21, 0.01] obejmuje zero, choć
      jego górna granica leży tuż nad nim. Dane nie dają podstaw, by twierdzić,
      że opakowanie wpływa na zawartość tłuszczu. Nie dowodzą jednak, że
      wpływu nie ma: zgodne z nimi są zarówno brak różnicy, jak i różnica
      około 0.2 punktu procentowego na korzyść plastiku."),

    lc_p("Scenariusz C to pułapka. Przedziały linii, [9.24, 9.61] i [8.91, 9.25],
      nakładają się na odcinku zaledwie 0.01 g, ale się nakładają. Patrząc na nie,
      łatwo uznać, że linie produkują podobne płatki. Tymczasem przedział
      różnicy [0.09, 0.59] nie zawiera zera: linia 1 daje płatki bogatsze
      w błonnik. Źródłem rozbieżności jest własność, którą widzieliśmy przy
      wzroście. Błąd standardowy różnicy to"),

    lc_formula_annotated(
      "⟦1|SE_{różnicy}⟧ = \\sqrt{⟦2|SE_1⟧^2 + ⟦3|SE_2⟧^2} \\;<\\; SE_1 + SE_2",
      notes  = list("błąd standardowy różnicy", "błąd standardowy grupy 1",
                    "błąd standardowy grupy 2"),
      colors = c("wrzos", "niebo", "bursztyn"),
      above  = 2
    ),

    lc_p("W scenariuszu C błędy standardowe grup wynoszą 0.093 i 0.086 g.
      Ich suma to 0.179, a pierwiastek z sumy kwadratów tylko 0.127. Dlatego
      połowa szerokości przedziału różnicy (0.25 g) jest wyraźnie mniejsza
      niż suma połówek przedziałów obu grup (0.36 g). Przedziały grup mogą
      się więc lekko nakładać, mimo że różnica jest istotna. Rozłączne
      przedziały grup są mocnym sygnałem różnicy, ale nakładające się nie
      rozstrzygają niczego."),

    lc_note("Zasada", rule = TRUE,
      "Porównując dwie grupy, patrz na przedział różnicy. Nakładanie się
       przedziałów grup nie dowodzi, że różnicy nie ma."
    ),

    lc_p("Na Ryc. 3.4 można rozsuwać średnie dwóch linii płatków ze scenariusza C.
      Warto znaleźć ustawienie, w którym przedziały grup jeszcze się nakładają,
      a przedział różnicy już nie obejmuje zera."),

    ci_explorer("difference"),

    lc_h2("ch3-case-studies", "Case studies — jak interpretować CI w praktyce"),

    lc_p("Na koniec kilka realistycznych sytuacji do samodzielnej analizy.
      W każdej przedział buduje się w tych samych krokach co wyżej. Po
      ostatnim kroku można wybrać hipotezę: na wykresie pojawia się
      zacieniowany obszar wartości, które ją spełniają. Jeśli cały przedział
      leży w tym obszarze, dane potwierdzają hipotezę. Jeśli cały leży poza
      nim, dane ją wykluczają. Jeśli przedział przecina granicę, dane nie
      rozstrzygają. Przed kliknięciem „Pokaż werdykt” warto odpowiedzieć samodzielnie.
      Przypadki A dotyczą jednej średniej, B różnicy dwóch średnich, a C
      wielu grup naraz."),

    lc_h3("Przedział dla jednej średniej", num = "A"),

    figure_panel(
      label = "Przykład A1",
      title = "Wzrost studentów — czytanie pojedynczego CI",
      p("Zmierzono wzrost 30 studentów. Średnia z próby wynosi ",
        withMathJax("\\(\\bar{x} = 173.4\\)"), " cm, odchylenie standardowe ",
        withMathJax("\\(s = 8.2\\)"), " cm. Zbudujmy przedział dla średniego
        wzrostu i sprawdźmy dwie hipotezy."),
      ch3_case_ui("A1")
    ),

    figure_panel(
      label = "Przykład A2",
      title = "Ten sam pomiar, trzy różne wielkości próby",
      p("Trzy badania mierzą stężenie zanieczyszczenia (µg/m³). Wszystkie
        dały średnią 32.0 i s = 8.0, ale różnią się liczebnością próby:
        n = 10, 50 i 200. Kolejne kroki dodają przedziały jeden po drugim."),
      ch3_case_ui("A2")
    ),

    lc_h3("Przedział dla różnicy średnich", num = "B"),

    figure_panel(
      label = "Przykład B1",
      title = "Test leku na ciśnienie — CI dla różnicy nie obejmuje 0",
      p("Badamy nowy lek na obniżenie ciśnienia krwi. ",
        tags$b("Lek:"), " n = 40, średnie obniżenie 12.3 mmHg, s = 4.5. ",
        tags$b("Placebo:"), " n = 40, średnie obniżenie 4.1 mmHg, s = 4.2."),
      ch3_case_ui("B1")
    ),

    figure_panel(
      label = "Przykład B2",
      title = "Dwa nawozy — CI dla różnicy obejmuje 0",
      p("Porównano plon kukurydzy dla dwóch nawozów. ",
        tags$b("Nawóz X:"), " n = 25, średnia 8.4 t/ha, s = 1.2. ",
        tags$b("Nawóz Y:"), " n = 25, średnia 8.1 t/ha, s = 1.3."),
      ch3_case_ui("B2")
    ),

    figure_panel(
      label = "Przykład B3",
      title = "Pułapka nakładających się CI",
      p("Zmierzono czas reakcji w dwóch grupach po 150 osób. ",
        tags$b("Grupa A:"), " średnia 350 ms, s = 45. ",
        tags$b("Grupa B:"), " średnia 362 ms, s = 45. Przedziały
        obu grup nakładają się. Czy różnica jest istotna?"),
      ch3_case_ui("B3")
    ),

    figure_panel(
      label = "Przykład B4",
      title = "Istotne statystycznie ≠ ważne praktycznie",
      p("Bardzo duże badanie porównuje IQ w dwóch województwach. ",
        tags$b("Wojew. A:"), " n = 20 000, średnia 100.4, s = 15. ",
        tags$b("Wojew. B:"), " n = 20 000, średnia 100.0, s = 15.
        Różnica 0.4 pkt IQ — dużo czy mało?"),
      ch3_case_ui("B4")
    ),

    lc_h3("Wiele grup — forest plot", num = "C"),

    figure_panel(
      label = "Przykład C1",
      title = "Cztery metody nauczania — czy któraś wystaje?",
      p("Porównano średni wynik egzaminu (0–40 pkt) studentów uczących
        się czterema metodami, po 25 osób w każdej grupie. Kolejne kroki
        dodają punkty, średnie i przedziały."),
      ch3_case_ui("C1")
    ),

    figure_panel(
      label = "Przykład C2",
      title = "Pięć oddziałów szpitalnych — czas oczekiwania",
      p("Zmierzono średni czas oczekiwania na konsultację (minuty) w pięciu
        oddziałach szpitala. Który wymaga interwencji?"),
      ch3_case_ui("C2")
    ),

    lc_p("Przypadki powtarzają wnioski z całego rozdziału. W A2 przedział
      zwęża się wraz z liczebnością próby: ma około 11.4 µg/m³ szerokości
      przy n = 10, 4.5 przy n = 50 i 2.2 przy n = 200. Czterokrotnie większa
      próba daje mniej więcej dwukrotnie węższy przedział, bo błąd
      standardowy maleje jak \\(1/\\sqrt{n}\\). B3 to pułapka ze scenariusza C:
      przedziały grup się nakładają, a przedział różnicy [-22.2, -1.8] ms
      leży w całości poniżej zera. B4 pokazuje drugą stronę dużych prób.
      Przy 20 000 osób w grupie przedział różnicy [0.11, 0.69] pkt nie
      zawiera zera, ale różnica 0.4 punktu to około 0.03 odchylenia
      standardowego IQ. Wynik jest istotny statystycznie, ale nie ma ",
      gloss("istotność praktyczna", "istotności praktycznej"), "."),

    lc_p("Żeby ocenić znaczenie praktyczne, trzeba przed badaniem ustalić, jaka
      zmiana jest użyteczna. Na Ryc. 3.5 progiem jest oszczędność 2 minut.
      Przedział może leżeć nad zerem, a mimo to w całości poniżej progu. Może też
      być tak szeroki, że nie rozstrzyga ani kierunku, ani użyteczności."),

    ci_explorer("useful"),

    lc_p("Przy wielu grupach przedziały zestawia się na jednej osi w ",
      gloss("forest plot", "forest plocie"), ". Taki wykres to szybka mapa:
      rozłączne przedziały wskazują wyraźne różnice, a nakładające się
      nie rozstrzygają. W C1 rozłączne przedziały mają tylko metoda tradycyjna
      i tutoring. W C2 SOR (średnio 75 min, przedział [69.4, 80.6]) odstaje od
      wszystkich pozostałych oddziałów, a wśród pozostałych rozłączne
      przedziały mają jeszcze trzy pary. Żeby rozstrzygnąć konkretną parę grup,
      liczymy przedział dla różnicy ich średnich."),

    lc_p("Ta sama konstrukcja, estymata ± wartość krytyczna × błąd standardowy,
      działa także dla odsetków. Przy proporcjach błąd standardowy zależy jednak
      od samego szacowanego odsetka, co przy małych próbach sprawia kłopoty.
      Tym zajmiemy się w następnym rozdziale."),

    lc_chapter_next(
      num       = "04",
      title     = "Przedział dla proporcji",
      lead      = "CI dla odsetków: Wald, Wilson, Clopper-Pearson",
      target_id = "ch-proporcja"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch3_server <- function(input, output, session) {

  # Ryc. 3.1 (s04-tnorm, sf_steps.js), Ryc. 3.2–3.3 (ci_construction.js: próby,
  # przedziały i tekst kroku) oraz Przykłady A–C (s04-comp) liczy i rysuje przeglądarka.

  # Przykłady A1–C2 (case studies) liczy i rysuje przeglądarka: rycina lcSF
  # „s4-case” (sf_examples.js); dane i werdykty — ch3_case_ui() na górze pliku.
}
