# ============================================================================
# Ryciny wykładu 05: opisy liczone w R przy budowaniu strony
# ============================================================================
# Ryciny o skończonej liczbie ustawień (albo bez kontrolek) opisuje R i rysuje
# wbudowana rycina lc_spec (lc_sf_spec() w shared/lc_stat_figures.R). To, co zależy
# od losowania i suwaków, liczy modules/figures.js.
# Wymaga: helpers.R (kolory testu), lc_stat_figures.R.

# Rycina bez wykresu (quiz, wynik testu): kontener lcSF z atrybutem data-lc-nochart.
s05_reads_figure <- function(fig, ..., config = NULL) {
  tags$div(class = "lc-sf", `data-fig` = fig, `data-lc-nochart` = NA,
    `data-config` = if (!is.null(config)) as.character(jsonlite::toJSON(config, auto_unbox = TRUE, digits = NA)),
    ...)
}

# ---------------------------------------------------------------------------
# Rozdział 1
# ---------------------------------------------------------------------------

# Ryc. 3.5: jeden wynik na N(0, 1) pod H₀. Środek (|z| < 1.96) podpisany H₀,
# krańce Hₐ; pole za ±|z| to p-wartość, jej liczba w odczycie.
ch1_palpha_fig <- function() {
  cases <- c("2.17", "1.50")
  crit <- qnorm(0.975)
  x <- seq(-4, 4, length.out = 801)
  one <- function(case) {
    z <- as.numeric(case)
    tail <- function(sel) sf_line(x[sel], dnorm(x[sel]), color = "@bursztyn", width = 0, fill = "@bursztyn", fill_alpha = 0.6)
    sf_spec(c(-4, 4), c(0, 0.43 * 1.04), c(
        list(list(type = "band", lo = -4, hi = -crit, color = "@accent", alpha = 0.06),
             list(type = "band", lo = crit, hi = 4, color = "@accent", alpha = 0.06),
             tail(x <= -abs(z)), tail(x >= abs(z)),
             sf_line(x, dnorm(x), color = "@niebo", width = 2.6),
             sf_vline(-crit, color = "@accent"), sf_vline(crit, color = "@accent"),
             sf_vline(z, color = "@secondary", width = 2.4, dash = NULL, from = 0, to = 0.41),
             sf_text(z, 0.41, sprintf("z = %.2f", z), color = "@secondary", mono = TRUE, halo = TRUE)),
        sf_texts(c(-3.05, 0, 3.05), rep(0.22, 3), c("Hₐ", "H₀", "Hₐ"), valign = "middle", bold = TRUE, size = 2.2)),
      "Statystyka testowa", "", aria = "Rozkład statystyki testowej pod hipotezą zerową z obszarami odrzucenia i polem p-wartości.",
      xTicks = c(-1.96, 0, 1.96), hideY = TRUE, noYGrid = TRUE)
  }
  variants <- lapply(stats::setNames(cases, cases), one)
  # Napisy regionów w barwach: Hₐ jak obszar odrzucenia, H₀ jak krzywa.
  for (k in names(variants)) {
    n <- length(variants[[k]]$layers)
    cols <- c("@accent", "@niebo", "@accent")
    for (i in 1:3) variants[[k]]$layers[[n - 3 + i]]$color <- cols[i]
  }
  reads <- lapply(stats::setNames(cases, cases), function(case) {
    p <- 2 * pnorm(-abs(as.numeric(case)))
    list(palpha_p = sprintf("%.3f", p), palpha_verdict = if (p < 0.05) "p < α" else "p ≥ α")
  })
  lc_sf_spec(variants = variants, reads = reads,
    control = lc_segmented("ch1d_palpha_case", "Wynik",
      choices = c("z = 2.17" = "2.17", "z = 1.50" = "1.50"), selected = "2.17"),
    toolbar = lc_readouts(
      lc_readout("p", lc_sf_value("palpha_p"), color = col_pvalue, swatch = TRUE),
      lc_readout("α", "0.05", color = col_reject, swatch = TRUE),
      lc_readout(NULL, lc_sf_value("palpha_verdict"))
    ),
    ratio = "2/1", max_height = "380px")
}

# Ryc. 3.6: scenariusze quizu „odrzucić czy nie?”.
ch1_quiz_scenarios <- list(
  list(p = 0.003, alpha = 0.05, context = "Badanie wpływu kawy na czas reakcji: p = 0.003, α = 0.05"),
  list(p = 0.12, alpha = 0.05, context = "Czy notatki odręczne dają lepsze wyniki niż notatki na laptopie? p = 0.12, α = 0.05"),
  list(p = 0.048, alpha = 0.05, context = "Korelacja między długością snu a oceną z egzaminu: p = 0.048, α = 0.05"),
  list(p = 0.06, alpha = 0.01, context = "Czy kierunek studiów wpływa na zarobki po 5 latach? ANOVA: p = 0.06, α = 0.01"),
  list(p = 0.001, alpha = 0.01, context = "Czy płeć wpływa na wybór specjalizacji? Test χ²: p = 0.001, α = 0.01"),
  list(p = 0.052, alpha = 0.05, context = "Porównanie skuteczności dwóch metod nauki: p = 0.052, α = 0.05")
)
ch1_quiz_fig <- function() {
  tile <- function(letter, value, text) tags$button(type = "button", class = "quiz-tile", `data-stat-act` = value,
    div(class = "tile-letter", letter), div(class = "tile-text", text))
  s05_reads_figure("s05_quiz", config = list(scenarios = ch1_quiz_scenarios),
    lc_status(p(tags$strong("Scenariusz:"), " ", lc_sf_value("quiz_context"))),
    p("Decyzja:"),
    div(`data-role` = "options", class = "quiz-tiles quiz-cols-2",
      tile("A", "reject", "Odrzucamy H₀"), tile("B", "fail_to_reject", "Brak podstaw do odrzucenia H₀")),
    div(`data-role` = "feedback"),
    lc_sf_action("next", "ch1_quiz_next", "Nowy scenariusz", variant = "outline"))
}

# ---------------------------------------------------------------------------
# Wspólne dla rozdziałów „test krok po kroku”
# ---------------------------------------------------------------------------

# Panel hipotezy nad krokami: wariant wybiera lista scenariuszy `input_id`; podpowiedź
# `empty` widać, dopóki nie ma próby (element [data-when = "empty"], przełącza rycina).
# params: nazwana lista scenariuszy z polami question, h0_text, h1_text (i *_1s dla
# testu jednostronnego).
# heading: nagłówek ramki hipotezy, gdy inny niż „… (dwustronna/jednostronna):”.
s05_hypothesis_panel <- function(params, input_id, one_sided = FALSE, empty = NULL, heading = NULL) {
  field <- function(par, name) par[[if (one_sided) paste0(name, "_1s") else name]]
  tagList(
    lc_variants(input_id, lapply(params, function(par) {
      tagList(
        lc_status(
          p(tags$b(if (one_sided) "Pytanie potoczne (kierunkowe):" else "Pytanie potoczne:")),
          p(tags$em(paste0("„", field(par, "question"), "”")))
        ),
        lc_formula_box(
          p(tags$b(if (!is.null(heading)) heading else if (one_sided) "Hipoteza formalna (jednostronna):" else "Hipoteza formalna (dwustronna):")),
          p(withMathJax(field(par, "h0_text"))),
          p(withMathJax(field(par, "h1_text")))
        )
      )
    })),
    tags$div(`data-when` = "empty", lc_empty(if (!is.null(empty)) empty else if (one_sided) {
      "Najpierw wylosuj próbę w teście dwustronnym powyżej"
    } else {
      "Kliknij „Losuj próbę”"
    }))
  )
}

# ---------------------------------------------------------------------------
# Rozdział 8: dwie grupy
# ---------------------------------------------------------------------------

# Ryc. 8.3 (lewa): analiza niesparowana tych samych danych; pięć osób bez kontroli
# zawyża średnią „przed”.
ch6_compare_ind_fig <- function() {
  d <- .ch6_compare$ind_data
  set.seed(83)
  d$x <- as.integer(d$grupa) + stats::runif(nrow(d), -0.12, 0.12)
  groups <- split(d$wartosc, d$grupa)
  m <- vapply(groups, mean, 0)
  lc_sf_spec(sf_spec(c(0.4, 2.6), sf_range(d$wartosc, 0.06), c(list(
      sf_boxes(groups[1], "@niebo/0.35", width = 0.45, stroke = "@subtle", at = 1),
      sf_boxes(groups[2], "@accent/0.35", width = 0.45, stroke = "@subtle", at = 2),
      sf_points(d$x[d$typ == "para" & d$grupa == "Przed"], d$wartosc[d$typ == "para" & d$grupa == "Przed"], color = "@niebo", size = 8, alpha = 0.8),
      sf_points(d$x[d$grupa == "Po"], d$wartosc[d$grupa == "Po"], color = "@accent", size = 8, alpha = 0.8),
      sf_points(d$x[d$typ == "dropout"], d$wartosc[d$typ == "dropout"], color = "#8B1A1A", size = 9, alpha = 0.8, symbol = "triangle")),
      list(sf_text(0.9, m[[1]] + 2, paste0("x̄ = ", round(m[[1]], 1)), color = "@secondary", bold = TRUE, align = "right", valign = "middle"),
           sf_text(2.1, m[[2]] + 2, paste0("x̄ = ", round(m[[2]], 1)), color = "@secondary", bold = TRUE, align = "left", valign = "middle"))),
      "", "Ciśnienie skurczowe (mmHg)", aria = "Wykresy pudełkowe ciśnienia przed i po; pięć osób bez pomiaru kontrolnego zaznaczono trójkątami.",
      xLabels = sf_labels(c("Przed", "Po")),
      legend = sf_legend(c("Przed", "Po", "Brak kontroli (n = 5)"), c("@niebo", "@accent", "#8B1A1A"), box = TRUE)),
    ratio = "1.5/1", max_height = "260px", legend = TRUE)
}

# Ryc. 8.3 (prawa): te same pary połączone liniami.
ch6_compare_paired_fig <- function() {
  pr <- .ch6_compare$pairs
  lc_sf_spec(sf_spec(c(0.4, 2.6), sf_range(c(pr$przed, pr$po), 0.06), list(
      sf_segs(rep(1, nrow(pr)), pr$przed, rep(2, nrow(pr)), pr$po, color = "@kurkuma", width = 1.4, alpha = 0.3),
      sf_points(rep(1, nrow(pr)), pr$przed, color = "@niebo", size = 8, alpha = 0.8),
      sf_points(rep(2, nrow(pr)), pr$po, color = "@accent", size = 8, alpha = 0.8)),
      "", "Ciśnienie skurczowe (mmHg)", aria = "Ciśnienie tych samych piętnastu osób przed i po, połączone liniami.",
      xLabels = sf_labels(c("Przed", "Po"))),
    ratio = "1.5/1", max_height = "260px")
}

# ---------------------------------------------------------------------------
# Rozdział 9: ANOVA
# ---------------------------------------------------------------------------

# Ryc. 9.1: k grup na okręgu i wszystkie pary połączone odcinkami; barwa odcinków
# zależy od ryzyka co najmniej jednego błędu I rodzaju. Rysunki dla k = 2…8.
ch7_fwer_svg <- function(k) {
  angles <- seq(0, 2 * pi, length.out = k + 1)[-(k + 1)]
  x <- 150 + 100 * cos(angles); y <- 150 - 100 * sin(angles)
  pairs_idx <- utils::combn(k, 2)
  fwer <- 1 - (1 - 0.05)^ncol(pairs_idx)
  edge <- if (fwer < 0.15) "var(--upwr-accent)" else if (fwer < 0.35) "#D4860A" else "#9B1C1C"
  HTML(paste0(
    sprintf('<svg viewBox="0 0 300 300" style="display:block;width:100%%;max-width:340px;margin:0 auto" role="img" aria-label="%d grup i %d porównań par">', k, ncol(pairs_idx)),
    paste0(sprintf('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="%s" stroke-opacity="0.55" stroke-width="3"/>',
                   x[pairs_idx[1, ]], y[pairs_idx[1, ]], x[pairs_idx[2, ]], y[pairs_idx[2, ]], edge), collapse = ""),
    paste0(sprintf('<circle cx="%.1f" cy="%.1f" r="24" fill="var(--upwr-accent)" stroke="var(--upwr-surface)" stroke-width="2"/><text x="%.1f" y="%.1f" text-anchor="middle" dominant-baseline="central" font-weight="700" font-size="20" fill="var(--upwr-surface)">%s</text>',
                   x, y, x, y, LETTERS[seq_len(k)]), collapse = ""),
    "</svg>"))
}
ch7_fwer_fig <- function() {
  s05_reads_figure("s05_fwer",
    lc_toolbar(
      lc_sf_action("add", "ch7_motyw_add", "Dodaj grupę +", variant = "solid"),
      lc_sf_action("reset", "ch7_motyw_reset", icon = "reset", variant = "ghost", aria_label = "Reset"),
      lc_readouts(
        lc_readout("Grup", lc_sf_value("fwer_k")),
        lc_readout("Testów t", lc_sf_value("fwer_tests")),
        lc_readout("Ryzyko ≥ 1 błędu", lc_sf_value("fwer_risk"), color = "var(--upwr-sage)")
      )
    ),
    lapply(2:8, function(k) tags$div(`data-when` = k, hidden = if (k > 2) NA, ch7_fwer_svg(k)))
  )
}

# Konfiguracja scenariuszy ANOVA (Ryc. 9.2 i 9.3): etykieta grup, zmienne zależne, podpisy osi.
ch7_anova_config <- list(scenarios = list(
  fermentation = list(
    group_label = "Temperatura fermentacji",
    vars = list("pH jogurtu" = "pH", "Kwasowość miareczkowa (°SH)" = "kwasowosc_SH"),
    var_labels = list(pH = "pH jogurtu", kwasowosc_SH = "Kwasowość (°SH)")
  ),
  workplace = list(
    group_label = "Stanowisko pracy",
    vars = list("Dni nieobecności (urazy) / rok" = "nieobecnosci", "Poziom stresu (0–100)" = "stres"),
    var_labels = list(nieobecnosci = "Dni nieobecności / rok", stres = "Poziom stresu (0–100)")
  ),
  students = list(
    group_label = "Kierunek",
    vars = list("Średnia ocen" = "srednia_ocen", "Wzrost" = "wzrost", "Czas dojazdu" = "czas_dojazdu"),
    var_labels = list(srednia_ocen = "Średnia ocen", wzrost = "Wzrost (cm)", czas_dojazdu = "Czas dojazdu (min)")
  )
))
