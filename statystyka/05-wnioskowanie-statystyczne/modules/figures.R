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
