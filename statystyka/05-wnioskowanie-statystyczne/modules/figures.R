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

# Ryc. 3.7: statystyka na N(0, 1) pod H₀ w trzech widokach: samo pole ogonów (p-wartość),
# to samo z α i wartościami krytycznymi (środek podpisany H₀, krańce Hₐ), potem inny wynik.
# Wersja z 2026-11-19 (dawny prototyp 5Y); wcześniej dwa wyniki od razu z α.
ch1_palpha_fig <- function() {
  crit <- qnorm(0.975)
  x <- seq(-4, 4, length.out = 801)
  one <- function(z, with_alpha) {
    tail <- function(sel) sf_line(x[sel], dnorm(x[sel]), color = "@bursztyn", width = 0, fill = "@bursztyn", fill_alpha = 0.6)
    alpha_layers <- if (with_alpha) list(
      list(type = "band", lo = -4, hi = -crit, color = "@accent", alpha = 0.06),
      list(type = "band", lo = crit, hi = 4, color = "@accent", alpha = 0.06),
      sf_vline(-crit, color = "@accent"), sf_vline(crit, color = "@accent"))
    labels <- if (with_alpha) sf_texts(c(-3.05, 0, 3.05), rep(0.22, 3), c("Hₐ", "H₀", "Hₐ"), valign = "middle", bold = TRUE, size = 2.2)
    spec <- sf_spec(c(-4, 4), c(0, 0.43 * 1.04), c(alpha_layers,
        list(tail(x <= -abs(z)), tail(x >= abs(z)),
             sf_line(x, dnorm(x), color = "@niebo", width = 2.6),
             sf_vline(z, color = "@secondary", width = 2.4, dash = NULL, from = 0, to = 0.41),
             sf_text(z, 0.41, sprintf("z = %.2f", z), color = "@secondary", mono = TRUE, halo = TRUE)),
        labels),
      "Statystyka testowa", "", aria = "Rozkład statystyki testowej pod hipotezą zerową z polem p-wartości w ogonach.",
      xTicks = if (with_alpha) c(-1.96, 0, 1.96) else c(-2, 0, 2), hideY = TRUE, noYGrid = TRUE)
    if (with_alpha) {
      n <- length(spec$layers); cols <- c("@accent", "@niebo", "@accent")
      for (i in 1:3) spec$layers[[n - 3 + i]]$color <- cols[i]
    }
    spec
  }
  views <- list(p = list(2.17, FALSE), pa = list(2.17, TRUE), pb = list(1.50, TRUE))
  variants <- lapply(views, function(v) one(v[[1]], v[[2]]))
  reads <- lapply(views, function(v) {
    p <- 2 * pnorm(-abs(v[[1]]))
    list(pz_p = sprintf("%.3f", p), pz_alpha = if (v[[2]]) "0.05" else "—",
         pz_verdict = if (!v[[2]]) "" else if (p < 0.05) "p < α" else "p ≥ α")
  })
  lc_sf_spec(variants = variants, reads = reads,
    control = lc_segmented("ch1d_palpha_view", "Widok",
      choices = c("Pole ogonów" = "p", "Dodaj α" = "pa", "Inny wynik" = "pb"), selected = "p"),
    toolbar = lc_readouts(
      lc_readout("p", lc_sf_value("pz_p"), color = col_pvalue, swatch = TRUE),
      lc_readout("α", lc_sf_value("pz_alpha"), color = col_reject, swatch = TRUE),
      lc_readout(NULL, lc_sf_value("pz_verdict"))
    ),
    ratio = "2/1", max_height = "380px")
}

# Ryc. 3.9: scenariusze quizu „odrzucić czy nie?”.
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

# Ryc. 9.2: k grup na okręgu i wszystkie pary połączone odcinkami; barwa odcinków
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

# Konfiguracja scenariuszy ANOVA (Ryc. 9.5 i 9.7): etykieta grup, zmienne zależne, podpisy osi.
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
  ),
  # Scenariusz kierunkowy (IDSiK): zachmurzenie nie zależy od typu terenu (H₀ prawdziwa).
  landcover = list(
    group_label = "Typ pokrycia terenu",
    vars = list("Temperatura powierzchni (°C)" = "temp_pow", "Zieleń (NDVI)" = "ndvi", "Zachmurzenie (%)" = "zachmurzenie"),
    var_labels = list(temp_pow = "Temperatura powierzchni (°C)", ndvi = "Zieleń (NDVI, od 0 do 1)", zachmurzenie = "Zachmurzenie (%)")
  )
))

# ---------------------------------------------------------------------------
# Rozdział 10: siła efektu
# ---------------------------------------------------------------------------
# Ryc. 10.2–10.5 mają dwie kontrolki dyskretne (przykład i poziom efektu), więc R liczy
# każdy wariant: opis rysunku i fragment HTML z odczytami, tabelą i komentarzem.
# Dane przykładów (ch10_*_scenarios, ch10_*_examples) są w ch10_sila_efektu.R.

s05_html <- function(...) as.character(tagList(...))
# Warianty dla wszystkich par (przykład, poziom): nazwy "przykład|poziom".
s05_grid <- function(scenarios, levels, fun) {
  keys <- unlist(lapply(scenarios, function(s) paste(s, levels, sep = "|")))
  stats::setNames(lapply(keys, function(k) { part <- strsplit(k, "|", fixed = TRUE)[[1]]; fun(part[1], part[2]) }), keys)
}
ch10_scenario_ids <- c("TZ", "ROL", "IB")

# Ryc. 10.2: Cohen's d w surowych liczbach — dwa rozkłady normalne z przykładu.
ch10_d_fig <- function() {
  levels <- c("0.2", "0.5", "0.8", "1.2")
  one <- function(scenario, level) {
    e <- ch10_d_scenarios[[scenario]][[level]]
    x <- seq(min(e$x1, e$x2) - 3 * e$s, max(e$x1, e$x2) + 3 * e$s, length.out = 600)
    list(
      spec = sf_spec(range(x), c(0, dnorm(0, 0, e$s) * 1.08), list(
          sf_line(x, dnorm(x, e$x1, e$s), color = "@niebo", width = 1.8, fill = "@niebo", fill_alpha = 0.35),
          sf_line(x, dnorm(x, e$x2, e$s), color = "@accent", width = 1.8, fill = "@accent", fill_alpha = 0.35),
          sf_vline(e$x1, color = "@niebo"), sf_vline(e$x2, color = "@accent")),
        paste0("Wartość (", e$jednostka, ")"), "Gęstość", aria = "Rozkłady wartości w dwóch grupach przykładu.",
        legend = sf_legend(c(e$etyk1, e$etyk2), c("@niebo", "@accent"), box = TRUE)),
      html = list(table = s05_html(
        lc_readouts(lc_readout("Różnica", paste0(round(e$x2 - e$x1, 2), " ", e$jednostka)), lc_readout("d", level)),
        lc_table(data.frame(group = c(e$etyk1, e$etyk2), ms = paste0(c(e$x1, e$x2), " ± ", e$s, " ", e$jednostka)),
                 cols = list(lc_col("group", "Grupa", "row"), lc_col("ms", "x̄ ± s"))),
        lc_caption(e$kontekst)))
    )
  }
  res <- s05_grid(ch10_scenario_ids, levels, one)
  lc_sf_spec(variants = lapply(res, `[[`, "spec"), html = lapply(res, `[[`, "html"), key = c("ch10_d_scenario", "ch10_d_level"),
    control = list(
      selectInput("ch10_d_scenario", "Przykład", selectize = FALSE,
        choices = c("Jogurt (TŻ)" = "TZ", "Pszenica (ROL)" = "ROL", "BHP (IB)" = "IB"), selected = "TZ"),
      lc_segmented("ch10_d_level", "Wielkość efektu", choices = c(
        "d = 0.2 (mały)" = "0.2", "d = 0.5 (średni)" = "0.5", "d = 0.8 (duży)" = "0.8", "d = 1.2 (b. duży)" = "1.2"), selected = "0.5")
    ),
    legend = TRUE, body = tagList(lc_sf_chart(0, "3/1", "240px"), tags$div(`data-role` = "table")))
}

# Ryc. 10.3: r w surowych liczbach — 50 punktów o zadanej korelacji (stałe ziarno przykładu).
ch10_r_data <- function(scenario, level) {
  r_target <- as.numeric(level)
  sc <- ch10_r_scenarios[[scenario]]
  set.seed(sc$seed)
  z <- rnorm(50); e <- rnorm(50)
  y_raw <- sc$r_sign * r_target * z + sqrt(1 - r_target^2) * e
  data.frame(x = sc$x_center + sc$x_scale * z, y = sc$y_center + sc$y_scale * y_raw)
}
ch10_r_fig <- function() {
  levels <- c("0.1", "0.3", "0.5", "0.7", "0.9")
  opis <- c(
    "0.1" = "Punkty rozproszone, ledwo widoczny trend — czynnik tłumaczy 1% zmienności y.",
    "0.3" = "Trend zauważalny, ale duży rozrzut wokół linii — 9% zmienności wyjaśnione.",
    "0.5" = "Wyraźny trend, około ¼ zmienności y wyjaśniona przez x — reszta na inne czynniki.",
    "0.7" = "Mocny związek liniowy, prawie połowa zmienności y wyjaśniona.",
    "0.9" = "Punkty bardzo blisko linii prostej — 81% zmienności y wyjaśnione przez x.")
  one <- function(scenario, level) {
    sc <- ch10_r_scenarios[[scenario]]; df <- ch10_r_data(scenario, level); r_val <- as.numeric(level)
    list(
      spec = sf_spec(sf_range(df$x), sf_range(df$y), c(
          sf_smooth_lm(df$x, df$y, color = "@ref", width = 2, se = FALSE),
          list(sf_points(df$x, df$y, color = "@wrzos", size = 8, alpha = 0.7))),
        sc$x_label, sc$y_label, aria = "Wykres rozrzutu pięćdziesięciu punktów o zadanej korelacji."),
      html = list(table = s05_html(
        lc_readouts(
          lc_readout("r zadane", r_val),
          lc_readout("r w próbie", round(cor(df$x, df$y), 2), color = col_effect),
          lc_readout("r²", round(r_val^2, 2)),
          lc_readout("Wariancji wyjaśnione", paste0(round(100 * r_val^2), "%"))),
        lc_caption(opis[[level]])))
    )
  }
  res <- s05_grid(ch10_scenario_ids, levels, one)
  lc_sf_spec(variants = lapply(res, `[[`, "spec"), html = lapply(res, `[[`, "html"), key = c("ch10_r_scenario", "ch10_r_level"),
    control = list(
      selectInput("ch10_r_scenario", "Przykład", selectize = FALSE,
        choices = c("Jogurt (TŻ)" = "TZ", "Plon (ROL)" = "ROL", "Wypadki (IB)" = "IB"), selected = "TZ"),
      lc_segmented("ch10_r_level", "Wielkość korelacji", choices = c(
        "r = 0.1 (mała)" = "0.1", "r = 0.3 (średnia)" = "0.3", "r = 0.5 (duża)" = "0.5",
        "r = 0.7 (b. duża)" = "0.7", "r = 0.9 (b. duża)" = "0.9"), selected = "0.5")
    ),
    body = tagList(lc_sf_chart(0, "3/1", "240px"), tags$div(`data-role` = "table"),
                   lc_variants("ch10_r_scenario", lapply(ch10_r_hints, lc_caption))))
}

# Ryc. 10.4: Cramér's V w tabeli 2 × 2 — odsetki w dwóch grupach (słupki skumulowane do 100%).
ch10_v_fig <- function() {
  levels <- c("0.10", "0.30", "0.50", "0.70")
  one <- function(scenario, level) {
    e <- ch10_v_examples[[level]]; sc <- ch10_v_scenarios[[scenario]]
    pos <- 100 * c(e$p_a, e$p_b)
    list(
      spec = sf_spec(c(0.4, 2.6), c(0, 104), c(list(
          sf_bars(c(1, 2) - 0.275, c(1, 2) + 0.275, rep(100, 2), color = "@niebo", alpha = 0.9, stroke = NULL),
          sf_bars(c(1, 2) - 0.275, c(1, 2) + 0.275, pos, color = "@accent", alpha = 0.9, stroke = NULL)),
          sf_texts(c(1, 2), pos / 2, paste0(round(pos), "%"), color = "#ffffff", bold = TRUE, valign = "middle", size = 1.25),
          sf_texts(c(1, 2), pos + (100 - pos) / 2, paste0(round(100 - pos), "%"), color = "#ffffff", bold = TRUE, valign = "middle", size = 1.25)),
        "", "Odsetek w grupie", aria = "Odsetki stanu w dwóch grupach jako słupki skumulowane.",
        xLabels = sf_labels(c(sc$grp_a, sc$grp_b)), yLabels = sf_labels(paste0(seq(0, 100, 25), "%"), seq(0, 100, 25)),
        legend = sf_legend(c(sc$stan_neg, sc$stan_pos), c("@niebo", "@accent"), box = TRUE)),
      html = list(table = s05_html(
        lc_readouts(lc_readout("Różnica", paste0(round(100 * (e$p_b - e$p_a)), " pp")), lc_readout("V", level)),
        lc_table(
          data.frame(group = c(sc$grp_a, sc$grp_b), pos = round(100 * c(e$p_a, e$p_b)), neg = round(100 * (1 - c(e$p_a, e$p_b)))),
          cols = list(lc_col("group", "Grupa", "row"), lc_col("pos", paste0(sc$stan_pos, " (%)")), lc_col("neg", paste0(sc$stan_neg, " (%)"))))))
    )
  }
  res <- s05_grid(ch10_scenario_ids, levels, one)
  lc_sf_spec(variants = lapply(res, `[[`, "spec"), html = lapply(res, `[[`, "html"), key = c("ch10_v_scenario", "ch10_v_level"),
    control = list(
      selectInput("ch10_v_scenario", "Przykład", selectize = FALSE,
        choices = c("Pleśń (TŻ)" = "TZ", "Chwasty (ROL)" = "ROL", "Szczepienie (IB)" = "IB"), selected = "TZ"),
      lc_segmented("ch10_v_level", "Wielkość efektu", choices = c(
        "V = 0.10 (mały)" = "0.10", "V = 0.30 (średni)" = "0.30", "V = 0.50 (duży)" = "0.50", "V = 0.70 (b. duży)" = "0.70"), selected = "0.30")
    ),
    legend = TRUE,
    body = tagList(lc_sf_chart(0, "3/1", "240px"), tags$div(`data-role` = "table"),
                   lc_variants("ch10_v_scenario", lapply(ch10_v_scenarios, function(sc) lc_caption(sc$hint)))))
}

# Ryc. 10.5: η² w ANOVA — trzy grupy o średnich rozsuniętych tak, by dać zadane η² (stałe ziarno).
ch10_eta_means <- function(eta, mu_ctr, s) {
  delta <- sqrt(3 * eta * s^2 / (2 * (1 - eta)))
  c(mu_ctr - delta, mu_ctr, mu_ctr + delta)
}
ch10_eta_fig <- function() {
  levels <- c("0.01", "0.06", "0.14", "0.30")
  fills <- c("@grafit/0.5", "@bursztyn/0.5", "@niebo/0.5")
  one <- function(scenario, level) {
    eta <- as.numeric(level); sc <- ch10_eta_scenarios[[scenario]]
    mus <- ch10_eta_means(eta, sc$mu_ctr, sc$s)
    set.seed(202)
    df <- data.frame(grupa = factor(rep(sc$grp, each = 30), levels = sc$grp),
                     y = c(rnorm(30, mus[1], sc$s), rnorm(30, mus[2], sc$s), rnorm(30, mus[3], sc$s)))
    groups <- split(df$y, df$grupa)
    set.seed(105)
    jit <- stats::runif(nrow(df), -0.15, 0.15)
    out <- do.call(rbind, lapply(seq_along(groups), function(i) { o <- sf_box_stats(groups[[i]])$out; if (length(o)) data.frame(x = i, y = o) }))
    grand <- mean(df$y); grp_means <- tapply(df$y, df$grupa, mean)
    eta_sample <- sum(table(df$grupa) * (grp_means - grand)^2) / sum((df$y - grand)^2)
    list(
      spec = sf_spec(c(0.4, 3.6), sf_range(df$y), list(
          sf_boxes(groups, fills),
          if (!is.null(out)) sf_points(out$x, out$y, color = "@accent", size = 6, alpha = 0.5),
          sf_points(as.integer(df$grupa) + jit, df$y, color = "@accent", size = 5.5, alpha = 0.4)),
        "", sc$y_lab, aria = "Wykresy pudełkowe trzech grup przykładu.", xLabels = sf_labels(sc$grp)),
      html = list(table = s05_html(
        lc_readouts(lc_readout("η² w populacji", level), lc_readout("η² w próbie", sprintf("%.3f", eta_sample), color = col_effect)),
        lc_table(
          data.frame(group = sc$grp[1:3], mu = mus[1:3], mean = unname(grp_means[sc$grp[1:3]]), s = sc$s),
          cols = list(lc_col("group", "Grupa", "row"), lc_col("mu", "μ (populacja)", digits = 1),
                      lc_col("mean", "x̄ (próba)", digits = 1), lc_col("s", "σ", digits = if (sc$s %% 1 == 0) 0 else 1))),
        lc_caption(ch10_eta_examples[[level]]$kontekst)))
    )
  }
  res <- s05_grid(ch10_scenario_ids, levels, one)
  lc_sf_spec(variants = lapply(res, `[[`, "spec"), html = lapply(res, `[[`, "html"), key = c("ch10_eta_scenario", "ch10_eta_level"),
    control = list(
      selectInput("ch10_eta_scenario", "Przykład", selectize = FALSE,
        choices = c("Pasteryzacja (TŻ)" = "TZ", "Nawozy (ROL)" = "ROL", "Zmiany (IB)" = "IB"), selected = "TZ"),
      lc_segmented("ch10_eta_level", "Wielkość efektu", choices = c(
        "η² = 0.01 (mały)" = "0.01", "η² = 0.06 (średni)" = "0.06", "η² = 0.14 (duży)" = "0.14", "η² = 0.30 (b. duży)" = "0.30"), selected = "0.06")
    ),
    body = tagList(lc_sf_chart(0, "3/1", "240px"), tags$div(`data-role` = "table"),
                   lc_variants("ch10_eta_scenario", lapply(ch10_eta_scenarios, function(sc) {
                     lc_caption(paste0("3 grupy (", paste(sc$grp, collapse = " / "), ") × ", sc$y_lab, "."))
                   }))))
}
