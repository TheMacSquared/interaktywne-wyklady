# PROTOTYPY (2026-11-03) — punkty stop rozdziałów „zasoby” i „zawodzi” wykładu 05 ----
#
# Obiekty UI (wpinane w block.R):
#   d5p_cost_ui      — Galeria „Koszt pewności” (P10): rozkład P(X = x) dla r = 3, p = 0.10,
#                      przełącznik limitu i odczyt zapasu planistycznego.
#   d5p_suppliers_ui — Piaskownica „Plan na 61 a dwóch dostawców” (P11): mieszanka dwóch
#                      partii o p = 0.10 − d i p = 0.10 + d, suwak różnicy d.
# Serwer: d5p_plan_server(input, output, session).
# Input id: d5p_cost_variant (lc_segmented), d5p_sup_d (lc_slider).
# Rysunki i odczyty renderowane w: d5p_cost_fig, d5p_cost_read, d5p_sup_fig, d5p_sup_read.
# Wymaga: palette.R, theme_upwr.R, shared.R, lecture_layout.R, risk_math.R, risk_block.R
#         oraz stop_proto_common.R (helpery d5p_*, źródłowany przed tym plikiem).

d5p_plan_p <- 0.10
d5p_plan_r <- 3

# Wartość zastępcza, gdy input jeszcze nie istnieje.
d5p_or <- function(x, default) if (is.null(x)) default else x

d5p_cost_choices <- c("Średnia" = "mean", "90%" = "p90", "95%" = "p95", "99%" = "p99",
  "Wszystkie" = "all")

# Limity planistyczne w kontrolach: średnia r/p oraz kwantyle + r (qnbinom liczy wady ponad r).
d5p_cost_limits <- function(p = d5p_plan_p, r = d5p_plan_r) {
  c(mean = round(r / p),
    p90 = qnbinom(0.90, r, p) + r,
    p95 = qnbinom(0.95, r, p) + r,
    p99 = qnbinom(0.99, r, p) + r)
}

# Odczyt pod rysunkiem: limit, zapas względem średniej i P(ukończenia do limitu).
d5p_cost_text <- function(variant, p = d5p_plan_p, r = d5p_plan_r) {
  lim <- d5p_cost_limits(p, r)
  if (identical(variant, "all")) {
    gap <- lim[c("p90", "p95", "p99")] - lim[["mean"]]
    return(sprintf("każdy kolejny punkt pewności kosztuje więcej: %s kontroli względem średniej",
      paste(sprintf("%+d", gap), collapse = ", ")))
  }
  L <- lim[[variant]]
  gap <- L - lim[["mean"]]
  gap_txt <- if (gap == 0) "0" else sprintf("%+d", gap)
  sprintf("limit %d · zapas względem średniej %s · P(ukończenia) = %.2f",
    L, gap_txt, pnbinom(L - r, r, p))
}

# Galeria: słupki P(X = x) dla x = 3…100, limit w szałwii (do limitu) i akcencie (za limitem).
d5p_cost_svg <- function(variant = "p95", p = d5p_plan_p, r = d5p_plan_r) {
  lim <- d5p_cost_limits(p, r)
  xs <- 3:100
  pmf <- dnbinom(xs - r, r, p)
  W <- 680; H <- 300; x0 <- 24; pw <- 640; base <- 250; top <- 46
  x_of <- function(x) x0 + (x - 3) / 97 * pw
  y_of <- function(v) base - v / max(pmf) * (base - top)
  bw <- pw / 97 * 0.8
  bars <- function(cls) sprintf('<rect class="%s" x="%.2f" y="%.2f" width="%.2f" height="%.2f" rx="1"/>',
    cls, x_of(xs) - bw / 2, y_of(pmf), bw, base - y_of(pmf))
  axis <- d5p_axis(x_of, base, ticks = c(3, 20, 40, 60, 80, 100), label = "liczba kontroli X")

  if (identical(variant, "all")) {
    median_n <- qnbinom(0.5, r, p) + r
    body <- c(
      sprintf('<g style="fill:var(--upwr-rule)">%s</g>', paste(bars("")[seq_along(xs)], collapse = "")),
      sprintf('<line class="d5p-limit" x1="%.2f" x2="%.2f" y1="%g" y2="%g"/>', x_of(lim), x_of(lim), top, base),
      sprintf('<text class="d5p-mono" x="%.2f" y="%g" text-anchor="middle" font-size="13">%d</text>',
        x_of(lim), top - 10, lim),
      sprintf('<line x1="%.2f" x2="%.2f" y1="%g" y2="%g" style="stroke:var(--upwr-ink-subtle);stroke-width:1"/>',
        x_of(median_n), x_of(median_n), top, base),
      sprintf('<text class="d5p-mono d5p-subtle" x="%.2f" y="%g" text-anchor="end" font-size="13">mediana %d</text>',
        x_of(median_n) - 6, top - 10, median_n)
    )
    aria <- "Cztery limity planu na rozkładzie liczby kontroli: 30, 52, 61 i 81, z mediana 27."
  } else {
    L <- lim[[variant]]
    body <- c(
      bars(ifelse(xs <= L, "d5p-ok", "d5p-bad")),
      sprintf('<line class="d5p-limit" x1="%.2f" x2="%.2f" y1="%g" y2="%g"/>', x_of(L), x_of(L), top, base),
      sprintf('<text class="d5p-mono" x="%.2f" y="%g" font-size="13">limit %d</text>', x_of(L) + 6, top - 10, L)
    )
    aria <- sprintf("Rozkład liczby kontroli z pionową linią limitu %d; słupki za limitem oznaczają zabrakło kontroli.", L)
  }
  d5p_svg(W, H, aria, body, axis)
}

# ============================================================================
# (P10) GALERIA „KOSZT PEWNOŚCI”
# ============================================================================

d5p_cost_ui <- d5p_panel("Galeria", "Koszt pewności: P(X = x), r = 3, p = 0.10",
  d5p_css,
  lc_toolbar(
    lc_segmented("d5p_cost_variant", NULL, choices = d5p_cost_choices, selected = "p95")
  ),
  uiOutput("d5p_cost_fig"),
  uiOutput("d5p_cost_read"),
  lc_caption("Zielone słupki: audyt ukończony do limitu. Czerwone: kontroli zabrakło. Linia przerywana: limit planu.")
)

# ============================================================================
# (P11) PIASKOWNICA „PLAN NA 61 A DWÓCH DOSTAWCÓW”
# ============================================================================

# Rozkład mieszanki: połowa partii z p = p − d, połowa z p = p + d.
d5p_sup_pmf <- function(xs, d, p = d5p_plan_p, r = d5p_plan_r) {
  0.5 * dnbinom(xs - r, r, p - d) + 0.5 * dnbinom(xs - r, r, p + d)
}

# P(X ≤ n) dla mieszanki.
d5p_sup_cdf <- function(n, d, p = d5p_plan_p, r = d5p_plan_r) {
  0.5 * pnbinom(n - r, r, p - d) + 0.5 * pnbinom(n - r, r, p + d)
}

# Najmniejsze n z P(X ≤ n) ≥ a dla mieszanki.
d5p_sup_quantile <- function(d, a = 0.95, p = d5p_plan_p, r = d5p_plan_r) {
  ns <- r:400
  ns[which(d5p_sup_cdf(ns, d, p, r) >= a)[1]]
}

# Średnia liczba kontroli mieszanki: 0.5·r/p_A + 0.5·r/p_B.
d5p_sup_mean <- function(d, p = d5p_plan_p, r = d5p_plan_r) {
  0.5 * r / (p - d) + 0.5 * r / (p + d)
}

d5p_sup_svg <- function(d = 0.05, p = d5p_plan_p, r = d5p_plan_r) {
  xs <- 3:150
  mix <- d5p_sup_pmf(xs, d, p, r)
  const <- dnbinom(xs - r, r, p)
  ymax <- max(c(mix, const)) * 1.08
  W <- 680; H <- 300; x0 <- 24; pw <- 640; base <- 250; top <- 46
  x_of <- function(x) x0 + (x - 3) / 147 * pw
  y_of <- function(v) base - v / ymax * (base - top)
  bw <- pw / 147 * 0.8

  rects <- sprintf('<rect x="%.2f" y="%.2f" width="%.2f" height="%.2f" rx="1"/>',
    x_of(xs) - bw / 2, y_of(mix), bw, base - y_of(mix))
  mix_bars <- sprintf('<g style="fill:var(--upwr-accent);fill-opacity:.55">%s</g>', paste(rects, collapse = ""))

  # Schodkowy kontur stałego p = 0.10: poziomy odcinek na każdą wartość, pionowy przy przejściu.
  yc <- y_of(const)
  segs <- paste0(sprintf(" H%.2f", x_of(xs + 0.5)), c(sprintf(" V%.2f", yc[-1]), ""))
  contour <- sprintf('<path class="d5p-limit" fill="none" d="M%.2f %.2f%s"/>',
    x_of(xs[1] - 0.5), yc[1], paste(segs, collapse = ""))

  plan <- d5p_cost_limits(p, r)[["p95"]]
  q <- d5p_sup_quantile(d, 0.95, p, r)
  q_x <- min(x_of(q), x_of(150))
  right_edge <- q > 150
  plan_line <- sprintf('<line class="d5p-limit" x1="%.2f" x2="%.2f" y1="%g" y2="%g"/>', x_of(plan), x_of(plan), top, base)
  q_line <- sprintf('<line x1="%.2f" x2="%.2f" y1="%g" y2="%g" style="stroke:var(--upwr-accent);stroke-width:2"/>',
    q_x, q_x, top, base)
  plan_label <- sprintf('<text class="d5p-mono" x="%.2f" y="%g" text-anchor="end" font-size="13">plan %d</text>',
    x_of(plan) - 6, top - 10, plan)
  q_label <- if (right_edge) {
    sprintf('<text class="d5p-mono d5p-bad" x="%.2f" y="%g" text-anchor="end" font-size="13">95%%: %d →</text>',
      q_x - 6, top - 10, q)
  } else {
    sprintf('<text class="d5p-mono d5p-bad" x="%.2f" y="%g" font-size="13">95%%: %d</text>',
      q_x + 6, top - 10, q)
  }
  axis <- d5p_axis(x_of, base, ticks = c(3, 30, 60, 90, 120, 150), label = "liczba kontroli X")

  d5p_svg(W, H,
    "Rozkład mieszanki dwóch partii na tle stałego p = 0.10, z linią planu 61 i 95. percentylem mieszanki.",
    mix_bars, contour, plan_line, q_line, plan_label, q_label, axis)
}

d5p_suppliers_ui <- d5p_panel("Piaskownica", "Plan na 61 a dwóch dostawców",
  d5p_css,
  lc_toolbar(
    lc_slider("d5p_sup_d", "Różnica jakości dostawców d", 0, 0.08, 0.05, 0.01),
    lc_readouts(uiOutput("d5p_sup_read"))
  ),
  uiOutput("d5p_sup_fig"),
  lc_caption("Średnie p zawsze wynosi 0.10, a plan zawodzi coraz częściej: partie o małym p wydłużają audyt bardziej, niż dobre go skracają.")
)

# ============================================================================
# SERWER
# ============================================================================

d5p_plan_server <- function(input, output, session) {
  cost_variant <- reactive(d5p_or(input$d5p_cost_variant, "p95"))
  output$d5p_cost_fig <- renderUI(d5p_cost_svg(cost_variant()))
  output$d5p_cost_read <- renderUI(tags$p(class = "d5p-read", d5p_cost_text(cost_variant())))

  sup_d <- reactive(d5p_or(input$d5p_sup_d, 0.05))
  output$d5p_sup_fig <- renderUI(d5p_sup_svg(sup_d()))
  output$d5p_sup_read <- renderUI({
    d <- sup_d()
    plan <- d5p_cost_limits()[["p95"]]
    p_const <- pnbinom(plan - d5p_plan_r, d5p_plan_r, d5p_plan_p)
    p_mix <- d5p_sup_cdf(plan, d)
    q_const <- d5p_cost_limits()[["p95"]]
    q_mix <- d5p_sup_quantile(d)
    m_const <- d5p_cost_limits()[["mean"]]
    m_mix <- d5p_sup_mean(d)
    tagList(
      lc_readout("P(ukończenia w planie 61)",
        sprintf("stałe p %.2f · dwaj dostawcy %.2f", p_const, p_mix)),
      lc_readout("95% naprawdę",
        sprintf("stałe p %d · dwaj dostawcy %d", q_const, q_mix), color = unname(upwr_accent)),
      lc_readout("średnia",
        sprintf("stałe p %s · dwaj dostawcy %s", format(m_const), format(round(m_mix, 1))))
    )
  })
}
