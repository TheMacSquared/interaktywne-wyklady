# PROTOTYPY (2026-10-28) — punkty stop wykresowe do przeglądu ----------------
#
# Każdy panel ma złotą plakietkę „Prototyp · <rodzaj>”. Stare widgety zostają
# obok, żeby dało się porównać i wybrać. Numery jak w analizie punktów stop:
#   p4_proto_assumptions_ui — Galeria „Trzy partie, ta sama średnia” (3), zaakceptowana 2026-10-28
#   p4_proto_batches_ui     — Symulacja „Partia po partii” (6), zaakceptowany 2026-10-28
#   p4_proto_binom_gallery_ui — Galeria „Rozkład dwumianowy B(n, p)” (Ryc. 3.4 ze statystyki 03), zaakceptowana 2026-10-28
#   p4_proto_np_ui          — Przypadek brzegowy „Skrót n · p” (7), zaakceptowany 2026-10-28
#   p4_proto_examples_ui    — Przykłady „Ten sam wzór w trzech sytuacjach” (8), zaakceptowany 2026-10-28
#   p4_proto_oc_ui          — Piaskownica „Krzywa operacyjna planu (n, c)” (10), zaakceptowany 2026-10-28
#   p4_proto_zero_ui        — Galeria „Trzech dostawców bez wad” (12), zaakceptowany 2026-10-28
#   p4_proto_closing_ui     — Domknięcie „Jeden rozkład, pięć pytań” (13), zaakceptowany 2026-10-28
# Serwer: p4_proto_charts_server(input, output, session).
# Wymaga: palette.R, theme_upwr.R, shared.R, lecture_layout.R, risk_math.R, risk_block.R.

# proto = FALSE: prototyp zaakceptowany, zwykła plakietka rodzaju.
p4_proto_panel <- function(kind, title, ..., width_mode = "wide", proto = TRUE) {
  if (!proto) return(figure_panel(label = kind, title = title, width_mode = width_mode, ...))
  figure_panel(label = paste("Prototyp ·", kind), title = title,
    color = upwr_single_alt, width_mode = width_mode, ...)
}

p4_proto_css <- tags$style(HTML("
  .p4p-svg { display: block; width: 100%; height: auto; margin: .2em auto .4em; }
  .p4p-svg text { font-family: var(--upwr-sans); fill: var(--upwr-ink); }
  .p4p-head { font-weight: 700; font-size: 17px; }
  .p4p-sub { font-size: 13px; fill: var(--upwr-ink-subtle) !important; }
  .p4p-num { font-family: var(--upwr-mono) !important; font-size: 14px; }
  .p4p-big { font-family: var(--upwr-mono) !important; font-size: 22px; font-weight: 700; }
  .p4p-ok { fill: var(--upwr-sage); }
  .p4p-bad { fill: var(--upwr-accent); }
  .p4p-cell-ok { fill: color-mix(in srgb, var(--upwr-sage) 30%, var(--upwr-surface)); stroke: var(--upwr-sage); stroke-width: 1; }
  .p4p-cell-bad { fill: var(--upwr-accent); }
  .p4p-axis { stroke: var(--upwr-ink-subtle); stroke-width: 1.4; }
  .p4p-tick { font-family: var(--upwr-mono) !important; font-size: 12px; fill: var(--upwr-ink-subtle) !important; text-anchor: middle; }
  .p4p-card { border: 1px solid var(--upwr-rule); border-left: 4px solid var(--upwr-accent);
    background: var(--upwr-surface); padding: 14px 18px; border-radius: 6px; margin: .4em 0; }
  .p4p-card h4 { margin: 0 0 .3em; font-size: 1.1em; }
  .p4p-card .p4p-calc { font-family: var(--upwr-mono); font-size: 1.25em; margin: .4em 0; }
  .p4p-card .p4p-verdict { color: var(--upwr-accent); }
"))

# ============================================================================
# (3) GALERIA „TRZY PARTIE, TA SAMA ŚREDNIA” — przypadek brzegowy założeń
# ============================================================================

# Symulacja raz przy starcie (stałe ziarno), więc przełączanie jest natychmiastowe.
# Zależność: po niesprawnym zaworze następny zawodzi z p = 0.2; p po sprawnym
# dobrane tak, żeby długookresowy udział wad wynosił 0.02. Przy mieszance pamiętamy
# magazyn każdej partii (mixture_p), żeby dało się ją rozdzielić na dwa schematy Bernoulliego.
p4_assumption_sims <- local({
  set.seed(4403)
  reps <- 4000L; n <- 100L
  stable <- rbinom(reps, n, 0.02)
  mixture_p <- sample(c(0.01, 0.03), reps, replace = TRUE)
  mixture <- rbinom(reps, n, mixture_p)
  p_after <- 0.2; p_base <- 0.02 * (1 - p_after) / (1 - 0.02)
  dependent <- vapply(seq_len(reps), function(i) {
    x <- logical(n); x[1] <- runif(1) < 0.02
    for (j in 2:n) x[j] <- runif(1) < (if (x[j - 1]) p_after else p_base)
    sum(x)
  }, integer(1))
  list(stable = stable, mixture = mixture, dependent = dependent, mixture_p = mixture_p)
})

p4_assumption_labels <- c(stable = "Jedna stabilna linia", mixture = "Dwie dostawy: p = 0.01 albo 0.03",
  dependent = "Wada ciągnie następną")

p4_proto_assumptions_ui <- p4_proto_panel("Galeria", "Trzy partie, ta sama średnia", proto = FALSE,
  lc_toolbar(
    lc_segmented("p4p_assume", "Partia", choices = c("Stabilna linia" = "stable",
      "Dwie dostawy" = "mixture", "Wada ciągnie następną" = "dependent")),
    conditionalPanel("input.p4p_assume == 'mixture'",
      lc_segmented("p4p_split", "Magazyny", choices = c("Razem" = "together", "Osobno" = "apart"))),
    lc_readouts(uiOutput("p4p_assume_read"))
  ),
  lc_plot("p4p_assume_plot", ratio = "2/1"),
  uiOutput("p4p_assume_caption")
)

# Mieszanka rozdzielona: każdy magazyn osobno, każdy ze swoim modelem B(100, p).
p4_assumptions_split_plot <- function() {
  x <- p4_assumption_sims$mixture; wp <- p4_assumption_sims$mixture_p
  kmax <- 10
  lab <- c("0.01" = "Magazyn A: p = 0.01", "0.03" = "Magazyn B: p = 0.03")
  one <- function(pv) {
    xs <- x[wp == pv]
    data.frame(k = 0:kmax, obs = vapply(0:kmax, function(k) mean(pmin(xs, kmax) == k), numeric(1)),
      ref = dbinom(0:kmax, 100, pv), mag = lab[[format(pv)]])
  }
  d <- rbind(one(0.01), one(0.03))
  ggplot(d, aes(k)) +
    geom_col(aes(y = obs, fill = mag), width = 0.75) +
    geom_point(aes(y = ref), colour = upwr_ink, size = 2.4, shape = 21, fill = "white", stroke = 1.2) +
    facet_wrap(~mag) +
    scale_fill_manual(values = setNames(c(unname(upwr_cat["szalwia"]), upwr_accent), unname(lab)), guide = "none") +
    scale_x_continuous(breaks = seq(0, kmax, 2), labels = c(seq(0, kmax - 2, 2), paste0(kmax, "+"))) +
    scale_y_continuous(limits = c(0, 0.4), labels = function(v) sprintf("%.2f", v)) +
    labs(x = "Liczba niesprawnych w partii", y = "Udział partii") +
    theme_upwr(grid = "y")
}

p4_assumptions_plot <- function(variant) {
  x <- p4_assumption_sims[[variant]]
  kmax <- 10
  obs <- data.frame(k = 0:kmax, f = vapply(0:kmax, function(k) mean(pmin(x, kmax) == k), numeric(1)))
  ref <- data.frame(k = 0:kmax, f = dbinom(0:kmax, 100, 0.02))
  ggplot() +
    geom_col(data = obs, aes(k, f), fill = if (variant == "stable") upwr_secondary else upwr_accent,
      width = 0.75) +
    geom_point(data = ref, aes(k, f), colour = upwr_ink, size = 2.6, shape = 21, fill = "white", stroke = 1.2) +
    scale_x_continuous(breaks = 0:kmax, labels = c(0:(kmax - 1), paste0(kmax, "+"))) +
    scale_y_continuous(limits = c(0, 0.32), labels = function(v) sprintf("%.2f", v)) +
    labs(x = "Liczba niesprawnych w partii", y = "Udział partii") +
    theme_upwr(grid = "y")
}

# ============================================================================
# GALERIA „ROZKŁAD DWUMIANOWY B(n, p)” — przeniesiona z Ryc. 3.4 (statystyka 03)
# ============================================================================

# Moneta i egzamin ze statystyki (ta sama E(X) = 5, inne kształty) oraz dwie
# historie Bananpolu z notek „Dane Bananpolu”: skrzynia 12 kiści i partia 100 zaworów;
# rzuty wolne koszykarza to duże p i dłuższy lewy ogon.
p4_binom_gallery_defs <- list(
  b1 = list(label = "Moneta: B(10, 0.5)", n = 10, p = 0.5),
  b2 = list(label = "Egzamin: B(20, 0.25)", n = 20, p = 0.25),
  b3 = list(label = "Banany: B(12, 0.1)", n = 12, p = 0.1),
  b4 = list(label = "Zawory: B(100, 0.02)", n = 100, p = 0.02),
  b5 = list(label = "Rzuty wolne: B(10, 0.8)", n = 10, p = 0.8)
)

p4_proto_binom_gallery_ui <- p4_proto_panel("Galeria", "Rozkład dwumianowy B(n, p)", proto = FALSE,
  lc_toolbar(
    checkboxGroupInput("p4p_bg", "Scenariusze",
      choices = setNames(names(p4_binom_gallery_defs), vapply(p4_binom_gallery_defs, `[[`, "", "label")),
      selected = "b1")
  ),
  lc_plot("p4p_bg_plot", max_height = "400px"),
  uiOutput("p4p_bg_stats")
)

p4_binom_gallery_plot <- function(selected) {
  defs <- p4_binom_gallery_defs[selected]
  labels <- vapply(defs, `[[`, "", "label")
  # oś do wartości, które mają realną szansę (zawory: 0–9 zamiast 0–100)
  df <- do.call(rbind, lapply(defs, function(s) {
    x <- 0:min(s$n, qbinom(0.9999, s$n, s$p) + 1)
    data.frame(x = x, prob = dbinom(x, s$n, s$p), scenario = s$label)
  }))
  df$scenario <- factor(df$scenario, levels = labels)
  dodge <- if (length(selected) > 1) position_dodge(width = 0.5) else "identity"
  ggplot(df, aes(x = x, y = prob, color = scenario)) +
    geom_point(size = 4, alpha = 0.85, position = dodge) +
    geom_segment(aes(xend = x, yend = 0), linewidth = 1, alpha = 0.6, position = dodge) +
    scale_color_manual(values = setNames(upwr_cat_n(length(selected)), labels), name = NULL,
      guide = guide_legend(nrow = if (length(selected) > 2) 2 else 1)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(x = "Liczba sukcesów (k)", y = "P(X = k)") +
    theme_upwr() +
    theme(legend.position = "top", legend.text = element_text(size = 11))
}

# ============================================================================
# (6) SYMULACJA „PARTIA PO PARTII” — powtórzenia się kumulują
# ============================================================================

p4_proto_batches_ui <- p4_proto_panel("Symulacja", "Partia po partii: n = 100, p = 0.02", proto = FALSE,
  lc_toolbar(
    lc_action_group(p4p_bt_1 = "+1", p4p_bt_10 = "+10", p4p_bt_100 = "+100", p4p_bt_1000 = "+1000",
      label = "Skontroluj partie"),
    lc_action("p4p_bt_reset", icon = "reset", variant = "ghost", aria_label = "Wyzeruj partie"),
    lc_readouts(uiOutput("p4p_bt_read"))
  ),
  lc_plot("p4p_bt_plot", ratio = "2.2/1"),
  lc_caption("Linia: średnia liczba wad ze wszystkich partii. Kropki: model B(100, 0.02).")
)

# ============================================================================
# (7) PRZYPADEK BRZEGOWY „SKRÓT n · p”
# ============================================================================

p4_proto_np_ui <- p4_proto_panel("Przypadek brzegowy", "Ryzyko skumulowane a skrót n · p", proto = FALSE,
  lc_toolbar(
    lc_slider("p4p_np_p", "p w jednej próbie", .001, .02, .005, .001),
    lc_readouts(uiOutput("p4p_np_read"))
  ),
  lc_plot("p4p_np_plot", ratio = "2.2/1"),
  lc_caption("Linia ciągła: 1 - (1 - p)ⁿ. Linia przerywana: skrót n · p.")
)

p4_np_plot <- function(p) {
  n <- 1:300
  d <- data.frame(n = c(n, n), y = c(1 - (1 - p)^n, n * p),
    what = rep(c("exact", "np"), each = length(n)))
  ggplot(d, aes(n, y, linetype = what)) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1, ymax = Inf, fill = upwr_accent, alpha = 0.07) +
    geom_hline(yintercept = 1, colour = upwr_reference, linewidth = 0.6) +
    geom_line(aes(colour = what), linewidth = 1.2) +
    scale_colour_manual(values = c(exact = upwr_accent, np = upwr_secondary), guide = "none") +
    scale_linetype_manual(values = c(exact = "solid", np = "22"), guide = "none") +
    coord_cartesian(ylim = c(0, 1.6)) +
    scale_y_continuous(breaks = seq(0, 1.5, 0.25), labels = function(v) sprintf("%.2f", v)) +
    labs(x = "Liczba prób n", y = "Prawdopodobieństwo") +
    theme_upwr()
}

# ============================================================================
# (8) PRZYKŁADY „TEN SAM WZÓR W TRZECH SYTUACJACH” — wzór (4.6)
# ============================================================================

p4_examples <- data.frame(
  key = c("zawory", "rusztowanie", "wozek"),
  name = c("Partia zaworów", "Mocowania rusztowania", "Kariera kierowcy wózka"),
  situation = c(
    "Dostawa 100 zaworów, każdy niesprawny z prawdopodobieństwem 0.02.",
    "Brygada sprawdza 30 mocowań, każde wadliwe z prawdopodobieństwem 0.01.",
    "Jeden przejazd przez skrzyżowanie hali dziennie, 250 dni w roku przez 10 lat; ryzyko kolizji 0.0001 na przejazd."
  ),
  n = c(100, 30, 2500), p = c(0.02, 0.01, 0.0001),
  decision = c(
    "Prawie każda partia ma jakąś wadę: kontrola przyjęcia musi być regułą, nie wyjątkiem.",
    "Co czwarte rusztowanie ma co najmniej jedno wadliwe mocowanie: sprawdzamy wszystkie, nie próbkę.",
    "Mniej więcej jeden kierowca na pięciu: „to się zdarza rzadko” trzeba mówić razem z horyzontem."
  ),
  stringsAsFactors = FALSE
)
p4_examples$risk <- 1 - (1 - p4_examples$p)^p4_examples$n

p4_proto_examples_ui <- p4_proto_panel("Przykłady", "Ten sam wzór w trzech sytuacjach", proto = FALSE,
  width_mode = "text",
  lc_toolbar(lc_segmented("p4p_ex", NULL, choices = c(setNames(p4_examples$key, p4_examples$name),
    "Zestawienie" = "all"))),
  uiOutput("p4p_ex_out")
)

p4_examples_axis_svg <- function(d = p4_examples) {
  x <- function(v) 40 + v * 600
  ax <- 150
  ticks <- paste(vapply(seq(0, 1, 0.25), function(t) sprintf(
    '<line class="p4p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/><text class="p4p-tick" x="%g" y="%g">%.2f</text>',
    x(t), x(t), ax, ax + 8, x(t), ax + 26, t), character(1)), collapse = "")
  pts <- paste(vapply(seq_len(nrow(d)), function(i) {
    y <- c(24, 64, 104)[i]
    sprintf(paste0('<line x1="%g" x2="%g" y1="%g" y2="%g" stroke="var(--upwr-rule)" stroke-dasharray="3 3"/>',
      '<circle class="p4p-bad" cx="%g" cy="%g" r="8"/>',
      '<text class="p4p-num" x="%g" y="%g" text-anchor="middle">%s: %.3f</text>',
      '<text class="p4p-tick" x="%g" y="%g">n = %s · p = %s</text>'),
      x(d$risk[i]), x(d$risk[i]), y + 22, ax, x(d$risk[i]), ax,
      x(d$risk[i]), y, d$name[i], d$risk[i],
      x(d$risk[i]), y + 16, format(d$n[i], big.mark = " "), format(d$p[i], scientific = FALSE))
  }, character(1)), collapse = "")
  HTML(paste0('<svg class="p4p-svg" style="max-width:700px" viewBox="0 0 680 190" role="img" aria-label="Trzy ryzyka skumulowane na jednej osi, z n i p każdej sytuacji.">',
    sprintf('<line class="p4p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/>', x(0), x(1), ax, ax), ticks, pts, '</svg>'))
}

# ============================================================================
# (10) PIASKOWNICA „KRZYWA OPERACYJNA PLANU (n, c)”
# ============================================================================

p4_oc_good <- 0.02
p4_oc_bad <- 0.08

p4_proto_oc_ui <- p4_proto_panel("Piaskownica", "Krzywa operacyjna planu (n, c)", proto = FALSE,
  lc_toolbar(
    lc_slider("p4p_oc_n", "Liczba kontrolowanych n", 20, 200, 50, 10),
    lc_slider("p4p_oc_c", "Liczba akceptacji c", 0, 6, 1, 1),
    lc_readouts(uiOutput("p4p_oc_read"))
  ),
  lc_plot("p4p_oc_plot", ratio = "2.1/1"),
  lc_caption("Linia ciągła: P(przyjęcia | p) dla ustawionego planu. Przerywana: plan (50, 1). Kropki: partia dobra (p = 0.02) i słaba (p = 0.08).")
)

p4_oc_plot <- function(n, c) {
  p <- seq(0, 0.15, by = 0.001)
  d <- data.frame(p = p, now = pbinom(c, n, p), ref = pbinom(1, 50, p))
  pts <- data.frame(p = c(p4_oc_good, p4_oc_bad), y = pbinom(c, n, c(p4_oc_good, p4_oc_bad)),
    kind = c("good", "bad"))
  ggplot(d, aes(p)) +
    geom_vline(xintercept = c(p4_oc_good, p4_oc_bad), colour = upwr_rule, linewidth = 0.6) +
    geom_line(aes(y = ref), colour = upwr_reference, linetype = "22", linewidth = 0.9) +
    geom_line(aes(y = now), colour = upwr_secondary, linewidth = 1.4) +
    geom_point(data = pts, aes(p, y, fill = kind), shape = 21, size = 5, stroke = 1.2, colour = "white") +
    scale_fill_manual(values = c(good = unname(upwr_cat["szalwia"]), bad = upwr_accent), guide = "none") +
    scale_x_continuous(breaks = seq(0, 0.15, 0.02), labels = function(v) sprintf("%.2f", v)) +
    scale_y_continuous(limits = c(0, 1), labels = function(v) sprintf("%.2f", v)) +
    labs(x = "Jakość partii p (udział niesprawnych)", y = "P(przyjęcia partii)") +
    theme_upwr()
}

# ============================================================================
# (12) GALERIA „TRZECH DOSTAWCÓW BEZ WAD”
# ============================================================================

p4_zero_n <- c(30, 100, 300)

p4_zero_plot <- function() {
  p <- seq(0, 0.14, by = 0.0005)
  d <- do.call(rbind, lapply(p4_zero_n, function(n) data.frame(p = p, y = (1 - p)^n, n = factor(n, p4_zero_n))))
  pu <- data.frame(n = factor(p4_zero_n, p4_zero_n), p = 1 - 0.05^(1 / p4_zero_n), y = 0.05)
  pu$label <- sprintf("%d kontroli: p ≤ %.3f", p4_zero_n, pu$p)
  ggplot(d, aes(p, y, linetype = n)) +
    geom_vline(xintercept = 0.02, colour = upwr_reference, linetype = "22", linewidth = 0.7) +
    geom_hline(yintercept = 0.05, colour = upwr_accent, linewidth = 0.6) +
    geom_line(colour = upwr_secondary, linewidth = 1.1) +
    geom_point(data = pu, aes(p, y), colour = upwr_accent, size = 3.6) +
    geom_label(data = pu, aes(p, y, label = label), nudge_y = c(0.13, 0.25, 0.37), size = 4.2,
      colour = upwr_accent, fill = "white", label.size = 0, hjust = 0.1) +
    scale_linetype_manual(values = c("30" = "solid", "100" = "42", "300" = "12"), guide = "none") +
    scale_x_continuous(breaks = seq(0, 0.14, 0.02), labels = function(v) sprintf("%.2f", v)) +
    scale_y_continuous(breaks = c(0, 0.05, 0.25, 0.5, 0.75, 1), labels = function(v) sprintf("%.2f", v)) +
    labs(x = "Prawdziwe p dostawcy", y = "P(zero wad w n kontrolach)") +
    theme_upwr()
}

p4_proto_zero_ui <- p4_proto_panel("Galeria", "Trzech dostawców bez wad", proto = FALSE,
  lc_plot("p4p_zero_plot", ratio = "2.1/1"),
  lc_caption("Krzywe (1 - p)ⁿ dla 30, 100 i 300 czystych kontroli. Pozioma linia: 5%. Przerywana pionowa: przeciętna jakość Bananpolu, p = 0.02.")
)

# ============================================================================
# (13) DOMKNIĘCIE „JEDEN ROZKŁAD, PIĘĆ PYTAŃ”
# ============================================================================

p4_closing_plot <- function() {
  k <- 0:8
  d <- data.frame(k = k, f = dbinom(k, 100, 0.02))
  brackets <- data.frame(
    x0 = c(-0.4, 0.6, -0.4, 2.6), x1 = c(0.4, 8.4, 2.4, 8.4),
    y = c(0.49, 0.37, 0.43, 0.31),
    label = c("P(X = 0) = 0.133", "P(X ≥ 1) = 0.867 · co najmniej jedna",
      "P(X ≤ 2) = 0.677 · przyjęcie w planie (100, 2)", "P(X ≥ 3) = 0.323")
  )
  ggplot(d, aes(k, f)) +
    geom_col(fill = upwr_secondary, width = 0.75) +
    annotate("segment", x = 2, xend = 2, y = 0, yend = 0.29, colour = upwr_accent, linewidth = 1) +
    annotate("label", x = 2, y = 0.02, label = "E(X) = 2", colour = upwr_accent, fill = "white",
      label.size = 0, size = 4.4, fontface = "bold") +
    geom_segment(data = brackets, aes(x = x0, xend = x1, y = y, yend = y), inherit.aes = FALSE,
      colour = upwr_accent, linewidth = 0.9) +
    geom_segment(data = brackets, aes(x = x0, xend = x0, y = y, yend = y - 0.012), inherit.aes = FALSE,
      colour = upwr_accent, linewidth = 0.9) +
    geom_segment(data = brackets, aes(x = x1, xend = x1, y = y, yend = y - 0.012), inherit.aes = FALSE,
      colour = upwr_accent, linewidth = 0.9) +
    geom_text(data = brackets, aes(x = x0, y = y + 0.022, label = label), inherit.aes = FALSE,
      size = 4.2, colour = upwr_ink, hjust = 0) +
    scale_x_continuous(breaks = k) +
    scale_y_continuous(limits = c(0, 0.52), breaks = seq(0, 0.3, 0.1), labels = function(v) sprintf("%.1f", v)) +
    labs(x = "Liczba niesprawnych w partii 100 zaworów", y = "P(X = k)") +
    theme_upwr(grid = "y")
}

p4_proto_closing_ui <- p4_proto_panel("Domknięcie", "Jeden rozkład, pięć pytań: B(100, 0.02)", proto = FALSE,
  lc_plot("p4p_closing_plot", ratio = "2/1")
)

# ============================================================================
# SERWER
# ============================================================================

p4_proto_charts_server <- function(input, output, session) {
  # (3) galeria założeń
  assume <- reactive(input$p4p_assume %||% "stable")
  split <- reactive(identical(assume(), "mixture") && identical(input$p4p_split, "apart"))
  zoom_plot_server("p4p_assume_plot", reactive(if (split()) p4_assumptions_split_plot() else p4_assumptions_plot(assume())),
    alt = "Histogram liczby wad w symulowanych partiach na tle modelu dwumianowego.")
  output$p4p_assume_caption <- renderUI(lc_caption(if (split())
    "Te same 4000 partii rozdzielone według magazynu. Kropki: osobny model B(100, p) dla każdego magazynu; każdy pasuje, bo w magazynie p jest stałe."
    else "Słupki: 4000 symulowanych partii po 100 zaworów. Kropki: model B(100, 0.02)."))
  output$p4p_assume_read <- renderUI({
    if (split()) {
      x <- p4_assumption_sims$mixture; wp <- p4_assumption_sims$mixture_p
      return(tagList(
        lc_readout("Średnia A · B", sprintf("%.2f · %.2f", mean(x[wp == 0.01]), mean(x[wp == 0.03]))),
        lc_readout("P(X = 0) w A", sprintf("%.3f", mean(x[wp == 0.01] == 0)), color = unname(upwr_cat["szalwia"])),
        lc_readout("P(X = 0) w B", sprintf("%.3f", mean(x[wp == 0.03] == 0)), color = upwr_accent)
      ))
    }
    x <- p4_assumption_sims[[assume()]]
    tagList(
      lc_readout("Średnia", sprintf("%.2f", mean(x))),
      lc_readout("P(X = 0)", sprintf("%.3f", mean(x == 0)), color = upwr_accent),
      lc_readout("P(X ≥ 5)", sprintf("%.3f", mean(x >= 5)), color = upwr_accent)
    )
  })

  # galeria B(n, p)
  zoom_plot_server("p4p_bg_plot", reactive({
    req(length(input$p4p_bg) > 0)
    p4_binom_gallery_plot(input$p4p_bg)
  }), alt = "Rozkłady dwumianowe dla wybranych scenariuszy, nałożone na jednej osi.")
  output$p4p_bg_stats <- renderUI({
    req(length(input$p4p_bg) > 0)
    tags$ul(lapply(p4_binom_gallery_defs[input$p4p_bg], function(s) tags$li(sprintf("%s:  E(X) = %s,  SD = %.2f",
      s$label, format(round(s$n * s$p, 1)), sqrt(s$n * s$p * (1 - s$p))))))
  })

  # (6) partia po partii
  batches <- reactiveVal(integer(0))
  add <- function(m) batches(c(batches(), rbinom(m, 100, 0.02)))
  observeEvent(input$p4p_bt_1, add(1))
  observeEvent(input$p4p_bt_10, add(10))
  observeEvent(input$p4p_bt_100, add(100))
  observeEvent(input$p4p_bt_1000, add(1000))
  observeEvent(input$p4p_bt_reset, batches(integer(0)))
  zoom_plot_server("p4p_bt_plot", reactive({
    x <- batches(); m <- length(x); kmax <- 9
    obs <- data.frame(k = 0:kmax, f = if (m) vapply(0:kmax, function(k) mean(pmin(x, kmax) == k), numeric(1)) else 0)
    ref <- data.frame(k = 0:kmax, f = dbinom(0:kmax, 100, 0.02))
    g <- ggplot() +
      geom_col(data = obs, aes(k, f), fill = upwr_secondary, width = 0.75) +
      geom_point(data = ref, aes(k, f), shape = 21, fill = "white", colour = upwr_ink, size = 2.6, stroke = 1.2) +
      scale_x_continuous(breaks = 0:kmax, labels = c(0:(kmax - 1), paste0(kmax, "+"))) +
      scale_y_continuous(limits = c(0, 0.45), labels = function(v) sprintf("%.2f", v)) +
      labs(x = "Liczba niesprawnych w partii", y = "Udział partii") +
      theme_upwr(grid = "y")
    if (m) g <- g + geom_vline(xintercept = mean(x), colour = upwr_accent, linewidth = 1.1)
    g
  }), alt = "Histogram liczby wad w kolejnych partiach, z bieżącą średnią i modelem dwumianowym.")
  output$p4p_bt_read <- renderUI({
    x <- batches(); m <- length(x)
    tagList(
      lc_readout("Partie", m),
      lc_readout("Średnia", if (m) sprintf("%.2f", mean(x)) else "—", color = upwr_accent),
      lc_readout("Dokładnie 2", if (m) sprintf("%.0f%%", 100 * mean(x == 2)) else "—"),
      lc_readout("Zero", if (m) sprintf("%.0f%%", 100 * mean(x == 0)) else "—")
    )
  })

  # (7) skrót n · p
  np_p <- reactive(input$p4p_np_p %||% 0.005)
  zoom_plot_server("p4p_np_plot", reactive(p4_np_plot(np_p())),
    alt = "Krzywa ryzyka skumulowanego i prosta n razy p, która przekracza 1.")
  output$p4p_np_read <- renderUI({
    p <- np_p(); n_cross <- ceiling(1 / p)
    tagList(
      lc_readout("n = 100", sprintf("%.3f vs %.3f", risk_at_least_one(100, p), 100 * p)),
      lc_readout("n · p = 1 przy n", n_cross, color = upwr_accent),
      lc_readout("Wtedy naprawdę", sprintf("%.3f", risk_at_least_one(n_cross, p)))
    )
  })

  # (8) przykłady
  output$p4p_ex_out <- renderUI({
    key <- input$p4p_ex %||% p4_examples$key[1]
    if (identical(key, "all")) {
      return(tagList(p4_proto_css, p4_examples_axis_svg(),
        lc_caption("Trzy sytuacje, jeden wzór 1 - (1 - p)ⁿ. Ryzyko zależy od p i n naraz: kierowca ma p sto razy mniejsze niż mocowania, a ryzyko prawie takie samo, bo przejazdów jest 2500, a mocowań 30.")))
    }
    e <- p4_examples[p4_examples$key == key, ]
    tagList(p4_proto_css, tags$div(class = "p4p-card",
      tags$h4(e$name),
      tags$p(e$situation),
      tags$p(class = "p4p-calc", sprintf("n = %s · p = %s", format(e$n, big.mark = " "), format(e$p, scientific = FALSE))),
      tags$p(class = "p4p-calc", sprintf("P(X ≥ 1) = 1 - %s%s ≈ %.3f", format(1 - e$p, scientific = FALSE),
        chartr("0123456789", "⁰¹²³⁴⁵⁶⁷⁸⁹", format(e$n, big.mark = "")), e$risk)),
      tags$p(class = "p4p-verdict", e$decision)
    ))
  })

  # (10) krzywa operacyjna
  oc_n <- reactive(input$p4p_oc_n %||% 50)
  oc_c <- reactive(input$p4p_oc_c %||% 1)
  zoom_plot_server("p4p_oc_plot", reactive(p4_oc_plot(oc_n(), oc_c())),
    alt = "Krzywa operacyjna planu odbioru: prawdopodobieństwo przyjęcia partii w zależności od jej jakości.")
  output$p4p_oc_read <- renderUI({
    n <- oc_n(); c <- oc_c()
    tagList(
      lc_readout("Ryzyko dostawcy: dobra odrzucona", sprintf("%.3f", 1 - pbinom(c, n, p4_oc_good)), color = unname(upwr_cat["szalwia"])),
      lc_readout("Ryzyko odbiorcy: słaba przyjęta", sprintf("%.3f", pbinom(c, n, p4_oc_bad)), color = upwr_accent)
    )
  })

  # (12) zero wad, (13) domknięcie
  zoom_plot_server("p4p_zero_plot", reactive(p4_zero_plot()),
    alt = "Trzy krzywe prawdopodobieństwa zera wad i górne granice p dla 30, 100 i 300 kontroli.")
  zoom_plot_server("p4p_closing_plot", reactive(p4_closing_plot()),
    alt = "Rozkład B(100, 0.02) z zaznaczonymi odpowiedziami na pytania z wykładu.")
}
