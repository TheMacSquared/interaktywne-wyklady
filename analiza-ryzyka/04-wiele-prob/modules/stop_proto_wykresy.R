# PUNKTY STOP WYKRESOWE (przyjęte 2026-10-28) ---------------------------------
#
# Wykresy rysuje przeglądarka: ryciny lcSF (R/lc_stat_figures.js, ECharts)
# zarejestrowane w modules/wykresy.js, więc działają też offline. Tu zostają
# kontrolki, dane liczone raz przy starcie (symulacje ze stałym ziarnem) i opisy.
# Numery jak w analizie punktów stop:
#   p4_proto_assumptions_ui — Galeria „Trzy partie, ta sama średnia” (3), zaakceptowana 2026-10-28
#   p4_proto_batches_ui     — Symulacja „Partia po partii” (6), zaakceptowany 2026-10-28
#   p4_proto_binom_gallery_ui — Galeria „Rozkład dwumianowy B(n, p)” (Ryc. 3.4 ze statystyki 03), zaakceptowana 2026-10-28
#   p4_proto_np_ui          — Przypadek brzegowy „Skrót n · p” (7), zaakceptowany 2026-10-28
#   p4_proto_examples_ui    — Przykłady „Ten sam wzór w trzech sytuacjach” (8), zaakceptowany 2026-10-28
#   p4_proto_oc_ui          — Piaskownica „Krzywa operacyjna planu (n, c)” (10), zaakceptowany 2026-10-28
#   p4_proto_zero_ui        — Galeria „Trzech dostawców bez wad” (12), zaakceptowany 2026-10-28
#   p4_proto_closing_ui     — Domknięcie „Jeden rozkład, pięć pytań” (13), zaakceptowany 2026-10-28
# Serwer: p4_proto_charts_server(input, output, session) — tylko przykłady (8).
# Wymaga: palette.R, lecture_layout.R, lc_stat_figures.R, risk_math.R, risk_block.R.

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

# Odczyt wypełniany przez rycinę (etykieta też, gdy zmienia się z widokiem).
p4_sf_read <- function(key, label, color = NULL) {
  lc_readout(lc_sf_value(paste0(key, "_l"), label), lc_sf_value(key), color = color)
}

# ============================================================================
# (3) GALERIA „TRZY PARTIE, TA SAMA ŚREDNIA” — przypadek brzegowy założeń
# ============================================================================

# Symulacja raz przy starcie (stałe ziarno); do przeglądarki idą gotowe udziały
# i odczyty. Zależność: po niesprawnym zaworze następny zawodzi z p = 0.2; p po
# sprawnym dobrane tak, żeby długookresowy udział wad wynosił 0.02. Przy mieszance
# pamiętamy magazyn każdej partii (mixture_p), żeby dało się ją rozdzielić.
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

p4_assumption_config <- local({
  S <- p4_assumption_sims; kmax <- 10
  share <- function(x) vapply(0:kmax, function(k) mean(pmin(x, kmax) == k), numeric(1))
  reads <- function(x) list(mean = sprintf("%.2f", mean(x)), p0 = sprintf("%.3f", mean(x == 0)),
    p5 = sprintf("%.3f", mean(x >= 5)))
  a <- S$mixture[S$mixture_p == 0.01]; b <- S$mixture[S$mixture_p == 0.03]
  list(
    ref = dbinom(0:kmax, 100, 0.02),
    obs = lapply(S[c("stable", "mixture", "dependent")], share),
    reads = lapply(S[c("stable", "mixture", "dependent")], reads),
    split = list(obsA = share(a), obsB = share(b), refA = dbinom(0:kmax, 100, 0.01), refB = dbinom(0:kmax, 100, 0.03),
      mean = sprintf("%.2f · %.2f", mean(a), mean(b)), p0A = sprintf("%.3f", mean(a == 0)), p0B = sprintf("%.3f", mean(b == 0)))
  )
})

p4_proto_assumptions_ui <- p4_proto_panel("Galeria", "Trzy partie, ta sama średnia", proto = FALSE,
  lc_sf_figure("p4_assume",
    control = list(
      lc_segmented("p4p_assume", "Partia", choices = c("Stabilna linia" = "stable",
        "Dwie dostawy" = "mixture", "Wada ciągnie następną" = "dependent")),
      lc_segmented("p4p_split", "Magazyny", choices = c("Razem" = "together", "Osobno" = "apart"))
    ),
    toolbar = lc_readouts(p4_sf_read("as1", "Średnia"), p4_sf_read("as2", "P(X = 0)", upwr_accent),
      p4_sf_read("as3", "P(X ≥ 5)", upwr_accent)),
    ratio = "2/1", max_height = "380px", config = p4_assumption_config, selected = "joint",
    notes = list(
      joint = lc_caption("Słupki: 4000 symulowanych partii po 100 zaworów. Kropki: model B(100, 0.02)."),
      split = lc_caption("Te same 4000 partii rozdzielone według magazynu. Kropki: osobny model B(100, p) dla każdego magazynu; każdy pasuje, bo w magazynie p jest stałe.")
    ))
)

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
  lc_sf_figure("p4_bg",
    control = checkboxGroupInput("p4p_bg", "Scenariusze",
      choices = setNames(names(p4_binom_gallery_defs), vapply(p4_binom_gallery_defs, `[[`, "", "label")),
      selected = "b1"),
    ratio = "2/1", max_height = "400px", legend = TRUE,
    config = list(defs = lapply(p4_binom_gallery_defs, function(s) c(s, list(
      mean = format(round(s$n * s$p, 1)), sd = sprintf("%.2f", sqrt(s$n * s$p * (1 - s$p))))))),
    after = tags$div(`data-role` = "stats"))
)

# ============================================================================
# (6) SYMULACJA „PARTIA PO PARTII” — powtórzenia się kumulują
# ============================================================================

p4_proto_batches_ui <- p4_proto_panel("Symulacja", "Partia po partii: n = 100, p = 0.02", proto = FALSE,
  lc_sf_figure("p4_bt",
    toolbar = tagList(
      lc_sf_action_group("Skontroluj partie", one = "+1", ten = "+10", hund = "+100", thou = "+1000"),
      lc_sf_action("reset", "p4p_bt_reset", icon = "reset", variant = "ghost", aria_label = "Wyzeruj partie"),
      lc_readouts(p4_sf_read("bt1", "Partie"), p4_sf_read("bt2", "Średnia", upwr_accent),
        p4_sf_read("bt3", "Dokładnie 2"), p4_sf_read("bt4", "Zero"))
    ),
    ratio = "2.2/1", max_height = "360px",
    after = lc_caption("Linia: średnia liczba wad ze wszystkich partii. Kropki: model B(100, 0.02)."))
)

# ============================================================================
# (7) PRZYPADEK BRZEGOWY „SKRÓT n · p”
# ============================================================================

p4_proto_np_ui <- p4_proto_panel("Przypadek brzegowy", "Ryzyko skumulowane a skrót n · p", proto = FALSE,
  lc_sf_figure("p4_np",
    control = lc_sf_slider("p4p_np_p", "p w jednej próbie", .001, .02, .005, .001),
    toolbar = lc_readouts(p4_sf_read("np1", "n = 100"), p4_sf_read("np2", "n · p = 1 przy n", upwr_accent),
      p4_sf_read("np3", "Wtedy naprawdę")),
    ratio = "2.2/1", max_height = "360px",
    after = lc_caption("Linia ciągła: 1 - (1 - p)ⁿ. Linia przerywana: skrót n · p."))
)

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
  lc_sf_figure("p4_oc",
    control = list(lc_sf_slider("p4p_oc_n", "Liczba kontrolowanych n", 20, 200, 50, 10),
      lc_sf_slider("p4p_oc_c", "Liczba akceptacji c", 0, 6, 1, 1)),
    toolbar = lc_readouts(p4_sf_read("oc1", "Ryzyko dostawcy: dobra odrzucona", unname(upwr_cat["szalwia"])),
      p4_sf_read("oc2", "Ryzyko odbiorcy: słaba przyjęta", upwr_accent)),
    ratio = "2.1/1", max_height = "360px", config = list(good = p4_oc_good, bad = p4_oc_bad),
    after = lc_caption("Linia ciągła: P(przyjęcia | p) dla ustawionego planu. Przerywana: plan (50, 1). Kropki: partia dobra (p = 0.02) i słaba (p = 0.08)."))
)

# ============================================================================
# (12) GALERIA „TRZECH DOSTAWCÓW BEZ WAD”
# ============================================================================

p4_zero_n <- c(30, 100, 300)

p4_proto_zero_ui <- p4_proto_panel("Galeria", "Trzech dostawców bez wad", proto = FALSE,
  lc_sf_figure("p4_zero", ratio = "2.1/1", max_height = "380px", config = list(n = p4_zero_n),
    after = lc_caption("Krzywe (1 - p)ⁿ dla 30, 100 i 300 czystych kontroli. Pozioma linia: 5%. Przerywana pionowa: przeciętna jakość Bananpolu, p = 0.02."))
)

# ============================================================================
# (13) DOMKNIĘCIE „JEDEN ROZKŁAD, PIĘĆ PYTAŃ”
# ============================================================================

p4_proto_closing_ui <- p4_proto_panel("Domknięcie", "Jeden rozkład, pięć pytań: B(100, 0.02)", proto = FALSE,
  lc_sf_figure("p4_closing", ratio = "2/1", max_height = "420px")
)

# ============================================================================
# SERWER — tylko przykłady (8); wykresy liczy przeglądarka
# ============================================================================

p4_proto_charts_server <- function(input, output, session) {
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
}
