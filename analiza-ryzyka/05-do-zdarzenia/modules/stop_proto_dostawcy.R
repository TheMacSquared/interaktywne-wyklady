# PUNKTY STOP (2026-11-04) — dlaczego mieszanka dostawców wydłuża średnią (zamiast przykładu 5.8)
#
#   d5p_dost_scene_ui — Scena „Dziesięć audytów u każdego dostawcy”, 3 kadry:
#     B (p = 0.15) → A (p = 0.05) → razem; każdy audyt to pasek długości X
#     z kreskami w miejscach trzech wad. Ziarno 4398: średnie 20.1 i 60.0.
# Kadry rysuje R z góry (d5p_frames_ui), przełącza je modules/sceny.js.
# Wymaga: stop_proto_common.R (d5p_panel, d5p_css, d5p_svg, d5p_axis).

d5p_dost_audits <- local({
  set.seed(4398)
  gen <- function(p) lapply(1:10, function(i) cumsum(rgeom(3, p) + 1))
  list(B = gen(0.15), A = gen(0.05))
})
d5p_dost_mean <- function(l) mean(vapply(l, function(x) x[3], numeric(1)))
d5p_dost_col <- c(B = "var(--upwr-cat-niebo)", A = "var(--upwr-accent)")

d5p_dost_frames <- list(
  list(name = "Dostawca B: p = 0.15",
    text = "Dziesięć audytów u dostawcy z częstszymi wadami. Każdy pasek to jeden audyt, kreski to trzy wady; pasek kończy się na trzeciej wadzie.",
    read = sprintf("Średnio %.0f kontroli", d5p_dost_mean(d5p_dost_audits$B))),
  list(name = "Dostawca A: p = 0.05",
    text = "Dziesięć audytów u dostawcy z rzadszymi wadami. Jego towar jest lepszy, ale wady są trzy razy rzadsze, więc audyt trwa około trzy razy dłużej.",
    read = sprintf("Średnio %.0f kontroli", d5p_dost_mean(d5p_dost_audits$A))),
  list(name = "Razem: średnie p = 0.10",
    text = "Wszystkie dwadzieścia audytów razem. Średnie p obu dostawców to 0.10, a model ze średnim p obiecuje 30 kontroli. Krótkie paski B nie nadrabiają długich pasków A.",
    read = sprintf("Średnio %.0f kontroli, a nie 30", mean(c(d5p_dost_mean(d5p_dost_audits$A), d5p_dost_mean(d5p_dost_audits$B)))))
)

d5p_dost_svg <- function(step) {
  W <- 780; x0 <- 120; x1 <- 760; xmax <- 120; rowh <- 13; gap <- 26
  x_of <- function(t) x0 + pmin(t, xmax) / xmax * (x1 - x0)
  block <- function(key, y0, dim = FALSE) {
    rows <- vapply(seq_along(d5p_dost_audits[[key]]), function(i) {
      a <- d5p_dost_audits[[key]][[i]]; y <- y0 + (i - 1) * rowh
      paste0(
        sprintf('<rect x="%g" y="%g" width="%g" height="8" rx="2" class="d5p-cell-ok" style="opacity:%s"/>',
          x0, y, x_of(a[3]) - x0, if (dim) .35 else 1),
        paste(sprintf('<rect x="%g" y="%g" width="3" height="8" style="fill:%s"/>', x_of(a) - 1.5, y, d5p_dost_col[[key]]), collapse = ""),
        if (a[3] > xmax) sprintf('<text class="d5p-mono d5p-subtle" x="%g" y="%g" font-size="10">→ %d</text>', x1 + 3, y + 8, a[3]) else "")
    }, character(1))
    m <- d5p_dost_mean(d5p_dost_audits[[key]])
    paste0(
      sprintf('<text x="0" y="%g" font-size="13" font-weight="700" style="fill:%s">Dostawca %s</text>', y0 + 30, d5p_dost_col[[key]], key),
      sprintf('<text class="d5p-mono d5p-subtle" x="0" y="%g" font-size="11">p = %s</text>', y0 + 46, if (key == "A") "0.05" else "0.15"),
      paste(rows, collapse = ""),
      sprintf('<line x1="%g" x2="%g" y1="%g" y2="%g" style="stroke:%s" stroke-width="2"/>', x_of(m), x_of(m), y0 - 4, y0 + 10 * rowh, d5p_dost_col[[key]]),
      sprintf('<text class="d5p-mono" x="%g" y="%g" font-size="12" style="fill:%s">średnio %.0f</text>', x_of(m) + 5, y0 + 10 * rowh + 2, d5p_dost_col[[key]], m))
  }
  yB <- 16; yA <- yB + 10 * rowh + gap
  parts <- c(if (step >= 1) block("B", yB), if (step >= 2) block("A", yA))
  base <- yA + 10 * rowh + 14
  extra <- if (step == 3) {
    mall <- mean(c(d5p_dost_mean(d5p_dost_audits$A), d5p_dost_mean(d5p_dost_audits$B)))
    paste0(
      sprintf('<line class="d5p-limit" x1="%g" x2="%g" y1="%g" y2="%g"/>', x_of(30), x_of(30), yB - 8, base),
      sprintf('<text class="d5p-mono d5p-subtle" x="%g" y="%g" font-size="12" text-anchor="end">model: 30</text>', x_of(30) - 4, base - 4),
      sprintf('<line x1="%g" x2="%g" y1="%g" y2="%g" style="stroke:var(--upwr-ink)" stroke-width="2.5"/>', x_of(mall), x_of(mall), yB - 8, base),
      sprintf('<text class="d5p-mono" x="%g" y="%g" font-size="13" font-weight="700">razem: %.0f</text>', x_of(mall) + 5, base - 4, mall))
  } else ""
  d5p_svg(W, base + 40, "Dwadzieścia audytów dwóch dostawców jako paski długości audytu z kreskami wad.",
    paste(parts, collapse = ""), extra,
    d5p_axis(x_of, base + 6, c(0, 20, 40, 60, 80, 100, 120), label = "liczba kontroli do trzeciej wady"))
}

d5p_dost_scene_ui <- d5p_frames_ui("Scena", "Dziesięć audytów u każdego dostawcy",
  d5p_dost_frames, d5p_dost_svg, d5p_frame_body)
