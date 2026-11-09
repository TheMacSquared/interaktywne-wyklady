# RYCINY — rozdział 2 -----------------------------------------------------------
#   fig_2a_ui — Ryc. 2.1 „Odwrócenie: od μ do x̄ i z powrotem” (JS: fig_ch2.js).
#     Puenta: x̄ leży w pasie μ ± 1.96·SE dokładnie wtedy, gdy przedział x̄ ± 1.96·SE łapie μ.
#   fig_2_draw() — próby dla Ryc. 2.2 (ci_explorers.R).

# Świat wykładu: wzrost N(170, 10) cm, grupka n = 25.
fig_2_mu    <- 170
fig_2_sigma <- 10
fig_2_n     <- 25L

# k grupek po n osób z N(mu, sigma): x̄ i s każdej grupki.
fig_2_draw <- function(k, n = fig_2_n, mu = fig_2_mu, sigma = fig_2_sigma) {
  if (k <= 0) return(data.frame(xbar = numeric(0), s = numeric(0)))
  x <- matrix(rnorm(k * n, mu, sigma), nrow = n)
  data.frame(xbar = colMeans(x), s = apply(x, 2, sd))
}

# ============================================================================
# (2a) POJĘCIE „PRZECIĄGNIJ x̄” — tylko UI, rysunek i logika w fig_ch2.js
# ============================================================================

fig_2a_ui <- fig_panel("2a", "Ryc. 2.1", "Odwrócenie: od μ do x̄ i z powrotem", width_mode = "text",
  tags$style(HTML("
    .fig-flip-stage svg { display: block; width: 100%; height: auto; touch-action: none;
      user-select: none; -webkit-user-select: none; cursor: pointer; }
    .fig-flip-handle { cursor: grab; outline: none; }
    .fig-flip-handle:active { cursor: grabbing; }
    .fig-flip-handle:focus-visible .fig-flip-hit { fill: none; stroke: var(--upwr-accent); stroke-width: 2; }
    .fig-flip-read { margin-top: 0.6rem; font-family: var(--upwr-mono); font-size: 0.95em;
      color: var(--upwr-ink); line-height: 1.5; }
    .fig-flip-verdict { font-weight: 400; }
    .fig-flip-verdict[data-hit='yes'] { color: var(--upwr-cat-szalwia); }
    .fig-flip-verdict[data-hit='no'] { color: var(--upwr-cat-terakota); }
  ")),
  tags$div(class = "fig-flip", `data-mu` = "170", `data-se` = "2", `data-x` = "170",
    `data-min` = "163", `data-max` = "177",
    tags$div(class = "fig-flip-stage"),
    tags$div(class = "fig-flip-read", `aria-live` = "polite")
  ),
  lc_caption("x̄ w pasie μ ± 1.96·SE ⇔ μ w przedziale x̄ ± 1.96·SE.")
)
