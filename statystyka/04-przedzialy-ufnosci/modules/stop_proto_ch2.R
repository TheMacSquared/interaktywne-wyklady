# PROTOTYPY PUNKTÓW STOP — rozdział 2 (2026-10-30) ---------------------------
#   s4p_2a_ui — Ryc. 2.1 pojęcie „Odwrócenie: od μ do x̄ i z powrotem” (JS: s4p_ch2.js;
#     zaakceptowany 2026-10-30, zastąpił statyczną Ryc. 2.1)
#     Puenta: x̄ leży w pasie μ ± 1.96·SE dokładnie wtedy, gdy przedział x̄ ± 1.96·SE łapie μ.
# Prototypy 2b, 2c i 2d usunięte 2026-11-09: zastąpiły je warianty B3 i B4
# (brainstorm.R), które korzystają z s4p_2_draw().

# Świat wykładu: wzrost N(170, 10) cm, grupka n = 25.
s4p_2_mu    <- 170
s4p_2_sigma <- 10
s4p_2_n     <- 25L

# k grupek po n osób z N(mu, sigma): x̄ i s każdej grupki.
s4p_2_draw <- function(k, n = s4p_2_n, mu = s4p_2_mu, sigma = s4p_2_sigma) {
  if (k <= 0) return(data.frame(xbar = numeric(0), s = numeric(0)))
  x <- matrix(rnorm(k * n, mu, sigma), nrow = n)
  data.frame(xbar = colMeans(x), s = apply(x, 2, sd))
}

# ============================================================================
# (2a) POJĘCIE „PRZECIĄGNIJ x̄” — tylko UI, rysunek i logika w s4p_ch2.js
# ============================================================================

s4p_2a_ui <- s4p_panel("2a", "Ryc. 2.1", "Odwrócenie: od μ do x̄ i z powrotem", width_mode = "text",
  proto = FALSE,
  tags$style(HTML("
    .s4p-flip-stage svg { display: block; width: 100%; height: auto; touch-action: none;
      user-select: none; -webkit-user-select: none; cursor: pointer; }
    .s4p-flip-handle { cursor: grab; outline: none; }
    .s4p-flip-handle:active { cursor: grabbing; }
    .s4p-flip-handle:focus-visible .s4p-flip-hit { fill: none; stroke: var(--upwr-accent); stroke-width: 2; }
    .s4p-flip-read { margin-top: 0.6rem; font-family: var(--upwr-mono); font-size: 0.95em;
      color: var(--upwr-ink); line-height: 1.5; }
    .s4p-flip-verdict { font-weight: 400; }
    .s4p-flip-verdict[data-hit='yes'] { color: var(--upwr-cat-szalwia); }
    .s4p-flip-verdict[data-hit='no'] { color: var(--upwr-cat-terakota); }
  ")),
  tags$div(class = "s4p-flip", `data-mu` = "170", `data-se` = "2", `data-x` = "170",
    `data-min` = "163", `data-max` = "177",
    tags$div(class = "s4p-flip-stage"),
    tags$div(class = "s4p-flip-read", `aria-live` = "polite")
  ),
  lc_caption("x̄ w pasie μ ± 1.96·SE ⇔ μ w przedziale x̄ ± 1.96·SE.")
)
