# SCENA (2026-11-17, przyjęta) — grupa rj: P19B, rozdział 6 -----------------------------
#
# Dwa światy z powtórzeniem, przed pojęciem skośności. Dwa procesy z inżynierii
# bezpieczeństwa o tej samej średniej i tym samym SD: A symetryczny N(μ, σ), B z twardą
# dolną granicą i długim prawym ogonem (min + Gamma(k, θ)). Przy progu 3σ nad średnią
# B przekracza go kilka razy częściej. Losowanie i rysunek: modules/proto_rj.js
# (scena „dwa_swiaty”). Liczby fikcyjne.
#
#   (Warianty odrzucone: P19 „Dwie trasy do pracy” — nie z inżynierii bezpieczeństwa;
#   P19A „Amoniak w dwóch halach chłodni” — słabiej czytelny skutek przekroczenia.)
#   z6p_19b_ui — Czas zatrzymania dwóch pras po przecięciu kurtyny świetlnej (P19B):
#                dopuszczalne 260 ms. Prasa A N(200, 20); prasa B 160 + Gamma(4, 10)
#                (szybciej niż mechanika pozwala się nie da, czasem zwłoka zaworu).
#                1.3 wobec 10 na 1000 zatrzymań.
# Wymaga: stop_proto_common.R (z6p_scene_ui, z6p_out, z6p_button).

z6p19_css <- tags$style(HTML("
  .z6p19-bar { fill: color-mix(in srgb, var(--upwr-secondary) 70%, var(--upwr-surface)); }
  .z6p19-today { fill: var(--upwr-ink); }
"))

z6p19_ui <- function(num, title, cfg, caption, proto = TRUE) {
  tagList(z6p19_css,
    z6p_scene_ui(num, "scena", title, "dwa_swiaty", cfg, proto = proto,
      z6p_out("stage"),
      z6p_out("read"),
      tags$div(class = "z6p-nav",
        lapply(seq_along(cfg$steps), function(i) {
          s <- cfg$steps[[i]]
          z6p_button(paste0("add", s$n), s$label, variant = if (i == 1) "solid" else "outline")
        }),
        z6p_button("reset", icon = "reset", variant = "ghost", aria_label = "Od nowa")),
      tags$p(class = "z6p-caption", caption)))
}

# ---- P19B: czas zatrzymania prasy ----------------------------------------------------
z6p19b_cfg <- list(
  unit = "ms", digits = 0, axis = c(140, 320), bin = 5,
  ticks = c(160, 200, 240, 280, 320), tick_labels = c("160", "200", "240", "280", "320 ms"),
  limit = 260, limit_label = "dopuszczalne: 260 ms",
  item = "Zatrzymanie", over_word = "dłużej niż 260 ms",
  aria = "Histogramy czasów zatrzymania dwóch pras po przecięciu kurtyny świetlnej; zatrzymania dłuższe niż 260 ms wyróżnione.",
  a = list(name = "Prasa A", sub = "hamulec mechaniczny: raz szybciej, raz wolniej", who = "Prasa A", mu = 200, sd = 20),
  b = list(name = "Prasa B", sub = "zwykle szybko, czasem zwłoka zaworu", who = "Prasa B", min = 160, k = 4, theta = 10),
  steps = list(list(n = 1, label = "Kolejne zatrzymanie"), list(n = 100, label = "+ tydzień (100)"), list(n = 1000, label = "+ 1000"))
)

z6p_19b_ui <- z6p19_ui("P19B", "Czas zatrzymania dwóch pras", z6p19b_cfg,
  "Gdy operator przetnie kurtynę świetlną, prasa musi się zatrzymać. Odległość kurtyny od narzędzia dobrano dla czasu zatrzymania najwyżej 260 ms.",
  proto = FALSE)
