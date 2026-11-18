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
#   z6p_19b_ui — To samo łożysko w dwóch komorach dojrzewalni (P19B, od 18.11.2026; wcześniej
#                czas zatrzymania pras). Komora 1: smarownik automatyczny, N(82, 3).
#                Komora 2: ręczne smarowanie raz w tygodniu (najchłodniej tuż po smarowaniu,
#                spóźnione smarowanie = prawy ogon): 76.8 + Gamma(3, √3), ta sama średnia i SD — ten sam
#                model skośny co w galerii P14A. Próg 91°C (3σ): 1.3 wobec 12 na 1000 pomiarów.
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

# ---- P19B: łożysko w dwóch komorach ---------------------------------------------------
z6p19b_cfg <- list(
  unit = "°C", digits = 1, axis = c(72, 100), bin = 1,
  ticks = c(75, 80, 85, 90, 95, 100), tick_labels = c("75", "80", "85", "90", "95", "100°C"),
  limit = 91, limit_label = "próg 91°C",
  item = "Pomiar", over_word = "ponad 91°C",
  aria = "Histogramy temperatury tego samego łożyska w dwóch komorach dojrzewalni; pomiary powyżej 91°C wyróżnione.",
  a = list(name = "Komora 1", sub = "smarownik co godzinę", who = "Komora 1", mu = z6p_mu, sd = z6p_sd),
  b = list(name = "Komora 2", sub = "smarowanie raz w tygodniu", who = "Komora 2",
    min = z6p_mu - sqrt(3) * z6p_sd, k = 3, theta = z6p_sd / sqrt(3)),
  steps = list(list(n = 1, label = "Kolejny pomiar"), list(n = 100, label = "+ 100 pomiarów"), list(n = 1000, label = "+ 1000"))
)

z6p_19b_ui <- z6p19_ui("P19B", "To samo łożysko w dwóch komorach", z6p19b_cfg,
  "W komorze 1 automatyczny smarownik podaje smar co godzinę. W komorze 2 konserwator smaruje łożysko raz w tygodniu: zaraz po smarowaniu jest najchłodniej, potem z każdym dniem cieplej, a gdy smarowanie się spóźni, łożysko grzeje się dalej.",
  proto = FALSE)
