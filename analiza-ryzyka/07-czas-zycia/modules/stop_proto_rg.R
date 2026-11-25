# PROTOTYPY PUNKTÓW STOP (2026-11-14) — grupa rg (rozdz. 4, gamma) ----------------
#
#   c7p_13_ui — Scena „Magazyn z trzema zapasowymi” (P13): historia z powtórzeniem,
#               scena magazyn w modules/proto_rg.js (window.lcC7). Liczby z przykładu
#               7.6: awarie średnio co 500 h, trzy zapasowe wentylatory, dostawa po
#               1000 h; P(T₃ > 1000) = 5e⁻² ≈ 0.677, E(T₃) = 1500 h.
#   c7p_24_ui — Scena „Wentylator za wentylatorem” (P24): ta sama scena magazyn
#               z k = 1, czyli czas do pierwszej awarii przy stałym ryzyku
#               (oferta A, MTTF 1500 h). Wentylator po wentylatorze powstaje
#               rozkład wykładniczy: R(1000 h) = e^(−2/3) ≈ 0.513.
# Wymaga: stop_proto_common.R.

c7p13_css <- tags$style(HTML("
  .c7p13-run { stroke: var(--upwr-secondary); stroke-width: 3; opacity: .55; }
  .c7p13-last { stroke-width: 3.6; }
  .c7p13-bar { fill: color-mix(in srgb, var(--upwr-sage) 70%, var(--upwr-surface)); }
  .c7p13-bar.is-bad { fill: color-mix(in srgb, var(--upwr-accent) 70%, var(--upwr-surface)); }
  .c7p13-model { stroke: var(--upwr-ink); stroke-width: 2; stroke-dasharray: 6 4; fill: none; }
  .c7p13-reads { display: flex; flex-wrap: wrap; gap: .4em 1.6em; margin: .2em 0 .6em; }
"))

c7p13_config <- list(gap = 500, k = 3, limit = 1000, tmax = 4000, bin = 250, seed = 7,
  p = ppois(2, 1000 / 500))   # P(T₃ > 1000) = P(N(1000) ≤ 2), wzór (7.11)

c7p_13_ui <- tagList(c7p13_css,
  c7p_scene_ui("P13", "scena", "Magazyn z trzema zapasowymi wentylatorami", "magazyn", c7p13_config,
    tags$div(class = "c7p13-reads",
      lc_readout("Okresów", c7p_out("n")),
      lc_readout("Zapas wystarczył do dostawy", c7p_out("ok"), color = upwr_accent),
      lc_readout("Średni czas do trzeciej awarii", c7p_out("mean"))),
    c7p_out("stage"),
    tags$div(class = "c7p-nav",
      c7p_button("one", "Następny okres", variant = "solid"),
      c7p_button("m10", "+10"), c7p_button("m100", "+100"), c7p_button("m1000", "+1000"),
      c7p_button("reset", "Od nowa"),
      c7p_seg("model", "Model", c("ukryty" = "off", "pokazany" = "on"))),
    tags$p(class = "c7p-read", c7p_out("model")),
    tags$p(class = "c7p-caption",
      "Magazynier zaczyna okres z trzema zapasowymi wentylatorami. Kreska to awaria w hali i wydanie jednego wentylatora; trzecia kreska opróżnia magazyn. Dostawa przychodzi po 1000 h: czerwona kropka oznacza okres, w którym zapas skończył się wcześniej.")))

# ============================================================================
# P24 — SCENA „WENTYLATOR ZA WENTYLATOREM” (rozkład wykładniczy, przed definicją 7.7)
# ============================================================================

c7p24_config <- list(gap = c7p_mttf, k = 1, limit = c7p_mission, tmax = 6000, bin = 250, seed = 11,
  p = exp(-c7p_mission / c7p_mttf),   # R(1000 h) oferty A
  txt = list(
    axis = "czas pracy od montażu (h)", limit = "przegląd",
    hist = "Kiedy przyszła awaria (wszystkie wentylatory)", haxis = "czas do awarii (h)",
    aria = "Linie życia ostatnich wentylatorów z kreską awarii i histogram czasu do awarii.",
    model = "Model wykładniczy: do przeglądu dotrwa wentylator z prawdopodobieństwem ",
    model2 = ", średni czas do awarii "))

c7p_24_ui <- tagList(c7p13_css,
  c7p_scene_ui("P24", "scena", "Wentylator za wentylatorem", "magazyn", c7p24_config,
    tags$div(class = "c7p13-reads",
      lc_readout("Wentylatorów", c7p_out("n")),
      lc_readout("Dotrwało do przeglądu po 1000 h", c7p_out("ok"), color = upwr_accent),
      lc_readout("Średni czas do awarii", c7p_out("mean"))),
    c7p_out("stage"),
    tags$div(class = "c7p-nav",
      c7p_button("one", "Następny wentylator", variant = "solid"),
      c7p_button("m10", "+10"), c7p_button("m100", "+100"), c7p_button("m1000", "+1000"),
      c7p_button("reset", "Od nowa"),
      c7p_seg("model", "Model", c("ukryty" = "off", "pokazany" = "on"))),
    tags$p(class = "c7p-read", c7p_out("model")),
    tags$p(class = "c7p-caption",
      "W komorze pracuje jeden wentylator; po awarii montuje się nowy tego samego typu. W każdej godzinie pracy szansa awarii jest taka sama, bez względu na to, ile wentylator już przepracował. Kreska to awaria, a kropka mówi, czy wentylator dotrwał do przeglądu po 1000 h.")))
