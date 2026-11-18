# DOMKNIĘCIE P16A (2026-11-14, szablon raportu 17.11.2026) — style ---------
#
#   z6p16a_css — style kart raportu do
#                domknięcia P16A; sam panel z6p_16a_ui (szablon raportu przed i po) jest
#                w stop_proto_rl.R.
# P16B („Od pytania do działania”, ściąga w krokach) usunięta 17.11.2026: te same kroki
# pokazuje szablon raportu (stop_proto_rl.R).
# Statyczne: HTML/CSS i SVG składa R, bez JS i bez serwera (działa offline).
# Wymaga: stop_proto_common.R (z6p_panel, z6p_css, z6p_svg, z6p_axis, z6p_norm_path,
# z6p_mu, z6p_sd, z6p_c). Liczby z wykładu (przykład 6.7, wzory 6.4–6.7).

# ============================================================================
# P16A — DOMKNIĘCIE „RAPORT, WERSJA POPRAWIONA”
# ============================================================================

z6p16a_css <- tags$style(HTML("
  .z6p16a { display: grid; gap: 1em; margin: .4em 0; }
  .z6p16a-card { background: var(--upwr-surface); border: 1px solid var(--upwr-rule); border-radius: 6px; padding: .8em 1em; }
  .z6p16a-before { max-width: 640px; opacity: .7; padding: .6em .9em; font-size: .9em; }
  .z6p16a-head { font-family: var(--upwr-mono); font-size: .75em; letter-spacing: .06em; text-transform: uppercase;
    color: var(--upwr-ink-subtle); border-bottom: 1px solid var(--upwr-rule); padding-bottom: .4em; margin-bottom: .5em; }
  .z6p16a-line { margin: 0; font-family: var(--upwr-mono); font-size: .95em; color: var(--upwr-ink-subtle); }
  .z6p16a-struck { text-decoration: line-through; text-decoration-thickness: 2px; }
  .z6p16a-after { border-top: 3px solid var(--upwr-ink); }
  .z6p16a-main { display: grid; gap: .6em; justify-items: start; }
  .z6p16a-rows { list-style: none; margin: 0; padding: 0; }
  .z6p16a-row { display: grid; grid-template-columns: 1.6em 11.5em minmax(0, 1fr) auto; gap: .2em .7em; align-items: start;
    padding: .65em 0; border-bottom: 1px solid var(--upwr-rule); }
  .z6p16a-row:last-child { border-bottom: none; padding-bottom: 0; }
  .z6p16a-num { font-family: var(--upwr-mono); font-weight: 700; color: var(--upwr-ink-subtle); }
  .z6p16a-lbl { font-size: .95em; }
  .z6p16a-text { font-size: .95em; line-height: 1.45; }
  .z6p16a-src { font-family: var(--upwr-mono); font-size: .72em; color: var(--upwr-ink-subtle); white-space: nowrap;
    border: 1px solid var(--upwr-rule); border-radius: 999px; padding: .15em .6em; justify-self: start; margin-top: .15em; }
  @media (max-width: 720px) {
    .z6p16a-row { grid-template-columns: 1.6em minmax(0, 1fr); }
    .z6p16a-num { grid-row: span 3; }
    .z6p16a-lbl, .z6p16a-text, .z6p16a-src { grid-column: 2; }
  }
"))

# z6p_16a_ui: szablon raportu w stop_proto_rl.R (wersja poprawiona z przełącznikiem na wersję
# z rozdziału 1); tu zostają style z6p16a-*.
