# PROTOTYPY PUNKTÓW STOP (2026-11-14) — P16A i P16B: domknięcia wykładu 06 ----------
#
#   z6p_16a_ui — Domknięcie „Raport, wersja poprawiona” (P16A): raport z kadru 1 sceny P1
#                wraca poprawiony. Karta „przed” (przygaszona), karta komunikatu progowego
#                (trzy punkty ze źródłem) i rysunek N(82, 3) z progiem 85°C, policzony w R.
#   z6p_16b_ui — Domknięcie „Od pytania do działania” (P16B): pięć kroków ściągi w rzędzie,
#                każdy z wynikiem dla łożyska Bananpolu i znacznikiem rozdziału.
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
  .z6p16a-row { display: grid; grid-template-columns: 1.6em 8.6em minmax(0, 1fr) auto; gap: .2em .7em; align-items: start;
    padding: .65em 0; border-bottom: 1px solid var(--upwr-rule); }
  .z6p16a-row:last-child { border-bottom: none; padding-bottom: 0; }
  .z6p16a-num { font-family: var(--upwr-mono); font-weight: 700; color: var(--upwr-ink-subtle); }
  .z6p16a-lbl { font-size: .95em; }
  .z6p16a-text { font-size: .95em; line-height: 1.45; }
  .z6p16a-src { font-family: var(--upwr-mono); font-size: .72em; color: var(--upwr-ink-subtle); white-space: nowrap;
    border: 1px solid var(--upwr-rule); border-radius: 999px; padding: .15em .6em; justify-self: start; margin-top: .15em; }
  .z6p16a-fig { margin: 0; width: 100%; max-width: 420px; }
  @media (max-width: 720px) {
    .z6p16a-row { grid-template-columns: 1.6em minmax(0, 1fr); }
    .z6p16a-num { grid-row: span 3; }
    .z6p16a-lbl, .z6p16a-text, .z6p16a-src { grid-column: 2; }
  }
"))

z6p16a_rows <- list(
  list(lbl = "Liczba i horyzont",
    text = "Około 159 na 1000 pomiarów przekracza 85°C. Przy ośmiu pomiarach na zmianę alarm pojawia się w około 75% zmian.",
    src = "wzory (6.5), (6.7)"),
  list(lbl = "Mechanizm",
    text = "Średnia leży tylko jedno odchylenie standardowe pod progiem: ryzyko tworzą położenie i rozrzut temperatury.",
    src = "wzór (6.4)"),
  list(lbl = "Działanie",
    text = "Chłodzenie o 2°C obniża częstość do 48 na 1000, razem z ograniczeniem σ do 2°C do około 6 na 1000. Progu nie podnosimy bez badania materiałowego.",
    src = "przykład 6.7")
)

# Skala rysunku: temperatura 70–94°C na 20–340 px, gęstość 0–0.14 na 120–30 px.
z6p16a_x <- function(t) 20 + (t - 70) / 24 * 320
z6p16a_y <- function(d) 120 - d / 0.14 * 90

z6p16a_svg <- function() {
  z6p_svg(360, 150,
    "Krzywa rozkładu temperatury N(82, 3) z progiem 85 stopni; pole ponad progiem to około 159 na tysiąc pomiarów.",
    sprintf('<path class="z6p-area" d="%s"/>',
      z6p_norm_path(z6p_mu, z6p_sd, z6p16a_x, z6p16a_y, z6p_c, 94, close = TRUE)),
    sprintf('<path class="z6p-curve" d="%s"/>',
      z6p_norm_path(z6p_mu, z6p_sd, z6p16a_x, z6p16a_y, 70, 94)),
    z6p_axis(z6p16a_x, 120, c(70, 76, 82, 88, 94), labels = c("70°C", "76°C", "82°C", "88°C", "94°C")),
    sprintf('<line class="z6p-limit" x1="%g" x2="%g" y1="22" y2="120"/>', z6p16a_x(z6p_c), z6p16a_x(z6p_c)),
    sprintf('<text class="z6p-mono" x="%g" y="14" font-size="13" font-weight="700" text-anchor="middle">próg 85°C</text>',
      z6p16a_x(z6p_c)),
    sprintf('<text class="z6p-mono" x="%g" y="50" font-size="15" font-weight="700" style="fill:var(--upwr-accent)">159 na 1000</text>',
      z6p16a_x(z6p_c) + 12)
  )
}

z6p_16a_ui <- z6p_panel("P16A", "domknięcie", "Raport, wersja poprawiona", z6p_css, z6p16a_css,
  tags$div(class = "z6p16a",
    tags$div(class = "z6p16a-card z6p16a-before",
      tags$div(class = "z6p16a-head", "RAPORT MIESIĘCZNY · temperatura łożysk"),
      tags$p(class = "z6p16a-line", "Wentylator 2 · średnia 82°C · próg 85°C · ",
        tags$span(class = "z6p16a-struck", "w normie"))
    ),
    tags$div(class = "z6p16a-main",
      tags$div(class = "z6p16a-card z6p16a-after",
        tags$div(class = "z6p16a-head", "KOMUNIKAT PROGOWY · wentylator 2"),
        tags$ol(class = "z6p16a-rows",
          lapply(seq_along(z6p16a_rows), function(i) {
            r <- z6p16a_rows[[i]]
            tags$li(class = "z6p16a-row",
              tags$span(class = "z6p16a-num", i),
              tags$strong(class = "z6p16a-lbl", r$lbl),
              tags$span(class = "z6p16a-text", r$text),
              tags$span(class = "z6p16a-src", r$src))
          }))
      ),
      tags$figure(class = "z6p16a-fig", z6p16a_svg())
    )
  )
)

# ============================================================================
# P16B — DOMKNIĘCIE „OD PYTANIA DO DZIAŁANIA”
# ============================================================================

z6p16b_css <- tags$style(HTML("
  .z6p16b { margin: .4em 0; }
  .z6p16b-flow { display: flex; align-items: stretch; gap: .35em; }
  .z6p16b-step { flex: 1 1 0; min-width: 0; background: var(--upwr-surface); border: 1px solid var(--upwr-rule);
    border-top: 4px solid var(--upwr-ink); border-radius: 6px; padding: .7em .75em .8em; display: flex; flex-direction: column; gap: .35em; }
  .z6p16b-step.is-result { border-top-color: var(--upwr-accent); }
  .z6p16b-step.is-action { border-top-color: var(--upwr-sage); }
  .z6p16b-n { font-family: var(--upwr-mono); font-size: .72em; letter-spacing: .06em; text-transform: uppercase; color: var(--upwr-ink-subtle); }
  .z6p16b-name { font-size: 1.02em; }
  .z6p16b-val { margin: 0; font-size: .9em; line-height: 1.45; flex: 1 1 auto; }
  .z6p16b-hot { font-family: var(--upwr-mono); font-weight: 700; font-size: 1.2em; color: var(--upwr-accent); }
  .z6p16b-ok { font-family: var(--upwr-mono); font-weight: 700; color: var(--upwr-sage); }
  .z6p16b-ref { font-family: var(--upwr-mono); font-size: .72em; color: var(--upwr-ink-subtle);
    border-top: 1px solid var(--upwr-rule); padding-top: .35em; margin-top: .2em; }
  .z6p16b-arrow { flex: 0 0 auto; align-self: center; font-size: 1.3em; line-height: 1; color: var(--upwr-ink-subtle); }
  @media (max-width: 760px) {
    .z6p16b-flow { flex-direction: column; }
    .z6p16b-arrow { transform: rotate(90deg); padding: .1em 0; }
  }
"))

z6p16b_steps <- list(
  list(name = "Pytanie", cls = "",
    val = "Jaka część pomiarów przekracza 85°C?",
    ref = "rozdz. 1"),
  list(name = "Model", cls = "",
    val = "T ~ N(82, 3), próg leży w z = 1",
    ref = "rozdz. 2–3"),
  list(name = "Założenia", cls = "",
    val = "pomiar to nie zmiana; kształt ogona sprawdza wykres Q–Q",
    ref = "rozdz. 3 i 6"),
  list(name = "Wynik", cls = "is-result",
    val = tagList("P(T > 85) = ", tags$span(class = "z6p16b-hot", "0.159"),
      ": 159 na 1000 pomiarów, alarm w około 75% zmian"),
    ref = "wzory (6.5), (6.7)"),
  list(name = "Działanie", cls = "is-action",
    val = tagList("chłodzenie i stabilizacja: ", tags$span(class = "z6p16b-ok", "około 6 na 1000"),
      "; próg zostaje"),
    ref = "rozdz. 4")
)

z6p16b_tile <- function(i, s) {
  tags$div(class = paste("z6p16b-step", s$cls),
    tags$span(class = "z6p16b-n", paste("Krok", i)),
    tags$strong(class = "z6p16b-name", s$name),
    tags$p(class = "z6p16b-val", s$val),
    tags$span(class = "z6p16b-ref", s$ref)
  )
}

z6p16b_items <- unlist(lapply(seq_along(z6p16b_steps), function(i) {
  if (i < length(z6p16b_steps)) list(z6p16b_tile(i, z6p16b_steps[[i]]),
    tags$span(class = "z6p16b-arrow", `aria-hidden` = "true", "→"))
  else list(z6p16b_tile(i, z6p16b_steps[[i]]))
}), recursive = FALSE)

z6p_16b_ui <- z6p_panel("P16B", "domknięcie", "Od pytania do działania", z6p_css, z6p16b_css,
  tags$div(class = "z6p16b",
    do.call(tags$div, c(list(class = "z6p16b-flow"), z6p16b_items))
  )
)
