# Punkt stop sekcji „Koszt reakcji a koszt zaniechania” (przed przykładem 3.8):
#   a3_stop_examples_ui — Przykład „Ten sam wzór w trzech firmach”.
# Trzy realizacje wzoru (3.9) q* = c/L na konkretnych liczbach i slajd
# zbiorczy z trzema progami na jednej osi. Przełączanie i rysunek działają
# w przeglądarce (modules/przyklady.js); bez serwera.
# Liczby są fikcyjne. Bananpol: posterior 0.161 z rozdziału „Wzór Bayesa”.

a3_examples_fmt_zl <- function(x) paste(format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE), "zł")
a3_examples_fmt_p <- function(x) if (x >= 0.01) sprintf("%.2f", x) else sprintf("%.4f", x)
a3_examples_is_exact <- function(x) abs(as.numeric(a3_examples_fmt_p(x)) - x) < 1e-12

a3_examples_raw <- list(
  list(key = "banan", name = "Bananpol", q = 0.161,
    text = "Trzecia w nocy, czujnik w dojrzewalni zgłasza przegrzanie. Wyjazd ekipy ratuje partię bananów."),
  list(key = "pharma", name = "Hurtownia leków", q = 0.02,
    text = "Alarm temperatury w chłodni szczepionek. Takie alarmy są tu prawie zawsze fałszywe, ale partia jest bardzo droga."),
  list(key = "meat", name = "Zakład mięsny", q = 0.161,
    text = "Czujnik drgań łożyska na linii rozbioru, posterior jak w Bananpolu. Pewna reakcja to zatrzymanie linii na całą zmianę.")
)
a3_examples_raw[[1]]$c <- 100;   a3_examples_raw[[1]]$L <- 2000
a3_examples_raw[[2]]$c <- 400;   a3_examples_raw[[2]]$L <- 60000
a3_examples_raw[[3]]$c <- 12000; a3_examples_raw[[3]]$L <- 30000

a3_examples <- lapply(a3_examples_raw, function(e) {
  qstar <- e$c / e$L
  react <- e$q > qstar
  loss <- e$q * e$L
  list(
    key = e$key, name = e$name, text = e$text,
    q = e$q, qstar = qstar,
    qText = if (e$q >= 0.1) sprintf("%.3f", e$q) else sprintf("%.2f", e$q),
    cText = a3_examples_fmt_zl(e$c),
    LText = a3_examples_fmt_zl(e$L),
    qstarText = a3_examples_fmt_p(qstar),
    qstarEq = if (a3_examples_is_exact(qstar)) "=" else "≈",
    cmp = if (react) ">" else "<",
    react = react,
    verdict = if (react) "reagować" else "nie reagować — najpierw zweryfikować",
    read = sprintf("Oczekiwana strata bez reakcji: q · L ≈ %s %s c = %s",
      a3_examples_fmt_zl(loss), if (react) ">" else "<", a3_examples_fmt_zl(e$c))
  )
})

a3_examples_summary <- list(
  name = "Razem",
  title = "Trzy progi na jednej osi",
  text = "Bananpol i zakład mięsny mają ten sam posterior, a różne decyzje. Hurtownia reaguje przy posteriorze ośmiokrotnie niższym."
)

a3_stop_examples_css <- "
.lc-ex-top { display: flex; flex-wrap: wrap; gap: .5em; margin: .1em 0 .7em; }
.lc-ex .lc-seg button[aria-pressed='true'] { background: var(--upwr-accent); color: var(--upwr-surface); }
.lc-ex-text { font-size: 1.05em; color: var(--upwr-ink-soft); min-height: 3em; margin: 0 0 .6em; max-width: 52em; }
.lc-ex-tiles { display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: .6em; margin: 0 0 .7em; }
.lc-ex-tile { border: 1px solid var(--upwr-rule); border-radius: 8px; padding: .45em .7em .5em; background: var(--upwr-surface); }
.lc-ex-tile-label { font: .82em var(--upwr-sans); color: var(--upwr-ink-subtle); }
.lc-ex-tile-sym { font: 600 1em var(--upwr-mono); }
.lc-ex-tile-num { font: 700 2em/1.15 var(--upwr-mono); white-space: nowrap; }
.lc-ex-is-q { color: var(--upwr-cat-indygo); }
.lc-ex-is-c { color: var(--upwr-single-alt); }
.lc-ex-is-L { color: var(--upwr-accent); }
.lc-ex-is-qs { color: var(--upwr-ink); }
.lc-ex-formula { font: 600 1.6em var(--upwr-mono); color: var(--upwr-ink); margin: .1em 0 .25em; text-align: center; }
.lc-ex-formula .lc-ex-op { color: var(--upwr-ink-subtle); font-weight: 400; margin: 0 .25em; }
.lc-ex-svg { display: block; width: 100%; max-width: 820px; height: auto; margin: 0 auto; }
.lc-ex-svg text { font-family: var(--upwr-mono); }
.lc-ex-zone { fill: var(--upwr-sage-tint); }
.lc-ex-zone-label { font: 600 14px var(--upwr-sans) !important; fill: var(--upwr-cat-szalwia); }
.lc-ex-axis { stroke: var(--upwr-ink-soft); stroke-width: 2; }
.lc-ex-tick { stroke: var(--upwr-ink-subtle); stroke-width: 1.2; }
.lc-ex-tick-label { font-size: 14px; fill: var(--upwr-ink-subtle); text-anchor: middle; }
.lc-ex-thr { stroke: var(--upwr-ink); stroke-width: 3; }
.lc-ex-thr-label { font-size: 19px; font-weight: 700; fill: var(--upwr-ink); }
.lc-ex-dot { fill: var(--upwr-cat-indygo); stroke: var(--upwr-surface); stroke-width: 2.5; }
.lc-ex-dot-label { font-size: 19px; font-weight: 700; fill: var(--upwr-cat-indygo); }
.lc-ex-all .lc-ex-thr-label, .lc-ex-all .lc-ex-dot-label { font-size: 15px; }
.lc-ex-row-name { font: 600 16px var(--upwr-sans) !important; fill: var(--upwr-ink); }
.lc-ex-row-verdict { font: 600 15px var(--upwr-sans) !important; }
.lc-ex-row-verdict.is-react { fill: var(--upwr-cat-szalwia); }
.lc-ex-row-verdict.is-verify { fill: var(--upwr-ink-soft); }
.lc-ex-move { transition: transform .6s ease; }
.lc-ex-status { display: flex; flex-wrap: wrap; align-items: center; justify-content: center; gap: .4em .9em; margin: .4em 0 .2em; }
.lc-ex-cmp { font: 700 1.25em var(--upwr-mono); color: var(--upwr-ink); }
.lc-ex-verdict { font: 700 1.1em var(--upwr-sans); border-radius: 999px; padding: .25em .9em; border: 2px solid; }
.lc-ex-verdict.is-react { color: var(--upwr-cat-szalwia); border-color: var(--upwr-cat-szalwia); background: var(--upwr-sage-tint); }
.lc-ex-verdict.is-verify { color: var(--upwr-ink); border-color: var(--upwr-ink-soft); background: var(--upwr-surface-sunken); }
.lc-ex-read { font: 600 .92em var(--upwr-mono); color: var(--upwr-ink-soft); text-align: center; margin: .2em 0 .6em; min-height: 1.4em; }
.lc-ex[data-mode='summary'] .lc-ex-one { display: none; }
.lc-ex[data-mode='one'] .lc-ex-all { display: none; }
@media (max-width: 640px) {
  .lc-ex-tiles { grid-template-columns: 1fr; }
  .lc-ex-formula { font-size: 1.15em; }
}
@media (prefers-reduced-motion: reduce) { .lc-ex-move { transition: none !important; } }
"

a3_stop_examples_ui <- figure_panel(
  label = "Przykład",
  title = "Ten sam wzór w trzech firmach",
  full_width = TRUE,
  tags$style(HTML(a3_stop_examples_css)),
  tags$div(class = "lc-ex", `data-mode` = "one",
    `data-config` = jsonlite::toJSON(list(
      examples = a3_examples,
      summary = a3_examples_summary,
      axisMax = 0.5
    ), auto_unbox = TRUE, digits = NA),
    tags$div(class = "lc-ex-top",
      tags$div(class = "lc-seg", role = "group", `aria-label` = "Wybór przykładu",
        lapply(seq_along(a3_examples), function(i)
          tags$button(type = "button", `data-ex-go` = i - 1L, `aria-pressed` = if (i == 1L) "true" else "false",
            a3_examples[[i]]$name)),
        tags$button(type = "button", `data-ex-go` = length(a3_examples), `aria-pressed` = "false",
          a3_examples_summary$name)
      )
    ),
    tags$div(class = "lc-sp-head",
      tags$span(class = "lc-sp-name", `data-ex-out` = "name", a3_examples[[1]]$name),
      tags$span(class = "lc-sp-step", `data-ex-out` = "step", "Przykład 1 z 3")
    ),
    tags$p(class = "lc-ex-text", `data-ex-out` = "text", `aria-live` = "polite", a3_examples[[1]]$text),
    tags$div(class = "lc-ex-one",
      tags$div(class = "lc-ex-tiles",
        tags$div(class = "lc-ex-tile",
          tags$div(class = "lc-ex-tile-label", "posterior po alarmie"),
          tags$div(class = "lc-ex-tile-sym lc-ex-is-q", "q = P(A | 🚨)"),
          tags$div(class = "lc-ex-tile-num lc-ex-is-q", `data-ex-out` = "q")),
        tags$div(class = "lc-ex-tile",
          tags$div(class = "lc-ex-tile-label", "koszt reakcji"),
          tags$div(class = "lc-ex-tile-sym lc-ex-is-c", "c"),
          tags$div(class = "lc-ex-tile-num lc-ex-is-c", `data-ex-out` = "c")),
        tags$div(class = "lc-ex-tile",
          tags$div(class = "lc-ex-tile-label", "strata, której reakcja zapobiega"),
          tags$div(class = "lc-ex-tile-sym lc-ex-is-L", "L"),
          tags$div(class = "lc-ex-tile-num lc-ex-is-L", `data-ex-out` = "L"))
      ),
      tags$div(class = "lc-ex-formula", `data-ex-out` = "formula")
    ),
    tags$div(class = "lc-ex-stage", role = "img", `aria-label` = "Posterior q i próg q* na osi od 0 do 0.5"),
    tags$div(class = "lc-ex-one",
      tags$div(class = "lc-ex-status", `aria-live` = "polite",
        tags$span(class = "lc-ex-cmp", `data-ex-out` = "cmp"),
        tags$span(class = "lc-ex-verdict", `data-ex-out` = "verdict")
      ),
      tags$p(class = "lc-ex-read", `data-ex-out` = "read")
    ),
    tags$div(class = "lc-sp-nav",
      tags$button(type = "button", class = "lc-action is-outline", `data-ex` = "prev", lc_icon("prev"), tags$span("Wstecz")),
      tags$button(type = "button", class = "lc-action is-solid", `data-ex` = "next", tags$span("Dalej"), lc_icon("next"))
    )
  ),
  lc_caption("Ta sama reguła daje różne decyzje, bo próg q* zależy od kosztów, a nie od detektora.")
)
