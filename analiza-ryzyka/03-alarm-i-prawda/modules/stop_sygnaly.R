# Punkt stop sekcji „Pułapka częstości bazowej” (po akapicie o wzorze 3.7,
# przed przykładem 3.5):
#   a3_stop_signals_ui — Przykład „Ten sam start, różne sygnały”.
# Trzy realizacje wzoru (3.7) szanse po = szanse przed · LR dla hali Bananpolu
# (szanse a priori 1 : 99): alarm czujnika, dodatni odczyt ręczny termometru
# i cisza czujnika, oraz slajd zbiorczy z trzema strzałkami na jednej osi
# szans w skali logarytmicznej. Przełączanie i rysunek działają w przeglądarce
# (modules/sygnaly.js); bez serwera.
# Liczby: czujnik 0.95/0.05 z rozdziału „Czułość i swoistość”, termometr
# 0.80/0.10 z ćwiczenia rozdziału „Wzór Bayesa”.

a3_signals_prior <- 0.01
a3_signals_prior_odds <- a3_signals_prior / (1 - a3_signals_prior)   # 1/99

a3_signals_fmt_num <- function(x) sprintf("%.2f", x)
a3_signals_fmt_p <- function(p) {
  if (p >= 0.01) sprintf("%.3f", p) else formatC(signif(p, 2), format = "f", digits = 5)
}
a3_signals_fmt_int <- function(x) format(round(x), scientific = FALSE, trim = TRUE)

a3_signals_raw <- list(
  list(key = "alarm", name = "🚨 Alarm", sym = "🚨", lrName = "LR₊ czujnika",
    lrWords = "czułość / FPR", pA = 0.95, pAc = 0.05,
    text = "Trzecia w nocy, czujnik w dojrzewalni zgłasza przegrzanie 🚨. Alarm pojawia się przy 95% awarii i przy 5% zmian bez awarii."),
  list(key = "thermo", name = "🌡️ Termometr", sym = "🌡️", lrName = "LR odczytu ręcznego",
    lrWords = "P(🌡️ | A) / P(🌡️ | Aᶜ)", pA = 0.80, pAc = 0.10,
    text = "Zamiast czujnika operator idzie z termometrem i odczyt pokazuje przegrzanie. Taki odczyt zdarza się przy 80% awarii i przy 10% zmian bez awarii."),
  list(key = "silence", name = "🔕 Cisza", sym = "🔕", lrName = "LR₋ czujnika",
    lrWords = "(1 − czułość) / swoistość", pA = 0.05, pAc = 0.95,
    text = "Czujnik milczy 🔕 przez całą zmianę. Przy awarii milczy tylko w 5% przypadków, bez awarii w 95%.")
)

a3_signals <- lapply(a3_signals_raw, function(s) {
  lr <- s$pA / s$pAc
  post_odds <- a3_signals_prior_odds * lr
  p <- post_odds / (1 + post_odds)
  up <- lr > 1
  # Szanse po jako ułamek całkowity: 1/99 · LR.
  if (up) {
    num <- round(lr); den <- 99
    lrText <- a3_signals_fmt_int(lr)
  } else {
    inv <- round(1 / lr); num <- 1; den <- 99 * inv
    lrText <- paste0("1/", inv)
  }
  list(
    key = s$key, name = s$name, sym = s$sym, text = s$text,
    lr = lr, logLr = log10(lr), postOdds = post_odds, p = p, up = up,
    lrName = s$lrName, lrWords = s$lrWords,
    lrCalc = paste(a3_signals_fmt_num(s$pA), "/", a3_signals_fmt_num(s$pAc)),
    lrText = lrText,
    postFrac = paste0(num, "/", a3_signals_fmt_int(den)),
    postText = paste(num, ":", a3_signals_fmt_int(den)),
    pText = a3_signals_fmt_p(p),
    cmp = if (up) "LR > 1" else "LR < 1",
    verdict = if (up) sprintf("szanse rosną %s razy", lrText) else sprintf("szanse maleją %s razy", round(1 / lr)),
    arrowText = paste("×", lrText),
    read = sprintf("P(A | %s) = %s / (%s + %s) = %s/%s ≈ %s",
      s$sym, num, num, a3_signals_fmt_int(den), num, a3_signals_fmt_int(num + den), a3_signals_fmt_p(p))
  )
})

a3_signals_summary <- list(
  name = "Razem",
  title = "Trzy sygnały z tego samego startu",
  text = "Ta sama hala, trzy mnożniki. Alarm i odczyt przesuwają ocenę w prawo, cisza w lewo — dokładnie o tyle, o ile alarm w prawo, bo LR₋ = 1/19."
)

a3_stop_signals_css <- "
.lc-sg-top { display: flex; flex-wrap: wrap; gap: .5em; margin: .1em 0 .7em; }
.lc-sg .lc-seg button[aria-pressed='true'] { background: var(--upwr-accent); color: var(--upwr-surface); }
.lc-sg-text { font-size: 1.05em; color: var(--upwr-ink-soft); min-height: 3em; margin: 0 0 .6em; max-width: 52em; }
.lc-sg-tiles { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: .6em; margin: 0 0 .7em; }
.lc-sg-tile { border: 1px solid var(--upwr-rule); border-radius: 8px; padding: .45em .7em .5em; background: var(--upwr-surface); }
.lc-sg-tile-label { font: .82em var(--upwr-sans); color: var(--upwr-ink-subtle); }
.lc-sg-tile-num { font: 700 2em/1.15 var(--upwr-mono); white-space: nowrap; }
.lc-sg-tile-sub { font: .8em var(--upwr-mono); color: var(--upwr-ink-subtle); min-height: 1.3em; }
.lc-sg-tile-sub b { font-weight: 700; }
.lc-sg-is-prior { color: var(--upwr-cat-indygo); }
.lc-sg-is-lr { color: var(--upwr-single-alt); }
.lc-sg-is-post { color: var(--upwr-accent); }
.lc-sg-is-p { color: var(--upwr-ink); }
.lc-sg-formula { font: 600 1.5em var(--upwr-mono); color: var(--upwr-ink); margin: .1em 0 .25em; text-align: center; }
.lc-sg-formula .lc-sg-op { color: var(--upwr-ink-subtle); font-weight: 400; margin: 0 .25em; }
.lc-sg-svg { display: block; width: 100%; max-width: 820px; height: auto; margin: 0 auto; }
.lc-sg-svg text { font-family: var(--upwr-mono); }
.lc-sg-axis { stroke: var(--upwr-ink-soft); stroke-width: 2; }
.lc-sg-tick { stroke: var(--upwr-ink-subtle); stroke-width: 1.2; }
.lc-sg-tick-label { font-size: 14px; fill: var(--upwr-ink-subtle); text-anchor: middle; }
.lc-sg-axis-name { font: 13px var(--upwr-sans) !important; fill: var(--upwr-ink-subtle); }
.lc-sg-start { fill: var(--upwr-surface); stroke: var(--upwr-cat-indygo); stroke-width: 3; }
.lc-sg-start-line { stroke: var(--upwr-cat-indygo); stroke-width: 1.5; stroke-dasharray: 4 4; opacity: .7; }
.lc-sg-start-label { font-size: 17px; font-weight: 700; fill: var(--upwr-cat-indygo); }
.lc-sg-drop { stroke: var(--upwr-ink-subtle); stroke-width: 1; stroke-dasharray: 3 3; }
.lc-sg-arrow { stroke: var(--upwr-single-alt); stroke-width: 4; fill: none; stroke-linecap: round; }
.lc-sg-head { fill: var(--upwr-single-alt); }
.lc-sg-arrow-label { font-size: 19px; font-weight: 700; fill: var(--upwr-single-alt); text-anchor: middle; }
.lc-sg-dot { fill: var(--upwr-accent); stroke: var(--upwr-surface); stroke-width: 2.5; }
.lc-sg-dot-label { font-size: 17px; font-weight: 700; fill: var(--upwr-accent); }
.lc-sg-row-name { font: 600 16px var(--upwr-sans) !important; fill: var(--upwr-ink); }
.lc-sg-all .lc-sg-arrow-label { font-size: 16px; }
.lc-sg-all .lc-sg-dot-label { font-size: 15px; }
.lc-sg-move { transition: transform .6s ease, opacity .4s ease; }
.lc-sg-draw { transition: stroke-dashoffset .6s ease; }
.lc-sg-status { display: flex; flex-wrap: wrap; align-items: center; justify-content: center; gap: .4em .9em; margin: .4em 0 .2em; }
.lc-sg-cmp { font: 700 1.25em var(--upwr-mono); color: var(--upwr-single-alt); }
.lc-sg-verdict { font: 700 1.1em var(--upwr-sans); border-radius: 999px; padding: .25em .9em; border: 2px solid; }
.lc-sg-verdict.is-up { color: var(--upwr-accent); border-color: var(--upwr-accent); background: var(--upwr-accent-tint); }
.lc-sg-verdict.is-down { color: var(--upwr-cat-szalwia); border-color: var(--upwr-cat-szalwia); background: var(--upwr-sage-tint); }
.lc-sg-read { font: 600 .95em var(--upwr-mono); color: var(--upwr-ink-soft); text-align: center; margin: .2em 0 .6em; min-height: 1.4em; }
.lc-sg[data-mode='summary'] .lc-sg-one { display: none; }
.lc-sg[data-mode='one'] .lc-sg-all { display: none; }
@media (max-width: 640px) {
  .lc-sg-tiles { grid-template-columns: repeat(2, minmax(0, 1fr)); }
  .lc-sg-tile-num { font-size: 1.5em; }
  .lc-sg-formula { font-size: 1.1em; }
}
@media (prefers-reduced-motion: reduce) { .lc-sg-move, .lc-sg-draw { transition: none !important; } }
.lc-sg[data-anim='0'] .lc-sg-move, .lc-sg[data-anim='0'] .lc-sg-draw { transition: none !important; }
"

a3_signals_tile <- function(label_key, label, num_key, cls, sub_key = NULL, sub = NULL) {
  tags$div(class = "lc-sg-tile",
    tags$div(class = "lc-sg-tile-label", `data-sg-out` = label_key, label),
    tags$div(class = paste("lc-sg-tile-num", cls), `data-sg-out` = num_key),
    tags$div(class = "lc-sg-tile-sub", `data-sg-out` = sub_key, sub)
  )
}

a3_stop_signals_ui <- figure_panel(
  label = "Przykład",
  title = "Ten sam start, różne sygnały",
  full_width = TRUE,
  tags$style(HTML(a3_stop_signals_css)),
  tags$div(class = "lc-sg", `data-mode` = "one",
    `data-config` = jsonlite::toJSON(list(
      signals = a3_signals,
      summary = a3_signals_summary,
      prior = list(odds = a3_signals_prior_odds, text = "1 : 99", frac = "1/99", calc = "0.01 / 0.99"),
      logMin = -4, logMax = 0
    ), auto_unbox = TRUE, digits = NA),
    tags$div(class = "lc-sg-top",
      tags$div(class = "lc-seg", role = "group", `aria-label` = "Wybór sygnału",
        lapply(seq_along(a3_signals), function(i)
          tags$button(type = "button", `data-sg-go` = i - 1L, `aria-pressed` = if (i == 1L) "true" else "false",
            a3_signals[[i]]$name)),
        tags$button(type = "button", `data-sg-go` = length(a3_signals), `aria-pressed` = "false",
          a3_signals_summary$name)
      )
    ),
    tags$div(class = "lc-sp-head",
      tags$span(class = "lc-sp-name", `data-sg-out` = "name", a3_signals[[1]]$name),
      tags$span(class = "lc-sp-step", `data-sg-out` = "step", "Sygnał 1 z 3")
    ),
    tags$p(class = "lc-sg-text", `data-sg-out` = "text", `aria-live` = "polite", a3_signals[[1]]$text),
    tags$div(class = "lc-sg-one",
      tags$div(class = "lc-sg-tiles",
        a3_signals_tile("priorLabel", "szanse przed (hala)", "prior", "lc-sg-is-prior", "priorSub", "0.01 / 0.99"),
        a3_signals_tile("lrLabel", "", "lr", "lc-sg-is-lr", "lrSub"),
        a3_signals_tile("postLabel", "szanse po", "post", "lc-sg-is-post", "postSub"),
        a3_signals_tile("pLabel", "prawdopodobieństwo awarii po", "p", "lc-sg-is-p", "pSub")
      ),
      tags$div(class = "lc-sg-formula", `data-sg-out` = "formula")
    ),
    tags$div(class = "lc-sg-stage", role = "img",
      `aria-label` = "Szanse awarii na osi logarytmicznej od 1 : 10 000 do 1 : 1; strzałka od szans przed do szans po sygnale"),
    tags$div(class = "lc-sg-one",
      tags$div(class = "lc-sg-status", `aria-live` = "polite",
        tags$span(class = "lc-sg-cmp", `data-sg-out` = "cmp"),
        tags$span(class = "lc-sg-verdict", `data-sg-out` = "verdict")
      ),
      tags$p(class = "lc-sg-read", `data-sg-out` = "read")
    ),
    tags$div(class = "lc-sp-nav",
      tags$button(type = "button", class = "lc-action is-outline", `data-sg` = "prev", lc_icon("prev"), tags$span("Wstecz")),
      tags$button(type = "button", class = "lc-action is-solid", `data-sg` = "next", tags$span("Dalej"), lc_icon("next"))
    )
  ),
  lc_caption("Każdy sygnał to mnożnik szans; ile zmienia ocenę, zależy od jego LR.")
)
