# Punkt stop sekcji „Druga informacja” (rozdział „Baza”):
#   a3_stop_second_ui — scena „Drugi telefon”: alarmy pierwszego czujnika
#   przechodzą przez sito drugiego, warunkowo niezależnego czujnika.
# Rysunek i kadry działają w przeglądarce (modules/drugi.js, kontener .lc-dt).
# Bez części serwerowej.

# Naturalne częstości na 10 000 zmian, te same co w przykładzie 3.7.
a3_second_n <- 10000L
a3_second_prev <- 0.01
a3_second_sens <- 0.95
a3_second_fpr <- 0.05
a3_second_scale <- 5L   # jedno pole = 5 alarmów

a3_second_tp1 <- round(a3_second_n * a3_second_prev * a3_second_sens)          # 95
a3_second_fp1 <- round(a3_second_n * (1 - a3_second_prev) * a3_second_fpr)     # 495
a3_second_tp2 <- round(a3_second_tp1 * a3_second_sens)                         # 90
a3_second_fp2 <- round(a3_second_fp1 * a3_second_fpr)                          # 25
a3_second_ppv1 <- sprintf("%.2f", a3_second_tp1 / (a3_second_tp1 + a3_second_fp1))  # 0.16
a3_second_ppv2 <- sprintf("%.2f", a3_second_tp2 / (a3_second_tp2 + a3_second_fp2))  # 0.78

a3_second_frames <- list(
  list(name = "Pierwszy alarm", step = 1L,
    read = sprintf("1 pole = %d alarmów · prawdziwe %d · fałszywe %d", a3_second_scale, a3_second_tp1, a3_second_fp1),
    text = sprintf("Na 10 000 zmian pierwszy czujnik dzwoni %d razy. Prawdziwych telefonów jest %d, fałszywych %d.",
      a3_second_tp1 + a3_second_fp1, a3_second_tp1, a3_second_fp1)),
  list(name = "Drugi czujnik", step = 2L,
    read = sprintf("drugi czujnik: czułość %.2f · FPR %.2f", a3_second_sens, a3_second_fpr),
    text = "Zanim wyśle ekipę, dyżurny dzwoni do drugiego czujnika w tej samej dojrzewalni. Ma on te same parametry, ale mierzy osobno: o tym, czy zaalarmuje, decyduje tylko stan hali."),
  list(name = "Przesiew", step = 3L,
    read = sprintf("1 pole = %d alarmów · prawdziwe %d → %d · fałszywe %d → %d",
      a3_second_scale, a3_second_tp1, a3_second_tp2, a3_second_fp1, a3_second_fp2),
    text = sprintf("Ekipa jedzie tylko wtedy, gdy drugi czujnik też alarmuje. Przy awarii potwierdza prawie zawsze: zostaje %d z %d. Fałszywy alarm potwierdza rzadko: zostaje %d z %d.",
      a3_second_tp2, a3_second_tp1, a3_second_fp2, a3_second_fp1)),
  list(name = "Po dwóch alarmach", step = 4L,
    read = sprintf("P(awaria | 🚨🚨) = %d/%d ≈ %s · po jednym alarmie P(awaria | 🚨) = %d/%d ≈ %s",
      a3_second_tp2, a3_second_tp2 + a3_second_fp2, a3_second_ppv2,
      a3_second_tp1, a3_second_tp1 + a3_second_fp1, a3_second_ppv1),
    text = sprintf("Prawdziwych telefonów ubyło niewiele, fałszywych prawie wszystkie. Z %d podwójnych alarmów %d to awarie.",
      a3_second_tp2 + a3_second_fp2, a3_second_tp2))
)

# Prawdziwe alarmy — akcent, fałszywe — złoto (--upwr-single-alt), jak w „Dzienniku dyżurnego”.
a3_stop_second_css <- "
.lc-dt-svg { display: block; width: 100%; max-width: 820px; height: auto; margin: 0 auto; }
.lc-dt-cell { transition: transform .9s cubic-bezier(.45,.05,.3,1), opacity .6s ease; }
.lc-dt-cell.is-true { fill: var(--upwr-accent); }
.lc-dt-cell.is-false { fill: var(--upwr-single-alt); }
.lc-dt-cell.is-gone { opacity: .22; }
.lc-dt-cell.is-out { opacity: .07; }
.lc-dt-pile { font: 600 15px var(--upwr-sans); transition: opacity .5s ease; }
.lc-dt-pile.is-true { fill: var(--upwr-accent); }
.lc-dt-pile.is-false { fill: color-mix(in srgb, var(--upwr-single-alt) 70%, var(--upwr-ink)); }
.lc-dt-pile.is-right { opacity: 0; transition-delay: .6s; }
.lc-dt-svg.is-passed .lc-dt-pile.is-right { opacity: 1; }
.lc-dt-svg.is-final .lc-dt-pile.is-left { opacity: .45; }
.lc-dt-sieve { opacity: 0; transition: opacity .6s ease; }
.lc-dt-svg.is-sieve .lc-dt-sieve { opacity: 1; }
.lc-dt-frame { fill: var(--upwr-surface-sunken); stroke: var(--upwr-ink-soft); stroke-width: 2; }
.lc-dt-mesh { stroke: var(--upwr-ink-subtle); stroke-width: 1.2; }
.lc-dt-sieve-title { font: 600 15px var(--upwr-sans); fill: var(--upwr-ink); }
.lc-dt-sieve-sub { font: 13px var(--upwr-sans); fill: var(--upwr-ink-soft); }
.lc-dt-rate { font: 700 16px var(--upwr-mono); }
.lc-dt-rate.is-true { fill: var(--upwr-accent); }
.lc-dt-rate.is-false { fill: color-mix(in srgb, var(--upwr-single-alt) 70%, var(--upwr-ink)); }
.lc-dt-hit { opacity: 0; transition: opacity .6s ease; font-family: var(--upwr-mono); }
.lc-dt-hit.is-before { font-size: 15px; fill: var(--upwr-ink-subtle); }
.lc-dt-hit.is-after { font-size: 18px; font-weight: 700; fill: var(--upwr-ink); }
.lc-dt-svg.is-final .lc-dt-hit { opacity: 1; }
.lc-dt-readout { font: 600 15px var(--upwr-mono); color: var(--upwr-ink); margin: .2em 0 .6em; min-height: 1.4em; }
@media (prefers-reduced-motion: reduce) { .lc-dt-svg * { transition: none !important; } }
"

a3_stop_second_ui <- figure_panel(
  label = "Scena",
  title = "Drugi telefon",
  full_width = TRUE,
  tags$style(HTML(a3_stop_second_css)),
  tags$div(class = "lc-dt",
    `data-config` = jsonlite::toJSON(list(
      scale = a3_second_scale, seed = 20261021L,
      tp1 = a3_second_tp1, fp1 = a3_second_fp1, tp2 = a3_second_tp2, fp2 = a3_second_fp2,
      sens = a3_second_sens, fpr = a3_second_fpr,
      ppv1 = a3_second_ppv1, ppv2 = a3_second_ppv2,
      sieve = sprintf("%d%% przy awarii, %d%% bez", round(100 * a3_second_sens), round(100 * a3_second_fpr)),
      frames = a3_second_frames), auto_unbox = TRUE),
    tags$div(class = "lc-sp-head",
      tags$span(class = "lc-sp-name", `data-dt-out` = "name", "Pierwszy alarm"),
      tags$span(class = "lc-sp-step", `data-dt-out` = "step", "Kadr 1 z 4")
    ),
    tags$div(class = "lc-dt-stage", role = "img"),
    tags$p(class = "lc-sp-caption lc-dt-caption", `aria-live` = "polite"),
    tags$p(class = "lc-dt-readout"),
    tags$div(class = "lc-sp-nav",
      tags$button(type = "button", class = "lc-action is-outline", `data-dt` = "prev", lc_icon("prev"), tags$span("Wstecz")),
      tags$button(type = "button", class = "lc-action is-solid", `data-dt` = "next", tags$span("Dalej"), lc_icon("next"))
    )
  )
)
