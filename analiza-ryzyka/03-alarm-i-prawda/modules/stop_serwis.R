# Punkt stop sekcji „Więcej niż dwa stany” (przed wzorem 3.6):
#   a3_stop_service_ui — scena „Serwis szuka winnego” (kadry historii).
# Rysunek i kadry działają w przeglądarce (modules/serwis.js); bez serwera.
# Liczby są fikcyjne (Bananpol): 100 dawnych awarii chłodzenia, objaw
# „temperatura rosła stopniowo przez godzinę”.

a3_service_groups <- data.frame(
  name  = c("Sterownik", "Sprężarka", "Wentylator"),
  n     = c(50L, 30L, 20L),
  sym   = c(5L, 12L, 18L),
  color = c("indygo", "terakota", "szalwia"),
  stringsAsFactors = FALSE
)
a3_service_groups$prior <- sprintf("%.2f", a3_service_groups$n / sum(a3_service_groups$n))
a3_service_groups$lik   <- paste0(a3_service_groups$sym / a3_service_groups$n * 10, "/10")
a3_service_groups$post  <- sprintf("%.2f", a3_service_groups$sym / sum(a3_service_groups$sym))

a3_service_frames <- local({
  g <- a3_service_groups
  sym_total <- sum(g$sym)
  list(
    list(name = "Rejestr awarii", step = 1L,
      read = sprintf("%s %d · %s %d · %s %d", g$name[1], g$n[1], tolower(g$name[2]), g$n[2], tolower(g$name[3]), g$n[3]),
      text = "W Bananpolu potwierdzono awarię chłodzenia. Serwisant ma trzech podejrzanych i otwiera rejestr: 100 dawnych awarii, każda przypisana do podzespołu, który zawiódł."),
    list(name = "Kto zwykle zawodzi", step = 2L,
      read = sprintf("Przed obserwacją: P(sterownik) = %s · P(sprężarka) = %s · P(wentylator) = %s", g$prior[1], g$prior[2], g$prior[3]),
      text = "Gdyby serwisant nie wiedział nic więcej, zacząłby od sterownika: to połowa dawnych awarii. Wentylator zawodzi najrzadziej."),
    list(name = "Objaw", step = 3L,
      read = sprintf("Awarie z tym objawem: %d + %d + %d = %d", g$sym[1], g$sym[2], g$sym[3], sym_total),
      text = "Dziś temperatura rosła stopniowo przez godzinę. W rejestrze taki objaw miała co dziesiąta awaria sterownika, cztery na dziesięć awarii sprężarki i dziewięć na dziesięć awarii wentylatora."),
    list(name = "Po obserwacji", step = 4L,
      read = sprintf("P(wentylator | objaw) = %d/%d ≈ %s · sprężarka %d/%d ≈ %s · sterownik %d/%d ≈ %s",
        g$sym[3], sym_total, g$post[3], g$sym[2], sym_total, g$post[2], g$sym[1], sym_total, g$post[1]),
      text = sprintf("Serwisant zostawia tylko dawne awarie z takim samym objawem jak dziś. Kontury pokazują, ile ich było, pełne pola, ile zostało. Z %d takich przypadków %d to wentylator: najrzadszy podejrzany jest teraz najbardziej prawdopodobny.", sym_total, g$sym[3]))
  )
})

a3_stop_service_css <- "
.lc-sw-svg { display: block; width: 100%; max-width: 820px; height: auto; margin: 0 auto; }
.lc-sw-cell { fill: var(--sw-c); transition: transform .9s ease, opacity .5s ease, fill .4s ease; }
.lc-sw-cell.is-dim { fill: color-mix(in srgb, var(--sw-c) 20%, var(--upwr-surface)); }
.lc-sw-cell.is-gone { opacity: 0; }
.lc-sw-ghost { fill: none; stroke: color-mix(in srgb, var(--sw-c) 40%, var(--upwr-surface)); stroke-width: 1.2; opacity: 0; transition: opacity .6s ease .3s; }
.lc-sw-svg.is-final .lc-sw-ghost { opacity: 1; }
.lc-sw-box { fill: color-mix(in srgb, var(--sw-c) 12%, transparent); stroke: var(--sw-c); stroke-width: 2;
  opacity: 0; transition: opacity .4s ease; }
.lc-sw-box.is-on { opacity: 1; }
.lc-sw-base { stroke: var(--upwr-rule); stroke-width: 1.5; }
.lc-sw-name { font: 600 19px var(--upwr-sans); fill: var(--upwr-ink); text-anchor: middle; }
.lc-sw-num { font: 700 30px var(--upwr-mono); fill: var(--sw-c); text-anchor: middle; }
.lc-sw-num.is-lik { font-weight: 600; font-size: 24px; fill: var(--upwr-ink-soft); }
.lc-sw-num.is-top { font-size: 36px; }
.lc-sw-readout { font: 600 15px var(--upwr-mono); color: var(--upwr-ink); margin: .2em 0 .6em; min-height: 1.4em; }
@media (prefers-reduced-motion: reduce) { .lc-sw-svg * { transition: none !important; } }
"

a3_stop_service_ui <- figure_panel(
  label = "Scena",
  title = "Serwis szuka winnego",
  full_width = TRUE,
  tags$style(HTML(a3_stop_service_css)),
  tags$div(class = "lc-sw",
    `data-config` = jsonlite::toJSON(list(
      groups = a3_service_groups,
      symTotal = sum(a3_service_groups$sym),
      frames = a3_service_frames
    ), auto_unbox = TRUE, dataframe = "rows"),
    tags$div(class = "lc-sp-head",
      tags$span(class = "lc-sp-name", `data-sw-out` = "name", a3_service_frames[[1]]$name),
      tags$span(class = "lc-sp-step", `data-sw-out` = "step", "Kadr 1 z 4")
    ),
    tags$div(class = "lc-sw-stage", role = "img"),
    tags$p(class = "lc-sp-caption lc-sw-caption", `aria-live` = "polite"),
    tags$p(class = "lc-sw-readout"),
    tags$div(class = "lc-sp-nav",
      tags$button(type = "button", class = "lc-action is-outline", `data-sw` = "prev", lc_icon("prev"), tags$span("Wstecz")),
      tags$button(type = "button", class = "lc-action is-solid", `data-sw` = "next", tags$span("Dalej"), lc_icon("next"))
    )
  )
)
