# Punkt stop na początku sekcji „Pułapka częstości bazowej” (przed krzywą):
#   a3_stop_halls_ui — scena „Dwie hale, ten sam czujnik” (dwa światy obok siebie).
# Rysunek i kadry działają w przeglądarce (modules/dwiehale.js); bez serwera.
# Ten sam czujnik (czułość 0.95, FPR 0.05) w dwóch halach Bananpolu, po 2000
# zmian z czterech lat: hala A nowa (awarie 1%), hala B stara (awarie 5%).

a3_halls <- local({
  sens <- 0.95; fpr <- 0.05; n <- 2000L
  h <- data.frame(
    key  = c("A", "B"),
    name = c("Hala A · nowa", "Hala B · stara"),
    base = c(0.01, 0.05),
    stringsAsFactors = FALSE
  )
  h$events <- as.integer(round(n * h$base))
  h$hits   <- as.integer(round(sens * h$events))
  h$false  <- as.integer(round(fpr * (n - h$events)))
  h$alarms <- h$hits + h$false
  h$ppv    <- sprintf("%.2f", h$hits / h$alarms)
  list(n = n, sens = sens, fpr = fpr, halls = h)
})

a3_halls_frames <- local({
  h <- a3_halls$halls
  approx <- function(i) {
    r <- h$hits[i] / h$alarms[i]
    if (abs(r - round(r, 2)) < 1e-9) "=" else "≈"
  }
  list(
    list(name = "Dwie hale", step = 1L,
      read = sprintf("Hala A: %d zmian · hala B: %d zmian", a3_halls$n, a3_halls$n),
      text = "Bananpol ma dwie dojrzewalnie z tym samym czujnikiem i tym samym telefonem do dyżurnego. Hala A jest nowa, hala B stara. Każde pole to jedna zmiana z czterech lat."),
    list(name = "Awarie", step = 2L,
      read = sprintf("Awarie: hala A %d · hala B %d", h$events[1], h$events[2]),
      text = sprintf("W nowej hali awaria zdarza się na jednej zmianie na sto, w starej na pięciu zmianach na sto. W czterech latach to %d i %d zmian.", h$events[1], h$events[2])),
    list(name = "Alarmy", step = 3L,
      read = sprintf("Prawdziwe alarmy: %d i %d · fałszywe: %d i %d", h$hits[1], h$hits[2], h$false[1], h$false[2]),
      text = "Czujnik jest ten sam: wykrywa 95% awarii i alarmuje na 5% spokojnych zmian. W nowej hali spokojnych zmian jest więcej, więc fałszywych alarmów też."),
    list(name = "Telefony", step = 4L,
      read = sprintf("Hala A: %d/%d %s %s · hala B: %d/%d %s %s",
        h$hits[1], h$alarms[1], approx(1), h$ppv[1], h$hits[2], h$alarms[2], approx(2), h$ppv[2]),
      text = "Dyżurny widzi tylko telefony. W nowej hali prawdziwy jest mniej więcej co szósty, w starej co drugi. Ten sam czujnik i ten sam telefon, ale w starej hali alarm znaczy co innego.")
  )
})

a3_stop_halls_css <- "
.lc-dh-svg { display: block; width: 100%; max-width: 900px; height: auto; margin: 0 auto; }
.lc-dh-book { fill: var(--upwr-surface); stroke: var(--upwr-rule); stroke-width: 1.5; }
.lc-dh-hall { font: 600 17px var(--upwr-sans); fill: var(--upwr-ink); }
.lc-dh-cell { fill: color-mix(in srgb, var(--upwr-ink-subtle) 22%, var(--upwr-surface));
  transition: transform 1.1s cubic-bezier(.2, .7, .2, 1), opacity .6s ease, fill .4s ease; }
.lc-dh-cell.is-event { fill: var(--upwr-accent-tint); stroke: var(--upwr-accent); stroke-width: 1.2; }
.lc-dh-cell.is-hit { fill: var(--upwr-accent); }
.lc-dh-cell.is-false { fill: var(--upwr-single-alt); }
.lc-dh-cell.is-faded { opacity: .12; }
.lc-dh-late { opacity: 0; transition: opacity .5s ease .8s; }
.lc-dh-svg.is-piles .lc-dh-late { opacity: 1; }
.lc-dh-pile { font: 600 14px var(--upwr-sans); }
.lc-dh-pile.is-true { fill: var(--upwr-accent); }
.lc-dh-pile.is-false { fill: var(--upwr-single-alt); }
.lc-dh-ppv-label { font: 14px var(--upwr-sans); fill: var(--upwr-ink-soft); text-anchor: middle; }
.lc-dh-ppv { font: 700 40px var(--upwr-mono); fill: var(--upwr-ink); text-anchor: middle; }
.lc-dh-readout { font: 600 15px var(--upwr-mono); color: var(--upwr-ink); margin: .2em 0 .6em; min-height: 1.4em; }
@media (prefers-reduced-motion: reduce) { .lc-dh-svg * { transition: none !important; } }
"

a3_stop_halls_ui <- figure_panel(
  label = "Scena",
  title = "Dwie hale, ten sam czujnik",
  full_width = TRUE,
  tags$style(HTML(a3_stop_halls_css)),
  tags$div(class = "lc-dh",
    `data-config` = jsonlite::toJSON(list(
      cols = 50L, rows = 40L,
      halls = a3_halls$halls[, c("key", "name", "events", "hits", "false", "alarms", "ppv")],
      frames = a3_halls_frames
    ), auto_unbox = TRUE, dataframe = "rows"),
    tags$div(class = "lc-sp-head",
      tags$span(class = "lc-sp-name", `data-dh-out` = "name", a3_halls_frames[[1]]$name),
      tags$span(class = "lc-sp-step", `data-dh-out` = "step", sprintf("Kadr 1 z %d", length(a3_halls_frames)))
    ),
    tags$div(class = "lc-dh-stage", role = "img"),
    tags$p(class = "lc-sp-caption lc-dh-caption", `aria-live` = "polite"),
    tags$p(class = "lc-dh-readout"),
    tags$div(class = "lc-sp-nav",
      tags$button(type = "button", class = "lc-action is-outline", `data-dh` = "prev", lc_icon("prev"), tags$span("Wstecz")),
      tags$button(type = "button", class = "lc-action is-solid", `data-dh` = "next", tags$span("Dalej"), lc_icon("next"))
    )
  )
)
