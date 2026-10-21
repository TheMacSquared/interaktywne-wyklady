# Punkt stop sekcji „Od drzewa do Bayesa” (rozdział „Częstość bazowa”, po
# akapicie „W Bananpolu P(🚨) = 0.95 · 0.01 + …”):
#   a3_stop_total_panel(trees) — Przykład „Prawdopodobieństwo alarmu w trzech sytuacjach”.
# Drzewa (podświetlone obie drogi do alarmu) rysuje alarm_tree_svg z block.R,
# dlatego panel składa block.R po zdefiniowaniu drzewa.
# Trzy realizacje wzoru (3.3) P(🚨) = P(🚨 | A) P(A) + P(🚨 | Aᶜ) P(Aᶜ):
# hala A z obecnym czujnikiem, hala B ze starszym sprzętem i hala A
# z detektorem ostrożnym. Każda sytuacja to tablica krzyżowa na 10 000 zmian
# (kolumna 🚨 obrysowana) i wzór rozpisany w trzech linijkach; składniki
# w kolorach swoich komórek (prawdziwe alarmy — akcent, fałszywe — złoto).
# Przełączanie działa w przeglądarce (modules/calkowite.js); bez serwera.
# Liczby: czujnik 0.95/0.05 z rozdziału „Czułość i swoistość”, detektor
# ostrożny 0.80/0.01 (detektor B z rozdziału „Częstość bazowa”).

a3_total_population <- 10000

a3_total_fmt_p <- function(x) {
  s <- sprintf("%.6f", x)
  sub("\\.$", "", sub("0+$", "", s))
}
# Parametry wejściowe z co najmniej dwoma miejscami (0.80, nie 0.8).
a3_total_fmt_par <- function(x) {
  if (abs(x * 100 - round(x * 100)) < 1e-9) sprintf("%.2f", x) else a3_total_fmt_p(x)
}
a3_total_fmt_int <- function(x) {
  x <- round(x)
  # Twarda spacja tysięcy (intToUtf8(160)), żeby „10 000” nie łamało się w wąskiej kolumnie.
  if (x >= 10000) formatC(x, format = "d", big.mark = intToUtf8(160)) else as.character(x)
}

a3_total_raw <- list(
  list(key = "a", short = "Hala A", name = "Hala A, czujnik obecny",
    prevalence = 0.01, sensitivity = 0.95, fpr = 0.05,
    text = "Awaria zdarza się na 1% zmian; czujnik łapie 95% awarii i alarmuje fałszywie na 5% zmian bez awarii."),
  list(key = "b", short = "Hala B", name = "Hala B ze starszym sprzętem",
    prevalence = 0.05, sensitivity = 0.95, fpr = 0.05,
    text = "Ten sam czujnik, ale awarie zdarzają się pięć razy częściej: na 5% zmian."),
  list(key = "c", short = "Detektor ostrożny", name = "Hala A, detektor ostrożny",
    prevalence = 0.01, sensitivity = 0.80, fpr = 0.01,
    text = "Hala A z innym detektorem: łapie tylko 80% awarii, ale alarmuje fałszywie na 1% zmian bez awarii.")
)

a3_total_cases <- lapply(a3_total_raw, function(s) {
  tab <- risk_detector_counts(a3_total_population, s$prevalence, s$sensitivity, s$fpr)
  tp <- tab$alarm[1]; fn <- tab$no_alarm[1]
  fp <- tab$alarm[2]; tn <- tab$no_alarm[2]
  p_ac <- 1 - s$prevalence
  term_tp <- s$sensitivity * s$prevalence
  term_fp <- s$fpr * p_ac
  p_alarm <- term_tp + term_fp
  stopifnot(abs(p_alarm * a3_total_population - (tp + fp)) < 1e-6)
  f <- a3_total_fmt_p; n <- a3_total_fmt_int
  list(
    key = s$key, short = s$short, name = s$name, text = s$text,
    sens = a3_total_fmt_par(s$sensitivity), pA = a3_total_fmt_par(s$prevalence),
    fpr = a3_total_fmt_par(s$fpr), pAc = a3_total_fmt_par(p_ac),
    termTp = f(term_tp), termFp = f(term_fp), pAlarm = f(p_alarm),
    countTp = n(tp), countFp = n(fp), countAlarm = n(tp + fp),
    population = n(a3_total_population)
  )
})

a3_stop_total_css <- "
.lc-pc { container-type: inline-size; }
.lc-pc-top { display: flex; flex-wrap: wrap; gap: .5em; margin: .1em 0 .7em; }
.lc-pc .lc-seg button[aria-pressed='true'] { background: var(--upwr-accent); color: var(--upwr-surface); }
.lc-pc-text { font-size: 1.05em; color: var(--upwr-ink-soft); min-height: 1.6em; margin: 0 0 .9em; max-width: 52em; }
.lc-pc-body { display: grid; grid-template-columns: minmax(0, 1fr); gap: .6em; justify-items: center; margin: .2em 0 .9em; transition: opacity .35s ease; }
.lc-pc[data-swap='1'] .lc-pc-body { opacity: .15; transition: none; }
.lc-pc-formula { display: grid; grid-template-columns: auto minmax(0, 1fr); gap: .45em .45em; align-items: baseline; font-family: var(--upwr-mono); }
.lc-pc-lhs { text-align: right; font-size: 1.2em; font-weight: 600; color: var(--upwr-ink); white-space: nowrap; }
.lc-pc-rhs { font-size: 1.2em; font-weight: 600; color: var(--upwr-ink-soft); }
.lc-pc-rhs.is-big, .lc-pc-lhs.is-big { font-size: 1.55em; }
.lc-pc-rhs span { white-space: nowrap; }
.lc-pc-tp { color: var(--upwr-accent); }
.lc-pc-fp { color: var(--upwr-single-alt); }
.lc-pc-res { color: var(--upwr-ink); font-weight: 800; border: 3px solid var(--upwr-ink); border-radius: 8px; padding: 0 .25em; }
.lc-pc-counts { grid-column: 1 / -1; margin-top: .55em; padding-top: .55em; border-top: 1px solid var(--upwr-rule); font: 600 1.15em var(--upwr-mono); color: var(--upwr-ink-soft); }
.lc-pc-counts b { color: var(--upwr-ink); font-weight: 800; }
.lc-pc-counts small { font: 1em var(--upwr-sans); color: var(--upwr-ink-subtle); }
.lc-pc-trees { width: 100%; }
.lc-pc-trees [hidden] { display: none; }
@container (max-width: 520px) {
  .lc-pc-lhs, .lc-pc-rhs { font-size: 1em; }
  .lc-pc-rhs.is-big, .lc-pc-lhs.is-big { font-size: 1.3em; }
}
@media (prefers-reduced-motion: reduce) { .lc-pc-body { transition: none !important; } }
"

a3_total_first <- a3_total_cases[[1]]

# Pola wypełniane przez JS; startowo wartości pierwszej sytuacji (widok bez JS).
a3_total_out <- function(key, cls = NULL, ...) {
  tags$span(class = cls, `data-pc-out` = key, if (length(list(...))) list(...) else a3_total_first[[key]])
}

a3_stop_total_panel <- function(trees) figure_panel(
  label = "Przykład",
  title = "Prawdopodobieństwo alarmu w trzech sytuacjach",
  full_width = TRUE,
  tags$style(HTML(a3_stop_total_css)),
  tags$div(class = "lc-pc",
    `data-config` = jsonlite::toJSON(list(cases = a3_total_cases), auto_unbox = TRUE, digits = NA),
    tags$div(class = "lc-pc-top",
      tags$div(class = "lc-seg", role = "group", `aria-label` = "Wybór sytuacji",
        lapply(seq_along(a3_total_cases), function(i)
          tags$button(type = "button", `data-pc-go` = i - 1L,
            `aria-pressed` = if (i == 1L) "true" else "false",
            paste0(i, " · ", a3_total_cases[[i]]$short)))
      )
    ),
    tags$div(class = "lc-sp-head",
      tags$span(class = "lc-sp-name", `data-pc-out` = "name", a3_total_first$name),
      tags$span(class = "lc-sp-step", `data-pc-out` = "step", "Sytuacja 1 z 3")
    ),
    tags$p(class = "lc-pc-text", `data-pc-out` = "text", `aria-live` = "polite", a3_total_first$text),
    tags$div(class = "lc-pc-body",
      tags$div(class = "lc-pc-trees",
        lapply(seq_along(trees), function(i)
          tags$div(`data-pc-tree` = i - 1L, hidden = if (i > 1L) NA, trees[[i]]))
      ),
      tags$div(class = "lc-pc-formula", `aria-label` = "Wzór 3.3 dla wybranej sytuacji",
        tags$div(class = "lc-pc-lhs", "P(🚨) ="),
        tags$div(class = "lc-pc-rhs",
          tags$span(class = "lc-pc-tp", "P(🚨 | A)·P(A)"), " + ",
          tags$span(class = "lc-pc-fp", "P(🚨 | Aᶜ)·P(Aᶜ)")),
        tags$div(class = "lc-pc-lhs is-big", "="),
        tags$div(class = "lc-pc-rhs is-big",
          tags$span(class = "lc-pc-tp", a3_total_out("sens"), "·", a3_total_out("pA")), " + ",
          tags$span(class = "lc-pc-fp", a3_total_out("fpr"), "·", a3_total_out("pAc"))),
        tags$div(class = "lc-pc-lhs is-big", "="),
        tags$div(class = "lc-pc-rhs is-big",
          a3_total_out("termTp", "lc-pc-tp"), " + ", a3_total_out("termFp", "lc-pc-fp"),
          " = ", a3_total_out("pAlarm", "lc-pc-res")),
        tags$div(class = "lc-pc-counts",
          a3_total_out("countTp", "lc-pc-tp"), " + ", a3_total_out("countFp", "lc-pc-fp"),
          " = ", tags$b(`data-pc-out` = "countAlarm", a3_total_first$countAlarm), " ",
          tags$small("alarmów na ", a3_total_out("population"), " zmian"))
      )
    ),
    tags$div(class = "lc-sp-nav",
      tags$button(type = "button", class = "lc-action is-outline", `data-pc` = "prev", lc_icon("prev"), tags$span("Wstecz")),
      tags$button(type = "button", class = "lc-action is-solid", `data-pc` = "next", tags$span("Dalej"), lc_icon("next"))
    )
  ),
  lc_caption("P(🚨) to suma dwóch dróg do alarmu: przez awarię (burgund) i bez awarii (złoto).")
)
