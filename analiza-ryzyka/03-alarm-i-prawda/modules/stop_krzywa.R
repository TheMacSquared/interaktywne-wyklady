# Galeria sekcji „Pułapka częstości bazowej”:
#   a3_stop_curve_ui — „P(awaria | 🚨) a częstość bazowa: trzy czujniki”.
# Zamiast suwaków trzy warianty czujnika, każdy ze swoim powodem: obecny,
# tańszy (niższa czułość) i z cichym tłem (niski FPR). Krzywa bieżącego wariantu
# na tle bladej krzywej obecnego czujnika, trzy nazwane hale jako punkty
# i odczyt w naturalnych częstościach. Wykresy to SVG z R; przełączanie
# obsługuje modules/calkowite.js (te same klasy lc-pc-*).

a3_curve_halls <- data.frame(
  name = c("Nowa hala", "Stara hala", "Upał w lipcu"),
  per100 = c(1, 5, 20)
)

a3_curve_variants <- list(
  list(key = "now", short = "Obecny czujnik", name = "Obecny czujnik", sens = 0.95, fpr = 0.05,
    text = "Czujnik z hali Bananpolu: łapie 95 awarii na 100 i alarmuje fałszywie na 5 spokojnych zmianach na 100."),
  list(key = "sens", short = "Tańszy czujnik", name = "Tańszy czujnik", sens = 0.80, fpr = 0.05,
    text = "Oszczędzamy: tańszy czujnik łapie tylko 80 awarii na 100 zamiast 95. Tło fałszywych alarmów zostaje takie samo: 5 na 100 spokojnych zmian."),
  list(key = "fpr", short = "Ciche tło", name = "Ciche tło", sens = 0.95, fpr = 0.01,
    text = "Czułość zostaje 0.95, ale ściszamy tło: fałszywy alarm tylko na 1 spokojnej zmianie na 100.")
)

# „1 z 6”, „co drugi”, „5 z 6”: najmniejszy mianownik do 25, który oddaje p
# z błędem < 0.02; przy p > 0.9 liczymy fałszywe („1 fałszywy na 25”).
a3_curve_in_words <- function(p) {
  if (p > 0.9) return(sprintf("tylko 1 fałszywy na %d alarmów", round(1 / (1 - p))))
  for (m in 2:25) {
    k <- round(p * m)
    if (k >= 1 && abs(k / m - p) < 0.02) {
      if (k == 1 && m == 2) return("co drugi alarm")
      return(sprintf("%d z %d alarmów", k, m))
    }
  }
  sprintf("%.0f na 100 alarmów", 100 * p)
}

a3_curve_svg <- function(sens, fpr, ref_sens = 0.95, ref_fpr = 0.05) {
  x0 <- 70; x1 <- 690; y0 <- 290; y1 <- 24
  X <- function(per100) x0 + (x1 - x0) * per100 / 20
  Y <- function(p) y0 - (y0 - y1) * p
  path <- function(s, f) {
    a <- seq(0, 20, length.out = 160)
    p <- vapply(a / 100, risk_bayes, numeric(1), sensitivity = s, false_positive_rate = f)
    p[is.na(p)] <- 0
    paste0("M", paste(sprintf("%.1f %.1f", X(a), Y(p)), collapse = " L"))
  }
  grid <- paste0(
    paste(sprintf('<line class="a3-cv-grid" x1="%g" x2="%g" y1="%g" y2="%g"/><text class="a3-cv-tick" x="%g" y="%g" text-anchor="end">%s</text>',
      x0, x1, Y(c(0, .25, .5, .75, 1)), Y(c(0, .25, .5, .75, 1)), x0 - 8, Y(c(0, .25, .5, .75, 1)) + 4,
      c("0", "0.25", "0.5", "0.75", "1")), collapse = ""),
    paste(sprintf('<text class="a3-cv-tick" x="%g" y="%g" text-anchor="middle">%d</text>', X(c(0, 5, 10, 15, 20)), y0 + 20, c(0, 5, 10, 15, 20)), collapse = ""),
    sprintf('<text class="a3-cv-axis" x="%g" y="%g" text-anchor="middle">awarie na 100 zmian</text>', (x0 + x1) / 2, y0 + 42),
    sprintf('<text class="a3-cv-axis" x="18" y="%g" transform="rotate(-90 18 %g)" text-anchor="middle">P(awaria | 🚨)</text>', (y0 + y1) / 2, (y0 + y1) / 2)
  )
  pts <- paste(vapply(seq_len(nrow(a3_curve_halls)), function(i) {
    h <- a3_curve_halls[i, ]
    p <- risk_bayes(h$per100 / 100, sens, fpr)
    # Krzywa jest wklęsła: pod nią i na prawo od punktu jest wolne miejsce;
    # ostatnia hala stoi przy prawej krawędzi, więc jej podpis idzie nad punkt.
    last <- i == nrow(a3_curve_halls)
    anchor <- if (last) "end" else "start"
    lx <- X(h$per100) + if (last) -10 else 12
    ly <- Y(p) + if (last) -14 else 24
    sprintf('<line class="a3-cv-drop" x1="%g" x2="%g" y1="%g" y2="%g"/><circle class="a3-cv-pt" cx="%g" cy="%g" r="7"/><text class="a3-cv-pt-lab" x="%g" y="%g" text-anchor="%s">%s · %.2f</text>',
      X(h$per100), X(h$per100), Y(p), y0, X(h$per100), Y(p), lx, ly, anchor, h$name, p)
  }, character(1)), collapse = "")
  is_ref <- isTRUE(all.equal(c(sens, fpr), c(ref_sens, ref_fpr)))
  HTML(paste0(
    sprintf('<svg class="a3-cv-svg" viewBox="0 0 720 345" role="img" aria-label="Krzywa P(awaria | alarm) względem liczby awarii na 100 zmian dla czujnika o czułości %.2f i FPR %.2f.">', sens, fpr),
    grid,
    if (!is_ref) sprintf('<path class="a3-cv-ref" d="%s"/>', path(ref_sens, ref_fpr)) else "",
    sprintf('<path class="a3-cv-line" d="%s"/>', path(sens, fpr)),
    pts,
    if (!is_ref) sprintf('<text class="a3-cv-ref-lab" x="%g" y="%g">obecny czujnik</text>', X(20) - 4, Y(risk_bayes(.2, ref_sens, ref_fpr)) + 22) else "",
    '</svg>'
  ))
}

a3_curve_cases <- lapply(a3_curve_variants, function(v) {
  ps <- vapply(a3_curve_halls$per100 / 100, risk_bayes, numeric(1), sensitivity = v$sens, false_positive_rate = v$fpr)
  rows <- sprintf("%s (%d na 100): %.2f ≈ %s", a3_curve_halls$name, a3_curve_halls$per100, ps, vapply(ps, a3_curve_in_words, ""))
  list(key = v$key, short = v$short, name = v$name, text = v$text,
    params = sprintf("czułość %.2f · FPR %.2f", v$sens, v$fpr),
    r1 = rows[1], r2 = rows[2], r3 = rows[3])
})

a3_curve_css <- "
.a3-cv-svg { display: block; width: 100%; max-width: 820px; height: auto; margin: 0 auto; }
.a3-cv-svg text { font-family: var(--upwr-mono); }
.a3-cv-grid { stroke: var(--upwr-rule); stroke-width: 1; }
.a3-cv-tick { font-size: 13px; fill: var(--upwr-ink-subtle); }
.a3-cv-axis { font: 14px var(--upwr-sans) !important; fill: var(--upwr-ink-soft); }
.a3-cv-ref { fill: none; stroke: var(--upwr-reference); stroke-width: 2.5; stroke-dasharray: 6 5; opacity: .8; }
.a3-cv-ref-lab { font-size: 12px; fill: var(--upwr-reference); text-anchor: end; }
.a3-cv-line { fill: none; stroke: var(--upwr-accent); stroke-width: 4; }
.a3-cv-drop { stroke: var(--upwr-ink-subtle); stroke-width: 1; stroke-dasharray: 3 3; }
.a3-cv-pt { fill: var(--upwr-accent); stroke: var(--upwr-surface); stroke-width: 2.5; }
.a3-cv-pt-lab { font-size: 14px; font-weight: 700; fill: var(--upwr-accent); }
.lc-pc-curve .lc-pc-params { font: 600 .95em var(--upwr-mono); color: var(--upwr-ink-soft); margin: -.4em 0 .6em; }
.lc-pc-curve .lc-pc-rows { list-style: none; margin: .2em 0 .4em; padding: 0; font: 600 1.05em var(--upwr-mono); color: var(--upwr-ink); }
.lc-pc-curve .lc-pc-rows li { padding: .15em 0; }
"

a3_stop_curve_ui <- local({
  first <- a3_curve_cases[[1]]
  out <- function(key, tag = tags$span) tag(`data-pc-out` = key, first[[key]])
  figure_panel(
    label = "Galeria",
    title = "P(awaria | 🚨) a częstość bazowa: trzy czujniki",
    full_width = TRUE,
    tags$style(HTML(a3_stop_total_css)),
    tags$style(HTML(a3_curve_css)),
    tags$div(class = "lc-pc lc-pc-curve", `data-step-label` = "Wariant",
      `data-config` = jsonlite::toJSON(list(cases = a3_curve_cases), auto_unbox = TRUE, digits = NA),
      tags$div(class = "lc-pc-top",
        tags$div(class = "lc-seg", role = "group", `aria-label` = "Wybór czujnika",
          lapply(seq_along(a3_curve_cases), function(i)
            tags$button(type = "button", `data-pc-go` = i - 1L,
              `aria-pressed` = if (i == 1L) "true" else "false",
              paste0(i, " · ", a3_curve_cases[[i]]$short)))
        )
      ),
      tags$div(class = "lc-sp-head",
        tags$span(class = "lc-sp-name", `data-pc-out` = "name", first$name),
        tags$span(class = "lc-sp-step", `data-pc-out` = "step", "Wariant 1 z 3")
      ),
      tags$p(class = "lc-pc-text", `data-pc-out` = "text", `aria-live` = "polite", first$text),
      tags$p(class = "lc-pc-params", `data-pc-out` = "params", first$params),
      tags$div(class = "lc-pc-body",
        tags$div(class = "lc-pc-trees",
          lapply(seq_along(a3_curve_variants), function(i) {
            v <- a3_curve_variants[[i]]
            tags$div(`data-pc-tree` = i - 1L, hidden = if (i > 1L) NA, a3_curve_svg(v$sens, v$fpr))
          })
        ),
        tags$ul(class = "lc-pc-rows",
          tags$li(`data-pc-out` = "r1", first$r1),
          tags$li(`data-pc-out` = "r2", first$r2),
          tags$li(`data-pc-out` = "r3", first$r3))
      ),
      tags$div(class = "lc-sp-nav",
        tags$button(type = "button", class = "lc-action is-outline", `data-pc` = "prev", lc_icon("prev"), tags$span("Wstecz")),
        tags$button(type = "button", class = "lc-action is-solid", `data-pc` = "next", tags$span("Dalej"), lc_icon("next"))
      )
    ),
    lc_caption("Linia przerywana: obecny czujnik dla porównania. Alarm to mieszanka dwóch strumieni: prawdziwych (rosną z awaryjnością hali) i fałszywych (stałe tło spokojnych zmian).")
  )
})
