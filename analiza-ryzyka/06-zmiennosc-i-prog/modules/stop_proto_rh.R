# PROTOTYP PUNKTU STOP (2026-11-17) — grupa rh: P17 ---------------------------------
#
#   z6p_17_ui — Przykłady „Interwencje w praktyce” (P17): po jednym slajdzie na
#               działanie z zakładu (chłodzenie, stabilizacja, próg), każdy z sytuacją,
#               zmianą μ, σ albo c, wynikiem i decyzją (oba podniesienia progu na jednym
#               slajdzie, nieuprawnione z „Uwaga”); ostatni slajd zestawia wszystkie
#               na jednej osi P(T > c). Kadry SVG policzone w R, bez JS.
# Wymaga: stop_proto_common.R (z6p_*). Stan bazowy: N(82, 3), próg 85°C.

# ============================================================================
# P17 — PRZYKŁADY „INTERWENCJE W PRAKTYCE”
# ============================================================================

z6p17_groups <- list(
  mu  = list(tag = "chłodzenie", what = "zmienia μ", css = "z6p17-mu"),
  sd  = list(tag = "stabilizacja", what = "zmienia σ", css = "z6p17-sd"),
  both = list(tag = "stabilizacja i chłodzenie", what = "zmienia σ i μ", css = "z6p17-sd"),
  c   = list(tag = "próg", what = "zmienia c", css = "z6p17-c")
)

# Przykłady: grupa, nazwa, parametry po zmianie, opis sytuacji i decyzja, krótka
# etykieta do zestawienia, ok = FALSE dla działania bez uzasadnienia.
z6p17_examples <- list(
  list(group = "mu", name = "Dodatkowy wentylator obudowy", short = "wentylator obudowy",
    mu = 80, sd = 3, c = 85, ok = TRUE,
    text = "Na obudowie silnika montuje się drugi wentylator. Łożysko pracuje przeciętnie o 2°C chłodniej, a wahania zostają takie same. Decyzja: działanie na mechanizm, ryzyko spada naprawdę."),
  list(group = "mu", name = "Smar o mniejszym tarciu", short = "smar o mniejszym tarciu",
    mu = 81, sd = 3, c = 85, ok = TRUE,
    text = "Zmiana smaru zmniejsza tarcie, więc przy tych samych obrotach powstaje mniej ciepła. Średnia spada o 1°C. Decyzja: efekt mniejszy niż przy wentylatorze, ale też prawdziwy, a zmiana jest tania."),
  list(group = "sd", name = "Falownik zamiast termostatu", short = "falownik",
    mu = 82, sd = 2, c = 85, ok = TRUE,
    text = "Wentylator sterowany termostatem pracuje cyklami: pełna moc, potem przestój. Falownik trzyma stałe, pośrednie obroty. Przeciętna temperatura ta sama, wahania mniejsze. Decyzja: ryzyko spada, bo rzadziej zdarzają się gorące szczyty."),
  list(group = "sd", name = "Rozłożony załadunek komór", short = "rozłożony załadunek",
    mu = 82, sd = 2.5, c = 85, ok = TRUE,
    text = "Dotąd wszystkie komory dojrzewania ładowano w poniedziałek rano: wentylatory miały szczyt na początku tygodnia i luz pod koniec. Załadunek rozłożony na cały tydzień spłaszcza szczyty. Decyzja: zmiana organizacyjna, bez nowego sprzętu."),
  list(group = "both", name = "Automatyczny smarownik", short = "automatyczny smarownik",
    mu = 81.5, sd = 2.5, c = 85, ok = TRUE,
    text = "Ręczne smarowanie „gdy ktoś pamięta” daje okresy pracy na suchym łożysku z gorącymi skokami. Automatyczny smarownik podaje małe dawki w stałych odstępach: znikają skoki, a przy okazji średnia spada o pół stopnia. Prawdziwe działania rzadko ruszają tylko jeden parametr."),
  # Dwa podniesienia progu mają ten sam rysunek i te same liczby, więc dzielą
  # jeden slajd (dwa podpisy); w zestawieniu stoją osobno. slide = FALSE: bez slajdu.
  list(group = "c", name = "Podniesienie progu do 87°C", short = "próg: łożysko wyższej klasy",
    mu = 82, sd = 3, c = 87, ok = TRUE,
    text = tagList(
      tags$strong("Łożysko wyższej klasy:"), " łożysko wymieniono na typ, dla którego producent dopuszcza pracę ciągłą do 90°C, więc próg ostrzegawczy podniesiono do 87°C, z zapasem. Zmiana uprawniona: stoi za nią dowód konstrukcyjny.",
      tags$br(),
      tags$strong(class = "z6p17-warn", "Uwaga, za dużo alarmów:"), " ten sam rysunek powstaje, gdy kierownik podnosi próg, bo raport miesięczny „świeci na czerwono”, a łożysko się nie zmieniło. Zmiana nieuprawniona: przekroczeń ubywa tylko w raporcie.")),
  list(group = "c", name = "Za dużo alarmów w raporcie", short = "próg: za dużo alarmów",
    mu = 82, sd = 3, c = 87, ok = FALSE, slide = FALSE)
)

# Przykłady z własnym slajdem.
z6p17_slides <- Filter(function(e) !isFALSE(e$slide), z6p17_examples)

z6p17_p <- function(e) pnorm(e$c, e$mu, e$sd, lower.tail = FALSE)
z6p17_fmt_p <- function(p) formatC(p, format = "f", digits = 3)

z6p17_css <- tags$style(HTML("
  .z6p17-mu { fill: var(--upwr-cat-niebo); }
  .z6p17-sd { fill: var(--upwr-cat-szalwia); }
  .z6p17-c  { fill: var(--upwr-cat-indygo); }
  .z6p17-tag { font-size: 13px; font-weight: 700; letter-spacing: .04em; fill: var(--upwr-surface) !important; }
  .z6p17-val { font-family: var(--upwr-mono) !important; font-size: 16px; }
  .z6p17-changed { fill: var(--upwr-accent) !important; font-weight: 700; }
  .z6p17-big { font-family: var(--upwr-mono) !important; font-size: 26px; font-weight: 700; fill: var(--upwr-accent) !important; }
  .z6p17-warn { color: var(--upwr-accent); }
  .z6p17-hollow { fill: var(--upwr-surface); stroke-width: 2.4; }
  .z6p17-hollow.z6p17-c { stroke: var(--upwr-cat-indygo); }
"))

# Mini wykres: oś 68–98°C jak w całym wykładzie, gęstość do 0.21.
z6p17_x <- function(t) 30 + (t - 68) / 30 * 440
z6p17_y <- function(d) 270 - d / 0.21 * 220

z6p17_plot <- function(e) {
  curve_moved <- e$mu != z6p_mu || e$sd != z6p_sd
  thr_moved <- e$c != z6p_c
  xc <- z6p17_x(e$c)
  paste0(
    sprintf('<path class="z6p-area" d="%s"/>', z6p_norm_path(e$mu, e$sd, z6p17_x, z6p17_y, e$c, 98, close = TRUE)),
    if (curve_moved) sprintf('<path class="z6p-curve-ghost" d="%s"/>',
      z6p_norm_path(z6p_mu, z6p_sd, z6p17_x, z6p17_y, 68, 98)),
    sprintf('<path class="z6p-curve" d="%s"/>', z6p_norm_path(e$mu, e$sd, z6p17_x, z6p17_y, 68, 98)),
    z6p_axis(z6p17_x, 270, seq(70, 95, 5), label = "temperatura łożyska (°C)"),
    if (thr_moved) sprintf('<line class="z6p-mean" x1="%g" x2="%g" y1="70" y2="270"/><text class="z6p-subtle" x="%g" y="64" font-size="12" text-anchor="end">był %d°C</text>',
      z6p17_x(z6p_c), z6p17_x(z6p_c), z6p17_x(z6p_c) - 4, z6p_c),
    sprintf('<line class="z6p-limit" x1="%g" x2="%g" y1="50" y2="270"/>', xc, xc),
    sprintf('<text class="z6p-mono" x="%g" y="44" font-size="13">próg %s°C</text>', xc + 4, z6p_num(e$c))
  )
}

# Prawa kolumna: plakietka grupy, parametry (zmienione na czerwono), z i wynik.
z6p17_side <- function(e) {
  g <- z6p17_groups[[e$group]]
  par_row <- function(y, sym, now, was, unit = "°C") {
    changed <- now != was
    sprintf('<text class="z6p17-val%s" x="520" y="%g">%s = %s%s%s</text>', if (changed) " z6p17-changed" else "", y, sym,
      z6p_num(now), unit, if (changed) sprintf(' <tspan class="z6p-subtle" font-size="13" font-weight="400">(było %s)</tspan>', z6p_num(was)) else "")
  }
  p <- z6p17_p(e)
  tag_w <- 18 + 8.2 * nchar(g$tag)
  paste0(
    sprintf('<rect class="%s" x="520" y="20" width="%g" height="26" rx="13"/>', g$css, tag_w),
    sprintf('<text class="z6p17-tag" x="%g" y="38" text-anchor="middle">%s</text>', 520 + tag_w / 2, toupper(g$tag)),
    sprintf('<text class="z6p-subtle" x="520" y="70" font-size="13">%s</text>', g$what),
    par_row(104, "μ", e$mu, z6p_mu), par_row(130, "σ", e$sd, z6p_sd), par_row(156, "c", e$c, z6p_c),
    sprintf('<text class="z6p17-val" x="520" y="190">z = %s</text>', formatC((e$c - e$mu) / e$sd, format = "f", digits = 2)),
    sprintf('<text class="z6p17-big" x="520" y="232">%s</text>', z6p17_fmt_p(p)),
    sprintf('<text class="z6p-mono z6p-subtle" x="520" y="254" font-size="13">%d na 1000 · było 159</text>', round(1000 * p))
  )
}

# Ostatni slajd: wszystkie przykłady na jednej osi P(T > c), stan bazowy jako kreska.
z6p17_summary <- function() {
  x_of <- function(p) 300 + p / 0.17 * 440
  rows <- vapply(seq_along(z6p17_examples), function(i) {
    e <- z6p17_examples[[i]]; g <- z6p17_groups[[e$group]]
    y <- 40 + (i - 1) * 34; p <- z6p17_p(e)
    paste0(
      sprintf('<line class="z6p-grid" x1="300" x2="740" y1="%g" y2="%g"/>', y, y),
      sprintf('<text x="0" y="%g" font-size="14">%s</text>', y + 5, e$short),
      if (isTRUE(e$ok)) sprintf('<circle class="%s" cx="%g" cy="%g" r="8"/>', g$css, x_of(p), y)
      else sprintf('<circle class="z6p17-hollow %s" cx="%g" cy="%g" r="7"/>', g$css, x_of(p), y),
      sprintf('<text class="z6p-mono" x="%g" y="%g" font-size="13">%s%s</text>', x_of(p) + 14, y + 5,
        z6p17_fmt_p(p), if (isTRUE(e$ok)) "" else " · tylko w raporcie")
    )
  }, character(1))
  y_end <- 40 + (length(z6p17_examples) - 1) * 34 + 24
  p0 <- pnorm(z6p_c, z6p_mu, z6p_sd, lower.tail = FALSE)
  paste0(
    paste(rows, collapse = ""),
    sprintf('<line class="z6p-mean" x1="%g" x2="%g" y1="20" y2="%g"/>', x_of(p0), x_of(p0), y_end),
    sprintf('<text class="z6p-subtle" x="%g" y="14" font-size="12" text-anchor="middle">stan bazowy 0.159</text>', x_of(p0)),
    z6p_axis(x_of, y_end, c(0, 0.05, 0.10, 0.15), label = "P(T > c)", labels = c("0", "0.05", "0.10", "0.15")),
    # legenda grup
    paste(vapply(seq_along(z6p17_groups[c("mu", "sd", "c")]), function(k) {
      g <- z6p17_groups[c("mu", "sd", "c")][[k]]; x <- 300 + (k - 1) * 150
      sprintf('<circle class="%s" cx="%g" cy="%g" r="6"/><text class="z6p-subtle" x="%g" y="%g" font-size="12">%s</text>',
        g$css, x, y_end + 52, x + 10, y_end + 56, g$tag)
    }, character(1)), collapse = "")
  )
}

z6p17_n <- length(z6p17_slides)

z6p17_frames <- c(
  lapply(z6p17_slides, function(e) {
    p <- z6p17_p(e)
    list(name = paste0(toupper(substr(z6p17_groups[[e$group]]$tag, 1, 1)), substring(z6p17_groups[[e$group]]$tag, 2), " · ", e$name),
      text = e$text,
      read = sprintf("P(T > c) = %s · %d na 1000 pomiarów (było 159)", z6p17_fmt_p(p), round(1000 * p)))
  }),
  list(list(name = "Zestawienie",
    text = "Wentylator obudowy i oba podniesienia progu dają tę samą liczbę, 48 na 1000. Tylko wentylator zmienia temperaturę łożyska, a tylko jedno z podniesień progu ma uzasadnienie. Sama liczba nie mówi, które działanie poprawiło łożysko.",
    read = NULL))
)

z6p17_svg <- function(step) {
  if (step > z6p17_n) {
    return(z6p_svg(800, 360, "Zestawienie przykładów na osi prawdopodobieństwa przekroczenia, z kreską stanu bazowego 0.159.",
      z6p17_summary()))
  }
  e <- z6p17_slides[[step]]
  z6p_svg(800, 312,
    sprintf("%s: rozkład temperatury N(%s, %s) z progiem %s stopni na tle stanu bazowego; prawdopodobieństwo przekroczenia %s.",
      e$name, z6p_num(e$mu), z6p_num(e$sd), z6p_num(e$c), z6p17_fmt_p(z6p17_p(e))),
    z6p17_plot(e), z6p17_side(e))
}

z6p_17_ui <- tagList(z6p17_css,
  z6p_frames_ui("P17", "przykłady", "Interwencje w praktyce", z6p17_frames, z6p17_svg, proto = FALSE))
