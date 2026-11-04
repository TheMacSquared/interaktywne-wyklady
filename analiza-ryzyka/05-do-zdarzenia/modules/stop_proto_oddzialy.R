# PUNKTY STOP (2026-11-03) — oddziały i taśma przecięta na n, wykład 05 ----------
#
# Punkty stop rysowane po stronie serwera (SVG z R + renderUI), jak w stop_proto_sceny.R.
#   d5p_branches_ui — Scena „Dwadzieścia oddziałów” (historia z powtórzeniem) (P8)
#   d5p_cut_ui      — Pojęcie „Taśma przecięta na n” (wzór 5.7) (P9)
# Serwer: d5p_oddzialy_server(input, output, session).
# Wejścia: d5p_br_one, d5p_br_all, d5p_br_sez, d5p_br_reset (P8);
#          d5p_cut_n (suwak), d5p_cut_new (P9).
# Wymaga: stop_proto_common.R (d5p_panel, d5p_css, d5p_svg, d5p_head, d5p_tape,
#   d5p_series, d5p_axis) oraz lc_action / lc_slider z R/lecture_layout.R.

d5p_br_budget <- 30L      # budżet kontroli na oddział (średnia r / p = 3 / 0.1)
d5p_br_rows <- 20L        # liczba oddziałów
d5p_br_seed <- 2L         # ziarno pierwszych 20 audytów: dokładnie 8 przekroczeń
d5p_br_model_over <- 1 - pnbinom(27, 3, 0.1)  # P(X > 30) = 0.411 (wzór 5.7)

# Pierwsze 20 audytów z ustalonym ziarnem (taśmy do trzeciej wady).
d5p_br_seed_tapes <- local({
  set.seed(d5p_br_seed)
  lapply(seq_len(d5p_br_rows), function(i) d5p_series(3, 0.1))
})

# Startowa taśma P9 z ziarnem 43: trzecia wada w 24. kontroli.
d5p_cut_seed <- 43L
d5p_cut_len <- 80L
d5p_cut_start <- local({
  set.seed(d5p_cut_seed)
  runif(d5p_cut_len) < 0.1
})

d5p_or <- function(x, y) if (is.null(x)) y else x

# ============================================================================
# (P8) DWADZIEŚCIE ODDZIAŁÓW — taśmy do 20 audytów, potem histogram końców X
# ============================================================================

# Kafelki oddziałów (n <= 20) albo histogram końców X (n > 20) na osi 0–120.
#   xs    — wektor liczb kontroli X do trzeciej wady, po jednej na audyt
#   tapes — listy taśm (logical) dla pierwszych audytów
d5p_branches_svg <- function(xs, tapes) {
  W <- 780; x0 <- 46; p <- 5.5; s <- 4.4; rowh <- 14; top <- 40; base <- 290
  budget <- d5p_br_budget
  x_of <- function(t) x0 + t * p
  n <- length(xs)
  budget_x <- x_of(budget)
  aria <- sprintf("Oddziały: %d audytów, pionowa kreska budżetu na %d kontrolach.", n, budget)

  if (n <= d5p_br_rows) {
    # Siatka 4 × 5 kafelków: numer oddziału, liczba kontroli, ✓/× i pasek
    # długości audytu z kreską budżetu (pasek do 90 kontroli, dalej „→”).
    cols <- 5L; tw <- 144; th <- 84; gap <- 12; gx <- (W - cols * tw - (cols - 1) * gap) / 2
    tiles <- vapply(seq_len(d5p_br_rows), function(i) {
      cx <- gx + ((i - 1) %% cols) * (tw + gap); cy <- 8 + ((i - 1) %/% cols) * (th + gap)
      name <- sprintf('<text class="d5p-mono" x="%g" y="%g" font-size="11" style="fill:%s">Oddział %d</text>',
        cx + 10, cy + 17, if (i <= n && xs[i] > budget) "var(--upwr-surface)" else "var(--upwr-ink-subtle)", i)
      if (i > n) return(paste0(
        sprintf('<rect class="d5p-cell-idle" x="%g" y="%g" width="%g" height="%g" rx="6"/>', cx, cy, tw, th), name))
      X <- xs[i]; ok <- X <= budget
      ink <- if (ok) "var(--upwr-ink)" else "var(--upwr-surface)"
      bx <- cx + 10; bw <- tw - 20; by <- cy + th - 14; bmax <- 90
      fill_w <- min(X, bmax) / bmax * bw
      paste0(
        sprintf('<rect class="%s" x="%g" y="%g" width="%g" height="%g" rx="6"/>',
          if (ok) "d5p-cell-ok" else "d5p-cell-bad", cx, cy, tw, th),
        name,
        sprintf('<text class="d5p-mono" x="%g" y="%g" font-size="28" font-weight="700" style="fill:%s">%d</text>',
          cx + 10, cy + 52, ink, X),
        sprintf('<text x="%g" y="%g" font-size="24" font-weight="700" text-anchor="end" style="fill:%s">%s</text>',
          cx + tw - 12, cy + 50, ink, if (ok) "✓" else "×"),
        sprintf('<rect x="%g" y="%g" width="%g" height="5" rx="2" style="fill:%s;opacity:.25"/>', bx, by, bw, ink),
        sprintf('<rect x="%g" y="%g" width="%g" height="5" rx="2" style="fill:%s"/>', bx, by, fill_w, ink),
        sprintf('<line x1="%g" x2="%g" y1="%g" y2="%g" style="stroke:%s" stroke-width="1.5"/>',
          bx + budget / bmax * bw, bx + budget / bmax * bw, by - 3, by + 8, ink),
        if (X > bmax) sprintf('<text x="%g" y="%g" font-size="11" style="fill:%s">→</text>', bx + bw + 1, by + 6, ink) else ""
      )
    }, character(1))
    legend <- sprintf('<text class="d5p-subtle" x="%g" y="%g" font-size="12">pasek: długość audytu (do 90 kontroli), kreska: budżet %d</text>',
      gx, 8 + 4 * (th + gap) + 8, budget)
    return(d5p_svg(W, 8 + 4 * (th + gap) + 16, aria, paste(tiles, collapse = ""), legend))
  }

  # Histogram końców X: słupki co 3 kontrole; X > 120 wpada do ostatniego słupka.
  H <- base + 50
  k <- pmin(ceiling(xs / 3), 40)
  cnt <- tabulate(k, 40)
  w <- 3 * p; hmax <- 200
  bars <- vapply(seq_len(40), function(j) {
    if (cnt[j] == 0) return("")
    h <- cnt[j] / max(cnt) * hmax
    cls <- if (3 * j <= budget) "d5p-ok" else "d5p-bad"
    bar <- sprintf('<rect class="%s" x="%g" y="%g" width="%g" height="%g" rx="1.5"/>',
      cls, x_of(3 * (j - 1)) + 1, base - h, w - 2, h)
    lab <- if (n <= 200) sprintf('<text class="d5p-mono d5p-subtle" x="%g" y="%g" font-size="10" text-anchor="middle">%d</text>',
      x_of(3 * (j - 1)) + w / 2, base - h - 4, cnt[j]) else ""
    paste0(bar, lab)
  }, character(1))
  line <- sprintf('<line class="d5p-limit" x1="%g" x2="%g" y1="%g" y2="%g"/>', budget_x, budget_x, 30, base)
  body <- paste0(paste(bars, collapse = ""), line,
    sprintf('<text class="d5p-subtle" x="%g" y="%g" font-size="12">budżet %d</text>', budget_x + 6, 24, budget),
    d5p_axis(x_of, base, c(0, 30, 60, 90, 120), label = "kontrola nr (koniec audytu)"))
  d5p_svg(W, H, aria, body)
}

# Odczyt pod rysunkiem: „Zabrakło: k z N”, a po 2000 audytach także model.
d5p_branches_read <- function(xs) {
  n <- length(xs)
  k <- sum(xs > d5p_br_budget)
  txt <- sprintf("Zabrakło: %d z %d", k, n)
  if (n > 0) txt <- paste0(txt, sprintf(" (%d%%)", round(100 * k / n)))
  if (n >= 2000) txt <- paste0(txt, sprintf(" · model: %.2f", d5p_br_model_over))
  tags$p(class = "d5p-read", txt)
}

d5p_branches_caption <- function(xs) {
  n <- length(xs)
  k <- sum(xs > d5p_br_budget)
  txt <- if (n == 0) {
    "Dyrektor jakości rozdaje każdemu z 20 oddziałów budżet po 30 kontroli. Wada co dziesiątą paletę, więc trzy wady to średnio 30 kontroli."
  } else if (n < d5p_br_rows) {
    "Jedne oddziały kończą szybko, inne wychodzą daleko poza budżet."
  } else if (n == d5p_br_rows) {
    sprintf("Budżet „średni” zawiódł w %d z 20 oddziałów.", k)
  } else if (n < 2000) {
    "Kolejne audyty dokładają słupki; odsetek przekroczeń jeszcze się ustala."
  } else {
    "Po sezonach ok. 4 audyty na 10 przekraczają budżet równy średniej."
  }
  tags$p(class = "d5p-caption", txt)
}

d5p_branches_ui <- d5p_panel("Scena", "Dwadzieścia oddziałów",
  d5p_css,
  uiOutput("d5p_br_head"),
  uiOutput("d5p_br_stage"),
  uiOutput("d5p_br_read"),
  uiOutput("d5p_br_caption"),
  tags$div(class = "d5p-nav",
    lc_action("d5p_br_one", "Audyt w kolejnym oddziale", variant = "solid"),
    lc_action("d5p_br_all", "Wszystkie oddziały"),
    lc_action("d5p_br_sez", "Sto sezonów"),
    lc_action("d5p_br_reset", icon = "reset", variant = "ghost", aria_label = "Od nowa")))

# ============================================================================
# (P9) TAŚMA PRZECIĘTA NA n — wzór (5.7): P(X ≤ n) = P(Y_n ≥ r)
# ============================================================================

# Kolory jak we wzorze (5.7): X i reguła „do trzeciej wady” w kolorze akcentu,
# Y_n i reguła „n prób” w kolorze nieba.
d5p_cut_col_x <- "var(--upwr-accent)"
d5p_cut_col_y <- "var(--upwr-cat-niebo)"

d5p_cut_css <- tags$style(HTML("
  .d5p-cut-two { display: grid; grid-template-columns: 1fr 1fr; gap: .9em 1.6em; margin: .4em 0 .4em; }
  .d5p-cut-eq { font-size: 1.15em; margin: .2em 0 .2em; }
  @media (max-width: 640px) { .d5p-cut-two { grid-template-columns: 1fr; } }
  .d5p-cut-col { padding-left: .8em; border-left: 3px solid var(--c); }
  .d5p-cut-rule { font-family: var(--upwr-mono); font-size: .8em; color: var(--upwr-ink-subtle); }
  .d5p-cut-val { font-family: var(--upwr-mono); font-weight: 700; font-size: 1.2em; color: var(--c); margin: .1em 0 .3em; }
  .d5p-cut-verdict { font-weight: 600; margin: .2em 0 .6em; }
"))

# Taśma 80 kontroli (rysowana także za trzecią wadą), cięcie za n-tą kratką.
# Nad taśmą klamra X (do trzeciej wady), pod taśmą klamra Y_n (pierwsze n kontroli);
# wady w pierwszych n kontrolach obrysowane kolorem Y.
d5p_cut_svg <- function(outcomes, n) {
  W <- 780; x0 <- 30; p <- 9; s <- 7; y <- 78
  x_of <- function(t) x0 + t * p
  len <- length(outcomes)
  X <- which(cumsum(outcomes) >= 3)[1]
  Y <- sum(outcomes[seq_len(n)])
  cells <- d5p_tape(outcomes, x0, y, pitch = p, size = s, dim = seq_len(len) > n)
  bracket <- function(i1, i2, yb, dir, col) sprintf(
    '<path d="M %g %g V %g H %g V %g" fill="none" style="stroke:%s" stroke-width="1.8"/>',
    x_of(i1 - 1), yb + 5 * dir, yb, x_of(i2) - 2, yb + 5 * dir, col)
  # X: klamra nad taśmą od pierwszej kontroli do trzeciej wady
  xend <- if (is.na(X)) len else X
  xpart <- paste0(
    bracket(1, xend, y - 10, 1, d5p_cut_col_x),
    if (!is.na(X)) sprintf('<rect x="%g" y="%g" width="%g" height="%g" rx="2" style="fill:none;stroke:%s;stroke-width:2.2"/>',
      x0 + (X - 1) * p - 1.5, y - 1.5, s + 3, s + 3, d5p_cut_col_x) else "",
    sprintf('<text class="d5p-mono" x="%g" y="%g" font-size="13" font-weight="700" style="fill:%s">%s</text>',
      x_of(0), y - 18, d5p_cut_col_x,
      if (is.na(X)) "X > 80: trzecia wada poza taśmą" else sprintf("X = %d: kontrola z trzecią wadą", X)))
  # Y_n: klamra pod taśmą od pierwszej kontroli do cięcia; wady w niej obrysowane
  ydef <- which(outcomes[seq_len(n)])
  ypart <- paste0(
    bracket(1, n, y + s + 10, -1, d5p_cut_col_y),
    paste(sprintf('<circle cx="%g" cy="%g" r="2.6" style="fill:%s"/>', x0 + (ydef - 1) * p + s / 2, y + s + 4, d5p_cut_col_y), collapse = ""),
    sprintf('<text class="d5p-mono" x="%g" y="%g" font-size="13" font-weight="700" style="fill:%s">Y%s = %d: wady w pierwszych %d kontrolach</text>',
      x_of(0), y + s + 28, d5p_cut_col_y, d5p_sub(n), Y, n))
  cut_x <- x_of(n) - 1
  cut <- paste0(
    sprintf('<line class="d5p-limit" x1="%g" x2="%g" y1="%g" y2="%g"/>', cut_x, cut_x, y - 6, y + s + 14),
    sprintf('<text class="d5p-mono" x="%g" y="%g" font-size="12" text-anchor="start">n = %d</text>', cut_x + 4, y - 38, n))
  d5p_svg(W, 178, sprintf("Taśma %d kontroli: klamra X do trzeciej wady nad taśmą, klamra pierwszych %d kontroli pod taśmą.", len, n),
    cells, xpart, ypart, cut, d5p_axis(x_of, y + s + 42, c(0, 20, 40, 60, 80), label = "numer kontroli"))
}

# Indeks dolny cyframi Unicode (Y₃₀).
d5p_sub <- function(n) chartr("0123456789", "₀₁₂₃₄₅₆₇₈₉", as.character(n))

# Dwa odczyty obok siebie (ta sama seria w dwóch językach) i jedna linijka
# P(X ≤ n) = … = P(Y_n ≥ 3): wspólny wynik obu reguł.
d5p_cut_read <- function(outcomes, n) {
  X <- which(cumsum(outcomes) >= 3)[1]
  Y <- sum(outcomes[seq_len(n)])
  okx <- !is.na(X) && X <= n
  prob <- pnbinom(n - 3, 3, 0.1)
  sx <- function(t) sprintf("\\style{color:%s}{%s}", d5p_cut_col_x, t)
  sy <- function(t) sprintf("\\style{color:%s}{%s}", d5p_cut_col_y, t)
  px <- sx(sprintf("P(X\\le %d)", n)); py <- sy(sprintf("P(Y_{%d}\\ge 3)", n))
  col <- function(color, rule, val) tags$div(class = "d5p-cut-col", style = paste0("--c:", color),
    tags$div(class = "d5p-cut-rule", rule),
    tags$div(class = "d5p-cut-val", val))
  withMathJax(tagList(
    tags$div(class = "d5p-cut-two",
      col(d5p_cut_col_x, "Reguła „do trzeciej wady”: liczymy kontrole",
        if (is.na(X)) sprintf("X > 80 > %d", n) else sprintf("X = %d %s %d", X, if (okx) "≤" else ">", n)),
      col(d5p_cut_col_y, sprintf("Reguła „%d prób”: liczymy wady", n),
        sprintf("Y%s = %d %s 3", d5p_sub(n), Y, if (Y >= 3) "≥" else "<"))),
    tags$div(class = "d5p-cut-eq", d5p_math(sprintf("%s=%.3f=%s", px, prob, py))),
    tags$p(class = "d5p-cut-verdict",
      style = paste0("color:", if (okx) "var(--upwr-ink)" else "var(--upwr-accent)"),
      if (okx) sprintf("Ta seria: audyt mieści się w limicie %d kontroli ✓", n)
      else sprintf("Ta seria: przy limicie %d kontroli zabraknie ×", n))
  ))
}

d5p_cut_ui <- d5p_panel("Pojęcie", "Taśma przecięta na n",
  d5p_css, d5p_cut_css,
  uiOutput("d5p_cut_head"),
  uiOutput("d5p_cut_stage"),
  lc_slider("d5p_cut_n", "Cięcie po kontroli n", 5, 80, 30, 1),
  uiOutput("d5p_cut_read"),
  tags$p(class = "d5p-caption",
    "Dwa opisy tego samego zdarzenia: trzecia wada pojawia się do n-tej kontroli włącznie, czyli w pierwszych n kontrolach są co najmniej trzy wady. Lewy odczyt liczy kontrole do trzeciej wady, prawy liczy wady w pierwszych n kontrolach; dlatego oba prawdopodobieństwa są równe."),
  tags$div(class = "d5p-nav",
    lc_action("d5p_cut_new", icon = "shuffle", variant = "ghost", aria_label = "Inna taśma")))

# ============================================================================
# SERWER
# ============================================================================

d5p_oddzialy_server <- function(input, output, session) {
  # (P8) stan: xs — X każdego audytu; tapes — taśmy pierwszych audytów (do 20)
  br <- reactiveVal(list(xs = numeric(0), tapes = list()))

  br_add <- function(m) {
    s <- br()
    room <- d5p_br_rows - length(s$xs)
    if (room <= 0) return(invisible(NULL))
    for (i in seq_len(min(m, room))) {
      tape <- d5p_br_seed_tapes[[length(s$xs) + 1L]]
      s$tapes[[length(s$tapes) + 1L]] <- tape
      s$xs <- c(s$xs, length(tape))
    }
    br(s)
  }
  observeEvent(input$d5p_br_one, br_add(1L))
  observeEvent(input$d5p_br_all, br_add(d5p_br_rows))
  observeEvent(input$d5p_br_sez, {
    s <- br()
    s$xs <- c(s$xs, 3 + rnbinom(2000, 3, 0.1))
    br(s)
  })
  observeEvent(input$d5p_br_reset, br(list(xs = numeric(0), tapes = list())))

  output$d5p_br_head <- renderUI({
    n <- length(br()$xs)
    d5p_head(sprintf("Budżet: %d kontroli na oddział", d5p_br_budget), sprintf("Audytów: %d", n))
  })
  output$d5p_br_stage <- renderUI({
    s <- br()
    d5p_branches_svg(s$xs, s$tapes)
  })
  output$d5p_br_read <- renderUI(d5p_branches_read(br()$xs))
  output$d5p_br_caption <- renderUI(d5p_branches_caption(br()$xs))

  # (P9) stan: taśma 80 kontroli; cięcie z suwaka
  cut_tape <- reactiveVal(d5p_cut_start)
  observeEvent(input$d5p_cut_new, cut_tape(runif(d5p_cut_len) < 0.1))
  cut_n <- reactive(as.integer(d5p_or(input$d5p_cut_n, 30L)))

  output$d5p_cut_head <- renderUI({
    d5p_head("Taśma kontroli", sprintf("Cięcie po kontroli %d", cut_n()))
  })
  output$d5p_cut_stage <- renderUI(d5p_cut_svg(cut_tape(), cut_n()))
  output$d5p_cut_read <- renderUI(d5p_cut_read(cut_tape(), cut_n()))
}
