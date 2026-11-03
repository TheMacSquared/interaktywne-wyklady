# PROTOTYPY (2026-11-03) — oddziały i taśma przecięta na n, wykład 05 ----------
#
# Prototypy rysowane po stronie serwera (SVG z R + renderUI), jak w stop_proto_sceny.R.
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

# Wiersze taśm (n <= 20) albo histogram końców X (n > 20) na tej samej osi 0–120.
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
    H <- top + d5p_br_rows * rowh + 46
    axis_y <- top + d5p_br_rows * rowh + 6
    rows <- vapply(seq_len(d5p_br_rows), function(i) {
      y <- top + (i - 1) * rowh
      label <- sprintf('<text class="d5p-mono d5p-subtle" x="4" y="%g" font-size="10">O%d</text>', y + 9, i)
      if (i > n) return(paste0(label, d5p_tape(logical(0), x0, y, pitch = p, size = s, n_cells = budget)))
      tape <- tapes[[i]]
      X <- length(tape)
      ok <- X <= budget
      end_x <- if (X > 120) x_of(120) + 16 else x_of(X - 1) + s + 6
      paste0(label,
        d5p_tape(tape, x0, y, pitch = p, size = s, max_cells = 120),
        sprintf('<circle class="%s" cx="%g" cy="%g" r="3"/>', if (ok) "d5p-ok" else "d5p-bad", end_x, y + s / 2),
        sprintf('<text class="d5p-mono" style="fill:%s" x="%g" y="%g" font-size="11">%d</text>',
          if (ok) "var(--upwr-sage)" else "var(--upwr-accent)", end_x + 6, y + 9, X))
    }, character(1))
    band <- sprintf('<rect x="%g" y="%g" width="%g" height="%g" style="fill:var(--upwr-accent);fill-opacity:.07"/>',
      budget_x, top - 4, x_of(120) - budget_x, d5p_br_rows * rowh + 4)
    line <- sprintf('<line class="d5p-limit" x1="%g" x2="%g" y1="%g" y2="%g"/>',
      budget_x, budget_x, top - 8, top + d5p_br_rows * rowh)
    body <- paste0(band, paste(rows, collapse = ""), line,
      sprintf('<text class="d5p-subtle" x="%g" y="%g" font-size="12">budżet %d</text>', budget_x + 6, top - 12, budget),
      d5p_axis(x_of, axis_y, c(0, 30, 60, 90, 120), label = "kontrola nr"))
    return(d5p_svg(W, H, aria, body))
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
    lc_action("d5p_br_reset", "Od nowa", icon = "reset", variant = "ghost")))

# ============================================================================
# (P9) TAŚMA PRZECIĘTA NA n — wzór (5.7): P(X ≤ n) = P(Y_n ≥ r)
# ============================================================================

# Taśma 80 kontroli (rysowana także za trzecią wadą), cięcie za n-tą kratką.
d5p_cut_svg <- function(outcomes, n) {
  W <- 780; x0 <- 30; p <- 9; s <- 7; y <- 62
  x_of <- function(t) x0 + t * p
  len <- length(outcomes)
  X <- which(cumsum(outcomes) >= 3)[1]
  cells <- d5p_tape(outcomes, x0, y, pitch = p, size = s, dim = seq_len(len) > n)
  mark <- if (!is.na(X)) paste0(
    sprintf('<rect x="%g" y="%g" width="%g" height="%g" rx="2" style="fill:none;stroke:var(--upwr-ink);stroke-width:2"/>',
      x0 + (X - 1) * p - 1, y - 1, s + 2, s + 2),
    sprintf('<text class="d5p-mono" x="%g" y="%g" font-size="11" text-anchor="middle">X</text>',
      x0 + (X - 1) * p + s / 2, y - 6)) else ""
  cut_x <- x_of(n) - 1
  right <- n > 60
  cut <- paste0(
    sprintf('<line class="d5p-limit" x1="%g" x2="%g" y1="%g" y2="%g"/>', cut_x, cut_x, y - 14, y + s + 6),
    sprintf('<text class="d5p-mono" x="%g" y="%g" font-size="12" text-anchor="%s">n = %d</text>',
      if (right) cut_x - 4 else cut_x + 4, y - 18, if (right) "end" else "start", n))
  d5p_svg(W, 130, sprintf("Taśma %d kontroli z trzecią wadą; cięcie po kontroli %d.", len, n),
    cells, mark, cut, d5p_axis(x_of, y + s + 14, c(0, 20, 40, 60, 80), label = "numer kontroli"))
}

# Dwa odczyty obok siebie; oba mają zawsze tę samą wartość logiczną.
d5p_cut_read <- function(outcomes, n) {
  X <- which(cumsum(outcomes) >= 3)[1]
  Y <- sum(outcomes[seq_len(n)])
  ok1 <- !is.na(X) && X <= n
  ok2 <- Y >= 3
  col <- function(ok) paste0("color:", if (ok) "var(--upwr-sage)" else "var(--upwr-accent)")
  tags$div(class = "d5p-nav d5p-read", style = "gap: 1.2em",
    tags$span(style = col(ok1), sprintf("%s %s n = %d",
      if (is.na(X)) "X > 80" else sprintf("X = %d", X), if (ok1) "≤" else ">", n)),
    tags$span(class = "d5p-subtle", "⇔"),
    tags$span(style = col(ok2), sprintf("w pierwszych %d kontrolach: Y = %d %s 3",
      n, Y, if (ok2) "≥" else "<")))
}

d5p_cut_ui <- d5p_panel("Pojęcie", "Taśma przecięta na n",
  d5p_css,
  uiOutput("d5p_cut_head"),
  uiOutput("d5p_cut_stage"),
  lc_slider("d5p_cut_n", "Cięcie po kontroli n", 5, 80, 30, 1),
  uiOutput("d5p_cut_read"),
  tags$p(class = "d5p-caption",
    "Dwa opisy tego samego zdarzenia: trzecia wada pojawia się do n-tej kontroli włącznie, czyli w pierwszych n kontrolach są co najmniej trzy wady. Przesuń cięcie i sprawdź, że oba odczyty zawsze się zgadzają."),
  tags$div(class = "d5p-nav",
    lc_action("d5p_cut_new", "Inna taśma", variant = "solid")))

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
    d5p_head("Dwadzieścia oddziałów", sprintf("Audytów: %d", n))
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
