# PROTOTYPY PUNKTÓW STOP — rozdział 2 (2026-10-30) ---------------------------
#   s4p_2a_ui — Ryc. 2.1 pojęcie „Odwrócenie: od μ do x̄ i z powrotem” (JS: s4p_ch2.js;
#     zaakceptowany 2026-10-30, zastąpił statyczną Ryc. 2.1)
#     Puenta: x̄ leży w pasie μ ± 1.96·SE dokładnie wtedy, gdy przedział x̄ ± 1.96·SE łapie μ.
#   s4p_2b_ui — Galeria „Te same grupki, trzy poziomy”, po akapicie o cenie pewności
#     Puenta: z tych samych danych 99% daje dłuższe siatki, które rzadziej chybiają, a 80% krótsze, które chybiają często.
#   s4p_2c_ui — Piaskownica „Suwak ufności”, zaraz po 2b
#     Puenta: podnosząc poziom ufności, wydłużasz wszystkie siatki naraz; chybionych ubywa, ale nic nie jest za darmo.
#   s4p_2d_ui — Piaskownica „Przedział dla μ to nie zakres ludzi”, po zdaniu B z quizu
#     Puenta: pas, w którym mieści się 95% ludzi, stoi w miejscu, a przedział ufności dla μ kurczy się wraz z liczbą zmierzonych osób.
# Serwer: s4p_ch2_server(input, output, session).

# Świat wykładu: wzrost N(170, 10) cm, grupka n = 25.
s4p_2_mu    <- 170
s4p_2_sigma <- 10
s4p_2_n     <- 25L

# k grupek po n osób z N(mu, sigma): x̄ i s każdej grupki.
s4p_2_draw <- function(k, n = s4p_2_n, mu = s4p_2_mu, sigma = s4p_2_sigma) {
  if (k <= 0) return(data.frame(xbar = numeric(0), s = numeric(0)))
  x <- matrix(rnorm(k * n, mu, sigma), nrow = n)
  data.frame(xbar = colMeans(x), s = apply(x, 2, sd))
}

# Czy przedział x̄ ± me łapie μ.
s4p_2_hit <- function(xbar, me, mu = s4p_2_mu) xbar - me <= mu & mu <= xbar + me

# ============================================================================
# (2a) POJĘCIE „PRZECIĄGNIJ x̄” — tylko UI, rysunek i logika w s4p_ch2.js
# ============================================================================

s4p_2a_ui <- s4p_panel("2a", "Ryc. 2.1", "Odwrócenie: od μ do x̄ i z powrotem", width_mode = "text",
  proto = FALSE,
  tags$style(HTML("
    .s4p-flip-stage svg { display: block; width: 100%; height: auto; touch-action: none;
      user-select: none; -webkit-user-select: none; cursor: pointer; }
    .s4p-flip-handle { cursor: grab; outline: none; }
    .s4p-flip-handle:active { cursor: grabbing; }
    .s4p-flip-handle:focus-visible .s4p-flip-hit { fill: none; stroke: var(--upwr-accent); stroke-width: 2; }
    .s4p-flip-read { margin-top: 0.6rem; font-family: var(--upwr-mono); font-size: 0.95em;
      color: var(--upwr-ink); line-height: 1.5; }
    .s4p-flip-verdict { font-weight: 400; }
    .s4p-flip-verdict[data-hit='yes'] { color: var(--upwr-cat-szalwia); }
    .s4p-flip-verdict[data-hit='no'] { color: var(--upwr-cat-terakota); }
  ")),
  tags$div(class = "s4p-flip", `data-mu` = "170", `data-se` = "2", `data-x` = "172.4",
    `data-min` = "163", `data-max` = "177",
    tags$div(class = "s4p-flip-stage"),
    tags$div(class = "s4p-flip-read", `aria-live` = "polite")
  ),
  lc_caption("x̄ w pasie μ ± 1.96·SE ⇔ μ w przedziale x̄ ± 1.96·SE.")
)

# ============================================================================
# (2b) GALERIA „TE SAME GRUPKI, TRZY POZIOMY”
# ============================================================================

s4p_2b_levels <- c("80%" = 0.80, "95%" = 0.95, "99%" = 0.99)
s4p_2b_tstar  <- c("80%" = qt(0.90, s4p_2_n - 1),
                   "95%" = qt(0.975, s4p_2_n - 1),
                   "99%" = qt(0.995, s4p_2_n - 1))

# Trzy przedziały z każdej grupki (ta sama grupka, ten sam s).
s4p_2b_intervals <- function(g) {
  if (!nrow(g)) {
    return(data.frame(level = character(0), idx = integer(0), lo = numeric(0),
                      hi = numeric(0), hit = logical(0)))
  }
  do.call(rbind, lapply(names(s4p_2b_tstar), function(lv) {
    me <- s4p_2b_tstar[[lv]] * g$s / sqrt(s4p_2_n)
    data.frame(level = lv, idx = seq_len(nrow(g)),
               lo = g$xbar - me, hi = g$xbar + me,
               hit = s4p_2_hit(g$xbar, me))
  }))
}

# Pokrycie na poziom: udział trafień we wszystkich grupkach.
s4p_2b_coverage <- function(g) {
  iv <- s4p_2b_intervals(g)
  vapply(names(s4p_2b_tstar), function(lv) {
    if (!nrow(g)) NA_real_ else mean(iv$hit[iv$level == lv])
  }, numeric(1))
}

# Wykres: w każdej kolumnie ostatnie 40 przedziałów, najnowszy na górze.
s4p_2b_plot_fn <- function(g) {
  iv <- s4p_2b_intervals(g)
  lv_levels <- names(s4p_2b_tstar)
  recent <- data.frame(level = character(0), lo = numeric(0), hi = numeric(0),
                       y = integer(0), outcome = character(0))
  if (nrow(iv)) {
    recent <- do.call(rbind, lapply(lv_levels, function(lv) {
      d <- iv[iv$level == lv, , drop = FALSE]
      d <- utils::tail(d, 40)
      data.frame(level = lv, lo = d$lo, hi = d$hi,
                 y = seq_len(nrow(d)),
                 outcome = ifelse(d$hit, "hit", "miss"))
    }))
  }
  recent$level <- factor(recent$level, levels = lv_levels)
  ref <- data.frame(level = factor(lv_levels, levels = lv_levels), x = s4p_2_mu)

  ggplot() +
    geom_vline(data = ref, aes(xintercept = x), linetype = "22",
               colour = upwr_secondary, linewidth = 0.6) +
    geom_segment(data = recent,
                 aes(x = lo, xend = hi, y = y, yend = y, colour = outcome),
                 linewidth = 0.9) +
    facet_wrap(~level, nrow = 1) +
    scale_colour_manual(values = c(hit = col_hit, miss = col_miss), guide = "none") +
    coord_cartesian(xlim = c(160, 180), ylim = c(0, 41), expand = FALSE) +
    scale_x_continuous(breaks = c(165, 170, 175)) +
    labs(x = "Wzrost (cm)", y = NULL) +
    theme_upwr(grid = "x") +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          panel.grid.major.y = element_blank())
}

s4p_2b_ui <- s4p_panel("2b", "Galeria", "Te same grupki, trzy poziomy", width_mode = "wide",
  lc_toolbar(
    lc_action_group(label = "Grupki", s4p_2b_add1 = "+1", s4p_2b_add20 = "+20", s4p_2b_add100 = "+100"),
    lc_action("s4p_2b_reset", icon = "reset", variant = "ghost", aria_label = "Od nowa"),
    lc_readouts(uiOutput("s4p_2b_read"))
  ),
  lc_plot("s4p_2b_plot", ratio = "2.2/1"),
  lc_caption("Ostatnie 40 przedziałów w każdej kolumnie. Szałwia łapie μ, terakota chybia.")
)

# ============================================================================
# (2c) PIASKOWNICA „SUWAK UFNOŚCI”
# ============================================================================

s4p_2c_ui <- s4p_panel("2c", "Piaskownica", "Suwak ufności", width_mode = "wide",
  lc_toolbar(
    lc_slider("s4p_2c_level", "Poziom ufności", min = 0.5, max = 0.995, value = 0.95,
              step = 0.005, digits = 3),
    lc_action("s4p_2c_new", label = "Nowe 50 grupek", icon = "shuffle", variant = "outline"),
    lc_readouts(uiOutput("s4p_2c_read"))
  ),
  lc_plot("s4p_2c_plot", ratio = "1.6/1", max_height = "520px")
)

# 50 przedziałów pod sobą; kolor według trafienia, środek x̄ jako kropka.
s4p_2c_plot_fn <- function(g, level) {
  tq <- qt((1 + level) / 2, s4p_2_n - 1)
  me <- tq * g$s / sqrt(s4p_2_n)
  d <- data.frame(y = seq_len(nrow(g)), lo = g$xbar - me, hi = g$xbar + me,
                  xbar = g$xbar, outcome = ifelse(s4p_2_hit(g$xbar, me), "hit", "miss"))
  ref <- data.frame(x = s4p_2_mu)

  ggplot(d) +
    geom_vline(data = ref, aes(xintercept = x), linetype = "22",
               colour = upwr_secondary, linewidth = 0.6) +
    geom_segment(aes(x = lo, xend = hi, y = y, yend = y, colour = outcome), linewidth = 0.8) +
    geom_point(aes(x = xbar, y = y), colour = col_estimate, size = 1.1) +
    scale_colour_manual(values = c(hit = col_hit, miss = col_miss), guide = "none") +
    coord_cartesian(xlim = c(155, 185), ylim = c(0.5, nrow(g) + 0.5), expand = FALSE) +
    labs(x = "Wzrost (cm)", y = NULL) +
    theme_upwr(grid = "x") +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          panel.grid.major.y = element_blank())
}

# ============================================================================
# (2d) PIASKOWNICA „PRZEDZIAŁ DLA μ TO NIE ZAKRES LUDZI”
# ============================================================================

s4p_2d_seed <- 4300L
s4p_2d_band_me <- 1.96 * s4p_2_sigma    # pas 95% ludzi: μ ± 19.6 cm

s4p_2d_ui <- s4p_panel("2d", "Piaskownica", "Przedział dla μ to nie zakres ludzi", width_mode = "wide",
  lc_toolbar(
    lc_slider("s4p_2d_logn", "log10(n)", min = 0.7, max = 3, value = 1.4, step = 0.01, digits = 2),
    lc_readouts(uiOutput("s4p_2d_read"))
  ),
  lc_plot("s4p_2d_pop", ratio = "3.2/1"),
  lc_plot("s4p_2d_ci", ratio = "3.2/0.45")
)

# Góra: gęstość wzrostu N(170, 10) z pasem 95% ludzi.
s4p_2d_pop_fn <- function() {
  x <- seq(130, 210, length.out = 500)
  d <- data.frame(x = x, y = dnorm(x, s4p_2_mu, s4p_2_sigma))
  band <- d[d$x >= s4p_2_mu - s4p_2d_band_me & d$x <= s4p_2_mu + s4p_2d_band_me, , drop = FALSE]
  peak <- dnorm(s4p_2_mu, s4p_2_mu, s4p_2_sigma)
  ref <- data.frame(x = s4p_2_mu)

  ggplot(d, aes(x, y)) +
    geom_area(data = band, aes(x, y), fill = upwr_reference, alpha = 0.35) +
    geom_line(colour = upwr_secondary, linewidth = 0.9) +
    geom_vline(data = ref, aes(xintercept = x), linetype = "22",
               colour = upwr_secondary, linewidth = 0.6) +
    annotate("text", x = 179, y = peak * 0.25, hjust = 0, colour = upwr_reference,
             fontface = "bold", label = "95% ludzi") +
    coord_cartesian(xlim = c(130, 210), ylim = c(0, peak * 1.08), expand = FALSE) +
    labs(x = "Wzrost (cm)", y = NULL) +
    theme_upwr(grid = "x") +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          panel.grid.major.y = element_blank())
}

# Liczba osób jako n = 10^(log10 n), rozwinięta do liczby całkowitej.
s4p_2d_n <- function(logn) round(10^logn)

# Jedna próba o rozmiarze n; seed zależy od n, więc obraz jest stabilny.
s4p_2d_sample <- function(n) {
  set.seed(s4p_2d_seed + n)
  x <- rnorm(n, s4p_2_mu, s4p_2_sigma)
  s <- sd(x)
  list(n = n, xbar = mean(x), s = s, me = qt(0.975, n - 1) * s / sqrt(n))
}

# Dół: 95% przedział ufności dla μ z tej próby, na tej samej osi.
s4p_2d_ci_fn <- function(smp) {
  ci <- data.frame(y = 1, lo = smp$xbar - smp$me, hi = smp$xbar + smp$me, xbar = smp$xbar)
  ref <- data.frame(x = s4p_2_mu)

  ggplot() +
    geom_vline(data = ref, aes(xintercept = x), linetype = "22",
               colour = upwr_secondary, linewidth = 0.6) +
    geom_segment(data = ci, aes(x = lo, xend = hi, y = y, yend = y),
                 colour = col_ci, linewidth = 3) +
    geom_point(data = ci, aes(x = xbar, y = y), colour = col_estimate, size = 4) +
    coord_cartesian(xlim = c(130, 210), ylim = c(0.5, 1.5), expand = FALSE) +
    labs(x = "Wzrost (cm)", y = NULL) +
    theme_upwr(grid = "x") +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          panel.grid.major.y = element_blank())
}

# ============================================================================
# SERWER
# ============================================================================

s4p_ch2_server <- function(input, output, session) {

  # --- 2b -------------------------------------------------------------------
  s4p_2b_groups <- reactiveVal(s4p_2_draw(0))
  add_2b <- function(k) s4p_2b_groups(rbind(s4p_2b_groups(), s4p_2_draw(k)))
  observeEvent(input$s4p_2b_add1, add_2b(1))
  observeEvent(input$s4p_2b_add20, add_2b(20))
  observeEvent(input$s4p_2b_add100, add_2b(100))
  observeEvent(input$s4p_2b_reset, s4p_2b_groups(s4p_2_draw(0)))

  output$s4p_2b_read <- renderUI({
    g <- s4p_2b_groups()
    cov <- s4p_2b_coverage(g)
    tagList(
      lc_readout("Grupek", lc_fmt(nrow(g), 0, " ")),
      lapply(names(s4p_2b_levels), function(lv) {
        lc_readout(paste("pokrycie", lv),
                   if (nrow(g)) s4p_pct(cov[[lv]], 1) else "–",
                   color = upwr_accent)
      })
    )
  })

  zoom_plot_server("s4p_2b_plot", reactive(s4p_2b_plot_fn(s4p_2b_groups())),
    alt = "Trzy kolumny po 40 ostatnich przedziałów ufności z tych samych grupek: 80%, 95% i 99%. Przedziały trafiające w przerywaną linię μ = 170 cm są szałwiowe, chybiające terakotowe; szersze przedziały chybiają rzadziej")

  # --- 2c -------------------------------------------------------------------
  s4p_2c_seed <- reactiveVal(2402L)
  s4p_2c_groups <- reactive({
    set.seed(s4p_2c_seed())
    s4p_2_draw(50L)
  })
  observeEvent(input$s4p_2c_new, s4p_2c_seed(s4p_2c_seed() + 1L))

  s4p_2c_level <- reactive(input$s4p_2c_level %||% 0.95)

  output$s4p_2c_read <- renderUI({
    g <- s4p_2c_groups()
    lv <- s4p_2c_level()
    tq <- qt((1 + lv) / 2, s4p_2_n - 1)
    me <- tq * g$s / sqrt(s4p_2_n)
    miss <- sum(!s4p_2_hit(g$xbar, me))
    tagList(
      lc_readout("t*", lc_fmt(tq, 3)),
      lc_readout("chybionych z 50", lc_fmt(miss, 0)),
      lc_readout("średnia szerokość", paste(lc_fmt(mean(2 * me), 1), "cm"), color = upwr_accent)
    )
  })

  zoom_plot_server("s4p_2c_plot", reactive(s4p_2c_plot_fn(s4p_2c_groups(), s4p_2c_level())),
    alt = "Pięćdziesiąt przedziałów ufności pod sobą dla tych samych grupek; przy wybranym poziomie część przedziałów obejmuje przerywaną linię μ = 170 cm, a chybione są oznaczone terakotą")

  # --- 2d -------------------------------------------------------------------
  s4p_2d_smp <- reactive(s4p_2d_sample(s4p_2d_n(input$s4p_2d_logn %||% 1.4)))

  output$s4p_2d_read <- renderUI({
    smp <- s4p_2d_smp()
    tagList(
      lc_readout("n", lc_fmt(smp$n, 0, " ")),
      lc_readout("pas ludzi", paste(lc_fmt(2 * s4p_2d_band_me, 1), "cm")),
      lc_readout("przedział dla μ", paste(lc_fmt(2 * smp$me, 1), "cm"), color = upwr_accent)
    )
  })

  zoom_plot_server("s4p_2d_pop", reactive(s4p_2d_pop_fn()),
    alt = "Gęstość wzrostu N(170, 10) z zacieniowanym pasem 150.4–189.6 cm, w którym leży 95% ludzi")

  zoom_plot_server("s4p_2d_ci", reactive(s4p_2d_ci_fn(s4p_2d_smp())),
    alt = "Pojedynczy 95% przedział ufności dla średniej wzrostu na tej samej osi co pas ludzi; im więcej osób, tym jest węższy")
}
