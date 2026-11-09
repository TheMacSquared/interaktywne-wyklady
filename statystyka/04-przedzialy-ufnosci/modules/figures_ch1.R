# RYCINY — rozdział 1 -----------------------------------------------------------
#   fig_1b_ui — Ryc. 1.1 galeria „Tarcza: obciążenie i rozrzut”:
#               obciążenie i rozrzut to dwie niezależne wady (po wstępie do „Trzech własności”).
#   fig_1a_ui — Ryc. 1.2 symulacja „Cztery miarki”: trzy estymatory
#               nieobciążone różnią się rozrzutem, uszkodzona miarka jest przesunięta (po Efektywności).
#   fig_1c_ui — Ryc. 1.3 galeria „4× więcej osób, połowa rozrzutu”:
#               czterokrotnie większa grupka daje dwa razy węższy stos średnich (po Zasadzie).
#   fig_1d_ui — Ryc. 1.4 scena „Dwie pizzerie, obie 4.4 ★”: ta sama
#               średnia ocen dnia z 5 i z 200 opinii; sama liczba nie mówi, jak bardzo się chwieje
#               (w „Sam punkt nie wystarczy”).
# Serwer: fig_ch1_server(input, output, session).
# Świat wykładu: wzrost N(μ = 170, σ = 10) cm, grupka n = 25 (net_world, net_n z helpers.R).

# ============================================================================
# (1a) SYMULACJA „CZTERY MIARKI”
# ============================================================================

fig_1a_levels <- c("Średnia", "Mediana", "Środek rozstępu", "Uszkodzona miarka")
fig_1a_cols   <- c("mean_est", "median_est", "midrange_est", "biased_est")
fig_1a_fill   <- c("Średnia" = col_estimate, "Mediana" = col_estimate,
                   "Środek rozstępu" = col_estimate, "Uszkodzona miarka" = col_miss)
fig_1a_empty  <- data.frame(mean_est = numeric(0), median_est = numeric(0),
                            midrange_est = numeric(0), biased_est = numeric(0))

fig_1a_ui <- fig_panel("1a", "Ryc. 1.2", "Cztery miarki",
  tags$style(HTML("
    .fig-tab { border-collapse: collapse; margin-top: .6rem; font-size: .92em; }
    .fig-tab th, .fig-tab td { padding: .25rem .7rem; text-align: right; border-bottom: 1px solid var(--upwr-rule); }
    .fig-tab th:first-child, .fig-tab td:first-child { text-align: left; }
  ")),
  lc_toolbar(
    lc_action_group(label = "Grupki", fig_1a_add1 = "+1", fig_1a_add100 = "+100", fig_1a_add1000 = "+1000"),
    lc_action("fig_1a_reset", icon = "reset", variant = "ghost", aria_label = "Od nowa"),
    lc_readouts(uiOutput("fig_1a_read"))
  ),
  lc_ec_plot_ui("fig_1a_plot", ratio = "1.9/1"),
  uiOutput("fig_1a_table"),
  width_mode = "wide"
)

# Jedna grupka = 25 wzrostów z N(170, 10); z każdej cztery estymaty. Wiersze: grupki.
fig_1a_simulate <- function(k) {
  x <- matrix(rnorm(k * net_n, net_world$mu, net_world$sigma), nrow = k)
  data.frame(
    mean_est     = rowMeans(x),
    median_est   = apply(x, 1, stats::median),
    midrange_est = (apply(x, 1, min) + apply(x, 1, max)) / 2,
    biased_est   = rowMeans(x + 2)   # „uszkodzona miarka”: brakuje 2 cm początku taśmy, każdy pomiar o 2 cm za duży
  )
}

fig_1a_long <- function(groups) {
  data.frame(
    est   = factor(rep(fig_1a_levels, each = nrow(groups)), levels = fig_1a_levels),
    value = unlist(groups[fig_1a_cols], use.names = FALSE)
  )
}

fig_1a_plot_fn <- function(groups) {
  ggplot(fig_1a_long(groups), aes(value, fill = est)) +
    geom_blank(data = data.frame(value = net_world$mu,
                                 est = factor(fig_1a_levels, levels = fig_1a_levels))) +
    geom_histogram(binwidth = 0.5, boundary = 0, colour = "white") +
    geom_vline(xintercept = net_world$mu, linetype = "22", colour = upwr_secondary, linewidth = 0.8) +
    facet_wrap(~est, ncol = 2) +
    scale_fill_manual(values = fig_1a_fill, guide = "none") +
    coord_cartesian(xlim = c(155, 185)) +
    labs(x = "Estymata wzrostu (cm)", y = "Liczba grupek") +
    theme_upwr(grid = "y")
}

fig_1a_table <- function(groups) {
  rows <- lapply(seq_along(fig_1a_cols), function(i) {
    v <- groups[[fig_1a_cols[i]]]
    tags$tr(
      tags$td(fig_1a_levels[i]),
      tags$td(if (length(v)) sprintf("%.1f", mean(v)) else "–"),
      tags$td(if (length(v) > 1) sprintf("%.1f", sd(v)) else "–")
    )
  })
  tags$table(class = "fig-tab", `aria-label` = "Średnia i odchylenie estymat dla czterech miarek",
    tags$thead(tags$tr(tags$th("Estymator"), tags$th("Średnia estymat, cm"), tags$th("SD estymat, cm"))),
    tags$tbody(rows)
  )
}

# ============================================================================
# (1b) GALERIA „TARCZA: OBCIĄŻENIE I ROZRZUT”
# ============================================================================

fig_1b_panels <- c("Nieobciążony, mały rozrzut", "Nieobciążony, duży rozrzut",
                   "Obciążony, mały rozrzut", "Obciążony, duży rozrzut")

# 20 strzałów na tarczę. Środek tarczy = θ; obciążony estymator strzela przesunięty o 1.5.
fig_1b_shots <- local({
  set.seed(4102)
  specs <- data.frame(panel = factor(fig_1b_panels, levels = fig_1b_panels),
                      bias = c(0, 0, 1.5, 1.5), spread = c(0.6, 1.4, 0.6, 1.4))
  do.call(rbind, lapply(seq_len(nrow(specs)), function(i) {
    data.frame(panel = specs$panel[i],
               x = specs$bias[i] + rnorm(20, sd = specs$spread[i]),
               y = rnorm(20, sd = specs$spread[i]))
  }))
})

# Średnia chmury tylko dla obciążonych paneli (małym krzyżykiem).
fig_1b_biased_mean <- local({
  m <- aggregate(cbind(x, y) ~ panel, data = fig_1b_shots, FUN = mean)
  m[m$panel %in% fig_1b_panels[3:4], ]
})

# Koncentryczne okręgi (promienie 1–4) w każdym panelu.
fig_1b_rings <- do.call(rbind, lapply(fig_1b_panels, function(p) {
  t <- seq(0, 2 * pi, length.out = 181)
  do.call(rbind, lapply(1:4, function(r) {
    data.frame(panel = factor(p, levels = fig_1b_panels), ring_id = paste(p, r),
               x = r * cos(t), y = r * sin(t))
  }))
}))

fig_1b_ui <- fig_panel("1b", "Ryc. 1.1", "Tarcza: obciążenie i rozrzut",
  lc_ec_plot_ui("fig_1b_plot", ratio = "1/1"),
  width_mode = "text"
)

fig_1b_plot_fn <- function() {
  ggplot() +
    geom_path(data = fig_1b_rings, aes(x, y, group = ring_id), colour = upwr_reference, linewidth = 0.4) +
    geom_point(data = data.frame(x = 0, y = 0), aes(x, y), shape = 16, size = 2.5, colour = col_true) +
    geom_point(data = fig_1b_shots, aes(x, y), colour = col_estimate, size = 2.4) +
    geom_point(data = fig_1b_biased_mean, aes(x, y), shape = 4, size = 3, stroke = 1, colour = upwr_ink) +
    facet_wrap(~panel, ncol = 2) +
    coord_equal(xlim = c(-4.2, 4.2), ylim = c(-4.2, 4.2)) +
    labs(x = NULL, y = NULL) +
    theme_upwr(grid = "none") +
    theme(axis.text = element_blank(), axis.ticks = element_blank(),
          axis.title = element_blank(), panel.border = element_rect(colour = upwr_rule, fill = NA))
}

# ============================================================================
# (1c) GALERIA „4× WIĘCEJ OSÓB, POŁOWA ROZRZUTU”
# ============================================================================

fig_1c_ns <- c(25L, 100L, 400L)

# 3000 średnich x̄ dla każdego n. Brak zmiennej losowej w UI: obraz jest statyczny.
fig_1c_draws <- local({
  set.seed(4103)
  do.call(rbind, lapply(fig_1c_ns, function(n) {
    data.frame(n = n, x = colMeans(matrix(rnorm(3000 * n, net_world$mu, net_world$sigma), nrow = n)))
  }))
})

fig_1c_sd <- vapply(fig_1c_ns, function(n) sd(fig_1c_draws$x[fig_1c_draws$n == n]), numeric(1))
fig_1c_labels <- sprintf("n = %d · SD(x̄) ≈ %.1f cm", fig_1c_ns, fig_1c_sd)

fig_1c_draws$panel <- factor(fig_1c_labels[match(fig_1c_draws$n, fig_1c_ns)], levels = fig_1c_labels)

# Klamry ±2·SD pod każdym histogramem (w jednostkach gęstości, poniżej zera).
fig_1c_brackets <- data.frame(panel = factor(fig_1c_labels, levels = fig_1c_labels),
                              lo = net_world$mu - 2 * fig_1c_sd,
                              hi = net_world$mu + 2 * fig_1c_sd)

fig_1c_ui <- fig_panel("1c", "Ryc. 1.3", "4× więcej osób, połowa rozrzutu",
  lc_ec_plot_ui("fig_1c_plot", ratio = "1.1/1"),
  width_mode = "text"
)

fig_1c_plot_fn <- function() {
  ggplot(fig_1c_draws, aes(x)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 0.2, boundary = 0,
                   fill = col_estimate, colour = "white") +
    geom_vline(xintercept = net_world$mu, linetype = "22", colour = upwr_secondary, linewidth = 0.8) +
    geom_segment(data = fig_1c_brackets, aes(x = lo, xend = hi, y = -0.06, yend = -0.06),
                 colour = col_ci, linewidth = 1.3, inherit.aes = FALSE) +
    geom_segment(data = fig_1c_brackets, aes(x = lo, xend = lo, y = -0.085, yend = -0.035),
                 colour = col_ci, linewidth = 1.3, inherit.aes = FALSE) +
    geom_segment(data = fig_1c_brackets, aes(x = hi, xend = hi, y = -0.085, yend = -0.035),
                 colour = col_ci, linewidth = 1.3, inherit.aes = FALSE) +
    facet_wrap(~panel, ncol = 1) +
    coord_cartesian(xlim = c(163, 177), ylim = c(-0.11, 0.95)) +
    scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8)) +
    labs(x = "Średnia z próby x̄ (cm)", y = "Gęstość") +
    theme_upwr(grid = "y")
}

# ============================================================================
# (1d) SCENA „DWIE PIZZERIE, OBIE 4.4 ★” (dwa światy obok siebie)
# ============================================================================

# Wczoraj obie pizzerie miały średnią ocen dnia 4.4 ★. U Gośki ocenia 5 klientów dziennie,
# w Napoli 200. „Kolejny dzień” stawia średnią ocen z następnego dnia. Obie pizzerie są
# naprawdę równie dobre (ten sam ukryty rozkład gwiazdek, średnio ok. 4.4 ★).
fig_1d_init  <- 4.4
fig_1d_rows  <- data.frame(who = c("U Gośki: 5 opinii dziennie", "Napoli: 200 opinii dziennie"),
                           n = c(5L, 200L), row_y = c(2, 1), stringsAsFactors = FALSE)
fig_1d_star_p <- c(0.03, 0.04, 0.08, 0.22, 0.63)   # 1–5 ★, średnio 4.38
fig_1d_empty <- data.frame(who = character(0), x = numeric(0), y = numeric(0), stringsAsFactors = FALSE)

fig_1d_ui <- fig_panel("1d", "Ryc. 1.4", "Dwie pizzerie, obie 4.4 ★",
  lc_toolbar(
    lc_action("fig_1d_rep1", "Kolejny dzień", variant = "solid"),
    lc_action("fig_1d_rep20", "+20 dni", variant = "outline"),
    lc_action("fig_1d_reset", icon = "reset", variant = "ghost", aria_label = "Od nowa"),
    lc_readouts(uiOutput("fig_1d_read"))
  ),
  lc_ec_plot_ui("fig_1d_plot", ratio = "3/1"),
  uiOutput("fig_1d_caption"),
  width_mode = "wide"
)

fig_1d_plot_fn <- function(reps) {
  init <- data.frame(x = fig_1d_init, y = fig_1d_rows$row_y, who = fig_1d_rows$who)
  ggplot() +
    geom_segment(data = fig_1d_rows, aes(x = 1, xend = 5, y = row_y, yend = row_y),
                 colour = upwr_rule, linewidth = 0.6) +
    geom_point(data = reps, aes(x, y), colour = col_estimate, alpha = 0.35, size = 2.2) +
    geom_point(data = init, aes(x, y), shape = 18, size = 6, colour = col_estimate) +
    geom_text(data = init, aes(x, y + 0.32, label = sprintf("%.1f ★", x)),
              colour = col_estimate, fontface = "bold") +
    scale_x_continuous(breaks = 1:5, labels = paste(1:5, "★")) +
    scale_y_continuous(breaks = fig_1d_rows$row_y, labels = fig_1d_rows$who, limits = c(0.4, 2.6),
                       expand = c(0, 0)) +
    coord_cartesian(xlim = c(1, 5.1)) +
    labs(x = "Średnia ocena", y = NULL) +
    theme_upwr(grid = "x")
}

# ============================================================================
# SERWER
# ============================================================================

fig_ch1_server <- function(input, output, session) {

  # --- (1a) Cztery miarki ----------------------------------------------------
  groups_1a <- reactiveVal(fig_1a_empty)
  add_groups_1a <- function(k) groups_1a(rbind(groups_1a(), fig_1a_simulate(k)))
  observeEvent(input$fig_1a_add1, add_groups_1a(1))
  observeEvent(input$fig_1a_add100, add_groups_1a(100))
  observeEvent(input$fig_1a_add1000, add_groups_1a(1000))
  observeEvent(input$fig_1a_reset, groups_1a(fig_1a_empty))

  output$fig_1a_read <- renderUI({
    lc_readout("Grupek", lc_fmt(nrow(groups_1a()), 0, " "), color = upwr_accent)
  })

  output$fig_1a_table <- renderUI(fig_1a_table(groups_1a()))

  lc_ec_plot_server("fig_1a_plot", reactive(fig_1a_plot_fn(groups_1a())),
    alt = "Cztery histogramy estymat z grupek po 25 osób: średnia, mediana, środek rozstępu i uszkodzona miarka, na wspólnej osi wzrostu, z przerywaną linią μ = 170 cm")

  # --- (1b) Tarcza (statyczna) -----------------------------------------------
  lc_ec_plot_server("fig_1b_plot", reactive(fig_1b_plot_fn()),
    alt = "Cztery tarcze z dwudziestoma strzałami: nieobciążony mały i duży rozrzut w górnym rzędzie, obciążony mały i duży rozrzut w dolnym; środek tarczy to parametr")

  # --- (1c) Cztery razy więcej osób (statyczna) ------------------------------
  lc_ec_plot_server("fig_1c_plot", reactive(fig_1c_plot_fn()),
    alt = "Trzy histogramy średnich z 3000 grupek dla n = 25, 100 i 400, pod sobą; węższy stos przy większej grupce, klamry ±2 SD pod każdym")

  # --- (1d) Dwie pizzerie ------------------------------------------------------
  state_1d <- reactiveVal(fig_1d_empty)

  add_reps_1d <- function(k) {
    new_rows <- do.call(rbind, lapply(seq_len(nrow(fig_1d_rows)), function(i) {
      data.frame(who = fig_1d_rows$who[i],
                 x = replicate(k, mean(sample(1:5, fig_1d_rows$n[i], replace = TRUE,
                                              prob = fig_1d_star_p))),
                 y = fig_1d_rows$row_y[i] + runif(k, -0.22, 0.22),
                 stringsAsFactors = FALSE)
    }))
    state_1d(rbind(state_1d(), new_rows))
  }
  observeEvent(input$fig_1d_rep1, add_reps_1d(1))
  observeEvent(input$fig_1d_rep20, add_reps_1d(20))
  observeEvent(input$fig_1d_reset, state_1d(fig_1d_empty))

  # Średnie ocen z kolejnych dni danej pizzerii (bez wczorajszej, pokazanej 4.4 ★).
  est_1d <- function(st, i) st$x[st$who == fig_1d_rows$who[i]]
  sd_1d <- function(st, i) {
    v <- c(fig_1d_init, est_1d(st, i))
    if (length(v) > 1) sd(v) else NA_real_
  }

  output$fig_1d_read <- renderUI({
    st <- state_1d()
    tagList(
      lc_readout("Dni", lc_fmt(sum(st$who == fig_1d_rows$who[1]), 0)),
      lc_readout("SD średnich U Gośki", if (is.na(sd_1d(st, 1))) "–" else sprintf("%.2f ★", sd_1d(st, 1)),
                 color = upwr_accent),
      lc_readout("SD średnich Napoli", if (is.na(sd_1d(st, 2))) "–" else sprintf("%.2f ★", sd_1d(st, 2)))
    )
  })

  output$fig_1d_caption <- renderUI({
    st <- state_1d()
    reps_n <- sum(st$who == fig_1d_rows$who[1])
    if (reps_n == 0)
      return(lc_caption("Wczoraj obie pizzerie miały średnią ocen 4.4 ★. Która średnia mówi więcej?"))
    lc_caption(sprintf("Po %d dniach średnia dnia U Gośki chwieje się o SD %.2f ★, a w Napoli o SD %.2f ★.",
                       reps_n, sd_1d(st, 1), sd_1d(st, 2)))
  })

  lc_ec_plot_server("fig_1d_plot", reactive(fig_1d_plot_fn(state_1d())),
    alt = "Dwa wiersze na osi ocen od 1 do 5 gwiazdek: pizzeria U Gośki z 5 opiniami dziennie i Napoli z 200, obie ze znacznikiem 4.4 i średnimi ocen z kolejnych dni")
}
