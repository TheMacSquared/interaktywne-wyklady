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
# Wszystkie cztery ryciny rysuje przeglądarka (sf_ch12.js, silnik lcSF): losowanie w 1.2 i 1.4
# liczy JS, stałe dane rycin 1.1 i 1.3 (z ustalonym ziarnem) liczy R i przekazuje w config.
# Świat wykładu: wzrost N(μ = 170, σ = 10) cm, grupka n = 25 (net_world, net_n z helpers.R).

# Wykresy panelowe (facet_wrap): siatka lc_sf_chart z nagłówkiem nad każdym wykresem.
fig_ch1_facets <- function(headings, ratio, cols = 2, max_height = "340px", class = NULL) {
  tags$div(class = paste(c(paste0("fig-facets fig-facets-", cols), class), collapse = " "),
    lapply(seq_along(headings), function(k)
      tags$div(tags$h4(class = "fig-facet-h", headings[[k]]), lc_sf_chart(k - 1, ratio, max_height))))
}

fig_ch1_styles <- tags$style(HTML("
  .fig-facets { display: grid; grid-template-columns: minmax(0, 1fr); gap: .4em 1.2em; }
  @media (min-width: 560px) { .fig-facets-2 { grid-template-columns: repeat(2, minmax(0, 1fr)); } }
  .fig-facet-h { margin: .5em 0 .1em; font-size: .9em; font-weight: 400; color: var(--upwr-ink); }
  .lc-sf[data-lc-chart-large='true'] .fig-facets-2 { grid-template-columns: minmax(0, 1fr); }
  /* Tarcze: kwadratowe wykresy (okręgi jak coord_equal), więc ograniczamy szerokość, nie wysokość. */
  .fig-facets.is-square { max-width: 36em; margin-inline: auto; }
  .fig-facets.is-square .lc-sf-chart { max-height: none !important; }
"))

# ============================================================================
# (1a) SYMULACJA „CZTERY MIARKI” (losuje przeglądarka)
# ============================================================================

fig_1a_levels <- c("Średnia", "Mediana", "Środek rozstępu", "Uszkodzona miarka")

fig_1a_ui <- fig_panel("1a", "Ryc. 1.2", "Cztery miarki",
  fig_ch1_styles,
  lc_sf_figure("d04-miarki",
    toolbar = tagList(
      lc_sf_action_group("Grupki", add1 = "+1", add100 = "+100", add1000 = "+1000"),
      lc_sf_action("reset", "fig_1a_reset", icon = "reset", variant = "ghost", aria_label = "Od nowa"),
      lc_readouts(lc_readout("Grupek", lc_sf_value("groups", "0"), color = upwr_accent))
    ),
    config = list(mu = net_world$mu, sigma = net_world$sigma, n = net_n),
    body = fig_ch1_facets(fig_1a_levels, ratio = "1.6/1"),
    after = tags$div(`data-role` = "table")
  ),
  width_mode = "wide"
)

# ============================================================================
# (1b) GALERIA „TARCZA: OBCIĄŻENIE I ROZRZUT” (stałe strzały z R)
# ============================================================================

fig_1b_panels <- c("Nieobciążony, mały rozrzut", "Nieobciążony, duży rozrzut",
                   "Obciążony, mały rozrzut", "Obciążony, duży rozrzut")

# 20 strzałów na tarczę. Środek tarczy = θ; obciążony estymator strzela przesunięty o 1.5.
# Średnia chmury (krzyżyk) tylko dla obciążonych paneli.
fig_1b_config <- local({
  set.seed(4102)
  bias <- c(0, 0, 1.5, 1.5); spread <- c(0.6, 1.4, 0.6, 1.4)
  panels <- lapply(seq_along(fig_1b_panels), function(i) {
    x <- bias[i] + rnorm(20, sd = spread[i])
    y <- rnorm(20, sd = spread[i])
    list(shots = round(cbind(x, y), 4),
         mean = if (bias[i] > 0) round(c(mean(x), mean(y)), 4))
  })
  list(labels = fig_1b_panels, panels = panels)
})

fig_1b_ui <- fig_panel("1b", "Ryc. 1.1", "Tarcza: obciążenie i rozrzut",
  fig_ch1_styles,
  lc_sf_figure("d04-tarcza",
    config = fig_1b_config,
    body = fig_ch1_facets(fig_1b_panels, ratio = "1/1", class = "is-square")
  ),
  width_mode = "text"
)

# ============================================================================
# (1c) GALERIA „4× WIĘCEJ OSÓB, POŁOWA ROZRZUTU” (stałe średnie z R)
# ============================================================================

fig_1c_ns <- c(25L, 100L, 400L)

# 3000 średnich x̄ dla każdego n. Obraz jest statyczny: R liczy słupki gęstości jak
# geom_histogram(aes(y = after_stat(density)), binwidth = 0.2, boundary = 0)
# (przedziały domknięte z prawej) i SD średnich do klamer ±2·SD.
fig_1c_config <- local({
  set.seed(4103)
  w <- 0.2
  panels <- lapply(fig_1c_ns, function(n) {
    x <- colMeans(matrix(rnorm(3000 * n, net_world$mu, net_world$sigma), nrow = n))
    bin <- ceiling(x / w - 1e-9)
    counts <- table(bin)
    k <- as.numeric(names(counts))
    list(n = n, sd = round(sd(x), 4),
         bins = unname(cbind(round((k - 1) * w, 4), round(k * w, 4),
                             round(as.numeric(counts) / (length(x) * w), 5))))
  })
  list(mu = net_world$mu, panels = panels)
})

fig_1c_labels <- vapply(fig_1c_config$panels, function(p) sprintf("n = %d · SD(x̄) ≈ %.1f cm", p$n, p$sd), "")

fig_1c_ui <- fig_panel("1c", "Ryc. 1.3", "4× więcej osób, połowa rozrzutu",
  fig_ch1_styles,
  lc_sf_figure("d04-stos",
    config = fig_1c_config,
    body = fig_ch1_facets(fig_1c_labels, ratio = "3.8/1", cols = 1)
  ),
  width_mode = "text"
)

# ============================================================================
# (1d) SCENA „DWIE PIZZERIE, OBIE 4.4 ★” (dwa światy obok siebie; losuje przeglądarka)
# ============================================================================

# Wczoraj obie pizzerie miały średnią ocen dnia 4.4 ★. U Gośki ocenia 5 klientów dziennie,
# w Napoli 200. „Kolejny dzień” stawia średnią ocen z następnego dnia. Obie pizzerie są
# naprawdę równie dobre (ten sam ukryty rozkład gwiazdek, średnio ok. 4.4 ★).
fig_1d_config <- list(
  init = 4.4,
  rows = list(list(who = "U Gośki: 5 opinii dziennie", n = 5L, y = 2),
              list(who = "Napoli: 200 opinii dziennie", n = 200L, y = 1)),
  prob = c(0.03, 0.04, 0.08, 0.22, 0.63)   # 1–5 ★, średnio 4.38
)

fig_1d_ui <- fig_panel("1d", "Ryc. 1.4", "Dwie pizzerie, obie 4.4 ★",
  lc_sf_figure("d04-pizzerie",
    toolbar = tagList(
      lc_sf_action("rep1", "fig_1d_rep1", "Kolejny dzień", variant = "solid"),
      lc_sf_action("rep20", "fig_1d_rep20", "+20 dni", variant = "outline"),
      lc_sf_action("reset", "fig_1d_reset", icon = "reset", variant = "ghost", aria_label = "Od nowa"),
      lc_readouts(
        lc_readout("Dni", lc_sf_value("days", "0")),
        lc_readout("SD średnich U Gośki", lc_sf_value("sd1"), color = upwr_accent),
        lc_readout("SD średnich Napoli", lc_sf_value("sd2"))
      )
    ),
    config = fig_1d_config,
    ratio = "3/1",
    # Podpis: przed pierwszym dniem pytanie, potem SD średnich (liczby wypełnia JS).
    notes = list(
      empty = lc_caption("Wczoraj obie pizzerie miały średnią ocen 4.4 ★. Która średnia mówi więcej?"),
      reps = lc_caption("Po ", lc_sf_value("days"), " dniach średnia dnia U Gośki chwieje się o SD ",
                        tags$span(`data-v` = "sd1", "–", .noWS = "after"), ", a w Napoli o SD ",
                        tags$span(`data-v` = "sd2", "–", .noWS = "after"), ".")
    ),
    selected = "empty"
  ),
  width_mode = "wide"
)

# ============================================================================
# SERWER
# ============================================================================

# Ryciny 1.1–1.4 liczy i rysuje przeglądarka (sf_ch12.js); serwer nie ma tu nic do zrobienia.
fig_ch1_server <- function(input, output, session) {
  invisible(NULL)
}
