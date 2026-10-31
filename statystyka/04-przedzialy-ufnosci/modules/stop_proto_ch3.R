# PROTOTYPY PUNKTÓW STOP — rozdział 3 (2026-10-30) ---------------------------
#   s4p_3a_ui — Piaskownica „Rozsuń linie”: istnieje pas różnic, w którym przedziały
#               grup się nakładają, a przedział różnicy już nie obejmuje zera.
#   s4p_3b_ui — Pojęcie „Trójkąt błędów”: SE różnicy to przeciwprostokątna, więc jest
#               krótszy niż SE1 + SE2.
#   s4p_3c_ui — Piaskownica „Istotne, ale nieważne”: przy wielkiej próbie różnica
#               0.4 pkt IQ jest istotna statystycznie, ale nie praktycznie.
# Serwer: s4p_ch3_server(input, output, session).

s4p_3_col <- function(key) unname(upwr_cat[[key]])

# ============================================================================
# (3a) PIASKOWNICA „ROZSUŃ LINIE”
# ============================================================================

s4p_3a_n   <- 120
s4p_3a_sd1 <- 1.02
s4p_3a_sd2 <- 0.94
s4p_3a_m2  <- 9.08   # średnia linii 2; linia 1 ma średnią m2 + d

# Wszystkie wielkości potrzebne do rysunku: przedziały grup, przedział różnicy,
# granice stref (d_ist: różnica istotna, d_roz: przedziały grup rozłączne).
s4p_3a_stats <- function(d) {
  n    <- s4p_3a_n
  se1  <- s4p_3a_sd1 / sqrt(n)
  se2  <- s4p_3a_sd2 / sqrt(n)
  m1   <- s4p_3a_m2 + d
  m2   <- s4p_3a_m2
  t_g  <- qt(0.975, df = n - 1)
  me1  <- t_g * se1
  me2  <- t_g * se2
  se_d <- sqrt(se1^2 + se2^2)
  v1   <- se1^2
  v2   <- se2^2
  df_w <- (v1 + v2)^2 / (v1^2 / (n - 1) + v2^2 / (n - 1))
  t_d  <- qt(0.975, df = df_w)
  me_d <- t_d * se_d
  d_ist <- me_d
  d_roz <- me1 + me2
  zone  <- if (d < d_ist) "nieistotna" else if (d < d_roz) "nakladaja" else "rozlaczne"
  list(d = d, n = n, se1 = se1, se2 = se2, se_d = se_d, df_w = df_w, t_d = t_d,
       m1 = m1, m2 = m2,
       ci1 = c(m1 - me1, m1 + me1), ci2 = c(m2 - me2, m2 + me2),
       ci_d = c(d - me_d, d + me_d),
       d_ist = d_ist, d_roz = d_roz, zone = zone)
}

s4p_3a_zone_label <- c(
  nieistotna = "różnica nieistotna",
  nakladaja  = "nakładają się, ale różnica istotna",
  rozlaczne  = "rozłączne"
)

# Górny (dwa przedziały grup, odcinek nakładania na żółto) i dolny (przedział różnicy).
s4p_3a_plot_fn <- function(d) {
  st <- s4p_3a_stats(d)
  col_1  <- s4p_3_col("niebo")
  col_2  <- s4p_3_col("indygo")
  col_ov <- s4p_3_col("bursztyn")

  grp <- data.frame(
    y   = c(2, 1),
    m   = c(st$m1, st$m2),
    lo  = c(st$ci1[1], st$ci2[1]),
    hi  = c(st$ci1[2], st$ci2[2]),
    col = c(col_1, col_2)
  )
  ov_lo <- max(grp$lo)
  ov_hi <- min(grp$hi)
  xlim_top <- range(c(grp$lo, grp$hi))
  pad_top  <- diff(xlim_top) * 0.06
  xlim_top <- xlim_top + c(-pad_top, pad_top)

  p_top <- ggplot()
  if (ov_lo < ov_hi) {
    p_top <- p_top + annotate("rect", xmin = ov_lo, xmax = ov_hi, ymin = -Inf, ymax = Inf,
                              fill = col_ov, alpha = 0.35)
  }
  p_top <- p_top +
    geom_segment(data = grp, aes(x = lo, xend = hi, y = y, yend = y, colour = col),
                 linewidth = 3.2) +
    geom_point(data = grp, aes(x = m, y = y), shape = 18, size = 5,
               colour = s4p_3_col("grafit")) +
    scale_colour_identity() +
    scale_y_continuous(breaks = c(2, 1), labels = c("Linia 1", "Linia 2")) +
    coord_cartesian(xlim = xlim_top, ylim = c(0.4, 2.6)) +
    labs(x = "Zawartość błonnika (g / 100 g)", y = NULL) +
    theme_upwr(grid = "x")

  dd <- data.frame(y = 0, lo = st$ci_d[1], hi = st$ci_d[2], d = d)
  xlim_bot <- range(c(0, st$ci_d))
  pad_bot  <- diff(xlim_bot) * 0.12
  xlim_bot <- xlim_bot + c(-pad_bot, pad_bot)

  p_bot <- ggplot() +
    geom_vline(xintercept = 0, colour = s4p_3_col("grafit"), linewidth = 0.8) +
    geom_segment(data = dd, aes(x = lo, xend = hi, y = y, yend = y),
                 colour = s4p_3_col("niebo"), linewidth = 3.2) +
    geom_point(data = dd, aes(x = d, y = y), shape = 18, size = 5,
               colour = s4p_3_col("bursztyn")) +
    coord_cartesian(xlim = xlim_bot, ylim = c(-0.6, 0.6)) +
    labs(x = "Różnica średnich: linia 1 − linia 2 (g / 100 g)", y = NULL) +
    theme_upwr(grid = "x") +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

  patchwork::wrap_plots(p_top, p_bot, ncol = 1, heights = c(2, 1))
}

# Cienki pasek stref na osi d od 0 do 0.8, z markerem bieżącego d.
s4p_3a_zones_fn <- function(d) {
  st <- s4p_3a_stats(d)
  ggplot() +
    annotate("rect", xmin = 0, xmax = st$d_ist, ymin = 0, ymax = 1,
             fill = s4p_3_col("grafit"), alpha = 0.12) +
    annotate("rect", xmin = st$d_ist, xmax = st$d_roz, ymin = 0, ymax = 1,
             fill = s4p_3_col("terakota"), alpha = 0.35) +
    annotate("rect", xmin = st$d_roz, xmax = 0.8, ymin = 0, ymax = 1,
             fill = s4p_3_col("szalwia"), alpha = 0.35) +
    annotate("segment", x = d, xend = d, y = -0.1, yend = 1.1,
             colour = s4p_3_col("grafit"), linewidth = 1.4) +
    scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8),
                       labels = c("0", "0.2", "0.4", "0.6", "0.8")) +
    coord_cartesian(xlim = c(0, 0.8), ylim = c(-0.1, 1.1), expand = FALSE) +
    labs(x = "Różnica średnich d (g / 100 g)", y = NULL) +
    theme_upwr(grid = "none") +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
}

s4p_3a_ui <- s4p_panel("3a", "Piaskownica", "Rozsuń linie", width_mode = "wide",
  lc_toolbar(
    lc_slider("s4p_3a_d", "Różnica średnich d (g/100 g)", min = 0, max = 0.8,
              value = 0.34, step = 0.01, digits = 2),
    lc_readouts(uiOutput("s4p_3a_read"))
  ),
  lc_plot("s4p_3a_zones", ratio = "9/1", max_height = "90px"),
  lc_legend("Strefy na pasku", unname(s4p_3a_zone_label),
            c(s4p_3_col("grafit"), s4p_3_col("terakota"), s4p_3_col("szalwia"))),
  lc_plot("s4p_3a_plot", ratio = "1.8/1", max_height = "520px"),
  uiOutput("s4p_3a_caption")
)

# ============================================================================
# (3b) POJĘCIE „TRÓJKĄT BŁĘDÓW”
# ============================================================================

# Trójkąt prostokątny: A = (0, 0), B = (SE1, 0), C = (SE1, SE2); kąt prosty w B.
# Przeciwprostokątna AC ma długość sqrt(SE1^2 + SE2^2) = SE różnicy.
# Obok: dwa słupki na wspólnej skali, SE1 + SE2 (składany) i SE różnicy.
s4p_3b_plot_fn <- function(se1, se2) {
  se_d <- sqrt(se1^2 + se2^2)
  col_1 <- s4p_3_col("niebo")
  col_2 <- s4p_3_col("bursztyn")
  col_d <- s4p_3_col("wrzos")
  ink   <- s4p_3_col("grafit")

  tri <- data.frame(x = c(0, se1, se1), y = c(0, 0, se2))
  legs <- data.frame(
    x    = c(0, se1, 0),
    y    = c(0, 0, 0),
    xend = c(se1, se1, se1),
    yend = c(0, se2, se2),
    col  = c(col_1, col_2, col_d)
  )
  p_tri <- ggplot() +
    geom_polygon(data = tri, aes(x, y), fill = ink, alpha = 0.06) +
    geom_segment(data = legs, aes(x = x, xend = xend, y = y, yend = yend, colour = col),
                 linewidth = 2.4, lineend = "round") +
    annotate("text", x = se1 / 2, y = -0.014, label = "SE₁", colour = col_1,
             fontface = "bold", size = 5) +
    annotate("text", x = se1 + 0.008, y = se2 / 2, label = "SE₂", colour = col_2,
             fontface = "bold", size = 5, hjust = 0) +
    annotate("text", x = se1 / 2 - 0.012, y = se2 / 2 + 0.008, label = "SE różnicy",
             colour = col_d, fontface = "bold", size = 5, hjust = 1) +
    scale_colour_identity() +
    coord_equal(xlim = c(-0.15, 0.25), ylim = c(-0.03, 0.24), expand = FALSE) +
    theme_void()

  bars <- data.frame(
    xmin = c(0, se1, 0),
    xmax = c(se1, se1 + se2, se_d),
    ymin = c(1.6, 1.6, 0.6),
    ymax = c(2.4, 2.4, 1.4),
    fill = c(col_1, col_2, col_d)
  )
  p_bar <- ggplot(bars) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill)) +
    scale_fill_identity() +
    scale_y_continuous(breaks = c(2, 1), labels = c("SE₁ + SE₂", "SE różnicy")) +
    coord_cartesian(xlim = c(0, 0.42), ylim = c(0.4, 2.6), expand = FALSE) +
    labs(x = "Błąd standardowy (g / 100 g)", y = NULL) +
    theme_upwr(grid = "x")

  patchwork::wrap_plots(p_tri, p_bar, ncol = 2, widths = c(1, 1.3))
}

s4p_3b_ui <- s4p_panel("3b", "Pojęcie", "Trójkąt błędów", width_mode = "wide",
  lc_toolbar(
    lc_slider("s4p_3b_se1", "SE grupy 1", min = 0.02, max = 0.20, value = 0.093,
              step = 0.005, digits = 3),
    lc_slider("s4p_3b_se2", "SE grupy 2", min = 0.02, max = 0.20, value = 0.086,
              step = 0.005, digits = 3),
    lc_readouts(uiOutput("s4p_3b_read"))
  ),
  lc_plot("s4p_3b_plot", ratio = "2.4/1", max_height = "420px")
)

# ============================================================================
# (3c) PIASKOWNICA „ISTOTNE, ALE NIEWAŻNE”
# ============================================================================

s4p_3c_diff <- 0.4    # różnica średnich, pkt IQ
s4p_3c_sd   <- 15     # odchylenie standardowe IQ w obu grupach
s4p_3c_band <- 3      # pas „praktycznie bez znaczenia”: −3 … 3 pkt

# Suwak działa na log10(n); n zaokrąglone do dwóch cyfr znaczących (50 … 50 000).
s4p_3c_n_of <- function(lg) round(signif(10^lg, 2))

s4p_3c_stats <- function(n) {
  se <- s4p_3c_sd * sqrt(2 / n)
  me <- 1.96 * se
  ci <- s4p_3c_diff + c(-me, me)
  sig <- !(ci[1] <= 0 && 0 <= ci[2])
  # „ważna praktycznie”: tak, gdy cały przedział leży poza pasem; nie, gdy cały
  # w środku pasa; inaczej nie wiadomo.
  practical <- if (ci[1] > -s4p_3c_band && ci[2] < s4p_3c_band) "nie"
               else if (ci[1] > s4p_3c_band || ci[2] < -s4p_3c_band) "tak"
               else "nie wiadomo"
  list(n = n, se = se, ci = ci, sig = sig, practical = practical)
}

s4p_3c_plot_fn <- function(n) {
  st <- s4p_3c_stats(n)
  ggplot() +
    annotate("rect", xmin = -s4p_3c_band, xmax = s4p_3c_band, ymin = -Inf, ymax = Inf,
             fill = s4p_3_col("szalwia"), alpha = 0.15) +
    geom_vline(xintercept = 0, colour = s4p_3_col("grafit"), linewidth = 0.6) +
    annotate("segment", x = st$ci[1], xend = st$ci[2], y = 0, yend = 0,
             colour = s4p_3_col("niebo"), linewidth = 4.5, lineend = "butt") +
    annotate("point", x = s4p_3c_diff, y = 0, shape = 18, size = 5,
             colour = s4p_3_col("bursztyn")) +
    coord_cartesian(xlim = c(-8, 8), ylim = c(-1, 1), expand = FALSE) +
    labs(x = "Różnica średnich IQ, województwo A − B (pkt IQ)", y = NULL) +
    theme_upwr(grid = "x") +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          panel.grid.major.y = element_blank())
}

s4p_3c_ui <- s4p_panel("3c", "Piaskownica", "Istotne, ale nieważne", width_mode = "wide",
  lc_toolbar(
    tags$div(class = "lc-grp lc-grow lc-slider",
      tags$div(class = "lc-slider-label",
        tags$label(`for` = "s4p_3c_logn", "Osób w każdej grupie"),
        tags$output(textOutput("s4p_3c_n_txt", inline = TRUE))
      ),
      sliderInput("s4p_3c_logn", label = NULL, min = 1.7, max = 4.7, value = 4.3,
                  step = 0.01, ticks = FALSE, width = "100%"),
      tags$div(class = "lc-slider-ends", `aria-hidden` = "true",
        tags$span("50"), tags$span("50 000"))
    ),
    lc_readouts(
      lc_readout("Różnica", "0.4 pkt IQ", color = s4p_3_col("bursztyn")),
      lc_readout("Pas bez znaczenia", "±3 pkt", color = s4p_3_col("szalwia"), swatch = TRUE),
      uiOutput("s4p_3c_read")
    )
  ),
  lc_plot("s4p_3c_plot", ratio = "4/1", max_height = "260px"),
  uiOutput("s4p_3c_caption")
)

# ============================================================================
# SERWER
# ============================================================================

s4p_ch3_server <- function(input, output, session) {

  # --- 3a -------------------------------------------------------------------
  s4p_3a_d <- reactive(input$s4p_3a_d %||% 0.34)

  output$s4p_3a_read <- renderUI({
    st <- s4p_3a_stats(s4p_3a_d())
    tagList(
      lc_readout("Różnica d", lc_fmt(st$d, 2)),
      lc_readout("95% CI różnicy",
                 paste0("[", lc_fmt(st$ci_d[1], 2), " ; ", lc_fmt(st$ci_d[2], 2), "]"),
                 color = upwr_accent)
    )
  })

  output$s4p_3a_caption <- renderUI({
    st <- s4p_3a_stats(s4p_3a_d())
    verdict <- switch(st$zone,
      nieistotna = "przedział różnicy obejmuje 0.",
      nakladaja  = "przedziały grup się nakładają, ale przedział różnicy nie obejmuje 0.",
      rozlaczne  = "przedziały grup są rozłączne.")
    lc_caption(paste0("Strefa „", s4p_3a_zone_label[[st$zone]], "”: ", verdict))
  })

  zoom_plot_server("s4p_3a_zones", reactive(s4p_3a_zones_fn(s4p_3a_d())),
    alt = "Pasek od 0 do 0.8 g / 100 g: szary odcinek bez różnicy, terakotowy odcinek, w którym przedziały grup się nakładają, a różnica jest istotna, i szałwiowy odcinek rozłącznych przedziałów; czarna kreska pokazuje bieżące d")

  zoom_plot_server("s4p_3a_plot", reactive(s4p_3a_plot_fn(s4p_3a_d())),
    alt = "Górny wykres: dwa poziome 95% przedziały średnich linii 1 i 2 z żółtym odcinkiem nakładania; dolny wykres: przedział dla różnicy średnich z pionową linią w zerze")

  # --- 3b -------------------------------------------------------------------
  s4p_3b_se1 <- reactive(input$s4p_3b_se1 %||% 0.093)
  s4p_3b_se2 <- reactive(input$s4p_3b_se2 %||% 0.086)

  output$s4p_3b_read <- renderUI({
    se1 <- s4p_3b_se1()
    se2 <- s4p_3b_se2()
    sum_se  <- se1 + se2
    root_se <- sqrt(se1^2 + se2^2)
    tagList(
      lc_readout("SE₁ + SE₂", lc_fmt(sum_se, 3)),
      lc_readout("√(SE₁² + SE₂²)", lc_fmt(root_se, 3), color = s4p_3_col("wrzos")),
      lc_readout("stosunek", lc_fmt(root_se / sum_se, 2))
    )
  })

  zoom_plot_server("s4p_3b_plot", reactive(s4p_3b_plot_fn(s4p_3b_se1(), s4p_3b_se2())),
    alt = "Po lewej trójkąt prostokątny: przyprostokątne SE₁ i SE₂, przeciwprostokątna SE różnicy; po prawej dwa słupki na wspólnej skali: SE₁ + SE₂ oraz krótszy SE różnicy")

  # --- 3c -------------------------------------------------------------------
  s4p_3c_n <- reactive(s4p_3c_n_of(input$s4p_3c_logn %||% 4.3))

  output$s4p_3c_n_txt <- renderText(lc_fmt(s4p_3c_n(), 0, " "))

  output$s4p_3c_read <- renderUI({
    st <- s4p_3c_stats(s4p_3c_n())
    tagList(
      lc_readout("95% CI różnicy",
                 paste0("[", lc_fmt(st$ci[1], 2), " ; ", lc_fmt(st$ci[2], 2), "]"),
                 color = upwr_accent),
      lc_readout("Istotna statystycznie", if (st$sig) "tak" else "nie"),
      lc_readout("Ważna praktycznie", st$practical)
    )
  })

  output$s4p_3c_caption <- renderUI({
    st <- s4p_3c_stats(s4p_3c_n())
    txt <- if (!st$sig) {
      "Przedział różnicy obejmuje 0: dane nie wskazują różnicy."
    } else if (st$practical == "tak") {
      "Przedział leży poza pasem ±3 pkt: różnica istotna i praktycznie ważna."
    } else if (st$practical == "nie") {
      "Przedział nie obejmuje 0, ale mieści się w pasie ±3 pkt: różnica istotna, bez znaczenia praktycznego."
    } else {
      "Przedział nie obejmuje 0, ale sięga poza pas ±3 pkt: sama istotność nie rozstrzyga, czy różnica ma znaczenie."
    }
    lc_caption(txt)
  })

  zoom_plot_server("s4p_3c_plot", reactive(s4p_3c_plot_fn(s4p_3c_n())),
    alt = "Oś różnicy IQ od −8 do 8 pkt: zacieniowany pas od −3 do 3, pionowa linia w zerze i gruby odcinek przedziału 95% różnicy 0.4 pkt, którego szerokość maleje wraz z liczbą osób")
}
