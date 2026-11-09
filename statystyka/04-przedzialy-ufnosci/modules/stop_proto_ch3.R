# PROTOTYPY PUNKTÓW STOP — rozdział 3 (2026-10-30) ---------------------------
#   3a „Rozsuń linie” — UI usunięte 2026-11-09, zastąpił je wariant B5
#               (brainstorm.R); zostają s4p_3a_stats() i stałe, z których B5 korzysta.
#   3b „Trójkąt błędów” — usunięty 2026-11-09.
#   s4p_3c_ui — Piaskownica „Istotne, ale nieważne”: przy wielkiej próbie różnica
#               0.4 pkt IQ jest istotna statystycznie, ale nie praktycznie.
# Serwer: s4p_ch3_server(input, output, session).

s4p_3_col <- function(key) unname(upwr_cat[[key]])

# ============================================================================
# (3a) „ROZSUŃ LINIE” — obliczenia dla wariantu B5
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
