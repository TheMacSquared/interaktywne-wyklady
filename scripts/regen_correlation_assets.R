# Regeneruje statyczne PNG-i dla rozdzialu o korelacji
# (wnioskowanie-statystyczne/assets/correlation-*.png).
#
# Uruchom z roota repo:
#   Rscript scripts/regen_correlation_assets.R
#
# Generowane pliki:
#   correlation-strength.png  — Ryc. 6.1: trzy r przy STALYM rozrzucie globalnym
#   correlation-direction.png — Ryc. 6.2: ujemna / zero / dodatnia
#   correlation-scatter.png   — Ryc. 6.3: ten sam trend, rozny rozrzut -> rozne r
#
# Render przez ragg::agg_png — daje czyste znaki cyfr i polskie znaki bez
# konfliktu z showtext (ktore w shared.R rejestruje Atkinson Hyperlegible
# z dpi=96 i przy ggsave(dpi=100) zniekształca cyfry).

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(patchwork)
})

source("R/palette.R")
source("R/theme_upwr.R")

ggplot2::theme_set(theme_upwr())

assets_dir <- "wnioskowanie-statystyczne/assets"
stopifnot(dir.exists(assets_dir))

W <- 15
H <- 5.25
DPI <- 100

ggsave_ragg <- function(name, plot) {
  ggsave(file.path(assets_dir, name),
         plot = plot, width = W, height = H, dpi = DPI, bg = "white",
         device = ragg::agg_png)
  cat("OK -> ", file.path(assets_dir, name), "\n")
}

# ----------------------------------------------------------------------------
# Pomocnicze
# ----------------------------------------------------------------------------

# Generator par (x, y) takich, ze var(x) = var(y) = 1 (po standaryzacji)
# i korelacja Pearsona = target_r. n punktow.
make_xy <- function(n, target_r, seed) {
  set.seed(seed)
  x <- rnorm(n)
  x <- as.numeric(scale(x))               # var(x) = 1
  noise <- rnorm(n)
  noise <- as.numeric(scale(residuals(lm(noise ~ x))))
  y <- target_r * x + sqrt(1 - target_r^2) * noise
  y <- as.numeric(scale(y))               # var(y) = 1
  data.frame(x = x, y = y)
}

scatter_panel <- function(df, panel_title, free_y = FALSE) {
  p <- ggplot(df, aes(x = x, y = y)) +
    stat_ellipse(geom = "polygon", level = 0.95,
                 fill = upwr_cat["niebo"], alpha = 0.18,
                 color = upwr_cat["niebo"], linewidth = 0.4) +
    geom_point(color = upwr_cat["niebo"], alpha = 0.65, size = 2.2) +
    geom_smooth(method = "lm", se = FALSE, color = upwr_accent, linewidth = 1.1) +
    labs(title = panel_title, x = NULL, y = NULL) +
    theme(plot.title = element_text(face = "bold", size = rel(1.05)))
  if (!free_y) {
    p <- p + coord_cartesian(xlim = c(-3, 3), ylim = c(-3, 3))
  }
  p
}

# ----------------------------------------------------------------------------
# Ryc. 6.1 (po zamianie kolejnosci) — Korelacja vs nachylenie
# Trzy rozne nachylenia (lagodne / srednie / strome), staly maly szum.
# Wszystkie trzy maja podobnie wysokie r — bo r nie zalezy od nachylenia,
# tylko od ciasnosci punktow wokol prostej.
# ----------------------------------------------------------------------------

make_slope <- function(n, slope, sigma, seed) {
  set.seed(seed)
  x <- as.numeric(scale(rnorm(n)))
  y <- slope * x + sigma * rnorm(n)
  data.frame(x = x, y = y)
}

# Szum proporcjonalny do nachylenia daje WSZEDZIE podobne r (~ slope/sqrt(slope^2+sigma_rel^2)
# = 1/sqrt(1 + sigma_rel^2)). Dla sigma_rel = 0.33 -> r ~ 0.95 dla kazdego nachylenia.
n <- 90
sigma_rel <- 0.33

d_low  <- make_slope(n, slope = 0.4, sigma = 0.4 * sigma_rel, seed = 41)
d_med  <- make_slope(n, slope = 0.8, sigma = 0.8 * sigma_rel, seed = 42)
d_high <- make_slope(n, slope = 1.6, sigma = 1.6 * sigma_rel, seed = 43)

r_low  <- round(cor(d_low$x,  d_low$y),  2)
r_med  <- round(cor(d_med$x,  d_med$y),  2)
r_high <- round(cor(d_high$x, d_high$y), 2)

cat("slope panels (nowa Ryc. 6.1):\n")
cat("  slope 0.4 -> r =", r_low,  "\n")
cat("  slope 0.8 -> r =", r_med,  "\n")
cat("  slope 1.6 -> r =", r_high, "\n")

slope_panel <- function(df, panel_title) {
  ggplot(df, aes(x = x, y = y)) +
    stat_ellipse(geom = "polygon", level = 0.95,
                 fill = upwr_cat["niebo"], alpha = 0.18,
                 color = upwr_cat["niebo"], linewidth = 0.4) +
    geom_point(color = upwr_cat["niebo"], alpha = 0.65, size = 2.2) +
    geom_smooth(method = "lm", se = FALSE, color = upwr_accent, linewidth = 1.1) +
    coord_cartesian(xlim = c(-3, 3), ylim = c(-4.5, 4.5)) +
    labs(title = panel_title, x = NULL, y = NULL) +
    theme(plot.title = element_text(face = "bold", size = rel(1.05)))
}

g <- slope_panel(d_low,  paste0("Łagodne nachylenie (r = ", format(r_low,  nsmall = 2), ")")) +
     slope_panel(d_med,  paste0("Średnie nachylenie (r = ", format(r_med,  nsmall = 2), ")")) +
     slope_panel(d_high, paste0("Strome nachylenie (r = ", format(r_high, nsmall = 2), ")")) +
     plot_annotation(title = "Różne nachylenia, podobnie wysokie r") +
     plot_layout(nrow = 1)

ggsave_ragg("correlation-strength.png", g)

# ----------------------------------------------------------------------------
# Ryc. 6.2 — Kierunek korelacji (ujemna / zero / dodatnia)
# ----------------------------------------------------------------------------

d_neg  <- make_xy(n, target_r = -0.6, seed = 21)
d_zero <- make_xy(n, target_r =  0,   seed = 22)
d_pos  <- make_xy(n, target_r =  0.6, seed = 23)

g <- scatter_panel(d_neg,  "r = −0.6 (ujemna)") +
     scatter_panel(d_zero, "r = 0 (brak)") +
     scatter_panel(d_pos,  "r = +0.6 (dodatnia)") +
     plot_layout(nrow = 1)

ggsave_ragg("correlation-direction.png", g)

# ----------------------------------------------------------------------------
# Ryc. 6.3 — Ten sam trend, rozny rozrzut -> rozne r
# Wszystkie trzy panele: y = 1 * x + eps, gdzie sd(eps) rosnie.
# Bez standaryzacji y — chmura rosnie globalnie.
# ----------------------------------------------------------------------------

make_scatter_same_slope <- function(n, sigma, seed) {
  set.seed(seed)
  x <- as.numeric(scale(rnorm(n)))
  y <- 1 * x + sigma * rnorm(n)
  data.frame(x = x, y = y)
}

d_low  <- make_scatter_same_slope(n, sigma = 0.4,  seed = 31)
d_med  <- make_scatter_same_slope(n, sigma = 1.25, seed = 32)
d_high <- make_scatter_same_slope(n, sigma = 4.0,  seed = 33)

r_low  <- round(cor(d_low$x,  d_low$y),  2)
r_med  <- round(cor(d_med$x,  d_med$y),  2)
r_high <- round(cor(d_high$x, d_high$y), 2)

cat("scatter panels:\n")
cat("  small sd:", r_low, "\n")
cat("  med sd:  ", r_med, "\n")
cat("  large sd:", r_high, "\n")

g <- scatter_panel(d_low,  paste0("Mały rozrzut (r = ",  format(r_low,  nsmall = 2), ")"), free_y = TRUE) +
     scatter_panel(d_med,  paste0("Średni rozrzut (r = ", format(r_med,  nsmall = 2), ")"), free_y = TRUE) +
     scatter_panel(d_high, paste0("Duży rozrzut (r = ",   format(r_high, nsmall = 2), ")"), free_y = TRUE) +
     plot_annotation(title = "Ten sam trend, różny rozrzut → różne r") +
     plot_layout(nrow = 1)

ggsave_ragg("correlation-scatter.png", g)

cat("\nDone.\n")
