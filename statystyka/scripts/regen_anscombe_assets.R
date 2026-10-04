# Regeneruje statyczne PNG-i do pułapek korelacji
# (04-wnioskowanie-statystyczne/assets/).
#
# Uruchom z roota repo:
#   Rscript statystyka/scripts/regen_anscombe_assets.R
#
# Generowane pliki:
#   anscombe-quartet.png      — Ryc. 6.6: kwartet Anscombe'a (datasets::anscombe)
#   correlation-nonlinear.png — Ryc. 6.7: zależność kwadratowa, r ≈ 0
#
# Bez tytułów wykresów (tytuł i opis są w panelu wykładu); etykiety
# zestawów z r zostają, bo rozróżniają panele.

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})

script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg)) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = TRUE)))
  }
  normalizePath(getwd(), mustWork = TRUE)
}

project_root <- normalizePath(file.path(script_dir(), ".."), mustWork = TRUE)

source(file.path(project_root, "R", "palette.R"))
source(file.path(project_root, "R", "theme_upwr.R"))
source(file.path(project_root, "R", "shared.R"))
lc_apply_ggplot_defaults()

assets_dir <- file.path(project_root, "04-wnioskowanie-statystyczne", "assets")
stopifnot(dir.exists(assets_dir))

DPI <- 100

ggsave_ragg <- function(name, plot, width, height) {
  ggsave(file.path(assets_dir, name),
         plot = plot, width = width, height = height, dpi = DPI, bg = "white",
         device = ragg::agg_png)
  cat("OK -> ", file.path(assets_dir, name), "\n")
}

point_colour <- unname(upwr_cat["niebo"])

# ----------------------------------------------------------------------------
# Ryc. 6.6 — Kwartet Anscombe'a
# ----------------------------------------------------------------------------

anscombe_panel <- function(i) {
  df <- data.frame(x = anscombe[[paste0("x", i)]], y = anscombe[[paste0("y", i)]])
  r <- formatC(cor(df$x, df$y), format = "f", digits = 3)
  ggplot(df, aes(x, y)) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                colour = upwr_accent, linewidth = 1.1, fullrange = TRUE) +
    geom_point(colour = point_colour, alpha = 0.85, size = 2.8) +
    labs(subtitle = paste0("Zestaw ", i, " (r = ", r, ")"), x = "x", y = "y") +
    theme(text = element_text(size = 19),
          axis.text = element_text(size = 15),
          plot.subtitle = element_text(face = "bold", hjust = 0.5, size = 19))
}

g <- anscombe_panel(1) + anscombe_panel(2) + anscombe_panel(3) + anscombe_panel(4) +
  plot_layout(nrow = 1)

ggsave_ragg("anscombe-quartet.png", g, width = 16.5, height = 5)

# ----------------------------------------------------------------------------
# Ryc. 6.7 — Silny związek kwadratowy, r ≈ 0 (seed 42 daje r = -0.004,
# wartość podana w tekście wykładu)
# ----------------------------------------------------------------------------

x <- seq(-3, 3, length.out = 100)
set.seed(42)
y <- x^2 + rnorm(100, 0, 0.5)
cat("r (nieliniowość) =", round(cor(x, y), 3), "\n")

g <- ggplot(data.frame(x = x, y = y), aes(x, y)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
              colour = upwr_accent, linewidth = 1.1, linetype = "dashed") +
  geom_point(colour = point_colour, alpha = 0.75, size = 2.4) +
  labs(x = "x", y = "y") +
  theme(text = element_text(size = 17), axis.text = element_text(size = 14))

ggsave_ragg("correlation-nonlinear.png", g, width = 9, height = 6)

cat("\nDone.\n")
