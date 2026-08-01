# ==============================================================================
# theme_upwr() — motyw ggplot2 spójny z paletą UPWr
# ==============================================================================
#
# Wymaga: R/palette.R (stałe upwr_*)
# Używa: ggplot2
#
# Domyślne fonty zostawione systemowe — docelowo warto podpiąć Fraunces
# (display) + Inter (sans) + JetBrains Mono przez sysfonts/showtext albo ragg.
#
# ==============================================================================


#' Motyw UPWr dla ggplot2
#'
#' @param base_size bazowy rozmiar tekstu (pt)
#' @param base_family rodzina czcionek dla tekstu głównego
#' @param grid czy pokazać siatkę ("both", "x", "y", "none")
#' @param panel czy rysować ramkę panelu
#'
#' @return obiekt theme()
theme_upwr <- function(base_size   = 11,
                       base_family = "",
                       grid        = c("both", "x", "y", "none"),
                       panel       = FALSE) {
  grid <- match.arg(grid)

  t <- ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # Tła — białe, żeby wykres zlewał się z kafelkiem figure_panel.
      # Domyślny cream z palety zostaje dla kontekstów poza UI wykładu.
      plot.background  = ggplot2::element_rect(fill = "#ffffff", color = NA),
      panel.background = ggplot2::element_rect(fill = "#ffffff", color = NA),

      # Siatka — subtelna, w kolorze linii pomocniczych
      panel.grid.major = ggplot2::element_line(color = upwr_rule, linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),

      # Osie
      axis.line  = ggplot2::element_line(color = upwr_reference, linewidth = 0.4),
      axis.ticks = ggplot2::element_line(color = upwr_reference, linewidth = 0.3),
      axis.text  = ggplot2::element_text(color = upwr_ink_soft, size = ggplot2::rel(0.85)),
      axis.title = ggplot2::element_text(color = upwr_ink,      size = ggplot2::rel(0.95)),

      # Tytuły
      plot.title = ggplot2::element_text(
        color  = upwr_ink,
        face   = "plain",
        size   = ggplot2::rel(1.3),
        margin = ggplot2::margin(b = 6)
      ),
      plot.subtitle = ggplot2::element_text(
        color  = upwr_ink_soft,
        face   = "italic",
        size   = ggplot2::rel(1.0),
        margin = ggplot2::margin(b = 12)
      ),
      plot.caption = ggplot2::element_text(
        color  = upwr_reference,
        size   = ggplot2::rel(0.8),
        hjust  = 0,
        margin = ggplot2::margin(t = 8)
      ),
      plot.caption.position = "plot",
      plot.title.position   = "plot",

      # Legenda
      legend.background = ggplot2::element_blank(),
      legend.key        = ggplot2::element_blank(),
      legend.title      = ggplot2::element_text(color = upwr_ink,      size = ggplot2::rel(0.85)),
      legend.text       = ggplot2::element_text(color = upwr_ink_soft, size = ggplot2::rel(0.8)),
      legend.position   = "right",

      # Panele (facets)
      strip.background = ggplot2::element_rect(fill = upwr_rule, color = NA),
      strip.text       = ggplot2::element_text(
        color  = upwr_ink,
        face   = "italic",
        size   = ggplot2::rel(0.9),
        margin = ggplot2::margin(t = 4, b = 4)
      ),

      # Marginesy
      plot.margin = ggplot2::margin(t = 16, r = 16, b = 12, l = 12)
    )

  # Kontrola siatki
  if (grid == "x") {
    t <- t + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  } else if (grid == "y") {
    t <- t + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
  } else if (grid == "none") {
    t <- t + ggplot2::theme(panel.grid.major = ggplot2::element_blank())
  }

  # Ramka panelu (opcjonalna)
  if (panel) {
    t <- t + ggplot2::theme(
      panel.border = ggplot2::element_rect(color = upwr_rule, fill = NA, linewidth = 0.4)
    )
  }

  t
}


#' Warianty motywu — shortcuty
theme_upwr_x   <- function(...) theme_upwr(grid = "x", ...)    # tylko linie pionowe (dla wykresów z kat. na y)
theme_upwr_y   <- function(...) theme_upwr(grid = "y", ...)    # tylko linie poziome (najczęściej)
theme_upwr_min <- function(...) theme_upwr(grid = "none", ...) # minimalistyczny, bez siatki
