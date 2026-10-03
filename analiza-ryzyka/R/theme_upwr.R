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


# ============================================================================
# Wykresy w widgetach krokowych: role warstw (handoff „Widget krokowy v2”)
# Każdy element wykresu ma w danym kroku jedną rolę: dane, grupa (druga
# kategoria), nowe (wprowadzone w tym kroku), znane (z wcześniejszych kroków),
# tło (dane zastąpione konstrukcją). Kontury kształtów wyniku są czarne.
# ============================================================================

STEP_ROLES <- list(
  data       = list(colour = unname(upwr_cat["niebo"]),    alpha = 0.70, linewidth = 1.00),
  group      = list(colour = unname(upwr_cat["bursztyn"]), alpha = 0.70, linewidth = 1.00),
  new        = list(colour = upwr_accent,                  alpha = 1.00, linewidth = 0.90),
  known      = list(colour = upwr_secondary,               alpha = 1.00, linewidth = 0.55),
  background = list(colour = unname(upwr_cat["niebo"]),    alpha = 0.22, linewidth = 0.50)
)
STEP_EDGE <- list(colour = upwr_ink, linewidth = 0.35)

# Rola elementu wprowadzonego w kroku `from`: „new” w tym kroku, potem „known”.
step_role <- function(step, from) if (step == from) "new" else "known"

# Warstwa widoczna od kroku `from` do `to` (włącznie); poza zakresem NULL.
step_show <- function(step, from, layer, to = Inf) {
  if (step >= from && step <= to) layer else NULL
}

# Dowolny geom w danej roli; fill_role = TRUE koloruje też wypełnienie.
# Argumenty geomu (także fill = NA) przechodzą przez `...`.
step_layer <- function(geom, role, ..., fill_role = FALSE) {
  r <- STEP_ROLES[[match.arg(role, names(STEP_ROLES))]]
  a <- list(...)
  a$colour <- a$colour %||% r$colour
  a$alpha <- a$alpha %||% r$alpha
  if (isTRUE(fill_role)) a$fill <- a$fill %||% r$colour
  if (!identical(geom, ggplot2::geom_point) && !identical(geom, ggplot2::geom_jitter)) {
    a$linewidth <- a$linewidth %||% r$linewidth
  }
  do.call(geom, a)
}

# Słupki / pudełko wyniku: wypełnienie w roli „data”, czarna krawędź.
# Wypełnienie zmapowane w aes(fill = …) nie jest nadpisywane.
step_result <- function(geom, ...) {
  a <- list(...)
  if (is.null(a$mapping$fill)) a$fill <- a$fill %||% STEP_ROLES$data$colour
  a$alpha <- a$alpha %||% STEP_ROLES$data$alpha
  a$colour <- a$colour %||% STEP_EDGE$colour
  a$linewidth <- a$linewidth %||% STEP_EDGE$linewidth
  do.call(geom, a)
}

# Linia: przerywana dla konstrukcji pomocniczej, ciągła dla elementu wyniku.
step_line <- function(role, xintercept = NULL, yintercept = NULL, helper = TRUE) {
  lt <- if (helper) "22" else "solid"
  if (!is.null(xintercept)) {
    return(step_layer(ggplot2::geom_vline, role, xintercept = xintercept, linetype = lt))
  }
  step_layer(ggplot2::geom_hline, role, yintercept = yintercept, linetype = lt)
}

# Etykieta przy elemencie: krótki symbol (Me, Q1, n = 12), pogrubiony, kolor roli.
# Domyślny krój ma znaki x̄, p̂, ₁, β (mono ich nie ma); parse = TRUE dla plotmath.
step_label <- function(x, y, label, role = "known", hjust = 0, vjust = 0, size = 3.6,
                       family = "", parse = FALSE) {
  ggplot2::annotate("text", x = x, y = y, label = label, hjust = hjust, vjust = vjust,
                    colour = STEP_ROLES[[role]]$colour, family = family,
                    fontface = if (isTRUE(parse)) "plain" else "bold",
                    size = size, parse = parse)
}

# Stała rama: limity osi liczone raz z pełnych danych, wspólne dla kroków.
# y_axis = FALSE ukrywa oś Y, gdy w danym kroku nie niesie znaczenia.
step_frame <- function(xlim, ylim = NULL, y_axis = TRUE) {
  list(
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE),
    if (!y_axis) ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                                axis.title.y = ggplot2::element_blank(),
                                panel.grid.major.y = ggplot2::element_blank(),
                                axis.ticks.y = ggplot2::element_blank()),
    ggplot2::theme(legend.position = "none")
  )
}
