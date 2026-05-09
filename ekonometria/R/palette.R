# ==============================================================================
# Paleta kolorów UPWr — źródło prawdy
# ==============================================================================
#
# Wszystkie kolory używane w wykresach projektu są zdefiniowane tutaj.
# Reguły użycia: patrz palette-guide.md (lub sekcja "Paleta kolorów" w CLAUDE.md).
#
# Paleta kategoryczna jest CVD-friendly:
#   - min. ΔE ≥ 12.8 dla protanopii, deuteranopii i tritanopii
#   - wzorowana na Okabe-Ito, przesunięta tonalnie ku odcieniom ziemistym
#   - luminancje L* od 24 do 75 (rozróżnialna w skali szarości)
#
# ==============================================================================


# ---- Role semantyczne --------------------------------------------------------

upwr_accent      <- "#6b1a2a"  # burgund — akcent, wyróżnienie
upwr_single      <- "#6b1a2a"  # pojedyncza seria (neutralna)
upwr_single_alt  <- "#b48a2a"  # złoto — gdy burgund zajęty przez akcent
upwr_secondary   <- "#2d3b3d"  # grafit — tło, kontekst, kontrola
upwr_reference   <- "#8b8175"  # linia referencyjna (dashed)

# Kolory "neutralne" interfejsu wykresu
upwr_bg          <- "#f5f0e6"  # tło kremowe
upwr_panel       <- "#faf6ec"  # tło panelu (jaśniejsze)
upwr_rule        <- "#d9cfbc"  # linie siatki, osie
upwr_ink         <- "#1c1a17"  # tekst główny
upwr_ink_soft    <- "#3d3832"  # tekst drugorzędny


# ---- Paleta kategoryczna (8 kolorów) -----------------------------------------
# UWAGA: burgund świadomie pominięty — jest zarezerwowany dla akcentu.

upwr_cat <- c(
  grafit   = "#2d3b3d",
  bursztyn = "#c08540",
  niebo    = "#6a9dc4",
  szalwia  = "#4a8a6a",
  kurkuma  = "#cbb858",
  indygo   = "#3a5a8a",
  terakota = "#b25838",
  wrzos    = "#a07894"
)


# ---- Skale ciągłe ------------------------------------------------------------

# Sekwencyjna burgundowa (7 punktów)
upwr_seq_burgundy <- c(
  "#f5f0e6", "#e8d5c8", "#d9a89a", "#c27570",
  "#a04550", "#7a2535", "#4a0e1e"
)

# Sekwencyjna złota (7 punktów) — gdy burgund koliduje z akcentem
upwr_seq_gold <- c(
  "#f5f0e6", "#ecdcb8", "#d9bc7a", "#c29a4a",
  "#a67a2a", "#7a5a1a", "#4a3510"
)

# Rozbieżna (9 punktów) — burgund ↔ zielono-szary, środek = tło strony
upwr_div <- c(
  "#4a0e1e", "#8a3a4a", "#c27a80", "#e8d5c8",
  "#f5f0e6",
  "#c8d8c8", "#7a9a8a", "#3d5a4a", "#1e3530"
)

# Porządkowa 5 poziomów (jednokierunkowa)
upwr_ord5 <- c("#ecdcb8", "#c9a85a", "#a6754a", "#8a3a4a", "#4a0e1e")

# Porządkowa 7 poziomów (Likert, z neutralnym środkiem)
upwr_ord7 <- c(
  "#4a0e1e", "#8a3a4a", "#c27a80",
  "#c4b8a8",
  "#7a8a7a", "#3d5a4a", "#1e3530"
)


# ---- Helpery ggplot2 ---------------------------------------------------------
# Wrappery które od razu podpinają odpowiednią paletę do ggplot2.
# Używaj ich zamiast pisać scale_*_manual() ręcznie.

#' Skala kategoryczna (fill)
#' @param ... argumenty przekazywane do ggplot2::discrete_scale
scale_fill_upwr <- function(...) {
  ggplot2::scale_fill_manual(values = unname(upwr_cat), ...)
}

#' Skala kategoryczna (color)
scale_color_upwr <- function(...) {
  ggplot2::scale_color_manual(values = unname(upwr_cat), ...)
}

#' Skala sekwencyjna (fill) — burgund domyślnie, opcjonalnie złoto
#' @param variant "burgundy" (domyślnie) lub "gold"
scale_fill_upwr_seq <- function(..., variant = c("burgundy", "gold")) {
  variant <- match.arg(variant)
  pal <- if (variant == "burgundy") upwr_seq_burgundy else upwr_seq_gold
  ggplot2::scale_fill_gradientn(colours = pal, ...)
}

#' Skala sekwencyjna (color)
scale_color_upwr_seq <- function(..., variant = c("burgundy", "gold")) {
  variant <- match.arg(variant)
  pal <- if (variant == "burgundy") upwr_seq_burgundy else upwr_seq_gold
  ggplot2::scale_color_gradientn(colours = pal, ...)
}

#' Skala rozbieżna (fill) — dla korelacji, residuów, różnic
#' @param midpoint wartość w środku skali (zwykle 0)
scale_fill_upwr_div <- function(..., midpoint = 0) {
  ggplot2::scale_fill_gradient2(
    low      = upwr_div[1],
    mid      = upwr_div[5],
    high     = upwr_div[9],
    midpoint = midpoint,
    ...
  )
}

#' Skala rozbieżna (color)
scale_color_upwr_div <- function(..., midpoint = 0) {
  ggplot2::scale_color_gradient2(
    low      = upwr_div[1],
    mid      = upwr_div[5],
    high     = upwr_div[9],
    midpoint = midpoint,
    ...
  )
}

#' Skala porządkowa (fill)
#' @param n liczba poziomów: 5 lub 7
scale_fill_upwr_ord <- function(..., n = 5) {
  pal <- switch(as.character(n),
    "5" = upwr_ord5,
    "7" = upwr_ord7,
    stop("Dostępne są tylko warianty n = 5 lub n = 7")
  )
  ggplot2::scale_fill_manual(values = pal, ...)
}


# ---- Funkcje pomocnicze ------------------------------------------------------

#' Zwraca n kolorów z palety kategorycznej (bez recyklingu)
#' @param n liczba żądanych kolorów (maks. 8)
upwr_cat_n <- function(n) {
  if (n > length(upwr_cat)) {
    stop(
      "Paleta kategoryczna ma 8 kolorów, zażądano ",
      n,
      ". Rozważ inne kodowanie (np. kształt + kolor) albo zmianę strategii wizualizacji."
    )
  }
  unname(upwr_cat[seq_len(n)])
}

#' Podgląd palety w RStudio/Quarto (prosty wykres słupkowy)
#' @param palette nazwa palety lub wektor kolorów
upwr_show_palette <- function(palette = upwr_cat) {
  if (is.character(palette) && length(palette) == 1) {
    palette <- get(palette, envir = globalenv())
  }
  palette_names <- names(palette)
  if (is.null(palette_names)) {
    palette_names <- seq_along(palette)
  }
  graphics::barplot(
    rep(1, length(palette)),
    col    = palette,
    border = NA,
    axes   = FALSE,
    names.arg = palette_names,
    las    = 2
  )
}


# ---- Mapowanie "typ zmiennej" → kolor kategoryczny --------------------------
# Używane w całym wykładzie typy-danych do spójnego kodowania typów zmiennych
# (nominalna/porządkowa/dyskretna/ciągła). Wartości pochodzą z upwr_cat.

type_colors <- c(
  nominalna           = unname(upwr_cat["terakota"]),
  porzadkowa          = unname(upwr_cat["bursztyn"]),
  ilosciowa_dyskretna = unname(upwr_cat["niebo"]),
  ilosciowa_ciagla    = unname(upwr_cat["szalwia"])
)


# ---- Notka ------------------------------------------------------------------
# W razie potrzeby rozszerzenia palety:
#   - nie dodawaj kolorów ad-hoc w kodzie wykresów,
#   - dodaj nową stałą lub paletę w tym pliku,
#   - zaktualizuj palette-guide.md (reguły użycia),
#   - sprawdź CVD-safety przed dodaniem kolejnych kolorów kategorycznych.
