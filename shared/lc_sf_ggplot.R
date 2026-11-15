# ============================================================================
# Wykres ggplot → rycina „opis z R” (lc_spec)
# ============================================================================
# Wykres na stałych danych, dotąd rysowany na serwerze (zoom_plot_server), można
# przenieść na stronę statyczną bez przepisywania: sf_ggplot() buduje obiekt
# ggplot (ggplot_build()), odczytuje warstwy po obliczeniach statystyk, zakresy
# osi, nazwy kategorii i legendę i składa z nich opis dla wbudowanej ryciny
# lc_spec (shared/lc_stat_figures.js). Barwy z palety (palette.R) zamienia na
# tokeny motywu („@niebo”, „@accent/0.4”), więc rycina zmienia się w trybie ciemnym.
#
#   lc_sf_ggplot(p, aria = "Opis wykresu", ratio = "2/1")
#   lc_sf_ggplot(list(raw = p1, clean = p2), control = lc_segmented("view", …))   # warianty
#
# Obsługiwane geometrie: punkty (także jitter), linie i ścieżki, smooth (z pasem),
# słupki i histogramy, prostokąty, kafelki, teksty i etykiety, hline/vline/abline,
# odcinki (ze strzałkami), pudełka (z odstającymi), słupki błędów, wstęgi i pola,
# gęstość. Panele facet_wrap()/facet_grid() stają się osobnymi wykresami ryciny
# (z nagłówkami). Nieobsługiwana geometria daje ostrzeżenie przy budowaniu.
# Wymaga: lc_stat_figures.R, palette.R.

.sf_gg_tokens <- function() {
  pal <- c(accent = upwr_accent, secondary = upwr_secondary, ref = upwr_reference, ink = upwr_ink,
           rule = upwr_rule, upwr_cat)
  stats::setNames(paste0("@", names(pal)), toupper(unname(pal)))
}

# Barwa ggplot (nazwa albo #RRGGBB[AA]) i przezroczystość → token motywu albo rgba().
sf_gg_color <- function(col, alpha = NA) {
  if (is.null(col) || length(col) == 0 || is.na(col)) return(NULL)
  rgb <- grDevices::col2rgb(col, alpha = TRUE)
  a <- if (!is.na(alpha)) alpha else rgb[4] / 255
  hex <- toupper(sprintf("#%02X%02X%02X", rgb[1], rgb[2], rgb[3]))
  if (hex == "#FFFFFF") tok <- "@surface" else tok <- .sf_gg_tokens()[hex]
  if (!is.na(tok)) return(if (a < 1) paste0(tok, "/", round(a, 3)) else unname(tok))
  if (a < 1) sprintf("rgba(%d,%d,%d,%s)", rgb[1], rgb[2], rgb[3], format(round(a, 3))) else hex
}
.sf_gg_dash <- function(lt) {
  if (is.null(lt) || is.na(lt)) return(NULL)
  lt <- as.character(lt)
  if (lt %in% c("solid", "1", "blank", "0")) return(NULL)
  if (lt %in% c("dashed", "2", "longdash", "5")) return(sf_dash)
  if (lt %in% c("dotted", "3")) return(sf_dot)
  if (lt %in% c("dotdash", "4", "twodash", "6")) return(c(6, 3, 2, 3))
  if (grepl("^[0-9A-Fa-f]+$", lt)) return(strtoi(strsplit(lt, "")[[1]], 16L) * 1.6)
  sf_dash
}
.sf_gg_lw <- function(lw) max(1, round(2.2 * (if (is.null(lw) || is.na(lw)) 0.5 else lw), 2))
.sf_gg_size <- function(s) round(3 * (if (is.null(s) || is.na(s)) 1.5 else s) + 1, 1)
.sf_gg_first <- function(d, col, default = NA) if (!is.null(d[[col]])) d[[col]][1] else default
.sf_gg_alpha <- function(a, default = NA) if (is.null(a) || all(is.na(a))) default else a

# Jedna warstwa ggplot (dane po ggplot_build dla jednego panelu) → lista warstw opisu.
.sf_gg_layer <- function(geom, d, layer, xr, yr) {
  if (!nrow(d)) return(list())
  cls <- class(geom)[1]
  by_group <- function(f) unname(lapply(split(d, d$group), f))
  rect_layers <- function(d) {
    fills <- vapply(seq_len(nrow(d)), function(i) sf_gg_color(d$fill[i], .sf_gg_alpha(d$alpha[i])) %||% "@secondary", "")
    if (all(abs(d$ymin) < 1e-12)) {
      list(Filter(Negate(is.null), list(type = "bins", data = lapply(seq_len(nrow(d)), function(i) c(d$xmin[i], d$xmax[i], d$ymax[i])),
           colors = as.list(fills), stroke = if (!is.na(.sf_gg_first(d, "colour"))) sf_gg_color(d$colour[1]))))
    } else {
      list(list(type = "cells", items = lapply(seq_len(nrow(d)), function(i)
        list(r = c(d$xmin[i], d$xmax[i], d$ymin[i], d$ymax[i]), color = fills[i]))))
    }
  }
  switch(cls,
    GeomPoint = {
      d <- d[!is.na(d$x) & !is.na(d$y), , drop = FALSE]
      if (!nrow(d)) return(list())
      cols <- vapply(seq_len(nrow(d)), function(i) sf_gg_color(d$colour[i]) %||% "@secondary", "")
      a <- .sf_gg_alpha(d$alpha[1], 1)
      shape <- c(`16` = "circle", `19` = "circle", `1` = "emptyCircle", `21` = "circle", `17` = "triangle",
                 `2` = "emptyTriangle", `15` = "rect", `0` = "emptyRect", `18` = "diamond", `23` = "diamond")[as.character(d$shape[1])]
      pts <- list(type = "points", data = sf_xy(d$x, d$y), size = .sf_gg_size(d$size[1]),
                  alpha = if (a < 1) a, symbol = if (!is.na(shape) && shape != "circle") unname(shape))
      if (length(unique(cols)) == 1) pts$color <- cols[1] else pts$colors <- as.list(cols)
      # Kształt 21/23 z wypełnieniem: kolor wypełnienia.
      if (d$shape[1] %in% c(21, 22, 23, 24, 25) && !is.na(.sf_gg_first(d, "fill"))) {
        pts$color <- sf_gg_color(d$fill[1]); pts$colors <- NULL
      }
      list(Filter(Negate(is.null), pts))
    },
    GeomLine = , GeomPath = , GeomStep = unlist(by_group(function(g) {
      if (cls == "GeomLine") g <- g[order(g$x), ]
      # Brak wartości przerywa linię (jak w ggplot): osobny odcinek dla każdego ciągu bez braków.
      ok <- !is.na(g$x) & !is.na(g$y)
      runs <- split(which(ok), cumsum(!ok)[ok])
      lapply(unname(runs[lengths(runs) > 1]), function(r) sf_line(g$x[r], g$y[r],
        color = sf_gg_color(g$colour[1], .sf_gg_alpha(g$alpha[1])) %||% "@ink",
        width = .sf_gg_lw(g$linewidth[1]), dash = .sf_gg_dash(g$linetype[1])))
    }), recursive = FALSE),
    GeomSmooth = unlist(by_group(function(g) {
      g <- g[order(g$x), ]
      out <- list()
      if (!is.null(g$ymin) && !all(is.na(g$ymin)) && isTRUE(layer$stat_params$se %||% TRUE)) {
        out <- c(out, list(sf_ribbon(g$x, g$ymin, g$ymax, sf_gg_color(g$fill[1], .sf_gg_alpha(g$alpha[1], 0.4)))))
      }
      c(out, list(sf_line(g$x, g$y, color = sf_gg_color(g$colour[1]) %||% "@niebo", width = .sf_gg_lw(g$linewidth[1]),
                          dash = .sf_gg_dash(g$linetype[1]))))
    }), recursive = FALSE),
    GeomBar = , GeomCol = , GeomRect = rect_layers(d),
    GeomTile = rect_layers(transform(d,
      xmin = if (is.null(d$xmin)) d$x - d$width / 2 else d$xmin, xmax = if (is.null(d$xmax)) d$x + d$width / 2 else d$xmax,
      ymin = if (is.null(d$ymin)) d$y - d$height / 2 else d$ymin, ymax = if (is.null(d$ymax)) d$y + d$height / 2 else d$ymax)),
    GeomText = , GeomLabel = lapply(seq_len(nrow(d)), function(i) {
      hj <- suppressWarnings(as.numeric(d$hjust[i])); vj <- suppressWarnings(as.numeric(d$vjust[i]))
      if (is.na(hj)) hj <- c(left = 0, center = 0.5, middle = 0.5, right = 1, inward = 0.5, outward = 0.5)[[as.character(d$hjust[i])]]
      if (is.na(vj)) vj <- c(bottom = 0, center = 0.5, middle = 0.5, top = 1, inward = 0.5, outward = 0.5)[[as.character(d$vjust[i])]]
      x <- d$x[i]; y <- d$y[i]
      if (is.infinite(x)) x <- if (x > 0) xr[2] else xr[1]
      if (is.infinite(y)) y <- if (y > 0) yr[2] else yr[1]
      # vjust poza [0, 1]: przesunięcie o wysokości wiersza (ok. 6% wysokości wykresu na wiersz).
      if (vj > 1) y <- y - (vj - 1) * 0.06 * diff(yr)
      if (vj < 0) y <- y - vj * 0.06 * diff(yr)
      face <- .sf_gg_first(d[i, , drop = FALSE], "fontface", 1)
      sf_text(x, y, as.character(d$label[i]), color = sf_gg_color(d$colour[i]) %||% "@ink",
              align = if (hj < 0.25) "left" else if (hj > 0.75) "right" else NULL,
              valign = if (vj < 0.25) "bottom" else if (vj > 0.75) "top" else "middle",
              bold = if (face %in% c(2, "bold", 4, "bold.italic")) TRUE, size = round((d$size[i] %||% 3.88) / 3.9, 2),
              halo = if (cls == "GeomLabel") TRUE)
    }),
    GeomHline = lapply(seq_len(nrow(d)), function(i) sf_line(xr, rep(d$yintercept[i], 2),
      color = sf_gg_color(d$colour[i], .sf_gg_alpha(d$alpha[i])) %||% "@ref", width = .sf_gg_lw(d$linewidth[i]), dash = .sf_gg_dash(d$linetype[i]))),
    GeomVline = lapply(seq_len(nrow(d)), function(i) sf_vline(d$xintercept[i],
      color = sf_gg_color(d$colour[i], .sf_gg_alpha(d$alpha[i])) %||% "@ref", width = .sf_gg_lw(d$linewidth[i]), dash = .sf_gg_dash(d$linetype[i]))),
    GeomAbline = lapply(seq_len(nrow(d)), function(i) sf_line(xr, d$intercept[i] + d$slope[i] * xr,
      color = sf_gg_color(d$colour[i], .sf_gg_alpha(d$alpha[i])) %||% "@ref", width = .sf_gg_lw(d$linewidth[i]), dash = .sf_gg_dash(d$linetype[i]))),
    GeomSegment = {
      arrow <- !is.null(layer$geom_params$arrow)
      lapply(seq_len(nrow(d)), function(i) {
        col <- sf_gg_color(d$colour[i], .sf_gg_alpha(d$alpha[i])) %||% "@ink"
        if (arrow) list(type = "arrow", from = c(d$x[i], d$y[i]), to = c(d$xend[i], d$yend[i]), color = col, width = .sf_gg_lw(d$linewidth[i]))
        else sf_line(c(d$x[i], d$xend[i]), c(d$y[i], d$yend[i]), color = col, width = .sf_gg_lw(d$linewidth[i]), dash = .sf_gg_dash(d$linetype[i]))
      })
    },
    GeomBoxplot = {
      boxes <- lapply(seq_len(nrow(d)), function(i) list(pos = d$x[i], width = d$xmax[i] - d$xmin[i], q1 = d$lower[i],
        med = d$middle[i], q3 = d$upper[i], lo = d$ymin[i], hi = d$ymax[i],
        fill = sf_gg_color(d$fill[i], .sf_gg_alpha(d$alpha[i])) %||% "@surface"))
      out <- list(list(type = "box", stroke = sf_gg_color(d$colour[1]) %||% "@ink", boxes = boxes))
      if (!is.null(d$outliers)) {
        ox <- unlist(lapply(seq_len(nrow(d)), function(i) rep(d$x[i], length(d$outliers[[i]]))))
        if (length(ox)) out <- c(out, list(sf_points(ox, unlist(d$outliers), color = sf_gg_color(d$colour[1]) %||% "@ink", size = 6)))
      }
      out
    },
    GeomErrorbar = lapply(seq_len(nrow(d)), function(i) {
      col <- sf_gg_color(d$colour[i]) %||% "@ink"
      if (!is.null(d$xmin) && !is.null(d$ymin) && !is.na(d$xmin[i]) && is.na(d$x[i] %||% NA)) NULL
      if (isTRUE(layer$geom_params$orientation == "y") || (is.null(d$ymin) || all(is.na(d$ymin)))) {
        h <- (d$ymax[i] %||% (d$y[i] + 0.1)) - (d$ymin[i] %||% (d$y[i] - 0.1))
        sf_errorbars_h(d$xmin[i], d$xmax[i], d$y[i], cap = if (is.finite(h)) h else 0.2, color = col, width = .sf_gg_lw(d$linewidth[i]))
      } else {
        sf_errorbars_v(d$ymin[i], d$ymax[i], d$x[i], cap = d$xmax[i] - d$xmin[i], color = col, width = .sf_gg_lw(d$linewidth[i]))
      }
    }),
    GeomRibbon = , GeomArea = , GeomDensity = unlist(by_group(function(g) {
      g <- g[order(g$x), ]
      lo <- if (!is.null(g$ymin)) g$ymin else rep(0, nrow(g))
      out <- list(sf_ribbon(g$x, lo, g$ymax, sf_gg_color(g$fill[1], .sf_gg_alpha(g$alpha[1], if (cls == "GeomDensity") 1 else 1)) %||% "@secondary/0.3"))
      if (cls != "GeomRibbon" && !is.na(.sf_gg_first(g, "colour"))) out <- c(out, list(sf_line(g$x, g$ymax, color = sf_gg_color(g$colour[1]), width = .sf_gg_lw(g$linewidth[1]))))
      out
    }), recursive = FALSE),
    GeomBlank = list(),
    { warning("sf_ggplot: pominięta geometria ", cls, call. = FALSE); list() })
}

# Legenda dyskretnych skal koloru i wypełnienia (bez guide = "none" i legend.position = "none").
.sf_gg_legend <- function(b) {
  pos <- b$plot$theme$legend.position %||% "right"
  if (identical(pos, "none")) return(NULL)
  items <- list()
  for (s in b$plot$scales$scales) {
    aes <- intersect(s$aesthetics, c("colour", "fill"))
    if (!length(aes) || !s$is_discrete()) next
    if (identical(s$guide, "none") || identical(b$plot$guides$guides[[aes[1]]], "none")) next
    br <- s$get_breaks(); br <- br[!is.na(br)]
    if (!length(br)) next
    cols <- s$map(br); labs <- s$get_labels(br)
    items <- c(items, lapply(seq_along(br), function(i) sf_legend(labs[i], list(sf_gg_color(cols[i]) %||% "@ink"), box = aes[1] == "fill")[[1]]))
  }
  if (length(items)) unname(items[!duplicated(vapply(items, `[[`, "", "text"))]) else NULL
}

sf_ggplot <- function(p, aria = "Wykres", ...) {
  b <- ggplot2::ggplot_build(p)
  lay <- b$layout$layout
  labs <- b$plot$labels
  lab_text <- function(v) if (is.null(v)) "" else if (is.language(v) || is.expression(v)) paste(deparse(v), collapse = "") else as.character(v)
  # Nazwa osi: etykieta z labs(); bez niej nazwa zmiennej z mapowania (jak w ggplot).
  axis_name <- function(a) {
    if (a %in% names(p$labels) && is.null(p$labels[[a]])) return("")   # labs(x = NULL): bez nazwy osi
    v <- labs[[a]]
    if (inherits(v, "waiver") || is.null(v)) { m <- p$mapping[[a]]; v <- if (is.null(m)) "" else rlang::as_label(m) }
    lab_text(v)
  }
  legend <- .sf_gg_legend(b)
  specs <- lapply(seq_len(nrow(lay)), function(k) {
    pp <- b$layout$panel_params[[k]]
    xr <- pp$x.range; yr <- pp$y.range
    layers <- unlist(lapply(seq_along(p$layers), function(i) {
      d <- b$data[[i]]; d <- d[d$PANEL == lay$PANEL[k], , drop = FALSE]
      .sf_gg_layer(p$layers[[i]]$geom, d, p$layers[[i]], xr, yr)
    }), recursive = FALSE)
    extra <- list()
    # Osie kategorii i osie liczbowe z własnymi podpisami (miesiące, daty): podpisy z ggplot.
    disc <- function(axis) {
      sc <- pp[[axis]]
      if (is.null(sc)) return(NULL)
      is_disc <- isTRUE(tryCatch(sc$is_discrete(), error = function(e) FALSE))
      br <- sc$get_breaks(); lb <- as.character(sc$get_labels())
      # Oś liczbowa, której podpisy to te same liczby (także z odstępem tysięcy): podpisy rysuje silnik.
      if (!is_disc) {
        ok <- !is.na(br) & !is.na(lb)
        num <- suppressWarnings(as.numeric(gsub("[^0-9.eE+-]", "", gsub("−", "-", lb[ok]))))
        if (!any(ok) || (is.numeric(br) && all(!is.na(num) & abs(num - br[ok]) <= 1e-9 * pmax(1, abs(br[ok]))))) return(NULL)
      }
      at <- if (is.numeric(br)) br else seq_along(lb)
      keep <- !is.na(at) & !is.na(lb)
      if (!all(abs(at[keep] - round(at[keep])) < 1e-9)) return(NULL)   # podpisy osi muszą stać na liczbach całkowitych
      sf_labels(lb[keep], round(at[keep]))
    }
    extra$xLabels <- disc("x"); extra$yLabels <- disc("y")
    if (!is.null(legend) && k == 1) extra$legend <- legend
    th <- p$theme
    blank <- function(...) any(vapply(list(...), function(e) inherits(th[[e]], "element_blank"), logical(1)))
    if (blank("axis.text.y")) extra$hideY <- TRUE
    if (blank("axis.text.x") && is.null(extra$xLabels)) extra$xTicks <- list()
    # Krok podziałek ggplot na osi liczbowej (gdy równy).
    step_of <- function(sc) {
      br <- tryCatch(sc$get_breaks(), error = function(e) NULL)
      if (is.null(br) || !is.numeric(br)) return(NULL)
      br <- br[!is.na(br)]
      if (length(br) < 2) return(NULL)
      dd <- diff(br)
      if (all(abs(dd - dd[1]) < 1e-9 * max(1, abs(dd[1])))) dd[1] else NULL
    }
    # (Stały krok ECharts liczy od krańca osi, nie od wielokrotności kroku — dlatego tylko
    # podziałki całkowite, gdy ggplot ma krok 1, np. liczba nocy.)
    if (is.null(extra$xLabels) && is.null(extra$xTicks) && !is.null(pp$x) && !isTRUE(tryCatch(pp$x$is_discrete(), error = function(e) FALSE)) &&
        identical(step_of(pp$x), 1)) extra$xInt <- TRUE
    if (blank("panel.grid", "panel.grid.major", "panel.grid.major.x")) extra$noXGrid <- TRUE
    if (blank("panel.grid", "panel.grid.major", "panel.grid.major.y")) extra$noYGrid <- TRUE
    # Podpisy osi z odstępem tysięcy (scales::label_number(big.mark = " ")).
    big <- function(sc) { lb <- tryCatch(as.character(sc$get_labels()), error = function(e) character()); any(grepl("[0-9] [0-9]{3}", lb)) }
    if (!is.null(pp$x) && big(pp$x)) extra$xBig <- TRUE
    if (!is.null(pp$y) && big(pp$y)) extra$yBig <- TRUE
    do.call(sf_spec, c(list(xr, yr, layers, axis_name("x"), axis_name("y"), aria = aria), extra, list(...)))
  })
  if (length(specs) == 1) return(specs[[1]])
  # Zmienne paneli z definicji facet (układ ma też kolumny techniczne, np. COORD w ggplot2 4.x).
  fp <- p$facet$params
  fvars <- intersect(names(c(fp$facets, fp$rows, fp$cols)), names(lay))
  headings <- if (length(fvars)) apply(lay[fvars], 1, function(r) paste(trimws(r), collapse = " · ")) else NULL
  list(charts = specs, headings = unname(headings))
}

# Rycina z wykresu ggplot (albo nazwanej listy wykresów — wariantów dla kontrolki).
# Pozostałe argumenty jak w lc_sf_spec() / lc_sf_figure() (control, html, reads, ratio, …).
lc_sf_ggplot <- function(p, aria = "Wykres", ..., spec_args = list()) {
  one <- function(q) do.call(sf_ggplot, c(list(q, aria = aria), spec_args))
  if (inherits(p, "ggplot")) {
    s <- one(p)
    if (!is.null(s$charts)) {
      return(lc_sf_spec(list(charts = s$charts), charts = length(s$charts), headings = s$headings,
                        legend = !is.null(s$charts[[1]]$legend), ...))
    }
    return(lc_sf_spec(s, legend = !is.null(s$legend), ...))
  }
  variants <- lapply(p, one)
  multi <- !is.null(variants[[1]]$charts)
  if (multi) variants <- lapply(variants, function(s) list(charts = s$charts))
  has_legend <- any(vapply(variants, function(s) !is.null((if (multi) s$charts[[1]] else s)$legend), logical(1)))
  lc_sf_spec(variants = variants, charts = if (multi) length(variants[[1]]$charts), legend = has_legend, ...)
}
