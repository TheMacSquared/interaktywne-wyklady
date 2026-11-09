# Wektorowe sceny CI: istniejące modele i skale R, natywne osie i serie ECharts.
# Adapter lokalny dla tego wykładu; nie używa PNG ani zrzutów SVG z R.
ci_echarts_ui <- function(id, ratio = NULL, max_height = NULL, zoom = TRUE) {
  tags$div(class = "ci-ec-plot lc-plot", `data-chart-id` = id,
    style = paste0("position:relative;",
      if (!is.null(ratio)) paste0("--lc-plot-ratio:",ratio,";"),
      if (!is.null(max_height)) paste0("--lc-plot-max:", max_height, ";")),
    if (zoom) tags$div(style="position:absolute;right:.3em;top:.3em;z-index:3;",lc_chart_zoom_button()),
    tags$div(class = "lc-ec", role = "img", `aria-label` = "Przedział ufności"),
    tags$div(class = "ci-ec-config", style = "display:none", uiOutput(paste0(id, "_ec")))
  )
}

ci_echarts_spec <- function(plot) {
  panels <- list()
  visit <- function(p, box = c(0, 0, 1, 1)) {
    if (inherits(p, "patchwork")) {
      n <- length(p); layout <- p$patches$layout
      nc <- if (!is.null(layout$ncol)) layout$ncol else if (!is.null(layout$nrow)) ceiling(n/layout$nrow) else n
      nr <- ceiling(n/nc)
      widths <- rep_len(if (is.null(layout$widths)) 1 else as.numeric(layout$widths), nc)
      heights <- rep_len(if (is.null(layout$heights)) 1 else as.numeric(layout$heights), nr)
      widths <- widths/sum(widths); heights <- heights/sum(heights)
      for (i in seq_len(n)) {
        col <- (i-1) %% nc + 1; row <- (i-1) %/% nc + 1
        visit(p[[i]], c(box[1]+sum(widths[seq_len(col-1)])*box[3],
          box[2]+sum(heights[seq_len(row-1)])*box[4], widths[col]*box[3], heights[row]*box[4]))
      }
      return(invisible(NULL))
    }
    if (inherits(p, "spacer")) return(invisible(NULL))
    b <- ggplot2::ggplot_build(p)
    layout <- b$layout$layout
    theme <- b$plot$theme
    blank <- function(name) inherits(theme[[name]], "element_blank")
    for (pi in seq_along(b$layout$panel_params)) {
      pp <- b$layout$panel_params[[pi]]
      xr <- pp$x.range; yr <- pp$y.range
      row <- layout$ROW[pi]; col <- layout$COL[pi]
      nc <- max(layout$COL); nr <- max(layout$ROW)
      pb <- c(box[1]+(col-1)*box[3]/nc, box[2]+(row-1)*box[4]/nr, box[3]/nc, box[4]/nr)
      axis <- function(a, range, name, show) {
        breaks <- a$breaks
        # Panel estymat w kroku 1 może nie mieć jeszcze żadnej warstwy danych.
        if (!length(breaks) && is.atomic(a$scale$breaks)) {
          breaks <- a$scale$breaks
          labels <- if (is.atomic(a$scale$labels) && length(a$scale$labels)==length(breaks))
            a$scale$labels else as.character(breaks)
        } else labels <- if (length(breaks)) a$get_labels() else character()
        if (isTRUE(a$scale$is_discrete())) breaks <- a$scale$map(breaks)
        keep <- is.finite(breaks)
        list(range = range, breaks = as.list(as.numeric(breaks[keep])), labels = as.list(as.character(labels[keep])),
          name = if (is.null(name)) "" else as.character(name), show = show)
      }
      elements <- list()
      add <- function(type, coords, r, extra = list()) {
        value <- function(key, default) if (key %in% names(r) && !is.na(r[[key]][1])) r[[key]][1] else default
        if (is.matrix(coords)) {
          coords[is.infinite(coords[,1]),1] <- ifelse(coords[is.infinite(coords[,1]),1]>0,xr[2],xr[1])
          coords[is.infinite(coords[,2]),2] <- ifelse(coords[is.infinite(coords[,2]),2]>0,yr[2],yr[1])
        } else {
          for (j in seq_along(coords)) if (is.infinite(coords[j])) {
            range <- if (j %% 2 == 1) xr else yr
            coords[j] <- if (coords[j]>0) range[2] else range[1]
          }
        }
        item <- c(list(type=type, coords=coords,
          color=as.character(value("colour", if (type == "rect") "transparent" else "#333333")), fill=as.character(value("fill", "transparent")),
          alpha=as.numeric(value("alpha", 1)), width=as.numeric(value("linewidth", .6)),
          size=as.numeric(value("size", 3)), shape=as.character(value("shape", 16)),
          dash=as.character(value("linetype", "solid"))), extra)
        elements[[length(elements)+1]] <<- item
      }
      for (li in seq_along(b$data)) {
        d <- b$data[[li]]
        if (!nrow(d)) next
        d <- d[as.integer(d$PANEL)==pi,,drop=FALSE]
        if (!nrow(d)) next
        geom <- class(b$plot$layers[[li]]$geom)[1]
        if (geom %in% c("GeomLine", "GeomPath")) {
          for (g in unique(d$group)) {
            dd <- d[d$group==g,,drop=FALSE]
            add("line", unname(as.matrix(dd[c("x","y")])), dd[1,,drop=FALSE])
          }
          next
        }
        for (ri in seq_len(nrow(d))) {
          r <- d[ri,,drop=FALSE]
          if (geom == "GeomPoint") add("point", c(r$x,r$y),r)
          else if (geom == "GeomText") add("text", c(r$x,r$y),r,
            list(text=as.character(r$label), hjust=r$hjust, vjust=r$vjust, angle=r$angle))
          else if (geom == "GeomVline") add("line", rbind(c(r$xintercept,yr[1]),c(r$xintercept,yr[2])),r)
          else if (geom == "GeomHline") add("line", rbind(c(xr[1],r$yintercept),c(xr[2],r$yintercept)),r)
          else if (geom == "GeomSegment") add("line",rbind(c(r$x,r$y),c(r$xend,r$yend)),r)
          else if (geom %in% c("GeomRect","GeomCol","GeomBar")) add("rect",c(r$xmin,r$ymin,r$xmax,r$ymax),r)
          else if (geom == "GeomErrorbar") {
            if (isTRUE(r$flipped_aes)) {
              add("line",rbind(c(r$xmin,r$y),c(r$xmax,r$y)),r)
              for (x in c(r$xmin,r$xmax)) add("line",rbind(c(x,r$ymin),c(x,r$ymax)),r)
            } else {
              add("line",rbind(c(r$x,r$ymin),c(r$x,r$ymax)),r)
              for (y in c(r$ymin,r$ymax)) add("line",rbind(c(r$xmin,y),c(r$xmax,y)),r)
            }
          } else stop("Nieobsługiwana geometria sceny CI: ",geom)
        }
      }
      facet_keys <- setdiff(names(layout), c("PANEL","ROW","COL","SCALE_X","SCALE_Y","COORD"))
      facet_label <- if (nrow(layout)>1 && length(facet_keys)) paste(as.character(layout[pi,facet_keys]),collapse=" · ") else ""
      panels[[length(panels)+1]] <<- list(box=pb,
        x=axis(pp$x,xr,b$plot$labels$x,!blank("axis.text.x")),
        y=axis(pp$y,yr,b$plot$labels$y,!blank("axis.text.y")),
        elements=elements, label=facet_label)
    }
  }
  visit(plot)
  color_keys <- stats::setNames(as.list(names(upwr_cat)), unname(upwr_cat))
  color_keys[[upwr_accent]] <- "accent"
  color_keys[[upwr_secondary]] <- "secondary"
  color_keys[[upwr_reference]] <- "reference"
  color_keys[["#8e44ad"]] <- "indygo"
  color_keys[["#f1c40f"]] <- "bursztyn"
  list(panels=panels, color_keys=color_keys)
}

ci_echarts_server <- function(id, plot, session = shiny::getDefaultReactiveDomain()) {
  output <- session$output
  output[[paste0(id,"_ec")]] <- shiny::renderUI({
    spec <- ci_echarts_spec(plot())
    tags$span(`data-ci-spec` = jsonlite::toJSON(spec, auto_unbox=TRUE,
      null="null", na="null", digits=12))
  })
  shiny::outputOptions(output, paste0(id,"_ec"), suspendWhenHidden=FALSE)
}
