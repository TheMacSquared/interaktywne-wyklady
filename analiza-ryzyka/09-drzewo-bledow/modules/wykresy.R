# Ryciny wykładu 09 liczone w przeglądarce (modules/wykresy.js, silnik lcSF).
# Tutaj: rysunek małego drzewa błędów składany przy budowaniu strony — liczby
# przy węzłach (elementy z data-v) wpisuje przeglądarka z suwaków.
# Wymaga: f9p_svg() ze stop_proto_common.R.

# Drzewo: TOP (AND) nad inicjacją i bramką OR dwóch niesprawności zabezpieczeń.
# Współrzędne jak w dawnym wykresie ggplot (jednostka = 110 px, oś Y w górę).
f9_tree_svg <- function() {
  u <- 110; x0 <- .2; y1 <- 3.3
  px <- function(x) round((x - x0) * u, 1)
  py <- function(y) round((y1 - y) * u, 1)
  nodes <- data.frame(
    x = c(2, 1, 3, 2.5, 3.5), y = c(3, 2, 2, 1, 1),
    type = c("top", "leaf", "gate", "leaf", "leaf"),
    key = c("tree_top", "tree_init", "tree_or", "tree_detect", "tree_suppress")
  )
  lines <- list(c("TOP (AND)"), c("Inicjacja"), c("OR"), c("Brak", "detekcji"), c("Brak", "tłumienia"))
  edges <- data.frame(x = c(2, 2, 3, 3), y = c(3, 3, 2, 2), xend = c(1, 3, 2.5, 3.5), yend = c(2, 2, 1, 1))
  colour <- c(leaf = "var(--upwr-cat-grafit)", gate = "var(--upwr-cat-bursztyn)", top = "var(--upwr-cat-niebo)")
  mark <- function(type, cx, cy, r = 13) switch(type,
    leaf = sprintf('<circle cx="%s" cy="%s" r="%s" fill="%s"/>', cx, cy, r, colour[[type]]),
    gate = sprintf('<path d="M%s %s L%s %s L%s %s Z" fill="%s"/>', cx, cy - r * 1.15, cx + r * 1.1, cy + r * .85, cx - r * 1.1, cy + r * .85, colour[[type]]),
    top  = sprintf('<rect x="%s" y="%s" width="%s" height="%s" fill="%s"/>', cx - r, cy - r, 2 * r, 2 * r, colour[[type]])
  )
  # Napisy liści pod węzłem, napisy bramek z boku, żeby nie nachodziły na gałęzie.
  label <- function(i) {
    n <- nodes[i, ]; txt <- c(lines[[i]], sprintf('<tspan data-v="%s">–</tspan>', n$key))
    if (n$type == "leaf") {
      sprintf('<text x="%s" y="%s" text-anchor="middle" font-size="16">%s</text>', px(n$x), py(n$y - .25) + 12,
        paste0(sprintf('<tspan x="%s" dy="%s">%s</tspan>', px(n$x), c(0, rep(18, length(txt) - 1)), txt), collapse = ""))
    } else {
      sprintf('<text x="%s" y="%s" font-size="16">%s</text>', px(n$x + .25), py(n$y) - 3,
        paste0(sprintf('<tspan x="%s" dy="%s">%s</tspan>', px(n$x + .25), c(0, 18), txt), collapse = ""))
    }
  }
  legend <- c(leaf = "Bazowe", gate = "Bramka", top = "Szczytowe")
  f9p_svg(as.integer((4.6 - x0) * u), as.integer((y1 + .1) * u) + 34L,
    "Drzewo błędów z inicjacją połączoną przez AND z bramką OR dwóch niesprawności zabezpieczeń.",
    sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="var(--upwr-reference)" stroke-width="1.6"/>',
            px(edges$x), py(edges$y), px(edges$xend), py(edges$yend)),
    vapply(seq_len(nrow(nodes)), function(i) mark(nodes$type[i], px(nodes$x[i]), py(nodes$y[i])), ""),
    vapply(seq_len(nrow(nodes)), label, ""),
    vapply(seq_along(legend), function(i) paste0(
      mark(names(legend)[i], 24 + (i - 1) * 130, py(-.1) + 18, r = 8),
      sprintf('<text x="%s" y="%s" font-size="15">%s</text>', 40 + (i - 1) * 130, py(-.1) + 23, legend[[i]])), "")
  )
}

# f9m_rank: parametry bazowe drzewa (przykład 9.3) i wyjściowe P(TOP).
f9_rank_config <- local({
  base <- c(init = .005, detect = .05, suppress = .08)
  top0 <- do.call(risk_fta_top, unname(as.list(base)))
  list(base = as.list(base), top0 = top0, top_text = "P(TOP) = 0.00063",
       labels = c("Inicjacja", "Detekcja", "Tłumienie"))
})
