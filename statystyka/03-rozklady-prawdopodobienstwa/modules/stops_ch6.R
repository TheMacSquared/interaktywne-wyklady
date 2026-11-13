# PUNKTY STOP — rozdział 6 i domknięcie wykładu ----------------------------------
#   s3p_6b_ui — Ryc. 6.6 piaskownica „Grupka przyjaciół” (przypadek brzegowy: zależność)
#   s3p_d2_ui — Ryc. 6.7 domknięcie „Drzewo rodu rozkładów”: dwumianowy i Poisson
#               obok siebie jako „ile sukcesów?” w próbach i w czasie,
#               geometryczny i wykładniczy jako „ile czekać?”
# Serwer: s3p_ch6_server(input, output, session).

# ============================================================================
# (6b) PIASKOWNICA „GRUPKA PRZYJACIÓŁ”
# ============================================================================

s3p_6b_ui <- s3p_panel("Ryc. 6.6", "Grupka przyjaciół",
  lc_toolbar(
    lc_slider("s3p_6b_rho", "Jak bardzo grupka myśli podobnie (ρ)", 0, 0.8, 0, 0.1),
    lc_segmented("s3p_6b_n", "Osób w grupce", choices = c("5" = "5", "10" = "10", "30" = "30"), selected = "10"),
    lc_readouts(uiOutput("s3p_6b_read"))
  ),
  lc_plot("s3p_6b_plot", ratio = "2.4/1"),
  uiOutput("s3p_6b_caption")
)

# ============================================================================
# (D2) DOMKNIĘCIE „DRZEWO RODU ROZKŁADÓW”
# ============================================================================

s3p_d2_svg <- local({
  # Dwie kolumny pytań: w próbach (dyskretne) i w czasie lub przestrzeni (ciągłe).
  # Wiersze: „ile sukcesów?” i „ile czekać?”. Linie przerywane łączą rozkłady
  # odpowiadające na to samo pytanie — nie oznaczają przejścia granicznego.
  nodes <- list(
    bern  = list(x = 110, y = 140, lab = "Bernoulli", sub = "jedna próba: 0 albo 1", col = col_binomial),
    binom = list(x = 330, y = 90,  lab = "Dwumianowy", sub = "w n próbach: 0…n", col = col_binomial),
    pois  = list(x = 610, y = 90,  lab = "Poissona", sub = "w przedziale: 0, 1, 2, …", col = col_poisson),
    geom  = list(x = 330, y = 200, lab = "Geometryczny", sub = "ile prób do sukcesu", col = col_geometric),
    expo  = list(x = 610, y = 200, lab = "Wykładniczy", sub = "ile czasu do zdarzenia", col = col_exponential),
    unif  = list(x = 810, y = 320, lab = "Jednostajny", sub = "każda wartość równie częsta", col = col_uniform),
    norm  = list(x = 470, y = 320, lab = "Normalny", sub = NULL, col = col_normal),
    t     = list(x = 240, y = 440, lab = "t-Studenta", sub = NULL, col = col_t_student),
    chi   = list(x = 470, y = 440, lab = "χ²", sub = NULL, col = col_chi_sq),
    lnorm = list(x = 700, y = 440, lab = "Log-normalny", sub = NULL, col = col_lognormal)
  )
  W <- 170
  half_h <- function(nd) if (is.null(nd$sub)) 20 else 26
  edge <- function(a, b, lab, dx = 0, dy = -8, bend = 0) {
    A <- nodes[[a]]; B <- nodes[[b]]
    mx <- (A$x + B$x) / 2 + bend; my <- (A$y + B$y) / 2
    sprintf('<path class="s3p-edge" d="M %g %g Q %g %g %g %g" marker-end="url(#s3p-arr)"/><text class="s3p-edge-lab" x="%g" y="%g">%s</text>',
      A$x, A$y, mx, my, B$x, B$y, mx + dx, my + dy, lab)
  }
  # Ten sam rodzaj pytania w dwóch kolumnach: linia przerywana bez grotu.
  link <- function(a, b, lab) {
    A <- nodes[[a]]; B <- nodes[[b]]
    x1 <- A$x + W / 2; x2 <- B$x - W / 2
    sprintf('<path class="s3p-edge s3p-link" d="M %g %g L %g %g"/><text class="s3p-edge-lab" x="%g" y="%g">%s</text>',
      x1, A$y, x2, B$y, (x1 + x2) / 2, A$y - 8, lab)
  }
  edges <- paste0(
    edge("bern", "binom", "suma n prób", dx = -14, dy = -10),
    edge("bern", "geom", "czekanie na sukces", dx = -48, dy = 26),
    link("binom", "pois", "ile sukcesów?"),
    link("geom", "expo", "ile czekać?"),
    edge("pois", "expo", "odstępy", dx = 34, dy = 4),
    edge("binom", "norm", "CTG: n duże", dx = -42, dy = 70, bend = 60),
    edge("pois", "norm", "CTG: λ duże", dx = 44, dy = 70, bend = -60),
    edge("norm", "t", "s zamiast σ", dx = -48, dy = 0),
    edge("norm", "chi", "suma kwadratów", dx = 58, dy = 4),
    edge("norm", "lnorm", "e do potęgi X", dx = 52, dy = 0)
  )
  boxes <- paste(vapply(nodes, function(nd) {
    h <- half_h(nd)
    if (is.null(nd$sub)) {
      txt <- sprintf('<text x="%g" y="%g">%s</text>', nd$x, nd$y + 5, nd$lab)
    } else {
      txt <- sprintf('<text x="%g" y="%g">%s</text><text class="s3p-node-sub" x="%g" y="%g">%s</text>',
                     nd$x, nd$y - 2, nd$lab, nd$x, nd$y + 15, nd$sub)
    }
    sprintf('<g class="s3p-node"><rect x="%g" y="%g" width="%g" height="%g" rx="8" style="stroke:%s"/>%s</g>',
            nd$x - W / 2, nd$y - h, W, 2 * h, nd$col, txt)
  }, character(1)), collapse = "")
  HTML(paste0('<svg class="s3p-svg" style="max-width:900px" viewBox="0 0 900 480" role="img" aria-label="Powiązania między rozkładami z wykładu: dwumianowy i Poissona liczą sukcesy w próbach i w czasie, geometryczny i wykładniczy mierzą czekanie">',
    '<defs><marker id="s3p-arr" viewBox="0 0 10 10" refX="10" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse">',
    '<path d="M 0 0 L 10 5 L 0 10 z" fill="currentColor" style="fill:var(--upwr-ink-subtle)"/></marker></defs>',
    '<text class="s3p-group" x="330" y="30" text-anchor="middle">W PRÓBACH</text>',
    '<text class="s3p-group" x="610" y="30" text-anchor="middle">W CZASIE LUB PRZESTRZENI</text>',
    '<text class="s3p-group" x="20" y="400">ROZKŁADY ZBUDOWANE Z NORMALNEGO</text>',
    edges, boxes, '</svg>'))
})

s3p_d2_ui <- s3p_panel("Ryc. 6.7", "Drzewo rodu rozkładów",
  s3p_d2_svg,
  lc_caption("Górne dwa wiersze to te same pytania zadane w dwóch sytuacjach: ile sukcesów i ile czekać — w policzalnych próbach albo w czasie lub przestrzeni, gdzie sukcesów może być dowolnie wiele. Strzałka prowadzi od rozkładu do tego, który z niego powstaje. Liczby sukcesów przy dużym n lub λ zbiegają się w rozkładzie normalnym (CTG), a z niego wyrastają rozkłady potrzebne do wnioskowania.")
)

# ============================================================================
# SERWER
# ============================================================================

s3p_ch6_server <- function(input, output, session) {

  # --- 6b -------------------------------------------------------------------
  means_6b <- reactive({
    rho <- input$s3p_6b_rho %||% 0; n <- as.integer(input$s3p_6b_n %||% "10")
    set.seed(3602)
    g <- rnorm(3000); e <- matrix(rnorm(3000 * n), 3000)
    6 + 2 * (sqrt(rho) * g + sqrt(1 - rho) * rowMeans(e))
  })

  output$s3p_6b_read <- renderUI({
    rho <- input$s3p_6b_rho %||% 0; n <- as.integer(input$s3p_6b_n %||% "10")
    tagList(lc_readout("SE z symulacji", lc_fmt(sd(means_6b()), 2), color = upwr_accent),
            lc_readout("σ / √n", lc_fmt(2 / sqrt(n), 2)),
            lc_readout("Ile niezależnych osób to jest", lc_fmt(n / (1 + (n - 1) * rho), 1)))
  })

  zoom_plot_server("s3p_6b_plot", reactive({
    m <- means_6b(); n <- as.integer(input$s3p_6b_n %||% "10")
    x <- seq(1, 11, length.out = 400)
    ggplot(data.frame(m = m), aes(m)) +
      geom_histogram(aes(y = after_stat(density)), binwidth = 0.1, fill = upwr_secondary, alpha = 0.75, colour = "white") +
      geom_line(data = data.frame(x = x, y = dnorm(x, 6, 2 / sqrt(n))), aes(x, y), colour = upwr_accent, linewidth = 1.2) +
      coord_cartesian(xlim = c(2, 10)) +
      labs(x = "Średnia ocena prowadzącego w grupce (1–10)", y = "Gęstość",
           subtitle = "Słupki: 3000 grupek · krzywa: obietnica CTG, N(μ, σ/√n)") + theme_upwr(grid = "y")
  }))

  output$s3p_6b_caption <- renderUI({
    lc_caption(if ((input$s3p_6b_rho %||% 0) == 0)
      "Gdy oceny w grupce są niezależne, histogram średnich pasuje do krzywej CTG."
    else "Pytamy grupkę przyjaciół, którzy siedzą razem i rozmawiają o zajęciach: ich oceny są do siebie podobne. Średnie rozjeżdżają się szerzej, niż obiecuje σ/√n, a dziesięć zgodnych osób jest warte tyle, co kilka niezależnych. CTG wymaga niezależności.")
  })

}
