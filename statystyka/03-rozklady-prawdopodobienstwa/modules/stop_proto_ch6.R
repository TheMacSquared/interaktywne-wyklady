# PROTOTYPY PUNKTÓW STOP — rozdział 6 i domknięcie wykładu (2026-10-29) ------
#   s3p_6a_ui — Scena „Waga wołu, Galton 1906” (otwarcie CTG)
#   s3p_6b_ui — Piaskownica „Grupka przyjaciół” (przypadek brzegowy: zależność)
#   s3p_6c_ui — Symulacja „Średnia Cauchy'ego” (przypadek brzegowy: brak wariancji)
#   s3p_6d_ui — Galeria „Od jednej obserwacji do średniej ze 100” (scalenie Ryc. 6.3 i 6.4)
#   s3p_d1_ui — Przypadek „Zakład pakowania, cztery rozkłady” (domknięcie wykładu)
#   s3p_d2_ui — Domknięcie „Drzewo rodu rozkładów”
# Serwer: s3p_ch6_server(input, output, session).

# ============================================================================
# (6a) SCENA „WAGA WOŁU, GALTON 1906”
# ============================================================================

# Rekonstrukcja 787 typów z percentyli opublikowanych przez Galtona (Vox Populi,
# Nature 1907); skrajne wartości przyjęte. Waga po uboju: 1198 funtów.
s3p_ox_true <- 1198
s3p_ox_guess <- local({
  q <- c(0, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 1)
  v <- c(900, 1074, 1109, 1162, 1207, 1236, 1267, 1293, 1400)
  set.seed(1906)
  round(approx(q, v, xout = runif(787))$y)
})

s3p_6a_ui <- s3p_panel("6a", "Scena", "Waga wołu, Galton 1906",
  lc_step_widget("s3p_6a",
    steps = c("Jarmark", "Pojedyncze typy", "Średnia z grupki", "Cały tłum"),
    toolbar = lc_toolbar(lc_step_from(3,
      lc_segmented("s3p_6a_n", "Osób w grupce", choices = c("1" = "1", "5" = "5", "20" = "20", "100" = "100"),
                   selected = "5"))),
    plot_id = "s3p_6a_plot", ratio = "2.4/1"
  )
)

# ============================================================================
# (6b) PIASKOWNICA „GRUPKA PRZYJACIÓŁ”
# ============================================================================

s3p_6b_ui <- s3p_panel("6b", "Piaskownica", "Grupka przyjaciół",
  lc_toolbar(
    lc_slider("s3p_6b_rho", "Jak bardzo grupka myśli podobnie (ρ)", 0, 0.8, 0, 0.1),
    lc_segmented("s3p_6b_n", "Osób w grupce", choices = c("5" = "5", "10" = "10", "30" = "30"), selected = "10"),
    lc_readouts(uiOutput("s3p_6b_read"))
  ),
  lc_plot("s3p_6b_plot", ratio = "2.4/1"),
  uiOutput("s3p_6b_caption")
)

# ============================================================================
# (6c) SYMULACJA „ŚREDNIA CAUCHY'EGO”
# ============================================================================

s3p_6c_ui <- s3p_panel("6c", "Symulacja", "Średnia Cauchy'ego",
  lc_toolbar(
    lc_action_group(label = "Dołóż obserwacji", s3p_6c_a100 = "+100", s3p_6c_a1000 = "+1000",
                    s3p_6c_a10000 = "+10 000"),
    lc_action("s3p_6c_reset", icon = "reset", variant = "ghost", aria_label = "Od nowa"),
    lc_readouts(uiOutput("s3p_6c_read"))
  ),
  lc_plot("s3p_6c_plot", ratio = "2.2/1"),
  uiOutput("s3p_6c_caption")
)

# ============================================================================
# (6d) GALERIA „OD JEDNEJ OBSERWACJI DO ŚREDNIEJ ZE 100” (Ryc. 6.3 + 6.4)
# ============================================================================

s3p_6d_n <- c(1, 2, 5, 30, 100)

s3p_6d_ui <- s3p_panel("6d", "Galeria", "Od jednej obserwacji do średniej ze 100",
  lc_step_widget("s3p_6d",
    steps = paste("n =", s3p_6d_n),
    toolbar = lc_toolbar(lc_segmented("s3p_6d_pop", "Populacja", choices = c(
      "Wykładnicza" = "exponential", "Jednostajna" = "uniform", "U-kształtna" = "u_shape"))),
    plot_id = "s3p_6d_plot", ratio = "2.4/1"
  )
)

# ============================================================================
# (D1) PRZYPADEK „ZAKŁAD PAKOWANIA, CZTERY ROZKŁADY”
# ============================================================================

s3p_d1_data <- local({
  dir <- file.path(app_dir, "cwiczenia", "zywnosc", "dane")
  list(
    pack = read.csv(file.path(dir, "kontrola_opakowania.csv"))$liczba_wadliwych,
    comp = read.csv(file.path(dir, "reklamacje_tygodniowo.csv"))$liczba_reklamacji,
    fail = read.csv(file.path(dir, "czas_miedzy_awariami.csv"))$godziny_od_poprzedniej,
    mass = with(read.csv(file.path(dir, "masa_netto_linie.csv")), masa_netto_g[linia == "A_nowa"])
  )
})

s3p_d1_ui <- s3p_panel("D1", "Przypadek", "Zakład pakowania, cztery rozkłady",
  lc_toolbar(
    lc_segmented("s3p_d1_q", "Pytanie", choices = c("Wadliwe w partii" = "pack",
      "Reklamacje w tygodniu" = "comp", "Czas między awariami" = "fail", "Masa paczki" = "mass")),
    lc_readouts(uiOutput("s3p_d1_read"))
  ),
  lc_plot("s3p_d1_plot", ratio = "2.4/1"),
  uiOutput("s3p_d1_caption")
)

# ============================================================================
# (D2) DOMKNIĘCIE „DRZEWO RODU ROZKŁADÓW”
# ============================================================================

s3p_d2_svg <- local({
  nodes <- list(
    bern  = list(x = 110, y = 70,  lab = "Bernoulli", col = col_binomial),
    binom = list(x = 330, y = 70,  lab = "Dwumianowy", col = col_binomial),
    geom  = list(x = 110, y = 200, lab = "Geometryczny", col = col_geometric),
    pois  = list(x = 560, y = 70,  lab = "Poissona", col = col_poisson),
    expo  = list(x = 790, y = 70,  lab = "Wykładniczy", col = col_exponential),
    unif  = list(x = 790, y = 200, lab = "Jednostajny", col = col_uniform),
    norm  = list(x = 450, y = 300, lab = "Normalny", col = col_normal),
    t     = list(x = 220, y = 430, lab = "t-Studenta", col = col_t_student),
    chi   = list(x = 450, y = 430, lab = "χ²", col = col_chi_sq),
    lnorm = list(x = 680, y = 430, lab = "Log-normalny", col = col_lognormal)
  )
  edge <- function(a, b, lab, dx = 0, dy = -8, bend = 0) {
    A <- nodes[[a]]; B <- nodes[[b]]
    mx <- (A$x + B$x) / 2 + bend; my <- (A$y + B$y) / 2
    sprintf('<path class="s3p-edge" d="M %g %g Q %g %g %g %g" marker-end="url(#s3p-arr)"/><text class="s3p-edge-lab" x="%g" y="%g">%s</text>',
      A$x, A$y, mx, my, B$x, B$y, mx + dx, my + dy, lab)
  }
  edges <- paste0(
    edge("bern", "binom", "suma n prób"),
    edge("bern", "geom", "czekanie na sukces", dx = -62, dy = 4),
    edge("binom", "pois", "n duże, p małe"),
    edge("pois", "expo", "odstępy między zdarzeniami"),
    edge("binom", "norm", "CTG: n duże", dx = -48),
    edge("pois", "norm", "CTG: λ duże", dx = 44),
    edge("norm", "t", "s zamiast σ", dx = -48, dy = 0),
    edge("norm", "chi", "suma kwadratów", dx = 58, dy = 4),
    edge("norm", "lnorm", "e do potęgi X", dx = 52, dy = 0)
  )
  boxes <- paste(vapply(nodes, function(nd) sprintf(
    '<g class="s3p-node"><rect x="%g" y="%g" width="150" height="40" rx="8" style="stroke:%s"/><text x="%g" y="%g">%s</text></g>',
    nd$x - 75, nd$y - 20, nd$col, nd$x, nd$y + 5, nd$lab), character(1)), collapse = "")
  HTML(paste0('<svg class="s3p-svg" style="max-width:900px" viewBox="0 0 900 480" role="img" aria-label="Powiązania między rozkładami z wykładu">',
    '<defs><marker id="s3p-arr" viewBox="0 0 10 10" refX="10" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse">',
    '<path d="M 0 0 L 10 5 L 0 10 z" fill="currentColor" style="fill:var(--upwr-ink-subtle)"/></marker></defs>',
    '<text class="s3p-group" x="20" y="22">ZLICZENIA I CZEKANIE</text>',
    '<text class="s3p-group" x="20" y="375">ROZKŁADY ZBUDOWANE Z NORMALNEGO</text>',
    edges, boxes, '</svg>'))
})

s3p_d2_ui <- s3p_panel("D2", "Domknięcie", "Drzewo rodu rozkładów",
  s3p_d2_svg,
  lc_caption("Strzałka prowadzi od rozkładu do tego, który z niego powstaje. Wszystkie drogi z góry zbiegają się w rozkładzie normalnym (CTG), a z niego wyrastają rozkłady potrzebne do wnioskowania.")
)

# ============================================================================
# SERWER
# ============================================================================

s3p_ch6_server <- function(input, output, session) {

  # --- 6a -------------------------------------------------------------------
  step_6a <- lc_step_server("s3p_6a", input)$step
  n_6a <- reactive(as.integer(input$s3p_6a_n %||% "5"))
  group_means <- reactive({
    n <- n_6a(); set.seed(1907 + n)
    replicate(2000, mean(sample(s3p_ox_guess, n)))
  })

  zoom_plot_server("s3p_6a_plot", reactive({
    step <- step_6a()
    lim <- c(900, 1400)
    base <- function(p) p + coord_cartesian(xlim = lim) +
      labs(x = "Typowana waga wołu (funty)", y = "Liczba") + theme_upwr(grid = "y")
    if (step == 3) {
      gm <- group_means()
      return(base(ggplot(data.frame(v = gm), aes(v)) +
        geom_histogram(binwidth = 5, fill = upwr_secondary, alpha = 0.85, colour = "white") +
        geom_vline(xintercept = s3p_ox_true, colour = upwr_accent, linewidth = 1.1) +
        annotate("text", x = s3p_ox_true, y = Inf, vjust = 1.5, hjust = -0.05, colour = upwr_accent,
                 fontface = "bold", label = "prawdziwa waga 1198") +
        labs(y = "Liczba grupek", subtitle = sprintf("2000 grupek po %d osób: typowy błąd średniej ≈ %s funtów",
          n_6a(), lc_fmt(median(abs(gm - s3p_ox_true)), 0)))))
    }
    p <- ggplot(data.frame(v = s3p_ox_guess), aes(v)) +
      geom_histogram(binwidth = 10, fill = upwr_secondary, alpha = if (step == 2) 0.35 else 0.85, colour = "white")
    if (step == 2) {
      set.seed(42); few <- data.frame(v = sample(s3p_ox_guess, 8))
      p <- p + geom_point(data = few, aes(v, 2), colour = upwr_ink, size = 3.5) +
        geom_vline(xintercept = s3p_ox_true, colour = upwr_accent, linewidth = 1.1)
    }
    if (step == 4) {
      p <- p + geom_vline(xintercept = s3p_ox_true, colour = upwr_accent, linewidth = 1.1) +
        geom_vline(xintercept = mean(s3p_ox_guess), colour = upwr_ink, linetype = "22", linewidth = 1) +
        annotate("text", x = 1300, y = Inf, vjust = 1.5, hjust = 0, fontface = "bold", size = 3.6,
                 label = sprintf("średnia tłumu %s\nprawdziwa 1198", lc_fmt(mean(s3p_ox_guess), 0)))
    }
    base(p)
  }))

  s3p_step_text(output, "s3p_6a", step_6a, list(
    "Plymouth, jarmark bydła, 1906 rok. Za 6 pensów można zgadywać, ile będzie ważyło mięso wołu po uboju. Kartki wypełnia 787 osób: rzeźnicy, rolnicy, ale też ludzie, którzy krowę widzieli z daleka. Francis Galton zbiera wszystkie kartki.",
    sprintf("Pojedyncze typy mylą się mocno: typowy błąd to około %s funtów, a skrajne pomyłki przekraczają 200. Na żadną pojedynczą kartkę nie warto stawiać.",
      lc_fmt(median(abs(s3p_ox_guess - s3p_ox_true)), 0)),
    "Teraz bierzemy losowe grupki i liczymy ich średni typ. Zmieniaj liczebność grupki: średnie skupiają się wokół prawdziwej wagi coraz ciaśniej, a ich histogram przypomina dzwon.",
    sprintf("Średnia z 787 kartek wynosi %s funtów, prawdziwa waga to 1198. Pomyłki w przeciwne strony się znoszą: średnia z wielu niezależnych typów jest dokładniejsza niż prawie każdy pojedynczy ekspert. (Typy zrekonstruowane z percentyli podanych przez Galtona.)",
      lc_fmt(mean(s3p_ox_guess), 0))
  ))

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

  # --- 6c -------------------------------------------------------------------
  draws_6c <- reactiveVal(list(norm = numeric(0), cau = numeric(0)))
  add_6c <- function(k) {
    d <- draws_6c(); draws_6c(list(norm = c(d$norm, rnorm(k)), cau = c(d$cau, rcauchy(k))))
  }
  observeEvent(input$s3p_6c_a100, add_6c(100))
  observeEvent(input$s3p_6c_a1000, add_6c(1000))
  observeEvent(input$s3p_6c_a10000, add_6c(10000))
  observeEvent(input$s3p_6c_reset, draws_6c(list(norm = numeric(0), cau = numeric(0))))

  output$s3p_6c_read <- renderUI({
    d <- draws_6c(); n <- length(d$norm)
    tagList(lc_readout("Obserwacje", lc_fmt(n, 0, " ")),
            lc_readout("Średnia N(0, 1)", if (n) lc_fmt(mean(d$norm), 3) else "–", color = upwr_cat[["indygo"]], swatch = TRUE),
            lc_readout("Średnia Cauchy'ego", if (n) lc_fmt(mean(d$cau), 3) else "–", color = upwr_accent, swatch = TRUE))
  })

  zoom_plot_server("s3p_6c_plot", reactive({
    d <- draws_6c(); n <- length(d$norm)
    if (n < 2) return(ggplot() + annotate("text", 0, 0, label = "Dołóż obserwacji", colour = upwr_reference, size = 5) + theme_void())
    idx <- unique(round(exp(seq(0, log(n), length.out = 400))))
    pd <- rbind(data.frame(n = idx, m = (cumsum(d$norm) / seq_len(n))[idx], who = "N(0, 1)"),
                data.frame(n = idx, m = (cumsum(d$cau) / seq_len(n))[idx], who = "Cauchy = t(1)"))
    ggplot(pd, aes(n, m, colour = who)) +
      geom_hline(yintercept = 0, linetype = "22", colour = upwr_reference) +
      geom_line(linewidth = 0.9) +
      scale_x_log10(labels = function(v) lc_fmt(v, 0, " ")) +
      scale_colour_manual(values = c("N(0, 1)" = upwr_cat[["indygo"]], "Cauchy = t(1)" = upwr_accent), guide = "none") +
      coord_cartesian(ylim = c(-6, 6)) +
      labs(x = "Liczba obserwacji (skala log.)", y = "Średnia bieżąca") + theme_upwr(grid = "y")
  }))

  output$s3p_6c_caption <- renderUI({
    lc_caption("Obie populacje są symetryczne wokół zera. Średnia z normalnej szybko się uspokaja. Średnia z rozkładu Cauchy'ego (t(1) z Ryc. 4.11) co jakiś czas skacze, bo jedna skrajna obserwacja przeważa tysiące innych, i nie stabilizuje się przy żadnym n. Bez skończonej wariancji nie ma ani prawa wielkich liczb, ani CTG.")
  })

  # --- 6d -------------------------------------------------------------------
  step_6d <- lc_step_server("s3p_6d", input)$step
  sims_6d <- reactive({
    pop <- input$s3p_6d_pop %||% "exponential"
    set.seed(3604)
    lapply(s3p_6d_n, function(n) rowMeans(matrix(generate_population_sample(pop, 4000 * n), 4000)))
  })

  zoom_plot_server("s3p_6d_plot", reactive({
    step <- step_6d(); pop <- input$s3p_6d_pop %||% "exponential"
    pr <- get_population_params(pop); n <- s3p_6d_n[step]
    m <- sims_6d()[[step]]; se <- pr$sigma / sqrt(n)
    xlim <- c(min(sims_6d()[[1]]), quantile(sims_6d()[[1]], 0.995))
    x <- seq(xlim[1], xlim[2], length.out = 400)
    p <- ggplot(data.frame(m = m), aes(m)) +
      geom_histogram(aes(y = after_stat(density)), bins = 60, fill = upwr_secondary, alpha = 0.75, colour = "white") +
      coord_cartesian(xlim = xlim, ylim = c(0, dnorm(0, 0, pr$sigma / sqrt(100)) * 1.15)) +
      labs(x = "Średnia z próby", y = "Gęstość") + theme_upwr(grid = "y")
    if (n > 1) p <- p + geom_line(data = data.frame(x = x, y = dnorm(x, pr$mu, se)), aes(x, y),
                                  colour = upwr_accent, linewidth = 1.2)
    p
  }))

  s3p_step_text(output, "s3p_6d", step_6d, lapply(seq_along(s3p_6d_n), function(i) function() {
    m <- sims_6d()[[i]]; sk <- mean((m - mean(m))^3) / sd(m)^3
    sprintf("n = %d: SE = %s, skośność %s. %s", s3p_6d_n[i], lc_fmt(sd(m), 2), lc_fmt(sk, 2),
      c("Pojedyncza obserwacja: kształt populacji.", "Dwie obserwacje: skrajne wartości się mieszają.",
        "Pięć: populacje symetryczne mają już dzwon, skośne jeszcze nie.",
        "Trzydzieści: dzwon prawie wszędzie, ślad skośności zostaje w ogonach.",
        "Sto: rozkład wąski i niemal idealnie normalny.")[i])
  }))

  # --- D1 -------------------------------------------------------------------
  fit_d1 <- reactive({
    q <- input$s3p_d1_q %||% "pack"; x <- s3p_d1_data[[q]]
    switch(q,
      pack = { p <- mean(x) / 40; list(x = x, disc = TRUE, dens = function(k) dbinom(k, 40, p),
        model = sprintf("B(40, %s)", lc_fmt(p, 3)), check = sprintf("p̂ = średnia / 40 = %s", lc_fmt(p, 3)),
        answer = sprintf("P(partia ma 4 lub więcej wadliwych) = %s", lc_fmt(1 - pbinom(3, 40, p), 3)),
        xlab = "Wadliwe opakowania w partii 40 sztuk") },
      comp = { l <- mean(x); list(x = x, disc = TRUE, dens = function(k) dpois(k, l),
        model = sprintf("Pois(%s)", lc_fmt(l, 2)), check = sprintf("średnia %s, wariancja %s", lc_fmt(l, 2), lc_fmt(var(x), 2)),
        answer = sprintf("P(tydzień z 8 lub więcej reklamacjami) = %s", lc_fmt(1 - ppois(7, l), 3)),
        xlab = "Reklamacje w tygodniu") },
      fail = { r <- 1 / mean(x); list(x = x, disc = FALSE, dens = function(t) dexp(t, r),
        model = sprintf("Exp(%s / h)", lc_fmt(r, 3)), check = sprintf("średnia %s h, SD %s h", lc_fmt(mean(x), 2), lc_fmt(sd(x), 2)),
        answer = sprintf("P(kolejna awaria w ciągu godziny) = %s", lc_fmt(pexp(1, r), 3)),
        xlab = "Godziny między awariami") },
      mass = { m <- mean(x); s <- sd(x); list(x = x, disc = FALSE, dens = function(v) dnorm(v, m, s),
        model = sprintf("N(%s, %s)", lc_fmt(m, 1), lc_fmt(s, 2)), check = sprintf("skośność %s", lc_fmt(mean((x - m)^3) / s^3, 2)),
        answer = sprintf("Odsetek paczek lżejszych niż 990 g = %s", s3p_pct(pnorm(990, m, s))),
        xlab = "Masa netto paczki 1 kg, linia A (g)") })
  })

  output$s3p_d1_read <- renderUI({
    f <- fit_d1()
    tagList(lc_readout("Model", f$model, color = upwr_accent), lc_readout("Sprawdzenie", f$check))
  })

  zoom_plot_server("s3p_d1_plot", reactive({
    f <- fit_d1(); x <- f$x
    if (f$disc) {
      k <- 0:max(x + 2)
      d <- data.frame(k = k, obs = tabulate(x + 1, length(k)) / length(x), mod = f$dens(k))
      return(ggplot(d, aes(k)) + geom_col(aes(y = obs), fill = upwr_secondary, alpha = 0.85, width = 0.75) +
        geom_point(aes(y = mod), shape = 21, fill = "white", colour = upwr_accent, size = 3.4, stroke = 1.4) +
        scale_x_continuous(breaks = k) + labs(x = f$xlab, y = "Udział") + theme_upwr(grid = "y"))
    }
    g <- seq(min(x), max(x), length.out = 400)
    ggplot(data.frame(x = x), aes(x)) +
      geom_histogram(aes(y = after_stat(density)), bins = 20, fill = upwr_secondary, alpha = 0.75, colour = "white") +
      geom_line(data = data.frame(x = g, y = f$dens(g)), aes(x, y), colour = upwr_accent, linewidth = 1.2) +
      labs(x = f$xlab, y = "Gęstość") + theme_upwr(grid = "y")
  }))

  output$s3p_d1_caption <- renderUI({
    f <- fit_d1()
    q <- input$s3p_d1_q %||% "pack"
    lc_caption(sprintf("%s Model %s, odpowiedź: %s.",
      switch(q, pack = "„Ile z n?”: partia 40 opakowań, każde wadliwe albo nie, więc dwumianowy.",
        comp = "„Ile razy w tygodniu?”: niezależne reklamacje w ustalonym czasie, więc Poisson.",
        fail = "„Ile czasu do następnego zdarzenia?”: odstępy między awariami, więc wykładniczy.",
        mass = "Masa paczki to suma wielu drobnych wpływów maszyny, więc normalny."),
      f$model, f$answer))
  })
}
