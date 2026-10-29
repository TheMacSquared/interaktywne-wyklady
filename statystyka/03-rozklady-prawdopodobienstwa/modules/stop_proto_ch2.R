# PROTOTYPY PUNKTÓW STOP — rozdział 2 (2026-10-29) ---------------------------
#   s3p_2a_ui — Ryc. 2.2 „Ile wraca z 1 zł” (zaakceptowany 2026-10-29; zastąpił „Grę w loterie” A–D)
#   s3p_2b_ui — Ryc. 2.4 „Dwie trasy na egzamin” (zaakceptowany 2026-10-29)
#   s3p_2c_ui — Ryc. 2.3 „Ubezpieczenie telefonu” (zaakceptowany 2026-10-29; zastąpił „Punkt równowagi”)
# Serwer: s3p_ch2_server(input, output, session).

# ============================================================================
# (2a) SYMULACJA „ILE WRACA Z 1 ZŁ” — złoty środek między Ryc. 2.2 a dawnym 2a
# ============================================================================

# Lotto 6 z 49: prawdopodobieństwa trafień dokładne; kwoty za 4, 5 i 6 trafień
# zmieniają się z losowania na losowanie, przyjmujemy orientacyjne.
s3p_2a_games <- local({
  total <- choose(49, 6)
  lotto_p <- sapply(3:6, function(k) choose(6, k) * choose(43, 6 - k) / total)
  tk <- scene_tickets$main
  list(
    lotto = list(name = "Lotto", price = 3, prize = c(24, 170, 6000, 5e6), p = lotto_p,
                 lab = c("3 trafienia", "4 trafienia", "5 trafień", "6 trafień")),
    roulette = list(name = "Ruletka", price = 1, prize = 2, p = 18/37, lab = "czerwone"),
    scratch = list(name = "Zdrapka", price = scene_ticket_price, prize = tk$prizes[-1],
                   p = tk$probs[-1], lab = paste(tk$prizes[-1], "zł"))
  )
})
s3p_2a_ev <- function(g) sum(g$prize * g$p)

# Rozkład wygranej jednej gry: wartości i prawdopodobieństwa (z brakiem wygranej).
s3p_2a_outcomes <- function(gm) list(x = c(0, gm$prize), p = c(1 - sum(gm$p), gm$p))

s3p_2a_ui <- s3p_panel("2a", "Ryc. 2.2", "Ile wraca z 1 zł", proto = FALSE,
  lc_toolbar(
    lc_segmented("s3p_2a_game", "Gra", choices = c("Ruletka" = "roulette", "Zdrapka" = "scratch",
      "Lotto" = "lotto", "Zestawienie" = "all")),
    conditionalPanel("input.s3p_2a_game != 'all'",
      lc_action_group(label = "Graj", s3p_2a_p1 = "1×", s3p_2a_p10 = "10×", s3p_2a_p100 = "100×",
                      s3p_2a_p1000 = "1000×", s3p_2a_p10000 = "10 000×")),
    conditionalPanel("input.s3p_2a_game != 'all'",
      lc_action("s3p_2a_reset", icon = "reset", variant = "ghost", aria_label = "Od nowa")),
    tags$div(class = "lc-checks", checkboxInput("s3p_2a_ev", "Wartość oczekiwana (wzór i linia)", value = FALSE)),
    lc_readouts(uiOutput("s3p_2a_read"))
  ),
  uiOutput("s3p_2a_calc"),
  lc_plot("s3p_2a_plot", ratio = "2.6/1"),
  uiOutput("s3p_2a_caption")
)

# ============================================================================
# (2b) PIASKOWNICA „DWIE TRASY NA EGZAMIN”
# ============================================================================

# Czas przejazdu jako rozkład gamma (zawsze dodatni) o zadanej średniej i SD.
s3p_2b_routes <- list(
  A = list(name = "Trasa A: obwodnica", mean = 30, sd = 2),
  B = list(name = "Trasa B: przez centrum", mean = 25, sd = 12)
)
s3p_2b_shape <- function(r) (r$mean / r$sd)^2
s3p_2b_scale <- function(r) r$sd^2 / r$mean

s3p_2b_ui <- s3p_panel("2b", "Ryc. 2.4", "Dwie trasy na egzamin", proto = FALSE,
  lc_toolbar(
    lc_slider("s3p_2b_dead", "Masz na dojazd", 20, 70, 35, 1, suffix = " min"),
    lc_readouts(uiOutput("s3p_2b_read"))
  ),
  lc_plot("s3p_2b_plot", ratio = "2.2/1"),
  uiOutput("s3p_2b_caption")
)

# ============================================================================
# (2c) PRZYPADEK BRZEGOWY „UBEZPIECZENIE TELEFONU”
# ============================================================================

s3p_2c_phone <- 3000
s3p_2c_risk <- 0.05

s3p_2c_ui <- s3p_panel("2c", "Ryc. 2.3", "Ubezpieczenie telefonu", proto = FALSE,
  lc_toolbar(
    lc_slider("s3p_2c_premium", "Składka roczna", 50, 400, 250, 10, suffix = " zł"),
    lc_readouts(uiOutput("s3p_2c_read"))
  ),
  lc_plot("s3p_2c_plot", ratio = "2.4/1"),
  uiOutput("s3p_2c_caption")
)

# ============================================================================
# SERWER
# ============================================================================

s3p_ch2_server <- function(input, output, session) {

  # --- 2a -------------------------------------------------------------------
  game_2a <- reactive(input$s3p_2a_game %||% "roulette")
  wins_2a <- reactiveVal(numeric(0))
  observeEvent(input$s3p_2a_game, wins_2a(numeric(0)), ignoreInit = TRUE)
  play_2a <- function(n) {
    gm <- s3p_2a_games[[game_2a()]]; o <- s3p_2a_outcomes(gm)
    wins_2a(c(wins_2a(), sample(o$x, n, replace = TRUE, prob = o$p)))
  }
  observeEvent(input$s3p_2a_p1, play_2a(1))
  observeEvent(input$s3p_2a_p10, play_2a(10))
  observeEvent(input$s3p_2a_p100, play_2a(100))
  observeEvent(input$s3p_2a_p1000, play_2a(1000))
  observeEvent(input$s3p_2a_p10000, play_2a(10000))
  observeEvent(input$s3p_2a_reset, wins_2a(numeric(0)))

  output$s3p_2a_read <- renderUI({
    if (game_2a() == "all") return(NULL)
    gm <- s3p_2a_games[[game_2a()]]; w <- wins_2a(); n <- length(w)
    tagList(lc_readout("Gier", lc_fmt(n, 0, " ")),
            lc_readout("Wydane", paste(lc_fmt(n * gm$price, 0, " "), "zł")),
            lc_readout("Wygrane", paste(lc_fmt(sum(w), 0, " "), "zł")),
            lc_readout("Z 1 zł wraca", if (n) lc_fmt(sum(w) / (n * gm$price), 2) else "–", color = upwr_accent))
  })

  # Jeden wzór dla wybranej gry, duży i czytelny z rzutnika (MathJax).
  show_ev_2a <- reactive(isTRUE(input$s3p_2a_ev) || game_2a() == "all")

  output$s3p_2a_calc <- renderUI({
    if (!show_ev_2a()) return(NULL)
    g <- s3p_2a_games; l <- g$lotto; sc <- g$scratch
    big <- function(v) gsub(" ", "\\\\,", lc_fmt(v, 0, " "))
    tex <- switch(game_2a(),
      lotto = sprintf(paste0("\\begin{aligned} E &= 24 \\cdot \\frac{1}{%s} + 170 \\cdot \\frac{1}{%s} + 6000 \\cdot \\frac{1}{%s} + 5\\,000\\,000 \\cdot \\frac{1}{%s} \\\\ ",
                     "&= %s + %s + %s + %s \\approx \\mathbf{%s\\ \\text{zł}} \\;\\text{ z 3 zł} \\end{aligned}"),
        big(1 / l$p[1]), big(1 / l$p[2]), big(1 / l$p[3]), big(1 / l$p[4]),
        lc_fmt(l$prize[1] * l$p[1], 2), lc_fmt(l$prize[2] * l$p[2], 2), lc_fmt(l$prize[3] * l$p[3], 2),
        lc_fmt(l$prize[4] * l$p[4], 2), lc_fmt(s3p_2a_ev(l), 2)),
      roulette = sprintf("E = 2\\ \\text{zł} \\cdot \\frac{18}{37} \\approx \\mathbf{%s\\ \\text{zł}} \\;\\text{ z 1 zł}", lc_fmt(36 / 37, 2)),
      scratch = sprintf("E = 4 \\cdot 0.25 + 10 \\cdot 0.08 + 100 \\cdot 0.02 = 1 + 0.8 + 2 = \\mathbf{%s\\ \\text{zł}} \\;\\text{ z 5 zł}",
        formatC(s3p_2a_ev(sc), format = "f", digits = 2)),
      all = sprintf("\\text{Lotto: } \\frac{%s}{3} = \\mathbf{%s} \\qquad \\text{Zdrapka: } \\frac{%s}{5} = \\mathbf{%s} \\qquad \\text{Ruletka: } \\frac{0.97}{1} = \\mathbf{0.97}",
        lc_fmt(s3p_2a_ev(l), 2), lc_fmt(s3p_2a_ev(l) / 3, 2), formatC(s3p_2a_ev(sc), format = "f", digits = 2),
        lc_fmt(s3p_2a_ev(sc) / 5, 2)))
    lc_formula_box(withMathJax(paste0("$$", tex, "$$")))
  })

  zoom_plot_server("s3p_2a_plot", reactive({
    g <- s3p_2a_games
    if (game_2a() == "all") {
      d <- do.call(rbind, lapply(g[c("roulette", "scratch", "lotto")], function(gm)
        data.frame(game = gm$name, back = s3p_2a_ev(gm) / gm$price, stake = 1)))
      d$y <- seq_len(nrow(d))
      return(ggplot(d) +
        geom_rect(aes(xmin = 0, xmax = back, ymin = y - 0.32, ymax = y + 0.32), fill = upwr_secondary) +
        geom_rect(aes(xmin = back, xmax = stake, ymin = y - 0.32, ymax = y + 0.32), fill = upwr_rule) +
        geom_text(aes(x = back / 2, y = y, label = paste("wraca", lc_fmt(back, 2))), colour = "white", fontface = "bold", size = 4.4) +
        geom_text(aes(x = 1.012, y = y, label = paste("zostaje", lc_fmt(stake - back, 2))), colour = upwr_ink, hjust = 0, size = 4.2) +
        scale_y_continuous(breaks = d$y, labels = d$game) +
        scale_x_continuous(limits = c(0, 1.25), breaks = seq(0, 1, 0.25)) +
        labs(x = "Z 1 zł stawki", y = NULL, subtitle = "Ciemne: średnio wraca do gracza · jasne: zostaje u organizatora") +
        theme_upwr(grid = "x"))
    }
    gm <- g[[game_2a()]]; w <- wins_2a(); n <- length(w); ev <- s3p_2a_ev(gm) / gm$price
    p <- ggplot() +
      geom_hline(yintercept = 1, colour = upwr_reference, linetype = "22") +
      annotate("text", x = 1, y = 1, label = "gra „na zero”", colour = upwr_reference, hjust = 0, vjust = -0.5) +
      { if (show_ev_2a()) list(
          geom_hline(yintercept = ev, colour = upwr_accent, linetype = "22", linewidth = 1),
          annotate("text", x = 1, y = ev, label = sprintf("E = %s z 1 zł", lc_fmt(ev, 2)), colour = upwr_accent,
                   hjust = 0, vjust = 1.5, fontface = "bold")) } +
      scale_x_log10(limits = c(1, max(10, n)), labels = function(v) lc_fmt(v, 0, " ")) +
      coord_cartesian(ylim = c(0, 2)) +
      labs(x = "Liczba gier (skala log.)", y = NULL) + theme_upwr(grid = "y")
    if (n > 0) {
      idx <- unique(round(exp(seq(0, log(n), length.out = min(n, 400)))))
      d <- data.frame(n = idx, m = (cumsum(w) / (seq_len(n) * gm$price))[idx])
      p <- p + geom_line(data = d, aes(n, m), colour = upwr_secondary, linewidth = 1) +
        geom_point(data = tail(d, 1), aes(n, m), colour = upwr_secondary, size = 3)
    }
    p
  }))

  output$s3p_2a_caption <- renderUI({
    n <- length(wins_2a())
    if (!show_ev_2a()) return(lc_caption(if (n == 0) "Wybierz grę i graj. Gdzie uspokoi się średnia?"
      else "Gdzie uspokaja się średnia? Włącz wartość oczekiwaną, żeby porównać ją z wynikiem ze wzoru."))
    lc_caption(switch(game_2a(),
      roulette = "Ruletka ma mały rozrzut: każdy obrót to −1 albo +1 zł. Średnia szybko siada na 0.97.",
      scratch = "Zdrapka ma większy rozrzut przez rzadką wygraną 100 zł: średnia skacze dłużej, zanim zbliży się do 0.76.",
      lotto = if (n < 1000) "Lotto: graj tysiącami. Większość zakładów kończy się niczym, czasem trafi się 24 zł."
        else "Nawet po tysiącach zakładów średnia leży zwykle poniżej E: 36 groszy z wartości oczekiwanej to jackpot, który trafia się raz na 14 milionów zakładów. Prawo wielkich liczb działa, ale tu potrzebowałoby milionów gier.",
      all = "Po przeliczeniu na 1 zł stawki żadna gra nie oddaje całej złotówki: organizator zawsze ma przewagę. Kwoty Lotto za 4–6 trafień są orientacyjne."))
  })

  # --- 2b -------------------------------------------------------------------
  late_2b <- reactive({
    t <- input$s3p_2b_dead %||% 35
    vapply(s3p_2b_routes, function(r) pgamma(t, s3p_2b_shape(r), scale = s3p_2b_scale(r),
                                             lower.tail = FALSE), numeric(1))
  })

  output$s3p_2b_read <- renderUI({
    l <- late_2b()
    tagList(lc_readout("Spóźnienie A", s3p_pct(l[["A"]]), color = upwr_cat[["indygo"]], swatch = TRUE),
            lc_readout("Spóźnienie B", s3p_pct(l[["B"]]), color = upwr_cat[["terakota"]], swatch = TRUE))
  })

  zoom_plot_server("s3p_2b_plot", reactive({
    t <- input$s3p_2b_dead %||% 35
    x <- seq(0, 80, length.out = 600)
    d <- do.call(rbind, lapply(names(s3p_2b_routes), function(k) {
      r <- s3p_2b_routes[[k]]
      data.frame(x = x, y = dgamma(x, s3p_2b_shape(r), scale = s3p_2b_scale(r)), route = k)
    }))
    cols <- c(A = upwr_cat[["indygo"]], B = upwr_cat[["terakota"]])
    ggplot(d, aes(x, y, colour = route)) +
      geom_area(data = d[d$x >= t, ], aes(fill = route), alpha = 0.3, position = "identity", colour = NA) +
      geom_line(linewidth = 1.1) +
      geom_vline(xintercept = t, colour = upwr_ink, linewidth = 0.9) +
      annotate("text", x = t, y = Inf, label = "  egzamin", hjust = 0, vjust = 1.5, fontface = "bold") +
      scale_colour_manual(values = cols, guide = "none") + scale_fill_manual(values = cols, guide = "none") +
      coord_cartesian(ylim = c(0, 0.21)) +
      labs(x = "Czas przejazdu (min)", y = "Gęstość",
           subtitle = "A: średnio 30 min, SD 2 min · B: średnio 25 min, SD 12 min") +
      theme_upwr(grid = "y")
  }))

  output$s3p_2b_caption <- renderUI({
    l <- late_2b()
    better <- if (l[["A"]] < l[["B"]]) "obwodnicą (A)" else "przez centrum (B)"
    lc_caption(sprintf("Trasa B jest średnio o 5 minut szybsza. Przy tym czasie bezpieczniej jechać %s. Gdy liczy się termin, o wyborze decyduje rozrzut, a nie średnia.", better))
  })

  # --- 2c -------------------------------------------------------------------
  output$s3p_2c_read <- renderUI({
    prem <- input$s3p_2c_premium %||% 250
    ev <- -s3p_2c_phone * s3p_2c_risk
    sd <- s3p_2c_phone * sqrt(s3p_2c_risk * (1 - s3p_2c_risk))
    tagList(lc_readout("Bez: E / SD", sprintf("%s / %s zł", lc_fmt(ev), lc_fmt(sd))),
            lc_readout("Z polisą: E / SD", sprintf("%s / 0 zł", lc_fmt(-prem)), color = upwr_accent))
  })

  zoom_plot_server("s3p_2c_plot", reactive({
    prem <- input$s3p_2c_premium %||% 250
    lev <- c("Bez ubezpieczenia", "Z ubezpieczeniem")
    d <- data.frame(
      opt = factor(c(lev[1], lev[1], lev[2]), levels = lev),
      x = c("0 zł\n(nic się nie stało)", "−3000 zł\n(nowy telefon)", sprintf("−%s zł\n(składka)", prem)),
      p = c(1 - s3p_2c_risk, s3p_2c_risk, 1))
    d$x <- factor(d$x, levels = d$x)
    d$opt <- factor(sprintf("%s · E = %s zł", d$opt, ifelse(d$opt == lev[1], lc_fmt(-s3p_2c_phone * s3p_2c_risk), lc_fmt(-prem))))
    ggplot(d, aes(x, p)) +
      geom_col(width = 0.5, fill = upwr_secondary, alpha = 0.85) +
      geom_text(aes(label = lc_fmt(p, 2)), vjust = -0.6) +
      facet_wrap(~opt, ncol = 2, scales = "free_x") +
      scale_y_continuous(limits = c(0, 1.12)) +
      labs(x = "Bilans roku", y = "Prawdopodobieństwo") + theme_upwr(grid = "y")
  }))

  output$s3p_2c_caption <- renderUI({
    prem <- input$s3p_2c_premium %||% 250
    lc_caption(if (prem > 150)
      sprintf("Telefon za 3000 zł psuje się albo ginie w ciągu roku z szansą 5%%. Bez polisy tracisz średnio 150 zł, z polisą pewne %s zł. Rachunek E(X) mówi „nie ubezpieczaj”, a jednak wiele osób kupuje polisę: płaci za to, żeby nie trafić na stratę 3000 zł.", prem)
    else "Przy składce do 150 zł polisa wygrywa nawet w rachunku E(X). Ubezpieczyciel nie mógłby takiej sprzedawać: na każdej polisie średnio by tracił.")
  })
}
