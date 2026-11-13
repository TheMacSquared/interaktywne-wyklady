# PUNKTY STOP — rozdział 2 -----------------------------------------------------
#   s3p_2a_ui — Ryc. 2.2 „Ile wraca z 1 zł”
#   s3p_2c_ui — Ryc. 2.3 „Ubezpieczenie telefonu”
#   s3p_2b_ui — Ryc. 2.4 „Dwie trasy na egzamin”
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

# Wzór E dla każdej gry (MathJax), policzony z góry; JS pokazuje wzór wybranej gry.
s3p_2a_tex <- local({
  g <- s3p_2a_games; l <- g$lotto; sc <- g$scratch
  big <- function(v) gsub(" ", "\\\\,", lc_fmt(v, 0, " "))
  list(
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
})

# Dane dla przeglądarki (stops12_charts.js, d03-back): rozkład wygranej jednej gry,
# zwrot z 1 zł (E / cena), wzór i podpisy.
s3p_2a_config <- list(
  games = lapply(s3p_2a_games, function(gm) {
    o <- s3p_2a_outcomes(gm)
    list(name = gm$name, price = gm$price, x = I(o$x), p = I(o$p), back = s3p_2a_ev(gm) / gm$price)
  }),
  order = I(c("roulette", "scratch", "lotto")),
  tex = s3p_2a_tex,
  captions = list(
    start = "Wybierz grę i graj. Gdzie uspokoi się średnia?",
    hint = "Gdzie uspokaja się średnia? Włącz wartość oczekiwaną, żeby porównać ją z wynikiem ze wzoru.",
    roulette = "Ruletka ma mały rozrzut: każdy obrót to −1 albo +1 zł. Średnia szybko siada na 0.97.",
    scratch = "Zdrapka ma większy rozrzut przez rzadką wygraną 100 zł: średnia skacze dłużej, zanim zbliży się do 0.76.",
    lotto_few = "Lotto: graj tysiącami. Większość zakładów kończy się niczym, czasem trafi się 24 zł.",
    lotto = "Nawet po tysiącach zakładów średnia leży zwykle poniżej E: 36 groszy z wartości oczekiwanej to jackpot, który trafia się raz na 14 milionów zakładów. Prawo wielkich liczb działa, ale tu potrzebowałoby milionów gier.",
    all = "Po przeliczeniu na 1 zł stawki żadna gra nie oddaje całej złotówki: organizator zawsze ma przewagę. Kwoty Lotto za 4–6 trafień są orientacyjne.")
)

s3p_2a_ui <- s3p_panel("Ryc. 2.2", "Ile wraca z 1 zł",
  lc_sf_figure("d03-back", config = s3p_2a_config,
    toolbar = tagList(
      lc_sf_ctl(lc_segmented("s3p_2a_game", "Gra", choices = c("Ruletka" = "roulette", "Zdrapka" = "scratch",
        "Lotto" = "lotto", "Zestawienie" = "all"))),
      htmltools::tagAppendAttributes(
        lc_sf_action_group("Graj", p1 = "1×", p10 = "10×", p100 = "100×", p1000 = "1000×", p10000 = "10 000×"),
        `data-role` = "play"),
      htmltools::tagAppendAttributes(
        lc_sf_action("reset", "s3p_2a_reset", icon = "reset", variant = "ghost", aria_label = "Od nowa"),
        `data-role` = "reset"),
      lc_sf_ctl(checkboxInput("s3p_2a_ev", "Wartość oczekiwana (wzór i linia)", value = FALSE)),
      htmltools::tagAppendAttributes(lc_readouts(
        lc_readout("Gier", lc_sf_value("n", "0")),
        lc_readout("Wydane", lc_sf_value("spent", "0 zł")),
        lc_readout("Wygrane", lc_sf_value("won", "0 zł")),
        lc_readout("Z 1 zł wraca", lc_sf_value("back"), color = upwr_accent)), `data-role` = "reads")),
    body = tagList(
      lc_formula_box(`data-role` = "calc", hidden = NA),
      lc_sf_chart(0, ratio = "2.6/1", max_height = "440px")),
    legend = TRUE,
    after = lc_caption(lc_sf_value("cap", s3p_2a_config$captions$start)))
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


# Gęstości, pola spóźnienia i odczyty liczy przeglądarka (stops12_charts.js, d03-routes).
s3p_2b_ui <- s3p_panel("Ryc. 2.4", "Dwie trasy na egzamin",
  lc_sf_figure("d03-routes", ratio = "2.2/1", max_height = "440px", legend = TRUE,
    config = list(routes = lapply(s3p_2b_routes, function(r)
      list(name = r$name, mean = r$mean, sd = r$sd, shape = s3p_2b_shape(r), scale = s3p_2b_scale(r)))),
    control = lc_sf_slider("s3p_2b_dead", "Masz na dojazd", 20, 70, 35, 1, suffix = " min"),
    toolbar = lc_readouts(
      lc_readout("Spóźnienie A", lc_sf_value("lateA"), color = upwr_cat[["indygo"]], swatch = TRUE),
      lc_readout("Spóźnienie B", lc_sf_value("lateB"), color = upwr_cat[["terakota"]], swatch = TRUE)),
    after = lc_caption(lc_sf_value("cap", "")))
)

# ============================================================================
# (2c) PRZYPADEK BRZEGOWY „UBEZPIECZENIE TELEFONU”
# ============================================================================

s3p_2c_phone <- 3000
s3p_2c_risk <- 0.05

# Dwa wykresy (bez polisy / z polisą), odczyt i podpis zależne od składki liczy
# przeglądarka (stops12_charts.js, d03-phone). Stałe wartości „bez polisy” są tu.
s3p_2c_ui <- local({
  ev <- -s3p_2c_phone * s3p_2c_risk
  sd <- s3p_2c_phone * sqrt(s3p_2c_risk * (1 - s3p_2c_risk))
  s3p_panel("Ryc. 2.3", "Ubezpieczenie telefonu",
      # Dwa wykresy obok siebie jak facet_wrap(ncol = 2): para od 560 px kontenera.
    lc_sf_figure("d03-phone",
      body = lc_plots(pair = TRUE,
        tags$div(tags$h4(sprintf("Bez ubezpieczenia · E = %s zł", lc_fmt(ev))),
                 lc_sf_chart(0, ratio = "1.25/1", max_height = "340px")),
        tags$div(tags$h4("Z ubezpieczeniem · E = ", lc_sf_value("e2", "-250"), " zł"),
                 lc_sf_chart(1, ratio = "1.25/1", max_height = "340px"))),
      config = list(phone = s3p_2c_phone, risk = s3p_2c_risk),
      control = lc_sf_slider("s3p_2c_premium", "Składka roczna", 50, 400, 250, 10, suffix = " zł"),
      toolbar = lc_readouts(
        lc_readout("Bez: E / SD", sprintf("%s / %s zł", lc_fmt(ev), lc_fmt(sd))),
        lc_readout("Z polisą: E / SD", tagList(lc_sf_value("withp", "-250"), " / 0 zł"), color = upwr_accent)),
      after = lc_caption(lc_sf_value("cap", ""))))
})

# ============================================================================
# SERWER
# ============================================================================

# Ryc. 2.2, 2.3 i 2.4 liczy przeglądarka (stops12_charts.js); funkcję wywołuje app.R.
s3p_ch2_server <- function(input, output, session) {
  # Ryc. 2.2–2.4 liczy przeglądarka (stops12_charts.js); app.R wywołuje tę funkcję.
}
