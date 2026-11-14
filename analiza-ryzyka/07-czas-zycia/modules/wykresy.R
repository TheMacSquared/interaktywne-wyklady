# Ryciny wykładu 07 liczone w przeglądarce (modules/wykresy.js, silnik lcSF).
# Tutaj: to, co R liczy raz przy budowaniu strony i przekazuje rycinom w `config`
# — odczyty dla każdego położenia suwaka tam, gdzie zaokrąglenie R i JS mogłoby
# się rozejść, oraz stałe modeli.
# Wymaga: risk_math.R, risk_block.R i zycie_num() z block.R (ładowany później,
# dlatego formatowanie jest tu powtórzone lokalnie).

# c7_timeline: osiem czasów awarii i odczyty dla każdego końca obserwacji
# (suwak 300–2500 h co 50 h). Kolejność odczytów: awarie, cenzorowane, łączny
# czas, średnia z samych awarii, szacunek MTTF ze wzoru (7.2).
c7_timeline_config <- local({
  times <- c(220, 480, 760, 990, 1350, 1750, 2300, 3100)
  num <- function(x) format(round(x, 1), nsmall = 0, scientific = FALSE, trim = TRUE)
  follow <- seq(300, 2500, 50)
  list(
    times = times, min = 300, step = 50,
    stats = lapply(follow, function(f) {
      time <- pmin(times, f); failed <- times <= f
      c(paste(sum(failed), "z 8"), paste(sum(!failed), "z 8"), paste(sum(time), "h"),
        paste(num(mean(time[failed])), "h"), paste(num(sum(time) / sum(failed)), "h"))
    })
  )
})

# c7_same_mean: trzy modele Weibulla o MTTF = 1500 h; skala η = 1500 / Γ(1 + 1/β).
c7_same_mean_config <- local({
  shapes <- c(.7, 1, 2.5)
  list(shapes = shapes, scales = 1500 / gamma(1 + 1 / shapes))
})

# c7_bathtub: dno wanny (minimum sumy trzech składowych) dla każdego nasilenia
# zużycia (suwak 0.2–2 co 0.1) — to samo optimize() co w dawnym serwerze.
c7_bathtub_config <- local({
  wear <- seq(.2, 2, .1)
  list(min = .2, step = .1, bottom = lapply(wear, function(w) {
    total <- function(t) 1.2 * exp(-t / 350) + .12 + w * (t / 4000)^3
    opt <- optimize(total, c(1, 4000))
    list(t = opt$minimum, h = opt$objective,
         t_text = paste0("t ≈ ", round(opt$minimum / 10) * 10),
         h_text = formatC(opt$objective, format = "f", digits = 3))
  }))
})
