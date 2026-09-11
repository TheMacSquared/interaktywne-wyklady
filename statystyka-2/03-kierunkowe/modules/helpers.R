# ============================================================================
# FUNKCJE POMOCNICZE - Material kierunkowy
# ============================================================================

fmt <- function(x, digits = 2) {
  formatC(x, format = "f", digits = digits, decimal.mark = ",")
}

fmt_pct <- function(x, digits = 1) {
  paste0(fmt(100 * x, digits), "%")
}

mini_table <- function(df, digits = 3) {
  df[] <- lapply(df, function(x) {
    if (is.numeric(x)) fmt(x, digits) else x
  })
  tags$table(
    class = "lc-table lc-table-bordered lc-table-striped",
    tags$thead(tags$tr(lapply(names(df), tags$th))),
    tags$tbody(
      lapply(seq_len(nrow(df)), function(i) {
        tags$tr(lapply(df[i, , drop = FALSE], tags$td))
      })
    )
  )
}

generate_agri_rcbd <- function(interaction = 4, block_sd = 3, noise_sd = 2) {
  set.seed(2026)
  nitrogen <- factor(rep(c("0", "60", "120"), each = 3 * 4),
                     levels = c("0", "60", "120"))
  cultivar <- factor(rep(rep(c("Aster", "Bogatka", "Ceres"), each = 4), times = 3))
  block <- factor(rep(paste("Blok", 1:4), times = 9))
  n_num <- as.numeric(as.character(nitrogen))
  cult_eff <- c(Aster = 0, Bogatka = 2.5, Ceres = -1.2)[as.character(cultivar)]
  block_eff <- rep(rnorm(4, 0, block_sd), times = 9)
  slope <- c(Aster = 0.050, Bogatka = 0.040 + interaction / 600,
             Ceres = 0.060 - interaction / 900)[as.character(cultivar)]
  yield <- 48 + cult_eff + slope * n_num + block_eff + rnorm(length(n_num), 0, noise_sd)
  data.frame(nitrogen, cultivar, block, yield = round(yield, 1))
}

generate_sensory <- function(panel_agreement = 0.65) {
  set.seed(2026)
  products <- c("Ser A", "Ser B", "Ser C", "Ser D")
  judges <- paste("Oceniający", 1:12)
  true_rank <- c(Ser.A = 4.4, Ser.B = 3.7, Ser.C = 2.8, Ser.D = 2.1)
  expand.grid(judge = judges, product = products, KEEP.OUT.ATTRS = FALSE) |>
    mutate(
      signal = true_rank[gsub(" ", ".", product)],
      score = signal * panel_agreement + runif(n(), 1, 5) * (1 - panel_agreement) +
        rnorm(n(), 0, 0.35),
      score = pmin(5, pmax(1, round(score)))
    )
}

kendall_w <- function(df) {
  wide <- xtabs(score ~ judge + product, data = df)
  ranks <- t(apply(wide, 1, rank))
  rank_sums <- colSums(ranks)
  m <- nrow(ranks)
  n <- ncol(ranks)
  s <- sum((rank_sums - mean(rank_sums))^2)
  12 * s / (m^2 * (n^3 - n))
}

friedman_p <- function(df) {
  wide <- xtabs(score ~ judge + product, data = df)
  stats::friedman.test(wide)$p.value
}

generate_spc <- function(shift = 0, trend = 0) {
  set.seed(2026)
  sample_id <- rep(1:25, each = 5)
  step_shift <- ifelse(sample_id >= 17, shift, 0)
  value <- 42 + step_shift + trend * sample_id + rnorm(length(sample_id), 0, 0.65)
  data.frame(sample_id, value)
}

generate_env_lod <- function(lod = 0.8) {
  set.seed(2026)
  distance <- seq(0.2, 12, length.out = 90)
  concentration <- 7.5 * distance^-0.9 * exp(rnorm(length(distance), 0, 0.28))
  observed <- concentration
  observed[observed < lod] <- NA
  data.frame(distance, concentration, observed, below_lod = concentration < lod)
}

generate_floods <- function(n = 45) {
  set.seed(2026)
  data.frame(
    year = 1981:(1980 + n),
    qmax = round(180 + 55 * (-log(runif(n)))^0.32 + rnorm(n, 0, 14), 1)
  )
}

generate_oze_series <- function() {
  set.seed(2026)
  day <- 1:365
  seasonal <- 1.8 * sin(2 * pi * (day - 35) / 365)
  wind <- numeric(length(day))
  wind[1] <- 6 + seasonal[1] + rnorm(1, 0, 1)
  for (i in 2:length(day)) {
    wind[i] <- 0.72 * wind[i - 1] + 0.28 * (6 + seasonal[i]) + rnorm(1, 0, 1.15)
  }
  wind <- pmax(0.2, wind)
  power <- pmin(1, (wind / 12)^3)
  data.frame(day, wind, power)
}
