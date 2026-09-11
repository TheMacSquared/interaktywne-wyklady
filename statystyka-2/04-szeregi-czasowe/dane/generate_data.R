# generate_data.R
# Generuje realistyczne zbiory danych do wykładu o szeregach czasowych.
# Uruchom raz: Rscript generate_data.R z katalogu dane/
# Dane są syntetyczne, ale wzorowane na rzeczywistych polskich statystykach.

set.seed(2024)
out_dir <- Sys.getenv("TS_DATA_OUTPUT_DIR", unset = ".")

# Każdy CSV zachowuje informację o pochodzeniu także poza aplikacją.
write_synthetic_csv <- function(data, path, row.names = FALSE) {
  data$data_origin <- "Dane syntetyczne — scenariusz dydaktyczny, nie obserwacje urzędowe"
  write.csv(data, path, row.names = row.names)
}

# Węzły są przypisane do dat; nie ma recyklingu krótszego wektora.
interpolate_trend <- function(dates, knot_dates, values) {
  knot_dates <- as.Date(knot_dates)
  stopifnot(length(knot_dates) == length(values),
            all(diff(knot_dates) > 0),
            min(dates) >= min(knot_dates), max(dates) <= max(knot_dates))
  result <- approx(as.numeric(knot_dates), values, xout = as.numeric(dates))$y
  stopifnot(length(result) == length(dates), all(is.finite(result)))
  result
}

# ============================================================================
# 1. Temperatura Warszawa (miesięczna, 1951-2023)
# ============================================================================

years  <- 1951:2023
months <- 1:12
n      <- length(years) * 12

date_seq <- seq.Date(as.Date("1951-01-01"), by = "month", length.out = n)

seasonal_means <- c(-3.2, -1.8, 3.2, 9.0, 14.5, 17.8, 19.6, 19.2, 14.0, 8.0, 2.2, -1.5)

trend <- seq(0, 1.8, length.out = n)

temp <- numeric(n)
for (i in seq_len(n)) {
  m <- as.integer(format(date_seq[i], "%m"))
  yr_idx <- as.integer(format(date_seq[i], "%Y")) - 1950
  temp[i] <- seasonal_means[m] + trend[i] + rnorm(1, 0, 1.4)
}

warszawa_temp <- data.frame(
  date  = format(date_seq, "%Y-%m-%d"),
  year  = as.integer(format(date_seq, "%Y")),
  month = as.integer(format(date_seq, "%m")),
  temp  = round(temp, 2)
)

write_synthetic_csv(warszawa_temp, file.path(out_dir, "warszawa_temperatura.csv"), row.names = FALSE)

# ============================================================================
# 2. Bezrobocie w Polsce (miesięczne, 2000-2023)
# ============================================================================

n_bezr <- length(seq.Date(as.Date("2000-01-01"), as.Date("2023-12-01"), by = "month"))
date_bezr <- seq.Date(as.Date("2000-01-01"), by = "month", length.out = n_bezr)

# Fikcyjny scenariusz: spadek i przejściowy wzrost w roku 2020.
trend_bezr <- interpolate_trend(date_bezr,
  c("2000-01-01", "2003-01-01", "2008-01-01", "2012-01-01",
    "2015-01-01", "2019-12-01", "2020-07-01", "2021-12-01", "2023-12-01"),
  c(15, 20, 11.5, 6.5, 5, 3.5, 6.5, 4.5, 3.5)
)

seasonal_bezr <- c(0.8, 0.6, 0.1, -0.4, -0.6, -0.7, -0.5, -0.5, -0.3, 0.1, 0.6, 0.8)
month_seq_bezr <- as.integer(format(date_bezr, "%m"))
seasonal_comp <- seasonal_bezr[month_seq_bezr]

noise_bezr <- arima.sim(model = list(ar = 0.65), n = n_bezr, sd = 0.15)

stopifnot(length(trend_bezr) == n_bezr, length(seasonal_comp) == n_bezr)
bezrobocie <- pmax(1.5, trend_bezr + seasonal_comp + as.numeric(noise_bezr))

df_bezr <- data.frame(
  date     = format(date_bezr, "%Y-%m-%d"),
  year     = as.integer(format(date_bezr, "%Y")),
  month    = as.integer(format(date_bezr, "%m")),
  stopa    = round(bezrobocie, 2)
)

write_synthetic_csv(df_bezr, file.path(out_dir, "bezrobocie_pl.csv"), row.names = FALSE)

# ============================================================================
# 3. Noclegi w Polsce (miesięczne, 2010-2023)
# ============================================================================

date_noclegi <- seq.Date(as.Date("2010-01-01"), as.Date("2023-12-01"), by = "month")
n_noclegi <- length(date_noclegi)

trend_noclegi <- seq(14000, 24000, length.out = n_noclegi)

seasonal_noclegi <- c(0.45, 0.42, 0.58, 0.75, 0.88, 1.20, 1.75, 1.80, 1.35, 0.92, 0.55, 0.55)
m_noclegi <- as.integer(format(date_noclegi, "%m"))
seas_comp_noc <- seasonal_noclegi[m_noclegi]

yr_noclegi <- as.integer(format(date_noclegi, "%Y"))
covid_factor <- rep(1, n_noclegi)
shock_2020 <- yr_noclegi == 2020 & m_noclegi %in% 3:12
covid_factor[shock_2020] <- c(.2, .05, .05, .10, .15, .20, .30, .40, .35, .25)[
  m_noclegi[shock_2020] - 2
]
shock_2021 <- yr_noclegi == 2021 & m_noclegi %in% 1:6
covid_factor[shock_2021] <- c(.3, .25, .30, .35, .40, .55)[m_noclegi[shock_2021]]

noise_noc <- rnorm(n_noclegi, 0, 500)

noclegi_val <- pmax(500, trend_noclegi * seas_comp_noc * covid_factor + noise_noc)

df_noclegi <- data.frame(
  date    = format(date_noclegi, "%Y-%m-%d"),
  year    = as.integer(format(date_noclegi, "%Y")),
  month   = m_noclegi,
  noclegi = round(noclegi_val / 1000, 1)
)

write_synthetic_csv(df_noclegi, file.path(out_dir, "noclegi_pl.csv"), row.names = FALSE)

# ============================================================================
# 4. Sprzedaż detaliczna w Polsce (miesięczna, 2005-2023, mld PLN)
# ============================================================================

date_sprzedaz <- seq.Date(as.Date("2005-01-01"), as.Date("2023-12-01"), by = "month")
n_sp <- length(date_sprzedaz)

trend_sp <- seq(24, 95, length.out = n_sp)

seasonal_sp <- c(0.75, 0.72, 0.85, 0.90, 0.95, 0.98, 1.00, 1.00, 0.95, 1.00, 1.05, 1.40)
m_sp <- as.integer(format(date_sprzedaz, "%m"))
seas_sp <- seasonal_sp[m_sp]

noise_sp <- rnorm(n_sp, 0, 1.5)

sprzedaz_val <- trend_sp * seas_sp + noise_sp

df_sprzedaz <- data.frame(
  date     = format(date_sprzedaz, "%Y-%m-%d"),
  year     = as.integer(format(date_sprzedaz, "%Y")),
  month    = m_sp,
  sprzedaz = round(pmax(15, sprzedaz_val), 1)
)

write_synthetic_csv(df_sprzedaz, file.path(out_dir, "sprzedaz_detaliczna.csv"), row.names = FALSE)

# ============================================================================
# 5. Ceny pszenicy skupu (miesięczne, 2000-2023, PLN/dt)
# ============================================================================

date_pszen <- seq.Date(as.Date("2000-01-01"), as.Date("2023-12-01"), by = "month")
n_ps <- length(date_pszen)

# Fikcyjny szok cenowy umieszczony jawnie w 2022, a nie przesunięty
# przez błędną długość wcześniejszych segmentów.
base_price <- interpolate_trend(date_pszen,
  c("2000-01-01", "2003-01-01", "2005-01-01", "2007-01-01",
    "2009-01-01", "2010-01-01", "2012-01-01", "2014-01-01",
    "2018-01-01", "2021-12-01", "2022-02-01", "2022-06-01", "2023-12-01"),
  c(50, 50, 65, 55, 80, 120, 75, 90, 85, 95, 110, 300, 170)
)

seasonal_ps <- c(1.05, 1.02, 0.98, 0.96, 0.92, 0.88, 0.94, 1.08, 1.12, 1.08, 1.06, 1.05)
m_ps <- as.integer(format(date_pszen, "%m"))
seas_ps <- seasonal_ps[m_ps]

ar_noise <- arima.sim(model = list(ar = 0.75), n = n_ps, sd = 4)
stopifnot(length(base_price) == n_ps, length(seas_ps) == n_ps)
pszenica_val <- pmax(30, base_price * seas_ps + as.numeric(ar_noise))

df_pszen <- data.frame(
  date     = format(date_pszen, "%Y-%m-%d"),
  year     = as.integer(format(date_pszen, "%Y")),
  month    = m_ps,
  cena_pln = round(pszenica_val, 1)
)

write_synthetic_csv(df_pszen, file.path(out_dir, "ceny_pszenicy.csv"), row.names = FALSE)

# ============================================================================
# 6. PM10 Kraków (miesięczne, 2015-2023, µg/m³)
# ============================================================================

date_pm10 <- seq.Date(as.Date("2015-01-01"), as.Date("2023-12-01"), by = "month")
n_pm <- length(date_pm10)

trend_pm <- seq(62, 40, length.out = n_pm)

seasonal_pm <- c(2.2, 1.8, 1.3, 0.6, 0.4, 0.3, 0.3, 0.3, 0.5, 0.9, 1.6, 2.2)
m_pm <- as.integer(format(date_pm10, "%m"))
seas_pm <- seasonal_pm[m_pm]

noise_pm <- rnorm(n_pm, 0, 4)

pm10_val <- pmax(5, trend_pm * seas_pm + noise_pm)

df_pm10 <- data.frame(
  date  = format(date_pm10, "%Y-%m-%d"),
  year  = as.integer(format(date_pm10, "%Y")),
  month = m_pm,
  pm10  = round(pm10_val, 1)
)

write_synthetic_csv(df_pm10, file.path(out_dir, "pm10_krakow.csv"), row.names = FALSE)

# ============================================================================
# 7. WIG20 tygodniowy (2000-2023, punkty)
# ============================================================================

date_wig <- seq.Date(as.Date("2000-01-07"), as.Date("2023-12-29"), by = "week")
n_wig <- length(date_wig)

log_ret <- rnorm(n_wig, mean = 0.0008, sd = 0.025)

log_ret[52:78] <- rnorm(27, -0.005, 0.05)
log_ret[520:560] <- rnorm(41, -0.015, 0.06)
log_ret[800:840] <- rnorm(41, -0.010, 0.04)
log_ret[1000:1030] <- rnorm(31, 0.003, 0.035)

wig20 <- numeric(n_wig)
wig20[1] <- 1700
for (i in 2:n_wig) wig20[i] <- wig20[i-1] * exp(log_ret[i])
wig20 <- pmax(600, wig20)

log_returns <- c(NA, diff(log(wig20)))

df_wig <- data.frame(
  date        = format(date_wig, "%Y-%m-%d"),
  year        = as.integer(format(date_wig, "%Y")),
  wig20       = round(wig20, 0),
  log_return  = round(log_returns, 5)
)

write_synthetic_csv(df_wig, file.path(out_dir, "wig20_tygodniowy.csv"), row.names = FALSE)

cat("Dane wygenerowane. Pliki w katalogu:", out_dir, "\n")
