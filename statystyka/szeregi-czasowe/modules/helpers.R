# ============================================================================
# HELPERS — Szeregi Czasowe
# Wczytywanie danych i wspólne funkcje generowania / formatowania
# ============================================================================

# ============================================================================
# WCZYTYWANIE DANYCH (lokalne CSV z dane/)
# ============================================================================

.ts_load <- function(filename) {
  path <- file.path(app_dir, "dane", filename)
  if (file.exists(path)) {
    read.csv(path, stringsAsFactors = FALSE)
  } else {
    NULL
  }
}

.ts_warszawa <- local({
  df <- .ts_load("warszawa_temperatura.csv")
  if (!is.null(df)) {
    df$date <- as.Date(df$date)
    df
  } else {
    ts_gen_temp_warsaw()
  }
})

.ts_bezrobocie <- local({
  df <- .ts_load("bezrobocie_pl.csv")
  if (!is.null(df)) { df$date <- as.Date(df$date); df }
  else NULL
})

.ts_noclegi <- local({
  df <- .ts_load("noclegi_pl.csv")
  if (!is.null(df)) { df$date <- as.Date(df$date); df }
  else NULL
})

.ts_sprzedaz <- local({
  df <- .ts_load("sprzedaz_detaliczna.csv")
  if (!is.null(df)) { df$date <- as.Date(df$date); df }
  else NULL
})

.ts_pszenica <- local({
  df <- .ts_load("ceny_pszenicy.csv")
  if (!is.null(df)) { df$date <- as.Date(df$date); df }
  else NULL
})

.ts_pm10 <- local({
  df <- .ts_load("pm10_krakow.csv")
  if (!is.null(df)) { df$date <- as.Date(df$date); df }
  else NULL
})

.ts_wig20 <- local({
  df <- .ts_load("wig20_tygodniowy.csv")
  if (!is.null(df)) { df$date <- as.Date(df$date); df }
  else NULL
})

# ============================================================================
# GENERATORY DANYCH SYNTETYCZNYCH
# ============================================================================

ts_gen_ar <- function(n = 200, phi = 0.8, sigma = 1, seed = 42) {
  set.seed(seed)
  arima.sim(model = list(ar = phi), n = n, sd = sigma)
}

ts_gen_ma <- function(n = 200, theta = 0.7, sigma = 1, seed = 42) {
  set.seed(seed)
  arima.sim(model = list(ma = theta), n = n, sd = sigma)
}

ts_gen_arma <- function(n = 200, phi = 0.6, theta = 0.5, sigma = 1, seed = 42) {
  set.seed(seed)
  arima.sim(model = list(ar = phi, ma = theta), n = n, sd = sigma)
}

ts_gen_arima <- function(n = 200, p = 1, d = 1, q = 0, sigma = 1, seed = 42) {
  set.seed(seed)
  if (p == 0 && q == 0) {
    ts <- cumsum(rnorm(n, 0, sigma))
  } else {
    ar_spec <- if (p > 0) rep(0.5 / p, p) else NULL
    ma_spec <- if (q > 0) rep(0.4 / q, q) else NULL
    model <- list()
    if (!is.null(ar_spec)) model$ar <- ar_spec
    if (!is.null(ma_spec)) model$ma <- ma_spec
    ts <- arima.sim(model = model, n = n + d, sd = sigma)
    if (d > 0) ts <- cumsum(ts[seq_len(n)])
  }
  as.numeric(ts)
}

ts_gen_seasonal <- function(n_years = 10, amplitude = 8, trend = 0.15,
                             noise = 1.2, seed = 42) {
  set.seed(seed)
  n <- n_years * 12
  t <- seq_len(n)
  base_mean <- 10
  trend_comp  <- trend * t
  seas_comp   <- amplitude * sin(2 * pi * t / 12 - pi / 2)
  noise_comp  <- rnorm(n, 0, noise)
  as.numeric(base_mean + trend_comp + seas_comp + noise_comp)
}

ts_gen_white_noise <- function(n = 200, sigma = 1, seed = 42) {
  set.seed(seed)
  rnorm(n, 0, sigma)
}

ts_gen_rw <- function(n = 200, sigma = 1, seed = 42) {
  set.seed(seed)
  cumsum(rnorm(n, 0, sigma))
}

ts_gen_fermentation <- function(n = 120, seed = 42) {
  set.seed(seed)
  t <- seq_len(n)
  base <- 20 + 10 * (1 - exp(-t / 30))
  noise <- rnorm(n, 0, 0.4)
  anomaly <- ifelse(t %in% c(45, 46, 47), 3.5, 0)
  base + noise + anomaly
}

# ============================================================================
# FUNKCJE KONWERSJI DO OBIEKTÓW ts()
# ============================================================================

df_to_ts <- function(df, value_col, start_year = NULL, start_month = 1,
                     frequency = 12) {
  if (!is.null(start_year)) {
    ts(df[[value_col]], start = c(start_year, start_month), frequency = frequency)
  } else {
    ts(df[[value_col]], frequency = frequency)
  }
}

warszawa_ts <- function() {
  df <- .ts_warszawa
  ts(df$temp, start = c(min(df$year), 1), frequency = 12)
}

bezrobocie_ts <- function() {
  df <- .ts_bezrobocie
  ts(df$stopa, start = c(min(df$year), 1), frequency = 12)
}

noclegi_ts <- function() {
  df <- .ts_noclegi
  ts(df$noclegi, start = c(min(df$year), 1), frequency = 12)
}

sprzedaz_ts <- function() {
  df <- .ts_sprzedaz
  ts(df$sprzedaz, start = c(min(df$year), 1), frequency = 12)
}

pszenica_ts <- function() {
  df <- .ts_pszenica
  ts(df$cena_pln, start = c(min(df$year), 1), frequency = 12)
}

pm10_ts <- function() {
  df <- .ts_pm10
  ts(df$pm10, start = c(min(df$year), 1), frequency = 12)
}

wig20_ts <- function() {
  df <- .ts_wig20
  ts(df$wig20, frequency = 52)
}

wig20_log_returns_ts <- function() {
  df <- .ts_wig20
  ts(df$log_return[-1], frequency = 52)
}

# ============================================================================
# KONWERSJA ts() → data.frame (do ggplot)
# ============================================================================

ts_to_df <- function(x, value_name = "value") {
  t_vals <- time(x)
  years  <- floor(t_vals)
  months <- round((t_vals - years) * frequency(x)) + 1
  if (frequency(x) == 12) {
    date <- as.Date(paste(years, months, "01", sep = "-"))
  } else if (frequency(x) == 52) {
    date <- as.Date(paste(years, "01", "01", sep = "-")) +
            floor((months - 1) * 365 / 52)
  } else {
    date <- as.numeric(t_vals)
  }
  df <- data.frame(date = date, value = as.numeric(x))
  names(df)[2] <- value_name
  df
}

# ============================================================================
# DANE DEFINICJI ZBIORÓW (dla widgetów wyboru)
# ============================================================================

.ts_datasets <- list(
  warszawa = list(
    label  = "Temperatura Warszawa",
    unit   = "°C",
    get_ts = warszawa_ts,
    get_df = function() ts_to_df(warszawa_ts(), "temp"),
    desc   = "Miesięczna średnia temperatura w Warszawie (1951–2023). Wyraźna sezonowość ± 22°C i powolny trend ocieplenia."
  ),
  bezrobocie = list(
    label  = "Stopa bezrobocia PL",
    unit   = "%",
    get_ts = bezrobocie_ts,
    get_df = function() ts_to_df(bezrobocie_ts(), "stopa"),
    desc   = "Miesięczna stopa bezrobocia w Polsce (2000–2023). Trend malejący z epizodami wzrostu i słabą sezonowością."
  ),
  noclegi = list(
    label  = "Noclegi w Polsce",
    unit   = "tys.",
    get_ts = noclegi_ts,
    get_df = function() ts_to_df(noclegi_ts(), "noclegi"),
    desc   = "Miesięczna liczba noclegów w Polsce (2010–2023). Silna sezonowość wakacyjna i wyraźny dołek pandemii COVID-19."
  ),
  sprzedaz = list(
    label  = "Sprzedaż detaliczna PL",
    unit   = "mld PLN",
    get_ts = sprzedaz_ts,
    get_df = function() ts_to_df(sprzedaz_ts(), "sprzedaz"),
    desc   = "Miesięczna sprzedaż detaliczna w Polsce (2005–2023). Rosnący trend i silny szczyt grudniowy."
  ),
  pszenica = list(
    label  = "Ceny pszenicy skupu",
    unit   = "PLN/dt",
    get_ts = pszenica_ts,
    get_df = function() ts_to_df(pszenica_ts(), "cena"),
    desc   = "Miesięczne ceny skupu pszenicy w Polsce (2000–2023). Cykliczność, zmienna wariancja, gwałtowny skok w 2022 r."
  ),
  pm10 = list(
    label  = "PM10 Kraków",
    unit   = "µg/m³",
    get_ts = pm10_ts,
    get_df = function() ts_to_df(pm10_ts(), "pm10"),
    desc   = "Miesięczne stężenie pyłu PM10 w Krakowie (2015–2023). Silna sezonowość grzewcza i malejący trend."
  )
)

.ts_dataset_choices <- setNames(
  names(.ts_datasets),
  vapply(.ts_datasets, `[[`, character(1), "label")
)

# Subset .ts_dataset_choices by internal key names (values), not by display labels
.ts_choices_for <- function(...) {
  keys <- c(...)
  .ts_dataset_choices[.ts_dataset_choices %in% keys]
}

# ============================================================================
# FORMATTERY WYNIKÓW
# ============================================================================

fmt_num <- function(x, digits = 2) {
  s <- formatC(round(x, digits), format = "f", digits = digits)
  gsub("\\.", ",", s)
}

fmt_pct <- function(x, digits = 1) {
  paste0(fmt_num(x * 100, digits), "%")
}

# Opis metody prognozowania po polsku
forecast_method_label <- function(method) {
  switch(method,
    naive      = "Metoda naiwna",
    snaive     = "Naiwna sezonowa",
    ets        = "Wygładzanie wykładnicze (ETS)",
    arima      = "ARIMA (auto)",
    "Nieznana metoda"
  )
}

# ============================================================================
# METRYKI DOKŁADNOŚCI PROGNOZY
# ============================================================================

compute_accuracy_metrics <- function(actual, predicted) {
  e  <- actual - predicted
  ae <- abs(e)
  list(
    mae  = mean(ae, na.rm = TRUE),
    rmse = sqrt(mean(e^2, na.rm = TRUE)),
    mape = mean(ae / abs(actual) * 100, na.rm = TRUE)
  )
}

# ============================================================================
# GGPLOT HELPERS DLA TS
# ============================================================================

ts_line_plot <- function(df, x_col = "date", y_col = "value",
                          color = NULL, y_label = NULL, title = NULL) {
  col <- if (!is.null(color)) color else upwr_accent
  p <- ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_line(color = col, linewidth = 0.9) +
    labs(x = NULL, y = y_label, title = title) +
    theme_upwr()
  p
}

acf_df <- function(x, lag.max = 24, type = "correlation") {
  acf_obj <- acf(x, lag.max = lag.max, type = type, plot = FALSE)
  data.frame(
    lag = as.numeric(acf_obj$lag),
    acf = as.numeric(acf_obj$acf)
  )
}

pacf_df <- function(x, lag.max = 24) {
  pacf_obj <- pacf(x, lag.max = lag.max, plot = FALSE)
  data.frame(
    lag  = as.numeric(pacf_obj$lag),
    pacf = as.numeric(pacf_obj$acf)
  )
}

ci_acf <- function(n, lag.max = 24, level = 0.95) {
  qnorm((1 + level) / 2) / sqrt(n)
}

plot_acf_gg <- function(x, lag.max = 24, type = "ACF",
                         title = NULL, show_ci = TRUE) {
  n <- length(x)
  ci <- ci_acf(n, lag.max)

  if (type == "ACF") {
    df  <- acf_df(x, lag.max)
    df  <- df[df$lag > 0, ]
    y   <- df$acf
    ylab <- "Autokorelacja r(k)"
    df$val <- y
  } else {
    df  <- pacf_df(x, lag.max)
    y   <- df$pacf
    ylab <- "Cząstkowa autokorelacja"
    df$val <- y
  }

  ggplot(df, aes(x = lag, y = val)) +
    geom_hline(yintercept = 0, color = upwr_reference) +
    {if (show_ci) list(
      geom_hline(yintercept =  ci, linetype = "dashed", color = upwr_secondary, linewidth = 0.7),
      geom_hline(yintercept = -ci, linetype = "dashed", color = upwr_secondary, linewidth = 0.7)
    )} +
    geom_segment(aes(xend = lag, yend = 0), color = upwr_accent, linewidth = 1.1) +
    geom_point(color = upwr_accent, size = 2.2) +
    scale_x_continuous(breaks = seq(0, lag.max, by = 4)) +
    labs(x = "Lag k", y = ylab, title = title) +
    theme_upwr()
}

plot_forecast_gg <- function(ts_data, fc, value_label = "wartość",
                              show_actual = TRUE) {
  n <- length(ts_data)
  t_obs <- seq_len(n)
  df_obs <- data.frame(t = t_obs, value = as.numeric(ts_data), type = "Dane historyczne")

  fc_mean <- as.numeric(fc$mean)
  n_fc    <- length(fc_mean)
  t_fc    <- n + seq_len(n_fc)
  df_fc   <- data.frame(t = t_fc, value = fc_mean, type = "Prognoza")

  df_all  <- rbind(df_obs, df_fc)

  p <- ggplot(df_all, aes(x = t, y = value, color = type, linetype = type)) +
    geom_line(linewidth = 0.9) +
    scale_color_manual(
      values = c("Dane historyczne" = upwr_secondary,
                 "Prognoza"          = upwr_accent),
      name = NULL
    ) +
    scale_linetype_manual(
      values = c("Dane historyczne" = "solid",
                 "Prognoza"          = "dashed"),
      name = NULL
    ) +
    labs(x = "Czas", y = value_label) +
    theme_upwr() +
    theme(legend.position = "bottom")

  if (!is.null(fc$lower) && !is.null(fc$upper)) {
    ci80 <- data.frame(
      t  = t_fc,
      lo = as.numeric(fc$lower[, 1]),
      hi = as.numeric(fc$upper[, 1])
    )
    ci95 <- data.frame(
      t  = t_fc,
      lo = as.numeric(fc$lower[, 2]),
      hi = as.numeric(fc$upper[, 2])
    )
    p <- p +
      geom_ribbon(data = ci95, aes(x = t, ymin = lo, ymax = hi),
                  inherit.aes = FALSE, fill = upwr_accent, alpha = 0.12) +
      geom_ribbon(data = ci80, aes(x = t, ymin = lo, ymax = hi),
                  inherit.aes = FALSE, fill = upwr_accent, alpha = 0.20)
  }

  p
}
