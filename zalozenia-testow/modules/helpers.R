# ============================================================================
# FUNKCJE POMOCNICZE - Zalozenia testow
# ============================================================================

# Generowanie danych o roznych rozkladach
generate_test_data <- function(n = 50, dist = "normal") {
  set.seed(NULL)
  switch(dist,
    "normal"     = rnorm(n, 170, 10),
    "skewed"     = rgamma(n, shape = 2, scale = 5) + 150,
    "heavy_tail" = 170 + 10 * rt(n, df = 3),
    "bimodal"    = c(rnorm(n/2, 160, 4), rnorm(n/2, 180, 4)),
    "uniform"    = runif(n, 150, 190),
    rnorm(n, 170, 10)
  )
}

# Generowanie danych do 2 grup o roznej wariancji
generate_two_groups <- function(n1 = 30, n2 = 30, sd1 = 10, sd2 = 10,
                                 mean1 = 170, mean2 = 175) {
  set.seed(NULL)
  data.frame(
    value = c(rnorm(n1, mean1, sd1), rnorm(n2, mean2, sd2)),
    group = factor(c(rep("A", n1), rep("B", n2)))
  )
}

# Generowanie danych regresji z naruszonymi zalozeniami
generate_reg_violations <- function(n = 100, violation = "none") {
  set.seed(NULL)
  x <- runif(n, 0, 10)

  if (violation == "none") {
    y <- 2 + 3 * x + rnorm(n, 0, 2)
  } else if (violation == "heteroscedasticity") {
    y <- 2 + 3 * x + rnorm(n, 0, 0.5 * x)
  } else if (violation == "nonlinear") {
    y <- 2 + 3 * x - 0.3 * x^2 + rnorm(n, 0, 1.5)
  } else if (violation == "non_normal_resid") {
    y <- 2 + 3 * x + (rexp(n, 0.5) - 2)
  } else if (violation == "autocorrelation") {
    errors <- arima.sim(n = n, model = list(ar = 0.8), sd = 2)
    y <- 2 + 3 * x + as.numeric(errors)
  }

  data.frame(x = x, y = y)
}

# Nazwy rozkladow
dist_names_pl <- c(
  "normal"     = "Normalny",
  "skewed"     = "Prawoskośny (Gamma)",
  "heavy_tail" = "Ciężkie ogony (t)",
  "bimodal"    = "Dwumodalny",
  "uniform"    = "Jednostajny"
)

