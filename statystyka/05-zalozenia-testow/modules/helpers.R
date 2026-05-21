# ============================================================================
# FUNKCJE POMOCNICZE - Zalozenia testow
# ============================================================================

# Kolory domenowe dla założeń testów. Wartości pochodzą z palety UPWr.
col_ok   <- unname(upwr_cat["szalwia"])   # założenie spełnione
col_fail <- upwr_accent                    # założenie naruszone
col_test <- unname(upwr_cat["niebo"])      # dane/test
col_alt  <- unname(upwr_cat["wrzos"])      # alternatywa

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

# Nazwy rozkladow
dist_names_pl <- c(
  "normal"     = "Normalny",
  "skewed"     = "Prawoskośny (Gamma)",
  "heavy_tail" = "Ciężkie ogony (t)",
  "bimodal"    = "Dwumodalny",
  "uniform"    = "Jednostajny"
)
