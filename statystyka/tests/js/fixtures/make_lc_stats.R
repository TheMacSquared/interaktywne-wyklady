# Wzorce R dla shared/lc_stats.js: zapisuje statystyka/tests/js/fixtures/lc_stats.json.
# Uruchamianie z katalogu repo: Rscript statystyka/tests/js/fixtures/make_lc_stats.R
suppressMessages(library(rstatix))
set.seed(20261115)
samples <- list(
  n3 = round(rnorm(3, 170, 10), 2), n5 = round(rnorm(5, 170, 10), 2), n8 = round(rgamma(8, 2, scale = 5), 2),
  n11 = round(rnorm(11), 3), n12 = round(rt(12, 3), 3), n50 = round(rnorm(50, 170, 10), 2),
  skew80 = round(rgamma(80, shape = 2, scale = 5) + 1, 3), unif200 = round(runif(200, 150, 190), 2),
  heavy500 = round(170 + 10 * rt(500, 3), 2), big2000 = round(120 + 6 * rgamma(2000, 10), 2)
)
shapiro <- lapply(samples, function(x) {
  s <- shapiro.test(x); q <- quantile(x, c(.25, .75), names = FALSE); qx <- qnorm(c(.25, .75))
  slope <- diff(q) / diff(qx)
  list(x = x, W = unname(s$statistic), p = s$p.value, mean = mean(x), sd = sd(x), median = median(x),
       q10 = unname(quantile(x, .1)), qq_slope = slope, qq_intercept = q[1] - slope * qx[1],
       qq_first = qnorm(ppoints(length(x)))[1])
})
# format_p_value() jak w shared/shared.R.
format_p_value <- function(p_value) {
  if (is.na(p_value)) return("NA")
  if (p_value < 0.0001) return("<0.0001")
  rounded <- signif(p_value, 2)
  if (rounded >= 1) return(">0.99")
  s <- formatC(rounded, format = "fg", digits = 2)
  if (!grepl("\\.", s)) s <- paste0(s, ".0")
  s
}
for (k in names(shapiro)) shapiro[[k]]$p_text <- format_p_value(shapiro[[k]]$p)
two <- lapply(list(c(30, 30, 10, 10), c(20, 80, 20, 5), c(8, 12, 3, 9), c(100, 15, 10, 25)), function(s) {
  a <- round(rnorm(s[1], 170, s[3]), 2); b <- round(rnorm(s[2], 175, s[4]), 2)
  df <- data.frame(value = c(a, b), group = factor(rep(c("A", "B"), c(s[1], s[2]))))
  lev <- rstatix::levene_test(df, value ~ group); bart <- bartlett.test(value ~ group, data = df)
  st <- t.test(a, b, var.equal = TRUE); we <- t.test(a, b)
  list(a = a, b = b,
       levene = list(F = lev$statistic, df1 = lev$df1, df2 = lev$df2, p = lev$p),
       bartlett = list(K2 = unname(bart$statistic), p = bart$p.value),
       student = list(t = unname(st$statistic), df = unname(st$parameter), p = st$p.value),
       welch = list(t = unname(we$statistic), df = unname(we$parameter), p = we$p.value))
})
tabs <- list(c(5, 5, 5, 5), c(1, 9, 8, 2), c(0, 3, 7, 10), c(12, 3, 4, 21), c(1, 0, 4, 15), c(30, 20, 25, 25), c(2, 18, 17, 163), c(0, 0, 5, 15))
tables <- lapply(tabs, function(v) {
  m <- matrix(v, 2, byrow = TRUE)
  chi <- tryCatch(suppressWarnings(chisq.test(m, correct = FALSE)), error = function(e) NULL)
  list(tab = list(v[1:2], v[3:4]), X2 = if (is.null(chi)) NA else unname(chi$statistic),
       chi_p = if (is.null(chi)) NA else chi$p.value, fisher_p = fisher.test(m)$p.value)
})
pvals <- c(0.5, 0.05, 0.0499, 0.123456, 0.00012, 0.00009999, 0.999, 0.9951, 0.3, 0.0456, 0.00105, 1)
out <- list(shapiro = shapiro, two = two, tables = tables,
  pf = list(args = list(c(2.5, 1, 58), c(0.3, 3, 20), c(11, 2, 7), c(1, 1, 1), c(45, 4, 100)),
            upper = c(pf(2.5, 1, 58, lower.tail = FALSE), pf(.3, 3, 20, lower.tail = FALSE), pf(11, 2, 7, lower.tail = FALSE),
                      pf(1, 1, 1, lower.tail = FALSE), pf(45, 4, 100, lower.tail = FALSE))),
  pvalue = list(p = pvals, text = vapply(pvals, format_p_value, "")),
  round = list(x = c(10.004, 2.5, 3.14159, 100, 0.125, 12.3456), d = c(2, 0, 3, 1, 2, 1),
               text = c(as.character(round(10.004, 2)), as.character(round(2.5, 0)), as.character(round(3.14159, 3)),
                        as.character(round(100, 1)), as.character(round(0.125, 2)), as.character(round(12.3456, 1)))))
dir.create("statystyka/tests/js/fixtures", showWarnings = FALSE)
writeLines(jsonlite::toJSON(out, auto_unbox = TRUE, digits = NA, na = "null"), "statystyka/tests/js/fixtures/lc_stats.json")
cat("ok\n")
