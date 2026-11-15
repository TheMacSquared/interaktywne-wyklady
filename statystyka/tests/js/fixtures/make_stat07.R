# Wzorce R dla statystyka/07-regresja/modules/figures.js: zapisuje statystyka/tests/js/fixtures/stat07.json.
# Uruchamianie z katalogu repo: Rscript statystyka/tests/js/fixtures/make_stat07.R
set.seed(5)
x <- sort(runif(35, 0, 10)); y <- 3 * sin(x) + rnorm(35)
xt <- seq(0.3, 9.7, length.out = 7)
out <- list(x = x, y = y, xt = xt, poly = lapply(c(1, 4, 12, 15), function(k) {
  m <- lm(y ~ poly(x, k))
  list(k = k, r2 = summary(m)$r.squared, adj = summary(m)$adj.r.squared, aic = AIC(m), bic = BIC(m),
       pred = unname(predict(m, data.frame(x = xt))))
}))
d <- data.frame(o = y, a = x, b = sqrt(x) + rnorm(35, 0, .3))
m <- lm(o ~ a + b, d)
out$lm <- list(r2 = summary(m)$r.squared, adj = summary(m)$adj.r.squared, aic = AIC(m), bic = BIC(m), rmse = sqrt(mean(resid(m)^2)), b = d$b)
s <- lm(y ~ x)
out$simple <- list(b0 = coef(s)[[1]], b1 = coef(s)[[2]], r = cor(x, y), p1 = summary(s)$coefficients[2, 4], se1 = summary(s)$coefficients[2, 2])
writeLines(jsonlite::toJSON(out, auto_unbox = TRUE, digits = NA), "statystyka/tests/js/fixtures/stat07.json")
