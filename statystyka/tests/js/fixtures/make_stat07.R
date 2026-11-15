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
# Regresja logistyczna: glm(binomial), Wald, przedziały profilowe (confint) i AIC/BIC.
set.seed(11)
g <- round(runif(150, 0, 40), 1); s <- round(pmin(pmax(rnorm(150, 3.5, 0.6), 2), 5), 2)
z <- rbinom(150, 1, plogis(-4 + 0.08 * g + 1.2 * s))
lg <- glm(z ~ g + s, family = binomial)
ci <- suppressMessages(confint(lg))
out$logit <- list(g = g, s = s, z = z, est = unname(coef(lg)), se = unname(summary(lg)$coefficients[, 2]),
                  p = unname(summary(lg)$coefficients[, 4]), aic = AIC(lg), bic = BIC(lg),
                  lo = unname(ci[, 1]), hi = unname(ci[, 2]), fitted = unname(fitted(lg))[1:5])
writeLines(jsonlite::toJSON(out, auto_unbox = TRUE, digits = NA), "statystyka/tests/js/fixtures/stat07.json")
