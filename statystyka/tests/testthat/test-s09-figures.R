# Wykład 09: opisy rycin liczone w R przy budowaniu (09-case-studies/modules/figures.R)
# i ogólna rycina „opis z R” (lc_sf_spec() w shared/lc_stat_figures.R).

testthat::test_that("opis ryciny: warstwy, zakresy i pomijanie pustych argumentów", {
  env <- new.env(parent = asNamespace("shiny"))
  shared <- file.path(dirname(stat_root), "shared")
  sys.source(file.path(shared, "palette.R"), envir = env)
  sys.source(file.path(shared, "lecture_layout.R"), envir = env)
  sys.source(file.path(shared, "lc_stat_figures.R"), envir = env)

  spec <- env$sf_spec(c(0, 1), c(-2, 5), list(env$sf_points(1, 2), NULL), "x", "y", legend = NULL)
  testthat::expect_false("legend" %in% names(spec))          # NULL nie trafia do JSON jako {}
  testthat::expect_true(spec$yNegative)                       # oś poniżej zera: podpisy ujemne
  testthat::expect_length(spec$layers, 1)
  json <- jsonlite::toJSON(spec, auto_unbox = TRUE)
  testthat::expect_match(as.character(json), '"data":[[1,2]]', fixed = TRUE)   # jeden punkt zostaje parą w tablicy

  testthat::expect_equal(env$sf_range(c(0, 10)), c(-0.5, 10.5))
  # Pudełko jak geom_boxplot(): kwartyle typu 7, wąsy do 1.5 IQR.
  b <- env$sf_box_stats(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 30))
  testthat::expect_equal(c(b$q1, b$med, b$q3, b$lo, b$hi), c(3.25, 5.5, 7.75, 1, 9))
  testthat::expect_equal(b$out, 30)
  # Histogram na zadanych granicach: przedziały (a, b], jak hist().
  h <- env$sf_hist(c(1, 2, 2, 3, 10), breaks = c(0, 2, 4, 10))
  testthat::expect_equal(vapply(h$data, function(r) r[3], 0), c(3, 1, 1))
  # Wykres Q-Q: prosta przez kwartyle, punkty w kolejności rosnącej.
  set.seed(1); v <- rnorm(40, 10, 2)
  qq <- env$sf_qq(v)
  testthat::expect_equal(vapply(qq[[2]]$data, function(p) p[2], 0), round(sort(v), 4))
  # Prosta regresji z pasem ufności: krańce prostej jak predict(lm()).
  x <- 1:20; y <- 3 + 2 * x + sin(x)
  sm <- env$sf_smooth_lm(x, y)
  testthat::expect_equal(sm[[2]]$data[[1]][2], unname(predict(lm(y ~ x), data.frame(x = 1))), tolerance = 1e-4)
  fig <- as.character(env$lc_sf_spec(variants = list(a = spec, b = spec), reads = list(a = list(k = "1"), b = list(k = "2"))))
  testthat::expect_match(fig, 'data-fig="lc_spec"', fixed = TRUE)
})

testthat::test_that("wykład 09: warianty rycin i liczby jak w dawnych wynikach serwera", {
  testthat::skip_if_not_installed("callr")
  res <- callr::r(function(app) {
    setwd(app)
    options(lc.static = TRUE)
    env <- new.env(parent = globalenv())
    sys.source("app.R", envir = env)
    cfg <- function(tag) {
      html <- as.character(tag)
      raw <- regmatches(html, regexpr('data-config="[^"]*"', html))
      jsonlite::fromJSON(gsub("&quot;", '"', gsub("&amp;", "&", gsub("&lt;", "<", gsub("&gt;", ">", sub('^data-config="', "", sub('"$', "", raw)))))),
                         simplifyVector = FALSE)
    }
    reg <- cfg(env$ch1_reg_fig())
    m <- lm(score ~ str + income + english + lunch, data = env$ca)
    m1 <- lm(score ~ str, data = env$ca)
    sex <- cfg(env$ch2_sex_fig()); or <- cfg(env$ch3_or_fig()); eda <- cfg(env$ch1_eda_fig())
    t <- htmltools::renderTags(env$ui)
    list(
      reg_keys = names(reg$variants), reg_first = names(reg$variants)[1],
      reg_reads = reg$reads[["str+income+english+lunch"]], none_reads = reg$reads[["none"]],
      want = list(reg_adj = env$lc_fmt(summary(m)$adj.r.squared, 3), reg_aic = env$lc_fmt(AIC(m), 0),
                  reg_rmse = env$lc_fmt(sqrt(mean(residuals(m)^2)), 1)),
      want_none = env$lc_fmt(summary(m1)$adj.r.squared, 3),
      reg_rows = lengths(regmatches(reg$html[["str+income+english+lunch"]]$coefs, gregexpr("<tr", reg$html[["str+income+english+lunch"]]$coefs))),
      sex = sex$reads, or_keys = names(or$variants), or_rows = c(m0 = length(or$variants$m0$layers[[3]]$data), m1 = length(or$variants$m1$layers[[3]]$data)),
      eda_keys = names(eda$variants), eda_income = eda$reads$income,
      outputs = lengths(regmatches(t$html, gregexpr("shiny-html-output|shiny-plot-output", t$html)))
    )
  }, args = list(app = file.path(stat_root, "09-case-studies")), timeout = 240)

  testthat::expect_length(res$reg_keys, 32)                        # każdy podzbiór pięciu predyktorów
  testthat::expect_equal(res$reg_first, "str+income+english+lunch")  # ustawienie domyślne rysuje się pierwsze
  testthat::expect_equal(res$reg_reads, res$want)
  testthat::expect_equal(res$none_reads$reg_adj, res$want_none)    # nic nie zaznaczono: model z samym STR
  testthat::expect_equal(res$reg_rows, 6L)                         # nagłówek i pięć współczynników
  # Liczby z tekstu wykładu: samice 158 g (p = 0.023), samce −105 g.
  testthat::expect_equal(res$sex$Samice$sex_ac, "158 g, p = 0.023")
  testthat::expect_match(res$sex$Samce$sex_ac, "^-105 g")
  testthat::expect_setequal(res$or_keys, c("m0", "m1"))
  testthat::expect_equal(res$or_rows, c(m0 = 3L, m1 = 5L))
  testthat::expect_length(res$eda_keys, 7)
  testthat::expect_equal(res$eda_income$eda_n, "420")
  testthat::expect_equal(res$outputs, 0L)
})
