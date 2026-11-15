# ============================================================================
# Ryciny liczone w przeglądarce (modules/figures.js, silnik lcSF)
# ============================================================================
# Dane TeachingRatings są stałe, więc wszystko, co od nich zależy, R liczy raz
# przy budowaniu strony i przekazuje rycinom w `config`. W przeglądarce liczy się
# tylko to, co zmienia student: model własny (Ryc. 7.2, regresja w shared/lc_stats.js),
# podgląd danych (Ryc. 1.2) i podgląd konspektu.
# Wymaga: helpers.R (tr_data, tr_tropy, tr_labels, tr_label_term), lc_stat_figures.R.

# Wykres pudełkowy z punktami jak geom_boxplot() + geom_jitter(width = 0.12):
# kwartyle typu 7, wąsy do 1.5 IQR, rozsunięcie punktów losowane raz (stałe ziarno).
s10_box_config <- function(group_var, y_var, x_label, y_label, fills, seed = 10) {
  g <- factor(tr_data[[group_var]]); y <- tr_data[[y_var]]
  set.seed(seed)
  jitter <- runif(length(y), -0.12, 0.12)
  # Rozsunięcie w pionie jak domyślne geom_jitter(): 40% najmniejszego odstępu wartości.
  lift <- runif(length(y), -1, 1) * 0.4 * ggplot2::resolution(y, zero = FALSE)
  list(
    kind = "box", x_label = x_label, y_label = y_label, levels = levels(g),
    fills = rep(fills, length.out = nlevels(g)),
    boxes = lapply(levels(g), function(l) {
      v <- y[g == l]
      q <- unname(quantile(v, c(.25, .5, .75)))
      inside <- v[v >= q[1] - 1.5 * (q[3] - q[1]) & v <= q[3] + 1.5 * (q[3] - q[1])]
      list(q1 = q[1], med = q[2], q3 = q[3], lo = min(inside), hi = max(inside),
           out = as.list(setdiff(v, inside)))
    }),
    points = lapply(seq_along(y), function(i) c(round(as.integer(g)[i] + jitter[i], 3), round(y[i] + lift[i], 4)))
  )
}

# Wykres rozrzutu z prostą regresji i pasem ufności jak geom_smooth(method = "lm").
s10_scatter_config <- function(x_var, x_label, y_label = "Ocena kursu (eval)") {
  x <- tr_data[[x_var]]; y <- tr_data$eval
  grid <- seq(min(x), max(x), length.out = 80)
  pred <- predict(lm(y ~ x), data.frame(x = grid), interval = "confidence")
  list(
    kind = "scatter", x_label = x_label, y_label = y_label,
    points = lapply(seq_along(x), function(i) c(x[i], y[i])),
    fit = lapply(seq_along(grid), function(i) c(round(grid[i], 4), round(pred[i, "fit"], 5))),
    lwr = lapply(seq_along(grid), function(i) c(round(grid[i], 4), round(pred[i, "lwr"], 5))),
    upr = lapply(seq_along(grid), function(i) c(round(grid[i], 4), round(pred[i, "upr"], 5)))
  )
}

# Wykres jednego tropu (rozdział 5): korelacja (ilościowe) albo pudełka (grupy).
s10_trop_config <- function(id) {
  tr <- tr_tropy[[id]]
  if (tr$method == "cor") {
    s10_scatter_config(tr$var, unname(tr_labels[tr$var]))
  } else {
    s10_box_config(tr$var, "eval", unname(tr_labels[tr$var]), "Ocena kursu (eval)",
                   fills = c("niebo", "wrzos", "szalwia"))
  }
}
s10_plot <- function(config, ratio = "1.9/1", max_height = "320px") {
  lc_sf_figure("s10_plot", ratio = ratio, max_height = max_height, config = config)
}

# Seria modeli kontrolnych (Ryc. 7.1): liczona raz.
ch7_model_series <- tr_model_table(list(
  list(label = "1: beauty", model = lm(eval ~ beauty, data = tr_data)),
  list(label = "2: + cechy osoby", model = lm(eval ~ beauty + gender + age + minority + native + tenure, data = tr_data)),
  list(label = "3: + kontekst kursu", model = lm(eval ~ beauty + gender + age + minority + native + tenure + division + credits + students, data = tr_data)),
  list(label = "4: + response rate", model = lm(eval ~ beauty + gender + age + minority + native + tenure + division + credits + students + response.rate, data = tr_data))
))

ch7_models_table_ui <- function() {
  df <- ch7_model_series
  df$p_label <- lc_pval(df$p_beauty)
  lc_table_split(df,
    cols = list(
      lc_col("model", "Model", "row"),
      lc_col("beta_beauty", "β beauty", digits = 3),
      lc_col("p_label", "p"),
      lc_col("adj_r2", "adj.R²", digits = 3),
      lc_col("aic", "AIC")
    ),
    groups = list(c("beta_beauty", "p_label"), c("adj_r2", "aic")),
    label = "Seria modeli kontrolnych"
  )
}

s10_beta_config <- list(
  models = ch7_model_series$model, beta = ch7_model_series$beta_beauty,
  significant = ch7_model_series$p_beauty < 0.05
)

# Model własny (Ryc. 7.2): kolumny macierzy modelu dla każdej zmiennej kontrolnej
# (zmienne jakościowe jako 0/1, jak model.matrix()), z etykietami współczynników.
s10_custom_config <- local({
  vars <- c("gender", "age", "minority", "native", "tenure", "division", "credits", "students", "response.rate")
  list(
    y = tr_data$eval, beauty = tr_data$beauty, beauty_label = tr_label_term("beauty"),
    vars = lapply(stats::setNames(vars, vars), function(v) {
      mm <- model.matrix(stats::reformulate(v), tr_data)[, -1, drop = FALSE]
      lapply(seq_len(ncol(mm)), function(j) list(label = tr_label_term(colnames(mm)[j]), values = unname(mm[, j])))
    })
  )
})

# Podgląd danych (Ryc. 1.2): cała tabela jest w dokumencie; przeglądarka sortuje
# wiersze, pokazuje okno dziesięciu i chowa kolumny spoza zestawu kluczowego.
s10_data_key_cols <- c("eval", "beauty", "gender", "age", "minority", "native",
                       "division", "credits", "students", "allstudents", "response.rate")
s10_data_sort_vars <- c("eval", "beauty", "response.rate", "age")

s10_data_table <- function() {
  out <- cbind(data.frame(row = rownames(tr_data), stringsAsFactors = FALSE), tr_data)
  # Liczby całkowite bez miejsc po kropce, pozostałe liczby z 2 miejscami.
  cols <- c(
    list(lc_col("row", "", "row")),
    lapply(names(out)[-1], function(key) {
      x <- out[[key]]
      if (is.numeric(x)) lc_col(key, key, "num", digits = if (is.integer(x)) 0 else 2) else lc_col(key, key, "text")
    })
  )
  hidden <- which(!names(tr_data) %in% s10_data_key_cols) + 1L      # +1: kolumna numeru wiersza
  tagList(
    tags$style(HTML(paste0(
      sprintf(".s10-data.is-key tr > :nth-child(%d)", hidden), collapse = ",\n"), " { display: none; }")),
    lc_table(out, cols, scroll = TRUE, sticky_first = TRUE, label = "Podgląd danych TeachingRatings")
  )
}
s10_data_config <- list(sort = lapply(stats::setNames(s10_data_sort_vars, s10_data_sort_vars), function(v) tr_data[[v]]))
