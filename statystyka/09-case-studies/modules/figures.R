# ============================================================================
# Ryciny studiów przypadku: opisy rysunków liczone w R przy budowaniu strony
# ============================================================================
# Wszystkie trzy studia pracują na stałych zbiorach danych, a kontrolki mają
# skończoną liczbę ustawień. Dlatego R liczy każdy wariant raz (te same funkcje
# co dawniej na serwerze) i składa opis rysunku dla ryciny lc_spec
# (shared/lc_stat_figures.R); przeglądarka tylko rysuje wybrany wariant.
# Funkcje są wywoływane w opisach rozdziałów, gdy dane (ca, pg, tt) już istnieją.
# Wymaga: helpers.R, lc_stat_figures.R.

s09_pval_text <- function(p) if (p < 0.001) "< 0.001" else lc_fmt(p, 3)
s09_json <- function(x) as.character(x)          # fragment HTML jako napis do config
# Rozsunięcie punktów w poziomie jak geom_jitter(width = w): stałe ziarno, żeby
# rysunek był ten sam przy każdym budowaniu.
s09_jitter <- function(n, w, seed) { set.seed(seed); stats::runif(n, -w, w) }

# ---------------------------------------------------------------------------
# Studium 1: CASchools
# ---------------------------------------------------------------------------

ch1_eda_labels <- c(
  score = "Średni wynik testu (pkt)", str = "Uczniowie na nauczyciela",
  expenditure = "Wydatki na ucznia (USD)", income = "Dochód okręgu (tys. USD)",
  english = "Uczący się angielskiego (%)", lunch = "Z dotacją do obiadu (%)",
  calworks = "Z rodzin na zasiłku (%)"
)

# Ryc. 1.2: histogram i wykres pudełkowy wybranej zmiennej.
ch1_eda_fig <- function() {
  vars <- names(ch1_eda_labels)
  variants <- lapply(stats::setNames(vars, vars), function(var) {
    x <- ca[[var]]
    hist <- sf_hist(x, bins = 30, color = "@niebo", alpha = 0.6)
    top <- max(vapply(hist$data, function(b) b[3], 0))
    box <- sf_box_stats(x)
    list(charts = list(
      sf_spec(range(unlist(lapply(hist$data, function(b) b[1:2]))), c(0, top * 1.05), list(hist),
              ch1_eda_labels[[var]], "Liczba okręgów", aria = "Histogram wybranej zmiennej."),
      sf_spec(c(-0.6, 0.6), sf_range(x), list(
                sf_boxes(list(x), "@niebo/0.4", at = 0),
                if (length(box$out)) sf_points(rep(0, length(box$out)), box$out, color = "@accent", size = 6)),
              "", ch1_eda_labels[[var]], aria = "Wykres pudełkowy wybranej zmiennej.",
              xLabels = sf_labels("", 0), noXGrid = TRUE)
    ))
  })
  reads <- lapply(stats::setNames(vars, vars), function(var) {
    x <- ca[[var]]
    list(eda_n = as.character(length(x)), eda_mean = lc_fmt(mean(x), 1), eda_sd = lc_fmt(sd(x), 1),
         eda_range = paste0(lc_fmt(min(x), 1), "–", lc_fmt(max(x), 1)))
  })
  lc_sf_spec(variants = variants, reads = reads,
    control = selectInput("ch1_eda_var", "Zmienna", selectize = FALSE,
      choices = c(
        "Średni wynik testu (pkt)" = "score",
        "Uczniowie na nauczyciela (STR)" = "str",
        "Wydatki na ucznia (USD)" = "expenditure",
        "Dochód okręgu (tys. USD)" = "income",
        "Uczniowie uczący się angielskiego (%)" = "english",
        "Uczniowie z dotacją do obiadu (%)" = "lunch",
        "Uczniowie z rodzin na zasiłku CalWORKs (%)" = "calworks"
      ),
      selected = "score"),
    toolbar = lc_readouts(
      lc_readout("n", lc_sf_value("eda_n"), color = case_explore),
      lc_readout("Średnia", lc_sf_value("eda_mean"), color = case_reference),
      lc_readout("SD", lc_sf_value("eda_sd"), color = case_reference),
      lc_readout("Zakres", lc_sf_value("eda_range"), color = case_reference)
    ),
    # Histogram dwa razy szerszy od pudełka, jak arrangeGrob(widths = c(2, 1)).
    body = tags$div(style = "display:grid;grid-template-columns:minmax(0,2fr) minmax(0,1fr);gap:1em",
      lc_sf_chart(0, "1.6/1", "280px"), lc_sf_chart(1, "0.8/1", "280px")))
}

# Ryc. 1.3: mapa korelacji; barwy komórek jak scale_fill_gradient2().
ch1_corr_fig <- function() {
  vars <- c("score", "str", "expenditure", "income", "english", "lunch", "calworks")
  labels_pl <- c(score = "Wynik", str = "STR", expenditure = "Wydatki", income = "Dochód",
                 english = "Angielski (%)", lunch = "Dotacje (%)", calworks = "CalWORKs (%)")
  cm <- cor(ca[, vars], use = "complete.obs")
  dimnames(cm) <- list(labels_pl[vars], labels_pl[vars])
  ord <- sort(unname(labels_pl[vars]))            # oś kategorii: kolejność alfabetyczna
  ramp <- scales::div_gradient_pal(case_highlight, "white", case_explore, space = "Lab")
  cells <- list(); texts <- list()
  for (i in seq_along(ord)) for (j in seq_along(ord)) {
    v <- cm[ord[i], ord[j]]
    cells[[length(cells) + 1]] <- list(r = c(i - 0.5, i + 0.5, j - 0.5, j + 0.5), color = ramp((v + 1) / 2))
    texts[[length(texts) + 1]] <- sf_text(i, j, as.character(round(v, 2)), color = "#1f2a2c", valign = "middle", size = 0.9)
  }
  n <- length(ord)
  lc_sf_spec(sf_spec(c(0.5, n + 0.5), c(0.5, n + 0.5), c(list(list(type = "cells", items = cells)), texts),
      aria = "Mapa korelacji między siedmioma zmiennymi opisującymi okręgi szkolne.",
      xLabels = sf_labels(ord), yLabels = sf_labels(ord), xRotate = 45, noXGrid = TRUE, noYGrid = TRUE,
      legend = list(title = "r", from = "-1", to = "1", stops = ramp(c(0, 0.5, 1)))),
    ratio = "1.25/1", max_height = "400px", legend = TRUE)
}

# Ryc. 1.4: wynik a STR; pole wyboru koloruje punkty poziomem ubóstwa.
ch1_str_fig <- function() {
  cols <- c("@niebo", "@bursztyn", "@accent")
  smooth <- sf_smooth_lm(ca$str, ca$score, color = "@szalwia", fill_alpha = 0.1)
  base <- function(points, legend = NULL) sf_spec(sf_range(ca$str), sf_range(ca$score), c(points, smooth),
    "Uczniowie na nauczyciela (STR)", "Średni wynik testu (pkt)",
    aria = "Wykres rozrzutu wyniku testu względem liczby uczniów na nauczyciela z prostą regresji.", legend = legend)
  lv <- levels(ca$poverty)
  lc_sf_spec(variants = list(
      off = base(list(sf_points(ca$str, ca$score, color = "@secondary", size = 7, alpha = 0.4))),
      on = base(lapply(seq_along(lv), function(i) sf_points(ca$str[ca$poverty == lv[i]], ca$score[ca$poverty == lv[i]],
                                                             color = cols[i], size = 7, alpha = 0.6)),
                legend = sf_legend(paste("Poziom ubóstwa:", lv), cols, box = TRUE))
    ),
    control = checkboxInput("ch1_str_color", "Koloruj wg poziomu ubóstwa", value = FALSE),
    ratio = "1.8/1", max_height = "380px", legend = TRUE)
}

# Ryc. 1.5: dotacje do obiadów względem wyniku i względem STR.
ch1_conf_fig <- function(y_var, y_label, color) {
  lc_sf_spec(sf_spec(sf_range(ca$lunch), sf_range(ca[[y_var]]),
      c(list(sf_points(ca$lunch, ca[[y_var]], color = "@secondary", size = 5, alpha = 0.3)),
        sf_smooth_lm(ca$lunch, ca[[y_var]], color = color, width = 2.8, se = FALSE)),
      "Uczniowie z dotacją do obiadu (%)", y_label, aria = paste("Wykres rozrzutu:", y_label, "względem odsetka uczniów z dotacją do obiadu.")),
    ratio = "1.4/1", max_height = "280px")
}

# Ryc. 1.6: wyniki w trzech grupach ubóstwa.
ch1_anova_fig <- function() {
  groups <- split(ca$score, ca$poverty)
  fills <- c("@niebo/0.6", "@bursztyn/0.6", "@accent/0.6")
  out <- do.call(rbind, lapply(seq_along(groups), function(i) {
    o <- sf_box_stats(groups[[i]])$out
    if (length(o)) data.frame(x = i, y = o)
  }))
  x <- as.integer(ca$poverty) + s09_jitter(nrow(ca), 0.15, 916)
  lc_sf_spec(sf_spec(c(0.4, 3.6), sf_range(ca$score), list(
      sf_boxes(groups, fills),
      if (!is.null(out)) sf_points(out$x, out$y, color = "@accent", size = 6, alpha = 0.2),
      sf_points(x, ca$score, color = "@accent", size = 4, alpha = 0.1)),
      "Poziom ubóstwa (odsetek uczniów z dotacją do obiadu)", "Średni wynik testu (pkt)",
      aria = "Wykresy pudełkowe wyniku testu w trzech grupach poziomu ubóstwa.", xLabels = sf_labels(names(groups))),
    ratio = "1.9/1", max_height = "320px")
}

# Seria modeli (krok 4): liczona raz.
ch1_models_data <- function() {
  models <- list(lm(score ~ str, data = ca), lm(score ~ str + income, data = ca),
                 lm(score ~ str + income + english, data = ca), lm(score ~ str + income + english + lunch, data = ca))
  labels <- c("1: sam STR", "2: + dochód", "3: + dochód + angielski", "4: + dochód + angielski + dotacje")
  do.call(rbind, lapply(seq_along(models), function(i) {
    m <- models[[i]]; g <- broom::glance(m); coefs <- broom::tidy(m)
    data.frame(model = labels[i], r2 = g$r.squared, adj_r2 = g$adj.r.squared, aic = g$AIC,
               rmse = sqrt(mean(residuals(m)^2)), beta_str = coefs$estimate[coefs$term == "str"],
               p_str = coefs$p.value[coefs$term == "str"])
  }))
}

# Ryc. 1.8: współczynnik przy STR w kolejnych modelach.
ch1_beta_str_fig <- function() {
  df <- ch1_models_data()
  sig <- df$p_str < 0.05
  keys <- unique(sig)
  lc_sf_spec(sf_spec(c(0.4, nrow(df) + 0.6), c(min(df$beta_str) * 1.08, 0.2), list(
      sf_cols(df$beta_str, width = 0.6, colors = ifelse(sig, "@szalwia/0.8", "@ref/0.8"), stroke = NULL),
      sf_hline(0, c(0.4, nrow(df) + 0.6))),
      "", "β przy STR", aria = "Współczynnik przy STR w czterech kolejnych modelach.",
      xLabels = sf_labels(df$model), xRotate = 20,
      legend = sf_legend(ifelse(keys, "p < 0.05", "nieistotny"), ifelse(keys, "@szalwia", "@ref"), box = TRUE)),
    ratio = "2.4/1", max_height = "250px", legend = TRUE)
}

# Ryc. 1.9: model wieloraki dla każdego podzbioru predyktorów (2⁵ wariantów).
ch1_reg_fig <- function() {
  preds <- c("str", "income", "english", "lunch", "expenditure")
  table_labels <- c("(Intercept)" = "Wyraz wolny", str = "STR", income = "Dochód (tys. USD)",
                    english = "Angielski (%)", expenditure = "Wydatki (USD)", lunch = "Dotacje (%)")
  plot_labels <- c(str = "STR", income = "Dochód", english = "Angielski (%)", expenditure = "Wydatki", lunch = "Dotacje (%)")
  subsets <- lapply(0:31, function(mask) preds[bitwAnd(mask, 2^(0:4)) > 0])
  one <- function(chosen) {
    use <- if (length(chosen) == 0) "str" else chosen
    model <- lm(stats::reformulate(use, "score"), data = ca)
    coefs <- broom::tidy(model, conf.int = TRUE)
    g <- broom::glance(model)
    sig <- coefs$p.value < 0.05
    # Nieistotne predyktory są wyszarzone; gwiazdka oznacza p < 0.05.
    table <- lc_table(
      data.frame(term = unname(table_labels[coefs$term]), estimate = coefs$estimate, se = coefs$std.error,
                 p = paste0(lc_pval(coefs$p.value), ifelse(sig, " *", ""))),
      cols = list(lc_col("term", "Zmienna", "row"), lc_col("estimate", "β", digits = 3),
                  lc_col("se", "SE", digits = 3), lc_col("p", "p")),
      row_class = ifelse(!sig & coefs$term != "(Intercept)", "is-dim", NA_character_),
      label = "Współczynniki modelu")
    cf <- coefs[coefs$term != "(Intercept)", ]
    cf$label <- unname(plot_labels[cf$term])
    cf <- cf[order(cf$label), ]                    # oś kategorii: alfabetycznie od dołu
    cf$y <- seq_len(nrow(cf))
    cf$col <- ifelse(cf$p.value < 0.05, "@szalwia", "@accent")
    keys <- unique(cf$p.value < 0.05)
    list(
      spec = sf_spec(sf_range(c(cf$conf.low, cf$conf.high, 0)), c(0.4, nrow(cf) + 0.6), c(
          lapply(seq_len(nrow(cf)), function(i) sf_points(cf$estimate[i], cf$y[i], color = cf$col[i], size = 10)),
          list(sf_errorbars_h(cf$conf.low, cf$conf.high, cf$y, color = "@ink", colors = as.list(rep(cf$col, 3)), width = 1.8),
               sf_vline(0, color = "@secondary"))),
        "β", "", aria = "Współczynniki modelu wielorakiego z 95% przedziałami ufności.",
        yLabels = sf_labels(cf$label), noYGrid = TRUE,
        legend = sf_legend(ifelse(keys, "p < 0.05", "p ≥ 0.05"), ifelse(keys, "@szalwia", "@accent"), box = TRUE)),
      reads = list(reg_adj = lc_fmt(g$adj.r.squared, 3), reg_aic = lc_fmt(g$AIC, 0),
                   reg_rmse = lc_fmt(sqrt(mean(residuals(model)^2)), 1)),
      html = list(coefs = s09_json(table))
    )
  }
  res <- lapply(subsets, one)
  keys <- vapply(subsets, paste, "", collapse = "+")
  keys[keys == ""] <- "none"                        # nic nie zaznaczono: model z samym STR
  named <- function(field) stats::setNames(lapply(res, `[[`, field), keys)
  first <- match("str+income+english+lunch", keys)   # ustawienie domyślne idzie jako pierwszy wariant
  ord <- c(first, setdiff(seq_along(keys), first))
  lc_sf_spec(variants = named("spec")[ord], reads = named("reads"), html = named("html"),
    control = checkboxGroupInput("ch1_reg_vars", "Predyktory", inline = TRUE,
      choices = c(
        "Uczniowie na nauczyciela (STR)" = "str",
        "Dochód okręgu (tys. USD)" = "income",
        "Uczniowie uczący się angielskiego (%)" = "english",
        "Uczniowie z dotacją do obiadu (%)" = "lunch",
        "Wydatki na ucznia (USD)" = "expenditure"
      ),
      selected = c("str", "income", "english", "lunch")),
    toolbar = lc_readouts(
      lc_readout("R² skor.", lc_sf_value("reg_adj"), color = case_model),
      lc_readout("AIC", lc_sf_value("reg_aic"), color = case_conclude),
      lc_readout("RMSE", lc_sf_value("reg_rmse"), color = case_highlight)
    ),
    body = tagList(tags$div(`data-role` = "coefs"), lc_sf_chart(0, "3/1", "230px")),
    legend = TRUE)
}

# ---------------------------------------------------------------------------
# Studium 2: pingwiny
# ---------------------------------------------------------------------------

pg_keys <- c(Adelie = "@niebo", Chinstrap = "@bursztyn", Gentoo = "@szalwia")
fmt0 <- function(v) formatC(v, format = "f", digits = 0)

# Pudełka (bez wartości odstających) z punktami w kolorach gatunków.
ch2_box_spec <- function(d, ylim = NULL, seed = 92) {
  groups <- split(d$body_mass_g, d$species)
  jit <- s09_jitter(nrow(d), 0.15, seed)
  layers <- c(
    lapply(seq_along(groups), function(i) sf_boxes(groups[i], paste0(pg_keys[[i]], "/0.25"), width = 0.55, stroke = pg_keys[[i]], at = i)),
    lapply(seq_along(groups), function(i) {
      sel <- as.integer(d$species) == i
      sf_points(i + jit[sel], d$body_mass_g[sel], color = pg_keys[[i]], size = 6, alpha = 0.45)
    }))
  sf_spec(c(0.4, 3.6), if (is.null(ylim)) sf_range(d$body_mass_g) else ylim, layers, "", "Masa ciała (g)",
          aria = "Wykres pudełkowy masy ciała trzech gatunków pingwinów.", xLabels = sf_labels(names(groups)))
}
ch2_desc_fig <- function() lc_sf_spec(ch2_box_spec(pg), ratio = "1.6/1", max_height = "340px")

# Ryc. 2.3: wykresy Q-Q masy w trzech gatunkach (osobne osie pionowe).
ch2_qq_fig <- function() {
  sp <- levels(pg$species)
  charts <- lapply(seq_along(sp), function(i) {
    v <- pg$body_mass_g[pg$species == sp[i]]
    q <- stats::qnorm(stats::ppoints(length(v)))
    sf_spec(sf_range(q), sf_range(v), sf_qq(v, color = pg_keys[[i]], line_color = "@secondary", line_dash = sf_dash, size = 5.5),
            "Kwantyle teoretyczne", if (i == 1) "Masa ciała (g)" else "",
            aria = paste("Wykres kwantyl-kwantyl masy ciała, gatunek", sp[i]))
  })
  lc_sf_spec(list(charts = charts),
    body = tags$div(style = "display:grid;grid-template-columns:repeat(auto-fit,minmax(11em,1fr));gap:.8em",
      lapply(seq_along(sp), function(i) tags$div(tags$h4(sp[i]), lc_sf_chart(i - 1, "1/1", "300px")))))
}

ch2_gh_data <- function(d = pg) {
  gh <- as.data.frame(rstatix::games_howell_test(d, body_mass_g ~ species))
  gh$para <- paste(gh$group2, "−", gh$group1)
  gh
}

# Ryc. 2.4: różnice średnich między parami gatunków (Games-Howell).
ch2_gh_fig <- function() {
  d <- ch2_gh_data()
  n <- nrow(d)
  y <- rev(seq_len(n))                              # pierwsza para u góry
  col <- ifelse(d$p.adj < 0.05, "@szalwia", "@accent")
  lc_sf_spec(sf_spec(sf_range(c(d$conf.low, d$conf.high, 0)), c(0.4, n + 0.6), c(
      list(sf_vline(0, color = "@secondary"),
           sf_errorbars_h(d$conf.low, d$conf.high, y, colors = as.list(rep(col, 3)), width = 2.2)),
      lapply(seq_len(n), function(i) sf_points(d$estimate[i], y[i], color = col[i], size = 10))),
      "Różnica średnich masy (g) z 95% przedziałem ufności", "",
      aria = "Różnice średnich masy między parami gatunków z przedziałami ufności.",
      yLabels = sf_labels(d$para, y), noYGrid = TRUE),
    ratio = "2.2/1", max_height = "260px")
}

# Ryc. 2.5: masa gatunków osobno dla samic i samców.
ch2_sex_fig <- function() {
  groups <- levels(pg_sex$plec)
  parts <- lapply(stats::setNames(groups, groups), function(g) {
    d <- pg_sex[pg_sex$plec == g, ]
    gh <- ch2_gh_data(d)
    m <- tapply(d$body_mass_g, d$species, mean)
    ac <- gh[gh$group1 == "Adelie" & gh$group2 == "Chinstrap", ]
    list(spec = ch2_box_spec(d, ylim = c(2600, 6400), seed = 93),
         reads = c(stats::setNames(as.list(paste0(fmt0(m), " g")), paste0("sex_", names(m))),
                   list(sex_ac = paste0(fmt0(ac$estimate), " g, p = ", s09_pval_text(ac$p.adj)))))
  })
  lc_sf_spec(variants = lapply(parts, `[[`, "spec"), reads = lapply(parts, `[[`, "reads"),
    control = lc_segmented("ch2_sex_group", "Grupa", choices = c("Samice" = "Samice", "Samce" = "Samce")),
    toolbar = lc_readouts(
      lapply(levels(pg$species), function(s) lc_readout(s, lc_sf_value(paste0("sex_", s)), color = pg_cols[[s]], swatch = TRUE)),
      lc_readout("Chinstrap − Adelie", lc_sf_value("sex_ac"))
    ),
    ratio = "1.6/1", max_height = "340px")
}

# ---------------------------------------------------------------------------
# Studium 3: Titanic
# ---------------------------------------------------------------------------

pct1 <- function(v) paste0(formatC(v * 100, format = "f", digits = 1), "%")

ch3_rates <- function(d, by) {
  agg <- aggregate(przezyl ~ g, data = data.frame(g = d[[by]], przezyl = d$przezyl),
                   FUN = function(x) c(p = mean(x), n = length(x)))
  data.frame(g = agg$g, p = agg$przezyl[, "p"], n = agg$przezyl[, "n"])
}

# Słupki odsetka ocalałych (oś w procentach) z wartością nad słupkiem i liczebnością pod osią.
ch3_bar_spec <- function(r, fills) {
  at <- seq_len(nrow(r))
  sf_spec(c(0.4, nrow(r) + 0.6), c(-12, 108), c(
      list(sf_cols(100 * r$p, width = 0.6, colors = paste0(unname(fills[as.character(r$g)]), "/0.85"), stroke = NULL)),
      sf_texts(at, 100 * r$p, pct1(r$p)),
      sf_texts(at, rep(-11, nrow(r)), paste0("n = ", r$n), color = "@secondary", size = 0.85)),
    "", "Odsetek ocalałych", aria = "Odsetek ocalałych w grupach.",
    xLabels = sf_labels(r$g), yLabels = sf_labels(paste0(seq(0, 100, 25), "%"), seq(0, 100, 25)))
}
tt_keys <- c(`1. klasa` = "@niebo", `2. klasa` = "@wrzos", `3. klasa` = "@bursztyn", `Załoga` = "@ref")

# Ryc. 3.2: odsetek ocalałych według klasy, płci albo wieku.
ch3_desc_fig <- function() {
  fills <- list(klasa = tt_keys, plec = c(`Mężczyźni` = "@niebo", `Kobiety` = "@bursztyn"),
                wiek = c(`Dorośli` = "@niebo", `Dzieci` = "@bursztyn"))
  lc_sf_spec(variants = lapply(stats::setNames(names(fills), names(fills)), function(by) ch3_bar_spec(ch3_rates(tt, by), fills[[by]])),
    control = lc_segmented("ch3_desc_by", "Podział", choices = c("Klasa" = "klasa", "Płeć" = "plec", "Wiek" = "wiek")),
    toolbar = lc_readouts(lc_readout("Wszyscy", pct1(mean(tt$przezyl)))),
    ratio = "1.8/1", max_height = "320px")
}

# Ryc. 3.5: odsetek ocalałych w klasach, osobno dla płci.
ch3_sex_fig <- function() {
  groups <- levels(tt$plec)
  lc_sf_spec(
    variants = lapply(stats::setNames(groups, groups), function(g) ch3_bar_spec(ch3_rates(tt[tt$plec == g, ], "klasa"), tt_keys)),
    reads = lapply(stats::setNames(groups, groups), function(g) list(sex_label = paste0(g, ": wszyscy"),
                                                                     sex_all = pct1(mean(tt$przezyl[tt$plec == g])))),
    control = lc_segmented("ch3_sex_group", "Grupa", choices = c("Mężczyźni" = "Mężczyźni", "Kobiety" = "Kobiety")),
    toolbar = lc_readouts(lc_readout(lc_sf_value("sex_label"), lc_sf_value("sex_all"))),
    ratio = "1.8/1", max_height = "320px")
}

# Ryc. 3.6: ilorazy szans z regresji logistycznej (oś logarytmiczna: pozycja = 100 · log10).
ch3_or_fig <- function() {
  tt_m <- tt
  tt_m$klasa <- relevel(tt_m$klasa, "1. klasa")
  models <- list(m0 = glm(przezyl ~ klasa, data = tt_m, family = binomial),
                 m1 = glm(przezyl ~ klasa + plec + wiek, data = tt_m, family = binomial))
  terms <- c("klasa2. klasa" = "2. klasa", "klasa3. klasa" = "3. klasa", "klasaZałoga" = "Załoga",
             "plecKobiety" = "Kobieta", "wiekDzieci" = "Dziecko")
  lx <- function(v) 100 * log10(v)
  breaks <- c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20)
  f2 <- function(v) formatC(v, format = "f", digits = 2)
  one <- function(m) {
    co <- summary(m)$coefficients; ci <- confint.default(m)
    keep <- rownames(co) != "(Intercept)"
    d <- data.frame(term = unname(terms[rownames(co)[keep]]), or = exp(co[keep, 1]), lo = exp(ci[keep, 1]),
                    hi = exp(ci[keep, 2]), p = co[keep, 4])
    y <- length(terms) + 1 - match(d$term, unname(terms))     # wszystkie wyrazy na osi, pierwszy u góry
    list(
      spec = sf_spec(lx(c(0.08, 25)), c(0.4, length(terms) + 0.6), list(
          sf_vline(lx(1), color = "@secondary"),
          sf_errorbars_h(lx(d$lo), lx(d$hi), y, color = "@wrzos", width = 2.2),
          sf_points(lx(d$or), y, color = "@wrzos", size = 10)),
        "Iloraz szans (skala logarytmiczna), 95% przedział ufności", "",
        aria = "Ilorazy szans przeżycia z przedziałami ufności.",
        xLabels = sf_labels(as.character(breaks), round(lx(breaks))),
        yLabels = sf_labels(rev(unname(terms))), noYGrid = TRUE),
      html = list(or_table = s09_json(lc_table(
        data.frame(term = d$term, or = f2(d$or), ci = paste0(f2(d$lo), " do ", f2(d$hi)), p = lc_pval(d$p)),
        cols = list(lc_col("term", "Względem: dorosły mężczyzna, 1. klasa", "row"), lc_col("or", "Iloraz szans", "text"),
                    lc_col("ci", "95% przedział", "text"), lc_col("p", "p")))))
    )
  }
  res <- lapply(models, one)
  lc_sf_spec(variants = lapply(res, `[[`, "spec")[c("m1", "m0")], html = lapply(res, `[[`, "html"),
    control = lc_segmented("ch3_model", "Model", choices = c("Sama klasa" = "m0", "Klasa, płeć i wiek" = "m1"), selected = "m1"),
    body = tagList(lc_sf_chart(0, "2/1", "300px"), tags$div(`data-role` = "or_table")))
}
