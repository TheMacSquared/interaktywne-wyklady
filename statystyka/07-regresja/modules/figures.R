# ============================================================================
# Ryciny wykładu 07: opisy liczone w R przy budowaniu strony
# ============================================================================
# Ryciny na stałych danych i o skończonej liczbie ustawień opisuje R i rysuje
# wbudowana rycina lc_spec (lc_sf_spec() w shared/lc_stat_figures.R). To, co zależy
# od losowania, suwaków i kliknięć, liczy modules/figures.js.
# Wymaga: helpers.R, lc_stat_figures.R.

# Rycina bez wykresu (same odczyty albo tekst): kontener lcSF z atrybutem data-lc-nochart.
s07_reads_figure <- function(fig, ..., config = NULL) {
  tags$div(class = "lc-sf", `data-fig` = fig, `data-lc-nochart` = NA,
    `data-config` = if (!is.null(config)) as.character(jsonlite::toJSON(config, auto_unbox = TRUE, digits = NA)),
    ...)
}
s07_html <- function(...) as.character(tagList(...))

# ---------------------------------------------------------------------------
# Rozdział 1
# ---------------------------------------------------------------------------

# Ryc. 1.1: anatomia prostej — wyraz wolny przy X = 0, trójkąt nachylenia o podstawie 1
# i pionowy odcinek błędu jednego punktu.
ch1_anatomy_fig <- function() {
  b0 <- 1.5; b1 <- 0.8
  pts <- data.frame(x = c(0.7, 1.4, 2.2, 2.9, 3.6, 4.4, 5.1, 5.7), e = c(0.5, -0.6, 0.3, 1.3, -0.4, -0.7, 0.6, 0.2))
  pts$y <- b0 + b1 * pts$x + pts$e
  hi <- pts[4, ]
  tx <- 4.5  # początek trójkąta nachylenia
  lc_sf_spec(sf_spec(c(-0.3, 6.2), c(0, 7.2), list(
      sf_vline(0, color = "@ref", width = 0.8, dash = NULL),
      sf_line(c(-0.3, 6.2), b0 + b1 * c(-0.3, 6.2), color = "@secondary", width = 2),
      sf_points(pts$x[-4], pts$y[-4], color = "@niebo", size = 9, alpha = 0.8),
      # ε: pionowy odcinek od prostej do punktu
      sf_vline(hi$x, color = "@bursztyn", width = 2.2, dash = NULL, from = b0 + b1 * hi$x, to = hi$y),
      sf_points(hi$x, hi$y, color = "@bursztyn", size = 11),
      sf_text(hi$x - 0.12, b0 + b1 * hi$x + hi$e / 2, "ε", color = "@bursztyn", align = "right", valign = "middle", size = 1.6),
      # β₁: krok o 1 w poziomie i β₁ w pionie
      sf_line(c(tx, tx + 1), rep(b0 + b1 * tx, 2), color = "@indygo", width = 1.8),
      sf_vline(tx + 1, color = "@indygo", width = 2.2, dash = NULL, from = b0 + b1 * tx, to = b0 + b1 * (tx + 1)),
      sf_text(tx + 0.5, b0 + b1 * tx - 0.3, "1", color = "@indygo", mono = TRUE, valign = "middle", size = 1.3),
      sf_text(tx + 1.12, b0 + b1 * (tx + 0.5), "β₁", color = "@indygo", align = "left", valign = "middle", size = 1.6),
      # β₀: wysokość prostej przy X = 0
      sf_vline(0, color = "@terakota", width = 2.6, dash = NULL, from = 0, to = b0),
      sf_points(0, b0, color = "@terakota", size = 12),
      sf_text(0.15, b0 / 2, "β₀", color = "@terakota", align = "left", valign = "middle", size = 1.6)),
    "X", "Y", aria = "Prosta regresji z zaznaczonym wyrazem wolnym przy X równym zero, trójkątem nachylenia o podstawie 1 i pionowym odcinkiem błędu jednego punktu.",
    xTicks = list(0), hideY = TRUE, noYGrid = TRUE, noXGrid = TRUE),
    ratio = "1.6/1", max_height = "340px")
}

# Ryc. 1.2: te same losowania co dawniej na serwerze (set.seed(101); rnorm(80) razy σ).
ch1_beta_config <- local({ set.seed(101); list(z = round(rnorm(80), 5)) })

# Ryc. 1.4: regresja do średniej na stałych danych (regression_to_mean_data()):
# trzy warianty grupy, odczyty średnich grupy na obu kolokwiach.
ch1_rtm_fig <- function() {
  d <- regression_to_mean_data()
  model <- lm(k2 ~ k1, d)
  o <- order(d$k1)
  groups <- list(top = tail(o, 10), bottom = head(o, 10), all = o)
  fmt <- function(v) formatC(v, format = "f", digits = 1)
  line_x <- c(30, 90)
  line_y <- unname(predict(model, data.frame(k1 = line_x)))
  variants <- lapply(groups, function(idx) {
    grp <- seq_len(nrow(d)) %in% idx
    m1 <- mean(d$k1[idx]); m2 <- mean(d$k2[idx])
    sf_spec(c(30, 90), c(30, 90), list(
        sf_line(c(30, 90), c(30, 90), color = "@ref", width = 1.4, dash = sf_dash),
        sf_line(line_x, line_y, color = "@niebo", width = 2.4),
        if (any(!grp)) sf_points(d$k1[!grp], d$k2[!grp], color = "@secondary", size = 7, alpha = 0.35),
        sf_points(d$k1[grp], d$k2[grp], color = "@accent", size = 8, alpha = 0.85),
        if (any(!grp)) list(type = "arrow", from = c(m1, m1), to = c(m1, m2), color = "@accent", width = 2),
        if (any(!grp)) sf_points(m1, m2, color = "@accent", size = 13)),
      "1. kolokwium (pkt)", "2. kolokwium (pkt)",
      aria = "Wyniki 100 studentów z dwóch kolokwiów z przekątną i prostą regresji.")
  })
  reads <- lapply(groups, function(idx) {
    list(rtm_k1 = fmt(mean(d$k1[idx])), rtm_k2 = fmt(mean(d$k2[idx])), rtm_all = fmt(mean(d$k1)))
  })
  lc_sf_spec(variants = variants, reads = reads,
    control = lc_segmented("ch1_rtm_group", "Grupa",
      choices = c("10 najlepszych na 1. kolokwium" = "top",
                  "10 najsłabszych na 1. kolokwium" = "bottom",
                  "Wszyscy" = "all")),
    toolbar = lc_readouts(
      lc_readout("Grupa: 1. kolokwium", lc_sf_value("rtm_k1")),
      lc_readout("Grupa: 2. kolokwium", lc_sf_value("rtm_k2"), color = upwr_accent),
      lc_readout("Średnia wszystkich", lc_sf_value("rtm_all"))),
    ratio = "1.25/1", max_height = "420px")
}

# Wzór złożony w HTML (fragmenty z data-config nie przechodzą przez MathJax przy budowaniu).
s07_hbox <- function(...) tags$div(class = "lc-formula-box", tags$span(class = "lc-hmath", HTML(paste0(...))))

# Ryc. 1.6: cztery scenariusze na stałych ziarnach (jak dawniej na serwerze).
ch1_pval_scenarios <- list(
  strong_positive = list(seed = 3101, n = 70, beta0 = 6, beta1 = 1.25, sigma = 3.0),
  none            = list(seed = 3102, n = 70, beta0 = 6, beta1 = 0.00, sigma = 4.2),
  strong_negative = list(seed = 3103, n = 70, beta0 = 8, beta1 = -1.15, sigma = 3.0),
  small_sample    = list(seed = 3104, n = 14, beta0 = 6, beta1 = 1.25, sigma = 5.0)
)
ch1_pval_fig <- function() {
  parts <- lapply(ch1_pval_scenarios, function(s) {
    set.seed(s$seed)
    x <- runif(s$n, -4, 4)
    y <- s$beta0 + s$beta1 * x + rnorm(s$n, 0, s$sigma)
    model <- lm(y ~ x)
    co <- broom::tidy(model)
    p_val <- co$p.value[2]; b1 <- co$estimate[2]; sig <- p_val < 0.05
    col <- if (sig) "@niebo" else "@ref"
    band <- predict(model, data.frame(x = range(x)), interval = "confidence")
    spec <- sf_spec(sf_range(x), sf_range(c(y, band)), c(
        list(sf_points(x, y, color = "@secondary", alpha = 0.55, size = 7),
             sf_hline(mean(y), sf_range(x), color = "@bursztyn", width = 1.8)),
        sf_smooth_lm(x, y, color = col, fill_alpha = 0.16, width = 2.8),
        list(sf_text(min(x), max(y), if (sig) "p < 0.05: nachylenie istotne" else "p ≥ 0.05: brak istotności",
                     color = col, align = "left", valign = "top", halo = TRUE))),
      "X", "Y", aria = "Wykres rozrzutu z prostą regresji, pasem ufności i średnią Y.")
    co$term <- ifelse(co$term == "(Intercept)", "wyraz wolny", "X")
    co$p_txt <- lc_pval(co$p.value)
    html <- list(
      table = s07_html(lc_table(as.data.frame(co), cols = list(
        lc_col("term", "Zmienna", "row"), lc_col("estimate", "Estymata", digits = 2),
        lc_col("statistic", "t", digits = 2), lc_col("p_txt", "p")))),
      verdict = s07_html(if (sig) {
        lc_status(lc_verdict(tags$strong("Wniosek: "), type = "ok"),
          sprintf("odrzucamy H₀. Nachylenie b₁ = %.2f jest istotnie różne od zera.", b1))
      } else {
        lc_status(lc_verdict(tags$strong("Wniosek: "), type = "warning"),
          sprintf("nie odrzucamy H₀. Dane nie dają mocnych podstaw, by uznać nachylenie b₁ = %.2f za różne od zera.", b1))
      }),
      stats = s07_html(
        lc_readout("b₁", round(b1, 2), color = unname(upwr_cat["szalwia"])),
        lc_readout("SE(b₁)", round(co$std.error[2], 2), color = upwr_secondary),
        lc_readout("t", round(co$statistic[2], 2), color = unname(upwr_cat["bursztyn"])),
        lc_readout("p-wartość", if (p_val < 0.001) "< 0.001" else round(p_val, 3),
                   color = if (sig) unname(upwr_cat["niebo"]) else upwr_reference))
    )
    list(spec = spec, html = html)
  })
  lc_sf_spec(variants = lapply(parts, `[[`, "spec"), html = lapply(parts, `[[`, "html"),
    control = selectInput("ch1_pval_scenario", "Scenariusz", selectize = FALSE,
      choices = c("Wyraźny dodatni wpływ" = "strong_positive", "Brak wpływu" = "none",
                  "Wyraźny ujemny wpływ" = "strong_negative",
                  "Ten sam trend, mała próba i większy szum" = "small_sample"),
      selected = "strong_positive"),
    toolbar = lc_readouts(tags$div(`data-role` = "stats", style = "display: contents")),
    ratio = "2/1", max_height = "360px",
    after = tagList(tags$div(`data-role` = "table"), tags$div(`data-role` = "verdict")))
}

# Ryc. 1.7: każda para zmiennych CASchools opisana przy budowaniu (wykres, tabela,
# odpowiedź). Odpowiedź odsłania przycisk; zmiana pary ją chowa (rycina s07_reveal).
ch1_cas_x_choices <- c(
  "Dochód okręgu (income)" = "income",
  "Uczniowie na nauczyciela (STR)" = "student_teacher_ratio",
  "Wydatki na ucznia (expenditure)" = "expenditure",
  "Udział uczniów z angielskim jako drugim językiem (english)" = "english",
  "Uczniowie z dotacją do obiadu (lunch)" = "lunch",
  "Komputery" = "computer",
  "Zakres klas (grades)" = "grades")
ch1_cas_y_choices <- c(
  "Czytanie (read)" = "read",
  "Matematyka (math)" = "math",
  "Dochód okręgu (income)" = "income",
  "Wydatki na ucznia (expenditure)" = "expenditure",
  "Uczniowie na nauczyciela (STR)" = "student_teacher_ratio")

ch1_cas_variant <- function(xv, yv) {
  if (xv == yv) {
    return(list(
      spec = sf_spec(c(0, 1), c(0, 1), list(sf_text(0.5, 0.5, "Wybierz dwie różne zmienne.", color = "@ref", size = 1.2)),
                     aria = "Wybierz dwie różne zmienne.", hideY = TRUE, xTicks = list()),
      html = list(table = s07_html(lc_caption("Wybierz dwie różne zmienne.")), pre = "", summary = "", answer = "")))
  }
  d <- .cas_data
  x_label <- unname(.cas_labels[xv]); y_label <- unname(.cas_labels[yv])
  grades <- identical(xv, "grades")
  if (grades) {
    d$grades01 <- ifelse(d$grades == "KK-08", 1, 0)
    model <- lm(as.formula(paste(yv, "~ grades01")), data = d)
    pos <- ifelse(d$grades == "KK-08", 2, 1)
    jit <- withr::with_seed(707, runif(nrow(d), -0.12, 0.12))
    pred <- unname(predict(model, data.frame(grades01 = c(0, 1))))
    means <- tapply(d[[yv]], pos, mean)
    spec <- sf_spec(c(0.4, 2.6), sf_range(d[[yv]]), list(
        sf_points(pos + jit, d[[yv]], color = "@secondary", alpha = 0.42, size = 6),
        sf_points(c(1, 2), means, color = "@terakota", size = 12),
        sf_segs(c(1, 2) - 0.275, pred, c(1, 2) + 0.275, pred, color = "@niebo", width = 2.4)),
      x_label, y_label, xLabels = sf_labels(c("KK-06", "KK-08")),
      aria = paste0(y_label, " w okręgach KK-06 i KK-08 ze średnimi grup."))
  } else {
    model <- lm(as.formula(paste(yv, "~", xv)), data = d)
    band <- predict(model, data.frame(setNames(list(range(d[[xv]])), xv)), interval = "confidence")
    spec <- sf_spec(sf_range(d[[xv]]), sf_range(c(d[[yv]], band)), c(
        list(sf_points(d[[xv]], d[[yv]], color = "@secondary", alpha = 0.45, size = 6)),
        sf_smooth_lm(d[[xv]], d[[yv]], color = "@niebo", fill_alpha = 0.15)),
      x_label, y_label, aria = paste0("Wykres rozrzutu: ", y_label, " a ", x_label, ", z prostą regresji."))
  }
  co <- broom::tidy(model)
  p_val <- co$p.value[2]; b0 <- co$estimate[1]; b1 <- co$estimate[2]
  co$term <- ifelse(co$term == "(Intercept)", "wyraz wolny",
                    ifelse(co$term == "grades01", "Zakres klas: KK-08 vs KK-06", x_label))
  co$p_txt <- lc_pval(co$p.value)
  table <- lc_table(as.data.frame(co), cols = list(
    lc_col("term", "Zmienna", "row"), lc_col("estimate", "Estymata", digits = 3),
    lc_col("std.error", "SE", digits = 3), lc_col("statistic", "t", digits = 2), lc_col("p_txt", "p")))
  reads <- lc_readouts(
    lc_readout("b₀", round(b0, 2), color = upwr_secondary),
    lc_readout("b₁", round(b1, 3), color = unname(upwr_cat["szalwia"])),
    lc_readout("p dla b₁", signif(p_val, 3), color = unname(upwr_cat["bursztyn"])))
  sgn <- function(v) sprintf("%s %.2f", if (v < 0) "−" else "+", abs(v))
  summary <- if (grades) {
    tagList(reads,
      tags$div(class = "lc-formula-box",
        tags$span(class = "lc-hmath", HTML(sprintf("<i>Ŷ</i> = %.2f %s · <i>X</i><sub>KK-08</sub>", b0, sgn(b1)))),
        p(tags$strong("Kodowanie:"), " KK-06 = 0, KK-08 = 1."),
        tags$p(tags$span(class = "lc-hmath", HTML(sprintf("KK-06: <i>Ŷ</i> = %.2f %s · 0 = %.2f", b0, sgn(b1), b0)))),
        tags$p(tags$span(class = "lc-hmath", HTML(sprintf("KK-08: <i>Ŷ</i> = %.2f %s · 1 = %.2f", b0, sgn(b1), b0 + b1))))),
      lc_status(p(tags$strong("Interpretacja: "),
        paste0("w tym kodowaniu b₀ to średni przewidywany ", y_label, " dla KK-06, a b₁ to różnica KK-08 minus KK-06."))))
  } else {
    tagList(reads, lc_status(p(tags$strong("Interpretacja: "),
      paste0("gdy ", x_label, " rośnie o 1, przewidywane ", y_label, " zmienia się średnio o ", round(b1, 3), "."))))
  }
  relation <- if (b1 > 0) "dodatni" else "ujemny"
  answer <- if (p_val < 0.05) {
    lc_status(lc_verdict(tags$strong("Odpowiedź: "), type = "ok"), if (grades) {
      sprintf("tak, %s istotnie przewiduje %s. Okręgi KK-08 różnią się od KK-06 średnio o %.3f punktu, p = %.3g.", x_label, y_label, b1, p_val)
    } else {
      sprintf("tak, %s istotnie przewiduje %s. Efekt jest %s: b₁ = %.3f, p = %.3g.", x_label, y_label, relation, b1, p_val)
    })
  } else {
    lc_status(lc_verdict(tags$strong("Odpowiedź: "), type = "warning"), if (grades) {
      sprintf("nie mamy podstaw, by uznać różnicę między KK-08 i KK-06 w %s za istotną: b₁ = %.3f, p = %.3g.", y_label, b1, p_val)
    } else {
      sprintf("nie mamy podstaw, by uznać wpływ %s na %s za istotny: b₁ = %.3f, p = %.3g.", x_label, y_label, b1, p_val)
    })
  }
  list(spec = spec, html = list(table = s07_html(table),
    pre = s07_html(lc_caption("Przed odsłonięciem odpowiedzi odczytaj z tabeli znak b₁ i p-wartość.")),
    summary = s07_html(summary), answer = s07_html(answer)))
}

ch1_cas_fig <- function() {
  keys <- expand.grid(x = unname(ch1_cas_x_choices), y = unname(ch1_cas_y_choices), stringsAsFactors = FALSE)
  parts <- lapply(seq_len(nrow(keys)), function(i) ch1_cas_variant(keys$x[i], keys$y[i]))
  names(parts) <- paste0(keys$x, "|", keys$y)
  lc_sf_figure("s07_reveal", ratio = "2/1", max_height = "360px",
    config = list(variants = lapply(parts, `[[`, "spec"), html = lapply(parts, `[[`, "html"), key = list("ch1_cas_x", "ch1_cas_y")),
    control = list(
      selectInput("ch1_cas_x", "Zmienna X", choices = ch1_cas_x_choices, selected = "income", selectize = FALSE),
      selectInput("ch1_cas_y", "Zmienna Y", choices = ch1_cas_y_choices, selected = "read", selectize = FALSE)),
    toolbar = lc_sf_action("reveal", "ch1_cas_reveal", "Pokaż odpowiedź", variant = "solid"),
    after = tagList(
      tags$div(`data-role` = "table"),
      tags$div(`data-reveal` = "before", tags$div(`data-role` = "pre")),
      tags$div(`data-reveal` = "after", hidden = NA, tags$div(`data-role` = "summary"), tags$div(`data-role` = "answer"))))
}

# Ryc. 1.9: cztery gotowe modele; wartość X wpisuje student, predykcję liczy przeglądarka
# (rycina s07_pred) ze współczynników zaokrąglonych jak w tabeli.
.ch1_fmt_coef <- function(x) {
  if (abs(x) >= 1) sprintf("%.2f", x) else format(signif(x, 3), scientific = FALSE)
}
ch1_pred_fig <- function() {
  cases <- lapply(.ch1_pred_specs, function(s) {
    d <- .cas_data
    model <- lm(as.formula(paste(s$y, "~", s$x)), data = d)
    co <- coef(model)
    x_label <- unname(.cas_labels[s$x]); y_label <- unname(.cas_labels[s$y])
    band <- predict(model, data.frame(setNames(list(range(d[[s$x]])), s$x)), interval = "confidence")
    spec <- sf_spec(sf_range(d[[s$x]]), sf_range(c(d[[s$y]], band)), c(
        list(sf_points(d[[s$x]], d[[s$y]], color = "@secondary", alpha = 0.42, size = 6)),
        sf_smooth_lm(d[[s$x]], d[[s$y]], color = "@niebo", fill_alpha = 0.15)),
      x_label, y_label, aria = paste0("Wykres rozrzutu: ", y_label, " a ", x_label, ", z prostą regresji."))
    terms <- c("wyraz wolny", x_label)
    list(spec = spec, b0 = unname(co[1]), b1 = unname(co[2]),
         b0txt = .ch1_fmt_coef(co[1]), b1txt = .ch1_fmt_coef(co[2]), b1abs = .ch1_fmt_coef(abs(co[2])),
         xlab = x_label, ylab = y_label, unit = s$unit, question = s$question, default = s$default, step = s$step,
         min = floor(min(d[[s$x]], na.rm = TRUE)), max = ceiling(max(d[[s$x]], na.rm = TRUE)),
         table = s07_html(lc_table(data.frame(term = terms, estimate = c(.ch1_fmt_coef(co[1]), .ch1_fmt_coef(co[2]))),
           cols = list(lc_col("term", "Zmienna", "row"), lc_col("estimate", "Estymata")))))
  })
  first <- cases[[1]]
  lc_sf_figure("s07_pred", ratio = "2/1", max_height = "360px",
    config = list(variants = lapply(cases, `[[`, "spec"), cases = lapply(cases, function(cs) { cs$spec <- NULL; cs })),
    control = selectInput("ch1_pred_case", "Model", choices = .ch1_pred_choices, selected = "read_students", selectize = FALSE),
    toolbar = tagList(lc_sf_action("reveal", "ch1_pred_reveal", "Pokaż odpowiedź", variant = "solid"),
                      lc_readouts(tags$div(`data-role` = "stats", style = "display: contents"))),
    after = tagList(
      tags$div(`data-role` = "table"),
      lc_sf_ctl(numericInput("ch1_pred_x", label = paste0("Wartość X (", first$unit, "):"), value = first$default,
                             min = first$min, max = first$max, step = first$step)),
      tags$div(`data-role` = "question"),
      tags$div(`data-role` = "answer")))
}

# ---------------------------------------------------------------------------
# Rozdział 2
# ---------------------------------------------------------------------------

# Ryc. 2.1: dane z prostą, reszty względem przewidywanych (z linią loess jak
# geom_smooth) i wykres Q-Q reszt dla czterech modeli CASchools.
ch2_resid_fig <- function() {
  parts <- lapply(.ch2_resid_specs, function(s) {
    d <- .cas_data
    x <- d[[s$x]]; y <- d[[s$y]]
    model <- lm(y ~ x)
    fit <- fitted(model); res <- residuals(model)
    grid <- seq(min(fit), max(fit), length.out = 80)
    smooth <- predict(loess(res ~ fit, span = 0.75), data.frame(fit = grid))
    charts <- list(
      sf_spec(sf_range(x), sf_range(y), list(
          sf_points(x, y, color = "@secondary", alpha = 0.4, size = 5),
          sf_smooth_lm(x, y, color = "@niebo", se = FALSE)[[1]]),
        unname(.cas_labels[s$x]), unname(.cas_labels[s$y]), aria = "Dane z prostą regresji."),
      sf_spec(sf_range(fit), sf_range(c(res, smooth)), list(
          sf_points(fit, res, color = "@secondary", alpha = 0.4, size = 5),
          sf_hline(0, sf_range(fit), color = "@ref", width = 1.4),
          sf_line(grid, smooth, color = "@terakota", width = 2.4)),
        "Ŷ", "eᵢ = yᵢ − ŷᵢ", aria = "Reszty względem wartości przewidywanych z linią trendu reszt."),
      local({
        q <- stats::qnorm(stats::ppoints(length(res)))
        sf_spec(sf_range(q), sf_range(res), sf_qq(res, color = "@secondary", line_color = "@niebo", size = 5, alpha = 0.4),
          "Kwantyle teoretyczne", "Kwantyle próbki", aria = "Wykres kwantyl-kwantyl reszt.")
      }))
    g <- broom::glance(model)
    list(spec = list(charts = charts),
         html = list(
           stats = s07_html(
             lc_readout("R²", round(g$r.squared, 3), color = unname(upwr_cat["niebo"])),
             lc_readout("RMSE", round(sqrt(mean(res^2)), 2), color = unname(upwr_cat["bursztyn"])),
             lc_readout("n", nrow(d), color = upwr_secondary)),
           verdict = s07_html(lc_status(lc_verdict(tags$strong(s$title), type = s$verdict), p(s$comment)))))
  })
  lc_sf_spec(variants = lapply(parts, `[[`, "spec"), html = lapply(parts, `[[`, "html"), charts = 3, triple = TRUE,
    control = selectInput("ch2_resid_case", "Model", choices = .ch2_resid_choices, selected = "read_income", selectize = FALSE),
    toolbar = lc_readouts(tags$div(`data-role` = "stats", style = "display: contents")),
    ratio = "0.85/1", max_height = "300px",
    after = tags$div(`data-role` = "verdict"))
}

# Ryc. 2.2: ustawione przykłady obserwacji wpływowej.
ch2_infl_fig <- function() {
  base <- data.frame(x = c(1, 2, 3, 4, 5, 5.5, 6, 7, 8, 8.5, 9, 10, 11, 12),
                     e = c(3, -4, 2, 5, -3, 1, -5, 4, -2, 3, -4, 2, -1, -2))
  base$y <- 35 + 2.5 * base$x + base$e
  extra <- list(none = NULL, mid = c(6.5, 92), lev = c(24, 96), infl = c(24, 40))
  m0 <- lm(y ~ x, base)
  xs <- c(0, 26)
  fmt <- function(v) formatC(v, format = "f", digits = 2)
  parts <- lapply(extra, function(e) {
    m1 <- if (is.null(e)) NULL else lm(y ~ x, rbind(base[, c("x", "y")], data.frame(x = e[1], y = e[2])))
    spec <- sf_spec(xs, c(25, 105), list(
        sf_line(xs, predict(m0, data.frame(x = xs)), color = "@ref", width = 1.8, dash = sf_dash),
        sf_points(base$x, base$y, color = "@secondary", size = 9, alpha = 0.8),
        if (!is.null(m1)) sf_line(xs, predict(m1, data.frame(x = xs)), color = "@accent", width = 2.6),
        if (!is.null(e)) sf_points(e[1], e[2], color = "@surface", size = 15),
        if (!is.null(e)) sf_points(e[1], e[2], color = "@accent", size = 12)),
      "Godziny nauki w tygodniu", "Wynik egzaminu (pkt)",
      aria = "Wykres punktowy godzin nauki i wyniku z prostą bez dodatkowego punktu i z nim.")
    list(spec = spec, reads = list(infl_m0 = fmt(coef(m0)[[2]]), infl_m1 = if (is.null(m1)) "—" else fmt(coef(m1)[[2]])))
  })
  lc_sf_spec(variants = lapply(parts, `[[`, "spec"), reads = lapply(parts, `[[`, "reads"),
    control = lc_segmented("ch2_infl_case", "Dodatkowy student",
      choices = c("Brak" = "none", "Odstaje w środku" = "mid",
                  "Daleko, zgodnie z trendem" = "lev", "Daleko, wbrew trendowi" = "infl")),
    toolbar = lc_readouts(
      lc_readout("Nachylenie bez niego", lc_sf_value("infl_m0"), color = upwr_reference, swatch = TRUE),
      lc_readout("Nachylenie z nim", lc_sf_value("infl_m1"), color = upwr_accent, swatch = TRUE)),
    ratio = "1.6/1", max_height = "340px")
}

# Ryc. 2.3: ta sama prosta, trzy poziomy szumu (set.seed(103) jak dawniej).
ch2_r2_compare_fig <- function() {
  set.seed(103)
  sigmas <- c("Niskie R²" = 12.0, "Średnie R²" = 4.0, "Wysokie R²" = 0.9)
  x <- seq(-3, 3, length.out = 70)
  ys <- lapply(sigmas, function(s) 10 + 2.2 * x + rnorm(length(x), 0, s))
  yr <- sf_range(unlist(ys))
  charts <- lapply(ys, function(y) {
    r2 <- summary(lm(y ~ x))$r.squared
    sf_spec(sf_range(x), yr, list(
        sf_points(x, y, color = "@secondary", alpha = 0.5, size = 6),
        sf_smooth_lm(x, y, color = "@niebo", width = 2.2, se = FALSE)[[1]],
        sf_text(-2.8, "top", paste0("R² = ", round(r2, 2)), color = "@secondary", align = "left", bold = TRUE)),
      "X", "Y", aria = "Wykres rozrzutu z prostą regresji.")
  })
  lc_sf_spec(list(charts = unname(charts)), charts = 3, triple = TRUE, headings = names(sigmas),
    ratio = "0.85/1", max_height = "320px")
}

# Ryc. 2.4: kwartet Anscombe'a — zestaw z prostą i wykres jego reszt.
ch2_ansc_fig <- function() {
  sets <- c("1", "2", "3", "4")
  fmt <- function(v) formatC(v, format = "f", digits = 2)
  parts <- lapply(sets, function(i) {
    x <- anscombe[[paste0("x", i)]]; y <- anscombe[[paste0("y", i)]]
    m <- lm(y ~ x)
    xs <- c(2, 20)
    charts <- list(
      sf_spec(c(2, 20), c(2, 14), list(
          sf_line(xs, predict(m, data.frame(x = xs)), color = "@accent", width = 2.4),
          sf_points(x, y, color = "@secondary", size = 10)),
        "x", "y", aria = paste("Zestaw", i, "kwartetu Anscombe'a z prostą regresji.")),
      sf_spec(c(4, 13), c(-3.5, 3.5), list(
          sf_hline(0, c(4, 13), color = "@ref", width = 1.4),
          sf_points(fitted(m), resid(m), color = "@secondary", size = 10)),
        "Wartość przewidywana", "Reszta", aria = paste("Reszty zestawu", i, "względem wartości przewidywanych.")))
    list(spec = list(charts = charts),
         reads = list(ansc_b0 = fmt(coef(m)[[1]]), ansc_b1 = fmt(coef(m)[[2]]), ansc_r2 = fmt(summary(m)$r.squared)))
  })
  names(parts) <- sets
  lc_sf_spec(variants = lapply(parts, `[[`, "spec"), reads = lapply(parts, `[[`, "reads"), charts = 2, pair = TRUE,
    control = lc_segmented("ch2_ansc_set", "Zestaw", choices = c("1" = "1", "2" = "2", "3" = "3", "4" = "4")),
    toolbar = lc_readouts(
      lc_readout("b₀", lc_sf_value("ansc_b0")),
      lc_readout("b₁", lc_sf_value("ansc_b1")),
      lc_readout("R²", lc_sf_value("ansc_r2"))),
    ratio = "1.3/1", max_height = "340px")
}

# Ryc. 2.5: przeuczenie — wielomiany stopnia 1, 4 i 12 na tych samych danych (set.seed(26)).
ch2_overfit_fig <- function() {
  set.seed(26)
  f <- function(x) 4.8 * sin(x)
  train_x <- sort(runif(30, 0, 10))
  test_x <- sort(runif(180, 0, 10))
  train <- data.frame(x = train_x, y = f(train_x) + rnorm(length(train_x), 0, 0.9))
  test <- data.frame(x = test_x, y = f(test_x) + rnorm(length(test_x), 0, 0.9))
  degrees <- c(1, 4, 12)
  labels <- c("1" = "Zbyt prosty", "4" = "Rozsądnie elastyczny", "12" = "Przeuczony")
  x_grid <- seq(0, 10, length.out = 260)
  models <- lapply(degrees, function(k) lm(y ~ poly(x, k), data = train))
  charts <- lapply(models, function(model) {
    yg <- predict(model, newdata = data.frame(x = x_grid))
    sf_spec(c(-0.5, 10.5), c(-8, 8), list(
        sf_points(test$x, test$y, color = "@bursztyn", alpha = 0.22, size = 5),
        sf_points(train$x, train$y, color = "@secondary", alpha = 0.72, size = 7),
        sf_line(x_grid, yg, color = "@niebo", width = 2.2)),
      "X", "Y", aria = "Dane treningowe i testowe z dopasowanym wielomianem.")
  })
  metrics <- data.frame(
    model = unname(labels[as.character(degrees)]), degree = degrees,
    train_rmse = vapply(models, function(m) sqrt(mean((train$y - predict(m, train))^2)), 0),
    test_rmse = vapply(models, function(m) sqrt(mean((test$y - predict(m, test))^2)), 0))
  lc_sf_spec(list(charts = charts), charts = 3, triple = TRUE, headings = unname(labels),
    ratio = "0.8/1", max_height = "340px",
    after = tagList(
      lc_caption("Ciemne punkty = trening; jasne bursztynowe = nowe dane testowe"),
      lc_table(metrics, cols = list(
        lc_col("model", "Model", "row"), lc_col("degree", "Stopień"),
        lc_col("train_rmse", "RMSE trening", digits = 2), lc_col("test_rmse", "RMSE test", digits = 2)))))
}

# Ryc. 2.6: pasmo ±RMSE wokół prostej dla czterech modeli CASchools.
ch2_rmse_fig <- function() {
  parts <- lapply(.ch2_rmse_specs, function(s) {
    d <- .cas_data
    x <- d[[s$x]]; y <- d[[s$y]]
    model <- lm(y ~ x)
    rmse <- sqrt(mean(residuals(model)^2))
    ord <- order(x)
    fit <- fitted(model)[ord]
    g <- broom::glance(model)
    y_range <- diff(range(y))
    y_label <- unname(.cas_labels[s$y])
    spec <- sf_spec(sf_range(x), sf_range(c(y, fit - rmse, fit + rmse)), list(
        sf_points(x, y, color = "@secondary", alpha = 0.42, size = 6),
        sf_smooth_lm(x, y, color = "@niebo", se = FALSE)[[1]],
        sf_ribbon(range(x), predict(model, data.frame(x = range(x))) - rmse,
                  predict(model, data.frame(x = range(x))) + rmse, "@bursztyn/0.18"),
        sf_text(min(x), max(y), paste0("Pasmo ±RMSE = ±", round(rmse, 1)), color = "@bursztyn",
                align = "left", valign = "top", halo = TRUE)),
      unname(.cas_labels[s$x]), y_label, aria = "Wykres rozrzutu z prostą regresji i pasmem ±RMSE.")
    list(spec = spec, html = list(
      stats = s07_html(
        lc_readout("R²", round(g$r.squared, 3), color = unname(upwr_cat["niebo"])),
        lc_readout("RMSE", round(rmse, 2), color = unname(upwr_cat["bursztyn"])),
        lc_readout("Zakres Y (max − min)", round(y_range, 1), color = upwr_secondary),
        lc_readout("RMSE / zakres", paste0(round(rmse / y_range * 100, 1), "%"), color = unname(upwr_cat["terakota"]))),
      interp = s07_html(lc_caption(sprintf("Typowa pomyłka modelu to ±%.1f w skali „%s” (zakres %.0f, SD %.1f).",
                                           rmse, y_label, y_range, sd(y)), tone = "info"))))
  })
  lc_sf_spec(variants = lapply(parts, `[[`, "spec"), html = lapply(parts, `[[`, "html"),
    control = selectInput("ch2_rmse_case", "Model", choices = .ch2_rmse_choices, selected = "read_lunch", selectize = FALSE),
    toolbar = lc_readouts(tags$div(`data-role` = "stats", style = "display: contents")),
    ratio = "2/1", max_height = "320px",
    after = tags$div(`data-role` = "interp"))
}

# Ryc. 2.7: ekstrapolacja — punkty i model z R, prognozę dla suwaka liczy przeglądarka (s07_extrap).
ch2_extrap_config <- local({
  m <- lm(read ~ income, data = .cas_data)
  list(x = round(.cas_data$income, 3), y = round(.cas_data$read, 2),
       b0 = unname(coef(m)[1]), b1 = unname(coef(m)[2]), range = range(.cas_data$income))
})

# ---------------------------------------------------------------------------
# Rozdział 3
# ---------------------------------------------------------------------------

ch3_labels_pl <- c(
  "read" = "Wynik: czytanie", "math" = "Wynik: matematyka",
  "lunch" = "Dotacje do obiadów (%)", "income" = "Dochód okręgu (tys. USD)",
  "english" = "Angielski jako drugi język (%)", "student_teacher_ratio" = "Uczniowie / nauczyciel",
  "expenditure" = "Wydatki na ucznia", "computer" = "Komputery", "calworks" = "CalWORKs (%)")
ch3_extra_choices <- c(
  "Dotacje do obiadów (%)" = "lunch", "Angielski jako drugi język (%)" = "english",
  "Uczniowie / nauczyciel" = "student_teacher_ratio", "Wydatki na ucznia" = "expenditure",
  "Komputery" = "computer", "CalWORKs (%)" = "calworks")

# Grupy kwantylowe jak dawniej na serwerze (cut() przy kwantylach, mediana grupy).
ch3_bins <- function(x, labels) {
  probs <- seq(0, 1, length.out = length(labels) + 1)
  breaks <- unique(as.numeric(quantile(x, probs = probs, na.rm = TRUE)))
  g <- if (length(breaks) <= 2) {
    cut(x, breaks = 2, include.lowest = TRUE, labels = labels[seq_len(2)])
  } else {
    cut(x, breaks = breaks, include.lowest = TRUE, labels = labels[seq_len(length(breaks) - 1)])
  }
  list(g = as.integer(g) - 1L, levels = levels(g), med = unname(tapply(x, g, median, na.rm = TRUE)))
}

# Ryc. 3.1: tabela i miary każdego modelu (Y × zestaw predyktorów) liczy R przy budowaniu;
# wykresy (grupy, panele, linie) rysuje przeglądarka ze współczynników (rycina s07_multi).
ch3_multi_fig <- function() {
  d <- .cas_data
  vars <- unname(ch3_extra_choices)
  subsets <- unlist(lapply(0:length(vars), function(k) combn(vars, k, simplify = FALSE)), recursive = FALSE)
  labels_pl <- c("(Intercept)" = "Wyraz wolny", ch3_labels_pl)
  variants <- list(); html <- list()
  for (outcome in c("read", "math")) for (s in subsets) {
    preds <- c("income", s)
    key <- paste0(outcome, "|", if (length(s)) paste(s, collapse = "+") else "none")
    model <- lm(as.formula(paste(outcome, "~", paste(preds, collapse = " + "))), data = d)
    co <- broom::tidy(model)
    met <- compute_model_metrics(model)
    tab <- data.frame(term = unname(ifelse(co$term %in% names(labels_pl), labels_pl[co$term], co$term)),
      estimate = co$estimate, se = co$std.error, t = co$statistic,
      p = paste0(lc_pval(co$p.value), ifelse(co$p.value < 0.05, " *", "")))
    # Wiersze tabeli jako liczby (tabelę jak lc_table_split() składa przeglądarka).
    variants[[key]] <- list(coef = as.list(setNames(round(co$estimate, 10), co$term)),
      rows = lapply(seq_len(nrow(tab)), function(i) list(tab$term[i], signif(tab$estimate[i], 9), signif(tab$se[i], 9),
                                                          signif(tab$t[i], 9), gsub("&lt;", "<", tab$p[i], fixed = TRUE))))
    html[[key]] <- list(
      stats = s07_html(
        lc_readout("R²", round(met$r_squared, 3), color = unname(upwr_cat["niebo"])),
        lc_readout("adj.R²", round(met$adj_r_squared, 3), color = unname(upwr_cat["szalwia"])),
        lc_readout("AIC", round(met$aic, 1), color = unname(upwr_cat["bursztyn"])),
        lc_readout("RMSE", round(met$rmse, 3), color = unname(upwr_cat["terakota"]))))
  }
  simple <- lapply(c(read = "read", math = "math"), function(y) as.list(coef(lm(d[[y]] ~ d$income))))
  config <- list(
    key = list("ch3_outcome", "ch3_predictors"), variants = variants, html = html,
    labels = as.list(ch3_labels_pl),
    data = lapply(c(list(income = d$income, read = d$read, math = d$math), setNames(lapply(vars, function(v) d[[v]]), vars)), function(v) round(v, 4)),
    means = lapply(setNames(vars, vars), function(v) mean(d[[v]])),
    bins3 = lapply(setNames(vars, vars), function(v) ch3_bins(d[[v]], c("niskie", "średnie", "wysokie"))),
    bins2 = lapply(setNames(vars, vars), function(v) ch3_bins(d[[v]], c("niższe", "wyższe"))),
    simple = lapply(simple, function(cf) unname(unlist(cf))))
  facet <- function(k) tags$div(class = "s07-facet", tags$h4(`data-role` = paste0("facet", k)), lc_sf_chart(k, "1.5/1", "300px"))
  lc_sf_figure("s07_multi", config = config, legend = TRUE,
    control = list(
      selectInput("ch3_outcome", "Zmienna zależna Y", choices = c("Wynik: czytanie" = "read", "Wynik: matematyka" = "math"),
                  selected = "read", selectize = FALSE),
      checkboxGroupInput("ch3_predictors", "Predyktory dodane do dochodu okręgu", choices = ch3_extra_choices,
                         selected = "lunch", inline = TRUE)),
    toolbar = lc_readouts(tags$div(`data-role` = "stats", style = "display: contents")),
    body = tagList(
      tags$style(HTML(".s07-facets{display:grid;grid-template-columns:minmax(0,1fr);gap:.6em 1em}
        .s07-facets[data-n='3']{grid-template-columns:repeat(3,minmax(0,1fr))}
        .s07-facets[data-n='4']{grid-template-columns:repeat(2,minmax(0,1fr))}
        .s07-facet[hidden]{display:none!important}.s07-facet h4{margin:.2em 0;font-size:.9em}.s07-facet h4:empty{display:none}")),
      tags$div(class = "s07-facets", facet(0), facet(1), facet(2), facet(3)),
      tags$div(class = "lc-sf-legend", `data-role` = "cmp-legend", hidden = NA),
      lc_sf_chart(4, "2.6/1", "230px")),
    after = tags$div(`data-role` = "coefs"))
}

# Ryc. 3.2: trzy kroki na stałych danych — opisy rysunku, teksty i tabela z R (rycina s07_steps).
ch3_control_fig <- function() {
  d <- .cas_data
  step_scatter <- function(x, xl) sf_spec(sf_range(x), sf_range(d$read), list(
      sf_points(x, d$read, color = "@niebo", alpha = 0.7, size = 6),
      sf_smooth_lm(x, d$read, color = "@accent", se = FALSE)[[1]]),
    xl, "Wynik: czytanie", aria = paste("Wynik z czytania a", tolower(xl), "z prostą regresji."))
  ti <- broom::tidy(lm(read ~ income, data = d))
  tl <- broom::tidy(lm(read ~ lunch, data = d))
  tm <- broom::tidy(lm(read ~ income + lunch + english, data = d))
  co <- tm[-1, ]
  labels <- c(income = "Dochód okręgu", lunch = "Dotacje do obiadów", english = "Angielski jako drugi język")
  lo <- co$estimate - 1.96 * co$std.error; hi <- co$estimate + 1.96 * co$std.error
  # Kolejność od dołu jak oś kategorii ggplot: etykiety alfabetycznie.
  lab <- unname(labels[co$term])
  at <- match(lab, sort(lab))
  step3 <- sf_spec(sf_range(c(0, lo, hi)), c(0.5, nrow(co) + 0.5), list(
      sf_vline(0, color = "@secondary", width = 1.2, dash = c(3, 3)),
      sf_errorbars_h(lo, hi, at, cap = 0.2, color = "@accent", width = 1.6),
      sf_points(co$estimate, at, color = "@accent", size = 10)),
    "β w modelu wielorakim", "", yLabels = sf_labels(sort(lab)),
    aria = "Współczynniki modelu z kontrolą z przedziałami 95%.")
  b <- function(v) as.character(tags$b(v))
  texts <- list(
    `1` = paste0("β dochód = ", b(round(ti$estimate[2], 3)), ", p = ", b(HTML(lc_pval(ti$p.value[2]))), ". ",
                 "W modelu prostym bogatsze okręgi mają wyższe wyniki czytania."),
    `2` = paste0("β lunch = ", b(round(tl$estimate[2], 3)), ", p = ", b(HTML(lc_pval(tl$p.value[2]))), ". ",
                 "Odsetek uczniów z dotacją do obiadu jest silnie ujemnie powiązany z wynikiem czytania."),
    `3` = paste0("β dochód: ", b(round(ti$estimate[2], 3)), " w modelu prostym, ",
                 b(round(tm$estimate[tm$term == "income"], 3)),
                 " po kontroli dotacji do obiadów i angielskiego jako drugiego języka."))
  table <- s07_html(lc_table(data.frame(term = unname(ch3_labels_pl[co$term]), estimate = co$estimate,
      se = co$std.error, p = lc_pval(co$p.value)),
    cols = list(lc_col("term", "Zmienna", "row"), lc_col("estimate", "β", digits = 4),
                lc_col("se", "SE", digits = 4), lc_col("p", "p"))))
  lc_sf_figure("s07_steps", bare = TRUE,
    config = list(variants = list(`1` = step_scatter(d$income, "Dochód okręgu (tys. USD)"),
                                  `2` = step_scatter(d$lunch, "Dotacje do obiadów (%)"), `3` = step3),
                  texts = texts, html = list(`1` = list(info = ""), `2` = list(info = ""), `3` = list(info = table))),
    body = lc_step_widget("ch3_control",
      title = "Efekt pozorny i kontrola zmiennych",
      steps = c("Czytanie ~ dochód", "Czytanie ~ lunch", "Model z kontrolą"),
      toolbar = lc_toolbar(lc_chart_zoom_button()),
      body = lc_sf_chart(0, "2/1", "440px"),
      extra = tags$div(`data-role` = "info")))
}

# Ryc. 3.3: współliniowość na ustawionych przykładach (collinearity_case()).
ch3_case_fig <- function() {
  cases <- c(weak = "weak", cancel = "cancel", flip = "flip")
  captions <- list(
    weak = "Predyktory są słabo powiązane: w modelu z oboma każdy zachowuje
        swój kierunek i istotność, współczynniki tylko trochę maleją.",
    cancel = "Osobno każdy predyktor jest wyraźnie istotny. Razem żaden nie
        jest, choć model z oboma przewiduje wynik tak samo dobrze jak każdy
        z nich osobno (R² 0.49 wobec 0.47 i 0.48) — nie potrafi tylko
        rozdzielić wpływu między godziny a zadania.",
    flip = "Osobno więcej zadań to wyższy wynik. W modelu z godzinami
        współczynnik zadań staje się ujemny, a współczynnik godzin rośnie
        prawie dwukrotnie — model przerzucił wspólny wpływ na jedną zmienną.")
  parts <- lapply(cases, function(cs) {
    d <- collinearity_case(cs)
    pick <- function(m, term) {
      s <- summary(m)$coefficients
      if (term %in% rownames(s)) s[term, c(1, 4)] else c(NA, NA)
    }
    m_hz <- lm(wynik ~ godziny + zadania, d)
    rows <- list(lm(wynik ~ godziny, d), lm(wynik ~ zadania, d), m_hz)
    tab <- data.frame(model = c("Tylko godziny", "Tylko zadania", "Godziny + zadania"),
      b_h = sapply(rows, function(m) pick(m, "godziny")[1]), p_h = sapply(rows, function(m) pick(m, "godziny")[2]),
      b_z = sapply(rows, function(m) pick(m, "zadania")[1]), p_z = sapply(rows, function(m) pick(m, "zadania")[2]))
    fs <- summary(m_hz)$fstatistic
    p_model <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)
    cell <- function(b, p) lapply(seq_along(b), function(k) {
      if (is.na(b[k])) return(HTML("–"))
      tagList(formatC(b[k], format = "f", digits = 2),
              tags$small(style = "display:block;color:var(--upwr-ink-subtle);",
                         HTML(paste0("p ", if (p[k] < 0.001) "" else "= ", lc_pval(p[k])))))
    })
    tab$h <- I(cell(tab$b_h, tab$p_h)); tab$z <- I(cell(tab$b_z, tab$p_z))
    list(
      spec = sf_spec(c(2, 22), c(5, 55), list(sf_points(d$godziny, d$zadania, color = "@secondary", alpha = 0.7, size = 9)),
        "Godziny nauki", "Rozwiązane zadania", aria = "Wykres punktowy godzin nauki i liczby rozwiązanych zadań."),
      reads = list(case_r = formatC(cor(d$godziny, d$zadania), format = "f", digits = 2),
                   case_r2 = formatC(summary(m_hz)$r.squared, format = "f", digits = 2),
                   case_p = gsub("&lt;", "<", lc_pval(p_model), fixed = TRUE)),
      html = list(table = s07_html(lc_table(tab, cols = list(lc_col("model", "Model", "row"),
                    lc_col("h", "Godziny", sub = "pkt na godz."), lc_col("z", "Zadania", sub = "pkt na zadanie")))),
                  caption = s07_html(lc_caption(captions[[cs]]))))
  })
  lc_sf_spec(variants = lapply(parts, `[[`, "spec"), reads = lapply(parts, `[[`, "reads"), html = lapply(parts, `[[`, "html"),
    control = lc_segmented("ch3_case", "Przykład",
      choices = c("Słabo powiązane" = "weak", "Wyniki się znoszą" = "cancel", "Odwrócony znak" = "flip")),
    toolbar = lc_readouts(lc_readout("r", lc_sf_value("case_r")), lc_readout("R² modelu", lc_sf_value("case_r2")),
                          lc_readout("p modelu", lc_sf_value("case_p"))),
    body = lc_plots(lc_sf_chart(0, "1.3/1", "340px"), tags$div(`data-role` = "table")),
    after = tags$div(`data-role` = "caption"))
}

# ---------------------------------------------------------------------------
# Rozdział 3B
# ---------------------------------------------------------------------------

# Punkty gatunków: kolor i kształt jak w ggplot (koło, trójkąt, kwadrat).
ch3b_species_points <- function(d, x, y, alpha) {
  sp <- levels(d$species)
  cols <- c(Adelie = "@szalwia", Chinstrap = "@bursztyn", Gentoo = "@terakota")
  shapes <- c(Adelie = "circle", Chinstrap = "triangle", Gentoo = "rect")
  lapply(sp, function(s) {
    k <- d$species == s
    sf_points(d[[x]][k], d[[y]][k], color = cols[[s]], alpha = alpha, size = 7, symbol = shapes[[s]])
  })
}
ch3b_species_legend <- function() sf_legend(c("Adelie", "Chinstrap", "Gentoo"), list("@szalwia", "@bursztyn", "@terakota"))

# Ryc. 3B.1: paradoks Simpsona w trzech krokach (rycina s07_steps).
ch3b_simpson_fig <- function() {
  d <- .penguins_data
  simple <- lm(bill_depth_mm ~ bill_length_mm, data = d)
  ctrl <- lm(bill_depth_mm ~ bill_length_mm + species, data = d)
  xr <- range(d$bill_length_mm); yr <- range(d$bill_depth_mm)
  sx <- xr; sy <- predict(simple, data.frame(bill_length_mm = sx))
  sp <- levels(d$species)
  cols <- c(Adelie = "@szalwia", Chinstrap = "@bursztyn", Gentoo = "@terakota")
  own <- lapply(sp, function(s) {
    k <- d$species == s
    sf_smooth_lm(d$bill_length_mm[k], d$bill_depth_mm[k], color = cols[[s]], width = 2.2, se = FALSE)[[1]]
  })
  par <- lapply(sp, function(s) {
    r <- range(d$bill_length_mm[d$species == s])
    sf_line(r, predict(ctrl, data.frame(bill_length_mm = r, species = factor(s, levels = sp))), color = cols[[s]], width = 2.8)
  })
  spec <- function(layers, legend = NULL) sf_spec(sf_range(xr), sf_range(yr), layers,
    "Długość dzioba (mm)", "Wysokość dzioba (mm)", legend = legend,
    aria = "Punkty długości i wysokości dzioba pingwinów z prostymi regresji.")
  steps <- list(
    `1` = spec(list(sf_points(d$bill_length_mm, d$bill_depth_mm, color = "@secondary", alpha = 0.55, size = 7),
                    sf_line(sx, sy, color = "@accent", width = 2.6))),
    `2` = spec(c(ch3b_species_points(d, "bill_length_mm", "bill_depth_mm", 0.62), own), ch3b_species_legend()),
    `3` = spec(c(ch3b_species_points(d, "bill_length_mm", "bill_depth_mm", 0.35),
                 list(sf_line(sx, sy, color = "@secondary", width = 1.8, dash = c(3, 3))), par), ch3b_species_legend()))
  texts <- list(
    `1` = "Jedna prosta dla 333 pingwinów. Model nie wie, że punkty pochodzą z trzech gatunków.",
    `2` = "Kolor ujawnia trzy skupiska. W każdym gatunku prosta rośnie.",
    `3` = "Model z gatunkiem: trzy równoległe proste o wspólnym nachyleniu. Przerywana prosta bez gatunku opada.")
  lc_sf_figure("s07_steps", bare = TRUE,
    config = list(variants = steps, texts = texts),
    body = lc_step_widget("ch3b_simpson",
      title = "Paradoks Simpsona krok po kroku",
      steps = c("Jedna linia dla wszystkich", "Pokaż gatunki", "Kontroluj gatunek"),
      toolbar = lc_toolbar(lc_readouts(
        lc_readout("Nachylenie bez gatunku", sprintf("%.2f", coef(simple)[["bill_length_mm"]])),
        lc_readout("Nachylenie po kontroli gatunku", sprintf("%.2f", coef(ctrl)[["bill_length_mm"]]), color = upwr_accent)),
        lc_chart_zoom_button()),
      # Legenda gatunków nad wykresem, pod paskiem kroków.
      body = tagList(tags$div(class = "lc-sf-legend", hidden = NA), lc_sf_chart(0, "2/1", "440px"))))
}

# Ryc. 3B.4: model addytywny i model z interakcją (warianty z R).
ch3b_interaction_fig <- function() {
  d <- .penguins_data
  sp <- levels(d$species)
  cols <- c(Adelie = "@szalwia", Chinstrap = "@bursztyn", Gentoo = "@terakota")
  fr <- range(d$flipper_length_mm)
  parts <- lapply(c(add = "add", interaction = "interaction"), function(kind) {
    model <- if (kind == "interaction") lm(body_mass_g ~ flipper_length_mm * species, data = d) else
      lm(body_mass_g ~ flipper_length_mm + species, data = d)
    lines <- lapply(sp, function(s) sf_line(fr, predict(model, data.frame(flipper_length_mm = fr, species = factor(s, levels = sp))),
                                            color = cols[[s]], width = 2.4))
    table <- broom::tidy(model)
    table$label <- vapply(table$term, function(term) if (term %in% names(.ch3b_term_labels)) unname(.ch3b_term_labels[[term]]) else term, character(1))
    rmse <- sqrt(mean(residuals(model)^2))
    list(
      spec = sf_spec(sf_range(fr), sf_range(d$body_mass_g), c(ch3b_species_points(d, "flipper_length_mm", "body_mass_g", 0.42), lines),
        "Długość płetwy (mm)", "Masa ciała (g)", legend = ch3b_species_legend(),
        aria = "Masa ciała względem długości płetwy dla trzech gatunków pingwinów z liniami modelu."),
      html = list(
        stats = s07_html(
          lc_readout("AIC", round(AIC(model), 1), color = unname(upwr_cat["wrzos"])),
          lc_readout("RMSE", round(rmse, 1), color = upwr_secondary),
          lc_readout("Parametry", length(coef(model)), color = unname(upwr_cat["bursztyn"]))),
        table = s07_html(lc_table(
          data.frame(label = table$label, estimate = table$estimate, se = table$std.error, p = lc_pval(table$p.value)),
          cols = list(lc_col("label", "Składnik", "row"),
                      lc_col("estimate", "Współczynnik", digits = 3, short = "b", desc = "współczynnik"),
                      lc_col("se", "Błąd stand.", digits = 3, short = "SE", desc = "błąd standardowy"),
                      lc_col("p", "p"))))))
  })
  lc_sf_spec(variants = lapply(parts, `[[`, "spec"), html = lapply(parts, `[[`, "html"), legend = TRUE,
    control = lc_segmented("ch3b_interaction_model", "Model",
      choices = c("Addytywny: płetwa + gatunek" = "add", "Z interakcją: płetwa × gatunek" = "interaction"), selected = "add"),
    toolbar = lc_readouts(tags$div(`data-role` = "stats", style = "display: contents")),
    ratio = "2/1", max_height = "400px",
    after = tags$div(`data-role` = "table"))
}
