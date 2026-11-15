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
