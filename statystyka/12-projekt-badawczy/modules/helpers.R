# ============================================================================
# Helpery - projekt badawczy / TeachingRatings
# ============================================================================

proj_col_data <- unname(upwr_cat["niebo"])
proj_col_hyp  <- unname(upwr_cat["wrzos"])
proj_col_ctrl <- unname(upwr_cat["szalwia"])
proj_col_warn <- unname(upwr_cat["bursztyn"])
proj_col_risk <- upwr_accent
proj_col_ref  <- upwr_reference

load_teaching_ratings <- function() {
  dat <- NULL
  if (requireNamespace("AER", quietly = TRUE)) {
    dat <- tryCatch({
      data("TeachingRatings", package = "AER", envir = environment())
      get("TeachingRatings", envir = environment())
    }, error = function(e) NULL)
  }
  if (is.null(dat)) {
    dat <- read.csv(file.path(app_dir, "dane", "teaching_ratings.csv"),
                    stringsAsFactors = FALSE)
  }

  if ("rownames" %in% names(dat)) dat$rownames <- NULL
  dat$response.rate <- round(dat$students / dat$allstudents * 100, 1)
  dat$minority <- factor(dat$minority, levels = c("no", "yes"),
                         labels = c("nie", "tak"))
  dat$gender <- factor(dat$gender, levels = c("female", "male"),
                       labels = c("kobieta", "mężczyzna"))
  dat$credits <- factor(dat$credits, levels = c("more", "single"),
                        labels = c("więcej niż 1", "jednopunktowy"))
  dat$division <- factor(dat$division, levels = c("lower", "upper"),
                         labels = c("niższy", "wyższy"))
  dat$native <- factor(dat$native, levels = c("no", "yes"),
                       labels = c("nie", "tak"))
  dat$tenure <- factor(dat$tenure, levels = c("no", "yes"),
                       labels = c("nie", "tak"))
  dat$prof <- factor(dat$prof)
  dat$age_group <- cut(dat$age,
                       breaks = c(-Inf, 40, 55, Inf),
                       labels = c("do 40", "41-55", "powyżej 55"),
                       right = TRUE)
  dat
}

tr_data <- load_teaching_ratings()

tr_labels <- c(
  eval = "Ocena kursu",
  beauty = "Ocena atrakcyjności",
  age = "Wiek prowadzącego",
  age_group = "Grupa wieku",
  students = "Liczba odpowiedzi",
  allstudents = "Liczba zapisanych",
  response.rate = "Response rate (%)",
  gender = "Płeć",
  minority = "Mniejszość",
  native = "Native speaker",
  tenure = "Tenure track",
  division = "Poziom kursu",
  credits = "Punkty ECTS/credits"
)

tr_numeric_vars <- c("eval", "beauty", "age", "students", "allstudents", "response.rate")
tr_confounder_vars <- c("gender", "age", "tenure", "minority", "native", "credits", "division")

tr_fmt_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("< 0,001")
  gsub("\\.", ",", sprintf("%.3f", p))
}

tr_model_table <- function(models) {
  rows <- lapply(seq_along(models), function(i) {
    m <- models[[i]]$model
    g <- broom::glance(m)
    coefs <- broom::tidy(m)
    b <- coefs$estimate[coefs$term == "beauty"]
    p <- coefs$p.value[coefs$term == "beauty"]
    data.frame(
      model = models[[i]]$label,
      beta_beauty = b,
      p_beauty = p,
      adj_r2 = g$adj.r.squared,
      aic = AIC(m),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

tr_coef_labels <- c(
  "(Intercept)" = "Intercept",
  beauty = "Atrakcyjność",
  age = "Wiek",
  students = "Liczba odpowiedzi",
  allstudents = "Liczba zapisanych",
  response.rate = "Response rate",
  "gendermężczyzna" = "Płeć: mężczyzna",
  "minoritytak" = "Mniejszość: tak",
  "nativetak" = "Native: tak",
  "tenuretak" = "Tenure: tak",
  "divisionwyższy" = "Poziom: wyższy",
  "creditsjednopunktowy" = "Kurs jednopunktowy"
)

tr_label_term <- function(term) {
  out <- unname(tr_coef_labels[term])
  ifelse(is.na(out), term, out)
}

tr_discussion_box <- function(title, ...) {
  div(class = "lc-feedback lc-feedback-info",
    tags$strong(title),
    tags$ul(...)
  )
}

tr_question_card <- function(title, lead, ...) {
  div(class = "question-card",
    h4(title),
    p(lead),
    tags$ul(...)
  )
}

tr_mean_diff <- function(group_var) {
  dat <- tr_data[!is.na(tr_data$eval) & !is.na(tr_data[[group_var]]), ]
  grp <- droplevels(dat[[group_var]])
  lev <- levels(grp)
  if (length(lev) != 2) return(NULL)
  m1 <- mean(dat$eval[grp == lev[1]])
  m2 <- mean(dat$eval[grp == lev[2]])
  data.frame(
    group1 = lev[1],
    group2 = lev[2],
    mean1 = m1,
    mean2 = m2,
    diff = m2 - m1,
    stringsAsFactors = FALSE
  )
}

tr_group_test <- function(group_var, method = "t") {
  dat <- tr_data[!is.na(tr_data$eval) & !is.na(tr_data[[group_var]]), ]
  dat[[group_var]] <- droplevels(dat[[group_var]])
  form <- as.formula(paste("eval ~", group_var))
  if (method == "wilcox") {
    res <- wilcox.test(form, data = dat)
    list(name = "Mann-Whitney", statistic = unname(res$statistic), p = res$p.value)
  } else {
    res <- t.test(form, data = dat)
    list(name = "test t", statistic = unname(res$statistic), p = res$p.value)
  }
}

tr_multi_group_test <- function(group_var, method = "anova") {
  dat <- tr_data[!is.na(tr_data$eval) & !is.na(tr_data[[group_var]]), ]
  form <- as.formula(paste("eval ~", group_var))
  if (method == "kruskal") {
    res <- kruskal.test(form, data = dat)
    list(name = "Kruskal-Wallis", statistic = unname(res$statistic), p = res$p.value)
  } else {
    res <- summary(aov(form, data = dat))[[1]]
    list(name = "ANOVA", statistic = unname(res$`F value`[1]), p = res$`Pr(>F)`[1])
  }
}

tr_research_verdict <- function(p, effect_text) {
  if (is.na(p)) {
    "Nie udało się policzyć testu dla tych danych."
  } else if (p < 0.05) {
    paste0("Wynik wzmacnia trop: ", effect_text,
           ". To jeszcze nie dowód przyczynowy, ale dobry powód, żeby zadać kolejne pytanie.")
  } else {
    paste0("Wynik osłabia prostą wersję tropu: ", effect_text,
           ". To nie znaczy, że temat znika; raczej trzeba zmienić hipotezę albo pomiar.")
  }
}

tr_assoc_strength <- function(x, z) {
  ok <- !is.na(x) & !is.na(z)
  x <- x[ok]
  z <- z[ok]
  if (is.numeric(z)) {
    val <- suppressWarnings(abs(cor(x, z, use = "complete.obs")))
    list(value = val, label = paste0("|r| = ", round(val, 3)), is_linked = val > 0.10)
  } else {
    z <- droplevels(factor(z))
    meds <- tapply(x, z, median, na.rm = TRUE)
    val <- max(meds, na.rm = TRUE) - min(meds, na.rm = TRUE)
    list(value = val, label = paste0("różnica median = ", round(val, 3)),
         is_linked = val > 0.05)
  }
}

tr_confounder_summary <- local({
  rows <- lapply(tr_confounder_vars, function(var) {
    bx <- tr_assoc_strength(tr_data$beauty, tr_data[[var]])
    ey <- tr_assoc_strength(tr_data$eval, tr_data[[var]])
    data.frame(
      variable = var,
      label = unname(tr_labels[var]),
      beauty_value = bx$value,
      beauty_label = bx$label,
      beauty_linked = bx$is_linked,
      eval_value = ey$value,
      eval_label = ey$label,
      eval_linked = ey$is_linked,
      confounder = bx$is_linked && ey$is_linked,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
})

tr_confounder_row <- function(var) {
  tr_confounder_summary[tr_confounder_summary$variable == var, , drop = FALSE]
}
